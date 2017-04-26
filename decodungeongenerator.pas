{Copyright (C) 2012-2017 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

{---------------------------------------------------------------------------}

{ Dungeon (a maze-like interior) map generator }
unit decodungeongenerator;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, CastleRandom, fgl, CastleGenericLists,
  decoabstractgenerator, decodungeontiles,
  decoglobal;

type
  {a "dock point" of a tile or a map. This is a xyz coordinate of an open face
   we use it to dock the tile to the map -
   i.e. we "dock" two dock points together if they fit each other like in puzzles}
  DDockPoint = record
    {coordinates of the dock point (relative to parent)}
    x,y,z: TIntCoordinate;
    {face where dock point is open. Each dock point contains one face.
     if there are two (or more) exits from a tile element - it spawns two (or more) dock points}
    face: TAngle;
    {the face type of the dock point. For faster checking}
    facetype: TTileFace;
  end;
type TDockPointList = specialize TGenericStructList<DDockPoint>;

type
  {extended functionality for DTileMap;
   we have some "excess" functionality from DTileMap left here like load,
   blocker, etc. But excluding it will be a bit messy, because for DGeneratorTile
   we need both DTileMap functionality and DGeneratorMap functionality}
  DGeneratorMap = class(DTileMap)
    private
      fFreeFaces: integer;
      fVolume: integer;
      fMaxDepth: integer;
    public
      {shift that blocker makes. *Physically* blocker is placed in the NEXT tile
       but *logically* it is related to the PREVIOUS tile. These coordinate shifts
       allow to switch from one logic to another}
      b_x,b_y,b_z: integer;
      {calculates b_x,b_y,b_z for 1x1x1 blockers with only ONE face
       horizontal angles only (for now) - todo}
      procedure ProcessBlockers;
    public
      {what TTileFaces do leave from this tile?}
      //FacesList: TTileFace;
      {list of dock point to this tile}
      Dock: TDockPointList;
      {total free Faces of the map/tile}
      property FreeFaces: integer read fFreeFaces;
      {total passable volume of the map/tile}
      property Volume: integer read fVolume;
      {deepest level of the map (0..maxz-1)}
      property MaxDepth: integer read fMaxDepth;
      {calculates faces&volume of the tile/map and prepares it for work
       optimized for DGeneratorMap}
      function CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

      destructor destroy; override;
  end;
type
  {DGeneratorMap extended with some tile-related parameters}
  DGeneratorTile = class(DGeneratorMap)
  private
    fHasStairsDown: boolean;
  public
    {does this tile/map has stairs down?}
    property HasStairsDown: boolean read fHasStairsDown;
    {calculates faces&volume of the tile/map and prepares it for work
     full version}
    function CalculateFaces: integer; reintroduce; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
end;
type TTileList = specialize TFPGObjectList<DGeneratorTile>;

type
  {this is a basic "add a tile" generator step. We can build a map by followig these
   we can also use preset generatoe steps to make some tiles already available at map}
  DGeneratorStep = record
    {id of the tile}
    tile: TTileType;
    {coordinates to place the tile}
    x,y,z: TIntCoordinate;
  end;

{how often will the dynamic array be rescaled. The more this number, the less
 is RAM-efficiency but higher CPU-efficiency}
const GeneratorShift = 100;
type
  {Generator steps array is a dynamic array because it is an optimization trick
   to support for quick undo's and it is not required to be RAM-efficient, but
   CPU-efficient. We resize the array only on Undo or every 100 steps, and while
   the map in normal conditions is under 100 steps then it shouldn't slow us down in any way}
  TGeneratorStepsArray = array of DGeneratorStep;

type
  {this is almost equal to TGeneratorStep except it refers to tiles by their names}
  DFirstStep = record
    {name of the tile}
    tile: string;
    {coordinates to place the tile}
    x,y,z: TIntCoordinate;
  end;
  TFirstStepsArray = specialize TGenericStructList<DFirstStep>;

type
  {a set of map generation parameters}
  DGeneratorParameters = class
    {the map cannot be larger than these}
    maxx,maxy,maxz: TIntCoordinate;
    {the map cannot be smaller than these}
    minx,miny,minz: TIntCoordinate;
    {target map volume. The map will be regenerated until it's met
     Usually the algorithm can handle up to ~30% packaging of maxx*maxy*maxz}
    Volume: integer;
    {when the map has FreeFaces>MaxFaces it will try to "Shrink" the amount
     of free faces by using poor tiles}
    MaxFaces: integer;
    {when the map has FreeFaces<MinFaces it will try to "extend" the amount
     of free faces by using rich tiles (in case the Volume is not yet met)}
    MinFaces: integer;
    {random generation seed}
    Seed: LongWord;
    {are links in TilesList absolute URL or just a tile name}
    absoluteURL: boolean;
    {list of tiles}
    TilesList: TStringList;
    {list of first generation steps}
    FirstSteps: TFirstStepsArray;

    constructor create;
    destructor destroy; override;
  end;

type TIndexList = specialize TFPGList<TTileType>;

type
  { Preforms all map generation routines
    This is a relatively CPU-intensive work therefore must be heavily optimized }
  DDungeonGenerator = class(DAbstractGenerator)
  private
    {list of tiles used in current map}
    Tiles: TTileList;
    {a list of "normal" tiles used in generation}
    NormalTiles: TIndexList;
    {how many dock points are there in the normal tileset?
     this is a constant used in the algorithm:
     We try to get a random tile NormalTilesTotalDocks tries
     and if we fail where we just use sequential selection
     if sequential selection fails, we use a blocker as a last resource
     The logic behind such approach is the following:
     it's regularly "normal" to add a random tile and it's very fast
     However, in some cases (e.g. if only one specific tile can dock here
     or no tile at all can dock at this map dock point) we need to try
     all the tiles sequentially and see which ones fit. This is a CPU-heavy
     procedure, but it improves the overall map quality.
     NormalTilesTotalDocks is a limit where we give up "quick" random and start
     sequential search
     SEQUENTIAL search is predictable! It'll just pick up the first tile that
     fits the current dock point and will stop.}
    NormalTilesTotalDocks: integer;
    {a list of blockers}
    BlockerTiles: TIndexList;
    {a list of down-going tiles}
    DownTiles: TIndexList;
    {a list of tiles with 3 and more free faces}
    RichTiles: TIndexList;
    {a list of tiles with 1 or 2 free faces}
    PoorTiles: TindexList;
    {searches Tiles for a specific tile name and returns it
     used to make first steps of the generation as they are bound to
     a specific tile name, not to a tile ID which can change}
    //function GetTileByName(TileName: string): DGeneratorMap;
    function GetTileByName(TileName: string): TTileType;

    {gets a "normal" tile, not a blocker}
    function GetNormalTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {gets a "blocker", not a normal tile}
    function GetBlockerTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {gets a tile with stairs down}
    function GetDownTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {gets a tile with 3 and more free faces
     in case we need to "broaden" the map}
    function GetRichTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {gets a tile with 1 or 2 free faces
     in case we need to "narrow" the map}
    function GetPoorTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  private
    {the main operating item - map of the resulting dungeon}
    Map: DGeneratorMap;

    {this stores a sequence of generator steps}
    Gen: TGeneratorStepsArray;
    {minSteps: minimal steps before undo, this corresponds to the pre-generated tiles which may not be cancelled}
    minsteps: integer;
    {current size of the dynamic array}
    maxsteps: integer;
    {current (the last used) step of the generator;
     careful, currentStep starts from -1, while corresponding min/max steps start from 0
     so conversion is min/maxStep = currentStep+1}
    CurrentStep: integer;
    {adds +1 to CurrentStep and resizes the array if necessary}
    procedure incCurrentStep; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {resizes the dynamic array - adds 100 steps if needed}
    procedure ResizeSteps; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    {checks if the selected 1x1x1 element is compatible to the already-generated map
     usually should not be added manually, but is used inline in "AddTile"
     Returns false if tile mismatches Map
     Returns True if tile matches Map and can be placed }
    function CheckCompatibility(Tile: BasicTile; x,y,z: TIntCoordinate): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {Core of the algorithm : Tries to add a random tile to the map}
    procedure AddRandomTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {checks tile compatibility to the map and adds it if its possible
     returns true if tile is put successfully
     and false if the tile cannot be placed at these coordinates  }
    function AddTile(tile: TTileType; x,y,z: TIntCoordinate): boolean;
    {When we're sure what we are doing, we're not making any checks, just add the tile as fast as it is possible}
    procedure AddTileUnsafe(Tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {overloaded version that accepts a DGeneratorStep;}
    procedure AddTileUnsafe(step: DGeneratorStep);
    {creates a minimap as Map.img}
    procedure MakeMinimap;
    {resizes the generated map for its real size}
    procedure ShrinkMap;
  public
    {map parameters}
    Parameters: DGeneratorParameters;
    {copy the "internal" map to external request}
    function GetMap: DMap; //maybe better saveTo when finished?: DMap;

    procedure InitParameters; override;
    { the main procedure to generate a dungeon,
      may be launched in main thread (for testing or other purposes) }
    procedure Generate; override;

    constructor Create; override;
    destructor Destroy; override;
    procedure FreeLists;
    
    {loads and applies the map parameters}
    procedure Load(filename: string);
    
    //save temp state to file ---- unneeded for now
end;

type TIntMapArray = array of array of array of integer;
type
  {coordinate of a raycast_to candidate}
  Txyz = record
    x,y,z: TIntCoordinate;
  end;
type TRaycastList = specialize TGenericStructList<Txyz>;

type
  {a two-value description of a "neighbour" tile}
  DNeighbour = record
    {index of current tile}
    tile: TTileType;
    {visiblity of the tile ~spatial angle}
    visible: integer;
  end;

type TNeighboursList = specialize TGenericStructList<DNeighbour>;
type TNeighboursMapArray = array of array of array of TNeighboursList;

type
  {this is a Dungeon Generator with additional 3D world generation,
   and linked stuff like raycast and chunk-n-slice the dungeon into parts}
  D3DDungeonGenerator = class(DDungeonGenerator)
  private
    {how accurate will be determination of the "visible tile or not"
     64 is the basic number (cornerCount), actually 128 should be enough}
    const MaxNeighboursIndex = 128;
    const FailedIndex = -1000;
    //const CandidateIndex = -1;
    const CornerCount = 8*8;
  private
    {a map that stores tiles markers for quick access...
     maybe not needed?}
    TileIndexMap: TIntMapArray;
    {returns an integer array of (map.sizex,map.sizey,map.sizez) size}
    function ZeroIntegerMap: TIntMapArray;
    {puts tile # markers on a map to detect which tile is here}
    procedure MakeTileIndexMap;
  private
    {temporary map for first-order neighbours lists}
    tmpNeighboursMap: TNeighboursMapArray;
    {final neighbours map}
    NeighboursMap: TNeighboursMapArray;

    Groups: array of TIndexList;
    {merge the neighbours of neighbours in order for the light to work smoothly
     todo!!!}
    procedure Neighbours_of_neighbours;
    {initializes a neighbours map with nils}
    function NilIndexMap: TNeighboursMapArray;
    {free every element of a Neigobours map}
    procedure FreeNeighboursMap(var nmap: TNeighboursMapArray);
    {remove duplicates in the sorted tiles list and (!!!) sorts the array
     this operation is performed twice during the generation, so efficiency is not of concern}
    procedure RemoveDuplicatesNeighbours(var List: TNeighboursList);
  private
    {this procedure raycasts from each and every map base element
     and returns Neighbours array}
    procedure Raycast;
    {preforms all possible raycasting from a given tile}
    procedure RaycastTile(mx,my,mz: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {raycasts a single ray from x1y1z1 to x2y2z2
     returns true if ray can pass, false otherwise}
    function Ray(x1,y1,z1,x2,y2,z2: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {chunks the map according to the visibility of the tiles}
    procedure Chunk_N_Slice;
  public
    {launches DDungeonGenerator.Generate and builds 3D world afterwards
     can be launched directly for debugging}
    procedure Generate; override;

    destructor destroy; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils, CastleImages, CastleVectors;

{========================= GENERATION ALGORITHM ==============================}

procedure DDungeonGenerator.InitParameters;
var s: string;
    tmp: DGeneratorTile;
    i : integer;
begin
  if not isReady then
    raise exception.create('DDungeonGenerator.Generate FATAL - parameters are not loaded!');
  {load tiles}
  tiles.clear;
  For s in parameters.TilesList do begin
    if parameters.AbsoluteURL then
      tmp := DGeneratorTile.Load(s+'.map')
    else
      tmp := DGeneratorTile.Load(ApplicationData(s+'.map'+GZ_ext));
    tmp.CalculateFaces;
    tmp.ProcessBlockers;
    tiles.Add(tmp);
  end;
  {prepare different optimized lists}
  for i := 0 to Tiles.Count-1 do begin
    if Tiles[i].blocker then
      BlockerTiles.Add(i)
    else
    begin
      NormalTiles.add(i);
      if Tiles[i].HasStairsDown then
        DownTiles.add(i);
      if Tiles[i].FreeFaces>=3 then
        RichTiles.add(i)
      else
        PoorTiles.add(i);
    end;
  end;

  NormalTilesTotalDocks := 0;
  for i := 0 to NormalTiles.Count-1 do
    inc(NormalTilesTotalDocks,Tiles[NormalTiles[i]].FreeFaces);

  {initialize random seed}
  InitSeed(parameters.seed);

  {initialize map size}
  Map.SetSize(parameters.maxx,parameters.maxy,parameters.maxz); //we don't care about RAM now, we'll shrink the map later

  {load pregenerated tiles}
  maxSteps := 0;
  setlength(Gen, maxSteps);
  CurrentStep := -1;

  for i := 0 to parameters.FirstSteps.Count-1 do begin
    incCurrentStep;
    Gen[CurrentStep].tile := GetTileByName(parameters.FirstSteps[i].tile);
    Gen[CurrentStep].x := parameters.FirstSteps[i].x;
    Gen[CurrentStep].y := parameters.FirstSteps[i].y;
    Gen[CurrentStep].z := parameters.FirstSteps[i].z;
  end;

  minSteps := currentStep+1;
  fisInitialized := true;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.GetNormalTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := NormalTiles[RNDM.Random(NormalTiles.Count)];
end;
function DDungeonGenerator.GetBlockerTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  {$Warning tiles count might be zero!}
  Result := BlockerTiles[RNDM.Random(BlockerTiles.Count)];
end;
function DDungeonGenerator.GetDownTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  {$Warning tiles count might be zero!}
  Result := DownTiles[RNDM.Random(DownTiles.Count)];
end;
function DDungeonGenerator.GetRichTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  {$Warning tiles count might be zero!}
  Result := RichTiles[RNDM.Random(RichTiles.Count)];
end;
function DDungeonGenerator.GetPoorTile: TTileType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := PoorTiles[RNDM.Random(PoorTiles.Count)];
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.AddRandomTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var d,t,td: integer;
    tt: integer;
    Success: boolean;
    TriesCount: integer;

  function CanDock: boolean;
  begin
    Result := (InvertAngle(Tiles[t].Dock[td].face) = Map.Dock[d].face) and
               (Tiles[t].Dock[td].facetype = Map.Dock[d].facetype)
  end;
  function TryAdd: boolean;
  begin
    Result :=
      AddTile(t, Map.Dock[d].x - Tiles[t].Dock[td].x + a_dx(Map.Dock[d].face),
                 Map.Dock[d].y - Tiles[t].Dock[td].y + a_dy(Map.Dock[d].face),
                 Map.Dock[d].z - Tiles[t].Dock[td].z + a_dz(Map.Dock[d].face))
  end;

begin
  {select a map dock point
   we shall work with this dock point all the time and will finally
   either dock a tile to it or block it out}
  d := RNDM.Random(Map.Dock.Count);
  TriesCount := 0;
  repeat
    {try add a random normal tile}
    repeat
      if RNDM.Random<0.9 then begin
        if (Map.FreeFaces > parameters.MaxFaces) or (Map.Volume>Parameters.Volume) then
          t := GetPoorTile
        else
        if (Map.FreeFaces < parameters.MinFaces) and (Map.Volume<Parameters.Volume) then
          t := GetRichTile
        else
        if (Map.MaxDepth < parameters.minz*(Map.Volume/Parameters.Volume)) then
          t := GetDownTile
        else
          t := GetNormalTile;
      end else
        t := GetNormalTile;

      td := RNDM.Random(Tiles[t].Dock.count);
    until CanDock;
    Success := TryAdd;
    inc(TriesCount);
  until Success or (TriesCount>NormalTilesTotalDocks);

  if not Success then begin
    {if we've failed random search next we try sequential search}
    for tt := 0 to NormalTiles.count-1 do begin
      t := NormalTiles[tt];
      for td := 0 to Tiles[t].Dock.count-1 do
        if canDock then
          if TryAdd then exit;
    end;
    {if we haven't "exited" yet, then there is no tile that fits this
     map's dock point. We have to block it out}
    for tt := 0 to BlockerTiles.count-1 do begin
      t := BlockerTiles[tt];
      for td := 0 to Tiles[t].dock.count-1 do //actually it should be always just one
        if canDock then
          if TryAdd then exit;
    end;
    {finally, if we can't even block the face out... it's horrible :(
     The only alternative to just hanging forever up is...}
    writelnLog(inttostr(BlockerTiles.count));
    writelnLog('x='+inttostr(Map.Dock[d].x));
    writelnLog('y='+inttostr(Map.Dock[d].y));
    writelnLog('z='+inttostr(Map.Dock[d].z));
    writeLnLog('face='+inttostr(Map.Dock[d].facetype));
    WriteLnLog('at '+AngleToStr(Map.Dock[d].face));
    raise Exception.create('DDungeonGenerator.AddRandomTile: FATAL! Unable to block the map element.')
  end;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.Generate;
var i: integer;
    t1,t2: TDateTime;
begin
  if not isReady then
    raise exception.create('DDungeonGenerator.Generate FATAL - parameters are not loaded!');
  if not isInitialized then begin
    WriteLnLog('DDungeonGenerator.Generate','Warning: parameters were automatically initialized! It''s best to initialize parameters manually outside the thread.');
    InitParameters;
  end;
  if isWorking then begin
    WriteLnLog('DDungeonGenerator.Generate','Generation thread is buisy! Aborting...');
    exit;
  end;
  fisWorking := true;

  t1 := now;
  repeat
    t2 := now;
    Map.EmptyMap(false);
    //add prgenerated or undo tiles
    CurrentStep := RNDM.random(currentStep);
    if CurrentStep < MinSteps-1 then CurrentStep := MinSteps-1;
    WriteLnLog('DDungeonGenerator.Generate','Starting from '+inttostr(currentstep));
    for i := 0 to currentStep do AddTileUnsafe(Gen[i]);

    while Map.CalculateFaces<>0 do begin
      {writeLnLog(inttostr(Map.dock.count));}
      //add a tile
      AddRandomTile;
    end;
    {if map doesn't meet the paramters then undo}

    //if failed then try to undo and regenerate the map again
    {if false then begin
      if CurrentStep > MinSteps then begin
        CurrentStep := MinSteps + RNDM.random(currentStep-MinSteps);
        MaxSteps := CurrentStep;
        setLength(Gen,MaxSteps);

        LastUndo := min;
        ...
        if LastMax<current then
          LastMax := current;
          LastUndo := LastMax;
        end;
        goto 10% behind LastUndo>min;
        or %random to avoid some no-way-out cases

      end;
    end;  }
    WriteLnLog('DDungeonGenerator.Generate','Done in = '+inttostr(round((now-t2)*24*60*60*1000))+'ms');
    WriteLnLog('DDungeonGenerator.Generate','Map volume = '+inttostr(map.Volume) +'/'+inttostr(parameters.volume));
    WriteLnLog('DDungeonGenerator.Generate','Map volume = '+inttostr(map.MaxDepth+1)+'/'+inttostr(parameters.minz));
  until (map.Volume>=parameters.Volume) and (map.MaxDepth+1>=parameters.minz); {until map meets the paramters}

  //finally resize the dynamic array
  MaxSteps := CurrentStep+1;
  SetLength(Gen,MaxSteps);
  WriteLnLog('DDungeonGenerator.Generate','Job finished in = '+inttostr(round((now-t1)*24*60*60*1000))+'ms');

  // finalize
  FreeLists; //we no longer need them

  ShrinkMap;
  MakeMinimap;

  fisWorking := false;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.GetMap: DMap;
var ix,iy,iz: TIntCoordinate;
begin
  Result := DMap.create;
  Result.setsize(Map.sizex,Map.sizey,Map.sizez);
  for ix := 0 to Map.sizex-1 do
    for iy := 0 to Map.sizey-1 do
      for iz := 0 to Map.sizez-1 do
        Result.map[ix,iy,iz] := Map.Map[ix,iy,iz];
  setLength(Result.img,length(Map.img));
  for iz := 0 to Map.sizez-1 do
    Result.img[iz] := Map.img[iz].MakeCopy as TRGBAlphaImage;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.ResizeSteps; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if currentstep+1>=maxsteps then begin
    inc(maxsteps,GeneratorShift);
    setlength(Gen, maxsteps);
  end
end;
procedure DDungeonGenerator.incCurrentStep; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  inc(currentStep);
  ResizeSteps;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.AddTile(tile: TTileType; x,y,z: TIntCoordinate): boolean;
var jx,jy,jz: TIntCoordinate;
begin
  Result := false;
  //check all tiles against map area they are placed to
  for jx := 0 to tiles[tile].sizex-1 do
   for jy := 0 to tiles[tile].sizey-1 do
    for jz := 0 to tiles[tile].sizez-1 do
      if not CheckCompatibility(tiles[tile].Map[jx,jy,jz],x+jx,y+jy,z+jz)
      then exit;

  AddTileUnsafe(Tiles[tile],x,y,z);
  {add current step to GEN}
  incCurrentStep;
  Gen[CurrentStep].tile := tile;
  Gen[CurrentStep].x := x;
  Gen[CurrentStep].y := y;
  Gen[CurrentStep].z := z;

  Result := true;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.AddTileUnsafe(tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var jx,jy,jz: TIntCoordinate;
    a: TAngle;
begin
  for jx := 0 to tile.sizex-1 do
   for jy := 0 to tile.sizey-1 do
    for jz := 0 to tile.sizez-1 do begin
      if Tile.Map[jx,jy,jz].base <> tkNone then
        Map.Map[x+jx,y+jy,z+jz].base := Tile.Map[jx,jy,jz].base;
      for a in TAngle do if Tile.Map[jx,jy,jz].faces[a] <> tfNone then
        if Tile.blocker then begin
          Map.Map[x+jx,y+jy,z+jz].faces[a] := tfWall;
          Map.Map[x+jx+a_dx(a),y+jy+a_dy(a),z+jz+a_dz(a)].faces[InvertAngle(a)] := tfWall;
        end
        else
          Map.Map[x+jx,y+jy,z+jz].faces[a] := Tile.Map[jx,jy,jz].faces[a];
    end;
end;
procedure DDungeonGenerator.AddTileUnsafe(step: DGeneratorStep);
begin
  AddTileUnsafe(Tiles[step.tile],step.x,step.y,step.z);
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.CheckCompatibility(Tile: BasicTile; x,y,z: TIntCoordinate): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var TmpTile: BasicTile;
    a: TAngle;
begin
  Result := true;
  if Tile.base <> tkNone then begin   // we can place an "empty" tile anywhere whether it's just empty or a blocker
    TmpTile := Map.MapSafe(x,y,z);
    {check if tile base fits the map}
    if TmpTile.base <> tkNone then
      Result := false // if the "map" is occupied at (x,y,z) nothing but "empty" can be placed there
    else begin
      {check current tile faces fit the map}
      for a in TAngle do if (TmpTile.faces[a] <> Tile.faces[a]) and
                            (TmpTile.faces[a] <> tfNone) then Result := false;
      if Result then begin
        {if current tile ok, then check adjacent tiles}
        {this algorithm may be significantly optimized by
         moving into AddTile where each tile
         would add not only it's basicTile, but also inverse angles at adjacent tiles
         however, not sure about how bug-free that'll be.}
        //horizontal angles -> full 6 angles
        for a in TAngle do begin
          TmpTile := Map.MapSafe(x+a_dx(a), y+a_dy(a), z+a_dz(a));
          if TmpTile.base <> tkInacceptible then
            if (TmpTile.faces[InvertAngle(a)] <> Tile.faces[a]) and
               (TmpTile.faces[invertAngle(a)] <> tfNone) then Result := false;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure DDungeonGenerator.MakeMinimap;
var i: integer;
    iz: TIntCoordinate;
begin
  Map.FreeMinimap;
  setLength(Map.img,Map.sizez);
  for i := 0 to Map.SizeZ-1 do begin
    Map.img[i] := TRGBAlphaImage.create;
    Map.img[i].setsize((map.sizex)*16,(map.sizey)*16,1);
    Map.img[i].Clear(Vector4Byte(0,0,0,0));
  end;
  for i := 0 to currentStep do
    for iz := 0 to tiles[Gen[i].tile].sizez-1 do begin
      if not tiles[Gen[i].tile].blocker then begin
        tiles[Gen[i].tile].img[iz].DrawTo(Map.img[iz+Gen[i].z], Gen[i].x*16,
             (Map.sizey-Gen[i].y-tiles[Gen[i].tile].sizey)*16, dmBlendSmart);
      end else begin
        tiles[Gen[i].tile].img[iz].DrawTo(Map.img[iz+Gen[i].z], (Gen[i].x+Tiles[Gen[i].tile].b_x)*16,
             (Map.sizey-(Gen[i].y+Tiles[Gen[i].tile].b_y)-tiles[Gen[i].tile].sizey)*16, dmBlendSmart);
      end;
    end;
  writeLnLog('DDungeonGenerator.MakeMinimap',inttostr(length(Map.img)));
end;

{-------------------------------------------------------------------------}

procedure DDungeonGenerator.ShrinkMap;
{var ix,iy,iz: TIntCoordinate;
    maxx,maxy,maxz: TIntCoordinate;
    minx,miny,minz: TIntCoordinate;}
begin
  {only z-resize now. Maybe I won't make xy-resizes
   due to possible blockers problems
   (actually not a problem, but requires some work)}
  if map.maxDepth+1<map.sizez then begin
    map.sizez := map.maxDepth+1;
    map.setsize(map.sizex,map.sizey,map.sizez);
  end;
end;

{================== 3D DUNGEON GENERATOR ROUTINES ===========================}

function D3DDungeonGenerator.ZeroIntegerMap: TIntMapArray;
var ix,iy,iz: TIntCoordinate;
begin
  setLength(Result,Map.sizex);
  for ix := 0 to map.sizex-1 do begin
    setLength(Result[ix],map.sizey);
    for iy := 0 to map.sizey-1 do begin
      setLength(Result[ix,iy],map.sizez);
      for iz := 0 to map.sizez-1 do
        Result[ix,iy,iz] := -1;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function D3DDungeonGenerator.NilIndexMap: TNeighboursMapArray;
var ix,iy{,iz}: TIntCoordinate;
begin
  setLength(Result,Map.sizex);
  for ix := 0 to map.sizex-1 do begin
    setLength(Result[ix],map.sizey);
    for iy := 0 to map.sizey-1 do begin
      setLength(Result[ix,iy],map.sizez);
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.MakeTileIndexMap;
var i: integer;
    ix,iy,iz: TIntCoordinate;
begin
  TileIndexMap := ZeroIntegerMap;
  for i := 0 to maxsteps-1 do with Tiles[Gen[i].tile] do
    if not blocker then // we don't count blockers here, they are not normal tiles :) We'll have to add them later
    for ix := 0 to sizex-1 do
      for iy := 0 to sizey-1 do
        for iz := 0 to sizez-1 do
          if map[ix,iy,iz].base <> tkNone then
            TileIndexMap[ix+gen[i].x,iy+gen[i].y,iz+gen[i].z] := i;
end;

{----------------------------------------------------------------------------}

function D3DDungeonGenerator.Ray(x1,y1,z1,x2,y2,z2: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var //anglex,angley,anglez: TAngle;
    vx,vy,vz,a: float;
    ix,iy,iz: integer;
begin
  Result := false;

  {sorry for my horrible discrete math :(
   But this was the only way I could do the raycasting efficiently
   anybody is welcome to improve!}

  {define the parametric vector}
  vx := x2-x1;
  vy := y2-y1;
  vz := z2-z1;

  {raycat all the faces can be affected
   we don't care about checking faces sequentially.
   The "longest" the algorithm will work for all-open faces
   and will give up as soon as it encounters at least one blocker faces
   with result "false"}

  if x2>x1 then
    for ix := trunc(x1) to trunc(x2)-1 do begin
      a := (ix-x1+1)/vx;
      iy := trunc(y1+a*vy);
      iz := trunc(z1+a*vz);
      if not IsLookable(Map.map[ix,iy,iz].faces[aRight]) then exit;
    end
  else
    for ix := trunc(x2) to trunc(x1)-1 do begin
      a := (ix-x1+1)/vx;
      iy := trunc(y1+a*vy);
      iz := trunc(z1+a*vz);
      if not IsLookable(Map.map[ix,iy,iz].faces[aLeft]) then exit;
    end;
  if y2>y1 then
    for iy := trunc(y1) to trunc(y2)-1 do begin
      a := (iy-y1+1)/vy;
      ix := trunc(x1+a*vx);
      iz := trunc(z1+a*vz);
      if not IsLookable(Map.map[ix,iy,iz].faces[aTop]) then exit;
    end
  else
    for iy := trunc(y2) to trunc(y1)-1 do begin
      a := (iy-y1+1)/vy;
      ix := trunc(x1+a*vx);
      iz := trunc(z1+a*vz);
      if not IsLookable(Map.map[ix,iy,iz].faces[aBottom]) then exit;
    end;
  if z2>z1 then
    for iz := trunc(z1) to trunc(z2)-1 do begin
      a := (iz-z1+1)/vz;
      ix := trunc(x1+a*vx);
      iy := trunc(y1+a*vy);
      if not IsLookable(Map.map[ix,iy,iz].faces[aDown]) then exit;
    end
  else
    for iz := trunc(z2) to trunc(z1)-1 do begin
      a := (iz-z1+1)/vz;
      ix := trunc(x1+a*vx);
      iy := trunc(y1+a*vy);
      if not IsLookable(Map.map[ix,iy,iz].faces[aUp]) then exit;
    end;

  Result := true;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.RaycastTile(mx,my,mz: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var
    HelperMap: TIntMapArray;
    raycastList,OldList: TRaycastList;
    j: integer;
    nx,ny,nz: integer;
    raycount,raytrue: integer;
    Neighbour: DNeighbour;

    {safely set a tile candidate}
    procedure SetCandidate(x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    var NewCandidate: Txyz;
    begin
      {looks redundant as "open face" can't go into void
       but let it be here for now}
      if (x>=0) and (y>=0) and (z>=0) and
         (x<Map.Sizex) and (y<Map.Sizey) and (z<Map.SizeZ) and
         (HelperMap[x,y,z]=0) then
      begin
        //HelperMap[x,y,z] := CandidateIndex;
        NewCandidate.x := x;
        NewCandidate.y := y;
        NewCandidate.z := z;
        raycastList.Add(NewCandidate);
      end;
    end;
    {advance a next step of candidate tiles}
    procedure GrowHelperMap; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    var i: integer;
        a: TAngle;
    begin
      {$Hint floor/ceiling problems? Keep your eye on it!}
      for i := 0 to OldList.Count-1 do
        if HelperMap[OldList[i].x,OldList[i].y,OldList[i].z]>0 then
          for a in TAngle do
            if isLookable(Map.Map[OldList[i].x,OldList[i].y,OldList[i].z].faces[a]) then
              SetCandidate(OldList[i].x+a_dx(a),OldList[i].y+a_dy(a),OldList[i].z+a_dz(a));
    end;
    {raycast 8 corners of one tile to another}
    {procedure RayCastCorners(cand: integer);
    begin
      //not available yet!
      for dx:=0 to 1 do
       for dy:=0 to 1 do
        for dz:=0 to 1 do begin
          if dx=0 then x0:=ix+raycast_corner_accuracy*(random+0.1) else x0:=ix+1-raycast_corner_accuracy*(random+0.1);
          if dy=0 then y0:=iy+raycast_corner_accuracy*(random+0.1) else y0:=iy+1-raycast_corner_accuracy*(random+0.1);
          if dz=0 then z0:=iz+raycast_corner_accuracy*(random+0.1) else z0:=iz+1-raycast_corner_accuracy*(random+0.1);
        end;
    end;}
begin
  WriteLnLog(inttostr(mx)+inttostr(my)+inttostr(mz),'xxx');

  //init HelperMap and raycast list
  RaycastList := TRaycastList.Create;
  OldList := nil;
  HelperMap := ZeroIntegerMap;
  SetCandidate(mx,my,mz); //seed for the grow of the map
  HelperMap[mx,my,mz] := MaxNeighboursIndex; //and already define it.

  repeat
    //re-init all the variables, grow the map
    FreeAndNil(OldList);
    OldList := RaycastList;
    RaycastList := TRaycastList.create;
    //get candidates
    GrowHelperMap;

    //raycast mx,my,mz -> RaycastList[i] (candidates)
    for j := 0 to RaycastList.count-1 do begin
      RayCount := 0;
      RayTrue := 0;

      //raycast 8 corners (an optimization trick) 8*8=64=CornerCount
      //not done yet
      //RayCastCorners(j);

      //Monte-Carlo raycast;
      repeat
        inc(RayCount);
        if Ray(mx              +RNDM.Random,my              +RNDM.Random,mz              +RNDM.Random,
               RayCastList[j].x+RNDM.Random,RayCastList[j].y+RNDM.Random,RayCastList[j].z+RNDM.Random)
        then inc(RayTrue);
      until ((RayTrue >= RayCount div 2) or (RayCount >= MaxNeighboursIndex)) and (rayCount>CornerCount);

      if RayTrue>0 then
        HelperMap[RayCastList[j].x,RayCastList[j].y,RayCastList[j].z] := MaxNeighboursIndex * RayTrue div RayCount
      else
        HelperMap[RayCastList[j].x,RayCastList[j].y,RayCastList[j].z] := FailedIndex;

      //if RayTrue>0 then writeLnLog(inttostr(mx)+','+inttostr(my)+','+inttostr(mz)+' - '+inttostr(round(100*RayTrue/RayCount)));
    end;
  until RaycastList.Count=0;

  //convert HelperMap to NeighbourList

  tmpNeighboursMap[mx,my,mz] := TNeighboursList.create;
  //make a list of neighbours
  for nx := 0 to Map.sizex-1 do
    for ny := 0 to map.sizey-1 do
      for nz := 0 to map.sizez-1 do if HelperMap[nx,ny,nz]>0 then begin
        Neighbour.tile := TileIndexMap[nx,ny,nz];
        Neighbour.visible := HelperMap[nx,ny,nz];
        tmpNeighboursMap[mx,my,mz].add(Neighbour);

        //Add blockers
        //THIS IS UGLY AND INEFFICIENT both on CPU and RAM!!!
        for j := 0 to maxsteps-1 do if Tiles[Gen[j].tile].blocker then
          {warning! This works only for 1x1x1 blockers with only ONE open face}
          if (gen[j].x+Tiles[Gen[j].tile].b_x=nx) and (gen[j].y+Tiles[Gen[j].tile].b_y=ny) and (gen[j].z+Tiles[Gen[j].tile].b_z=nz) then begin
            Neighbour.tile := j;
            Neighbour.visible := HelperMap[nx,ny,nz];
            tmpNeighboursMap[mx,my,mz].add(Neighbour);
          end;
      end;

  RemoveDuplicatesNeighbours(tmpNeighboursMap[mx,my,mz]);

  FreeAndNil(OldList);
  FreeAndNil(RaycastList);
end;

{----------------------------------------------------------------------------}

function CompareNeighbours(const i1,i2: DNeighbour): integer;
begin
  result := i1.tile - i2.tile;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.RemoveDuplicatesNeighbours(var List: TNeighboursList);
var i: integer;
begin
  if list.count <= 1 then exit;
  List.sort(@CompareNeighbours);
  i := 0;
  repeat
    if List[i].tile = List[i+1].tile then begin
      {duplicate found, choose the largest value and go delete one duplicate}
      if List[i].visible < list[i+1].visible then
        list.Delete(i)
      else
        List.Delete(i+1);
      {"The argument cannot be assigned to" - are you joking???
       Really nasty thing about those generic lists! Should keep that in mind
       P.S. My bad. use List.L[I].Value. See:
       https://github.com/castle-engine/castle-engine/issues/63#issuecomment-292794723,
       Thanks, Michalis!}
    end
    else inc(i);
  until i >= List.count-1;
end;

{-----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.Neighbours_of_neighbours;
var i: integer;
    ix,iy,iz: TIntCoordinate;
    dx,dy,dz: TIntCoordinate;
begin
  NeighboursMap := NilIndexMap;

  writelnLog('D3DDungeonGenerator.Neighbours_of_neighbours','Merging neighbours of neighbours...');
  for ix := 0 to Map.sizex-1 do
    for iy := 0 to Map.sizey-1 do
      for iz := 0 to Map.sizez-1 do if tmpNeighboursMap[ix,iy,iz]<>nil then begin
        //writeLnLog('neighbours',inttostr(tmpNeighboursMap[ix,iy,iz].count));
        //process neighbours
 {       for dx := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do
          for dy := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do
            for dz := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do {***};
 }
        {$warning not working yet}
        NeighboursMap[ix,iy,iz] := TNeighboursList.create;
        for i := 0 to tmpNeighboursMap[ix,iy,iz].count-1 do
          NeighboursMap[ix,iy,iz].add(tmpNeighboursMap[ix,iy,iz].L[i]);
        RemoveDuplicatesNeighbours(NeighboursMap[ix,iy,iz]);
      end;
end;

{-----------------------------------------------------------------------}

{just a scaling coefficient to avoid float numbers}
procedure D3DDungeonGenerator.Raycast;
var ix,iy,iz: TIntCoordinate;
begin
 tmpNeighboursMap := NilIndexMap; //create a nil-initialized neighbours lists of all accessible map tiles

 {here we fill in the neighbours}
 for ix := 0 to Map.sizex-1 do
   for iy := 0 to Map.sizey-1 do
     for iz := 0 to Map.sizez-1 do if TileIndexMap[ix,iy,iz]>=0 {and is accessible} then
       RaycastTile(ix,iy,iz);

 Neighbours_of_neighbours;

 //and free temporary map
 FreeNeighboursMap(tmpNeighboursMap);
end;

{------------------------------------------------------------------------}

procedure D3DDungeonGenerator.FreeNeighboursMap(var nmap: TNeighboursMapArray);
var ix,iy,iz: TIntCoordinate;
begin
  if nmap = nil then exit;
  //just to be safe (in case the nmap was not initialized completely)
  if length(nmap) = 0 then exit;

  //free and nil all the elements
  for ix := 0 to length(nmap)-1 do
    for iy := 0 to length(nmap[ix])-1 do
      for iz := 0 to length(nmap[ix,iy])-1 do
        freeAndNil(nmap[ix,iy,iz]);

  nmap := nil; //this will automatically free the array
end;

{------------------------------------------------------------------------}

{can't make it nested because TCompareFunction cannot accept "is nested"}
type DIndexRec = record
  index: integer;
  hits: integer;
end;
type HitList = specialize TGenericStructList<DIndexRec>;
function CompareHits(const i1,i2: DIndexRec): integer;
begin
  result := i1.hits - i2.hits;
end;
procedure D3DDungeonGenerator.Chunk_N_Slice;
var i,j,g: integer;
    ix,iy,iz: integer;
    hit_count: HitList;
    tIndex: DIndexRec;
    tilesused: array of boolean;
begin
  {this part of the code will combine tiles in larger groups to boost FPS
  also this will provide for LOD of the whole group generation and far land support for overworld later
  I'm not yet sure how textures will behave... but let's leave this question for later}

  //prepare to count hits
  hit_count := HitList.create;
  for i := 0 to high(Gen) do begin
    tIndex.index := i;
    tIndex.hits := 0;
    hit_count.Add(tIndex);
  end;
  {first we just count how much time a tile is 'hit' by neighbours
  Therefore we find "more popular" tiles which will be 'seeds' for groups}
  for ix := 0 to Map.sizex-1 do
    for iy := 0 to Map.sizey-1 do
      for iz := 0 to Map.sizez-1 do if NeighboursMap[ix,iy,iz]<>nil then
        for i := 0 to NeighboursMap[ix,iy,iz].count-1 do
          inc(Hit_count.L[NeighboursMap[ix,iy,iz].L[i].Tile].hits);
  Hit_count.Sort(@CompareHits);

  {now let's start the main algorithm}
  g := 0;
  setlength(tilesUsed,length(Gen));
  for j := 0 to high(tilesused) do tilesUsed[i] := false;
  i := 0;
  repeat
    setLength(Groups,g+1);
    Groups[g] := TIndexList.create;
    {here we find the tile with minimal amount (>0) of currently visible neighbours,
    it produces smoother amount of members in a group
    versus less groups in case maximum is used
    Roughly, amount of groups will be ~sqrt(tiles) However, it's not as simple as it might seem}

    //skip sorted list to first unused tile
    while (i <= high(tilesUsed)) and (tilesUsed[Hit_count[i].index]) do inc(i);

    if (i <= high(tilesUsed)){ and (not tilesUsed[Hit_count[i].index]) } then begin
      {we don't actually need to scan the whole tile, only top-left element,
       if we'll miss something, we'll just add missed tiles to other groups later}
      ix := Gen[Hit_count[i].index].x;
      iy := Gen[Hit_count[i].index].y;
      iz := Gen[Hit_count[i].index].z;
      //blockers behave a bit differently
      if Tiles[Gen[Hit_count[i].index].tile].blocker then begin
        ix += Tiles[Gen[Hit_count[i].index].tile].b_x;
        iy += Tiles[Gen[Hit_count[i].index].tile].b_y;
        iz += Tiles[Gen[Hit_count[i].index].tile].b_z;
      end;
      {we're sure neighbours list is not nil!}
      if NeighboursMap[ix,iy,iz]=nil then raise exception.create('NeighboursMap is nil!');
      for j := 0 to NeighboursMap[ix,iy,iz].count do
        if not tilesUsed[Hit_count[i].index] then begin
          tilesUsed[Hit_count[i].index] := true;
          Groups[g].add(Hit_count[i].index);
        end;
    end;
    inc(g);
  until i >= high(tilesUsed);
  writeLnLog('D3DDungeonGenerator.Chunk_N_Slice','N groups = '+inttostr(length(groups)));
  freeAndNil(Hit_count);
end;

{------------------------------------------------------------------------}

procedure D3DDungeonGenerator.Generate;
var t: TDateTime;
begin
  //make the logic map
  inherited Generate;

  //raycast
  writeLnLog('D3DDungeonGenerator.Generate','Raycasting started...');
  t := now;

  MakeTileIndexMap;
  Raycast;
  //TileIndexMap := nil;  //this will free the array -- I'll need it later for debugging?!

  writeLnLog('D3DDungeonGenerator.Generate','Raycasting finished in '+inttostr(round((now-t)*24*60*60*1000))+'ms...');
  Chunk_N_Slice;
end;

{========================== DGENERATOR TILE ================================}

{we're making two almost identical copies of the procedure to
 optimize things for DGenerationMap}
{$DEFINE CompleteGen} //this will separate implementations for Map and Tile
function DGeneratorTile.CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$INCLUDE decodungeongenerator_calculatefaces.inc}

{$UNDEF CompleteGen}
function DGeneratorMap.CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$INCLUDE decodungeongenerator_calculatefaces.inc}

{--------------------------------------------------------------------------}

procedure DGeneratorMap.ProcessBlockers;
var a: TAngle;
begin
  if blocker then begin
    {this works only for 1x1x1 blockers with only ONE face and only HORIZONTAL!}
    for a in THorizontalAngle do
      if isPassable(Map[0,0,0].faces[a]) then begin
        b_x := a_dx(a);
        b_y := a_dy(a);
      end;
  end;
end;

{--------------------------------------------------------------------------}

destructor DGeneratorMap.destroy;
begin
  FreeAndNil(Dock);
  inherited;
end;

{========================= OTHER ROUTINES ===================================}

procedure DDungeonGenerator.Load(filename: string);
begin
  {$warning critical todo in DDungeonGenerator.Load}
  {initialize generator parameters}

  fisReady := true;
end;

{-----------------------------------------------------------------------------}

{Function DDungeonGenerator.GetTileByName(TileName: string): DGeneratorMap;
var I: DGeneratorMap;
begin
  Result := nil;
  for I in Tiles do if I.TileName = TileName then begin
    Result := I;
    exit;
  end;
  raise Exception.create('DDungeonGenerator.GetTileByName: FATAL! Tile cannot be found!');
end;}
Function DDungeonGenerator.GetTileByName(TileName: string): TTileType;
var i: integer;
begin
  for i := 0 to Tiles.count do if tiles[i].TileName = TileName then begin
    Result := i;
    exit;
  end;
  raise Exception.create('DDungeonGenerator.GetTileByName: FATAL! Tile cannot be found!');
end;

{-----------------------------------------------------------------------------}

constructor DGeneratorParameters.create;
begin
  inherited;
  TilesList := TStringList.create;
  FirstSteps := TFirstStepsArray.create;
end;

{-----------------------------------------------------------------------------}

constructor DDungeonGenerator.create;
begin
  inherited;
  Map := DGeneratorMap.create;
  Tiles := TTileList.Create(true);
  //parameters := DGeneratorParameters.create;

  NormalTiles := TIndexList.create;
  BlockerTiles := TIndexList.create;
  DownTiles := TIndexList.create;
  RichTiles := TIndexList.create;
  PoorTiles := TIndexList.create;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.FreeLists;
begin
  FreeAndNil(NormalTiles);
  FreeAndNil(PoorTiles);
  FreeAndNil(BlockerTiles);
  FreeAndNil(RichTiles);
  FreeAndNil(DownTiles);
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(Map);
  FreeAndNil(tiles);
  FreeAndNil(parameters);
  FreeLists;
  inherited;
end;

{-----------------------------------------------------------------------------}

destructor DGeneratorParameters.destroy;
begin
  FreeAndNil(TilesList);
  FreeAndNil(FirstSteps);

  inherited;
end;

{-----------------------------------------------------------------------------}

destructor D3DDungeonGenerator.Destroy;
var i: integer;
begin
  FreeNeighboursMap(tmpNeighboursMap);
  FreeNeighboursMap(NeighboursMap);
  if groups<>nil then
    for i := 0 to high(groups) do
      FreeAndNil(Groups[i]);
  inherited;
end;

end.

