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
unit DecoDungeonGenerator;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, CastleRandom, fgl, CastleGenericLists,
  DecoAbstractGenerator, DecoDungeonTiles,
  DecoNavigationNetwork,
  DecoThread, DecoGlobal;

type TIntMapArray = array of array of array of integer;

type
  {a "dock point" of a tile or a map. This is a xyz coordinate of an open face
   we use it to dock the tile to the map -
   i.e. we "dock" two dock points together if they fit each other like in puzzles}
  DDockPoint = record
    {coordinates of the dock point (relative to parent)}
    x,y,z: TIntCoordinate;
    {face where dock point is open. Each dock point contains one face.
     if there are two (or more) exits from a tile element - it spawns two (or more) dock points}
    Face: TAngle;
    {the face type of the dock point. For faster checking}
    FaceType: TTileFace;
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

      destructor Destroy; override;
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
type TGeneratorTileList = specialize TFPGObjectList<DGeneratorTile>;

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
    Tile: string;
    {coordinates to place the tile}
    x,y,z: TIntCoordinate;
  end;
  TFirstStepsArray = specialize TGenericStructList<DFirstStep>;

type
  {a set of map generation parameters}
  DDungeonGeneratorParameters = class(DAbstractGeneratorParameters)
    public
      {the map cannot be larger than these}
      MaxX,MaxY,MaxZ: TIntCoordinate;
      {the map cannot be smaller than these}
      MinX,MinY,MinZ: TIntCoordinate;
      {target map volume. The map will be regenerated until it's met
       Usually the algorithm can handle up to ~30% packaging of maxx*maxy*maxz}
      Volume: integer;
      {when the map has FreeFaces>MaxFaces it will try to "Shrink" the amount
       of free faces by using poor tiles}
      MaxFaces: integer;
      {when the map has FreeFaces<MinFaces it will try to "extend" the amount
       of free faces by using rich tiles (in case the Volume is not yet met)}
      MinFaces: integer;
      {list of tiles}
      TilesList: TStringList;
      {list of first generation steps}
      FirstSteps: TFirstStepsArray;

      {loads and applies the map parameters}
      procedure Load(URL: string);

      constructor Create;
      destructor Destroy; override;
  end;

type
  { Preforms all map generation routines
    This is a relatively CPU-intensive work therefore must be heavily optimized }
  DDungeonGenerator = class(DAbstractGenerator)
  private
    {list of tiles used in current map}
    Tiles: TGeneratorTileList;
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
    MinSteps: integer;
    {current size of the dynamic array}
    MaxSteps: integer;
    {current (the last used) step of the generator;
     careful, currentStep starts from -1, while corresponding min/max steps start from 0
     so conversion is min/maxStep = currentStep+1}
    CurrentStep: integer;
    {adds +1 to CurrentStep and resizes the array if necessary}
    procedure IncCurrentStep; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
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
    function AddTile(Tile: TTileType; x,y,z: TIntCoordinate): boolean;
    {When we're sure what we are doing, we're not making any checks, just add the tile as fast as it is possible}
    procedure AddTileUnsafe(Tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {overloaded version that accepts a DGeneratorStep;}
    procedure AddTileUnsafe(Step: DGeneratorStep);
    {creates a minimap as Map.img}
    procedure MakeMinimap;
    {resizes the generated map for its real size}
    procedure ShrinkMap;
  private
    {temporary, maybe it can be made local in BuildNav, but it's also required
     for building weenies at the moment}
    NavMap: TIntMapArray;
    procedure BuildNav;
    procedure BuildWeenies;
  public
    {map parameters}
    Parameters: DDungeonGeneratorParameters;
    {this forces isReady to true. Must be used only in constructor which skips
     loading of the map}
    procedure ForceReady;
    {initializes generation parameters and loads everything}
    procedure InitParameters; override;
    { the main procedure to generate a dungeon,
      may be directly launched in main thread (for testing or other purposes),
      automaticlaly called on thread.execute
      WARNING: if map parameters are not valid the Generate procedure
      might and *will* hang forever.
      We need to do something about this,
      however I'm not sure how exactly this issue may be solved without
      *ruining* the game by producing an non-complete map (e.g. lacking a critical area)
      other than always rpoviding valid initialization parameters
      (i.e. such map parameters that the map *can* be generated)
      So, for now: ALWAYS test the map a few times in the map editor}
    procedure Generate; override;

    constructor Create; override;
    destructor Destroy; override;
    procedure FreeLists;
    //save temp state to file ---- unneeded for now
  public
    {EXPORT routines. They create a *copy* of internal generator data
     to respond to external request.
     This is made to keep the generator clean and self-contained.
     HOWEVER, this is not optimal, as requires nearly *double* RAM during the
     export procedures.
     makes a *copy* of the "internal" map to external request
     it's "lower-level" than used DGenerationMap so useful :)}
    function ExportMap: DMap; //maybe better saveTo when finished?: DMap;
    function ExportTiles: TStringList; override;
    {nils GEN link!}
    function ExportSteps: TGeneratorStepsArray;
end;

type
  {coordinate of a raycast_to candidate}
  Txyz = record
    x,y,z: TIntCoordinate;
  end;
type TRaycastList = specialize TGenericStructList<Txyz>;

type TNeighboursMapArray = array of array of array of TNeighboursList;

{$HINT needs to be changed to fit the abstract world definition.}
type TGroupsArray = array of TIndexList;

type
  {this is a Dungeon Generator with additional 3D world generation,
   and linked stuff like raycast and chunk-n-slice the dungeon into parts}
  D3DDungeonGenerator = class(DDungeonGenerator)
  private
    {how accurate will be determination of the "visible tile or not"
     64 is the basic number (cornerCount), actually 128 should be enough}
    const MaxNeighboursIndex = 128;
    const FailedIndex = -1000;
    //const CandidateIndex = -10;
    const CornerCount = 8*8;
  private
    {a map that stores tiles markers for quick access...
     maybe not needed?}
    TileIndexMap: TIntMapArray;
    {returns an integer array of (map.sizex,map.sizey,map.sizez) size}
    //function ZeroIntegerMap: TIntMapArray;
    {puts tile # markers on a map to detect which tile is here}
    procedure MakeTileIndexMap;
  private
    RaycastCount: integer;
    {temporary map for first-order neighbours lists}
    tmpNeighboursMap: TNeighboursMapArray;
    {final neighbours map}
    NeighboursMap: TNeighboursMapArray;
    {groups of tiles (used to chunk the dungeon)}
    Groups: TGroupsArray;
    {merge the neighbours of neighbours in order for the light to work smoothly
     todo!!!}
    procedure Neighbours_of_neighbours;
    {initializes a neighbours map with nils}
    function NilIndexMap: TNeighboursMapArray;
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

    destructor Destroy; override;
  public
    {WARNING, exporting these WILL stop the Generator from freeing it
     and will NIL the corresponding links. Don't try to access it twice!}
    {exports groups of tiles and nils the link}
    function ExportGroups: TIndexGroups; override;
    {exports neighbours map and nils the link}
    function ExportNeighbours: TNeighboursMapArray;
  end;

{free every element of a Neigobours map}
procedure FreeNeighboursMap(var nmap: TNeighboursMapArray);
{free every element of a groups array}
procedure FreeGroups(ngroups: TGroupsArray);
{returns an integer array of (map.sizex,map.sizey,map.sizez) size}
function ZeroIntegerMap(sx,sy,sz: integer): TIntMapArray;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleFilesUtils, CastleImages, CastleVectors,
  DOM, CastleXMLUtils,
  DecoInputOutput, DecoTime, DecoLog;

{========================= GENERATION ALGORITHM ==============================}

procedure DDungeonGenerator.ForceReady;
begin
  Parameters.isReady := true;
  dLog(LogWorldInitSoftError,Self,'DDungeonGenerator.ForceReady','Warning: Be careful, parameters might not be initialized correctly.');
end;

{----------------------------------------------------------------------------}

procedure DDungeonGenerator.InitParameters;
var tmp: DGeneratorTile;
    i : integer;
begin
  if not Parameters.isReady then
    raise Exception.Create('DDungeonGenerator.Generate FATAL - parameters are not loaded!');
  {load tiles}
  Tiles.Clear;

  UpdateProgress('Loading tiles',0);

  For i := 0 to Parameters.TilesList.Count-1 do begin
    if Parameters.AbsoluteURL then
      tmp := DGeneratorTile.Load(Parameters.TilesList[i],false)
    else
      tmp := DGeneratorTile.Load(ApplicationData(TilesFolder+Parameters.TilesList[i]),true);
    tmp.CalculateFaces;
    tmp.ProcessBlockers;
    Tiles.Add(tmp);
    UpdateProgress('Loading tiles',0.3*i/(Parameters.TilesList.Count-1));
  end;

  UpdateProgress('Processing tiles',0.3);

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
  InitSeed(Parameters.seed);

  {initialize map size}
  Map.SetSize(Parameters.MaxX,Parameters.MaxY,Parameters.MaxZ); //we don't care about RAM now, we'll shrink the map later

  {load pregenerated tiles}
  MaxSteps := 0;
  SetLength(Gen, maxSteps);
  CurrentStep := -1;

  for i := 0 to Parameters.FirstSteps.Count-1 do begin
    incCurrentStep;
    Gen[CurrentStep].tile := GetTileByName(Parameters.FirstSteps[i].Tile);
    Gen[CurrentStep].x := Parameters.FirstSteps[i].x;
    Gen[CurrentStep].y := Parameters.FirstSteps[i].y;
    Gen[CurrentStep].z := Parameters.FirstSteps[i].z;
  end;

  MinSteps := CurrentStep+1;
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
    Result := (InvertAngle(Tiles[t].Dock[td].Face) = Map.Dock[d].Face) and
               (Tiles[t].Dock[td].FaceType = Map.Dock[d].FaceType)
  end;
  function TryAdd: boolean;
  begin
    Result :=
      AddTile(t, Map.Dock[d].x - Tiles[t].Dock[td].x + a_dx(Map.Dock[d].Face),
                 Map.Dock[d].y - Tiles[t].Dock[td].y + a_dy(Map.Dock[d].Face),
                 Map.Dock[d].z - Tiles[t].Dock[td].z + a_dz(Map.Dock[d].Face))
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
        if (Map.MaxDepth < parameters.MinZ*(Map.Volume/Parameters.Volume)) then
          t := GetDownTile
        else
          t := GetNormalTile;
      end else
        t := GetNormalTile;

      td := RNDM.Random(Tiles[t].Dock.Count);
    until CanDock;
    Success := TryAdd;
    inc(TriesCount);
  until Success or (TriesCount>NormalTilesTotalDocks);

  if not Success then begin
    {if we've failed random search next we try sequential search}
    for tt := 0 to NormalTiles.Count-1 do begin
      t := NormalTiles[tt];
      for td := 0 to Tiles[t].Dock.Count-1 do
        if canDock then
          if TryAdd then Exit;
    end;
    {if we haven't "exited" yet, then there is no tile that fits this
     map's dock point. We have to block it out}
    for tt := 0 to BlockerTiles.Count-1 do begin
      t := BlockerTiles[tt];
      for td := 0 to Tiles[t].dock.Count-1 do //actually it should be always just one
        if CanDock then
          if TryAdd then Exit;
    end;
    {finally, if we can't even block the face out... it's horrible :(
     The only alternative to just hanging forever up is...}
    dLog(LogError,Self,'BlockerTiles.Count',IntToStr(BlockerTiles.Count));
    dLog(LogError,Self,'x=',IntToStr(Map.Dock[d].x));
    dLog(LogError,Self,'y=',IntToStr(Map.Dock[d].y));
    dLog(LogError,Self,'z=',IntToStr(Map.Dock[d].z));
    dLog(LogError,Self,'face=',IntToStr(Map.Dock[d].FaceType));
    dLog(LogError,Self,'at ',AngleToStr(Map.Dock[d].Face));
    raise Exception.Create('DDungeonGenerator.AddRandomTile: FATAL! Unable to block the map element.')
  end;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.Generate;
var i: integer;
    t1,t2: DTime;
begin
  if Self is DDungeonGenerator then fMult := 1 else fMult := 2;
  fProgress := 0;
  UpdateProgress('Initialize',0);

  if not Parameters.isReady then
    raise Exception.Create('DDungeonGenerator.Generate FATAL - parameters are not loaded!');
  if not isInitialized then begin
    dLog(LogWorldInitSoftError,Self,'DDungeonGenerator.Generate','Warning: parameters were automatically initialized! It''s best to initialize parameters manually outside the thread.');
    InitParameters;
  end;
  if isWorking then begin
    dLog(LogWorldError,Self,'DDungeonGenerator.Generate','WARNING: Generation thread is buisy! Aborting...');
    Exit;
  end;
  fisWorking := true;

  t1 := GetNow;
  repeat
    t2 := GetNow;
    Map.EmptyMap(false);
    //add prgenerated or undo tiles
    CurrentStep := RNDM.random(CurrentStep);
    if CurrentStep < MinSteps-1 then CurrentStep := MinSteps-1;
    dLog(LogGenerateWorld,Self,'DDungeonGenerator.Generate','Starting from '+inttostr(CurrentStep));
    for i := 0 to CurrentStep do AddTileUnsafe(Gen[i]);

    while Map.CalculateFaces<>0 do begin
      {dLog(inttostr(Map.dock.count));}
      //add a tile
      AddRandomTile;
      UpdateProgress('Generating',Minimum(Map.Volume/Parameters.Volume,0.90));
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
    dLog(LogGenerateWorld,Self,'DDungeonGenerator.Generate','Done in = '+IntToStr(Round((GetNow-t2)*1000))+'ms');
    dLog(LogGenerateWorld,Self,'DDungeonGenerator.Generate','Map volume = '+IntToStr(Map.Volume) +'/'+IntToStr(Parameters.Volume));
    dLog(LogGenerateWorld,Self,'DDungeonGenerator.Generate','Max depth = '+IntToStr(Map.MaxDepth+1)+'/'+IntToStr(Parameters.MinZ));
  until (Map.Volume>=Parameters.Volume) and (Map.MaxDepth+1>=Parameters.MinZ); {until map meets the paramters}
  {$WARNING may hang up forever here, if paremeters cannot be satisfied}

  UpdateProgress('Finalizing',0.95);
  //finally resize the dynamic array
  MaxSteps := CurrentStep+1;
  SetLength(Gen,MaxSteps);
  dLog(LogGenerateWorld,Self,'DDungeonGenerator.Generate','Job finished in = '+IntToStr(Round((GetNow-t1)*1000))+'ms');

  // finalize
  FreeLists; //we no longer need them

  ShrinkMap;

  BuildNav; //build navigation network for the dungeon
  BuildWeenies;

  MakeMinimap;

  fisWorking := false;
  fisFinished := true;
  if Self is DDungeonGenerator then UpdateProgress('Done',1);
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.ExportMap: DMap;
var ix,iy,iz: TIntCoordinate;
begin
  if not isFinished then raise Exception.Create('DDungeonGenerator.ExportMap: ERROR! Trying to access an unfinished Generator');
  {Result := Map;
  Map := nil;}
  Result := DMap.Create;
  Result.SetSize(Map.SizeX,Map.SizeY,Map.SizeZ);
  for ix := 0 to Map.SizeX-1 do
    for iy := 0 to Map.SizeY-1 do
      for iz := 0 to Map.SizeZ-1 do
        Result.Map[ix,iy,iz] := Map.Map[ix,iy,iz];
  SetLength(Result.Img,Length(Map.Img));
  for iz := 0 to Map.SizeZ-1 do
    Result.Img[iz] := Map.Img[iz].MakeCopy as TRGBAlphaImage;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.ExportTiles: TStringList;
var s: string;
begin
  Result := TStringList.Create;
  for s in Self.Parameters.TilesList do
    Result.Add(ApplicationData(TilesFolder+s+'.x3d'+GZ_ext));
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.ExportSteps: TGeneratorStepsArray;
begin
  Result := Gen;
  Gen := nil;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.ResizeSteps; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if CurrentStep+1>=MaxSteps then begin
    inc(MaxSteps,GeneratorShift);
    SetLength(Gen, MaxSteps);
  end
end;
procedure DDungeonGenerator.incCurrentStep; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  inc(CurrentStep);
  ResizeSteps;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.AddTile(Tile: TTileType; x,y,z: TIntCoordinate): boolean;
var jx,jy,jz: TIntCoordinate;
begin
  Result := false;
  //check all tiles against map area they are placed to
  for jx := 0 to Tiles[Tile].SizeX-1 do
   for jy := 0 to Tiles[Tile].SizeY-1 do
    for jz := 0 to Tiles[Tile].SizeZ-1 do
      if not CheckCompatibility(Tiles[Tile].Map[jx,jy,jz],x+jx,y+jy,z+jz)
      then Exit;

  AddTileUnsafe(Tiles[Tile],x,y,z);
  {add current step to GEN}
  incCurrentStep;
  Gen[CurrentStep].Tile := Tile;
  Gen[CurrentStep].x := x;
  Gen[CurrentStep].y := y;
  Gen[CurrentStep].z := z;

  Result := true;
end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.AddTileUnsafe(Tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var jx,jy,jz: TIntCoordinate;
    a: TAngle;
begin
  for jx := 0 to Tile.SizeX-1 do
   for jy := 0 to Tile.SizeY-1 do
    for jz := 0 to Tile.SizeZ-1 do begin
      if Tile.Map[jx,jy,jz].Base <> tkNone then
        Map.Map[x+jx,y+jy,z+jz].Base := Tile.Map[jx,jy,jz].Base;
      for a in TAngle do if Tile.Map[jx,jy,jz].Faces[a] <> tfNone then
        if Tile.Blocker then begin
          Map.Map[x+jx,y+jy,z+jz].Faces[a] := tfWall;
          Map.Map[x+jx+a_dx(a),y+jy+a_dy(a),z+jz+a_dz(a)].Faces[InvertAngle(a)] := tfWall;
        end
        else
          Map.Map[x+jx,y+jy,z+jz].Faces[a] := Tile.Map[jx,jy,jz].Faces[a];
    end;
end;
procedure DDungeonGenerator.AddTileUnsafe(Step: DGeneratorStep);
begin
  AddTileUnsafe(Tiles[Step.Tile],Step.x,Step.y,Step.z);
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.CheckCompatibility(Tile: BasicTile; x,y,z: TIntCoordinate): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var TmpTile: BasicTile;
    a: TAngle;
begin
  Result := true;
  if Tile.Base <> tkNone then begin   // we can place an "empty" tile anywhere whether it's just empty or a blocker
    TmpTile := Map.MapSafe(x,y,z);
    {check if tile base fits the map}
    if TmpTile.Base <> tkNone then
      Result := false // if the "map" is occupied at (x,y,z) nothing but "empty" can be placed there
    else begin
      {check current tile faces fit the map}
      for a in TAngle do if (TmpTile.Faces[a] <> Tile.Faces[a]) and
                            (TmpTile.Faces[a] <> tfNone) then Result := false;
      if Result then begin
        {if current tile ok, then check adjacent tiles}
        {this algorithm may be significantly optimized by
         moving into AddTile where each tile
         would add not only it's basicTile, but also inverse angles at adjacent tiles
         however, not sure about how bug-free that'll be.}
        //horizontal angles -> full 6 angles
        for a in TAngle do begin
          TmpTile := Map.MapSafe(x+a_dx(a), y+a_dy(a), z+a_dz(a));
          if TmpTile.Base <> tkInacceptible then
            if (TmpTile.Faces[InvertAngle(a)] <> Tile.Faces[a]) and
               (TmpTile.Faces[invertAngle(a)] <> tfNone) then Result := false;
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
  setLength(Map.Img,Map.SizeZ);
  for i := 0 to Map.SizeZ-1 do begin
    Map.Img[i] := TRGBAlphaImage.Create;
    Map.Img[i].SetSize((Map.SizeX)*16,(Map.SizeY)*16,1);
    Map.Img[i].Clear(Vector4Byte(0,0,0,0));
  end;
  for i := 0 to CurrentStep do
    for iz := 0 to Tiles[Gen[i].Tile].SizeZ-1 do begin
      if not Tiles[Gen[i].Tile].Blocker then begin
        Tiles[Gen[i].Tile].Img[iz].DrawTo(Map.Img[iz+Gen[i].z], Gen[i].x*16,
             (Map.SizeY-Gen[i].y-Tiles[Gen[i].Tile].SizeY)*16, dmBlendSmart);
      end else begin
        Tiles[Gen[i].Tile].Img[iz].DrawTo(Map.Img[iz+Gen[i].z], (Gen[i].x+Tiles[Gen[i].Tile].b_x)*16,
             (Map.SizeY-(Gen[i].y+Tiles[Gen[i].Tile].b_y)-Tiles[Gen[i].Tile].SizeY)*16, dmBlendSmart);
      end;
    end;
  dLog(LogGenerateWorld,Self,'DDungeonGenerator.MakeMinimap',IntToStr(Length(Map.Img)));
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
  if Map.MaxDepth+1<Map.SizeZ then begin
    Map.SizeZ := Map.MaxDepth+1;
    Map.SetSize(Map.SizeX,Map.SizeY,Map.SizeZ);
  end;
end;

{-------------------------------------------------------------------------}

procedure DDungeonGenerator.BuildNav;
var ix,iy,iz: TIntCoordinate;
    tmpNav: DNavPt;
    aNav: TNavID;
    a: TAngle;
begin
  NavMap := ZeroIntegerMap(Map.SizeX,Map.SizeY,Map.SizeZ);

  NavList := TNavList.create;
  for iz := 0 to Map.SizeZ-1 do
    for ix := 0 to Map.SizeX-1 do
      for iy := 0 to Map.SizeZ-1 do if isPassable(Map.Map[ix,iy,iz].Base)
      and not isPassable(Map.Map[ix,iy,iz].Faces[aDown])
      and not isPassable(Map.Map[ix,iy,iz].Faces[aUp])         {$hint ignore up/down tiles for now}
        then begin
        tmpNav.Pos[0] := ix;
        tmpNav.Pos[1] := iy;
        tmpNav.Pos[2] := iz;
        tmpNav.Blocked := false;
        NavMap[ix,iy,iz] := NavList.Add(tmpNav);
      end;
  dLog(LogGenerateWorld,Self,'DDungeonGenerator.BuildNav','Navigation Graph created, nodes: '+IntToStr(NavList.Count));
  {build links between the nav points}
  for iz := 0 to Map.SizeZ-1 do
    for ix := 0 to Map.SizeX-1 do
      for iy := 0 to Map.SizeZ-1 do if NavMap[ix,iy,iz]>0 then with NavList.L[NavMap[ix,iy,iz]] do begin
        LinksCount := -1;
        {$hint ignore up/down tiles for now}
        for a in THorizontalAngle do if isPassable(Map.Map[ix,iy,iz].faces[a]) then begin
          aNav := NavMap[ix+a_dx(a),iy+a_dy(a),iz+a_dz(a)];
          if aNav>0 then begin
            inc(LinksCount);
            Links[LinksCount] := aNav;
          end;
        end;
      end;


end;

{-------------------------------------------------------------------------}

procedure DDungeonGenerator.BuildWeenies;
var w: DWeenie;
    i,j,k: integer;
begin
  {$hint All weenies actually will be defined by CONSTRUCTOR in first steps, not here!}
  Weenies := TWeeniesList.create;
  w.kind := wtEntrance;
  w.NavId := NavMap[Self.Gen[0].x,Self.Gen[0].y,Self.Gen[0].z];

  Weenies.Add(w);
  //make the area around the entranse safe
  NavList.L[w.NavId].isSafe := true;
  with NavList.L[w.NavId] do if LinksCount>=0 then
    for i := 0 to LinksCount do with NavList.L[Links[i]] do if LinksCount>=0 then begin
      isSafe := true;
      for j := 0 to LinksCount do with NavList.L[Links[j]] do if LinksCount>=0 then begin
        isSafe := true;
        for k := 0 to LinksCount do with NavList.L[Links[k]] do if LinksCount>=0 then begin
          isSafe := true;
        end;
      end;
    end;
end;

{================== 3D DUNGEON GENERATOR ROUTINES ===========================}

function ZeroIntegerMap(sx,sy,sz: integer): TIntMapArray;
var ix,iy,iz: TIntCoordinate;
begin
  SetLength(Result,sx);
  for ix := 0 to sx-1 do begin
    SetLength(Result[ix],sy);
    for iy := 0 to sy-1 do begin
      SetLength(Result[ix,iy],sz);
      for iz := 0 to sz-1 do
        Result[ix,iy,iz] := -1;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function D3DDungeonGenerator.NilIndexMap: TNeighboursMapArray;
var ix,iy{,iz}: TIntCoordinate;
begin
  SetLength(Result,Map.SizeX);
  for ix := 0 to Map.SizeX-1 do begin
    SetLength(Result[ix],Map.SizeY);
    for iy := 0 to Map.SizeY-1 do begin
      SetLength(Result[ix,iy],Map.SizeZ);
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.MakeTileIndexMap;
var i: integer;
    ix,iy,iz: TIntCoordinate;
begin
  TileIndexMap := ZeroIntegerMap(Map.SizeX,Map.SizeY,Map.SizeZ);
  for i := 0 to MaxSteps-1 do with Tiles[Gen[i].Tile] do
    if not Blocker then // we don't count blockers here, they are not normal tiles :) We'll have to add them later
    for ix := 0 to SizeX-1 do
      for iy := 0 to SizeY-1 do
        for iz := 0 to SizeZ-1 do
          if Map[ix,iy,iz].Base <> tkNone then
            TileIndexMap[ix+Gen[i].x,iy+Gen[i].y,iz+Gen[i].z] := i;
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
    for ix := Trunc(x1) to Trunc(x2)-1 do begin
      a := (ix-x1+1)/vx;
      iy := Trunc(y1+a*vy);
      iz := Trunc(z1+a*vz);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aRight]) then Exit;
    end
  else
    for ix := Trunc(x2) to Trunc(x1)-1 do begin
      a := (ix-x1+1)/vx;
      iy := Trunc(y1+a*vy);
      iz := Trunc(z1+a*vz);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aLeft]) then Exit;
    end;
  if y2>y1 then
    for iy := Trunc(y1) to Trunc(y2)-1 do begin
      a := (iy-y1+1)/vy;
      ix := Trunc(x1+a*vx);
      iz := Trunc(z1+a*vz);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aTop]) then Exit;
    end
  else
    for iy := Trunc(y2) to Trunc(y1)-1 do begin
      a := (iy-y1+1)/vy;
      ix := Trunc(x1+a*vx);
      iz := Trunc(z1+a*vz);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aBottom]) then Exit;
    end;
  if z2>z1 then
    for iz := Trunc(z1) to Trunc(z2)-1 do begin
      a := (iz-z1+1)/vz;
      ix := Trunc(x1+a*vx);
      iy := Trunc(y1+a*vy);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aDown]) then Exit;
    end
  else
    for iz := Trunc(z2) to Trunc(z1)-1 do begin
      a := (iz-z1+1)/vz;
      ix := Trunc(x1+a*vx);
      iy := Trunc(y1+a*vy);
      if not IsLookable(Map.Map[ix,iy,iz].Faces[aUp]) then Exit;
    end;

  Result := true;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.RaycastTile(mx,my,mz: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var
    HelperMap: TIntMapArray;
    RaycastList,OldList: TRaycastList;
    j: integer;
    nx,ny,nz: integer;
    RayCount,RayTrue: integer;
    Neighbour: DNeighbour;

    {safely set a tile candidate}
    procedure SetCandidate(x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    var NewCandidate: Txyz;
    begin
      //dLog(inttostr(HelperMap[x,y,z]));
      {looks redundant as "open face" can't go into void
       but let it be here for now}
      if {(x>=0) and (y>=0) and (z>=0) and
         (x<Map.Sizex) and (y<Map.Sizey) and (z<Map.SizeZ) and }
         (HelperMap[x,y,z] = -1) then
      begin
        //HelperMap[x,y,z] := CandidateIndex;
        NewCandidate.x := x;
        NewCandidate.y := y;
        NewCandidate.z := z;
        RaycastList.Add(NewCandidate);
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
            if isLookable(Map.Map[OldList[i].x,OldList[i].y,OldList[i].z].Faces[a]) then
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
  //init HelperMap and raycast list
  RaycastList := TRaycastList.Create;
  OldList := nil;
  HelperMap := ZeroIntegerMap(Map.SizeX,Map.SizeY,Map.SizeZ);
  SetCandidate(mx,my,mz); //seed for the growth of the map
  HelperMap[mx,my,mz] := MaxNeighboursIndex; //and already define it.

  repeat
    //re-init all the variables, grow the map
    FreeAndNil(OldList);
    OldList := RaycastList;
    RaycastList := TRaycastList.Create;
    //get candidates
    GrowHelperMap;

    //raycast mx,my,mz -> RaycastList[i] (candidates)
    for j := 0 to RaycastList.Count-1 do begin
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
      until ((RayTrue >= RayCount div 2) or (RayCount >= MaxNeighboursIndex)) and (RayCount>CornerCount);

      if RayTrue>0 then
        HelperMap[RayCastList[j].x,RayCastList[j].y,RayCastList[j].z] := MaxNeighboursIndex * RayTrue div RayCount
      else
        HelperMap[RayCastList[j].x,RayCastList[j].y,RayCastList[j].z] := FailedIndex;

      //if RayTrue>0 then dLog(inttostr(mx)+','+inttostr(my)+','+inttostr(mz)+' - '+inttostr(round(100*RayTrue/RayCount)));
    end;
  until RaycastList.Count=0;
  FreeAndNil(OldList);
  FreeAndNil(RaycastList);

  //convert HelperMap to NeighbourList

  tmpNeighboursMap[mx,my,mz] := TNeighboursList.Create;
  //make a list of neighbours
  for nx := 0 to Map.sizex-1 do
    for ny := 0 to map.sizey-1 do
      for nz := 0 to map.sizez-1 do if HelperMap[nx,ny,nz]>0 then begin
        Neighbour.tile := TileIndexMap[nx,ny,nz];
        Neighbour.visible := HelperMap[nx,ny,nz];
        tmpNeighboursMap[mx,my,mz].Add(Neighbour);

        //Add blockers
        //THIS IS UGLY AND INEFFICIENT both on CPU and RAM!!!
        for j := 0 to MaxSteps-1 do if Tiles[Gen[j].Tile].Blocker then
          {warning! This works only for 1x1x1 blockers with only ONE open face}
          if (Gen[j].x+Tiles[Gen[j].Tile].b_x=nx) and (Gen[j].y+Tiles[Gen[j].Tile].b_y=ny) and (Gen[j].z+Tiles[Gen[j].Tile].b_z=nz) then begin
            Neighbour.Tile := j;
            Neighbour.Visible := HelperMap[nx,ny,nz];
            tmpNeighboursMap[mx,my,mz].Add(Neighbour);
          end;
      end;

  //dLog(inttostr(mx)+inttostr(my)+inttostr(mz),inttostr(tmpNeighboursMap[mx,my,mz].count));
  RemoveDuplicatesNeighbours(tmpNeighboursMap[mx,my,mz]);
  //dLog(inttostr(mx)+inttostr(my)+inttostr(mz),inttostr(tmpNeighboursMap[mx,my,mz].count));

  inc(RaycastCount);
  UpdateProgress('Raycasting',1+(RaycastCount/Map.Volume)*0.8);
end;

{----------------------------------------------------------------------------}

function CompareNeighbours(const i1,i2: DNeighbour): integer;
begin
  Result := i1.tile - i2.tile;
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.RemoveDuplicatesNeighbours(var List: TNeighboursList);
var i: integer;
begin
  if list.count <= 1 then Exit;
  List.Sort(@CompareNeighbours);
  i := 0;
  repeat
    if List[i].Tile = List[i+1].Tile then begin
      {duplicate found, choose the largest value and go delete one duplicate}
      if List[i].Visible < List[i+1].Visible then
        List.Delete(i)
      else
        List.Delete(i+1);
      {"The argument cannot be assigned to" - are you joking???
       Really nasty thing about those generic lists! Should keep that in mind
       P.S. My bad. use List.L[I].Value. See:
       https://github.com/castle-engine/castle-engine/issues/63#issuecomment-292794723,
       Thanks, Michalis!}
    end
    else inc(i);
  until i >= List.Count-1;
end;

{-----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.Neighbours_of_neighbours;
var i: integer;
    ix,iy,iz: TIntCoordinate;
    dx,dy,dz: TIntCoordinate;
begin
  NeighboursMap := NilIndexMap;

  dLog(LogGenerateWorld,Self,'D3DDungeonGenerator.Neighbours_of_neighbours','Merging neighbours of neighbours...');
  for ix := 0 to Map.SizeX-1 do
    for iy := 0 to Map.SizeY-1 do
      for iz := 0 to Map.SizeZ-1 do if tmpNeighboursMap[ix,iy,iz]<>nil then begin
        //dLog('neighbours',inttostr(tmpNeighboursMap[ix,iy,iz].count));
        //process neighbours
 {       for dx := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do
          for dy := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do
            for dz := 0 to tiles[tmpNeighboursMap[ix,iy,iz].tile].sizex do {***};
 }
        {$warning not working yet}
        NeighboursMap[ix,iy,iz] := TNeighboursList.Create;
        for i := 0 to tmpNeighboursMap[ix,iy,iz].Count-1 do
          NeighboursMap[ix,iy,iz].Add(tmpNeighboursMap[ix,iy,iz].L[i]);
        RemoveDuplicatesNeighbours(NeighboursMap[ix,iy,iz]);
      end;
end;

{-----------------------------------------------------------------------}

{just a scaling coefficient to avoid float numbers}
procedure D3DDungeonGenerator.Raycast;
var ix,iy,iz: TIntCoordinate;
begin
 RaycastCount := 0;
 tmpNeighboursMap := NilIndexMap; //create a nil-initialized neighbours lists of all accessible map tiles

 {here we fill in the neighbours}
 for ix := 0 to Map.SizeX-1 do
   for iy := 0 to Map.SizeY-1 do
     for iz := 0 to Map.SizeZ-1 do if TileIndexMap[ix,iy,iz]>=0 {and is accessible} then
       RaycastTile(ix,iy,iz);

 UpdateProgress('Processing',0.8);

 Neighbours_of_neighbours;

 //and free temporary map
 FreeNeighboursMap(tmpNeighboursMap);

 UpdateProgress('Finishing',0.9);
end;

{------------------------------------------------------------------------}

{can't make it nested because TCompareFunction cannot accept "is nested"}
type DIndexRec = record
  Index: integer;
  Hits: integer;
end;
type HitList = specialize TGenericStructList<DIndexRec>;
//{$DEFINE InverseSort}
function CompareHits(const i1,i2: DIndexRec): integer;
begin
  {$IFNDEF InverseSort}
  result := i1.hits - i2.hits;
  {$ELSE}
  result := i2.hits - i1.hits;
  {$ENDIF}
end;
procedure D3DDungeonGenerator.Chunk_N_Slice;
var i,j,g: integer;
    ix,iy,iz: integer;
    HitCount: HitList;
    tIndex: DIndexRec;
    TilesUsed: array of boolean;
begin
  {this part of the code will combine tiles in larger groups to boost FPS
  also this will provide for LOD of the whole group generation and far land support for overworld later
  I'm not yet sure how textures will behave... but let's leave this question for later}

  //prepare to count hits
  HitCount := HitList.Create;
  for i := 0 to High(Gen) do begin
    tIndex.Index := i;
    tIndex.Hits := 0;
    HitCount.Add(tIndex);
  end;
  {first we just count how much time a tile is 'hit' by neighbours
  Therefore we find "more popular" tiles which will be 'seeds' for groups}
  for ix := 0 to Map.SizeX-1 do
    for iy := 0 to Map.SizeY-1 do
      for iz := 0 to Map.SizeZ-1 do if NeighboursMap[ix,iy,iz]<>nil then {begin}
        //dLog(inttostr(ix)+inttostr(iy)+inttostr(iz),inttostr(NeighboursMap[ix,iy,iz].count));
        for j := 0 to NeighboursMap[ix,iy,iz].Count-1 do
          inc(HitCount.L[NeighboursMap[ix,iy,iz].L[j].Tile].Hits);
      {end;}
  HitCount.Sort(@CompareHits);

  {now let's start the main algorithm}
  g := 0;
  setlength(TilesUsed,Length(Gen));
  for j := 0 to high(TilesUsed) do TilesUsed[j] := false;
  i := 0;
  repeat
    SetLength(Groups,g+1);
    Groups[g] := TIndexList.Create;
    {here we find the tile with minimal amount (>0) of currently visible neighbours,
    it produces smoother amount of members in a group
    versus less groups in case maximum is used
    Roughly, amount of groups will be ~sqrt(tiles) However, it's not as simple as it might seem}

    //skip sorted list to first unused tile

    {if (i <= high(tilesUsed)){ and (not tilesUsed[Hit_count[i].index]) } then} begin

      {we don't actually need to scan the whole tile(sizex,sizey,sizez), only top-left element,
       if we'll miss something, we'll just add missed tiles to *other* groups later}
      ix := Gen[HitCount[i].Index].x;
      iy := Gen[HitCount[i].Index].y;
      iz := Gen[HitCount[i].Index].z;
      //blockers behave a bit differently
      if Tiles[Gen[HitCount[i].index].Tile].Blocker then begin
        ix += Tiles[Gen[HitCount[i].Index].Tile].b_x;
        iy += Tiles[Gen[HitCount[i].Index].Tile].b_y;
        iz += Tiles[Gen[HitCount[i].Index].Tile].b_z;
      end;

      {we're sure neighbours list is not nil!}
      if NeighboursMap[ix,iy,iz]=nil then raise Exception.Create('NeighboursMap is nil!');
      for j := 0 to NeighboursMap[ix,iy,iz].Count-1 do
        if not TilesUsed[NeighboursMap[ix,iy,iz].L[j].Tile] then begin
          TilesUsed[NeighboursMap[ix,iy,iz].L[j].Tile] := true;
          Groups[g].Add(NeighboursMap[ix,iy,iz].L[j].Tile);
        end;

    end;

    while (i <= High(TilesUsed)) and (TilesUsed[HitCount[i].Index]) do inc(i);
    inc(g);

  until i >= High(Gen);

  dLog(LogGenerateWorld,Self,'D3DDungeonGenerator.Chunk_N_Slice','N groups = '+inttostr(length(groups)));
  for i := 0 to High(Groups) do dLog(LogGenerateWorld,Self,'group '+IntToStr(i),IntToStr(Groups[i].Count));

  FreeAndNil(HitCount);
end;

{------------------------------------------------------------------------}

procedure D3DDungeonGenerator.Generate;
var t,t0: DTime;
begin
  t0 := GetNow;

  //make the logic map
  inherited Generate;
  fisFinished := false;
  fisWorking := true;

  UpdateProgress('Raycasting',1);

  //raycast
  dLog(LogGenerateWorld,Self,'D3DDungeonGenerator.Generate','Raycasting started...');
  t := GetNow;

  MakeTileIndexMap;

  Raycast;

  //TileIndexMap := nil;  //this will free the array -- I'll need it later for debugging?!

  UpdateProgress('Chunking',0.95);

  dLog(LogGenerateWorld,Self,'D3DDungeonGenerator.Generate','Raycasting finished in '+IntToStr(Round((GetNow-t)*1000))+'ms.');
  Chunk_N_Slice;

  fisWorking := false;
  fisFinished := true;

  dLog(LogGenerateWorld,Self,'D3DDungeonGenerator.Generate','Finished. Everything done in '+IntToStr(Round((GetNow-t0)*1000))+'ms.');
  UpdateProgress('Done',2);
end;

{------------------------------------------------------------------------}

function D3DDungeonGenerator.ExportGroups: TIndexGroups;
var i: integer;
begin
  if not isFinished then raise Exception.Create('D3DDungeonGenerator.ExportGroups: ERROR! Trying to access an unfinished Generator');
  Result := TIndexGroups.Create(true);
  for i := 0 to High(Groups) do
    Result.Add(Groups[i]);
  Groups := nil;
end;

{------------------------------------------------------------------------}

function D3DDungeonGenerator.ExportNeighbours: TNeighboursMapArray;
begin
  if not isFinished then raise Exception.Create('Generator.ExportNeighbours: ERROR! Trying to access an unfinished Generator');
  Result := NeighboursMap;
  NeighboursMap := nil;
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
  if Blocker then begin
    {this works only for 1x1x1 blockers with only ONE face and only HORIZONTAL!}
    for a in THorizontalAngle do
      if isPassable(Map[0,0,0].Faces[a]) then begin
        b_x := a_dx(a);
        b_y := a_dy(a);
      end;
  end;
end;

{--------------------------------------------------------------------------}

destructor DGeneratorMap.Destroy;
begin
  FreeAndNil(Dock);
  inherited Destroy;
end;

{========================= OTHER ROUTINES ===================================}

procedure DDungeonGeneratorParameters.Load(URL: string);
var XMLdoc: TXMLDocument;
    RootNode, LargeContainer,SmallContainer: TDOMElement;
    Iterator: TXMLElementIterator;
    FS: DFirstStep;
begin
  dLog(LogGenerateWorld,Self,'DGeneratorParameters.Load',URL);

  if Self=nil then raise Exception.Create('DGeneratorParameters is nil!'); // HELLO, my best bug thing :)

  try
    XMLdoc := URLReadXMLSafe(URL);
    RootNode := XMLdoc.DocumentElement;
    LargeContainer := RootNode.ChildElement('Parameters');
    SmallContainer := LargeContainer.ChildElement('Size');
    MaxX := SmallContainer.AttributeInteger('maxx');
    MaxY := SmallContainer.AttributeInteger('maxy');
    MaxZ := SmallContainer.AttributeInteger('maxz');
    MinX := SmallContainer.AttributeInteger('minx');
    MinY := SmallContainer.AttributeInteger('miny');
    MinZ := SmallContainer.AttributeInteger('minz');

    SmallContainer := LargeContainer.ChildElement('Volume');
    Volume := SmallContainer.AttributeInteger('value');

    SmallContainer := LargeContainer.ChildElement('Faces');
    MaxFaces := SmallContainer.AttributeInteger('max');
    MinFaces := SmallContainer.AttributeInteger('min');

    SmallContainer := LargeContainer.ChildElement('Seed');
    Seed := SmallContainer.AttributeInteger('value');

    AbsoluteURL := false;
    LargeContainer := RootNode.ChildElement('TilesList');
    Iterator := LargeContainer.ChildrenIterator;
    try
      while Iterator.GetNext do if Iterator.current.NodeName = UTF8decode('Tile') then
      begin
        SmallContainer := Iterator.Current;
        TilesList.add({$IFDEF UTF8Encode}UTF8encode{$ENDIF}(SmallContainer.TextData));
      end;
    finally
      FreeAndNil(Iterator);
    end;

    LargeContainer := RootNode.ChildElement('FirstSteps');
    Iterator := LargeContainer.ChildrenIterator;
    try
      while Iterator.GetNext do if Iterator.current.NodeName = UTF8decode('Tile') then
      begin
        SmallContainer := Iterator.current;
        FS.tile := SmallContainer.TextData;
        FS.x := SmallContainer.AttributeInteger('x');
        FS.y := SmallContainer.AttributeInteger('y');
        FS.z := SmallContainer.AttributeInteger('z');
        FirstSteps.Add(FS);
      end;
    finally
      FreeAndNil(Iterator);
    end;

  except
    dLog(LogWorldError,Self,'DGeneratorParameters.Load','ERROR: Exception in GeneratorParameters load');
  end;
  FreeAndNil(XMLdoc);

  {initialize generator parameters}
  isReady := true;
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
  for i := 0 to Tiles.Count do if tiles[i].TileName = TileName then begin
    Result := i;
    Exit;
  end;
  raise Exception.Create('DDungeonGenerator.GetTileByName: FATAL! Tile cannot be found!');
end;

{-----------------------------------------------------------------------------}

constructor DDungeonGeneratorParameters.Create;
begin
  inherited Create;
  TilesList := TStringList.Create;
  FirstSteps := TFirstStepsArray.Create;
end;

{-----------------------------------------------------------------------------}

constructor DDungeonGenerator.Create;
begin
  inherited Create;
  Map := DGeneratorMap.Create;
  Tiles := TGeneratorTileList.Create(true);
  Parameters := DDungeonGeneratorParameters.Create;

  NormalTiles  := TIndexList.Create;
  BlockerTiles := TIndexList.Create;
  DownTiles    := TIndexList.Create;
  RichTiles    := TIndexList.Create;
  PoorTiles    := TIndexList.Create;
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
  FreeAndNil(Tiles);
  FreeAndNil(Parameters);
  FreeLists;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGeneratorParameters.Destroy;
begin
  FreeAndNil(TilesList);
  FreeAndNil(FirstSteps);

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

destructor D3DDungeonGenerator.Destroy;
begin
  FreeNeighboursMap(tmpNeighboursMap);
  FreeNeighboursMap(NeighboursMap);
  FreeGroups(Groups);
  inherited Destroy;
end;

{============================ FREEING ROUTINES ==============================}

procedure FreeGroups(nGroups: TGroupsArray);
var i: integer;
begin
  if nGroups<>nil then
    for i := 0 to high(nGroups) do
      FreeAndNil(nGroups[i]);
end;

{------------------------------------------------------------------------}

procedure FreeNeighboursMap(var nMap: TNeighboursMapArray);
var ix,iy,iz: TIntCoordinate;
begin
  if nMap = nil then Exit;
  //just to be safe (in case the nmap was not initialized completely)
  if Length(nMap) = 0 then Exit;

  //free and nil all the elements
  for ix := 0 to High(nMap) do
    for iy := 0 to High(nMap[ix]) do
      for iz := 0 to High(nMap[ix,iy]) do
        FreeAndNil(nMap[ix,iy,iz]);

  nMap := nil; //this will automatically free the array
end;

end.

