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
  decodungeontiles;

type
  {maybe I'll change it later}
  TTileType = word;
  TIntCoordinate = integer;

type DDockPoint = record
  x,y,z: TIntCoordinate;
  face: TAngle;
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
    public
      {what TTileFaces do leave from this tile?}
      //FacesList: TTileFace;
      {list of dock point to this tile}
      Dock: TDockPointList;
      {total free Faces of the map/tile}
      property FreeFaces: integer read fFreeFaces;
      {total passable volume of the map/tile}
      property Volume: integer read fVolume;
      {calculates faces&volume of the tile/map and prepares it for work
       optimized for DGeneratorMap}
      function CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

      destructor destroy; override;
  end;
type DGeneratorTile = class(DGeneratorMap)
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
    tile: TTileType;
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
    tile: string;
    x,y,z: TIntCoordinate;
  end;
  TFirstStepsArray = specialize TGenericStructList<DFirstStep>;

type
  {a set of map generation parameters}
  DGeneratorParameters = record
    {the map cannot be larger than these}
    maxx,maxy,maxz: integer;
    {the map cannot be smaller than these}
    minx,miny,minz: integer;
    {random generation seed}
    Seed: LongWord;
    {are links in TilesList absolute URL or just a tile name}
    absoluteURL: boolean;
    {list of tiles}
    TilesList: TStringList;
    {list of first generation steps}
    FirstSteps: TFirstStepsArray
  end;


type
  { Preforms all map generation routines
    This is a relatively CPU-intensive work therefore must be heavily optimized }
  DDungeonGenerator = class(TThread)
  private
    fisWorking: boolean;
    fisReady: boolean;
  private
    {list of tiles used in current map}
    Tiles: TTileList;
    {searches Tiles for a specific tile name and returns it
     used to make first steps of the generation as they are bound to
     a specific tile name, not to a tile ID which can change}
    //function GetTileByName(TileName: string): DGeneratorMap;
    function GetTileByName(TileName: string): TTileType;
  private
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
    {the main operating item - map of the resulting dungeon}
    Map: DGeneratorMap;

    {this stores a sequence of generator steps}
    Gen: TGeneratorStepsArray;
    {minSteps: minimal steps before undo, this corresponds to the pre-generated tiles which may not be cancelled}
    minsteps: integer;
    {current size of the dynamic array}
    maxsteps: integer;
    {current (the last used) step of the generator;
     careful, currentStep starts from -1, while corresponding min/max steps start from 0}
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
    {checks tile compatibility to the map and adds it if its possible
     returns true if tile is put successfully
     and false if the tile cannot be placed at these coordinates  }
    function AddTile(tile: TTileType; x,y,z: TIntCoordinate): boolean;
    {When we're sure what we are doing, we're not making any checks, just add the tile as fast as it is possible}
    procedure AddTileUnsafe(Tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {overloaded version that accepts a DGeneratorStep;}
    procedure AddTileUnsafe(step: DGeneratorStep);
    {Specific SEED of the random number for this algorithm }
    procedure InitSeed(newseed: longword = 0);
    {initialize parameters and load pre-generated tiles}
    procedure InitParameters;
  public

    {map parameters}
    Parameters: DGeneratorParameters;

    {copy the "internal" map to external request}
    function GetMap: DMap;
    //maybe better saveTo when finished?: DMap;

    {is the generator ready to wrok?
     Generatie will raise an exception if it isn't}
    property isReady: boolean read fisReady default false;
    {is the generator currently working?}
    property isWorking: boolean read fisWorking default false;
    {this forces isReady to true. Must be used only in constructor which skips
     loading of the map}
    procedure ForceReady;
    { the main procedure to generate a dungeon,
      may be launched in main thread (for testing or other purposes) }
    procedure Generate;

    constructor Create;// override;
    destructor Destroy; override;
    
    {loads and applies the map parameters}
    procedure Load(filename: string);
    
    //save temp state to file
  protected
    { here we simply launch "Generate" in a Thread }
      procedure Execute; override;
end;

type
  {this is a Dungeon Generator with additional 3D world generation,
   and linked stuff like raycast and chunk-n-slice the dungeon into parts}
  D3DDungeonGenerator = class(DDungeonGenerator)
  public
    {launches DDungeonGenerator.Generate and builds 3D world afterwards
     can be launched directly for debugging}
    procedure Generate3D;
  protected
    {launches Generate3D in a thread}
    procedure  Execute; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
  decoglobal;

{========================= GENERATION ALGORITHM ==============================}

procedure DDungeonGenerator.InitParameters;
var s: string;
    tmp: DGeneratorTile;
    i : integer;//: DFirstStep;
begin
  tiles.clear;
  For s in parameters.TilesList do begin
    if parameters.AbsoluteURL then
      tmp := DGeneratorTile.Load(s+'.map')
    else
      tmp := DGeneratorTile.Load(ApplicationData(s+'.map'+GZ_ext));
    tmp.CalculateFaces;
    tiles.Add(tmp);
  end;

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

end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.Generate;
var i: integer;
begin
  if not isReady then
    raise exception.create('DDungeonGenerator.Generate FATAL - parameters are not initialized!');
  if isWorking then begin
    WriteLnLog('DDungeonGenerator.Generate','Generation thread is buisy! Aborting...');
    exit;
  end;
  fisWorking := true;

  InitParameters;

  repeat
    Map.EmptyMap(false);
    //add prgenerated or undo tiles
    for i := 0 to currentStep-1 do AddTileUnsafe(Gen[i]);

    while Map.CalculateFaces<>0 do begin
      //add a tile
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
  until true; {until map meets the paramters}
  // finalize

  fisWorking := false;
end;

{-----------------------------------------------------------------------------}

function DDungeonGenerator.GetMap: DMap;
var ix,iy,iz: integer;
begin
  Result := DMap.create;
  Result.setsize(Map.sizex,Map.sizey,Map.sizez);
  for ix := 0 to Map.sizex-1 do
    for iy := 0 to Map.sizey-1 do
      for iz := 0 to Map.sizez-1 do
        Result.map[ix,iy,iz] := Map.Map[ix,iy,iz];
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
var jx,jy,jz:integer;
    TileCanBePlaced:boolean;
begin
  TileCanBePlaced := true;
  //check all tiles against map area they are placed to
  for jx := 0 to tiles[tile].sizex-1 do
   for jy := 0 to tiles[tile].sizey-1 do
    for jz := 0 to tiles[tile].sizez-1 do
      TileCanBePlaced := TileCanBePlaced and
                         CheckCompatibility(tiles[tile].Map[jx,jy,jz],x+jx,y+jy,z+jz);

  if TileCanBePlaced then begin
    // if tile can be placed - then place it
    AddTileUnsafe(Tiles[tile],x,y,z);
    {add current step to GEN}
    incCurrentStep;
    Gen[CurrentStep].tile := tile;
    Gen[CurrentStep].x := x;
    Gen[CurrentStep].y := y;
    Gen[CurrentStep].z := z;

    Result := true;
  end else
    Result := false;


end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.AddTileUnsafe(tile: DGeneratorTile; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var jx,jy,jz: integer;
    a: TAngle;
begin
  for jx := 0 to tile.sizex-1 do
   for jy := 0 to tile.sizey-1 do
    for jz := 0 to tile.sizez-1 do begin
      if Tile.Map[jx,jy,jz].base <> tkNone then Map.Map[x+jx,y+jy,z+jz].base := Tile.Map[jx,jy,jz].base;
      for a in TAngle do if Tile.Map[jx,jy,jz].faces[a] <> tfNone then
        Map.Map[jx,jy,jz].faces[a] := Tile.Map[jx,jy,jz].faces[a];
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

{================== 3D DUNGEON GENERATOR ROUTINES ===========================}

procedure D3DDungeonGenerator.Generate3D;
begin
  Generate;
  //make 3D world
end;

{----------------------------------------------------------------------------}

procedure D3DDungeonGenerator.Execute;
begin
  Generate3D;
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

destructor DGeneratorMap.destroy;
begin
  FreeAndNil(Dock);
  inherited;
end;

{========================= OTHER ROUTINES ===================================}


procedure DDungeonGenerator.InitSeed(newseed: longword = 0);
begin
  RNDM.initialize(newseed);
end;

{-----------------------------------------------------------------------------}


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

procedure DDungeonGenerator.ForceReady;
begin
  fisReady := true;
  WriteLnLog('DDungeonGenerator.ForceReady','Be careful, parameters might not be initialized correctly.');
end;
{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.execute;
begin
  Generate;
end;

{-----------------------------------------------------------------------------}

constructor DDungeonGenerator.create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);

  Map := DGeneratorMap.create;
  Tiles := TTileList.Create(true);
  parameters.TilesList := TStringList.create;
  parameters.FirstSteps := TFirstStepsArray.create;
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  FreeAndNil(Map);
  FreeAndNil(tiles);
  FreeAndNil(parameters.TilesList);
  FreeAndNil(parameters.FirstSteps);
  inherited;
end;

end.

