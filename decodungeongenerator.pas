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
  {extended functionality for DTileMap;}
  DGeneratorMap = class(DTileMap)
    private
      fFreeFaces: integer;
      fVolume: integer;
      fHasStairsDown: boolean;
    public
      {what TTileFaces do leave from this tile?}
      //FacesList: TTileFace;
      {list of dock point to this tile}
      Dock: TDockPointList;
      {does this tile/map has stairs down?}
      property HasStairsDown: boolean read fHasStairsDown;
      {total free Faces of the map/tile}
      property FreeFaces: integer read fFreeFaces;
      {total passable volume of the map/tile}
      property Volume: integer read fVolume;
      {calculates faces&volume of the tile/map and prepares it for work}
      function CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

      destructor destroy; override;
  end;
type TTileMapList = specialize TFPGObjectList<DGeneratorMap>;

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
    {list of tiles}
    TilesList: TStringList;
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
    Tiles: TTileMapList;
    {searches Tiles for a specific tile name and returns it
     used to make first steps of the generation as they are bound to
     a specific tile name, not to a tile ID which can change}
    function GetTileByName(TileName: string): DGeneratorMap;
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
    {current (the last used) step of the generator}
    currentStep: integer;
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
    procedure AddTileUnsafe(Tile: DGeneratorMap; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {overloaded version that accepts a DGeneratorStep;}
    procedure AddTileUnsafe(step: DGeneratorStep);
    { Specific SEED of the random number for this algorithm }
    procedure InitSeed(newseed: longword = 0);
  public

    {map parameters}
    Parameters: DGeneratorParameters;

    {copy the "internal" map to external request}
    function GetMap: DMap;
    //maybe better saveTo when finished?: DMap;


    property isReady: boolean read fisReady default false;
    property isWorking: boolean read fisWorking default false;
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
    procedure Generate3D;
  protected
    procedure  Execute; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog,
  decoglobal;

procedure DDungeonGenerator.execute;
begin
  Generate;
end;

{========================= GENERATION ALGORITHM ==============================}

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

  //load pregenerated tiles
  maxSteps := 0;
  setlength(Gen, maxSteps);

  {initialize random seed}
  InitSeed(parameters.seed);

  {initialize map size}
  Map.SetSize(parameters.maxx,parameters.maxy,parameters.maxz); //we don't care about RAM now, we'll shrink the map later

  minSteps := 0;
  CurrentStep := minSteps;

  repeat
    Map.EmptyMap(false);
    for i := 0 to currentStep-1 do AddTileUnsafe(Gen[i]);

    //add prgenerated or undo tiles
    while Map.CalculateFaces<>0 do begin
      //add a tile
    end;
    {if map doesn't meet the paramters then undo}
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
  if currentstep>maxsteps then begin
    inc(maxsteps,GeneratorShift);
    setlength(Gen, maxsteps);
  end
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
    inc(currentStep);
    ResizeSteps;
    Gen[CurrentStep].tile := tile;
    Gen[CurrentStep].x := x;
    Gen[CurrentStep].y := y;
    Gen[CurrentStep].z := z;

    Result := true;
  end else
    Result := false;


end;

{-----------------------------------------------------------------------------}

procedure DDungeonGenerator.AddTileUnsafe(tile: DGeneratorMap; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
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

{$DEFINE CompleteGen}//this (maybe) will separate implementations for Map and Tile
function DGeneratorMap.CalculateFaces: integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var ix,iy,iz: integer;
    a: TAngle;
    f: TTileFace;
    tmpDock: DDockPoint;
begin
  {$WARNING Optimize for different variants}
  if Dock = nil then Dock := TDockPointList.Create else Dock.Clear;
  fFreeFaces := 0;
  fVolume := 0;
  {$IFDEF CompleteGen}fHasStairsDown := false;{$ENDIF}
  //FacesList := [];
  for ix := 0 to sizex-1 do
    for iy := 0 to sizey-1 do
      for iz := 0 to sizez-1 do begin
        if isPassable(Map[ix,iy,iz].base) then inc(fVolume);
        {$IFDEF CompleteGen}if Map[ix,iy,iz].base = tkDown then fHasStairsDown := true;{$ENDIF}
        for a in TAngle do
          if isPassable(Map[ix,iy,iz].faces[a]) then begin
            f := self.MapSafeFace(ix+a_dx(a),iy+a_dy(a),iz+a_dz(a),invertAngle(a));
            if (f = tfNone) or (f = tfInacceptible) then begin
              tmpDock.face := a;
              tmpDock.x := ix;
              tmpDock.y := iy;
              tmpDock.z := iz;
              Dock.Add(tmpDock);
              {$IFDEF CompleteGen}
              //Include(FacesList,Map[ix,iy,iz].faces[a]);
              {$ENDIF}
              inc(fFreeFaces);
            end;
          end;
      end;
  {$IFDEF CompleteGen}
  //check if it's a blocker tile
  if (FreeFaces = 1) and (sizex+sizey+sizez = 3) and (Map[0,0,0].base = tkNone) then
    blocker := true
  else
    blocker := false;
  {$ENDIF}
  Result := fFreeFaces;
  writeLnLog(inttostr(result));
end;

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

Function DDungeonGenerator.GetTileByName(TileName: string): DGeneratorMap;
var I: DGeneratorMap;
begin
  Result := nil;
  for I in Tiles do if I.TileName = TileName then begin
    Result := I;
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


constructor DDungeonGenerator.create;
begin
  inherited;
  Map := DGeneratorMap.create;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  FreeAndNil(Map);
  FreeAndNil(parameters.TilesList);
  inherited;
end;

end.

