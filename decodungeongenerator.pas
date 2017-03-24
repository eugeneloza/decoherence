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

{ Dungeon (a maze-like interior) generator }
unit decodungeongenerator;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, CastleRandom,
  decodungeontiles;

type
  {maybe I'll change it later}
  TTileType = word;
  TIntCoordinate = integer;

type
  {this is a basic "add a tile" generator step. We can build a map by followig these
   we can also use preset generatoe steps to make some tiles already available at map}
  TGeneratorStep = record
    tile: TTileType;
    x,y,z: TIntCoordinate;
end;

type GeneratorStepsArray = array of TGeneratorStep;

type
  { Preforms all generation routines from reading the parameters and tiles
    to raycast and chunk-n-slice the dungeon into parts }
  DDungeonGenerator = class(TThread)
  private
    fisWorking: boolean;
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
    {the main operating item - map of the resulting dungeon}
    Map: DMap;
    {this stores a sequence of generator steps}
    Gen: GeneratorStepsArray;
    {minSteps: minimal steps before undo, this corresponds to the pre-generated tiles which may not be cancelled}
    minsteps: integer;
    {current size of the dynamic array}
    maxsteps: integer;
    {current (the last used) step of the generator}
    currentStep: integer;
    procedure ResizeSteps; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    {checks if the selected 1x1x1 element is compatible to the already-generated map
     usually should not be added manually, but is used inline in "AddTile"
     Returns false if tile mismatches Map
     Returns True if tile matches Map and can be placed }
    function CheckCompatibility(tile: BasicTile; x,y,z: TIntCoordinate): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {checks tile compatibility to the map and adds it if its possible
     returns true if tile is put successfully
     and false if the tile cannot be placed at these coordinates  }
    function AddTile(tile: TTileType; x,y,z: TIntCoordinate): boolean;
    {When we're sure what we are doing, we're not making any checks, just add the tile as fast as it is possible}
    procedure AddTileUnsafe(tile: TTileType; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {overloaded version that accepts a TGeneratorStep;}
     procedure AddTileUnsafe(step: TGeneratorStep);
  public

    {map parameters}
  //  seed: longword;
    mapx,mapy,mapz: integer;

    {copy the "internal" map to external request}
    function GetMap: DMap;
    //maybe better saveTo when finished?: DMap;

    { Specific SEED of the random number for this algorithm }
    //property Seed: LongWord read fSeed write fSeed default 0;
    procedure InitSeed(newseed: longword = 0);
    
    property isWorking: boolean read fisWorking;
    { the main procedure to generate a dungeon,
      may be launched in main thread (for testing or other purposes) }
    procedure Generate;

    constructor Create;// override;
    destructor Destroy; override;
    
    {loads and applies the map parameters}
    {procedure LoadMap(filename: string);}
    
    //save temp state to file
  protected
    { here we simply launch "Generate" in a Thread }
      procedure Execute; override;
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
begin
  fisWorking := true;
  {initialize generator parameters}
  
  {initialize random seed}
  InitSeed(0);
  
  mapx := 10;
  mapy := 10;
  mapz := 3;

  maxSteps := 0;
  setlength(Gen, maxSteps);

  {initialize map size}
  Map.SetSize(mapx,mapy,mapz);
  Map.EmptyMap;

  CurrentStep := 0;

  //load pregenerated tiles
  minSteps := CurrentStep;


  //add prgenerated or undo tiles
  
  // main cycle
  // finalize

  FreeAndNil(RNDM);
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
    inc(maxsteps,100);
    setlength(Gen, maxsteps+100);
  end
end;

function DDungeonGenerator.AddTile(tile: TTileType; x,y,z: TIntCoordinate): boolean;
var {jx,jy,jz,jj:integer;}
    TileCanBePlaced:boolean;
begin
  TileCanBePlaced:=true;
{ with tiles[InTileType] do begin
  //check all tiles against map area they are placed to
  for jx:=1 to tilesizex do
   for jy:=1 to tilesizey do
    for jz:=1 to tilesizez do TileCanBePlaced:=TileCanBePlaced and CheckTileCompatible(TileMap[jx,jy,jz],x+jx-1,y+jy-1,z+jz-1);}

  if TileCanBePlaced then begin
    // if tile can be placed - then place it
    AddTileUnsafe(tile,x,y,z);
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

procedure DDungeonGenerator.AddTileUnsafe(tile: TTileType; x,y,z: TIntCoordinate); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{var jx,jy,jz,jj:integer; }
begin
{  for jx:=1 to tilesizex do
   for jy:=1 to tilesizey do
    for jz:=1 to tilesizez do begin
      if TileMap[jx,jy,jz].base<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].base:=TileMap[jx,jy,jz].base;
      for jj:=0 to maxangles do
        if TileMap[jx,jy,jz].faces[jj]<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].faces[jj]:=TileMap[jx,jy,jz].faces[jj];
      for jj:=1 to 2 do
        if TileMap[jx,jy,jz].floor[jj]<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].floor[jj]:=TileMap[jx,jy,jz].floor[jj];
    end;}
end;

procedure DDungeonGenerator.AddTileUnsafe(step: TGeneratorStep);
begin
  AddTileUnsafe(step.tile,step.x,step.y,step.z);
end;

function DDungeonGenerator.CheckCompatibility(tile: BasicTile; x,y,z: TIntCoordinate): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{var TmpTile:Basic_Tile_Type;
    i:integer;   }
begin
  Result := true;
  {if InTile.base<>tile_na then begin
    TmpTile:=getMap(cx,cy,cz);
    if TmpTile.base<>tile_na then checkTileCompatible:=false else begin
      //check current tile if it has some pre-determination
      for i:=0 to maxangles do if (TmpTile.faces[i]<>InTile.faces[i]) and (TmpTile.faces[i]<>face_na) then CheckTileCompatible:=false;
      for i:=1 to 2 do         if (TmpTile.floor[i]<>InTile.floor[i]) and (TmpTile.floor[i]<>floor_na) then CheckTileCompatible:=false;
      if checkTileCompatible then begin
        //if current tile ok, then check adjacent tiles
        for i:=0 to maxangles do begin
          TmpTile:=getMap(cx+a_dx(i),cy+a_dy(i),cz);
          if TmpTile.base<>tile_inacceptible then  //might be optimized at getMap level / todo
            if (TmpTile.faces[inverseAngle(i)]<>InTile.faces[i]) and (TmpTile.faces[inverseAngle(i)]<>face_na) then CheckTileCompatible:=false;
        end;
      end;
    end;
  end;}
end;


{========================= OTHER ROUTINES ===================================}


procedure DDungeonGenerator.InitSeed(newseed: longword = 0);
begin
  if newseed=0 then fseed := GetRandomSeed else fseed := newseed;
  RNDM.initialize(fseed); 
end;

constructor DDungeonGenerator.create;
begin
  inherited;
  fisWorking := false;
  Map := DMap.create;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time on it now}
  RNDM := TCastleRandom.Create(1);
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  FreeAndNil(Map);
  inherited;
end;

end.

