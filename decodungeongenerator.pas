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
  {this is a basic "add a tile" generator step. We can build a map by followig these
   we can also use preset generatoe steps to make some tiles already available at map}
  TGeneratorStep = record
    tile: integer;
    x,y,z: integer;
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
    
    {if currentstep+1>maxsteps then setlength(..., maxsteps+100)}
    
    {checks if the selected tile is compatible to the already-generated map
      usually should not be added manually, but is used inline in "AddTile"}
    {function CheckTile(tile, x,y,z: integer): boolean; inline;}
    {checks tile compatibility to the map and adds it if its possible}
    {procedure AddTile(tile, x,y,z: integer)}
    {When we're sure what we are doing, we're not making any checks, just add the tile as fast as it is possible}
    {procedure AddTileUnsafe(tile, x,y,z: integer)}
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
    { the main procedure to generate a dungeon, may be launched not-in-a-thread }
    procedure Generate;

    constructor Create;// override;
    destructor Destroy; override;
    
    {loads and applies the map parameters}
    {procedure LoadMap(filename: string);}
    
    //save temp state to file
  protected
    { here we launch Generate in a Thread }
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
  
  {initialize map size}
  Map.SetSize(mapx,mapy,mapz);
  Map.EmptyMap;
  
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
  {$warning check non-initialized version}
  RNDM := TCastleRandom.Create;
  // InitSeed(GetRandomSeed); // maybe this is not optimal as operation takes some time and not required when manually initializing the seed;
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  FreeAndNil(Map);
  inherited;
end;

end.

