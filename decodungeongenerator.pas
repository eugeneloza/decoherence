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
  { Preforms all generation routines from reading the parameters and tiles
    to raycast and chunk-n-slice the dungeon into parts }
  DDungeonGenerator = class(TThread)
  private
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
    {the main operating item - map of the resulting dungeon}
    Map: DMap;
  public

    {map sizes}
    mapx,mapy,mapz: integer;

    {copy the "internal" map to external request}
    function GetMap: DMap;
    //maybe better saveTo when finished?: DMap;

    { Specific SEED of the random number for this algorithm }
    property Seed: LongWord read fSeed write fSeed default 0;
    { the main procedure to generate a dungeon, may be launched not-in-a-thread }
    procedure Generate;

    constructor Create;// override;
    destructor Destroy; override;
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
  if fSeed = 0 then fSeed := GetRandomSeed;
  RNDM := TCastleRandom.Create(fSeed);

  // read/parse parameters?
  mapx := 10;
  mapy := 10;
  mapz := 3;

  Map.SetSize(mapx,mapy,mapz);
  Map.EmptyMap;

  // main cycle
  // finalize

  FreeAndNil(RNDM);
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

constructor DDungeonGenerator.create;
begin
  inherited;
  Map := DMap.create;
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  FreeAndNil(Map);
  inherited;
end;

end.

