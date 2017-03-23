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

uses Classes, CastleRandom
  ;

type
  { Preforms all generation routines from reading the parameters and tiles
    to raycast and chunk-n-slice the dungeon into parts }
  DDungeonGenerator = class(TThread)
  private
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
  public
    { Specific SEED of the random number for this algorithm }
    property Seed: LongWord read fSeed write fSeed default 0;
    { the main procedure to generate a dungeon }
    procedure Generate;

    //constructor Create; override;
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

  //dummy

  // read/parse parameters?
  // empty map
  // main cycle
  // finalize

  FreeAndNil(RNDM);
end;

{-----------------------------------------------------------------------------}

destructor DDungeonGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

end.

