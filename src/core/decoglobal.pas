{ Copyright (C) 2012-2017 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{ --------------------------------------------------------------------------- }

(* Defines some generic types and variables,
   also handles random initialzation and other minor but global tasks *)

unit DecoGlobal;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleRandom;

type
  DObject = TObject;

type
  DRandom = TCastleRandom;

type
  DFloat = Single;
  pFloat = ^DFloat;

var
  { random generator used for all interface random events }
  DRND: DRandom;

{$IFDEF LINUX}
{$DEFINE USE_DEV_URANDOM}
{$ENDIF}
{ a little modification of CastleRandom RandomSeed initialization algorithm
  to use /dev/urandom on Linux. Actually /dev/urandom exists on all *NIX OS,
  but I'm not exactly sure if it'll work as expected (test needed)
  We're not pursing cryptographic purposes, so /dev/urandom is perfectly enough }
function GetRandomSeed: LongWord;

{............................................................................}
procedure InitGlobal;
procedure FreeGlobal;
implementation

function GetRandomSeed: LongWord;
{$IFDEF USE_DEV_URANDOM}
var
  DevRnd: file of integer;
begin
  { algorithm according to http://wiki.freepascal.org/Dev_random
    /dev/urandom is a native *nix very high-quality random number generator.
    it's 1000 times slower than CastleRandom,
    but provides a perfect seed initialization. }
  AssignFile(DevRnd, '/dev/urandom');
  Reset(DevRnd);
  repeat
    Read(DevRnd, Result);
  until Result <> 0;
  // xorshift can't accept 0 as a random seed so we just read /dev/urandom until its not zero
  CloseFile(DevRnd);
end;
{$ELSE}
begin
  { otherwise just let an internal algorithm's random initialization do the job }
  GetRandomSeed := 0;
end;
{$ENDIF}

{.............................................................................}
procedure InitGlobal;
begin
  DRND := TCastleRandom.Create(GetRandomSeed);
end;

procedure FreeGlobal;
begin
  DRND.Free;
end;

end.

