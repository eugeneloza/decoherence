{ Copyright (C) 2012-2018 Yevhen Loza

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
  CastleRandom, CastleColors, CastleImages;

const
  { To quickly change Scaling Method. Maybe will be a variable some day to support older PCs. }
  InterfaceScalingMethod: TResizeInterpolation = riBilinear;

type
  { At this moment it's just a copy of TOjbect, might change in future,
    warning: keep an eye for non-virtual TObject.Create here as it's
    considered non-virtual in DObject descendants }
  DObject = TObject;

type
  { Internal random feature. Mostly for convenience.
    At this point only a copy of TCastleRandom }
  DRandom = TCastleRandom;

type
  { Convenient definition of "float" accuracy for easy changing
    (hardly will ever change into something different than single) }
  DFloat = Single;
  pFloat = ^DFloat;

type
  { Simple procedures for simple events }
  TSimpleProcedure = procedure of object;


var
  { Random generator used for all interface random events }
  DRND: DRandom;
  { Tint of the Interface (including cursor) }
  GUITint: TCastleColor;

{$IFDEF LINUX}
{$DEFINE USE_DEV_URANDOM}
{$ENDIF}
{ a little modification of CastleRandom RandomSeed initialization algorithm
  to use /dev/urandom on Linux. Actually /dev/urandom exists on all *NIX OS,
  but I'm not exactly sure if it'll work as expected (test needed)
  We're not pursing cryptographic purposes, so /dev/urandom is perfectly enough }
function GetRandomSeed: LongWord;

{ Initialize global elements, such as Random }
procedure InitGlobal;
{ Free global elements, such as Random }
procedure FreeGlobal;
{............................................................................}
implementation
uses
  SysUtils;

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

