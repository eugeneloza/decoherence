{Copyright (C) 2012-2018 Yevhen Loza

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

(* Basic math operations, inlined replacement for Math routines
   Name is too "strong" for such a tiny and minor unit *)

unit DecoMath;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

{.......................................................................}
{ Inlined equivalent of Math.Equals }
function dEqual(const a, b: DFloat): Boolean; TryInline
{ Inlined equivalent of Math.isZero }
function dZero(const a: DFloat): Boolean; TryInline
{ Returns a if a>0 or zero if a<0 }
function AboveZero(const a: DFloat): DFloat; TryInline
{ Returns larger number of a, b }
function Maximum(a, b: DFloat): DFloat; TryInline
{ Returns sign of a }
function Sign(a: DFloat): shortint; TryInline
implementation

const
  Epsilon = 1E-4;
  { EZeroResolution = 1E-16;
    DZeroResolution = 1E-12;
    SZeroResolution = 1E-4; }

function dEqual(const a, b: DFloat): Boolean; TryInline
begin
  if (a > b) then
    Result := ((a - b) <= Epsilon)
  else
    Result := ((b - a) <= Epsilon);
end;

{-----------------------------------------------------------------------------}

function dZero(const a: DFloat): Boolean; TryInline
begin
  if abs(a) <= Epsilon then
    Result := True
  else
    Result := False;

end;

{-----------------------------------------------------------------------------}

function AboveZero(const a: DFloat): DFloat; TryInline
begin
  if a > 0 then
    Result := a
  else
    Result := 0;
end;

{-----------------------------------------------------------------------------}

function Maximum(a, b: DFloat): DFloat; TryInline
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

{-----------------------------------------------------------------------------}

function Sign(a: DFloat): shortint; TryInline
begin
  if a > 0 then   {a < Epsilon?}
    Result := +1
  else
  if a < 0 then
    Result := -1
  else
    Result := 0;
end;

end.

