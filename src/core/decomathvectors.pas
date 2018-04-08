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

{---------------------------------------------------------------------------}

(* Some inlined CastleVectors conversions
   At the moment they are used only for reading/writing to a file *)

{$INCLUDE compilerconfig.inc}

unit DecoMathVectors;

interface

uses
  CastleVectors;

function VectorFloatToInteger(constref aValue: TVector2): TVector2Integer; TryInline
function VectorFloatToInteger(constref aValue: TVector3): TVector3Integer; TryInline
function VectorFloatToInteger(constref aValue: TVector4): TVector4Integer; TryInline
function VectorIntegerToFloat(constref aValue: TVector2Integer): TVector2; TryInline
function VectorIntegerToFloat(constref aValue: TVector3Integer): TVector3; TryInline
function VectorIntegerToFloat(constref aValue: TVector4Integer): TVector4; TryInline
{............................................................................}
implementation

function VectorFloatToInteger(constref aValue: TVector2): TVector2Integer; TryInline
begin
  Result[0] := Round(aValue[0]);
  Result[1] := Round(aValue[1]);
end;
function VectorFloatToInteger(constref aValue: TVector3): TVector3Integer; TryInline
begin
  Result[0] := Round(aValue[0]);
  Result[1] := Round(aValue[1]);
  Result[2] := Round(aValue[2]);
end;
function VectorFloatToInteger(constref aValue: TVector4): TVector4Integer; TryInline
begin
  Result[0] := Round(aValue[0]);
  Result[1] := Round(aValue[1]);
  Result[2] := Round(aValue[2]);
  Result[3] := Round(aValue[3]);
end;

function VectorIntegerToFloat(constref aValue: TVector2Integer): TVector2; TryInline
begin
  Result[0] := aValue[0];
  Result[1] := aValue[1];
end;
function VectorIntegerToFloat(constref aValue: TVector3Integer): TVector3; TryInline
begin
  Result[0] := aValue[0];
  Result[1] := aValue[1];
  Result[2] := aValue[2];
end;
function VectorIntegerToFloat(constref aValue: TVector4Integer): TVector4; TryInline
begin
  Result[0] := aValue[0];
  Result[1] := aValue[1];
  Result[2] := aValue[2];
  Result[3] := aValue[3];
end;

end.

