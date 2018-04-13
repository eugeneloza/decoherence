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

{$INCLUDE compilerconfig.inc}

unit DecoGenerics;

interface

uses
  Generics.Collections;

type
  {}
  DStringDictionary = specialize TDictionary<string, string>;
  {}
  DURLDictionary = DStringDictionary;

{}
function GetStringByKey(const Source: DStringDictionary; const aKey: string): string;
{............................................................................}
implementation
uses
  DecoLog;

function GetStringByKey(const Source: DStringDictionary; const aKey: string): string;
begin
  if not Source.TryGetValue(aKey, Result) then
  begin
    Log(LogWarning, CurrentRoutine, 'Unknown Key: ' + aKey);
    Result := '';
  end
end;


end.

