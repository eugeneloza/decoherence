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
  { Dictionary that links two strings
    Usually this is finding a "internal name" by specific "use case"
    Multiple use cases may reference to a single "internal name" }
  DStringDictionary = specialize TDictionary<string, string>;
  { Does exactly the same as the previous one
    Just to make it distinctive that this dictionary links
    "internal name" to a file URL
    It is not mandatory but recommended that each "internal name"
    would reference only to a single URL (file) }
  DURLDictionary = DStringDictionary;

{ Obtain a Dictionary record through a function
  Bypasses all the TDictionary heavy routines and automatically log erors }
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

