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

(* Defines a "TThrash" list, creates it and frees at game end. *)

unit DecoTrash;

{$INCLUDE compilerconfig.inc}
interface

uses
  Generics.Collections;

type
  TTrash = specialize TObjectList<TObject>;

var
  AutoFree: TTrash;

procedure InitTrash;
procedure FreeTrash;
{............................................................................}
implementation

procedure InitTrash;
begin
  AutoFree := TTrash.Create(true);
end;

{-----------------------------------------------------------------------------}

procedure FreeTrash;
begin
  AutoFree.Free;
end;


end.

