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

{$INCLUDE compilerconfig.inc}

unit DecoTrash;

interface

uses
  Generics.Collections;

type
  TTrash = specialize TObjectList<TObject>;

var
  AutoFree: TTrash;

{ Frees an object from the trash }
procedure FreeTrashObject(constref aObject: TObject);
{ Initialize trash routines }
procedure InitTrash;
{ Release trash routines}
procedure FreeTrash;
{............................................................................}
implementation
uses
  Profiler;

procedure FreeTrashObject(constref aObject: TObject);
begin
  {StartProfiler}

  {not sure if it works fine}
  AutoFree.Remove(aObject); //<------- automatically frees the object
  // aObject.Free;

  {StopProfiler}
end;

{............................................................................}

procedure InitTrash;
begin
  {StartProfiler}

  AutoFree := TTrash.Create(true);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure FreeTrash;
begin
  {StartProfiler}

  AutoFree.Free;

  {StopProfiler}
end;


end.

