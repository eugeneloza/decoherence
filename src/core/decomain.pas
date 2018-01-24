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

(* Core file of the game. Processes Window events and initializes the game *)

unit DecoMain;
{$INCLUDE compilerconfig.inc}

interface

uses
  CastleWindow, DecoGlobal;

{ Assign Window.OnBeforeRender event }
procedure InitManagement;
{ Release Window.OnBeforeRender event }
procedure FreeManagement;
{............................................................................}
implementation

uses
  DecoInit, DecoPlayer,
  DecoTime, DecoWindow;

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
{ not sure if it should be placed here? }
procedure doBeforeRender(Container: TUIContainer);
begin
  doTime;
  //World.Manage;
  //Actors.Manage;
  //Music.Manage;
  Player.Manage;
end;
{$POP}

{-----------------------------------------------------------------------------}

procedure InitManagement;
begin
  Window.OnBeforeRender := @doBeforeRender;
end;

{-----------------------------------------------------------------------------}

procedure FreeManagement;
begin
  Window.OnBeforeRender := nil;
end;

{............................................................................}
initialization
  InitDecoherence;

finalization
  FreeDecoherence;

end.

