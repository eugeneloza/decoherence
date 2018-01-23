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

(* Core file of the game. Processes Window events and initializes the game *)

unit DecoMain;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleWindow, DecoGlobal;

{ not sure if it should be placed here? }
procedure doBeforeRender(Container: TUIContainer);
{............................................................................}
implementation

uses
  DecoInit, DecoPlayer,
  DecoTime;

procedure doBeforeRender(Container: TUIContainer);
begin
  doTime;
  //World.Manage;
  //Actors.Manage;
  //Music.Manage;
  Player.Manage;
end;

{............................................................................}
initialization
  InitDecoherence;

finalization
  FreeDecoherence;

end.

