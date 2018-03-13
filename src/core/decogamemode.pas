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

(* Hanldes changes of game mode and corresponding interface changes *)

unit DecoGameMode;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;


{ Does this mode needs clearing screen?
  Equals to "if SceneManager is active at the moment?"
  Usually it discerns between 3D rendered world game mode and other screens
  (like menus or character interface) }
function GameModeNeedsClearingScreen: boolean;
{ Is the current game mode 3D using mouse/drag look feature }
function GameModeMouseLook: boolean;
{............................................................................}
implementation

uses
  DecoWindow;

function GameModeNeedsClearingScreen: boolean;
begin
  { Something more efficient, or world-attached might be here }
  Result := (Window.SceneManager = nil) or not Window.SceneManager.Exists;
end;

{-----------------------------------------------------------------------------}

function GameModeMouseLook: boolean;
begin
  Result := not GameModeNeedsClearingScreen; //todo
end;

end.

