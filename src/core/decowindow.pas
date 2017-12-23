{ Copyright (C) 2012-2017 Yevhen Loza

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

(* Handles window size and scale
   !!! NO RESCALES ALLOWED DURING THE GAME !!!
   Changing FullScreen mode or Resolution only by rebooting! *)

unit DecoWindow;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleWindow;

var
  { Main window of the game }
  Window: TCastleWindow;

{ creates and initializes the Window }
procedure InitWindow;
{............................................................................}
implementation

uses
  DecoGuiScale,
  DecoLog;

var
  ConfigFullScreen: boolean;
  ConfigWindowWidth: integer;
  ConfigWindowHeight: integer;

procedure ReadWindowConfiguration;
begin
  ConfigFullScreen := false;
  ConfigWindowWidth := 1024;
  ConfigWindowHeight := 600;
end;

procedure InitWindow;
begin
  Window := TCastleWindow.Create(Application);
  Window.DoubleBuffer := True;
  Window.ResizeAllowed := raOnlyAtOpen;

  ReadWindowConfiguration;

  Window.FullScreen := ConfigFullScreen;
  if ConfigFullScreen then
  begin
    Log(LogInit, CurrentRoutine, 'FullScreen mode ON');
  end
  else
  begin
    Log(LogInit, CurrentRoutine, 'FullScreen mode OFF, Window resolution = ' +
      ConfigWindowWidth.ToString + 'x' + ConfigWindowHeight.ToString);
    Window.Width := ConfigWindowWidth;
    Window.Height := ConfigWindowHeight;
  end;

  ResetGUIScale;
end;

end.

