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

(* Determines sacle and size of GUI elements based on current window size
   !!! NO RESCALES ALLOWED DURING THE GAME !!!
   Changing FullScreen mode or Resolution only by rebooting! *)

unit DecoGUIScale;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleVectors,
  DecoGlobal;

var
  { Width and height of GUI }
  GUIWidth, GUIHeight: integer;
  { =1/GUIHeight }
  GUIScale: DFloat;
  { What is considered a 1 pixel gap between some dense GUI elements
    It can become larger if resolution (window size) is large }
  GUISmallGap: integer;
  { A bit larger than GUISmallGap }
  GUILargeGap: integer;
  { Reference size of a clickable action button }
  GUIUnit: integer;
  { Center of the GUI/Window }
  GUICenter: TVector2;

{ Resets all scale variables to match Window.Width, Window.Height }
procedure ResetGUIScale;
{............................................................................}
implementation

uses
  DecoWindow, DecoLog;

var GuiScaleInitialized: boolean = false;

procedure ResetGUIScale;
  function RoundOne(a: DFloat): integer;
  begin
    Result := Round(a);
    if Result < 1 then Result := 1;
  end;
begin
  if GuiScaleInitialized then
    Log(LogInterfaceError, CurrentRoutine, 'WARNING: Reinitializing the GUI Scale!');
  GuiScaleInitialized := true;

  GUIWidth := Window.Width;
  GUIHeight := Window.Height;
  //GUIScaleW := 1/GUIWidth;
  GUIScale := 1/GUIHeight;

  GUISmallGap := RoundOne(1/768 * GUIHeight);
  GUILargeGap := RoundOne(3/768 * GUIHeight);

  GUIUnit := Round(GUIHeight / 13);

  GUICenter[0] := GUIWidth / 2;
  GUICenter[1] := GUIHeight / 2;
end;


end.

