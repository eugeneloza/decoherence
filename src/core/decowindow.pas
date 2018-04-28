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

(* Handles window size and scale
   !!! NO RESCALES ALLOWED DURING THE GAME !!!
   Changing FullScreen mode or Resolution only by rebooting! *)

{$INCLUDE compilerconfig.inc}

unit DecoWindow;

interface

uses
  CastleWindow;

var
  { Main window of the game }
  Window: TCastleWindow;

  ScreenShotPending: boolean;

{ Creates and initializes the Window }
procedure InitWindow;
{ Make a screenshot of current screen and save it to a file }
procedure MakeScreenShot;
{............................................................................}
implementation

uses
  SysUtils,
  CastleApplicationProperties,
  DOM, DecoFiles, DecoFolders,
  DecoGUIScale,
  DecoLog, DecoTime;

var
  ConfigFullScreen: boolean;
  ConfigWindowWidth: integer;
  ConfigWindowHeight: integer;

procedure MakeScreenShot;
begin
  ScreenShotPending := true;
  //careful: onBeforeRender and onRender are called here.
  Window.SaveScreen('Deco_' + NiceDate + '.png');
  ScreenShotPending := false;
end;


{.......................................................................}

procedure WriteWindowConfiguration;
var
  RootNode: TDOMElement;
begin
  RootNode := CreateFile(GameConfigFolder('Window.xml'));
  WriteBoolean(RootNode, 'FullScreen', ConfigFullScreen);
  WriteInteger(RootNode, 'Width', ConfigWindowWidth);
  WriteInteger(RootNode, 'Height', ConfigWindowHeight);
  WriteFile;
end;

{----------------------------------------------------------------------------}

procedure ReadWindowConfiguration;
  procedure DefaultWindowConfig;
  begin
    ConfigFullScreen := false;
    ConfigWindowWidth := 1024;
    ConfigWindowHeight := 600;
  end;
var
  RootNode: TDOMElement;
begin
  RootNode := StartReadFile(GameConfigFolder('Window.xml'));
  if RootNode = nil then
  begin
    DefaultWindowConfig;
    //WriteWindowConfiguration; //it's default, no need to write it
  end else
  begin
    { Read config from a file }
    ConfigFullScreen := ReadBoolean(RootNode, 'FullScreen');
    ConfigWindowWidth := ReadInteger(RootNode, 'Width');
    ConfigWindowHeight := ReadInteger(RootNode, 'Height');
    EndReadFile;
  end;
end;

{----------------------------------------------------------------------------}

procedure InitWindow;
begin
  Window := TCastleWindow.Create(Application);
  Window.DoubleBuffer := true;
  Window.ResizeAllowed := raOnlyAtOpen;

  ReadWindowConfiguration;

  Window.FullScreen := ConfigFullScreen;
  if ConfigFullScreen then
  begin
    Log(LogInit, CurrentRoutine, 'FullScreen mode ON');
  end else
  begin
    Log(LogInit, CurrentRoutine, 'FullScreen mode OFF, Window resolution = ' +
      ConfigWindowWidth.ToString + 'x' + ConfigWindowHeight.ToString);
    Window.Width := ConfigWindowWidth;
    Window.Height := ConfigWindowHeight;
  end;

  ApplicationProperties.LimitFPS := 0;

  ResetGUIScale;

  ScreenShotPending := false;
end;

end.

