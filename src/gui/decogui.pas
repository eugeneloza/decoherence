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

(* A highets-level container for the interface and its routines *)

unit DecoGUI;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceCore, DecoMouseCursor,
  {<temporary included for debugging>}
  DecoInterfaceImages,
  DecoWind,
  DecoFrames,
  DecoInterfaceBars,
  {</temporary>}
  DecoGlobal, DecoGUIScale;

type
  { GUI container, manages all other GUI elements }
  DGUI = class(DInterfaceElement)
  strict private
    Cursor: DCursor;
  strict private
    FFirstRender: boolean;
    { Some post-initialization routines, that require graphics context fully available }
    procedure FirstRender;
  public
    { A pop-up window, showing a message }
    procedure ShowMessage(const aMessage: string);
  public
    { (Re)sets the tint of the whole interface }
    procedure SetTint; override;
    { Draw the GUI and all its child elements }
    procedure Draw; override;
    { Updates cursor position and image }
    procedure UpdateCursor(const CursorX, CursorY: single; const MousePressed: boolean);
  public
    procedure TestInterface;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GUI: DGUI;

{ Initialize GUI instance }
procedure InitGUI;
{ Free GUI instance }
procedure FreeGUI;
{............................................................................}
implementation
uses
  SysUtils, CastleColors, CastleGLUtils,
  DecoPlayer, DecoGameMode,
  DecoLog;

procedure DGUI.ShowMessage(const aMessage: string);
begin
  Log(true, CurrentRoutine, aMessage); //temporary
  { will create a message window with text }
end;

{-----------------------------------------------------------------------------}

procedure DGUI.Draw;
begin
  if FFirstRender then FirstRender;

  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
  if GameModeNeedsClearingScreen then
    RenderContext.Clear([cbColor], Black);

  { draw children elements }
  inherited Draw;

  Cursor.Draw;

  { draw special elements }
  //FPSLabel.CountFPS;
end;

{-----------------------------------------------------------------------------}

procedure DGUI.UpdateCursor(const CursorX, CursorY: single; const MousePressed: boolean);
begin
  if Player.MouseLook then
    Cursor.CurrentCursor := ctMouseLook
  else
  begin
    if MousePressed then
      Cursor.CurrentCursor := ctDefault_Pressed
    else
      Cursor.CurrentCursor := ctDefault;
  end;

  if Player.MouseLook then
  begin
    Cursor.x := GUICenter[0];
    Cursor.y := GUICenter[1];
  end
  else
  begin
    Cursor.x := CursorX;
    Cursor.y := CursorY;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DGUI.SetTint;
begin
  inherited SetTint;
  Cursor.SetTint;
end;

{-----------------------------------------------------------------------------}

procedure DGUI.FirstRender;
begin
  FFirstRender := false;
  Cursor.HideOSCursor;
end;

{-----------------------------------------------------------------------------}

constructor DGUI.Create;
begin
  inherited Create;
  FullScreen;

  GUITint := White;

  FFirstRender := true;
  Cursor := DCursor.Create;

  SetTint;

  TestInterface;
end;

{-----------------------------------------------------------------------------}

destructor DGUI.Destroy;
begin
  FreeAndNil(Cursor);
  //...
  inherited Destroy;
end;

{===================== SPECIFIC INTERFACE KINDS ============================}

procedure DGUI.TestInterface;
var
  Frame: DRectagonalFrame;
begin
  Clear;
  Grab(DWind.Create);
  Frame := DRectagonalFrame.Create;
  Frame.SetSize(100, 100, 300, 300, 0.9, asZoomIn);
  Frame.Load(GetFrameByName('RegularFrame'));
  Grab(Frame);
end;

{............................................................................}

procedure InitGUI;
begin
  GUI := DGUI.Create;
end;

{-----------------------------------------------------------------------------}

procedure FreeGUI;
begin
  GUI.Free;
end;

end.

