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

(* A highets-level container for the interface and its routines *)

unit DecoGUI;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleGlImages, //temp
  DecoGlobal, DecoGUIScale, DecoInterfaceCore;

type
  DCursor = class(TObject)
  private
    CursorImg: TGLImage;
  public
    x, y: single;
    procedure Draw;
    constructor Create; //override;
    destructor Destroy; override;
  end;

type
  { GUI container, manages all other GUI elements }
  DGUI = class(DInterfaceElement)
  private
    Cursor: DCursor;
  public
    procedure ShowMessage(const aMessage: string);
  public
    procedure Draw; override;
    procedure UpdateCursor(const CursorX, CursorY: single);
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
  CastleImages, CastleFilesUtils, //temp
  CastleKeysMouse,
  SysUtils,
  DecoTime, DecoLog, DecoWindow;

procedure DGUI.ShowMessage(const aMessage: string);
begin
  Log(true, CurrentRoutine, aMessage); //temporary
  { will create a message window with text }
end;

{-----------------------------------------------------------------------------}

procedure DGUI.Draw;
begin
  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
//  if GameModeNeedsClearingScreen then
//    RenderContext.Clear([cbColor], Black);

  { draw children elements }
  //inherited Draw;

  Cursor.Draw;

  { draw special elements }
  //FPSLabel.CountFPS;
end;

{-----------------------------------------------------------------------------}

procedure DGUI.UpdateCursor(const CursorX, CursorY: single);
begin
  Cursor.x := CursorX;
  Cursor.y := CursorY;
end;

{-----------------------------------------------------------------------------}

constructor DGUI.Create;
begin
  inherited Create;
  Cursor := DCursor.Create;
  //...
end;

{-----------------------------------------------------------------------------}

destructor DGUI.Destroy;
begin
  FreeAndNil(Cursor);
  //...
  inherited Destroy;
end;

{============================================================================}

procedure DCursor.Draw;
begin
  CursorImg.Draw(x, y - CursorIMG.Height);
end;

{-----------------------------------------------------------------------------}

constructor DCursor.Create;
begin
  //inherited Create;
  //Window.SceneManager.Camera.Cursor := mcForceNone;
  CursorImg := TGLImage.Create(LoadImage(ApplicationData('GUI/Cursors/cursor.png')), true, true);
end;

{-----------------------------------------------------------------------------}

destructor DCursor.Destroy;
begin
  Window.SceneManager.Camera.Cursor := mcStandard;
  FreeAndNil(CursorImg);
  inherited Destroy;
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

