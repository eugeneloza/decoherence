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

(* Management of mouse cursor/pointer *)

{$INCLUDE compilerconfig.inc}

unit DecoMouseCursor;

interface

uses
  CastleVectors,
  DecoImages, DecoInterfaceCore,
  DecoGlobal;

type
  TCursorType = (ctNone, ctDefault, ctMouseLook);

type
  { Mouse/touch cursor }
  DCursor = class(TObject)
  private
    { A set of images to be used to display cursor in different situations }
    CursorImg: array[TCursorType] of DCursorImage;
  public
    { Coordinates of the cursor }
    x, y: single;
    { Current cursor type (determines displayed cursor image) }
    CurrentCursor: TCursorType;
    { Element this cursor currently drags or clicks }
    DragElement, ClickElement: DSingleInterfaceElement;
    { Change the tint of the cursor }
    procedure SetTint;
    { Draw the cursor on screen }
    procedure Draw;
    { Hides the OS cursor }
    procedure HideOSCursor;
  public
    constructor Create; //override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils, CastleKeysMouse,
  DecoImageLoader,
  DecoWindow;

constructor DCursor.Create;
begin
  //inherited Create; <--------- nothing to inherit

  {todo: remake it into something useful}
  CursorImg[ctDefault] := LoadCursorImage('GUI/Cursors/cursor.png', -1, +1);
  CursorImg[ctMouseLook] := LoadCursorImage('GUI/Cursors/mouselook.png', -15, +15);

  CurrentCursor := ctDefault;
end;

{-----------------------------------------------------------------------------}

destructor DCursor.Destroy;
begin
  Window.SceneManager.Camera.Cursor := mcStandard;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DCursor.HideOSCursor;
begin
  Window.SceneManager.Camera.Cursor := mcForceNone;
  Window.SceneManager.Camera.ExclusiveEvents := false;
  //InitInput will take care of providing cursor coordinates initialization to window center
end;

{-----------------------------------------------------------------------------}

procedure DCursor.Draw;
begin
  if DragElement = Nil then
  begin
    if (not ScreenShotPending) or (not HideMouseCursorInScreenshot) then //hide cursor for screenshots
      if (CurrentCursor <> ctNone) and (CursorImg[CurrentCursor].Image <> Nil) then
        CursorImg[CurrentCursor].Image.Draw(x + CursorImg[CurrentCursor].CursorShift.Data[0],
          y - CursorImg[CurrentCursor].Image.Height + CursorImg[CurrentCursor].CursorShift.Data[1]);
    // and draw hint
  end else
    DragElement.Draw;
end;

{-----------------------------------------------------------------------------}

procedure DCursor.SetTint;
var
  c: TCursorType;
begin
  for c in TCursorType do
    if CursorImg[c].Image <> nil then
      CursorImg[c].Image.Color := GUITint;
end;

end.

