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

unit DecoMouseCursor;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleVectors,
  DecoImages,
  DecoGlobal;

type TCursorType = (ctNone, ctDefault, ctMouseLook,
  ctDefault_pressed);

type
  DCursor = class(TObject)
  private
    CursorImg: array[TCursorType] of DImage;
    CursorShift: array[TCursorType] of TVector2Integer;
  public
    x, y: single;
    CurrentCursor: TCursorType;
    procedure SetTint;
    procedure Draw;
    procedure HideOSCursor;
    constructor Create; //override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils, CastleKeysMouse,
  DecoImageLoader, DecoWindow;

constructor DCursor.Create;
begin
  //inherited Create; <--------- nothing to inherit

  {todo: remake it into something useful}
  CursorImg[ctDefault] := LoadDecoImage('GUI/Cursors/cursor.png');
  CursorShift[ctDefault].Data[0] := -1;
  CursorShift[ctDefault].Data[1] := +1;
  CursorImg[ctDefault_pressed] := LoadDecoImage('GUI/Cursors/cursor_pressed.png');
  CursorShift[ctDefault_pressed].Data[0] := -1;
  CursorShift[ctDefault_pressed].Data[1] := +1;
  CursorImg[ctMouseLook] := LoadDecoImage('GUI/Cursors/mouselook.png');
  CursorShift[ctMouseLook].Data[0] := -15;
  CursorShift[ctMouseLook].Data[1] := +15;
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
  //InitInput will take care of providing cursor coordinates initialization
end;

{-----------------------------------------------------------------------------}

procedure DCursor.Draw;
begin
  if not ScreenShotPending then //hide cursor for screenshots
    if CurrentCursor <> ctNone then
      CursorImg[CurrentCursor].Draw(x + CursorShift[CurrentCursor].Data[0],
        y - CursorImg[CurrentCursor].Height + CursorShift[CurrentCursor].Data[1]);
end;

{-----------------------------------------------------------------------------}

procedure DCursor.SetTint;
var
  c: TCursorType;
begin
  for c in TCursorType do
    if CursorImg[c] <> nil then
      CursorImg[c].Color := GUITint;
end;

end.

