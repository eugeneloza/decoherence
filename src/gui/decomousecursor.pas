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

(* Management of mouse cursor/pointer *)

unit DecoMouseCursor;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleGlImages;

type TCursorType = (ctNone, ctDefault);

type
  DCursor = class(TObject)
  private
    CursorImg: array[TCursorType] of TGLImage;
  public
    x, y: single;
    CurrentCursor: TCursorType;
    procedure Draw;
    procedure HideOSCursor;
    constructor Create; //override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils, CastleImages, CastleFilesUtils, CastleKeysMouse,
  DecoWindow;

constructor DCursor.Create;
begin
  //inherited Create;
  CursorImg[ctDefault] := TGLImage.Create(LoadImage(ApplicationData('GUI/Cursors/cursor.png')), true, true);
  CurrentCursor := ctDefault;
end;

{-----------------------------------------------------------------------------}

destructor DCursor.Destroy;
var
  c: TCursorType;
begin
  Window.SceneManager.Camera.Cursor := mcStandard;
  for c in TCursorType do
    FreeAndNil(CursorImg[c]);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DCursor.HideOSCursor;
begin
  Window.SceneManager.Camera.Cursor := mcForceNone;
end;

{-----------------------------------------------------------------------------}

procedure DCursor.Draw;
begin
  if CurrentCursor <> ctNone then
    CursorImg[CurrentCursor].Draw(x, y - CursorImg[CurrentCursor].Height);
end;

end.

