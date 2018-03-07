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

(* Handles Frames of the interface elements:
   Image frames - drawn as simple underlying image
   Rectagonal frames - drawn as Draw3x3 *)

unit DecoFrames;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceImages,
  DecoGlobal;

type
  { Just an alias to be able to assign a DSimpleImage as a frame }
  DFrame = DSimpleImage;

type
  DRectagonalFrame = class(DFrame)
  public
    procedure Draw; override;
  end;


{............................................................................}
implementation

procedure DRectagonalFrame.Draw;
begin
  //inherited Draw; <------- this draw replaces parent method
  if Image <> nil then
  begin
    Update;
    Image.Draw(Current.x, Current.y, Current.w, Current.h);
  end;
end;

end.

