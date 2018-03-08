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
  DecoImages,
  DecoInterfaceImages,
  DecoGlobal;

type
  { Just an alias to be able to assign a DSimpleImage as a frame
    However, in some time, we might want to add "burner" to
    non-rectagonal frames. Thou I'm unsure if it worth the trouble. }
  DFrame = DAbstractImage;

type
  { Rectagonal frame is scaled 3x3 and accepts DFrameImage}
  DRectagonalFrame = class(DFrame)
  public
    procedure Load(const aImage: DFrameImage);
  end;


{............................................................................}
implementation

procedure DRectagonalFrame.Load(const aImage: DFrameImage);
begin

end;

end.

