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
  private
    FrameImage: DFrameImage;
    InitPending: boolean;
    procedure ResizeFrame;
  public
    procedure Draw; override;
    procedure Load(const aImage: DFrameImage);
  public
    constructor Create; override;
  end;


{............................................................................}
implementation
uses
  SysUtils,
  CastleImages, CastleVectors, CastleRectangles,
  {$IFDEF BurnerImage}DecoBurner{$ENDIF};

procedure DRectagonalFrame.ResizeFrame;
var
  TempImage: TCastleImage;
begin
  FreeAndNil(Image);
  TempImage := TRGBAlphaImage.Create(Next.w, Next.h);
  TempImage.Clear(Vector4(0,0,0,0));
  TempImage.DrawFrom3x3( Rectangle(0, 0, Next.w, Next.h),
    FrameImage, FrameImage.Corners, dmOverwrite, InterfaceScalingMethod);
  {$IFDEF BurnerImage}Burn(TempImage, Next);{$ENDIF}
  Image := DImage.Create(TempImage, true, true)
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.Draw;
begin
  if InitPending then ResizeFrame;
  inherited Draw;
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.Load(const aImage: DFrameImage);
begin
  FrameImage := aImage;
  InitPending := true;
end;

{-----------------------------------------------------------------------------}

constructor DRectagonalFrame.Create;
begin
  inherited Create;
  InitPending := false;
  OwnsImage := true;
end;

end.

