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

(* A stuipd wrapper for CastleGlImages with hope to provide for
   equal API for both animated and non-animated GL images *)

{$INCLUDE compilerconfig.inc}

unit DecoImages;

interface

uses
  Generics.Collections,
  CastleGLImages, CastleImages, CastleColors, CastleVectors,
  DecoGlobal;

type
  { Wrapper for TGLImage }
  DImage = class(DObject)
  strict private
    FImage: TGLImage;
    procedure SetColor(const aColor: TCastleColor);
  public
    { Cached values of width/height }
    Width, Height: integer;
    { Assign tint color of this image }
    property Color: TCastleColor write SetColor;
    procedure SetAlpha(const aValue: Single); TryInline
    { Wrappers for Drawing TGLImage }
    procedure Draw(const X, Y: Single); TryInline
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single); TryInline
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); TryInline
    procedure Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
      const CornerTop, CornerRight, CornerBottom, CornerLeft: Integer); TryInline
  public
    constructor Create(const AImage: TEncodedImage; const ASmoothScaling: boolean = true;
      const AOwnsImage: boolean = true);
    destructor Destroy; override;
  end;

type
  { At this point there is absolutely no difference between the two,
    However, in future AnimatedImage will implement looping between
    multiple FImages during some time, however, I'm not yet sure, how to
    load it. Maybe, it'd be something like loading from XML
    with determination of frame files, and frame durations. }
  DAnimatedImage = class(DImage)
  end;

type
  { A simple CastleImage with Corners attached }
  DFrameImage = class(DObject)
  public
    Image: TCastleImage;
    Corners: TVector4Integer;
  end;

type
  { Image supplemented by a predefined shift from the original coordinates
    to provide for accurate cursor image positioning relative to pointer coordinates }
  DCursorImage = record
    Image: DImage;
    { Coordinates shift of the cursor image vs cursor pointer position }
    CursorShift: TVector2Integer;
  end;

type
  TImagesDictionary = specialize TObjectDictionary<string, DImage>;

var
  ImagesDictionary: TImagesDictionary;

function GetImageByName(const ItemName: string): DImage; TryInline
{............................................................................}
implementation
uses
  DecoLog, Profiler;

function GetImageByName(const ItemName: string): DImage; TryInline
begin
  {StartProfiler}

  Result := nil; //to avoid uninitialized variable hint
  if not ImagesDictionary.TryGetValue(ItemName, Result) then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'Unknown Image Name: ' + ItemName);
    Result := nil;
  end;

  {StopProfiler}
end;

{=============================================================================}

procedure DImage.Draw(const X, Y: Single); TryInline
begin
  FImage.Draw(X, Y);
end;

{-----------------------------------------------------------------------------}

procedure DImage.Draw(const X, Y, DrawWidth, DrawHeight: Single); TryInline
begin
  FImage.Draw(X, Y, DrawWidth, DrawHeight);
end;

{-----------------------------------------------------------------------------}

procedure DImage.Draw(const X, Y, DrawWidth, DrawHeight: Single;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single); TryInline
begin
  FImage.Draw(X, Y, DrawWidth, DrawHeight, ImageX, ImageY, ImageWidth, ImageHeight);
end;

{-----------------------------------------------------------------------------}

procedure DImage.Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
      const CornerTop, CornerRight, CornerBottom, CornerLeft: Integer); TryInline
begin
  FImage.Draw3x3(X, Y, DrawWidth, DrawHeight, CornerTop, CornerRight, CornerBottom, CornerLeft);
end;

{-----------------------------------------------------------------------------}

procedure DImage.SetColor(const aColor: TCastleColor);
begin
  FImage.Color := aColor;
end;

{-----------------------------------------------------------------------------}

procedure DImage.SetAlpha(const aValue: Single); TryInline
begin
  FImage.Color[3] := aValue;
end;

{-----------------------------------------------------------------------------}

constructor DImage.Create(const AImage: TEncodedImage; const ASmoothScaling: boolean = true;
  const AOwnsImage: boolean = true);
begin
  {StartProfiler}

  if (AImage = nil) or (AImage.IsEmpty) then
    Log(LogInterfaceError, CurrentRoutine, 'Error: Input image is nil or empty!');
  FImage := TGLImage.Create(AImage, ASmoothScaling, AOwnsImage);
  Width := FImage.Width;
  Height := FImage.Height;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

destructor DImage.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

end.

