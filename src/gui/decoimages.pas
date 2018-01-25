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

unit DecoImages;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleGLImages, CastleImages, CastleColors;

type
  { Wrapper for TGLImage }
  DImage = class(TObject)
  strict private
    FImage: TGLImage;
    procedure SetColor(const aColor: TCastleColor);
  public
    Width, Height: integer;
    property Color: TCastleColor write SetColor;
    procedure Draw(const X, Y: Single); TryInline
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single); TryInline
  public
    constructor Create(const AImage: TEncodedImage; const ASmoothScaling: boolean = True;
      const AOwnsImage: boolean = True);
    destructor Destroy; override;
  end;

type
  { At this point there is absolutely no difference between the two,
    However, in future AnimatedImage will implement looping between
    multiple FImages during some time, however, I'm not yet sure, how to
    load it. Maybe, it'd be something like loading from XML
    with determination of frame files, and frame durations }
  DAnimatedImage = class(DImage)
  end;

{............................................................................}
implementation

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

procedure DImage.SetColor(const aColor: TCastleColor);
begin
  FImage.Color := aColor;
end;

{-----------------------------------------------------------------------------}

constructor DImage.Create(const AImage: TEncodedImage; const ASmoothScaling: boolean = true;
  const AOwnsImage: boolean = True);
begin
  FImage := TGLImage.Create(AImage, ASmoothScaling, AOwnsImage);
  Width := FImage.Width;
  Height := FImage.Height;
end;

{-----------------------------------------------------------------------------}

destructor DImage.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

end.

