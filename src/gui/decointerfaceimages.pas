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

(* Different types of interface images *)

unit DecoInterfaceImages;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleGLImages, CastleImages, CastleVectors,
  DecoInterfaceCore, DecoImages,
  DecoGlobal, DecoTime;

const
  { To quickly change Scaling Method. Maybe will be a variable some day to support older PCs. }
  InterfaceScalingMethod: TResizeInterpolation = riBilinear;

type
  { General routines shared by images, frames and labels }
  DAbstractImage = class abstract(DSingleInterfaceElement)
  strict protected
    { GL Image displayed by this interface element, may be animated }
    Image: DImage;
  public
    //property RealWidth: integer read GetWidth;
    //property RealHeight: integer read GetHeight;
    procedure Draw; override;
    procedure SetTint; override;
  public
    destructor Destroy; override;
  end;

type
  { Most basic image type, capable of loading and scaling }
  DSimpleImage = class(DAbstractImage)
  public
    { Load the image and scale it to given aWidth and aHeight if non-zero
      Doesn't claim ownership of the image! }
    procedure Load(const aImage: TEncodedImage; const aWidth: integer = 0;
      const aHeight: integer = 0; const KeepProportions: boolean = false);
  end;


{............................................................................}
implementation
uses
  SysUtils,
  DecoLog;

{============================================================================}
{========================== D ABSTRACT IMAGE ================================}
{============================================================================}

destructor DAbstractImage.Destroy;
begin
  FreeAndNil(Image);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.SetTint;
begin
  //inherited SetTint; <---------- parent is abstract
  if Image <> nil then begin
    Update;
    Image.Color := GUITint;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.Draw;
begin
  //inherited Draw; <---------- parent is abstract
  if Image <> nil then
    Image.Draw(Current.x, Current.y, Current.w, Current.h);
end;

{============================================================================}
{=========================== D SIMPLE IMAGE =================================}
{============================================================================}

procedure DSimpleImage.Load(const aImage: TEncodedImage; const aWidth: integer = 0;
  const aHeight: integer = 0; const KeepProportions: boolean = false);
var
  ScaledImage: TCastleImage;
  ScaledWidth, ScaledHeight: integer;
begin
  if (aImage = nil) or (aImage.IsEmpty) then
  begin
    Log(LogImageScaleError, CurrentRoutine, 'ERROR: No image to load.');
    Exit;
  end;

  FreeAndNil(Image);
  if aWidth = 0 then
    Image := DImage.Create(aImage, true, false)
  else
  begin
    if not (aImage is TCastleImage) then
    begin
      Log(LogImageScaleError, CurrentRoutine, 'ERROR: Cannot Scale image ' + aImage.ClassName);
      Exit;
    end;

    ScaledWidth := aWidth;
    ScaledHeight := aHeight;
    if KeepProportions then
    begin
      if aHeight / aWidth > aImage.Height / aImage.Width then
        ScaledHeight := Round(aHeight * aImage.Height / aImage.Width)
      else
        ScaledWidth := Round(aWidth * aImage.Width / aImage.Height);
    end;
    ScaledImage := aImage.CreateCopy as TCastleImage;
    ScaledImage.Resize(ScaledWidth, ScaledHeight, InterfaceScalingMethod);

    Image := DImage.Create(ScaledImage, true, true); //now Image owns the content because it's a copy
    //ScaledImage := nil; //redundant
  end;
end;

end.

