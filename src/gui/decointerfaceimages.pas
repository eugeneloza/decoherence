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

(* Basic types of interface images *)

{$INCLUDE compilerconfig.inc}

unit DecoInterfaceImages;

interface

uses
  DecoInterfaceCore, DecoImages,
  DecoGlobal;

type
  { General routines shared by images, frames and labels }
  DAbstractImage = class abstract(DSingleInterfaceElement)
  strict private
    function GetWidth: integer;
    function GetHeight: integer;
  strict protected
    { GL Image displayed by this interface element, may be animated }
    Image: DImage;
    { Is this image "owned" by parent? False for interface images
      and true for Labels (generated temporary images) }
    OwnsImage: boolean;
  public
    property RealWidth: integer read GetWidth;
    property RealHeight: integer read GetHeight;
    procedure ResetToRealSize(const ResetAnim: boolean = false);
    procedure Draw; override;
    procedure SetTint; override;
  public
    destructor Destroy; override;
  end;

type
  { Most basic image type, ready to display the provided image
    Warning: for this image type "image" is only a reference
    and must not be freed (specified by OwnsImage = false) }
  DSimpleImage = class(DAbstractImage)
  public
    { Load the image. Doesn't claim ownership of the image! }
    procedure Load(const aImage: DImage);
  public
    constructor Create; override;
  end;

type
  {}
  DFullScreenImage = class(DAbstractImage)
  public
    { Load the image. Doesn't claim ownership of the image! }
    procedure Load(const aImage: DImage);
  public
    constructor Create; override;
  end;
{............................................................................}
implementation
uses
  SysUtils, CastleGLImages,
  Profiler;

{============================================================================}
{========================== D ABSTRACT IMAGE ================================}
{============================================================================}

destructor DAbstractImage.Destroy;
begin
  if OwnsImage then
    FreeAndNil(Image);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.SetTint;
begin
  //inherited SetTint; <---------- parent is an "empty" virtual procedure
  if Image <> nil then
    Image.Color := GUITint;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.Draw;
begin
  {StartProfiler}

  //inherited Draw; <---------- parent is abstract
  if Image <> nil then
  begin
    Update;
    Image.SetAlpha(Self.Alpha);
    Image.Draw(Current.x, Current.y, Current.w, Current.h);
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

function DAbstractImage.GetWidth: integer;
begin
  if (Image <> nil) then
    Result := Image.Width
  else
    Result := -1;
end;
function DAbstractImage.GetHeight: integer;
begin
  if Image <> nil then
    Result := Image.Height
  else
    Result := -1;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.ResetToRealSize(const ResetAnim: boolean = false);
begin
  {StartProfiler}

  Next.SetIntWidthHeight(GetWidth, GetHeight, Next.a);
  if ResetAnim then ResetAnimation;

  {StopProfiler}
end;

{============================================================================}
{=========================== D SIMPLE IMAGE =================================}
{============================================================================}

constructor DSimpleImage.Create;
begin
  inherited Create;
  OwnsImage := false;
end;

{-----------------------------------------------------------------------------}

procedure DSimpleImage.Load(const aImage: DImage);
begin
  Image := aImage;
  SetTint;
end;

{============================================================================}

constructor DFullScreenImage.Create;
begin
  inherited Create;
  OwnsImage := false;
end;

{-----------------------------------------------------------------------------}

procedure DFullScreenImage.Load(const aImage: DImage);
begin
  Image := aImage;
  SetTint;
  FullScreen;
end;

end.

