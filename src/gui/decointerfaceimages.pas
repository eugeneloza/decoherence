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
    procedure Draw; override;
    procedure SetTint; override;
  public
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils;

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
  //inherited SetTint; <---------- parent is abstract
  if Image <> nil then
    Image.Draw(Current.x, Current.y, Current.w, Current.h);
end;

{============================================================================}
{=========================== D SIMPLE IMAGE =================================}
{============================================================================}

end.

