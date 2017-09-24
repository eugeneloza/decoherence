{Copyright (C) 2012-2017 Yevhen Loza

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

{ Image processing: apply burner to frames, actually part of DeoImageProcess,
  but this unit may be completely unused in case the BurnerImage compiler directive
  is not active.}
unit DecoBurner;

{$INCLUDE compilerconfig.inc}

interface

{$IFDEF BurnerImage}
{ Load Burner image and scale it }
procedure InitBurnerImage;
{ Burn the image (works directly on image, no copy!)}
procedure Burn(const aImage: TCastleImage; const x,y,w,h: integer);
procedure Burn(const aImage: TCastleImage; const Container: DAbstractContainer);
{$ENDIF}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

{$IFDEF BurnerImage}
var BURNER_IMAGE_UNSCALED,BURNER_IMAGE:TCastleImage;  //todo: not freed automatically!!!!
procedure InitBurnerImage;
begin
  {$IFNDEF AllowRescale}if BURNER_IMAGE<>nil then exit;{$ENDIF}
  WriteLnLog('Init_burner_image','started');
  if BURNER_IMAGE_UNSCALED = nil then
    BURNER_IMAGE_UNSCALED := LoadImage(ApplicationData(InterfaceFolder+'burner/burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if (BURNER_IMAGE=nil) or (BURNER_IMAGE.height <> window.height) or (BURNER_IMAGE.width <> window.width) then begin
    FreeAndNil(BURNER_IMAGE);
    BURNER_IMAGE := BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width, window.height, riBilinear);
  end;
  {$IFNDEF AllowRescale}FreeAndNil(BURNER_IMAGE_UNSCALED);{$ENDIF}

  WriteLnLog('Init_burner_image','finished');
end;

{working directly on image!}
procedure Burn(const aImage: TCastleImage; const x,y,w,h: integer);
begin
  aImage.DrawFrom(BURNER_IMAGE,0,0,x,y,w,h,dmMultiply);
end;
procedure Burn(const aImage: TCastleImage; const Container: DAbstractContainer);
begin
  aImage.DrawFrom(BURNER_IMAGE,0,0,container.x1,container.y1,container.w,container.h,dmMultiply);
end;
{$ENDIF}

end.

