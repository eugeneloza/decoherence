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
uses DecoLog;

{$IFDEF BurnerImage}
var BurnerImageUnscaled,BurnerImage:TCastleImage;  //todo: not freed automatically!!!!
procedure InitBurnerImage;
begin
  {$IFNDEF AllowRescale}if BurnerImage<>nil then exit;{$ENDIF}
  dLog(LogInitInterface,nil,'InitBurnerImage','Started');
  if BurnerImageUnscaled = nil then
    BurnerImageUnscaled := LoadImage(ApplicationData(InterfaceFolder+'burner/burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if (BurnerImage=nil) or (BurnerImage.height <> window.height) or (BurnerImage.width <> window.width) then begin
    FreeAndNil(BurnerImage);
    BurnerImage := BurnerImageUnscaled.MakeCopy;
    BurnerImage.Resize(window.width, window.height, riBilinear);
  end;
  {$IFNDEF AllowRescale}FreeAndNil(BurnerImageUnscaled);{$ENDIF}

  dLog(LogInitInterface,nil,'InitBurnerImage','Finished');
end;

{working directly on image!}
procedure Burn(const aImage: TCastleImage; const x,y,w,h: integer);
begin
  aImage.DrawFrom(BurnerImage,0,0,x,y,w,h,dmMultiply);
end;
procedure Burn(const aImage: TCastleImage; const Container: DAbstractContainer);
begin
  aImage.DrawFrom(BurnerImage,0,0,Container.x1,Container.y1,Container.w,Container.h,dmMultiply);
end;
{$ENDIF}

end.

