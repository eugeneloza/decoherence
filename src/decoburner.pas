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
uses CastleImages,
  DecoInterface;

{ Load Burner image and scale it }
procedure InitBurnerImage;
{ Burn the image (works directly on image, no copy!)}
procedure Burn(const aImage: TCastleImage; const x,y,w,h: integer);
procedure Burn(const aImage: TCastleImage; const Container: DAbstractContainer);
{$ENDIF}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$IFDEF BurnerImage}
uses SysUtils,
  DecoLog, Profiler;

var BurnerImageUnscaled, BurnerImage: TCastleImage;  //todo: not freed automatically!!!!
procedure InitBurnerImage;
begin
  StartProfiler;

  {$IFNDEF AllowRescale}if BurnerImage<>nil then Exit;{$ENDIF}
  dLog(LogInitInterface,nil,_CurrentRoutine,'Started');
  if BurnerImageUnscaled = nil then
    BurnerImageUnscaled := LoadImage(ApplicationData(InterfaceFolder+'burner/burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if (BurnerImage=nil) or (BurnerImage.Height <> Window.Height) or (BurnerImage.Width <> Window.Width) then begin
    FreeAndNil(BurnerImage);
    BurnerImage := BurnerImageUnscaled.MakeCopy;
    BurnerImage.Resize(Window.Width, Window.Height, riBilinear);
  end;
  {$IFNDEF AllowRescale}FreeAndNil(BurnerImageUnscaled);{$ENDIF}

  dLog(LogInitInterface,nil,_CurrentRoutine,'Finished');

  StopProfiler;
end;

{working directly on image!}
procedure Burn(const aImage: TCastleImage; const x,y,w,h: integer);
begin
  StartProfiler;

  aImage.DrawFrom(BurnerImage,0,0,x,y,w,h,dmMultiply);

  StopProfiler;
end;
procedure Burn(const aImage: TCastleImage; const Container: DAbstractContainer);
begin
  StartProfiler;

  aImage.DrawFrom(BurnerImage,0,0,Container.x1,Container.y1,Container.w,Container.h,dmMultiply);

  StopProfiler;
end;

finalization
  FreeAndNil(BurnerImageUnscaled);
  FreeAndNil(BurnerImage);
{$ENDIF}

end.

