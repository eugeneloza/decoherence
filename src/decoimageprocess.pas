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

{ TCastleImage processing }
unit DecoImageProcess;

{$INCLUDE compilerconfig.inc}
interface

uses CastleImages,
  DecoGlobal;

{
 creates a copy of the image!}
function Brighter(const aImage: TCastleImage; const mult: float = 1.2): TCastleImage;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleLog, CastleVectors;

function CheckImageValid(aImage: TCastleImage): boolean;
begin
  Result := true;
  if (aImage=nil) or (aImage.isEmpty) then begin
    Result := false;
    WriteLnLog('DecoImageProcess>','FATAL: Image is nil');
    Exit;
  end;
  if not (aImage is TRGBAlphaImage) then begin
    Result := false;
    WriteLnLog('DecoImageProcess>','FATAL: Image type '+aImage.ClassName+' is not supported for operation.');
    Exit;
  end;
end;

{------------------------------------------------------------------------}

function Clamp255(a: integer): byte;
begin
  if a<255 then Result := a else Result := 255;
end;

{------------------------------------------------------------------------}

function Brighter(const aImage: TCastleImage; const mult: float = 1.2): TCastleImage;
var IMG: TRGBAlphaImage;
  p: PVector4byte;
  i: integer;
begin
  if not CheckImageValid(aImage) then Exit;
  IMG := aImage.MakeCopy as TRGBAlphaImage;
  p := IMG.Pixels;
  for i := 0 to (IMG.Size div IMG.PixelSize) do begin
    p^[0] := Clamp255(Round(p^[0]*mult));
    p^[1] := Clamp255(Round(p^[1]*mult));
    p^[2] := Clamp255(Round(p^[2]*mult));
    {no alpha processing?}
    Inc(p,IMG.PixelSize);
  end;
  Result := IMG;
end;

{------------------------------------------------------------------------}

(*var BURNER_IMAGE_UNSCALED,BURNER_IMAGE:TCastleImage;  //todo: not freed automatically!!!!
procedure Init_burner_image;
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
*)

end.

