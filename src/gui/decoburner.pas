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

(* Image processing: apply burner to frames, actually part of DeoImageProcess,
   but this unit may be completely unused in case the BurnerImage compiler directive
   is not active. *)

{$INCLUDE compilerconfig.inc}

unit DecoBurner;

interface

{$IFDEF BurnerImage}
uses
  CastleImages,
  DecoInterfaceContainer;

{ Load Burner image and scale it }
procedure InitBurnerImage;
{ Burn the image (works directly on image, no copy!)}
procedure Burn(const aImage: TCastleImage; const x, y, w, h: integer);
procedure Burn(const aImage: TCastleImage; const aContainer: DInterfaceContainer);
{$ENDIF}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

{$IFDEF BurnerImage}
uses
  SysUtils,
  DecoGUISCale,
  DecoImageLoader, DecoLog;

var
  BurnerImage: TCastleImage;  //todo: not freed automatically!!!!

procedure Burn(const aImage: TCastleImage; const x, y, w, h: integer);
begin
  {working directly on image!}
  aImage.DrawFrom(BurnerImage, 0, 0, x, y, w, h, dmMultiply);
end;

procedure Burn(const aImage: TCastleImage; const aContainer: DInterfaceContainer);
begin
  {working directly on image!}
  aImage.DrawFrom(BurnerImage, 0, 0, aContainer.x, aContainer.y, aContainer.w,
    aContainer.h, dmMultiply);
end;

{............................................................................}

procedure InitBurnerImage;
begin
  Log(LogInit, CurrentRoutine, 'Loading burner...');
  BurnerImage := LoadCastleImage('GUI/Burner/abstract-background-1523717636ybk_cc0_by_Linnaea_Mallette_[burner].jpg');
  BurnerImage.Resize(GUIWidth, GUIHeight, riBilinear);
end;

{$ENDIF}

end.

