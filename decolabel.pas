{Copyright (C) 2012-2016 Yevhen Loza

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

unit decolabel;

{$mode objfpc}{$H+}

interface

uses classes,
  decoimages, decofont,
  decoglobal;

Type DLabel = class(DAbstractImage)
 public
  RealWidth,RealHeight: integer;
  Font: DFont;
  Shadow: Float;
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 private
  procedure PrepareTextImage;
  procedure settext(const value: string);
  function gettext: string;
 public
  property text: string read gettext write settext;
 private
  ftext:  string;
  BrokenString: DStringList;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation

uses sysutils;

constructor DLabel.create(AOwner : TComponent);
begin
  inherited create(AOwner);
  Shadow := 0;
end;

destructor DLabel.Destroy;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  inherited
end;

procedure DLabel.settext(const value : string);
begin
  if ftext<>value then begin
    ftext := value;
    PrepareTextImage;
  end;
end;

function DLabel.gettext : string;
begin
  result := ftext;
end;

procedure DLabel.PrepareTextImage;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  BrokenString := font.break_stings(text,base.w);
  FreeImage;

  // for i := 0 to brokenString.count-1 do writeLnLog('',inttostr(brokenstring[i].height));

//  SourceImage := nil; // let it be as a safeguard here. I don't want to freeannil GImage before it is instantly created to avoid sigsegvs

  if shadow = 0 then
    SourceImage := font.broken_string_to_image(BrokenString)
  else
    SourceImage := font.broken_string_to_image_with_shadow(BrokenString,shadow,3);

  RealHeight := SourceImage.height;
  RealWidth := sourceImage.width;

  InitGLPending := true;
  ImageLoaded := true;     //not good...
  //Rescale;
  ScaledImage := SourceImage.MakeCopy;
  base.w := RealWidth;
  base.h := RealHeight;
end;

end.

