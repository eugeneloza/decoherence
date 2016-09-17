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
unit DecoLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleFonts, CastleVectors, CastleFontFamily,
  DecoFont,
  CastleLog, castleFilesUtils,
  CastleImages, CastleGLImages,
  decointerface,
  decoglobal;

{HTML disabled at the moment}
Type DLabel=class(DAbstractElement)
 public
  text: string;            //todo read ftext write ftext
  Font:DFont;
  Shadow:Float;
  constructor Create(AOwner:TComponent); override;
  {destructor} Procedure DestroyMe; override;
  procedure DrawMe; override;
  procedure InitGL; override;
 private
  BrokenString:DStringList;
  GImage:TGLImage;
  SourceImage:TGrayscaleAlphaImage;
end;


implementation

constructor DLabel.create(AOwner:TComponent);
begin
  inherited create(AOwner);
  Color:=Vector4Single(1,1,1,1);
  Opacity:=1;
  Shadow:=0;
end;

procedure DLabel.DestroyMe;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  FreeAndNil(GImage);
end;

procedure DLabel.InitGL;
//var i:integer;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  BrokenString:=font.break_stings(text,w);

  // for i:=0 to brokenString.count-1 do writeLnLog('',inttostr(brokenstring[i].height));

  if shadow=0 then
    SourceImage:=font.broken_string_to_image(BrokenString)
  else
    SourceImage:=font.broken_string_to_image_with_shadow(BrokenString,shadow,3);

  h:=SourceImage.height;

  FreeAndNil(GImage);
  GImage:=TGLImage.create(SourceImage,true,true);
  SourceImage:=nil;
end;

procedure DLabel.DrawMe;
begin
  if (GImage<>nil) then begin
    Color[3]:=Opacity;
    GImage.color:=color;
    GImage.Draw(x,y);
  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

