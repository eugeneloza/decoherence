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
unit DecoFont;

{$mode objfpc}{$H+}

interface

uses
  sysutils, fgl,
  {$IFDEF Android}
  castletexturefont_linbiolinumrg_16,
  {$ENDIF}
  CastleFonts, CastleUnicode, CastleStringUtils,
  CastleImages,CastleTextureFontData, castleVectors, castlecolors,
  CastleLog, castleFilesUtils;

{$IFNDEF Android}
const NormalFontFile='interface/fonts/LinBiolinum_R_G.ttf';
{$ENDIF}

const dlinebreak=slinebreak;

type DString=class(TObject)
  {each line text content}
  value:String;
  {specific size parameters of this line}
  width,height,heightbase:integer;
end;
type DStringList = specialize TFPGObjectList<DString>;

Type DFont=class(TTextureFont)
  {Converts a broken string into a single image}
  function broken_string_to_image(const s:DStringList):TGrayscaleAlphaImage;
  {Converts a broken string into a single image with shadow}
  function broken_string_to_image_with_shadow(const s:DStringList;shadow_strength:single;shadow_length:integer):TGrayscaleAlphaImage;
  {Breaks a string to a DStringList}
  function break_stings(const s:String;const w:integer):DStringList;
 private
  //todo :RGB Alpha image;
  {Converts a single line of text to an image}
  function string_to_image(const s:string):TGrayscaleAlphaImage;
end;

var {$IFNDEF Android}MyCharSet:TUnicodeCharList;{$ENDIF}
  RegularFont16:DFont;

procedure InitializeFonts;

implementation

{------------------------------------------------------------------------------}

procedure InitializeFonts;
begin
  WritelnLog('DecoFont:InitializeFonts','Init started');
   {$IFDEF Android}
   RegularFont16:=DFont.Create(TextureFont_LinBiolinumRG_16);
   {$ELSE}
   if MyCharSet=nil then begin
      MyCharSet:=TUnicodeCharList.Create;
      MyCharSet.add(SimpleAsciiCharacters);
      MyCharSet.add('ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
   end;
   RegularFont16:=DFont.Create(ApplicationData(NormalFontFile),16,true,MyCharSet);
   {$ENDIF}
   WritelnLog('DecoFont:InitializeFonts','Fonts loaded successfully.');
end;

{-----------------------------------------------------------------------------}

function DFont.string_to_image(const s:string):TGrayscaleAlphaImage;
var P:Pvector2byte;
    i:integer;
begin
  Result:=TGrayscaleAlphaImage.create;
  Result.setsize(TextWidth(s),TextHeight(s));  //including baseline
  Result.Clear(Vector2Byte(0,255));

  PushProperties; // save previous TargetImage value
  TargetImage := Result;
  Print(0, TextHeight(s)-TextHeightBase(s), White, S);       //shift text up from a baseline
  PopProperties; // restore previous TargetImage value

  //reset alpha for correct next drawing
  //todo: RGB alpha image
  P :=result.GrayscaleAlphaPixels;
  for I := 1 to result.Width * result.Height * result.Depth do
  begin
    p^[1]:=p^[0];
    p^[0]:=255;
    Inc(P);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.broken_string_to_image(const s:DStringList):TGrayscaleAlphaImage;
var dummyImage:TGrayscaleAlphaImage;
    i:integer;
    maxh,maxhb,maxw:integer;
begin
  maxh:=0;
  maxhb:=0;
  maxw:=0;
  for i:=0 to s.count-1 do begin
    if maxh<s[i].height then maxh:=s[i].height;
    if maxhb<s[i].height-s[i].heightbase then maxhb:=s[i].height-s[i].heightbase;
    if maxw<s[i].width then maxw:=s[i].width;
  end;
//  writelnLog('DFont.broken_string_to_image','max height base = ', inttostr(maxhb));
  result:=TGRayScaleAlphaImage.create;
  result.SetSize(maxw,maxh*(s.count));
  result.Clear(Vector2Byte(0,0));
  for i:=0 to s.count-1 do begin
    DummyImage:=string_to_image(s[i].value);
    result.DrawFrom(DummyImage,0,maxh*(s.count-1-i)+maxhb-(s[i].height-s[i].heightbase),dmBlendSmart);
    freeandnil(dummyImage);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.broken_string_to_image_with_shadow(const s:DStringList;shadow_strength:single;shadow_length:integer):TGrayscaleAlphaImage;
var DummyImage,ShadowImage:TGrayscaleAlphaImage;
    iteration,i:integer;
    P:Pvector2byte;
begin
  DummyImage:=broken_string_to_image(s);
  if (shadow_strength>0) and (shadow_length>0) then begin
    Result:=TGrayscaleAlphaImage.Create(dummyImage.Width+shadow_length,dummyImage.Height+shadow_length);//dummyImage.MakeCopy as TGrayscaleAlphaImage;
    Result.Clear(vector2byte(0,0));
    shadowImage:=dummyImage.MakeCopy as TGrayscaleAlphaImage;
    for iteration:=1 to shadow_length do begin
      P :=shadowImage.GrayscaleAlphaPixels;
      for I := 1 to shadowImage.Width * shadowImage.Height * shadowImage.Depth do
        begin
          p^[1]:=round(p^[1] * shadow_strength / sqr(iteration));
          p^[0]:=0;        //shadow color intensity might be specified here... or even an RGB color if make Shadow a TRGBAlphaImage
          Inc(P);
        end;
      Result.DrawFrom(ShadowImage,iteration,shadow_length-iteration,dmBlendSmart);
    end;
    Result.DrawFrom(dummyImage,0,Shadow_Length,dmBlendSmart);
    freeAndNil(ShadowImage);
    freeAndNil(DummyImage);
  end else
    result:=DummyImage;
end;

{---------------------------------------------------------------------------}

function DFont.break_stings(const s:String;const w:integer):DStringList;
var i1,i2,i_break:integer;
    newstring:DString;
begin
  result:=DStringList.create;
  i1:=1;
  i2:=1;
  i_break:=i2;
  while i2<=length(s) do
  begin
    if (copy(s,i2,1)=' ') or (copy(s,i2,1)=dlinebreak) then i_break:=i2;
    if (textwidth(copy(s,i1,i2-i1))>w) or (copy(s,i2,1)=dlinebreak) then
    begin
      newString:=DString.create;
      newString.value:=copy(s,i1,i_break-i1);
      newString.heightbase:=textheightbase(newString.value);
      newString.height:=textheight(newString.value);
      newString.width:=textwidth(newString.value);
      result.add(newString);
      i1:=i_break+1;
    end;
    inc(i2);
  end;
  newString:=DString.create;
  newString.value:=copy(s,i1,i2-i1);
  newString.heightbase:=textheightbase(newString.value);
  newString.height:=textheight(newString.value);
  newString.width:=textwidth(newString.value);
  result.add(newString);
end;


initialization
end.

