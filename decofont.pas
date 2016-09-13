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
  CastleImages,CastleTextureFontData, castleVectors,
  CastleLog, castleFilesUtils;

{$IFNDEF Android}
const NormalFontFile='interface/fonts/LinBiolinum_R_G.ttf';
{$ENDIF}

const decolinebreak='@';

type DString=class(TObject)
  value:String;
  width,height:integer;
end;
type DStringList = specialize TFPGObjectList<DString>;

Type DFont=class(TTextureFont)
  function string_to_image(const s:string):TGrayscaleAlphaImage;
  function broken_string_to_image(const s:DStringList):TGrayscaleAlphaImage;
  function broken_string_to_image_with_shadow(const s:DStringList;shadow_strength:single;shadow_length:integer):TGrayscaleAlphaImage;
  function Break_String(const s:string;const maxwidth:integer):DStringList;
 private
   AlphaFontImage: TGrayscaleAlphaImage;
end;

var {$IFNDEF Android}MyCharSet:TUnicodeCharList;{$ENDIF}
  RegularFont16:DFont;

procedure InitializeFonts;

implementation

{------------------------------------------------------------------------------}

procedure InitializeFonts;
begin
   {$IFDEF Android}
   RegularFont16:=TTextureFont.Create(TextureFont_LinBiolinumRG_16) as DFont;
   {$ELSE}
   if MyCharSet=nil then begin
      MyCharSet:=TUnicodeCharList.Create;
      MyCharSet.add(SimpleAsciiCharacters);
      MyCharSet.add('ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
   end;
   RegularFont16:=DFont.Create(ApplicationData(NormalFontFile),16,true,MyCharSet);
   {$ENDIF}
   WritelnLog('DecoFont:initialization','Fonts loaded successfully.');
end;

{-----------------------------------------------------------------------------}

function DFont.string_to_image(const s:string):TGrayscaleAlphaImage;
var ScreenX, ScreenY: integer;
    G: TTextureFontData.TGlyph;
    C: TUnicodeChar;
    TextPtr: PChar;
    CharLen: Integer;

    imagewidth,imageheight,imagebonusheight,imagebaseline: integer;

    P: PVector2Byte;
    i:integer;
begin
  //first scan the line length and make the image of appropriate width and height
  imagewidth:=0;
  imageheight:=0;
  imagebonusheight:=0;
  imagebaseline:=0;
  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    G := FFont.Glyph(C);  //THIS LINE REQUIRES PUBLISHING OF FFONT IN TTextureFont
    if G <> nil then begin
      imagewidth+=G.AdvanceX;
      imagebonusheight+=G.advanceY;
      if imagebaseline<G.Y then imagebaseline:=G.Y;
      if imageheight<G.Height then ImageHeight:=G.Height;
    end;
    Inc(TextPtr, CharLen);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;

  //initialize ALPHA channel based on FFont.Image
  if AlphaFontImage=nil then begin
    AlphaFontImage:=TGrayscaleAlphaImage.create;
    AlphaFontImage.SetSize(FFont.Image);        //THIS LINE REQUIRES PUBLISHING OF FFONT IN TTextureFont
    AlphaFontImage.Clear(Vector2Byte(0,255));
    AlphaFontImage.DrawFrom(FFont.Image,0,0,dmAdd);     //THIS LINE REQUIRES PUBLISHING OF FFONT IN TTextureFont
     P :=AlphaFontImage.GrayscaleAlphaPixels;
     for I := 1 to AlphaFontImage.Width * AlphaFontImage.Height * AlphaFontImage.Depth do
     begin
       p^[1]:=p^[0];
       p^[0]:=255;
       Inc(P);
     end;
  end;

  Result:=TGrayscaleAlphaImage.create;
  Result.SetSize(imagewidth,ImageHeight+ImageBonusHeight+ImageBaseline);
  result.Clear(Vector2Byte(0,0));
  //now draw all the glyphs onto the image
  ScreenX := 0;
  ScreenY := ImageBaseline;
  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    G := FFont.Glyph(C);                       //THIS LINE REQUIRES PUBLISHING OF FFONT IN TTextureFont
    if G <> nil then begin
      result.DrawFrom(AlphaFontImage,
                      ScreenX - G.X,ScreenY - G.Y,
                      G.ImageX, G.ImageY, G.Width, G.Height,dmBlendSmart);
      ScreenX += G.AdvanceX;
      ScreenY += G.AdvanceY;
    end;
    Inc(TextPtr, CharLen);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.broken_string_to_image(const s:DStringList):TGrayscaleAlphaImage;
var dummyImage:TGrayscaleAlphaImage;
    i:integer;
    maxh,maxw:integer;
begin
  maxh:=0;
  maxw:=0;
  for i:=0 to s.count-1 do begin
    if maxh<s[i].height then maxh:=s[i].height;
    if maxw<s[i].width then maxw:=s[i].width;
  end;
  result:=TGRayScaleAlphaImage.create;
  result.SetSize(maxw,maxh*(s.count));
  result.Clear(Vector2Byte(0,0));
  for i:=0 to s.count-1 do begin
    DummyImage:=string_to_image(s[i].value);
    result.DrawFrom(DummyImage,0,maxh*(s.count-1-i),dmBlendSmart);
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

function DFont.Break_String(const s:string;const maxwidth:integer):DStringList;
var G: TTextureFontData.TGlyph;
    C: TUnicodeChar;
    TextPtr: PChar;
    CharLen: Integer;

    imagewidth,imageheight,imagebonusheight,imagebaseline: integer;

    newString:DString;
    tmpstring:String;
    breakpoint,widthatbreakpoint{,HeightAtBreakpoint}:integer;
begin
  result:=DStringList.create;
  imagewidth:=0;
  imageheight:=0;
  imagebonusheight:=0;
  imagebaseline:=0;
  breakpoint:=0;
  tmpString:='';
  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    G := FFont.Glyph(C);                          //THIS LINE REQUIRES PUBLISHING OF FFONT IN TTextureFont
    if G <> nil then begin
      tmpString+=UnicodeToUTF8(C);
      imagebonusheight+=G.advanceY;
      if imagebaseline<G.Y then imagebaseline:=G.Y;
      if imageheight<G.Height then ImageHeight:=G.Height;
      imagewidth+=G.AdvanceX;
      if (UnicodeToUTF8(C)=' ') or (UnicodeToUTF8(C)=decolinebreak) then begin
        WidthAtBreakpoint:=imagewidth;
        //HeightAtBreakpoint:=imagebonusheight+imageheight+imagebaseline;
        breakpoint:=length(tmpString);
      end;
      if (imagewidth>maxwidth) or (UnicodeToUTF8(C)=decolinebreak) then begin
        newString:=DString.create;
        newString.value:=copy(tmpString,0,breakPoint-1);
        newString.height:=imagebonusheight+imageheight+imagebaseline;
        newString.width:=WidthAtBreakpoint;
        result.add(newString);
        tmpString:=copy(tmpString,breakPoint+1,length(tmpString)-breakPoint);
        imagewidth-=WidthAtBreakpoint;
        imageheight:=0;
        imagebonusheight:=0;
        imagebaseline:=0;
        breakpoint:=0;
      end;
    end;
    Inc(TextPtr, CharLen);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
  newString:=DString.create;
  newString.value:=tmpString;
  newString.height:=imagebonusheight+imageheight+imagebaseline;
  newString.width:=imagewidth;
  result.add(newString);
end;

{---------------------------------------------------------------------------}


initialization
  InitializeFonts;
end.

