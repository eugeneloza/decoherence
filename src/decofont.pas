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

{ Describes the game fonts and handles their conversion to images }
unit DecoFont;

{$INCLUDE compilerconfig.inc}

interface

uses
  SysUtils, fgl,
  {$IFDEF Android}
  CastleTextureFont_LinBiolinumRG_16,
  {$ENDIF}
  CastleFonts, CastleUnicode, CastleStringUtils,
  CastleImages, {CastleTextureFontData,} CastleVectors, CastleColors,
  CastleLog, CastleFilesUtils,
  DecoGlobal;

{$IFNDEF Android}
const NormalFontFile = 'LinBiolinum_R_G.ttf';
{$ENDIF}

const dLineBreak = sLineBreak;

type DString = class(TObject)
  { each line text content }
  Value: String;
  { specific size parameters of this line }
  Width, Height, HeightBase: integer;
end;
type DStringList  =  specialize TFPGObjectList<DString>;

Type DFont = class(TTextureFont)
  { Converts a broken string into a single image }
  function BrokenStringToImage(const s: DStringList): TGrayscaleAlphaImage;
  { Converts a broken string into a single image with shadow }
  function BrokenStringToImageWithShadow(const s: DStringList; ShadowStrength: float; ShadowLength: integer): TGrayscaleAlphaImage;
  { Breaks a string to a DStringList }
  function BreakStings(const s : String; const w: integer): DStringList;
 private
  //todo  : RGB Alpha image;
  { Converts a single line of text to an image }
  function StringToImage(const s: string): TGrayscaleAlphaImage;
end;

var DefaultFont: DFont;

    CharHealthFont, CharNickNameFont: DFont;
    LoadScreenFont: DFont;
    PlayerDamageFont: DFont;
    DebugFont: DFont;

procedure InitializeFonts;
procedure DestroyFonts;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

var {$IFNDEF Android}MyCharSet: TUnicodeCharList;{$ENDIF}
    RegularFont12,RegularFont16,RegularFont100: DFont;

{------------------------------------------------------------------------------}

procedure SetFonts;
begin
  DefaultFont := RegularFont16;
  DebugFont := RegularFont12;

  CharHealthFont := RegularFont12;
  CharNickNameFont := RegularFont12;

  LoadScreenFont := RegularFont16;

  PlayerDamageFont := RegularFont100;
end;

procedure InitializeFonts;
begin
  WritelnLog('DecoFont : InitializeFonts','Init started');
   {$IFDEF Android}
   RegularFont16 := DFont.Create(TextureFont_LinBiolinumRG_16);
   RegularFont12 := RegularFont16; {!!!! TODO}
   RegularFont100 := RegularFont16;
   {$ELSE}
   if MyCharSet = nil then begin
      MyCharSet := TUnicodeCharList.Create;
      //MyCharSet := AllChars;
      MyCharSet.Add(SimpleAsciiCharacters);
      MyCharSet.Add('ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
   end;
   RegularFont12 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),12,true,MyCharSet);
   RegularFont16 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),16,true,MyCharSet);
   RegularFont100 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),100,true,MyCharSet);
   {$ENDIF}
   WriteLnLog('DecoFont : InitializeFonts','Fonts loaded successfully.');

   SetFonts;
end;

{----------------------------------------------------------------------------}

procedure DestroyFonts;
begin
  FreeAndNil(RegularFont12);
  FreeAndNil(RegularFont16);
  FreeAndNil(RegularFont100);
  FreeAndNil(MyCharSet);
end;

{-----------------------------------------------------------------------------}

function DFont.StringToImage(const s: string): TGrayscaleAlphaImage;
var P : Pvector2byte;
    i : integer;
begin
  Result := TGrayscaleAlphaImage.Create;
  Result.SetSize(TextWidth(s),TextHeight(s));  //including baseline
  Result.Clear(Vector2Byte(0,255));

  PushProperties; // save previous TargetImage value
  TargetImage  :=  Result;
  Print(0, TextHeight(s)-TextHeightBase(s), White, S);       //shift text up from a baseline
  PopProperties; // restore previous TargetImage value

  //reset alpha for correct next drawing
  //todo :  RGB alpha image
  P  := Result.GrayscaleAlphaPixels;
  for i  :=  1 to Result.Width * Result.Height * Result.Depth do
  begin
    p^[1] := p^[0];
    p^[0] := 255;
    Inc(P);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.BrokenStringToImage(const s: DStringList): TGrayscaleAlphaImage;
var DummyImage : TGrayscaleAlphaImage;
    i : integer;
    MaxH,MaxHb,MaxW : integer;
begin
  MaxH  := 0;
  MaxHb := 0;
  MaxW  := 0;
  for i := 0 to s.Count-1 do begin
    if MaxH < s[i].Height then MaxH := s[i].Height;
    if MaxHb < s[i].Height-s[i].HeightBase then MaxHb := s[i].Height-s[i].HeightBase;
    if MaxW < s[i].Width then MaxW := s[i].Width;
  end;
//  writelnLog('DFont.broken_string_to_image','max height base  =  ', inttostr(maxhb));
  Result := TGRayScaleAlphaImage.Create;
  Result.SetSize(MaxW,MaxH*(s.Count));
  Result.Clear(Vector2Byte(0,0));
  for i := 0 to s.Count-1 do begin
    DummyImage := StringToImage(s[i].Value);
    Result.DrawFrom(DummyImage,0,MaxH*(s.Count-1-i)+MaxHb-(s[i].Height-s[i].HeightBase),dmBlendSmart);
    FreeAndNil(DummyImage);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.BrokenStringToImageWithShadow(const s: DStringList; ShadowStrength: float; ShadowLength : integer): TGrayscaleAlphaImage;
var DummyImage,ShadowImage : TGrayscaleAlphaImage;
    Iteration,i : integer;
    P : Pvector2byte;
begin
  DummyImage := BrokenStringToImage(s);
  if (ShadowStrength>0) and (ShadowLength>0) then begin
    Result := TGrayscaleAlphaImage.Create(DummyImage.Width+ShadowLength,DummyImage.Height+ShadowLength);//dummyImage.MakeCopy as TGrayscaleAlphaImage;
    Result.Clear(Vector2Byte(0,0));
    ShadowImage := DummyImage.MakeCopy as TGrayscaleAlphaImage;
    for Iteration := 1 to ShadowLength do begin
      P := ShadowImage.GrayscaleAlphaPixels;
      for i :=  1 to ShadowImage.Width * ShadowImage.Height * ShadowImage.Depth do
        begin
          p^[1] := Round(p^[1] * ShadowStrength / Sqr(Iteration));
          p^[0] := 0;        //shadow color intensity might be specified here... or even an RGB color if make Shadow a TRGBAlphaImage
          Inc(P);
        end;
      Result.DrawFrom(ShadowImage,Iteration,ShadowLength-Iteration,dmBlendSmart);
    end;
    Result.DrawFrom(DummyImage,0,ShadowLength,dmBlendSmart);
    FreeAndNil(ShadowImage);
    FreeAndNil(DummyImage);
  end else
    Result := DummyImage;
end;

{---------------------------------------------------------------------------}

function DFont.BreakStings(const s: string; const w: integer): DStringList;
var i1,i2,i_break : integer;
    NewString : DString;
begin
  Result := DStringList.Create;
  i1 := 1;
  i2 := 1;
  i_break := i2;
  while i2 <= Length(s) do
  begin
    if (Copy(s,i2,1) = ' ') or (Copy(s,i2,Length(dLineBreak)) = dLineBreak) then i_break := i2;
    if (TextWidth(Copy(s,i1,i2-i1)) > w) or (Copy(s,i2,Length(dLineBreak)) = dLineBreak) then
    begin
      NewString := DString.Create;
      NewString.Value := Copy(s,i1,i_break-i1);
      NewString.HeightBase := TextHeightBase(NewString.Value);
      NewString.Height := TextHeight(NewString.Value);
      NewString.Width := TextWidth(NewString.Value);
      Result.Add(NewString);
      i1 := i_break+1;
    end;
    inc(i2);
  end;
  NewString := DString.Create;
  NewString.Value := Copy(s,i1,i2-i1);
  NewString.HeightBase := TextHeightBase(NewString.Value);
  NewString.Height := TextHeight(NewString.Value);
  NewString.Width := TextWidth(NewString.Value);
  Result.Add(NewString);
end;

end.

