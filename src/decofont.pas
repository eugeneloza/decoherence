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
  SysUtils, Generics.Collections,
  {$IFDEF Android}
  CastleTextureFont_LinBiolinumRG_16,
  {$ENDIF}
  CastleFonts, CastleUnicode, CastleStringUtils,
  CastleImages, {CastleTextureFontData,} CastleVectors, CastleColors,
  CastleFilesUtils,
  DecoGlobal;

{$IFNDEF Android}
const NormalFontFile = 'Xolonium-Regular.ttf';
const BoldFontFile = 'Xolonium-Bold.ttf';
{$ENDIF}

const dLineBreak = sLineBreak;

type DString = class(DObject) //maybe make it a record?
  { each line text content }
  Value: String;
  { specific size parameters of this line }
  Width, Height, HeightBase: integer;
end;
type DStringList  =  specialize TObjectList<DString>;

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

    PlayerHealthFont, PlayerNameFont: DFont;
    LoadScreenFont: DFont;
    PlayerDamageFont: DFont;
    DebugFont: DFont;

procedure InitializeFonts;
procedure DestroyFonts;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoLog;

{these are internal variables which are managed,
 "external" fonts variables are assigned to these}
var RegularFont12,RegularFont16,RegularFont100: DFont;

{------------------------------------------------------------------------------}

procedure SetFonts;
begin
  DefaultFont := RegularFont16;
  DebugFont := RegularFont12;

  PlayerHealthFont := RegularFont12;
  PlayerNameFont := RegularFont12;

  LoadScreenFont := RegularFont16;

  PlayerDamageFont := RegularFont100;
end;

procedure InitializeFonts;
{$IFNDEF Android}
var FullCharSet: TUnicodeCharList;
    NumCharSet: TUnicodeCharList;
{$ENDIF}
begin
  fLog(LogInitInterface,{$I %CURRENTROUTINE%},'Init started');
   {$IFDEF Android}
   RegularFont16 := DFont.Create(TextureFont_LinBiolinumRG_16);
   RegularFont12 := RegularFont16; {!!!! TODO}
   RegularFont100 := RegularFont16;
   {$ELSE}
   //assign char sets
   FullCharSet := TUnicodeCharList.Create;
   //MyCharSet := AllChars;
   FullCharSet.Add(SimpleAsciiCharacters);
   FullCharSet.Add('ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
   NumCharSet := TUnicodeCharList.Create;
   NumCharSet.Add('1234567890');

   //load the font files
   RegularFont12 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),12,true,FullCharSet);
   RegularFont16 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),16,true,FullCharSet);
   RegularFont100 := DFont.Create(ApplicationData(FontFolder+NormalFontFile),100,true,NumCharSet);

   //release char sets
   FreeAndNil(FullCharSet);
   FreeAndNil(NumCharSet);
   {$ENDIF}
   fLog(LogInitInterface,{$I %CURRENTROUTINE%},'Fonts loaded successfully.');

   SetFonts;
end;

{----------------------------------------------------------------------------}

procedure DestroyFonts;
begin
  FreeAndNil(RegularFont12);
  FreeAndNil(RegularFont16);
  FreeAndNil(RegularFont100);
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
  P  := Result.Pixels;
  for i  :=  1 to Result.Width * Result.Height * Result.Depth do
  begin
    p^[1] := p^[0];
    p^[0] := 255;
    inc(P);
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
//  dLog('DFont.broken_string_to_image','max height base  =  ', inttostr(maxhb));
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
    p : PVector2byte;
begin
  DummyImage := BrokenStringToImage(s);
  if (ShadowStrength > 0) and (ShadowLength > 0) then begin
    Result := TGrayscaleAlphaImage.Create(DummyImage.Width+ShadowLength,DummyImage.Height+ShadowLength);//dummyImage.MakeCopy as TGrayscaleAlphaImage;
    Result.Clear(Vector2Byte(0,0));
    ShadowImage := DummyImage.MakeCopy as TGrayscaleAlphaImage;
    for Iteration := 1 to ShadowLength do begin
      p := ShadowImage.Pixels;
      for i := 0 to ShadowImage.Width * ShadowImage.Height * ShadowImage.Depth-1 do
        begin
          p^[1] := Round(p^[1] * ShadowStrength / Sqr(Iteration));
          p^[0] := 0;        //shadow color intensity might be specified here... or even an RGB color if make Shadow a TRGBAlphaImage
          Inc(p);
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

