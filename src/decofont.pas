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
const
  NormalFontFile = 'Xolonium-Regular.ttf';

const
  BoldFontFile = 'Xolonium-Bold.ttf';
{$ENDIF}

const
  dLineBreak = sLineBreak;

type
  DString = class(DObject) //maybe make it a record?
    { each line text content }
    Value: string;
    { specific size parameters of this line }
    Width, Height, HeightBase: integer;
  end;

type
  DStringList = specialize TObjectList<DString>;

type

  DFont = class(TTextureFont)
  private
    //todo  : RGB Alpha image;
    { Converts a single line of text to an image }
    function StringToImage(const s: string): TGrayscaleAlphaImage;
  public

    Gap: integer;
    { Converts a broken string into a single image }
    function BrokenStringToImage(const s: DStringList): TGrayscaleAlphaImage;
    { Converts a broken string into a single image with shadow }
    function BrokenStringToImageWithShadow(const s: DStringList;
      ShadowStrength: float; ShadowLength: integer): TGrayscaleAlphaImage;
    { Breaks a string to a DStringList }
    function BreakStings(const s: string; const w: integer): DStringList;
  end;

type
  { }
  DLoadedFonts = specialize TObjectDictionary<string, DFont>;
  DFontList = specialize TDictionary<string, DFont>;

var
  Fonts: DFontList;

  DefaultFont: DFont;
  DebugFont: DFont;

function GetFont(const FontString: string): DFont; TryInline
procedure InitializeFonts;
procedure DestroyFonts;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses DecoLog, Profiler;

{these are internal variables which are managed,
 "external" fonts variables are assigned to these}
var
  LoadedFonts: DLoadedFonts;

{------------------------------------------------------------------------------}

function GetFont(const FontString: string): DFont; TryInline
begin
  Result := nil; //to avoid uninitialized variable hint
  if not Fonts.TryGetValue(FontString, Result) then
  begin
    Log(LogInterfaceError, _CurrentRoutine, 'Unknown Font: '+FontString);
    Result := DefaultFont;
  end;
end;

{------------------------------------------------------------------------------}

procedure SetFonts;
  function GetLoadedFont(const FontString: string): DFont;
  begin
    Result := nil; //to avoid uninitialized variable hint
    if not LoadedFonts.TryGetValue(FontString, Result) then
    begin
      Log(LogInterfaceError, _CurrentRoutine, 'Unknown Font: '+FontString);
      Result := DefaultFont;
    end;
  end;
begin
  {StartProfiler}

  Fonts := DFontList.Create;

  //make DebugFont more fail-proof!!! e.g. by using UIFont
  LoadedFonts.TryGetValue('xolonium-12', DebugFont);

  if not LoadedFonts.TryGetValue('xolonium-16', DefaultFont) then
    raise Exception.Create('FATAL: cannot create default font!');

  Fonts.Add('PlayerHealth',GetLoadedFont('xolonium-12'));
  Fonts.Add('PlayerName',GetLoadedFont('xolonium-12'));
  Fonts.Add('LoadScreen',GetLoadedFont('xolonium-16'));
  Fonts.Add('PlayerDamage',GetLoadedFont('xolonium-num-99'));

  {StopProfiler}
end;

procedure InitializeFonts;
{$IFNDEF Android}
var
  FullCharSet: TUnicodeCharList;
  NumCharSet: TUnicodeCharList;
  {maybe also ASCII-only? char list}

  function GetFontFile(const FontName: string; const FullChar: boolean; const FontSize: integer; const FontGap: integer = 0): DFont;
  var
    CharSet: TUnicodeCharList;
  begin
    if FullChar then CharSet := FullCharSet else CharSet := NumCharSet;
    Result := DFont.Create(ApplicationData(FontFolder + FontName),
      FontSize, True, CharSEt);
    Result.Gap := FontGap;
  end;
{$ENDIF}
begin
  {StartProfiler}

  LoadedFonts := DLoadedFonts.Create([doOwnsValues]);

  Log(LogInitInterface, _CurrentRoutine, 'Init started');
   {$IFDEF Android}
  RegularFont16 := DFont.Create(TextureFont_LinBiolinumRG_16);
  RegularFont12 := RegularFont16; {!!!! TODO}
  RegularFont100 := RegularFont16;
   {$ELSE}
  //assign char sets
  FullCharSet := TUnicodeCharList.Create;
  //MyCharSet := AllChars;
  FullCharSet.Add(SimpleAsciiCharacters);
  FullCharSet.Add(
    'ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбю'); {ІЇЄіїє}
  NumCharSet := TUnicodeCharList.Create;
  NumCharSet.Add('1234567890');

  //load the font files


  LoadedFonts.Add('xolonium-12',GetFontFile(NormalFontFile, true, 12, 3));
  LoadedFonts.Add('xolonium-16',GetFontFile(NormalFontFile, true, 16, 3));
  LoadedFonts.Add('xolonium-num-99',GetFontFile(NormalFontFile, false, 99, 3));

  //release char sets
  FreeAndNil(FullCharSet);
  FreeAndNil(NumCharSet);
   {$ENDIF}
  Log(LogInitInterface, _CurrentRoutine, 'Fonts loaded successfully.');

  SetFonts;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DestroyFonts;
begin
  {StartProfiler}

  FreeAndNil(LoadedFonts); //will free children
  FreeAndNil(Fonts);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

function DFont.StringToImage(const s: string): TGrayscaleAlphaImage;
var
  P: Pvector2byte;
  i: integer;
begin
  {StartProfiler}

  Result := TGrayscaleAlphaImage.Create;
  Result.SetSize(TextWidth(s), TextHeight(s) + Self.Gap);  //including baseline
  Result.Clear(Vector2Byte(0, 255));

  PushProperties; // save previous TargetImage value
  TargetImage := Result;
  Print(0, TextHeight(s) - TextHeightBase(s), White, S);
  //shift text up from a baseline
  PopProperties; // restore previous TargetImage value

  //reset alpha for correct next drawing
  //todo :  RGB alpha image
  P := Result.Pixels;
  for i := 1 to Result.Width * Result.Height * Result.Depth do
  begin
    p^[1] := p^[0];
    p^[0] := 255;
    Inc(P);
  end;

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

function DFont.BrokenStringToImage(const s: DStringList): TGrayscaleAlphaImage;
var
  DummyImage: TGrayscaleAlphaImage;
  i: integer;
  MaxH, MaxHb, MaxW: integer;
begin
  {StartProfiler}

  MaxH := 0;
  MaxHb := 0;
  MaxW := 0;
  for i := 0 to s.Count - 1 do
  begin
    if MaxH < s[i].Height then
      MaxH := s[i].Height;
    if MaxHb < s[i].Height - s[i].HeightBase then
      MaxHb := s[i].Height - s[i].HeightBase;
    if MaxW < s[i].Width then
      MaxW := s[i].Width;
  end;
  MaxH += Self.Gap;
  //  Log('DFont.broken_string_to_image','max height base  =  ', inttostr(maxhb));
  Result := TGRayScaleAlphaImage.Create;
  Result.SetSize(MaxW, MaxH * (s.Count));
  Result.Clear(Vector2Byte(0, 0));
  for i := 0 to s.Count - 1 do
  begin
    DummyImage := StringToImage(s[i].Value);
    Result.DrawFrom(DummyImage, 0, MaxH * (s.Count - 1 - i) + MaxHb -
      (s[i].Height - s[i].HeightBase), dmBlendSmart);
    FreeAndNil(DummyImage);
  end;

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

function DFont.BrokenStringToImageWithShadow(const s: DStringList;
  ShadowStrength: float; ShadowLength: integer): TGrayscaleAlphaImage;
var
  DummyImage, ShadowImage: TGrayscaleAlphaImage;
  Iteration, i: integer;
  p: PVector2byte;
begin
  {StartProfiler}

  DummyImage := BrokenStringToImage(s);
  if (ShadowStrength > 0) and (ShadowLength > 0) then
  begin
    Result := TGrayscaleAlphaImage.Create(DummyImage.Width + ShadowLength,
      DummyImage.Height + ShadowLength);//dummyImage.MakeCopy as TGrayscaleAlphaImage;
    Result.Clear(Vector2Byte(0, 0));
    ShadowImage := DummyImage.MakeCopy as TGrayscaleAlphaImage;
    for Iteration := 1 to ShadowLength do
    begin
      p := ShadowImage.Pixels;
      for i := 0 to ShadowImage.Width * ShadowImage.Height * ShadowImage.Depth - 1 do
      begin
        p^[1] := Round(p^[1] * ShadowStrength / Sqr(Iteration));
        p^[0] := 0;
        //shadow color intensity might be specified here... or even an RGB color if make Shadow a TRGBAlphaImage
        Inc(p);
      end;
      Result.DrawFrom(ShadowImage, Iteration, ShadowLength - Iteration, dmBlendSmart);
    end;
    Result.DrawFrom(DummyImage, 0, ShadowLength, dmBlendSmart);
    FreeAndNil(ShadowImage);
    FreeAndNil(DummyImage);
  end
  else
    Result := DummyImage;

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

function DFont.BreakStings(const s: string; const w: integer): DStringList;
var
  i1, i2, i_break: integer;
  NewString: DString;
begin
  {StartProfiler}

  Result := DStringList.Create;
  i1 := 1;
  i2 := 1;
  i_break := i2;
  while i2 <= Length(s) do
  begin
    if (Copy(s, i2, 1) = ' ') or (Copy(s, i2, Length(dLineBreak)) = dLineBreak) then
      i_break := i2;
    if (TextWidth(Copy(s, i1, i2 - i1)) > w) or (Copy(s, i2, Length(dLineBreak)) =
      dLineBreak) then
    begin
      NewString := DString.Create;
      NewString.Value := Copy(s, i1, i_break - i1);
      NewString.HeightBase := TextHeightBase(NewString.Value);
      NewString.Height := TextHeight(NewString.Value);
      NewString.Width := TextWidth(NewString.Value);
      Result.Add(NewString);
      i1 := i_break + 1;
    end;
    Inc(i2);
  end;
  NewString := DString.Create;
  NewString.Value := Copy(s, i1, i2 - i1);
  NewString.HeightBase := TextHeightBase(NewString.Value);
  NewString.Height := TextHeight(NewString.Value);
  NewString.Width := TextWidth(NewString.Value);
  Result.Add(NewString);

  {StopProfiler}
end;

end.
