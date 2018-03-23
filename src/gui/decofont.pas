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

(* Game fonts and text to image conversion *)

unit DecoFont;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleFonts, CastleUtils {for TStructList}, CastleImages,
  DecoFontEncoding,
  DecoGlobal;

const
  dLineBreak = sLineBreak;

type
  {}
  DString = record
    { Each line text content }
    Value: string;
    { Specific size parameters of this line }
    Width, Height, HeightBase: integer;
    FullWidth: integer;
    { how much additional space may/should be used to adjust to width? }
    AdditionalSpace: integer;
    { how many words are in the line? }
    Words: array of string;
    { Can this line be adjusted to width?
      (e.g. the last line can't) }
    AdjustWidth: boolean;
  end;

type
  {}
  DBrokenString = specialize TStructList<DString>;

type
  {}
  DFont = class(TTextureFont)
  private
    { Converts a single line of text to an image
      if Width < s.NoSpaceWidth then it just renders the string
      otherwise - justifies it along width }
    function LineToImage(const aString: DString; const FitWidth: boolean = false): TGrayscaleAlphaImage;
  public
    { Additional spacing between lines }
    AdditionalLineSpacing: integer;
    { Converts a broken string into a single image }
    function StringToImage(const aString: string; const aWidth: integer;
      const FitWidth: boolean = false): TGrayscaleAlphaImage;
    { Converts a broken string into a single image with a shadow }
    function StringToImageWithShadow(const aString: string; const aWidth: integer;
      ShadowStrength: DFloat; ShadowLength: integer; const FitWidth: boolean = false): TGrayscaleAlphaImage;
    { Breaks a string to a DBrokenString }
    function BreakStings(const aString: string; const aWidth: integer): DBrokenString;
  end;

var
  { Debug and DefaultFont }
  DebugFont, DefaultFont: DFont;

{}
function GetFontByName(const FontName: string): DFont; TryInline
{}
procedure InitFonts;
{}
procedure FreeFonts;
{............................................................................}
implementation
uses
  SysUtils, Generics.Collections,
  CastleUnicode, CastleColors, CastleVectors,
  CastleTextureFont_LinBiolinumRG_16, //a debug font
  DecoTrash, DecoLog, DecoMath;

{-----------------------------------------------------------------------------}

function DFont.LineToImage(const aString: DString; const FitWidth: boolean = false): TGrayscaleAlphaImage;
var
  P: Pvector2byte;
  i: integer;
  TotalFitSpace, CurrentPos, y, WhiteSpace: integer;
  StringWidth: integer;
begin
  Result := TGrayscaleAlphaImage.Create;
  if FitWidth then
    StringWidth := aString.FullWidth
  else
    StringWidth := aString.Width;
  Result.SetSize(StringWidth, aString.Height + Self.AdditionalLineSpacing);  //including baseline
  Result.Clear(Vector2Byte(0, 255));

  PushProperties; // save previous TargetImage value
  TargetImage := Result;

  if FitWidth and aString.AdjustWidth then
  begin
    {draw word-by-word, adjusting space to fit width}
    TotalFitSpace := aString.AdditionalSpace;
    //WhiteSpace := TotalFitSpace div Pred(Length(aString.Words));
    CurrentPos := 0;
    y := aString.Height - aString.HeightBase; {shift text up from a baseline }
    for i := 0 to Pred(Length(aString.Words)) do
    begin
      Print(CurrentPos, y, White, aString.Words[i]);
      CurrentPos += TextWidth(aString.Words[i]);
      if Length(aString.Words) - i - 1 > 0 then
        WhiteSpace := TotalFitSpace div (Length(aString.Words) - i - 1)
      else
        WhiteSpace := TotalFitSpace;
      CurrentPos += WhiteSpace;
      TotalFitSpace -= WhiteSpace;
    end;
  end
  else
    {Simply output the string}
    Print(0, aString.Height - aString.HeightBase {shift text up from a baseline},
      White, aString.Value);

  PopProperties; // restore previous TargetImage value

  //reset alpha for correct next drawing
  //todo :  RGB alpha image
  P := Result.Pixels;
  for i := 1 to Result.Width * Result.Height * Result.Depth do
  begin
    p^[1] := p^[0];
    p^[0] := 255;
    inc(P);
  end;
end;

{---------------------------------------------------------------------------}

function DFont.StringToImage(const aString: string; const aWidth: integer;
      const FitWidth: boolean = false): TGrayscaleAlphaImage;
var
  DummyImage: TGrayscaleAlphaImage;
  i: integer;
  MaxH, MaxHb, MaxW: integer;
  BrokenString: DBrokenString;
begin
  BrokenString := Self.BreakStings(aString, aWidth);

  MaxH := 0;
  MaxHb := 0;
  MaxW := 0;
  {get maximum values of height, baseheight and width}
  for i := 0 to BrokenString.Count - 1 do
  begin
    AssignMax(MaxH, BrokenString[i].Height);
    AssignMax(MaxHb, BrokenString[i].Height - BrokenString[i].HeightBase);
    if FitWidth then
      AssignMax(MaxW, BrokenString[i].FullWidth)
    else
      AssignMax(MaxW, BrokenString[i].Width)
  end;
  MaxH += Self.AdditionalLineSpacing;
  Result := TGRayScaleAlphaImage.Create;
  Result.SetSize(MaxW, MaxH * (BrokenString.Count));
  Result.Clear(Vector2Byte(0, 0));
  for i := 0 to BrokenString.Count - 1 do
  begin
    DummyImage := LineToImage(BrokenString[i], FitWidth);
    Result.DrawFrom(DummyImage, 0, MaxH * (BrokenString.Count - 1 - i) + MaxHb -
      (BrokenString[i].Height - BrokenString[i].HeightBase), dmBlendSmart);
    DummyImage.Free;
  end;

  BrokenString.Free;
end;

{---------------------------------------------------------------------------}

function DFont.StringToImageWithShadow(const aString: string; const aWidth: integer;
      ShadowStrength: DFloat; ShadowLength: integer; const FitWidth: boolean = false): TGrayscaleAlphaImage;
var
  DummyImage, ShadowImage: TGrayscaleAlphaImage;
  Iteration, i: integer;
  p: PVector2byte;
begin
  DummyImage := Self.StringToImage(aString, aWidth, FitWidth);

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
        p^[1] := Round(p^[1] * ShadowStrength / sqr(Iteration));
        p^[0] := 0;
        //shadow color intensity might be specified here... or even an RGB color if make Shadow a TRGBAlphaImage
        inc(p);
      end;
      Result.DrawFrom(ShadowImage, Iteration, ShadowLength - Iteration, dmBlendSmart);
    end;
    Result.DrawFrom(DummyImage, 0, ShadowLength, dmBlendSmart);
    ShadowImage.Free;
    DummyImage.Free;
  end
  else
    Result := DummyImage;
end;

{---------------------------------------------------------------------------}

function DFont.BreakStings(const aString: string; const aWidth: integer): DBrokenString;
var
  SpaceWidth: integer;
  LineStart, CurrentChar, LastBreakPoint: integer;
  isLineBreak: boolean;
  isSpaceBar: boolean;
  Words: array of string;
  procedure AddWord;
  begin
    SetLength(Words, Length(Words) + 1);
    Words[Pred(Length(Words))] := Copy(aString, LastBreakPoint + 1, CurrentChar - LastBreakPoint - 1);
  end;
  procedure AddNewString;
  var
    NewString: DString;
    i: integer;
  begin
    NewString.Value := Copy(aString, LineStart, LastBreakPoint - LineStart);
    NewString.HeightBase := TextHeightBase(NewString.Value);
    NewString.Height := TextHeight(NewString.Value);
    NewString.Width := TextWidth(NewString.Value);
    if NewString.Width <= aWidth then
      NewString.FullWidth := aWidth
    else
    begin
      NewString.FullWidth := NewString.Width;
      Log(LogInterfaceInfo, CurrentRoutine, '"' + NewString.Value +
        '" has width ' + NewString.Width.ToString +
        ' which is larger than requested text width ' + aWidth.ToString);
    end;
    if Length(Words) > 1 then
      NewString.AdditionalSpace := aWidth - NewString.Width + SpaceWidth * (Length(Words) - 1)
    else
      NewString.AdditionalSpace := 0;
    {for i := 0 to Pred(Length(Words)) do
      NewString.AdditionalSpace -= TextWidth(Words[i]);}
    NewString.Words := Words;
    NewString.AdjustWidth := (not isLineBreak) and (Length(Words) > 1);
    Result.Add(NewString);
    Words := nil;
  end;
  var
    s: string;
begin
  Result := DBrokenString.Create;

  SpaceWidth := TextWidth(' ');

  LineStart := 1;
  CurrentChar := 1;
  LastBreakPoint := 0;
  Words := nil;
  while CurrentChar <= Length(aString) do
  begin
    isLineBreak := Copy(aString, CurrentChar, Length(dLineBreak)) = dLineBreak;
    isSpaceBar := Copy(aString, CurrentChar, 1) = ' ';

    if ((TextWidth(Copy(aString, LineStart, CurrentChar - LineStart)) >= aWidth)
      and (Length(Words) > 0)) or isLineBreak then
    begin
      { this is a line break until the text is over }
      AddNewString;
      LineStart := LastBreakPoint + 1;
    end;

    { find the end of the word }
    if isSpaceBar or isLineBreak then
    begin
      AddWord;
      LastBreakPoint := CurrentChar;
    end;

    inc(CurrentChar);
  end;
  { add the last line }
  AddWord;
  isLineBreak := true; //so that it'll be arranged correctly
  LastBreakPoint := Length(aString) + 1; //the last break point is EOL symbol
  AddNewString;
end;

{============================================================================}

type DFontDictionary = specialize TObjectDictionary<string, DFont>;

var
  { a list of loaded fonts with aliases
    Owns values }
  LoadedFonts: DFontDictionary;
  { a list of references to fonts based on game situation }
  FontDictionary: DFontDictionary;

function GetFontByName(const FontName: string): DFont; TryInline
begin
  if not FontDictionary.TryGetValue(FontName, Result) then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'Unknown Font: ' + FontName);
    Result := DefaultFont;
  end;
end;

{............................................................................}

procedure SetFonts;
  function GetLoadedFont(const FontString: string): DFont;
  begin
    if not LoadedFonts.TryGetValue(FontString, Result) then
    begin
      Log(LogInterfaceError, CurrentRoutine, 'Unknown Font: ' + FontString);
      Result := DefaultFont;
    end;
  end;
begin
  Log(LogInit, CurrentRoutine, 'Setting up fonts.');

  FontDictionary := DFontDictionary.Create([]);  //doesn't own children

  DefaultFont := GetLoadedFont('xolonium-12');

  FontDictionary.Add('PlayerHealth', GetLoadedFont('xolonium-12'));
  FontDictionary.Add('PlayerName', GetLoadedFont('xolonium-12'));
  FontDictionary.Add('LoadScreen', GetLoadedFont('xolonium-16'));
  FontDictionary.Add('PlayerDamage', GetLoadedFont('xolonium-num-99'));
end;

{---------------------------------------------------------------------------}

procedure InitFonts;
  const
    NormalFontFile = 'Xolonium-Regular.ttf';
  function GetFontFile(const FontName: string; const CharSet: TUnicodeCharList;
    const FontSize: integer; const AdditionalLineSpacing: integer = 0): DFont;
  var
    FontURL: string;
  begin
    FontURL := GameFolder('GUI/Fonts/' + FontName);
    try
      Result := DFont.Create(FontURL, FontSize, True, CharSet);
      Result.AdditionalLineSpacing := AdditionalLineSpacing;
    except
      Log(LogInterfaceError, CurrentRoutine, 'Unable to load font ' + FontURL);
    end;
  end;
begin
  Log(LogInit, CurrentRoutine, 'Initializing fonts.');
  InitEncoding;

  DebugFont := DFont.Create(TextureFont_LinBiolinumRG_16);
  AutoFree.Add(DebugFont); //debug font is managed separately from others

  LoadedFonts := DFontDictionary.Create([doOwnsValues]);

  Log(LogInit, CurrentRoutine, 'Loading fonts.');

  LoadedFonts.Add('xolonium-12', GetFontFile(NormalFontFile, FullCharSet, 12, 3));
  LoadedFonts.Add('xolonium-16', GetFontFile(NormalFontFile, FullCharSet, 16, 3));
  LoadedFonts.Add('xolonium-num-99', GetFontFile(NormalFontFile, NumberCharSet, 99, 3));

  FreeEncoding; //as soon as all fonts are loaded, we don't need encoding anymore

  SetFonts;
end;

{-----------------------------------------------------------------------------}

procedure FreeFonts;
begin
  FontDictionary.Free; //will not free children
  LoadedFonts.Free; //will free children
end;

end.

