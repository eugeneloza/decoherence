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

(* Management of font reading and writing *)

{$INCLUDE compilerconfig.inc}

unit DecoFontFile;

interface

uses
  Generics.Collections,
  DOM,
  DecoGenerics, DecoFontEncoding;

type
  { For reading-writing a Font file
    Specifies all required paramters to initialize a specific font }
  DFontInfo = record
    { Relative URL to the file }
    URL: string;
    { Size of the font }
    Size: integer;
    { Additional spacing between font lines }
    AdditionalLineSpacing: integer;
    { What char set should this font use (numbers/ascii/full) }
    CharSet: TCharSet;
  end;

type
  { Dictionary of DFontInfo to read/write all fonts for the game }
  DFontInfoDictionary = specialize TDictionary<string, DFontInfo>;

var
  { Info of all the fonts in game
    used only for loading and in Constructor }
  FontInfo: DFontInfoDictionary;
  { Link between font alias used in game and font instance alias
    used only for loading and in constructor }
  FontAlias: DStringDictionary;

{ Read/write all fonts data (FontInfo and FontAlias) }
function ReadFontsInfo: boolean;
function WriteFontsInfo: boolean;
{}
procedure DefaultFontInfo;
{}
procedure FreeFontsInfo;
{............................................................................}
implementation
uses
  SysUtils, CastleXMLUtils,
  DecoFiles, DecoFolders,
  DecoLog;

procedure WriteFontInfo(const aParent: TDOMElement; const aName: string; const aValue: DFontInfoDictionary);
var
  ContainerNode: TDOMElement;
  WorkNode: TDOMElement;
  i: integer;
  v: string;
  f: DFontInfo;
begin
  if aValue = nil then
  begin
    Log(LogFontError, CurrentRoutine, 'ERROR: DFontInfoDictionary is nil!');
    Exit;
  end;

  ContainerNode := aParent.CreateChild(aName);
  i := 0;
  for v in aValue.keys do
  begin
    WorkNode := ContainerNode.CreateChild('Font_' + i.ToString);
    WorkNode.AttributeSet('Alias', v);
    if aValue.TryGetValue(v, f) then
    begin
      WorkNode.AttributeSet('URL', f.URL);
      WorkNode.AttributeSet('Size', f.Size);
      WorkNode.AttributeSet('AdditionalLineSpacing', f.AdditionalLineSpacing);
      WorkNode.AttributeSet('Charset', CharSetToString(f.CharSet));
    end
    else
      Log(LogFontError, CurrentRoutine, 'Cannot find font alias ' + v);
    inc(i);
  end;
end;

function ReadFontInfo(const aParent: TDOMElement; const aName: string): DFontInfoDictionary;
var
  Iterator: TXMLElementIterator;
  WorkNode: TDOMElement;
  f: DFontInfo;
begin
  Result := DFontInfoDictionary.Create;
  Iterator := aParent.ChildElement(aName).ChildrenIterator;
  try
    while Iterator.GetNext do
    begin
      WorkNode := Iterator.Current;
      f.URL := WorkNode.AttributeString('URL');
      f.Size := WorkNode.AttributeInteger('Size');
      f.AdditionalLineSpacing := WorkNode.AttributeInteger('AdditionalLineSpacing');
      f.CharSet := StringToCharSet(WorkNode.AttributeString('CharSet'));
      Result.Add(WorkNode.AttributeString('Alias'), f);
    end;
  finally
    FreeAndNil(Iterator);
  end;
end;

{--------------------------------------------------------------------------}

function ReadFontsInfo: boolean;
var
  RootNode: TDOMElement;
begin
  RootNode := StartReadFile(GameFolder('GUI/Fonts/Fonts.xml'));
  if RootNode = nil then
    Result := false
  else
  begin
    { Read config from a file }
    FontInfo := ReadFontInfo(RootNode, 'FontInfo');
    FontAlias := ReadStringDictionary(RootNode, 'FontAlias');
    EndReadFile;
    Result := true;
  end;
end;

{--------------------------------------------------------------------------}

function WriteFontsInfo: boolean;
var
  RootNode: TDOMElement;
begin
  RootNode := CreateFile(GameFolder('GUI/Fonts/Fonts.xml'));
  Result := not (RootNode = nil);
  if RootNode <> nil then
  begin
    WriteFontInfo(RootNode, 'FontInfo', FontInfo);
    WriteStringDictionary(RootNode, 'FontAlias', FontAlias);
    WriteFile;
  end;
end;

{--------------------------------------------------------------------------}

procedure DefaultFontInfo;
var
  f: DFontInfo;
begin
  Log(LogFontWarning, CurrentRoutine, 'Warning: Loading default fonts configuration.');
  FontInfo := DFontInfoDictionary.Create;

  f.URL := 'Xolonium-Regular.ttf';
  f.Size := 12;
  f.CharSet := csFull;
  f.AdditionalLineSpacing := 3;
  FontInfo.Add('xolonium-12', f);

  f.URL := 'Xolonium-Regular.ttf';
  f.Size := 16;
  f.CharSet := csFull;
  f.AdditionalLineSpacing := 3;
  FontInfo.Add('xolonium-16', f);

  f.URL := 'Xolonium-Regular.ttf';
  f.Size := 99;
  f.CharSet := csNumeric;
  f.AdditionalLineSpacing := 3;
  FontInfo.Add('xolonium-num-99', f);

  FontAlias := DStringDictionary.Create;
  FontAlias.Add('Default', 'xolonium-12');
  FontAlias.Add('PlayerHealth', 'xolonium-12');
  FontAlias.Add('PlayerName', 'xolonium-12');
  FontAlias.Add('LoadScreen', 'xolonium-16');
  FontAlias.Add('PlayerDamage', 'xolonium-num-99');

end;

{--------------------------------------------------------------------------}

procedure FreeFontsInfo;
begin
  FreeAndNil(FontInfo);
end;

end.

