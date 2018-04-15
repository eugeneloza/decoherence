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
  { For reading-writing a Font file }
  DFontInfo = record
    {}
    URL: string;
    {}
    Size: integer;
    {}
    AdditionalLineSpacing: integer;
    {}
    Charset: TCharSet;
  end;

type
  {}
  DFontInfoDictionary = specialize TDictionary<string, DFontInfo>;

var
  {}
  FontInfo: DFontInfoDictionary;
  {}
  FontAlias: DStringDictionary;

{}
function ReadFonts: boolean;
function WriteFonts: boolean;
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
      WorkNode.AttributeSet('Charset', CharSetToString(f.Charset));
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
      f.Charset := StringToCharSet(WorkNode.AttributeString('Charset'));
      Result.Add(WorkNode.AttributeString('Alias'), f);
    end;
  finally
    FreeAndNil(Iterator);
  end;
end;

{--------------------------------------------------------------------------}

function ReadFonts: boolean;
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

function WriteFonts: boolean;
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

end.

