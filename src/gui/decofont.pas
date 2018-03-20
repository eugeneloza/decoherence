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
  CastleFonts,
  DecoFontEncoding,
  DecoGlobal;

const
  dLineBreak = sLineBreak;


type
  {}
  DFont = class(TTextureFont)
  end;

var
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
  Generics.Collections,
  CastleTextureFont_LinBiolinumRG_16, //a debug font
  DecoTrash, DecoLog;

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

procedure InitFonts;
begin
  Log(LogInit, CurrentRoutine, 'Initializing fonts.');
  InitEncoding;

  DebugFont := DFont.Create(TextureFont_LinBiolinumRG_16);
  AutoFree.Add(DebugFont); //debug font is managed separately from others
  DefaultFont := DebugFont;

  LoadedFonts := DFontDictionary.Create([doOwnsValues]);
  FontDictionary := DFontDictionary.Create([]);


  Log(LogInit, CurrentRoutine, 'Loading fonts.');

  FreeEncoding; //as soon as all fonts are loaded, we don't need encoding anymore
end;

{-----------------------------------------------------------------------------}

procedure FreeFonts;
begin
  FontDictionary.Free; //will not free children
  LoadedFonts.Free; //will free children
end;

end.

