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
  CastleTextureFont_LinBiolinumRG_16, //a debug font
  DecoTrash, DecoLog;

function GetFontByName(const FontName: string): DFont; TryInline
begin
  Result := DefaultFont;
end;

{............................................................................}

procedure InitFonts;
begin
  Log(LogInit, CurrentRoutine, 'Loading fonts.');
  InitEncoding;

  DebugFont := DFont.Create(TextureFont_LinBiolinumRG_16);
  AutoFree.Add(DebugFont);
  DefaultFont := DebugFont;

  FreeEncoding; //as soon as all fonts are loaded, we don't need encoding anymore
end;

{-----------------------------------------------------------------------------}

procedure FreeFonts;
begin
  {actually everything should be freed automatically,
   but let it remain here for now.}
end;

end.

