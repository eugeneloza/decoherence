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

(* Presets for different encodings *)

unit DecoFontEncoding;

{$INCLUDE compilerconfig.inc}

interface
uses
  CastleUnicode;

var
  { Used to display nubmers, maybe fixed-point, e.g. health }
  NumberCharSet: TUnicodeCharList;
  { Used to display English text, e.g. debug info }
  AsciiCharSet: TUnicodeCharList;
  { Full char set supported in game, always includes ASCII symbols }
  FullCharSet: TUnicodeCharList;

{ Initialize char sets }
procedure InitEncoding;
{ Free char sets after texture fonts are initialized }
procedure FreeEncoding;
{............................................................................}
implementation
uses
  SysUtils, CastleStringUtils,
  DecoTranslation, DecoLog;

const
  { simple numbrs for a label, displaying damage }
  NumberString = '0123456789.';  //maybe add ":" (for clock) and "+"/"-" here thou they would me more likely to be in "normal" fonts

  (* Latin languages *)
  AsciiString = SimpleAsciiCharacters;
  //GermanString = 'ÄäÖöÜüẞß';

  (* Cyrillic languages *)
  CyrillicString = 'ЙЦУКЕНГШЩЗХФВАПРОЛДЖЯЧСМИТЬБЮйцукенгшщзхфвапролджячсмитьбю';
  RussianString = 'ЁЪЫЭёъыэ';
  //UkrainianString = 'ЇІЄїіє';

procedure InitEncoding;
begin
  Log(LogInterfaceInfo, CurrentRoutine, 'Init encoding.');

  // create basic char sets
  NumberCharSet := TUnicodeCharList.Create;
  NumberCharSet.Add(NumberString);
  AsciiCharSet := TUnicodeCharList.Create;
  AsciiCharSet.Add(AsciiString);

  //initialize a full char set depending on current game language
  FullCharSet := TUnicodeCharList.Create;
  FullCharSet.Add(AsciiString);

  //add language-specific characters
  case CurrentLanguage of
    //Language_English: basic ASCII is enough
    Language_Russian: begin
                        FullCharSet.Add(CyrillicString);
                        FullCharSet.Add(RussianString);
                      end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure FreeEncoding;
begin
  Log(LogInterfaceInfo, CurrentRoutine, 'Free encoding.');
  FreeAndNil(NumberCharSet);
  FreeAndNil(AsciiCharSet);
  FreeAndNil(FullCharSet);
end;

end.

