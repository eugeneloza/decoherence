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

{$INCLUDE compilerconfig.inc}

unit DecoFontEncoding;

interface
uses
  CastleUnicode;

type
  { csNumeric includes only numbers 0..9 and a floating point symbol
    csASCII includes all typable ASCII characters
    csFull includes ASCII characters and additional local symbols if needed }
  TCharSet = (csNumeric, csASCII, csFull);

var
  { Used to display nubmers, maybe fixed-point, e.g. health }
  NumberCharSet: TUnicodeCharList;
  { Used to display English text, e.g. debug info }
  AsciiCharSet: TUnicodeCharList;
  { Full char set supported in game, always includes ASCII symbols }
  FullCharSet: TUnicodeCharList;

{ Converts a file string to TCharSet }
function StringToCharSet(const aString: string): TCharSet;
{ Converts a TCharSet to a string to write to a file}
function CharSetToString(const aCharSet: TCharSet): string;
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
  //GermanString = 'ÄäÖöÜüẞß';  // not available
  //PolishString = 'ŚĆĘĄŹŁŻÓŃśćęąźłżóń';  // not available

  (* Cyrillic languages *)
  CyrillicString = 'ЙЦУКЕНГШЩЗХФВАПРОЛДЖЯЧСМИТЬБЮйцукенгшщзхфвапролджячсмитьбю';
  RussianString = 'ЁЪЫЭёъыэ';
  //UkrainianString = 'ЇІЄҐїієґ'; // not available

function isLanguageCyrillic(const aLanguage: TLanguage): boolean;
begin
  if aLanguage in [Language_Russian] then
    Result := true
  else
    Result := false;
end;

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
  if isLanguageCyrillic(CurrentLanguage) then FullCharSet.Add(CyrillicString);

  //add language-specific characters
  case CurrentLanguage of
    //Language_English: basic ASCII is enough
    //Language_German: FullCharSet.Add(GermanString); // not available
    //Language_Polish: FullCharSet.Add(PolishString); // not available
    Language_Russian: FullCharSet.Add(RussianString);
    //Language_Ukrainian: FullCharSet.Add(RussianString); // not available
  end;
end;

{-----------------------------------------------------------------------------}

function CharSetToString(const aCharSet: TCharSet): string;
begin
  case aCharSet of
    csNumeric: Result := 'csNumeric';
    csASCII: Result := 'csASCII';
    csFull: Result := 'csFull';
    else
      Log(LogFontError, CurrentRoutine, 'Error: unknown CharSet!');
  end;
end;

{-----------------------------------------------------------------------------}

function StringToCharSet(const aString: string): TCharSet;
begin
  case aString of
    'csNumeric': Result := csNumeric;
    'csASCII': Result := csASCII;
    'csFull': Result := csFull;
    else
      Log(LogFontError, CurrentRoutine, 'Error: unknown CharSet!');
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

