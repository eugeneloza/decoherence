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

{ Management of game translations }
unit decotranslation;

{$INCLUDE compilerconfig.inc}
interface

//uses SysUtils;

type TLanguage = (Language_English, Language_Russian);

var CurrentLanguage: TLanguage = Language_Russian;

{Provides a name for the current language directory without backslashes}
function LanguageDir(Lang: TLanguage): string;
{Says the language name in English}
function SayLaugnage(Lang: TLanguage): string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils;

function LanguageDir(Lang: TLanguage): string;
begin
  case Lang of
    language_English: result := 'ENG/';
    language_Russian: result := 'RUS/';
    else raise Exception.Create('Unknown Language in decotranslation.LanguageDir!');
  end;
end;

function SayLaugnage(Lang: TLanguage): string;
begin
  case Lang of
    language_English: Result := 'English';
    language_Russian: Result := 'Russian';
    else              Result := 'Unknown Language';
  end;
end;

end.

