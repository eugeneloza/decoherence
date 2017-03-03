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

var CurrentLanguage: TLanguage;

{Provides a name for the current language directory without backslashes}
function LanguageDir: string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils;

function LanguageDir: string;
begin
  case CurrentLanguage of
    language_English: result := 'ENG/';
    language_Russian: result := 'RUS/';
    else raise Exception.Create('Unknown Language in global.LanguageDir!');
  end;
end;

end.

