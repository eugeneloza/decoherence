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

(* Management of game translations *)

unit DecoTranslation;

{$INCLUDE compilerconfig.inc}

interface

uses DecoGlobal;

type
  { Enumerates all available languages. To add another language, first step
   is to add it to this unit }
  TLanguage = (Language_English, Language_Russian);

var
  { Current game language }
  CurrentLanguage: TLanguage;

{ Provides a name for the current language directory }
function LanguageDir(const Lang: TLanguage): string;
{ Says the language name in English }
function SayLanguage(const Lang: TLanguage): string;
{ Read language }
procedure InitTranslation;
{............................................................................}
implementation

//uses SysUtils;

function LanguageDir(const Lang: TLanguage): string;
begin
  {StartProfiler}

  case Lang of
    Language_English: Result := 'ENG/';
    Language_Russian: Result := 'RUS/';
    else
      raise Exception.Create('Unknown Language in DecoTranslation.LanguageDir!');
  end;
  //Result := GetScenarioFolder + TextFolder + Result;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

function SayLanguage(const Lang: TLanguage): string;
begin
  {StartProfiler}

  case Lang of
    Language_English: Result := 'English';
    Language_Russian: Result := 'Russian';
    else
      Result := 'Unknown Language';
  end;

  {StopProfiler}
end;

{............................................................................}

procedure InitTranslation;
begin
  {todo: read language here from settings}
  CurrentLanguage := Language_Russian;
end;

end.
