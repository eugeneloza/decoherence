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

(* Management of game translations *)

{$INCLUDE compilerconfig.inc}

unit DecoTranslation;

interface

//uses DecoGlobal;

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

uses
  DecoLog;

function LanguageDir(const Lang: TLanguage): string;
begin
  case Lang of
    Language_English: Result := 'ENG/';
    Language_Russian: Result := 'RUS/';
    else
      begin
        Result := 'ENG/';
        Log(LogLanguageError, CurrentRoutine, 'ERROR: Unknown Language in DecoTranslation.LanguageDir! Falling back to English.');
      end;
  end;
  //Result := GetScenarioFolder + TextFolder + Result;
end;

{-----------------------------------------------------------------------------}

function SayLanguage(const Lang: TLanguage): string;
begin
  case Lang of
    Language_English: Result := 'English';
    Language_Russian: Result := 'Russian';
    else
      Result := 'Unknown Language';
  end;
end;

{............................................................................}

procedure InitTranslation;
begin
  {todo: read language here from settings}
  CurrentLanguage := Language_Russian;
  Log(LogInit, CurrentRoutine, 'Current language: ' + SayLanguage(CurrentLanguage));
end;

end.
