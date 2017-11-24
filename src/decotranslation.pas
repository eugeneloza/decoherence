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
unit DecoTranslation;

{$INCLUDE compilerconfig.inc}

interface

//uses SysUtils;

type
  { Enumerates all available languages. To add another language, first step
   is to add it to this unit }
  TLanguage = (Language_English, Language_Russian);

var
  { Current game language }
  CurrentLanguage: TLanguage = Language_Russian;

{ Provides a name for the current language directory without backslashes }
function LanguageDir(const Lang: TLanguage): string;
{ Says the language name in English }
function SayLanguage(const Lang: TLanguage): string;
{ Displays a "Loading..." image for the language }
procedure SetLoadingImage;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils,
  CastleControls, DecoLoadEmbedded, {needed to use load image}
  DecoGlobal;

function LanguageDir(const Lang: TLanguage): string;
begin
  case Lang of
    Language_English: Result := 'ENG/';
    Language_Russian: Result := 'RUS/';
    else raise Exception.Create('Unknown Language in DecoTranslation.LanguageDir!');
  end;
  Result := GetScenarioFolder + TextFolder + Result;
end;

{-----------------------------------------------------------------------------}

function SayLanguage(const Lang: TLanguage): string;
begin
  case Lang of
    Language_English: Result := 'English';
    Language_Russian: Result := 'Russian';
    else              Result := 'Unknown Language';
  end;
end;

{-----------------------------------------------------------------------------}

{thanks to Michalis, it's simple :) see https://github.com/eugeneloza/decoherence/issues/22}
procedure SetLoadingImage;
begin
  {no need yet}
  //Theme.LoadingBackgroundColor := Black; // adjust as needed
  //Theme.LoadingTextColor := White; // adjust as needed
  case CurrentLanguage of
    Language_English: Theme.Images[tiLoading] := Loading_eng;
    Language_Russian: Theme.Images[tiLoading] := Loading_rus;
    else              Theme.Images[tiLoading] := Loading_eng; //we don't have Log initialized here yet. So, don't report this error, just fall back to English.
  end;

  Theme.OwnsImages[tiLoading] := false;
end;

end.

