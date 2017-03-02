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

{ ******** Unit description here ****** }
unit constructor_global;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, Forms, SysUtils;

{type
  IWriter = interface
    ['{0A2E585A-7FEF-4169-B926-2B78E5343FDE}']
    procedure WriteMe(ToGameFolder: boolean);
    procedure LoadMe;
  end;}

type
  TWriterForm = class(TForm)
  private
    fisLoaded: boolean;
    fisChanged: boolean;
  public
    property isLoaded: boolean read fisLoaded write fisLoaded default false;
    property isChanged: boolean read fisChanged write fisChanged default false;
    procedure LoadMe; virtual; abstract;
    procedure WriteMe(ToGameFolder: boolean); virtual; abstract;
  end;

type TLanguage = (Language_English,Language_Russian);

var CurrentLanguage: TLanguage;

{Provides a name for the current language directory without backslashes}
function LanguageDir: string;
{analogue to castleFilesUtils.ApplicationData (and made based on it)
 but points to ARCHITECT directory (true) or ApplicationData (false)
 TODO
 AT THIS MOMENT IT JUST POINTS TO ApplicationData RESULT}
function ConstructorData(URL: string; ToGameFolder:boolean = true): string;

function FileExtension(zipped: boolean = true): string;

implementation

uses CastleFilesUtils;

function LanguageDir: string;
begin
  case CurrentLanguage of
    language_English: result := 'ENG/';
    language_Russian: result := 'RUS/';
    else raise Exception.Create('Unknown Language in global.LanguageDir!');
  end;

end;

function ConstructorData(URL: string; ToGameFolder:boolean = true): string;
begin
  Result := ApplicationData(URL);
  if not ToGameFolder then ;//todo: replace '/data/' for '/architect/'
end;

function FileExtension(zipped: boolean = true): string;
begin
  if zipped then result := '.gz' else result := '.xml';
end;


end.

