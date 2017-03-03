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

{ Global variables and definitions for Constructor }
unit constructor_global;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, Forms, SysUtils;

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

{analogue to castleFilesUtils.ApplicationData (and made based on it)
 but points to ARCHITECT directory (true) or ApplicationData (false)}
function ConstructorData(URL: string; ToGameFolder:boolean = true): string;
{get file extension (archive/xml) based on save location}
function FileExtension(zipped: boolean = true): string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleFilesUtils, StrUtils;

function ConstructorData(URL: string; ToGameFolder:boolean = true): string;
begin
  Result := ApplicationData(URL);
  if not ToGameFolder then result := AnsiReplaceText(Result,'/data/','/architect/');
end;

{-----------------------------------------------------------------------------}

function FileExtension(zipped: boolean = true): string;
begin
  if zipped then result := '.gz' else result := '.xml';
end;


end.

