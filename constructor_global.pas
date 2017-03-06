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
  Classes, Forms, SysUtils,
  decotranslation;

type
  TWriterForm = class(TForm)
  private
    fisLoaded: boolean;
    fisChanged: boolean;
    fMyLanguage: TLanguage;
  public
    property isLoaded: boolean read fisLoaded write fisLoaded default false;
    property isChanged: boolean read fisChanged write fisChanged default false;
    property MyLanguage: TLanguage read fMyLanguage write fMyLanguage; //if applicable
    procedure LoadMe; virtual; abstract;
    procedure FreeMe; virtual; abstract;
    procedure WriteMe(ToGameFolder: boolean); virtual; abstract;
  end;

var ConstructorLanguage: TLanguage;

{analogue to castleFilesUtils.ApplicationData (and made based on it)
 but points to ARCHITECT directory (true) or ApplicationData (false)}
function ConstructorData(URL: string; ToGameFolder:boolean): string;
{Desktop-only ApplicationData analogue. Replacement for ApplicationData for
 native FPC functions that don't work with URLs}
function FakeApplicationData(URL: string): string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleFilesUtils, StrUtils;

function ConstructorData(URL: string; ToGameFolder:boolean): string;
begin
  Result := ApplicationData(URL);
  if not ToGameFolder then begin
    result := AnsiReplaceText(Result,'/data/','/architect/');
    //invoke data compression
    {$IFDEF gzipdata}result := AnsiReplaceText(Result,'.gz','.xml');{$ENDIF}
  end;
end;

function FakeApplicationData(URL: string): string;
begin
  Result := 'data/'+URL;
  Result := AnsiReplaceText(Result,'/',pathdelim); //we're using native OS file access
end;

{-----------------------------------------------------------------------------}


end.

