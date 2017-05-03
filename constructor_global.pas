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

const
  {all data that will remain in Architect folder and used only in Constructor}
  LocalFolder = 'local/';

type
  {a generic Form with abstract write/load and several other abstract
   routines to simplify and automate constructor's data management}
  TWriterForm = class(TForm)
  private
    fisLoaded: boolean;
    fisChanged: boolean;
    fMyLanguage: TLanguage;
  public
    {if the TWriterForm instance is loaded from the Architect?}
    property isLoaded: boolean read fisLoaded write fisLoaded default false;
    {Was the TWriterForm changed since last save/load?}
    property isChanged: boolean read fisChanged write fisChanged default false;
    {TWriterForm current displayed language (if appliccable / else unused)}
    property MyLanguage: TLanguage read fMyLanguage write fMyLanguage;
    {TWriterForm abstract load procedure}
    procedure LoadMe; virtual; abstract;
    {TWriterForm abstract save procedure}
    procedure WriteMe(ToGameFolder: boolean); virtual;
    {TWriterForm abstract destructor}
    procedure FreeMe; virtual; abstract;
  end;

var ConstructorLanguage: TLanguage;

{analogue to castleFilesUtils.ApplicationData (and made based on it)
 but points to ARCHITECT directory (true) or ApplicationData (false)
 WARNING! This is a buggy procedure. No folder can be named "data" in the game files!!!}
function ConstructorData(URL: string; ToGameFolder:boolean): string;
{Desktop-only ApplicationData analogue. Replacement for ApplicationData for
 native FPC functions that don't work with URLs
 The result is relative to application folder!}
function FakeConstructorData(URL: string; ToGameFolder:boolean): string;

{searches a StringList for the specific element}
function StringListContains(SL: TStringList; search: string): boolean;

{reads a specific file extensions from a specific path
 Creates a TStringList, don't forget to free manually
 !Android incompatible}
function GetFilesList(path,ext: string): TStringList;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleFilesUtils, StrUtils, CastleLog;

{case-sensitive replace the last occurence of searchstring to replacestring}
procedure ReplaceStringReverse(var s: string; const searchstring,replacestring: string);
var i: integer;
begin
  for i := length(s)-length(searchstring) downto 0 do
    if copy(s,i,length(searchstring)) = searchstring then begin
      s := copy(s,0,i-1) + replacestring + copy(s,i+length(searchstring),length(s));
      break;
    end;
end;

{-----------------------------------------------------------------------------}

function ConstructorData(URL: string; ToGameFolder:boolean): string;
begin
  Result := ApplicationData(URL);
  if not ToGameFolder then begin
    ReplaceStringReverse(Result,'/data/','/architect/');
    //{$Warning folder names cannot contain /data/ folder!}
    //result := AnsiReplaceText(Result,'/data/','/architect/');
    //invoke data compression
    {$IFDEF gzipdata}result := AnsiReplaceText(Result,'.gz','.xml');{$ENDIF}
  end;
end;

{-----------------------------------------------------------------------------}

function FakeConstructorData(URL: string; ToGameFolder:boolean): string;
begin
  if ToGameFolder then
    Result := 'data/'+URL
  else
    Result := 'architect/'+URL;
  Result := AnsiReplaceText(Result,'/',pathdelim); //we're using native OS file access
end;

{-----------------------------------------------------------------------------}

function StringListContains(SL: TStringList; search: string): boolean;
var s: string;
begin
  Result := false;
  if SL=nil then begin
    WriteLnLog('StringListContains','ERROR: String List is nil!');
    exit;
  end;
  for s in SL do if s=search then begin
    Result := true;
    break;
  end;
end;

{-----------------------------------------------------------------------------}

function GetFilesList(path,ext: string): TStringList;
var Rec: TSearchRec;
begin
  Result := TStringList.create;
  // Android incompatible
  if FindFirst (FakeConstructorData(path + '*.'+ext,false), faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       Result.Add(AnsiReplaceText(Rec.Name,'.'+ext,''));
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
end;

{============================================================================}

procedure TWriterForm.WriteMe(ToGameFolder: boolean);
begin
  if not ToGameFolder then isChanged := false;
end;


end.

