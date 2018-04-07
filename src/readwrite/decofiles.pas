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

(* Implementation of reading and writing routines and relative objects
   this is highly experimental feature for now.
   WARNING: only one file may be open at a time. So, this unit's routines are
   absolutely thread-unsafe. *)

unit DecoFiles;

{$INCLUDE compilerconfig.inc}

interface

uses
  DOM;

type
  { This is an abstract class with support of read and write procedures
    capable of inheritance }
  //RWObject = class abstract(DObject)
  IReadWrite = interface
   ['{8834C4DE-5CA0-4293-8A8A-C633A509B40A}']
    procedure ReadMe;
    procedure WriteMe;
  end;

{ Start reading or writing file }
function StartReadFile(const URL: string): TDOMElement;
function CreateFile(const URL: string): TDOMElement;
{ Finish reading or rwiting file }
procedure WriteFile;
procedure EndReadFile;

{ Pairs of read/write procedures }
procedure WriteInteger(const aParent: TDOMElement; const aName: string; const aInteger: integer);
function ReadInteger(const aParent: TDOMElement; const aName: string): integer;

{............................................................................}
implementation
uses
  CastleXMLUtils, CastleURIUtils,
  DecoHDD, DecoLog;

var
  CurrentFileURL: string;
  FileOpen: boolean = false;

  XMLDoc: TXMLDocument;

{ set the global variables and make a FileOpen check }
procedure PrepareFileOpen(const URL: string);
begin
  if FileOpen then
    Log(LogReadWriteError, CurrentRoutine, 'ERROR: File ' + CurrentFileURL +
      ' was not closed properly!');
  CurrentFileURL := URL;
  FileOpen := true;
end;

{-----------------------------------------------------------------------------}

function StartReadFile(const URL: string): TDOMElement;
begin
  PrepareFileOpen(URL);
  if URIFileExists(CurrentFileURL) then
  begin
    Log(LogFileAccess, CurrentRoutine, 'Reading file ' + CurrentFileURL);
    XMLDoc := URLReadXMLSafe(CurrentFileURL);
    Result := XMLDoc.DocumentElement;
  end
  else
  begin
    Log(LogFileAccess, CurrentRoutine, 'Reading file ' + CurrentFileURL);
    Result := nil;
    FileOpen := false;
  end;
end;

{-----------------------------------------------------------------------------}

procedure EndReadFile;
begin
  if FileOpen then
    XMLDoc.Free
  else
    Log(LogReadWriteError, CurrentRoutine, 'Error: Cannot close file for read. It''s not open! ');

  FileOpen := false;
end;

{-----------------------------------------------------------------------------}

function CreateFile(const URL: string): TDOMElement;
begin
  PrepareFileOpen(URL);
  Log(LogFileAccess, CurrentRoutine, 'Creating file ' + CurrentFileURL);
  XMLDoc := TXMLDocument.Create;
  Result := XMLDoc.CreateElement('Root');
  XMLDoc.AppendChild(Result);
end;

{-----------------------------------------------------------------------------}

procedure WriteFile;
begin
  if (FileOpen) or (XMLDoc = nil) then
  begin
    URLWriteXMLSafe(XMLdoc, CurrentFileURL);
    XMLDoc.Free;
  end
  else
    Log(LogReadWriteError, CurrentRoutine, 'Error: Cannot write file. It''s not open! ');

  FileOpen := false;
end;

{================================ READ/WRITE =================================}

procedure WriteInteger(const aParent: TDOMElement; const aName: string; const aInteger: integer);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aInteger);
end;
function ReadInteger(const aParent: TDOMElement; const aName: string): integer;
begin
  Result := aParent.ChildElement(aName).AttributeInteger('Value');
end;

end.

