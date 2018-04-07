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
   this is highly experimental feature for now. *)

unit DecoFiles;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

type
  { This is an abstract class with support of read and write procedures
    capable of inheritance }
  //RWObject = class abstract(DObject)
  IReadWrite = interface
   ['{8834C4DE-5CA0-4293-8A8A-C633A509B40A}']
    procedure ReadMe;
    procedure WriteMe;
  end;

{}
procedure StartReadFile(const URL: string);
{}
procedure EndReadFile;
{}
procedure CreateFile(const URL: string);
{}
procedure WriteFile;

{............................................................................}
implementation
uses
  DOM, CastleXMLUtils,
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

procedure StartReadFile(const URL: string);
begin
  PrepareFileOpen(URL);
  Log(LogFileAccess, CurrentRoutine, 'Reading file ' + CurrentFileURL);
  XMLDoc := URLReadXMLSafe(CurrentFileURL);
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

procedure CreateFile(const URL: string);
begin
  PrepareFileOpen(URL);
  Log(LogFileAccess, CurrentRoutine, 'Creating file ' + CurrentFileURL);
  XMLdoc := TXMLDocument.Create;
end;

{-----------------------------------------------------------------------------}

procedure WriteFile;
begin
  if FileOpen then
  begin
    URLWriteXMLSafe(XMLdoc, CurrentFileURL);
    XMLDoc.Free;
  end
  else
    Log(LogReadWriteError, CurrentRoutine, 'Error: Cannot close file for read. It''s not open! ');

  FileOpen := false;
end;

{-----------------------------------------------------------------------------}

end.

