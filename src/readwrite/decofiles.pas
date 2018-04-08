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
  DOM, CastleVectors,
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

{ Start reading or writing file }
function StartReadFile(const URL: string): TDOMElement;
function CreateFile(const URL: string): TDOMElement;
{ Finish reading or rwiting file }
procedure WriteFile;
procedure EndReadFile;

{ Pairs of read/write procedures }
procedure WriteInteger(const aParent: TDOMElement; const aName: string; const aValue: integer);
function ReadInteger(const aParent: TDOMElement; const aName: string): integer;
procedure WriteBoolean(const aParent: TDOMElement; const aName: string; const aValue: boolean);
function ReadBoolean(const aParent: TDOMElement; const aName: string): boolean;
procedure WriteFloat(const aParent: TDOMElement; const aName: string; const aValue: DFloat);
function ReadFloat(const aParent: TDOMElement; const aName: string): DFloat;
procedure WriteString(const aParent: TDOMElement; const aName: string; const aValue: string);
function ReadString(const aParent: TDOMElement; const aName: string): string;

procedure WriteVector2(const aParent: TDOMElement; const aName: string; const aValue: TVector2);
function ReadVector2(const aParent: TDOMElement; const aName: string): TVector2;
procedure WriteVector3(const aParent: TDOMElement; const aName: string; const aValue: TVector3);
function ReadVector3(const aParent: TDOMElement; const aName: string): TVector3;
procedure WriteVector4(const aParent: TDOMElement; const aName: string; const aValue: TVector4);
function ReadVector4(const aParent: TDOMElement; const aName: string): TVector4;
procedure WriteVector2int(const aParent: TDOMElement; const aName: string; const aValue: TVector2Integer);
function ReadVector2int(const aParent: TDOMElement; const aName: string): TVector2Integer;
procedure WriteVector3int(const aParent: TDOMElement; const aName: string; const aValue: TVector3Integer);
function ReadVector3int(const aParent: TDOMElement; const aName: string): TVector3Integer;
procedure WriteVector4int(const aParent: TDOMElement; const aName: string; const aValue: TVector4Integer);
function ReadVector4int(const aParent: TDOMElement; const aName: string): TVector4Integer;

{............................................................................}
implementation
uses
  CastleXMLUtils, CastleURIUtils,
  DecoMathVectors,
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
    Log(LogFileAccess, CurrentRoutine, 'WARNING: File does not exist: ' + CurrentFileURL);
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

procedure WriteInteger(const aParent: TDOMElement; const aName: string; const aValue: integer);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadInteger(const aParent: TDOMElement; const aName: string): integer;
begin
  Result := aParent.ChildElement(aName).AttributeInteger('Value');
end;
procedure WriteBoolean(const aParent: TDOMElement; const aName: string; const aValue: boolean);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadBoolean(const aParent: TDOMElement; const aName: string): boolean;
begin
  Result := aParent.ChildElement(aName).AttributeBoolean('Value');
end;
procedure WriteFloat(const aParent: TDOMElement; const aName: string; const aValue: DFloat);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadFloat(const aParent: TDOMElement; const aName: string): DFloat;
begin
  Result := aParent.ChildElement(aName).AttributeFloat('Value');
end;
procedure WriteString(const aParent: TDOMElement; const aName: string; const aValue: string);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadString(const aParent: TDOMElement; const aName: string): string;
begin
  Result := aParent.ChildElement(aName).AttributeString('Value');
end;
procedure WriteVector2(const aParent: TDOMElement; const aName: string; const aValue: TVector2);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadVector2(const aParent: TDOMElement; const aName: string): TVector2;
begin
  Result := aParent.ChildElement(aName).AttributeVector2('Value');
end;
procedure WriteVector3(const aParent: TDOMElement; const aName: string; const aValue: TVector3);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadVector3(const aParent: TDOMElement; const aName: string): TVector3;
begin
  Result := aParent.ChildElement(aName).AttributeVector3('Value');
end;
procedure WriteVector4(const aParent: TDOMElement; const aName: string; const aValue: TVector4);
begin
  aParent.CreateChild(aName).AttributeSet('Value', aValue);
end;
function ReadVector4(const aParent: TDOMElement; const aName: string): TVector4;
begin
  Result := aParent.ChildElement(aName).AttributeColor('Value');
end;

{ we're using a relatively clean hack to convert integer->float->integer
  here through DecoMathVectors as there is no support
  for directly reading/writing integer vectors in CastleXMLUtils at the moment
  (and might be unneeded actually }
procedure WriteVector2int(const aParent: TDOMElement; const aName: string; const aValue: TVector2Integer);
begin
  aParent.CreateChild(aName).AttributeSet('Value', VectorIntegerToFloat(aValue));
end;
function ReadVector2int(const aParent: TDOMElement; const aName: string): TVector2Integer;
begin
  Result := VectorFloatToInteger(aParent.ChildElement(aName).AttributeVector2('Value'));
end;
procedure WriteVector3int(const aParent: TDOMElement; const aName: string; const aValue: TVector3Integer);
begin
  aParent.CreateChild(aName).AttributeSet('Value', VectorIntegerToFloat(aValue));
end;
function ReadVector3int(const aParent: TDOMElement; const aName: string): TVector3Integer;
begin
  Result := VectorFloatToInteger(aParent.ChildElement(aName).AttributeVector3('Value'));
end;
procedure WriteVector4int(const aParent: TDOMElement; const aName: string; const aValue: TVector4Integer);
begin
  aParent.CreateChild(aName).AttributeSet('Value', VectorIntegerToFloat(aValue));
end;
function ReadVector4int(const aParent: TDOMElement; const aName: string): TVector4Integer;
begin
  Result := VectorFloatToInteger(aParent.ChildElement(aName).AttributeVector4('Value'));
end;



end.

