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

{$INCLUDE compilerconfig.inc}

unit DecoFiles;

interface

uses
  Classes, DOM, CastleVectors,
  DecoGlobal;

type
  { This is a reference to a generic list enumerator procedure
    which should be a local/nested procedure in the writing routine

    like this:
      procedure SomeNestedProcedure(constref aParent: TDOMElement);
      var
        i: TObject1;
      begin
        for i in List do
          i.WriteMe(aParent);
      end;
  }
  TListWriterProcedure = procedure(constref aParent: TDOMElement) is nested;

  { This is a reference to a generic list reader procedure
    which should be a local/nested procedure in the reading routine
    it creates and reads the object

    like this:
      procedure SomeNestedProcedure(constref aParent: TDOMElement);
      begin
        List.Add(TObject1.Create.ReadMe(aParent));
      end;

    I also highly insist on making a wrapping function to create and
    read the whole generic list

    like this:
    function ReadGenericList(aParentBase: TDomElement): TGenericList;
    var
      GenericList: TGenericList;
      procedure SomeNestedProcedure(constref aParent: TDOMElement);
      begin
        GenericList.Add(TObject1.Create.ReadMe(aParent)); //Maybe I can use Result here?
      end;
    begin
      GenericList := TGenericList.Create;
      ReadList(aParentBase, 'ThisListName', @SomeNestedProcedure);
      Result := GenericList;
    end;
  }
  TListReaderProcedure = procedure(constref aParent: TDOMElement) is nested;

type
  { This is an abstract class with support of read and write procedures
    capable of inheritance }
  //RWObject = class abstract(DObject)
  IReadWrite = interface
   ['{8834C4DE-5CA0-4293-8A8A-C633A509B40A}']
    procedure ReadMe(constref aParent: TDOMElement);
    procedure WriteMe(constref aParent: TDOMElement);
  end;

{type
  TInterfaceLink = function: IReadWrite;}

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

{ Reading and writing float vectors }
procedure WriteVector2(const aParent: TDOMElement; const aName: string; const aValue: TVector2);
function ReadVector2(const aParent: TDOMElement; const aName: string): TVector2;
procedure WriteVector3(const aParent: TDOMElement; const aName: string; const aValue: TVector3);
function ReadVector3(const aParent: TDOMElement; const aName: string): TVector3;
procedure WriteVector4(const aParent: TDOMElement; const aName: string; const aValue: TVector4);
function ReadVector4(const aParent: TDOMElement; const aName: string): TVector4;
{ Reading and writing integer vectors,
  Caution, integer vectors are managed through a "hack"
  and therefore there is no type-checking during reading
  (i.e. reading a float vector would just round it to integer) }
procedure WriteVector2int(const aParent: TDOMElement; const aName: string; const aValue: TVector2Integer);
function ReadVector2int(const aParent: TDOMElement; const aName: string): TVector2Integer;
procedure WriteVector3int(const aParent: TDOMElement; const aName: string; const aValue: TVector3Integer);
function ReadVector3int(const aParent: TDOMElement; const aName: string): TVector3Integer;
procedure WriteVector4int(const aParent: TDOMElement; const aName: string; const aValue: TVector4Integer);
function ReadVector4int(const aParent: TDOMElement; const aName: string): TVector4Integer;

{ Write simple predefined generic lists }
procedure WriteStringList(const aParent: TDOMElement; const aName: string; const aValue: TStringList);
function ReadStringList(const aParent: TDOMElement; const aName: string): TStringList;

{ This is an ugly endeavour to automatize reading of a generic lists
  See examples of how aWriterProcedure/aReaderProcedure should look like
  Pay attention, that ReadList is a procedure, not a function,
  so the generic list and all its children should be created in host reading routines }
procedure WriteList(const aParent: TDOMElement; const aName: string; const aWriterProcedure: TListWriterProcedure);
procedure ReadList(const aParent: TDOMElement; const aName: string; const aReaderProcedure: TListReaderProcedure);
{............................................................................}
implementation
uses
  SysUtils,
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

procedure WriteStringList(const aParent: TDOMElement; const aName: string; const aValue: TStringList);
var
  ContainerNode: TDOMElement;
  i: integer;
begin
  ContainerNode := aParent.CreateChild(aName);
  for i := 0 to Pred(aValue.Count) do
    ContainerNode.CreateChild('String_' + i.ToString).AttributeSet('Value', aValue[i]);
end;
function ReadStringList(const aParent: TDOMElement; const aName: string): TStringList;
var
  Iterator: TXMLElementIterator;
begin
  Result := TStringList.Create;
  Iterator := aParent.ChildElement(aName).ChildrenIterator;
  try
    while Iterator.GetNext do
      Result.Add(Iterator.Current.AttributeString('Value'));
  finally
    FreeAndNil(Iterator);
  end;
end;

procedure WriteList(const aParent: TDOMElement; const aName: string; const aWriterProcedure: TListWriterProcedure);
var
  ContainerNode: TDOMElement;
begin
  ContainerNode := aParent.CreateChild(aName);
  aWriterProcedure(ContainerNode);
end;
procedure ReadList(const aParent: TDOMElement; const aName: string; const aReaderProcedure: TListReaderProcedure);
var
  Iterator: TXMLElementIterator;
begin
  Iterator := aParent.ChildElement(aName).ChildrenIterator;
  try
    while Iterator.GetNext do
      aReaderProcedure(Iterator.Current);
  finally
    FreeAndNil(Iterator);
  end;
end;

end.

