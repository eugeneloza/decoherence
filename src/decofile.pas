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

{ Reading and Writing XML files wrapper to automate more functions
  Not sure how efficient this "automation" is...}
unit DecoFile;

{$INCLUDE compilerconfig.inc}

interface

uses DecoGlobal, CastleFilesUtils,
  DOM, CastleXMLUtils;

type TSimpleMethod = procedure of object;

type
  { the most generic data module }
  DDataModule = class abstract(DObject)
  public
    Parent: TXMLDocument;
    {}
    {$IFDEF Constructor}ToGameFolder: boolean;{$ELSE}const ToGameFolder = true;{$ENDIF}
  public
    {}
    isChanged: boolean;
    {}
    function WriteModule: TDOMNode; virtual;
    {}
    procedure ReadModule(const aParent: TDOMElement); virtual;
  {$IFDEF Constructor}
  public
    {}
    procedure ConstructInterface; virtual; abstract;
    {}
    procedure ReadInterface; virtual; abstract;
  {$ENDIF}
  end;

type
  { General routines shared by writer and reader }
  DAbstractFile = class abstract(DObject)
  strict protected
    { Full URL to the file, being processed }
    URL: string;
    { XML document reference / automanaged }
    XMLdoc: TXMLDocument;
    { Root node of the document }
    RootNode: TDOMElement;
    { Formats the file URL }
    procedure AssignFileName(FileName: string; ToGameFolder: boolean);
  end;

type
  { Relatively abstract file reader.
    Should be inherited in children with specific content reading procedures }
  DFileReader = class(DAbstractFile)
  strict private
    { Opens the file for reading }
    procedure OpenFile;
    { Closes the file and frees internal stuff }
    procedure CloseFile;
    {}
    procedure ReadBlock(aParent: TDOMElement; ReadContent: TSimpleMethod);
  public
    { Reads header from the file. Must be overriden in children. }
    procedure ReadHeader; virtual;
    { Reads the whole file}
    procedure ReadFile;
  end;

type
  { Relatively abstract file writer.
    Should be inherited in children with specific content writing procedures }
  DFileWriter = class(DAbstractFile)
  strict private
    { Opens the file for reading }
    procedure OpenFile;
    { Closes the file and frees internal stuff }
    procedure CloseFile;
  public
    { Writes the header of the file. Must be overriden in children. }
    procedure WriteHeader; virtual;
    { Writes the whole file }
    procedure WriteFile;
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils,
  DecoLog;

function DDataModule.WriteModule: TDOMNode;
begin
  Result := nil;  //to avoid Result not set warning
  isChanged := false;
end;
procedure DDataModule.ReadModule(const aParent: TDOMElement);
begin
  isChanged := false;
end;


{==============================================================================}

procedure DAbstractFile.AssignFileName(FileName: string; ToGameFolder: boolean);
begin
  {$WARNING this is wrong! As reader/writer will be used in game also}
  {make a @procedure for formatting URLs properly dependless of constructor/game}
  {if ToGameFolder then
    URL := ConstructorData(FileName+gz_ext,ToGameFolder)
  else
    URL := ConstructorData(FileName,ToGameFolder); }
end;

{==============================================================================}

procedure DFileWriter.WriteHeader;
begin
  //to be overriden in children
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.OpenFile;
begin
  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('Root'); // create Root
  XMLdoc.Appendchild(RootNode); // and add Root node to the document
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.CloseFile;
begin
  URLWriteXML(XMLdoc, URL);
  FreeAndNil(XMLdoc);
  Log(LogConstructorInfo,{$I %CURRENTROUTINE%},'File Written: '+URL);
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.WriteFile;
begin
  OpenFile;
  WriteHeader;
  //Write content
  CloseFile;
end;

{==============================================================================}

procedure DFileReader.ReadHeader;
begin
  //to be overriden in children
end;

{------------------------------------------------------------------------------}

procedure DFileReader.OpenFile;
begin
  XMLdoc := URLReadXML(URL);
  RootNode := XMLdoc.DocumentElement;
end;

{------------------------------------------------------------------------------}

procedure DFileReader.CloseFile;
begin
  FreeAndNil(XMLdoc);
  Log(LogInitInterface,{$I %CURRENTROUTINE%},'File read:'+URL);
end;

{------------------------------------------------------------------------------}

procedure DFileReader.ReadFile;
begin
  OpenFile;
  ReadHeader;
  //...
  CloseFile;
end;

{------------------------------------------------------------------------------}

procedure DFileReader.ReadBlock(aParent: TDOMElement; ReadContent: TSimpleMethod);
var Iterator: TXMLElementIterator;
begin
  Iterator := aParent.ChildrenIterator;
  try
    while Iterator.GetNext do
      ReadContent;
  finally
    FreeAndNil(Iterator);
  end;
end;

end.

