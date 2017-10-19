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

//uses  ;

type
  {}
  DAbstractFile = class abstract(TObject)
  strict protected
    {}
    URL: string;
    {}
    XMLdoc: TXMLDocument;
    {}
    procedure AssignFileName(FileName: string; ToGameFolder: boolean);
  end;

type
  {}
  DFileReader = class(DAbstractFile)
  strict private
    {}
    procedure OpenFile;
    {}
    procedure CloseFile;
  public
    {}
    procedure ReadHeader; virtual;
    {}
    procedure ReadFile;
  end;

type
  {}
  DFileWriter = class(DAbstractFile)
  strict private
    {}
    procedure OpenFile;
    {}
    procedure CloseFile;
  public
    {}
    procedure WriteHeader; virtual;
    {}
    procedure WriteFile;
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoLog;

procedure DAbstractFile.AssignFileName(FileName: string; ToGameFolder: boolean);
begin
  if ToGameFolder then
    URL := ConstructorData(FileName+gz_ext,ToGameFolder)
  else
    URL := ConstructorData(FileName,ToGameFolder);
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
  RootNode := XMLdoc.CreateElement('Root');
  XMLdoc.Appendchild(RootNode);
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.CloseFile;
begin
  URLWriteXML(XMLdoc, URL);
  FreeAndNil(XMLdoc);
  dLog(LogConstructorInfo,Self,'DFileWriter.CloseFile','File Written: '+URL);
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.WriteFile;
begin
  OpenFile;
  WriteHeader
  //Write content
  CloseFile;
end;

{==============================================================================}

procedure DFileWriter.WriteHeader;
begin
  //to be overriden in children
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.OpenFile;
begin
  {XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('Root');
  XMLdoc.Appendchild(RootNode);}
end;

{------------------------------------------------------------------------------}

procedure DFileWriter.CloseFile;
begin
  {if ToGameFolder then
    f := ConstructorData(GetScenarioFolder+MapsFolder+filename+'.xml'+gz_ext,ToGameFolder)
  else
    f := ConstructorData(GetScenarioFolder+MapsFolder+filename+'.xml',ToGameFolder);
  URLWriteXML(XMLdoc, f);

  dLog(LogConstructorInfo,Self,'TMapEditor.SaveMap','File Written: '+f);

  FreeAndNil(XMLdoc);}
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

end.

