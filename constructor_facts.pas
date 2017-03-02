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
unit constructor_facts;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleXMLUtils,
  decofacts,
  constructor_global;

type

  { TFactsEditor }

  TFactsEditor = class(TWriterForm)
    ListBox1: TListBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Facts: TFactList;
    FactsLanguage: TLanguage;
    procedure LoadMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
    function GetFileLink(ToGameFolder: boolean): string;
  end;

var
  FactsEditor: TFactsEditor;

implementation

uses DOM,
  CastleDownload, XMLWrite;

{$R *.lfm}

{ TFactsEditor }

{hack temporary}
procedure URLWriteXML(Doc: TXMLDocument; const URL: String; const Options: TStreamOptions = []);
var
  Stream: TStream;
begin
  Stream := URLSaveStream(URL, Options);
  try
    WriteXMLFile(Doc, Stream);
  finally FreeAndNil(Stream) end;
end;
{/hack}

{$PUSH}{$WARN 4105 OFF} // string conversion is ok here
procedure TFactsEditor.LoadMe;
var FactsDoc: TXMLDocument;
    Base: TDOMElement;
    Iter: TXMLElementIterator;
    f: DFact;
    i: integer;
begin
  freeandnil(Facts);
  Facts := TFactList.create(true);

  try
    FactsDoc := URLReadXML(GetFileLink(false));
  except
    //freeandnil(Facts);
    exit;
  end;

{  Base := FactsDoc.DocumentElement;

  Iter := Base.ChildElement('Fact', false).ChildrenIterator; //todo: sigsegv
  try
    while Iter.GetNext do
    begin
      F := DFact.create;
      F.value := Iter.current.TextData;
      Facts.add(F);
    end;
  finally FreeAndNil(Iter) end; }

  freeandnil(FactsDoc);
  FactsLanguage := CurrentLanguage;
  isLoaded := true;
  isChanged := false;

  LoadFacts;
  for i := 0 to N_facts-1 do begin
    F := DFact.create;
    F.Value := Facts_text[i];
    Facts.add(F);
  end;
end;
{$POP}

{$PUSH}{$WARN 4104 OFF} // string conversion is ok here
procedure TFactsEditor.WriteMe(ToGameFolder: boolean);
var XMLdoc: TXMLDocument;
    RootNode, ContainerNode, TextNode: TDOMNode;
    i: DFact;
begin
  if Facts = nil then exit;

  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('FactsList');
  XMLdoc.Appendchild(RootNode);

  for i in Facts do begin
    ContainerNode := XMLdoc.CreateElement('Fact');
    TextNode := XMLdoc.CreateTextNode(i.value);
    ContainerNode.Appendchild(TextNode);
    RootNode.Appendchild(ContainerNode);
  end;

  if ToGameFolder then
    URLWriteXML(XMLdoc, GetFileLink(ToGameFolder),[soGzip])
  else
    URLWriteXML(XMLdoc, GetFileLink(ToGameFolder));

  FreeAndNil(XMLdoc);
end;
{$POP}


function TFactsEditor.GetFileLink(ToGameFolder: boolean): string;
begin
  Result := ConstructorData('scenario/'+LanguageDir+'facts'+FileExtension(ToGameFolder),ToGameFolder);
end;

procedure TFactsEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) or (FactsLanguage<>CurrentLanguage) then LoadMe;
end;

procedure TFactsEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Facts);
end;

end.

