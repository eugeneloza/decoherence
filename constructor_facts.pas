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

{ Editor for loadscreens: facts and images }
unit constructor_facts;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleXMLUtils,
  decofacts, decotranslation,
  constructor_global;

type

  { TFactsEditor }

  TFactsEditor = class(TWriterForm)
    FactsListbox: TListBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Facts: array [TLanguage] of TFactList;
    FactsLanguage: TLanguage;
    procedure LoadMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
    function GetFileLink(ToGameFolder: boolean): string;
  end;

var
  FactsEditor: TFactsEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses DOM,
  CastleDownload,
  CastleLog, decoglobal;

{$R *.lfm}

{-----------------------------------------------------------------------------}

{$PUSH}{$WARN 4105 OFF} // string conversion is ok here
procedure TFactsEditor.LoadMe;
var FactsDoc: TXMLDocument;
    Base: TDOMElement;
    Iterator: TXMLElementIterator;
    f: DFact;
    //i: integer;
begin
  freeandnil(Facts);

  try
    WriteLnLog(GetFileLink(false));
    FactsDoc := URLReadXML(GetFileLink(false));
  except
    exit;
  end;

  Facts[Language_Russian] := TFactList.create(true);

  Base := FactsDoc.DocumentElement;

  Iterator := base.ChildrenIterator; //todo: sigsegv if no "fact" field found
  try
    while Iterator.GetNext do
    begin
      F := DFact.create;
      F.value := Iterator.current.TextData;
      Facts[Language_Russian].add(F);
    end;
  finally
    FreeAndNil(Iterator)
  end;

  freeandnil(FactsDoc);
  FactsLanguage := CurrentLanguage;
  isLoaded := true;
  isChanged := false;

  FactsListbox.Clear;
  for F in Facts[Language_Russian] do
    FactsListbox.Items.Add(F.value);

{  LoadFacts;
  for i := 0 to N_facts-1 do begin
    F := DFact.create;
    F.Value := Facts_text[i];
    Facts.add(F);
  end; }
end;
{$POP}

{-----------------------------------------------------------------------------}

{$PUSH}{$WARN 4104 OFF} // string conversion is ok here
procedure TFactsEditor.WriteMe(ToGameFolder: boolean);
var XMLdoc: TXMLDocument;
    RootNode, ContainerNode, TextNode: TDOMNode;
    i: DFact;
begin
  if Facts[Language_Russian] = nil then exit;

  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('FactsList');
  XMLdoc.Appendchild(RootNode);

  for i in Facts[Language_Russian] do begin
    ContainerNode := XMLdoc.CreateElement('Fact');
    TextNode := XMLdoc.CreateTextNode(i.value);
    ContainerNode.Appendchild(TextNode);
    RootNode.Appendchild(ContainerNode);
  end;

  if ToGameFolder then
    URLWriteXML(XMLdoc, GetFileLink(ToGameFolder),[ssoGzip])
  else
    URLWriteXML(XMLdoc, GetFileLink(ToGameFolder));

  FreeAndNil(XMLdoc);
end;
{$POP}

{-----------------------------------------------------------------------------}

function TFactsEditor.GetFileLink(ToGameFolder: boolean): string;
begin
  Result := ConstructorData(ScenarioFolder+LanguageDir+'facts'+FileExtension(ToGameFolder),ToGameFolder);
end;

{-----------------------------------------------------------------------------}

procedure TFactsEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) or (FactsLanguage<>CurrentLanguage) then LoadMe;
end;

{-----------------------------------------------------------------------------}

procedure TFactsEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Facts);
end;

end.

