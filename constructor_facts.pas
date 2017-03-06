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
  decoloadscreen, decotranslation,
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
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
  end;

var
  FactsEditor: TFactsEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses DOM, CastleDownload, CastleXMLUtils,
  CastleLog, decoglobal;

{$R *.lfm}

{-----------------------------------------------------------------------------}

procedure TFactsEditor.FreeMe;
var L: TLanguage;
begin
  for L in TLanguage do
    FreeAndNil(Facts[L]);
end;

procedure TFactsEditor.LoadMe;
var
    CurrentFile: string;
   // i: integer;
    F: DFact;
    L: TLanguage;
begin
  for L in TLanguage do {if L=Language_English then} begin
    FreeAndNil(Facts[L]);

    try
      CurrentFile := ConstructorData(Scenario_Folder+LanguageDir(L)+'facts.xml',false);
      LoadFacts(CurrentFile);
      Facts[L] := decoloadscreen.Facts;
      decoloadscreen.Facts := nil;
    except
      freeandnil(decoloadscreen.Facts);
      writeLnLog('TFactsEditor.LoadMe','Exception reading '+CurrentFile);
    end;
  end;

  MyLanguage := ConstructorLanguage;    (*not sure about it*)
  isLoaded := true;
  isChanged := false;

  FactsListbox.Clear;
  for f in Facts[MyLanguage] do
    FactsListbox.Items.Add(F.value);

{  for i := 0 to Facts[MyLanguage].count-1 do
    FactsListbox.Items.Add(Facts[MyLanguage][i].value);}
end;

{-----------------------------------------------------------------------------}

//{$PUSH}{$WARN 4104 OFF} // string conversion is ok here
procedure TFactsEditor.WriteMe(ToGameFolder: boolean);
var XMLdoc: TXMLDocument;
    RootNode, ContainerNode, valueNode, TextNode: TDOMNode;
    i: DFact;
begin
  if Facts[Language_Russian] = nil then begin
    WriteLnLog('TFactsEditor.WriteMe','LANGUAGE IS NIL!');
    exit;
  end;

  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('FactsList');
  XMLdoc.Appendchild(RootNode);

  for i in Facts[Language_Russian] do begin
    ContainerNode := XMLdoc.CreateElement('Fact');
    ValueNode := XMLdoc.CreateElement('Value');
    TextNode := XMLdoc.CreateTextNode(UTF8decode(i.value));
    ValueNode.AppendChild(TextNode);
    //compatibility
    ContainerNode.AppendChild(ValueNode);
    RootNode.Appendchild(ContainerNode);
  end;

  if ToGameFolder then
    URLWriteXML(XMLdoc, ConstructorData(Scenario_Folder+LanguageDir(ConstructorLanguage)+'facts.xml',ToGameFolder){$IFDEF gzipdata},[ssoGzip]{$ENDIF})
  else
    URLWriteXML(XMLdoc, ConstructorData(Scenario_Folder+LanguageDir(ConstructorLanguage)+'facts.xml',ToGameFolder));

  FreeAndNil(XMLdoc);
end;
//{$POP}

{-----------------------------------------------------------------------------}

procedure TFactsEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) or (MyLanguage<>ConstructorLanguage) then LoadMe;
end;

{-----------------------------------------------------------------------------}

procedure TFactsEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

end.

