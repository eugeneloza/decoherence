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

(* Main Form of the Constructor
   It spawns and manages all other Constructor Forms *)

{$INCLUDE compilerconfig.inc}

unit ConstructorFont;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ValEdit,
  StdCtrls,
  AliasEditor,
  ConstructorGlobal, DecoGenerics;

type
  TFontEditor = class(TConstructorForm)
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    AliasEdit: TAliasEditor;
  public

  end;

var
  FontEditor: TFontEditor;

{............................................................................}
implementation
uses
  DecoFont, DecoFontEncoding, DecoFontFile,
  DecoLog;

{$R *.lfm}

{ TFontEditor }

{to move outside?}
procedure MakeInfoTab(constref aTab: TTabSheet; constref aFontInfo: DFontInfoDictionary);
begin
  aTab.Caption := 'Fonts';

end;

{-----------------------------------------------------------------------------}

function StringDictionaryEditor(constref aParent: TWinControl; constref aStringDictionary: DStringDictionary): TAliasEditor;
var
  s: string;
begin
  Result := TAliasEditor.Create(aParent);
  Result.Parent := aParent;
  Result.Flat := true;
  Result.AssignDictionary(aStringDictionary);
end;

{-----------------------------------------------------------------------------}

{this one certanily should be moved outisde}
procedure MakeAliasTab(constref aTab: TTabSheet; constref aAliasDictionary: DStringDictionary);
var
  AliasEdit: TAliasEditor;
  s: string;
begin
  aTab.Caption := 'Aliases';
  AliasEdit := StringDictionaryEditor(aTab, aAliasDictionary);

  //ValueListEditor.OnSelectCell := procedure(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  //ValueListEditor.CellRect();
  //ValueListEditor.OnEditingDone := @.SetChanged;
end;

{-----------------------------------------------------------------------------}

procedure TFontEditor.FormCreate(Sender: TObject);
begin
  {FontInfo and FontAlias are read here}
  if not ReadFontsInfo then
    DefaultFontInfo;

  WriteFontsInfo;

  //load alias dictionary
  MakeAliasTab(PageControl1.AddTabSheet, FontAlias);
  MakeInfoTab(PageControl1.AddTabSheet, FontInfo);
end;

{-----------------------------------------------------------------------------}

procedure TFontEditor.FormDestroy(Sender: TObject);
begin
  FreeFontsInfo;
end;

end.

