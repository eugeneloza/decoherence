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
  StdCtrls, ConstructorGlobal, DecoGenerics;

type
  TFontEditor = class(TConstructorForm)
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

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

function StringDictionaryEditor(constref aParent: TWinControl; constref aStringDictionary: DStringDictionary): TValueListEditor;
var
  s: string;
begin
  Result := TValueListEditor.Create(aParent);
  Result.Parent := aParent;
  Result.Flat := true;
  for s in aStringDictionary.Keys do
    Result.InsertRow(s, aStringDictionary.Items[s], false);
end;

{-----------------------------------------------------------------------------}

type
  TComboBoxHelper = class helper for TComboBox
    procedure SetBoundsRect(const aRect: TRect);
  end;

procedure TComboBoxHelper.SetBoundsRect(const aRect: TRect);
begin
  Self.SetBounds(aRect.Left, aRect.Top,
    aRect.Right - aRect.Left, aRect.Bottom - aRect.Top);
end;

{this one certanily should be moved outisde}
procedure MakeAliasTab(constref aTab: TTabSheet; constref aAliasDictionary: DStringDictionary);
var
  ValueListEditor: TValueListEditor;
  ComboBox: TComboBox;
  s: string;
begin
  aTab.Caption := 'Aliases';
  ValueListEditor := StringDictionaryEditor(aTab, aAliasDictionary);
  ValueListEditor.Cells[0, 0] := 'Alias';
  ValueListEditor.Cells[1, 0] := 'Reference';

  ComboBox := TComboBox.Create(aTab);
  ComboBox.Parent := aTab;
  for s in aAliasDictionary.Keys do
    ComboBox.Items.Add(s);
  ComboBox.ItemIndex := 0;
  ComboBox.SetBoundsRect(ValueListEditor.CellRect(1, 1));
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

