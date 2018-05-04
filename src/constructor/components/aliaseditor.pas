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

(* Convenient editor for DStringList *)

{$INCLUDE compilerconfig.inc}

unit AliasEditor;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls,
  DecoGenerics;

type
  { Combo box that automatically appears over the edited cell and
    propose a selection of predefined items instead of free edit }
  THoverComboBox = class(TComboBox)
  private
    eCol, eRow: integer;
  public
    { wrapper for SetBounds to accept TRect }
    procedure SetBoundsRect(const aRect: TRect);
    { finish editing and save }
    procedure Finish(Sender: TObject);
    {}
    procedure EditCell(const aCol, aRow: integer);
  end;

type
  { Editor for DStringDictionary }
  TStringDictionaryEdit = class(TStringGrid)
  protected
    StringDictionary: DStringDictionary;
    AliasList: TStringList;
  public
    { Initialize the editor with data }
    procedure AssignDictionary(aStringDictionary: DStringDictionary; aAliasList: TStringList);
    { Update the data displayed on the grid }
    procedure UpdateData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  { Editor for Alias-Reference pair,
    Reference is chosen from a predefined set of References in a comboBox }
  // reordering of the elements is not needed
  TAliasEditor = class(TStringDictionaryEdit)
  private
    ComboBox: THoverComboBox;
    procedure SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  public
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{.............................................................................}
implementation
uses
  DecoLog;

procedure THoverComboBox.SetBoundsRect(const aRect: TRect);
begin
  Self.SetBounds(aRect.Left, aRect.Top,
    aRect.Right - aRect.Left, aRect.Bottom - aRect.Top);
end;

{-----------------------------------------------------------------------------}

procedure THoverComboBox.Finish(Sender: TObject);
begin
  (Parent as TStringDictionaryEdit).Cells[eCol, eRow] := Self.Text;
  Visible := false;
end;

{-----------------------------------------------------------------------------}

procedure THoverComboBox.EditCell(const aCol, aRow: integer);
begin
  Visible := true;
  eCol := aCol;
  eRow := aRow;
end;

{=============================================================================}

procedure TStringDictionaryEdit.AssignDictionary(aStringDictionary: DStringDictionary; aAliasList: TStringList);
begin
  StringDictionary := aStringDictionary;
  AliasList := aAliasList;
  UpdateData;
end;

{-----------------------------------------------------------------------------}

procedure TStringDictionaryEdit.UpdateData;
var
  s: string;
begin
  Clear;

  RowCount := 1;
  Cells[0, 0] := 'Alias';
  Cells[1, 0] := 'Reference';

  for s in StringDictionary.Keys do
  begin
    RowCount := RowCount + 1;
    Self.Cells[0, Pred(RowCount)] := s;
    Self.Cells[1, Pred(RowCount)] := StringDictionary.Items[s];
  end;
end;

{-----------------------------------------------------------------------------}

constructor TStringDictionaryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoFillColumns := true;
  RowCount := 1;
  ColCount := 2;
  FixedCols := 0;
  FixedRows := 1;
  Flat := false;
  Options := [goAutoAddRows, goEditing{, goCellHints}];
  ScrollBars := ssAutoVertical;
  //Columns[0].ReadOnly := true;
end;


{-----------------------------------------------------------------------------}

destructor TStringDictionaryEdit.Destroy;
begin
  FreeAndNil(AliasList);
  inherited Destroy;
end;

{=============================================================================}

constructor TAliasEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnSelectCell := @SelectCell;

  ComboBox := THoverComboBox.Create(Self);
  ComboBox.Parent := Self;
  ComboBox.Visible := false;
  ComboBox.OnEditingDone := @ComboBox.Finish;
end;

{-----------------------------------------------------------------------------}

procedure TAliasEditor.SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if (aCol = 1) and (aRow > 0) then
  begin
    ComboBox.SetBoundsRect(CellRect(aCol, aRow));
    ComboBox.EditCell(aCol, aRow);
    //Self.EditorHide;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TAliasEditor.UpdateData;
var
  s: string;
begin
  inherited UpdateData;
  ComboBox.Clear;
  if AliasList <> nil then
  begin
    for s in AliasList do
      ComboBox.Items.Add(s);
    ComboBox.ItemIndex := 0;
  end
  else
    Log(true, CurrentRoutine, 'ERROR: TAliasEditor.AliasList is nil!');
end;



end.

