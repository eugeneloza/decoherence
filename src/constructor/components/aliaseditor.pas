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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ValEdit,
  StdCtrls,
  DecoGenerics;

type
  {}
  THoverComboBox = class(TComboBox)
    { wrapper for SetBounds to accept TRect }
    procedure SetBoundsRect(const aRect: TRect);
    { finish editing and save }
    procedure Finish(Sender: TObject);
  end;

type
  {}
  TStringDictionaryEdit = class(TValueListEditor)
  protected
    StringDictionary: DStringDictionary;
    AliasList: TStringList;
  public
    {}
    procedure AssignDictionary(aStringDictionary: DStringDictionary; aAliasList: TStringList);
    {}
    procedure UpdateData; virtual;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  {}
  // reordering of the elements is not needed
  TAliasEditor = class(TStringDictionaryEdit)
  private
    ComboBox: THoverComboBox;
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
  Visible := false;
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
  for s in StringDictionary.Keys do
    InsertRow(s, StringDictionary.Items[s], false);
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
  Cells[0, 0] := 'Alias';
  Cells[1, 0] := 'Reference';
  //Columns[0].ReadOnly := true;

  ComboBox := THoverComboBox.Create(Self);
  ComboBox.Parent := Self;
  ComboBox.SetBoundsRect(CellRect(1, 1));
  ComboBox.OnEditingDone := @ComboBox.Finish;
  ComboBox.Visible := true;
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

