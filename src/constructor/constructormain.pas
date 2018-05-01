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
   It spawns and manages all other Constructor Forms

   DESKTOP ONLY.

   While Constructor works on internationalized data, the tool itself is
   ENGLISH ONLY. I'm not going to add any multilingual support in any future,
   Because it's a specific tool to solve specific tasks that
   while trying to keep everything simple, is a very complex thing to do
   - it's not for "all and everybody" to use. It requires knowledge of English anyway.
   If you want it - this is a FOSS project - do it :). *)

{$INCLUDE compilerconfig.inc}

unit ConstructorMain;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ConstructorGlobal,
  {Editor forms}
  ConstructorFont,
  {other}
  DecoGlobal;

type
  TMainForm = class(TConstructorForm)
    FontButton: TButton;
    procedure FontButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

{............................................................................}
implementation
uses
  DecoLog, DecoTrash, DecoHDD;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FontButtonClick(Sender: TObject);
begin
  FontEditor.Show;
end;

{-----------------------------------------------------------------------------}

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  UnsavedData: boolean;
begin
  UnsavedData := false;

  if UnsavedData then
    if MessageDlg('There is unsaved data in ' + '...' +
      '! Really exit?', mtCustom, [mbYes, mbNo], 0) = mrNo then
    begin
      CloseAction := caNone;
      Exit;
    end;
end;

{-----------------------------------------------------------------------------}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitLog;
  InitHDDLock;
  InitTrash;
end;

{-----------------------------------------------------------------------------}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeLog;
  FreeHDDLock;
  FreeTrash;
end;

end.

