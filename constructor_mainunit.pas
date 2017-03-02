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
unit constructor_mainunit;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fgl,
  constructor_global;

type TFormList = specialize TFPGObjectList<TWriterForm>;

type
  TMainForm = class(TForm)
    SaveButton: TButton;
    CompileButton: TButton;
    FactsEditorButton: TButton;
    LanguageSelect: TComboBox;
    procedure CompileButtonClick(Sender: TObject);
    procedure FactsEditorButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LanguageSelectChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
  public
    AllForms: TFormList;
    procedure GetLanguage;
    procedure MakeFormsList;
    procedure WriteMe(ToGameFolder: boolean); //override;
  end;

var
  MainForm: TMainForm;

implementation

uses constructor_facts;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FactsEditorButtonClick(Sender: TObject);
begin
  FactsEditor.Show;
end;

procedure TMainForm.GetLanguage;
begin
  case LanguageSelect.Items[LanguageSelect.ItemIndex] of
    'English': CurrentLanguage := Language_English;
    'Russian': CurrentLanguage := Language_Russian;
    else raise Exception.Create('Unknown Language in constructor_mainunit.LanguageSelect!');
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GetLanguage;
end;

procedure TMainForm.LanguageSelectChange(Sender: TObject);
begin
  MakeFormsList;
  GetLanguage;
end;

procedure TMainForm.MakeFormsList;
begin
  AllForms := TFormList.create(false);
  //add all forms here
  AllForms.Add(FactsEditor);
end;

procedure TMainForm.WriteMe(ToGameFolder: boolean);
var WF: TWriterForm;
begin
  {for WF in AllForms do begin
    if not ToGameFolder then begin
      // if we're saving the constructor's own data, we save only changed data
      if WF.isLoaded and WF.isChanged then
        WF.WriteMe(ToGameFolder);
    end
    else begin
      // when compiling we have to save everything
      if not WF.isLoaded then WF.LoadMe;
      WF.WriteMe(ToGameFolder);
    end
  end; }
  FactsEditor.WriteMe(ToGameFolder);
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  WriteMe(False);
end;

procedure TMainForm.CompileButtonClick(Sender: TObject);
begin
  WriteMe(True);
end;


end.

