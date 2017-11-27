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

{ Editor for placeholders and placeholders atlas }
unit Constructor_PlaceHolders;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Constructor_Global;

type
  { Editor for placeholders in tiles/overworld }
  TPlaceholdersEditor = class(TWriterForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(const ToGameFolder: boolean); override;
  end;

var
  PlaceholdersEditor: TPlaceholdersEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

{$R *.lfm}

procedure TPlaceholdersEditor.LoadMe;
begin
  {$Warning dummy}
  isLoaded := True;
  isChanged := False;
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.FreeMe;
begin
  {$Warning dummy}
end;

procedure TPlaceholdersEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.WriteMe(const ToGameFolder: boolean);
begin
  {$Warning dummy}
  inherited WriteMe(ToGameFolder);
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.FormShow(Sender: TObject);
begin
  {$warning dummy}
  LoadMe;
end;

end.
