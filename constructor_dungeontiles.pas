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

{ Tile manager for dungeon generator. }
unit constructor_dungeontiles;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  decodungeontiles,
  CastleControl, constructor_global;

type

  { TDungeonTilesEditor }

  TDungeonTilesEditor = class(TWriterForm)
    TileDisplay: TCastleControl;
    TilesBox: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
  end;
var
  DungeonTilesEditor: TDungeonTilesEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$R *.lfm}

//uses

procedure TDungeonTilesEditor.FreeMe;
begin

end;
procedure TDungeonTilesEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadMe;
begin

end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.WriteMe(ToGameFolder: boolean);
begin

end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) LoadMe;
end;


end.

