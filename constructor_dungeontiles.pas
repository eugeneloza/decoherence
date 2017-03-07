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

{ Tile manager for dungeon generator.
  TODO: At this moment language-relative tiles/textures
  (e.g. signs in native language) are not allowed.
  Use some alien fonts :) }
unit constructor_dungeontiles;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  castleLog,
  decodungeontiles,
  CastleControl, constructor_global;

type
  TDungeonTilesEditor = class(TWriterForm)
    TileDisplay: TCastleControl;
    TilesBox: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    TilesList: TStringList;
    procedure ReadTilesList;
    procedure FillTilesList;
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

uses
     decoglobal;

procedure TDungeonTilesEditor.FreeMe;
begin
  freeandnil(TilesList);
end;
procedure TDungeonTilesEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.ReadTilesList;
var Rec: TSearchRec;
begin
  FreeAndNil(TilesList);
  DestroyTiles;
  TilesList := TStringList.Create;
  MaxTileTypes := 0;
  if FindFirst (Tiles_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       inc(MaxTileTypes);
       TilesList.Add(Rec.Name);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
  WriteLnLog('TDungeonTilesEditor.ReadTilesList','Tiles found = '+inttostr(MaxTileTypes));
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FillTilesList;
var s: string;
begin
  TilesBox.clear;
  for s in TilesList do
    TilesBox.Items.Add(s);
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadMe;
begin
  ReadTilesList;
  FillTilesList;
  isLoaded := true;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.WriteMe(ToGameFolder: boolean);
begin

end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) then LoadMe;
end;


end.

