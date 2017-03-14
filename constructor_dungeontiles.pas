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
  (e.g. inscriptions in current game language) are not allowed.
  Use some abstract alien fonts :) }
unit constructor_dungeontiles;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  CastleControl, constructor_global, CastleScene, CastleImages,
  decodungeontiles;

type
  {Edit tile map, generate tile minimap image}
  TDungeonTilesEditor = class(TWriterForm)
    ScreenShotButton: TButton;
    SymmetricEditCheckBox: TCheckBox;
    LoadButton: TButton;
    TileMapPanel: TPanel;
    ResetCameraButton: TButton;
    TileDisplay: TCastleControl;
    TilesBox: TComboBox;
    {take a screenshot of the current render. Yet not needed.}
    procedure ScreenShotButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ResetCameraButtonClick(Sender: TObject);
    procedure SymmetricEditCheckBoxChange(Sender: TObject);
  private
    fisTileLoaded: boolean;
  public
    { list of tiles files in the game folder }
    TilesList: TStringList;
    { name of current tile }
    TileName: string;
    { current displayed tile }
    TileScene: TCastleScene;

    property isTileLoaded: boolean read fisTileLoaded write fistileloaded default false;
    { Read tiles files from the HDD }
    procedure ReadTilesList;
    { Put tiles into a ComboBox }
    procedure FillTilesList;
    { Return the camera to strictly oriented position.
      It's really important to make the tile map correctly }
    procedure ResetCamera;
    { Loads the selected tile }
    procedure LoadTile(FileName: string);
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

uses CastleVectors, CastleCameras, StrUtils, castleLog,
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

procedure TDungeonTilesEditor.ScreenShotButtonClick(Sender: TObject);
begin
   SaveImage(TileDisplay.SaveScreen,TileName+'screen.png');
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
       TilesList.Add(AnsiReplaceText(Rec.Name,'.x3d',''));
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
  TilesBox.ItemIndex := 0;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadMe;
begin
  ReadTilesList;
  FillTilesList;
  isLoaded := true;
  isChanged := false;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.WriteMe(ToGameFolder: boolean);
begin
  writeLnLog('TDungeonTilesEditor.WriteMe','Working directly on game data, nothing to save.');
  if not ToGameFolder then isChanged := false;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FormShow(Sender: TObject);
begin
  if (not isTileLoaded) then LoadMe;
end;

{============================================================================}
{==================== 3d routines ===========================================}
{============================================================================}

procedure TDungeonTilesEditor.ResetCamera;
begin
  if (TileDisplay.scenemanager.camera<>nil) then
  begin
    if isTileLoaded {?} then begin
      //set up the camera
      if TileDisplay.scenemanager.camera<>nil then begin
        TileDisplay.scenemanager.camera.setView(TileScene.BoundingBox.center+Vector3Single(0,0,TileScene.BoundingBox.maxsize+1),Vector3Single(0,0,-1),Vector3Single(0,1,0));
        TileDisplay.scenemanager.camera.input:=TCamera.DefaultInput;
      end;
      TileDisplay.update;
    end else
      WriteLnLog('TDungeonTilesEditor.ResetCamera','No Tile Loaded.');
  end else
    WriteLnLog('TDungeonTilesEditor.ResetCamera','No Camera To Reset.');
end;

{---------------------------------------------------------------------------}

procedure TDungeonTilesEditor.ResetCameraButtonClick(Sender: TObject);
begin
  ResetCamera;
end;

procedure TDungeonTilesEditor.SymmetricEditCheckBoxChange(Sender: TObject);
begin
  if not SymmetricEditCheckBox.checked then
    showmessage('It is highly recommended to leave Symmetric Edit on, unles you know what you are doing. The tile must be 2-abundantly consistent and Symmetric Edit tries to do as much as possible of that automatically.');
end;

{---------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadTile(FileName: string);
begin
  if isChanged then begin
    if MessageDlg('Unsaved changes?', 'Your changes are unsaved! Really load a new tile?', mtConfirmation, [mbYes, mbNo],0) = mrNo then exit;
  end;
  //...
  ResetCamera;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadButtonClick(Sender: TObject);
begin
  LoadTile( TilesBox.Items[TilesBox.ItemIndex] );
end;


end.

