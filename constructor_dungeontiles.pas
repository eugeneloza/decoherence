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
  public
    { list of tiles files in the game folder }
    TilesList: TStringList;

    TileName: string;
    TileScene: TCastleScene;
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
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.WriteMe(ToGameFolder: boolean);
begin
  writeLnLog('TDungeonTilesEditor.WriteMe','Working directly on game data, nothing to save.');
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) then LoadMe;
end;

{============================================================================}
{==================== 3d routines ===========================================}
{============================================================================}

procedure TDungeonTilesEditor.ResetCamera;
begin
  if (TileDisplay.scenemanager.camera<>nil) then
  begin
    if true{tile is loaded} then begin
      //set upthe camera
      {TileDisplay.scenemanager.camera.setView(MyTile.Tile_Scene.BoundingBox.middle+Vector3Single(0,0,MyTile.Tile_Scene.BoundingBox.maxsize+1),Vector3Single(0,0,-1),Vector3Single(0,1,0));
      TileDisplay.scenemanager.camera.input:=TCamera.DefaultInput; }
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
  {well... 2-abundance is obsolete, really.
   I was making it in order to protect the code from errors
   when writing a lot of code without being able to visually test
   if it produces a correct (intended) result, so I had to introduce
   a lot of internal checks including 2-abundance...
   Maybe, some day this should be simplified and rewritten to remove abundance.
   But for now it works and that is all I want :)
   WARNING, if someone who is not "me" will try to rewrite the 2-abundance,
   be careful, because it is used not only to validate the tiles docking,
   but for a lot of other purposes in the generation algorithm
   (maybe in pathfinding and tile management too).
   It can't be done as easy as I'd wanted :) Otherwise, I'd have done it long ago...
   So change it only in case you're absolutely sure what you are doing. }
end;

{---------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadTile(FileName: string);
begin
 { if unsavedChanges then begin
    if MessageDlg('Unsaved changes?', 'Your changes are unsaved! Really load a new tile?', mtConfirmation, [mbYes, mbNo],0) = mrNo then exit;
  end;}
  //...
  ResetCamera;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadButtonClick(Sender: TObject);
begin
  LoadTile( TilesBox.Items[TilesBox.ItemIndex] );
end;


end.

