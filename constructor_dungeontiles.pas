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
  CastleControl, constructor_global, CastleScene, CastleImages, X3DNodes,
  decodungeontiles;

Type
  DTileAtlasRecord = record
     FriendlyName: string;
     color:integer;    //for display
     //style:TBrushStyle;
end;

type
  {Edit tile map, generate tile minimap image}
  TDungeonTilesEditor = class(TWriterForm)
    FaceAtlasBox: TComboBox;
    BaseAtlasBox: TComboBox;
    ZLabel: TLabel;
    ScreenShotButton: TButton;
    ZScroll: TScrollBar;
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
    procedure ZScrollChange(Sender: TObject);
    procedure SymmetricEditCheckBoxChange(Sender: TObject);
  private
    fisTileLoaded: boolean;
  public
    { list of tiles files in the game folder }
    TilesList: TStringList;
    { name of current tile }
    TileName: string;
    { current displayed tile }
    TileRoot: TX3DRootNode;
    TileScene: TCastleScene;

    //todo: generic list
    {atlas for the faces types}
    FaceAtlas: array [TTileFace] of DTileAtlasRecord;
    {atlas for base of the tile}
    BaseAtlas: array [TTileKind] of DTileAtlasRecord;

    LastFaceAtlasIndex: TTileFace;
    LastBaseAtlasIndex: TTileKind;
    CurrentZ: integer;

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
    { Fills the Face and Base atlas; todo: load/save ini }
    procedure FillAtlas;
    { fills Face and base boxes }
    procedure MakeAtlasBoxes;
    { conversion from combobox index to TTileFace }
    function FaceByIndex(index: integer): TTileFace;
    { conversion from combobox index to TTileKind }
    function BaseByIndex(index: integer): TTileKind;
    procedure DrawTileMap;
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
     deco3dLoad,
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
var tmpImg: TRGBImage;
begin
  tmpImg := TileDisplay.SaveScreen;
  SaveImage(tmpImg, TileName+'.scr.png');
  FreeAndNil(tmpImg);
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.ReadTilesList;
var Rec: TSearchRec;
begin
  FreeAndNil(TilesList);
  DestroyTiles;
  TilesList := TStringList.Create;
  MaxTileTypes := 0;
  {$Warning Fix folders!}
  writelnLog('','data'+pathdelim+'models'+pathdelim+ 'tiles'+pathdelim + '*.x3d');
  if FindFirst ('data'+pathdelim+'models'+pathdelim+ 'tiles'+pathdelim + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
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
  FillAtlas;
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
      if (TileDisplay.scenemanager.camera<>nil) and (TileScene<>nil) then begin
        TileDisplay.scenemanager.camera.setView(TileScene.BoundingBox.center+Vector3Single(0,0,TileScene.BoundingBox.maxsize+1),Vector3Single(0,0,-1),Vector3Single(0,1,0));
        TileDisplay.scenemanager.camera.input:=TCamera.DefaultInput;
        TileDisplay.update;
      end else
        WriteLnLog('TDungeonTilesEditor.ResetCamera','Camera or TileScene is nil! Can''t reset camera');
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

{---------------------------------------------------------------------------}

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

  TileName := FileName;
  isTileLoaded := false;
  if FileName = '' then begin
    showmessage('No tiles found!');
    exit;
  end;

  if TileScene<>nil then
    TileDisplay.SceneManager.Items.remove(TileScene);
  //FreeAndNil(TileRoot); //owned by TileScene
  FreeAndNil(TileScene);

  TileRoot := LoadBlenderX3D( ConstructorData(TilesFolder+Filename+'.x3d',true) );
  TileScene := TCastleScene.create(TileDisplay);
  TileScene.Load(TileRoot, true);
  TileDisplay.SceneManager.Items.Add(TileScene);
  TileDisplay.SceneManager.MainScene := TileScene;

  isTileLoaded := true;

  ZScroll.max := 1;
  ZSCroll.min := 0;
  ZScroll.position := 0;

  ResetCamera;
  DrawTileMap;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadButtonClick(Sender: TObject);
begin
  LoadTile( TilesBox.Items[TilesBox.ItemIndex] );
  //Save3D(TileRoot,'home.x3d.gz');
end;

{============================================================================}
{========================== map editing routines ============================}
{============================================================================}

//I think some day it should start from an ini file. But now that's more than enough
procedure TDungeonTilesEditor.FillAtlas;
var i: integer;
begin
 with FaceAtlas[tfNone] do begin
  FriendlyName := 'n/a';
  color := 0;
 end;
 with FaceAtlas[tfWall] do begin
  FriendlyName := 'WALL';
  color := $FFFFFF;
 end;
 for i := tfFree to high(TTileFace) do with FaceAtlas[i] do begin
  FriendlyName := 'free #'+inttostr(i-1);
  color := $990000*(high(FaceAtlas)-i) div (high(FaceAtlas))+$000099*i div (high(FaceAtlas))+$009900;
 end;

 with BaseAtlas[tkNone] do begin
  FriendlyName := 'n/a';
  color := 0;
 end;
 with BaseAtlas[tkWall] do begin
  FriendlyName := 'WALL';
  color := $FFFFFF;
 end;
 with BaseAtlas[tkFree] do begin
  FriendlyName := 'FREE';
  color := $444444;
 end;
 with BaseAtlas[tkDown] do begin
  FriendlyName := 'STAIRS DOWN';
  color := $0000FF;
 end;
 with BaseAtlas[tkUp] do begin
  FriendlyName := 'STAIRS UP';
  color := $00FF00;
 end;

 LastFaceAtlasIndex := tfFree;
 LastBaseAtlasIndex := tkFree;

 MakeAtlasBoxes;
end;

{-------------------------------------------------------------------------}

procedure TDungeonTilesEditor.MakeAtlasBoxes;
var tf: TTileFace;
    tk: TTileKind;
begin
  FaceAtlasBox.Clear;
  BaseAtlasBox.Clear;
  for tf in TTileFace do begin
    FaceAtlasBox.Items.Add(FaceAtlas[tf].friendlyName);
  end;
  for tk in TTileKind do begin
    BaseAtlasBox.Items.Add(BaseAtlas[tk].friendlyName);
  end;
  FaceAtlasBox.ItemIndex := 1; //set to wall
  BaseAtlasBox.ItemIndex := 1; //set to free
end;

{-------------------------------------------------------------------------}

function TDungeonTilesEditor.FaceByIndex(index: integer): TTileFace;
var tf: TTileFace;
begin
  for tf in TTileFace do if FaceAtlasBox.Items[index]=FaceAtlas[tf].friendlyName then begin
    Result := tf;
    break;
  end;
  Result := tfNone;
  writelnLog('TDungeonTilesEditor.FaceByIndex','ERROR: Face not found! for index '+ inttostr(index));
end;

{-------------------------------------------------------------------------}

function TDungeonTilesEditor.BaseByIndex(index: integer): TTileKind;
var tk: TTileKind;
begin
  for tk in TTileKind do if BaseAtlasBox.Items[index]=BaseAtlas[tk].friendlyName then begin
    Result := tk;
    break;
  end;
  Result := tkNone;
  writelnLog('TDungeonTilesEditor.BaseByIndex','ERROR: Face not found! for index '+ inttostr(index));
end;

{-------------------------------------------------------------------------}

procedure TDungeonTilesEditor.ZScrollChange(Sender: TObject);
begin
  if CurrentZ<>ZScroll.Position then begin
    currentZ := ZScroll.Position;
    DrawTileMap
  end;
end;

{-------------------------------------------------------------------------}

procedure TDungeonTilesEditor.DrawTileMap;
begin
  ZLabel.Caption := inttostr(currentZ);
  //dummy
end;

{-------------------------------------------------------------------------}


end.

