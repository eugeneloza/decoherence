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
unit Constructor_Tiles;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  Constructor_Global,
  CastleControl, CastleScene, CastleImages, X3DNodes,
  DecoDungeonTiles;

{graphical preferences}
const delta = 0.2; //this is 'wall thickness' at wall editor
      tile_image_scale = 32;

Type
  {atlas record for faces and base}
  DTileAtlasRecord = record
     {name displayed in the editor}
     FriendlyName: string;
     {color the atlas element is displayed on the image}
     color: integer;
  end;

type
  {these routines that should be used only in Constructor}
  DTileMapHelper = class helper for DTileMap
    {make the tile map an empty closed box}
    procedure EmptyTile;
    {guess the tile size based on model
     BUG: sometimes fails (<1%)}
    procedure GuessSize(Tile3D: TCastleScene);
    {detect if this tile is a blocker
     todo: WILL fail if blocker is ~0.5 dx}
    procedure DetectBlocker;
    {safe wrapper for Load. Just catches exception in case the file is not found}
    constructor LoadForConstructor(URL: string);
    {save procedure}
    procedure Save(TName: string; ToGameFolder: boolean);
end;

type
  {Edit tile map, generate tile minimap image}
  TDungeonTilesEditor = class(TWriterForm)
    SaveTileMapButton: TButton;
    EmptyMapButton: TButton;
    BaseRadio: TRadioButton;
    FloorRadio: TRadioButton;
    CeilingRadio: TRadioButton;
    SavePNGButton: TButton;
    FaceAtlasBox: TComboBox;
    BaseAtlasBox: TComboBox;
    MapImage: TImage;
    ZLabel: TLabel;
    ScreenShotButton: TButton;
    ZScroll: TScrollBar;
    SymmetricEditCheckBox: TCheckBox;
    LoadButton: TButton;
    TileMapPanel: TPanel;
    ResetCameraButton: TButton;
    TileDisplay: TCastleControl;
    TilesBox: TComboBox;
    procedure RadioChange(Sender: TObject);
    procedure EmptyMapButtonClick(Sender: TObject);
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    {take a screenshot of the current render. Yet not needed.}
    procedure SavePNGButtonClick(Sender: TObject);
    procedure SaveTileMapButtonClick(Sender: TObject);
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
    { Scene of the tile }
    TileScene: TCastleScene;
    { map of the tile. The main goal of this module is to edit it }
    TileM: DTileMap;

    //todo: generic list
    {atlas for the faces types}
    FaceAtlas: array [TTileFace] of DTileAtlasRecord;
    {atlas for base of the tile}
    BaseAtlas: array [TTileKind] of DTileAtlasRecord;

    {current z of the tile edited}
    CurrentZ: integer;

    { was a tile loaded? }
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
    { Save/compile all the tiles }
    procedure SaveAllTiles;
    { Save Tile map }
    procedure SaveTileMap(FileName: string; toGameFolder: boolean);
    { compile the tile to game folder }
    procedure CompileTile(FileName: string);
    { Fills the Face and Base atlas; todo: load/save ini }
    procedure FillAtlas;
    { fills Face and base boxes }
    procedure MakeAtlasBoxes;
    { conversion from combobox index to TTileFace }
    function FaceByIndex(index: integer): TTileFace;
    { conversion from combobox index to TTileKind }
    function BaseByIndex(index: integer): TTileKind;
    { draws tile map to MapImage}
    procedure DrawTileMap;
    { prepares controls for MapImage editing (reset sizes, enable/disable etc)}
    procedure PrepareMapEditor;
    { this procedure creates a tile png representation to use in minimap.
      On one hand it should be a 'top-down' screenshot
      however, at this moment I'm absolutely enough with this simple 'duplication' of the tile map.
      based on caeles CC0 template from OpenGameArt }
    procedure MakePNGMap;
    { the actual editor of the tile map, responds to mouse x,y click }
    procedure EditTileMap(x,y: integer);
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

uses CastleVectors, CastleCameras,
     DOM, CastleXMLUtils,
     decoLoad3d, x3dLoad, CastleURIUtils, BlenderCleaner,
     CastleFilesUtils,
     DecoGlobal, DecoLog;

procedure TDungeonTilesEditor.FreeMe;
begin
  FreeAndNil(TilesList);
  FreeAndNil(TileM);
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
begin
  FreeAndNil(TilesList);
  TilesList := GetFilesList(TilesFolder,'x3d');
  dLog(LogConstructorInfo,Self,'TDungeonTilesEditor.ReadTilesList','Tiles found = '+IntToStr(TilesList.Count));
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.FillTilesList;
var s: string;
begin
  TilesBox.clear;
  TilesBox.Sorted := true;
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

procedure TDungeonTilesEditor.SaveTileMap(FileName: string; toGameFolder: boolean);
begin
  TileM.Save(FileName,ToGameFolder);
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.SaveAllTiles;
var t: string;
begin
  for t in TilesList do begin
    compileTile(t);
    //copy tile image?
    //SaveTileMap(t,true);
  end;
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.WriteMe(ToGameFolder: boolean);
begin
  //dLog('TDungeonTilesEditor.WriteMe','Working directly on game data, nothing to save.');
  if not ToGameFolder then begin
    if isTileLoaded then begin
      SaveTileMap(TileName,false);
    end else dLog(LogConstructorError,Self,'TDungeonTilesEditor.WriteMe','No tile loaded to save...');
  end else
    SaveAllTiles;
  inherited WriteMe(ToGameFolder);
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
      if (TileScene<>nil) then begin
        TileDisplay.scenemanager.camera.setView(TileScene.BoundingBox.center+Vector3(0,0,TileScene.BoundingBox.maxsize+1),Vector3(0,0,-1),Vector3(0,1,0));
        TileDisplay.scenemanager.camera.input:=TCamera.DefaultInput;
        TileDisplay.update;
      end else
        dLog(LogConstructorError,Self,'TDungeonTilesEditor.ResetCamera','ERROR: TileScene is nil! Can''t reset camera');
    end else
      dLog(LogConstructorError,Self,'TDungeonTilesEditor.ResetCamera','No Tile Loaded.');
  end else
    dLog(LogConstructorError,Self,'TDungeonTilesEditor.ResetCamera','No Camera To Reset.');
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

  TileRoot := CleanUp( Load3D( ConstructorData(TilesFolder+Filename+'.x3d',false) ) ,true,true);
  TileScene := TCastleScene.create(TileDisplay);
  TileScene.Load(TileRoot, true);
  TileDisplay.SceneManager.Items.Add(TileScene);
  TileDisplay.SceneManager.MainScene := TileScene;

  isTileLoaded := true;

  {load tile map}

  FreeAndNil(TileM);
  TileM := DTileMap.LoadForConstructor( ConstructorData(TilesFolder+Filename,false) );
  if not TileM.Ready then TileM.GuessSize(TileScene);

  ResetCamera;
  PrepareMapEditor;
  DrawTileMap;
end;

{---------------------------------------------------------------------------}

procedure FixTextures(root: TX3DRootNode);
{maybe, a better name would be nice.
 attaches texture properties (anisotropic smoothing) to the texture of the object.
 TODO: Normal map still doesn't work. I should fix it one day...}
  procedure ScanNodesRecoursive(source: TAbstractX3DGroupingNode);
  var i: integer;
      tex: TImageTextureNode;
      s: string;
  begin
    for i := 0 to source.FdChildren.Count-1 do
    if source.FdChildren[i] is TAbstractX3DGroupingNode then
      ScanNodesRecoursive(TAbstractX3DGroupingNode(source.FdChildren[i]))
    else
      if (source.FdChildren[i] is TShapeNode) then
        try
          tex := (TShapeNode(source.FdChildren[i]).fdAppearance.Value.FindNode(TImageTextureNode,false) as TImageTextureNode);
          //blender only! Using only the first texture file link
          s := tex.FdUrl.ItemsSafe[0];
          s := ChangeURIExt(s,'.dds');
          s := '../textures/'+ExtractURIName(s);
          tex.FdUrl.Items.Clear;
          tex.FdUrl.Items.Add(s);
          //check URIFileExists
        except
          dLog(LogConstructorInfo,nil,'FixTextures.ScanNodesRecoursive','try..except fired (texture node not found)');
        end;
  end;
begin
  ScanNodesRecoursive(Root);
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.CompileTile(FileName: string);
var TmpRoot: TX3DRootNode;
    tmpMap: DTileMap;
    f: string;
    iz: integer;
begin
  dLog(LogConstructorInfo,Self,'TDungeonTilesEditor.CompileTile','Compile: '+FileName);

  //load tile from Architect folder
  TmpRoot := CleanUp( Load3D(ConstructorData(TilesFolder+Filename+'.x3d',false)) ,true,true);
  FixTextures(TmpRoot);
  //todo: check for used/unused textures and delete/add them
  //save tile to game folder
  save3D(TmpRoot, ConstructorData(TilesFolder+Filename+'.x3d'+GZ_ext,true));
  FreeAndNil(TmpRoot);

  //compile tile map
  tmpMap := DTileMap.LoadForConstructor( ConstructorData(TilesFolder+FileName,false) );
  if tmpMap.Ready then begin
    tmpMap.Save(FileName, true);

    //copy map image/images
    for iz := 0 to tmpMap.sizez-1 do begin
      f := TilesFolder+FileName+'_'+inttostr(iz)+'.png';
      if URIFileExists(ConstructorData(f,false)) then begin
        if not CopyFile(FakeConstructorData(f,false), FakeConstructorData(f,true),[cffOverwriteFile],false) then
          dLog(LogConstructorError,Self,'TDungeonTilesEditor.CompileTile','ERROR: failed to copy map image: '+f);

      end else
        dLog(LogConstructorError,Self,'TDungeonTilesEditor.CompileTile','ERROR: Map image not found: '+f);
    end;

  end;
  FreeAndNil(tmpMap);

end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.LoadButtonClick(Sender: TObject);
begin
  if (TilesBox.ItemIndex>=0) and (TilesBox.Items[TilesBox.ItemIndex]<>'') then
    LoadTile( TilesBox.Items[TilesBox.ItemIndex] )
  else
    dLog(LogConstructorError,Self,'TDungeonTilesEditor.LoadButtonClick','ERROR: Tile List is empty');
end;

{============================================================================}
{========================== map editing routines ============================}
{============================================================================}

//I think some day it should start from an ini file. But now that's more than enough
procedure TDungeonTilesEditor.FillAtlas;
var i: integer;
begin
   {make faces}

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

   {make base}

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
  FaceAtlasBox.ItemIndex := 3; //set to wall
  BaseAtlasBox.ItemIndex := 1; //set to free
end;

{-------------------------------------------------------------------------}

function TDungeonTilesEditor.FaceByIndex(index: integer): TTileFace;
var tf: TTileFace;
begin
  for tf in TTileFace do if FaceAtlasBox.Items[index]=FaceAtlas[tf].friendlyName then begin
    Result := tf;
    exit;
  end;
  Result := tfNone;
  dLog(LogConstructorError,Self,'TDungeonTilesEditor.FaceByIndex','ERROR: Face not found! For index '+ inttostr(index));
end;

{-------------------------------------------------------------------------}

function TDungeonTilesEditor.BaseByIndex(index: integer): TTileKind;
var tk: TTileKind;
begin
  for tk in TTileKind do if BaseAtlasBox.Items[index]=BaseAtlas[tk].FriendlyName then begin
    Result := tk;
    Exit;
  end;
  Result := tkNone;
  dLog(LogConstructorError,Self,'TDungeonTilesEditor.BaseByIndex','ERROR: Base not found! For index '+ inttostr(index));
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
var ix,iy,iz: integer;
    x1,y1,x2,y2,x3,y3,x4,y4: integer;
    scalex,scaley: float;
begin
  if (isTileLoaded) and (TileM <> nil) and (TileM.Ready) then begin
    if BaseRadio.checked then
      BaseAtlasBox.visible := true
    else
      BaseAtlasBox.visible := false;

    //currentZ:=scrollbar1.position;
    ZLabel.Caption := inttostr(currentZ);
    with MapImage.canvas do begin
      //clear current image
      brush.color := clSilver;
      fillrect(0,0,MapImage.width,MapImage.height);
      //working with currentZ
      iz := currentZ;
      //determine draw scale
      scalex := MapImage.width/TileM.Sizex;
      scaley := MapImage.height/TileM.Sizey;
      //draw a tile element
      for ix := 0 to TileM.Sizex-1 do
       for iy := 0 to TileM.Sizey-1 do with TileM.Map[ix,iy,iz] do begin
         //determine basic coordinates inside the tile
         { x1,y1 - x3 ------ x4 - x2,y1
               |   |          |   |
           x1,y3 - + -------- + - x2,y3
               |   |          |   |
               |   |          |   |
               |   |          |   |
           x1,y4 - + -------- + - x2,y4
               |   |          |   |
           x1,y2 - x3 ------ x4 - x2,y2 }
         x1 := round( (ix)        *scalex );
         y1 := round( (iy)        *scaley );
         x2 := round( (ix+1)      *scalex );
         y2 := round( (iy+1)      *scaley );
         x3 := round( (ix+delta)  *scalex );
         y3 := round( (iy+delta)  *scaley );
         x4 := round( (ix+1-delta)*scalex );
         y4 := round( (iy+1-delta)*scaley );

         {draw central spot base. As bsCross and onther styles are transparent
          we always have to draw a bsSolid base first}
         brush.style := bsSolid;
         brush.color := BaseAtlas[base].color;
         fillrect(x3,y3,x4,y4);

         //draw floor/ceiling if selected
         if CeilingRadio.checked then begin
           brush.style := bsCross;
           brush.color := FaceAtlas[faces[aUp]].color;
           fillrect(x3,y3,x4,y4);
         end else
         if FloorRadio.checked then begin
           brush.style := bsDiagCross;
           brush.color := FaceAtlas[faces[aDown]].color;
           fillrect(x3,y3,x4,y4);
         end;

         //draw tile faces
         brush.style := bsSolid;

         brush.color := FaceAtlas[faces[aTop]].color;
         fillrect(x3,y1,x4,y3); //top

         brush.color := FaceAtlas[faces[aLeft]].color;
         fillrect(x1,y3,x3,y4); //left

         brush.color := FaceAtlas[faces[aBottom]].color;
         fillrect(x3,y4,x4,y2); //bottom

         brush.color := FaceAtlas[faces[aRight]].color;
         fillrect(x4,y3,x2,y4); //right
       end;
    end;


  end else dLog(LogConstructorError,Self,'TDungeonTilesEditor.DrawTileMap','TileMap is not ready to draw');
end;

{-------------------------------------------------------------------------}

procedure TDungeonTilesEditor.PrepareMapEditor;
begin
  {actualize vertical scrollbar}
  ZScroll.Min := 0;
  ZScroll.Max := TileM.sizez-1;
  ZScroll.position := ZScroll.max;
  if TileM.sizez = 1 then begin
    ZScroll.enabled := false;
    ZLabel.visible := false;
  end else begin
    ZScroll.enabled := true;
    ZLabel.visible := true;
  end;
  CurrentZ := ZScroll.position;

  {prepare image size}
  {$Warning Will exceed the screen size for very large tiles}
  MapImage.width := tile_image_scale*TileM.Sizex;
  MapImage.height := tile_image_scale*TileM.Sizey;
  { fix Lazarus [BUG]:
    Bitmap.Resize should be done automatically on OnResize but it isn't
    They've said it's a feature not a bug. Ой, всё :) }
  MapImage.Picture.Bitmap.SetSize(MapImage.Width,MapImage.height);

  //set height and width of controls based on tile map size
  ZScroll.height := MapImage.height{-zLabel.height};
end;

{----------------------------------------------------------------------------}

procedure TDungeonTilesEditor.SavePNGButtonClick(Sender: TObject);
begin
  MakePNGMap;
end;

{----------------------------------------------------------------------------}

procedure TDungeonTilesEditor.SaveTileMapButtonClick(Sender: TObject);
begin
  WriteMe(false);
end;

{$INCLUDE constructor_tiles_png_map.inc}

{---------------------------------------------------------------------------}

procedure TDungeonTilesEditor.EmptyMapButtonClick(Sender: TObject);
begin
  if isTileLoaded and (TileM <> nil) then begin
    if MessageDlg('Clear tile?', 'Clear tile map?', mtConfirmation, [mbYes, mbNo],0) = mrYes then begin
      TileM.EmptyTile;
      DrawTileMap;
      isChanged := false;
    end;
  end else dLog(LogConstructorError,Self,'TDungeonTilesEditor.EmptyMapButtonClick','Tile is not ready!');
end;

{--------------------------------------------------------------------------}

procedure TDungeonTilesEditor.RadioChange(Sender: TObject);
begin
  DrawTileMap;
end;

{---------------------------------------------------------------------------}

procedure TDungeonTilesEditor.MapImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EditTileMap(x,y);
end;

procedure TDungeonTilesEditor.EditTileMap(x,y: integer);
var scalex,scaley: float;
    dx,dy: float;
    x1,y1,z1,ax,ay: integer;
    angle1: TAngle;
    face_value: TTileFace;
    base_value: TTileKind;
begin
  //working with rectagonal grid only (for now, maybe forever :))
 if (isTileLoaded) and (TileM<>nil) then begin
   currentZ := ZScroll.Position;  //fool's check

   scalex := MapImage.width/ TileM.Sizex;
   scaley := MapImage.height/TileM.Sizey;

   z1 := currentZ;
   x1 := trunc(X / scalex);
   y1 := trunc(Y / scaley);

   //a fool's check, just in case...
   if x1 < 0 then x1 := 0;
   if y1 < 0 then y1 := 0;
   if z1 < 0 then z1 := 0;
   if x1 >= TileM.sizex then x1 := TileM.sizex-1;
   if y1 >= TileM.sizey then y1 := TileM.sizey-1;
   if z1 >= TileM.sizez then z1 := TileM.sizez-1;

   dx := X/scalex-x1;
   dy := Y/scaley-y1;

   if dx < delta then ax := -1 else begin
     if dx > 1-delta then ax := 1 else ax := 0;
   end;
   if dy < delta then ay := -1 else begin
     if dy > 1-delta then ay := 1 else ay := 0;
   end;

   {yeah, ugly, but I don't want to take care of it later
   and I don't want to work with diagonals, maybe later}
   if abs(ax)+abs(ay) > 1 then exit;

   //grab current palette values from the ComboBoxes
   base_value := BaseByIndex(BaseAtlasBox.ItemIndex);
   face_value := FaceByIndex(FaceAtlasBox.ItemIndex);

   if (ax = 0) and (ay = 0) then begin
     {edit central spot}

     if CeilingRadio.checked then begin
       //edit ceiling
       if  TileM.Map[x1,y1,z1].faces[aUp] = tfWall
       then
           TileM.Map[x1,y1,z1].faces[aUp] := face_value
       else
           TileM.Map[x1,y1,z1].faces[aUp] := tfWall;
       if (SymmetricEditCheckBox.checked) and (z1>0) then
           TileM.Map[x1,y1,z1-1].faces[aDown] := TileM.Map[x1,y1,z1].faces[aUp];
     end

     else
     if FloorRadio.checked then begin
       //edit floor
       if  TileM.Map[x1,y1,z1].faces[aDown] = tfWall
       then
           TileM.Map[x1,y1,z1].faces[aDown] := face_value
       else
           TileM.Map[x1,y1,z1].faces[aDown] := tfWall;

       if (SymmetricEditCheckBox.checked) and (z1<TileM.SizeZ-1) then
           TileM.Map[x1,y1,z1+1].faces[aUp] := TileM.Map[x1,y1,z1].faces[aDown];
     end

     else begin
       //edit base tile
       if TileM.Map[x1,y1,z1].base = base_value then begin
         //clear the tile to n/a
         TileM.Map[x1,y1,z1].base := tkNone;
         if SymmetricEditCheckBox.checked then
           //and reset all the walls around it
           for angle1 in TAngle do TileM.Map[x1,y1,z1].faces[angle1] := tfNone;
       end else begin
         //assign values and create walls around it if face=face_na;
         TileM.Map[x1,y1,z1].base := base_value;
         if SymmetricEditCheckBox.checked then begin
           //reset all faces to 'face-free' here because it's meant to be inside the tile map, so no external links will be necessary, just to point that there is a passage here
           if TileM.Map[x1,y1,z1].faces[aTop] = tfNone then begin
             if y1=0 then TileM.Map[x1,y1,z1].faces[aTop] := tfWall else
                          TileM.Map[x1,y1,z1].faces[aTop] := tfFree;
           end;
           if TileM.Map[x1,y1,z1].faces[aBottom] = tfNone then begin
             if y1=TileM.SizeY-1 then TileM.Map[x1,y1,z1].faces[aBottom] := tfWall else
                                      TileM.Map[x1,y1,z1].faces[aBottom] := tfFree;
           end;
           if TileM.Map[x1,y1,z1].faces[aLeft] = tfNone then begin
             if x1=0 then TileM.Map[x1,y1,z1].faces[aLeft] := tfWall else
                          TileM.Map[x1,y1,z1].faces[aLeft] := tfFree;
           end;
           if TileM.Map[x1,y1,z1].faces[aRight] = tfNone then begin
             if x1=TileM.SizeX-1 then TileM.Map[x1,y1,z1].faces[aRight] := tfWall else
                                      TileM.Map[x1,y1,z1].faces[aRight] := tfFree;
           end;
           if TileM.Map[x1,y1,z1].faces[aUp] = tfNone then begin
             if z1=0 then TileM.Map[x1,y1,z1].faces[aUp] := tfWall else
                          TileM.Map[x1,y1,z1].faces[aUp] := tfFree;
           end;
           if TileM.Map[x1,y1,z1].faces[aDown] = tfNone then begin
             if z1 = TileM.SizeZ-1 then TileM.Map[x1,y1,z1].faces[aDown] := tfWall else
                                        TileM.Map[x1,y1,z1].faces[aDown] := tfFree;
           end;
           //moreover, we have to check if this is a stairs-up-down and if the tile is correct
           if (TileM.Map[x1,y1,z1].base = tkUp) then begin
             if z1>0 then
               TileM.Map[x1,y1,z1-1].base := tkDown
             else showmessage('The ladder goes UP beyond the tile border!');
           end;
           if (TileM.Map[x1,y1,z1].base = tkDown) then begin
             if z1<TileM.SizeZ-1 then
               TileM.Map[x1,y1,z1+1].base := tkUp
             else showmessage('The ladder goes DOWN beyond the tile border!');
           end;
         end;
       end;
     end;
   end else begin
     {edit walls}
     //get angle from ax,ay
     angle1 := GetAngle(ax,ay);

     if TileM.Map[x1,y1,z1].faces[angle1] = tfWall
     then
        TileM.Map[x1,y1,z1].faces[angle1] := face_value
     else
        TileM.Map[x1,y1,z1].faces[angle1] := tfWall;

     {preform symmetric edit}
     if SymmetricEditCheckBox.Checked then begin
       if (TileM.Map[x1,y1,z1].base <> tkNone) and
          (TileM.MapSafeBase(x1+a_dx(angle1),y1+a_dy(angle1),z1) <> tkInacceptible) and
          (TileM.MapSafeBase(x1+a_dx(angle1),y1+a_dy(angle1),z1) <> tkNone)
       then
          TileM.Map[x1+a_dx(angle1),y1+a_dy(angle1),z1].faces[InvertAngle(angle1)] := TileM.Map[x1,y1,z1].faces[angle1];
     end;
   end;
   isChanged := true;
   DrawTileMap;
 end;
end;

{========================= DTileMap Helper ==================================}

procedure DTileMapHelper.EmptyTile;
var a: TAngle;
begin
  if not blocker then
    {this is a normal tile}
    emptyMap(true)
  else
    {this is a blocker tile}
    with Map[0,0,0] do begin
      base := tkNone;
      for a in TAngle do faces[a] := tfNone;
    end;
end;

{-----------------------------------------------------------------------}

procedure DTileMapHelper.DetectBlocker;
begin
  if (sizex=0) or (sizey=0) or (sizez=0) then begin
    blocker := true;
    {a hack to allow for 1x2x0 stlye blockers, maybe will never be used
     as it's unsafe to do something "large and ugly" like that}
    if sizex = 0 then sizex := 1;
    if sizey = 0 then sizey := 1;
    if sizez = 0 then sizez := 1;
  end else
    blocker := false;
end;

{-----------------------------------------------------------------------}

procedure DTileMapHelper.GuessSize(Tile3D: TCastleScene);
begin
  dLog(LogConstructorError,Self,'DTileMapHelper.GuessSize','Guessing tile size: '+floattostr(Tile3D.BoundingBox.sizex)+'x'+floattostr(Tile3D.BoundingBox.sizey)+'x'+floattostr(Tile3D.BoundingBox.sizez));
  {$Warning this solution is not optimal and might fail for complex tiles}
  Sizex := round((Tile3D.BoundingBox.sizex+0.1)/TileScale);
  Sizey := round((Tile3D.BoundingBox.sizey+0.1)/TileScale{*(1+0.5)}); //whhyyyyyyyy it falied at 1 of ~150 tiles ?????
  Sizez := round((Tile3D.BoundingBox.sizez+0.1)/TileScale);
  DetectBlocker;
  GetMapMemory;
  EmptyTile;
  Ready := true;
end;

{-----------------------------------------------------------------------}

constructor DTileMapHelper.LoadForConstructor(URL: string);
begin
  try
    Load(URL,false);
  except
    dLog(LogConstructorInfo,Self,'DTileMapHelper.LoadForConstructor','Exception caught. Usually it''s ok.');
  end;
end;

{--------------------------------------------------------------------------}

Procedure DTileMapHelper.Save(TName: string; ToGameFolder: boolean);
var TileDOC: TXMLDocument;
    RootNode: TDOMNode;
    WorkNode, ContainerNode: TDOMElement;
    jx,jy,jz: integer;
    j: TAngle;
begin
  TileDOC := TXMLDocument.Create;
  RootNode := TileDOC.CreateElement('TileMap');
  TileDOC.Appendchild(RootNode);

  WorkNode := TileDOC.CreateElement('Size');
  WorkNode.AttributeSet('size_x',SizeX);
  WorkNode.AttributeSet('size_y',SizeY);
  WorkNode.AttributeSet('size_z',SizeZ);
  WorkNode.AttributeSet('blocker',blocker);
  RootNode.AppendChild(WorkNode);

  for jx := 0 to SizeX-1 do
    for jy := 0 to SizeY-1 do
      for jz := 0 to SizeZ-1 do begin
        ContainerNode := TileDOC.CreateElement('Tile');
        ContainerNode.AttributeSet('x',jx);
        ContainerNode.AttributeSet('y',jy);
        ContainerNode.AttributeSet('z',jz);

          WorkNode := TileDOC.CreateElement('base');
          WorkNode.AttributeSet('tile_kind',TileKindToStr(Map[jx,jy,jz].base));
          ContainerNode.AppendChild(WorkNode);

          WorkNode := TileDOC.CreateElement('faces');
          for j in TAngle do
            WorkNode.AttributeSet(AngleToStr(j),TileFaceToStr(Map[jx,jy,jz].faces[j]));
          ContainerNode.AppendChild(WorkNode);

        RootNode.AppendChild(ContainerNode);
      end;

  if ToGameFolder then
    URLWriteXML(TileDOC, ConstructorData(TilesFolder+TName+'.map'+gz_ext,ToGameFolder))
  else
    URLWriteXML(TileDOC, ConstructorData(TilesFolder+TName+'.map',ToGameFolder));

  dLog(LogConstructorInfo,Self,'DTileMapHelper.Save',ConstructorData(TilesFolder+TName+'.map',ToGameFolder));

  FreeAndNil(TileDOC);
end;

end.

