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

{ Map parameters editor and testing tool }
unit constructor_map;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, CastleControl, CastleControls, constructor_global,
  decodungeontiles, decodungeongenerator;

type
  TMapEditor = class(TWriterForm)
    CastleImageControl1: TCastleImageControl;
    Label10: TLabel;
    VolumeEdit: TEdit;
    EditSizeX1: TEdit;
    EditMaxF: TEdit;
    EditMinF: TEdit;
    EditSizeY1: TEdit;
    EditSizeZ: TEdit;
    EditSizeX: TEdit;
    EditSizeY: TEdit;
    EditSizeZ1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ZLabel: TLabel;
    ZScroll: TScrollBar;
    TilesBox: TCheckListBox;
    GenerateButton: TButton;
    MapDisplay: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure ZScrollChange(Sender: TObject);
  public
    {List of tiles names}
    TilesList: TStringList;
    {list of actual tiles}
    Tiles: TTileList;

    DungeonMap: DMap;

    {fills the checklistbox with tiles names and checks them all}
    procedure FillBox;
    {reads tiles list from the folder}
    procedure GetTileList;
    {if map is ready then draws the map}
    procedure DrawMap;
  public
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
  end;

var
  MapEditor: TMapEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$R *.lfm}

uses StrUtils, CastleLog, castleimages, castlevectors,
  decoglobal;


{-------------------------------------------------------------------------}

procedure TMapEditor.GenerateButtonClick(Sender: TObject);
var GENERATOR: DDungeonGenerator;
  i: integer;
  fs: DFirstStep;
begin
  FreeAndNil(DungeonMap);
  GENERATOR := DDungeonGenerator.Create;
  //GENERATOR.load('');
  with GENERATOR.parameters do begin
    maxx := strToInt(EditSizeX.text);
    maxy := strToInt(EditSizeY.text);
    maxz := strToInt(EditSizeZ.text);

    Volume := round(maxx*maxy*maxz * strToFloat(VolumeEdit.Text)/100);
    MaxFaces := strToInt(EditMaxF.text);
    MinFaces := strToInt(EditMinF.text);

    minx := strToInt(EditSizeX1.text);
    miny := strToInt(EditSizeY1.text);
    minz := strToInt(EditSizeZ1.text);

    seed := 0;

    AbsoluteURL := true;
    for i := 0 to TilesBox.Items.count-1 do
      if TilesBox.Checked[i] then TilesList.Add(ConstructorData(TilesFolder+TilesBox.Items[i],false));

    fs.tile := 'library1_16_P';
    fs.x := maxx div 2;
    fs.y := maxy div 2;
    fs.z := 0;
    FirstSteps.Add(fs);
  end;
  GENERATOR.ForceReady;
  GENERATOR.Generate;

  DungeonMap := GENERATOR.GetMap;

  FreeAndNil(GENERATOR);

  ZScroll.Min := 0;
  ZScroll.Max := DungeonMap.sizez-1;
  DrawMap;
end;

procedure TMapEditor.ZScrollChange(Sender: TObject);
begin
  DrawMap;
end;

{------------------------------------------------------------------------}

Procedure TMapEditor.DrawMap;
var currentZ: integer;
begin
  if DungeonMap<>nil then begin
    currentZ := ZScroll.Position;
    ZLabel.Caption := inttostr(currentz);
    //CastleImageControl1.Image.FreeInstance;
    //FreeAndNil(CastleImageControl1.Image);
    CastleImagecontrol1.OwnsImage := false;
    CastleImageControl1.Image := DungeonMap.img[CurrentZ];//.MakeCopy;
    MapDisplay.Width :=  CastleImageControl1.Image.width;
    MapDisplay.height :=  CastleImageControl1.Image.height;
  end;
end;

{------------------------------------------------------------------------}

procedure TMapEditor.FillBox;
var s: string;
begin
  TilesBox.Clear;
  for s in TilesList do
    TilesBox.Items.add(s);
  TilesBox.CheckAll(cbChecked);
end;


{------------------------------------------------------------------------}

procedure TMapEditor.LoadMe;
begin
  GetTileList;
  FillBox;
  isChanged := false;
  isLoaded := true;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.FreeMe;
begin
  FreeAndNil(TilesList);
  FreeAndNil(DungeonMap);
end;
procedure TMapEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

procedure TMapEditor.FormCreate(Sender: TObject);
begin
  MapDisplay.SceneManager.InsertFront(CastleImageControl1);
  CastleImageControl1.Left := 0;
  CastleImageControl1.Bottom := 0;
end;

{--------------------------------------------------------------------------}

procedure TMapEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) then LoadMe;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.WriteMe(ToGameFolder: boolean);
begin
  //dummy
  if not ToGameFolder then isChanged := false;
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.GetTileList;
var Rec: TSearchRec;
begin
  FreeAndNil(TilesList);
  TilesList := TStringList.Create;
  // Android incompatible
  if FindFirst (FakeConstructorData(TilesFolder+'*.map',false), faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       TilesList.Add(AnsiReplaceText(Rec.Name,'.map',''));
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
  WriteLnLog('TMapEditor.GetTileList','Tiles found = '+inttostr(TilesList.count));
end;

end.

