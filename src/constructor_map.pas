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
unit Constructor_Map;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, ComCtrls, CastleControl, CastleControls,
  Constructor_Global,
  DecoDungeonTiles, DecoDungeonGenerator;

type
  TMapEditor = class(TWriterForm)
    SaveButton: TButton;
    MapSelector: TComboBox;
    TileImage: TCastleImageControl;
    EditMaxF: TEdit;
    EditMinF: TEdit;
    EditSizeX: TEdit;
    EditSizeX1: TEdit;
    EditSizeY: TEdit;
    EditSizeY1: TEdit;
    EditSizeZ: TEdit;
    EditSizeZ1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TilesBox: TCheckListBox;
    VolumeEdit: TEdit;
    ZLabel: TLabel;
    ZScroll: TScrollBar;
    GenerateButton: TButton;
    MapDisplay: TCastleControl;
    procedure EditMaxFChange(Sender: TObject);
    procedure EditMinFChange(Sender: TObject);
    procedure EditSizeX1Change(Sender: TObject);
    procedure EditSizeXChange(Sender: TObject);
    procedure EditSizeY1Change(Sender: TObject);
    procedure EditSizeYChange(Sender: TObject);
    procedure EditSizeZ1Change(Sender: TObject);
    procedure EditSizeZChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure VolumeEditChange(Sender: TObject);
    procedure ZScrollChange(Sender: TObject);
  public
    {List of tiles names}
    MapsList: TStringList;
    {List of tiles names}
    TilesList: TStringList;

    DungeonMap: DMap;

    {if map is ready then draws the map}
    procedure DrawMap;
    {fills a DGeneratorParameters from the form elements
     Result must be assigned to the generator (or freed)}
    function GetMapParameters: DDungeonGeneratorParameters;
    {saves current map parameters to a file}
    procedure SaveMap(FileName: string; const ToGameFolder: boolean);
    {compile all the maps to game folder}
    procedure SaveAll;

    {read list of maps available in the scenario}
    procedure ReadMapsList;
    {copies the map list into TComboBox}
    procedure FillMapsList;
    {fills the checklistbox with tiles names and checks them all}
    procedure FillTilesList;
    {reads tiles list from the folder}
    procedure ReadTilesList;
  public
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(const ToGameFolder: boolean); override;
  end;

var
  MapEditor: TMapEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$R *.lfm}

uses CastleImages, CastleVectors,
  DOM, CastleXMLUtils,
  DecoGlobal, DecoLog;


{-------------------------------------------------------------------------}

function TMapEditor.GetMapParameters: DDungeonGeneratorParameters;
var i: integer;
    fs: DFirstStep;
begin
  Result := DDungeonGeneratorParameters.Create; //this creates tiles and first steps
  with Result do begin
    MaxX := StrToInt(EditSizeX.Text);
    MaxY := StrToInt(EditSizeY.Text);
    MaxZ := StrToInt(EditSizeZ.Text);

    Volume := round(MaxX*MaxY*MaxZ * strToFloat(VolumeEdit.Text)/100);
    MaxFaces := StrToInt(EditMaxF.Text);
    MinFaces := StrToInt(EditMinF.Text);

    MinX := StrToInt(EditSizeX1.Text);
    MinY := StrToInt(EditSizeY1.Text);
    MinZ := StrToInt(EditSizeZ1.Text);

    Seed := 0;

    AbsoluteURL := true;
    for i := 0 to TilesBox.Items.Count-1 do
      if TilesBox.Checked[i] then TilesList.Add(ConstructorData(TilesFolder+TilesBox.Items[i], false));

    fs.Tile := 'library1_16_P';
    fs.x := MaxX div 2;
    fs.y := MaxY div 2;
    fs.z := 0;
    FirstSteps.Add(fs);
  end;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.GenerateButtonClick(Sender: TObject);
var GENERATOR: D3dDungeonGenerator;
begin
  GenerateButton.Enabled := false;
  FreeAndNil(DungeonMap);
  GENERATOR := D3dDungeonGenerator.Create;
  //GENERATOR.load('');
  FreeAndNil(Generator.Parameters); //purge autocreated parameters
  GENERATOR.Parameters := GetMapParameters;  //will be autofreed by GENERATOR destructor

  GENERATOR.ForceReady;
  GENERATOR.InitParameters;
  GENERATOR.Generate;

  DungeonMap := GENERATOR.ExportMap;

  FreeAndNil(GENERATOR);

  ZScroll.Min := 0;
  ZScroll.Position := 0;
  ZScroll.Max := DungeonMap.SizeZ - 1;
  DrawMap;

  GenerateButton.Enabled := true;
end;

procedure TMapEditor.SaveButtonClick(Sender: TObject);
begin
  SaveMap('',false);
end;

procedure TMapEditor.ZScrollChange(Sender: TObject);
begin
  DrawMap;
end;

{------------------------------------------------------------------------}

Procedure TMapEditor.DrawMap;
var CurrentZ: integer;
begin
  if DungeonMap<>nil then begin
    CurrentZ := ZScroll.Position;
    ZLabel.Caption := IntToStr(CurrentZ);
    //TileImage.Image.FreeInstance;
    //FreeAndNil(TileImage.Image);
    TileImage.OwnsImage := false;
    TileImage.Image := DungeonMap.Img[CurrentZ];//.MakeCopy;
    MapDisplay.Width :=  TileImage.Image.Width;
    MapDisplay.Height :=  TileImage.Image.Height;
  end;
end;

{------------------------------------------------------------------------}

procedure TMapEditor.LoadMe;
begin
  ReadTilesList;
  FillTilesList;
  ReadMapsList;
  FillMapsList;
  isChanged := false;
  isLoaded := true;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.FreeMe;
begin
  FreeAndNil(MapsList);
  FreeAndNil(TilesList);
  FreeAndNil(DungeonMap);
end;
procedure TMapEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{--------------------------------------------------------------------------}

procedure TMapEditor.FormCreate(Sender: TObject);
begin
  MapDisplay.SceneManager.InsertFront(TileImage);
  TileImage.Left := 0;
  TileImage.Bottom := 0;
end;

{================== EDITORS CHANGE ==========================================}

procedure TMapEditor.EditSizeZChange(Sender: TObject);
var MaxZ,MinZ,k1,k2: integer;
begin
  val(EditSizeZ.Text,MaxZ,k1);
  val(EditSizeZ1.Text,MinZ,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinZ > MaxZ then EditSizeZ1.Text := IntToStr(MaxZ);
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeZ1Change(Sender: TObject);
var MaxZ,MinZ,k1,k2: integer;
begin
  val(EditSizeZ.Text,MaxZ,k1);
  val(EditSizeZ1.Text,MinZ,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinZ > MaxZ then EditSizeZ.Text := IntToStr(MinZ);
    //if (MaxZ>(MaxX+MaxY) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeYChange(Sender: TObject);
var MaxY,MinY,k1,k2: integer;
begin
  val(EditSizeY.Text,MaxY,k1);
  val(EditSizeY1.Text,MinY,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinY > MaxY then EditSizeY1.Text := IntToStr(MaxY);
    //if (MaxZ>(MaxX+MaxY) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeY1Change(Sender: TObject);
var MaxY,MinY,k1,k2: integer;
begin
  val(EditSizeY.Text,MaxY,k1);
  val(EditSizeY1.Text,MinY,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinY > MaxY then EditSizeY.Text := IntToStr(MinY);
    //if (MaxZ>(MaxX+MaxY) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeXChange(Sender: TObject);
var MaxX,MinX,k1,k2: integer;
begin
  val(EditSizeX.Text,MaxX,k1);
  val(EditSizeX1.Text,MinX,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinX > MaxX then EditSizeX1.Text := IntToStr(MaxX);
    //if (MaxZ>(MaxX+MaxY) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeX1Change(Sender: TObject);
var MaxX,MinX,k1,k2: integer;
begin
  val(EditSizeX.Text,MaxX,k1);
  val(EditSizeX1.Text,MinX,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinX > MaxX then EditSizeX1.Text := IntToStr(MaxX);
    //if (MaxZ>(MaxX+MaxY) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditMaxFChange(Sender: TObject);
var MaxF,MinF,k1,k2: integer;
begin
  val(EditMaxF.Text,MaxF,k1);
  val(EditMinF.Text,MinF,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinF > MaxF then EditMinF.Text := IntToStr(MaxF);
    if (MaxF < 5) then EditMaxF.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditMinFChange(Sender: TObject);
var MaxF,MinF,k1,k2: integer;
begin
  val(EditMaxF.Text,MaxF,k1);
  val(EditMinF.Text,MinF,k2);
  if (k1 = 0) and (k2 = 0) then begin
    if MinF > MaxF then EditMaxF.Text := IntToStr(MinF);
    if (MinF < 3) then EditMinF.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.VolumeEditChange(Sender: TObject);
var Vol,k: integer;
begin
  VolumeEdit.Color := clDefault;
  val(VolumeEdit.Text,Vol,k);
  if k = 0 then begin
    if (Vol < 2) or (Vol > 50) then VolumeEdit.Color := clRed;
    isChanged := true;
  end;
end;

{--------------------------------------------------------------------------}

procedure TMapEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) then LoadMe;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.WriteMe(const ToGameFolder: boolean);
begin
  if ToGameFolder then
    SaveAll
  else
    SaveMap('',ToGameFolder);   //empty string is "current file"
  inherited WriteMe(ToGameFolder);
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.SaveMap(FileName: string; const ToGameFolder: boolean);
var GParam: DDungeonGeneratorParameters;
    XMLdoc: TXMLDocument;
    RootNode: TDOMNode;
    LargeContainer, SmallContainer: TDOMElement;
    TextNode: TDOMNode;
    {s,}f: string;
    //j: DFirstStep;
    i: integer;
    //flg: boolean;
begin
  if FileName = '' then begin
    FileName := MapSelector.Text;
    if FileName = '' then begin
      ShowMessage('Please, specify a map name!');
      MapEditor.SetFocusedControl(MapSelector);
      Exit;
    end;
    {for s in MapSelector.Items do if s=FileName then begin
      if MessageDlg('File exists',FileName+' exists, overwrite',mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
      break;
    end; }
    //if not duplicate
    MapSelector.Items.Add(FileName);
    GParam := Self.GetMapParameters;
  end else begin
    GParam := DDungeonGeneratorParameters.Create;
    GParam.Load(ConstructorData(GetScenarioFolder+MapsFolder+FileName+'.xml',false));
  end;

  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('InteriorMap');
  XMLdoc.Appendchild(RootNode);

  //write generation parameters
  LargeContainer := XMLdoc.CreateElement('Parameters');
  SmallContainer := XMLdoc.CreateElement('Size');
  SmallContainer.AttributeSet('MaxX',GParam.MaxX);
  SmallContainer.AttributeSet('MaxY',GParam.MaxY);
  SmallContainer.AttributeSet('MaxZ',GParam.MaxZ);
  SmallContainer.AttributeSet('MinX',GParam.MinX);
  SmallContainer.AttributeSet('MinY',GParam.MinY);
  SmallContainer.AttributeSet('MinZ',GParam.MinZ);
  LargeContainer.AppendChild(SmallContainer);

  SmallContainer := XMLdoc.CreateElement('Volume');
  SmallContainer.AttributeSet('Value',GParam.Volume);
  LargeContainer.AppendChild(SmallContainer);

  LargeContainer.AppendChild(SmallContainer);
  SmallContainer := XMLdoc.CreateElement('Faces');
  SmallContainer.AttributeSet('Max',GParam.MaxFaces);
  SmallContainer.AttributeSet('Min',GParam.MinFaces);
  LargeContainer.AppendChild(SmallContainer);

  LargeContainer.AppendChild(SmallContainer);
  SmallContainer := XMLdoc.CreateElement('Seed');
  SmallContainer.AttributeSet('Value',GParam.Seed);
  LargeContainer.AppendChild(SmallContainer);

  RootNode.AppendChild(LargeContainer);

  //write tiles list
  LargeContainer := XMLdoc.CreateElement('TilesList');
  {If we're saving a current map:
   using a copy of the algorithm to make non-absolute URLs
   it is guaranteed to produce exactly the same result
   (as long as self.GetMapParameters is not changed)}
  if not ToGameFolder then begin
    for i := 0 to TilesBox.Items.Count-1 do if TilesBox.Checked[i] then begin
      SmallContainer := XMLdoc.CreateElement('Tile');
      TextNode := XMLdoc.CreateTextNode(UTF8decode(TilesBox.Items[i]));
      SmallContainer.AppendChild(TextNode);
      LargeContainer.AppendChild(SmallContainer);
    end
  end
  else
  {else use "normal" way
   because "load" will load non-absolute URLs}
    for i := 0 to GParam.TilesList.Count-1 {s in GParam.TilesList} do begin
      SmallContainer := XMLdoc.CreateElement('Tile');
      TextNode := XMLdoc.CreateTextNode(UTF8decode(GParam.TilesList[i]));
      SmallContainer.AppendChild(TextNode);
      LargeContainer.AppendChild(SmallContainer);
    end;
  RootNode.AppendChild(LargeContainer);

  //write first steps
  LargeContainer := XMLdoc.CreateElement('FirstSteps');
  for i := 0 to GParam.FirstSteps.Count-1{j in GParam.FirstSteps} do begin
    SmallContainer := XMLdoc.CreateElement('Tile');
    SmallContainer.AttributeSet('x',GParam.FirstSteps.Items[i].x);
    SmallContainer.AttributeSet('y',GParam.FirstSteps.Items[i].y);
    SmallContainer.AttributeSet('z',GParam.FirstSteps.Items[i].z);
    TextNode := XMLdoc.CreateTextNode(UTF8decode(GParam.FirstSteps.Items[i].Tile));
    SmallContainer.AppendChild(TextNode);
    LargeContainer.AppendChild(SmallContainer);
  end;
  RootNode.AppendChild(LargeContainer);

  //write the file
  if ToGameFolder then
    f := ConstructorData(GetScenarioFolder+MapsFolder+FileName+'.xml'+gz_ext,ToGameFolder)
  else
    f := ConstructorData(GetScenarioFolder+MapsFolder+FileName+'.xml',ToGameFolder);
  URLWriteXML(XMLdoc, f);

  Log(LogConstructorInfo,_CurrentRoutine,'File Written: '+f);

  FreeAndNil(XMLdoc);
  FreeAndNil(GParam);
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.SaveAll;
var s: string;
begin
  for s in MapSelector.Items do if s<>'' then SaveMap(s,true);
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.ReadMapsList;
begin
  FreeAndNil(MapsList);
  MapsList := GetFilesList(GetScenarioFolder+MapsFolder,'xml');
  Log(LogConstructorInfo,_CurrentRoutine,'Maps found = '+IntToStr(MapsList.Count));
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.ReadTilesList;
begin
  FreeAndNil(TilesList);
  TilesList := GetFilesList(TilesFolder,'map');
  Log(LogConstructorInfo,_CurrentRoutine,'Tiles found = '+IntToStr(TilesList.Count));
end;

{------------------------------------------------------------------------}

procedure TMapEditor.FillTilesList;
var s: string;
begin
  TilesBox.Clear;
  for s in TilesList do
    TilesBox.Items.Add(s);
  TilesBox.CheckAll(cbChecked);
end;

{------------------------------------------------------------------------}

procedure TMapEditor.FillMapsList;
var s: string;
begin
  MapSelector.Clear;
  for s in MapsList do
    MapSelector.Items.Add(s);
end;

end.

