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
  ExtCtrls, CheckLst, ComCtrls, CastleControl, CastleControls,
  constructor_global, decodungeontiles, decodungeongenerator;

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
    function GetMapParameters: DGeneratorParameters;
    {saves current map parameters to a file}
    procedure SaveMap(filename: string; togamefolder: boolean);
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
    procedure WriteMe(ToGameFolder: boolean); override;
  end;

var
  MapEditor: TMapEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$R *.lfm}

uses {StrUtils, }CastleLog, castleimages, castlevectors,
  DOM, CastleXMLUtils,
  decoglobal;


{-------------------------------------------------------------------------}

function TMapEditor.GetMapParameters: DGeneratorParameters;
var i: integer;
    fs: DFirstStep;
begin
  Result := DGeneratorParameters.create; //this creates tiles and first steps
  with Result do begin
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
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.GenerateButtonClick(Sender: TObject);
var GENERATOR: D3dDungeonGenerator;
begin
  GenerateButton.Enabled := false;
  FreeAndNil(DungeonMap);
  GENERATOR := D3dDungeonGenerator.Create;
  //GENERATOR.load('');
  FreeAndNil(Generator.parameters); //purge autocreated parameters
  GENERATOR.parameters := GetMapParameters;  //will be autofreed by GENERATOR destructor

  GENERATOR.ForceReady;
  GENERATOR.InitParameters;
  //todo: in a thread + stop
  GENERATOR.Generate;

  DungeonMap := GENERATOR.ExportMap;

  FreeAndNil(GENERATOR);

  ZScroll.Min := 0;
  ZScroll.Position := 0;
  ZScroll.Max := DungeonMap.sizez-1;
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
var currentZ: integer;
begin
  if DungeonMap<>nil then begin
    currentZ := ZScroll.Position;
    ZLabel.Caption := inttostr(currentz);
    //TileImage.Image.FreeInstance;
    //FreeAndNil(TileImage.Image);
    TileImage.OwnsImage := false;
    TileImage.Image := DungeonMap.img[CurrentZ];//.MakeCopy;
    MapDisplay.Width :=  TileImage.Image.width;
    MapDisplay.height :=  TileImage.Image.height;
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
var maxz,minz,k1,k2: integer;
begin
  val(EditSizeZ.Text,maxz,k1);
  val(EditSizeZ1.Text,minz,k2);
  if (k1=0) and (k2=0) then begin
    if minz>maxz then EditSizeZ1.Text := inttostr(maxz);
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeZ1Change(Sender: TObject);
var maxz,minz,k1,k2: integer;
begin
  val(EditSizeZ.Text,maxz,k1);
  val(EditSizeZ1.Text,minz,k2);
  if (k1=0) and (k2=0) then begin
    if minz>maxz then EditSizeZ.Text := inttostr(minz);
    //if (maxz>(maxx+maxy) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeYChange(Sender: TObject);
var maxy,miny,k1,k2: integer;
begin
  val(EditSizeY.Text,maxy,k1);
  val(EditSizeY1.Text,miny,k2);
  if (k1=0) and (k2=0) then begin
    if miny>maxy then EditSizeY1.Text := inttostr(maxy);
    //if (maxz>(maxx+maxy) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeY1Change(Sender: TObject);
var maxy,miny,k1,k2: integer;
begin
  val(EditSizeY.Text,maxy,k1);
  val(EditSizeY1.Text,miny,k2);
  if (k1=0) and (k2=0) then begin
    if miny>maxy then EditSizeY.Text := inttostr(miny);
    //if (maxz>(maxx+maxy) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeXChange(Sender: TObject);
var maxx,minx,k1,k2: integer;
begin
  val(EditSizeX.Text,maxx,k1);
  val(EditSizeX1.Text,minx,k2);
  if (k1=0) and (k2=0) then begin
    if minx>maxx then EditSizeX1.Text := inttostr(maxx);
    //if (maxz>(maxx+maxy) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditSizeX1Change(Sender: TObject);
var maxx,minx,k1,k2: integer;
begin
  val(EditSizeX.Text,maxx,k1);
  val(EditSizeX1.Text,minx,k2);
  if (k1=0) and (k2=0) then begin
    if minx>maxx then EditSizeX1.Text := inttostr(maxx);
    //if (maxz>(maxx+maxy) div 2) then EditSizeZ.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditMaxFChange(Sender: TObject);
var maxf,minf,k1,k2: integer;
begin
  val(EditMaxF.Text,maxf,k1);
  val(EditMinF.Text,minf,k2);
  if (k1=0) and (k2=0) then begin
    if minf>maxf then EditMinF.Text := inttostr(maxf);
    if (maxF<5) then EditMaxF.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.EditMinFChange(Sender: TObject);
var maxf,minf,k1,k2: integer;
begin
  val(EditMaxF.Text,maxf,k1);
  val(EditMinF.Text,minf,k2);
  if (k1=0) and (k2=0) then begin
    if minf>maxf then EditMaxF.Text := inttostr(minf);
    if (minF<3) then EditMinF.Color := clRed else EditMinF.Color := clDefault;
    isChanged := true;
  end;
end;

procedure TMapEditor.VolumeEditChange(Sender: TObject);
var vol,k: integer;
begin
  VolumeEdit.Color := clDefault;
  val(VolumeEdit.Text,vol,k);
  if k=0 then begin
    if (vol<2) or (vol>50) then VolumeEdit.Color := clRed;
    isChanged := true;
  end;
end;

{--------------------------------------------------------------------------}

procedure TMapEditor.FormShow(Sender: TObject);
begin
  if (not isLoaded) then LoadMe;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.WriteMe(ToGameFolder: boolean);
begin
  if ToGameFolder then
    SaveAll
  else
    SaveMap('',ToGameFolder);   //empty string is "current file"
  inherited WriteMe(ToGameFolder);
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.SaveMap(filename: string; togamefolder: boolean);
var GParam: DGeneratorParameters;
    XMLdoc: TXMLDocument;
    RootNode: TDOMNode;
    LargeContainer, SmallContainer: TDOMElement;
    TextNode: TDOMNode;
    {s,}f: string;
    //j: DFirstStep;
    i: integer;
    //flg: boolean;
begin
  if filename='' then begin
    filename := MapSelector.text;
    if filename='' then begin
      showmessage('Please, specify a map name!');
      MapEditor.SetFocusedControl(MapSelector);
      exit;
    end;
    {for s in MapSelector.Items do if s=filename then begin
      if MessageDlg('File exists',filename+' exists, overwrite',mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
      break;
    end; }
    //if not duplicate
    MapSelector.Items.Add(filename);
    GParam := self.GetMapParameters;
  end else begin
    GParam := DGeneratorParameters.create;
    GParam.Load(ConstructorData(GetScenarioFolder+MapsFolder+filename+'.xml',false));
  end;

  XMLdoc := TXMLDocument.Create;
  RootNode := XMLdoc.CreateElement('InteriorMap');
  XMLdoc.Appendchild(RootNode);

  //write generation parameters
  LargeContainer := XMLdoc.CreateElement('Parameters');
  SmallContainer := XMLdoc.createElement('Size');
  SmallContainer.AttributeSet('maxx',GParam.maxx);
  SmallContainer.AttributeSet('maxy',GParam.maxy);
  SmallContainer.AttributeSet('maxz',GParam.maxz);
  SmallContainer.AttributeSet('minx',GParam.minx);
  SmallContainer.AttributeSet('miny',GParam.miny);
  SmallContainer.AttributeSet('minz',GParam.minz);
  LargeContainer.AppendChild(SmallContainer);

  SmallContainer := XMLdoc.createElement('Volume');
  SmallContainer.AttributeSet('value',GParam.volume);
  LargeContainer.AppendChild(SmallContainer);

  LargeContainer.AppendChild(SmallContainer);
  SmallContainer := XMLdoc.createElement('Faces');
  SmallContainer.AttributeSet('max',GParam.MaxFaces);
  SmallContainer.AttributeSet('min',GParam.MinFaces);
  LargeContainer.AppendChild(SmallContainer);

  LargeContainer.AppendChild(SmallContainer);
  SmallContainer := XMLdoc.createElement('Seed');
  SmallContainer.AttributeSet('value',GParam.seed);
  LargeContainer.AppendChild(SmallContainer);

  RootNode.AppendChild(LargeContainer);

  //write tiles list
  LargeContainer := XMLdoc.CreateElement('TilesList');
  {If we're saving a current map:
   using a copy of the algorithm to make non-absolute URLs
   it is guaranteed to produce exactly the same result
   (as long as self.GetMapParameters is not changed)}
  if not ToGameFolder then begin
    for i := 0 to TilesBox.Items.count-1 do if TilesBox.Checked[i] then begin
      SmallContainer := XMLdoc.CreateElement('Tile');
      TextNode := XMLdoc.CreateTextNode(UTF8decode(TilesBox.Items[i]));
      SmallContainer.AppendChild(TextNode);
      LargeContainer.AppendChild(SmallContainer);
    end
  end
  else
  {else use "normal" way
   because "load" will load non-absolute URLs}
    for i := 0 to GParam.TilesList.count-1 {s in GParam.TilesList} do begin
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
    SmallContainer.AttributeSet('x',GParam.FirstSteps.L[i].x);
    SmallContainer.AttributeSet('y',GParam.FirstSteps.L[i].y);
    SmallContainer.AttributeSet('z',GParam.FirstSteps.L[i].z);
    TextNode := XMLdoc.CreateTextNode(UTF8decode(GParam.FirstSteps.L[i].tile));
    SmallContainer.AppendChild(TextNode);
    LargeContainer.AppendChild(SmallContainer);
  end;
  RootNode.AppendChild(LargeContainer);

  //write the file
  if ToGameFolder then
    f := ConstructorData(GetScenarioFolder+MapsFolder+filename+'.xml'+gz_ext,ToGameFolder)
  else
    f := ConstructorData(GetScenarioFolder+MapsFolder+filename+'.xml',ToGameFolder);
  URLWriteXML(XMLdoc, f);

  WriteLnLog('TMapEditor.SaveMap','File Written: '+f);

  FreeAndNil(XMLdoc);
  freeandnil(GParam);
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
  WriteLnLog('TMapEditor.ReadTilesList','Maps found = '+inttostr(MapsList.count));
end;

{------------------------------------------------------------------------------}

procedure TMapEditor.ReadTilesList;
begin
  FreeAndNil(TilesList);
  TilesList := GetFilesList(TilesFolder,'map');
  WriteLnLog('TMapEditor.GetTileList','Tiles found = '+inttostr(TilesList.count));
end;

{------------------------------------------------------------------------}

procedure TMapEditor.FillTilesList;
var s: string;
begin
  TilesBox.Clear;
  for s in TilesList do
    TilesBox.Items.add(s);
  TilesBox.CheckAll(cbChecked);
end;

{------------------------------------------------------------------------}

procedure TMapEditor.FillMapsList;
var s: string;
begin
  MapSelector.clear;
  for s in MapsList do
    MapSelector.Items.add(s);
end;

end.

