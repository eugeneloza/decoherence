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
  decodungeongenerator;

type
  TMapEditor = class(TWriterForm)
    CastleImageControl1: TCastleImageControl;
    TilesBox: TCheckListBox;
    GenerateButton: TButton;
    MapDisplay: TCastleControl;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
  public
    {List of tiles names}
    TilesList: TStringList;
    {list of actual tiles}
    Tiles: TTileList;

    {fills the checklistbox with tiles names and checks them all}
    procedure FillBox;
    {reads tiles list from the folder}
    procedure GetTileList;
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
  decodungeontiles,
  decoglobal;


{-------------------------------------------------------------------------}

procedure TMapEditor.GenerateButtonClick(Sender: TObject);
var GENERATOR: DDungeonGenerator;
  i: integer;
  fs: DFirstStep;
  tmpmap: DMap;
begin
  GENERATOR := DDungeonGenerator.Create;
  //GENERATOR.load('');
  with GENERATOR.parameters do begin
    maxx := 25;
    maxy := 25;
    maxz := 9;

    Volume := maxx*maxy*maxz div 5;
    MaxFaces := 9;
    MinFaces := 4;

    minx := 9;
    miny := 9;
    minz := 9;

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
  //somethinguseful := GEN.GetMap;
  {$Warning memory leaks here!}
  tmpMap := GENERATOR.GetMap;
  CastleImageControl1.Image := tmpMap.img[0].MakeCopy;
  freeAndNil(tmpMap);
  FreeAndNil(GENERATOR);
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
end;
procedure TMapEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{--------------------------------------------------------------------------}

procedure TMapEditor.FormShow(Sender: TObject);
begin
  {$Warning should be on create, not on show!}
  MapDisplay.SceneManager.InsertFront(CastleImageControl1);
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

