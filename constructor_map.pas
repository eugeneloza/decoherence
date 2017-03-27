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
  ExtCtrls, CheckLst, CastleControl, constructor_global,
  decodungeongenerator;

type
  TMapEditor = class(TWriterForm)
    CheckListBox1: TCheckListBox;
    GenerateButton: TButton;
    MapDisplay: TCastleControl;
    procedure FormDestroy(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
  public
    TilesList: TStringList;


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

uses StrUtils, CastleLog;

procedure TMapEditor.LoadMe;
begin
  //dummy
  isChanged := false;
  isLoaded := true;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.FreeMe;
begin
  //dummy
end;
procedure TMapEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{-------------------------------------------------------------------------}

procedure TMapEditor.GenerateButtonClick(Sender: TObject);
var GENERATOR: DDungeonGenerator;
begin
  GENERATOR := DDungeonGenerator.Create;
  {$warning set up parameters here}
  //GENERATOR.load('');
  with GENERATOR.parameters do begin
    maxx := 10;
    maxy := 10;
    maxz := 3;

    minx := 9;
    miny := 9;
    minz := 3;

    seed := 0;
  end;
  GENERATOR.ForceReady;
  GENERATOR.Generate;
  //somethinguseful := GEN.GetMap;
  FreeAndNil(GENERATOR);
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
  if FindFirst (FakeConstructorData('models/tiles/*.x3d',false), faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       TilesList.Add(AnsiReplaceText(Rec.Name,'.x3d',''));
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
  WriteLnLog('TMapEditor.GetTileList','Tiles found = '+inttostr(TilesList.count));


end;

end.

