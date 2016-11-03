{Copyright (C) 2012-2016 Yevhen Loza

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

unit decogui;

{$mode objfpc}{$H+}

interface

uses classes, CastleRandom,
  decointerface, decoimages, decolabel,
  decofont,
  decoglobal;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { just = window.height, wihdow.width. Maybe I'll deprecate it later }
    width,height:integer;
    { random generator used for all interface random events }
    rnd: TCastleRandom;

    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Rescale; override;
    procedure draw; override;
  private
    { wind images to provide background }
    wind1,wind2: DWindImage;
    floater: DFloatImage;
    LoadScreenLabel, FloaterLabel: DLabel;
    procedure DoLoadNewImage;
    procedure loadwind;
  strict private
    FPS_label: DLabel;
    FPS_count: Integer;
    Last_render_time: TDateTime;

end;

var GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog,
  decofacts;

const LoadScreenFolder='interface/loadscreen/';


{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited create(AOwner);
  rnd := TCastleRandom.Create;
  LoadWind;
  InitializeFonts;
  LoadFacts;

  Last_render_time := now;
  FPS_count := 0;
  FPS_Label := DLabel.create(self);
  FPS_Label.base.setsize(0,0,1,1);
  FPS_Label.base.Opacity := 1;
  FPS_Label.Shadow := 0;
  FPS_Label.Font := RegularFont16;
  FPS_Label.text := 'x';

  width := -1;
  height := -1;
end;

procedure DInterfaceContainer.LoadWind;
begin
  Wind1 := DWindImage.create(self);
  Wind1.phasespeed := 1/(15+rnd.Random);
  Wind1.Load(LoadScreen_Folder+'WindClouds1_GIMP.jpg');
  Wind1.Opacity:=0.1;
  wind1.base.setsize(0,0,fullwidth,fullheight);
  Wind2 := DWindImage.create(self);
  Wind2.phasespeed := 1/(10+rnd.Random);
  Wind2.Load(LoadScreen_Folder+'WindClouds2_GIMP.jpg');
  wind2.base.setsize(0,0,fullwidth,fullheight);
  Wind2.Opacity:=0.1;
end;

destructor DInterfaceContainer.destroy;
begin
  writeLnLog('DInterfaceContainer.destroy','Game over...');
  freeandnil(rnd);
  DestroyFonts;
  inherited;
end;

procedure DInterfaceContainer.rescale;
begin
  writeLnLog('DInterfaceContainer.rescale',inttostr(window.Width)+'x'+inttostr(window.Height));
  if window.width < window.height then begin
    // BUG HERE! DOESN'T WORK AS EXPECTED in Linux !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    writeLnLog('DInterfaceContainer.rescale','ERROR: Only landscape orientation supported!');
    window.width := window.height+10;
  end;
  width := window.Width;
  height := window.Height;
  wind1.rescale;
  wind2.rescale;
  if floater<>nil then floater.rescale;
  inherited;
end;

procedure DInterfaceContainer.DoLoadNewImage;
begin
  if floater=nil then floater := DFloatImage.create(self);
  floater.FreeImage;
  LoadNewFloaterImage := false;
  floater.opacity := 0.8;
  floater.phasespeed := 1/15;
  floater.base.setsize(0,0,proportionalscale,fullheight);
  floater.LoadThread(LoadScreenFolder+GetRandomFactImage);

  if LoadScreenLabel=nil then LoadScreenLabel := DLabel.create(self);
  LoadScreenLabel.base.setsize(1,-2,10,10);
  LoadScreenLabel.base.Opacity := 1;
  LoadScreenLabel.Shadow := 1;
  LoadScreenLabel.Font := RegularFont16;
  LoadScreenLabel.text := 'Добро пожаловать в Decoherence :)'+dlinebreak+'Идёт загрузка, подождите...'+dlinebreak+'П.С. пока "почти нечего грузить" :)'+dlinebreak+'Просто нажмите любую клавишу...';

  if floaterLabel = nil then FloaterLabel := DLabel.create(self);
  floaterLabel.base.setsize(-11,1,10,10);
  floaterLabel.base.Opacity := 0;
  floaterLabel.Font := RegularFont16;
  floaterLabel.Shadow := 1;
  floaterLabel.text := GetRandomFact;
end;

procedure DInterfaceContainer.Draw;
begin
  if (floater = nil) or (LoadNewFloaterImage) then DoLoadNewImage;
  floater.draw;
  wind1.draw;
  wind2.draw;
  LoadScreenLabel.base.Opacity := 1;
  LoadScreenLabel.draw;

  floaterLabel.base.fy := 1 + 5*Floater.phase;
  floaterLabel.base.recalculate;
  floaterLabel.base.w := floaterLabel.RealWidth;           //make something as "keep scale"? or override dlabel.draw? (NO, animations!)
  floaterLabel.base.h := floaterLabel.RealHeight;
  floaterLabel.base.Opacity := sin(Pi*Floater.Phase);
  floaterLabel.draw;

  if ((now-Last_render_time)*24*60*60>=1) then begin
    FPS_label.text := Inttostr(FPS_count);
    FPS_count := 0;
    Last_Render_time:=now;
  end else inc(FPS_count);
  FPS_label.base.Opacity := 1;
  FPS_label.draw;
end;

end.

