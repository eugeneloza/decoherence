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
  decointerface, decoimages,
  decoglobal;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { just = window.height, wihdow.width. Maybe I'll deprecate it later }
    width,height:integer;
    { random generator used for all interface random events }
    rnd: TCastleRandom;
    { wind images to provide background }
    wind1,wind2: DWindImage;

    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Rescale; override;
    procedure draw; override;
    procedure loadwind;
end;

var GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog;


{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited create(AOwner);
  rnd := TCastleRandom.Create;
  LoadWind;
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
  inherited;
end;

procedure DInterfaceContainer.rescale;
begin
  writeLnLog('DInterfaceContainer.rescale',inttostr(window.Width)+'x'+inttostr(window.Height));
  width := window.Width;
  height := window.Height;
  wind1.rescale;
  wind2.rescale;
  inherited;
end;

procedure DInterfaceContainer.Draw;
begin
  wind1.draw;
  wind2.draw;
end;

end.

