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

{---------------------------------------------------------------------------}

{ A highets-level container for the interface and its routines }
unit decogui;

{$INCLUDE compilerconfig.inc}

interface

uses classes, CastleRandom,
  decointerface, decoimages, decolabels,
  decofont,
  decoglobal;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { just = window.height, wihdow.width. Maybe I'll deprecate it later }
    width,height: integer;
    { random generator used for all interface random events }
    rnd: TCastleRandom;

    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Rescale; override;
    procedure draw; override;

    procedure FreeLoadScreen(freeWind: boolean);
  private
    { wind images to provide background }
    wind1,wind2: DWindImage;
    floater: DFloatImage;
    background: DStaticImage;
    LoadScreenLabel, FloaterLabel: DLabel;
    procedure DoLoadNewImage;
    procedure loadwind;

    { draw loadscreen elements }
    procedure DrawLoadScreen;
    { draw CharacterGeneration background elements }
    procedure DrawCharacterGenerationBackground;
    procedure DrawWind;
  strict private
    FPS_label: DLabel;
    FPS_count: Integer;
    Last_render_time: TDateTime;
  public
    procedure MakeCharacterGenerationInterface;
end;

var GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog,
  decofacts,
  decointerfacecomposite, decointerfaceblocks,
  decoplayercharacter,
  decogamemode;

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited create(AOwner);
  rnd := TCastleRandom.Create;

  Last_render_time := now;
  FPS_count := 0;
  FPS_Label := DLabel.create(self);
  FPS_Label.setbasesize(0,0,1,1,1,false);
  FPS_Label.Shadow := 0;
  FPS_Label.Font := RegularFont16;
  FPS_Label.text := 'x';

  width := -1;
  height := -1;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.LoadWind;
begin
  Wind1 := DWindImage.create(self);
  Wind1.phasespeed := 1/(15+rnd.Random);
  Wind1.Load(WindFolder+'WindClouds1_GIMP.jpg');
  Wind1.Opacity:=0.1;
  wind1.base.setsize(0,0,fullwidth,fullheight);
  Wind1.rescale;
  Wind2 := DWindImage.create(self);
  Wind2.phasespeed := 1/(10+rnd.Random);
  Wind2.Load(WindFolder+'WindClouds2_GIMP.jpg');
  wind2.base.setsize(0,0,fullwidth,fullheight);
  Wind2.Opacity := 0.1;
  Wind2.rescale;
end;

{-----------------------------------------------------------------------------}

destructor DInterfaceContainer.destroy;
begin
  writeLnLog('DInterfaceContainer.destroy','Game over...');
  freeandnil(rnd);
  DestroyFonts;
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.rescale;
begin
  writeLnLog('DInterfaceContainer.rescale',inttostr(window.Width)+'x'+inttostr(window.Height));
  //prevent Window from scaling too small and/or into portrait orientation
  if window.width < window.height then begin
    // BUG HERE! DOESN'T WORK AS EXPECTED (at least in Linux) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    writeLnLog('DInterfaceContainer.rescale','ERROR: Only landscape orientation supported!');
    window.width := window.height+10;
  end;
  width := window.Width;
  height := window.Height;
  //rescale "base" for some routines to work correctly
  base.setsize(0,0,fullwidth,fullheight);

  { rescale special elements }
  if fps_label <> nil then fps_label.rescale;

  if (CurrentGameMode=gmLoadScreen) or (CurrentGameMode=gmCharacterGeneration) then begin
    if wind1 <> nil then wind1.rescale;
    if wind2 <> nil then wind2.rescale;
  end;
  if CurrentGameMode=gmLoadScreen then begin
    if floater <> nil then floater.rescale;
    if floaterLabel <> nil then floaterlabel.rescale;
    if LoadScreenLabel <> nil then LoadScreenLabel.rescale;
  end;

  if background <> nil then background.rescale; //todo maybe just check show/hide

  { rescale burner image }
  Init_burner_image;

  { rescale children }
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.DoLoadNewImage;
begin
  if floater = nil then floater := DFloatImage.create(self);
  floater.FreeImage;
  LoadNewFloaterImage := false;
  floater.opacity := 0.8;
  floater.phasespeed := 1/15;
  floater.base.setsize(0,0,proportionalscale,fullheight);
  floater.LoadThread(LoadScreenFolder+GetRandomFactImage);

  if LoadScreenLabel=nil then begin
    LoadScreenLabel := DLabel.create(self);
    LoadScreenLabel.setbasesize(1,-2,10,10,1,false);
    LoadScreenLabel.Shadow := 1;
    LoadScreenLabel.Font := RegularFont16;
  end;
  LoadScreenLabel.text := 'Добро пожаловать в Decoherence :)'+dlinebreak+'Идёт загрузка, подождите...'+dlinebreak+'П.С. пока "почти нечего грузить" :)'+dlinebreak+'Просто нажмите любую клавишу...';

  if floaterLabel = nil then begin
    FloaterLabel := DLabel.create(self);
    floaterLabel.Font := RegularFont16;
    floaterLabel.Shadow := 1;
  end;
  floaterLabel.setbasesize(-11,1,10,10,0,false); //need to reset it each new fact, because w is reset to realwidth after text initialize
  floaterLabel.text := GetRandomFact;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.FreeLoadScreen(freeWind: boolean);
begin
  freeandnil(Floater);
  freeandnil(LoadScreenLabel);
  freeandnil(floaterLabel);
  if freeWind then begin
    freeandnil(wind1);
    freeandnil(wind2);
  end;
  {I'll just put it here to avoid making another procedure... as background
   is nil in LoadScreen and defined only in Charactergeneration.
   maybe I'll do it later}
  freeandnil(background);
end;

{==================== Drawing routines ========================================}

procedure DInterfaceContainer.DrawWind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if wind1 = nil then LoadWind;
  wind1.draw;
  wind2.draw;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.DrawLoadScreen;
begin
  if (floater = nil) or (LoadNewFloaterImage) then DoLoadNewImage;
  floater.draw;

  DrawWind;

  LoadScreenLabel.draw;

  floaterLabel.base.y1 := round((1 + 5*Floater.phase)*Window.height*GUI_scale_unit_float);
  floaterLabel.base.Opacity := sin(Pi*Floater.Phase);
  floaterLabel.draw;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.DrawCharacterGenerationBackground;
begin
  if background = nil then begin
    background := DStaticImage.create(self);
    background.LoadThread(BackgroundsFolder+'spaceship-1548838_1280_CC0_by_JAKO5D_[gmic].jpg');
    background.setbasesize(0,0,fullwidth,fullheight,1,false);
  end;
  background.draw;

  DrawWind;
end;

{------------------------------------------------------------------------------}

procedure DInterfaceContainer.Draw;
begin
  //some drawing for specific gamemodes
  // if CurrentGameMode = gmCharacterScreen then DrawCharacterScreenBackground
  if CurrentGameMode = gmCharacterGeneration then DrawCharacterGenerationBackground else
  if CurrentGameMode = gmLoadScreen then DrawLoadScreen;

  //draw the interface {children}
  inherited draw;

  //draw FPS label
  if ((now-Last_render_time)*24*60*60 >= 1) then begin
    FPS_label.text := Inttostr(FPS_count){+' '+inttostr(round(Window.Fps.RealTime))};
    FPS_count := 0;
    Last_Render_time:=now;
  end else inc(FPS_count);
  FPS_label.draw;
end;

{======================== Interface modes creation ===========================}

procedure DInterfaceContainer.MakeCharacterGenerationInterface;
var i: integer;
  tmp: DPartyView;
begin
  for i := 0 to maxparty do
    Party[i] := DPlayerCharacter.create(Window);
  tmp := DPartyView.create(self);
  GUI.children.add(tmp);
end;

end.

