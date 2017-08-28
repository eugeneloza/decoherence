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

{ A highets-level container for the interface and its routines }
unit DecoGui;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  DecoInterface, DecoImages, DecoLabels, DecoFont,
  DecoGlobal;

Type
  DInterfaceContainer = class(DInterfaceElement)
  private
    FPSLabel: DFPSLAbel;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Rescale; override;
    procedure Draw; override;

{    procedure FreeLoadScreen(freeWind: boolean);
  private
    { wind images to provide background }
    Wind1,Wind2: DWindImage;
    Floater: DFloatImage;
    Background: DStaticImage;
    LoadScreenLabel, FloaterLabel: DLabel;
    procedure DoLoadNewImage;
    procedure LoadWind;

    { draw loadscreen elements }
    procedure DrawLoadScreen;
    { draw CharacterGeneration background elements }
    procedure DrawCharacterGenerationBackground;
    procedure DrawWind;
  public
    procedure MakeCharacterGenerationInterface; }
end;

var GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog,
  CastleGLUtils, CastleColors,
  {DecoLoadScreen,}
  DecoInterfaceComposite, DecoInterfaceBlocks,
  {DecoPlayerCharacter,}
  DecoGameMode, DecoTime;

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.Create;
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited Create;

  Base.ScaleToWindow := true;

  FPSLabel := DFPSLAbel.Create;
end;

{-----------------------------------------------------------------------------}

{procedure DInterfaceContainer.LoadWind;
begin
  Wind1 := DWindImage.create(self);
  Wind1.phasespeed := 1/(15+drnd.Random);
  Wind1.Load(WindFolder+'WindClouds1_GIMP.jpg');
  Wind1.Opacity:=0.1;
  Wind1.base.setsize(0,0,fullwidth,fullheight);
  Wind1.rescale;
  Wind2 := DWindImage.create(self);
  Wind2.phasespeed := 1/(10+drnd.Random);
  Wind2.Load(WindFolder+'WindClouds2_GIMP.jpg');
  Wind2.base.setsize(0,0,fullwidth,fullheight);
  Wind2.Opacity := 0.1;
  Wind2.rescale;
end;  }

{-----------------------------------------------------------------------------}

destructor DInterfaceContainer.Destroy;
begin
  WriteLnLog('DInterfaceContainer.destroy','Game over...');
  { Free special elements that are not freed automatically (are not Children) }
  FreeAndNil(FPSLabel);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.Rescale;
begin
  writeLnLog('DInterfaceContainer.rescale',inttostr(window.Width)+'x'+inttostr(window.Height));
  inherited Rescale;

  { THIS ROUTINE IS NOT YET IMPLEMENTED IN CASTLE GAME ENGINE
    see https://github.com/castle-engine/castle-engine/issues/36 }
{ //prevent Window from scaling too small and/or into portrait orientation
  if window.width < window.height then begin
    writeLnLog('DInterfaceContainer.rescale','ERROR: Only landscape orientation supported!');
    window.width := window.height+10;
  end;
  //rescale "base" for some routines to work correctly}
  //base.setsize(0,0,fullwidth,fullheight);

  {
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
  }

  { rescale special elements }
  if FPSLabel <> nil then FPSLabel.Rescale;
end;

{-----------------------------------------------------------------------------}

{procedure DInterfaceContainer.DoLoadNewImage;
begin
  if LoadScreenLabel=nil then begin
    LoadScreenLabel := DLabel.create(self);
    LoadScreenLabel.setbasesize(1/17,-2/17,10/17,10/17,1,asNone);
    LoadScreenLabel.Shadow := 1;
    LoadScreenLabel.Font := LoadScreenFont;
  end;
  LoadScreenLabel.text := LoadScreenMainText;

  if floaterLabel = nil then begin
    FloaterLabel := DLabel.create(self);
    floaterLabel.Font := LoadScreenFont;
    floaterLabel.Shadow := 1;
  end;
  floaterLabel.setbasesize(-11/17,1/17,10/17,10/17,0,asNone); //need to reset it each new fact, because w is reset to realwidth after text initialize
  floaterLabel.text := GetRandomFact;

  if floater = nil then floater := DFloatImage.create(self);
  floater.FreeImage;
  LoadNewFloaterImage := false;
  floater.opacity := 0.8;
  floater.phasespeed := 1/15;
  floater.base.setsize(0,0,proportionalscale,fullheight);
  floater.LoadThread(LoadScreenFolder+GetRandomFactImage);
end; }

{-----------------------------------------------------------------------------}

{procedure DInterfaceContainer.FreeLoadScreen(freeWind: boolean);
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
end; }

{==================== Drawing routines ========================================}

{procedure DInterfaceContainer.DrawWind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if wind1 = nil then LoadWind;
  wind1.draw;
  wind2.draw;
end;}

{-----------------------------------------------------------------------------}

{procedure DInterfaceContainer.DrawLoadScreen;
begin
  if (floater = nil) or (LoadNewFloaterImage) then DoLoadNewImage;
  floater.Draw;

  DrawWind;

  LoadScreenLabel.Draw;

  floaterLabel.base.y1 := round((1 + 5*Floater.phase)*Window.height/17);
  floaterLabel.base.Opacity := sin(Pi*Floater.Phase);
  floaterLabel.draw;
end; }

{-----------------------------------------------------------------------------}

{procedure DInterfaceContainer.DrawCharacterGenerationBackground;
begin
  if background = nil then begin
    background := DStaticImage.create(self);
    background.LoadThread(BackgroundsFolder+'spaceship-1548838_1280_CC0_by_JAKO5D_[gmic].jpg');
    background.setbasesize(0,0,fullwidth,fullheight,1,asNone);
  end;
  background.draw;

  DrawWind;
end;}

{------------------------------------------------------------------------------}

procedure DInterfaceContainer.Draw;
begin
  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
  if GameModeNeedsClearingScreen then RenderContext.Clear([cbColor], Black);
  inherited Draw;

  //some drawing for specific gamemodes
  // if CurrentGameMode = gmCharacterScreen then DrawCharacterScreenBackground
{  if CurrentGameMode = gmCharacterGeneration then DrawCharacterGenerationBackground else
  if CurrentGameMode = gmLoadScreen then DrawLoadScreen;}

  { draw special elements }
  FPSLabel.CountFPS;
end;

{======================== Interface modes creation ===========================}

{var interfaceReady: boolean = false; //UGLY FIX THIS AT ONCE!!!!
procedure DInterfaceContainer.MakeCharacterGenerationInterface;
var
  tmp: DPartyView;
  tmp2: DDecorations;
begin
  if interfaceReady then exit;
  interfaceReady := true;
  tmp := DPartyView.create(self);
  tmp2 := DDecorations.create(self);
  GUI.grab(tmp2);
  GUI.grab(tmp);
end;}

end.

