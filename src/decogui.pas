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
  DecoInterface, DecoImages, DecoLabels, {DecoFont, }
  DecoGlobal;

Type
  { GUI container, manages all other GUI elements }
  DInterfaceContainer = class(DInterfaceElement)
  private
    { This label is separate from all the GUI and managed separately }
    FPSLabel: DFPSLAbel;
  public
    procedure Rescale; override;
    procedure Draw; override;
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure tmpInterface;
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

  Base.AnchorToWindow := true;

  FPSLabel := DFPSLAbel.Create;
end;

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

  { rescale special elements }
  if FPSLabel <> nil then FPSLabel.Rescale;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.tmpInterface;
var tmp: DInterfaceElement;
begin
  tmp := DWindElement.Create;
  Grab(tmp)
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

procedure DInterfaceContainer.Draw;
begin
  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
  if GameModeNeedsClearingScreen then RenderContext.Clear([cbColor], Black);
  inherited Draw;

  { draw special elements }
  FPSLabel.CountFPS;
end;

end.

