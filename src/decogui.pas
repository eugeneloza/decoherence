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

uses Classes, CastleVectors,
  DecoInterfaceCore, DecoLabels, DecoPlayerCharacter,
  DecoGlobal;

type
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
    Width, Height: integer;
    Center: TVector2;
    {Initialize and show animated loadscreen}
    procedure LoadScreen;
    {Initialize and show Party interface}
    procedure PartyInterface;
  end;

var
  GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils,
  CastleGLUtils, CastleColors,
  DecoInterfaceComposite, DecoInterfaceBlocks,
  DecoInterfaceLoader,
  DecoGameMode, DecoTime, DecoLog, Profiler;

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.Create;
begin
  {StartProfiler}

  Log(LogInitInterface, _CurrentRoutine, 'Creating interface.');
  inherited Create;

  Base.AnchorToWindow := True;

  FPSLabel := DFPSLAbel.Create;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

destructor DInterfaceContainer.Destroy;
begin
  {StartProfiler}

  Log(LogInitInterface, _CurrentRoutine, 'Game over...');
  { Free special elements that are not freed automatically (are not Children) }
  FreeAndNil(FPSLabel);
  inherited Destroy;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.Rescale;
begin
  {StartProfiler}

  if (Window.Width = Width) and (Window.Height = Height) then
  begin
    Log(LogInterfaceInfo, _CurrentRoutine, 'No need in rescale, abort.');
    Exit;
  end;

  Log(LogInterfaceInfo, _CurrentRoutine,
    IntToStr(Window.Width) + 'x' + IntToStr(Window.Height));

  Width := Window.Width;
  Height := Window.Height;
  Center := Vector2(Width div 2, Height div 2);

  {$WARNING no rescaling children. This can go wild!}
  //inherited Rescale;
  Self.RescaleRecoursive;


  { THIS ROUTINE IS NOT YET IMPLEMENTED IN CASTLE GAME ENGINE
    see https://github.com/castle-engine/castle-engine/issues/36 }
  //prevent Window from scaling too small and/or into portrait orientation
  {if window.width < window.height then begin
    dLog('DInterfaceContainer.rescale','ERROR: Only landscape orientation supported!');
    window.width := window.height+10;
  end;}

  { rescale special elements }
  FPSLabel.Rescale;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.Draw;
begin
  {StartProfiler}

  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
  if GameModeNeedsClearingScreen then
    RenderContext.Clear([cbColor], Black);

  inherited Draw;

  { draw special elements }
  FPSLabel.CountFPS;

  {StopProfiler}
end;

{===========================================================================}
{==================== INTERFACE MODES ======================================}
{===========================================================================}

procedure DInterfaceContainer.LoadScreen;
begin
  {StartProfiler}

  Self.Clear;
  Grab(DLoadScreen.Create);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.PartyInterface;
var
  tmp: DSingleInterfaceElement;
begin
  {StartProfiler}

  Self.Clear; //make it more optimal?
  {tmp := DPlayerBarsFull.Create;
  tmp.Base.AnchorToWindow := true;
  tmp.SetBaseSize(0.1,0.1,0.1,0.1);
  (tmp as DPlayerBarsFull).Target := Player.CurrentParty.Character[0];
  (tmp as DPlayerBarsFull).RearrangeChildren;
  Grab(tmp);
  tmp.RescaleRecoursive;}

 { tmp := DFloatLabel.Create;
  (tmp as DFloatLabel).Target := @base.fx1;
  grab(tmp); }

 { tmp := DFramedImage.Create;
  tmp.Base.AnchorToWindow := true;
  //(tmp as DFramedImage).Frame := BlackFrame; //<-------- here it doesn't work
  tmp.SetBaseSize(0.2,0.1,0.1,0.1);
  (tmp as DFramedImage).Frame := DecorationFrame2_Left;
  (tmp as DFramedImage).Image.Load(Portrait_Img[0]);
  //(tmp as DFramedImage).RearrangeChildren; //<------- doesn't matter
  Grab(tmp);                   }

  {StopProfiler}
end;

end.
