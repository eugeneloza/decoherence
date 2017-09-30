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

uses Classes,{$IFDEF AllowRescale}CastleWindow,{$ENDIF}
  DecoInterface, DecoLabels, DecoPlayerCharacter,
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
    Width,Height: integer;
    {}
    procedure LoadScreen;
    {}
    procedure PartyInterface;
end;

var GUI: DInterfaceContainer;

{$IFDEF AllowRescale}
Procedure WindowResize(Container: TUIContainer);
{$ENDIF}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils,
  CastleGLUtils, CastleColors,
  DecoInterfaceComposite, DecoInterfaceBlocks,
  DecoInterfaceLoader,
  DecoGameMode, DecoTime, DecoLog;

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.Create;
begin
  dLog(LogInitInterface,Self,'DInterfaceContainer.Create','Creating interface.');
  inherited Create;

  Base.AnchorToWindow := true;

  FPSLabel := DFPSLAbel.Create;
end;

{-----------------------------------------------------------------------------}

destructor DInterfaceContainer.Destroy;
begin
  dLog(LogInitInterface,Self,'DInterfaceContainer.Destroy','Game over...');
  { Free special elements that are not freed automatically (are not Children) }
  FreeAndNil(FPSLabel);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}
{$IFDEF AllowRescale}
{ this procedure is mostly needed for Desktops in windowed mode
  and in normal situations should be called only once }
{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
Procedure WindowResize(Container: TUIContainer);
begin
  if (Window.Width<>GUI.Width) or (Window.Height<>GUI.Height) then begin
    GUI.Rescale;
  end;
end;
{$POP}
{$ENDIF}
{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.Rescale;
begin
  dLog(LogInterfaceInfo,Self,'DInterfaceContainer.Rescale',
    IntToStr(Window.Width) + 'x' + IntToStr(Window.Height));
  inherited Rescale;
  Width := Window.Width;
  Height := Window.Height;

  { THIS ROUTINE IS NOT YET IMPLEMENTED IN CASTLE GAME ENGINE
    see https://github.com/castle-engine/castle-engine/issues/36 }
  //prevent Window from scaling too small and/or into portrait orientation
  {if window.width < window.height then begin
    dLog('DInterfaceContainer.rescale','ERROR: Only landscape orientation supported!');
    window.width := window.height+10;
  end;}

  { rescale special elements }
  FPSLabel.Rescale;
end;

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

{===========================================================================}
{==================== INTERFACE MODES ======================================}
{===========================================================================}

procedure DInterfaceContainer.LoadScreen;
begin
  Self.Clear;
  Grab(DLoadScreen.Create);
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.PartyInterface;
var tmp: DPlayerBarsFull;
begin
  Self.Clear; //make it more optimal?
  tmp := DPlayerBarsFull.Create;
  tmp.Base.AnchorToWindow := true;
  tmp.SetBaseSize(0,0,0.1,0.1);
  tmp.Target := Player.CurrentParty.Character[0];
  Grab(tmp);
end;

end.

