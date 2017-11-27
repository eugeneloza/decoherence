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

{ Core file of the game }
unit Decoherence;

{$INCLUDE compilerconfig.inc}

interface

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses Classes, SysUtils,
  CastleWindow, {CastleWindowTouch,} CastleKeysMouse,
  CastleScene,
  DecoThrash,

  {---- temporary units (for testing) -----}
  DecoSound,
  DecoContext,
  DecoFile,
  DecoPerks,
  DecoActorBody,
  DecoLoad3d,
  DecoNavigation, {should be in DecoPlayerCharacter}
  DecoLoadScreen, {should be in DecoInterfaceLoader}
  {---- end temporary units here ----------}

  DecoGui,
  DecoLevel, DecoAbstractWorld,
  DecoInterfaceLoader,
  DecoPlayerCharacter, DecoInput,
  DecoGlobal, DecoTranslation, DecoGamemode, DecoTime, DecoLog, Profiler;

{==========================================================================}

{ this is a management procedure that takes place before
  WindowRender }
{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure WindowManage(Container: TUIContainer);
begin
  {StartProfiler}

  doTime; {advance time for this frame}

  if Player <> nil then
  begin
    Player.Manage;
    if CurrentWorld <> nil then
      CurrentWorld.Manage(Camera.Position);
  end;

  if Music <> nil then
    Music.Manage;

  {StopProfiler}
end;

{$POP}

{==========================================================================}

{$IFDEF AllowRescale}
{ this procedure is mostly needed for Desktops in windowed mode
  and in normal situations should be called only once }
{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure GuiResize(Container: TUIContainer);
begin
  {StartProfiler}

  GUI.Rescale;

  {StopProfiler}
end;

{$POP}
{$ENDIF}

{-------------------------------------------------------------------------}

{ generic rendering procedure. 3D world is rendered automatically
  on each Window.Render, so we just need to add a GUI render here }
{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure GuiRender(Container: TUIContainer);
begin
  {StartProfiler}

  GUI.Draw;

  {StopProfiler}
end;

{$POP}

{======================== Mouse & keyboard =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  {StartProfiler}

  // todo Joystick
  if Event.EventType = itMouseButton then
    doMousePress(Event)
  else
  if Event.EventType = itKey then
  begin
    {some generic buttons here}
    case Event.key of
      K_P, K_PrintScreen:
        //k_printscreen doesn't work in x-window system if assigned to some external program like scrot
        Window.SaveScreen('deco_' + NiceDate + '.jpg');
      K_r: Player.CurrentParty.Rest;
      k_i: if AmbientIntensity.Ambient = 0 then
          AmbientIntensity.SetAmbientIntensity(3)
        else
          AmbientIntensity.SetAmbientIntensity(0);
       {k_1: shaders.WhichChoice := 0;
       k_2: shaders.WhichChoice := 1;
       k_3: shaders.WhichChoice := 2;}

    end;

    if (CurrentGameMode = gmTravel) and (Player <> nil) then
      doKeyboardPress(Event.Key);
  end;
  //  SetGameMode(gmCharacterGeneration);
  InitTestLevel;                         //ugly! I'll fix this soon.

  {StopProfiler}
end;

{$POP}

{--------------------------------------------------------------------------}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  {StartProfiler}

  if Event.EventType = itMouseButton then
    doMouseRelease(Event)
  else
  if Event.EventType = itKey then
    doKeyboardRelease(Event.Key);

  {StopProfiler}
end;

{$POP}

{--------------------------------------------------------------------------}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  {StartProfiler}

  doMouseMotion(Event);

  {StopProfiler}
end;

{$POP}

{======================= initialization routines ==============================}

procedure LoadAndInitData;
begin
  {StartProfiler}

  InitMusicManager;
  SetGameMode(gmLoadScreen);

  InitPerks;

  LoadTestLevel; //remake it

  //Assign window events
  Window.onBeforeRender := @WindowManage;
  Window.onPress := @doPress;
  Window.onRelease := @doRelease;
  Window.onMotion := @doMotion;

  LoadCompleted := True;

  {StopProfiler}
end;

procedure ApplicationInitialize;
begin
  {StartProfiler}

  InitLog;
  Log(LogInit, _CurrentRoutine, 'Init');

  //Application.LimitFPS := 60;

  //create GUI
  Log(LogInit, _CurrentRoutine, 'Create interface');
  //fLog(true,BackTraceStrFunc(Get_Frame),'');
  InitInterface;
  InitLoadScreen;
  GUI := DInterfaceContainer.Create;
  GUI.Rescale;

  GUI.LoadScreen;

  Log(LogInit, _CurrentRoutine, 'Initialize interface');

  //finally we're ready to show game loading screen
  {$IFDEF AllowRescale}
  Window.onResize := @GuiResize;
  {$ENDIF}
  Window.onRender := @GuiRender;

  Log(LogInit, _CurrentRoutine, 'Init finished');

  LoadAndInitData;

  {StopProfiler}
end;

{==========================================================================}

function MyGetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
initialization
  {StartProfiler}

  OnGetApplicationName := @MyGetApplicationName;

  SetLoadingImage;

  Window := TCastleWindow.Create(Application);

  Window.DoubleBuffer := True;

  {$IFNDEF AllowRescale}
  window.ResizeAllowed := raOnlyAtOpen;
  {$ENDIF}
  {$IFDEF Fullscreen}
  Window.FullScreen := True;
  {$ELSE}
  Window.Width := 1024;
  Window.Height := 600;
  {$ENDIF}

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;

  {StopProfiler}

finalization
  {StartProfiler}

  Log(LogInit, _CurrentRoutine, 'Going down...');
  { free all assigned memory }
  FreeAndNil(GUI);

  //DestroyGlobal;
  FreeLoadScreen;
  FreePerks;
  FreeWorld;
  FreeMusicManager;
  FreePlayer;
  FreeCreatures;
  FreeInterface;
  //FreeTextureProperties;
  Log(LogInit, _CurrentRoutine, 'Bye...');

  {StopProfiler}
end.
