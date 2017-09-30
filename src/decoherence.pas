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
     CastleWindow, CastleWindowTouch, CastleKeysMouse,

     CastleScene,

     DecoThread, DecoThrash,

     DecoGui, DecoInterface, DecoInput,
     DecoLevel, DecoAbstractWorld,
     DecoSound,
     DecoLoadScreen, DecoPerks, DecoActorBody,
     DecoInterfaceLoader,
     DecoPlayerCharacter, DecoLoad3d,
     DecoNavigation,
     DecoGlobal, DecoTranslation, DecoGamemode, DecoTime, DecoLog;


{==========================================================================}
{==========================================================================}

{ this is a management procedure that takes place before
  WindowRender }
{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure WindowManage(Container: TUIContainer);
begin
  doTime; {advance time for this frame}

  if Player <> nil then begin
    Player.Manage;
    if CurrentWorld <> nil then CurrentWorld.Manage(Camera.Position);
  end;

  if Music <> nil then Music.Manage;
end;
{$POP}


{-------------------------------------------------------------------------}



{======================== Mouse & keyboard =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // todo Joystick
  if Event.EventType = itMouseButton then begin
    doMousePress(Event);
    {todo: if interface didn't catch the click then}
    if CurrentGameMode = gmTravel then
      if mbRight = Event.MouseButton then Camera.MouseLook := not Camera.MouseLook;

  end else if Event.EventType = itKey then begin
    case Event.key of
       K_P,K_PrintScreen:                //k_printscreen doesn't work in x-window system if assigned to some external program like scrot
                         Window.SaveScreen('deco_'+NiceDate+'.jpg');
       K_r: Player.CurrentParty.Rest;
       k_i: if AmbientIntensity.Ambient = 0 then
               AmbientIntensity.SetAmbientIntensity(3)
            else
               AmbientIntensity.SetAmbientIntensity(0);
       {k_1: shaders.WhichChoice := 0;
       k_2: shaders.WhichChoice := 1;
       k_3: shaders.WhichChoice := 2;}

    end;

    if (CurrentGameMode=gmTravel) and (Player<>nil) then begin
     case Event.key of
        k_W: Player.InputMove(mdForward);
        k_S: Player.InputMove(mdBack);
        k_A: Player.InputMove(mdLeft);
        k_D: Player.InputMove(mdRight);
     end;
    end;
  end;
//  SetGameMode(gmCharacterGeneration);
  InitTestLevel;                         //ugly! I'll fix this soon.
end;
{$POP}

{--------------------------------------------------------------------------}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then begin
    doMouseRelease(Event);
  end else
  if Event.EventType = itKey then begin
    case Event.key of
      k_W: Player.InputRelease(mdForward);
      k_S: Player.InputRelease(mdBack);
      k_A: Player.InputRelease(mdLeft);
      k_D: Player.InputRelease(mdRight);
    end;
  end;
end;
{$POP}

{--------------------------------------------------------------------------}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
var
    tmpLink: DAbstractElement;
    Dragging: boolean;
begin
  if doMouseLook(Event) then Exit;

  Dragging := doMouseDrag(Event);

  {mouse over / if no drag-n-drop}
  //this is not needed at the moment, we'll turn here a bit later when implementing drag-n-drop
  //no mouseover is detected if no ifmouseover is run, so should still be here
  if not Dragging then begin
    tmpLink := GUI.IfMouseOver(Round(Event.Position[0]),Round(Event.Position[1]),true,true);
    if tmpLink <> nil then
      dLog(logVerbose,nil,'doMotion','Motion caught '+tmpLink.ClassName);
  end;

end;
{$POP}

{======================= initialization routines ==============================}

procedure LoadAndInitData;
begin
  InitMusicManager;
  SetGameMode(gmLoadScreen);

  InitPerks;

  LoadTestLevel; //remake it

  //Assign window events
  Window.OnBeforeRender := @WindowManage;
  Window.OnPress := @doPress;
  Window.onRelease := @doRelease;
  Window.OnMotion := @doMotion;

  LoadCompleted := true;
end;

type
  DLoadGameThread = class(TAbstractThread)
  protected
    procedure Execute; override;
  end;

var LoadThread: DLoadGameThread;

procedure DLoadGameThread.execute;
begin
  LoadAndInitData;
end;

procedure ApplicationInitialize;
begin
  InitLog;
  dLog(LogInit,nil,'ApplicationInitialize','Init');

  //Application.LimitFPS := 60;

  //create GUI
  dLog(LogInit,nil,'ApplicationInitialize','Create interface');
  InitInterface;
  InitLoadScreen;
  GUI := DInterfaceContainer.Create;
  GUI.Rescale;

  GUI.LoadScreen;

  dLog(LogInit,nil,'ApplicationInitialize','Initialize interface');

  //finally we're ready to show game loading screen
  {$IFDEF AllowRescale}Window.onResize := @GuiResize;{$ENDIF}
  Window.onRender := @GuiRender;

  dLog(LogInit,nil,'ApplicationInitialize','Init finished');

  LoadThread := DLoadGameThread.Create(true);
  {$WARNING BUUUUUUUUUUUUUUUUUG!!!!!}

  {$DEFINE NoThreads}
  {$IFDEF Linux}{$IFNDEF RELEASE}{$DEFINE NoThreads}{$ENDIF}{$ENDIF}

  {$IFNDEF NoThreads}
  LoadThread.Priority := tpNormal;
  LoadThread.FreeOnTerminate := true;
  LoadThread.Start;
  {$ELSE}
  LoadThread.FreeOnTerminate := false;
  LoadThread.Execute;
  FreeAndNil(LoadThread);
  {$ENDIF}
end;

{==========================================================================}

function MyGetApplicationName: string;
begin
  Result  :=  'Decoherence 1';
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
Initialization
  OnGetApplicationName  :=  @MyGetApplicationName;

  SetLoadingImage;

  Window := TCastleWindowTouch.Create(Application);

  Window.DoubleBuffer := true;//true;             //what's the difference? speed? memory?

  {$IFNDEF AllowRescale}window.ResizeAllowed := raOnlyAtOpen;{$ENDIF}
  {$IFDEF Fullscreen}
    Window.FullScreen := true;
  {$ELSE}
    Window.Width := 1024;
    Window.Height := 600;
  {$ENDIF}

  Application.MainWindow  :=  Window;
  Application.OnInitialize  :=  @ApplicationInitialize;

Finalization
  dLog(LogInit,nil,'Finalization','Started...');
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
  dLog(LogInit,nil,'Finalization','Bye...');
end.

