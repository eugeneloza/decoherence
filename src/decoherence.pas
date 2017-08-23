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

const Version={$INCLUDE version.inc};

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses Classes, SysUtils,
     CastleLog, CastleTimeUtils,
     CastleWindow, CastleWindowTouch, CastleKeysMouse,

     CastleScene,

     DecoThread, DecoThrash,

     DecoGui, DecoInterface, DecoMouse, DecoFont,
     DecoLevel, DecoAbstractWorld,
     DecoSound,
     DecoLoadScreen, DecoPerks, DecoBody,
     DecoInterfaceComposite,
     DecoPlayerCharacter, DecoLoad3d,
     DecoNavigation, DecoGlobal, DecoTranslation, DecoGamemode, DecoTime;

type
  DLoadThread = class(TAbstractThread)
  protected
    procedure Execute; override;
  end;

var LoadThread: DLoadThread;

{==========================================================================}
{==========================================================================}

{$IFDEF AllowRescale}
{ this procedure is mostly needed for Desktops in windowed mode
  and in normal situations should be called only once }
Procedure WindowResize(Container : TUIContainer);
begin
  if (window.width<>GUI.width) or (window.height<>GUI.height) then begin
    GUI.rescale;
  end;
end;
{$ENDIF}

function NiceDate: string;
var s: string;
    i: integer;
begin
  s := DateTimeToAtStr(Now); //only place where I'm using SysUtils.Now
  Result := '';
  for i := 1 to Length(s) do
    if Copy(s,i,1) = ' ' then Result += '_' else
    if Copy(s,i,1) = ':' then Result += '-' else
    Result += Copy(s,i,1);
end;

{-------------------------------------------------------------------------}

var LastRender: DTime = -1;
procedure ProcessTimeEvents;
begin
  DecoNow := GetNow;
  If LastRender = -1 then LastRender := DecoNow;
  DeltaT := DecoNow - LastRender;

  if SoftPause < deltaT then begin
    SoftPause := -1;
    DeltaTLocal := DeltaT;
    DecoNowLocal := DecoNowLocal + DeltaTLocal;
  end else begin
    SoftPause -= DeltaT;
    DeltaTLocal := 0;
  end;
  LastRender := DecoNow;

  Case CurrentGameMode of
    { time flows normally in travel mode }
    gmTravel: {TimeFlow(TimePassed)};
    { time stops at softpause }
    gmBattle: begin
{                  If not pause then TimeFlow(TimePassed);
                If not softPause then animations.freeze!}
              end;
    else {NOP};//no time flow;
  end;
end;

{-------------------------------------------------------------------------}

{ this is a management procedure that takes place before
  WindowRender }
procedure WindowManage(Container : TUIContainer);
begin
  if CurrentParty <> nil then begin
    CurrentParty.Manage;
    if CurrentWorld <> nil then CurrentWorld.Manage(Camera.Position);
  end;

  if Music <> nil then Music.Manage;
  ProcessTimeEvents;
end;

{-------------------------------------------------------------------------}

{ generic rendering procedure. 3D world is rendered automatically
  on each Window.Render, so we just need to add a GUI render here }
Procedure WindowRender(Container : TUIContainer);
begin
  GUI.draw;
end;

{======================== Mouse & keyboard =================================}

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
       K_y: CurrentParty.Char[0].Hit(1,1);
       K_r: CurrentParty.Rest;
       k_i: if AmbientIntensity.Ambient = 0 then
               AmbientIntensity.SetAmbientIntensity(3)
            else
               AmbientIntensity.SetAmbientIntensity(0);
       {k_1: shaders.WhichChoice := 0;
       k_2: shaders.WhichChoice := 1;
       k_3: shaders.WhichChoice := 2;}

    end;

    if (CurrentGameMode=gmTravel) and (CurrentParty<>nil) then begin
     case Event.key of
        k_W: CurrentParty.InputMove(mdForward);
        k_S: CurrentParty.InputMove(mdBack);
        k_A: CurrentParty.InputMove(mdLeft);
        k_D: CurrentParty.InputMove(mdRight);
     end;
    end;
  end;
//  SetGameMode(gmCharacterGeneration);
  InitTestLevel;                         //ugly! I'll fix this soon.
end;

{--------------------------------------------------------------------------}

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then begin
    doMouseRelease(Event);
  end else
  if Event.EventType = itKey then begin
    case Event.key of
      k_W: CurrentParty.InputRelease(mdForward);
      k_S: CurrentParty.InputRelease(mdBack);
      k_A: CurrentParty.InputRelease(mdLeft);
      k_D: CurrentParty.InputRelease(mdRight);
    end;
  end;
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
var
    tmpLink: DAbstractElement;
    Dragging: boolean;
begin
  if doMouseLook then begin
    WriteLnLog('doMotion>MouseLook','Call-back encountered...');
    Exit;
  end;

  Dragging := doMouseDrag(Event);

  {mouse over / if no drag-n-drop}
  //this is not needed at the moment, we'll turn here a bit later when implementing drag-n-drop
  //no mouseover is detected if no ifmouseover is run, so should still be here
  if not Dragging then begin
    tmpLink := GUI.IfMouseOver(Round(Event.Position[0]),Round(Event.Position[1]),true,true);
    if tmpLink <> nil then
      WriteLnLog('doMotion','Motion caught '+tmpLink.ClassName);
  end;

end;

{======================= initialization routines ==============================}

procedure LoadAndInitData;
begin
  InitMusicManager;
  SetGameMode(gmLoadScreen);

  InitInterface;
  InitPerks;

  Load_test_level; //remake it
  window.OnBeforeRender := @WindowManage;

  LoadCompleted := true;
end;

procedure DLoadThread.execute;
begin
  LoadAndInitData;
end;

procedure ApplicationInitialize;
begin
  //initialize the log
  {$IFDEF Android}
  InitializeLog;
  {$ELSE}
    {$IFDEF WriteLog}
      LogStream := TFileStream.Create('log_'+NiceDate+'.txt',fmCreate);
      InitializeLog(Version,LogStream,ltTime);
    {$ELSE}
      InitializeLog(Version,nil,ltTime);
    {$ENDIF}
  {$ENDIF}
  WritelnLog('(i)','Compillation Date: ' + {$I %DATE%} + ' Time: ' + {$I %TIME%});
  WritelnLog('FullScreen mode',{$IFDEF Fullscreen}'ON'{$ELSE}'OFF'{$ENDIF});
  WritelnLog('Allow rescale',{$IFDEF AllowRescale}'ON'{$ELSE}'OFF'{$ENDIF});
  WritelnLog('ApplicationInitialize','Init');

  //Assign window events
  Window.OnPress := @doPress;
  Window.onRelease := @doRelease;
  Window.OnMotion := @doMotion;
  Application.LimitFPS := 60;

  WritelnLog('ApplicationInitialize','Initialize fonts');
  InitializeFonts;      //load fonts
  //InitGlobal;           //start random

  //create GUI
  WritelnLog('ApplicationInitialize','Create interface');
  GUI := DInterfaceContainer.create(Window);
  GUI.Rescale;

  WritelnLog('ApplicationInitialize','Initialize interface');
  InitLoadScreen;

  //finally (fonts, random and facts loaded), we're ready to show game loading screen
  {$IFDEF AllowRescale}window.OnResize := @WindowResize;{$ENDIF}
  window.OnRender := @WindowRender;

  WritelnLog('ApplicationInitialize','Init finished');

  {$WARNING BUUUUUUUUUUUUUUUUUG!!!!!}
  {$DEFINE NoThreads}
  {$IFDEF Linux}{$IFNDEF RELEASE}{$DEFINE NoThreads}{$ENDIF}{$ENDIF}
  {$IFNDEF NoThreads}
  LoadThread := DLoadThread.create(false);
  LoadThread.Priority := tpNormal;
  LoadThread.FreeOnTerminate := true;
  {$ELSE}
  LoadAndInitData;
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

  Window := TCastleWindowTouch.create(Application);

  Window.DoubleBuffer := true;//true;             //what's the difference? speed? memory?

  {$IFNDEF AllowRescale}window.ResizeAllowed := raOnlyAtOpen;{$ENDIF}
  {$IFDEF Fullscreen}
    Window.fullscreen := true;
  {$ELSE}
    Window.width := 1024;
    Window.height := 600;
  {$ENDIF}

  Application.MainWindow  :=  Window;
  Application.OnInitialize  :=  @ApplicationInitialize;

Finalization
  { free all assigned memory }

  DestroyCompositeInterface;
  //DestroyGlobal;
  FreeLoadScreen;
  FreePerks;
  DestroyFonts;
  FreeWorld;
  FreeMusicManager;
  FreeParty;
  FreeCreatures;
  //FreeTextureProperties;
  WriteLnLog('Finalization','Bye...');
end.

