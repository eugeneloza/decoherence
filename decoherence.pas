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

     {needed to use load image}
     CastleControls, DecoLoadEmbedded,

     DecoThread, DecoThrash,

     DecoGui, DecoInterface, DecoMouse, DecoFont,
     DecoLevel, DecoAbstractWorld,
     DecoSound,
     DecoLoadScreen, DecoPerks,
     DecoInterfaceComposite,
     DecoPlayerCharacter, DecoLoad3d,
     DecoNavigation, DecoGlobal, DecoGamemode;

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
  s := DateTimeToAtStr(Now);
  Result := '';
  for i := 1 to Length(s) do
    if Copy(s,i,1) = ' ' then Result += '_' else
    if Copy(s,i,1) = ':' then Result += '-' else
    Result += Copy(s,i,1);
end;

{-------------------------------------------------------------------------}

var LastRender: DTime = -1;
procedure ProcessTimeEvents;
var TimePassed: DTime;
begin
  DecoNow := Now;
  If LastRender = -1 then LastRender := DecoNow;
  TimePassed := DecoNow - LastRender;
  {if not softpause...}
  DecoNowLocal := DecoNowLocal + TimePassed;
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
  if Party <> nil then begin
    Party.Manage;
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
       K_r: party.char[0].hit(1,1);
       k_i: if AmbientIntensity.Ambient = 0 then
               AmbientIntensity.SetAmbientIntensity(3)
            else
               AmbientIntensity.SetAmbientIntensity(0);
       {k_1: shaders.WhichChoice := 0;
       k_2: shaders.WhichChoice := 1;
       k_3: shaders.WhichChoice := 2;}

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
  end;
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
var i: integer;
    tmpLink: DAbstractElement;
    Dragging: boolean;
begin
  {check for drag-n-drops}
  Dragging := false;
  {if Event.EventType = itMouseButton then }begin
    if touchArray.count > 0 then begin
     i:=0;
     repeat
       if TouchArray[i].FingerIndex = Event.FingerIndex then begin
         TouchArray[i].update(Event);
         if (TouchArray[i].ClickElement<>nil) and (TouchArray[i].ClickElement.CanDrag) then begin
           TouchArray[i].ClickElement.Drag(Round(event.Position[0]),Round(Event.Position[1]));
           Dragging := true;
         end;
         Break;
       end;
       inc(i);
     until (i >= TouchArray.Count);
    end;

  end;
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
  window.OnPress := @doPress;
  window.onRelease := @doRelease;
  window.OnMotion := @doMotion;
  application.LimitFPS := 60;

  WritelnLog('ApplicationInitialize','Initialize fonts');
  InitializeFonts;      //load fonts
  //InitGlobal;           //start random

  //create GUI
  WritelnLog('ApplicationInitialize','Create interface');
  GUI := DInterfaceContainer.create(Window);
  GUI.rescale;

  WritelnLog('ApplicationInitialize','Initialize interface');
  InitLoadScreen;

  //finally (fonts, random and facts loaded), we're ready to show game loading screen
  {$IFDEF AllowRescale}window.OnResize := @WindowResize;{$ENDIF}
  window.OnRender := @WindowRender;

  WritelnLog('ApplicationInitialize','Init finished');

  {$WARNING BUUUUUUUUUUUUUUUUUG!!!!!}
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

{thanks to Michalis, it's simple :) see https://github.com/eugeneloza/decoherence/issues/22}
procedure SetLoadingImage;
begin
  {no need yet}
  //Theme.LoadingBackgroundColor := Black; // adjust as needed
  //Theme.LoadingTextColor := White; // adjust as needed

  Theme.Images[tiLoading] := Loading_image;
  Theme.OwnsImages[tiLoading] := false;
end;


function MyGetApplicationName: string;
begin
  Result  :=  'Decoherence 1';
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
Initialization
  OnGetApplicationName  :=  @MyGetApplicationName;

  SetLoadingImage;

  Window := TCastleWindowTouch.create(Application);

  window.DoubleBuffer := false;//true;             //what's the difference? speed? memory?

  {$IFNDEF AllowRescale}window.ResizeAllowed := raOnlyAtOpen;{$ENDIF}
  {$IFDEF Fullscreen}
    window.fullscreen := true;
  {$ELSE}
    window.width := 1024;
    window.height := 600;
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
  //FreeTextureProperties;
  WriteLnLog('Finalization','Bye...');
end.

