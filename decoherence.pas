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

{ Core file of the game }

unit Decoherence;

{$INCLUDE compilerconfig.inc}
{$mode objfpc}{$H+}
//{$DEFINE WriteLog}{$IFDEF Windows}{$APPTYPE GUI}{$ENDIF}


interface

const Version='interfa3-161107-48';

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses Classes, SysUtils,
     CastleLog,
     CastleWindow, CastleWindowTouch, CastleKeysMouse,
     decogui, decointerface, decomouse, decofont,
     decolevel,
     decoactor, decointerfacecomposite,
     decoglobal, decogamemode;

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

var RenderFinished: boolean = true;
Procedure WindowRender(Container : TUIContainer);
begin
  //todo if renderfinished to make frameskip, but this might conflict with 3D world render
  if RenderFinished then begin
    RenderFinished := false;
    GUI.draw;
    RenderFinished := true;
  end else
    WriteLnLog('WindowRender','CRITICAL ERROR!!! Interface render frameskip!');
end;

{======================== Mouse & keyboard =================================}

procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // todo Joystick
  if Event.EventType = itMouseButton then begin
    doMousePress(Event);
    {if interface didn't catch the click then}
    if mbRight=event.MouseButton then camera.MouseLook := not Camera.MouseLook;
  end;
  SetGameMode(gmCharacterGeneration);
  //InitTestLevel;                         //ugly! I'll fix this soon.
end;

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then begin
    doMouseRelease(Event);
  end;
end;


procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
var i: integer;
    tmpLink: DAbstractElement;
    dragging: boolean;
begin
  {check for drag-n-drops}
  dragging := false;
  {if Event.EventType = itMouseButton then }begin
    if touchArray.count>0 then begin
     i:=0;
     repeat
       if touchArray[i].fingerindex=event.fingerindex then begin
         touchArray[i].update(Event);
         if (touchArray[i].click_element<>nil) and (touchArray[i].click_element.CanDrag) then begin
           touchArray[i].click_element.drag(round(event.Position[0]),round(event.Position[1]));
           dragging := true;
         end;
         break;
       end;
       inc(i);
     until (i>=touchArray.Count);
    end;

  end;
  {mouse over / if no drag-n-drop}
  if not dragging then begin
    tmpLink := GUI.IfMouseOver(round(event.Position[0]),round(event.Position[1]),true);
    if tmpLink <> nil then
      writelnLog('doMotion','Motion caught '+tmpLink.ClassName);
  end;
end;

{======================= initialization routines ==============================}

{$IFNDEF Android}
{$IFDEF WriteLog}
function NiceDate:string;
var s:String;
    i:integer;
begin
  s := DateTimeToAtStr(now);
  result := '';
  for i := 1 to length(s) do
    if copy(s,i,1)=' ' then result+='_' else
    if copy(s,i,1)=':' then result+='-' else
    result+=copy(s,i,1);
end;
{$ENDIF}
{$ENDIF}

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

  {$IFDEF Fullscreen}window.fullscreen := true;{$ENDIF}

  //doesn't work in Linux?
  {$IFNDEF AllowRescale}window.ResizeAllowed := raOnlyAtOpen;{$ENDIF}
  //Assign window events
  window.OnPress := @doPress;
  window.onRelease := @doRelease;
  window.OnMotion := @doMotion;
  application.LimitFPS := 60;

  WritelnLog('ApplicationInitialize','DTouchList.create');
  TouchArray := DTouchList.create;

  WritelnLog('ApplicationInitialize','Initialize fonts');
  InitializeFonts;

  //create GUI
  WritelnLog('ApplicationInitialize','Initialize interface');
  GUI := DInterfaceContainer.create(Window);
  GUI.rescale;

  {$IFDEF AllowRescale}window.OnResize := @WindowResize;{$ENDIF}
  window.OnRender := @WindowRender;

  WritelnLog('ApplicationInitialize','Init finished');

  SetGameMode(gmLoadScreen);

  //InitInterface;
  Load_test_level; //remake it

end;

{==========================================================================}

function MyGetApplicationName: string;
begin
  Result  :=  'Decoherence 1';
end;

Initialization
  OnGetApplicationName  :=  @MyGetApplicationName;
  Window := TCastleWindowTouch.create(Application);
  Application.MainWindow  :=  Window;
  Application.OnInitialize  :=  @ApplicationInitialize;

Finalization
  WriteLnLog('Finalization','Bye...');
end.

