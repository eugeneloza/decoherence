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

unit Decoherence;

{$mode objfpc}{$H+}
//{$DEFINE WriteLog}{$IFDEF Windows}{$APPTYPE GUI}{$ENDIF}

interface

const Version='Interfa2-160913-25';

implementation

uses Classes, SysUtils,
     CastleLog, CastleTimeUtils,
     CastleWindow, CastleWindowTouch, CastleKeysMouse,
     decomouse, decointerface, DecoFont,
     DecoLoadScreen,
     decolevel,
     decoglobal;


{$R+}{$Q+}

procedure doWindowRender(Container: TUIContainer);
begin
  DrawInterface
{    UIFont.Print(10, 10, Yellow, Format('FPS : %f (real : %f). Shapes : %d / %d',
   [Window.Fps.FrameTime,
    Window.Fps.RealTime,
    Window.SceneManager.Statistics.ShapesRendered,
    Window.SceneManager.Statistics.ShapesVisible]));}
end;

{------------------------------------------------------------------}

procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then begin
    doMousePress(Event);
    {if interface didn't catch the click then}
    if mbRight=event.MouseButton then camera.MouseLook:=not Camera.MouseLook;
  end;
  InitTestLevel;                         //ugly! I'll fix this soon.
  window.OnRender:=@doWindowRender;
end;

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then begin
    doMouseRelease(Event);
  end;
end;


{======================= initialization routines ==============================}

{$IFNDEF Android}
{$IFDEF WriteLog}
function NiceDate:string;
var s:String;
    i:integer;
begin
  s:=DateTimeToAtStr(now);
  result:='';
  for i:=1 to length(s) do
    if copy(s,i,1)=' ' then result+='_' else
    if copy(s,i,1)=':' then result+='-' else
    result+=copy(s,i,1);
end;
{$ENDIF}
{$ENDIF}

procedure ApplicationInitialize;
begin
  {$IFDEF Android}
  InitializeLog;
  {$ELSE}
    {$IFDEF WriteLog}
      LogStream:=TFileStream.Create('log_'+NiceDate+'.txt',fmCreate);
      InitializeLog(Version,LogStream);
    {$ELSE}
      InitializeLog;
    {$ENDIF}
  {$ENDIF}
  WritelnLog('ApplicationInitialize','Init');

   { Window.Container.UIScaling := usEncloseReferenceSize;
    Window.Container.UIReferenceWidth := 1024;
    Window.Container.UIReferenceHeight := 768;}

  window.OnPress:=@doPress;
  window.onRelease:=@doRelease;
  WritelnLog('ApplicationInitialize','DTouchList.create');
  TouchArray:=DTouchList.create;

{  application.TimerMilisec:=1000 div 60; //60 fps
  application.OnTimer:=@dotimer;}

  randomize;
  WritelnLog('ApplicationInitialize','InitializeFonts');
  InitializeFonts;

  WritelnLog('ApplicationInitialize','Init finished');

  MakeLoadScreen;
  InitInterface;
  Load_test_level;

end;

function MyGetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;

Initialization
  OnGetApplicationName := @MyGetApplicationName;
  Window:=TCastleWindowTouch.create(Application);
  { This should be done as early as possible to mark our log lines correctly. }
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;

Finalization
  WriteLnLog('Finalization','Bye...');
end.

