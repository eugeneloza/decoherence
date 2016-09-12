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
{$DEFINE WriteLog}{$IFDEF Windows}{$APPTYPE GUI}{$ENDIF}

interface

const Version='Interfa2-160912-19';

implementation

uses Classes, SysUtils,
     CastleLog, CastleTimeUtils,
     CastleWindow, CastleKeysMouse,
     decomouse, decointerface,
     DecoLoadScreen,
     decolevel,
     decoglobal;


{$R+}{$Q+}

procedure doWindowRender(Container: TUIContainer);
begin
  DrawInterface
end;

{------------------------------------------------------------------}

procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then doMousePress(Event);
  InitTestLevel;                         //ugly! I'll fix this soon.
  window.OnRender:=@doWindowRender;
end;


{======================= initialization routines ==============================}

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

  window.OnPress:=@doPress;
  TouchArray:=DTouchList.create;

{  window.OnRender:=@doWindowRender;
  window.OnResize:=@doWindowResize;

  window.onRelease:=@MenuKeyRelease;
  window.OnMotion:=nil;}

{  application.TimerMilisec:=1000 div 60; //60 fps
  application.OnTimer:=@dotimer;}

  WritelnLog('ApplicationInitialize','Init finished');

  MakeLoadScreen;
  InitInterface;
  Load_test_level;
end;

function MyGetApplicationName: string;
begin
  Result := 'Decoherence-1';
end;

Initialization
  OnGetApplicationName := @MyGetApplicationName;
  Window:=TCastleWindow.create(Application);
  { This should be done as early as possible to mark our log lines correctly. }
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.

