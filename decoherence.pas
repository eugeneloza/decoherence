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

interface


implementation

uses Classes, SysUtils,
     CastleLog,
     CastleWindow, CastleKeysMouse,
     decomouse,
     DecoLoadScreen,
     global_var;

procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then doMousePress(Event);
  DestroyLoadScreen;
end;


{======================= initialization routines ==============================}

procedure ApplicationInitialize;
begin
  InitializeLog;
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
end;

function MyGetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;

Initialization
  OnGetApplicationName := @MyGetApplicationName;
  Window:=TCastleWindow.create(Application);
  { This should be done as early as possible to mark our log lines correctly. }
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.

