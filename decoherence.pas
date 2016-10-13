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
{$R+}{$Q+}

interface

const Version='interfa3-161013-40';

implementation

uses Classes, SysUtils,
     CastleLog,
     CastleWindow, CastleWindowTouch, CastleKeysMouse,
     {decomouse, decointerface, DecoFont,
     DecoLoadScreen,
     decolevel,}
     decointerface, decoglobal;




{==========================================================================}
{==========================================================================}

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
  {$IFDEF Android}
  InitializeLog;
  {$ELSE}
    {$IFDEF WriteLog}
      LogStream := TFileStream.Create('log_'+NiceDate+'.txt',fmCreate);
      InitializeLog(Version,LogStream,ldTime);
    {$ELSE}
      InitializeLog(Version,nil,ltTime);
    {$ENDIF}
  {$ENDIF}
  WritelnLog('ApplicationInitialize','Init');

  //window.OnPress := @doPress;
  //window.onRelease := @doRelease;
  //WritelnLog('ApplicationInitialize','DTouchList.create');
  //TouchArray := DTouchList.create;

  //randomize;
  //WritelnLog('ApplicationInitialize','InitializeFonts');
  //InitializeFonts;

  //WritelnLog('ApplicationInitialize','Init finished');

  //MakeLoadScreen;
  //InitInterface;
  //Load_test_level;

end;

{==========================================================================}

function MyGetApplicationName: string;
begin
  Result  :=  'Decoherence 1';
end;

Initialization
  OnGetApplicationName  :=  @MyGetApplicationName;
  Window := TCastleWindowTouch.create(Application);
  { This should be done as early as possible to mark our log lines correctly. }
  Application.MainWindow  :=  Window;
  Application.OnInitialize  :=  @ApplicationInitialize;

Finalization
  WriteLnLog('Finalization','Bye...');
end.

