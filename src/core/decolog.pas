{ Copyright (C) 2012-2018 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{---------------------------------------------------------------------------}

(* Logging utility supporting different verbosity levels
   Most of it is just a wrapper to CastleLog *)

{$INCLUDE compilerconfig.inc}

unit DecoLog;

interface

const
  { Normal log levels }
  LogInit = true;
  LogWarning = true;

  { Interface info and errors }
  LogInterfaceError = true;
  LogInterfaceWarning = true;
  LogInterfaceInfo = true;
  LogImageScaleError = true;
  LogLanguageError = true;
  LogInterfaceImageLoading = true;
  LogFontError = true;
  LogFontWarning = true;

  { Input }
  LogMouseError = true;
  LogMouseInfo = true;

  { HDD }
  LogReadWriteError = true;
  LogFileAccess = true;

  { Actors }
  LogActorWarning = true;

{ Initializes Castle Log and display basic info }
procedure InitLog;
procedure FreeLog;
{ Writes a log string
  should be used like Log(true, CurrentRoutine, 'message');}
procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
{............................................................................}
implementation

uses
  SysUtils,
  {$IFDEF WriteLog}Classes, DecoTime,{$ENDIF}
  CastleApplicationProperties, CastleLog, Profiler;

{$IFDEF WriteLog}
var
  LogStream: TFileStream;
{$ENDIF}

procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
begin
  if LogLevel then
    WriteLnLog(aPrefix, aMessage);
end;

{............................................................................}

procedure InitLog;
const
  Version = {$INCLUDE version.inc};
begin
  {StartProfiler}

  //initialize the log
  {$IFDEF WriteLog}
  LogStream := TFileStream.Create(NiceDate + '.log', fmCreate);
  InitializeLog(LogStream, ltTime);
  {$ELSE}
  ApplicationProperties.Version := Version;
  InitializeLog(nil, ltTime);
  {$ENDIF}

  {this is basic information, so just output directly}
  WriteLnLog('(i) Compilation Date',{$I %DATE%} + ' Time: ' + {$I %TIME%});
  WriteLnLog('(i) Pointer is', IntToStr(SizeOf(Pointer) * 8) + ' bit');

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure FreeLog;
begin
  {$IFDEF WriteLog}
  LogStream.Free;
  {$ENDIF}
end;

end.
