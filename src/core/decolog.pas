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

{ Logging utility supporting different verbosity levels
 Most of it is just a wrapper to CastleLog }
unit DecoLog;

{$INCLUDE compilerconfig.inc}
interface

uses Classes;

const Version = {$INCLUDE version.inc};

{ --- HIGHEST LEVEL --- }

const
  LogError = True;

const
  LogInitError = LogError;

const
  LogConstructorError = LogError;

const
  LogAnimationError = LogError;

const
  LogNavigationError = LogError;

const
  LogSoundError = LogError;

const
  LogParserError = LogError;

const
  LogWorldError = LogError;

const
  LogMouseError = LogError;

const
  LogActorError = LogError;

const
  LogInterfaceError = LogError;

const
  LogInterfaceGLError = LogError;

const
  LogContextError = LogError;

const
  LogLabelError = LogInterfaceError;

const
  LogInterfaceScaleError = LogInterfaceError;

const
  LogTemp = True;

{ --- MEDIUM LEVEL --- }

const
  LogInit = True;

const
  LogInit2 = True;

const
  LogInitSound = LogInit2;

const
  LogInitCharacters = LogInit2;

const
  LogInitPlayer = LogInit2;

const
  LogInitData = LogInit2;

const
  LogInitInterface = LogInit2;

const
  LogWorldInit = LogInit2;

const
  LogGenerateWorld = LogWorldInit;

const
  LogWorldInitSoftError = LogInitInterface;

const
  LogMouseSoftError = LogInit2;

{ --- LOWEST LEVEL --- }

const
  LogVerbose = True;

const
  LogInterfaceInfo = LogVerbose;

const
  LogInterfaceScaleHint = LogVerbose;

const
  Log3DLoadSoftError = LogVerbose;

const
  LogMouseInfo = LogVerbose;

const
  LogConstructorInfo = LogVerbose;

var
  doLog: boolean = True;

  {$IFDEF WriteLog}
  LogStream: TFileStream;

  {$ENDIF}

{ Initializes Castle Log and display basic info }
procedure InitLog;
{ Writes a log string
  should be used like dLog(1, Self,_CurrentRoutine,message)
  Self = nil inside a procedure }
procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
//procedure fLog(const LogLevel: boolean; const aObj: TObject; const aPrefix, aMessage: string);
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleLog, SysUtils,
  DecoTime, Profiler;

procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
begin
  {StartProfiler}

  if not doLog then
    Exit;
  if LogLevel then
    WriteLnLog(aPrefix, aMessage);

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

procedure InitLog;
begin
  {StartProfiler}

  if not doLog then
    Exit;
  //initialize the log
  {$IFDEF Android}
  InitializeLog;
  {$ELSE}
    {$IFDEF WriteLog}
  LogStream := TFileStream.Create('log_' + NiceDate + '.txt', fmCreate);
  InitializeLog(Version, LogStream, ltTime);
    {$ELSE}
  InitializeLog(Version, nil, ltTime);
    {$ENDIF}
  {$ENDIF}
  //BacktraceOnLog := true;
  {this is basic information, so just output directly}
  WriteLnLog('(i) Compillation Date',{$I %DATE%} + ' Time: ' + {$I %TIME%});
  WriteLnLog('(i) FullScreen mode',{$IFDEF Fullscreen}'ON'{$ELSE}'OFF'{$ENDIF});
  WriteLnLog('(i) Allow rescale',{$IFDEF AllowRescale}'ON'{$ELSE}'OFF'{$ENDIF});
  WriteLnLog('(i) Pointer is', IntToStr(SizeOf(Pointer) * 8) + ' bit');

  {StopProfiler}
end;

end.
