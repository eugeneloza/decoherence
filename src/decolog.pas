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
//uses

const Version = {$INCLUDE version.inc};

{ Constants for logging verbosity level }
const Log0 = 1; //this is the most important message
const LogError = 1;
const LogVerbose = 20; //this is very unimportant message
const LogTemp = 1;

const LogInit = 5;
const LogInit2 = LogInit+1;
const LogInitError = LogError;

const LogInitSound = LogInit2;
const LogInitCharacters = LogInit2;
const LogInitPlayer = LogInit2;
const LogInitData = LogInit2;

const LogInterfaceError = LogError;
const LogInitInterface = LogInit2;
const LogInterfaceInfo = LogVerbose-1;
const LogInterfaceGLError = LogError;
const LogInterfaceScaleHint = LogVerbose;
const LogInterfaceSceleError = LogError+2;

{errors}
const LogLabelError = LogInterfaceError;
const LogThreadError = LogError;
const LogAnimationError = LogError;

const LogNavigationError = LogError;
const LogSoundError = LogError;
const LogParserError = LogError;

{soft errors}
const Log3DLoadSoftError = LogError+5;

{World}
const LogWorldInitSoftError = 5;
const LogWorldInit = LogInit2;
const LogWorldError = LogError;
const LogGenerateWorld = LogWorldInit;

{Mouse}
const LogMouseInfo = LogVerbose;
const LogMouseError = LogError;
const LogMouseSoftError = LogMouseError+2;

{Constructor}
const LogConstructorInfo = LogVerbose;
const LogConstructorError = LogError;


{Actor}
const LogActorError = LogError;

var Verbosity: integer = 100;

{ Initializes Castle Log and display basic info }
procedure InitLog;
{ Writes a log string
  should be used like dLog(1, Self,prefix,message)
  Self = nil inside a procedure }
procedure dLog(const LogLevel: integer; const aObj: TObject; const aPrefix, aMessage: string);
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleLog,
  DecoTime;

procedure dLog(const LogLevel: integer; const aObj: TObject; const aPrefix, aMessage: string);
var objName: string;
begin
  if Verbosity <= 0 then Exit;
  if Verbosity >= LogLevel then begin
    if aObj<>nil then objName := ' in '+aObj.ClassName else objName := '';
    WriteLnLog(aPrefix+objName,aMessage)
  end;  
end;

{---------------------------------------------------------------------------}

procedure InitLog;
begin
  if Verbosity <= 0 then Exit;
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
  {this is basic information, so just output directly}
  WritelnLog('(i)','Compillation Date: ' + {$I %DATE%} + ' Time: ' + {$I %TIME%});
  WritelnLog('(i) FullScreen mode',{$IFDEF Fullscreen}'ON'{$ELSE}'OFF'{$ENDIF});
  WritelnLog('(i) Allow rescale',{$IFDEF AllowRescale}'ON'{$ELSE}'OFF'{$ENDIF});
end;

end.

