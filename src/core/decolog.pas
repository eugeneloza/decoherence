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

(* Logging utility supporting different verbosity levels
   Most of it is just a wrapper to CastleLog *)

unit DecoLog;

{$INCLUDE compilerconfig.inc}

interface

{$IFDEF WriteLog}
uses Classes;
{$ENDIF}

const
  Version = {$INCLUDE version.inc};

const
  { normal log levels }
  LogInit = true;

  { interface info and errors }
  LogInterfaceError = true;
  LogLanguageError = true;

  {$IFDEF WriteLog}
var
  LogStream: TFileStream;
  {$ENDIF}

{ Initializes Castle Log and display basic info }
procedure InitLog;
{ Writes a log string
  should be used like Log(true, CurrentRoutine, 'message');}
procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils,
  {$IFDEF WriteLog}DecoTime,{$ENDIF}
  CastleLog;

procedure Log(const LogLevel: boolean; const aPrefix, aMessage: string);
begin
  if LogLevel then
    WriteLnLog(aPrefix, aMessage);
end;

{---------------------------------------------------------------------------}

procedure InitLog;
begin
  //initialize the log
  {$IFDEF WriteLog}
  LogStream := TFileStream.Create(NiceDate + '.log', fmCreate);
  InitializeLog(Version, LogStream, ltTime);
  {$ELSE}
  InitializeLog(Version, nil, ltTime);
  {$ENDIF}

  {this is basic information, so just output directly}
  WriteLnLog('(i) Compillation Date',{$I %DATE%} + ' Time: ' + {$I %TIME%});
  WriteLnLog('(i) FullScreen mode',{$IFDEF Fullscreen}'ON'{$ELSE}'OFF'{$ENDIF});
  WriteLnLog('(i) Pointer is', IntToStr(SizeOf(Pointer) * 8) + ' bit');
end;

end.
