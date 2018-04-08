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

(* Init file of the game. Here everything starts. *)

{$INCLUDE compilerconfig.inc}

program Decoherence;

{$IFDEF Windows}{$IFDEF RELEASE}{$APPTYPE GUI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF}

uses
  {$IFDEF useCMEM}cmem,{$ENDIF}
  {$IFDEF UNIX}cthreads,{$ENDIF}

  {$IFDEF DEBUG}{$IFDEF HEAP_FILE}
  SysUtils,
  {$ENDIF}{$ENDIF}

  DecoMain,
  DecoWindow;

{$R *.res}

begin
  { if heap requested to be written in a file }
  {$IFDEF DEBUG}{$IFDEF HEAP_FILE}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  // Set up -gh output for the Leakview package:
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}{$ENDIF}

  { Write an error if FPC version is too low }
  {$IF FPC_FULLVERSION < 30101}
  {$ERROR FPC version 3.1.1+ is required!}
  {$ENDIF}

  { Actually start the game }
  Window.OpenAndRun;
end.

