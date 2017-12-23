program Decoherence;
{$INCLUDE compilerconfig.inc}

{$IFDEF Windows}{$IFDEF RELEASE}{$APPTYPE GUI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF}

uses
  {$IFDEF useCMEM}cmem,{$ENDIF}
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  DecoMain,
  DecoWindow;

{$R *.res}

begin
  //if heap requested to be written in a file
  {$IFDEF DEBUG}{$IFDEF HEAP_FILE}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  // Set up -gh output for the Leakview package:
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}{$ENDIF}

  Window.OpenAndRun;
end.

