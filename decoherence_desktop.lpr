{$INCLUDE compilerconfig.inc}
program decoherence_desktop;

Uses {$IFDEF UNIX}cthreads,{$ENDIF}{$IFDEF DEBUG}SysUtils,{$ENDIF} decoglobal, decoherence;

begin
  {$IFDEF DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
    // Set up -gh output for the Leakview package:
    SetHeapTraceOutput('heap.trc');
  {$ENDIF}
  window.OpenAndRun;
end.

