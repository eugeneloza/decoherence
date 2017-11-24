program Decoherence_Desktop;

{$INCLUDE src/compilerconfig.inc}

{$IFDEF Windows}{$IFDEF RELEASE}{$APPTYPE GUI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF}

Uses {$IFDEF useCMEM}cmem,{$ENDIF}{$IFDEF UNIX}cthreads,{$ENDIF} SysUtils,
  DecoGlobal, Decoherence;

{$R *.res}

begin
  {$IFDEF DEBUG}{$IFDEF HEAP_FILE}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
    // Set up -gh output for the Leakview package:
    SetHeapTraceOutput('heap.trc');
  {$ENDIF}{$ENDIF}
  Window.OpenAndRun;
end.

