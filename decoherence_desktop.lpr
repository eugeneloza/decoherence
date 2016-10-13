{$mode objfpc}{$H+}
program decoherence_desktop;

Uses {$IFDEF UNIX}cthreads,{$ENDIF} decoglobal, decoherence;

begin
  window.openandrun;
end.

