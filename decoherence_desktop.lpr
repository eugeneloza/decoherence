{$mode objfpc}{$H+}
program decoherence_desktop;
Uses {$IFDEF UNIX}cthreads,{$ENDIF} decoherence, {castle_base, castle_window, }
  decoglobal;
begin
  window.openandrun;
end.



