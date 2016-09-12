{$mode objfpc}{$H+}
program decoherence_desktop;
Uses {$IFDEF UNIX}cthreads,{$ENDIF} decoherence, castle_base, global_var;
begin
  window.openandrun;
end.



