{$mode objfpc}{$H+}
program decoherence_desktop;
Uses {$IFDEF UNIX}cthreads,{$ENDIF} decoherence, castle_base, castle_window,
  global_var, decoloadscreen, DecoFont, decolevel;
begin
  window.openandrun;
end.



