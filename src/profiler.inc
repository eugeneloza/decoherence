{$MACRO ON}

{$IFDEF UseProfiler} //if we use profiler then create macros

//this will start profiling the procedure
{$DEFINE StartProfiler:=doStartProfiler({$IF DECLARED(ClassName)}ClassName+'.'+{$ENDIF}{$I %CURRENTROUTINE%})}
//this will stop profiling the procedure
{$DEFINE StopProfiler:=doStopProfiler({$IF DECLARED(ClassName)}ClassName+'.'+{$ENDIF}{$I %CURRENTROUTINE%})}

{$ELSE} //otherwise just do nothing

{$DEFINE StartProfiler:=}
{$DEFINE StopProfiler:=}

{$ENDIF}
