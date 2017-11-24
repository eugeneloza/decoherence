{$MACRO ON}

//temporary place it here
{$DEFINE _CurrentRoutine:={$IF DECLARED(ClassName)}ClassName+'.'+{$ENDIF}{$I %CURRENTROUTINE%}}

{$DEFINE UseProfiler}

//main part

{$IFDEF UseProfiler} //if we use profiler then create macros

//this will start profiling the procedure
{$DEFINE StartProfiler:=doStartProfiler(_CurrentRoutine)}
//this will stop profiling the procedure
{$DEFINE StopProfiler:=doStopProfiler}

{$ELSE} //otherwise just replace fbegin/fend with begin/end

{$DEFINE StartProfiler:=}
{$DEFINE StopProfiler:=}

{$ENDIF}
