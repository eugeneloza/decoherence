find src/ -type f -name '*.pas' -exec sed -i -- "s/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StartProfiler}/g" {} +
find src/ -type f -name '*.inc' -exec sed -i -- "s/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StartProfiler}/g" {} +
find src/ -type f -name '*.pas' -exec sed -i -- "s/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StopProfiler}/g" {} +
find src/ -type f -name '*.inc' -exec sed -i -- "s/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StopProfiler}/g" {} +

sed -i -- "s/{\$DEFINE UseProfiler}/{\$UNDEF UseProfiler}/g" src/profiler.inc
