find src/ -type f -name '*.pas' -exec sed -i -- "s/{StartProfiler}/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" {} +
find src/ -type f -name '*.inc' -exec sed -i -- "s/{StartProfiler}/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" {} +
find src/ -type f -name '*.pas' -exec sed -i -- "s/{StopProfiler}/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" {} +
find src/ -type f -name '*.inc' -exec sed -i -- "s/{StopProfiler}/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" {} +
