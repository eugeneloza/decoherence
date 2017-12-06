cd src
sed -i -- "s/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StartProfiler}/g" *.pas
sed -i -- "s/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StartProfiler}/g" *.inc
sed -i -- "s/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StopProfiler}/g" *.pas
sed -i -- "s/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/{StopProfiler}/g" *.inc
cd ..