cd src
sed -i -- "s/{StartProfiler}/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" *.pas
sed -i -- "s/{StartProfiler}/doStartProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" *.inc
sed -i -- "s/{StopProfiler}/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" *.pas
sed -i -- "s/{StopProfiler}/doStopProfiler({\$IF DECLARED(ClassName)}ClassName+'.'+{\$ENDIF}{\$I %CURRENTROUTINE%});/g" *.inc
cd ..