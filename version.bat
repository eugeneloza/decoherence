@echo off
FOR %%I IN (.) DO SET version=%%~nI%%~xI
echo '%version%' > ./src/version.inc