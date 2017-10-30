@echo off
for %%a in (.) do set currentfolder=%%~nxa
@echo %currentfolder% > version.inc