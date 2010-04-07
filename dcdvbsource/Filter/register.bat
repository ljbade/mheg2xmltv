@echo off
if "%OS%"=="Windows_NT" goto NT
%WINDIR%\system\regsvr32.exe DCDVBSource.ax
goto END
:NT
regsvr32.exe DCDVBSource.ax
:END