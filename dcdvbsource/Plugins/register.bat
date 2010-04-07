@echo off
if "%OS%"=="Windows_NT" goto NT
%WINDIR%\system\regsvr32.exe BDAWrapper.ddp
%WINDIR%\system\regsvr32.exe TwinhanWDM.ddp
%WINDIR%\system\regsvr32.exe DemuxControl.ddp
%WINDIR%\system\regsvr32.exe TunerControl.ddp
%WINDIR%\system\regsvr32.exe PIDCounter.ddp
goto END
:NT
regsvr32.exe BDAWrapper.ddp
regsvr32.exe TwinhanWDM.ddp
regsvr32.exe DemuxControl.ddp
regsvr32.exe TunerControl.ddp
regsvr32.exe PIDCounter.ddp
:END