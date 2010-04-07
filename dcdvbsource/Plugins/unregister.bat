@echo off
if "%OS%"=="Windows_NT" goto NT
%WINDIR%\system\regsvr32.exe /U BDAWrapper.ddp
%WINDIR%\system\regsvr32.exe /U TwinhanWDM.ddp
%WINDIR%\system\regsvr32.exe /U DemuxControl.ddp
%WINDIR%\system\regsvr32.exe /U TunerControl.ddp
%WINDIR%\system\regsvr32.exe /U PIDCounter.ddp
goto END
:NT
regsvr32.exe /U BDAWrapper.ddp
regsvr32.exe /U TwinhanWDM.ddp
regsvr32.exe /U DemuxControl.ddp
regsvr32.exe /U TunerControl.ddp
regsvr32.exe /U PIDCounter.ddp
:END