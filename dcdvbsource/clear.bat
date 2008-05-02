@echo off

cd Source\EPGReader
del *.class

cd ..\Filter
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd PropertyPages
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\ChannelScan
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\Interface
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\WMPPlugin
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\..\PluginSDK\SDK
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\Samples\TuningPlugins\TwinhanWDM
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\BDAWrapper
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\FileReader
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\..\DataPlugins\PIDCounter
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\DemuxControl
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\TunerControl
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\ImageCapture
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\EPG2XML
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\..\..\..\Shared
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd MPEG
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd ..\ZLib
del *.~*;*.dcu;*.hpp;*.ddp

cd ..\..\..\Binary\Filter
upx -9 DCDVBSource.ax

cd ..\Plugins
upx -9 BDAWrapper.ddp
upx -9 TwinhanWDM.ddp
upx -9 PIDCounter.ddp
upx -9 DemuxControl.ddp
upx -9 TunerControl.ddp
upx -9 EPG2XML.ddp

cd ..\..\Temp
del *.~*;*.dcu;*.hpp;*.ddp