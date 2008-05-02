(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004-2006 Milenko "DCoder" Mitrovic                        *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  This Program is free software; you can redistribute it and/or modify     *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2, or (at your option)      *
 *  any later version.                                                       *
 *                                                                           *
 *  This Program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the             *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with GNU Make; see the file COPYING.  If not, write to             *
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.    *
 *  http://www.gnu.org/copyleft/gpl.html                                     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

library DCDVBSource;

uses
  FastMM4,
  FastMove,
  BaseClass,
  DVBFilter in 'DVBFilter.pas',
  DVBInterface in 'DVBInterface.pas',
  DVBSettings in 'DVBSettings.pas',
  DVBGraphBuilder in 'DVBGraphBuilder.pas',
  PropAbout in 'PropertyPages\PropAbout.pas' {FormPropAbout},
  PropStatistics in 'PropertyPages\PropStatistics.pas' {FormPropStatistics},
  PropPlugins in 'PropertyPages\PropPlugins.pas' {FormPropPlugins},
  PropEPG in 'PropertyPages\PropEPG.pas' {FormPropEPG},
  PropMain in 'PropertyPages\PropMain.pas' {FormPropSettings},
  DVBAudioFilter in 'DVBAudioFilter.pas',
  PropMHP in 'PropertyPages\PropMHP.pas' {FormPropMHP},
  DVBMHPParser in 'DVBMHPParser.pas',
  DVBTeletextFilter in 'DVBTeletextFilter.pas',
  DVBSourceFilter in 'DVBSourceFilter.pas',
  IDVBSource in 'Interface\IDVBSource.pas',
  DVBOSDFilter in 'DVBOSDFilter.pas',
  PropRecordings in 'PropertyPages\PropRecordings.pas' {FormPropRecordings},
  formAddRecording in 'PropertyPages\formAddRecording.pas' {frmAddRecording},
  DVBRecordings in 'DVBRecordings.pas',
  DVBEPG in 'DVBEPG.pas',
  BDAConst in '..\Shared\MPEG\BDAConst.pas',
  BDATSDumpFilter in '..\Shared\MPEG\BDATSDumpFilter.pas',
  BDATuning in '..\Shared\MPEG\BDATuning.pas',
  BDAUtils in '..\Shared\MPEG\BDAUtils.pas',
  DSMCCBIOP in '..\Shared\MPEG\DSMCCBIOP.pas',
  DSMCCConst in '..\Shared\MPEG\DSMCCConst.pas',
  DSMCCSections in '..\Shared\MPEG\DSMCCSections.pas',
  DVBChannelList in '..\Shared\MPEG\DVBChannelList.pas',
  DVBConst in '..\Shared\MPEG\DVBConst.pas',
  DVBDataParser in '..\Shared\MPEG\DVBDataParser.pas',
  DVBDescriptors in '..\Shared\MPEG\DVBDescriptors.pas',
  DVBFrequencyList in '..\Shared\MPEG\DVBFrequencyList.pas',
  DVBScanner in '..\Shared\MPEG\DVBScanner.pas',
  DVBUtils in '..\Shared\MPEG\DVBUtils.pas',
  ISO639LanguageCode in '..\Shared\MPEG\ISO639LanguageCode.pas',
  Logger in '..\Shared\MPEG\Logger.pas',
  MHPConst in '..\Shared\MPEG\MHPConst.pas',
  MHPDescriptors in '..\Shared\MPEG\MHPDescriptors.pas',
  MHPSections in '..\Shared\MPEG\MHPSections.pas',
  MHPUtils in '..\Shared\MPEG\MHPUtils.pas',
  MPEGConst in '..\Shared\MPEG\MPEGConst.pas',
  MPEGDescriptors in '..\Shared\MPEG\MPEGDescriptors.pas',
  MPEGParser in '..\Shared\MPEG\MPEGParser.pas',
  MPEGSections in '..\Shared\MPEG\MPEGSections.pas',
  MPEGUtils in '..\Shared\MPEG\MPEGUtils.pas',
  MulticastClientServer in '..\Shared\MPEG\MulticastClientServer.pas',
  MulticastTransportStream in '..\Shared\MPEG\MulticastTransportStream.pas',
  TCPClientServer in '..\Shared\MPEG\TCPClientServer.pas',
  TCPTransportStream in '..\Shared\MPEG\TCPTransportStream.pas',
  TransportStream in '..\Shared\MPEG\TransportStream.pas',
  JvSimpleXml in '..\Shared\JvSimpleXml.pas',
  ZLIBEX in '..\Shared\Zlib\ZLIBEX.PAS',
  DVBSubtitleFilter in 'DVBSubtitleFilter.pas',
  DVBNetworkFilter in 'DVBNetworkFilter.pas',
  FileTransportStream in '..\Shared\MPEG\FileTransportStream.pas',
  DVBVideoAnalyzer in 'DVBVideoAnalyzer.pas',
  FileTransportStream2 in '..\Shared\MPEG\FileTransportStream2.pas',
  DVBTeletextParser in '..\Shared\MPEG\DVBTeletextParser.pas',
  JvTrayIcon in '..\Shared\JvTrayIcon.pas',
  DVBSubtitlingParser in '..\Shared\MPEG\DVBSubtitlingParser.pas',
  DCDVBPluginManager in '..\Shared\MPEG\DCDVBPluginManager.pas',
  DCDVBTuningPlugins in '..\PluginSDK\SDK\DCDVBTuningPlugins.pas',
  DCDVBDataPlugins in '..\PluginSDK\SDK\DCDVBDataPlugins.pas',
  DCDVBShared in '..\PluginSDK\SDK\DCDVBShared.pas',
  DCDVBTuningPluginTransportStream in '..\Shared\MPEG\DCDVBTuningPluginTransportStream.pas',
  DLLExports in 'DLLExports.pas',
  formWMPAbout in 'WMPPlugin\formWMPAbout.pas' {frmWMPAbout},
  formWMPPlugin in 'WMPPlugin\formWMPPlugin.pas' {frmWMPPlugin},
  formWMPPluginInfo in 'WMPPlugin\formWMPPluginInfo.pas' {frmWMPPlugin},
  WMPPlugin in 'WMPPlugin\WMPPlugin.pas',
  WMPPluginInfo in 'WMPPlugin\WMPPluginInfo.pas',
  WMPPluginAPI in 'WMPPlugin\WMPPluginAPI.pas',
  formAbout in 'ChannelScan\formAbout.pas' {frmAbout},
  formAddChannel in 'ChannelScan\formAddChannel.pas' {frmAddChannel},
  formAddStream in 'ChannelScan\formAddStream.pas' {frmAddStream},
  formDebug in 'ChannelScan\formDebug.pas' {frmDebug},
  formFrequency in 'ChannelScan\formFrequency.pas' {frmFrequency},
  formLNBSelection in 'ChannelScan\formLNBSelection.pas' {frmLNBSelection},
  formNetwork in 'ChannelScan\formNetwork.pas' {frmNetwork},
  formOSDColor in 'ChannelScan\formOSDColor.pas' {frmOSDColor},
  formParameters in 'ChannelScan\formParameters.pas' {frmParameters},
  formPropertyPage in 'ChannelScan\formPropertyPage.pas' {frmPropertyPage},
  formScan in 'ChannelScan\formScan.pas' {frmScan},
  FormQuickEPG in 'FormQuickEPG.pas' {frmQuickEPG},
  MPEGDemultiplexer in 'MPEGDemultiplexer.pas',
  BitReader in '..\Shared\MPEG\BitReader.pas',
  NetworkParser in 'NetworkParser.pas',
  TSTimeShift in 'TSTimeShift.pas';

{$E ax}
{$R Resource.res}

exports
  DLLExports.DllGetClassObject,
  DLLExports.DllCanUnloadNow,
  DLLExports.DllRegisterServer,
  DLLExports.DllUnregisterServer,
  DLLExports.ChannelScan,
  DLLExports.DeviceReset;

begin
end.
