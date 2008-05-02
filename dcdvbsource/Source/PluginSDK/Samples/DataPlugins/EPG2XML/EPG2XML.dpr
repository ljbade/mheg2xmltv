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

library EPG2XML;

{$E ddp}

uses
  UEPG2XML in 'UEPG2XML.pas',
  PropSettings in 'PropSettings.pas',
  DCDVBShared in '..\..\..\SDK\DCDVBShared.pas',
  DCDVBDataPlugins in '..\..\..\SDK\DCDVBDataPlugins.pas',
  IDVBSource in '..\..\..\..\Filter\Interface\IDVBSource.pas',
  JvSimpleXml in '..\..\..\..\Shared\JvSimpleXml.pas',
  DVBEPG in '..\..\..\..\Filter\DVBEPG.pas',
  DVBSettings in '..\..\..\..\Filter\DVBSettings.pas',
  DVBMHPParser in '..\..\..\..\Filter\DVBMHPParser.pas',
  DVBRecordings in '..\..\..\..\Filter\DVBRecordings.pas',
  IffDecoder_com in '..\..\..\..\Filter\IffDecoder_com.pas',
  BDAConst in '..\..\..\..\Shared\MPEG\BDAConst.pas',
  BDATuning in '..\..\..\..\Shared\MPEG\BDATuning.pas',
  BDAUtils in '..\..\..\..\Shared\MPEG\BDAUtils.pas',
  DCDVBPluginManager in '..\..\..\..\Shared\MPEG\DCDVBPluginManager.pas',
  DSMCCBIOP in '..\..\..\..\Shared\MPEG\DSMCCBIOP.pas',
  DSMCCConst in '..\..\..\..\Shared\MPEG\DSMCCConst.pas',
  DSMCCSections in '..\..\..\..\Shared\MPEG\DSMCCSections.pas',
  DVBChannelList in '..\..\..\..\Shared\MPEG\DVBChannelList.pas',
  DVBConst in '..\..\..\..\Shared\MPEG\DVBConst.pas',
  DVBDataParser in '..\..\..\..\Shared\MPEG\DVBDataParser.pas',
  DVBDescriptors in '..\..\..\..\Shared\MPEG\DVBDescriptors.pas',
  DVBFrequencyList in '..\..\..\..\Shared\MPEG\DVBFrequencyList.pas',
  DVBScanner in '..\..\..\..\Shared\MPEG\DVBScanner.pas',
  DVBSubtitlingParser in '..\..\..\..\Shared\MPEG\DVBSubtitlingParser.pas',
  DVBTeletextParser in '..\..\..\..\Shared\MPEG\DVBTeletextParser.pas',
  DVBUtils in '..\..\..\..\Shared\MPEG\DVBUtils.pas',
  ISO639LanguageCode in '..\..\..\..\Shared\MPEG\ISO639LanguageCode.pas',
  Logger in '..\..\..\..\Shared\MPEG\Logger.pas',
  MHPConst in '..\..\..\..\Shared\MPEG\MHPConst.pas',
  MHPDescriptors in '..\..\..\..\Shared\MPEG\MHPDescriptors.pas',
  MHPSections in '..\..\..\..\Shared\MPEG\MHPSections.pas',
  MHPUtils in '..\..\..\..\Shared\MPEG\MHPUtils.pas',
  MPEGConst in '..\..\..\..\Shared\MPEG\MPEGConst.pas',
  MPEGDescriptors in '..\..\..\..\Shared\MPEG\MPEGDescriptors.pas',
  MPEGParser in '..\..\..\..\Shared\MPEG\MPEGParser.pas',
  MPEGSections in '..\..\..\..\Shared\MPEG\MPEGSections.pas',
  MPEGUtils in '..\..\..\..\Shared\MPEG\MPEGUtils.pas',
  ZLibEx in '..\..\..\..\Shared\Zlib\ZLIBEX.PAS',
  DCDVBTuningPlugins in '..\..\..\SDK\DCDVBTuningPlugins.pas';

exports

  DCDVBDataPlugins.DllGetClassObject,
  DCDVBDataPlugins.DllCanUnloadNow,
  DCDVBDataPlugins.DllRegisterServer,
  DCDVBDataPlugins.DllUnregisterServer;

begin
end.
 