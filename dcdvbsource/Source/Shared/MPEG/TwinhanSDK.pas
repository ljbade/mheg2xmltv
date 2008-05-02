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

unit TwinhanSDK;

interface

uses
  DVBConst, Windows;

type
  THWType = (
    HW_ERROR = 0,
  	HW_DST_FTA_CARD = 1,
    HW_DST_CI_CARD = 2,
  	HW_DCT_FTA_CARD = 3,
    HW_DCT_CI_CARD = 4,
  	HW_DTT_FTA_CARD = 5,
  	HW_DTT_CI_CARD = 6,
  	HW_DTT_AD_CARD = 7,
  	HW_MANTIS_FTA_CARD = 8,
  	HW_MANTIS_CI_CARD = 9,

  	//MANTIS
  	// Satellite
  	HW_VP_10260 = 10, // 0x0003 Fujitsu MB86A15  By Jason
  	HW_VP_1026A = 11, // 0x0012  Fujitsu MB86A15 By Jason
  	HW_VP_10270 = 12, // 0x0005  LG TDQS_F001F   By Jason
  	HW_VP_1027A = 13, // 0x0013  LG TDQS_F001F   By Jason
  	HW_VP_10330 = 14, // 0x0016  LG TDQS_F001F   By Patrick
  	HW_VP_10340 = 15, // 0x0014  Fujitsu MB86A15 By Patrick
  	HW_VP_10410 = 16, // 0x0015  Fujitsu MB86A15 By Patrick

  	// Cable
  	HW_VP_20230 = 17, //0x0007  Philips CU1216		By Eric
  	HW_VP_20331 = 18, //0x0008  Philips CU1216		By Eric

  	// Terrestrial
  	HW_VP_30230 = 19, //0x0004  Motolora 44801 + Zarlink MT352   By Jason
  	HW_VP_30240 = 20, //0x0009  Panasonic ENV57H12D5 + Zarlink   By Jason
  	HW_VP_30410 = 21, //0x0010  Philips TU1216                   By Eric
  	HW_VP_40430 = 22, //0x5511  N/A
  	HW_M_MT352  = 23, //0x1111

  	HW_DTT_FTA_USB = 24,
  	HW_DST_FTA_USB = 25,
  	HW_DTT_DIBCOM_USB = 26,
  	HW_DTT_BDA_AGENT_USB = 27
  );

const
  TWINHAN_DEVICE_ID = 'dcdvbdevice:twinhan:';

  CLSID_TwinhanDVBSource: TGuid = '{7CDF271B-B889-4CFB-9F61-9D5E7C180894}';
  IID_ITwinhanDVBSource:  TGuid = '{8430234f-d800-42a6-948e-237a0be659a0}';
  IID_ITwinhanDVBSource4: TGuid = '{C9CB1F43-255E-45c8-A52E-CFE876757DFA}';

type
  ITwinhanDVBSource = interface(IUnknown)
  ['{8430234f-d800-42a6-948e-237a0be659a0}']
    function set_LNBType(ALNBType: Integer; ALNB1: Integer; ALNB2: Integer): HRESULT; stdcall;
    function get_LNBType(out ALNBType: Integer; out ALNB1: Integer; out ALNB2: Integer): HRESULT; stdcall;
    function LockChannel(AFrequency: Integer; ASymbolRate: Integer; AHV: Integer; ATone: Integer; ADiSEqC: Integer): HRESULT; stdcall;
  end;

  ITwinhanDVBSource4 = interface(IUnknown)
  ['{C9CB1F43-255E-45c8-A52E-CFE876757DFA}']
    function set_Power(AFlag: Boolean): HRESULT; stdcall;
    function get_HWNum(out ACardNum: Integer): HRESULT; stdcall;
    function get_HWInfo(ACardSn: Integer; out AHWType: THWType): HRESULT; stdcall;
    function get_CurrentHW(out ACardSn: Integer): HRESULT; stdcall;
    function set_CurrentHW(ACardSn: Integer): HRESULT; stdcall;
    function get_PlayBack(out APlayBack: Boolean): HRESULT; stdcall;
    function set_PlayBack(APlayBack: Boolean): HRESULT; stdcall;
    function set_PBFileName(AFileName: PChar): HRESULT; stdcall;
  end;

  function GetTwinhanDeviceName(ADeviceID: THWType): String;
  function GetTwinhanDeviceType(ADeviceID: THWType): TDVBNetworkType;
  function GetTwinhanDeviceTypeFromDeviceString(ADeviceID: String): THWType;

implementation

uses SysUtils;

function GetTwinhanDeviceName(ADeviceID: THWType): String;
begin
  case ADeviceID of
  	HW_DST_FTA_CARD:        Result := 'Twinhan Satellite FTA';
    HW_DST_CI_CARD:         Result := 'Twinhan Satellite CI';
  	HW_DCT_FTA_CARD:        Result := 'Twinhan Cable FTA';
    HW_DCT_CI_CARD:         Result := 'Twinhan Cable CI';
  	HW_DTT_FTA_CARD:        Result := 'Twinhan Terrestrial FTA';
  	HW_DTT_CI_CARD:         Result := 'Twinhan Terrestrial CI';
  	HW_DTT_AD_CARD:         Result := 'Twinhan Terrestrial AD';
  	HW_MANTIS_FTA_CARD:     Result := 'Twinhan Mantis FTA';
  	HW_MANTIS_CI_CARD:      Result := 'Twinhan Mantis CI';

  	//MANTIS
  	// Satellite
	  HW_VP_10260:            Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
	  HW_VP_1026A:            Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
	  HW_VP_10270:            Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
	  HW_VP_1027A:            Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
	  HW_VP_10330:            Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
	  HW_VP_10340:            Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
	  HW_VP_10410:            Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';

	  // Cable
	  HW_VP_20230:            Result := 'Twinhan DVB-C (Philips CU1216)';
	  HW_VP_20331:            Result := 'Twinhan DVB-C (Philips CU1216)';

    // Terrestrial
    HW_VP_30230:            Result := 'Twinhan DVB-T (Motolora 44801/Zarlink MT352)';
    HW_VP_30240:            Result := 'Twinhan DVB-T (Panasonic ENV57H12D5/Zarlink MT352)';
    HW_VP_30410:            Result := 'Twinhan DVB-T (Philips TU1216)';
    HW_VP_40430:            Result := 'Twinhan DVB-T';
    HW_M_MT352:             Result := 'Twinhan DVB-T (Zarlink MT352)';

    HW_DTT_FTA_USB:         Result := 'Twinhan DVB-T USB';
    HW_DST_FTA_USB:         Result := 'Twinhan StarBox USB';
    HW_DTT_DIBCOM_USB:      Result := 'Twinhan DVB-T USB/Dibcom';
    HW_DTT_BDA_AGENT_USB:   Result := 'Twinhan DVB-T USB/BDA';
    else                    Result := 'Twinhan (Unknown)';
  end;
end;

function GetTwinhanDeviceType(ADeviceID: THWType): TDVBNetworkType;
begin
  case ADeviceID of
  	HW_DST_FTA_CARD:        Result := ntDVBS;
    HW_DST_CI_CARD:         Result := ntDVBS;
  	HW_DCT_FTA_CARD:        Result := ntDVBC;
    HW_DCT_CI_CARD:         Result := ntDVBC;
  	HW_DTT_FTA_CARD:        Result := ntDVBT;
  	HW_DTT_CI_CARD:         Result := ntDVBT;
  	HW_DTT_AD_CARD:         Result := ntDVBT;
  	HW_MANTIS_FTA_CARD:     Result := ntDVBS;
  	HW_MANTIS_CI_CARD:      Result := ntDVBS;

  	//MANTIS
  	// Satellite
	  HW_VP_10260:            Result := ntDVBS;
	  HW_VP_1026A:            Result := ntDVBS;
	  HW_VP_10270:            Result := ntDVBS;
	  HW_VP_1027A:            Result := ntDVBS;
	  HW_VP_10330:            Result := ntDVBS;
	  HW_VP_10340:            Result := ntDVBS;
	  HW_VP_10410:            Result := ntDVBS;

	  // Cable
	  HW_VP_20230:            Result := ntDVBC;
	  HW_VP_20331:            Result := ntDVBC;

    // Terrestrial
    HW_VP_30230:            Result := ntDVBT;
    HW_VP_30240:            Result := ntDVBT;
    HW_VP_30410:            Result := ntDVBT;
    HW_VP_40430:            Result := ntDVBT;
    HW_M_MT352:             Result := ntDVBT;

    HW_DTT_FTA_USB:         Result := ntDVBT;
    HW_DST_FTA_USB:         Result := ntDVBS;
    HW_DTT_DIBCOM_USB:      Result := ntDVBT;
    HW_DTT_BDA_AGENT_USB:   Result := ntDVBT;
    else                    Result := ntUnknown;
  end;
end;

function GetTwinhanDeviceTypeFromDeviceString(ADeviceID: String): THWType;
var
  n: String;
  c: Integer;
begin
  c := Length(ADeviceID);
  n := '';

  while (c > 0) and (ADeviceID[c] <> ':') do
  begin
    n := ADeviceID[c] + n;
    dec(c);
  end;

  if not TryStrToInt(n, c)
    then Result := HW_ERROR
    else Result := THWType(c);
end;

end.
