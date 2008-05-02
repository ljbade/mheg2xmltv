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
  Windows, DCDVBTuningPlugins;

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

    // MANTIS
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
    HW_M_MT352 = 23,  //0x1111

    HW_DTT_FTA_USB = 24,
    HW_DST_FTA_USB = 25,
    HW_DTT_DIBCOM_USB = 26,
    HW_DTT_BDA_AGENT_USB = 27
  );

const
  TWINHAN_DEVICE_ID = 'dcdvb:device:twinhan:';
  CLSID_TwinhanDVBSource: TGuid = '{7CDF271B-B889-4CFB-9F61-9D5E7C180894}';

  IID_IDVBSource: TGuid = '{8430234F-D800-42A6-948E-237A0BE659A0}';
  IID_IDVBSource2: TGuid = '{965AE5D0-C66B-4ca5-BB24-50CFDB240E29}';
  IID_IDVBDishDrv: TGuid = '{3f91196e-c493-4eaa-b77f-eb1f443051cc}';
  IID_IDVBCIMenu: TGuid = '{39d4fb19-934d-4884-bc31-4a2d0db874da}';
  IID_IGetDeviceInfo2: TGuid = '{A46E99E5-0F47-49c9-A869-37F02CEEFB4F}';
  IID_IDVBSource4: TGuid = '{C9CB1F43-255E-45c8-A52E-CFE876757DFA}';
  IID_IDVBSource5: TGuid = '{939E9505-030E-43e7-BEEA-D43B5A50F7E8}';
  IID_ISrcPidFilter: TGuid = '{93E28118-C39F-4f14-87E7-FFA7D8030F90}';


type
  TDVBSourceCallback = procedure(Data: Pchar); stdcall;
  TPCIConfig = record
    VendorID: Word;
    DeviceID: Word;
    Command: Word;
    Status: Word;
    RevisionID: Word;
    SubVendorID: Word;
    SubSystemID: Word;
  end;

  TAPPInfo = packed record
    app_type: DWord;
    application_manufacture: DWord;
    manufacture_code: DWord;
    Application_Info: array[0..63] of char;
  end;

  TMMIInfo = record
    Header: array[0..255] of char;
    SubHeader: array[0..255] of char;
    BottomLine: array[0..255] of char;
    Menuitem: array[0..8, 0..41] of char;
    ItemCount: integer;
    EnqFlag: LongBool;
    Blind_Answer: LongBool;
    Answer_Text_Length: integer;
    Prompt: array[0..255] of char;
    Answer: integer;
    AnswerStr: array[0..255] of char;
  end;

  IDVBSource = interface(IUnknown)
    ['{8430234F-D800-42A6-948E-237A0BE659A0}']
    function set_LNBType(LNBType, LNB1, LNB2: Integer): HRESULT; stdcall;
    function get_LNBType(out LNBType, LNB1, LNB2: Integer): HRESULT; stdcall;
    //[in]  1 for Horizotal, 0 for Vertical
    function LockChannel(Frequency, SymbolRate: DWORD; HV, Tone, DiSEqC: Integer): HRESULT; stdcall; // Mhz
    function SetPid(SetType, VideoPid1, AudioPid1: Integer; Scrambled: Boolean; PMTPid, ProgramNumber, VideoPid2, AudioPid2: Integer): HRESULT; stdcall;
    function ScanChannel: HRESULT; stdcall;
    function QueryScanStatus(out pStatus, pProgramnum: Integer): HRESULT; stdcall;
    function ReadChannel(pProgramValue: PInteger; pProgramValueSenior: PInteger): HRESULT; stdcall;
    function GetSignalState(out pbLockFlag: Boolean; out pStrength, pQuality: Integer): HRESULT; stdcall;
    function StartRecording(pFileName: PChar; VPid, APid: Integer): HRESULT; stdcall;
    function StopRecording: HRESULT; stdcall;
    function GetEPGCount(out pEPGNum: Integer): HRESULT; stdcall;
    function GetEPGList(pEPGValue: pointer): HRESULT; stdcall;
    function ResetEPG: HRESULT; stdcall;
    function InitTeleTextDataBuf(pBuf: PByte; BufSize: Integer; pWritePtr: PCardinal): HRESULT; stdcall;
    function SetTeleTextDataTempFilePath(pFilePath: PChar): HRESULT; stdcall;
    function InitSectionDataBuffer(pBuf: pointer; BufSize: DWORD; pWritePtr: pointer; Pid: DWORD): HRESULT; stdcall;
    function SetSectionPid(Pid: Integer): HRESULT; stdcall;
    function SetTSFile(pFileName: PChar): HRESULT; stdcall;
    function TSResume: HRESULT; stdcall;
    function AddPidFilter(pfPidFilter: TDVBSourceCallback; Pid: Integer): HRESULT; stdcall;
    function DelPidFilter(pfPidFilter: TDVBSourceCallback; Pid: Integer): HRESULT; stdcall;
    function ClearAllPidFilter: HRESULT; stdcall;
  end;

  IDVBSource2 = interface(IUnknown)
    ['{965AE5D0-C66B-4ca5-BB24-50CFDB240E29}']
    function SetTone(Tone: Integer): HRESULT; stdcall;
    function SetToneBurst(ToneBurst: Integer): HRESULT; stdcall;
    function SetDataBurst(DataBurst: Integer): HRESULT; stdcall;
    function SetDiSEqC(Frame, Adress, Command, Data: Integer): HRESULT; stdcall;
    function LockChannel2(Frequency, SymbolRate: Integer; HV: Integer; LNB: Integer): HRESULT; stdcall;
  end;

  IDVBSource4 = interface(IUnknown)
    ['{C9CB1F43-255E-45c8-A52E-CFE876757DFA}']
    function set_Power(Flag: Boolean): HRESULT; stdcall;
    function get_HWNum(out nCardNum: Integer): HRESULT; stdcall;
    function get_HWInfo(nCardSn: Integer; out Typ: THWType): HRESULT; stdcall;
    function get_CurrentHW(out nCardSN: Integer): HRESULT; stdcall;
    function set_CurrentHW(nCardSN: Integer): HRESULT; stdcall;
    function get_PlayBack(out bPlayBack: Boolean): HRESULT; stdcall;
    function set_PlayBack(bPlayBack: Boolean): HRESULT; stdcall;
    function set_PBFileName(Path: PChar): HRESULT; stdcall;
  end;

  IDVBSource5 = interface(IUnknown)
    ['{939E9505-030E-43e7-BEEA-D43B5A50F7E8}']
    function RCStart: HRESULT; stdcall;
    function ReadRCKey(pKey: PChar): HResult; stdcall;
    function RCStop: HRESULT; stdcall;
    function GetUSBSpeed(Speed: pChar): HRESULT; stdcall;
    function SetTunerPower(nPower: Integer): HRESULT; stdcall;
  end;

  IDVBDishDrv = interface(IUnknown)
    ['{3f91196e-c493-4eaa-b77f-eb1f443051cc}']
    function DriveEast(Step: WORD): HRESULT; stdcall;
    function DriveWest(Step: WORD): HRESULT; stdcall;
    function StopMotor: HRESULT; stdcall;
    function SetEastLimit: HRESULT; stdcall;
    function SetWestLimit: HRESULT; stdcall;
    function CancelLimit: HRESULT; stdcall;
    function EnableLimit: HRESULT; stdcall;
    function GotoZero: HRESULT; stdcall;
    function GotoSat(Degree, Longitude, Latitude: DOUBLE; pDegrees, pElevation: pointer): HRESULT; stdcall;
    function StoreSatPosition(n: WORD): HRESULT; stdcall;
    function GotoSatPosition(n: WORD): HRESULT; stdcall;
  end;

  IDVBCIMenu = interface(IUnknown)
    ['{39d4fb19-934d-4884-bc31-4a2d0db874da}']
    function GetCAMState(out pwCAM_Exist_Flag: Integer; out pwMMI_Info_Flag: Integer): HRESULT; stdcall;
    function GetAppInfo(var AppInfo: TAppInfo): HRESULT; stdcall;
    function InitMMI: HRESULT; stdcall;
    function GetMMI(var MMIInfo: TMMIInfo; var pwType: integer): HRESULT; stdcall;
    function Answer(var MMIInfo: TMMIInfo; wType: integer): HRESULT; stdcall;
    function CloseMMI: HRESULT; stdcall;
  end;

  IGetDeviceInfo2 = interface(IUnknown)
    ['{A46E99E5-0F47-49c9-A869-37F02CEEFB4F}']
    function GetDeviceInfo(out PCIConfig: TPCIConfig): HRESULT; stdcall;
  end;

  ISrcPidFilter = interface(IUnknown)
    ['{93E28118-C39F-4f14-87E7-FFA7D8030F90}']
    function AddPidFilterToList(lPid: Dword): HRESULT; stdcall;
    function RemovePidFilterFromList(bRemoveAll: Boolean; lPid: DWord): HRESULT; stdcall;
    function EnablePidFilter: HRESULT; stdcall;
    function GetMaxPidFilterNumber(out pnMaxPidFilterNumber: Integer): HRESULT; stdcall;
    function GetCurPidFilterList(pLPid: Pointer; out pnPidCount: Integer): HRESULT; stdcall;
    function GetCurPidFilterInfo(pPLDInfo: pointer): HRESULT; stdcall;
  end;

  function GetTwinhanDeviceName(ADeviceID: THWType): string;
  function GetTwinhanDeviceType(ADeviceID: THWType): Integer;
  function GetTwinhanDeviceTypeFromDeviceString(ADeviceID: string): THWType;

implementation

uses SysUtils;

function GetTwinhanDeviceName(ADeviceID: THWType): string;
begin
  case ADeviceID of
    HW_DST_FTA_CARD: Result := 'Twinhan Satellite FTA';
    HW_DST_CI_CARD: Result := 'Twinhan Satellite CI';
    HW_DCT_FTA_CARD: Result := 'Twinhan Cable FTA';
    HW_DCT_CI_CARD: Result := 'Twinhan Cable CI';
    HW_DTT_FTA_CARD: Result := 'Twinhan Terrestrial FTA';
    HW_DTT_CI_CARD: Result := 'Twinhan Terrestrial CI';
    HW_DTT_AD_CARD: Result := 'Twinhan Terrestrial AD';
    HW_MANTIS_FTA_CARD: Result := 'Twinhan Mantis FTA';
    HW_MANTIS_CI_CARD: Result := 'Twinhan Mantis CI';

   //MANTIS
   // Satellite
    HW_VP_10260: Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
    HW_VP_1026A: Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
    HW_VP_10270: Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
    HW_VP_1027A: Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
    HW_VP_10330: Result := 'Twinhan Mantis DVB-S (TDQS_F001F)';
    HW_VP_10340: Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';
    HW_VP_10410: Result := 'Twinhan Mantis DVB-S (Fujitsu MB86A15)';

   // Cable
    HW_VP_20230: Result := 'Twinhan DVB-C (Philips CU1216)';
    HW_VP_20331: Result := 'Twinhan DVB-C (Philips CU1216)';

    // Terrestrial
    HW_VP_30230: Result := 'Twinhan DVB-T (Motolora 44801/Zarlink MT352)';
    HW_VP_30240: Result := 'Twinhan DVB-T (Panasonic ENV57H12D5/Zarlink MT352)';
    HW_VP_30410: Result := 'Twinhan DVB-T (Philips TU1216)';
    HW_VP_40430: Result := 'Twinhan DVB-T';
    HW_M_MT352: Result := 'Twinhan DVB-T (Zarlink MT352)';

    HW_DTT_FTA_USB: Result := 'Twinhan DVB-T USB';
    HW_DST_FTA_USB: Result := 'Twinhan StarBox USB';
    HW_DTT_DIBCOM_USB: Result := 'Twinhan DVB-T USB/Dibcom';
    HW_DTT_BDA_AGENT_USB: Result := 'Twinhan DVB-T USB/BDA';
    else Result := 'Twinhan (Unknown)';
  end;
end;

function GetTwinhanDeviceType(ADeviceID: THWType): Integer;
begin
  case ADeviceID of
    HW_DST_FTA_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_DST_CI_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_DCT_FTA_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
    HW_DCT_CI_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
    HW_DTT_FTA_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_DTT_CI_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_DTT_AD_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_MANTIS_FTA_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_MANTIS_CI_CARD: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;

   //MANTIS
   // Satellite
    HW_VP_10260: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_1026A: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_10270: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_1027A: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_10330: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_10340: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_VP_10410: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;

   // Cable
    HW_VP_20230: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
    HW_VP_20331: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;

    // Terrestrial
    HW_VP_30230: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_VP_30240: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_VP_30410: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_VP_40430: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_M_MT352: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;

    HW_DTT_FTA_USB: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_DST_FTA_USB: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    HW_DTT_DIBCOM_USB: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    HW_DTT_BDA_AGENT_USB: Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    else Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN;
  end;
end;

function GetTwinhanDeviceTypeFromDeviceString(ADeviceID: string): THWType;
var
  n: string;
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

