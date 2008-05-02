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

unit TwinhanDriver;

interface

uses
  DCDVBTuningPlugins, TwinhanSDK, DCDVBTSDumpFilter, DirectShow9, ActiveX,
  SysUtils, DSUtil, Windows, DCDVBShared, Classes, Messages;

const
  CLSID_DCDVB_TwinhanWDMTuner: TGuid = '{295B3B36-E8CF-448D-93FD-E21FA81D1CAD}';

type
  TTwinhanDevice = class(TDCDVBTuningPluginDevice)
  protected
    FResult: Integer;
    FGraph: IFilterGraph2;
    FTuner: IBaseFilter;
    FTuning: IDVBSource;
    FPIDFilter: ISrcPidFilter;
    FDevices: IDVBSource4;
    FControl: IMediaControl;
    FDump: IBaseFilter;
    procedure OnTSData(ABuffer: PByte; ASize: Integer);
    function Activate: Integer;
    function DeActivate: Integer;
    function TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;
    function GetSignalStatistics(AStatistics: Pointer): Integer;
    function ProgramChanged(AProgram: PProgram): Integer;
    function OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; override;
    procedure Initialize; override;
  end;

  TTwinhanTuner = class(TDCDVBTuningPlugin)
  protected
    procedure Initialize; override;
  end;

implementation

(*** TTwinhanDevice ***********************************************************)

function TTwinhanDevice.OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  case AMessage of
    DCDVB_TUNING_PLUGIN_CTRL_ACTIVATE: Result := Activate;
    DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE: Result := DeActivate;
    DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST: Result := TuneRequest(Pointer(AParam1), AParam2);
    DCDVB_TUNING_PLUGIN_CTRL_GET_ACTIVATE_STATE: Result := _IF(FGraph = nil, DCDVB_TUNING_PLUGIN_INACTIVE, DCDVB_TUNING_PLUGIN_ACTIVE);
    DCDVB_TUNING_PLUGIN_CTRL_GET_SIGNAL_STATISTICS: Result := GetSignalStatistics(Pointer(AParam1));
    DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED: Result := ProgramChanged(PProgram(AParam1));
    else Result := E_NOTIMPL;
  end;
end;

procedure TTwinhanDevice.Initialize;
begin

end;

function TTwinhanDevice.Activate: Integer;

  function ClearInternal: Integer;
  begin
    FDevices := nil;
    FTuning := nil;
    FTuner := nil;
    FGraph := nil;
    FControl := nil;
    FDump := nil;
    Result := E_FAIL;
  end;

var
  hr: HRESULT;
  num_devices: Integer;
  i: Integer;
  dev_type: THWType;
  cur_type: THWType;
  found: Boolean;
  InPin, OutPin: IPin;
begin
  Log('Activate', 'Request to Activate the Device');
  if (FGraph <> nil) then
  begin
    Log('Activate', 'Device already active');
    Result := DCDVB_TUNING_PLUGIN_E_ALREADY_ACTIVE;
    Exit;
  end;

  cur_type := GetTwinhanDeviceTypeFromDeviceString(FDeviceID);
  if (cur_type = HW_ERROR) then
  begin
    Log('Activate', 'Failed recieving device Type for ' + FDeviceID);
    Result := ClearInternal;
    Exit;
  end;

  hr := CoCreateInstance(CLSID_FilterGraphNoThread, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FGraph);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed creating Filtergraph');
    Result := ClearInternal;
    Exit;
  end;

  hr := CoCreateInstance(CLSID_TwinhanDVBSource, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FTuner);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed creating Twinhan DVBSource Filter');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.AddFilter(FTuner, StringToOleStr('Twinhan Tuner'));
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed adding Twinhan DVBSource Filter to the Graph');
    Result := ClearInternal;
    Exit;
  end;

  hr := FTuner.QueryInterface(IID_IDVBSource, FTuning);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Tuning Interface not found in Twinhan DVBSource Filter');
    Result := ClearInternal;
    Exit;
  end;

  hr := FTuner.QueryInterface(IID_IDVBSource4, FDevices);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Devices Interface not found in Twinhan DVBSource Filter');
    Result := ClearInternal;
    Exit;
  end;

  if FTuner.QueryInterface(IID_ISrcPidFilter, FPIDFilter) = S_OK then
  begin
    FPIDFilter.EnablePidFilter;
    FPIDFilter.EnablePidFilter;
    FPIDFilter.RemovePidFilterFromList(True, 0);
    FPIDFilter.EnablePidFilter;
  end;

  hr := FDevices.get_HWNum(num_devices);
  if (hr <> S_OK) or (num_devices = 0) then
  begin
    Log('Activate', 'Failed receiving Device Count from Twinhan DVBSource Filter');
    Result := ClearInternal;
    Exit;
  end;

  found := False;

  for i := 0 to num_devices - 1 do
  begin
    hr := FDevices.get_HWInfo(i + 1, dev_type);
    if (hr = S_OK) and (dev_type <> HW_ERROR) and (dev_type = cur_type) then
    begin
      hr := FDevices.set_CurrentHW(i + 1);
      if (hr = S_OK) then
      begin
        found := True;
        break;
      end;
    end;
  end;

  if (not found) then
  begin
    Log('Activate', 'Failed to find the Device in Twinhan DVBSource Filter');
    Result := ClearInternal;
    Exit;
  end;

  FDump := TDCDVBTSDumpFilter.Create;
  if (FDump = nil) then
  begin
    Log('Activate', 'Failed creating TS Stream Filter');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.AddFilter(FDump, StringToOleStr('TS Dump'));
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed adding TS Stream Filter to the Graph');
    Result := ClearInternal;
    Exit;
  end;

  (FDump as IDCDVBTSDump).SetCallback(OnTSData);

  InPin := GetInPin(FDump, 0);
  FTuner.FindPin('Transponder Stream', outPin);

  hr := FGraph.ConnectDirect(outPin, inPin, nil);
  if hr <> S_OK then
  begin
    Log('Activate', 'Failed connecting Tuner to TS Stream Filter');
    Result := ClearInternal;
    Exit;
  end;
  InPin := nil;
  OutPin := nil;

  hr := FGraph.QueryInterface(IID_IMediaControl, FControl);
  if hr <> S_OK then
  begin
    Log('Activate', 'IMediaControl not found');
    Result := ClearInternal;
    Exit;
  end;

  Log('Activate', 'Activating succeeded');

  FDevices.set_Power(True);
  FControl.Run;

  Result := S_OK;
end;

function TTwinhanDevice.DeActivate: Integer;
begin
  Log('DeActivate', 'Request to Deactivate the Device');
  if (FGraph = nil) then
  begin
    Log('DeActivate', 'Device is already DeActivated');
    Result := DCDVB_TUNING_PLUGIN_E_ALREADY_INACTIVE;
    Exit;
  end;

  if Assigned(FControl) then
  begin
    Log('DeActivate', 'Stopping the Device');
    FControl.Stop;
    FControl := nil;
  end;
  if Assigned(FPIDFilter) then
  begin
    FPIDFilter.RemovePidFilterFromList(True, 0);
    FPIDFilter := nil;
  end;
  FTuning := nil;
  FDevices := nil;
  FTuner := nil;
  FDump := nil;
  FGraph := nil;

  Log('DeActivate', 'Device stopped');
  Result := S_OK;
end;

function TTwinhanDevice.TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;
var
  pol: Integer;
  hr: HRESULT;
begin
  Log('TuneRequest', 'Request to change the Frequency/Transponder');
  if (ATuneRequest = nil) then
  begin
    Log('TuneRequest', 'No TuningSpace present');
    Result := E_POINTER;
    Exit;
  end;

  if (FGraph = nil) then
  begin
    Log('TuneRequest', 'Not Active');
    Result := DCDVB_TUNING_PLUGIN_E_INACTIVE;
    Exit;
  end;

  if (FTuning = nil) then
  begin
    Log('TuneRequest', 'Tuning Interface not present');
    Result := E_FAIL;
    Exit;
  end;

  if (ATuneRequestType = DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN) then
  begin
    Log('TuneRequest', 'TuneRequestType is Unknown');
    Result := E_INVALIDARG;
    Exit;
  end;

  if (ATuneRequestType <> FDeviceType) then
  begin
    Log('TuneRequest', 'TuneRequestType is different from DeviceType');
    Result := E_INVALIDARG;
    Exit;
  end;

  if (PBaseTuneRequest(ATuneRequest).DeviceType <> ATuneRequestType) then
  begin
    Log('TuneRequest', 'DeviceType from TuneRequest is different from DeviceType');
    Result := E_INVALIDARG;
    Exit;
  end;

  case ATuneRequestType of
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:
    begin
      with PDVBTTuneRequest(ATuneRequest)^ do
      begin
        Log('TuneRequest', 'DVBt -  Frequency: ' + inttostr(Frequency) + ' Bandwidth: ' + inttostr(Bandwidth));
        hr := FTuning.LockChannel(Frequency, Bandwidth, 1, 0, 0);
        Log('TuneRequest', 'DVBt - LockChannel returned ' + inttohex(hr, 8));
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:
    begin
      with PDVBCTuneRequest(ATuneRequest)^ do
      begin
        Log('TuneRequest', 'DVBc -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate));
        hr := FTuning.LockChannel(Frequency, SymbolRate, 0, 0, 0);
        Log('TuneRequest', 'DVBc - LockChannel returned ' + inttohex(hr, 8));
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:
    begin
      with PDVBSTuneRequest(ATuneRequest)^ do
      begin
        Log('TuneRequest', 'DVBs -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate) + ' Polarization: ' + inttostr(Polarization));
        pol := 0;

        if Assigned(FPIDFilter) then
        begin
          FPIDFilter.GetMaxPidFilterNumber(pol);

          FPIDFilter.RemovePidFilterFromList(True, 0);
          FPIDFilter.AddPidFilterToList(0);
          FPIDFilter.AddPidFilterToList($10); // PSI_PID_NIT
          FPIDFilter.AddPidFilterToList($11); // PSI_PID_SDT
          FPIDFilter.AddPidFilterToList($12); // PSI_PID_EIT
        end;

        case Polarization of
          DVBS_POLARIZATION_HORIZONTAL: pol := 1;
          DVBS_POLARIZATION_VERTICAL: pol := 0;
        end;

        FDevices.set_Power(True);
        FTuning.set_LNBType(1, 9750, 11600);

        hr := FTuning.LockChannel(Frequency div 1000, SymbolRate, pol, 0, 0);
        Log('TuneRequest', 'DVBs - LockChannel returned ' + inttohex(hr, 8));
      end;
    end;
  end;

  Result := S_OK;
end;

function TTwinhanDevice.GetSignalStatistics(AStatistics: Pointer): Integer;
var
  locked: Boolean;
  strength: Integer;
  quality: Integer;
  hr: HRESULT;
begin
  Log('GetSignalStatistics', 'Receiving Statistics');
  if not Assigned(AStatistics) then
  begin
    Log('GetSignalStatistics', 'Statistics is NULL');
    Result := E_POINTER;
    Exit;
  end;

  if (FGraph = nil) then
  begin
    Log('GetSignalStatistics', 'Graph is NULL');
    Result := DCDVB_TUNING_PLUGIN_E_INACTIVE;
    Exit;
  end;

  if (FTuning = nil) then
  begin
    Log('GetSignalStatistics', 'Tuning Interface is NULL');
    Result := E_FAIL;
    Exit;
  end;

  if PSignalStatistics(AStatistics)^.Size <> SizeOf(TSignalStatistics) then
  begin
    Log('GetSignalStatistics', 'Statistics definition is unknown');
    Result := E_FAIL;
    Exit;
  end;

  with PSignalStatistics(AStatistics)^ do
  begin
    SignalLocked := False;
    SignalPresent := False;
    SignalStrength := 0;
    SignalQuality := 0;
  end;

  Log('GetSignalStatistics', 'Receiving Statistics from Tuner');
  hr := FTuning.GetSignalState(locked, strength, quality);
  Log('GetSignalStatistics', 'Receiving Statistics returned: ' + inttohex(hr, 8));
  if hr <> S_OK then
  begin
    Result := E_FAIL;
    Exit;
  end;

  with PSignalStatistics(AStatistics)^ do
  begin
    SignalLocked := locked;
    SignalPresent := locked;
    SignalStrength := strength;
    SignalQuality := strength;

    Log('GetSignalStatistics', 'SignalPresent: ' + inttostr(Integer(SignalPresent)) +
      ' SignalLocked: ' + inttostr(Integer(SignalLocked)) +
      ' SignalStrength: ' + inttostr(SignalStrength) +
      ' SignalQuality: ' + inttostr(SignalQuality));
  end;

  Result := S_OK;
end;

function TTwinhanDevice.ProgramChanged(AProgram: PProgram): Integer;
var
  i: Integer;
begin
  if Assigned(FPIDFilter) then
  begin
    FPIDFilter.RemovePidFilterFromList(True, 0);
//    FPIDFilter.AddPidFilterToList(0);
//    FPIDFilter.AddPidFilterToList($10); // PSI_PID_NIT
//    FPIDFilter.AddPidFilterToList($11); // PSI_PID_SDT
    FPIDFilter.AddPidFilterToList($12); // PSI_PID_EIT

    FPIDFilter.AddPidFilterToList(AProgram.PCRPID);
    FPIDFilter.AddPidFilterToList(AProgram.PMTPID);
    for i := 0 to AProgram.NumPIDs -1 do
    begin
      FPIDFilter.AddPidFilterToList(AProgram.PIDs[i]);
    end;
  end;

  Result :=  S_OK;
end;

procedure TTwinhanDevice.OnTSData(ABuffer: PByte; ASize: Integer);
begin
  PushData(ABuffer, ASize);
end;

(*** TTwinhanTuner ************************************************************)

procedure TTwinhanTuner.Initialize;
var
  filter: IBaseFilter;
  hr: HRESULT;
  device: IDCDVBTuningPluginDevice;
  device_intf: IDVBSource4;
  hw_num, i: Integer;
  hw_type: THWType;
begin
  hr := CoCreateInstance(CLSID_TwinhanDVBSource, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, filter);
  if hr = S_OK then
  begin
    hr := filter.QueryInterface(IID_IDVBSource4, device_intf);
    if hr = S_OK then
    begin
      hw_num := 0;
      hr := device_intf.get_HWNum(hw_num);
      if hr = S_OK then
      begin
        for i := 0 to hw_num - 1 do
        begin
          hr := device_intf.get_HWInfo(i + 1, hw_type);
          if (hr = S_OK) and (hw_type <> HW_ERROR) then
          begin
            device := TTwinhanDevice.Create(
              GetTwinhanDeviceName(hw_type),
              GetTwinhanDeviceType(hw_type),
              TWINHAN_DEVICE_ID + inttostr(integer(hw_type))
            );
            FDeviceList.Add(device);
          end;
        end;
      end;
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TTwinhanTuner, // Tuner Class Name
    CLSID_DCDVB_TwinhanWDMTuner, // Tuner CLSID
    'Twinhan WDM Wrapper', // Name
    'Twinhan WDM Plugin for DC-DVB Source', // Description
    'Milenko Mitrovic <dcoder@dsp-worx.de>', // Author
    '0.0.0.5' // Version
  );

end.

