(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004, 2005 Milenko Mitrovic                                *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  SDK for DC-DVB Filter Version 0.1.6                                      *
 *                                                                           *
 *  The Source Code is given "as is" without warranty of any kind. The       *
 *  Author is not responsible for any damage due to the use of this Code.    *
 *  The complete Source Code remains property of the Author and must be      *
 *  used only for creating Plugins for the DC-DVB Filter.                    *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit BDADriver;

interface

uses
  DCDVBTSDumpFilter, DirectShow9, ActiveX, DSUtil, Windows, SysUtils, Classes,
  DCDVBTuningPlugins, PropertyPages, DCDVBShared;

const
  CLSID_DCDVB_BDATuner: TGuid  = '{5877EAB7-406B-4CF4-AB83-545E6BA8462B}';

type
  TBDADevice = class(TDCDVBTuningPluginDevice, IBDADriver)
  protected
    FPushData: Boolean;
    FGraph: IFilterGraph2;
    FNetworkProvider: IBaseFilter;
    FTuner: IBaseFilter;
    FDump: IBaseFilter;
    FControl: IMediaControl;
    FTIFID: Cardinal;
    FSignalStatistics: IInterfaceList;
    FDeviceControl: IInterfaceList;
    FFrequencyFilter: IInterfaceList;
    FDigitalDemodulator: IInterfaceList;
    FLNB: IInterfaceList;
    FDirectTuning: Boolean;
    procedure OnTSData(ABuffer: PByte; ASize: Integer);
    function ClearInternal: Integer;
    function Activate: Integer;
    function DeActivate: Integer;
    function TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;
    function GetSignalStatistics(AStatistics: Pointer): Integer;
    function LoadSettings(ABuffer: PByte; ASize: Integer): Integer;
    function SaveSettings(out ABuffer: PByte; out ASize: Integer): Integer;
    function OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; override;
    procedure Initialize; override;
    // IBDADriver
    function get_DirectTuning(out ADirectTuning: BOOL): HRESULT; stdcall;
    function put_DirectTuning(ADirectTuning: BOOL): HRESULT; stdcall;
  end;

  TBDATuner = class(TDCDVBTuningPlugin)
  protected
    procedure Initialize; override;
  end;

implementation

const
  DVBS_TUNING_SPACE = 'DC-DVBs';
  DVBC_TUNING_SPACE = 'DC-DVBc';
  DVBT_TUNING_SPACE = 'DC-DVBt';
  ATSC_TUNING_SPACE = 'DC-ATSC';
  
(*** TBDADevice ***************************************************************)

procedure TBDADevice.Initialize;
begin
  AddPropertyPage(CLSID_PropPageSettings);
  FPushData := False;
end;

function TBDADevice.OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  case AMessage of
    DCDVB_TUNING_PLUGIN_CTRL_ACTIVATE:                Result := Activate;
    DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE:              Result := DeActivate;
    DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST:             Result := TuneRequest(Pointer(AParam1), AParam2);
    DCDVB_TUNING_PLUGIN_CTRL_GET_ACTIVATE_STATE:      Result := _IF(FGraph = nil, DCDVB_TUNING_PLUGIN_INACTIVE, DCDVB_TUNING_PLUGIN_ACTIVE);
    DCDVB_TUNING_PLUGIN_CTRL_GET_SIGNAL_STATISTICS:   Result := GetSignalStatistics(Pointer(AParam1));
    DCDVB_TUNING_PLUGIN_CTRL_LOAD_SETTINGS:           Result := LoadSettings(PByte(AParam1), AParam2);
    DCDVB_TUNING_PLUGIN_CTRL_SAVE_SETTINGS:           Result := SaveSettings(PPByte(AParam1)^, PInteger(AParam2)^);
    else                                              Result := E_NOTIMPL;
  end;
end;

function TBDADevice.Activate: Integer;

  function GetFilterFromSysEnum(Category: TGuid; Name: WideString; ID: WideString): IBaseFilter;
  var
    sysenum: TSysDevEnum;
    i: integer;
    did: PWideChar;
  begin
    Result := nil;

    sysenum := TSysDevEnum.Create(Category);
    for i := 0 to sysenum.CountFilters -1 do
    begin
      if (sysenum.Filters[i].FriendlyName = Name) then
      begin
        sysenum.GetMoniker(i).GetDisplayName(nil, nil, did);
        if Assigned(did) then
        begin
          if (did = ID) then
          begin
            Result := sysenum.GetBaseFilter(i);
            CoTaskMemFree(did);
            break;
          end else
            CoTaskMemFree(did);
        end;
      end;
    end;
    sysenum.free;
  end;

  function GetTopology(AGraph: IFilterGraph2; ATopology: TGuid): IInterfaceList;
  var
    enum: IEnumFilters;
    filter: IBaseFilter;
    topology: IBDA_Topology;
    nodetypeslist: array[0..9] of Cardinal;
    nodetypes: Cardinal;
    nodeinterfaceslist: array[0..9] of TGuid;
    nodeinterfaces: Cardinal;
    i, c: Integer;
    unk: IUnknown;
    bda: IUnknown;
  begin
    Result := TInterfaceList.Create;
    if AGraph.EnumFilters(enum) = S_OK then
    begin
      while (enum.Next(1, filter, nil) = S_OK) do
      begin
        try
          if (filter.QueryInterface(IID_IBDA_Topology, topology) = S_OK) then
            if topology.GetNodeTypes(nodetypes, 10, @nodetypeslist) = S_OK then
              if nodetypes > 0 then
                for i := 0 to nodetypes -1 do
                  if topology.GetNodeInterfaces(nodetypeslist[i], nodeinterfaces, 10, @nodeinterfaceslist) = S_OK then
                    if nodeinterfaces > 0 then
                      for c := 0 to nodeinterfaces -1 do
                        if IsEqualGUID(nodeinterfaceslist[c], ATopology) then
                          if topology.GetControlNode(0, 1, nodetypeslist[i], unk) = S_OK then
                            if unk.QueryInterface(ATopology, bda) = S_OK then
                            begin
                              Result.Add(bda);
                            end;
        finally
          unk := nil;
          topology := nil;
        end;
        filter := nil;
      end;
      enum := nil;
    end;
  end;

  function GetDeviceControl(AGraph: IFilterGraph2): IInterfaceList;
  var
    enum: IEnumFilters;
    filter: IBaseFilter;
    bda: IBDA_DeviceControl;
    class_id: TGuid;
  begin
    Result := TInterfaceList.Create;
    if AGraph.EnumFilters(enum) = S_OK then
    begin
      while (enum.Next(1, filter, nil) = S_OK) do
      begin
        filter.GetClassID(class_id);
        if not IsEqualGUID(class_id, CLSID_MPEG2Demultiplexer) then
        begin
          if filter.QueryInterface(IID_IBDA_DeviceControl, bda) = S_OK then
          begin
            Result.Add(bda);
          end;
          bda := nil;
        end;
        filter := nil;
      end;
      enum := nil;
    end;
  end;

var
  hr: HRESULT;
  tuner: ITuner;
  tuning_space: ITuningSpace;
  tune_request: ITuneRequest;
  enum: TSysDevEnum;
  i: Integer;
  filter: IBaseFilter;
  receiver: IBaseFilter;
  p1, p2: IPin;
  s: IUnknown;
  tuning_spaces: IEnumTuningSpaces;
  fetched: Cardinal;
  name: WideString;
  locator: ILocator;
begin
  Log('Activate', 'Request to Activate the Device');
  if (FGraph <> nil) then
  begin
    Log('Activate', 'Device already active');
    Result := DCDVB_TUNING_PLUGIN_E_ALREADY_ACTIVE;
    Exit;
  end;

  hr := CoCreateInstance(CLSID_FilterGraphNoThread, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FGraph);
//  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FGraph);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed creating Filtergraph');
    Result := ClearInternal;
    Exit;
  end;

  case FDeviceType of
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:  hr := CoCreateInstance(CLSID_DVBTNetworkProvider, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FNetworkProvider);
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:  hr := CoCreateInstance(CLSID_DVBCNetworkProvider, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FNetworkProvider);
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:  hr := CoCreateInstance(CLSID_DVBSNetworkProvider, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FNetworkProvider);
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:  hr := CoCreateInstance(CLSID_ATSCNetworkProvider, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, FNetworkProvider);
    else                   hr := E_FAIL;
  end;
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed creating Network Provider');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.AddFilter(FNetworkProvider, StringToOleStr('Network Provider'));
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed adding Network Provider to the Graph');
    Result := ClearInternal;
    Exit;
  end;

  hr := FNetworkProvider.QueryInterface(IID_ITuner, tuner);
  if hr = S_OK then
  begin
    if tuner.EnumTuningSpaces(tuning_spaces) = S_OK then
    begin
      while tuning_spaces.Next(1, tuning_space, fetched) = S_OK do
      begin
        tuning_space.get_UniqueName(name);
        case FDeviceType of
          DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:
          begin
            if name = DVBT_TUNING_SPACE then
            begin
              tuner.put_TuningSpace(tuning_space);
              break;
            end;
          end;
          DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:
          begin
            if name = DVBS_TUNING_SPACE then
            begin
              tuner.put_TuningSpace(tuning_space);
              break;
            end;
          end;
          DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:
          begin
            if name = DVBC_TUNING_SPACE then
            begin
              tuner.put_TuningSpace(tuning_space);
              break;
            end;
          end;
          DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:
          begin
            if name = ATSC_TUNING_SPACE then
            begin
              tuner.put_TuningSpace(tuning_space);
              break;
            end;
          end;
        end;
      end;
    end;

    if Assigned(tuning_space) then
    begin
      tuner.put_TuningSpace(tuning_space);
      hr := tuning_space.get_DefaultLocator(locator);
      if hr <> S_OK
        then locator := nil;
      hr := tuning_space.CreateTuneRequest(tune_request);
      if hr = S_OK then
      begin
        if Assigned(locator) then
        begin
          tune_request.put_Locator(locator);
        end;
        tuner.put_TuneRequest(tune_request);
      end;
    end;
  end;

  Log('Activate', 'Searching for Device: ' + FDeviceName);
  Log('Activate', 'with Device ID: ' + FDeviceID);
  FTuner := GetFilterFromSysEnum(KSCATEGORY_BDA_NETWORK_TUNER, FDeviceName, FDeviceID);
  if (not Assigned(FTuner)) then
  begin
    Log('Activate', 'Failed creating Tuner');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.AddFilter(FTuner, PWideChar(WideString(FDeviceName)));
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed adding Tuner to the Graph');
    Result := ClearInternal;
    Exit;
  end;

  p1 := GetOutPin(FNetworkProvider, 0);
  if (not Assigned(p1)) then
  begin
    Log('Activate', 'Failed getting Output Pin from Network Provider');
    Result := ClearInternal;
    Exit;
  end;

  p2 := GetInPin(FTuner, 0);
  if (not Assigned(p2)) then
  begin
    Log('Activate', 'Failed getting Input Pin from Tuner');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.ConnectDirect(p1, p2, nil);
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed connecting Network Provider with Tuner');
    Result := ClearInternal;
    Exit;
  end;

  receiver := FTuner;
  enum := TSysDevEnum.Create(KSCATEGORY_BDA_RECEIVER_COMPONENT);

  for i := 0 to enum.CountFilters -1 do
  begin
    filter := enum.GetBaseFilter(i);
    FGraph.AddFilter(filter, PWideChar(WideString(enum.Filters[i].FriendlyName)));
    if (FGraph.ConnectDirect(GetOutPin(receiver, 0), GetInPin(filter, 0), nil) <> S_OK) then
    begin
      FGraph.RemoveFilter(filter);
    end else
    begin
      receiver := filter;
    end;
    filter := nil;
  end;

  for i := 0 to enum.CountFilters -1 do
  begin
    filter := enum.GetBaseFilter(i);
    FGraph.AddFilter(filter, PWideChar(WideString(enum.Filters[i].FriendlyName)));
    if (FGraph.ConnectDirect(GetOutPin(receiver, 0), GetInPin(filter, 0), nil) <> S_OK) then
    begin
      FGraph.RemoveFilter(filter);
    end else
    begin
      receiver := filter;
    end;
  end;
  filter := nil;

  enum.Free;

  FDump := TDCDVBTSDumpFilter.Create;
  if (not Assigned(FDump)) then
  begin
    Log('Activate', 'Failed creating TS Stream Filter');
    Result := ClearInternal;
    Exit;
  end;

  hr := FGraph.AddFilter(FDump, PWideChar(WideString('TS Dump')));
  if (hr <> S_OK) then
  begin
    Log('Activate', 'Failed adding TS Stream Filter to the Graph');
    Result := ClearInternal;
    Exit;
  end;

  (FDump as IDCDVBTSDump).SetCallback(OnTSData);

  if (FGraph.ConnectDirect(GetOutPin(receiver, 0), GetInPin(FDump, 0), nil) <> S_OK) then
  begin
    Log('Activate', 'Failed connecting Tuner with TS Stream Filter');
    Result := ClearInternal;
    Exit;
  end;

  Log('Activate', 'Registering TS Stream Filter with Network Provider');
  (FNetworkProvider as IBDA_TIF_REGISTRATION).RegisterTIFEx(GetInPin(FDump, 0), FTIFID, s);
  s := nil;

  Log('Activate', 'Searching for Tuning Interfaces');
  FSignalStatistics := GetTopology(FGraph, IID_IBDA_SignalStatistics);
  FDeviceControl := GetDeviceControl(FGraph);
  FFrequencyFilter := GetTopology(FGraph, IID_IBDA_FrequencyFilter);
  FDigitalDemodulator := GetTopology(FGraph, IID_IBDA_DigitalDemodulator);
  FLNB := GetTopology(FGraph, IID_IBDA_LNBInfo);

  hr := FGraph.QueryInterface(IID_IMediaControl, FControl);
  if hr <> S_OK then
  begin
    Log('Activate', 'Control Interface not found');
    Result := ClearInternal;
    Exit;
  end;

  try
    FControl.Run;
  except on E: Exception do
  end;

  Log('Activate', 'Activating succeeded');
  Result := S_OK;
end;

function TBDADevice.ClearInternal: Integer;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
  enum_pins: TPinList;
  i: Integer;
begin
  Result := E_FAIL;

  if Assigned(FGraph) then
  begin
    if FGraph.EnumFilters(enum) = S_OK then
    begin
      while enum.Next(1, filter, nil) = S_OK do
      begin
        try
          filter.Stop;
        except
        end;
        try
          enum_pins := TPinList.Create(filter);
          for i := 0 to enum_pins.Count -1 do
          begin
            enum_pins[i].Disconnect;
          end;
          enum_pins.Free;
          filter := nil;
        except
        end;
      end;
    end;
    if FGraph.EnumFilters(enum) = S_OK then
    begin
      while enum.Next(1, filter, nil) = S_OK do
      begin
        try
          FGraph.RemoveFilter(filter);
          enum.Reset;
          filter := nil;
        except
        end;
      end;
    end;
  end;

  if Assigned(FControl) then
  begin
    try
      FControl.Stop;
      FControl := nil;
    except
      Pointer(FControl) := nil;
    end;
  end;

  if Assigned(FDump) then
  begin
    (FDump as IDCDVBTSDump).SetCallback(nil);
    FDump := nil;
  end;

  FSignalStatistics := nil;
  FDeviceControl := nil;
  FFrequencyFilter := nil;
  FDigitalDemodulator := nil;
  FLNB := nil;

  FTuner := nil;
  try
    if Assigned(FNetworkProvider) then
    begin
      if FTIFID <> 0 then
      begin
        (FNetworkProvider as IBDA_TIF_REGISTRATION).UnregisterTIF(FTIFID);
        FNetworkProvider := nil;
      end;
    end;
  except
  end;

  FGraph := nil;
  FTIFID := 0;
end;

function TBDADevice.DeActivate: Integer;
begin
  Log('DeActivate', 'Request to Deactivate the Device');
  if (FGraph = nil) then
  begin
    Log('DeActivate', 'Device is already DeActivated');
    Result := DCDVB_TUNING_PLUGIN_E_ALREADY_INACTIVE;
    Exit;
  end;

  Log('DeActivate', 'Stopping the Device');
  ClearInternal;

  Log('DeActivate', 'Device stopped');
  Result := S_OK;
end;

function TBDADevice.TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;

  procedure StartChanges;
  var
    i: Integer;
    hr: HRESULT;
  begin
    Log('TuneRequest', 'Calling StartChanges');
    for i := 0 to FDeviceControl.Count -1 do
    begin
      hr := (FDeviceControl[i] as IBDA_DeviceControl).StartChanges;
      Log('TuneRequest', 'Calling StartChanges ' + inttostr(i) + ' returned ' + inttohex(hr, 8));
    end;
  end;

  procedure CommitChanges;
  var
    i: Integer;
    hr: HRESULT;
  begin
    Log('TuneRequest', 'Calling CommitChanges');
    for i := 0 to FDeviceControl.Count -1 do
    begin
      hr := (FDeviceControl[i] as IBDA_DeviceControl).CommitChanges;
      Log('TuneRequest', 'Calling CommitChanges ' + inttostr(i) + ' returned ' + inttohex(hr, 8));
    end;
  end;

var
  i: Integer;
  sr: Cardinal;
  tuning_space: ITuningSpace;
  locator: ILocator;
  tune_request: ITuneRequest;
  hr: HRESULT;
begin
  Result := E_UNEXPECTED;

  Log('TuneRequest', 'Request to change the Frequency/Transponder');
  FPushData := False;

  try
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

    if (FDeviceControl = nil) then
    begin
      Log('TuneRequest', 'DeviceControl = NULL');
      Result := E_FAIL;
      Exit;
    end;

    if (FFrequencyFilter = nil) then
    begin
      Log('TuneRequest', 'FrequencyFilter = NULL');
      Result := E_FAIL;
      Exit;
    end;

    if (FDigitalDemodulator = nil) then
    begin
      Log('TuneRequest', 'DigitalDemodulator = NULL');
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

    if not FDirectTuning then
    begin
      Log('TuneRequest', 'Using MS Network Provider for Tuning');
      Log('TuneRequest', 'Getting Tuning Space from Network Provider');
      hr := (FNetworkProvider as ITuner).get_TuningSpace(tuning_space);
      Log('TuneRequest', 'ITuner::get_TuningSpace returned ' + inttohex(hr, 8));
      if hr <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;

      Log('TuneRequest', 'Getting default Locator from Tuning Space');
      hr := tuning_space.get_DefaultLocator(locator);
      Log('TuneRequest', 'tuning_space::get_DefaultLocator returned ' + inttohex(hr, 8));
      if hr <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;

      Log('TuneRequest', 'Creating TuneRequest');
      hr := tuning_space.CreateTuneRequest(tune_request);
      Log('TuneRequest', 'tuning_space::CreateTuneRequest returned ' + inttohex(hr, 8));
      if hr <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;
    end else
    begin
      Log('TuneRequest', 'Using Tuning on BDA Interfaces');
    end;

    case ATuneRequestType of
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:
      begin
        with PDVBTTuneRequest(ATuneRequest)^ do
        begin
          if FDirectTuning then
          begin
            StartChanges;

            for i := 0 to FFrequencyFilter.Count -1 do
            begin
              Log('TuneRequest', 'DVBt -  Frequency: ' + inttostr(Frequency) + ' Bandwidth: ' + inttostr(Bandwidth));
              with (FFrequencyFilter[i] as IBDA_FrequencyFilter) do
              begin
                Log('TuneRequest', 'DVBt - Setting up Frequency Multiplier to 1000');
                hr := put_FrequencyMultiplier(1000);
                Log('TuneRequest', 'DVBt - put_FrequencyMultiplier returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBt - Setting up Frequency');
                hr := put_Frequency(Frequency);
                Log('TuneRequest', 'DVBt - put_Frequency returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBt - Setting up Bandwidth');
                hr := put_Bandwidth(Bandwidth);
                Log('TuneRequest', 'DVBt - put_Bandwidth returned ' + inttohex(hr, 8));
              end;
            end;

            CommitChanges;
          end else
          begin
            with IDVBTLocator(locator) do
            begin
              Log('TuneRequest', 'DVBt -  Frequency: ' + inttostr(Frequency) + ' Bandwidth: ' + inttostr(Bandwidth));
              Log('TuneRequest', 'DVBt - Setting up Frequency');
              hr := put_CarrierFrequency(Frequency);
              Log('TuneRequest', 'DVBt - locator::put_CarrierFrequency returned ' + inttohex(hr, 8));
              Log('TuneRequest', 'DVBt - Setting up Bandwidth');
              hr := put_Bandwidth(Bandwidth);
              Log('TuneRequest', 'DVBt - locator::put_Bandwidth returned ' + inttohex(hr, 8));
            end;
          end;
        end;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:
      begin
        with PDVBCTuneRequest(ATuneRequest)^ do
        begin
          if FDirectTuning then
          begin
            StartChanges;

            Log('TuneRequest', 'DVBc -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate));
            for i := 0 to FFrequencyFilter.Count -1 do
            begin
              with (FFrequencyFilter[i] as IBDA_FrequencyFilter) do
              begin
                Log('TuneRequest', 'DVBc - Setting up Frequency Multiplier to 1000');
                hr := put_FrequencyMultiplier(1000);
                Log('TuneRequest', 'DVBc - put_FrequencyMultiplier returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBc - Setting up Frequency');
                hr := put_Frequency(Frequency);
                Log('TuneRequest', 'DVBc - put_Frequency returned ' + inttohex(hr, 8));
              end;
            end;

            for i := 0 to FDigitalDemodulator.Count -1 do
            begin
              with (FDigitalDemodulator[i] as IBDA_DigitalDemodulator) do
              begin
                Log('TuneRequest', 'DVBc - Setting up SymbolRate');
                sr := SymbolRate;
                hr := put_SymbolRate(sr);
                Log('TuneRequest', 'DVBc - put_SymbolRate returned ' + inttohex(hr, 8));
              end;
            end;

            CommitChanges;
          end else
          begin
            with IDVBCLocator(locator) do
            begin
              Log('TuneRequest', 'DVBc -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate));
              Log('TuneRequest', 'DVBc - Setting up Frequency');
              hr := put_CarrierFrequency(Frequency);
              Log('TuneRequest', 'DVBc - locator::put_CarrierFrequency returned ' + inttohex(hr, 8));
              Log('TuneRequest', 'DVBc - Setting up SymbolRate');
              hr := put_SymbolRate(SymbolRate);
              Log('TuneRequest', 'DVBc - locator::put_SymbolRate returned ' + inttohex(hr, 8));
            end;
          end;
        end;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:
      begin
        with PDVBSTuneRequest(ATuneRequest)^ do
        begin
          if FDirectTuning then
          begin
            StartChanges;

            Log('TuneRequest', 'DVBs -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate) + ' Polarization: ' + inttostr(Polarization));
            for i := 0 to FFrequencyFilter.Count -1 do
            begin
              with (FFrequencyFilter[i] as IBDA_FrequencyFilter) do
              begin
                Log('TuneRequest', 'DVBs - Setting up Frequency Multiplier to 1000');
                hr := put_FrequencyMultiplier(1000);
                Log('TuneRequest', 'DVBs - put_FrequencyMultiplier returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up Frequency');
                hr := put_Frequency(Frequency);
                Log('TuneRequest', 'DVBs - put_Frequency returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up Polarization');
                case Polarization of
                  DVBS_POLARIZATION_HORIZONTAL: hr := put_Polarity(BDA_POLARISATION_LINEAR_H);
                  DVBS_POLARIZATION_VERTICAL:   hr := put_Polarity(BDA_POLARISATION_LINEAR_V);
                end;
                Log('TuneRequest', 'DVBs - put_Polarity returned ' + inttohex(hr, 8));
              end;
            end;

            for i := 0 to FDigitalDemodulator.Count -1 do
            begin
              with (FDigitalDemodulator[i] as IBDA_DigitalDemodulator) do
              begin
                Log('TuneRequest', 'DVBs - Setting up SymbolRate');
                sr := SymbolRate;
                hr := put_SymbolRate(sr);
                Log('TuneRequest', 'DVBs - put_SymbolRate returned ' + inttohex(hr, 8));
              end;
            end;

            for i := 0 to FLNB.Count -1 do
            begin
              with (FLNB[i] as IBDA_LNBInfo) do
              begin
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_LocalOscilatorFrequencyLowBand');
                hr := put_LocalOscilatorFrequencyLowBand(9750 * 1000);
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_LocalOscilatorFrequencyLowBand returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_LocalOscilatorFrequencyHighBand');
                hr := put_LocalOscilatorFrequencyHighBand(10600 * 1000);
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_LocalOscilatorFrequencyHighBand returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_HighLowSwitchFrequency');
                hr := put_HighLowSwitchFrequency(11600 * 1000);
                Log('TuneRequest', 'DVBs - Setting up LNBInfo::put_HighLowSwitchFrequency returned ' + inttohex(hr, 8));
              end;
            end;

            CommitChanges;
          end else
          begin
            with IDVBSLocator(locator) do
            begin
              Log('TuneRequest', 'DVBs -  Frequency: ' + inttostr(Frequency) + ' SymbolRate: ' + inttostr(SymbolRate) + ' Polarization: ' + inttostr(Polarization));
              Log('TuneRequest', 'DVBs - Setting up Frequency');
              hr := put_CarrierFrequency(Frequency);
              Log('TuneRequest', 'DVBs - locator::put_CarrierFrequency returned ' + inttohex(hr, 8));
              Log('TuneRequest', 'DVBs - Setting up SymbolRate');
              hr := put_SymbolRate(SymbolRate);
              Log('TuneRequest', 'DVBs - locator::put_SymbolRate returned ' + inttohex(hr, 8));
              case Polarization of
                DVBS_POLARIZATION_HORIZONTAL: hr := put_SignalPolarisation(BDA_POLARISATION_LINEAR_H);
                DVBS_POLARIZATION_VERTICAL:   hr := put_SignalPolarisation(BDA_POLARISATION_LINEAR_V);
              end;
              Log('TuneRequest', 'DVBs - locator::put_SignalPolarisation returned ' + inttohex(hr, 8));

              with IDVBSTuningSpace(tuning_space) do
              begin
                Log('TuneRequest', 'DVBs - Setting up Low Oscillator');
                hr := put_LowOscillator(9750 * 1000);
                Log('TuneRequest', 'DVBs - tuning_space::put_LowOscillator returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up High Oscillator');
                hr := put_HighOscillator(10600 * 1000);
                Log('TuneRequest', 'DVBs - tuning_space::put_HighOscillator returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'DVBs - Setting up LNB Switch');
                hr := put_LNBSwitch(11700 * 1000);
                Log('TuneRequest', 'DVBs - tuning_space::put_LNBSwitch returned ' + inttohex(hr, 8));
              end;
            end;
          end;
        end;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:
      begin
        with PATSCTuneRequest(ATuneRequest)^ do
        begin
          if FDirectTuning then
          begin
            StartChanges;

            Log('TuneRequest', 'ATSC -  Frequency: ' + inttostr(Frequency));
            for i := 0 to FFrequencyFilter.Count -1 do
            begin
              with (FFrequencyFilter[i] as IBDA_FrequencyFilter) do
              begin
                Log('TuneRequest', 'ATSC - Setting up Frequency Multiplier to 1000');
                hr := put_FrequencyMultiplier(1000);
                Log('TuneRequest', 'ATSC - put_FrequencyMultiplier returned ' + inttohex(hr, 8));
                Log('TuneRequest', 'ATSC - Setting up Frequency');
                hr := put_Frequency(Frequency);
                Log('TuneRequest', 'ATSC - put_Frequency returned ' + inttohex(hr, 8));
              end;
            end;

            CommitChanges;
          end else
          begin
            with IATSCLocator(locator) do
            begin
              Log('TuneRequest', 'ATSC -  Frequency: ' + inttostr(Frequency));
              Log('TuneRequest', 'ATSC - Setting up Frequency');
              hr := put_CarrierFrequency(Frequency);
              Log('TuneRequest', 'ATSC - locator::put_CarrierFrequency returned ' + inttohex(hr, 8));
            end;
          end;
        end;
      end;
    end;

    if not FDirectTuning then
    begin
      Log('TuneRequest', 'Submitting Locator to TuneRequest');
      hr := tune_request.put_Locator(locator);
      Log('TuneRequest', 'tune_request::put_Locator returned ' + inttohex(hr, 8));

      Log('TuneRequest', 'Submitting TuneRequest');
      hr := (FNetworkProvider as ITuner).put_TuneRequest(tune_request);
      Log('TuneRequest', 'ITuner::put_TuneRequest returned ' + inttohex(hr, 8));
    end;
  finally
    FPushData := True;
  end;

  Result := S_OK;
end;

function TBDADevice.GetSignalStatistics(AStatistics: Pointer): Integer;
var
  s1, s2, q1, q2: Integer;
  p1, p2, l1, l2: LongBool;
  i: Integer;
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

  if (FSignalStatistics = nil) then
  begin
    Log('GetSignalStatistics', 'Statistics Interface is NULL');
    Result := E_FAIL;
    Exit;
  end;

  if PSignalStatistics(AStatistics)^.Size <> SizeOf(TSignalStatistics) then
  begin
    Log('GetSignalStatistics', 'Statistics definition is unknown');
    Result := E_FAIL;
    Exit;
  end;

  s2 := 0;
  q2 := 0;
  p2 := False;
  l2 := False;

  with PSignalStatistics(AStatistics)^ do
  begin
    SignalLocked := False;
    SignalPresent := False;
    SignalStrength := 0;
    SignalQuality := 0;
  end;

  for i := 0 to FSignalStatistics.Count -1 do
  begin
    with IBDA_SignalStatistics(FSignalStatistics[i]) do
    begin
      if get_SignalQuality(q1) = S_OK then q2 := q1;
      if get_SignalStrength(s1) = S_OK then s2 := s1;
      if get_SignalPresent(p1) = S_OK then p2 := p1;
      if get_SignalLocked(l1) = S_OK then l2 := l1;
    end;
  end;

  with PSignalStatistics(AStatistics)^ do
  begin
    SignalLocked := l2;
    SignalPresent := p2;
    SignalStrength := s2;
    SignalQuality := q2;

    Log('GetSignalStatistics', 'SignalPresent: ' + inttostr(Integer(SignalPresent)) +
        ' SignalLocked: ' + inttostr(Integer(SignalLocked)) +
        ' SignalStrength: ' + inttostr(SignalStrength) +
        ' SignalQuality: ' + inttostr(SignalQuality));
  end;

  Result := S_OK;
end;

type
  TBDASettings = packed record
    Size: Integer;
    DirectTuning: Boolean;
  end;
  PBDASettings = ^TBDASettings;

function TBDADevice.LoadSettings(ABuffer: PByte; ASize: Integer): Integer;
var
  settings: PBDASettings;
begin
  if not Assigned(ABuffer) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if (ASize <> SizeOf(TBDASettings)) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  settings := PBDASettings(ABuffer);

  if (settings.Size <> SizeOf(TBDASettings)) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  FDirectTuning := settings.DirectTuning;
  Result := S_OK;
end;

function TBDADevice.SaveSettings(out ABuffer: PByte; out ASize: Integer): Integer;
var
  settings: PBDASettings;
begin
  if not Assigned(@ABuffer) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if not Assigned(@ASize) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  settings := CoTaskMemAlloc(SizeOf(TBDASettings));

  if not Assigned(settings) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  settings.Size := SizeOf(TBDASettings);
  settings.DirectTuning := FDirectTuning;

  ASize := settings.Size;
  ABuffer := PByte(settings);

  Result := S_OK;
end;

procedure TBDADevice.OnTSData(ABuffer: PByte; ASize: Integer);
begin
  if not FPushData
    then Exit;

  PushData(ABuffer, ASize);
end;

function TBDADevice.get_DirectTuning(out ADirectTuning: BOOL): HRESULT; stdcall;
begin
  if not Assigned(@ADirectTuning) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ADirectTuning := FDirectTuning;

  Result := S_OK;
end;

function TBDADevice.put_DirectTuning(ADirectTuning: BOOL): HRESULT; stdcall;
begin
  FDirectTuning := ADirectTuning;
  Result := S_OK;
end;

(*** TBDATuner ****************************************************************)

procedure TBDATuner.Initialize;

  function DeviceHasNode(ADevice: IBaseFilter; ANode: TGuid): Boolean;
  const
    NODE_SIZE = 20;
  var
    hr: HRESULT;
    topology: IBDA_Topology;
    cnt: Cardinal;
    i: Integer;
    nodes: array[0..NODE_SIZE -1] of TBDANodeDescriptor;
  begin
    if ADevice.QueryInterface(IID_IBDA_Topology, topology) = S_OK then
    begin
      hr := topology.GetNodeDescriptors(cnt, NODE_SIZE, @nodes);
      if (hr = S_OK) and (cnt > 0) then
      begin
        for i := 0 to Int64(cnt) -1 do
        begin
          if IsEqualGUID(nodes[i].guidFunction, ANode) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;

    Result := False;
  end;

  procedure CheckNode(ADevice: IBaseFilter; ANode: TGuid; var ASupported: Integer; AValue: Integer);
  begin
    if DeviceHasNode(ADevice, ANode)
      then ASupported := ASupported or AValue;
  end;

var
  i: Integer;
  sysEnum: TSysDevEnum;
  deviceID: PWideChar;
  device_name: WideString;
  device_id: WideString;
  device_type: Integer;
  filter: IBaseFilter;

  procedure CheckDevice(AType: Integer);
  begin
    if BOOL(device_type and AType)
      then FDeviceList.Add(TBDADevice.Create(device_name, AType, device_id));
  end;

  procedure CheckTuningSpaces;
  var
    tuning_spaces: ITuningSpaceContainer;
    tuning_space: ITuningSpace;
    locator: ILocator;
    count: Integer;
    tmp: OLEVARIANT;

    function IsPresent(const AType: String; ACount: Integer): Boolean;
    var
      name: WideString;
      i: Integer;
    begin
      for i := 0 to ACount -1 do
      begin
        if tuning_spaces.get_Item(i, tuning_space) = S_OK then
        begin
          if tuning_space.get_UniqueName(name) = S_OK then
          begin
            if name = AType then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;

      Result := False;
    end;

  begin
    if CoCreateInstance(CLSID_SystemTuningSpaces, nil, CLSCTX_INPROC_SERVER, IID_ITuningSpaceContainer, tuning_spaces) = S_OK then
    begin
      // Check if the DVBt Tuning Space is present
      if (tuning_spaces.get_Count(count) = S_OK) and not IsPresent(DVBT_TUNING_SPACE, count) then
      begin
        if CoCreateInstance(CLSID_DVBTuningSpace,  nil, CLSCTX_INPROC_SERVER, IID_IDVBTuningSpace2, tuning_space) = S_OK then
        begin
          with IDVBTuningSpace2(tuning_space) do
          begin
            put_SystemType(DVB_Terrestrial);
            put_UniqueName(DVBT_TUNING_SPACE);
            put_FriendlyName(DVBT_TUNING_SPACE);
            put_NetworkType(GUIDToString(CLSID_DVBTNetworkProvider));
            put_NetworkID(-1);
            put_FrequencyMapping('-1');
          end;

          if CoCreateInstance(CLSID_DVBTLocator,  nil, CLSCTX_INPROC_SERVER, IID_IDVBTLocator, locator) = S_OK then
          begin
            with IDVBTLocator(locator) do
            begin
               put_Bandwidth(-1);
               put_LPInnerFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_LPInnerFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_HAlpha(HierarchyAlpha(BDA_HALPHA_NOT_SET));
               put_Guard(GuardInterval(BDA_GUARD_NOT_SET));
               put_Mode(TransmissionMode(BDA_XMIT_MODE_NOT_SET));
               put_OtherFrequencyInUse(False);
               put_CarrierFrequency(-1);
               put_InnerFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_InnerFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_OuterFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_OuterFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_Modulation(ModulationType(BDA_MOD_NOT_SET));
               put_SymbolRate(0);
            end;
            tuning_space.put_DefaultLocator(locator);
            tuning_spaces.Add(tuning_space, tmp);
          end;
        end;
      end;

      // Check if the DVBc Tuning Space is present
      if (tuning_spaces.get_Count(count) = S_OK) and not IsPresent(DVBC_TUNING_SPACE, count) then
      begin
        if CoCreateInstance(CLSID_DVBTuningSpace,  nil, CLSCTX_INPROC_SERVER, IID_IDVBTuningSpace2, tuning_space) = S_OK then
        begin
          with IDVBTuningSpace2(tuning_space) do
          begin
            put_SystemType(DVB_Cable);
            put_UniqueName(DVBC_TUNING_SPACE);
            put_FriendlyName(DVBC_TUNING_SPACE);
            put_NetworkType(GUIDToString(CLSID_DVBCNetworkProvider));
            put_NetworkID(-1);
            put_FrequencyMapping('-1');
          end;

          if CoCreateInstance(CLSID_DVBCLocator,  nil, CLSCTX_INPROC_SERVER, IID_IDVBCLocator, locator) = S_OK then
          begin
            with IDVBCLocator(locator) do
            begin
               put_CarrierFrequency(-1);
               put_InnerFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_InnerFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_OuterFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_OuterFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_Modulation(BDA_MOD_64QAM);
               put_SymbolRate(6900);
            end;
            tuning_space.put_DefaultLocator(locator);
            tuning_spaces.Add(tuning_space, tmp);
          end;
        end;
      end;

      // Check if the DVBs Tuning Space is present
      if (tuning_spaces.get_Count(count) = S_OK) and not IsPresent(DVBS_TUNING_SPACE, count) then
      begin
        if CoCreateInstance(CLSID_DVBSTuningSpace,  nil, CLSCTX_INPROC_SERVER, IID_IDVBSTuningSpace, tuning_space) = S_OK then
        begin
          with IDVBSTuningSpace(tuning_space) do
          begin
            put_SystemType(DVB_Satellite);
            put_UniqueName(DVBS_TUNING_SPACE);
            put_FriendlyName(DVBS_TUNING_SPACE);
            put_NetworkType(GUIDToString(CLSID_DVBSNetworkProvider));
            put_NetworkID(-1);
            put_FrequencyMapping('-1');
            put_LowOscillator(9750000);
            put_HighOscillator(10600000);
            put_LNBSwitch(11700000);
            put_InputRange('-1');
            put_SpectralInversion(BDA_SPECTRAL_INVERSION_AUTOMATIC);
          end;

          if CoCreateInstance(CLSID_DVBSLocator,  nil, CLSCTX_INPROC_SERVER, IID_IDVBSLocator, locator) = S_OK then
          begin
            with IDVBSLocator(locator) do
            begin
               put_CarrierFrequency(-1);
               put_InnerFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_InnerFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_OuterFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_OuterFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_Modulation(BDA_MOD_QPSK);
               put_SymbolRate(27500000);
               put_SignalPolarisation(BDA_POLARISATION_LINEAR_V);
               put_OrbitalPosition(0);
               put_Azimuth(-1);
               put_Elevation(-1);
               put_WestPosition(False);
            end;
            tuning_space.put_DefaultLocator(locator);
            tuning_spaces.Add(tuning_space, tmp);
          end;
        end;
      end;

      // Check if the ATSC Tuning Space is present
      if (tuning_spaces.get_Count(count) = S_OK) and not IsPresent(DVBS_TUNING_SPACE, count) then
      begin
        if CoCreateInstance(CLSID_ATSCTuningSpace,  nil, CLSCTX_INPROC_SERVER, IID_IATSCTuningSpace, tuning_space) = S_OK then
        begin
          with IATSCTuningSpace(tuning_space) do
          begin
            put_UniqueName(ATSC_TUNING_SPACE);
            put_FriendlyName(ATSC_TUNING_SPACE);
            put_NetworkType(GUIDToString(CLSID_ATSCNetworkProvider));
            put_InputType(TunerInputCable);
            put_CountryCode(0);
            put_MaxMinorChannel(999);
            put_MaxPhysicalChannel(69);
            put_MaxChannel(99);
            put_MinMinorChannel(0);
            put_MinPhysicalChannel(2);
            put_MinChannel(1);
          end;

          if CoCreateInstance(CLSID_ATSCLocator,  nil, CLSCTX_INPROC_SERVER, IID_IATSCLocator, locator) = S_OK then
          begin
            with IATSCLocator(locator) do
            begin
               put_PhysicalChannel(-1);
               put_TSID(-1);
               put_CarrierFrequency(-1);
               put_InnerFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_InnerFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_OuterFEC(FECMethod(BDA_FEC_METHOD_NOT_SET));
               put_OuterFECRate(BinaryConvolutionCodeRate(BDA_BCC_RATE_NOT_SET));
               put_Modulation(BDA_MOD_128QAM);
               put_SymbolRate(-1);
            end;
            tuning_space.put_DefaultLocator(locator);
            tuning_spaces.Add(tuning_space, tmp);
          end;
        end;
      end;
    end;
  end;

begin
  CheckTuningSpaces;

  sysEnum := TSysDevEnum.Create(KSCATEGORY_BDA_NETWORK_TUNER);
  for i := 0 to sysEnum.CountFilters -1 do
  begin
    device_name := sysEnum.Filters[i].FriendlyName;
    device_id := '';
    device_type := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN;

    if (sysEnum.GetMoniker(i).GetDisplayName(nil, nil, deviceID) = S_OK) then
    begin
      device_id := deviceID;
      CoTaskMemFree(deviceID);
    end else
    begin
      Continue;
    end;

    filter := sysEnum.GetBaseFilter(i);
    if not Assigned(filter)
      then Continue;

    CheckNode(filter, KSNODE_BDA_COFDM_DEMODULATOR, device_type, DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT);
    CheckNode(filter, KSNODE_BDA_QPSK_DEMODULATOR,  device_type, DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS);
    CheckNode(filter, KSNODE_BDA_QAM_DEMODULATOR,   device_type, DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC);
    CheckNode(filter, KSNODE_BDA_8VSB_DEMODULATOR,  device_type, DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC);

    CheckDevice(DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT);
    CheckDevice(DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS);
    CheckDevice(DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC);
    CheckDevice(DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC);
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TBDATuner,                                // Tuner Class Name
    CLSID_DCDVB_BDATuner,                     // Tuner CLSID
    'BDA Wrapper',                            // Name
    'BDA Plugin for DC-DVB Source',           // Description
    'Milenko Mitrovic <dcoder@dsp-worx.de>',  // Author
    '0.0.0.5'                                 // Version
  );

end.
