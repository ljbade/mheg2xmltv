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

unit DCDVBPluginManager;

interface

uses
  Windows, Registry, DCDVBDataPlugins, Classes, ActiveX, SysUtils, Logger, MPEGUtils,
  DirectShow9, DCDVBTuningPlugins, DCDVBShared, DSUtil;

type
  TDCDVBDPlugin = class
  private
    FName: String;
    FID: TGuid;
    FEnabled: Boolean;
    FIntf: IDCDVBDataPlugin;
    procedure SetSettings(ASettings: String);
    function GetSettings: String;
  public
    destructor Destroy; override;
    procedure ProcessBuffer(ABuffer: PByte; var ASize: Integer);
    procedure SetFilterGraph(AFilterGraph: IFilterGraph);
    procedure ProgramChanged(AProgram: PProgram);
  published
    property Name: String read FName write FName;
    property ID: TGuid read FID write FID;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Intf: IDCDVBDataPlugin read FIntf write FIntf;
    property Settings: String read GetSettings write SetSettings;
  end;

  TDCDVBDPluginList = class
  private
    FLog: TLogger;
    FList: TList;
    function GetCount: Integer;
    function GetPlugin(AIndex: Integer): TDCDVBDPlugin;
    function GetPluginEnabled(AIndex: Integer): Boolean;
  public
    constructor Create(ALog: TLogger);
    destructor Destroy; override;

    procedure Clear;
    procedure Update;
    procedure ProcessBuffer(ABuffer: PByte; var ASize: Integer);
    procedure SetFilterGraph(AFilterGraph: IFilterGraph);
    procedure ProgramChanged(AProgram: PProgram);

    property Count: Integer read GetCount;
    property Plugin[Index: Integer]: TDCDVBDPlugin read GetPlugin; default;
    property PluginEnabled[Index: Integer]: Boolean read GetPluginEnabled;
  end;

type
  TDCDVBTPlugin = class
  private
    FLog: TLogger;
    FLibraryPath: String;
    FAuthor: String;
    FDescription: String;
    FVersion: String;
    FTuner: IDCDVBTuningPlugin;
    function GetCount: Integer;
    function GetDeviceID: TGuid;
    function GetDevice(AIndex: Integer): IDCDVBTuningPluginDevice;
  public
    constructor Create(ALog: TLogger);
    destructor Destroy; override;

    procedure Load(ATunerID: TGuid);
    property Device[AIndex: Integer]: IDCDVBTuningPluginDevice read GetDevice; default;
  published
    property LibraryPath: String read FLibraryPath write FLibraryPath;
    property Author: String read FAuthor write FAuthor;
    property Description: String read FDescription write FDescription;
    property Version: String read FVersion write FVersion;

    property Count: Integer read GetCount;
    property DeviceID: TGuid read GetDeviceID;
  end;

  TDCDVBTPluginList = class
  private
    FLog: TLogger;
    FList: TList;
    procedure ClearList;
    function GetCount: Integer;
    function GetDevice(AIndex: Integer): IDCDVBTuningPluginDevice;
    function GetDeviceName(AIndex: Integer): String;
    function GetDeviceTypeString(AIndex: Integer): String;
    function GetDeviceID(AIndex: Integer): String;
    function GetDeviceType(AIndex: Integer): Integer;
    function GetDeviceSettings(AIndex: Integer): String;
    procedure SetDeviceSettings(AIndex: Integer; ASettings: String);
  public
    constructor Create(ALog: TLogger);
    destructor Destroy; override;

    procedure ShowPropertyPage(AIndex: Integer; AHandle: THandle);
    function HasPropertyPage(AIndex: Integer): Boolean;

    procedure Update;
    property Count: Integer read GetCount;
    property Device[AIndex: Integer]: IDCDVBTuningPluginDevice read GetDevice; default;
    property DeviceName[AIndex: Integer]: String read GetDeviceName;
    property DeviceTypeString[AIndex: Integer]: String read GetDeviceTypeString;
    property DeviceType[AIndex: Integer]: Integer read GetDeviceType;
    property DeviceID[AIndex: Integer]: String read GetDeviceID;
    property DeviceSettings[AIndex: Integer]: String read GetDeviceSettings write SetDeviceSettings;
    function GetDeviceSettings2(ADevice: IDCDVBTuningPluginDevice): String;
  end;

implementation

procedure GetStringFromHex(const AString: String; out ABuffer: PByte; out ASize: Integer);
var
  i: Integer;
  buffer: PByte;
begin
  ASize := Length(AString) shr 1;
  ABuffer := CoTaskMemAlloc(ASize);
  buffer := ABuffer;
  for i := 0 to ASize - 1 do
  begin
    buffer^ := Byte(strtoint('$' + AString[i shl 1 + 1] + AString[i shl 1 + 2]));
    inc(buffer);
  end;
end;

(*** TDCDVBDPlugin ************************************************************)

destructor TDCDVBDPlugin.Destroy;
begin
  SetFilterGraph(nil);
  inherited Destroy;
end;

procedure TDCDVBDPlugin.ProcessBuffer(ABuffer: PByte; var ASize: Integer);
begin
  if FEnabled and (FIntf <> nil) then
  begin
    FIntf.ProcessBuffer(ABuffer, ASize);
  end;
end;

procedure TDCDVBDPlugin.SetSettings(ASettings: String);
var
  buffer: PByte;
  size: Integer;
begin
  if (FIntf <> nil) then
  begin
    GetStringFromHex(ASettings, buffer, size);
    if Assigned(buffer) then
    begin
      FIntf.put_ControlMessage(DCDVB_DATA_PLUGIN_CTRL_LOAD_SETTINGS, Integer(buffer), size);
      CoTaskMemFree(buffer);
    end;
  end;
end;

function TDCDVBDPlugin.GetSettings: String;
var
  buffer: PByte;
  size: Integer;
  hr: Integer;
begin
  if (FIntf <> nil) then
  begin
    hr := FIntf.put_ControlMessage(DCDVB_DATA_PLUGIN_CTRL_SAVE_SETTINGS, Integer(@buffer), Integer(@size));
    if hr = S_OK then
    begin
      Result := GetHexString(buffer, size, False);
      CoTaskMemFree(buffer);
      Exit;
    end;
  end;

  Result := '';
end;

procedure TDCDVBDPlugin.SetFilterGraph(AFilterGraph: IFilterGraph);
begin
  if (FIntf <> nil)
    then FIntf.put_ControlMessage(DCDVB_DATA_PLUGIN_CTRL_SET_FILTERGRAPH, Integer(AFilterGraph), 0);
end;

procedure TDCDVBDPlugin.ProgramChanged(AProgram: PProgram);
begin
  if (FIntf <> nil) and FEnabled
    then FIntf.put_ControlMessage(DCDVB_DATA_PLUGIN_CTRL_PROGRAM_CHANGED, Integer(AProgram), 0);
end;

(*** TDCDVBDPluginList ********************************************************)

constructor TDCDVBDPluginList.Create(ALog: TLogger);
begin
  inherited Create;
  FLog := ALog;
  FList := TList.Create;
  Update;
end;

destructor TDCDVBDPluginList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDCDVBDPluginList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDCDVBDPlugin(FList[i]).Free;
  FList.Clear;
end;

procedure TDCDVBDPluginList.Update;
var
  reg: TRegistry;
  str: TStringList;
  intf: IDCDVBDataPlugin;
  hr: HRESULT;
  i: Integer;
  plugin: TDCDVBDPlugin;
  plugin_name: PChar;
begin
  Clear;

  str := TStringList.Create;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CLASSES_ROOT;

  if reg.OpenKeyReadOnly(DCDVB_DATA_PLUGIN_REGISTRY_ROOT) then
  begin
    reg.GetKeyNames(str);
    reg.CloseKey;

    for i := 0 to str.Count -1 do
    begin
      hr := CoCreateInstance(StringToGUID(str[i]), nil, CLSCTX_INPROC_SERVER, IID_IDCDVBDataPlugin, intf);
      if hr = S_OK then
      begin
        plugin := TDCDVBDPlugin.Create;
        plugin.FIntf := intf;
        plugin.ID := StringToGUID(str[i]);
        plugin.FEnabled := False;
        intf.get_PluginName(plugin_name);
        plugin.FName := plugin_name;
        FList.Add(plugin);
      end;
    end;
  end;

  str.Free;
  reg.Free;
end;

function TDCDVBDPluginList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDCDVBDPluginList.GetPlugin(AIndex: Integer): TDCDVBDPlugin;
begin
  Result := TDCDVBDPlugin(FList[AIndex]);
end;

function TDCDVBDPluginList.GetPluginEnabled(AIndex: Integer): Boolean;
begin
  Result := TDCDVBDPlugin(FList[AIndex]).FEnabled;
end;

procedure TDCDVBDPluginList.ProcessBuffer(ABuffer: PByte; var ASize: Integer);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDCDVBDPlugin(FList[i]).ProcessBuffer(ABuffer, ASize);
end;

procedure TDCDVBDPluginList.SetFilterGraph(AFilterGraph: IFilterGraph);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDCDVBDPlugin(FList[i]).SetFilterGraph(AFilterGraph);
end;

procedure TDCDVBDPluginList.ProgramChanged(AProgram: PProgram);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDCDVBDPlugin(FList[i]).ProgramChanged(AProgram);
end;

(*** TDCDVBTPlugin ************************************************************)

constructor TDCDVBTPlugin.Create(ALog: TLogger);
begin
  inherited Create;
  FLog := ALog;
end;

destructor TDCDVBTPlugin.Destroy;
begin
  FTuner := nil;
  inherited Destroy;
end;

function TDCDVBTPlugin.GetCount: Integer;
begin
  if Assigned(FTuner)
    then FTuner.get_CountTunerDevices(Result)
    else Result := 0;
end;

function TDCDVBTPlugin.GetDeviceID: TGuid;
begin
  if Assigned(FTuner)
    then FTuner.get_TunerID(Result)
    else Result := GUID_NULL;
end;

function TDCDVBTPlugin.GetDevice(AIndex: Integer): IDCDVBTuningPluginDevice;
begin
  if Assigned(FTuner)
    then FTuner.get_TunerDevice(AIndex, Result)
    else Result := nil;
end;

procedure TDCDVBTPlugin.Load(ATunerID: TGuid);
var
  sdk: Cardinal;
  i: Integer;
  device: IDCDVBTuningPluginDevice;
  text: PChar;
  tp: Integer;
  hr: HRESULT;
begin
  Log(FLog, Self, 'Load', 'Loading Device Driver');
  FTuner := nil;

  hr := CoCreateInstance(ATunerID, nil, CLSCTX_INPROC_SERVER, IID_IDCDVBTuningPlugin, FTuner);
  if hr <> S_OK then
  begin
    Log(FLog, Self, 'Load', 'failed receiving Tuner Interface');
    Exit;
  end;

  if FTuner.get_SDKVersion(sdk) = S_OK then
  begin
    if (sdk > DCDVB_TUNING_PLUGIN_SDK_VERSION) then
    begin
      Log(FLog, Self, 'Load', 'UT_SDK_VERSION is not Supported');
      FTuner := nil;
      Exit;
    end;
  end;

  if Assigned(FLog) then
  begin
    Log(FLog, Self, 'Load', 'Driver has ' + inttostr(GetCount) + ' Devices');
    for i := 0 to GetCount -1 do
    begin
      device := GetDevice(i);
      if Assigned(device) then
      begin
        device.get_DeviceName(text);
        Log(FLog, Self, 'Load', 'Device ' + inttostr(i) + ' DeviceName: ' + text);
        device.get_DeviceType(tp);
        Log(FLog, Self, 'Load', 'Device ' + inttostr(i) + ' DeviceType: ' + inttostr(tp));
        device.get_DeviceID(text);
        Log(FLog, Self, 'Load', 'Device ' + inttostr(i) + ' DeviceID: ' + text);
      end else
      begin
        Log(FLog, Self, 'Load', 'Device ' + inttostr(i) + ' is NULL');
      end;
    end;
  end;
end;

(*** TDCDVBTPluginList ********************************************************)

constructor TDCDVBTPluginList.Create(ALog: TLogger);
begin
  inherited Create;
  FLog := ALog;
  FList := TList.Create;
  Update;
end;

destructor TDCDVBTPluginList.Destroy;
begin
  ClearList;
  FList.Free;
  inherited Destroy;
end;

procedure TDCDVBTPluginList.ClearList;
var
  i: Integer;
  device: TDCDVBTPlugin;
begin
  for i := 0 to FList.Count -1 do
  begin
    device := FList[i];
    device.Free;
  end;
  FList.Clear;
end;

function TDCDVBTPluginList.GetCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count -1 do
  begin
    inc(Result, TDCDVBTPlugin(FList[i]).Count);
  end;
end;

function TDCDVBTPluginList.GetDevice(AIndex: Integer): IDCDVBTuningPluginDevice;
var
  i, c, k: Integer;
begin
  c := 0;

  for i := 0 to FList.Count -1 do
  begin
    for k := 0 to TDCDVBTPlugin(FList[i]).Count -1 do
    begin
      if (c = AIndex) then
      begin
        Result := TDCDVBTPlugin(FList[i]).GetDevice(k);
        Exit;
      end;
      inc(c);
    end;
  end;

  Result := nil;
end;

function TDCDVBTPluginList.GetDeviceName(AIndex: Integer): String;
var
  device: IDCDVBTuningPluginDevice;
  p: PChar;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    device.get_DeviceName(p);
    Result := p;
  end else
  begin
    Result := 'Unknown';
  end;
end;

function TDCDVBTPluginList.GetDeviceID(AIndex: Integer): String;
var
  device: IDCDVBTuningPluginDevice;
  p: PChar;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    device.get_DeviceID(p);
    Result := p;
  end else
  begin
    Result := 'Unknown';
  end;
end;

function TDCDVBTPluginList.GetDeviceTypeString(AIndex: Integer): String;
var
  device: IDCDVBTuningPluginDevice;
  n: Integer;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    device.get_DeviceType(n);
    case n of
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN: Result := 'Unknown';
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:    Result := 'DVB-t';
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:    Result := 'DVB-c';
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:    Result := 'DVB-s';
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBH:    Result := 'DVB-h';
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:    Result := 'ATSC';
      else                                     Result := 'Unknown';
    end;
  end else
  begin
    Result := 'Unknown';
  end;
end;

function TDCDVBTPluginList.GetDeviceType(AIndex: Integer): Integer;
var
  device: IDCDVBTuningPluginDevice;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    device.get_DeviceType(Result);
  end else
  begin
    Result := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN;
  end;
end;

procedure TDCDVBTPluginList.Update;
var
  reg: TRegistry;
  str: TStringList;
  i: Integer;
  author: String;
  description: String;
  version: String;
  device: TDCDVBTPlugin;
begin
  Log(FLog, Self, 'Update', 'updating Device Driver List');
  ClearList;

  str := TStringList.Create;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CLASSES_ROOT;

  if reg.OpenKeyReadOnly(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT) then
  begin
    reg.GetKeyNames(str);
    reg.CloseKey;

    for i := 0 to str.Count -1 do
    begin
      if reg.OpenKeyReadOnly(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT + '\' + str[i]) then
      begin
        author := reg.ReadString('Author');
        description := reg.ReadString('Description');
        version := reg.ReadString('Version');
        Log(FLog, Self, 'Update', 'Found Driver: ' + description + ' Version: ' + version);

        device := TDCDVBTPlugin.Create(FLog);
        device.FAuthor := author;
        device.FDescription := description;
        device.FVersion := version;
        device.Load(StringToGUID(str[i]));
        FList.Add(device);

        reg.CloseKey;
      end;
    end;
  end;

  str.Free;
  reg.Free;
end;

procedure TDCDVBTPluginList.ShowPropertyPage(AIndex: Integer; AHandle: THandle);
var
  device: IDCDVBTuningPluginDevice;
  SpecifyPropertyPages : ISpecifyPropertyPages;
  hr: HRESULT;
  CAGUID  :TCAGUID;
  device_name: PChar;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    hr := device.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages);
    if hr <> S_OK then exit;
    hr := SpecifyPropertyPages.GetPages(CAGUID);
    device.get_DeviceName(device_name);
    if hr = S_OK then
    begin
      OleCreatePropertyFrame(AHandle, -1, -1, PWideChar(WideString(String(device_name))), 1, @device, CAGUID.cElems, CAGUID.pElems, 0, 0, nil );
    end;
    if Assigned(CAGUID.pElems)
      then CoTaskMemFree(CAGUID.pElems);
  end;
end;

function TDCDVBTPluginList.HasPropertyPage(AIndex: Integer): Boolean;
var
  device: IDCDVBTuningPluginDevice;
  SpecifyPropertyPages : ISpecifyPropertyPages;
  hr: HRESULT;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    hr := device.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages);
    Result := hr = S_OK;
    Exit;
  end;
  Result := False;
end;

function TDCDVBTPluginList.GetDeviceSettings(AIndex: Integer): String;
var
  device: IDCDVBTuningPluginDevice;
  buffer: PByte;
  size: Integer;
  hr: Integer;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    hr := device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_SAVE_SETTINGS, Integer(@buffer), Integer(@size));
    if hr = S_OK then
    begin
      Result := GetHexString(buffer, size, False);
      CoTaskMemFree(buffer);
      Exit;
    end;
  end;

  Result := '';
end;

procedure TDCDVBTPluginList.SetDeviceSettings(AIndex: Integer; ASettings: String);
var
  device: IDCDVBTuningPluginDevice;
  buffer: PByte;
  size: Integer;
begin
  device := GetDevice(AIndex);
  if Assigned(device) then
  begin
    GetStringFromHex(ASettings, buffer, size);
    if Assigned(buffer) then
    begin
      device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_LOAD_SETTINGS, Integer(buffer), size);
      CoTaskMemFree(buffer);
    end;
  end;
end;

function TDCDVBTPluginList.GetDeviceSettings2(ADevice: IDCDVBTuningPluginDevice): String;
var
  buffer: PByte;
  size: Integer;
  hr: Integer;
begin
  if Assigned(ADevice) then
  begin
    hr := ADevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_SAVE_SETTINGS, Integer(@buffer), Integer(@size));
    if hr = S_OK then
    begin
      Result := GetHexString(buffer, size, False);
      CoTaskMemFree(buffer);
      Exit;
    end;
  end;

  Result := '';
end;

end.
