unit DLLExports;

interface

uses
  Windows, BaseClass, WMPPlugin, WMPPluginAPI, DVBFilter, SysUtils, Registry,
  Forms, formScan, WMPPluginInfo;

  function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
  function DllCanUnloadNow: HResult; stdcall;
  function DllRegisterServer: HResult; stdcall;
  function DllUnregisterServer: HResult; stdcall;
  function ChannelScan: HResult; stdcall;
  function DeviceReset: HResult; stdcall;

implementation

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
begin
  if IsEqualGUID(CLSID, CLSID_WMPPlugin) or IsEqualGUID(CLSID_WMPPluginInfo, CLSID)
    then Result := WMPPluginAPI.DllGetClassObject(CLSID, IID, Obj)
    else Result := BaseClass.DllGetClassObject(CLSID, IID, Obj)
end;

function DllCanUnloadNow: HResult;
begin
  Result := BaseClass.DllCanUnloadNow;
  if Result <> S_OK
    then Exit;
  Result := WMPPluginAPI.DllCanUnloadNow;
end;

function DllRegisterServer: HResult;
var
  reg: TRegistry;
begin
  Result := DVBFilter.DllRegisterServer;
  Result := Result or WMPPluginAPI.DllRegisterServer;
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;
  if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\Descriptions', True) then
  begin
    reg.WriteString('317', 'DC-DVB Source File (*.dvb)');
    reg.CloseKey;
    if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\MUIDescriptions', True) then
    begin
      reg.WriteString('317', 'DC-DVB Source File');
      reg.CloseKey;
      if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\Types', True) then
      begin
        reg.WriteString('317', '*.dvb');
        reg.CloseKey;
      end;
    end;
  end;
  if reg.OpenKey('Software\Microsoft\MultiMedia\WMPlayer\Extensions\.dvb', True) then
  begin
    reg.WriteInteger('Permissions', 15);
    reg.WriteInteger('Runtime', 7);
    reg.CloseKey;
  end;
  reg.RootKey := HKEY_CURRENT_USER;
  if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\.dvb', True) then
  begin
    reg.WriteInteger('Permissions', 15);
    reg.WriteInteger('Runtime', 7);
    reg.CloseKey;
  end;
  reg.Free;
end;

function DllUnregisterServer: HResult;
var
  reg: TRegistry;
begin
  Result := DVBFilter.DllUnregisterServer;
  Result := Result or WMPPluginAPI.DllUnregisterServer;
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;
  if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\Descriptions', False) then
  begin
    if reg.ValueExists('317')
      then reg.DeleteValue('317');
    reg.CloseKey;
    if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\MUIDescriptions', False) then
    begin
      if reg.ValueExists('317')
        then reg.DeleteValue('317');
      reg.CloseKey;
      if reg.OpenKey('Software\Microsoft\MediaPlayer\Player\Extensions\Types', False) then
      begin
        if reg.ValueExists('317')
          then reg.DeleteValue('317');
        reg.CloseKey;
      end;
    end;
  end;
  if reg.KeyExists('Software\Microsoft\MultiMedia\WMPlayer\Extensions\.dvb')
    then reg.DeleteKey('Software\Microsoft\MultiMedia\WMPlayer\Extensions\.dvb');
  reg.RootKey := HKEY_CURRENT_USER;
  if reg.KeyExists('Software\Microsoft\MediaPlayer\Player\Extensions\.dvb')
    then reg.DeleteKey('Software\Microsoft\MediaPlayer\Player\Extensions\.dvb');
  reg.Free;
end;

function ChannelScan: HResult;
begin
  Application.Initialize;
  Application.Title := 'DVB/ATSC - ChannelScan';
  Application.CreateForm(TfrmScan, frmScan);
  Application.Run;
  Result := S_OK;
end;

const
  DIF_PROPERTYCHANGE    = $00000012;
  DICS_FLAG_GLOBAL      = $00000001;
  DIGCF_PRESENT         = $00000002;
  DIGCF_ALLCLASSES      = $00000004;
  DICS_ENABLE           = $00000001;
  DICS_DISABLE          = $00000002;

type
  HDEVINFO = Pointer;

  PSPDevInfoData = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record
    cbSize: Cardinal;
    ClassGuid: TGUID;
    DevInst: Cardinal;
    Reserved: Cardinal;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPClassInstallHeader = ^TSPClassInstallHeader;
  SP_CLASSINSTALL_HEADER = packed record
    cbSize: Cardinal;
    InstallFunction: Cardinal;
  end;
  TSPClassInstallHeader = SP_CLASSINSTALL_HEADER;

  PSPPropChangeParams = ^TSPPropChangeParams;
  SP_PROPCHANGE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    StateChange: Cardinal;
    Scope: Cardinal;
    HwProfile: Cardinal;
  end;
  TSPPropChangeParams = SP_PROPCHANGE_PARAMS;

  function SetupDiSetClassInstallParamsA(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader; ClassInstallParamsSize: Cardinal): LongBool; stdcall; external 'SetupApi.dll';
  function SetupDiCallClassInstaller(InstallFunction: Cardinal; DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): LongBool; stdcall; external 'SetupApi.dll';
  function SetupDiGetClassDevsA(ClassGuid: PGUID; const Enumerator: PAnsiChar; hwndParent: Cardinal; Flags: Cardinal): HDEVINFO; stdcall; external 'SetupApi.dll';
  function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: Cardinal; var DeviceInfoData: TSPDevInfoData): LongBool; stdcall; external 'SetupApi.dll';
  function SetupDiGetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO; const DeviceInfoData: TSPDevInfoData; Property_: Cardinal; var PropertyRegDataType: Cardinal; PropertyBuffer: PBYTE; PropertyBufferSize: Cardinal; var RequiredSize: Cardinal): LongBool; stdcall; external 'SetupApi.dll';
  function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): LongBool; stdcall; external 'SetupApi.dll';

procedure ChangeState(d: HDEVINFO; s: Cardinal; i: TSPDevInfoData);
var
  params: TSPPropChangeParams;
begin
  params.ClassInstallHeader.cbSize := SizeOf(TSPClassInstallHeader);
  params.ClassInstallHeader.InstallFunction := DIF_PROPERTYCHANGE;
  params.Scope := DICS_FLAG_GLOBAL;
  params.StateChange := s;
  if (SetupDiSetClassInstallParamsA(d, @i,PSPClassInstallHeader(@params), SizeOf(params)))
    then SetupDiCallClassInstaller(DIF_PROPERTYCHANGE, d, @i);
end;

function DeviceReset: HResult;
var
  DriverName: String;
  dev: HDEVINFO;
  info: TSPDevInfoData;
  i: Integer;
  buf: array[0..256] of Char;
  prop: Cardinal;
  siz: Cardinal;
  found: Boolean;
begin
  Result := S_OK;
  if ParamCount <> 2 then
  begin
    MessageBox(GetDesktopWindow, 'Invalid Syntax', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  found := False;
  DriverName := ParamStr(2);

  dev := SetupDiGetClassDevsA(nil, nil, 0, DIGCF_PRESENT or DIGCF_ALLCLASSES);
  if (Cardinal(dev) <> Cardinal(-1)) then
  begin
    i := 0;
    info.cbSize := SizeOf(TSPDevInfoData);
    while SetupDiEnumDeviceInfo(dev, i, info) do
    begin
      inc(i);
      prop := 0;
      siz := 256;

      SetupDiGetDeviceRegistryPropertyA(dev, info, prop, prop, @buf, siz, siz);

      if (buf = DriverName) then
      begin
        found := True;
        ChangeState(dev, DICS_DISABLE, info);
        ChangeState(dev, DICS_ENABLE, info);
        break;
      end;
    end;

    SetupDiDestroyDeviceInfoList(dev);
  end;

  if found
    then MessageBox(GetDesktopWindow, 'Device has been reset', 'Info', MB_OK or MB_ICONINFORMATION)
    else MessageBox(GetDesktopWindow, 'No Device found', 'Info', MB_OK or MB_ICONWARNING);
end;

end.
