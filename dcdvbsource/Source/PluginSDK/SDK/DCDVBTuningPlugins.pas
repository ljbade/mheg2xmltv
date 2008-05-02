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

unit DCDVBTuningPlugins;

interface

uses
  Windows, Classes, ActiveX, ComObj, Messages, DCDVBShared, SysUtils;

(*** Default Plugin Definitions ***********************************************)

const
  // Currently used SDK for the Tuning Plugins
  DCDVB_TUNING_PLUGIN_SDK_VERSION     = $00000003;

  // Root Directory within HKEY_CLASSES_ROOT where Plugins needs to Register
  // themselves using a unique GUID
  DCDVB_TUNING_PLUGIN_REGISTRY_ROOT   = 'DCDVBTuningPlugins';

(*** Device Type Definitions **************************************************)

const
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN    =  0;
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT       =  1;
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC       =  2;
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS       =  4;
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBH       =  8;
  DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC       = 16;
  
(*** LNB Control Definitions **************************************************)

type
  TLNBControl = packed record
  end;
  PLNBControl = ^TLNBControl;

(*** TuneRequest Definitions **************************************************)

type
  TBaseTuneRequest = packed record
    Size: Integer;
    DeviceType: Integer;
  end;
  PBaseTuneRequest = ^TBaseTuneRequest;

  TDVBTTuneRequest = packed record
    Size: Integer;          // sizeof(TDVBTTuneRequest)
    DeviceType: Integer;    // must be DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT
    Frequency: Integer;     // Frequency in kHz
    Bandwidth: Integer;     // Bandwidth in MHz
  end;
  PDVBTTuneRequest = ^TDVBTTuneRequest;

  TDVBCTuneRequest = packed record
    Size: Integer;          // sizeof(TDVBCTuneRequest)
    DeviceType: Integer;    // must be DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC
    Frequency: Integer;     // Frequency in kHz
    SymbolRate: Integer;    // SymbolRate in ksb/s e.g. 6900
  end;
  PDVBCTuneRequest = ^TDVBCTuneRequest;

const
  DVBS_POLARIZATION_HORIZONTAL = 0;
  DVBS_POLARIZATION_VERTICAL   = 1;

type
  TDVBSTuneRequest = packed record
    Size: Integer;          // sizeof(TDVBSTuneRequest)
    DeviceType: Integer;    // must be DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS
    Frequency: Integer;     // Frequency in kHz
    Polarization: Integer;  // DVBS_POLARIZATION_HORIZONTAL or DVBS_POLARIZATION_VERTICAL
    SymbolRate: Integer;    // SymbolRate in ksb/s e.g. 27500
    LNBControl: Pointer;    // NULL or a Pointer to an LNB Control Struct
  end;
  PDVBSTuneRequest = ^TDVBSTuneRequest;

  TDVBHTuneRequest = packed record
    Size: Integer;          // sizeof(TDVBHTuneRequest)
    DeviceType: Integer;    // must be DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBH
    Frequency: Integer;     // Frequency in kHz
  end;
  PDVBHTuneRequest = ^TDVBHTuneRequest;

  TATSCTuneRequest = packed record
    Size: Integer;          // sizeof(TATSCTuneRequest)
    DeviceType: Integer;    // must be DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC
    Frequency: Integer;     // Frequency in kHz
  end;
  PATSCTuneRequest = ^TATSCTuneRequest;

(*** Error/Success Definitions ************************************************)

const
  DCDVB_TUNING_PLUGIN_ACTIVE                 = 5;
  DCDVB_TUNING_PLUGIN_INACTIVE               = 6;

  DCDVB_TUNING_PLUGIN_E_ALREADY_ACTIVE       = 101;
  DCDVB_TUNING_PLUGIN_E_ALREADY_INACTIVE     = 102;
  DCDVB_TUNING_PLUGIN_E_INVALID_TUNEREQUEST  = 106;
  DCDVB_TUNING_PLUGIN_E_INACTIVE             = 107;

(*** Control Message Definitions **********************************************)

type
  TSignalStatistics = packed record
    Size: Integer;            // SizeOf(TSignalStatistics) filled in by calling Application !!
    SignalLocked: Boolean;    // Filled by Plugin
    SignalPresent: Boolean;   // Filled by Plugin
    SignalStrength: Integer;  // Filled by Plugin
    SignalQuality: Integer;   // Filled by Plugin
  end;
  PSignalStatistics = ^TSignalStatistics;

const
  DCDVB_TUNING_PLUGIN_CTRL_ACTIVATE                = 200; // Param1 = 0; Param2 = 0;
  DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE              = 201; // Param1 = 0; Param2 = 0;
  DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST             = 202; // Param1 = Pointer to a TuneRequest; Param2 = UT_DEVICE_TYPE_XXX that matches the Tune Request
  DCDVB_TUNING_PLUGIN_CTRL_LOAD_SETTINGS           = 203; // Param1 = Pointer to a Buffer; Param2 = Size of the Buffer
  DCDVB_TUNING_PLUGIN_CTRL_SAVE_SETTINGS           = 204; // Param1 = Pointer to a Buffer, must be allocated using CoTaskAllocMem; Param2 = Pointer to Size of the Buffer
  DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED         = 205; // Param1 = PProgram; Param2 = 0;
  DCDVB_TUNING_PLUGIN_CTRL_PMT_BUFFER              = 206; // Param1 = Pointer to PMT; Param2 = size of Buffer;

  DCDVB_TUNING_PLUGIN_CTRL_GET_ACTIVATE_STATE      = 400; // Param1 = 0; Param2 = 0; Returns UT_ACTIVE or UT_INACTIVE
  DCDVB_TUNING_PLUGIN_CTRL_GET_SIGNAL_STATISTICS   = 401; // Param1 = Pointer to PUTSignalStatistics; Param2 = 0; Returns UT_OK or UT_E_INACTIVE

(*** Plugin Architecture Interface Definitions ********************************)

const
  IID_IDCDVBTuningPlugin:                       TGuid = '{27666DB3-2F27-497B-9A88-A7D39341BDE0}';
  IID_IDCDVBTuningPluginDevice:                 TGuid = '{20520407-4149-4F38-8780-7B2144F3EE0F}';
  IID_IDCDVBTuningPluginApplicationCallback:    TGuid = '{F263974D-6A9C-4D29-A00A-2D04FD993AA4}';

type
  IDCDVBTuningPluginApplicationCallback = interface(IUnknown)
  ['{F263974D-6A9C-4D29-A00A-2D04FD993AA4}']
    function ReceiveTSStream(ABuffer: PByte; ASize: Integer): Integer; stdcall;
    function Log(AMethod: PChar; AText: PChar): Integer; stdcall;
  end;

  IDCDVBTuningPluginDevice = interface(IUnknown)
  ['{20520407-4149-4F38-8780-7B2144F3EE0F}']
    function get_DeviceName(out ADeviceName: PChar): Integer; stdcall;
    function get_DeviceType(out ADeviceType: Integer): Integer; stdcall;
    function get_DeviceID(out ADeviceID: PChar): Integer; stdcall;
    function put_ApplicationCallback(ACallback: IDCDVBTuningPluginApplicationCallback): Integer; stdcall;
    function put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; stdcall;
  end;

  IDCDVBTuningPlugin = interface(IUnknown)
  ['{27666DB3-2F27-497B-9A88-A7D39341BDE0}']
    function get_TunerID(out ATunerID: TGuid): Integer; stdcall;
    function get_CountTunerDevices(out ACountTunerDevices: Integer): Integer; stdcall;
    function get_TunerDevice(AIndex: Integer; out ATunerDevice: IDCDVBTuningPluginDevice): Integer; stdcall;
    function get_SDKVersion(out ADCDVBTuningPluginsSDKVersion: Cardinal): Integer; stdcall;
  end;

(*** Base Tuning Plugin Classes ***********************************************)

type
  TDCDVBTuningPluginDevice = class(TDCDVBBasePlugin, IDCDVBTuningPluginDevice)
  protected
    FAppCallback: IDCDVBTuningPluginApplicationCallback;
    FDeviceName: String;
    FDeviceType: Integer;
    FDeviceID: String;
    procedure Log(const AMethod: String; const AText: String);
    function OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; virtual; abstract;
    procedure PushData(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ADeviceName: String; ADeviceType: Integer; ADeviceID: String); virtual;
    destructor Destroy; override;
    // IUnifiedTunerDevice
    function put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; stdcall;
    function put_ApplicationCallback(ACallback: IDCDVBTuningPluginApplicationCallback): Integer; stdcall;
    function get_DeviceName(out ADeviceName: PChar): Integer; stdcall;
    function get_DeviceType(out ADeviceType: Integer): Integer; stdcall;
    function get_DeviceID(out ADeviceID: PChar): Integer; stdcall;
  published
    property DeviceName: String read FDeviceName write FDeviceName;
    property DeviceType: Integer read FDeviceType write FDeviceType;
    property DeviceID: String read FDeviceID write FDeviceID;
  end;

  TDCDVBTuningPlugin = class(TDCDVBInterfacedObject, IDCDVBTuningPlugin)
  protected
    FDeviceList: IInterfaceList;
    FTunerID: TGuid;
    procedure Initialize; virtual; abstract;
  public
    constructor Create(ATunerID: TGuid);
    destructor Destroy; override;
    // IUnifiedTuner
    function get_TunerID(out ATunerID: TGuid): Integer; stdcall;
    function get_CountTunerDevices(out ACountTuningDevices: Integer): Integer; stdcall;
    function get_TunerDevice(AIndex: Integer; out ATuningDevice: IDCDVBTuningPluginDevice): Integer; stdcall;
    function get_SDKVersion(out ASDKVersion: Cardinal): Integer; stdcall;
  end;
  TDCDVBTuningPluginClass = class of TDCDVBTuningPlugin;

(*** Custom COM Objects *******************************************************)

type
  TDCDVBClassFactory = class(TObject, IUnknown, IClassFactory)
  private
    FTunerClass: TDCDVBTuningPluginClass;
    FName: WideString;
    FTunerID: TGUID;
    FPropID: TGUID;
    FDescription: WideString;
    FAuthor: WideString;
    FVersion: WideString;
  {$IFDEF FORMS}
    FFormClass: TDCDVBFormPropertyPageClass;
  {$ELSE}
    FPropClass: TDCDVBPropertyPageClass;
    FDialogID: Integer;
    FCaption: WideString;
  {$ENDIF}
    procedure UpdateRegistry(ARegister: Boolean); overload;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IClassFactory
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID; out Obj): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(ATunerClass: TDCDVBTuningPluginClass; const ATunerID: TGUID; const AName: WideString; const ADescription: WideString; const AAuthor: WideString; const AVersion: WideString); overload;
  {$IFDEF FORMS}
    constructor Create(AFormClass: TDCDVBFormPropertyPageClass; const APropID: TGUID); overload;
  {$ELSE}
    constructor Create(APropClass: TDCDVBPropertyPageClass; const APropID: TGUID; ADialogID: Integer; const ACaption: WideString); overload;
  {$ENDIF}
  end;

  TDCDVBClassTemplate = class
  private
    FFactoryList: TList;
    procedure AddFactory(AFactory: TDCDVBClassFactory);
  public
    constructor Create;
    destructor Destroy; override;

    class function GetTemplate: TDCDVBClassTemplate;

    function RegisterServer(ARegister: Boolean): Boolean;
    function GetFactoryFromClassID(const ACLSID: TGUID): TDCDVBClassFactory;
  end;

(*** DLL Exports **************************************************************)

  function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
  function DllCanUnloadNow: HResult; stdcall;
  function DllRegisterServer: HResult; stdcall;
  function DllUnregisterServer: HResult; stdcall;

implementation

var
  ClassFactoryCount: Integer = 0;
  ObjectCount: Integer = 0;
  ClassTemplate: TDCDVBClassTemplate;

(*** TDCDVBTuningPluginDevice *************************************************)

constructor TDCDVBTuningPluginDevice.Create(ADeviceName: String; ADeviceType: Integer; ADeviceID: String);
begin
  inherited Create;
  FAppCallback := nil;
  FDeviceName := ADeviceName;
  FDeviceType := ADeviceType;
  FDeviceID := ADeviceID;
  Initialize;
end;

destructor TDCDVBTuningPluginDevice.Destroy;
begin
  put_ApplicationCallback(nil);
  OnControlMessage(DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE, 0, 0);
  inherited Destroy;
end;

procedure TDCDVBTuningPluginDevice.Log(const AMethod: String; const AText: String);
begin
  if Assigned(FAppCallback)
    then FAppCallback.Log(PChar(AMethod), PChar(AText));
end;

function TDCDVBTuningPluginDevice.put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  Log('put_ControlMessage', 'Message: ' + inttostr(AMessage));
  Result := OnControlMessage(AMessage, AParam1, AParam2);
end;

function TDCDVBTuningPluginDevice.get_DeviceName(out ADeviceName: PChar): Integer;
begin
  if not Assigned(@ADeviceName) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ADeviceName := PChar(FDeviceName);
  Result := S_OK;
end;

function TDCDVBTuningPluginDevice.get_DeviceType(out ADeviceType: Integer): Integer;
begin
  if not Assigned(@ADeviceType) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ADeviceType := FDeviceType;
  Result := S_OK;
end;

function TDCDVBTuningPluginDevice.get_DeviceID(out ADeviceID: PChar): Integer;
begin
  if not Assigned(@ADeviceID) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ADeviceID := PChar(FDeviceID);
  Result := S_OK;
end;

function TDCDVBTuningPluginDevice.put_ApplicationCallback(ACallback: IDCDVBTuningPluginApplicationCallback): Integer;
begin
  FAppCallback := ACallback;

  Result := S_OK;
end;

procedure TDCDVBTuningPluginDevice.PushData(ABuffer: PByte; ASize: Integer);
begin
  if (FAppCallback <> nil)
    then FAppCallback.ReceiveTSStream(ABuffer, ASize);
end;

(*** TDCDVBTuningPlugin *******************************************************)

constructor TDCDVBTuningPlugin.Create(ATunerID: TGuid);
begin
  inherited Create;
  FTunerID := ATunerID;
  FDeviceList := TInterfaceList.Create;
  Initialize;
end;

destructor TDCDVBTuningPlugin.Destroy;
begin
  FDeviceList.Clear;
  FDeviceList := nil;
  inherited Destroy;
end;

function TDCDVBTuningPlugin.get_TunerID(out ATunerID: TGuid): Integer;
begin
  if not Assigned(@ATunerID) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ATunerID := FTunerID;
  Result := S_OK;
end;

function TDCDVBTuningPlugin.get_CountTunerDevices(out ACountTuningDevices: Integer): Integer;
begin
  if not Assigned(@ACountTuningDevices) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ACountTuningDevices := FDeviceList.Count;
  Result := S_OK;
end;

function TDCDVBTuningPlugin.get_TunerDevice(AIndex: Integer; out ATuningDevice: IDCDVBTuningPluginDevice): Integer;
begin
  if not Assigned(@ATuningDevice) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ATuningDevice := nil;

  if (AIndex < 0) or (AIndex >= FDeviceList.Count) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := FDeviceList[AIndex].QueryInterface(IID_IDCDVBTuningPluginDevice, ATuningDevice);
end;

function TDCDVBTuningPlugin.get_SDKVersion(out ASDKVersion: Cardinal): Integer;
begin
  if not Assigned(@ASDKVersion) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  ASDKVersion := DCDVB_TUNING_PLUGIN_SDK_VERSION;
  Result := S_OK;
end;

(*** TDCDVBClassFactory *******************************************************)

constructor TDCDVBClassFactory.Create(ATunerClass: TDCDVBTuningPluginClass; const ATunerID: TGUID; const AName: WideString; const ADescription: WideString; const AAuthor: WideString; const AVersion: WideString);
begin
  inherited Create;
  TDCDVBClassTemplate.GetTemplate.AddFactory(Self);
  FTunerClass := ATunerClass;
  FName := AName;
  FTunerID  := ATunerID;
  FDescription := ADescription;
  FAuthor := AAuthor;
  FVersion := AVersion;
end;

{$IFDEF FORMS}

constructor TDCDVBClassFactory.Create(AFormClass: TDCDVBFormPropertyPageClass; const APropID: TGUID);
begin
  inherited Create;
  TDCDVBClassTemplate.GetTemplate.AddFactory(Self);
  FPropID := APropID;
  FFormClass := AFormClass;
end;

{$ELSE}

constructor TDCDVBClassFactory.Create(APropClass: TDCDVBPropertyPageClass; const APropID: TGUID; ADialogID: Integer; const ACaption: WideString);
begin
  inherited Create;
  TDCDVBClassTemplate.GetTemplate.AddFactory(Self);
  FPropClass := APropClass;
  FPropID := APropID;
  FDialogID := ADialogID;
  FCaption := ACaption;
end;

{$ENDIF}

function TDCDVBClassFactory.CreateInstance(const unkOuter: IUnknown; const iid: TIID; out obj): HResult;
var
  tuner: TDCDVBTuningPlugin;
  property_page: TDCDVBPropertyPage;
begin
  if (@obj = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  Pointer(obj) := nil;

{$IFDEF FORMS}
  if FFormClass <> nil then
  begin
    property_page := TDCDVBPropertyPage.Create(FFormClass);
{$ELSE}
  if FPropClass <> nil then
  begin
    property_page := TDCDVBPropertyPageClass(FPropClass).Create(FDialogID, FCaption);
{$ENDIF}
    Result := property_page.QueryInterface(IID, obj);
    if (property_page.RefCount = 0)
      then property_page.Free;
  end else
  begin
    tuner := TDCDVBTuningPluginClass(FTunerClass).Create(FTunerID);
    Result := tuner.QueryInterface(IID, obj);
    if (tuner.RefCount = 0)
      then tuner.Free;
  end;
end;

procedure TDCDVBClassFactory.UpdateRegistry(ARegister: Boolean);
var
  file_name: array[0..MAX_PATH-1] of Char;
  classID, serverKeyName: String;
begin
  if FTunerClass <> nil
    then classID := GUIDToString(FTunerID)
    else classID := GUIDToString(FPropID);

  ServerKeyName := 'CLSID\' + classID + '\' + 'InprocServer32';
  if ARegister then
  begin
    CreateRegKey('CLSID\' + classID, '', FName);
    GetModuleFileName(HInstance, file_name, MAX_PATH);
    CreateRegKey(ServerKeyName, '', file_name);
    CreateRegKey(ServerKeyName, 'ThreadingModel', 'Both');
    if FTunerClass <> nil then
    begin
      CreateRegKey(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FTunerID), 'Description', FDescription);
      CreateRegKey(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FTunerID), 'Author', FAuthor);
      CreateRegKey(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FTunerID), 'Version', FVersion);
    end;
  end else
  begin
    DeleteRegKey(ServerKeyName);
    DeleteRegKey('CLSID\' + classID);
    if FTunerClass <> nil then
    begin
      DeleteRegKey(DCDVB_TUNING_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FTunerID));
    end;
  end;
end;

function TDCDVBClassFactory._AddRef: Integer;
begin
  Result := InterlockedIncrement(ClassFactoryCount);
end;

function TDCDVBClassFactory._Release: Integer;
begin
  Result := InterlockedDecrement(ClassFactoryCount);
end;

function TDCDVBClassFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := CoLockObjectExternal(Self, fLock, True);
  if fLock
    then InterlockedIncrement(ObjectCount)
    else InterlockedDecrement(ObjectCount);
end;

function TDCDVBClassFactory.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := _IF(GetInterface(IID, Obj), S_OK, E_NOINTERFACE);
end;

(*** TDCDVBClassTemplate ******************************************************)

constructor TDCDVBClassTemplate.Create;
begin
  inherited Create;
  FFactoryList := TList.Create;
end;

procedure TDCDVBClassTemplate.AddFactory(AFactory: TDCDVBClassFactory);
begin
  FFactoryList.Add(AFactory);
end;

class function TDCDVBClassTemplate.GetTemplate: TDCDVBClassTemplate;
begin
  if (ClassTemplate = nil)
    then ClassTemplate := TDCDVBClassTemplate.Create;
  Result := ClassTemplate;
end;

destructor TDCDVBClassTemplate.Destroy;
var
  i: Integer;
begin
  for i := 0 to FFactoryList.Count -1
    do TDCDVBClassFactory(FFactoryList[i]).Free;
  FFactoryList.Clear;
  FFactoryList.Free;
  inherited Destroy;
end;

function TDCDVBClassTemplate.GetFactoryFromClassID(const ACLSID: TGUID): TDCDVBClassFactory;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FFactoryList.Count -1 do
  begin
    if IsEqualGUID(TDCDVBClassFactory(FFactoryList[i]).FTunerID, ACLSID) or
       IsEqualGUID(TDCDVBClassFactory(FFactoryList[i]).FPropID, ACLSID) then
    begin
      Result := FFactoryList[i];
      Exit;
    end;
  end;
end;

function TDCDVBClassTemplate.RegisterServer(ARegister: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to FFactoryList.Count -1 do
  begin
    with TDCDVBClassFactory(FFactoryList[i]) do
    begin
      if ARegister
        then UpdateRegistry(False);
      UpdateRegistry(ARegister);
    end;
  end;
end;

(*** DLL Exports **************************************************************)

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
var
  factory: TDCDVBClassFactory;
begin
  factory := TDCDVBClassTemplate.GetTemplate.GetFactoryFromClassID(CLSID);
  if (factory <> nil) then
  begin
    Result := _IF(factory.GetInterface(IID, Obj), S_OK, E_NOINTERFACE);
  end else
  begin
    Pointer(Obj) := nil;
    Result := CLASS_E_CLASSNOTAVAILABLE;
  end;
end;

function DllCanUnloadNow: HResult;
begin
  Result := _IF((ObjectCount or ClassFactoryCount) = 0, S_OK, S_FALSE);
end;

function DllRegisterServer: HResult;
begin
  Result := _IF(TDCDVBClassTemplate.GetTemplate.RegisterServer(True), S_OK, E_FAIL);
end;

function DllUnregisterServer: HResult; 
begin
  Result := _IF(TDCDVBClassTemplate.GetTemplate.RegisterServer(False), S_OK, E_FAIL);
end;

initialization

finalization

  if (ClassTemplate <> nil) then
  begin
    ClassTemplate.Free;
    ClassTemplate := nil;
  end;

end.
