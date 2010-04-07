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

unit DCDVBDataPlugins;

interface

uses
  Windows, Classes, ActiveX, ComObj, DirectShow9, DCDVBShared;

(*** Default Plugin Definitions ***********************************************)

const
  // Currently used SDK for the Data Plugins
  DCDVB_DATA_PLUGIN_SDK_VERSION     = $00000003;

  // Root Directory within HKEY_CLASSES_ROOT where Plugins needs to Register
  // themselves using a unique GUID
  DCDVB_DATA_PLUGIN_REGISTRY_ROOT   = 'DCDVBDataPlugins';

(*** Control Message Definitions **********************************************)

const
  DCDVB_DATA_PLUGIN_CTRL_SET_FILTERGRAPH       = 200; // Param1 = Pointer to IFilterGraph; Param2 = 0;
  DCDVB_DATA_PLUGIN_CTRL_PROGRAM_CHANGED       = 201; // Param1 = PProgram; Param2 = 0;
  DCDVB_DATA_PLUGIN_CTRL_LOAD_SETTINGS         = 202; // Param1 = Pointer to a Buffer; Param2 = Size of the Buffer
  DCDVB_DATA_PLUGIN_CTRL_SAVE_SETTINGS         = 203; // Param1 = Pointer to a Buffer, must be allocated using CoTaskAllocMem; Param2 = Pointer to Size of the Buffer

(*** Plugin Architecture Interface Definitions ********************************)

const
  IID_IDCDVBDataPlugin: TGuid = '{29BFE660-11C6-4B63-9B96-2A3AEB31E0DA}';

type
  IDCDVBDataPlugin = interface(IUnknown)
  ['{29BFE660-11C6-4B63-9B96-2A3AEB31E0DA}']
    function get_PluginName(out APluginName: PChar): Integer; stdcall;
    function get_PluginID(out APluginID: TGuid): Integer; stdcall;
    function put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; stdcall;
    function ProcessBuffer(AInBuffer: PByte; var AInSize: Integer): Integer; stdcall;
  end;

(*** Base Data Plugin Class ***************************************************)

type
  TDCDVBDataPlugin = class(TDCDVBBasePlugin, IDCDVBDataPlugin)
  protected
    FPluginID: TGuid;
    FPluginName: String;
    FFilterGraph: IFilterGraph;
    function OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; virtual;
  public
    constructor Create(APluginName: String; APluginID: TGuid); virtual;
    destructor Destroy; override;
    // IDCDVBDataPlugin
    function get_PluginName(out APluginName: PChar): Integer; virtual; stdcall;
    function get_PluginID(out APluginID: TGuid): Integer; virtual; stdcall;
    function put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; stdcall;
    function ProcessBuffer(AInBuffer: PByte; var AInSize: Integer): Integer; virtual; stdcall;
  published
    property PluginName: String read FPluginName write FPluginName;
    property PluginID: TGuid read FPluginID write FPluginID;
  end;
  TDCDVBDataPluginClass = class of TDCDVBDataPlugin;

(*** Custom COM Objects *******************************************************)

type
  TDCDVBClassFactory = class(TObject, IUnknown, IClassFactory)
  private
    FPluginClass: TDCDVBDataPluginClass;
    FName: WideString;
    FPluginID: TGUID;
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
    constructor Create(APluginClass: TDCDVBDataPluginClass; const APluginID: TGUID; const AName: WideString; const ADescription: WideString; const AAuthor: WideString; const AVersion: WideString); overload;
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

(*** TDCDVBDataPlugin *********************************************************)

constructor TDCDVBDataPlugin.Create(APluginName: String; APluginID: TGuid);
begin
  inherited Create;
  FPluginName := APluginName;
  FPluginID := APluginID;
  FFilterGraph := nil;
  Initialize;
end;

destructor TDCDVBDataPlugin.Destroy;
begin
  Pointer(FFilterGraph) := nil;
  inherited Destroy;
end;

function TDCDVBDataPlugin.get_PluginName(out APluginName: PChar): Integer;
begin
  if not Assigned(@APluginName) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  APluginName := PChar(FPluginName);
  Result := S_OK;
end;

function TDCDVBDataPlugin.get_PluginID(out APluginID: TGuid): Integer;
begin
  if not Assigned(@APluginID) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  APluginID := FPluginID;
  Result := S_OK;
end;

function TDCDVBDataPlugin.ProcessBuffer(AInBuffer: PByte; var AInSize: Integer): Integer;
begin
  Result := S_FALSE;
end;

function TDCDVBDataPlugin.put_ControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  if AMessage = DCDVB_DATA_PLUGIN_CTRL_SET_FILTERGRAPH then
  begin
    Pointer(FFilterGraph) := nil;
    Pointer(FFilterGraph) := Pointer(AParam1);
    Result := S_OK;
  end else
  begin
    Result := OnControlMessage(AMessage, AParam1, AParam2);
  end;
end;

function TDCDVBDataPlugin.OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  Result := E_FAIL;
end;

(*** TDCDVBClassFactory *******************************************************)

constructor TDCDVBClassFactory.Create(APluginClass: TDCDVBDataPluginClass; const APluginID: TGUID; const AName: WideString; const ADescription: WideString; const AAuthor: WideString; const AVersion: WideString);
begin
  inherited Create;
  TDCDVBClassTemplate.GetTemplate.AddFactory(Self);
  FPluginClass := APluginClass;
  FName := AName;
  FPluginID  := APluginID;
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
  plugin: TDCDVBBasePlugin;
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
    plugin := TDCDVBDataPluginClass(FPluginClass).Create(FName, FPluginID);
    Result := plugin.QueryInterface(IID, obj);
    if (plugin.RefCount = 0)
      then plugin.Free;
  end;
end;

procedure TDCDVBClassFactory.UpdateRegistry(ARegister: Boolean);
var
  file_name: array[0..MAX_PATH-1] of Char;
  classID, serverKeyName: String;
begin
  if FPluginClass <> nil
    then classID := GUIDToString(FPluginID)
    else classID := GUIDToString(FPropID);

  ServerKeyName := 'CLSID\' + classID + '\' + 'InprocServer32';
  if ARegister then
  begin
    CreateRegKey('CLSID\' + classID, '', FName);
    GetModuleFileName(HInstance, file_name, MAX_PATH);
    CreateRegKey(ServerKeyName, '', file_name);
    CreateRegKey(ServerKeyName, 'ThreadingModel', 'Both');
    if FPluginClass <> nil then
    begin
      CreateRegKey(DCDVB_DATA_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FPluginID), 'Description', FDescription);
      CreateRegKey(DCDVB_DATA_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FPluginID), 'Author', FAuthor);
      CreateRegKey(DCDVB_DATA_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FPluginID), 'Version', FVersion);
    end;
  end else
  begin
    DeleteRegKey(ServerKeyName);
    DeleteRegKey('CLSID\' + classID);
    if FPluginClass <> nil then
    begin
      DeleteRegKey(DCDVB_DATA_PLUGIN_REGISTRY_ROOT + '\' + GUIDToString(FPluginID));
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
    if IsEqualGUID(TDCDVBClassFactory(FFactoryList[i]).FPluginID, ACLSID) or
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
