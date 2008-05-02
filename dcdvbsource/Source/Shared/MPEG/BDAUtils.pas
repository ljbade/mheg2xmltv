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

unit BDAUtils;

interface

uses
  Windows, DirectShow9, DSUtil, ActiveX, Classes, SysUtils, SHDocVw, Forms,
  ComCtrls, CommCtrl, StdCtrls, Graphics,

  BDAConst, Logger, DVBConst;

  function GetFilterFromSysEnum(Category: TGuid; Name: WideString; ID: WideString): IBaseFilter; overload;
  function CreateTuningSpace(AType: TDVBNetworkType): ITuningSpace;

  function IsFilterAvailable(CLSID: TGuid): Boolean;
  function GetFilterFromSysEnum(Category: TGuid; Name: WideString): IBaseFilter; overload;
  function GetFilterFromSystem(CLSID: TGuid): IBaseFilter;

  function GetTopology(AGraph: IFilterGraph2; ATopology: TGuid; Logger: TLogger = nil): IInterfaceList;
  function GetDeviceControl(AGraph: IFilterGraph2; Logger: TLogger = nil): IInterfaceList;

  function RemoveUnwantedChars(const From: String): String;
  function ReplaceUnwantedCharsHTML(const From: String): String;
  procedure WBLoadHTML(WebBrowser: TWebBrowser; const HTMLCode: string);
  procedure UpdateTrackbars(Form: TForm);

  function GetHexFromString(const AString: String): String;
  function GetStringFromHex(const AString: String): String;

  function FindPinWithMajorType(Filter: IBaseFilter; MajorType: TGuid; SubType: TGuid): IPin;

  procedure DrawComboBox(ComboBox: TComboBox; Index: Integer; Rect: TRect; State: TOwnerDrawState);

  function AddBackSlash(const S: String): String; overload;
  function AddBackSlash(const S: WideString): WideString; overload;
  function RemoveBackSlash(const S: String): String;
  function GetShortName(const AFilename: String): String;

  function FindFilterWithCLSID(Graph: IFilterGraph2; CLSID: TGuid): IBaseFilter;
  procedure UnmapPIDs(AMap: IMPEG2PIDMap; ALogger: TLogger);
  function GetUTCDifference: Integer;
  function GetValidDirectoryName(const AName: String): String;
  procedure FillList(Mapper: IFilterMapper2; Types: Pointer; Combo: TComboBox; List: TStringList; AMerit: Integer; ACountTypes: Integer);
  function IsWindowsXP: Boolean;
  function GetFilterFromLibrary(ALibrary: WideString; AClassID: TGuid): IBaseFilter;

implementation

function IsFilterAvailable(CLSID: TGuid): Boolean;
var
  Filter: IBaseFilter;
begin
  Result := (CoCreateInstance(CLSID, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Filter) = S_OK);
  Filter := nil;
end;

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

function GetFilterFromSysEnum(Category: TGuid; Name: WideString): IBaseFilter;
var
  sysenum: TSysDevEnum;
  i: integer;
begin
  Result := nil;

  sysenum := TSysDevEnum.Create(Category);
  for i := 0 to sysenum.CountFilters -1 do
  begin
    if (sysenum.Filters[i].FriendlyName = Name) then
    begin
      Result := sysenum.GetBaseFilter(i);
      break;
    end;
  end;
  sysenum.free;
end;

function GetFilterFromSystem(CLSID: TGuid): IBaseFilter;
var
  hr: HRESULT;
begin
  hr := CoCreateInstance(CLSID, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Result);
  if (hr <> S_Ok) then
  begin
    Result := nil;
    Exit;
  end;
end;

function GetTopology(AGraph: IFilterGraph2; ATopology: TGuid; Logger: TLogger = nil): IInterfaceList;
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
  filter_info: TFilterInfo;
begin
  Result := TInterfaceList.Create;
  if AGraph.EnumFilters(enum) = S_OK then
  begin
    while (enum.Next(1, filter, nil) = S_OK) do
    begin
      if Assigned(Logger) then
      begin
        filter.QueryFilterInfo(filter_info);
        filter_info.pGraph := nil;
        Log(Logger, nil, 'GetTopology',  'Searching in ' + filter_info.achName);
      end;
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
                            if Assigned(Logger) then
                            begin
                              Log(Logger, nil, 'GetTopology', 'Found in ' + filter_info.achName);
                            end;
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

function GetDeviceControl(AGraph: IFilterGraph2; Logger: TLogger = nil): IInterfaceList;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
  bda: IBDA_DeviceControl;
  class_id: TGuid;
  filter_info: TFilterInfo;
begin
  Result := TInterfaceList.Create;
  if AGraph.EnumFilters(enum) = S_OK then
  begin
    while (enum.Next(1, filter, nil) = S_OK) do
    begin
      filter.GetClassID(class_id);
      if not IsEqualGUID(class_id, CLSID_MPEG2Demultiplexer) then
      begin
        if Assigned(Logger) then
        begin
          filter.QueryFilterInfo(filter_info);
          filter_info.pGraph := nil;
          Log(Logger, nil, 'GetDeviceControl', 'Searching Device Control in ' + filter_info.achName);
        end;
        if filter.QueryInterface(IID_IBDA_DeviceControl, bda) = S_OK then
        begin
          if Assigned(Logger) then
          begin
            Log(Logger, nil, 'GetDeviceControl', 'Device Control found in ' + filter_info.achName);
          end;
          Result.Add(bda);
        end;
        bda := nil;
      end;
      filter := nil;
    end;
    enum := nil;
  end;
end;

function CreateTuningSpace(AType: TDVBNetworkType): ITuningSpace;
var
  hr: HRESULT;
  locator: ILocator;
begin
  case AType of
    ntATSC: hr := CoCreateInstance(CLSID_ATSCTuningSpace, nil, CLSCTX_INPROC_SERVER, IID_IATSCTuningSpace, Result);
    ntDVBS: hr := CoCreateInstance(CLSID_DVBSTuningSpace, nil, CLSCTX_INPROC_SERVER, IID_IDVBTuningSpace,  Result);
    else    hr := CoCreateInstance(CLSID_DVBTuningSpace,  nil, CLSCTX_INPROC_SERVER, IID_IDVBTuningSpace,  Result);
  end;

  if hr <> S_OK then
  begin
    Result := nil;
    Exit;
  end;

  case AType of
    ntDVBT:
    begin
      Result.put_UniqueName('DC-DVBt TuningSpace');
      Result.put_FriendlyName('DC-DVBt TuningSpace');
      Result.put_FrequencyMapping('-1');
      IDVBTuningSpace(Result).put_SystemType(DVB_Terrestrial);
      Result.put_NetworkType(GUIDToString(CLSID_DVBTNetworkProvider));
      hr := CoCreateInstance(CLSID_DVBTLocator, nil, CLSCTX_INPROC_SERVER, IID_IDVBTLocator, locator);
      if hr = S_OK
        then Result.put_DefaultLocator(locator);
    end;
    ntDVBS:
    begin
      Result.put_UniqueName('DC-DVBs TuningSpace');
      Result.put_FriendlyName('DC-DVBs TuningSpace');
      Result.put_FrequencyMapping('-1');
      IDVBSTuningSpace(Result).put_SystemType(DVB_Satellite);
      Result.put_NetworkType(GUIDToString(CLSID_DVBSNetworkProvider));
      hr := CoCreateInstance(CLSID_DVBSLocator, nil, CLSCTX_INPROC_SERVER, IID_IDVBSLocator, locator);
      if hr = S_OK
        then Result.put_DefaultLocator(locator);
    end;
    ntDVBC:
    begin
      Result.put_UniqueName('DC-DVBc TuningSpace');
      Result.put_FriendlyName('DC-DVBc TuningSpace');
      Result.put_FrequencyMapping('-1');
      IDVBTuningSpace(Result).put_SystemType(DVB_Cable);
      Result.put_NetworkType(GUIDToString(CLSID_DVBCNetworkProvider));
      hr := CoCreateInstance(CLSID_DVBCLocator, nil, CLSCTX_INPROC_SERVER, IID_IDVBCLocator, locator);
      if hr = S_OK
        then Result.put_DefaultLocator(locator);
    end;
    ntATSC:
    begin
      Result.put_UniqueName('DC-ATSC TuningSpace');
      Result.put_FriendlyName('DC-ATSC TuningSpace');
      Result.put_FrequencyMapping('-1');
      Result.put_NetworkType(GUIDToString(CLSID_ATSCNetworkProvider));
      hr := CoCreateInstance(CLSID_ATSCLocator, nil, CLSCTX_INPROC_SERVER, IID_IATSCLocator, locator);
      if hr = S_OK
        then Result.put_DefaultLocator(locator);
    end;
  end;
end;

function RemoveUnwantedChars(const From: String): String;
var
  c: Integer;
begin
  Result := From;
  c := Length(Result);
  while (c > 0) do
  begin
    if Byte(Result[c]) < $20
      then Delete(Result, c, 1);
    dec(c);
  end;

  Result := StringReplace(Result, char($8A), #13#10, [rfReplaceAll]); // Line Break
  Result := StringReplace(Result, char($86), '', [rfReplaceAll]); // Emphesis (Italic) On
  Result := StringReplace(Result, char($87), '', [rfReplaceAll]); // Emphesis (Italic) Off
end;

function ReplaceUnwantedCharsHTML(const From: String): String;
var
  c: Integer;
begin
  Result := From;
  c := Length(Result);
  while (c > 0) do
  begin
    if Byte(Result[c]) < $20
      then Delete(Result, c, 1);
    dec(c);
  end;

  Result := StringReplace(Result, char($8A), '<br>', [rfReplaceAll]); // Line Break
  Result := StringReplace(Result, char($86), '<i>', [rfReplaceAll]); // Emphesis (Italic) On
  Result := StringReplace(Result, char($87), '</i>', [rfReplaceAll]); // Emphesis (Italic) Off
end;

procedure WBLoadHTML(WebBrowser: TWebBrowser; const HTMLCode: string);
var
  sl: TStringList;
  ms: TMemoryStream;
  tmp: IStream;
begin
  if Assigned(WebBrowser.Document) then
  begin
    WebBrowser.Navigate('about:blank');

    sl := TStringList.Create;
    try
      ms := TMemoryStream.Create;
      try
        sl.Text := HTMLCode;
        sl.SaveToStream(ms);
        ms.Seek(0, 0);
        tmp := TStreamAdapter.Create(ms);
        (WebBrowser.Document as IPersistStreamInit).Load(tmp);
        tmp := nil;
      finally
        ms.Free;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure UpdateTrackbars(Form: TForm);
var
  i: integer;
begin
  for i := 0 to Form.ComponentCount -1 do
    if (Form.Components[i].InheritsFrom(TTrackBar)) then
      SetWindowLong(TTrackBar(Form.Components[i]).Handle, GWL_STYLE,
                    GetWindowLong(TTrackBar(Form.Components[i]).Handle,
                                  GWL_STYLE) and not TBS_ENABLESELRANGE);
end;

function GetHexFromString(const AString: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AString)
    do Result := Result + IntToHex(Byte(AString[i]), 2)
end;

function GetStringFromHex(const AString: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AString) shr 1 - 1
    do  Result := Result + Char(strtoint('$' + AString[i shl 1 + 1] + AString[i shl 1 + 2]));
end;

function FindPinWithMajorType(Filter: IBaseFilter; MajorType: TGuid; SubType: TGuid): IPin;
var
  PinList: TPinList;
  i: Integer;
  pmt: PAMMediaType;
  EnumMediaTypes: IEnumMediaTypes;
begin
  Result := nil;

  PinList := TPinList.Create(Filter);
  try
    for i := 0 to PinList.Count -1 do
    begin
      if PinList.Items[i].EnumMediaTypes(EnumMediaTypes) = S_OK then
      begin
        EnumMediaTypes.Next(1, pmt, nil);
        if Assigned(pmt) then
        begin
          if IsEqualGUID(MajorType, pmt.majortype) then
          begin
            Result := PinList.Items[i];
            DeleteMediaType(pmt);
            Exit;
          end;
         DeleteMediaType(pmt);
        end;
      end;
    end;
  finally
    PinList.Free;
  end;
end;

procedure DrawComboBox(ComboBox: TComboBox; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  back_color: TColor;
  text_color: TColor;
  text: String;
  x: Integer;
  w: Integer;
  ch: Char;
  i: Integer;
  s: TFontStyles;
begin
  if (odFocused in State) or (odSelected in State) then
  begin
    back_color := clHighlight;
    text_color := clHighlightText;
  end else
  begin
    back_color := clWindow;
    text_color := clWindowText;
  end;
  ComboBox.Canvas.Brush.Color := back_color;
  ComboBox.Canvas.Pen.Color := back_color;
  ComboBox.Canvas.Rectangle(Rect);
  ComboBox.Canvas.Pen.Color := text_color;
  text := ComboBox.Items[index];
  s := ComboBox.Canvas.Font.Style;
  Exclude(s, fsItalic);
  ComboBox.Canvas.Font.Style := s;

  x := Rect.Left + 2;
  w := 0;
  for i := 0 to Length(text) -1 do
  begin
    ch := text[i+1];
    if byte(ch) < $20 then
      Continue;

    if byte(ch) = $86 then
    begin
      Include(s, fsItalic);
      ComboBox.Canvas.Font.Style := s;
      Continue;
    end;

    if byte(ch) = $87 then
    begin
      Exclude(s, fsItalic);
      ComboBox.Canvas.Font.Style := s;
      Continue;
    end;
    ComboBox.Canvas.TextOut(x + w,  Rect.Top + 1, ch);
    inc(w, ComboBox.Canvas.TextWidth(ch) + 1);
  end;
end;

function AddBackSlash(const S: String): String;
begin
  if (Length(S) > 0) and (S[Length(S)] <> '\')
    then Result := S + '\'
    else Result := S;
end;

function AddBackSlash(const S: WideString): WideString;
begin
  if (Length(S) > 0) and (S[Length(S)] <> '\')
    then Result := S + '\'
    else Result := S;
end;

function RemoveBackSlash(const S: String): String;
begin
  if (Length(S) > 0) then
  begin
    if S[Length(S)] = '\' then
    begin
      Result := S;
      Delete(Result, Length(S), 1);
    end else
    begin
      Result := S;
    end;
  end else
  begin
    Result := S;
  end;
end;

function GetShortName(const AFilename: String): String;
var
  buffer: array[0..1024] of Char;
begin
  if GetShortPathName(PChar(AFilename), buffer, 1024) = 0
    then Result := AFilename
    else Result := StrPas(buffer);
end;

function FindFilterWithCLSID(Graph: IFilterGraph2; CLSID: TGuid): IBaseFilter;
var
  enum_filters: IEnumFilters;
  filter: IBaseFilter;
  cls: TGuid;
begin
  Result := nil;
  try
    if not Graph.EnumFilters(enum_filters) = S_OK
      then Exit;

    while enum_filters.Next(1, filter, nil) = S_OK do
    begin
      filter.GetClassID(cls);
      if IsEqualGUID(cls, CLSID) then
      begin
        Result := filter;
        Exit;
      end;
    end;
  finally
    filter := nil;
    enum_filters := nil;
  end;
end;

procedure UnmapPIDs(AMap: IMPEG2PIDMap; ALogger: TLogger);
var
  enum_map: IEnumPIDMap;
  map: TPIDMap;
  c: Cardinal;
  hr: HRESULT;
begin
  if not Assigned(AMap) then
  begin
    Log(ALogger, nil, 'UnmapPIDs',  'PID Mappings not Assigned');
    Exit;
  end;

  Log(ALogger, nil, 'UnmapPIDs',  'Enumerating PID Mappings');
  hr := AMap.EnumPIDMap(enum_map);
  Log(ALogger, nil, 'UnmapPIDs',  'EnumPIDMap returned 0x' + inttohex(hr, 8));
  if (hr = S_OK) then
  begin
    Log(ALogger, nil, 'UnmapPIDs',  'Calling next PID');
    hr := enum_map.Next(1, @map, c);
    Log(ALogger, nil, 'UnmapPIDs',  'Calling next PID returned 0x' + inttohex(hr, 8));
    while (hr = S_OK) do
    begin
      Log(ALogger, nil, 'UnmapPIDs',  'Unmapping PID 0x' + inttohex(map.ulPID, 4));
      hr := AMap.UnmapPID(1, @map.ulPID);
      Log(ALogger, nil, 'UnmapPIDs',  'Unmapping PID 0x' + inttohex(map.ulPID, 4) + ' returned 0x' + inttohex(hr, 8));
      Log(ALogger, nil, 'UnmapPIDs',  'Calling next PID');
      hr := enum_map.Next(1, @map, c);
      Log(ALogger, nil, 'UnmapPIDs',  'Calling next PID returned 0x' + inttohex(hr, 8));
    end;
  end else
  begin
    Log(ALogger, nil, 'UnmapPIDs',  'Enumerating PID Mappings');
  end;
  Log(ALogger, nil, 'UnmapPIDs',  'Unmapping done');
  enum_map := nil;
end;

function GetUTCDifference: Integer;
var
  info: TTimeZoneInformation;
begin
  Result := 0;
  if GetTimeZoneInformation(info) = TIME_ZONE_ID_DAYLIGHT
    then inc(Result, info.DaylightBias);
  inc(Result, info.Bias);
  Result := Result * -1;
end;

function GetValidDirectoryName(const AName: String): String;
begin
  Result := StringReplace(AName, '\', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '*', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '>', '_', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '|', '_', [rfReplaceAll, rfIgnoreCase]);
end;

procedure FillList(Mapper: IFilterMapper2; Types: Pointer; Combo: TComboBox; List: TStringList; AMerit: Integer; ACountTypes: Integer);
var
  Enum: IEnumMoniker;
  Moniker: IMoniker;
  PropBag: IPropertyBag;
  Name: OleVariant;
  FilterName: WideString;
  hr: HRESULT;
  Fetched: Cardinal;
begin
  if Assigned(Mapper) then
  begin
    if(Mapper.EnumMatchingFilters(
        Enum,
        0,                  // Reserved.
        True,               // Use exact match?
        AMerit,             // Minimum merit.
        True,               // At least one input pin?
        ACountTypes,        // Number of major type/subtype pairs for input.
        Types,              // Array of major type/subtype pairs for input.
        nil,                // Input medium.
        nil,                // Input pin category.
        False,              // Must be a renderer?
        True,               // At least one output pin?
        0,                  // Number of major type/subtype pairs for output.
        nil,                // Array of major type/subtype pairs for output.
        nil,                // Output medium.
        nil) = S_OK) then
    begin
      if Assigned(Enum) then
      begin
        while(Enum.Next(1, Moniker, @Fetched) = S_OK) do
        begin
          hr := Moniker.BindToStorage(nil, nil, IID_IPropertyBag, PropBag);
          if (Succeeded(hr)) then
          begin
            VariantInit(Name);
            hr := PropBag.Read('FriendlyName', Name, nil);
            if (SUCCEEDED(hr)) then
            begin
              FilterName := Name;
              combo.Items.Add(FilterName);
            end;
            hr := PropBag.Read('CLSID', Name, nil);
            if (SUCCEEDED(hr)) then
            begin
              FilterName := Name;
              List.Add(LowerCase(FilterName));
            end;
            VariantClear(Name);
          end;
          Moniker := nil;
        end;
        Enum := nil;
      end;
    end;
  end;
end;

function IsWindowsXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
            ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and
            (Win32MinorVersion >= 1)));
end;

function GetFilterFromLibrary(ALibrary: WideString; AClassID: TGuid): IBaseFilter;
type
  TDllGetClassObject = function(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
var
  lib: Cardinal;
  dll: TDllGetClassObject;
  factory: IClassFactory;
begin
  Result := nil;
  lib := CoLoadLibrary(PWideChar(ALibrary), True);
  if lib = 0
    then Exit;
  dll := GetProcAddress(lib, 'DllGetClassObject');
  if not Assigned(dll)
    then Exit;

  if dll(AClassID, IClassFactory, factory) <> S_OK
    then Exit;

  if factory.CreateInstance(nil, IID_IBaseFilter, Result) <> S_OK
    then Exit;
end;

end.
