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

unit DVBFilter;

interface

{$I Compiler.inc}

uses
  Windows, BaseClass, SysUtils, DirectShow9, Registry, ActiveX, BDAUtils,
  DVBInterface, DVBSettings, DVBGraphBuilder, DVBEPG, BDAConst, IDVBSource,
  DSUtil, JvTrayIcon, Controls, Classes, FormQuickAccess, Forms, ShellAPI,
  Messages, Menus, FormQuickEPG;

type
  TMyPopupMenu = class(TPopupMenu)
  private
    FMyPopupList: TPopupList;
  public
    procedure Popup(X, Y: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TOnChannelChange = procedure(AIndex: Integer) of Object;

  TDCDVBSource = class(TBCBaseFilter, IFileSourceFilter, IAMStreamSelect, IPersist,
                       ISpecifyPropertyPages, IDCDVBSource, IDemuxControl,
                       IAMMediaContent)
  private
  {$IFDEF USE_FORCE_DEBUG}
    FFDLib: THandle;
  {$ENDIF}
    FMenu: TMyPopupMenu;
    FTrayIcon: TJvTrayIcon;
    FForm: TfrmQuickAccess;
    FFormEPG: TfrmQuickEPG;
    FInitialSet: Boolean;

    FCurrentAudioStream: Integer;
    FCurrentSubtitleStream: Integer;
    FFirstRun: Boolean;

    FSettings: TDVBSettings;
    FGraphBuilder: TDVBGraphBuilder;
    FEPG: TDVBEPG;

    FTeletextShow: Boolean;
    FTeletextTransparent: Boolean;
    FTeletextPage: Integer;
    FTeletextSubPage: Integer;
    FMemoryMapping: THandle;
    FTeletextSizeMode: TTeletextSizeMode;
    FLastLastChannel: Integer;
    FDecodingDisabled: Boolean;
  public
    procedure SetupQuickForm;
    procedure ChangeQuickFormChannel;
  public
    constructor Create(const Name: string; unk: IUnknown; const clsid: TGUID; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;

    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult; override;
    function GetPinCount: integer; override;
    function GetPin(n: Integer): TBCBasePin; override;
    function Run(Start: Int64): HRESULT; override; stdcall;
    function Stop: HRESULT; override; stdcall;

    function JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT; override; stdcall;

    // IFileSourceFilter
    function Load(pszFileName: PWCHAR; const pmt: PAMMediaType): HResult; stdcall;
    function GetCurFile(out ppszFileName: PWideChar; pmt: PAMMediaType): HResult; stdcall;

    // IAMStreamSelect
    function Count(out pcStreams: DWORD): HResult; stdcall;
    function Info(lIndex: Longint; out ppmt: PAMMediaType; out pdwFlags: DWORD; out plcid: LCID; out pdwGroup: DWORD; out ppszName: PWCHAR; out ppObject: IUnknown; out ppUnk : IUnknown): HResult; stdcall;
    function Enable(lIndex: Longint; dwFlags: DWORD): HResult; stdcall;

    // ISpecifyPropertyPages
    function GetPages(out pages: TCAGUID): HResult; stdcall;

    // IDCDVBSource
    function get_Version(out Version: Cardinal): HRESULT; stdcall;
    function get_SignalStatistics(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): HRESULT; stdcall;
    function get_ChannelCount(out ChannelCount: Integer): HRESULT; stdcall;
    function get_ChannelInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_ChannelSelected(out Index: Integer): HRESULT; stdcall;
    function put_ChannelSelected(Index: Integer): HRESULT; stdcall;
    function put_PreviousChannel: HRESULT; stdcall;
    function put_NextChannel: HRESULT; stdcall;
    function put_EPGClearAll: HRESULT; stdcall;
    function get_EPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): HRESULT; stdcall;
    function get_EPGTimeOffset(out TimeOffset: Integer): HRESULT; stdcall;
    function put_TeletextShow(Show: LongBool): HRESULT; stdcall;
    function get_TeletextShow(out Show: LongBool): HRESULT; stdcall;
    function put_TeletextTransparent(Transparent: LongBool): HRESULT; stdcall;
    function get_TeletextTransparent(out Transparent: LongBool): HRESULT; stdcall;
    function put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT; stdcall;
    function get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT; stdcall;
    function put_TeletextNumber(Number: Integer): HRESULT; stdcall;
    function put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT; stdcall;
    function get_AudioStreamCount(out CountStreams: Integer): HRESULT; stdcall;
    function get_AudioStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_AudioStreamSelected(out Index: Integer): HRESULT; stdcall;
    function put_AudioStreamSelected(Index: Integer): HRESULT; stdcall;
    function get_MHPRoot(out MHP: PChar): HRESULT; stdcall;
    function get_RecordingsCount(out Count: Integer): HRESULT; stdcall;
    function get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_Recording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_EditRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function get_SubtitleStreamCount(out CountStreams: Integer): HRESULT; stdcall;
    function get_SubtitleStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_SubtitleStreamSelected(out Index: Integer): HRESULT; stdcall;
    function put_SubtitleStreamSelected(Index: Integer): HRESULT; stdcall;
    function ShowOSD: HRESULT; stdcall;
    function get_StreamInfo(out AStreamInfo: TStreamInfo): HRESULT; stdcall;

    // IDemuxControl
    function put_VideoPID(APID: Integer; AType: TVideoType): HRESULT; stdcall;
    function get_VideoPID(out APID: Integer; out AType: TVideoType): HRESULT; stdcall;
    function put_AudioPID(APID: Integer; AType: TAudioType): HRESULT; stdcall;
    function get_AudioPID(out APID: Integer; out AType: TAudioType): HRESULT; stdcall;
    function put_TeletextPID(APID: Integer): HRESULT; stdcall;
    function get_TeletextPID(out APID: Integer): HRESULT; stdcall;
    function put_SubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer): HRESULT; stdcall;
    function get_SubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer): HRESULT; stdcall;

    // TrayIcon
    procedure OnTrayIconClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnTrayIconDblClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure WriteTSStream(AWrite: Boolean);
    function GetReceivedBytes: Int64;
    function put_TeletextSizeMode(ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
    function get_TeletextSizeMode(out ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
    function get_PluginsCount(out ACount: Integer): HRESULT; stdcall;
    function put_PluginEnabled(AIndex: Integer; AEnabled: Boolean): HRESULT; stdcall;
    function get_PluginEnabled(AIndex: Integer; out AEnabled: Boolean): HRESULT; stdcall;
    function get_Plugin(AIndex: Integer; out APlugin: IUnknown): HRESULT; stdcall;
    function put_TeletextFastext(AFastext: TTeletextFastext): HRESULT; stdcall;
    function put_CaptureVideoWindowCursor(ACapture: Boolean): HRESULT; stdcall;
    function put_OSDChannel(AChannel: String): HRESULT; stdcall;

    // IAMMediaContent
    function get_AuthorName(var pbstrAuthorName: TBSTR): HResult; stdcall;
    function get_Title(var pbstrTitle: TBSTR): HResult; stdcall;
    function get_Rating(var pbstrRating: TBSTR): HResult; stdcall;
    function get_Description(var pbstrDescription: TBSTR): HResult; stdcall;
    function get_Copyright(var pbstrCopyright: TBSTR): HResult; stdcall;
    function get_BaseURL(var pbstrBaseURL: TBSTR): HResult; stdcall;
    function get_LogoURL(var pbstrLogoURL: TBSTR): HResult; stdcall;
    function get_LogoIconURL(var pbstrLogoURL: TBSTR): HResult; stdcall;
    function get_WatermarkURL(var pbstrWatermarkURL: TBSTR): HResult; stdcall;
    function get_MoreInfoURL(var pbstrMoreInfoURL: TBSTR): HResult; stdcall;
    function get_MoreInfoBannerImage(var pbstrMoreInfoBannerImage: TBSTR): HResult; stdcall;
    function get_MoreInfoBannerURL(var pbstrMoreInfoBannerURL: TBSTR): HResult; stdcall;
    function get_MoreInfoText(var pbstrMoreInfoText: TBSTR): HResult; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  public
    OnChannelChange1: TOnChannelChange;
    OnChannelChange2: TOnChannelChange;
    property Settings: TDVBSettings read FSettings;
    property EPG: TDVBEPG read FEPG;
    procedure OnPopup(Sender: TObject);
    procedure OnClick(Sender: TObject);
    function put_CycleChannels: HRESULT; stdcall;
    function put_CycleTeletext: HRESULT; stdcall;
  end;

  function DllRegisterServer: HResult; stdcall;
  function DllUnregisterServer: HResult; stdcall;

implementation

uses
  DVBChannelList, DVBRecordings, DateUtils, Math;

constructor TMyPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopupList.Remove(Self);
  FMyPopupList := TPopupList.Create;
  FMyPopupList.Add(Self);
  Items.OnClick := DoPopup;
  AutoPopup := False;
end;

destructor TMyPopupMenu.Destroy;
begin
  inherited Destroy;
  FMyPopupList.Remove(Self);
  FMyPopupList.Free;
end;

procedure TMyPopupMenu.Popup(X, Y: Integer);
const
  Flags: array[Boolean, TPopupAlignment] of Word =
    ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
     (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
var
  AFlags: Integer;
begin
  DoPopup(Self);
  AdjustBiDiBehavior;
  AFlags := Flags[UseRightToLeftAlignment, Alignment] or Buttons[TrackButton] or
    (Byte(MenuAnimation) shl 10);
  TrackPopupMenu(Items.Handle, AFlags, X, Y, 0 { reserved }, FMyPopupList.Window, nil);
end;

(*** TDCDVBSource *************************************************************)

constructor TDCDVBSource.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, CLSID_DCDVBSource, hr);
end;

constructor TDCDVBSource.Create(const Name: string; unk: IUnknown; const clsid: TGUID; out hr: HRESULT);
begin
  inherited Create(Name, unk, TBCCritSec.Create, clsid, hr);
{$IFDEF USE_FORCE_DEBUG}
  if FileExists('C:\ForceDebug.dll')
    then FFDLib := LoadLibrary('C:\ForceDebug.dll');
{$ENDIF}
  FLastLastChannel := -1;
  FDecodingDisabled := False;
  FSettings := TDVBSettings.Create;
  FEPG := TDVBEPG.Create(FSettings);
  FGraphBuilder := TDVBGraphBuilder.Create(FSettings, FEPG);
  FTeletextShow := False;
  FTeletextTransparent := False;
  FTeletextPage := 100;
  FTeletextSubPage := 0;
  FMenu := TMyPopupMenu.Create(nil);
  FMenu.OnPopup := OnPopup;
  FFirstRun := True;
  FCurrentAudioStream := 0;
  FCurrentSubtitleStream := -1;
  FTrayIcon := TJvTrayIcon.Create(nil);
  FTrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(102));
  FTrayIcon.Hint := 'DC-DVB Source';
  FTrayIcon.Active := True;
  FTrayIcon.OnClick := OnTrayIconClick;
  FTrayIcon.OnDblClick := OnTrayIconDblClick;
  FTrayIcon.PopupMenu := FMenu;

  FInitialSet := True;
  FTeletextSizeMode := tsmNormal;
end;

destructor TDCDVBSource.Destroy;
begin
  FTrayIcon.Free;
  if (FMemoryMapping <> 0) then
  begin
    CloseHandle(FMemoryMapping);
    FMemoryMapping := 0;
  end;
  FGraphBuilder.Free;
  FEPG.Free;
  FSettings.Free;
  FMenu.Free;
{$IFDEF USE_FORCE_DEBUG}
  if FFDLib <> 0
    then FreeLibrary(FFDLib);
{$ENDIF}
  inherited Destroy;
end;

function TDCDVBSource.GetPinCount: integer;
begin
  Result := 0;
end;

function TDCDVBSource.GetPin(n: Integer): TBCBasePin;
begin
  Result := nil;
end;

function TDCDVBSource.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IID_IAMStreamSelect) or
     IsEqualGUID(IID, IID_IPersist) or
     IsEqualGUID(IID, ISpecifyPropertyPages) or
     IsEqualGUID(IID, IID_IDCDVBSource) or
     IsEqualGUID(IID, IID_IFileSourceFilter) then
  begin
    if GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited NonDelegatingQueryInterface(IID, Obj);
  end;
end;

function TDCDVBSource.Run(Start: Int64): HRESULT;
begin
  Result := inherited Run(Start);

  if FFirstRun then
  begin
    FGraphBuilder.Activate;
    if (FSettings.CurrentChannel <> -1) and not FSettings.TimeShiftingEnabled and FSettings.SaveLastChannel
      then put_ChannelSelected(FSettings.CurrentChannel);
    FFirstRun := False;
  end;
end;

function TDCDVBSource.Stop: HRESULT;
begin
  Result := inherited Stop();
end;

function TDCDVBSource.JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT;

  procedure RemoveForm;
  begin
    if FForm <> nil then
    begin
      FForm.Timer1.Enabled := False;
      FForm.Hide;
      try
        SendMessage(FForm.Handle, WM_CLOSE, 0, 0);
      except
      end;
      FForm := nil;
    end;
  end;

  procedure RemoveEPGForm;
  begin
    if FFormEPG <> nil then
    begin
      FFormEPG.Kill := True;
      try
        SendMessage(FFormEPG.Handle, WM_CLOSE, 0, 0);
      except
      end;
      FFormEPG := nil;
    end;
  end;

begin
  if (pGraph = nil) then
  begin
    RemoveForm;
    RemoveEPGForm;
    FGraphBuilder.Clear;
  end else
  begin
    RemoveForm;
    RemoveEPGForm;
    FForm := TfrmQuickAccess.Create(nil);
    FForm.Filter := Pointer(Self);
    FForm.Visible := False;
    FFormEPG := TfrmQuickEPG.Create(nil);
    FFormEPG.Filter := Pointer(Self);
    FFormEPG.Visible := False;
  end;
    
  Set8087CW($133F);
  Result := inherited JoinFilterGraph(pGraph, pName);
  Set8087CW($133F);
end;

(*** IFileSourceFilter ********************************************************)

function TDCDVBSource.Load(pszFileName: PWCHAR; const pmt: PAMMediaType): HResult;
var
  map: PCardinal;
begin
  Result := E_FAIL;

  if not Assigned(pszFileName) or not Assigned(FGraph)
    then Exit;

  if (GetFileAttributesW(pszFileName) = $FFFFFFFF)
    then Exit;

  if not FSettings.LoadFromFile(pszFileName)
    then Exit;

  if not FGraphBuilder.Render(FGraph as IFilterGraph2)
    then Exit;

  // This is for the Windows Media Player Plugin. Those Microsoft Dickheads
  // didn't specify a way to access the Graph, so we use a Memory Mapping
  // for interaction. 
  if (FMemoryMapping = 0) then
  begin
    FMemoryMapping := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, 4, 'DCDVBSource2');
    if (FMemoryMapping <> 0) then
    begin
      map := MapViewOfFile(FMemoryMapping, FILE_MAP_WRITE, 0, 0, 0);
      if (map <> nil) then
      begin
        map^ := Cardinal(Self);
      end;
      UnmapViewOfFile(map);
    end;
  end;

  SetupQuickForm;

  Result := S_OK;
end;

function TDCDVBSource.GetCurFile(out ppszFileName: PWideChar; pmt: PAMMediaType): HResult;
begin
  if not Assigned(@ppszFileName) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if (FSettings.FileName  = '') then
  begin
    ppszFileName := nil;
    Result := E_FAIL;
    Exit;
  end;

  AMGetWideString(FSettings.FileName, ppszFileName);
  Result := S_OK;
end;

(*** IAMStreamSelect **********************************************************)

function TDCDVBSource.Count(out pcStreams: DWORD): HResult;
var
  c: Cardinal;
begin
  pcStreams := FSettings.Channels.Count;
  Result := S_OK;

  if (FSettings.CurrentChannel < 0)
    then Exit;

  if (FSettings.CurrentChannel >= FSettings.Channels.Count)
    then Exit;

  c := FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount;
  if (c > 0)
    then inc(pcStreams, c);

  c := IfThen(FSettings.DisableOSD, 0, FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount);
  if (c > 0)
    then inc(pcStreams, c);
end;

type
  TDVBStreamSelectInfo = class(TInterfacedObject, IAMStreamSelectInfo)
  private
    FName: String;
    FNowEvent: String;
    FNowStart: TDateTime;
    FNowEnd: TDateTime;
    FNextEvent: String;
    FNextStart: TDateTime;
    FNextEnd: TDateTime;
  protected
    function get_Name(out Name: PWideChar): HRESULT; stdcall;
    function get_Now(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
    function get_Next(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
  end;

function TDVBStreamSelectInfo.get_Name(out Name: PWideChar): HRESULT;
begin
  if FName = '' then
  begin
    Result := E_FAIL;
    Name := nil;
  end else
  begin
    Result := AMGetWideString(FName, Name);
  end;
end;

function TDVBStreamSelectInfo.get_Now(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT;
begin
  if FNowEvent = '' then
  begin
    Result := E_FAIL;
    FillChar(Start, SizeOf(TEventDate), 0);
    FillChar(End_, SizeOf(TEventDate), 0);
    Event := nil;
  end else
  begin
    Result := AMGetWideString(FNowEvent, Event);
    Start := GetDate(FNowStart);
    End_ := GetDate(FNowEnd);
  end;
end;

function TDVBStreamSelectInfo.get_Next(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT;
begin
  if FNextEvent = '' then
  begin
    Result := E_FAIL;
    FillChar(Start, SizeOf(TEventDate), 0);
    FillChar(End_, SizeOf(TEventDate), 0);
    Event := nil;
  end else
  begin
    Result := AMGetWideString(FNextEvent, Event);
    Start := GetDate(FNextStart);
    End_ := GetDate(FNextEnd);
  end;
end;

function TDCDVBSource.Info(lIndex: Longint; out ppmt: PAMMediaType; out pdwFlags: DWORD; out plcid: LCID; out pdwGroup: DWORD; out ppszName: PWCHAR; out ppObject: IUnknown; out ppUnk : IUnknown): HResult;
var
  str: String;
  str2: String;
  c, d: Cardinal;
  s: TDVBStreamSelectInfo;
  event: TDVBEPGEvent;
  link: PDVBEPGPremiereLinkage;
  i: Integer;
  channel: TDVBChannel;
  found: Boolean;
begin
  c := 0;
  d := 0;

  if (FSettings.CurrentChannel >= 0) and (FSettings.CurrentChannel < FSettings.Channels.Count) then
  begin
    c := FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount;
    d := IfThen(FSettings.DisableOSD, 0, FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount);
  end;

  if (lIndex < 0) or (lIndex >= Int64(FSettings.Channels.Count) + c + d) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if Assigned(@ppmt)
    then ppmt := nil;

  if Assigned(@pdwGroup) then
  begin
    if lIndex < FSettings.Channels.Count
      then pdwGroup := 1
    else if lIndex - FSettings.Channels.Count < Int64(c)
      then pdwGroup := 2
    else pdwGroup := 3;
  end;

  if Assigned(@plcid)
    then plcid := GetUserDefaultLCID;

  if Assigned(@pdwFlags) then
  begin
    pdwFlags := 0;
    if lIndex < FSettings.Channels.Count then
    begin
      if (FSettings.CurrentChannel = lIndex)
        then pdwFlags := AMSTREAMSELECTINFO_ENABLED;
    end else
    begin
      if lIndex = FSettings.Channels.Count + FCurrentAudioStream
        then pdwFlags := AMSTREAMSELECTINFO_ENABLED
      else if (FCurrentSubtitleStream <> -1) and (lIndex = FCurrentSubtitleStream + Int64(c) + FSettings.Channels.Count)
        then pdwFlags := AMSTREAMSELECTINFO_ENABLED;
    end;
  end;

  if Assigned(@ppszName) then
  begin
    if lIndex < FSettings.Channels.Count then
    begin
      if FEPG.GetCurrentProgram(lIndex, str)
          then AMGetWideString(RemoveUnwantedChars(FSettings.Channels[lIndex].Name) + Char($9) + RemoveUnwantedChars(str), ppszName)
          else AMGetWideString(RemoveUnwantedChars(FSettings.Channels[lIndex].Name), ppszName);

      if FEPG.GetCurrentProgram(lIndex, event) then
      begin
        for c := 0 to FSettings.Channels.Count - 1 do
        begin
          found := False;
          channel := FSettings.Channels[c];
          for i := 0 to event.PremiereLinkCount -1 do
          begin
            link := event.PremiereLink[i];
            if (channel.Network.TSID = link.TSID) and
               (channel.Network.ONID = link.ONID) and
               (channel.SID = link.SID) then
            begin
              channel.Name := link.Name;
              found := True;
            end;
          end;
          if not found then
          begin
//            channel.Name := channel.OriginalName
          end;
        end;
      end;

      if Assigned(@ppUnk) then
      begin
        s := TDVBStreamSelectInfo.Create;
        s.FName := RemoveUnwantedChars(FSettings.Channels[lIndex].Name);
        FEPG.GetNowNext(lIndex, s.FNowEvent, s.FNowStart, s.FNowEnd, s.FNextEvent, s.FNextStart, s.FNextEnd);
        ppUnk := s;
      end;
    end else
    begin
      if lIndex < FSettings.Channels.Count + Int64(c) then
      begin
        str2 := FSettings.Channels[FSettings.CurrentChannel].GetAudioStreamExtension(lIndex - FSettings.Channels.Count);
        if str2 <> ''
          then str2 := ' ' + str2;
        AMGetWideString('Audio Stream ' + inttostr(lIndex - FSettings.Channels.Count + 1) + Char($9) + '[' + FSettings.Channels[FSettings.CurrentChannel].GetAudioStreamName(lIndex - FSettings.Channels.Count) + ']' + str2, ppszName)
      end else
      begin
        AMGetWideString('Subtitle Stream '+ inttostr(lIndex - FSettings.Channels.Count - Int64(c) + 1) + Char($9) + '[' + FSettings.Channels[FSettings.CurrentChannel].GetSubtitleStreamName(lIndex - FSettings.Channels.Count - Int64(c)) + ']', ppszName);
      end;
    end;
  end;

  Result := S_OK;
end;

function TDCDVBSource.Enable(lIndex: Longint; dwFlags: DWORD): HResult;
var
  c, d: Cardinal;
  idx: Integer;
begin
  Result := S_FALSE;

  c := 0;
  d := 0;
  if (FSettings.CurrentChannel >= 0) and (FSettings.CurrentChannel < FSettings.Channels.Count) then
  begin
    c := FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount;
    d := IfThen(FSettings.DisableOSD, 0, FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount);
  end;

  if (lIndex < 0) or (lIndex >= Int64(FSettings.Channels.Count) + c + d) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if (dwFlags = 0) or BOOL(dwFlags and AMSTREAMSELECTENABLE_ENABLEALL) then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;

  if lIndex < FSettings.Channels.Count then
  begin
    if (FSettings.CurrentChannel = lIndex) and FSettings.StopSelect then
    begin
      FDecodingDisabled := True;
      FCurrentAudioStream := -1;
      FCurrentSubtitleStream := -1;
      FLastLastChannel := FSettings.CurrentChannel;
      FSettings.CurrentChannel := -1;
      FGraphBuilder.StopChannel;
    end else
    begin
      FCurrentAudioStream := FSettings.Channels[lIndex].DefaultAudioStreamIndex;
      FCurrentSubtitleStream := FSettings.Channels[lIndex].DefaultSubtitleStreamIndex;
      FLastLastChannel := FSettings.CurrentChannel;
      FSettings.CurrentChannel := lIndex;
      ChangeQuickFormChannel;
      FGraphBuilder.TuneToChannel(FSettings.CurrentChannel);
      FSettings.MHPParser.Clear;
      if Assigned(OnChannelChange1)
        then OnChannelChange1(FSettings.CurrentChannel);
      if Assigned(OnChannelChange2)
        then OnChannelChange2(FSettings.CurrentChannel);

      FSettings.MHPParser.SavePath := FSettings.MHPDirectory + GetValidDirectoryName(RemoveUnwantedChars(FSettings.Channels[lIndex].Name)) + '\';
    end;
  end else
  begin
    if lIndex < FSettings.Channels.Count + Int64(c) then
    begin
      idx := lIndex - FSettings.Channels.Count;
      if ((FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count)) or
         (idx >= FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount)
         then Exit;
      FCurrentAudioStream := idx;
      FGraphBuilder.SetAudioStream(FSettings.CurrentChannel, idx);
    end else
    begin
      idx := lIndex - FSettings.Channels.Count - Int64(c);
      if ((FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count)) or
         (idx >= FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount)
         then Exit;
      if FCurrentSubtitleStream = idx then
      begin
        FCurrentSubtitleStream := -1;
        FGraphBuilder.DisableSubtitleStream;
      end else
      begin
        FCurrentSubtitleStream := idx;
        FGraphBuilder.SetSubtitleStream(FSettings.CurrentChannel, idx);
      end;
    end;
  end;

  Result := S_OK;
end;

(*** ISpecifyPropertyPages ****************************************************)

function TDCDVBSource.GetPages(out pages: TCAGUID): HResult;
begin
  Pages.cElems := 7;
  Pages.pElems := CoTaskMemAlloc(sizeof(TGUID) * Pages.cElems);

  if (Pages.pElems = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;

{$IFDEF FALSE}
  Pages.pElems^[0] := CLSID_PropertyPageSettings;
  Pages.pElems^[1] := CLSID_PropertyPageEPG;
  Pages.pElems^[2] := CLSID_PropertyPageMHP;
  Pages.pElems^[3] := CLSID_PropertyPageRecordings;
  Pages.pElems^[4] := CLSID_PropertyPagePlugins;
  Pages.pElems^[5] := CLSID_PropertyPageStatistics;
  Pages.pElems^[6] := CLSID_PropertyPageAbout;
{$ELSE}
  Pages.pElems^[0] := CLSID_PropertyPageEPG;
  Pages.pElems^[1] := CLSID_PropertyPagePlugins;
  Pages.pElems^[2] := CLSID_PropertyPageStatistics;
  Pages.pElems^[3] := CLSID_PropertyPageRecordings;
  Pages.pElems^[4] := CLSID_PropertyPageMHP;
  Pages.pElems^[5] := CLSID_PropertyPageSettings;
  Pages.pElems^[6] := CLSID_PropertyPageAbout;
{$ENDIF}

  Result := S_OK;
end;

(*** IDVBSource ***************************************************************)

function TDCDVBSource.get_Version(out Version: Cardinal): HRESULT; stdcall;
begin
  Version := DVB_FILTER_VERSION;
  Result := S_OK;
end;

function TDCDVBSource.get_SignalStatistics(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): HRESULT;
begin
  if FGraphBuilder.GetSignalStatistic(Strength, Quality, SignalPresent, SignalLocked)
    then Result := S_OK
    else Result := E_FAIL;
end;

function TDCDVBSource.get_ChannelCount(out ChannelCount: Integer): HRESULT;
begin
  ChannelCount := FSettings.Channels.Count;
  Result := S_OK;
end;

function TDCDVBSource.get_ChannelInfo(Index: Integer; out Name: PChar): HRESULT;
var
  s: String;
begin
  Result := S_FALSE;
  Name := '';
  if (Index < 0) or (Index >= FSettings.Channels.Count)
    then Exit;

  s := FSettings.Channels[Index].Name;
  if (Length(s) = 0)
    then s := 'Unknown';

  Name := CoTaskMemAlloc(Length(s) +1);
  Move(s[1], Name^, Length(s) +1);

  Result := S_OK;
end;

function TDCDVBSource.get_ChannelSelected(out Index: Integer): HRESULT;
begin
  Index := FSettings.CurrentChannel;
  Result := S_OK;
end;

function TDCDVBSource.put_ChannelSelected(Index: Integer): HRESULT;
begin
  Result := Enable(Index, AMSTREAMSELECTENABLE_ENABLE);
end;

function TDCDVBSource.put_PreviousChannel: HRESULT;
var
  c: Integer;
begin
  Result := S_FALSE;
  if FSettings.Channels.Count = 0
    then Exit;
  c := FSettings.CurrentChannel;
  dec(c);
  if c < 0
    then c := FSettings.Channels.Count -1;
  put_ChannelSelected(c);
  Result := S_OK;
end;

function TDCDVBSource.put_NextChannel: HRESULT;
var
  c: Integer;
begin
  Result := S_FALSE;
  if FSettings.Channels.Count = 0
    then Exit;
  c := FSettings.CurrentChannel;
  inc(c);
  if c > FSettings.Channels.Count -1
    then c := 0;
  put_ChannelSelected(c);
  Result := S_OK;
end;

function TDCDVBSource.put_EPGClearAll: HRESULT;
begin
  Result := S_OK;
  FEPG.Clear;
end;

function TDCDVBSource.get_EPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): HRESULT;
begin
  if FEPG.GetEPG(ChannelIndex, PByte(EPG), Size)
    then Result := S_OK
    else Result := S_FALSE;
end;

function TDCDVBSource.get_EPGTimeOffset(out TimeOffset: Integer): HRESULT;
begin
  Result := S_OK;
  TimeOffset := FSettings.EPGTimeOffset;
end;

function TDCDVBSource.get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT;
begin
  if not FGraphBuilder.GetTeletextPage(Page, SubPage) then
  begin
    Page := FTeletextPage;
    SubPage := FTeletextSubPage;
  end;
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextShow(Show: LongBool): HRESULT;
begin
  FTeletextShow := Show;
  FGraphBuilder.ShowTeletext(Show);
  if FForm <> nil
    then FForm.SpeedButton23.Down := Show;

  Result := S_OK;
end;

function TDCDVBSource.get_TeletextShow(out Show: LongBool): HRESULT;
begin
  Show := FTeletextShow;
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextTransparent(Transparent: LongBool): HRESULT;
begin
  FTeletextTransparent := Transparent;
  FGraphBuilder.TransparentTeletext(FTeletextTransparent);
  if FForm <> nil
    then FForm.SpeedButton25.Down := Transparent;
  Result := S_OK;
end;

function TDCDVBSource.get_TeletextTransparent(out Transparent: LongBool): HRESULT;
begin
  Transparent := FTeletextTransparent;
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT;
begin
  if Page < 100
    then FTeletextPage := 100
  else if Page > 899
    then FTeletextPage := 899
  else
    FTeletextPage := Page;

  if SubPage < 0
    then FTeletextSubPage := 0
  else if SubPage > 50
    then FTeletextSubPage := 50
  else
    FTeletextSubPage := SubPage;

  FGraphBuilder.ShowTeletextPage(FTeletextPage, FTeletextSubPage);
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextNumber(Number: Integer): HRESULT;
begin
  FGraphBuilder.SetTeletextNumber(Number);
  Result := S_OK;
end;

function TDCDVBSource.put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT;
begin
  FGraphBuilder.SaveTeletext(AType, AFilename);;
  Result := S_OK;
end;

function TDCDVBSource.get_AudioStreamCount(out CountStreams: Integer): HRESULT;
begin
  CountStreams := 0;

  if (FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if not Assigned(@CountStreams) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  CountStreams := FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount;
  Result := S_OK;
end;

function TDCDVBSource.get_AudioStreamInfo(Index: Integer; out Name: PChar): HRESULT;
var
  s: String;
begin
  Result := E_INVALIDARG;
  Name := '';

  if ((FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count)) or
     (Index >= FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount)
      then Exit;

  if not Assigned(@Name) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  s := 'Audio Stream ' + inttostr(Index + 1) + ' [' + FSettings.Channels[FSettings.CurrentChannel].GetAudioStreamName(Index) + ']';
  Name := CoTaskMemAlloc(Length(s) +1);
  Move(s[1], Name^, Length(s) +1);

  Result := S_OK;
end;

function TDCDVBSource.put_AudioStreamSelected(Index: Integer): HRESULT;
begin
  Enable(Index + FSettings.Channels.Count, AMSTREAMSELECTENABLE_ENABLE);
  Result := S_OK;
end;

function TDCDVBSource.get_AudioStreamSelected(out Index: Integer): HRESULT;
begin
  Index := FCurrentAudioStream;
  Result := S_OK;
end;

function TDCDVBSource.get_MHPRoot(out MHP: PChar): HRESULT;
begin
  if FSettings.MHPParser.GetMHPApps(MHP)
    then Result := S_OK
    else Result := S_FALSE;
end;

function TDCDVBSource.get_RecordingsCount(out Count: Integer): HRESULT;
begin
  Result := FSettings.Recordings.get_RecordingsCount(Count);
end;

function TDCDVBSource.get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT;
begin
  Result := FSettings.Recordings.get_Recording(Index, Recording);
end;

function TDCDVBSource.put_Recording(Recording: PDVBRecordingSetting): HRESULT;
begin
  Result := FSettings.Recordings.put_Recording(Recording);
end;

function TDCDVBSource.put_EditRecording(Recording: PDVBRecordingSetting): HRESULT;
begin
  Result := FSettings.Recordings.put_EditRecording(Recording);
end;

function TDCDVBSource.put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT;
begin
  Result := FSettings.Recordings.put_DeleteRecording(Recording);
end;

function TDCDVBSource.get_SubtitleStreamCount(out CountStreams: Integer): HRESULT;
begin
  CountStreams := 0;

  if (FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if not Assigned(@CountStreams) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  CountStreams := FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount;
  Result := S_OK;
end;

function TDCDVBSource.get_SubtitleStreamInfo(Index: Integer; out Name: PChar): HRESULT;
var
  s: String;
begin
  Result := E_INVALIDARG;
  Name := '';

  if ((FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count)) or
     (Index >= FSettings.Channels[FSettings.CurrentChannel].SubtitleStreamCount)
      then Exit;

  if not Assigned(@Name) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  s := 'Subtitle Stream ' + inttostr(Index + 1) + ' [' + FSettings.Channels[FSettings.CurrentChannel].GetSubtitleStreamName(Index) + ']';
  Name := CoTaskMemAlloc(Length(s) +1);
  Move(s[1], Name^, Length(s) +1);

  Result := S_OK;
end;

function TDCDVBSource.get_SubtitleStreamSelected(out Index: Integer): HRESULT;
begin
  Index := FCurrentSubtitleStream;
  Result := S_OK;
end;

function TDCDVBSource.put_SubtitleStreamSelected(Index: Integer): HRESULT;
begin
  Result := E_FAIL;
  if ((FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count))
    then Exit;
  Enable(Index + FSettings.Channels.Count + FSettings.Channels[FSettings.CurrentChannel].AudioStreamCount, AMSTREAMSELECTENABLE_ENABLE);
  Result := S_OK;
end;

function TDCDVBSource.ShowOSD: HRESULT;
begin
  FGraphBuilder.ShowOSD;
  Result := S_OK;
end;

function TDCDVBSource.get_StreamInfo(out AStreamInfo: TStreamInfo): HRESULT;
begin
  if not Assigned(@AStreamInfo) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  AStreamInfo := FGraphBuilder.GetStreamInfo;
  Result := S_OK
end;

procedure TDCDVBSource.OnTrayIconClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  bar_data: TAppBarData;
begin
  if (FForm = nil) or (Button <> mbLeft)
    then Exit;

  if not FForm.Visible then
  begin
    if FInitialSet then
    begin
      bar_data.cbSize := SizeOf(TAppBarData);
      SHAppBarMessage(ABM_GETTASKBARPOS, bar_data);
      SetWindowPos(FForm.Handle, 0, bar_data.rc.Right - FForm.Width, bar_data.rc.Top - FForm.Height, 0, 0, SWP_NOSIZE);
      FInitialSet := False;
    end;
    FForm.Show;
  end else
  begin
    FForm.Hide;
    FForm.Show;
  end;

  SetForegroundWindow(FForm.Handle);
  SetFocus(FForm.Handle);
end;

procedure TDCDVBSource.OnTrayIconDblClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Assigned(FFormEPG) then
  begin
    FFormEPG.Show;
    SetForegroundWindow(FFormEPG.Handle);
    SetFocus(FFormEPG.Handle);
  end;
end;

procedure TDCDVBSource.SetupQuickForm;
var
  i: Integer;
begin
  if FFormEPG <> nil then
  begin
    FFormEPG.EPG.Clear;
    for i := 0 to FSettings.Channels.Count -1 do
    begin
      FFormEPG.EPG.AddChannel(RemoveUnwantedChars(FSettings.Channels[i].Name));
    end;
    FFormEPG.Update2Click(nil);
  end;

  if FForm = nil
    then Exit;

  FForm.CanChangeChannels := False;
  FForm.ComboBox1.Clear;
  for i := 0 to FSettings.Channels.Count -1 do
  begin
    FForm.ComboBox1.Items.Add(inttostr(i+1) + ') ' + RemoveUnwantedChars(FSettings.Channels[i].Name));
  end;
  FForm.ComboBox2.Items.Add('Normal Size');
  FForm.ComboBox2.Items.Add('Double Upper');
  FForm.ComboBox2.Items.Add('Double Lower');
  FForm.ComboBox2.ItemIndex := 0;

  FForm.CanChangeChannels := True;
end;

procedure TDCDVBSource.ChangeQuickFormChannel;
begin
  if FForm = nil
    then Exit;

  FForm.CanChangeChannels := False;
  FForm.ComboBox1.ItemIndex := FSettings.CurrentChannel;
  FForm.CanChangeChannels := True;
end;

procedure TDCDVBSource.WriteTSStream(AWrite: Boolean);
begin
  FGraphBuilder.WriteTSStream(AWrite);
end;

function TDCDVBSource.GetReceivedBytes: Int64;
begin
  Result := FGraphBuilder.GetReceivedBytes;
end;

function TDCDVBSource.put_TeletextSizeMode(ASizeMode: TTeletextSizeMode): HRESULT;
begin
  FGraphBuilder.SetTeletextSizeMode(ASizeMode);
  FTeletextSizeMode := ASizeMode;
  Result := S_OK;
end;

function TDCDVBSource.get_TeletextSizeMode(out ASizeMode: TTeletextSizeMode): HRESULT;
begin
  Result := S_OK;
  ASizeMode := FTeletextSizeMode;
end;

function TDCDVBSource.get_PluginsCount(out ACount: Integer): HRESULT;
begin
  if Assigned(FSettings.Plugins)
    then ACount := FSettings.Plugins.Count
    else ACount := 0;
  Result := S_OK;
end;

function TDCDVBSource.put_PluginEnabled(AIndex: Integer; AEnabled: Boolean): HRESULT;
begin
  FSettings.Plugins[AIndex].Enabled := AEnabled;
  Result := S_OK;
end;

function TDCDVBSource.get_PluginEnabled(AIndex: Integer; out AEnabled: Boolean): HRESULT;
begin
  AEnabled := FSettings.Plugins[AIndex].Enabled;
  Result := S_OK;
end;

function TDCDVBSource.get_Plugin(AIndex: Integer; out APlugin: IUnknown): HRESULT;
begin
  APlugin := FSettings.Plugins[AIndex].Intf;
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextFastext(AFastext: TTeletextFastext): HRESULT;
begin
  FGraphBuilder.SetTeletextFastext(AFastext);
  Result := S_OK;
end;

function TDCDVBSource.put_CaptureVideoWindowCursor(ACapture: Boolean): HRESULT;
begin
  FGraphBuilder.CaptureVideoWindowCursor := ACapture;
  Result := S_OK;
end;

function TDCDVBSource.put_OSDChannel(AChannel: String): HRESULT;
begin
  FGraphBuilder.put_OSDChannel(AChannel);
  Result := S_OK;
end;

function TDCDVBSource.put_VideoPID(APID: Integer; AType: TVideoType): HRESULT;
begin
  FGraphBuilder.SetVideoPID(APID, AType);
  Result := S_OK;
end;

function TDCDVBSource.get_VideoPID(out APID: Integer; out AType: TVideoType): HRESULT;
begin
  FGraphBuilder.GetVideoPID(APID, AType);
  Result := S_OK;
end;

function TDCDVBSource.put_AudioPID(APID: Integer; AType: TAudioType): HRESULT;
begin
  FGraphBuilder.SetAudioPID(APID, AType);
  Result := S_OK;
end;

function TDCDVBSource.get_AudioPID(out APID: Integer; out AType: TAudioType): HRESULT;
begin
  FGraphBuilder.GetAudioPID(APID, AType);
  Result := S_OK;
end;

function TDCDVBSource.put_TeletextPID(APID: Integer): HRESULT;
begin
  FGraphBuilder.TeletextPID := APID;
  Result := S_OK;
end;

function TDCDVBSource.get_TeletextPID(out APID: Integer): HRESULT;
begin
  APID := FGraphBuilder.TeletextPID;
  Result := S_OK;
end;

function TDCDVBSource.put_SubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer): HRESULT;
begin
  FGraphBuilder.SetSubtitlePID(APID, APCRPID, ACPID, AAPID);
  Result := S_OK;
end;

function TDCDVBSource.get_SubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer): HRESULT;
begin
  FGraphBuilder.GetSubtitlePID(APID, APCRPID, ACPID, AAPID);
  Result := S_OK;
end;

procedure TDCDVBSource.OnPopup(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
  str: String;
  cnt: Cardinal;
  pmt: PAMMediaType;
  flags, lcid, group: Cardinal;
  name: PWideChar;
  u1, u2: IUnknown;
  last_group: Integer;
begin
  FMenu.Items.Clear;
  FMenu.AutoHotkeys := maManual;

  Count(cnt);
  last_group := -1;

  for i := 0 to Int64(cnt) -1 do
  begin
    pmt := nil;
    name := nil;
    Info(i, pmt, flags, lcid, group, name, u1, u2);
    if Assigned(pmt) then
    begin
      DeleteMediaType(pmt);
      pmt := nil;
    end;
    str := '';
    if Assigned(name) then
    begin
      str := name;
      CoTaskMemFree(name);
      name := nil;
    end;

    if (last_group <> Int64(group)) and (last_group <> -1) then
    begin
      FMenu.Items.Add(NewLine);
    end;
    last_group := group;

    item := TMenuItem.Create(FMenu);
    item.Tag := i;
    item.Checked := BOOL(flags and AMSTREAMSELECTINFO_ENABLED);
    item.AutoHotkeys := maManual;
    item.Caption := StringReplace(str, '&', '&&', [rfReplaceAll]);
    item.OnClick := OnClick;
    FMenu.Items.Add(item);
  end;
end;

procedure TDCDVBSource.OnClick(Sender: TObject);
begin
  Enable((Sender as TMenuItem).Tag, AMSTREAMSELECTENABLE_ENABLE);
end;

function TDCDVBSource.get_AuthorName(var pbstrAuthorName: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_Title(var pbstrTitle: TBSTR): HResult;
var
  str, s1, s2, s3: String;
begin
  if @pbstrTitle = nil then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if FSettings.CurrentChannel = -1 then
  begin
    Result := VFW_E_NOT_FOUND;
    Exit;
  end;

  if not FEPG.GetCurrentProgram(FSettings.CurrentChannel, str, s1, s2, s3) then
  begin
    Result := VFW_E_NOT_FOUND;
    Exit;
  end;

  pbstrTitle := SysAllocString(PWideChar(WideString(str)));

  Result := S_OK;
end;

function TDCDVBSource.get_Rating(var pbstrRating: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_Description(var pbstrDescription: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_Copyright(var pbstrCopyright: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_BaseURL(var pbstrBaseURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_LogoURL(var pbstrLogoURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_LogoIconURL(var pbstrLogoURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_WatermarkURL(var pbstrWatermarkURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_MoreInfoURL(var pbstrMoreInfoURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_MoreInfoBannerImage(var pbstrMoreInfoBannerImage: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_MoreInfoBannerURL(var pbstrMoreInfoBannerURL: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.get_MoreInfoText(var pbstrMoreInfoText: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBSource.put_CycleTeletext: HRESULT;
begin
  FGraphBuilder.CycleTeletext;
  Result := S_OK;
end;

function TDCDVBSource.put_CycleChannels: HRESULT;
begin
  if FSettings.CurrentChannel = -1 then
  begin
    if (FDecodingDisabled) then
    begin
      Enable(FLastLastChannel, AMSTREAMSELECTENABLE_ENABLE);
      Result := S_OK;
      Exit;
    end;

    Result := S_FALSE;
    Exit;
  end;

  if FLastLastChannel = -1 then
  begin
    if (FDecodingDisabled) then
    begin
      FCurrentAudioStream := -1;
      FCurrentSubtitleStream := -1;
      FLastLastChannel := FSettings.CurrentChannel;
      FSettings.CurrentChannel := -1;
      FGraphBuilder.StopChannel;
    end;

    Result := S_FALSE;
    Exit;
  end;

  Enable(FLastLastChannel, AMSTREAMSELECTENABLE_ENABLE);

  Result := S_OK;
end;

(*** Register/Unregister Filter ***********************************************)

{$IFDEF REGISTER_ICON}
const
  REGEX = 'DCDVBExtension';
{$ENDIF}

function DllRegisterServer: HResult; stdcall;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;

    // Setup DirectShow to use our Filesource for .dvb Extensions
    if OpenKey('Media Type\Extensions\.dvb',True) then
    begin
      WriteString('Source Filter',GUIDToString(CLSID_DCDVBSource));
      CloseKey;
    end;

  {$IFDEF REGISTER_ICON}
    // Setup the Icon for .dvb Files
    if OpenKey('.dvb', True) then
    begin
      WriteString('', REGEX);
      CloseKey;
    end;

    if OpenKey(REGEX, True) then
    begin
      WriteString('', 'Configuration File for DC-DVB Filter');
      CloseKey;
    end;

    if OpenKey(REGEX + '\DefaultIcon', True) then
    begin
      WriteString('', GetModuleName(HInstance) + ',0');
      CloseKey;
    end;
  {$ENDIF}

    Free;
  end;
  Result := BaseClass.DllRegisterServer;
end;

function DllUnregisterServer: HResult; stdcall;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    if KeyExists('Media Type\Extensions\.dvb')
      then DeleteKey('Media Type\Extensions\.dvb');
  {$IFDEF REGISTER_ICON}
    if KeyExists('.dvb')
      then DeleteKey('.dvb');
    if KeyExists(REGEX)
      then DeleteKey(REGEX);
  {$ENDIF}
    Free;
  end;
  Result := BaseClass.DllUnregisterServer;
end;

initialization

  TBCClassFactory.CreateFilter(TDCDVBSource, 'DC-DVB Source', CLSID_DCDVBSource,
    CLSID_LegacyAmFilterCategory, MERIT_UNLIKELY, 0, nil);

end.

