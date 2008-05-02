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

unit DVBGraphBuilder;

interface

{$I Compiler.inc}

uses
  Windows, Classes, MPEGSections, DirectShow9, ActiveX, DVBSettings, BDAUtils, DSUtil,
  DVBChannelList, MPEGConst, SysUtils, MMSystem, DVBEPG, DVBInterface,
  DVBAudioFilter, BDAConst, DVBTeletextFilter, MPEGUtils, DVBSourceFilter,
  Messages, DVBOSDFilter, IDVBSource, TransportStream, DCDVBTuningPlugins,
  MulticastTransportStream, TCPTransportStream, DVBConst, Math, DVBSubtitlingParser,
  Logger, DVBSubtitleFilter, BaseClass, DVBNetworkFilter, DCDVBDataPlugins,
  TCPClientServer, MulticastClientServer, FileTransportStream2, DVBVideoAnalyzer,
  SyncObjs, DCDVBTuningPluginTransportStream, DCDVBShared, MPEGDemultiplexer,
  NetworkParser, StrUtils;

const
  DEMUX_PIN_COUNT = 2;

type
  TTuningThread = class;

  TDVBGraphBuilder = class
  private
    FDecoderMode: TVideoType;
    FNetworkParser: TNetworkParser;
    FPMTList: TList;
    FStatistics: TStreamInfo;
    FDeliverLock: TCriticalSection;
    FWriteLock: TCriticalSection;
    FReceivedLock: TCriticalSection;
    FReceivedBytes: Int64;
    FTCPServer: TTCPServerSocket;
    FMulticastServer: TMulticastServer;
    FDump: TFileStream;
    FFirstWrite: Boolean;
    FCallback: TBufferCallback;
    FTransportStream: TTransportStream;
    FSettings: TDVBSettings;
    FEPG: TDVBEPG;
    FTuningThread: TTuningThread;
    FLastFrequency: Int64;
    FVMRWndMethod: Pointer;
    FVMRWndMethodInst: Pointer;
    FVMRHandle: THandle;
    FVMR9: IBaseFilter;
    FClearing: Boolean;

    FSourceGraph: IFilterGraph2;
    FRendererGraph: IFilterGraph2;
    FTeletext: ITeletextFilter;
    FMPEG2Demux: IBaseFilter;

    FVideoAnalysis: IBaseFilter;
    FStreamBufferSink: IBaseFilter;

    FStreamBufferSource: IBaseFilter;

    FMPEG2VideoDecoder: IBaseFilter;
    FH264VideoDecoder: IBaseFilter;
    FMPEG2AudioDecoder: IBaseFilter;
    FVideoPostProcessor: IBaseFilter;

    FOSDFilter: IBaseFilter;

    FAudioFilter: IBaseFilter;
    FVideoFilter: IBaseFilter;
    FDemuxPins: array[0..DEMUX_PIN_COUNT-1] of IMPEG2PIDMap;

    FStreamSource: IBaseFilter;

    FTeletextPID: Integer;
    FVideoPID: Integer;
    FAudioPID: Integer;
    FAudioType: TAudioType;
    FSubtitlePID: Integer;
    FSubtitlePCRPID: Integer;
    FSubtitleCPID: Integer;
    FSubtitleAPID: Integer;
    FVMRWidth: Integer;
    FVMRHeight: Integer;
    FTeletextVisible: Boolean;
    FLastCursor: THandle;
    FCaptureMouse: Boolean;
    FChannelChanged: Boolean;

    function RenderTuning: Boolean;
    function RenderStreamBufferSink: Boolean;
    function RenderStreamBufferSource: Boolean;
    function RenderVideoPin: Boolean;
    function RenderAudioPin: Boolean;
    procedure TuneToChannelInternal(AIndex: Integer);
    function MapMPEG2DemuxPins: Boolean;
    procedure ChangeReferenceClock;
    procedure OnTSData(ABuffer: PByte; ASize: Integer);
    procedure OnPMT(APMT: TProgramMapSection);
    procedure OnPMTBuffer(ABuffer: PByte; ASize: Integer);
    procedure VideoInfoCallback(AMode: TVideoType; AWidth: Integer; AHeight: Integer; AAspectRatio: TAspectRatio; AFrameRate: TFrameRate; ABitRate: Int64);
    procedure VideoBytesCallback(ASize: Integer);
    procedure DSMCCBytesCallback(ASize: Integer);
    procedure AudioBytesCallback(ASize: Integer);
    procedure AudioInfoCallback(AChannel: Integer; ABitrate: Integer; ASamplerate: Integer);
    procedure SetTeletextPID(APID: Integer);
    procedure WndMethod(var Message: TMessage);
    procedure VMRWndMethod(var Message: TMessage);
  public
    constructor Create(ASettings: TDVBSettings; AEPG: TDVBEPG);
    destructor Destroy; override;

    procedure Clear;
    procedure SwitchDecoder(AMode: TVideoType);
    procedure TuneToChannel(AIndex: Integer);
    procedure SetAudioStream(AChannelIndex: Integer; AStreamIndex: Integer);
    procedure SetSubtitleStream(AChannelIndex: Integer; AStreamIndex: Integer);
    procedure DisableSubtitleStream;
    procedure ShowTeletext(Show: LongBool);
    procedure SetTeletextSizeMode(ASizeMode: TTeletextSizeMode);
    procedure TransparentTeletext(Transparent: LongBool);
    procedure ShowTeletextPage(Page: Integer; SubPage: Integer);
    function GetTeletextPage(out APage: Integer; out ASubPage: Integer): Boolean;
    procedure SetTeletextNumber(ANumber: Integer);

    function GetSignalStatistic(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): Boolean;
    procedure StreamBufferSeekToEnd;
    function Render(AGraph: IFilterGraph2): Boolean;

    procedure SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar);
    procedure WriteTSStream(AWrite: Boolean);
    function GetReceivedBytes: Integer;
    procedure ShowOSD;
    function GetStreamInfo: TStreamInfo;
    procedure StopChannel;

    property TeletextPID: Integer read FTeletextPID write SetTeletextPID;
    procedure SetVideoPID(APID: Integer; AType: TVideoType);
    procedure GetVideoPID(out APID: Integer; out AType: TVideoType);
    procedure SetAudioPID(APID: Integer; AType: TAudioType);
    procedure GetAudioPID(out APID: Integer; out AType: TAudioType);
    procedure SetSubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer);
    procedure GetSubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer);
    procedure SetTeletextFastext(AFastext: TTeletextFastext);
    procedure put_OSDChannel(AChannel: String);
    property CaptureVideoWindowCursor: Boolean read FCaptureMouse write FCaptureMouse;
    procedure Activate;
    procedure CycleTeletext;
  end;

  TTuningThread = class(TThread)
  private
    FOwner: TDVBGraphBuilder;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TDVBGraphBuilder);
  end;

implementation

var
  g_CursorPoint: THandle;
  g_CursorHand: THandle;

(*** TTuningThread ************************************************************)

const
  UM_TUNE = WM_USER + 1000;
  UM_DIE = WM_USER + 1001;

constructor TTuningThread.Create(AOwner: TDVBGraphBuilder);
begin
  inherited Create(True);
  FOwner := AOwner;
  Log(FOwner.FSettings.Logger, Self, 'Create', 'constructor');
  Resume;
end;

procedure TTuningThread.Execute;
var
  msg: TMsg;
begin
  Log(FOwner.FSettings.Logger, Self, 'Execute', 'resumed');
  CoInitialize(nil);
  Priority := tpHighest;

  while (GetMessage(msg, 0, 0, 0)) do
  begin
    if Terminated
      then Exit;
    case msg.message of
      UM_TUNE:
      begin
        Log(FOwner.FSettings.Logger, Self, 'Execute', 'UM_TUNE');
        try
          FOwner.TuneToChannelInternal(msg.wParam);
        except on e: Exception
          do
          begin
//            OutputDebugString(PChar(e.Message));
            Log(FOwner.FSettings.Logger, Self, 'Execute', e.Message);
          end;
        end;
      end;
      UM_DIE:
      begin
        break;
      end;
    end;
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
  Log(FOwner.FSettings.Logger, Self, 'Execute', 'Terminated');

  CoUninitialize;
end;

(*** TDVBGraphBuilder *********************************************************)

constructor TDVBGraphBuilder.Create(ASettings: TDVBSettings; AEPG: TDVBEPG);
begin
  inherited Create;
  FDecoderMode := dmMPEG2;

  FNetworkParser := TNetworkParser.Create;
  FPMTList := TList.Create;
  FWriteLock := TCriticalSection.Create;
  FReceivedLock := TCriticalSection.Create;
  FDeliverLock := TCriticalSection.Create;
  FSettings := ASettings;
  Log(FSettings.Logger, Self, 'Create', 'constructor');
  FTuningThread := TTuningThread.Create(Self);
  FEPG := AEPG;
  FDump := nil;
  FFirstWrite := True;
  FCaptureMouse := True;
  Clear;
end;

destructor TDVBGraphBuilder.Destroy;
begin
  Log(FSettings.Logger, Self, 'Destroy', 'destructor');
  Clear;
  Log(FSettings.Logger, Self, 'Destroy', 'Terminating Thread');
  FTuningThread.Terminate;
  PostThreadMessage(FTuningThread.ThreadID, UM_DIE, 0, 0);
  Log(FSettings.Logger, Self, 'Destroy', 'waiting for Thread to End');
  FTuningThread.WaitFor;
  Log(FSettings.Logger, Self, 'Destroy', 'Thread terminated');
  FTuningThread.Free;
  FDeliverLock.Free;
  FWriteLock.Free;
  FReceivedLock.Free;
  FPMTList.Free;
  FNetworkParser.Free;
  inherited Destroy;
end;

procedure TDVBGraphBuilder.Clear;
var
  i: Integer;
begin
  FClearing := True;
  Log(FSettings.Logger, Self, 'Clear', 'clearing Graph');

  if FSettings.TimeShiftingEnabled and Assigned(FSourceGraph)
    then (FSourceGraph as IMediaControl).Stop;

  if FVMRHandle <> 0 then
  begin
    SetWindowLong(FVMRHandle, GWL_WNDPROC, Integer(FVMRWndMethod));
  end;

  if Assigned(FTransportStream) then
  begin
    Log(FSettings.Logger, Self, 'Clear', 'clearing TransportStream');
    FTransportStream.Active := False;
    FTransportStream.OnTSData := nil;
    if (FTransportStream is TDCDVBTuningPluginTransportStream) then
    begin
      with (FTransportStream as TDCDVBTuningPluginTransportStream) do
      begin
        FSettings.TunerSettings := FSettings.Devices.GetDeviceSettings2(Device);
        Device := nil;
      end;
    end;
    FTransportStream.Free;
    FTransportStream := nil;
  end;

  for i := 0 to DEMUX_PIN_COUNT -1
    do FDemuxPins[i] := nil;

  FDeliverLock.Enter;
  try
    FNetworkParser.SetTeletextCallback(nil);
    FNetworkParser.SetTeletextPID(-1);
    FNetworkParser.SetPCRCallBack(nil);
    FNetworkParser.SetPCRPID(-1);
    FNetworkParser.SetSubtitleCallBack(nil);
    FNetworkParser.SetSubtitlePID(-1);
    FNetworkParser.SetEPG(nil);
    FNetworkParser.SetPMTCallback(nil);
    FNetworkParser.SetDSMCCCallback(nil);
    FNetworkParser.SetDSMCC(nil);
  finally
    FDeliverLock.Leave;
  end;

  FStreamSource := nil;
  FH264VideoDecoder := nil;
  FMPEG2VideoDecoder := nil;
  FMPEG2AudioDecoder := nil;
  FVideoPostProcessor := nil;
  FStreamBufferSource := nil;

  FVideoAnalysis := nil;
  FStreamBufferSink := nil;
  FMPEG2Demux := nil;
  FAudioFilter := nil;
  FTeletext := nil;
  FVMR9 := nil;
  
  if Assigned(FVideoFilter) then
  begin
    with (FVideoFilter as IVideoFilter) do
    begin
      put_InfoCallback(nil);
      put_BytesCallback(nil);
      put_Enabled(False);
      put_Callback(nil);
    end;
    FVideoFilter := nil;
  end;
  FOSDFilter := nil;

  if FSettings.TimeShiftingEnabled
    then FSourceGraph := nil
    else Pointer(FSourceGraph) := nil;
  Pointer(FRendererGraph) := nil;

  if Assigned(FTCPServer) then
  begin
    Log(FSettings.Logger, Self, 'Clear', 'clearing TCPServer');
    FTCPServer.Close;
    FTCPServer.Free;
    FTCPServer := nil;
  end;
  if Assigned(FMulticastServer) then
  begin
    Log(FSettings.Logger, Self, 'Clear', 'clearing MulticastServer');
    FMulticastServer.Active := False;
    FMulticastServer.Free;
    FMulticastServer := nil;
  end;

  if Assigned(FDump) then
  begin
    Log(FSettings.Logger, Self, 'Clear', 'clearing Dump');
    FDump.Free;
    FDump := nil;
  end;
  FCallback := nil;
  FLastFrequency := -1;
  FReceivedBytes := 0;

  if FVMRWndMethodInst <> nil then
  begin
    FreeObjectInstance(FVMRWndMethodInst);
    FVMRWndMethodInst := nil;
  end;

  FVMRWndMethod := nil;
  FVMRHandle := 0;

  FTeletextPID := -1;
  FVideoPID := -1;
  FAudioPID := -1;
  FAudioType := atMPEG1Audio;
  FSubtitlePID := -1;
  FSubtitlePCRPID := -1;
  FSubtitleCPID := -1;
  FSubtitleAPID := -1;
  FTeletextVisible := False;
  FLastCursor := g_CursorPoint;

  FClearing := False;
  FDecoderMode := dmMPEG2;
  // This will free the Filters manually loaded from DLL's
  // FIXME: CoFreeUnusedLibraries crashes the Filter when Destroying
  // seems like COM automagically puts this somewhere on its own !?
  // Are those Libraries now free'd or not ???
//  CoFreeUnusedLibraries;
end;

function TDVBGraphBuilder.Render(AGraph: IFilterGraph2): Boolean;
begin
  Log(FSettings.Logger, Self, 'Render', 'request to Render the Graph');
  Result := False;
  Clear;

  Log(FSettings.Logger, Self, 'Render', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Pointer(FRendererGraph) := Pointer(AGraph);

  if FSettings.TimeShiftingEnabled then
  begin
    Log(FSettings.Logger, Self, 'Render', 'SBE Enabled -> Creating Source Graph');
    if (CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph, FSourceGraph) <> S_OK) or
       (not Assigned(FSourceGraph)) then
    begin
      Log(FSettings.Logger, Self, 'Render', 'Failed creating SourceGraph');
      Clear;
      Exit;
    end;
  end else
  begin
    // No reference Counting on the Graph !
    Pointer(FSourceGraph) := Pointer(FRendererGraph);
  end;

  Log(FSettings.Logger, Self, 'Render', 'Rendering Tuning');
  if not RenderTuning then
  begin
    Log(FSettings.Logger, Self, 'Render', 'Rendering Tuning failed');
    Clear;
    Exit;
  end;

  if FSettings.TimeShiftingEnabled then
  begin
    Log(FSettings.Logger, Self, 'Render', 'Rendering StreamBufferSink and StreamBufferSource');
    if not RenderStreamBufferSink or not RenderStreamBufferSource then
    begin
      Log(FSettings.Logger, Self, 'Render', 'Rendering StreamBufferSink and StreamBufferSource failed');
      Clear;
      Exit;
    end;
  end;

  Log(FSettings.Logger, Self, 'Render', 'Rendering VideoPin and AudioPin');
  if not RenderVideoPin or not RenderAudioPin then
  begin
    Log(FSettings.Logger, Self, 'Render', 'Rendering VideoPin and AudioPin failed');
    Clear;
    Exit;
  end;

  FSettings.GraphFailed := False;

  Log(FSettings.Logger, Self, 'Render', 'Loading EPG');
  FEPG.Load;

  FDeliverLock.Enter;
  try
    Log(FSettings.Logger, Self, 'Render', 'Setting up DSM-CC Parser');
    FNetworkParser.SetDSMCC(FSettings.MHPParser);
    FNetworkParser.SetDSMCCCallback(DSMCCBytesCallback);
    Log(FSettings.Logger, Self, 'Render', 'Setting up EPG Callback');
    FNetworkParser.SetEPG(FEPG);
    Log(FSettings.Logger, Self, 'Render', 'Setting up PMT Callback');
    FNetworkParser.SetPMTCallback(OnPMT);
    FNetworkParser.SetPMTBufferCallback(OnPMTBuffer);
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      Log(FSettings.Logger, Self, 'Render', 'Setting up Teletext Callback');
      FNetworkParser.SetTeletextCallback(FOSDFilter as ITeletextCallback);
      Log(FSettings.Logger, Self, 'Render', 'Setting up PCR Callback');
      FNetworkParser.SetPCRCallBack(FOSDFilter as IPCRCallback);
      Log(FSettings.Logger, Self, 'Render', 'Setting up Subtitle Callback');
      FNetworkParser.SetSubtitleCallBack(FOSDFilter as ISubtitleCallback);
      (FOSDFilter as IOSDFilter).put_OSDColor(DVBInterface.TOSDColor(FSettings.OSDColor));
    end;
    Log(FSettings.Logger, Self, 'Render', 'Setting up Enabled Functions');
    FNetworkParser.SetEnableDSMCC(not FSettings.DisableDSMCC);
  finally
    FDeliverLock.Leave;
  end;

  if FSettings.TSServerEnabled then
  begin
    Log(FSettings.Logger, Self, 'Render', 'Starting TS Server at ' + FSettings.TSServerGroup + 'on Port: ' + inttostr(FSettings.TSServerPort) + ' with Type: ' + inttostr(FSettings.TSServerType));
    case FSettings.TSServerType of
      0:
      begin
        FMulticastServer := TMulticastServer.Create;
        FMulticastServer.Port := FSettings.TSServerPort;
        FMulticastServer.MulticastIP := FSettings.TSServerGroup;
        FMulticastServer.Active := True;
      end;
      1:
      begin
        FTCPServer := TTCPServerSocket.Create;
        FTCPServer.Port := FSettings.TSServerPort;
        FTCPServer.Listen;
      end;
    end;
  end;

  if FSettings.ChangeRefClock
    then ChangeReferenceClock;

  FSettings.Plugins.SetFilterGraph(FSourceGraph);

  if (FSettings.CurrentChannel <> -1) and FSettings.TimeShiftingEnabled and FSettings.SaveLastChannel
    then TuneToChannelInternal(FSettings.CurrentChannel);

  if FSettings.TimeShiftingEnabled and Assigned(FSourceGraph) then
  begin
    Log(FSettings.Logger, Self, 'Render', 'starting Source Graph');
    (FSourceGraph as IMediaControl).Run;
    Log(FSettings.Logger, Self, 'Render', 'Source Graph started');
  end;

//  (FRendererGraph as IAMGraphStreams).SyncUsingStreamOffset(true);

  Result := True;
end;

const
  CLSID_CoreAVCH264Decoder: TGuid = '{09571A4B-F1FE-4C60-9760-DE6D310C7C31}';

  MEDIASUBTYPE_H264: TGuid = '{8D2D71CB-243F-45E3-B2D8-5FD7967EC09B}';

  VIDEO_PIN_PBFORMAT: array[0..53] of Cardinal = (
    $00000000, $00000000, $000002C0, $000001E0, $00000000, $00000000, $00000000, $00000000,
    $00B71B00, $00000000, $00028BB1, $00000000, $00000000, $00000000, $00000010, $00000009,
    $00000000, $00000000, $00000028, $000002C0, $000001E0, $00000000, $00000000, $00000000,
    $000007D0, $0000D842, $00000000, $00000000, $00C827C0, $0000004C, $FFFFFFFF, $FFFFFFFF,
    $00000000, $B3010000, $37E0012C, $81234C1D, $12111110, $13131212, $14141313, $15141414,
    $15151515, $16161615, $16161616, $17171717, $17171717, $19181818, $19181818, $1A1A1A1A,
    $1B1B1B19, $1C1C1B1B, $1E1E1C1C, $211F1F1E, $00000000, $00000000
  );

function TDVBGraphBuilder.MapMPEG2DemuxPins: Boolean;
var
  mt: array[0..DEMUX_PIN_COUNT -1] of TAMMediaType;
  pin: IPin;
  i: Integer;
  pin_name: String;
  vih: PVideoInfoHeader2;
  hr: HRESULT;
begin
  Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'deleting all Pins from the MPEG-2 Demultiplexer');
  with (FMPEG2Demux as IMpeg2Demultiplexer) do
  begin
    DeleteOutputPin('1');
    DeleteOutputPin('2');
    DeleteOutputPin('3');
    DeleteOutputPin('4');
    DeleteOutputPin('5');
    DeleteOutputPin('6');
    DeleteOutputPin('7');
    DeleteOutputPin('8');
    DeleteOutputPin('9');
  end;

  Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'creating Pin 1 (Video Stream)');
  if FSettings.PreferMPEG2Video then
  begin
    FDecoderMode := dmMPEG2;

    Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'Video Stream is prefered to MPEG-2');
    mt[0].majortype := KSDATAFORMAT_TYPE_VIDEO;
    mt[0].subtype := KSDATAFORMAT_SUBTYPE_MPEG2_VIDEO;
    mt[0].formattype := KSDATAFORMAT_SPECIFIER_MPEG2_VIDEO;
    mt[0].bFixedSizeSamples := True;
    mt[0].bTemporalCompression := False;
    mt[0].lSampleSize := 65536;
    mt[0].pUnk := nil;
    mt[0].cbFormat := 216;
    mt[0].pbFormat := CoTaskMemAlloc(216);
    CopyMemory(mt[0].pbFormat, @VIDEO_PIN_PBFORMAT, 216);
  end else
  begin
    FDecoderMode := dmH264;

    Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'Video Stream is prefered to H264');
    mt[0].majortype := MEDIATYPE_Video;
    if IsEqualGUID(CLSID_CoreAVCH264Decoder, FSettings.H264DecoderCLSID) then
    begin
      Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'CoreAVC Decoder used. Setup "special" H264 Mediatype');
      mt[0].subtype := FOURCCMap(FCC('h264'));
    end else
    begin
      Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'CoreAVC Decoder not used. Setup "default" H264 Mediatype');
      mt[0].subtype := MEDIASUBTYPE_H264;
    end;

    mt[0].formattype := FORMAT_VideoInfo2;
    mt[0].bFixedSizeSamples := False;
    mt[0].bTemporalCompression := True;
    mt[0].lSampleSize := 1;
    mt[0].pUnk := nil;
    mt[0].cbFormat := SizeOf(TVideoInfoHeader2);
    mt[0].pbFormat := CoTaskMemAlloc(mt[0].cbFormat);

    vih := mt[0].pbFormat;

    FillChar(PByte(mt[0].pbFormat)^, SizeOf(TVideoInfoHeader2), 0);

    vih.bmiHeader.biSize := 28;
    vih.bmiHeader.biWidth := 1920;
    vih.bmiHeader.biHeight := 1080;
    vih.dwPictAspectRatioX := 0;
    vih.dwPictAspectRatioY := 0;
    vih.bmiHeader.biPlanes := 0;
    vih.bmiHeader.biBitCount := 24;
    vih.bmiHeader.biCompression := FCC('h264');
  end;

  FStatistics.VideoType := FDecoderMode;

  Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'creating Pin 2 (Audio Stream)');
  mt[1].majortype := KSDATAFORMAT_TYPE_AUDIO;
  mt[1].subtype := KSDATAFORMAT_SUBTYPE_MPEG2_AUDIO;
  mt[1].formattype := KSDATAFORMAT_SPECIFIER_WAVEFORMATEX;
  mt[1].bFixedSizeSamples := True;
  mt[1].bTemporalCompression := False;
  mt[1].lSampleSize := 65536;
  mt[1].pUnk := nil;
  mt[1].cbFormat := 40;
  mt[1].pbFormat := CoTaskMemAlloc(40);
  with PWaveFormatExtensible(mt[1].pbFormat)^ do
  begin
    Format.wFormatTag := $0050;
    Format.nChannels := 2;
    Format.nSamplesPerSec := 48000;
    Format.nAvgBytesPerSec := 32000;
    Format.nBlockAlign := 768;
    Format.wBitsPerSample := 0;
    Format.cbSize := 22;
    Samples.wSamplesPerBlock := 2;
    dwChannelMask := 256000;
    SubFormat := StringToGUID('{00010001-0001-001C-0000-000000000000}'); // ???
  end;

  Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'mapping Pins on the MPEG-2 Demultiplexer');
  with (FMPEG2Demux as IMpeg2Demultiplexer) do
  begin
    for i := 0 to DEMUX_PIN_COUNT -1 do
    begin
      pin_name := 'Unknown';
      case i of
        0: pin_name := 'Video';
        1: pin_name := 'Audio';
      end;
      if not FSettings.OwnDemux
        then pin_name := '(' + inttostr(i+1) + ') ' + pin_name;

      Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'creating Pin (' + pin_name + ') ' + inttostr(i+1) + ' on MPEG-2 Demultiplexer');
      hr := CreateOutputPin(mt[i], StringToOleStr(pin_name), pin);
      Log(FSettings.Logger, Self, 'MapMPEG2DemuxPins', 'creating Pin (' + pin_name + ') ' + inttostr(i+1) + ' on MPEG-2 Demultiplexer returned 0x' + inttohex(hr, 8));
      pin := nil;
      FreeMediaType(@mt[i]);
    end;
  end;

  Result := True;
end;

function TDVBGraphBuilder.RenderTuning: Boolean;
var
  i, c: Integer;
  pin_list: TPinList;
  device: IDCDVBTuningPluginDevice;
  found: Boolean;
begin
  Log(FSettings.Logger, Self, 'RenderTuning', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Result := False;

  Log(FSettings.Logger, Self, 'RenderTuning', 'DC-DVB Stream Source');
  FStreamSource := TDVBStreamSource.Create(FSettings.Logger);
  Log(FSettings.Logger, Self, 'RenderTuning', 'getting TS Callback from DC-DVB Stream Source');
  (FStreamSource as IDVBSourceFilter).get_TSCallback(FCallback);
  Log(FSettings.Logger, Self, 'RenderTuning', 'adding DC-DVB Stream Source to Graph');
  FSourceGraph.AddFilter(FStreamSource, 'DC-DVB Stream Source');

  if FSettings.Streaming then
  begin
    if FSettings.TSFileName <> '' then
    begin
      Log(FSettings.Logger, Self, 'RenderTuning', 'creating File TransportStream');
      FTransportStream := TFileTransportStream2.Create(FSettings.Logger);
      with TFileTransportStream2(FTransportStream) do
      begin
        Log(FSettings.Logger, Self, 'RenderTuning', 'Loading File TransportStream');
        FileName := FSettings.TSFileName;
      end;
    end else
    begin
      case FSettings.ServerType of
        0:
        begin
          Log(FSettings.Logger, Self, 'RenderTuning', 'creating Multicast TransportStream');
          FTransportStream := TMulticastTransportStream.Create(FSettings.Logger);
          with TMulticastTransportStream(FTransportStream) do
          begin
            MulticastGroup := FSettings.ServerAddress;
            Port := FSettings.ServerPort;
          end;
        end;
        1:
        begin
          Log(FSettings.Logger, Self, 'RenderTuning', 'creating TCP TransportStream');
          FTransportStream := TTCPTransportStream.Create(FSettings.Logger);
          with TTCPTransportStream(FTransportStream) do
          begin
            IP := FSettings.ServerAddress;
            Port := FSettings.ServerPort;
          end;
        end;
        else
        begin
          Log(FSettings.Logger, Self, 'RenderTuning', 'Unknown Streaming Source (not TCP or Multicast)');
          Exit;
        end;
      end;
    end;
  end else
  begin
    found := False;
    device := nil;
    Log(FSettings.Logger, Self, 'RenderTuning', 'checking BDA Devices');

    for i := 0 to FSettings.Devices.Count -1 do
    begin
      device := FSettings.Devices[i];
      if (FSettings.Devices.DeviceType[i] = FSettings.DeviceType) and
         (FSettings.Devices.DeviceName[i] = FSettings.TunerName) then
      begin
        if FSettings.IgnoreDeviceID or (FSettings.Devices.DeviceID[i] = FSettings.TunerID) then
        begin
          FSettings.Devices.DeviceSettings[i] := FSettings.TunerSettings;
          Log(FSettings.Logger, Self, 'RenderTuning', 'found BDA Device');
          found := True;
          break;
        end;
      end;
    end;

    if not found or not Assigned(device)
      then Exit;

    (FStreamSource as IDVBSourceFilter).put_TunerDevice(device);
    Log(FSettings.Logger, Self, 'RenderTuning', 'creating BDA Transport Stream');
    FTransportStream := TDCDVBTuningPluginTransportStream.Create(FSettings.Logger);
    TDCDVBTuningPluginTransportStream(FTransportStream).Device := device;
  end;

  if not Assigned(FTransportStream) then
  begin
    Log(FSettings.Logger, Self, 'RenderTuning', 'no Transport Stream present');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderTuning', 'setting up Transport Stream Callback');
  FTransportStream.OnTSData := OnTSData;

  // Add MPEG-2 Demux
  Log(FSettings.Logger, Self, 'RenderTuning', 'creating MPEG-2 Demultiplexer');
  FMPEG2Demux := nil;
  if FSettings.OwnDemux
    then FMPEG2Demux := TTSDemux.Create
    else FMPEG2Demux := GetFilterFromSystem(CLSID_MPEG2Demultiplexer);

  if (not Assigned(FMPEG2Demux)) then
  begin
    if not FSettings.OwnDemux then
    begin
      Log(FSettings.Logger, Self, 'RenderTuning', 'failed creating MPEG-2 Demultiplexer from COM. Trying to load it manually.');
      FMPEG2Demux := GetFilterFromLibrary('mpg2splt.ax', CLSID_MPEG2Demultiplexer);
      if (not Assigned(FMPEG2Demux)) then
      begin
        Log(FSettings.Logger, Self, 'RenderTuning', 'failed creating MPEG-2 Demultiplexer');
        Exit;
      end;
    end else
    begin
      Log(FSettings.Logger, Self, 'RenderTuning', 'failed creating internal MPEG-2 Demultiplexer');
      Exit;
    end;
  end;
  Log(FSettings.Logger, Self, 'RenderTuning', 'adding MPEG-2 Demultiplexer to the Graph');
  if FSettings.OwnDemux
    then FSourceGraph.AddFilter(FMPEG2Demux, 'DC-TS Demux')
    else FSourceGraph.AddFilter(FMPEG2Demux, 'MPEG-2 Demultiplexer');

  // Connect DC-DVB Stream Source to MPEG-2 Demux
  Log(FSettings.Logger, Self, 'RenderTuning', 'connecting DC-DVB Stream Source to MPEG-2 Demultiplexer');
  if (FSourceGraph.ConnectDirect(GetOutPin(FStreamSource, 0), GetInPin(FMPEG2Demux, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderTuning', 'failed connecting DC-DVB Stream Source to MPEG-2 Demultiplexer');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderTuning', 'mapping MPEG-2 Demultiplexer Pins');
  if not MapMPEG2DemuxPins then
  begin
    Log(FSettings.Logger, Self, 'RenderTuning', 'failed mapping MPEG-2 Demultiplexer Pins');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderTuning', 'getting IMPEG2PIDMap from each Output Pin on the MPEG-2 Demultiplexer');
  pin_list := TPinList.Create(FMPEG2Demux);
  c := 0;
  for i := 0 to pin_list.Count -1 do
  begin
    if pin_list.PinInfo[i].dir = PINDIR_OUTPUT then
    begin
      FDemuxPins[c] := pin_list.Items[i] as IMPEG2PIDMap;
      inc(c);
    end;
  end;
  pin_list.Free;

  Result := True;
end;

function TDVBGraphBuilder.RenderStreamBufferSink: Boolean;
var
  key: HKEY;
begin
  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Result := False;

  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'creating MPEG-2 Video Analysis Filter');
  FVideoAnalysis := GetFilterFromSystem(CLSID_Mpeg2VideoStreamAnalyzer);
  if (not Assigned(FVideoAnalysis)) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed creating MPEG-2 Video Analysis Filter from COM. Trying to load it manually.');
    FVideoAnalysis := GetFilterFromLibrary('sbe.dll', CLSID_Mpeg2VideoStreamAnalyzer);
    if (not Assigned(FVideoAnalysis)) then
    begin
      Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed creating MPEG-2 Video Analysis Filter');
      Exit;
    end;
  end;
  FSourceGraph.AddFilter(FVideoAnalysis, 'MPEG-2 Video Analyzer');

  // Connect MPEG-2 Demux to MPEG-2 Video Analyzer
  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'connecting MPEG-2 Video Analysis Filter to MPEG-2 Demultiplexer');
  if (FSourceGraph.ConnectDirect(GetOutPin(FMPEG2Demux, 0), GetInPin(FVideoAnalysis, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed connecting MPEG-2 Video Analysis Filter to MPEG-2 Demultiplexer');
    Exit;
  end;

  // add Stream Buffer Source
  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'creating Stream Buffer Sink');
  FStreamBufferSink := GetFilterFromSystem(CLSID_StreamBufferSink);
  if (not Assigned(FStreamBufferSink)) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed creating Stream Buffer Sink Filter from COM. Trying to load it manually.');
    FStreamBufferSink := GetFilterFromLibrary('sbe.dll', CLSID_StreamBufferSink);
    if (not Assigned(FStreamBufferSink)) then
    begin
      Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed creating Stream Buffer Sink');
      Exit;
    end;
  end;
  FSourceGraph.AddFilter(FStreamBufferSink, 'Stream Buffer Source');

  // connect Video Analyzer to Stream Buffer Source
  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'connecting Stream Buffer Sink to MPEG-2 Video Analysis Filter');
  if (FSourceGraph.ConnectDirect(GetOutPin(FVideoAnalysis, 0), GetInPin(FStreamBufferSink, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed connecting Stream Buffer Sink to MPEG-2 Video Analysis Filter');
    Exit;
  end;

  // connect Audio Stream Pin (MPEG-2 Demux Pin 3) to Stream Buffer Source
  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'connecting Audio Stream Pin (MPEG-2 Demux Pin 2) to Stream Buffer Source');
  if (FSourceGraph.ConnectDirect(GetOutPin(FMPEG2Demux, 1), GetInPin(FStreamBufferSink, 1), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed connecting Audio Stream Pin (MPEG-2 Demux Pin 2) to Stream Buffer Source');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'setting up SBE Registry Key (first part)');
  if (RegCreateKey(HKEY_CURRENT_USER, 'SOFTWARE\DSP-worx\DVBSource\StreamBuffer', key) <> ERROR_SUCCESS) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'failed setting up SBE Registry Key (first part)');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderStreamBufferSink', 'setting up SBE Registry Key (second part)');
  (FStreamBufferSink as IStreamBufferInitialize).SetHKEY(key);
  (FStreamBufferSink as IStreamBufferSink).LockProfile(nil);

  Result := True;
end;

function TDVBGraphBuilder.RenderStreamBufferSource: Boolean;
begin
  Log(FSettings.Logger, Self, 'RenderStreamBufferSource', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Result := False;

  // Add Stream Buffer Source
  Log(FSettings.Logger, Self, 'RenderStreamBufferSource', 'creating Stream Buffer Source');
  FStreamBufferSource := GetFilterFromSystem(CLSID_StreamBufferSource);
  if (not Assigned(FStreamBufferSource)) then
  begin
    Log(FSettings.Logger, Self, 'RenderStreamBufferSource', 'failed creating Stream Buffer Source Filter from COM. Trying to load it manually.');
    FStreamBufferSource := GetFilterFromLibrary('sbe.dll', CLSID_StreamBufferSource);
    if (not Assigned(FStreamBufferSource)) then
    begin
      Log(FSettings.Logger, Self, 'RenderStreamBufferSource', 'failed creating Stream Buffer Source');
      Exit;
    end;
  end;
  FRendererGraph.AddFilter(FStreamBufferSource, 'Stream Buffer Source');

  // set Stream Buffer Sink
  Log(FSettings.Logger, Self, 'RenderStreamBufferSource', 'setting up Stream Buffer Sink on Stream Buffer Source');
  (FStreamBufferSource as IStreamBufferSource).SetStreamSink(FStreamBufferSink as IStreamBufferSink);

  Result := True;
end;

function TDVBGraphBuilder.RenderVideoPin: Boolean;
var
  pin: IPin;
  rect: VMR9NormalizedRect;
  num_streams: Cardinal;
//  mixing_prefs: Cardinal;
  vw: IVideoWindow;
  owner_wnd: OAHWND;
  parent: Thandle;
  r: TRect;
  filter: IBaseFilter;
  name: WideString;
  hr: HRESULT;
  ovm: IBaseFilter;
begin
  Log(FSettings.Logger, Self, 'RenderVideoPin', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Result := False;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'searching for compressed Video Output Pin');
  if FSettings.TimeShiftingEnabled
    then pin := FindPinWithMajorType(FStreamBufferSource, MEDIATYPE_Video, MEDIASUBTYPE_MPEG2_VIDEO)
    else pin := GetOutPin(FMPEG2Demux, 0);

  if not Assigned(pin) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'Output Pin not found');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'creating DC-DVB Video Analyzer Filter');
  FVideoFilter := TDVBVideoFilter.Create;
  if (not Assigned(FVideoFilter)) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed creating DC-DVB Video Analyzer Filter');
    Exit;
  end;
  FRendererGraph.AddFilter(FVideoFilter, PWideChar(WideString('DC-DVB Video Analyzer')));

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'connecting MPEG-Video Stream to DC-DVB Video Analyzer');
  if (FRendererGraph.ConnectDirect(pin, GetInPin(FVideoFilter, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'connecting MPEG-Video Stream to DC-DVB Video Analyzer');
    pin := nil;
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'creating MPEG-2 Video Decoder Filter ' + FSettings.VideoDecoderName);
  FMPEG2VideoDecoder := GetFilterFromSystem(FSettings.VideoDecoderCLSID);
  if (not Assigned(FMPEG2VideoDecoder)) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed creating MPEG-2 Video Decoder Filter');
    if FSettings.PreferMPEG2Video then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'ERROR: MPEG-2 is prefered, but Video Decoder is NULL');
      Exit;
    end;
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'creating H264 Video Decoder Filter ' + FSettings.H264DecoderName);
  FH264VideoDecoder := GetFilterFromSystem(FSettings.H264DecoderCLSID);
  if (not Assigned(FH264VideoDecoder)) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed creating H264 Video Decoder Filter');
    if not FSettings.PreferMPEG2Video then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'ERROR: H264 is prefered, but Video Decoder is NULL');
      Exit;
    end;
  end;

  filter := nil;
  if FSettings.PreferMPEG2Video then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'MPEG-2 is prefered as Video Decoder');
    filter := FMPEG2VideoDecoder;
    name := FSettings.VideoDecoderName;
  end else
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'H264 is prefered as Video Decoder');
    filter := FH264VideoDecoder;
    name := FSettings.H264DecoderName;
  end;

  if filter = nil then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'ERROR: Video Decoder is NULL');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'Adding Video Decoder to the Graph');
  hr := FRendererGraph.AddFilter(filter, PWideChar(name));
  Log(FSettings.Logger, Self, 'RenderVideoPin', 'Adding Video Decoder to the Graph returned 0x' + inttohex(hr, 8));

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'connecting Video Stream to Video Decoder');
  hr := FRendererGraph.ConnectDirect(GetOutPin(FVideoFilter, 0), GetInPin(filter, 0), nil);
  if (hr <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed connecting Video Stream to Video Decoder 0x' + inttohex(hr, 8));
    pin := nil;
    Exit;
  end;

  // create OSD Filter
  if not FSettings.DisableOSD then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'creating OSD Filter');
    FOSDFilter := TDVBOSDFilter.Create;
    if not Assigned(FOSDFilter) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed creating OSD Filter');
      Exit;
    end;
    FTeletext := (FOSDFilter as ITeletextFilter);
    FRendererGraph.AddFilter(FOSDFilter, 'DC-DVB OSD Filter');
    FTeletext.put_TeletextAspectRatio(FSettings.TeletextAspectRatio);
    FTeletext.put_TeletextFPS(FSettings.TeletextFPS);
  end;

  if Assigned(FVideoFilter) then
  begin
    with (FVideoFilter as IVideoFilter) do
    begin
      put_Callback(FOSDFilter as ISubtitleCallback);
      put_InfoCallback(VideoInfoCallback);
      put_BytesCallback(VideoBytesCallback);
    end;
  end;

  pin := GetOutPin(filter, 0);
  if not IsEqualGUID(FSettings.PostProcessorCLSID, GUID_NULL) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'adding Post Processor');

    // add postprocessor
    FVideoPostProcessor := GetFilterFromSystem(FSettings.PostProcessorCLSID);
    if (not Assigned(FVideoPostProcessor)) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed adding Post Processor');
      Exit;
    end;

    FRendererGraph.AddFilter(FVideoPostProcessor, PWideChar(FSettings.PostProcessorName));
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'connecting MPEG-2 Video Decoder to Post Processor');
    if (FRendererGraph.ConnectDirect(GetOutPin(filter, 0), GetInPin(FVideoPostProcessor, 0), nil) <> S_OK) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed connecting MPEG-2 Video Decoder to Post Processor');
      Exit;
    end;

    pin := GetOutPin(FVideoPostProcessor, 0);
  end;

  if not assigned(pin) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', '"pin" not Assigned');
    Exit;
  end;

  // Add VMR9
  Log(FSettings.Logger, Self, 'RenderVideoPin', 'searching VMR9 Renderer');
  FVMR9 := FindFilterWithCLSID(FRendererGraph, CLSID_VideoMixingRenderer9);
  if not Assigned(FVMR9) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'VMR9 Renderer not found, creating one');
    FVMR9 := GetFilterFromSystem(CLSID_VideoMixingRenderer9);
    if not assigned(FVMR9) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed creating VMR9 Renderer');
      pin := nil;
      Exit;
    end;

    if not FSettings.Overlay then
    begin
      FRendererGraph.AddFilter(FVMR9, 'Video Mixing Renderer 9');
    end;
  end else
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'VMR9 Renderer not found');
  end;

  num_streams := 1;
  if not FSettings.DisableOSD then
  begin
    inc(num_streams, 3);
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'Setting up ' + inttostr(num_streams) + ' Streams on the VMR9');
  (FVMR9 as IVMRFilterConfig9).SetNumberOfStreams(num_streams);
  with (FVMR9 as IVMRMixerControl9) do
  begin
//    GetMixingPrefs(mixing_prefs);
//    mixing_prefs := mixing_prefs and not MixerPref_RenderTargetMask;
//    mixing_prefs := mixing_prefs or MixerPref_RenderTargetYUV;
//    SetMixingPrefs(mixing_prefs);
  end;

  if FSettings.Overlay then
  begin
    ovm := GetFilterFromSystem(CLSID_OverlayMixer);
    FRendererGraph.AddFilter(ovm, StringToOleStr('Overlay Mixer'));
  end;

  // render Video Output Pin
  Log(FSettings.Logger, Self, 'RenderVideoPin', 'rendering the MPEG-2 Video Decoder');
  if (FRendererGraph.Render(pin) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed rendering the MPEG-2 Video Decoder');
    pin := nil;
    Exit;
  end;
  pin := nil;

  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'rendering the Teletext Filter');

    // render Teletext Output Pin
    if (FRendererGraph.Render(GetOutPin(FOSDFilter, 0)) <> S_OK) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed rendering the Teletext Filter');
      Exit;
    end;

    // render Subtitle Output Pin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'rendering the Subtitle Filter');
    if (FRendererGraph.Render(GetOutPin(FOSDFilter, 1)) <> S_OK) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed rendering the Subtitle Filter');
      Exit;
    end;

    // render OSD Output Pin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'rendering the OSD Filter');
    if (FRendererGraph.Render(GetOutPin(FOSDFilter, 2)) <> S_OK) then
    begin
      Log(FSettings.Logger, Self, 'RenderVideoPin', 'failed rendering the OSD Filter');
      Exit;
    end;
  end;

  Log(FSettings.Logger, Self, 'RenderVideoPin', 'setting up IVMRMixerControl9');
  with (FVMR9 as IVMRMixerControl9) do
  begin
    rect.left := 0 - (FSettings.VideoOffset / 100);
    rect.top := 0 - (FSettings.VideoOffset / 100);
    rect.right := 1.00 + (FSettings.VideoOffset / 100);
    rect.bottom := 1.00 + (FSettings.VideoOffset / 100);
    SetOutputRect(0, @rect);
    if not FSettings.DisableOSD then
    begin
      rect.left := FSettings.TeletextOffset / 100;
      rect.top := FSettings.TeletextOffset / 100;
      rect.right := 1.00 - (FSettings.TeletextOffset / 100);
      rect.bottom := 1.00 - (FSettings.TeletextOffset / 100);
      SetOutputRect(1, @rect);
      SetAlpha(1, 0);
      rect.left := 0 - (FSettings.SubtitleOffset / 100);
      rect.top := 0 - (FSettings.SubtitleOffset / 100);
      rect.right := 1.00 + (FSettings.SubtitleOffset / 100);
      rect.bottom := 1.00 + (FSettings.SubtitleOffset / 100);
      SetOutputRect(2, @rect);
      SetAlpha(2, (100 - FSettings.SubtitleAlpha) / 100);
      rect.left := 0;
      rect.top := 0.0 + (FSettings.OSDOffset / 100);
      rect.right := 1.00;
      rect.bottom := 1.0 + (FSettings.OSDOffset / 100);
      (FOSDFilter as IOSDFilter).put_OSDRect(rect);
      SetOutputRect(3, @rect);
      SetAlpha(3, 0.0);
    end;
  end;

  if not FSettings.DisableOSD and Assigned(FOSDFilter) then
  begin
    Log(FSettings.Logger, Self, 'RenderVideoPin', 'setting up Teletext Transparency');
    FTeletext.put_TeletextTransparency(FSettings.TeletextAlpha);
  end;

  if FVMR9.QueryInterface(IID_IVideoWindow, vw) = S_OK then
  begin
    if FVMRWndMethodInst <> nil then
    begin
      FreeObjectInstance(FVMRWndMethodInst);
      FVMRWndMethodInst := nil;
    end;

    vw.get_Owner(owner_wnd);
    parent := Classes.AllocateHWnd(WndMethod);
    vw.put_Owner(parent);
    FVMRHandle := Windows.GetWindow(parent, GW_CHILD);
    vw.put_Owner(owner_wnd);
    Classes.DeallocateHWnd(parent);

    FVMRWndMethod := Pointer(GetWindowLong(FVMRHandle, GWL_WNDPROC));
    FVMRWndMethodInst := MakeObjectInstance(VMRWndMethod);
    SetWindowLong(FVMRHandle, GWL_WNDPROC, Integer(FVMRWndMethodInst));
    GetWindowRect(FVMRHandle, r);
    FVMRWidth := r.Right - r.Left;
    FVMRHeight := r.Bottom - r.Top;
    FLastCursor := g_CursorPoint;
  end;

  Result := True;
end;

procedure TDVBGraphBuilder.WndMethod(var Message: TMessage);
begin
  inherited;
end;

function TDVBGraphBuilder.RenderAudioPin: Boolean;
var
  pin: IPin;
begin
  Log(FSettings.Logger, Self, 'RenderAudioPin', 'trying to disable FPU Exceptions');
  Set8087CW($133F);

  Result := False;

  // Audio MediaType Changer Filter
  Log(FSettings.Logger, Self, 'RenderAudioPin', 'creating DC-DVB Audio Analyzer');
  FAudioFilter := TDVBAudioFilter.Create;
  FRendererGraph.AddFilter(FAudioFilter, 'DC-DVB Audio Analyzer');
  with (FAudioFilter as IAudioFilter) do
  begin
    put_AudioInfoCallback(AudioInfoCallback);
    put_SBEEnabled(FSettings.TimeShiftingEnabled);
    put_AudioBytesCallback(AudioBytesCallback);
  end;

  Log(FSettings.Logger, Self, 'RenderAudioPin', 'searching for Output Pin');
  if FSettings.TimeShiftingEnabled
    then pin := FindPinWithMajorType(FStreamBufferSource, MEDIATYPE_Audio, MEDIASUBTYPE_MPEG2_AUDIO)
    else pin := GetOutPin(FMPEG2Demux, 1);

  if not Assigned(pin) then
  begin
    Log(FSettings.Logger, Self, 'RenderAudioPin', 'Output Pin not found');
    Exit;
  end;

  Log(FSettings.Logger, Self, 'RenderAudioPin', 'connecting "pin" to DC-DVB Audio Analyzer');
  if (FRendererGraph.ConnectDirect(pin, GetInPin(FAudioFilter, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderAudioPin', 'failed connecting "pin" to DC-DVB Audio Analyzer');
    pin := nil;
    Exit;
  end;
  pin := nil;

  // add MPEG-2 Audio Decoder
  Log(FSettings.Logger, Self, 'RenderAudioPin', 'creating MPEG-2 Audio Decoder ' + FSettings.AudioDecoderName);
  FMPEG2AudioDecoder := GetFilterFromSystem(FSettings.AudioDecoderCLSID);
  if (not Assigned(FMPEG2AudioDecoder)) then
  begin
    Log(FSettings.Logger, Self, 'RenderAudioPin', 'creating MPEG-2 Audio Decoder');
    Exit;
  end;
  FRendererGraph.AddFilter(FMPEG2AudioDecoder, PWideChar(FSettings.AudioDecoderName));

  Log(FSettings.Logger, Self, 'RenderAudioPin', 'connecting DC-DVB Audio Analyzer to MPEG-2 Audio Decoder');
  if (FRendererGraph.ConnectDirect(GetOutPin(FAudioFilter, 0), GetInPin(FMPEG2AudioDecoder, 0), nil) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderAudioPin', 'error connecting DC-DVB Audio Analyzer to MPEG-2 Audio Decoder');
    Exit;
  end;

  // render Audio Output Pin
  Log(FSettings.Logger, Self, 'RenderAudioPin', 'rendering MPEG-2 Audio Decoder');
  if (FRendererGraph.Render(GetOutPin(FMPEG2AudioDecoder, 0)) <> S_OK) then
  begin
    Log(FSettings.Logger, Self, 'RenderAudioPin', 'failed rendering MPEG-2 Audio Decoder');
    Exit;
  end;
  Result := True;
end;

function TDVBGraphBuilder.GetSignalStatistic(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): Boolean;
begin
  Log(FSettings.Logger, Self, 'GetSignalStatistic', 'receiving Signal Statistics');

  Strength := 0;
  Quality := 0;
  SignalPresent := False;
  SignalLocked := False;

  Result := False;

  if Assigned(FTransportStream) and (FTransportStream is TDCDVBTuningPluginTransportStream) then
  begin
    Result := (FTransportStream as TDCDVBTuningPluginTransportStream).GetSignalStatistic(Strength, Quality, SignalPresent, SignalLocked);
  end;
end;

procedure TDVBGraphBuilder.StreamBufferSeekToEnd;
var
  pos: Int64;
  p1: Int64;
  p2: Int64;
begin
  Log(FSettings.Logger, Self, 'StreamBufferSeekToEnd', 'seeking to end');

  if not FSettings.TimeShiftingEnabled or not Assigned(FStreamBufferSource)
    then Exit;

  with (FStreamBufferSource as IStreamBufferMediaSeeking) do
  begin
    GetStopPosition(pos);
    GetAvailable(p1, p2);
    SetPositions(pos, AM_SEEKING_AbsolutePositioning, pos, AM_SEEKING_NoPositioning);
  end;
end;

procedure TDVBGraphBuilder.TuneToChannelInternal(AIndex: Integer);
var
  str: TSubtitlingDescriptorItem;
  pid: Integer;
  channel: TDVBChannel;
  d: Cardinal;
  i, c, k: Integer;
  cnt: Integer;
  mm: PByteArray;
  dvbt_tunerequest: TDVBTTuneRequest;
  dvbs_tunerequest: TDVBSTuneRequest;
  dvbc_tunerequest: TDVBCTuneRequest;
  atsc_tunerequest: TATSCTuneRequest;
  pr: TProgram;
  np: Integer;
  prog: TProgram;
//  last_state: TFilterState;
begin
  Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'new Channel Request');

  dvbt_tunerequest.Size := SizeOf(TDVBTTuneRequest);
  dvbs_tunerequest.Size := SizeOf(TDVBSTuneRequest);
  dvbc_tunerequest.Size := SizeOf(TDVBCTuneRequest);
  atsc_tunerequest.Size := SizeOf(TATSCTuneRequest);

  dvbt_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
  dvbs_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
  dvbc_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
  atsc_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC;

  FStatistics.VideoPresent := False;
  FStatistics.AudioPresent := False;

  np := 0;
//  (FSourceGraph as IMediaControl).Stop;

//  if Assigned(FSourceGraph) and not FSettings.OwnDemux then
//  begin
//    (FSourceGraph as IMediaControl).GetState(0, last_state);
//    case last_state of
//      State_Running,
//      State_Paused:
//      begin
//        (FSourceGraph as IMediaControl).Stop;
//      end;
//    end;
//  end;

  if (AIndex < 0) or (AIndex >= FSettings.Channels.Count) then
  begin
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Channel Index is wrong -> exiting');
    Exit;
  end;

  FDeliverLock.Enter;
  try
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Removing Mappings on Pin 0');
    UnmapPIDs(FDemuxPins[0], FSettings.Logger);
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Removing Mappings on Pin 1');
    UnmapPIDs(FDemuxPins[1], FSettings.Logger);

    FNetworkParser.SetTeletextPID(-1);
    FNetworkParser.SetPCRPID(-1);
    FNetworkParser.SetSubtitlePID(-1);
    FNetworkParser.RemovePSIMappings;

    if FSettings.OwnDemux
      then (FMPEG2Demux as IDCMpeg2Demultiplexer).SetPCRPID(-1);

    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Reseting Subtitle PID');
      (FOSDFilter as ISubtitleCallback).put_PID(-1, -1, -1, -1);
      FTeletextPID := -1;
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Reseting Teletext PID');
      FTeletext.put_TeletextPID(-1);
    end;

    if Assigned(FVideoFilter) then
    begin
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Disabling Video Filter');
      (FVideoFilter as IVideoFilter).put_Enabled(False);
    end;
//  finally
//    FDeliverLock.Leave;
//  end;

  channel := FSettings.Channels[AIndex];
  FVideoPID := -1;
  FAudioPID := -1;
  FSubtitlePID := -1;
  FSubtitlePCRPID := -1;
  FSubtitleCPID := -1;
  FSubtitleAPID := -1;

  if Assigned(FTransportStream) and (FTransportStream is TDCDVBTuningPluginTransportStream) then
  begin
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Settings up Device Transport Stream');
    case channel.Network.Type_ of
      ntATSC:
      begin
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Setting up ATSC Frequency');
        if FLastFrequency <> channel.Network.ATSC.CentreFrequency then
        begin
          with atsc_tunerequest do
          begin
            Frequency := channel.Network.ATSC.CentreFrequency;
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin');
          try
            TDCDVBTuningPluginTransportStream(FTransportStream).TuneIn(@atsc_tunerequest);
          except on E: Exception do
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', e.Message);
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin returned');
          FLastFrequency := channel.Network.ATSC.CentreFrequency;
        end else
        begin
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Last Frequency = Current Frequency');
        end;
      end;
      ntDVBC:
      begin
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Setting up DVBC Frequency');
        if FLastFrequency <> channel.Network.Cable.Frequency then
        begin
          with dvbc_tunerequest do
          begin
            Frequency := channel.Network.Cable.Frequency;
            SymbolRate := channel.Network.Cable.SymbolRate;
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin');
          try
            TDCDVBTuningPluginTransportStream(FTransportStream).TuneIn(@dvbc_tunerequest);
          except on E: Exception do
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', e.Message);
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin returned');
          FLastFrequency := channel.Network.Cable.Frequency;
        end else
        begin
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Last Frequency = Current Frequency');
        end;
      end;
      ntDVBS:
      begin
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Setting up DVBS Frequency');
        if FLastFrequency <> channel.Network.Satellite.Frequency then
        begin
          with dvbs_tunerequest do
          begin
            Frequency := channel.Network.Satellite.Frequency;
            SymbolRate := channel.Network.Satellite.SymbolRate;
            case channel.Network.Satellite.Polarization of
              spLinearHorizontal: Polarization := DVBS_POLARIZATION_HORIZONTAL;
              spLinearVertical  : Polarization := DVBS_POLARIZATION_VERTICAL;
            end;
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin');
          try
            TDCDVBTuningPluginTransportStream(FTransportStream).TuneIn(@dvbs_tunerequest);
          except on E: Exception do
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', e.Message);
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin returned');
          FLastFrequency := channel.Network.Satellite.Frequency;
        end else
        begin
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Last Frequency = Current Frequency');
        end;
      end;
      ntDVBT:
      begin
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Setting up DVBT Frequency');
        if FLastFrequency <> channel.Network.Terrestrial.CentreFrequency then
        begin
          with dvbt_tunerequest do
          begin
            Frequency := channel.Network.Terrestrial.CentreFrequency;
            Bandwidth := channel.Network.Terrestrial.Bandwidth;
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin');
          try
            TDCDVBTuningPluginTransportStream(FTransportStream).TuneIn(@dvbt_tunerequest);
          except on E: Exception do
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', e.Message);
          end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Passing Tune Request to Plugin returned');
          FLastFrequency := channel.Network.Terrestrial.CentreFrequency;
        end else
        begin
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Last Frequency = Current Frequency');
        end;
      end;
    end;
    if ((FTransportStream as TDCDVBTuningPluginTransportStream).Device <> nil) then
    begin
      pr.ProgramNumber := channel.SID;
      pr.PCRPID := channel.PCRPID;
      pr.PMTPID := channel.ProgramMapPID;
      pr.NumPIDs := 0;

      c := 0;
      k := channel.DefaultAudioStreamIndex;
      for i := 0 to channel.Streams.Count -1 do
      begin
        if (channel.Streams[i].StreamType = stAudio) then
        begin
          if c = k then
          begin
            pr.PIDs[np] := channel.Streams[i].PID;
            inc(pr.NumPIDs);
            inc(np);
            break;
          end;
        end;
      end;

      for i := 0 to channel.Streams.Count -1 do
      begin
        if (channel.Streams[i].StreamType = stVideo) then
        begin
          pr.PIDs[np] := channel.Streams[i].PID;
          inc(pr.NumPIDs);
//            inc(np);
          break;
        end;
      end;

      (FTransportStream as TDCDVBTuningPluginTransportStream).Device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED, Integer(@pr), 0);
    end;
  end;

//  FDeliverLock.Enter;
//  try
    if FTransportStream is TFileTransportStream2
      then (FTransportStream as TFileTransportStream2).PCRPID := channel.PCRPID;

    if FSettings.OwnDemux
      then (FMPEG2Demux as IDCMpeg2Demultiplexer).SetPCRPID(channel.PCRPID);

    // Map Subtitle PID
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'searching Subtitle PID');
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      if channel.GetSubtitleStream(channel.DefaultSubtitleStreamIndex, str, pid) then
      begin
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Subtitle PID found');
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Subtitle Parser');
        (FOSDFilter as ISubtitleCallback).put_PID(channel.PCRPID, pid, str.CompositionPageID, str.AncillaryPageID);
        FSubtitlePID := pid;
        FSubtitlePCRPID := channel.PCRPID;
        FSubtitleCPID := str.CompositionPageID;
        FSubtitleAPID := str.AncillaryPageID;
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Subtitle Parser returned');
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up PCR PID');
        FNetworkParser.SetPCRPID(channel.PCRPID);
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up PCR PID returned');
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Subtitle PID');
        FNetworkParser.SetSubtitlePID(pid);
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Subtitle PID returned');
      end;
    end;

    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Teletext');
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Teletext');
      FTeletext.put_TeletextClear;
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Teletext returned');

      // Map Teletext PID
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'searching Teletext PID');
      for i := 0 to channel.Streams.Count -1 do
      begin
        if channel.Streams[i].StreamType = stTeletext then
        begin
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Teletext PID found');
          FTeletextPID := channel.Streams[i].PID;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Teletext Parser');
          FTeletext.put_TeletextPID(FTeletextPID);
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Teletext Parser returned');
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Teletext PID');
          FNetworkParser.SetTeletextPID(FTeletextPID);
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Teletext PID returned');
          break;
        end;
      end;
    end;

    // Map Carousel PID and EIT, SDT and NRT
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Network Filter');
    FNetworkParser.Reset;

    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Network Filter returned');
    cnt := IfThen(FSettings.DisableEPG, 0, 5);
    if cnt > 0 then
    begin
      k := 0;
      mm := AllocMem(cnt * 4);
      if not FSettings.DisableEPG then
      begin
        PCardinal(@mm[k * 4])^ := PSI_PID_EIT;
        inc(k);
        PCardinal(@mm[k * 4])^ := PSI_PID_SDT;
        inc(k);
        PCardinal(@mm[k * 4])^ := PSI_PID_RNT;
        inc(k);
        PCardinal(@mm[k * 4])^ := PSI_PID_CAT;
        inc(k);
        PCardinal(@mm[k * 4])^ := channel.ProgramMapPID;
      end;
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings up PID''s for Network Filter');
      FNetworkParser.SetPSIMappings(PCardinal(mm), cnt);
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings up PID''s for Network Filter returned');
      FreeMem(mm);
    end else
    begin
      Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'nothing to setup on Network Filter');
    end;

    // Map Audio PID
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'searching Audio PID');
    c := 0;
    k := channel.DefaultAudioStreamIndex;
    for i := 0 to channel.Streams.Count -1 do
    begin
      if (channel.Streams[i].StreamType = stAudio) then
      begin
        if c = k then
        begin
          FStatistics.AudioPresent := True;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'searching Audio PID');
  //        FDeliverLock.Enter;
  //        try
            d := channel.Streams[i].PID;

            FAudioPID := d;
            FStatistics.AudioPID := d;
            case TDVBAudioStream(channel.Streams[i]).Coding of
              acMP1:      FStatistics.AudioType := atMPEG1Audio;
              acMP2:      FStatistics.AudioType := atMPEG2Audio;
              acAC3:      FStatistics.AudioType := atAC3;
            end;

            FAudioType := FStatistics.AudioType;

            if TDVBAudioStream(channel.Streams[i]).Coding = acAC3 then
            begin
              Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings Audio PID to AC3');
              (FAudioFilter as IAudioFilter).put_MediaType(mtAC3);
              Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings Audio PID to AC3 returned');
            end else
            begin
              Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings Audio PID to MPEG Audio returned');
              (FAudioFilter as IAudioFilter).put_MediaType(mtMP2);
              Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'settings Audio PID to MPEG Audio returned');
            end;
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Audio PID found -> mapping PID');

            FDemuxPins[1].MapPID(1, @d, MEDIA_ELEMENTARY_STREAM);
            (FAudioFilter as IAudioFilter).put_FlushStream(True);
  //        finally
  //          FDeliverLock.Leave;
  //        end;
          Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Audio PID found -> mapping PID returned');
          break;
        end;
        inc(c);
      end;
    end;

    // Map Video PID
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'searching Video PID');
    for i := 0 to channel.Streams.Count -1 do
    begin
      if channel.Streams[i].StreamType = stVideo then
      begin
        case TDVBVideoStream(channel.Streams[i]).Coding of
          vcMPEG: SwitchDecoder(dmMPEG2);
          vcH264: SwitchDecoder(dmH264);
        end;

        FStatistics.VideoPresent := True;
        FStatistics.VideoType := FDecoderMode;

        d := channel.Streams[i].PID;
        FStatistics.VideoPID := d;
        FVideoPID := d;
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Video PID found -> mapping PID');
          FDemuxPins[0].MapPID(1, @d, MEDIA_ELEMENTARY_STREAM);
          if Assigned(FVideoFilter) then
          begin
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Video Filter');
            with (FVideoFilter as IVideoFilter) do
            begin
              put_Enabled(True);
              put_FlushStream(True);
            end;
            Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'setting up Video Filter returned');
          end;
  //      finally
  //        FDeliverLock.Leave;
  //      end;
        Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Video PID found -> mapping PID returned');
        break;
      end;
    end;

    (FStreamSource as IDVBSourceFilter).put_Discontinuity;

    // Seek to End if StreamBuffer is used
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Timeshifting');
  //  FDeliverLock.Enter;
  //  try
      StreamBufferSeekToEnd;
  //  finally
  //    FDeliverLock.Leave;
  //  end;
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'reseting Timeshifting returned');
  finally
    FDeliverLock.Leave;
  end;


  if Assigned(FTransportStream) and (FTransportStream is TDCDVBTuningPluginTransportStream) and ((FTransportStream as TDCDVBTuningPluginTransportStream).Device <> nil) then
  begin
    prog.ProgramNumber := channel.SID;
//    (FTransportStream as TDCDVBTuningPluginTransportStream).Device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED, Integer(@prog), 0);
  end;

  // needed for CI
//  sleep(1000);

  FChannelChanged := True;
  
//  if Assigned(FSourceGraph) and not FSettings.OwnDemux then
//  begin
//    case last_state of
//      State_Running:(FSourceGraph as IMediaControl).Run;
//      State_Paused: (FSourceGraph as IMediaControl).Pause;
//    end;
//  end;

//  (FSourceGraph as IMediaControl).Run;

  ShowOSD;
end;

procedure TDVBGraphBuilder.TuneToChannel(AIndex: Integer);
begin
  Log(FSettings.Logger, Self, 'TuneToChannel', 'posting Channel Request to Thread');

  PostThreadMessage(FTuningThread.ThreadID, UM_TUNE, AIndex, 0);
//  TuneToChannelInternal(AIndex);
end;

procedure TDVBGraphBuilder.SetAudioStream(AChannelIndex: Integer; AStreamIndex: Integer);
var
  chan: TDVBChannel;
  str: TDVBBaseStream;
  i, x: Integer;
  d: Cardinal;
begin
  Log(FSettings.Logger, Self, 'SetAudioStream', 'setting Stream of Channel ' + inttostr(AChannelIndex) + ' to ' + inttostr(AStreamIndex));

  if (AChannelIndex < 0) or (AChannelIndex >= FSettings.Channels.Count)
    then Exit;

  chan := FSettings.Channels[AChannelIndex];
  x := 0;
  for i := 0 to chan.Streams.Count -1 do
  begin
    str := chan.Streams[i];
    if str.StreamType = stAudio then
    begin
      if AStreamIndex = x then
      begin
        FDeliverLock.Enter;
        try
          UnmapPIDs(FDemuxPins[1], FSettings.Logger);
          d := str.PID;

          FStatistics.AudioPID := d;
          case TDVBAudioStream(str).Coding of
            acMP1:      FStatistics.AudioType := atMPEG1Audio;
            acMP2:      FStatistics.AudioType := atMPEG2Audio;
            acAC3:      FStatistics.AudioType := atAC3;
          end;

          FStatistics.AudioPresent := True;
          if TDVBAudioStream(str).Coding = acAC3
            then (FAudioFilter as IAudioFilter).put_MediaType(mtAC3)
            else (FAudioFilter as IAudioFilter).put_MediaType(mtMP2);
          FDemuxPins[1].MapPID(1, @d, MEDIA_ELEMENTARY_STREAM);
        finally
          FDeliverLock.Leave;
        end;
        Exit;
      end else
      begin
        inc(x);
      end;
    end;
  end;
end;

procedure TDVBGraphBuilder.SetSubtitleStream(AChannelIndex: Integer; AStreamIndex: Integer);
var
  str: TSubtitlingDescriptorItem;
  pid: Integer;
begin
  Log(FSettings.Logger, Self, 'SetSubtitleStream', 'setting Stream of Channel ' + inttostr(AChannelIndex) + ' to ' + inttostr(AStreamIndex));
  DisableSubtitleStream;
  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    if FSettings.Channels[AChannelIndex].GetSubtitleStream(AStreamIndex, str, pid) then
    begin
      FDeliverLock.Enter;
      try
        (FOSDFilter as ISubtitleCallback).put_PID(FSettings.Channels[AChannelIndex].PCRPID, pid, str.CompositionPageID, str.AncillaryPageID);
        if Assigned(FVideoFilter)
          then (FVideoFilter as IVideoFilter).put_Enabled(True);
        FNetworkParser.SetPCRPID(FSettings.Channels[AChannelIndex].PCRPID);
        FNetworkParser.SetSubtitlePID(pid);
      finally
        FDeliverLock.Leave;
      end;
    end;
  end;
end;

procedure TDVBGraphBuilder.DisableSubtitleStream;
begin
  Log(FSettings.Logger, Self, 'DisableSubtitleStream', 'unmapping PIDs');
  FDeliverLock.Enter;
  try
    FNetworkParser.SetPCRPID(-1);
    FNetworkParser.SetSubtitlePID(-1);
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      (FOSDFilter as ISubtitleCallback).put_PID(-1, -1, -1, -1);
      if Assigned(FVideoFilter)
        then (FVideoFilter as IVideoFilter).put_Enabled(False);
    end;
  finally
    FDeliverLock.Leave;
  end;
end;

procedure TDVBGraphBuilder.ShowTeletext(Show: LongBool);
begin
  Log(FSettings.Logger, Self, 'ShowTeletext', 'init');
  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    FTeletext.put_TeletextShow(Show);
    FTeletextVisible := Show;
    if FVMRHandle <> 0
      then PostMessage(FVMRHandle, WM_USER + 123, 0, 0);
  end;
end;

procedure TDVBGraphBuilder.TransparentTeletext(Transparent: LongBool);
begin
  Log(FSettings.Logger, Self, 'TransparentTeletext', 'init');
  if Assigned(FOSDFilter) and not FSettings.DisableOSD
    then FTeletext.put_TeletextTransparent(Transparent);
end;

procedure TDVBGraphBuilder.ShowTeletextPage(Page: Integer; SubPage: Integer);
begin
  Log(FSettings.Logger, Self, 'ShowTeletextPage', 'init');
  if Assigned(FOSDFilter) and not FSettings.DisableOSD
    then FTeletext.put_TeletextPage(Page, SubPage);
end;

function TDVBGraphBuilder.GetTeletextPage(out APage: Integer; out ASubPage: Integer): Boolean;
begin
  Result := False;

  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    FTeletext.get_TeletextPage(APage, ASubPage);
    Result := True;
  end;
end;

procedure TDVBGraphBuilder.ChangeReferenceClock;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
  clock: IReferenceClock;
  class_id: TGuid;
begin
  Log(FSettings.Logger, Self, 'ChangeReferenceClock', 'init');

  if FRendererGraph.EnumFilters(enum) = S_OK then
  begin
    while (enum.Next(1, filter, nil) = S_OK) do
    begin
      if filter.QueryInterface(IID_IReferenceClock, clock) = S_OK then
      begin
        filter.GetClassID(class_id);
        if IsEqualGUID(CLSID_DSoundRender, class_id) then
        begin
          (FRendererGraph as IMediaFilter).SetSyncSource(clock);
          break;
        end;
      end;
    end;
    filter := nil;
    enum := nil;
  end;
end;

procedure TDVBGraphBuilder.SetTeletextNumber(ANumber: Integer);
begin
  if Assigned(FOSDFilter) and not FSettings.DisableOSD
    then FTeletext.put_TeletextNumber(ANumber);
end;

procedure TDVBGraphBuilder.SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar);
begin
  Log(FSettings.Logger, Self, 'SaveTeletext', 'init');
  if Assigned(FOSDFilter) and not FSettings.DisableOSD
    then FTeletext.put_SaveTeletext(AType, AFilename);
end;

procedure TDVBGraphBuilder.OnTSData(ABuffer: PByte; ASize: Integer);
var
  buffer: PByte;
  size: Integer;
  s: Integer;
begin
  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Received ' + inttostr(ASize)); {$ENDIF}

  if FClearing then
    Exit;

  FSettings.Plugins.ProcessBuffer(ABuffer, ASize);

  FReceivedLock.Enter;
  try
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Incrementing ReceivedBytes'); {$ENDIF}
    FReceivedBytes := FReceivedBytes + Int64(ASize);
    FStatistics.TotalTSBytes := FReceivedBytes;
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'ReceivedBytes: ' + inttostr(FReceivedBytes)); {$ENDIF}
  finally
    FReceivedLock.Leave;
  end;

  FDeliverLock.Enter;
  try
    if Assigned(FCallback) and not FClearing then
    begin
      {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Sending Buffer to Source Filter'); {$ENDIF}
      FCallback(ABuffer, ASize);
      {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Sending Buffer to Source Filter returned'); {$ENDIF}
    end else
    begin
      {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Source Filter Callback not present !!!'); {$ENDIF}
    end;
    FNetworkParser.ProcessBuffer(ABuffer, ASize);
  finally
    FDeliverLock.Leave;
  end;

  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'checking Recordings'); {$ENDIF}
  if Assigned(FSettings) and Assigned(FSettings.Recordings) then
  begin
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'found Recording Callback -> sending Buffer to Recordings'); {$ENDIF}
    FSettings.Recordings.ParseData(ABuffer, ASize);
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'sending Buffer to Recordings returned'); {$ENDIF}
  end;

  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'checking for TCP Server'); {$ENDIF}
  if Assigned(FTCPServer) and FTCPServer.IsListening then
  begin
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'TCP Server found -> sending Buffer'); {$ENDIF}
    if FSettings.PacketSize <= 0 then
    begin
      FTCPServer.SendBuffer(ABuffer, ASize);
    end else
    begin
      buffer := ABuffer;
      size := ASize;
      while (size > 0) do
      begin
        s := IfThen(FSettings.PacketSize < size, FSettings.PacketSize, size);
        FTCPServer.SendBuffer(buffer, s);
        dec(size, s);
        inc(buffer, s)
      end;
    end;
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'sending Buffer to TCP Server returned'); {$ENDIF}
  end;

  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'checking for Multicast Server'); {$ENDIF}
  if Assigned(FMulticastServer) and FMulticastServer.Active then
  begin
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'Multicast Server found -> sending Buffer'); {$ENDIF}
    if FSettings.PacketSize <= 0 then
    begin
      FMulticastServer.SendBuffer(ABuffer, ASize);
    end else
    begin
      buffer := ABuffer;
      size := ASize;
      while (size > 0) do
      begin
        s := IfThen(FSettings.PacketSize < size, FSettings.PacketSize, size);
        FMulticastServer.SendBuffer(buffer, s);
        dec(size, s);
        inc(buffer, s)
      end;
    end;
    {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'sending Buffer to Multicast Server returned'); {$ENDIF}
  end;

  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'checking for TS File Writer'); {$ENDIF}
  FWriteLock.Enter;
  try
    if Assigned(FDump) then
    begin
      {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'TS File Writer found -> writing Data'); {$ENDIF}
      if FFirstWrite then
      begin
        Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'TS File Writer -> Searching for Sync Bytes');
        // Start writing at the next Syncbyte
        while (ASize > TS_PACKET_SIZE) do
        begin
          if (ABuffer^ = TS_PACKET_SYNC_BYTE) and (PMyByteArray(ABuffer)[TS_PACKET_SIZE] = TS_PACKET_SYNC_BYTE) then
          begin
            {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'TS File Writer -> Found Sync Bytes -> writing'); {$ENDIF}
            FFirstWrite := False;
            FDump.Write(ABuffer^, ASize);
            Exit;
          end;
          dec(ASize);
          inc(ABuffer);
        end;
      end else
      begin
        {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'TS File Writer -> writing'); {$ENDIF}
        FDump.Write(ABuffer^, ASize);
      end;
    end;
  finally
    FWriteLock.Leave;
  end;

  {$IFDEF LOG_PACKETS} Log(FSettings.Logger, Self, 'OnTSData (' + inttohex(integer(ABuffer), 8) + ')', 'leaving'); {$ENDIF}
end;

procedure TDVBGraphBuilder.WriteTSStream(AWrite: Boolean);
begin
  FWriteLock.Enter;
  try
    if Assigned(FDump) then
    begin
      FDump.Free;
      FDump := nil;
    end;
    FFirstWrite := True;

    if AWrite then
    begin
      try
        Log(FSettings.Logger, Self, 'WriteTSStream', 'creating TS File');
        FDump := TFileStream.Create(FSettings.DumpTSPath + 'TS_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.ts', fmCreate);
      except
        Log(FSettings.Logger, Self, 'WriteTSStream', 'Failed creating the TS Writer Stream');
        FDump := nil;
      end;
    end;
  finally
    FWriteLock.Leave;
  end;
end;

function TDVBGraphBuilder.GetReceivedBytes: Integer;
begin
  FReceivedLock.Enter;
  try
    Result := FReceivedBytes;
  finally
    FReceivedLock.Leave;
  end;
end;

procedure TDVBGraphBuilder.SetTeletextSizeMode(ASizeMode: TTeletextSizeMode);
begin
  if Assigned(FOSDFilter) and not FSettings.DisableOSD
    then FTeletext.put_SizeMode(ASizeMode);
end;

procedure TDVBGraphBuilder.ShowOSD;
var
  no, ne: String;
begin
  // Update OSD
  if Assigned(FOSDFilter) and not FSettings.DisableOSD and FSettings.OSDShow then
  begin
    Log(FSettings.Logger, Self, 'ShowOSD', 'updating OSD');

    if FSettings.CurrentChannel < 0 then
    begin
      Log(FSettings.Logger, Self, 'ShowOSD', 'Channel Index < 0');
      Exit;
    end;

    FDeliverLock.Enter;
    try
      FEPG.GetNowNext(FSettings.CurrentChannel, no, ne);
      Log(FSettings.Logger, Self, 'ShowOSD', 'Now and Next Event received');
      Log(FSettings.Logger, Self, 'ShowOSD', 'setting up OSD Filter');
      (FOSDFilter as IOSDFilter).put_OSDText(inttostr(FSettings.CurrentChannel + 1) + ') ' + RemoveUnwantedChars(FSettings.Channels[FSettings.CurrentChannel].Name), no, ne, FSettings.OSDAlpha, FSettings.OSDDuration);
      Log(FSettings.Logger, Self, 'ShowOSD', 'setting up OSD Filter returned');
    finally
      FDeliverLock.Leave;
    end;
  end;
end;

procedure TDVBGraphBuilder.OnPMTBuffer(ABuffer: PByte; ASize: Integer);
var
  pr: TProgram;
begin
  if Assigned(FTransportStream) and (FTransportStream is TDCDVBTuningPluginTransportStream) and ((FTransportStream as TDCDVBTuningPluginTransportStream).Device <> nil) then
  begin
    if FChannelChanged then
    begin
//      pr.ProgramNumber := fcurrentsid;
      (FTransportStream as TDCDVBTuningPluginTransportStream).Device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED, Integer(@pr), 0);
      (FTransportStream as TDCDVBTuningPluginTransportStream).Device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_PMT_BUFFER, Integer(ABuffer), ASize);
      FChannelChanged := False;
    end;
  end;
end;

procedure TDVBGraphBuilder.OnPMT(APMT: TProgramMapSection);

  function ContainsPID(AStream: TDVBBaseStream; AStreamList: TProgramStreamList): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to AStreamList.Count -1 do
    begin
      if AStream.PID = AStreamList[i].ElementaryPID then
      begin
        case AStream.StreamType of
          stVideo:
          begin
            if AStreamList[i].HasVideo then
            begin
              Result := True;
              Exit;
            end;
          end;
          stAudio:
          begin
            if AStreamList[i].IsAudioMP1 or AStreamList[i].IsAudioMP2 or AStreamList[i].IsAudioAC3 then
            begin
              with TDVBAudioStream(AStream) do
              begin
                case Coding of
                  acMP1:
                  begin
                    if AStreamList[i].IsAudioMP1 then
                    begin
                      Result := True;
                      Exit;
                    end;
                  end;
                  acMP2:
                  begin
                    if AStreamList[i].IsAudioMP2 then
                    begin
                      Result := True;
                      Exit;
                    end;
                  end;
                  acAC3:
                  begin
                    if AStreamList[i].IsAudioAC3 then
                    begin
                      Result := True;
                      Exit;
                    end;
                  end;
                end;
              end;
              Result := False;
              Exit;
            end;
          end;
          stTeletext:
          begin
            if AStreamList[i].IsTeletext then
            begin
              Result := True;
              Exit;
            end;
          end;
          stMHPAIT:
          begin
            if AStreamList[i].IsMHPAIT then
            begin
              Result := True;
              Exit;
            end;
          end;
          stMHPData:
          begin
            if AStreamList[i].IsMHPData then
            begin
              Result := True;
              Exit;
            end;
          end;
          stDSMCC:
          begin
            if AStreamList[i].IsDSMCC then
            begin
              Result := True;
              Exit;
            end;
          end;
          stSubtitle:
          begin
            if AStreamList[i].IsSubtitle then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
        Result := False;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  channel: TDVBChannel;
  all_in: Boolean;
  found: Boolean;
  i, c: Integer;
  p: TProgram;
//  stream: TDVBBaseStream;
//  ps: TProgramStream;
//  is_different: Boolean;
begin
  if (FSettings.CurrentChannel < 0) or (FSettings.CurrentChannel >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[FSettings.CurrentChannel];
  if (channel = nil) or (channel.PCRPID <> APMT.PCRPID)
    then Exit;

//  PrintPMT(APMT);

//Exit;

  channel := FSettings.Channels[FSettings.CurrentChannel];

  all_in := True;

  for i := 0 to FPMTList.Count -1 do
  begin
    found := False;
    for c := 0 to APMT.ProgramStreamList.Count -1 do
    begin
      if integer(FPMTList[i]) = APMT.ProgramStreamList[c].ElementaryPID then
      begin
        found := True;
        break;
      end;
    end;
    if not found then
    begin
      all_in := False;
      break;
    end;
  end;

  if all_in then
  begin
    for i := 0 to APMT.ProgramStreamList.Count -1 do
    begin
      found := False;
      for c := 0 to FPMTList.Count -1 do
      begin
        if integer(FPMTList[c]) = APMT.ProgramStreamList[i].ElementaryPID then
        begin
          found := True;
          break;
        end;
      end;
      if not found then
      begin
        all_in := False;
        break;
      end;
    end;
  end;

  if not all_in then
  begin
    FPMTList.Clear;
    for i := 0 to APMT.ProgramStreamList.Count -1 do
    begin
      c := APMT.ProgramStreamList[i].ElementaryPID;
      p.PIDs[i] := c;
      FPMTList.Add(Pointer(c));
    end;

    p.ProgramNumber := APMT.ProgramNumber;
    p.PCRPID := channel.PCRPID;
    p.PMTPID := channel.ProgramMapPID;
    p.NumPIDs := APMT.ProgramStreamList.Count;

    Log(FSettings.Logger, Self, 'OnPMT', 'Program changed');

    if Assigned(FTransportStream) and (FTransportStream is TDCDVBTuningPluginTransportStream) and ((FTransportStream as TDCDVBTuningPluginTransportStream).Device <> nil) then
    begin
      (FTransportStream as TDCDVBTuningPluginTransportStream).Device.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_PROGRAM_CHANGED, Integer(@p), 0);
    end;

    FSettings.Plugins.ProgramChanged(@p);
  end;
end;

function TDVBGraphBuilder.GetStreamInfo: TStreamInfo;
begin
  Result := FStatistics;
end;

procedure TDVBGraphBuilder.VideoInfoCallback(AMode: TVideoType; AWidth: Integer; AHeight: Integer; AAspectRatio: TAspectRatio; AFrameRate: TFrameRate; ABitRate: Int64);
begin
  FStatistics.VideoWidth := AWidth;
  FStatistics.VideoHeight := AHeight;
  FStatistics.VideoAspectRatio := AAspectRatio;
  FStatistics.VideoBitRate := ABitRate;
  FStatistics.VideoFrameRate := AFrameRate;
end;

procedure TDVBGraphBuilder.VideoBytesCallback(ASize: Integer);
begin
  FStatistics.VideoTotalBytes := FStatistics.VideoTotalBytes + Int64(ASize);
end;

procedure TDVBGraphBuilder.DSMCCBytesCallback(ASize: Integer);
begin
  FStatistics.DSMCCTotalBytes := FStatistics.DSMCCTotalBytes + Int64(ASize);
end;

procedure TDVBGraphBuilder.AudioBytesCallback(ASize: Integer);
begin
  FStatistics.AudioTotalBytes := FStatistics.AudioTotalBytes + Int64(ASize);
end;

procedure TDVBGraphBuilder.AudioInfoCallback(AChannel: Integer; ABitrate: Integer; ASamplerate: Integer);
begin
  FStatistics.AudioChannels := AChannel;
  FStatistics.AudioSamplerate := ASamplerate;
  FStatistics.AudioBitrate := ABitrate;
end;

procedure TDVBGraphBuilder.SetTeletextPID(APID: Integer);
begin
  FTeletextPID := APID;

  FDeliverLock.Enter;
  try
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      Log(FSettings.Logger, Self, 'SetTeletextPID', 'setting up Teletext');
      with FTeletext do
      begin
        put_TeletextClear;
        put_TeletextPID(FTeletextPID);
      end;
      FNetworkParser.SetTeletextPID(APID);
    end;
  finally
    FDeliverLock.Leave;
  end;
end;

procedure TDVBGraphBuilder.SetVideoPID(APID: Integer; AType: TVideoType);
var
  d: Cardinal;
begin
  FVideoPID := APID;
  SwitchDecoder(AType);

  FDeliverLock.Enter;
  try
    d := APID;
    FStatistics.VideoPresent := True;
    FStatistics.VideoPID := d;
    FStatistics.VideoType := AType;
    UnmapPIDs(FDemuxPins[0], FSettings.Logger);
    FDemuxPins[0].MapPID(1, @d, MEDIA_ELEMENTARY_STREAM);
  finally
    FDeliverLock.Leave;
  end;
end;

procedure TDVBGraphBuilder.GetVideoPID(out APID: Integer; out AType: TVideoType);
begin
  APID := FVideoPID;
  AType := FDecoderMode;
end;

procedure TDVBGraphBuilder.SetAudioPID(APID: Integer; AType: TAudioType);
var
  d: Cardinal;
begin
  FAudioPID := APID;
  FAudioType := AType;

  FDeliverLock.Enter;
  try
    d := APID;
    FStatistics.AudioPresent := True;
    FStatistics.AudioPID := d;
    FStatistics.AudioType := AType;
    UnmapPIDs(FDemuxPins[1], FSettings.Logger);
    case AType of
      atMPEG1Audio,
      atMPEG2Audio: (FAudioFilter as IAudioFilter).put_MediaType(mtMP2);
      atAC3:        (FAudioFilter as IAudioFilter).put_MediaType(mtAC3);
    end;
    FDemuxPins[1].MapPID(1, @d, MEDIA_ELEMENTARY_STREAM);
  finally
    FDeliverLock.Leave;
  end;
end;

procedure TDVBGraphBuilder.GetAudioPID(out APID: Integer; out AType: TAudioType);
begin
  APID := FAudioPID;
  AType := FAudioType;
end;

procedure TDVBGraphBuilder.SetSubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer);
begin
  FSubtitlePID := APID;
  FSubtitlePCRPID := APCRPID;
  FSubtitleCPID := ACPID;
  FSubtitleAPID := AAPID;

  FDeliverLock.Enter;
  try
    FNetworkParser.SetPCRPID(-1);
    FNetworkParser.SetSubtitlePID(-1);
    if Assigned(FOSDFilter) and not FSettings.DisableOSD then
    begin
      (FOSDFilter as ISubtitleCallback).put_PID(FSubtitlePCRPID, FSubtitlePID, FSubtitleCPID, FSubtitleAPID);
      FNetworkParser.SetPCRPID(FSubtitlePCRPID);
      FNetworkParser.SetSubtitlePID(FSubtitlePID);
    end;
  finally
    FDeliverLock.Leave
  end;
end;

procedure TDVBGraphBuilder.GetSubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer);
begin
  APID := FSubtitlePID;
  APCRPID := FSubtitlePCRPID;
  ACPID := FSubtitleCPID;
  AAPID := FSubtitleAPID;
end;

procedure TDVBGraphBuilder.SetTeletextFastext(AFastext: TTeletextFastext);
begin
  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    FTeletext.put_TeletextFastext(AFastext);
  end;
end;

procedure TDVBGraphBuilder.VMRWndMethod(var Message: TMessage);
var
  x, y: Integer;
  hand_cursor: Boolean;

  procedure CheckCursor;
  begin
    FTeletext.get_TeletextCursorHand(x, y, FVMRWidth, FVMRHeight, FSettings.TeletextOffset, hand_cursor);
    OutputDebugString(PChar(inttostr(x) + ' - ' + inttostr(y)));
    if hand_cursor then
    begin
      FLastCursor := g_CursorHand;
    end else
    begin
      FLastCursor := g_CursorPoint;
    end;
    
    if not FCaptureMouse
      then Exit;

    SetCursor(FLastCursor);
  end;

begin
  case Message.Msg of
    WM_SIZE:
    begin
      FVMRWidth := LOWORD(Message.lParam);
      FVMRHeight := HIWORD(Message.lParam);
    end;
    WM_LBUTTONDOWN:
    begin
      if Assigned(FTeletext) and FTeletextVisible then
      begin
        x := LOWORD(Message.lParam);
        y := HIWORD(Message.lParam);
        CheckCursor;
        if hand_cursor then
        begin
          FTeletext.put_ClickAt(x, y, FVMRWidth, FVMRHeight, FSettings.TeletextOffset);
        end;
      end;
    end;
    WM_MOUSEMOVE:
    begin
      if Assigned(FTeletext) and FTeletextVisible then
      begin
        x := LOWORD(Message.lParam);
        y := HIWORD(Message.lParam);
        CheckCursor;
      end;
    end;
    WM_SETCURSOR:
    begin
      if FCaptureMouse then
      begin
        SetCursor(FLastCursor);
        Message.Result := 0;
        Exit;
      end;
    end;
  end;

  Message.Result := CallWindowProc(FVMRWndMethod, FVMRHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TDVBGraphBuilder.put_OSDChannel(AChannel: String);
begin
  if Assigned(FOSDFilter) and not FSettings.DisableOSD and FSettings.OSDShow then
  begin
    Log(FSettings.Logger, Self, 'put_OSDChannel', 'updating OSD');

    FDeliverLock.Enter;
    try
      (FOSDFilter as IOSDFilter).put_OSDChannel(AChannel, FSettings.OSDAlpha, FSettings.OSDDuration);
    finally
      FDeliverLock.Leave;
    end;
  end;
end;

procedure TDVBGraphBuilder.Activate;
begin
  Log(FSettings.Logger, Self, 'Render', 'Activating the Transport Stream');
  FTransportStream.Active := True;
  if not FTransportStream.Active then
  begin
    Log(FSettings.Logger, Self, 'Render', 'Activating the Transport Stream failed');
//    Clear;
//    Exit;
  end;
end;

procedure TDVBGraphBuilder.SwitchDecoder(AMode: TVideoType);
var
  state: TFilterState;
  filter: IBaseFilter;
  pins: TPinList;
  i: Integer;
  name: WideString;
  mt: TAMMediaType;
  vih: PVideoInfoHeader2;
  pin: IPin;
  hr: HRESULT;
begin
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Request to change the Video Decoder');
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Current Mode: ' + IfThen(FDecoderMode = dmMPEG2, 'MPEG-2', 'H264'));
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Requested Mode: ' + IfThen(AMode = dmMPEG2, 'MPEG-2', 'H264'));

  if (AMode = FDecoderMode) then
  begin
    Log(FSettings.Logger, Self, 'SwitchDecoder', 'No need to change Video Decoder');
    Exit;
  end;

  if (AMode = dmH264) and (FH264VideoDecoder = nil) then
  begin
    Log(FSettings.Logger, Self, 'SwitchDecoder', 'ERROR: Switch to H264 requested but Decoder is NULL');
    Exit;
  end;

  // Stop the Graph
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Get Renderer state');
  hr := (FRendererGraph as IMediaFilter).GetState(0, state);
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Get Renderer state returned 0x' + inttohex(hr, 8) + ' and ' + inttostr(integer(state)));

  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Stop the Graph');
  hr := (FRendererGraph as IMediaControl).Stop;
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Stop the Graph returned 0x' + inttohex(hr, 8));

  // Disconnect old Decoder
  case FDecoderMode of
    dmMPEG2: filter := FMPEG2VideoDecoder;
    dmH264:  filter := FH264VideoDecoder;
  end;

  FDecoderMode := AMode;

  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Pins on "old" Decoder');
  pins := TPinList.Create(filter);
  for i := 0 to pins.Count - 1 do
  begin
    if (pins[i].ConnectedTo(pin)=S_OK) then
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Filter Pin ' + inttostr(i));
      hr := pins[i].Disconnect;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Filter Pin ' + inttostr(i) + ' returned 0x' + inttohex(hr, 8));
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting connected Filter Pin ' + inttostr(i));
      hr := pin.Disconnect;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting connected Filter Pin ' + inttostr(i) + ' returned 0x' + inttohex(hr, 8));
    end;
  end;
  pins.Free;

  // Disconnect Video Analyzer (will be disconnected anyway after setting
  // a new Mediatype on the Demux Pin)
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Pins on Video Analyzer');
  pins := TPinList.Create(FVideoFilter);
  for i := 0 to pins.Count - 1 do
  begin
    if (pins[i].ConnectedTo(pin)=S_OK) then
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Filter Pin ' + inttostr(i));
      hr := pins[i].Disconnect;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting Filter Pin ' + inttostr(i) + ' returned 0x' + inttohex(hr, 8));
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting connected Filter Pin ' + inttostr(i));
      hr := pin.Disconnect;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Disconnecting connected Filter Pin ' + inttostr(i) + ' returned 0x' + inttohex(hr, 8));
    end;
  end;
  pins.Free;

  // Remove old Decoder from Graph
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Removing "old" Decoder from the Graph');
  hr := FRendererGraph.RemoveFilter(filter);
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Removing "old" Decoder from the Graph returned 0x' + inttohex(hr, 8));

  // Setup new MediaType on output Pin of Demux
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'setup new Mediatype on Demux Pin');
  case FDecoderMode of
    dmMPEG2:
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'setup MPEG-2 Mediatype on Demux Pin');
      mt.majortype := KSDATAFORMAT_TYPE_VIDEO;
      mt.subtype := KSDATAFORMAT_SUBTYPE_MPEG2_VIDEO;
      mt.formattype := KSDATAFORMAT_SPECIFIER_MPEG2_VIDEO;
      mt.bFixedSizeSamples := True;
      mt.bTemporalCompression := False;
      mt.lSampleSize := 65536;
      mt.pUnk := nil;
      mt.cbFormat := 216;
      mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);
      CopyMemory(mt.pbFormat, @VIDEO_PIN_PBFORMAT, mt.cbFormat);

      hr := (FMPEG2Demux as IMpeg2Demultiplexer).SetOutputPinMediaType(PWideChar(WideString('(1) Video')), mt);
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'setup MPEG-2 Mediatype on Demux Pin returned 0x' + inttohex(hr, 8));
      CoTaskMemFree(mt.pbFormat);
    end;
    dmH264:
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'setup H264 Mediatype on Demux Pin');
      mt.majortype := MEDIATYPE_Video;
      if IsEqualGUID(CLSID_CoreAVCH264Decoder, FSettings.H264DecoderCLSID) then
      begin
        Log(FSettings.Logger, Self, 'SwitchDecoder', 'CoreAVC Decoder used. Setup "special" H264 Mediatype');
        mt.subtype := FOURCCMap(FCC('h264'));
      end else
      begin
        Log(FSettings.Logger, Self, 'SwitchDecoder', 'CoreAVC Decoder not used. Setup "default" H264 Mediatype');
        mt.subtype := MEDIASUBTYPE_H264;
      end;

      mt.formattype := FORMAT_VideoInfo2;
      mt.bFixedSizeSamples := False;
      mt.bTemporalCompression := True;
      mt.lSampleSize := 1;
      mt.pUnk := nil;
      mt.cbFormat := SizeOf(TVideoInfoHeader2);
      mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);

      vih := mt.pbFormat;

      FillChar(PByte(mt.pbFormat)^, SizeOf(TVideoInfoHeader2), 0);

      vih.bmiHeader.biSize := 28;
      vih.bmiHeader.biWidth := 1920;
      vih.bmiHeader.biHeight := 1080;
      vih.dwPictAspectRatioX := 0;
      vih.dwPictAspectRatioY := 0;
      vih.bmiHeader.biPlanes := 0;
      vih.bmiHeader.biBitCount := 24;
      vih.bmiHeader.biCompression := FCC('h264');

      hr := (FMPEG2Demux as IMpeg2Demultiplexer).SetOutputPinMediaType(PWideChar(WideString('(1) Video')), mt);
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'setup H264 Mediatype on Demux Pin returned 0x' + inttohex(hr, 8));
      CoTaskMemFree(mt.pbFormat);
    end;
  end;

  // connect new Decoder
  case FDecoderMode of
    dmMPEG2:
    begin
      filter := FMPEG2VideoDecoder;
      name   := FSettings.VideoDecoderName;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'new MPEG-2 Decoder is ' + name);
    end;
    dmH264:
    begin
      filter := FH264VideoDecoder;
      name   := FSettings.H264DecoderName;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'new H264 Decoder is ' + name);
    end;
  end;

  if filter = nil then
  begin
    Log(FSettings.Logger, Self, 'SwitchDecoder', 'CRITICAL ERROR: No Video Decoder found');
    Exit;
  end;

  // Add Filter to the Graph
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Adding "new" Decoder to the Graph');
  hr := FRendererGraph.AddFilter(filter, PWideChar(name));
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'Adding "new" Decoder to the Graph returned 0x' + inttohex(hr, 8));

  // connect Demux to Video Analyzer
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect Demux to Video Analyzer');
  hr := FRendererGraph.ConnectDirect(GetOutPin(FMPEG2Demux, 0), GetInPin(FVideoFilter, 0), nil);
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect Demux to Video Analyzer returned 0x' + inttohex(hr, 8));

  // connect Video Analyzer to Decoder
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect Video Analyzer to "new" Decoder');
  hr := FRendererGraph.ConnectDirect(GetOutPin(FVideoFilter, 0), GetInPin(filter, 0), nil);
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect Video Analyzer to "new" Decoder returned 0x' + inttohex(hr, 8));

  // connect Decoder to Renderer
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect "new" Decoder to Renderer');
  hr := FRendererGraph.ConnectDirect(GetOutPin(filter, 0), GetInPin(FVMR9, 0), nil);
  Log(FSettings.Logger, Self, 'SwitchDecoder', 'connect "new" Decoder to Renderer returned 0x' + inttohex(hr, 8));

  // set old Graph State
  case state of
    State_Paused:
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "paused" Mode');
      hr := (FRendererGraph as IMediaControl).Pause;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "paused" Mode returned 0x' + inttohex(hr, 8));
    end;
    State_Running:
    begin
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "running" Mode');
      hr := (FRendererGraph as IMediaControl).Run;
      Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "running" Mode returned 0x' + inttohex(hr, 8));

      i := 0;
      while (i < 10) do
      begin
        if i > 0
          then sleep(100);
        Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "running" Mode');
        hr := (FRendererGraph as IMediaControl).Run;
        Log(FSettings.Logger, Self, 'SwitchDecoder', 'Switch Graph back to "running" Mode returned 0x' + inttohex(hr, 8));

        if hr = S_OK
          then break;

        inc(i);
      end;

    end;
  end;
end;

procedure TDVBGraphBuilder.StopChannel;
begin
  Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Removing Mappings on Pin 0');
  UnmapPIDs(FDemuxPins[0], FSettings.Logger);
  Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Removing Mappings on Pin 1');
  UnmapPIDs(FDemuxPins[1], FSettings.Logger);

  FNetworkParser.SetTeletextPID(-1);
  FNetworkParser.SetPCRPID(-1);
  FNetworkParser.SetSubtitlePID(-1);
  FNetworkParser.RemovePSIMappings;

  if FSettings.OwnDemux
    then (FMPEG2Demux as IDCMpeg2Demultiplexer).SetPCRPID(-1);

  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Reseting Subtitle PID');
    (FOSDFilter as ISubtitleCallback).put_PID(-1, -1, -1, -1);
    FTeletextPID := -1;
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Reseting Teletext PID');
    FTeletext.put_TeletextPID(-1);
  end;

  if Assigned(FVideoFilter) then
  begin
    Log(FSettings.Logger, Self, 'TuneToChannelInternal', 'Disabling Video Filter');
    (FVideoFilter as IVideoFilter).put_Enabled(False);
  end;
end;

procedure TDVBGraphBuilder.CycleTeletext;
begin
  Log(FSettings.Logger, Self, 'ShowTeletext', 'init');
  if Assigned(FOSDFilter) and not FSettings.DisableOSD then
  begin
    FTeletext.put_CycleTeletext;
  end;
end;



initialization
  g_CursorPoint := LoadCursor(0, IDC_ARROW);
  g_CursorHand := LoadCursor(0, IDC_HAND);

end.
