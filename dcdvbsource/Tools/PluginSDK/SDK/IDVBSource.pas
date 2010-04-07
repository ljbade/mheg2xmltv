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

unit IDVBSource;

interface

const
  CLSID_DCDVBSource           : TGuid = '{C88CB623-2CEC-4870-A773-36F34114A1BD}';
  IID_IDCDVBSource            : TGuid = '{8FA96AC0-D500-4939-B022-D3820B400199}';
  IID_IAMStreamSelectInfo     : TGuid = '{3703EF76-94CF-41D7-B050-085E63C93562}';

  DVB_FILTER_VERSION = $00010600;

type
  TVideoType = (
    vtMPEG2,
    vtAVC
  );

  TAudioType = (
    atMPEG1Audio,
    atMPEG2Audio,
    atAC3,
    atDTS,
    atAAC
  );

  TAspectRatio = (
    ar4_3,
    ar16_9,
    ar221_1
  );

  TTeletextSizeMode = (
    tsmNormal,
    tsmDoubleUpper,
    tsmDoubleLower
  );

  TEventDate = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
    Hour: Integer;
    Minute: Integer;
  end;

  TDVBRecordingStatus = (
    rsWaiting,
    rsRecording,
    rsStopped,
    rsFailed,
    rsInvalid
  );

  TDVBRecordingSetting = record
    ID: TGuid;
    StartTime: TEventDate;
    EndTime: TEventDate;
    Name: WideString;
    Location: WideString;
    ChannelIndex: Integer;
    Status: TDVBRecordingStatus;
  end;
  PDVBRecordingSetting = ^TDVBRecordingSetting;

  TSaveTeletextType = (
    ttBitmap,
    ttTXT,
    ttVTX
  );

  TFrameRate = (
    frForbidden,
    fr23_976,
    fr24,
    fr25,
    fr29_97,
    fr30,
    fr50,
    fr59_94,
    fr60
  );

  TTeletextFastext = (
    tfeRed,
    tfeGreen,
    tfeYellow,
    tfeBlue,
    tfeReserved,
    tfeInfo
  );

  TStreamInfo = record
    TotalTSBytes: Int64;
    VideoPresent: Boolean;
    VideoPID: Integer;
    VideoType: TVideoType;
    VideoWidth: Integer;
    VideoHeight: Integer;
    VideoAspectRatio: TAspectRatio;
    VideoBitRate: Integer;
    VideoFrameRate: TFrameRate;
    VideoTotalBytes: Int64;
    DSMCCTotalBytes: Int64;
    AudioPresent: Boolean;
    AudioPID: Integer;
    AudioType: TAudioType;
    AudioChannels: Integer;
    AudioSamplerate: Integer;
    AudioBitrate: Integer;
    AudioTotalBytes: Int64;
  end;
  PStreamInfo = ^TStreamInfo;

  IDCDVBSource = interface(IUnknown)
  ['{8FA96AC0-D500-4939-B022-D3820B400199}']
    function get_Version(out Version: Cardinal): HRESULT; stdcall;
    function get_SignalStatistics(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): HRESULT; stdcall;
    // Channels
    function get_ChannelCount(out ChannelCount: Integer): HRESULT; stdcall;
    function get_ChannelInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_ChannelSelected(out Index: Integer): HRESULT; stdcall;
    function put_ChannelSelected(Index: Integer): HRESULT; stdcall;
    function put_PreviousChannel: HRESULT; stdcall;
    function put_NextChannel: HRESULT; stdcall;
    // Audio Streams
    function get_AudioStreamCount(out CountStreams: Integer): HRESULT; stdcall;
    function get_AudioStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_AudioStreamSelected(out Index: Integer): HRESULT; stdcall;
    function put_AudioStreamSelected(Index: Integer): HRESULT; stdcall;
    // Teletext
    function put_TeletextShow(Show: LongBool): HRESULT; stdcall;
    function get_TeletextShow(out Show: LongBool): HRESULT; stdcall;
    function put_TeletextTransparent(Transparent: LongBool): HRESULT; stdcall;
    function get_TeletextTransparent(out Transparent: LongBool): HRESULT; stdcall;
    function put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT; stdcall;
    function get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT; stdcall;
    function put_TeletextNumber(Number: Integer): HRESULT; stdcall;
    function put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT; stdcall;
    // EPG
    function put_EPGClearAll: HRESULT; stdcall;
    function get_EPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): HRESULT; stdcall;
    function get_EPGTimeOffset(out TimeOffset: Integer): HRESULT; stdcall;
    // MHP
    function get_MHPRoot(out MHP: PChar): HRESULT; stdcall;
    // Recordings
    function get_RecordingsCount(out Count: Integer): HRESULT; stdcall;
    function get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_Recording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_EditRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    function put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
    // Subtitle
    function get_SubtitleStreamCount(out CountStreams: Integer): HRESULT; stdcall;
    function get_SubtitleStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
    function get_SubtitleStreamSelected(out Index: Integer): HRESULT; stdcall;
    function put_SubtitleStreamSelected(Index: Integer): HRESULT; stdcall;

    function ShowOSD: HRESULT; stdcall;
    function get_StreamInfo(out AStreamInfo: TStreamInfo): HRESULT; stdcall;
    function put_TeletextSizeMode(ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
    function get_TeletextSizeMode(out ASizeMode: TTeletextSizeMode): HRESULT; stdcall;

    function get_PluginsCount(out ACount: Integer): HRESULT; stdcall;
    function put_PluginEnabled(AIndex: Integer; AEnabled: Boolean): HRESULT; stdcall;
    function get_PluginEnabled(AIndex: Integer; out AEnabled: Boolean): HRESULT; stdcall;
    function get_Plugin(AIndex: Integer; out APlugin: IUnknown): HRESULT; stdcall;

    function put_TeletextFastext(AFastext: TTeletextFastext): HRESULT; stdcall;
    function put_CaptureVideoWindowCursor(ACapture: Boolean): HRESULT; stdcall;
  end;

  // This is exposed as ppUnk in IAMStreamSelect::Info and shows the Now & Next Event
  // Any function returns S_OK when succeeded otherwise E_FAIL
  // Every PWideChar Parameter must be freed using CoTaskMemFree (only with S_OK Result)
  IAMStreamSelectInfo = interface(IUnknown)
  ['{3703EF76-94CF-41D7-B050-085E63C93562}']
    function get_Name(out Name: PWideChar): HRESULT; stdcall;
    function get_Now(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
    function get_Next(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
  end;

const
  IID_IDemuxControl: TGuid = '{25B7068D-83CE-488B-87EC-9B7DBD4FA447}';

type
  IDemuxControl = interface(IUnknown)
  ['{25B7068D-83CE-488B-87EC-9B7DBD4FA447}']
    function put_VideoPID(APID: Integer; AType: TVideoType): HRESULT; stdcall;
    function get_VideoPID(out APID: Integer; out AType: TVideoType): HRESULT; stdcall;
    function put_AudioPID(APID: Integer; AType: TAudioType): HRESULT; stdcall;
    function get_AudioPID(out APID: Integer; out AType: TAudioType): HRESULT; stdcall;
    function put_TeletextPID(APID: Integer): HRESULT; stdcall;
    function get_TeletextPID(out APID: Integer): HRESULT; stdcall;
    function put_SubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer): HRESULT; stdcall;
    function get_SubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer): HRESULT; stdcall;
  end;

implementation

end.
