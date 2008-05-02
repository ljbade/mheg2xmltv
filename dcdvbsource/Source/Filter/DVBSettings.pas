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

unit DVBSettings;

interface

uses
  Windows, Classes, DVBChannelList, ActiveX, MPEGConst, SysUtils, BDAUtils,
  ISO639LanguageCode, DirectShow9, BDAConst, Logger, IDVBSource, DVBMHPParser,
  DateUtils, JvSimpleXml, DVBRecordings, DCDVBTuningPlugins, DVBConst, DCDVBShared,
  DCDVBPluginManager, Graphics;

type
  TOSDColor = record
    StartColor: TColor;
    EndColor: TColor;
    FontColor: TColor;
    FontShadowColor: TColor;
  end;

  TARMode = record
    Width: Integer;
    Height: Integer;
    AX: Integer;
    AY: Integer;
  end;

  TDVBSettings = class
  private
    FARMode: TARMode;
    FOSDColor: TOSDColor;
    FPlugins: TDCDVBDPluginList;
    FDisableEPG: Boolean;
    FDisableDSMCC: Boolean;
    FCurrentChannel: Integer;
    FSaveLastChannel: Boolean;
    FIgnoreDeviceID: Boolean;

    FGraphFailed: Boolean;
    FChannels: TDVBChannelsList;

    FFileName: WideString;
    FDirectory: WideString;
    FMHPDirectory: WideString;
    FEPGDirectory: WideString;
    FLogDirectory: WideString;

    FTunerName: WideString;
    FTunerID: WideString;
    FTunerSettings: String;

    FVideoDecoderName: WideString;
    FVideoDecoderCLSID: TGuid;
    FH264DecoderName: WideString;
    FH264DecoderCLSID: TGuid;
    FAudioDecoderName: WideString;
    FAudioDecoderCLSID: TGuid;
    FAC3DecoderName: WideString;
    FAC3DecoderCLSID: TGuid;
    FAACDecoderName: WideString;
    FAACDecoderCLSID: TGuid;
    FPostProcessorName: WideString;
    FPostProcessorCLSID: TGuid;

    FTimeShiftingEnabled: Boolean;
    FTimeShiftingDuration: Integer;

    FEPGTimeOffset: Integer;

    FVideoOffset: Integer;
    FTeletextOffset: Integer;
    FTeletextAlpha: Integer;

    FDeviceType: Integer;

    FTSServerEnabled: Boolean;
    FTSServerPort: Integer;
    FTSServerGroup: String;
    FTSServerType: Integer;

    FDumpTSPath: WideString;

    FChangeRefClock: Boolean;

    FLog: TLogger;

    FStreaming: Boolean;
    FServerAddress: String;
    FServerPort: Integer;
    FServerType: Integer;

    FMHPParser: TDSMCCParser;
    FTSFileName: String;
    FOwnDemux: Boolean;

    FOSDShow: Boolean;
    FOSDAlpha: Integer;
    FOSDDuration: Integer;
    FOSDOffset: Integer;
    FRecordings: TDVBRecordings;
    FDisableOSD: Boolean;
    FDirectTuning: Boolean;
    FDevices: TDCDVBTPluginList;
    FSubtitleOffset: Integer;
    FSubtitleAlpha: Integer;
    FPacketSize: Integer;
    FTeletextAspectRatio: Integer;
    FTeletextFPS: Integer;
    FPreferMPEG2Video: Boolean;
    FOverlay: Boolean;
    FStopSelect: Boolean;
    procedure SetupStreamBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function LoadFromFile(AFileName: WideString): Boolean;

    property FileName: WideString read FFileName;
    property Directory: WideString read FDirectory;
    property MHPDirectory: WideString read FMHPDirectory;
    property EPGDirectory: WideString read FEPGDirectory;
    property LogDirectory: WideString read FLogDirectory;

    property TunerName: WideString read FTunerName;
    property TunerID: WideString read FTunerID;
    property TunerSettings: String read FTunerSettings write FTunerSettings;

    property VideoDecoderName: WideString read FVideoDecoderName;
    property VideoDecoderCLSID: TGuid read FVideoDecoderCLSID;
    property H264DecoderName: WideString read FH264DecoderName;
    property H264DecoderCLSID: TGuid read FH264DecoderCLSID;
    property AudioDecoderName: WideString read FAudioDecoderName;
    property AudioDecoderCLSID: TGuid read FAudioDecoderCLSID;
    property AC3DecoderName: WideString read FAC3DecoderName;
    property AC3DecoderCLSID: TGuid read FAC3DecoderCLSID;
    property AACDecoderName: WideString read FAACDecoderName;
    property AACDecoderCLSID: TGuid read FAACDecoderCLSID;
    property PostProcessorName: WideString read FPostProcessorName;
    property PostProcessorCLSID: TGuid read FPostProcessorCLSID;

    property TimeShiftingEnabled: Boolean read FTimeShiftingEnabled;
    property TimeShiftingDuration: Integer read FTimeShiftingDuration;

    property EPGTimeOffset: Integer read FEPGTimeOffset;

    property VideoOffset: Integer read FVideoOffset;
    property TeletextOffset: Integer read FTeletextOffset;
    property TeletextAlpha: Integer read FTeletextAlpha;

    property DeviceType: Integer read FDeviceType;

    property TSServerEnabled: Boolean read FTSServerEnabled;
    property TSServerPort: Integer read FTSServerPort;
    property TSServerGroup: String read FTSServerGroup;
    property TSServerType: Integer read FTSServerType;

    property DumpTSPath: WideString read FDumpTSPath;

    property ChangeRefClock: Boolean read FChangeRefClock;

    property Streaming: Boolean read FStreaming;
    property ServerAddress: String read FServerAddress;
    property ServerPort: Integer read FServerPort;
    property ServerType: Integer read FServerType;

    property OSDShow: Boolean read FOSDShow;
    property OSDAlpha: Integer read FOSDAlpha;
    property OSDDuration: Integer read FOSDDuration;
    property OSDOffset: Integer read FOSDOffset;

    property Channels: TDVBChannelsList read FChannels;
    property GraphFailed: Boolean read FGraphFailed write FGraphFailed;
    property CurrentChannel: Integer read FCurrentChannel write FCurrentChannel;
    property SaveLastChannel: Boolean read FSaveLastChannel write FSaveLastChannel;
    property Logger: TLogger read FLog write FLog;
    property DisableOSD: Boolean read FDisableOSD;
    property DirectTuning: Boolean read FDirectTuning;

    property MHPParser: TDSMCCParser read FMHPParser write FMHPParser;
    property Recordings: TDVBRecordings read FRecordings;
    property Devices: TDCDVBTPluginList read FDevices;
    property TSFileName: String read FTSFileName;
    property DisableEPG: Boolean read FDisableEPG;
    property DisableDSMCC: Boolean read FDisableDSMCC;
    property SubtitleOffset: Integer read FSubtitleOffset;
    property SubtitleAlpha: Integer read FSubtitleAlpha;
    property PacketSize: Integer read FPacketSize;
    property Plugins: TDCDVBDPluginList read FPlugins;
    property OSDColor: TOSDColor read FOSDColor;
    property TeletextAspectRatio: Integer read FTeletextAspectRatio;
    property OwnDemux: Boolean read FOwnDemux;
    property TeletextFPS: Integer read FTeletextFPS;
    property IgnoreDeviceID: Boolean read FIgnoreDeviceID;
    property PreferMPEG2Video: Boolean read FPreferMPEG2Video;
    property ARMode: TARMode read FARMode;
    property Overlay: Boolean read FOverlay;
    property StopSelect: Boolean read FStopSelect;
  end;

  function GetDate(ADate: TEventDate): TDateTime; overload;
  function GetDate(ADate: TDateTime): TEventDate; overload;

implementation

uses Math;

function GetDate(ADate: TEventDate): TDateTime;
begin
  Result := EncodeDateTime(ADate.Year, ADate.Month, ADate.Day, ADate.Hour, ADate.Minute, 0, 0);
end;

function GetDate(ADate: TDateTime): TEventDate;
begin
  Result.Year := YearOf(ADate);
  Result.Month := MonthOf(ADate);
  Result.Day := DayOf(ADate);
  Result.Hour := HourOf(ADate);
  Result.Minute := MinuteOf(ADate);
end;

(*** TDVBSettings *************************************************************)

constructor TDVBSettings.Create;
begin
  inherited Create;
  FPlugins := nil;
  FLog := nil;
  FDevices := nil;
  FChannels := TDVBChannelsList.Create;
  FMHPParser := TDSMCCParser.Create;
  FRecordings := TDVBRecordings.Create(Self);
  Clear;
end;

destructor TDVBSettings.Destroy;
var
  xml: TJvSimpleXml;
  n1, n2: TJvSimpleXmlElem;
  a1: TJvSimpleXmlProp;
  i, c: Integer;
  rec: PDVBRecordingSetting;
begin
  Log(FLog, Self, 'Destroy', 'destructor');
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    xml := TJvSimpleXml.Create(nil);
    xml.LoadFromFile(FFileName);

    if FSaveLastChannel then
    begin
      n1 := xml.root.Items.ItemNamed['settings'];
      if Assigned(n1) then
      begin
        n1 := n1.Items.ItemNamed['channel'];
        if Assigned(n1) then
        begin
          a1 := n1.Properties.ItemNamed['last'];
          if Assigned(a1) then
          begin
            a1.IntValue := FCurrentChannel;
          end else
          begin
            n1.Properties.Add('last', FCurrentChannel);
          end;
        end;
      end;
    end;

    xml.root.Items.Delete('recordings');
    FRecordings.get_RecordingsCount(c);
    if c > 0 then
    begin
      n1 := xml.Root.Items.Add('recordings');
      for i := 0 to c -1 do
      begin
        n2 := n1.Items.Add('recording');
        FRecordings.get_Recording(i, rec);

        n2.Properties.Add('id', GUIDToString(rec.ID));
        n2.Properties.Add('start', FloatToStr(GetDate(rec.StartTime)));
        n2.Properties.Add('end', FloatToStr(GetDate(rec.EndTime)));
        n2.Properties.Add('name', rec.Name);
        n2.Properties.Add('location', rec.Location);
        n2.Properties.Add('channel', rec.ChannelIndex);
      end;
    end;

    xml.root.Items.Delete('plugins');
    if Assigned(FPlugins) and (FPlugins.Count > 0) then
    begin
      n1 := xml.Root.Items.Add('plugins');
      for i := 0 to FPlugins.Count -1 do
      begin
        n2 := n1.Items.Add('plugins');
        n2.Properties.Add('clsid', GUIDToString(FPlugins[i].ID));
        n2.Properties.Add('enabled', IfThen(FPlugins[i].Enabled, 1, 0));
        n2.Properties.Add('settings', FPlugins[i].Settings);
      end;
    end;

    if FTunerSettings <> '' then
    begin
      n1 := xml.root.Items.ItemNamed['settings'];
      if Assigned(n1) then
      begin
        n1 := n1.Items.ItemNamed['tuner'];
        if Assigned(n1) then
        begin
          a1 := n1.Properties.ItemNamed['settings'];
          if Assigned(a1) then
          begin
            a1.Value := FTunerSettings;
          end else
          begin
            n1.Properties.Add('settings', FTunerSettings);
          end;
        end;
      end;
    end;

    xml.SaveToFile(FFileName);
    xml.Free;
  end;

  Clear;
  FChannels.Free;

  if Assigned(FDevices) then
  begin
    FDevices.Free;
    FDevices := nil;
  end;
  if Assigned(FLog) then
  begin
    FLog.Free;
    FLog := nil;
  end;

  if Assigned(FPlugins) then
  begin
    FPlugins.Free;
    FPlugins := nil;
  end;

  FMHPParser.Free;
  FRecordings.Free;
  inherited Destroy;
end;

procedure TDVBSettings.Clear;
begin
  Log(FLog, Self, 'Clear', 'clearing Settings');

  FTSServerEnabled := False;
  FTSServerPort := 12345;
  FTSServerGroup := '225.1.1.1';

  FChangeRefClock := False;

  FCurrentChannel := -1;
  FSaveLastChannel := False;

  FChannels.Clear;
  FGraphFailed := True;

  FFileName := '';
  FDirectory := '';
  FMHPDirectory := '';
  FEPGDirectory := '';
  FLogDirectory := '';
  FDumpTSPath := '';
  FTSFileName := '';

  FPacketSize := -1;
  FTunerName := '';
  FTunerID := '';

  FDisableEPG := False;
  FDisableDSMCC := False;

  FVideoDecoderName := '';
  FVideoDecoderCLSID := GUID_NULL;
  FAudioDecoderName := '';
  FAudioDecoderCLSID := GUID_NULL;
  FPostProcessorName := '';
  FPostProcessorCLSID := GUID_NULL;
  FH264DecoderName := '';
  FH264DecoderCLSID := GUID_NULL;
  FAC3DecoderName := '';
  FAC3DecoderCLSID := GUID_NULL;
  FAACDecoderName := '';
  FAACDecoderCLSID := GUID_NULL;

  FTimeShiftingEnabled := False;
  FTimeShiftingDuration := 1;

  FEPGTimeOffset := GetUTCDifference;

  FVideoOffset := 0;
  FTeletextOffset := 0;
  FTeletextAlpha := 10;

  FStreaming := False;
  FServerAddress := '225.1.1.1';
  FServerPort := 12345;

  FOSDShow := False;
  FOSDAlpha := 10;
  FOSDDuration := 0;
  FOSDOffset := 0;

  FSubtitleOffset := 0;
  FSubtitleAlpha := 10;
  FDeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN;

  FOSDColor.StartColor := $00B07D44;
  FOSDColor.EndColor := $00754923;
  FOSDColor.FontColor := clWhite;
  FOSDColor.FontShadowColor := clBlack;
  FTeletextAspectRatio := 0;

  FTeletextFPS := 2;
  FOwnDemux := False;
  FIgnoreDeviceID := False;
  FPreferMPEG2Video := False;
  FARMode.Width := 1920;
  FARMode.Height := 1080;
  FARMode.AX := 16;
  FARMode.AY := 9;
  FOverlay := False;
  FStopSelect := False;
end;

function TDVBSettings.LoadFromFile(AFileName: WideString): Boolean;

  function GetLogging(xml: TJvSimpleXml): Boolean;
  var
    n1, n2: TJvSimpleXmlElem;
    a1: TJvSimpleXmlProp;
  begin
    Result := False;

    n1 := xml.Root.Items.ItemNamed['settings'];
    if not Assigned(n1) then
    begin
      Clear;
      Exit;
    end;

    // Logging
    n2 := n1.Items.ItemNamed['debug'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['writelog'];
      if Assigned(a1) then Result := Boolean(a1.IntValue);
    end;
  end;

var
  xml: TJvSimpleXml;
  n1, n2, n3, n4, n5: TJvSimpleXmlElem;
  a1, a2, a3, a4, a5: TJvSimpleXmlProp;
  i, c, k: Integer;
  chan: TDVBChannel;
  stream: TDVBBaseStream;
  v: String;
  rec: TDVBRecordingSetting;
  clsid: TGuid;
begin
  Result := False;
  Clear;

  if Assigned(FDevices) then
  begin
    FDevices.Free;
    FDevices := nil;
  end;
  if Assigned(FLog) then
  begin
    FLog.Free;
    FLog := nil;
  end;

  FFileName := AFileName;
  FDirectory := ExtractFilePath(FFileName);
  FMHPDirectory := FDirectory + 'MHPData\';
  FEPGDirectory := FDirectory + 'EPGData\';
  FLogDirectory := FDirectory + 'LogData\';
  FDumpTSPath := FDirectory + 'TSData\';

  if not DirectoryExists(FMHPDirectory)
    then CreateDir(FMHPDirectory);

  if not DirectoryExists(FEPGDirectory)
    then CreateDir(FEPGDirectory);

  if not DirectoryExists(FLogDirectory)
    then CreateDir(FLogDirectory);

  if not DirectoryExists(FDumpTSPath)
    then CreateDir(FDumpTSPath);

  xml := TJvSimpleXml.Create(nil);
  try
    xml.LoadFromFile(AFileName);

    if GetLogging(xml) then
    begin
      FLog := TFileLogger.Create(FLogDirectory + 'DCDVBSource_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.log');
      FLog.Enabled := True;
    end;

    FDevices := TDCDVBTPluginList.Create(FLog);
    if Assigned(FPlugins) then
    begin
      FPlugins.Free;
      FPlugins := nil;
    end;
    FPlugins := TDCDVBDPluginList.Create(FLog);
    Log(FLog, Self, 'LoadFromFile', 'loading XML "plugins"');
    n1 := xml.root.Items.ItemNamed['plugins'];
    if Assigned(n1) then
    begin
      for i := 0 to n1.Items.Count -1 do
      begin
        n2 := n1.Items.Item[i];
        a1 := n2.Properties.ItemNamed['clsid'];
        a2 := n2.Properties.ItemNamed['enabled'];
        a3 := n2.Properties.ItemNamed['settings'];

        if not Assigned(a1)
          then Continue;

        try
          clsid := StringToGUID(a1.Value);
        except
          Continue;
        end;

        for c := 0 to FPlugins.Count -1 do
        begin
          if IsEqualGUID(clsid, FPlugins[c].ID) then
          begin
            if Assigned(a2)
              then FPlugins[c].Enabled := Boolean(a2.IntValue);
            if Assigned(a3) and (a3.Value <> '')
              then FPlugins[c].Settings := a3.Value;
          end;
        end;
      end;
    end;


    Log(FLog, Self, 'LoadFromFile', 'loading XML "settings"');
    n1 := xml.root.Items.ItemNamed['settings'];
    if not Assigned(n1) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML "settings" not present');
      Clear;
      Exit;
    end;

    // Tuner
    Log(FLog, Self, 'LoadFromFile', 'loading XML "tuner"');
    n2 := n1.Items.ItemNamed['tuner'];
    if not Assigned(n2) then
    begin
      n2 := n1.Items.ItemNamed['bdatuner'];
      if not Assigned(n2) then
      begin
        Log(FLog, Self, 'LoadFromFile', 'XML "tuner" not present');
        Clear;
        Exit;
      end;
    end;

    a1 := n2.Properties.ItemNamed['name'];
    if Assigned(a1) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML tuner is Hardware');
      a1 := n2.Properties.ItemNamed['name'];
      a2 := n2.Properties.ItemNamed['id'];
      a3 := n2.Properties.ItemNamed['type'];
      a4 := n2.Properties.ItemNamed['ignoreid'];
      if not Assigned(a1) or not Assigned(a2) or not Assigned(a3) then
      begin
        Log(FLog, Self, 'LoadFromFile', 'XML tuner "name", "id" or "type" not present');
        Clear;
        Exit;
      end;

      FTunerName := a1.Value;
      FTunerID := a2.Value;
      FDeviceType := a3.IntValue;

      a1 := n2.Properties.ItemNamed['settings'];
      if Assigned(a1)
        then FTunerSettings := a1.Value;

      if Assigned(a4)
        then FIgnoreDeviceID := Boolean(a4.IntValue);

      Log(FLog, Self, 'LoadFromFile', 'Tuner Name: ' + FTunerName);
      Log(FLog, Self, 'LoadFromFile', 'Tuner ID: ' + FTunerID);
      Log(FLog, Self, 'LoadFromFile', 'Tuner Settings: ' + FTunerSettings);
      Log(FLog, Self, 'LoadFromFile', 'Network Type: ' + IntToStr(FDeviceType));
    end else
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML tuner is not Hardware');
      a1 := n2.Properties.ItemNamed['host'];
      if Assigned(a1) then
      begin
        Log(FLog, Self, 'LoadFromFile', 'XML tuner is Stream');

        a1 := n2.Properties.ItemNamed['host'];
        a2 := n2.Properties.ItemNamed['port'];
        a3 := n2.Properties.ItemNamed['type'];
        if not Assigned(a1) or not Assigned(a2) or not Assigned(a3) then
        begin
          Log(FLog, Self, 'LoadFromFile', 'XML tuner "host" or "port" or "type" not present');
          Clear;
          Exit;
        end;

        FServerAddress := a1.Value;
        FServerPort := a2.IntValue;
        FServerType := a3.IntValue;

        a3 := n2.Properties.ItemNamed['filename'];
        if Assigned(a3) then
        begin
          FTSFileName := a3.Value;
        end;

        Log(FLog, Self, 'LoadFromFile', 'Server Addresse: ' + FServerAddress);
        Log(FLog, Self, 'LoadFromFile', 'Server Port: ' + inttostr(FServerPort));
        Log(FLog, Self, 'LoadFromFile', 'Server Type: ' + inttostr(FServerType));

        FStreaming := True;
      end else
      begin
        Log(FLog, Self, 'LoadFromFile', 'XML tuner is neither Hardware or Stream');
        Clear;
        Exit;
      end;
    end;

    // MPEG-2 Video Decoder
    Log(FLog, Self, 'LoadFromFile', 'loading XML "mpeg2videodecoder"');
    n2 := n1.Items.ItemNamed['mpeg2videodecoder'];
    if not Assigned(n2) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML "mpeg2videodecoder" not present');
      Clear;
      Exit;
    end;

    a1 := n2.Properties.ItemNamed['prefer'];
    if Assigned(a1) then
    begin
      FPreferMPEG2Video := Boolean(a1.IntValue);
    end;

    a1 := n2.Properties.ItemNamed['name'];
    a2 := n2.Properties.ItemNamed['clsid'];
    if not Assigned(a1) or not Assigned(a2) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML mpeg2videodecoder "name" or "clsid" not present');
      Clear;
      Exit;
    end;

    FVideoDecoderName := a1.Value;
    try
      FVideoDecoderCLSID := StringToGUID(a2.Value);
    except
      Log(FLog, Self, 'LoadFromFile', 'XML "mpeg2videodecoder" GUID is broken');
      Clear;
      Exit;
    end;

    Log(FLog, Self, 'LoadFromFile', 'MPEG-2 VideoDecoder Name: ' + FVideoDecoderName);
    Log(FLog, Self, 'LoadFromFile', 'MPEG-2 VideoDecoder CLSID: ' + GUIDToString(FVideoDecoderCLSID));
    Log(FLog, Self, 'LoadFromFile', 'MPEG-2 VideoDecoder Prefer: ' + inttostr(integer(FPreferMPEG2Video)));

    // H264 Video Decoder
    Log(FLog, Self, 'LoadFromFile', 'loading XML "h264videodecoder"');
    n2 := n1.Items.ItemNamed['h264videodecoder'];
    if not Assigned(n2) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML "h264videodecoder" not present');
    end else
    begin
      a1 := n2.Properties.ItemNamed['width'];
      if (Assigned(a1)) then FARMode.Width := a1.IntValue;
      a1 := n2.Properties.ItemNamed['height'];
      if (Assigned(a1)) then FARMode.Height := a1.IntValue;
      a1 := n2.Properties.ItemNamed['ax'];
      if (Assigned(a1)) then FARMode.AX := a1.IntValue;
      a1 := n2.Properties.ItemNamed['ay'];
      if (Assigned(a1)) then FARMode.AY := a1.IntValue;
      a1 := n2.Properties.ItemNamed['overlay'];
      if (Assigned(a1)) then FOverlay := Boolean(a1.IntValue);

      a1 := n2.Properties.ItemNamed['name'];
      a2 := n2.Properties.ItemNamed['clsid'];
      if not Assigned(a1) or not Assigned(a2) then
      begin
        Log(FLog, Self, 'LoadFromFile', 'XML h264videodecoder "name" or "clsid" not present');
      end else
      begin
        FH264DecoderName := a1.Value;
        try
          FH264DecoderCLSID := StringToGUID(a2.Value);
        except
          Log(FLog, Self, 'LoadFromFile', 'XML "h264videodecoder" GUID is broken');
        end;

        Log(FLog, Self, 'LoadFromFile', 'H264 VideoDecoder Name: ' + FH264DecoderName);
        Log(FLog, Self, 'LoadFromFile', 'H264 VideoDecoder CLSID: ' + GUIDToString(FH264DecoderCLSID));
      end;
    end;

    // Audio Decoder
    Log(FLog, Self, 'LoadFromFile', 'loading XML "mpegaudiodecoder"');
    n2 := n1.Items.ItemNamed['mpegaudiodecoder'];
    if not Assigned(n2) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML "mpegaudiodecoder" not present');
      Clear;
      Exit;
    end;

    a1 := n2.Properties.ItemNamed['name'];
    a2 := n2.Properties.ItemNamed['clsid'];
    if not Assigned(a1) or not Assigned(a2) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'XML mpegaudiodecoder "name" or "clsid" not present');
      Clear;
      Exit;
    end;

    FAudioDecoderName := a1.Value;
    try
      FAudioDecoderCLSID := StringToGUID(a2.Value);
    except
      Log(FLog, Self, 'LoadFromFile', 'XML "mpegaudiodecoder" GUID is broken');
      Clear;
      Exit;
    end;

    Log(FLog, Self, 'LoadFromFile', 'MPEG AudioDecoder Name: ' + FAudioDecoderName);
    Log(FLog, Self, 'LoadFromFile', 'MPEG AudioDecoder CLSID: ' + GUIDToString(FAudioDecoderCLSID));

    // Post Processor
    Log(FLog, Self, 'LoadFromFile', 'loading XML "postprocessor"');
    n2 := n1.Items.ItemNamed['postprocessor'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['name'];
      a2 := n2.Properties.ItemNamed['clsid'];
      if Assigned(a1) and Assigned(a2) then
      begin
        FPostProcessorName := a1.Value;
        Log(FLog, Self, 'LoadFromFile', 'PostProcessor Name: ' + FPostProcessorName);
        try
          FPostProcessorCLSID := StringToGUID(a2.Value);
        except
          Log(FLog, Self, 'LoadFromFile', 'PostProcessor CLSID corrupted');
          FPostProcessorCLSID := GUID_NULL;
          FPostProcessorName := '';
        end;
        Log(FLog, Self, 'LoadFromFile', 'PostProcessor CLSID: ' + GUIDToString(FPostProcessorCLSID));
      end;
    end else
    begin
      Log(FLog, Self, 'LoadFromFile', 'No PostProcessor selected');
    end;

    // Time Shifting
    Log(FLog, Self, 'LoadFromFile', 'loading XML "timeshift"');
    n2 := n1.Items.ItemNamed['timeshift'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['enabled'];
      if Assigned(a1) then FTimeShiftingEnabled := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['duration'];
      if Assigned(a1) then FTimeShiftingDuration := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Enable TimeShiftingloading: ' + inttostr(integer(FTimeShiftingEnabled)));
    Log(FLog, Self, 'LoadFromFile', 'TimeShifting duration: ' + inttostr(FTimeShiftingDuration));

    // EPG
    Log(FLog, Self, 'LoadFromFile', 'loading XML "epg"');
    n2 := n1.Items.ItemNamed['epg'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['timeoffset'];
      if Assigned(a1) then inc(FEPGTimeOffset, Integer(a1.IntValue));
    end;
    Log(FLog, Self, 'LoadFromFile', 'EPG Offset: ' + inttostr(FEPGTimeOffset));

    // Video
    Log(FLog, Self, 'LoadFromFile', 'loading XML "video"');
    n2 := n1.Items.ItemNamed['video'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['offset'];
      if Assigned(a1) then FVideoOffset := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Video Offset: ' + inttostr(FVideoOffset));

    // Teletext
    Log(FLog, Self, 'LoadFromFile', 'loading XML "teletext"');
    n2 := n1.Items.ItemNamed['teletext'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['offset'];
      if Assigned(a1) then FTeletextOffset := a1.IntValue;
      a1 := n2.Properties.ItemNamed['alpha'];
      if Assigned(a1) then FTeletextAlpha := a1.IntValue;
      a1 := n2.Properties.ItemNamed['ar'];
      if Assigned(a1) then FTeletextAspectRatio := a1.IntValue;
      a1 := n2.Properties.ItemNamed['fps'];
      if Assigned(a1) then FTeletextFPS := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Teletext Offset: ' + inttostr(FTeletextOffset));
    Log(FLog, Self, 'LoadFromFile', 'Teletext Alpha: ' + inttostr(FTeletextAlpha));
    Log(FLog, Self, 'LoadFromFile', 'Teletext AspectRatio: ' + inttostr(FTeletextAspectRatio));
    Log(FLog, Self, 'LoadFromFile', 'Teletext FPS: ' + inttostr(FTeletextFPS));

    // Last Channel
    Log(FLog, Self, 'LoadFromFile', 'loading XML "channel" (last tuned channel)');
    n2 := n1.Items.ItemNamed['channel'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['save'];
      if Assigned(a1) then FSaveLastChannel := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['last'];
      if Assigned(a1) then FCurrentChannel := a1.IntValue;
      if not FSaveLastChannel then
        FCurrentChannel := -1;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Save Last Channel: ' + inttostr(integer(FSaveLastChannel)));
    Log(FLog, Self, 'LoadFromFile', 'Last Channel: ' + inttostr(FCurrentChannel));

    // Streaming
    Log(FLog, Self, 'LoadFromFile', 'loading XML "streaming"');
    n2 := n1.Items.ItemNamed['streaming'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['enabled'];
      if Assigned(a1) then FTSServerEnabled := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['group'];
      if Assigned(a1) then FTSServerGroup := a1.Value;
      a1 := n2.Properties.ItemNamed['port'];
      if Assigned(a1) then FTSServerPort := a1.IntValue;
      a1 := n2.Properties.ItemNamed['type'];
      if Assigned(a1) then FTSServerType := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Enabled: ' + inttostr(integer(FTSServerEnabled)));
    Log(FLog, Self, 'LoadFromFile', 'Group: ' + FTSServerGroup);
    Log(FLog, Self, 'LoadFromFile', 'Port: ' + inttostr(FTSServerPort));
    Log(FLog, Self, 'LoadFromFile', 'Type: ' + inttostr(FTSServerType));

    // Clock
    Log(FLog, Self, 'LoadFromFile', 'loading XML "clock"');
    n2 := n1.Items.ItemNamed['clock'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['change'];
      if Assigned(a1) then FChangeRefClock := Boolean(a1.IntValue);
    end;
    Log(FLog, Self, 'LoadFromFile', 'Change Reference Clock Enabled: ' + inttostr(integer(FChangeRefClock)));

    // OSD
    Log(FLog, Self, 'LoadFromFile', 'loading XML "osd"');
    n2 := n1.Items.ItemNamed['osd'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['enabled'];
      if Assigned(a1) then FOSDShow := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['alpha'];
      if Assigned(a1) then FOSDAlpha := a1.IntValue;
      a1 := n2.Properties.ItemNamed['duration'];
      if Assigned(a1) then FOSDDuration := a1.IntValue;
      a1 := n2.Properties.ItemNamed['offset'];
      if Assigned(a1) then FOSDOffset := a1.IntValue;
      a1 := n2.Properties.ItemNamed['startcolor'];
      if Assigned(a1) then FOSDColor.StartColor := a1.IntValue;
      a1 := n2.Properties.ItemNamed['endcolor'];
      if Assigned(a1) then FOSDColor.EndColor := a1.IntValue;
      a1 := n2.Properties.ItemNamed['fontcolor'];
      if Assigned(a1) then FOSDColor.FontColor := a1.IntValue;
      a1 := n2.Properties.ItemNamed['fontshadowcolor'];
      if Assigned(a1) then FOSDColor.FontShadowColor := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Enabled: ' + inttostr(integer(FOSDShow)));
    Log(FLog, Self, 'LoadFromFile', 'Alpha: ' + inttostr(FOSDAlpha));
    Log(FLog, Self, 'LoadFromFile', 'Duration: ' + inttostr(FOSDDuration));
    Log(FLog, Self, 'LoadFromFile', 'Offset: ' + inttostr(FOSDOffset));
    Log(FLog, Self, 'LoadFromFile', 'StartColor: 0x' + inttohex(FOSDColor.StartColor, 8));
    Log(FLog, Self, 'LoadFromFile', 'EndColor: 0x' + inttohex(FOSDColor.EndColor, 8));
    Log(FLog, Self, 'LoadFromFile', 'FontColor: 0x' + inttohex(FOSDColor.FontColor, 8));
    Log(FLog, Self, 'LoadFromFile', 'FontShadowColor: 0x' + inttohex(FOSDColor.FontShadowColor, 8));

      // Subtitle
    Log(FLog, Self, 'LoadFromFile', 'loading XML "subtitle"');
    n2 := n1.Items.ItemNamed['subtitle'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['alpha'];
      if Assigned(a1) then FSubtitleAlpha := a1.IntValue;
      a1 := n2.Properties.ItemNamed['offset'];
      if Assigned(a1) then FSubtitleOffset := a1.IntValue;
    end;
    Log(FLog, Self, 'LoadFromFile', 'Alpha: ' + inttostr(FSubtitleAlpha));
    Log(FLog, Self, 'LoadFromFile', 'Offset: ' + inttostr(FSubtitleOffset));

    // Debug
    Log(FLog, Self, 'LoadFromFile', 'loading XML "debug"');
    n2 := n1.Items.ItemNamed['debug'];
    if Assigned(n2) then
    begin
      a1 := n2.Properties.ItemNamed['disableosd'];
      if Assigned(a1) then FDisableOSD := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['directtuning'];
      if Assigned(a1) then FDirectTuning := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['disableepg'];
      if Assigned(a1) then FDisableEPG := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['disabledsmcc'];
      if Assigned(a1) then FDisableDSMCC := Boolean(a1.IntValue);
      a1 := n2.Properties.ItemNamed['packetsize'];
      if Assigned(a1) then FPacketSize := a1.IntValue;
      a1 := n2.Properties.ItemNamed['stopselect'];
      if Assigned(a1) then FStopSelect := Boolean(a1.IntValue);
    end;
    Log(FLog, Self, 'LoadFromFile', 'Disable OSD Filter: ' + inttostr(integer(FDisableOSD)));
    Log(FLog, Self, 'LoadFromFile', 'Direct Tuning: ' + inttostr(integer(FDirectTuning)));
    Log(FLog, Self, 'LoadFromFile', 'Disable EPG: ' + inttostr(integer(FDisableEPG)));
    Log(FLog, Self, 'LoadFromFile', 'Disable DSMCC: ' + inttostr(integer(FDisableDSMCC)));

    Log(FLog, Self, 'LoadFromFile', 'loading XML "channels"');
    n1 := xml.root.Items.ItemNamed['channels'];
    if Assigned(n1) then
    begin
      Log(FLog, Self, 'LoadFromFile', 'Number of Channels: ' + inttostr(n1.Items.Count));
      for i := 0 to n1.Items.Count -1 do
      begin
        Log(FLog, Self, 'LoadFromFile', 'Channel ' + inttostr(i));
        n2 := n1.Items[i];
        chan := TDVBChannel.Create;
        a1 := n2.Properties.ItemNamed['name'];
        if Assigned(a1) then chan.Name := a1.Value;
        chan.OriginalName := chan.Name;
        a1 := n2.Properties.ItemNamed['provider'];
        if Assigned(a1) then chan.Provider := a1.Value;
        a1 := n2.Properties.ItemNamed['sid'];
        if Assigned(a1) then chan.SID := a1.IntValue;
        a1 := n2.Properties.ItemNamed['pcrpid'];
        if Assigned(a1) then chan.PCRPID := a1.IntValue;
        a1 := n2.Properties.ItemNamed['pmpid'];
        if Assigned(a1) then chan.ProgramMapPID := a1.IntValue;
        a1 := n2.Properties.ItemNamed['st'];
        if Assigned(a1) then chan.ServiceType := a1.IntValue;

        Log(FLog, Self, 'LoadFromFile', 'Name: ' + chan.Name);
        Log(FLog, Self, 'LoadFromFile', 'Provider: ' + chan.Provider);
        Log(FLog, Self, 'LoadFromFile', 'SID: ' + inttostr(chan.SID));
        Log(FLog, Self, 'LoadFromFile', 'PCRPID: ' + inttostr(chan.PCRPID));
        Log(FLog, Self, 'LoadFromFile', 'ProgramMapPID: ' + inttostr(chan.ProgramMapPID));
        Log(FLog, Self, 'LoadFromFile', 'ServiceType: ' + inttostr(chan.ServiceType));

        n3 := n2.Items.ItemNamed['network'];
        if Assigned(n3) then
        begin
          a1 := n3.Properties.ItemNamed['name'];
          if Assigned(a1) then chan.Network.Name := a1.Value;
          a1 := n3.Properties.ItemNamed['type'];
          if Assigned(a1) then chan.Network.Type_ := TDVBNetworkType(a1.IntValue);
          a1 := n3.Properties.ItemNamed['nid'];
          if Assigned(a1) then chan.Network.NetworkID := a1.IntValue;
          a1 := n3.Properties.ItemNamed['onid'];
          if Assigned(a1) then chan.Network.ONID := a1.IntValue;
          a1 := n3.Properties.ItemNamed['tsid'];
          if Assigned(a1) then chan.Network.TSID := a1.IntValue;

          Log(FLog, Self, 'LoadFromFile', 'NetworkName: ' + chan.Network.Name);
          Log(FLog, Self, 'LoadFromFile', 'NetworkType: ' + inttostr(Integer(chan.Network.Type_)));
          Log(FLog, Self, 'LoadFromFile', 'NetworkID: ' + inttostr(chan.Network.NetworkID));
          Log(FLog, Self, 'LoadFromFile', 'ONID: ' + inttostr(chan.Network.ONID));
          Log(FLog, Self, 'LoadFromFile', 'TSID: ' + inttostr(chan.Network.TSID));

          case chan.Network.Type_ of
            ntDVBT:
            begin
              a1 := n3.Properties.ItemNamed['cf'];
              if Assigned(a1) then chan.Network.Terrestrial.CentreFrequency := a1.IntValue;
              a1 := n3.Properties.ItemNamed['bw'];
              if Assigned(a1) then chan.Network.Terrestrial.Bandwidth := a1.IntValue;
              a1 := n3.Properties.ItemNamed['prio'];
              if Assigned(a1) then chan.Network.Terrestrial.Priority := a1.IntValue;
              a1 := n3.Properties.ItemNamed['tsi'];
              if Assigned(a1) then chan.Network.Terrestrial.TimeSlicingIndicator := a1.IntValue;
              a1 := n3.Properties.ItemNamed['fec'];
              if Assigned(a1) then chan.Network.Terrestrial.MPEFECIndicator := a1.IntValue;
              a1 := n3.Properties.ItemNamed['con'];
              if Assigned(a1) then chan.Network.Terrestrial.Constellation := TTerrestrialConstellation(a1.IntValue);
              a1 := n3.Properties.ItemNamed['hi'];
              if Assigned(a1) then chan.Network.Terrestrial.HierarchyInformation := TTerrestrialHierarchyInformation(a1.IntValue);
              a1 := n3.Properties.ItemNamed['crhp'];
              if Assigned(a1) then chan.Network.Terrestrial.CodeRateHPStream := TTerrestrialCodeRate(a1.IntValue);
              a1 := n3.Properties.ItemNamed['crlp'];
              if Assigned(a1) then chan.Network.Terrestrial.CodeRateLPStream := TTerrestrialCodeRate(a1.IntValue);
              a1 := n3.Properties.ItemNamed['gi'];
              if Assigned(a1) then chan.Network.Terrestrial.GuardInterval := TTerrestrialGuardInterval(a1.IntValue);
              a1 := n3.Properties.ItemNamed['tm'];
              if Assigned(a1) then chan.Network.Terrestrial.TransmissionMode := TTerrestrialTransmissionMode(a1.IntValue);
              a1 := n3.Properties.ItemNamed['off'];
              if Assigned(a1) then chan.Network.Terrestrial.OtherFrequencyFlag := a1.IntValue;

              Log(FLog, Self, 'LoadFromFile', 'CentreFrequency: ' + inttostr(chan.Network.Terrestrial.CentreFrequency));
              Log(FLog, Self, 'LoadFromFile', 'Bandwidth: ' + inttostr(chan.Network.Terrestrial.Bandwidth));
              Log(FLog, Self, 'LoadFromFile', 'Priority: ' + inttostr(chan.Network.Terrestrial.Priority));
              Log(FLog, Self, 'LoadFromFile', 'TimeSlicingIndicator: ' + inttostr(chan.Network.Terrestrial.TimeSlicingIndicator));
              Log(FLog, Self, 'LoadFromFile', 'MPEFECIndicator: ' + inttostr(chan.Network.Terrestrial.MPEFECIndicator));
              Log(FLog, Self, 'LoadFromFile', 'Constellation: ' + inttostr(integer(chan.Network.Terrestrial.Constellation)));
              Log(FLog, Self, 'LoadFromFile', 'HierarchyInformation: ' + inttostr(integer(chan.Network.Terrestrial.HierarchyInformation)));
              Log(FLog, Self, 'LoadFromFile', 'CodeRateHPStream: ' + inttostr(integer(chan.Network.Terrestrial.CodeRateHPStream)));
              Log(FLog, Self, 'LoadFromFile', 'CodeRateLPStream: ' + inttostr(integer(chan.Network.Terrestrial.CodeRateLPStream)));
              Log(FLog, Self, 'LoadFromFile', 'GuardInterval: ' + inttostr(integer(chan.Network.Terrestrial.GuardInterval)));
              Log(FLog, Self, 'LoadFromFile', 'TransmissionMode: ' + inttostr(integer(chan.Network.Terrestrial.TransmissionMode)));
              Log(FLog, Self, 'LoadFromFile', 'OtherFrequencyFlag: ' + inttostr(chan.Network.Terrestrial.OtherFrequencyFlag));
            end;
            ntDVBS:
            begin
              a1 := n3.Properties.ItemNamed['f'];
              if Assigned(a1) then chan.Network.Satellite.Frequency := a1.IntValue;
              a1 := n3.Properties.ItemNamed['op'];
              if Assigned(a1) then chan.Network.Satellite.OrbitalPosition := a1.IntValue;
              a1 := n3.Properties.ItemNamed['wef'];
              if Assigned(a1) then chan.Network.Satellite.WestEastFlag := a1.IntValue;
              a1 := n3.Properties.ItemNamed['p'];
              if Assigned(a1) then chan.Network.Satellite.Polarization := TSatellitePolarization(a1.IntValue);
              a1 := n3.Properties.ItemNamed['mod'];
              if Assigned(a1) then chan.Network.Satellite.Modulation := TSatelliteModulation(a1.IntValue);
              a1 := n3.Properties.ItemNamed['sr'];
              if Assigned(a1) then chan.Network.Satellite.SymbolRate := a1.IntValue;
              a1 := n3.Properties.ItemNamed['feci'];
              if Assigned(a1) then chan.Network.Satellite.FECInner := TFECInner(a1.IntValue);

              Log(FLog, Self, 'LoadFromFile', 'requency: ' + inttostr(chan.Network.Satellite.Frequency));
              Log(FLog, Self, 'LoadFromFile', 'OrbitalPosition: ' + inttostr(chan.Network.Satellite.OrbitalPosition));
              Log(FLog, Self, 'LoadFromFile', 'WestEastFlag: ' + inttostr(chan.Network.Satellite.WestEastFlag));
              Log(FLog, Self, 'LoadFromFile', 'Polarization: ' + inttostr(integer(chan.Network.Satellite.Polarization)));
              Log(FLog, Self, 'LoadFromFile', 'Modulation: ' + inttostr(integer(chan.Network.Satellite.Modulation)));
              Log(FLog, Self, 'LoadFromFile', 'SymbolRate: ' + inttostr(chan.Network.Satellite.SymbolRate));
              Log(FLog, Self, 'LoadFromFile', 'FECInner: ' + inttostr(integer(chan.Network.Satellite.FECInner)));
            end;
            ntDVBC:
            begin
              a1 := n3.Properties.ItemNamed['f'];
              if Assigned(a1) then chan.Network.Cable.Frequency := a1.IntValue;
              a1 := n3.Properties.ItemNamed['feco'];
              if Assigned(a1) then chan.Network.Cable.FECOuter := TFECOuter(a1.IntValue);
              a1 := n3.Properties.ItemNamed['mod'];
              if Assigned(a1) then chan.Network.Cable.Modulation := TCableModulation(a1.IntValue);
              a1 := n3.Properties.ItemNamed['sr'];
              if Assigned(a1) then chan.Network.Cable.SymbolRate := a1.IntValue;
              a1 := n3.Properties.ItemNamed['feci'];
              if Assigned(a1) then chan.Network.Cable.FECInner := TFECInner(a1.IntValue);

              Log(FLog, Self, 'LoadFromFile', 'CentreFrequency: ' + inttostr(chan.Network.Cable.Frequency));
              Log(FLog, Self, 'LoadFromFile', 'FECOuter: ' + inttostr(integer(chan.Network.Cable.FECOuter)));
              Log(FLog, Self, 'LoadFromFile', 'Modulation: ' + inttostr(integer(chan.Network.Cable.Modulation)));
              Log(FLog, Self, 'LoadFromFile', 'SymbolRate: ' + inttostr(chan.Network.Cable.SymbolRate));
              Log(FLog, Self, 'LoadFromFile', 'FECInner: ' + inttostr(integer(chan.Network.Cable.FECInner)));
            end;
            ntATSC:
            begin
              // TODO
            end;
          end;
        end;

        n3 := n2.Items.ItemNamed['streams'];
        if Assigned(n3) then
        begin
          Log(FLog, Self, 'LoadFromFile', 'Number of Streams: ' + inttostr(n3.Items.Count));
          for c := 0 to n3.Items.Count -1 do
          begin
            Log(FLog, Self, 'LoadFromFile', 'Stream: ' + inttostr(c));
            n4 := n3.Items[c];
            a1 := n4.Properties.ItemNamed['type'];
            if not Assigned(a1)
              then Continue;

            case TDVBChannelStreamType(a1.IntValue) of
              stAudio:    stream := TDVBAudioStream.Create;
              stTeletext: stream := TDVBTeletextStream.Create;
              stSubtitle: stream := TDVBSubtitleStream.Create;
              stVideo:    stream := TDVBVideoStream.Create;
              else        stream := TDVBBaseStream.Create;
            end;
            stream.StreamType := TDVBChannelStreamType(a1.IntValue);
            Log(FLog, Self, 'LoadFromFile', 'Type: ' + inttostr(integer(stream.StreamType)));

            a1 := n4.Properties.ItemNamed['name'];
            if Assigned(a1) then stream.Name := a1.Value;
            a1 := n4.Properties.ItemNamed['default'];
            if Assigned(a1) then stream.Default := Boolean(a1.IntValue);
            a1 := n4.Properties.ItemNamed['pid'];
            if Assigned(a1) then stream.PID := a1.IntValue;
            a1 := n4.Properties.ItemNamed['tag'];
            if Assigned(a1) then stream.Tag := a1.IntValue;

            Log(FLog, Self, 'LoadFromFile', 'Name: ' + stream.Name);
            Log(FLog, Self, 'LoadFromFile', 'Default: ' + inttostr(integer(stream.Default)));
            Log(FLog, Self, 'LoadFromFile', 'PID: ' + inttostr(stream.PID));
            Log(FLog, Self, 'LoadFromFile', 'Tag: ' + inttostr(stream.Tag));

            case stream.StreamType of
              stAudio:
              begin
                with TDVBAudioStream(stream) do
                begin
                  a1 := n4.Properties.ItemNamed['lng'];
                  if Assigned(a1) then
                  begin
                    v := a1.Value;
                    Language := PISO6392LanguageCode(PChar(v))^;
                  end;
                  a1 := n4.Properties.ItemNamed['code'];
                  if Assigned(a1) then Coding := TDVBAudioStreamCoding(a1.IntValue);
                  a1 := n4.Properties.ItemNamed['at'];
                  if Assigned(a1) then AudioType := TISO639LanguageDescriptorAudioType(a1.IntValue);
                end;
              end;
              stVideo:
              begin
                with TDVBVideoStream(stream) do
                begin
                  a1 := n4.Properties.ItemNamed['code'];
                  if Assigned(a1) then Coding := TDVBVideoStreamCoding(a1.IntValue);
                  Log(FLog, Self, 'LoadFromFile', 'TDVBVideoStreamCoding: ' + inttostr(Integer(Coding)));
                end;
              end;
              stTeletext:
              begin
                with TDVBTeletextStream(stream) do
                begin
                  a1 := n4.Properties.ItemNamed['lng'];
                  if Assigned(a1) then
                  begin
                    v := a1.Value;
                    Language := PISO6392LanguageCode(PChar(v))^;
                  end;
                end;
              end;
              stSubtitle:
              begin
                with TDVBSubtitleStream(stream) do
                begin
                  for k := 0 to n4.Items.Count -1 do
                  begin
                    n5 := n4.Items[k];
                    a1 := n5.Properties.ItemNamed['lng'];
                    a2 := n5.Properties.ItemNamed['st'];
                    a3 := n5.Properties.ItemNamed['cp'];
                    a4 := n5.Properties.ItemNamed['ap'];
                    a5 := n5.Properties.ItemNamed['default'];
                    if Assigned(a1) and Assigned(a2) and Assigned(a3) and Assigned(a4) and Assigned(a5) then
                    begin
                      AddItem(a1.Value, a2.IntValue, a3.IntValue, a4.IntValue, Boolean(a5.IntValue));
                    end;
                  end;
                end;
              end;
            end;
            chan.Streams.Add(stream);
          end;
        end;
        FChannels.Add(chan);
      end;
    end else
    begin
      Log(FLog, Self, 'LoadFromFile', 'No Channels found');
    end;

    n1 := xml.root.Items.ItemNamed['recordings'];
    if Assigned(n1) then
    begin
      for i := 0 to n1.Items.Count -1 do
      begin
        n2 := n1.Items[i];
        rec.ID := StringToGUID(n2.Properties.Value('id'));
        rec.StartTime := GetDate(StrToFloat(n2.Properties.Value('start')));
        rec.EndTime := GetDate(StrToFloat(n2.Properties.Value('end')));
        rec.Name := n2.Properties.Value('name');
        rec.Location := n2.Properties.Value('location');
        rec.ChannelIndex := n2.Properties.IntValue('channel');
        FRecordings.put_Recording(@rec);
      end;
    end;
  finally
    xml.Free;
  end;

  Result := True;
  SetupStreamBuffer;
end;

procedure TDVBSettings.SetupStreamBuffer;
var
  cfg: IStreamBufferConfigure;
  init: IStreamBufferInitialize;
  key: HKEY;
  min_files, max_files, duration: Cardinal;
begin
  Log(FLog, Self, 'SetupStreamBuffer', 'Setting up TimeShifting (Stream Buffer Engine)');

  CoCreateInstance(CLSID_StreamBufferConfig, nil, CLSCTX_INPROC_SERVER, IID_IStreamBufferConfigure, cfg);
  if not Assigned(cfg) then
  begin
    Log(FLog, Self, 'SetupStreamBuffer', 'StreamBufferConfig = NULL');
    Exit;
  end;

  cfg.QueryInterface(IID_IStreamBufferInitialize, init);
  if not Assigned(init) then
  begin
    Log(FLog, Self, 'SetupStreamBuffer', 'IStreamBufferInitialize = NULL');
    cfg := nil;
    Exit;
  end;

  if RegCreateKey(HKEY_CURRENT_USER, 'SOFTWARE\DSP-worx\DVBSource\StreamBuffer', key) <> ERROR_SUCCESS then
  begin
    Log(FLog, Self, 'SetupStreamBuffer', 'RegCreateKey failed');
    cfg := nil;
    init := nil;
    Exit;
  end;

  init.SetHKEY(key);

  duration := 300;
  min_files := (FTimeShiftingDuration * 60) div Int64(duration);
  max_files := min_files + 2;

  cfg.SetBackingFileDuration(duration);
  cfg.SetBackingFileCount(min_files, max_files);
  Log(FLog, Self, 'SetupStreamBuffer', 'TimeShifting Duration: ' + inttostr(FTimeShiftingDuration));

  cfg := nil;
  init := nil;
end;

end.
