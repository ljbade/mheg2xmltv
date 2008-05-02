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

unit DVBRecordings;

interface

uses
  Classes, BaseClass, ActiveX, Windows, DirectShow9, SysUtils, DVBChannelList,
  MPEGConst, MPEGUtils, DSUtil, BDAUtils, IDVBSource, MMSystem, BDAConst;

const
  CLSID_DVBStreamSource: TGuid = '{A89B83F0-0240-4094-B5E8-9B77536AA5A6}';

type
  TDVBRecordingPin = class(TBCBaseOutputPin)
  protected
    procedure OnReadData(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString);
    destructor Destroy; override;

    function CheckMediaType(mt: PAMMediaType): HRESULT; override;

    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;
  end;

  TDVBRecordingSource = class(TBCBaseFilter)
  private
    FPin: TDVBRecordingPin;
  public
    constructor Create;
    destructor Destroy; override;

    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: Integer; override;
  end;

  TDVBRecordings = class;

  TDVBRecording = class
  private
    FOwner: TDVBRecordings;
    FSettings: TDVBRecordingSetting;

    FGraph: IFilterGraph2;
    FSource: TDVBRecordingSource;
    FDemux: IBaseFilter;
    FSink: IBaseFilter;
    FRecorder: IUnknown;
    FROT: Integer;

    procedure StartRecording;
    procedure StopRecording;
    procedure CreateStream;
  public
    constructor Create(AOwner: TDVBRecordings);
    destructor Destroy; override;

    procedure ParseBuffer(ABuffer: PByte; ASize: Integer);
  end;

  TDVBRecordings = class
  private
    FOwner: Pointer;
    FLock: TBCCritSec;
    FList: TList;
  public
    constructor Create(AOwner: Pointer);
    destructor Destroy; override;

    procedure ParseData(ABuffer: PByte; ASize: Integer);

    function get_RecordingsCount(out Count: Integer): HRESULT;
    function get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT;
    function put_Recording(Recording: PDVBRecordingSetting): HRESULT;
    function put_EditRecording(Recording: PDVBRecordingSetting): HRESULT;
    function put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT;
  end;

implementation

uses
  DVBSettings;

(*** TDVBRecordingPin *********************************************************)

constructor TDVBRecordingPin.Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString);
begin
  inherited Create('Stream Source Output Pin', Filter, TBCCritSec.Create, hr, 'Out');
end;

destructor TDVBRecordingPin.Destroy;
begin
  inherited Destroy;;
end;

function TDVBRecordingPin.GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult;
begin
  if position < 0 then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if position > 0 then
  begin
    Result := VFW_S_NO_MORE_ITEMS;
    Exit;
  end;

  if (@MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  MediaType.majortype := MEDIATYPE_Stream;
  MediaType.subtype := KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT;
  MediaType.formattype := FORMAT_None;
  MediaType.bTemporalCompression := False;
  MediaType.bFixedSizeSamples := True;

  Result := S_OK;
end;

function TDVBRecordingPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

function TDVBRecordingPin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
var
  Actual: ALLOCATOR_PROPERTIES;
begin
  if (Allocator = nil) or (Properties = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    if (Properties.cBuffers = 0)
      then Properties.cBuffers := 1;

    Properties.cbBuffer := 200000;

    Result := Allocator.SetProperties(Properties^, Actual);
    if Failed(Result)
      then Exit;

    if (Actual.cbBuffer < Properties.cbBuffer)
      then Result := E_FAIL
      else Result := S_OK;

  finally
    FLock.UnLock;
  end;
end;

procedure TDVBRecordingPin.OnReadData(ABuffer: PByte; ASize: Integer);
var
  sample: IMediaSample;
  buffer: PByte;
  hr: HRESULT;
begin
  FLock.Lock;
  try
    if IsConnected and (FFilter.State = State_Running) then
    begin
      hr := Self.GetDeliveryBuffer(sample, nil, nil, 0);
      if hr = S_OK then
      begin
        sample.GetPointer(buffer);
        sample.SetActualDataLength(ASize);
        Move(ABuffer^, buffer^, ASize);
        FInputPin.Receive(sample);
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

(*** TDVBRecordingSource ******************************************************)

constructor TDVBRecordingSource.Create;
var
  hr: HRESULT;
begin
  inherited Create('DVB Stream Source', nil, TBCCritSec.Create, CLSID_DVBStreamSource);

  FPin := TDVBRecordingPin.Create('Stream Source Pin', Self, TBCCritSec.Create, hr, 'Stream Source Pin');
  if (hr <> S_OK) and (FPin = nil)
    then hr := E_OUTOFMEMORY;
end;

destructor TDVBRecordingSource.Destroy;
begin
  FreeAndNil(FPin);
  inherited Destroy;
end;

function TDVBRecordingSource.GetPin(n: Integer): TBCBasePin;
begin
  case n of
    0: Result := FPin;
    else Result := nil;
  end;
end;

function TDVBRecordingSource.GetPinCount: Integer;
begin
  Result := 1;
end;

(*** TDVBRecording ************************************************************)

constructor TDVBRecording.Create(AOwner: TDVBRecordings);
begin
  inherited Create;
  FOwner := AOwner;
  CreateGUID(FSettings.ID);
  FSettings.Status := rsWaiting;
end;

destructor TDVBRecording.Destroy;
begin
  StopRecording;
  inherited Destroy;
end;

procedure TDVBRecording.CreateStream;
var
  str: String;
  ext: String;
  tmp: String;
  i: Integer;
  t: TReferenceTime;
begin
  str := FSettings.Location;
  ext := ExtractFileExt(str);
  Delete(str, Length(str) - Length(ext) + 1, Length(ext));
  tmp := '';
  i := 1;

  while (FileExists(str + tmp + ext)) do
  begin
    tmp := '_' + Format('%.3d', [i]);
    inc(i);
  end;

  with (FSink as IStreamBufferSink) do
  begin
    LockProfile(nil);
    CreateRecorder(StringToOleStr(str + tmp + ext), RECORDING_TYPE_CONTENT, FRecorder);
  end;

  t := 0;
  (FRecorder as IStreamBufferRecordControl).Start(@t);

  (FGraph as IMediaControl).Run;
end;

procedure TDVBRecording.StartRecording;
const
  WAVE_FORMAT_DOLBY_AC3 = $2000;
const
  VIDEO_PIN_PBFORMAT: array[0..53] of Cardinal = (
    $00000000, $00000000, $000002C0, $000001E0, $00000000, $00000000, $00000000, $00000000,
    $00B71B00, $00000000, $00028BB1, $00000000, $00000000, $00000000, $00000010, $00000009,
    $00000000, $00000000, $00000028, $000002C0, $000001E0, $00000000, $00000000, $00000000,
    $000007D0, $0000D842, $00000000, $00000000, $00C827C0, $0000004C, $FFFFFFFF, $FFFFFFFF,
    $00000000, $B3010000, $37E0012C, $81234C1D, $12111110, $13131212, $14141313, $15141414,
    $15151515, $16161615, $16161616, $17171717, $17171717, $19181818, $19181818, $1A1A1A1A,
    $1B1B1B19, $1C1C1B1B, $1E1E1C1C, $211F1F1E, $00000000, $00000000
  );
var
  i: Integer;
  chan: TDVBChannel;
  map: Cardinal;
  mt: TAMMediaType;
  pin: IPin;
  idx, c, k: Integer;
  video_mapped: Boolean;
  audio_mapped: Boolean;
begin
  if Assigned(FGraph)
    then Exit;

  video_mapped := False;
  audio_mapped := False;

  idx := 0;

  if CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraph) = S_OK then
  begin
    FSource := TDVBRecordingSource.Create;
    FGraph.AddFilter(FSource, StringToOleStr('Recording Source Filter'));
    FDemux := GetFilterFromSystem(CLSID_MPEG2Demultiplexer);
    FGraph.AddFilter(FDemux, StringToOleStr('MPEG-2 Demultiplexer'));
    FGraph.ConnectDirect(GetOutPin(FSource, 0), GetInPin(FDemux, 0), nil);
    FSink := GetFilterFromSystem(CLSID_StreamBufferSink);
    FGraph.AddFilter(FSink, StringToOleStr('Sink'));

    chan := TDVBSettings(FOwner.FOwner).Channels[FSettings.ChannelIndex];
    for i := 0 to chan.Streams.Count -1 do
    begin
      case chan.Streams[i].StreamType of
        stVideo:
        begin
          if not video_mapped then
          begin
            mt.majortype := KSDATAFORMAT_TYPE_VIDEO;
            mt.subtype := KSDATAFORMAT_SUBTYPE_MPEG2_VIDEO;
            mt.formattype := KSDATAFORMAT_SPECIFIER_MPEG2_VIDEO;
            mt.bFixedSizeSamples := True;
            mt.bTemporalCompression := False;
            mt.lSampleSize := 65536;
            mt.pUnk := nil;
            mt.cbFormat := 216;
            mt.pbFormat := CoTaskMemAlloc(216);
            CopyMemory(mt.pbFormat, @VIDEO_PIN_PBFORMAT, 216);
            (FDemux as IMpeg2Demultiplexer).CreateOutputPin(mt, StringToOleStr(inttostr(i+1)), pin);
            FreeMediaType(@mt);

            map := chan.Streams[i].PID;
            (pin as IMPEG2PIDMap).MapPID(1, @map, MEDIA_ELEMENTARY_STREAM);

            FGraph.ConnectDirect(pin, GetInPin(FSink, idx), nil);
            inc(idx);

            pin := nil;

            video_mapped := True;
          end;
        end;
      end;
    end;

    c := chan.DefaultAudioStreamIndex;
    k := 0;

    for i := 0 to chan.Streams.Count -1 do
    begin
      case chan.Streams[i].StreamType of
        stAudio:
        begin
          if not audio_mapped and (c = k) then
          begin
            if TDVBAudioStream(chan.Streams[i]).Coding = acAC3 then
            begin
              mt.majortype := KSDATAFORMAT_TYPE_AUDIO;
              mt.subtype := KSDATAFORMAT_SUBTYPE_AC3_AUDIO;
              mt.formattype := KSDATAFORMAT_SPECIFIER_WAVEFORMATEX;
              mt.bFixedSizeSamples := True;
              mt.bTemporalCompression := False;
              mt.lSampleSize := 65536;
              mt.pUnk := nil;
              mt.cbFormat := SizeOf(TWaveFormatExtensible);
              mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);
              ZeroMemory(mt.pbFormat, mt.cbFormat);
              with PWaveFormatEx(mt.pbFormat)^ do
              begin
                wFormatTag := WAVE_FORMAT_DOLBY_AC3;
                nChannels := 2;
                nSamplesPerSec := 48000;
              end;
            end else
            begin
              mt.majortype := KSDATAFORMAT_TYPE_AUDIO;
              mt.subtype := KSDATAFORMAT_SUBTYPE_MPEG2_AUDIO;
              mt.formattype := KSDATAFORMAT_SPECIFIER_WAVEFORMATEX;
              mt.bFixedSizeSamples := True;
              mt.bTemporalCompression := False;
              mt.lSampleSize := 65536;
              mt.pUnk := nil;
              mt.cbFormat := SizeOf(TWaveFormatExtensible);;
              mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);
              ZeroMemory(mt.pbFormat, mt.cbFormat);
              with PWaveFormatExtensible(mt.pbFormat)^ do
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
            end;
            (FDemux as IMpeg2Demultiplexer).CreateOutputPin(mt, StringToOleStr(inttostr(i+1)), pin);
            FreeMediaType(@mt);

            map := chan.Streams[i].PID;
            (pin as IMPEG2PIDMap).MapPID(1, @map, MEDIA_ELEMENTARY_STREAM);

            FGraph.ConnectDirect(pin, GetInPin(FSink, idx), nil);
            inc(idx);

            pin := nil;
            audio_mapped := True;
          end;
          inc(k);
        end;
      end;
    end;

    AddGraphToRot(FGraph, FROT);
    CreateStream;

//    (FGraph as IMediaControl).Run;
  end;

//  FStream := CreateStream;
//  if not Assigned(FStream) then
//  begin
//    FSettings.Status := rsFailed;
//    Exit;
//  end;

  FSettings.Status := rsRecording;
end;

procedure TDVBRecording.StopRecording;
var
  control: IMediaControl;
begin
  if Assigned(FGraph) then
  begin
    if FGraph.QueryInterface(IID_IMediaControl, control) = S_OK
      then control.Stop;
    RemoveGraphFromRot(FROT);
    control := nil;
    FGraph := nil;
  end;
  Pointer(FSource) := nil;
  FDemux := nil;

  FSettings.Status := rsStopped;

//  if Assigned(FStream) then
//  begin
//    FStream.Free;
//    FStream := nil;
//  end;
end;

procedure TDVBRecording.ParseBuffer(ABuffer: PByte; ASize: Integer);
begin
  if (FSettings.Status = rsWaiting) then
  begin
    if GetDate(FSettings.StartTime) <= Now then
    begin
      StartRecording;
    end;
  end;

  if (FSettings.Status = rsInvalid) or
     (FSettings.Status = rsFailed)
    then Exit;

  if (FSettings.Status = rsRecording) and Assigned(FSource) then
  begin
    FSource.FPin.OnReadData(ABuffer, ASize);

    if GetDate(FSettings.EndTime) <= Now then
    begin
      StopRecording;
    end;
  end;
end;

(*** TDVBRecordings ***********************************************************)

constructor TDVBRecordings.Create(AOwner: Pointer);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
  FLock := TBCCritSec.Create;
end;

destructor TDVBRecordings.Destroy;
begin
  while FList.Count > 0 do
  begin
    TDVBRecording(FList[0]).Free;
    FList.Delete(0);
  end;
  FList.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TDVBRecordings.ParseData(ABuffer: PByte; ASize: Integer);
var
  i: Integer;
begin
  if (ASize <= 0)
    then Exit;

  FLock.Lock;
  try
    for i := 0 to FList.Count -1
      do TDVBRecording(FList[i]).ParseBuffer(ABuffer, ASize);
  finally
    FLock.UnLock;
  end;
end;

function TDVBRecordings.get_RecordingsCount(out Count: Integer): HRESULT;
begin
  if not Assigned(@Count) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    Count := FList.Count;
    Result := S_OK;
  finally
    FLock.UnLock;
  end;
end;

function TDVBRecordings.get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT;
var
  rec: TDVBRecording;
begin
  if (@Recording = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    if (Index < 0) or (Index >= FList.Count) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    rec := FList[Index];
    Recording := @rec.FSettings;

    Result := S_OK;
  finally
    FLock.UnLock;
  end;
end;

function TDVBRecordings.put_Recording(Recording: PDVBRecordingSetting): HRESULT;
var
  recs: PDVBRecordingSetting;
  rec: TDVBRecording;
  start: TDateTime;
  end_: TDateTime;
begin
  Result := S_FALSE;
  recs := Recording;

  if Recording = nil then
  begin
    Result := E_POINTER;
    Exit;
  end;

  start := GetDate(recs.StartTime);
  end_ := GetDate(recs.EndTime);

  if start >= end_ then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if end_ <= Now then
  begin
    Result := S_FALSE;
    Exit;
  end;

  FLock.Lock;
  try
    rec := TDVBRecording.Create(Self);
    rec.FSettings.StartTime := recs.StartTime;
    rec.FSettings.EndTime := recs.EndTime;
    rec.FSettings.Name := recs.Name;
    rec.FSettings.Location := recs.Location;
    rec.FSettings.ChannelIndex := recs.ChannelIndex;
    recs.ID := rec.FSettings.ID;
    recs.Status := rsWaiting;
    FList.Add(rec);
  finally
    FLock.UnLock;
  end;
end;

function TDVBRecordings.put_EditRecording(Recording: PDVBRecordingSetting): HRESULT;
var
  recs: PDVBRecordingSetting;
  rec: TDVBRecording;
  i: Integer;
begin
  Result := S_FALSE;
  recs := Recording;

  if Recording = nil then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if GetDate(recs.StartTime) >= GetDate(recs.EndTime) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  FLock.Lock;
  try
    for i := 0 to FList.Count -1 do
    begin
      rec := FList[i];
      if IsEqualGUID(rec.FSettings.ID, recs.ID) then
      begin
        if rec.FSettings.Status = rsWaiting then
        begin
          rec.FSettings.StartTime := recs.StartTime;
          rec.FSettings.Location := recs.Location;
          rec.FSettings.ChannelIndex := recs.ChannelIndex;
        end;
        rec.FSettings.EndTime := recs.EndTime;
        rec.FSettings.Name := recs.Name;
        Result := S_OK;
        Exit;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBRecordings.put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT;
var
  rec: TDVBRecording;
  i: Integer;
begin
  Result := S_FALSE;

  if Recording = nil then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    for i := 0 to FList.Count -1 do
    begin
      rec := FList[i];
      if IsEqualGUID(rec.FSettings.ID, PDVBRecordingSetting(Recording)^.ID) then
      begin
        rec.Free;
        FList.Delete(i);
        Result := S_OK;
        Exit;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

end.
