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

unit DVBAudioFilter;

interface

uses
  BaseClass, ActiveX, DirectShow9, Windows, DSUtil, MMSystem, DVBInterface,
  BDAConst, MPEGUtils, Classes, SysUtils, MPEGConst;

type
  TDVBAudioFilter = class(TBCTransInPlaceFilter, IAudioFilter)
  private
    FNextFlush: Boolean;
    FCurrentType: TAudioMediaType;
    FMediaType: TAudioMediaType;
    FAudioLock: TBCCritSec;
    FSBEEnabled: Boolean;
    FAudioBytesCallback: TAudioBytesCallback;
    FAudioInfoCallback: TAudioInfoCallback;
  public
    constructor Create;
    destructor Destroy; override;

    function Transform(Sample: IMediaSample): HRESULT; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    // IAudioFilter
    procedure put_MediaType(MediaType: TAudioMediaType);
    procedure put_SBEEnabled(AEnabled: LongBool);
    procedure put_AudioBytesCallback(ACallback: TAudioBytesCallback);
    procedure put_AudioInfoCallback(ACallback: TAudioInfoCallback);
    procedure put_FlushStream(AFlush: Boolean);
  end;

implementation

(*** TDVBAudioFilter **********************************************************)

constructor TDVBAudioFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create('DC-DVB Audio Filter', nil, CLSID_AudioFilter, hr);
  FCurrentType := mtUnknown;
  FMediaType := mtUnknown;
  FSBEEnabled := False;
  FAudioLock := TBCCritSec.Create;
end;

destructor TDVBAudioFilter.Destroy;
begin
  FAudioLock.Free;
  inherited Destroy;
end;

function TDVBAudioFilter.CheckInputType(mtin: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

const
  MPEG_AUDIO_BITRATE: array[1..3] of array[1..3] of array[0..15] of Integer =
  (
    (
      (0,32,64,96,128,160,192,224,256,288,320,352,384,416,448,0),
      (0,32,48,56, 64, 80, 96,112,128,160,192,224,256,320,384,0),
      (0,32,40,48, 56, 64, 80, 96,112,128,160,192,224,256,320,0)
    ),
    (
      (0,32,48, 56, 64, 80, 96,112,128,144,160,176,192,224,256,0),
      (0, 8,16,24, 32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0),
      (0, 8,16,24, 32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0)
    ),
    (
      (0,32,48, 56, 64, 80, 96,112,128,144,160,176,192,224,256,0),
      (0, 8,16,24, 32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0),
      (0, 8,16,24, 32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0)
    )
  );

  MPEG_AUDIO_SAMPLERATE: array[0..3] of array[0..3] of Integer =
  (
    (11025, 12000, 8000, 0),
    (0, 0, 0, 0),
    (22050, 24000, 16000, 0),
    (44100, 48000, 32000, 0)
  );

function GetMPEGAudioBitrate(AVersion: Integer; ALayer: Integer; ABitrate: Integer): Integer;
begin
  Result := MPEG_AUDIO_BITRATE[AVersion][ALayer][ABitrate];
end;

function GetMPEGAudioSamplerate(AVersion: Integer; ASamplerate: Integer): Integer;
begin
  Result := MPEG_AUDIO_SAMPLERATE[AVersion][ASamplerate];
end;

function GetMPEGAudioChannels(AChannels: Integer): Integer;
begin
  case AChannels of
    0:    Result := 2;
    1:    Result := 2;
    2:    Result := 2;
    3:    Result := 1;
    else  Result := 0;
  end;
end;

function GetAC3AudioSamplerate(ASamplerate: Integer): Integer;
begin
  case ASamplerate of
    0:    Result := 48000;
    1:    Result := 44100;
    2:    Result := 32000;
    else  Result := 0;
  end;
end;

function GetAC3AudioBitrate(ABitrate: Integer): Integer;
begin
  case ABitrate of
    0, 1: Result := 32000;
    2, 3: Result := 40000;
    4, 5: Result := 48000;
    6, 7: Result := 56000;
    8, 9: Result := 64000;
    10, 11: Result := 80000;
    12, 13: Result := 96000;
    14, 15: Result := 112000;
    16, 17: Result := 128000;
    18, 19: Result := 160000;
    20, 21: Result := 192000;
    22, 23: Result := 224000;
    24, 25: Result := 256000;
    26, 27: Result := 320000;
    28, 29: Result := 384000;
    30, 31: Result := 448000;
    32, 33: Result := 512000;
    34, 35: Result := 576000;
    36, 37: Result := 640000;
    else    Result := 0;
  end;
end;

function GetAC3AudioChannels(AChannels: Integer): Integer;
begin
  case AChannels of
    0: Result := 2;
    1: Result := 1;
    2: Result := 2;
    3: Result := 3;
    4: Result := 3;
    5: Result := 4;
    6: Result := 4;
    7: Result := 5;
    else Result := 0;
  end
end;

function TDVBAudioFilter.Transform(Sample: IMediaSample): HRESULT;
const
  WAVE_FORMAT_DOLBY_AC3 = $2000;
var
  mt: TAMMediaType;
  inbuf: PByteArray;
  size: Int64;
  version: Integer;
  layer: Integer;
  bitrate: Integer;
  samplerate: Integer;
  channels: Integer;
  lfeon: Integer;
  acmod: Integer;
  bp: Integer;
//  t1, t2: Int64;
begin
  Sample.GetPointer(pbyte(inbuf));
  size := Sample.GetActualDataLength;
  Result := S_OK;

  FAudioLock.Lock;
  try
    if FNextFlush then
    begin
      Sample.SetDiscontinuity(True);
      FNextFlush := False;
      FOutput.DeliverBeginFlush;
      FOutput.DeliverEndFlush;
    end;

    if Assigned(FAudioBytesCallback)
      then FAudioBytesCallback(size);

//  Sample.GetTime(t1, t2);
//  OutputDebugString(pchar(inttostr(t1) + ' - ' + inttostr(t2)));
  // TODO: Needs a better way to detect a change. Can produce garbage with
  // certain Decoders like PowerDVD's.
    if FSBEEnabled then
    begin
  //    OutputDebugString(PChar('Audio Check'));
      if (inbuf[0] = $FF) and (inbuf[1] and $E0 = $E0) then // MPEG 1/2 Header
      begin
        FMediaType := mtMP2;
      end else
      if (inbuf[0] = $0B) and (inbuf[1] = $77) then // AC3 Sync Byte
      begin
        FMediaType := mtAC3;
      end;
    end;

//  OutputDebugString(PChar(inttohex(inbuf[0], 2) + inttohex(inbuf[1], 2)));

    if FCurrentType <> FMediaType then
    begin
      FCurrentType := FMediaType;

      mt.majortype := KSDATAFORMAT_TYPE_AUDIO;
      mt.formattype := KSDATAFORMAT_SPECIFIER_WAVEFORMATEX;
      mt.bFixedSizeSamples := True;
      mt.bTemporalCompression := False;
      mt.lSampleSize := 65536;
      mt.pUnk := nil;
      mt.cbFormat := SizeOf(TWaveFormatExtensible);;
      mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);
      ZeroMemory(mt.pbFormat, mt.cbFormat);

      case FCurrentType of
        mtMP2:
        begin
//          OutputDebugString('Changing Mediatype to MP2');
          mt.subtype := KSDATAFORMAT_SUBTYPE_MPEG2_AUDIO;
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
        mtAC3:
        begin
//          OutputDebugString('Changing Mediatype to AC3');
          mt.subtype := KSDATAFORMAT_SUBTYPE_AC3_AUDIO;
          with PWaveFormatEx(mt.pbFormat)^ do
          begin
            wFormatTag := WAVE_FORMAT_DOLBY_AC3;
            nChannels := 2;
            nSamplesPerSec := 48000;
          end;
        end;
      end;

      Sample.SetDiscontinuity(True);
      Sample.SetMediaType(@mt);
      FreeMediaType(@mt);
    end;

    Sample.GetPointer(pbyte(inbuf));
    size := Sample.GetActualDataLength;
    case FCurrentType of
      mtMP2:
      begin
        while (size > 4) do
        begin
          if (inbuf[0] = $FF) and (inbuf[1] and $E0 = $E0) then
          begin
            version := (inbuf[1] and $18) shr 3;
            layer := (inbuf[1] and $6) shr 1;
            bitrate := (inbuf[2] and $F0) shr 4;
            samplerate := (inbuf[2] and $C) shr 2;
            channels := (inbuf[3] and $3F) shr 6;
            if (version <> 1) and (layer <> 0) and (bitrate <> 0) and (bitrate <> 15) and (samplerate <> 3) then
            begin
              bitrate := GetMPEGAudioBitrate(version, layer, bitrate);
              samplerate := GetMPEGAudioSamplerate(version, samplerate);
              channels := GetMPEGAudioChannels(channels);
              if Assigned(FAudioInfoCallback)
                then FAudioInfoCallback(channels, bitrate, samplerate);
              Exit;
            end;
          end;
          dec(size);
          inc(PByte(inbuf));
        end;
      end;
      mtAC3:
      begin
        while (size > 10) do
        begin
          if (inbuf[0] = $0B) and (inbuf[1] = $77) then
          begin
            samplerate := (inbuf[4] and $C0) shr 6;
            bitrate := (inbuf[4] and $3F);
            bitrate := GetAC3AudioBitrate(bitrate);
            acmod := (inbuf[6] and $E0) shr 5;
            channels := GetAC3AudioChannels(acmod);

            bp := 2;
            if (acmod and $1 > 0) and (acmod <> 1)
              then inc(bp, 2);
            if (acmod and $4 > 0)
              then inc(bp, 2);
            if (acmod = $2)
              then inc(bp, 2);

            lfeon := GetWordBits(@inbuf[6], bp, 2);
            if (samplerate <> 3) and (bitrate <> 0) then
            begin
              samplerate := GetAC3AudioSamplerate(samplerate);
              if Assigned(FAudioInfoCallback)
                then FAudioInfoCallback(channels + lfeon, bitrate div 1000, samplerate);
              Exit;
            end;
          end;
          dec(size);
          inc(PByte(inbuf));
        end;
      end;
    end;
  finally
    FAudioLock.UnLock;
  end;
end;

(*** IAudioFilter *************************************************************)

procedure TDVBAudioFilter.put_MediaType(MediaType: TAudioMediaType);
begin
  FAudioLock.Lock;
  try
    FMediaType := MediaType;
  finally
    FAudioLock.UnLock;
  end;
end;

procedure TDVBAudioFilter.put_SBEEnabled(AEnabled: LongBool);
begin
  FAudioLock.Lock;
  try
    FSBEEnabled := AEnabled;
  finally
    FAudioLock.UnLock;
  end;
end;

procedure TDVBAudioFilter.put_AudioBytesCallback(ACallback: TAudioBytesCallback);
begin
  FAudioLock.Lock;
  try
    FAudioBytesCallback := ACallback;
  finally
    FAudioLock.UnLock;
  end;
end;

procedure TDVBAudioFilter.put_AudioInfoCallback(ACallback: TAudioInfoCallback);
begin
  FAudioLock.Lock;
  try
    FAudioInfoCallback := ACallback;
  finally
    FAudioLock.UnLock;
  end;
end;

procedure TDVBAudioFilter.put_FlushStream(AFlush: Boolean);
begin
  FAudioLock.Lock;
  try
    FNextFlush := AFlush;
  finally
    FAudioLock.UnLock;
  end;
end;

end.
