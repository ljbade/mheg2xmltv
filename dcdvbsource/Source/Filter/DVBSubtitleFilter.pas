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

unit DVBSubtitleFilter;

interface

uses
  Windows, Classes, SysUtils, BaseClass, DirectShow9, ActiveX, DSUtil,
  DVBSubtitlingParser, IDVBSource;

const
  UNITS = 10000000;

  FPS_30  = UNITS div 30;
  FPS_25  = UNITS div 25;
  FPS_20  = UNITS div 20;
  FPS_10  = UNITS div 10;
  FPS_5   = UNITS div 5;
  FPS_4   = UNITS div 4;
  FPS_3   = UNITS div 3;
  FPS_2   = UNITS div 2;
  FPS_1   = UNITS div 1;

type
  TDVBSubOutputPin = class(TBCSourceStream)
  private
    FFrameLength: Integer;
    FSharedState: TBCCritSec;
    FSubtitleParser: TDVBSubtitlingParser;
    FFrameNumber: Int64;
    FNeedAspectRatioChange: Boolean;
    FAspectRatio: TAspectRatio;
    FFirstFill: Boolean;
  public
    constructor Create(const ObjectName: string; out hr: HRESULT; Filter: TBCSource; const Name: WideString);
    destructor Destroy; override;

    function GetMediaType(MediaType: PAMMediaType): HResult; override;
    function CheckMediaType(MediaType: PAMMediaType): HRESULT; override;

    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;
    function BeginFlush: HRESULT; override; stdcall;

    procedure OnPCRData(ABuffer: PByte; ASize: Integer);
    procedure OnSubtitleData(ABuffer: PByte; ASize: Integer);
    procedure put_PID(APCRPID: Integer; APID: Integer; ACompositionPageID: Integer; AAncillaryPageID: Integer);
    procedure put_AspectRatio(AAspectRatio: TAspectRatio);
  published
    property FrameNumber: Int64 read FFrameNumber write FFrameNumber;
  end;

implementation

uses Math;

(*** TDVBSubOutputPin *********************************************************)

constructor TDVBSubOutputPin.Create(const ObjectName: string; out hr: HRESULT; Filter: TBCSource; const Name: WideString);
begin
  inherited Create(ObjectName, hr, Filter, Name);
  FSharedState := TBCCritSec.Create;
  FSubtitleParser := TDVBSubtitlingParser.Create;
  FSubtitleParser.Enabled := True;

  FFrameLength := FPS_2;
  FFrameNumber := 0;
  put_AspectRatio(ar4_3);
  FFirstFill := True;
end;

destructor TDVBSubOutputPin.Destroy;
begin
  FreeAndNil(FSubtitleParser);
  FreeAndNil(FSharedState);
  inherited Destroy;
end;

function TDVBSubOutputPin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
var
  Actual: ALLOCATOR_PROPERTIES;
begin
  if (Allocator = nil) or (Properties = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FFilter.StateLock.Lock;
  try
    if (Properties.cBuffers = 0)
      then Properties.cBuffers := 1;

    Properties.cbBuffer := Abs(PVideoInfoHeader2(AMMediaType.pbFormat).bmiHeader.biSizeImage);

    Result := Allocator.SetProperties(Properties^, Actual);
    if Failed(Result)
      then Exit;

    if (Actual.cbBuffer < Properties.cbBuffer)
      then Result := E_FAIL
      else Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

function TDVBSubOutputPin.FillBuffer(Sample: IMediaSample): HResult;
var
  buffer: PByte;
  size: Integer;
  Start, Stop: REFERENCE_TIME;
  pmt: PAMMediaType;
  new_width: Integer;
  i: Integer;
  buf: PByte;
  w, w2: Integer;
begin
{$IFNDEF VER170} // Another Delphi <= 7 Problem...solved in Delphi 2005
//  Result := S_OK;
{$ENDIF}

  FSharedState.Lock;
  try
    if (Sample = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    if sample.GetMediaType(pmt) = S_OK then
    begin
      SetMediaType(pmt);
      DeleteMediaType(pmt);
    end;

    Start := FFrameNumber * FFrameLength;
    Stop := Start + FFrameLength;
    Sample.SetTime(@Start, @Stop);
    Inc(FFrameNumber);

    new_width := PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biWidth;
    size :=
      (Abs(PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biHeight) * new_width) shl 2;
    Sample.SetActualDataLength(size);
    Sample.GetPointer(PByte(buffer));

    if FNeedAspectRatioChange then
    begin
      case FAspectRatio of
        ar4_3:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            dwPictAspectRatioX := 4;
            dwPictAspectRatioY := 3;
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_4x3;
          end;
        end;
        ar16_9:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            dwPictAspectRatioX := 16;
            dwPictAspectRatioY := 9;
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
//            rcTarget := Rect(0, 0, DEFAULT_SPECS_WIDTH, DEFAULT_SPECS_WIDTH * 9 div 16);
          end;
        end;
        ar221_1:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            dwPictAspectRatioX := 221;
            dwPictAspectRatioY := 100;
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
//            rcTarget := Rect(0, 0, DEFAULT_SPECS_WIDTH, Round(DEFAULT_SPECS_WIDTH * 1 / 2.21));
          end;
        end;
      end;

      FNeedAspectRatioChange := False;
      Sample.SetMediaType(@Fmt);
      //Sample.SetDiscontinuity(True);
    end;

    if (FSubtitleParser.PID < 0) then
    begin
      if FFirstFill then
      begin
          // we need to pass one empty Buffer at the beginning otherwise the
        // first visible Teletext Frame is corrupted, so we get an ugly picture
        FillChar(buffer^, size, 0);
        FFirstFill := False;
      end else
      begin
        Sample.SetActualDataLength(0);
      end;
      Result := S_OK;
      Exit;
    end;

    if (FSubtitleParser.TimeOut >= 0) then
    begin
      FSubtitleParser.TimeOut := FSubtitleParser.TimeOut - (FFrameLength / 10000);
      if FSubtitleParser.TimeOut < 0 then
      begin
        FSubtitleParser.ErasePage;
      end;
    end;

    if PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biHeight < 0 then
    begin
      buf := PByte(FSubtitleParser.Buffer);
      w := FSubtitleParser.BitmapWidth shl 2;
      w2 := new_width shl 2;

      for i := 0 to FSubtitleParser.BitmapHeight -1 do
      begin
        Move(buf^, buffer^, w);
        inc(buffer, w2);
        inc(buf, w);
      end;
    end else
    begin
      for i := FSubtitleParser.BitmapHeight -1 downto 0 do
      begin
        Move(FSubtitleParser.Buffer[i * FSubtitleParser.BitmapWidth], buffer^,
             FSubtitleParser.BitmapWidth shl 2);
        inc(buffer, new_width shl 2);
      end;
    end;
  finally
    FSharedState.UnLock;
  end;

  Result := S_OK;
end;

function TDVBSubOutputPin.BeginFlush: HRESULT;
begin
  Result := inherited BeginFlush;
  FSubtitleParser.Flush;
end;

function TDVBSubOutputPin.GetMediaType(MediaType: PAMMediaType): HResult;
begin
  FSharedState.Lock;
  try
    // Return E_POINTER if MediaType Pointer == NULL
    if (MediaType = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    MediaType.cbFormat := SizeOf(TVideoInfoHeader2);
    MediaType.pbFormat := CoTaskMemAlloc(MediaType.cbFormat);
    if (MediaType.pbFormat = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    ZeroMemory(MediaType.pbFormat, MediaType.cbFormat);

    with PVideoInfoHeader2(MediaType.pbFormat)^ do
    begin
      // we need to setup a 4:3 Width/Height for the MediaType otherwise the
      // VMR9 will create some ugly black/white border.
      bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
      bmiHeader.biWidth := FSubtitleParser.BitmapWidth;
      bmiHeader.biHeight := FSubtitleParser.BitmapHeight;
      bmiHeader.biPlanes := 1;
      bmiHeader.biBitCount := 32;
      bmiHeader.biCompression := BI_RGB;
      bmiHeader.biSizeImage := GetBitmapSize(@bmiHeader);
      bmiHeader.biXPelsPerMeter := 0;
      bmiHeader.biYPelsPerMeter := 0;
      bmiHeader.biClrUsed := 0;
      bmiHeader.biClrImportant := 0;

      dwBitRate := 0;
      dwBitErrorRate := 0;
      AvgTimePerFrame := FFrameLength;
      dwInterlaceFlags := 0;
      dwCopyProtectFlags := 0;

      rcSource := Rect(0, 0, FSubtitleParser.BitmapWidth, FSubtitleParser.BitmapHeight);
      rcTarget := Rect(0, 0, FSubtitleParser.BitmapWidth, FSubtitleParser.BitmapHeight);

      case FAspectRatio of
        ar4_3:
        begin
          dwPictAspectRatioX := 4;
          dwPictAspectRatioY := 3;
          ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_4x3;
//          rcTarget := Rect(0, 0, DEFAULT_SPECS_WIDTH, DEFAULT_SPECS_WIDTH * 3 div 4);
        end;
        ar16_9:
        begin
          dwPictAspectRatioX := 16;
          dwPictAspectRatioY := 9;
          ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
//          rcTarget := Rect(0, 0, DEFAULT_SPECS_WIDTH, DEFAULT_SPECS_WIDTH * 9 div 16);
        end;
        ar221_1:
        begin
          dwPictAspectRatioX := 221;
          dwPictAspectRatioY := 100;
          ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
//          rcTarget := Rect(0, 0, DEFAULT_SPECS_WIDTH, Round(DEFAULT_SPECS_WIDTH * 1 / 2.21));
        end;
      end;

      MediaType.lSampleSize := bmiHeader.biSizeImage;
    end;

    MediaType.majortype := MEDIATYPE_Video;
    MediaType.subtype := MEDIASUBTYPE_ARGB32;
    MediaType.formattype := FORMAT_VideoInfo2;
    MediaType.bTemporalCompression := False;
    MediaType.bFixedSizeSamples := True;

    Result := S_OK;
  finally
    FSharedState.UnLock;
  end;
end;

function TDVBSubOutputPin.CheckMediaType(MediaType: PAMMediaType): HRESULT;
begin
  Result := E_INVALIDARG;

  if not IsEqualGUID(MediaType.majortype, MEDIATYPE_Video)
    then Exit;

  if not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_ARGB32)
    then Exit;

  if not IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo2)
    then Exit;

  if not MediaType.bFixedSizeSamples
    then Exit;

  if MediaType.pbFormat = nil
    then Exit;

  with PVideoInfoHeader2(MediaType.pbFormat)^ do
  begin
    if (bmiHeader.biWidth < FSubtitleParser.BitmapWidth) or
       (abs(bmiHeader.biHeight) < FSubtitleParser.BitmapHeight) or
       (bmiHeader.biBitCount <> 32)
       then Exit;
  end;

  Result := S_OK;
end;

procedure TDVBSubOutputPin.OnPCRData(ABuffer: PByte; ASize: Integer);
begin
  FSharedState.Lock;
  try
    FSubtitleParser.ParsePCRBuffer(ABuffer, ASize);
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBSubOutputPin.OnSubtitleData(ABuffer: PByte; ASize: Integer);
begin
  FSharedState.Lock;
  try
    FSubtitleParser.ParseSubtitleBuffer(ABuffer, ASize);
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBSubOutputPin.put_PID(APCRPID: Integer; APID: Integer; ACompositionPageID: Integer; AAncillaryPageID: Integer);
begin
  FSharedState.Lock;
  try
    FSubtitleParser.SetPID(APCRPID, APID, ACompositionPageID, AAncillaryPageID);
    if APID = -1
      then FFirstFill := True;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBSubOutputPin.put_AspectRatio(AAspectRatio: TAspectRatio);
begin
  FSharedState.Lock;
  try
    FAspectRatio := AAspectRatio;
    FNeedAspectRatioChange := True;
  finally
    FSharedState.UnLock;
  end;
end;

end.
