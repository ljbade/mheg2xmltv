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

unit DVBTeletextFilter;

interface

uses
  Windows, Classes, BaseClass, DirectShow9, SysUtils, BDAUtils, DVBInterface,
  Graphics, ActiveX, DSUtil, BDAConst, MPEGUtils, IDVBSource, MPEGConst, Math,
  MPEGParser, DVBTeletextParser, DVBSubtitlingParser;

const
  UNITS = 10000000;

  FPS_30 = UNITS div 30;
  FPS_25 = UNITS div 25;
  FPS_20 = UNITS div 20;
  FPS_10 = UNITS div 10;
  FPS_5 = UNITS div 5;
  FPS_4 = UNITS div 4;
  FPS_3 = UNITS div 3;
  FPS_2 = UNITS div 2;
  FPS_1 = UNITS div 1;

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..500000] of Cardinal;

  TDVBTeletextOutputPin = class(TBCSourceStream)
  protected
    FParser: TDVBTeletextParser;
    FSharedState: TBCCritSec;
    FFrameLength: Integer;
    FFrameNumber: Int64;
    FMixer: IVMRMixerControl9;
    FTransparency: Single;
    FNeedAspectRatioChange: Boolean;
    FAspectRatio: TAspectRatio;
    FFirstFill: Boolean;
    FFillNextBlack: Boolean;
    FDoubleSizeMode: TTeletextSizeMode;
    FAspectRatioMode: Integer;
    FIndexArray: array of Integer;
    FIndexArray2: array of Integer;
  public
    constructor Create(out hr: HResult; Filter: TBCSource);
    destructor Destroy; override;

    function GetMediaType(MediaType: PAMMediaType): HResult; override;
    function CheckMediaType(MediaType: PAMMediaType): HRESULT; override;
    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;
    function Notify(Filter: IBaseFilter; q: TQuality): HRESULT; override;  stdcall;

    procedure put_PID(APID: Integer);
    procedure put_ShowTeletext(Show: LongBool);
    procedure put_TeletextTransparent(Transparent: LongBool);
    procedure get_TeletextPage(out Page: Integer; out SubPage: Integer);
    procedure put_TeletextPage(Page: Integer; SubPage: Integer);
    procedure put_TeletextTransparency(Transparency: Integer);
    procedure put_TeletextClear;
    procedure put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar);
    procedure put_TeletextNumber(ANumber: Integer);
    procedure OnTeletextBuffer(ABuffer: PByte; ASize: Integer);
    procedure put_AspectRatio(AAspectRatio: TAspectRatio);
    procedure put_SizeMode(ASizeMode: TTeletextSizeMode);
    procedure put_TeletextAspectRatio(AAspectRatio: Integer);
    procedure put_TeletextFastext(AFastext: TTeletextFastext);
    procedure put_ClickAt(AX, AY, AWidth, AHeight, AOffset: Integer);
    procedure get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset: Integer; out AHand: Boolean);
    procedure put_TeletextFPS(AFPS: Integer);
    procedure put_CycleTeletext;
  published
    property FrameNumber: Int64 read FFrameNumber write FFrameNumber;
  end;

implementation

(*** TDVBTeletextOutputPin ****************************************************)

constructor TDVBTeletextOutputPin.Create(out hr: HResult; Filter: TBCSource);
var
  i: Integer;
begin
  inherited Create('Teletext Output Pin', hr, Filter, 'Teletext');
  FSharedState := TBCCritSec.Create;
  FFrameLength := FPS_2;
  FFrameNumber := 0;
  FParser := TDVBTeletextParser.Create;

  SetLength(FIndexArray, FParser.Height);
  SetLength(FIndexArray2, FParser.Height);
  for i := 0 to FParser.Height -1 do
  begin
    FIndexArray[i] := ((i shr 1) * 24) div 25;
    FIndexArray2[i] := FIndexArray[i] + ((FParser.Height * 24 div 25) shr 1);
  end;

  FNeedAspectRatioChange := False;
  FAspectRatio := ar4_3;
  FFirstFill := True;
  FFillNextBlack := False;
  FDoubleSizeMode := tsmNormal;
  FAspectRatioMode := 0;
end;

destructor TDVBTeletextOutputPin.Destroy;
begin
  FMixer := nil;
  FreeAndNil(FSharedState);
  FParser.Free;
  SetLength(FIndexArray, 0);
  SetLength(FIndexArray2, 0);
  inherited Destroy;;
end;

function TDVBTeletextOutputPin.GetMediaType(MediaType: PAMMediaType): HResult;
begin
  FFilter.StateLock.Lock;
  try
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
      bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
      bmiHeader.biWidth := FParser.Width;
      bmiHeader.biHeight := FParser.Height;

      bmiHeader.biPlanes := 1;
      bmiHeader.biBitCount := 32;
      bmiHeader.biCompression := BI_RGB;
      bmiHeader.biSizeImage := GetBitmapSize(@bmiHeader);
      bmiHeader.biXPelsPerMeter := 0;
      bmiHeader.biYPelsPerMeter := 0;
      bmiHeader.biClrUsed := 0;
      bmiHeader.biClrImportant := 0;

      dwInterlaceFlags := 0;
      dwCopyProtectFlags := 0;
      MediaType.lSampleSize := bmiHeader.biSizeImage;
      dwBitRate := 0;
      dwBitErrorRate := 0;

      rcSource := Rect(0, 0, FParser.Width, FParser.Height);
      rcTarget := Rect(0, 0, FParser.Width, FParser.Height);

      dwPictAspectRatioX := 4;
      if FAspectRatioMode = 0
        then dwPictAspectRatioY := 4
        else dwPictAspectRatioY := 3;
      ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_4x3;

      AvgTimePerFrame := FFrameLength;
    end;

    MediaType.majortype := MEDIATYPE_Video;
    MediaType.subtype := MEDIASUBTYPE_ARGB32;
    MediaType.formattype := FORMAT_VideoInfo2;
    MediaType.bTemporalCompression := False;
    MediaType.bFixedSizeSamples := True;

    Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

function TDVBTeletextOutputPin.CheckMediaType(MediaType: PAMMediaType): HRESULT;
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
    if (bmiHeader.biWidth < FParser.Width) or
       (abs(bmiHeader.biHeight) < FParser.Height) or
       (bmiHeader.biBitCount <> 32)
       then Exit;
  end;

  Result := S_OK;
end;

function TDVBTeletextOutputPin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
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

function TDVBTeletextOutputPin.FillBuffer(Sample: IMediaSample): HResult;
var
  buffer: PByte;
  size: Integer;
  Start, Stop: REFERENCE_TIME;
  pmt: PAMMediaType;
  new_width: Integer;
  i, c: Integer;
  w, w2: Integer;
  buffer2: PByte;
begin
{$IFNDEF VER170} // Another Delphi <= 7 Problem...solved in Delphi 2005
//  Result := S_OK;
{$ENDIF}

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
  size := (FParser.Height * new_width) shl 2;
  Sample.SetActualDataLength(size);
  Sample.GetPointer(PByte(buffer));

  FSharedState.Lock;
  try
    if FNeedAspectRatioChange then
    begin
      case FAspectRatio of
        ar4_3:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_4x3;
          end;
        end;
        ar16_9:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
          end;
        end;
        ar221_1:
        begin
          with PVideoInfoHeader2(Fmt.pbFormat)^ do
          begin
            ControlFlags.dwControlFlags := AMCONTROL_PAD_TO_16x9;
          end;
        end;
      end;

      FNeedAspectRatioChange := False;
      Sample.SetMediaType(@Fmt);
      //Sample.SetDiscontinuity(True);
    end;

  //  Sample.SetSyncPoint(True);
    if not FParser.Visible then
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
        if FFillNextBlack then
        begin
          FParser.Clear;
          FFillNextBlack := False;
          Sample.SetActualDataLength(size);
          Sample.GetPointer(PByte(buffer));
          FillChar(buffer^, size, 0);
        end;
      end;
      Result := S_OK;
      Exit;
    end;

    if FFillNextBlack then
    begin
      FParser.Clear;
      FFillNextBlack := False;
    end;

    if PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biHeight < 0 then
    begin
      case FDoubleSizeMode of
        tsmNormal:
        begin
          w := new_width shl 2;
          w2 := FParser.Width shl 2;
          buffer2 := PByte(FParser.GetBuffer);
          for i := 0 to FParser.Height -1 do
          begin
            Move(buffer2^, buffer^, w2);
            inc(buffer, w);
            inc(buffer2, w2);
          end;
        end;
        tsmDoubleUpper:
        begin
          w := new_width shl 2;
          w2 := FParser.Width shl 2;
          for i := 0 to FParser.Height -1 do
          begin
            buffer2 := PByte(FParser.GetBuffer);
            inc(buffer2, w2 * FIndexArray[i]);
            Move(buffer2^, buffer^, w2);
            inc(buffer, w);
          end;
        end;
        tsmDoubleLower:
        begin
          w := new_width shl 2;
          w2 := FParser.Width shl 2;
          for i := 0 to FParser.Height -1 do
          begin
            buffer2 := PByte(FParser.GetBuffer);
            inc(buffer2, w2 * FIndexArray2[i]);
            Move(buffer2^, buffer^, w2);
            inc(buffer, w);
          end;
        end;
      end;
    end else
    begin
      w := new_width shl 2;
      w2 := FParser.Width shl 2;
      for i := FParser.Height -1 downto 0 do
      begin
        case FDoubleSizeMode of
          tsmNormal:      Move(FParser.GetBuffer[i * FParser.Width], buffer^, w2);
          tsmDoubleUpper: Move(FParser.GetBuffer[((i * 24 div 25) shr 1) *  FParser.Width], buffer^, w2);
          tsmDoubleLower:
          begin
            c := FParser.Height div 25;
            c := c * 12;
            Move(FParser.GetBuffer[(((i * 24 div 25) shr 1) + c) * FParser.Width], buffer^, w2);
          end;
        end;
        inc(buffer, w);
      end;
    end;
  finally
    FSharedState.UnLock;
  end;

  Result := S_OK;
end;

function TDVBTeletextOutputPin.Notify(Filter: IBaseFilter; q: TQuality): HRESULT;
begin
  Result := S_OK;
end;

procedure TDVBTeletextOutputPin.put_ShowTeletext(Show: LongBool);
var
  pin: IPin;
  pin_info: TPinInfo;
begin
  FSharedState.Lock;
  try
    FParser.Visible := Show;

    if Show
      then FParser.Page := FParser.Page;

    if not Assigned(FMixer) then
    begin
      pin := GetConnected;
      if not Assigned(pin)
        then Exit;

      pin.QueryPinInfo(pin_info);
      if not Assigned(pin_info.pFilter)
        then Exit;

      pin_info.pFilter.QueryInterface(IID_IVMRMixerControl9, FMixer);
      if not Assigned(FMixer)
        then Exit;
    end;

    if FParser.Visible then
    begin
      FMixer.SetAlpha(1, FTransparency);
    end else
    begin
      FMixer.SetAlpha(1, 0.0);
    end;
  finally
    pin_info.pFilter := nil;
    pin := nil;
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextTransparent(Transparent: LongBool);
begin
  FSharedState.Lock;
  try
    FParser.Transparent := Transparent;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextPage(Page: Integer; SubPage: Integer);
begin
  FSharedState.Lock;
  try
    FParser.Page := Page;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.get_TeletextPage(out Page: Integer; out SubPage: Integer);
begin
  FSharedState.Lock;
  try
    Page := FParser.Page;
    SubPage := FParser.SubPage;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextTransparency(Transparency: Integer);
begin
  FTransparency := Transparency / 100;
end;

procedure TDVBTeletextOutputPin.put_TeletextClear;
begin
  FSharedState.Lock;
  try
    FParser.Clear;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar);
begin
  FSharedState.Lock;
  try
    if not Assigned(AFilename)
      then Exit;

    FParser.SaveTeletext(AType, AFilename);
    CoTaskMemFree(AFilename);
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextNumber(ANumber: Integer);
begin
  FSharedState.Lock;
  try
    FParser.SetPageNumber(ANumber);
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_PID(APID: Integer);
begin
  FSharedState.Lock;
  try
    FFillNextBlack := True;
    FParser.PID := APID;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.OnTeletextBuffer(ABuffer: PByte; ASize: Integer);
begin
  FSharedState.Lock;
  try
    FParser.ParseTeletextBuffer(ABuffer, ASize);
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_AspectRatio(AAspectRatio: TAspectRatio);
begin
  FSharedState.Lock;
  try
    FAspectRatio := AAspectRatio;
    FNeedAspectRatioChange := True;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_SizeMode(ASizeMode: TTeletextSizeMode);
begin
  FSharedState.Lock;
  try
    FDoubleSizeMode := ASizeMode;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextAspectRatio(AAspectRatio: Integer);
begin
  FSharedState.Lock;
  try
    FAspectRatioMode := AAspectRatio;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextFastext(AFastext: TTeletextFastext);
begin
  FSharedState.Lock;
  try
    FParser.SetFastext(AFastext);
  finally
    FSharedState.UnLock;
  end;
end;

type
  TMyRect = record
    X, Y, Width, Height: Integer;
  end;

function IsNumber(s: Char): Boolean;
begin
  case s of
    '0'..'9': Result := True;
    else      Result := False;
  end;
end;

procedure TDVBTeletextOutputPin.put_ClickAt(AX, AY, AWidth, AHeight, AOffset: Integer);
var
  fy: Integer;
  line, ch, i: Integer;
  r: TMyRect;
  text: TTeletextArray;
  str: String;

  function InTeletext: Boolean;
  begin
    Result := (AX >= r.X) and (AX < r.Width + r.X) and (AY >= r.Y) and (AY < r.Height + r.Y);
  end;

begin
//  OutputDebugString(PChar('Videowindow: ' + inttostr(0) + ',' + inttostr(0) + ',' + inttostr(AWidth) + ',' + inttostr(AHeight)));
  fy := AHeight * AOffset div 100;
  r.Y := fy;
  r.Height := AHeight - (r.Y * 2);

  case FAspectRatioMode of
    0:   r.Width := r.Height;
    else r.Width := r.Height * 4 div 3;
  end;

  r.X := (AWidth - r.Width) div 2;

  if InTeletext then
  begin
    line := ((AY - r.Y) * 25) div r.Height;
    ch := ((AX - r.X) * 40) div r.Width;
    text := FParser.Text;
    if (ch >= 0) and (ch < 40) then
    begin
      if line = 24 then
      begin
        // rot grün gelb blau
        if FParser.HasColorAtChar(ch, clRed)
          then FParser.SetFastext(tfeRed)
        else if FParser.HasColorAtChar(ch, clLime)
          then FParser.SetFastext(tfeGreen)
        else if FParser.HasColorAtChar(ch, clYellow)
          then FParser.SetFastext(tfeYellow)
        else if FParser.HasColorAtChar(ch, clBlue)
          then FParser.SetFastext(tfeBlue)
        else if FParser.HasColorAtChar(ch, clAqua)
          then FParser.SetFastext(tfeBlue);
      end else
      if (line < 24) and (line > 0) then
      begin
        if FParser.LineIsDouble(line-1) then
        begin
          dec(line);
        end;
        UnParityTeletextLine(@text[line]);
        if IsNumber(text[line][ch]) then
        begin
          str := text[line][ch];

          i := ch + 1;
          while i < 40 do
          begin
            if not IsNumber(text[line][i])
              then break;
            str := str + text[line][i];
            inc(i);
          end;

          i := ch - 1;
          while i > -1 do
          begin
            if not IsNumber(text[line][i])
              then break;
            str := text[line][i] + str;
            dec(i);
          end;

          if Length(str) = 3 then
          begin
            if TryStrToInt(str, i) then
            begin
              if (i > 99) and (i < 900)
                then put_TeletextPage(i, 0);
            end;
          end;
        end;
      end else
      if (line = 0) and (ch = 0) then
      begin
//        put_ShowTeletext(False);
      end;
    end;
  end;
end;

procedure TDVBTeletextOutputPin.get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset: Integer; out AHand: Boolean);
var
  fy: Integer;
  line, ch, i: Integer;
  r: TMyRect;
  text: TTeletextArray;
  str: String;

  function InTeletext: Boolean;
  begin
    Result := (AX >= r.X) and (AX < r.Width + r.X) and (AY >= r.Y) and (AY < r.Height + r.Y);
  end;

begin
//  OutputDebugString(PChar('Videowindow: ' + inttostr(0) + ',' + inttostr(0) + ',' + inttostr(AWidth) + ',' + inttostr(AHeight)));
  fy := AHeight * AOffset div 100;
  r.Y := fy;
  r.Height := AHeight - (r.Y * 2);

  case FAspectRatioMode of
    0:   r.Width := r.Height;
    else r.Width := r.Height * 4 div 3;
  end;

  r.X := (AWidth - r.Width) div 2;

  AHand := False;

  if InTeletext then
  begin
    line := ((AY - r.Y) * 25) div r.Height;
    ch := ((AX - r.X) * 40) div r.Width;
    text := FParser.Text;
    if (ch >= 0) and (ch < 40) then
    begin
      if line = 24 then
      begin
        // rot grün gelb blau
        if FParser.HasColorAtChar(ch, clRed)
          then AHand := True
        else if FParser.HasColorAtChar(ch, clLime)
          then AHand := True
        else if FParser.HasColorAtChar(ch, clYellow)
          then AHand := True
        else if FParser.HasColorAtChar(ch, clBlue)
          then AHand := True
        else if FParser.HasColorAtChar(ch, clAqua)
          then AHand := True;
      end else
      if (line < 24) and (line > 0) then
      begin
        if FParser.LineIsDouble(line-1) then
        begin
          dec(line);
        end;
        UnParityTeletextLine(@text[line]);
        if IsNumber(text[line][ch]) then
        begin
          str := text[line][ch];

          i := ch + 1;
          while i < 40 do
          begin
            if not IsNumber(text[line][i])
              then break;
            str := str + text[line][i];
            inc(i);
          end;

          i := ch - 1;
          while i > -1 do
          begin
            if not IsNumber(text[line][i])
              then break;
            str := text[line][i] + str;
            dec(i);
          end;

          if Length(str) = 3 then
          begin
            if TryStrToInt(str, i) then
            begin
              if (i > 99) and (i < 900)
                then AHand := True
            end;
          end;
        end;
      end else
      if (line = 0) and (ch = 0) then
      begin
//        put_ShowTeletext(False);
      end;
    end;
  end;
end;

procedure TDVBTeletextOutputPin.put_TeletextFPS(AFPS: Integer);
begin
  if AFPS < 1
    then AFPS := 1
  else if AFPS > 30
    then AFPS := 30;

  FFrameLength := UNITS div AFPS;
end;

procedure TDVBTeletextOutputPin.put_CycleTeletext;
begin
  FParser.CycleTeletextPage;
end;

end.


