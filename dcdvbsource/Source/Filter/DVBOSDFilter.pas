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

unit DVBOSDFilter;

interface

uses
  Windows, Classes, BaseClass, DirectShow9, SysUtils, BDAUtils, DVBInterface,
  Graphics, ActiveX, DSUtil, BDAConst, MPEGUtils, DVBTeletextFilter, IDVBSource,
  MPEGConst, DVBSubtitleFilter, DVBSubtitlingParser;

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

  TDVBOSDOutputPin = class;

  TOSDFadeThread = class(TThread)
  protected
    FOwner: TDVBOSDOutputPin;
    FAlpha: Integer;
    FMixer: IVMRMixerControl9;
    procedure Execute; override;
  public
    constructor Create(AOwner: TDVBOSDOutputPin; AAlpha: Integer; AMixer: IVMRMixerControl9);
  end;

  TDVBOSDOutputPin = class(TBCSourceStream)
  protected
    FOSDColor: TOSDColor;
    FSharedState: TBCCritSec;
    FWidth: Integer;
    FHeight: Integer;
    FSize: Integer;
    FWidth2: Integer;
    FHeight2: Integer;
    FFrameLength: Integer;
    FFrameNumber: Int64;
    FVisible: Boolean;
    FThisTime: Cardinal;
    FVMRVisible: Boolean;
    FAlpha: Integer;
    FFadeThread: TOSDFadeThread;
    FVMR: IVMRMixerControl9;
    FNeedAspectRatioChange: Boolean;
    FAspectRatio: TAspectRatio;
    FNeedUpdate: Boolean;
    FFirstFill: Boolean;
    FFontLarge: HFONT;
    FFontSmall: HFONT;
    FB, FB2, FB3: HBITMAP;
    FD, FD2, FD3: THandle;
    FBB, FBB2, FBB3: PByte;
    FRect: VMR9NormalizedRect;
    procedure ShowOSD;
    procedure HideOSD;
    procedure KillThread;
    function GetVMR: IVMRMixerControl9;
    procedure FillRect(AX, AY, AWidth, AHeight: Integer; AColor: Cardinal);
  public
    constructor Create(out hr: HResult; Filter: TBCSource);
    destructor Destroy; override;

    function GetMediaType(MediaType: PAMMediaType): HResult; override;
    function CheckMediaType(MediaType: PAMMediaType): HRESULT; override;
    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;
    function Notify(Filter: IBaseFilter; q: TQuality): HRESULT; override;  stdcall;

    procedure put_OSDText(AChannel: String; ANow: String; ANext: String; AAlpha: Integer; ADuration: Integer);
    procedure put_OSDColor(AColor: TOSDColor);
    procedure put_AspectRatio(AAspectRatio: TAspectRatio);
    procedure put_OSDChannel(AChannel: String; AAlpha: Integer; ADuration: Integer);
    procedure put_OSDRect(ARect: VMR9NormalizedRect);
  published
    property FrameNumber: Int64 read FFrameNumber write FFrameNumber;
  end;

  TDVBOSDFilter = class (TBCSource, IOSDFilter, ITeletextFilter, ITeletextCallBack, IPCRCallback, ISubtitleCallback)
  private
    FOSDPin: TDVBOSDOutputPin;
    FTeletextPin: TDVBTeletextOutputPin;
    FSubtitlePin: TDVBSubOutputPin;
  public
    constructor Create;
    destructor Destroy; override;

    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: integer; override;

    function GetState(dwMilliSecsTimeout: DWORD; out State: TFilterState): HRESULT; override; stdcall;
    function Run(tStart: Int64): HRESULT; override; stdcall;

    // IOSDFilter
    procedure put_OSDText(AChannel: String; ANow: String; ANext: String; AAlpha: Integer; ADuration: Integer);
    procedure put_OSDColor(AColor: TOSDColor);
    procedure put_OSDChannel(AChannel: String; AAlpha: Integer; ADuration: Integer);

    // ITeletextFilter
    function put_TeletextPID(APID: Integer): HRESULT; stdcall;
    function put_TeletextShow(Show: LongBool): HRESULT; stdcall;
    function put_TeletextTransparent(Transparent: LongBool): HRESULT; stdcall;
    function put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT; stdcall;
    function put_TeletextTransparency(Transparency: Integer): HRESULT; stdcall;
    function put_TeletextClear: HRESULT; stdcall;
    function put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT; stdcall;
    function put_TeletextNumber(ANumber: Integer): HRESULT; stdcall;
    function get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT; stdcall;
    function put_SizeMode(ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
    function put_TeletextAspectRatio(AAspectRatio: Integer): HRESULT; stdcall;
    function put_TeletextFastext(AFastext: TTeletextFastext): HRESULT; stdcall;
    function put_ClickAt(AX, AY, AWidth, AHeight, AOffset: Integer): HRESULT; stdcall;
    function get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset: Integer; out AHand: Boolean): HRESULT; stdcall;
    function put_TeletextFPS(AFPS: Integer): HRESULT; stdcall;
    function put_CycleTeletext: HRESULT; stdcall;
    // ITeletextCallBack
    procedure OnTeletextBuffer(ABuffer: PByte; ASize: Integer);
    // IPCRCallback
    procedure OnPCRData(ABuffer: PByte; ASize: Integer);
    // ISubtitleCallback
    procedure OnSubtitleData(ABuffer: PByte; ASize: Integer);
    procedure put_PID(APCRPID: Integer; APID: Integer; ACompositionPageID: Integer; AAncillaryPageID: Integer);
    procedure put_AspectRatio(AAspectRatio: TAspectRatio);
    procedure put_OSDRect(ARect: VMR9NormalizedRect);
  end;

implementation

uses Math;

(*** TOSDFadeThread ***********************************************************)

constructor TOSDFadeThread.Create(AOwner: TDVBOSDOutputPin; AAlpha: Integer; AMixer: IVMRMixerControl9);
begin
  inherited Create(True);
  FOwner := AOwner;
  FAlpha := AAlpha;
  FMixer := AMixer;
  Resume;
end;

procedure TOSDFadeThread.Execute;
var
  rect: VMR9NormalizedRect;
  diff: Single;
begin
  Priority := tpLowest;

  while Assigned(FMixer) and not Terminated and (FAlpha > 0) do
  begin
    sleep(10);
    dec(FAlpha, 4);
    if FAlpha < 0
      then FAlpha := 0;

    rect := TDVBOSDOutputPin(FOwner).FRect;
    if FAlpha > 0 then
    begin
      diff := ((rect.bottom - rect.top) / 2) * (100 - FAlpha)/ 100;
      rect.top := rect.top + diff;
      rect.bottom := rect.bottom - diff;
//      rect.left := rect.left + diff;
//      rect.right := rect.right - diff;
      FMixer.SetOutputRect(3, @rect);
    end;

    FMixer.SetAlpha(3, FAlpha / 100);
  end;
end;

(*** TDVBOSDFilter ************************************************************)

constructor TDVBOSDFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create('DC-DVB OSD Filter', nil, CLSID_DCDVBOSDFilter);

  FOSDPin := TDVBOSDOutputPin.Create(hr, Self);
  FTeletextPin := TDVBTeletextOutputPin.Create(hr, Self);
  FSubtitlePin := TDVBSubOutputPin.Create('Sub', hr, Self, 'Subtitle');
end;

destructor TDVBOSDFilter.Destroy;
begin
  FreeAndNil(FOSDPin);
  FreeAndNil(FTeletextPin);
  FreeAndNil(FSubtitlePin);
  inherited Destroy;
end;

function TDVBOSDFilter.GetPinCount: integer;
begin
  Result := 3;
end;

function TDVBOSDFilter.GetPin(n: Integer): TBCBasePin;
begin
  case n of
    0: Result := FTeletextPin;
    1: Result := FSubtitlePin;
    2: Result := FOSDPin;
    else Result := nil;
  end;
end;

procedure TDVBOSDFilter.put_OSDText(AChannel: String; ANow: String; ANext: String; AAlpha: Integer; ADuration: Integer);
begin
  FOSDPin.put_OSDText(AChannel, ANow, ANext, AAlpha, ADuration);
end;

function TDVBOSDFilter.put_TeletextPID(APID: Integer): HRESULT; stdcall;
begin
  FTeletextPin.put_PID(APID);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextShow(Show: LongBool): HRESULT;
begin
  FTeletextPin.put_ShowTeletext(Show);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextTransparent(Transparent: LongBool): HRESULT;
begin
  FTeletextPin.put_TeletextTransparent(Transparent);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT;
begin
  FTeletextPin.put_TeletextPage(Page, SubPage);
  Result := S_OK;
end;

function TDVBOSDFilter.get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT;
begin
  FTeletextPin.get_TeletextPage(Page, SubPage);
  Result := S_OK;
end;

function TDVBOSDFilter.put_SizeMode(ASizeMode: TTeletextSizeMode): HRESULT;
begin
  FTeletextPin.put_SizeMode(ASizeMode);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextAspectRatio(AAspectRatio: Integer): HRESULT;
begin
  FTeletextPin.put_TeletextAspectRatio(AAspectRatio);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextFastext(AFastext: TTeletextFastext): HRESULT;
begin
  FTeletextPin.put_TeletextFastext(AFastext);
  Result := S_OK;
end;

function TDVBOSDFilter.put_ClickAt(AX, AY, AWidth, AHeight, AOffset: Integer): HRESULT;
begin
  FTeletextPin.put_ClickAt(AX, AY, AWidth, AHeight, AOffset);
  Result := S_OK;
end;

function TDVBOSDFilter.get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset: Integer; out AHand: Boolean): HRESULT;
begin
  FTeletextPin.get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset, AHand);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextFPS(AFPS: Integer): HRESULT;
begin
  FTeletextPin.put_TeletextFPS(AFPS);
  Result := S_OK;
end;

function TDVBOSDFilter.put_CycleTeletext: HRESULT;
begin
  FTeletextPin.put_CycleTeletext();
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextTransparency(Transparency: Integer): HRESULT;
begin
  FTeletextPin.put_TeletextTransparency(100 - Transparency);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextClear: HRESULT;
begin
  FTeletextPin.put_TeletextClear;
  Result := S_OK;
end;

function TDVBOSDFilter.put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT;
begin
  FTeletextPin.put_SaveTeletext(AType, AFilename);
  Result := S_OK;
end;

function TDVBOSDFilter.put_TeletextNumber(ANumber: Integer): HRESULT;
begin
  FTeletextPin.put_TeletextNumber(ANumber);
  Result := S_OK;
end;

procedure TDVBOSDFilter.OnPCRData(ABuffer: PByte; ASize: Integer);
begin
  FSubtitlePin.OnPCRData(ABuffer, ASize);
end;

procedure TDVBOSDFilter.OnTeletextBuffer(ABuffer: PByte; ASize: Integer);
begin
  FTeletextPin.OnTeletextBuffer(ABuffer, ASize);
end;

procedure TDVBOSDFilter.OnSubtitleData(ABuffer: PByte; ASize: Integer);
begin
  FSubtitlePin.OnSubtitleData(ABuffer, ASize);
end;

procedure TDVBOSDFilter.put_PID(APCRPID: Integer; APID: Integer; ACompositionPageID: Integer; AAncillaryPageID: Integer);
begin
  FSubtitlePin.put_PID(APCRPID, APID, ACompositionPageID, AAncillaryPageID);
end;

procedure TDVBOSDFilter.put_AspectRatio(AAspectRatio: TAspectRatio);
begin
  FOSDPin.put_AspectRatio(AAspectRatio);
  FTeletextPin.put_AspectRatio(AAspectRatio);
  FSubtitlePin.put_AspectRatio(AAspectRatio);
end;

function TDVBOSDFilter.GetState(dwMilliSecsTimeout: DWORD; out State: TFilterState): HRESULT;
begin
  if not Assigned(@State) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  Result := inherited GetState(dwMilliSecsTimeout, State);

  if (State = State_Paused)
    then Result := VFW_S_CANT_CUE;
end;

function TDVBOSDFilter.Run(tStart: Int64): HRESULT;
begin
//  case FState of
//    State_Stopped: OutputDebugString('AAAAAAAA');
//    State_Paused: OutputDebugString('BBBBBBBB');
//    State_Running: OutputDebugString('CCCCCCCCCCC');
//  end;
//  FOSDPin.FrameNumber := 0;
//  FTeletextPin.FrameNumber := 0;
//  FSubtitlePin.FrameNumber := 0;
  Result := inherited Run(tStart);
end;

procedure TDVBOSDFilter.put_OSDColor(AColor: TOSDColor);
begin
  FOSDPin.put_OSDColor(AColor);
end;

procedure TDVBOSDFilter.put_OSDChannel(AChannel: String; AAlpha: Integer; ADuration: Integer);
begin
  FOSDPin.put_OSDChannel(AChannel, AAlpha, ADuration);
end;

procedure TDVBOSDFilter.put_OSDRect(ARect: VMR9NormalizedRect);
begin
  FOSDPin.put_OSDRect(ARect);
end;

(*** TDVBOSDOutputPin *********************************************************)

procedure DrawGradient(AStartColor: TColor; AEndColor: TColor; ABuffer: PByte; AWidth: Integer; AHeight: Integer);
var
  deltas : array[0..2] of Single;
  i : integer;
  p: PCardinal;
  c: Cardinal;
  k: Integer;
  r, g, b: Integer;
begin
  deltas[0] := (GetRValue(AEndColor) - GetRValue(AStartColor)) / AHeight;
  deltas[1] := (GetGValue(AEndColor) - GetGValue(AStartColor)) / AHeight;
  deltas[2] := (GetBValue(AEndColor) - GetBValue(AStartColor)) / AHeight;
  p := PCardinal(ABuffer);
  for i := 0 to AHeight -1 do
  begin
    r := Round(GetRValue(AStartColor) + i * Deltas[0]);
    g := Round(GetGValue(AStartColor) + i * Deltas[1]);
    b := Round(GetBValue(AStartColor) + i * Deltas[2]);

    c := RGB(b, g, r);
    for k := 0 to AWidth -1 do
    begin
      p^ := c;
      inc(p);
    end;
  end;
end;

constructor TDVBOSDOutputPin.Create(out hr: HResult; Filter: TBCSource);
var
  log_font: TLogFont;
  dc: HDC;
  screenLogPixels: Integer;
  bitmap_info: TBitmapInfo;
begin
  inherited Create('OSD Output Pin', hr, Filter, 'OSD');
  FSharedState := TBCCritSec.Create;
  FFrameLength := FPS_2;
  FFrameNumber := 0;

  FWidth2 := 1024;
  FHeight2 := 190;

  FWidth := 1400;
  FHeight := 190;
  FSize := (FWidth * FHeight) shl 2;

	dc := GetDC(0);
	screenLogPixels := GetDeviceCaps(dc, LOGPIXELSY);
	ReleaseDC(0,dc);

  ZeroMemory(@log_font, SizeOf(TLogFont));
  log_font.lfHeight := -MulDiv(36, screenLogPixels, 72);
  log_font.lfWeight := FW_BOLD;
  log_font.lfFaceName := 'Tahoma';
  log_font.lfQuality := ANTIALIASED_QUALITY;
  FFontLarge := CreateFontIndirect(log_font);

  ZeroMemory(@log_font, SizeOf(TLogFont));
  log_font.lfHeight := -MulDiv(28, screenLogPixels, 72);
  log_font.lfWeight := FW_BOLD;
  log_font.lfFaceName := 'Tahoma';
  log_font.lfQuality := ANTIALIASED_QUALITY;
  FFontSmall := CreateFontIndirect(log_font);

  FillChar(bitmap_info, SizeOf(TBitmapInfo), 0);
  with bitmap_info.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := FWidth2;
    biHeight := -FHeight2;
  end;
  FB := CreateDIBSection(0, bitmap_info, DIB_RGB_COLORS, Pointer(FBB), 0, 0);
  FD := CreateCompatibleDC(0);
  SelectObject(FD, FB);

  FillChar(bitmap_info, SizeOf(TBitmapInfo), 0);
  with bitmap_info.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := FWidth;
    biHeight := -FHeight;
  end;
  FB2 := CreateDIBSection(0, bitmap_info, DIB_RGB_COLORS, Pointer(FBB2), 0, 0);
  FD2 := CreateCompatibleDC(0);
  SelectObject(FD2, FB2);

  FillChar(bitmap_info, SizeOf(TBitmapInfo), 0);
  with bitmap_info.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := FWidth;
    biHeight := -FHeight;
  end;
  FB3 := CreateDIBSection(0, bitmap_info, DIB_RGB_COLORS, Pointer(FBB3), 0, 0);
  FD3 := CreateCompatibleDC(0);
  SelectObject(FD3, FB3);

  SetBkMode(FD3, Windows.TRANSPARENT);
//  SetStretchBltMode(FD, Windows.HALFTONE);
//  SetStretchBltMode(FD3, Windows.HALFTONE);

  DrawGradient($00B07D44, $00754923, FBB2, FWidth, FHeight);

  FVisible := False;
  FNeedAspectRatioChange := False;
  FAspectRatio := ar4_3;
  FFirstFill := True;
end;

destructor TDVBOSDOutputPin.Destroy;
begin
  KillThread;
  FVMR := nil;
  DeleteObject(FFontLarge);
  DeleteObject(FFontSmall);
  DeleteDC(FD);
  DeleteObject(FB);
  DeleteDC(FD2);
  DeleteObject(FB2);
  DeleteDC(FD3);
  DeleteObject(FB3);
  FreeAndNil(FSharedState);
  inherited Destroy;;
end;

function TDVBOSDOutputPin.GetMediaType(MediaType: PAMMediaType): HResult;
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
      bmiHeader.biWidth := FWidth2;
      bmiHeader.biHeight := FHeight2;

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

      rcSource := Rect(0, 0, FWidth2, FHeight2);
      rcTarget := Rect(0, 0, FWidth2, FHeight2);

      dwPictAspectRatioX := 7;
      dwPictAspectRatioY := 1;
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

function TDVBOSDOutputPin.CheckMediaType(MediaType: PAMMediaType): HRESULT;
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
    if (bmiHeader.biWidth < FWidth2) or
       (abs(bmiHeader.biHeight) < FHeight2) or
       (bmiHeader.biBitCount <> 32)
       then Exit;
  end;

  Result := S_OK;
end;

function TDVBOSDOutputPin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
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

function TDVBOSDOutputPin.FillBuffer(Sample: IMediaSample): HResult;
var
  buffer: PByte;
  size: Integer;
  Start, Stop: REFERENCE_TIME;
  pmt: PAMMediaType;
  new_width: Integer;
  i: Integer;
  p: PCardinal;
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

  FSharedState.Lock;
  try
    new_width := PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biWidth;
    size := (Abs(PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biHeight) * new_width) shl 2;
    Sample.SetActualDataLength(size);
    Sample.GetPointer(PByte(buffer));

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

    if not FVisible then
    begin
      if FFirstFill then
      begin
        FillChar(buffer^, size, 0);
        FFirstFill := False;
      end else
      begin
        Sample.SetActualDataLength(0);
      end;
      Result := S_OK;
      Exit;
    end;

    if GetTickCount > FThisTime then
    begin
      HideOSD;
      if FFirstFill then
      begin
        FillChar(buffer^, size, 0);
        FFirstFill := False;
      end else
      begin
        Sample.SetActualDataLength(0);
      end;
      Result := S_OK;
      FVisible := False;
      Exit;
    end;

    if not FVMRVisible then
    begin
      ShowOSD;
    end;

    if FNeedUpdate then
    begin
      p := PCardinal(FBB);
      if PVideoInfoHeader2(Fmt.pbFormat).bmiHeader.biHeight < 0 then
      begin
        for i := 0 to FHeight2 -1 do
        begin
          Move(p^, buffer^, FWidth2 shl 2);
          inc(p, FWidth2);
          inc(buffer, new_width shl 2);
        end;
      end else
      begin
        for i := FHeight2 -1 downto 0 do
        begin
          Move(p^, buffer^, FWidth2 shl 2);
          inc(p, FWidth2);
          inc(buffer, new_width shl 2);
        end;
      end;
      FNeedUpdate := False;
    end;
  finally
    FSharedState.UnLock;
  end;

  Result := S_OK;
end;

function TDVBOSDOutputPin.Notify(Filter: IBaseFilter; q: TQuality): HRESULT;
begin
  Result := S_OK;
end;

procedure ImageBilinearResize(SrcBitmap,DestBitmap: PByte; SrcWidth, SrcHeight, DestWidth, DestHeight: Integer);
Type
  TColor32 =
  Record
    cRed   : Byte;
    cGreen : Byte;
    cBlue  : Byte;
    cAlpha : Byte;
  End;
  PColor32          = ^TColor32;
  TMyScanLine32     = Array[0..4095] of TColor;
  PMyScanLine32 = ^TMyScanLine32;
  TMyScanLine32Grid = Array[0..4095] of PMyScanLine32;
var
  x,y,xP,yP      : Integer;
  yP2,xP2        :  Integer;
  Read32,Read232 : ^TMyScanLine32;
  t,z,z2,iz2     : Integer;
  pc32           : PColor32;
  w1,w2,w3,w4    : Integer;
  Col132,Col232  : PColor32;
  P32            : PMyScanLine32;
  PL32           : ^TMyScanLine32Grid;
  PDif           : Integer;
  ypShr          : Integer;
begin
  If (DestWidth<=2) or (DestHeight<=2) then Exit;

  xP2  := ((SrcWidth -1) shl 11) div DestWidth;
  yP2  := ((SrcHeight-1) shl 11) div DestHeight;
  yP   := 0;

  New(PL32);
  // Calculate memory used for each line display for quick scanline seeks (dest image)
  PL32^[0] := PMyScanLine32(SrcBitmap);
  PL32^[1] := PMyScanLine32(PByte(Integer(SrcBitmap) + (SrcWidth shl 2)));
  PDif     := Integer(PL32^[1])-Integer(PL32^[0]);

  // Pre-Calculate scanline positions for source image
  For Y := 2 to SrcHeight-1 do Integer(PL32^[Y]) := Integer(PL32^[Y-1])+PDif;

  P32  := PMyScanLine32(DestBitmap);
  PDif := (Integer(DestBitmap) + (DestWidth shl 2))-Integer(P32);
  For Y := 0 to DestHeight-1 do
  Begin
    xP   := 0;
    ypShr:= yP shr 11;
    Pointer(Read32) := Pointer(PL32^[ypShr]);
    If ypShr < SrcHeight-1 then Pointer(Read232) := Pointer(PL32^[ypShr+1]) else
      Pointer(Read232) := Pointer(PL32^[ypShr]);

    pc32 := @P32[0];
    z2  := yP and 2047;
    iz2 := 2048 - z2;
    For X := 0 to DestWidth-1 do
    Begin
      t      := xP shr 11;
      Col132 := @Read32[t];
      Col232 := @Read232[t];
      z      := xP and 2047;
      w2     := (z * iz2) shr 11;
      w1     := iz2 - w2;
      w4     := (z * z2) shr 11;
      w3     := z2-w4;
      pc32^.cBlue  := (Col132.cBlue  * w1 + PColor32(Integer(Col132) + 4).cBlue  * w2+
                       Col232.cBlue  * w3 + PColor32(Integer(Col232) + 4).cBlue  * w4) shr 11;
      pc32^.cGreen := (Col132.cGreen * w1 + PColor32(Integer(Col132) + 4).cGreen * w2+
                       Col232.cGreen * w3 + PColor32(Integer(Col232) + 4).cGreen * w4) shr 11;
      pc32^.cRed   := (Col132.cRed   * w1 + PColor32(Integer(Col132) + 4).cRed   * w2+
                       Col232.cRed   * w3 + PColor32(Integer(Col232) + 4).cRed   * w4) shr 11;
      Inc(pc32);
      Inc(xP,xP2);
    End;
    Inc(yP,yP2);
    Inc(Integer(P32),PDif);
  End;
  Dispose(PL32);
end;

procedure TDVBOSDOutputPin.put_OSDText(AChannel: String; ANow: String; ANext: String; AAlpha: Integer; ADuration: Integer);
var
  i, c: Integer;
  p:PCardinal;
  str, str2: String;

  function ShortenString(AText: String): String;
  var
    w: Integer;
    s: TSize;
  begin
    Result := AText;
    Windows.GetTextExtentPoint32(FD3, PChar(Result), Length(Result), s);
    w := s.cx;
    if w < 1330
      then Exit;
    while (w > 1320) do
    begin
      Delete(Result, Length(Result), 1);
      Windows.GetTextExtentPoint32(FD3, PChar(Result), Length(Result), s);
      w := s.cx;
    end;
    Result := Result + '...';
  end;

begin
  FSharedState.Lock;
  try
    str := FormatDateTime('hh:nn', Now);
    i := Length(AChannel);
    c := Length(str);

    Move(FBB2^, FBB3^, FSize);

    SelectObject(FD3, FFontLarge);
    SetTextColor(FD3, FOSDColor.FontShadowColor);
    ExtTextout(FD3, 23, 17, 0, nil, PChar(AChannel), i, nil);
    ExtTextout(FD3, 1238, 17, 0, nil, PChar(str), c, nil);
    SetTextColor(FD3, FOSDColor.FontColor);
    ExtTextout(FD3, 21, 15, 0, nil, PChar(AChannel), i, nil);
    ExtTextout(FD3, 1236, 15, 0, nil, PChar(str), c, nil);

    SelectObject(FD3, FFontSmall);
    SetTextColor(FD3, FOSDColor.FontShadowColor);

    str := ShortenString(ANow);
    str2 := ShortenString(ANext);
    i := Length(str);
    c := Length(str2);

    ExtTextout(FD3, 23, 82, 0, nil, PChar(str), i, nil);
    ExtTextout(FD3, 23, 126, 0, nil, PChar(str2), c, nil);
    SetTextColor(FD3, FOSDColor.FontColor);
    ExtTextout(FD3, 21, 80, 0, nil, PChar(str), i, nil);
    ExtTextout(FD3, 21, 124, 0, nil, PChar(str2), c, nil);

    ImageBilinearResize(FBB3, FBB, FWidth, FHeight, FWidth2, FHeight2);

    p := PCardinal(FBB);
    for i := 0 to FHeight2 -1 do
    begin
      for c := 0 to FWidth2 -1 do
      begin
        p^ := p^ or $FF000000;
        inc(p);
      end;
    end;

    p := PCardinal(FBB);
    for i := 0 to 2 do
    begin
      for c := 0 to FWidth2 -1 do
      begin
        p^ := $50FFFFFF;
        inc(p);
      end;
    end;

    p := PCardinal(FBB);
    inc(p, (FHeight2 - 2) * FWidth2);
    for i := 0 to 1 do
    begin
      for c := 0 to FWidth2 -1 do
      begin
        p^ := $40FFFFFF;
        inc(p);
      end;
    end;

    FAlpha := 100 - AAlpha;
    FThisTime := GetTickCount + Int64(ADuration * 1000);
    FVisible := True;

    FNeedUpdate := True;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBOSDOutputPin.ShowOSD;
begin
  KillThread;
  if GetVMR <> nil then
  begin
    with GetVMR do
    begin
      SetAlpha(3, FAlpha / 100);
      SetOutputRect(3, @FRect);
    end;
  end;
  FVMRVisible := True;
end;

procedure TDVBOSDOutputPin.HideOSD;
begin
  KillThread;
  FFadeThread := TOSDFadeThread.Create(Self, FAlpha, GetVMR);
  FVMRVisible := False;
end;

procedure TDVBOSDOutputPin.KillThread;
begin
  if Assigned(FFadeThread) then
  begin
    FFadeThread.Terminate;
    FFadeThread.Free;
    FFadeThread := nil;
  end;
end;

function TDVBOSDOutputPin.GetVMR: IVMRMixerControl9;
var
  pin_info: TPinInfo;
begin
  if not Assigned(FVMR) and (GetConnected.QueryPinInfo(pin_info) = S_OK)
    then pin_info.pFilter.QueryInterface(IID_IVMRMixerControl9, FVMR);

  Result := FVMR;
end;

procedure TDVBOSDOutputPin.put_AspectRatio(AAspectRatio: TAspectRatio);
begin
  FSharedState.Lock;
  try
    FAspectRatio := AAspectRatio;
    FNeedAspectRatioChange := True;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBOSDOutputPin.put_OSDColor(AColor: TOSDColor);
begin
  FOSDColor := AColor;
  DrawGradient(AColor.StartColor, AColor.EndColor, FBB2, FWidth, FHeight);
end;

procedure TDVBOSDOutputPin.FillRect(AX, AY, AWidth, AHeight: Integer; AColor: Cardinal);
var
  j: Integer;
  i: Integer;
  p: PCardinal;
begin
  if (AX >= FWidth2) or (AX < 0) or (AY >= FHeight2) or (AY < 0) or
     (AWidth < 0) or (AHeight < 0)
    then Exit;

  AWidth := EnsureRange(AWidth, 1, FWidth2 - AX);
  AHeight := EnsureRange(AHeight, 1, FHeight2 - AY);

  for j := AY to AY + AHeight - 1 do
  begin
    i := AWidth;
    p := @PCardinalArray(FBB)[j * FWidth2 + AX];
    while (i > 0) do
    begin
      p^ := AColor;
      inc(p);
      dec(i);
    end;
  end;
end;

procedure TDVBOSDOutputPin.put_OSDChannel(AChannel: String; AAlpha: Integer; ADuration: Integer);
var
  i: Integer;
//  p: PCardinal;
begin
  FSharedState.Lock;
  try
    Move(FBB2^, FBB3^, FSize);
//    FillChar(FBB3^, FSize, $FFFFFFFF);

//    AChannel := AChannel + '_';

    i := Length(AChannel);

    SelectObject(FD3, FFontLarge);
    SetTextColor(FD3, FOSDColor.FontShadowColor);
    ExtTextout(FD3, 42, 117, 0, nil, PChar(AChannel), i, nil);
    SetTextColor(FD3, FOSDColor.FontColor);
    ExtTextout(FD3, 39, 115, 0, nil, PChar(AChannel), i, nil);

    ImageBilinearResize(FBB3, FBB, FWidth, FHeight, FWidth2, FHeight2);

//    p := PCardinal(FBB);
//    for i := 0 to FHeight2 -1 do
//    begin
//      for c := 0 to FWidth2 -1 do
//      begin
//        inc(p);
//        p^ := p^ or $FF000000;
//      end;
//    end;

//    p := PCardinal(FBB);
//    for i := 0 to FHeight2 -80 do
//    begin
//      for c := 0 to FWidth2 -1 do
//      begin
//        p^ := $00000000;
//        inc(p);
//      end;
//    end;

//    p := PCardinal(FBB);
//    inc(p, (FHeight2 - 103) * FWidth2);
//    for i := 0 to 2 do
//    begin
//      for c := 0 to FWidth2 -1 do
//      begin
//        p^ := $50FFFFFF;
//        inc(p);
//      end;
//    end;

//    p := PCardinal(FBB);
//    inc(p, (FHeight2 - 9) * FWidth2);
//    for i := 0 to 8 do
//    begin
//      for c := 0 to FWidth2 -1 do
//      begin
//        p^ := $00000000;
//        inc(p);
//      end;
//    end;


//    FHeight2 := 190;

    FillRect(0, 0, FWidth2, 105, 0);
    FillRect(0, 0, 10, FHeight2, 0);
    FillRect(140, 0, FWidth2 - 140, FHeight2, 0);

    FillRect(10, 105, 130, 5, $50FFFFFF);
    FillRect(10, 105, 5, FHeight2 - 105, $50FFFFFF);
    FillRect(10, FHeight2 - 5, 130, 5, $50FFFFFF);
    FillRect(140, 105, 5, FHeight2 - 105, $50FFFFFF);

    FAlpha := 100 - AAlpha;
    FThisTime := GetTickCount + Int64(2 * 1000);
    FVisible := True;

    FNeedUpdate := True;
  finally
    FSharedState.UnLock;
  end;
end;

procedure TDVBOSDOutputPin.put_OSDRect(ARect: VMR9NormalizedRect);
begin
  FRect := ARect;
end;

end.

