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

unit DVBSourceFilter;

{$I Compiler.inc}

interface

uses
  BaseClass, DirectShow9, DSUtil, Windows, ActiveX, SysUtils, TCPTransportStream,
  MulticastTransportStream, DVBInterface, Classes, TransportStream, MPEGConst,
  MPEGTimeShift, Logger;

const
  CLSID_DVBStreamSource: TGuid = '{A89B83F0-0240-4094-B5E8-9B77536AA5A6}';

type
  TDVBStreamSourcePin = class(TBCBaseOutputPin)
  protected
    FLog: TLogger;
    FDiscontinuity: Boolean;
    FTimeShift: TTimeShift;
    procedure OnReadData(ABuffer: PByte; ASize: Integer);
    procedure OnData(ABuffer: PByte; ASize: Integer);
    procedure UpdateFromSeek;
  public
    constructor Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString; ALogger: TLogger);
    destructor Destroy; override;

    function CheckMediaType(mt: PAMMediaType): HRESULT; override;

    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;
  end;

  TDVBStreamSource = class(TBCBaseFilter, IDVBSourceFilter, IStreamBufferMediaSeeking)
  private
    FLog: TLogger;
    FTimeShiftEnabled: Boolean;
    FPin: TDVBStreamSourcePin;
    FTunerDevice: IUnknown;
  public
    constructor Create(ALogger: TLogger);
    destructor Destroy; override;

    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: Integer; override;
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;
    function Stop: HRESULT; override; stdcall;
    function Pause: HRESULT; override; stdcall;
    function Run(tStart: Int64): HRESULT; override; stdcall;
    procedure OnReadData(ABuffer: PByte; ASize: Integer);

    // IDVBSourceFilter
    procedure get_TSCallback(out ACallback: TBufferCallback);
    procedure put_TunerDevice(ATunerDevice: IUnknown);
    procedure put_Discontinuity;
    // IStreamBufferMediaSeeking
    function GetCapabilities(out pCapabilities: DWORD): HResult; stdcall;
    function CheckCapabilities(var pCapabilities: DWORD): HResult; stdcall;
    function IsFormatSupported(const pFormat: TGUID): HResult; stdcall;
    function QueryPreferredFormat(out pFormat: TGUID): HResult; stdcall;
    function GetTimeFormat(out pFormat: TGUID): HResult; stdcall;
    function IsUsingTimeFormat(const pFormat: TGUID): HResult; stdcall;
    function SetTimeFormat(const pFormat: TGUID): HResult; stdcall;
    function GetDuration(out pDuration: int64): HResult; stdcall;
    function GetStopPosition(out pStop: int64): HResult; stdcall;
    function GetCurrentPosition(out pCurrent: int64): HResult; stdcall;
    function ConvertTimeFormat(out pTarget: int64; pTargetFormat: PGUID; Source: int64; pSourceFormat: PGUID): HResult; stdcall;
    function SetPositions(var pCurrent: int64; dwCurrentFlags: DWORD; var pStop: int64; dwStopFlags: DWORD): HResult; stdcall;
    function GetPositions(out pCurrent, pStop: int64): HResult; stdcall;
    function GetAvailable(out pEarliest, pLatest: int64): HResult; stdcall;
    function SetRate(dRate: double): HResult; stdcall;
    function GetRate(out pdRate: double): HResult; stdcall;
    function GetPreroll(out pllPreroll: int64): HResult; stdcall;
  end;

implementation

uses Math;

(*** TDVBStreamSourcePin ******************************************************)

constructor TDVBStreamSourcePin.Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString; ALogger: TLogger);
begin
  inherited Create('Stream Source Output Pin', Filter, Lock, hr, 'Out');
  FLog := ALogger;
  FTimeShift := TTimeShift.Create;
  if TDVBStreamSource(FFilter).FTimeShiftEnabled
    then FTimeShift.SetFilename('C:\timeshift.ts');
  FTimeShift.OnData := OnData;
  FTimeShift.SetMaximumTime(3600);
end;

destructor TDVBStreamSourcePin.Destroy;
begin
  FTimeShift.Free;
  inherited Destroy;
end;

function TDVBStreamSourcePin.GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult;
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

function TDVBStreamSourcePin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

const
  MAX_BUFFER_SIZE = 512 * 188;

function TDVBStreamSourcePin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
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

    Properties.cbBuffer := MAX_BUFFER_SIZE;

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

procedure TDVBStreamSourcePin.OnData(ABuffer: PByte; ASize: Integer);
var
  sample: IMediaSample;
  buffer: PByte;
  hr: HRESULT;
  size: Integer;
  max_size: Integer;
begin
  {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'received ' + inttostr(ASize) + 'bytes'); {$ENDIF}
//  FLock.Lock;
  try
    if (ASize = 0) or (ABuffer = nil)
      then Exit;

    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'checking if Pin is connected'); {$ENDIF}
    if not IsConnected then
    begin
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'Pin is not connected'); {$ENDIF}
      Exit;
    end;
    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'Pin is connected'); {$ENDIF}

    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'checking if FFilter.State = State_Running'); {$ENDIF}
    if not (FFilter.State = State_Running) then
    begin
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'FFilter.State is not State_Running'); {$ENDIF}
      Exit;
    end;
    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'FFilter.State is State_Running'); {$ENDIF}


    while ASize > 0 do
    begin
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'trying to get Delivery Buffer'); {$ENDIF}
      hr := Self.GetDeliveryBuffer(sample, nil, nil, 0);
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'trying to get Delivery Buffer returned: ' + inttohex(hr, 8)); {$ENDIF}

      if (hr <> S_OK)
        then Exit;

      if (FDiscontinuity) then
      begin
        sample.SetDiscontinuity(True);
        FDiscontinuity := False;

        DeliverBeginFlush;
        DeliverEndFlush;
      end;
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'trying to get Delivery Buffer Pointer'); {$ENDIF}
      hr := sample.GetPointer(buffer);
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'trying to get Delivery Buffer Pointer returned: ' + inttohex(hr, 8)); {$ENDIF}

      if hr <> S_OK
        then Exit;

      max_size := sample.GetSize;
      if max_size <= 0
        then Exit;

      size := IfThen(ASize > max_size, max_size, ASize);

      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'Delivery Buffer has a Size of ' + inttostr(sample.GetSize) + ' and our Buffer Size is: ' + inttostr(ASize)); {$ENDIF}
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'setting Actual Data Length to ' + inttostr(ASize)); {$ENDIF}
      {$IFDEF LOG_PACKETS} hr := {$ENDIF} sample.SetActualDataLength(size);
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'setting Actual Data Length to ' + inttostr(ASize) + ' returned ' + inttohex(hr, 8)); {$ENDIF}
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'moving TS Buffer to Sample Buffer'); {$ENDIF}
      Move(ABuffer^, buffer^, size);
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'delivering Sample to Input Pin'); {$ENDIF}
      {$IFDEF LOG_PACKETS} hr := {$ENDIF} Deliver(sample);
      {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnData (' + inttohex(integer(ABuffer), 8) + ')', 'delivering Sample to Input Pin returned ' + inttohex(hr, 8)); {$ENDIF}

      inc(ABuffer, size);
      dec(ASize, size);      
    end;
  finally
//    FLock.UnLock;
  end;
end;

procedure TDVBStreamSourcePin.OnReadData(ABuffer: PByte; ASize: Integer);
begin
  if TDVBStreamSource(FFilter).FTimeShiftEnabled
    then FTimeShift.ParseBuffer(ABuffer, ASize)
    else OnData(ABuffer, ASize);
end;

procedure TDVBStreamSourcePin.UpdateFromSeek;
begin
//  FLock.Lock;
  try
    FDiscontinuity := True;
    DeliverBeginFlush;
    DeliverEndFlush;
  finally
//    FLock.UnLock;
  end;
end;

(*** TDVBStreamSource *********************************************************)

constructor TDVBStreamSource.Create(ALogger: TLogger);
var
  hr: HRESULT;
begin
//  inherited Create('DVB Stream Source', nil, TBCCritSec.Create, CLSID_StreamBufferSource);
  inherited Create('DVB Stream Source', nil, TBCCritSec.Create, CLSID_DVBStreamSource);

  FLog := ALogger;
  FTimeShiftEnabled := False;

  FPin := TDVBStreamSourcePin.Create('Stream Source Pin', Self, FLock, hr, 'Stream Source Pin', FLog);
  if (hr <> S_OK) and (FPin = nil)
    then hr := E_OUTOFMEMORY;
end;

destructor TDVBStreamSource.Destroy;
begin
  FreeAndNil(FPin);
  inherited;
end;

function TDVBStreamSource.GetPin(n: Integer): TBCBasePin;
begin
  case n of
    0: Result := FPin;
    else Result := nil;
  end;
end;

function TDVBStreamSource.GetPinCount: Integer;
begin
  Result := 1;
end;

function TDVBStreamSource.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IID_IStreamBufferMediaSeeking) then
  begin
    if not FTimeShiftEnabled then
    begin
      Result := E_NOINTERFACE;
      Exit;
    end;
  end;
  Result := inherited NonDelegatingQueryInterface(IID, Obj);

  if (Result <> S_OK) and Assigned(FTunerDevice) then
  begin
    Result := FTunerDevice.QueryInterface(IID, Obj);
  end;
end;

procedure TDVBStreamSource.get_TSCallback(out ACallback: TBufferCallback);
begin
  ACallback := OnReadData;
end;

procedure TDVBStreamSource.put_TunerDevice(ATunerDevice: IUnknown);
begin
  FTunerDevice := ATunerDevice;
end;

procedure TDVBStreamSource.put_Discontinuity;
begin
  FPin.FDiscontinuity := True;
end;

procedure TDVBStreamSource.OnReadData(ABuffer: PByte; ASize: Integer);
begin
//  FLock.Lock;
  try
    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnReadData (' + inttohex(integer(ABuffer), 8) + ')', 'sending Buffer to Output Pin'); {$ENDIF}
    FPin.OnReadData(ABuffer, ASize);
    {$IFDEF LOG_PACKETS} Log(FLog, Self, 'OnReadData (' + inttohex(integer(ABuffer), 8) + ')', 'sending Buffer to Output Pin returned'); {$ENDIF}
  finally
//    FLock.UnLock;
  end;
end;

function TDVBStreamSource.Stop: HRESULT;
begin
  Result := inherited Stop;

  FLock.Lock;
  try
    FPin.FTimeShift.Stopped := True;
  finally
    FLock.UnLock;
  end;
end;

function TDVBStreamSource.Pause: HRESULT;
begin
  Result := inherited Pause;

  FLock.Lock;
  try
    FPin.FTimeShift.Stopped := True;
  finally
    FLock.UnLock;
  end;
end;

function TDVBStreamSource.Run(tStart: Int64): HRESULT;
begin
  Result := inherited Run(tStart);

  FLock.Lock;
  try
    if Result = S_OK then
    begin
      FPin.FTimeShift.Stopped := False;
    end;
  finally
    FLock.UnLock;
  end;
end;

    // IStreamBufferMediaSeeking
function TDVBStreamSource.GetCapabilities(out pCapabilities: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.CheckCapabilities(var pCapabilities: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.IsFormatSupported(const pFormat: TGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.QueryPreferredFormat(out pFormat: TGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.GetTimeFormat(out pFormat: TGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.IsUsingTimeFormat(const pFormat: TGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.SetTimeFormat(const pFormat: TGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.GetStopPosition(out pStop: int64): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.GetCurrentPosition(out pCurrent: int64): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.ConvertTimeFormat(out pTarget: int64; pTargetFormat: PGUID; Source: int64; pSourceFormat: PGUID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.SetRate(dRate: double): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.GetRate(out pdRate: double): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDVBStreamSource.GetPreroll(out pllPreroll: int64): HResult;
begin
  Result := E_NOTIMPL;
end;

// -----------------------------------------------------------------------------

function TDVBStreamSource.GetPositions(out pCurrent, pStop: int64): HResult;
begin
  FPin.FLock.Lock;
  try
    pCurrent := FPin.FTimeShift.ReadStart;
    pStop := FPin.FTimeShift.Start + FPin.FTimeShift.Duration;
  finally
    FPin.FLock.UnLock;
  end;
  Result := S_OK;
end;

function TDVBStreamSource.GetAvailable(out pEarliest, pLatest: int64): HResult;
begin
  FPin.FLock.Lock;
  try
    pEarliest := FPin.FTimeShift.Start;
    pLatest := pEarliest + FPin.FTimeShift.Duration;
  finally
    FPin.FLock.UnLock;
  end;
  Result := S_OK;
end;

function TDVBStreamSource.GetDuration(out pDuration: int64): HResult;
begin
  FPin.FLock.Lock;
  try
    pDuration := FPin.FTimeShift.Duration;
  finally
    FPin.FLock.UnLock;
  end;
  Result := S_OK;
end;

function TDVBStreamSource.SetPositions(var pCurrent: int64; dwCurrentFlags: DWORD; var pStop: int64; dwStopFlags: DWORD): HResult;
begin
  FPin.FLock.Lock;
  try
    FPin.UpdateFromSeek;
    FPin.FTimeShift.SetPosition(pCurrent);
  finally
    FPin.FLock.UnLock;
  end;
  Result := S_OK;
end;

end.
