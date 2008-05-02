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

unit MPEGDemultiplexer;

interface

uses
  BaseClass, DirectShow9, ActiveX, SysUtils, Windows, Classes, DSUtil, MPEGConst,
  MPEGParser, MPEGUtils;

const
  CLSID_TSDemux: TGuid = '{1953A95D-8185-4CD0-9A59-1C93B40D3CAB}';
  OUT_PIN_BUFFER_SIZE = 65536;//8192;

type
  TTSDemuxInputPin = class(TBCBaseInputPin)
  public
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function Receive(pSample: IMediaSample): HRESULT; override; stdcall;
  end;

  TTSDemuxOutputPin = class(TBCBaseOutputPin, IMPEG2PIDMap)
  private
    FList: TList;
    FTSPinName: WideString;
    FIndex: Integer;
    FPinLock: TBCCritSec;
    FNow: Int64;
    procedure PushData(ABuffer: PByte; ASize: Integer; AFlush: Boolean; APTS: Int64; APTSDelta: Int64);
  public
    constructor Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString; APinLock: TBCCritSec);
    destructor Destroy; override;

    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;

    procedure ParseTSPacket(const APacket: PByte; const APID: Integer);
    procedure PushForward;
    function GetStreamTime: Int64;

    // IMPEG2PIDMap
    function MapPID(culPID: ULONG; pulPID: PULONG; MediaSampleContent: TMediaSampleContent): HResult; stdcall;
    function UnmapPID(culPID: ULONG; pulPID: PULONG): HResult; stdcall;
    function EnumPIDMap(out pIEnumPIDMap: IEnumPIDMap): HResult; stdcall;
    function Run(Start: Int64): HRESULT; override;
  end;

  IDCMpeg2Demultiplexer = interface(IMpeg2Demultiplexer)
  ['{33A01E2D-5D28-4FB8-8550-F0C3D5048488}']
    function SetPCRPID(APCRPID: Integer): HRESULT; stdcall;
  end;

  TTSDemux = class(TBCBaseFilter, IMpeg2Demultiplexer, IDCMpeg2Demultiplexer)
  private
    FPinLock: TBCCritSec;
    FOutPinList: TList;
    FInputPin: TTSDemuxInputPin;
    FBuffer: PMyByteArray;
    FBufferPos: Integer;
    FBufferSize: Integer;
    FPCRPID: Integer;
    FPCR: Int64;
  public
    constructor Create;
    destructor Destroy; override;

    function Receive(pSample: IMediaSample): HRESULT;

    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: integer; override;
    // IMpeg2Demultiplexer
    function CreateOutputPin(var pMediaType: TAMMediaType; pszPinName: PWideChar; out ppIPin: IPin): HResult; stdcall;
    function SetOutputPinMediaType(pszPinName: PWideChar; var pMediaType: TAMMediaType): HResult; stdcall;
    function DeleteOutputPin(pszPinName: PWideChar): HResult; stdcall;
    // IDCMpeg2Demultiplexer
    function SetPCRPID(APCRPID: Integer): HRESULT; stdcall;

    property Start_: Int64 read FStart;
  end;

  TBaseMapping = class
  protected
    FPID: Integer;
    FOwner: TTSDemuxOutputPin;
  public
    constructor Create(AOwner: TTSDemuxOutputPin; APID: Integer); virtual;

    procedure ParseTSPacket(APacket: PByte; const APID: Integer); virtual; abstract;
  end;

  TTSMapping = class(TBaseMapping)
  protected
    FBuffer: PByte;
    FBufferSize: Integer;
    FBufferPos: Integer;
  public
    constructor Create(AOwner: TTSDemuxOutputPin; APID: Integer); override;
    destructor Destroy; override;

    procedure ParseTSPacket(APacket: PByte; const APID: Integer); override;
  end;

  TPSIMapping = class(TBaseMapping)
  protected
    FPSIParser: TPSIParser;
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(AOwner: TTSDemuxOutputPin; APID: Integer); override;
    destructor Destroy; override;

    procedure ParseTSPacket(APacket: PByte; const APID: Integer); override;
  end;

  TESMapping = class(TBaseMapping)
  protected
    FFlush: Boolean;
    FStreamID: Integer;
    FContinuity: Integer;
    FBuffer: PByte;
    FBufferSize: Integer;
    FBufferPos: Integer;
    FPTSDTSFlags: Integer;
    FPTS: Int64;
    FFirstPTS: Int64;
    procedure PushPackets(AStart: Boolean);
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(AOwner: TTSDemuxOutputPin; APID: Integer); override;
    destructor Destroy; override;

    procedure ParseTSPacket(APacket: PByte; const APID: Integer); override;
  end;

  TEnumPIDMap = class(TInterfacedObject, IEnumPIDMap)
  private
    FList: TStringList;
    FOwner: TTSDemuxOutputPin;
    FPos: Integer;
  public
    constructor Create(AOwner: TTSDemuxOutputPin);
    destructor Destroy; override;
    // IEnumPIDMap
    function Next(cRequest: ULONG; PIDMap: PPIDMap; out pcReceived: ULONG): HResult; stdcall;
    function Skip(cRecords: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppIEnumPIDMap: IEnumPIDMap): HResult; stdcall;
  end;

implementation

uses Math;

(*** TTSDemuxInputPin *********************************************************)

function TTSDemuxInputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  if not Assigned(mt) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not IsEqualGUID(mt.majortype, MEDIATYPE_Stream) then
  begin
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;

  if not IsEqualGUID(mt.subtype, KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT) and
     not IsEqualGUID(mt.subtype, KSDATAFORMAT_TYPE_MPEG2_TRANSPORT) then
  begin
    Result := VFW_E_INVALIDSUBTYPE;
    Exit;
  end;

  Result := S_OK;
end;

function TTSDemuxInputPin.Receive(pSample: IMediaSample): HRESULT;
begin
  Result := inherited Receive(pSample);

  if (Result = S_OK) and (FFilter.State = State_Running) then
  begin
    Result := TTSDemux(FFilter).Receive(pSample);
  end;
end;

(*** TTSDemuxOutputPin ********************************************************)

constructor TTSDemuxOutputPin.Create(ObjectName: string; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT; const Name: WideString; APinLock: TBCCritSec);
begin
  inherited Create(ObjectName, Filter, Lock, hr, Name);
  FPinLock := APinLock;
  FList := TList.Create;
end;

destructor TTSDemuxOutputPin.Destroy;
begin
  while FList.Count > 0 do
  begin
    TBaseMapping(FList[0]).Free;
    FList.Delete(0);
  end;
  FList.Free;
  inherited Destroy;
end;

function TTSDemuxOutputPin.Run(Start: Int64): HRESULT;
begin
  Result := inherited Run(Start);
  FNow := GetTickCount;
end;

procedure TTSDemuxOutputPin.PushData(ABuffer: PByte; ASize: Integer; AFlush: Boolean; APTS: Int64; APTSDelta: Int64);
var
  sample: IMediaSample;
  buffer: PByte;
  hr: HRESULT;
  t1, t2: Int64;
//  pcr: Int64;
  pts: Int64;
  pts2: Int64;
  s: Integer;
  k: Integer;
begin
  k := 0;

  while (ASize > 0) do
  begin
    hr := Self.GetDeliveryBuffer(sample, nil, nil, 0);
    if hr = S_OK then
    begin
      s := IfThen(ASize > OUT_PIN_BUFFER_SIZE, OUT_PIN_BUFFER_SIZE, ASize);

      Sample.GetPointer(buffer);
      Sample.SetActualDataLength(s);

      if (APTS >= 0) and (k = 0) then
      begin
  //      APTS := APTS * 100;
//        pcr := TTSDemux(FFilter).FPCR * 10000 div 90;
        pts := APTS * 10000 div 90;
        pts2 := APTSDelta * 10000 div 90;

  //      t1 := APTS;
  //      t2 := APTS + 1;
  //      Sample.SetMediaTime(@t1, @t2);

//        t1 := GetStreamTime;
//        t1 := t1 + 400000;
        t1 := pts - pts2;// + 200000;
  //OutputDebugString(PChar(inttostr(pcr - pts)));
        t2 := t1 + 1;
        Sample.SetTime(@t1, @t2);
      end;

      Sample.SetSyncPoint(k = 0);
      Sample.SetPreroll(False);

      Sample.SetDiscontinuity(AFlush and (k = 0));

      inc(k);
      Move(ABuffer^, buffer^, s);
      dec(ASize, s);
      inc(ABuffer, s);
      FInputPin.Receive(sample);
    end;
  end;
end;

procedure TTSDemuxOutputPin.ParseTSPacket(const APacket: PByte; const APID: Integer);
var
  i: Integer;
  mapping: TBaseMapping;
begin
  for i := 0 to FList.Count -1 do
  begin
    mapping := FList[i];
    mapping.ParseTSPacket(APacket, APID);
  end;
end;

procedure TTSDemuxOutputPin.PushForward;
var
  i: Integer;
  mapping: TBaseMapping;
begin
  for i := 0 to FList.Count -1 do
  begin
    mapping := FList[i];
    if mapping is TTSMapping then
    begin
      with TTSMapping(mapping) do
      begin
        if FBufferPos > 0 then
        begin
          PushData(FBuffer, FBufferPos, False, -1, -1);
          FBufferPos := 0;
        end;
      end;
    end;
  end;
end;

function TTSDemuxOutputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

function TTSDemuxOutputPin.GetMediaType(Position: integer; out MediaType: PAMMediaType): HResult;
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

  CopyMediaType(MediaType, @FMT);

  Result := S_OK;
end;

function TTSDemuxOutputPin.DecideBufferSize(Allocator: IMemAllocator; Properties: PAllocatorProperties): HRESULT;
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
      then Properties.cBuffers := 2;

    Properties.cbBuffer := OUT_PIN_BUFFER_SIZE;

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

function TTSDemuxOutputPin.MapPID(culPID: ULONG; pulPID: PULONG; MediaSampleContent: TMediaSampleContent): HResult;
var
  mapping: TBaseMapping;
  i: Integer;
begin
//  Result := E_UNEXPECTED;

  FPinLock.Lock;
  try
    for i := 0 to FList.Count -1 do
    begin
      mapping := FList[i];
      if mapping.FPID = Integer(pulPID^) then
      begin
        Result := S_FALSE;
        Exit;
      end;
    end;
  finally
    FPinLock.UnLock;
  end;

  mapping := nil;

  case MediaSampleContent of
    MEDIA_TRANSPORT_PACKET:
    begin
      mapping := TTSMapping.Create(Self, integer(pulPID^));
    end;
    MEDIA_ELEMENTARY_STREAM:
    begin
      mapping := TESMapping.Create(Self, integer(pulPID^));
    end;
    MEDIA_MPEG2_PSI:
    begin
      mapping := TPSIMapping.Create(Self, integer(pulPID^));
    end;
    MEDIA_TRANSPORT_PAYLOAD:
    begin

    end;
  end;

  FPinLock.Lock;
  try
    if mapping <> nil
      then FList.Add(mapping);
  finally
    FPinLock.UnLock;
  end;

  Result := S_OK;
end;

function TTSDemuxOutputPin.UnmapPID(culPID: ULONG; pulPID: PULONG): HResult;
var
  mapping: TBaseMapping;
  i: Integer;
begin
//  Result := E_UNEXPECTED;

  FPinLock.Lock;
  try
    for i := 0 to FList.Count -1 do
    begin
      mapping := FList[i];
      if mapping.FPID = Integer(pulPID^) then
      begin
        FList.Remove(mapping);
        mapping.Free;
        Result := S_OK;
        Exit;
      end;
    end;
  finally
    FPinLock.UnLock;
  end;

  Result := S_FALSE;
end;

function TTSDemuxOutputPin.EnumPIDMap(out pIEnumPIDMap: IEnumPIDMap): HResult;
begin
  pIEnumPIDMap := TEnumPIDMap.Create(Self);
  Result := S_OK;
end;

function TTSDemuxOutputPin.GetStreamTime: Int64;
begin
  FFilter.StreamTime(Result);
end;

(*** TTSDemux *****************************************************************)

constructor TTSDemux.Create;
var
  hr: HRESULT;
begin
  inherited Create('DC-TS Demux', nil, TBCCritSec.Create, CLSID_TSDemux);
  FOutPinList := TList.Create;
  FPinLock := TBCCritSec.Create;
  FInputPin := TTSDemuxInputPin.Create('TS', Self, FLock, hr, 'TS');
  FBufferSize := 0;
  FBufferPos := 0;
  FBuffer := nil;
  FPCRPID := -1;
  FPCR := -1;
end;

destructor TTSDemux.Destroy;
begin
  FPinLock.Free;
  while FOutPinList.Count > 0 do
  begin
    TTSDemuxOutputPin(FOutPinList[0]).Free;
    FOutPinList.Delete(0);
  end;
  FOutPinList.Free;
  FInputPin.Free;
  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  inherited Destroy;
end;

function TTSDemux.GetPin(n: Integer): TBCBasePin;
begin
  FPinLock.Lock;
  try
    Result := nil;

    if n < 0
      then Exit;

    if n = 0 then
    begin
      Result := FInputPin;
      Exit;
    end;

    dec(n);

    if n >= FOutPinList.Count
      then Exit;

    Result := FOutPinList[n];
  finally
    FPinLock.UnLock;
  end;
end;

function TTSDemux.GetPinCount: integer;
begin
  FPinLock.Lock;
  try
    Result := FOutPinList.Count + 1;
  finally
    FPinLock.UnLock;
  end;
end;

{$DEFINE INCREMENTAL_TS_PARSING}

function TTSDemux.Receive(pSample: IMediaSample): HRESULT;
var
  buffer: PByte;
  size: Integer;
  buffer_size: Integer;
  buffer_pos: Integer;
  pid: Word;
  i: Integer;
  adaptation_field_control: Integer;
  adaptation_field_length: Integer;
begin
  if pSample.IsDiscontinuity = S_OK then
  begin
    FBufferPos := 0;
  end;

  if pSample = nil then
  begin
    Result := S_OK;
    Exit;
  end;

  if pSample.GetPointer(buffer) <> S_OK then
  begin
    Result := S_OK;
    Exit;
  end;

  size := pSample.GetActualDataLength;
  if size <= 0 then
  begin
    Result := S_OK;
    Exit;
  end;

  Result := S_OK;

  buffer_size := FBufferPos + size;
  if buffer_size > FBufferSize then
  begin
    if not Assigned(FBuffer)
      then FBuffer := AllocMem(buffer_size)
      else FBuffer := ReallocMemory(FBuffer, buffer_size);
    FBufferSize := buffer_size;
  end;

  if not Assigned(FBuffer)
    then Exit;

  Move(buffer^, FBuffer[FBufferPos], size);
  buffer_pos := 0;

{$IFDEF INCREMENTAL_TS_PARSING}
  while (buffer_size > TS_PACKET_SIZE) do
{$ELSE}
  while (buffer_size >= TS_PACKET_SIZE) do
{$ENDIF}
  begin
    if (FBuffer[buffer_pos] = TS_PACKET_SYNC_BYTE) then
    begin
      pid := FBuffer[buffer_pos + 1];
      if pid shr 7 = 0 then
      begin
        pid := FBuffer[buffer_pos + 2] or (pid shl 8);
        pid := pid and $1FFF;
        buffer := @FBuffer[buffer_pos];

        if pid = FPCRPID then
        begin
          inc(buffer, 3);
          adaptation_field_control := GetByteBits(buffer, 2, 2);
          if ((adaptation_field_control and $02) > 0) then
          begin
            inc(buffer);
            adaptation_field_length := buffer^;
            inc(buffer);
            if ((adaptation_field_control = 2) and (adaptation_field_length <> 183)) or
               ((adaptation_field_control = 3) and (adaptation_field_length > 182)) then
            begin

            end else
            begin
              if GetByteBits(buffer, 3, 1) = 1 then
              begin
                inc(buffer);
                FPCR := GetLongLongBits(buffer, 0, 33);
//                FPCR := FPCR shl 9;
//                inc(buffer, 4);
//                FPCR := (FPCR * 300) + GetWordBits(buffer, 7, 9);
//                OutputDebugString(PChar('PCR: ' + inttostr(FPCR)));
              end;
            end;
          end;
        end;

        buffer := @FBuffer[buffer_pos];
        FPinLock.Lock;
        try
          for i := 0 to FOutPinList.Count -1
            do TTSDemuxOutputPin(FOutPinList[i]).ParseTSPacket(buffer, pid);
        finally
          FPinLock.UnLock;
        end;
      end;

    {$IFDEF INCREMENTAL_TS_PARSING}
      if (FBuffer[buffer_pos + TS_PACKET_SIZE] = TS_PACKET_SYNC_BYTE) then
      begin
        inc(buffer_pos, TS_PACKET_SIZE - 1);
        dec(buffer_size, TS_PACKET_SIZE - 1);
      end;
    {$ENDIF}
    end;
    inc(buffer_pos);
    dec(buffer_size);
  end;

  FBufferPos := buffer_size;
  Move(FBuffer[buffer_pos], FBuffer[0], FBufferPos);

  FPinLock.Lock;
  try
    for i := 0 to FOutPinList.Count -1
      do TTSDemuxOutputPin(FOutPinList[i]).PushForward;
  finally
    FPinLock.UnLock;
  end;
end;

function TTSDemux.CreateOutputPin(var pMediaType: TAMMediaType; pszPinName: PWideChar; out ppIPin: IPin): HResult;
var
  pin: TTSDemuxOutputPin;
  i: Integer;
begin
  if (pszPinName = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FPinLock.Lock;
  try
    for i := 0 to FOutPinList.Count -1 do
    begin
      pin := FOutPinList[i];
      if (pin.FTSPinName = WideString(pszPinName)) then
      begin
        Result := VFW_E_DUPLICATE_NAME;
        Exit;
      end;
    end;

    pin := TTSDemuxOutputPin.Create(pszPinName, Self, TBCCritSec.Create, Result, pszPinName, FPinLock);
    pin.SetMediaType(@pMediaType);
    pin.FTSPinName := pszPinName;
    pin.FIndex := FOutPinList.Count;
    FOutPinList.Add(pin);
  finally
    FPinLock.UnLock;
  end;
end;

function TTSDemux.SetOutputPinMediaType(pszPinName: PWideChar; var pMediaType: TAMMediaType): HResult;
var
  pin: TTSDemuxOutputPin;
  i: Integer;
begin
  if (pszPinName = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FPinLock.Lock;
  try
    for i := 0 to FOutPinList.Count -1 do
    begin
      pin := FOutPinList[i];
      if (pin.FTSPinName = WideString(pszPinName)) then
      begin
        Result := pin.SetMediaType(@pMediaType);
        Exit;
      end;
    end;

    Result := S_FALSE;
  finally
    FPinLock.UnLock;
  end;
end;

function TTSDemux.DeleteOutputPin(pszPinName: PWideChar): HResult;
var
  pin: TTSDemuxOutputPin;
  i: Integer;
begin
  if (pszPinName = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FPinLock.Lock;
  try
    for i := 0 to FOutPinList.Count -1 do
    begin
      pin := FOutPinList[i];
      if (pin.FTSPinName = WideString(pszPinName)) then
      begin
        FOutPinList.Remove(pin);
        pin.Free;
        Result := S_OK;
        Exit;
      end;
    end;

    Result := S_FALSE;
  finally
    FPinLock.UnLock;
  end;
end;

function TTSDemux.SetPCRPID(APCRPID: Integer): HRESULT;
begin
  FPCRPID := APCRPID;
  FPCR := -1;
  Result := S_OK;
end;

constructor TBaseMapping.Create(AOwner: TTSDemuxOutputPin; APID: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FPID := APID;
end;

constructor TTSMapping.Create(AOwner: TTSDemuxOutputPin; APID: Integer);
begin
  inherited Create(AOwner, APID);
  FBufferSize := 1024 * 64;
  FBuffer := AllocMem(FBufferSize);
  FBufferPos := 0;
end;

destructor TTSMapping.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TTSMapping.ParseTSPacket(APacket: PByte; const APID: Integer);
var
  buffer: PByte;
begin
  if APID = FPID then
  begin
    buffer := FBuffer;
    inc(buffer, FBufferPos);
    move(APacket^, buffer^, TS_PACKET_SIZE);
    inc(FBufferPos, TS_PACKET_SIZE);
  end;
end;

constructor TPSIMapping.Create(AOwner: TTSDemuxOutputPin; APID: Integer);
begin
  inherited Create(AOwner, APID);
  FPSIParser := TPSIParser.Create(nil, APID, OnBuffer, True);
end;

destructor TPSIMapping.Destroy;
begin
  FPSIParser.Free;
  inherited Destroy;
end;

procedure TPSIMapping.ParseTSPacket(APacket: PByte; const APID: Integer);
begin
  if APID = FPID then
  begin
    FPSIParser.ParseTSPacket(APacket);
  end;
end;

procedure TPSIMapping.OnBuffer(ABuffer: PByte; ASize: Integer);
begin
  FOwner.PushData(ABuffer, ASize, False, -1, -1);
end;



constructor TESMapping.Create(AOwner: TTSDemuxOutputPin; APID: Integer);
begin
  inherited Create(AOwner, APID);
  FFlush := True;
  FBufferSize := 1024 * 128;
  FBuffer := AllocMem(FBufferSize);
  FBufferPos := 0;
  FFirstPTS := -1;
end;

destructor TESMapping.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TESMapping.ParseTSPacket(APacket: PByte; const APID: Integer);
var
  transport_error_indicator: Byte;
  payload_unit_start_indicator: Byte;
  adaptation_field_control: Byte;
  continuity_counter: Byte;
  data_bytes: Integer;
  adaptation_field_length: Byte;
  buffer: PByte;
begin
//  if APID = 338 // 337
//    then Exit;

  if (APID = FPID) then
  begin
//    FPES.ParseTSPacket(APacket);
    inc(APacket, 1);

    transport_error_indicator := GetByteBits(APacket, 0, 1);

    if (transport_error_indicator = 1) then
    begin
      Exit;
    end;

    payload_unit_start_indicator := GetByteBits(APacket, 1, 1);
    inc(APacket, 2);
    adaptation_field_control := GetByteBits(APacket, 2, 2);
    continuity_counter := GetByteBits(APacket, 4, 4);

    if (continuity_counter = FContinuity) then
    begin
//OutputDebugString('AAAAAAAAAAAAAASSSSSSS');
//      Exit;
    end;

    if (adaptation_field_control <> 0) and (adaptation_field_control <> 2) then
    begin
      FContinuity := (continuity_counter + 1) and $0F;
    end;

    inc(APacket, 1);

    data_bytes := 184;
    if ((adaptation_field_control and $02) > 0) then
    begin
      adaptation_field_length := APacket^;
      inc(APacket);
      dec(data_bytes);

      if (adaptation_field_control = 2) and (adaptation_field_length <> 183) then
      begin
        // Out of Specs
        Exit;
      end else
      if (adaptation_field_control = 3) and (adaptation_field_length > 182) then
      begin
        // Out of Specs
        Exit;
      end;

      inc(APacket, adaptation_field_length);
      dec(data_bytes, adaptation_field_length);
    end;

    if (data_bytes <= 0)
      then Exit;

    if ((adaptation_field_control and $01) > 0) then
    begin
      if payload_unit_start_indicator = 1 then
      begin
        FContinuity := -1;
        PushPackets(True);

        if FBufferSize < data_bytes then
        begin
          if Assigned(FBuffer) then
          begin
            FBuffer := ReallocMemory(FBuffer, data_bytes);
          end else
          begin
            FBuffer := AllocMem(data_bytes);
          end;
          FBufferSize := data_bytes;
        end;

        Move(APacket^, FBuffer^, data_bytes);
        FBufferPos := data_bytes;
      end else
      begin
        if ((continuity_counter + 1) and $0F = FContinuity) and (FBufferPos > 0) then
        begin
          if (FBufferSize < data_bytes + FBufferPos) then
          begin
            if Assigned(FBuffer) then
            begin
              FBuffer := ReallocMemory(FBuffer, FBufferPos + data_bytes);
            end else
            begin
              FBuffer := AllocMem(FBufferPos + data_bytes);
            end;
            FBufferSize := data_bytes + FBufferPos;
          end;
          buffer := FBuffer;
          inc(buffer, FBufferPos);
          Move(APacket^, buffer^, data_bytes);
          inc(FBufferPos, data_bytes);
//          PushPackets(False);
        end;
      end;
    end;
  end;
end;

//var
//  fs: TFileStream;

procedure TESMapping.PushPackets(AStart: Boolean);
var
  ASize: Integer;
  ABuffer: PByte;
  packet_start_code_prefix: Cardinal;
  PES_packet_length: Word;
  PES_header_data_length: Byte;
begin
//OutputDebugString('AAAAAAAAAA');
  if (FBufferPos <= 0)
    then Exit;

//OutputDebugString('BBBBBBBBBBBBB');
  ABuffer := PByte(FBuffer);
  ASize := FBufferPos;

//OutputDebugString('CCCCCCCCCCCCCC');
  if (ASize < 6)
    then Exit;
//OutputDebugString('DDDDDDDDDDDDDDDD');

  packet_start_code_prefix := GetLongBits(ABuffer, 0, 24);

  if packet_start_code_prefix <> PES_START_CODE_PREFIX
    then Exit;

//OutputDebugString('EEEEEEEEEEEEEEEEE');
  inc(ABuffer, 3);

  FStreamID := ABuffer^;
  inc(ABuffer);

  PES_packet_length := GetWord(ABuffer);
  inc(ABuffer, 2);

  if AStart
    then FPTS := Low(Int64);

  // Buffer increased by 6

  if (FStreamID <> STREAM_ID_PROGRAM_STREAM_MAP) and
     (FStreamID <> STREAM_ID_PADDING_STREAM) and
     (FStreamID <> STREAM_ID_PRIVATE_STREAM_2) and
     (FStreamID <> STREAM_ID_ECM) and
     (FStreamID <> STREAM_ID_EMM) and
     (FStreamID <> STREAM_ID_PROGRAM_STREAM_DIRECTORY) and
     (FStreamID <> STREAM_ID_DSMCC_STREAM) and
     (FStreamID <> STREAM_ID_H_222_1_TYPE_E) and
     (FStreamID <> STREAM_ID_IEC_14496_1_FLEXMUX) then
  begin
//OutputDebugString('FFFFFFFFFFFFFFFFFFFF');
    if (ASize < 9) // or (GetByteBits(ABuffer, 0, 2) <> 2)
      then Exit;

//OutputDebugString('GGGGGGGGGGGGGGGGGGGGGG');
    inc(ABuffer);

    FPTSDTSFlags := GetByteBits(ABuffer, 0, 2);
    inc(ABuffer);
    // Buffer increased by 8

    PES_header_data_length := ABuffer^;
    inc(ABuffer);
    // Buffer increased by 9

    if PES_header_data_length > 0 then
    begin
      if (FPTSDTSFlags = 2) or (FPTSDTSFlags = 3) then
      begin
//        FPTS := 0;
        FPTS := Int64(GetByteBits(ABuffer, 4, 3)) shl 30 ;
        inc(ABuffer);
        FPTS := FPTS or (Int64(GetWordBits(ABuffer, 0, 15)) shl 15);
        inc(ABuffer, 2);
        FPTS := FPTS or (Int64(GetWordBits(ABuffer, 0, 15)));

//        FPTS := (Int64(ABuffer^) and $0E) shl 29;
//        inc(ABuffer);
//        FPTS := FPTS or (Int64(ABuffer^) shl 22);
//        inc(ABuffer);
//        FPTS := FPTS or ((Int64(ABuffer^) and $FE) shl 14);
//        inc(ABuffer);
//        FPTS := FPTS or (Int64(ABuffer^) shl 7);
//        inc(ABuffer);
//        FPTS := FPTS or (Int64(ABuffer^) shr 1);
        dec(ABuffer, 3);
//OutputDebugString(PChar('PTS for ' + inttostr(FPID) + ' - ' + inttostr(FPTS)));
        if FPTSDTSFlags = 3 then
        begin
//          OutputDebugString(PChar('DTS ' + inttostr(FPID)));
          // TODO DTS
        end;
      end;

      inc(ABuffer, PES_header_data_length);
    end;
    // Buffer increased by 9 + PES_header_data_length

    if PES_packet_length = 0 then
    begin
      if ((FStreamID and $F0) <> STREAM_ID_VIDEO_STREAM_MIN) then
      begin
        // PES_packet_length = 0 only allowed for Video Streams !!
      end else
      begin
        if AStart then
        begin
          OnBuffer(ABuffer, ASize - PES_header_data_length - 9);
//          fs.Write(ABuffer^, ASize - PES_header_data_length - 9);
        end;
      end;
    end else
    begin
//OutputDebugString(PChar('HHHHHHHHHHHHHHHHHHHHH ' + inttostr(ASize) + ' - ' + inttostr(PES_packet_length) + ' - ' + inttostr(PES_header_data_length)));
      if (ASize - PES_packet_length - 6 >= 0)
        then  OnBuffer(ABuffer, PES_packet_length - PES_header_data_length - 3);
//          fs.Write(ABuffer^, PES_packet_length - PES_header_data_length - 3);
    end;
  end else
  if (FStreamID = STREAM_ID_PRIVATE_STREAM_2) or
     (FStreamID = STREAM_ID_ECM) or
     (FStreamID = STREAM_ID_EMM) or
     (FStreamID = STREAM_ID_PROGRAM_STREAM_DIRECTORY) or
     (FStreamID = STREAM_ID_DSMCC_STREAM) or
     (FStreamID = STREAM_ID_H_222_1_TYPE_E) or
     (FStreamID = STREAM_ID_IEC_14496_1_FLEXMUX) then
  begin
    OnBuffer(ABuffer, PES_packet_length);
  end else
  if (FStreamID = STREAM_ID_PADDING_STREAM) then
  begin
    OnBuffer(ABuffer, PES_packet_length);
  end;
end;

procedure TESMapping.OnBuffer(ABuffer: PByte; ASize: Integer);
begin
  if FFirstPTS = -1 then
  begin
    FFirstPTS := FPTS;
  end;

//    OutputDebugString(PChar('ES PID: ' + inttostr(FPID) + ' - ' + inttostr(FPTS)));
//  if FPID = 640 then
  begin
//    if FPID = 433 then
//      pes.PTS := pes.PTS + 50000;
//    OutputDebugString(PChar('ES PID: ' + inttostr(FPID) + ' - ' + inttostr(pes.PTS)));
    FOwner.PushData(ABuffer, ASize, FFlush, FPTS, FFirstPTS);
    FFlush := False;
  end;

//  end else
//  if FPID = 225 then
//  begin
//    inc(cnt);
//    if cnt > 100 then
//    begin
////    FOwner.PushData(ABuffer, ASize);
//      cnt := 0;
//    end;
//  end;
end;




constructor TEnumPIDMap.Create(AOwner: TTSDemuxOutputPin);
var
  i: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FList := TStringList.Create;
  for i := 0 to FOwner.FList.Count -1
    do FList.Add(inttostr(TBaseMapping(FOwner.FList[i]).FPID));
  FPos := -1;
end;

destructor TEnumPIDMap.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEnumPIDMap.Next(cRequest: ULONG; PIDMap: PPIDMap; out pcReceived: ULONG): HResult;
begin
  inc(FPos);
  if FPos >= FList.Count then
  begin
    Result := S_FALSE;
    Exit;
  end;

  PIDMap.ulPID := strtoint(FList[FPos]);
  pcReceived := 1;

  Result := S_OK;
end;

function TEnumPIDMap.Skip(cRecords: ULONG): HResult;
begin
  inc(FPos, cRecords);
  Result := S_OK;
end;

function TEnumPIDMap.Reset: HResult;
begin
  FPos := -1;
  Result := S_OK;
end;

function TEnumPIDMap.Clone(out ppIEnumPIDMap: IEnumPIDMap): HResult;
begin
  Result := E_FAIL;
end;

initialization
//  fs := TFileStream.Create('C:\Test.test', fmCreate);
finalization
//  fs.Free;
end.
