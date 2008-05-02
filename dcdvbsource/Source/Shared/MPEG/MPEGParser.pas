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

unit MPEGParser;

{$I Compiler.inc}

interface

{$DEFINE INCREMENTAL_TS_PARSING}

uses
  Classes, SysUtils, MPEGUtils, MPEGConst, Windows;

type
  TPayloadParser = class;

  // ---------------------------------------------------------------------------

  TTSParser = class
  private
    FEnabled: Boolean;
    FOnTSPacket: TPacketCallback;
    FList: TList;
    FBuffer: PMyByteArray;
    FBufferPos: Integer;
    FBufferSize: Integer;
    procedure AddPayloadParser(AParser: TPayloadParser);
    procedure RemovePayloadParser(AParser: TPayloadParser);
    procedure SetOnTSPacket(APacket: TPacketCallback);
  protected
  public
    constructor Create(AEnabled: Boolean = True);
    destructor Destroy; override;

    procedure ParseBuffer(ABuffer: PByte; ASize: Integer);
    procedure Flush;
  published
    property OnMPEGTSPacket: TPacketCallback read FOnTSPacket write SetOnTSPacket;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  // ---------------------------------------------------------------------------

  TPayloadParser = class
  protected
    FBuffer: PMyByteArray;
    FBufferPos: Integer;
    FBufferSize: Integer;
    FContinuity: Integer;
    FPID: Integer;
    FTSParser: TTSParser;
    FEnabled: Boolean;
    FPCR: Int64;
    FPCRFlag: Byte;
    FOnPayloadBuffer: TBufferCallback;
    FOnPCRCallback: TPCRCallback;
    FParseData: Boolean;
    procedure SetTSParser(AParser: TTSParser);
    procedure SetPID(APID: Integer);
    procedure ParseTSPacket(APacket: PByte; APID: Integer); overload;
    procedure SetOnPayloadBuffer(ABuffer: TBufferCallback);
    procedure SetOnPCRCallback(ACallback: TPCRCallback);
    procedure CheckPacket; virtual;
  public
    constructor Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TBufferCallback = nil; AEnabled: Boolean = True); virtual;
    destructor Destroy; override;

    procedure ParseTSPacket(APacket: PByte); overload; virtual;
    procedure Flush; virtual;
  published
    property TSParser: TTSParser read FTSParser write SetTSParser;
    property PID: Integer read FPID write SetPID;
    property Enabled: Boolean read FEnabled write FEnabled;
    property PCR: Int64 read FPCR write FPCR;
    property PCRFlag: Byte read FPCRFlag write FPCRFlag;
    property OnPCRCallback: TPCRCallback read FOnPCRCallback write SetOnPCRCallback;
    property ParseData: Boolean read FParseData write FParseData;
  end;

  // ---------------------------------------------------------------------------

  TPSIParser = class(TPayloadParser)
  protected
    procedure CheckPacket; override;
  published
    property OnPSIBuffer: TBufferCallback read FOnPayloadBuffer write SetOnPayloadBuffer;
  end;

  // ---------------------------------------------------------------------------

  TBasePESParser = class(TPayloadParser)
  protected
    FPTSDTSFlags: Byte;
    FStreamID: Integer;
    FPTS: Int64;
    procedure CheckPacket; override;
  public
    procedure Flush; override;
  published
    property StreamID: Integer read FStreamID write FStreamID;
    property PTS: Int64 read FPTS write FPTS;
    property PTSDTSFlags: Byte read FPTSDTSFlags write FPTSDTSFlags;
  end;

  // ---------------------------------------------------------------------------

  TPESParser = class(TBasePESParser)
  published
    property OnPESBuffer: TBufferCallback read FOnPayloadBuffer write SetOnPayloadBuffer;
  end;

  // ---------------------------------------------------------------------------

  TTeletextParser = class(TBasePESParser)
  protected
    FOnTeletextLine: TTeletextLineCallback;
    procedure SetOnTeletextLine(ACallback: TTeletextLineCallback);
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TTeletextLineCallback = nil; AEnabled: Boolean = True); reintroduce;
    destructor Destroy; override;
  published
    property OnTeletextLine: TTeletextLineCallback read FOnTeletextLine write SetOnTeletextLine;
  end;

  // ---------------------------------------------------------------------------

  TSubtitleParser = class(TBasePESParser)
  protected
    FOnPESBuffer: TBufferCallback;
    procedure SetOnPESBuffer(ACallback: TBufferCallback);
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TBufferCallback = nil; AEnabled: Boolean = True); reintroduce;
    destructor Destroy; override;
  published
    property OnPESBuffer: TBufferCallback read FOnPESBuffer write SetOnPESBuffer;
  end;

implementation

uses Math;

(*** TTSParser ****************************************************************)

constructor TTSParser.Create(AEnabled: Boolean = True);
begin
  inherited Create;
  FList := TList.Create;
  FOnTSPacket := nil;
  FEnabled := AEnabled;
  FBufferSize := 512 * 1024;
  FBuffer := AllocMem(FBufferSize);
  Flush;
end;

destructor TTSParser.Destroy;
var
  i: Integer;
begin
  // Remove Childs
  i := FList.Count;
  while (i > 0) do
  begin
    dec(i);
    TPayloadParser(FList[i]).SetTSParser(nil);
  end;
  FList.Free;
  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  inherited Destroy;
end;

procedure TTSParser.AddPayloadParser(AParser: TPayloadParser);
begin
  FList.Add(AParser);
end;

procedure TTSParser.RemovePayloadParser(AParser: TPayloadParser);
begin
  FList.Remove(AParser);
end;

procedure TTSParser.SetOnTSPacket(APacket: TPacketCallback);
begin
  FOnTSPacket := APacket;
end;

procedure TTSParser.Flush;
begin
  FBufferPos := 0;
end;

procedure TTSParser.ParseBuffer(ABuffer: PByte; ASize: Integer);
const
  MAX_SIZE = 1024 * 1024;
var
  buffer_size: Integer;
  buffer_pos: Integer;
  pid: Word;
  i: Integer;
  payload: TPayloadParser;
  buffer: PByte;
  s: Integer;
begin
  // check if Parameters are valid
  if not FEnabled or not Assigned(ABuffer)
    then Exit;

  while ASize > 0 do
  begin
    s := IfThen(ASize > MAX_SIZE, MAX_SIZE, ASize);

    buffer_size := FBufferPos + s;
    if buffer_size > FBufferSize then
    begin
      if not Assigned(FBuffer)
        then FBuffer := AllocMem(buffer_size)
        else FBuffer := ReallocMemory(FBuffer, buffer_size);
      FBufferSize := buffer_size;
    end;

    if not Assigned(FBuffer)
      then Exit;

    Move(ABuffer^, FBuffer[FBufferPos], s);
    buffer_pos := 0;

  {$IFDEF INCREMENTAL_TS_PARSING}
    while (buffer_size > TS_PACKET_SIZE) do
  {$ELSE}
    while (buffer_size >= TS_PACKET_SIZE) do
  {$ENDIF}
    begin
      if (FBuffer[buffer_pos] = TS_PACKET_SYNC_BYTE) then
      begin
        pid := GetWordBits(@FBuffer[buffer_pos + 1], 3, 13);
        buffer := @FBuffer[buffer_pos];

        if Assigned(FOnTSPacket)
          then FOnTSPacket(buffer, pid);

        for i := 0 to FList.Count -1 do
        begin
          payload := TPayloadParser(FList[i]);
          if (payload.FPID = pid) then
          begin
            payload.ParseTSPacket(buffer, pid);
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

    dec(ASize, s);
    inc(ABuffer, s);
  end;
end;

(*** TPayloadParser ***********************************************************)

constructor TPayloadParser.Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TBufferCallback = nil; AEnabled: Boolean = True);
begin
  inherited Create;
  FOnPCRCallback := nil;
  FEnabled := AEnabled;
  FBufferSize := 0;
  FBuffer := nil;
  FOnPayloadBuffer := ACallback;
  FPID := APID;
  SetTSParser(ATSParser);
  FParseData := True;
  Flush;
end;

destructor TPayloadParser.Destroy;
begin
  // Remove ourself from Parent
  SetTSParser(nil);
  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  inherited Destroy;
end;

procedure TPayloadParser.SetPID(APID: Integer);
begin
  if (APID = FPID)
    then Exit;

  FPID := APID;
  Flush;
end;

procedure TPayloadParser.SetTSParser(AParser: TTSParser);
begin
  if (AParser = FTSParser)
    then Exit;

  if Assigned(FTSParser) then
  begin
    FTSParser.RemovePayloadParser(Self);
    FTSParser := nil;
  end;
  if Assigned(AParser) then
  begin
    FTSParser := AParser;
    FTSParser.AddPayloadParser(Self);
  end;
end;

procedure TPayloadParser.Flush;
begin
  FBufferPos := 0;
  FContinuity := -1;
  FPCR := -1;
  FPCRFlag := 0;
end;

procedure TPayloadParser.SetOnPayloadBuffer(ABuffer: TBufferCallback);
begin
  FOnPayloadBuffer := ABuffer;
end;

procedure TPayloadParser.SetOnPCRCallback(ACallback: TPCRCallback);
begin
  FOnPCRCallback := ACallback;
end;

procedure TPayloadParser.ParseTSPacket(APacket: PByte);
var
  pid: Integer;
begin
  if not FEnabled or not Assigned(APacket) or (APacket^ <> TS_PACKET_SYNC_BYTE)
    then Exit;

  inc(APacket);
  pid := GetWordBits(APacket, 3, 13);
  dec(APacket);

  ParseTSPacket(APacket, pid);
end;

procedure TPayloadParser.ParseTSPacket(APacket: PByte; APID: Integer);

  procedure CheckBufferSize(ASize: Integer);
  begin
    if FBufferSize < ASize then
    begin
      if Assigned(FBuffer) then
      begin
        FBuffer := ReallocMemory(FBuffer, ASize);
      end else
      begin
        FBuffer := AllocMem(ASize);
      end;
      FBufferSize := ASize;
    end;
  end;

var
  transport_error_indicator: Byte;
  payload_unit_start_indicator: Byte;
  adaptation_field_control: Byte;
  continuity_counter: Byte;
  data_bytes: Integer;
  adaptation_field_length: Byte;
  pointer_field: PByte;
  pointer_field_size: Integer;
begin
  if not FEnabled or (FPID <> APID) or not IsValidPID(APID)
    then Exit;

  inc(APacket, 1);

  transport_error_indicator := GetByteBits(APacket, 0, 1);

  if (transport_error_indicator = 1) then
  begin
//OutputDebugString(PChar('transport_error_indicator ' + inttostr(APID)));
    Exit;
  end;

  payload_unit_start_indicator := GetByteBits(APacket, 1, 1);
  inc(APacket, 2);
  adaptation_field_control := GetByteBits(APacket, 2, 2);
  continuity_counter := GetByteBits(APacket, 4, 4);

  if ((adaptation_field_control and $01) = 0) then
  begin
    if (FContinuity = -1) then
    begin
//OutputDebugString(PChar('FContinuity ' + inttostr(APID)));
//      Exit;
    end;
  end else
  begin
    if (FContinuity <> -1) and (continuity_counter <> FContinuity) then
    begin
//OutputDebugString(PChar('FContinuity ' + inttostr(APID)));
      Exit;
    end;
  end;

  FContinuity := (continuity_counter + 1) and $0F;

  inc(APacket, 1);

  data_bytes := 184;
  if ((adaptation_field_control and $02) > 0) then
  begin
    adaptation_field_length := APacket^;
    inc(APacket);
    dec(data_bytes);

    if (adaptation_field_control = 2) and (adaptation_field_length <> 183) then
    begin
//OutputDebugString(PChar('adaptation_field_control specs 1'));
      // Out of Specs
      Exit;
    end else
    if (adaptation_field_control = 3) and (adaptation_field_length > 182) then
    begin
//OutputDebugString(PChar('adaptation_field_control specs 2'));
      // Out of Specs
      Exit;
    end;

    FPCRFlag := GetByteBits(APacket, 3, 1);
    if FPCRFlag = 1 then
    begin
      inc(APacket);
      FPCR := GetLongLongBits(APacket, 0, 33);
      dec(APacket);
      if Assigned(FOnPCRCallback)
        then FOnPCRCallback(FPCR);
    end;

    inc(APacket, adaptation_field_length);
    dec(data_bytes, adaptation_field_length);
  end;


//  if ((adaptation_field_control and $01) > 0) then
//  begin
//    FContinuity := (continuity_counter + 1) and $0F;
//  end;

  if (data_bytes <= 0) or not FParseData then
  begin
    Exit;
  end;

  if ((adaptation_field_control and $01) > 0) then
  begin
    if payload_unit_start_indicator = 1 then
    begin
      // The Pointer Field can contain Data for the previous PSI Payload!
      pointer_field_size := APacket^;
      if (FBufferPos > 0) and (Self is TPSIParser) and (pointer_field_size > 0) then
      begin
        pointer_field := APacket;
        inc(pointer_field);

        CheckBufferSize(FBufferPos + pointer_field_size);
        Move(pointer_field^, FBuffer[FBufferPos], pointer_field_size);
        FBufferPos := FBufferPos + pointer_field_size;
        CheckPacket;
      end;
      Flush;

      CheckBufferSize(data_bytes);
      Move(APacket^, FBuffer[0], data_bytes);
      FBufferPos := data_bytes;
      CheckPacket;
    end else
    begin
      CheckBufferSize(data_bytes + FBufferPos);
      Move(APacket^, FBuffer[FBufferPos], data_bytes);
      inc(FBufferPos, data_bytes);
      CheckPacket;
    end;
  end;
end;

procedure TPayloadParser.CheckPacket;
begin

end;

(*** TPSIParser ***************************************************************)

procedure TPSIParser.CheckPacket;
var
  ASize, pos: Integer;
  section_length: Integer;
  ABuffer, buffer: PByte;
  buffer_size: Integer;
  section_syntax_indicator: Byte;
begin
  if not Assigned(FOnPayloadBuffer) or (FBufferPos <= 0)
    then Exit;

  ABuffer := PByte(FBuffer);
  ASize := FBufferPos;

  // Pointer Field
  pos := ABuffer^ + 1;

  // We need at least 3 valid bytes
  if (pos + 3 >= ASize)
    then Exit;

  inc(ABuffer, pos);
  dec(ASize, pos);
  buffer := ABuffer;

  inc(buffer);

  section_length := GetWordBits(buffer, 4, 12);
  section_syntax_indicator := GetByteBits(buffer, 0, 1);
  buffer_size := section_length + 3;

  // check if the Size in the header matches the Size of the Buffer
  // > because of stuffing bytes ???
  if buffer_size > ASize then
  begin
    Exit;
  end;

  // check if section has a CRC, if so, check it
  if (section_syntax_indicator = 1) and not CheckPSICRC(ABuffer, buffer_size) then
  begin
      if FPID = 650
        then OutputDebugString(PChar('Error Syntax'));
    // not valid, so Exit !
    // TODO remove CRC checks in PSI Sections with CRC Validation or better make it optional !
    Exit;
  end;

  FOnPayloadBuffer(ABuffer, buffer_size);
  FBufferPos := 0;
end;

(*** TBasePESParser ***********************************************************)

procedure TBasePESParser.Flush;
begin
  inherited Flush;
  FStreamID := -1;
  FPTS := -1;
  FPTSDTSFlags := 0;
end;

procedure TBasePESParser.CheckPacket;
var
  ASize: Integer;
  ABuffer: PByte;
  packet_start_code_prefix: Cardinal;
  PES_packet_length: Word;
  PES_header_data_length: Byte;
begin
  if not Assigned(FOnPayloadBuffer) or (FBufferPos <= 0)
    then Exit;

  ABuffer := PByte(FBuffer);
  ASize := FBufferPos;

  if (ASize < 6)
    then Exit;

  packet_start_code_prefix := GetLongBits(ABuffer, 0, 24);

  if packet_start_code_prefix <> PES_START_CODE_PREFIX
    then Exit;

  inc(ABuffer, 3);

  FStreamID := ABuffer^;
  inc(ABuffer);

  PES_packet_length := GetWord(ABuffer);
  inc(ABuffer, 2);

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
    if (ASize < 9) // or (GetByteBits(ABuffer, 0, 2) <> 2)
      then Exit;

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
        FPTS := 0;
        FPTS := Int64(GetByteBits(ABuffer, 4, 3)) shl 30 ;
        inc(ABuffer);
        FPTS := FPTS or (Int64(GetWordBits(ABuffer, 0, 15)) shl 15);
        inc(ABuffer, 2);
        FPTS := FPTS or (Int64(GetWordBits(ABuffer, 0, 15)));
        dec(ABuffer, 3);
        if FPTSDTSFlags = 3 then
        begin
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
        FOnPayloadBuffer(ABuffer, ASize - PES_header_data_length - 9);
      end;
    end else
    begin
      if (ASize - PES_packet_length - 6 >= 0)
        then FOnPayloadBuffer(ABuffer, PES_packet_length - PES_header_data_length - 3);
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
    FOnPayloadBuffer(ABuffer, PES_packet_length);
  end else
  if (FStreamID = STREAM_ID_PADDING_STREAM) then
  begin
    FOnPayloadBuffer(ABuffer, PES_packet_length);
  end;
end;

(*** TTeletextParser **********************************************************)

constructor TTeletextParser.Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TTeletextLineCallback = nil; AEnabled: Boolean = True);
begin
  inherited Create(ATSParser, APID, OnBuffer, AEnabled);
  FOnTeletextLine := ACallback;
end;

destructor TTeletextParser.Destroy;
begin
  inherited Destroy;
end;

procedure TTeletextParser.SetOnTeletextLine(ACallback: TTeletextLineCallback);
begin
  FOnTeletextLine := ACallback;
end;

procedure TTeletextParser.OnBuffer(ABuffer: PByte; ASize: Integer);
var
  c, i: Integer;
  magazine, line: Byte;
  b: PTeletextLine;
  buffer: PMyByteArray;
begin
  if not Assigned(FOnTeletextLine)
    then Exit;

  if (ASize < 10) or (FStreamID <> STREAM_ID_PRIVATE_STREAM_1) then
  begin
    Exit;
  end;

  buffer := PMyByteArray(ABuffer);

  if (buffer[0] < $10) or (buffer[0] > $1F)
    then Exit;

  c := 1;

  while (c < ASize) do
  begin
    if (buffer[c] = $2) or (buffer[c] = $3) then
    begin
      for i := c + 2 to c + 46 -1 do
        buffer[i] := BIT_REVERSE_TABLE[buffer[i]];

      magazine := HammingDecode(@buffer[c + 4]);
      line := (magazine shr 3) and $1F;
      magazine := magazine and $07;

      if (magazine = 0)
        then magazine := 8;

      b := @buffer[c + 6];

      FOnTeletextLine(magazine, line, b);
    end;

    inc(c, 46);
  end;
end;

(*** TSubtitleParser **********************************************************)

constructor TSubtitleParser.Create(ATSParser: TTSParser = nil; APID: Integer = -1; ACallback: TBufferCallback = nil; AEnabled: Boolean = True);
begin
  inherited Create(ATSParser, APID, OnBuffer, AEnabled);
  FOnPESBuffer := ACallback;
end;

destructor TSubtitleParser.Destroy;
begin
  inherited Destroy;
end;

procedure TSubtitleParser.SetOnPESBuffer(ACallback: TBufferCallback);
begin
  FOnPESBuffer := ACallback;
end;

procedure TSubtitleParser.OnBuffer(ABuffer: PByte; ASize: Integer);
begin
  if not Assigned(FOnPESBuffer)
    then Exit;

  if (FStreamID <> STREAM_ID_PRIVATE_STREAM_1) then
  begin
    Exit;
  end;

  FOnPESBuffer(ABuffer, ASize);
end;

end.
