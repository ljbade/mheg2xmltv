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

unit MPEGUtils;

{$I Compiler.inc}

interface

uses
  MPEGConst, MPEGDescriptors, MPEGSections, SysUtils, DateUtils, MHPConst,
  MHPDescriptors, DVBConst, Graphics;

  procedure dbg(str: String);

  function SwapWord(Value: Word): Word;
  function SwapLong(Value: Cardinal): Cardinal;
  function SwapLongLong(Value: Int64): Int64;

  function GetWord(Value: PByte): Word; {$IFDEF VER170} inline;{$ENDIF}
  function GetLong(Value: PByte): Cardinal; {$IFDEF VER170} inline;{$ENDIF}
  function GetLongLong(Value: PByte): Int64; {$IFDEF VER170} inline;{$ENDIF}

  function GetUTCTime(Value: PByte): TUTCTime; {$IFDEF VER170} inline;{$ENDIF}
  function GetMJDTime(Value: PByte): TMJDTime; {$IFDEF VER170} inline;{$ENDIF}
  function GetBCDTime(Value: PByte): TBCDTime; {$IFDEF VER170} inline;{$ENDIF}

  function GetDateTimeFromUTCTime(const ATime: TUTCTime): TDateTime; {$IFDEF VER170} inline;{$ENDIF}
  function GetDateTimeFromMJDTime(const ATime: TMJDTime): TDateTime; {$IFDEF VER170} inline;{$ENDIF}
  function GetDateTimeFromBCDTime(const ATime: TBCDTime): TDateTime; {$IFDEF VER170} inline;{$ENDIF}
  function ApplyTimeCorrection(const StartTime: TUTCTime; MinutesCorrection: Integer): TUTCTime; {$IFDEF VER170} inline;{$ENDIF}
  function IsCurrentProgram(const ANow: TDateTime; const StartTime: TDateTime; const Stoptime: TDateTime): Boolean; {$IFDEF VER170} inline;{$ENDIF}
  function GetUTCTimeString(const UTCTime: TUTCTime): String;
  function GetMJDTimeString(const MJDTime: TMJDTime): String;
  function GetBCDTimeString(const BCDTime: TBCDTime): String;

  function GetByteBits(Value: PByte; Offset: Byte; Count: Byte): Byte; {$IFDEF VER170} inline;{$ENDIF}
  function GetWordBits(Value: PByte; Offset: Byte; Count: Byte): Word; {$IFDEF VER170} inline;{$ENDIF}
  function GetLongBits(Value: PByte; Offset: Byte; Count: Byte): Cardinal; {$IFDEF VER170} inline;{$ENDIF}
  function GetLongLongBits(Value: PByte; Offset: Byte; Count: Byte): Int64; {$IFDEF VER170} inline;{$ENDIF}

  function CheckPSICRC(ABuffer: PByte; ALength: Cardinal): Boolean;
  function IsValidPID(APID: Integer): Boolean; {$IFDEF VER170} inline;{$ENDIF}

  function GetBandwidth(AValue: Byte): Byte; {$IFDEF VER170} inline;{$ENDIF}
  function GetTerrestrialConstellation(AValue: Byte): TTerrestrialConstellation; {$IFDEF VER170} inline;{$ENDIF}
  function GetTerrestrialCodeRate(AValue: Byte): TTerrestrialCodeRate; {$IFDEF VER170} inline;{$ENDIF}
  function GetTerrestrialTransmissionMode(AValue: Byte): TTerrestrialTransmissionMode; {$IFDEF VER170} inline;{$ENDIF}

  function GetRunningStatus(AValue: Byte): TRunningStatus; {$IFDEF VER170} inline;{$ENDIF}

  function GetISO639LanguageDescriptorAudioType(AValue: Byte): TISO639LanguageDescriptorAudioType; {$IFDEF VER170} inline;{$ENDIF}

  function GetProgrammeIdentificationLabel(AValue: PByte): TProgrammeIdentificationLabel; {$IFDEF VER170} inline;{$ENDIF}
  function GetFECInner(AValue: Byte): TFECInner; {$IFDEF VER170} inline;{$ENDIF}
  function GetFECOuter(AValue: Byte): TFECOuter; {$IFDEF VER170} inline;{$ENDIF}
  function GetSatelliteModulation(AValue: Byte): TSatelliteModulation; {$IFDEF VER170} inline;{$ENDIF}
  function GetCableModulation(AValue: Byte): TCableModulation; {$IFDEF VER170} inline;{$ENDIF}

  function GetDescriptorIDString(AValue: Byte): String;

  function GetStreamContentValid(cn1, cn2: Byte): Boolean;
  function GetContentDescriptionValid(cn1, cn2: Byte): Boolean;

  function GetStreamTypeString(AValue: Byte): String;
  function GetServiceTypeString(AValue: Byte): String;
  function GetISO639LanguageDescriptorAudioTypeString(AValue: TISO639LanguageDescriptorAudioType): String;
  function GetTerrestrialPriorityString(AValue: Byte): String;
  function GetConstellationString(AValue: TTerrestrialConstellation): String;
  function GetHierarchyInformationString(AValue: TTerrestrialHierarchyInformation): String;
  function GetTTerrestrialCodeRateString(AValue: TTerrestrialCodeRate): String;
  function GetGuardIntervalString(AValue: TTerrestrialGuardInterval): String;
  function GetTransmissionModeString(AValue: TTerrestrialTransmissionMode): String;
  function GetHexString(AValue: PByte; Count: Integer; Seperator: Boolean = False): String;
  function GetRunningStatusString(AValue: TRunningStatus): String;
  function GetTeletextTypeString(AValue: Byte): String;
  function GetDataServiceIDString(AValue: Byte): String;
  function GetContentDescriptionString(cn1, cn2: Byte): String;
  function GetStreamContentString(cn1, cn2: Byte): String;
  function GetParentalRatingString(AValue: Byte): String;
  function GetLinkageTypeString(AValue: Byte): String;
  function GetSatellitePolarizationString(AValue: TSatellitePolarization): String;
  function GetSatelliteModulationString(AValue: TSatelliteModulation): String;
  function GetFECInnerString(AValue: TFECInner): String;
  function GetFECOuterString(AValue: TFECOuter): String;
  function GetCableModulationString(AValue: TCableModulation): String;
  function GetDataAlignementString(AValue: Byte): String;
  function GetCAIDString(AValue: Integer): String;

  procedure PrintDescriptors(Descriptors: TDescriptorList; Space: String = '');
  procedure PrintPAT(PAT: TProgramAssociationSection);
  procedure PrintPMT(PMT: TProgramMapSection);
  procedure PrintNIT(NIT: TNetworkInformationSection);
  procedure PrintSDT(SDT: TServiceDescriptionSection);
  procedure PrintBAT(BAT: TBouquetAssociationSection);
  procedure PrintEIT(EIT: TEventInformationSection);
  procedure PrintCAT(CAT: TConditionalAccessSection);
  procedure PrintPremiereCIT(CIT: TPremiereContentInformationSection);

  function HexToDec(AValue: Cardinal): Cardinal; {$IFDEF VER170} inline;{$ENDIF}

  function IsEqualTransactionID(ID1: Cardinal; ID2: Cardinal): Boolean;
  function GetTransactionID(AID: Cardinal): Cardinal;

  function HammingDecode(Value: PByteArray): Byte;
  function GetVTXChar(t: Char; Lang: Integer): Char;
  function GetTeletextColor(Index: Integer): TColor;
  procedure UnParityTeletextLine(Line: PTeletextLine);
  procedure SFree(var AClass);

implementation

uses
  MHPUtils, DVBUtils, DVBDescriptors, Windows;

procedure dbg(str: String);
begin
  OutputDebugString(pchar(str));
end;

function SwapWord(Value: Word): Word;
begin
{$IFDEF BIG_ENDIAN}
  Result := Value;
{$ELSE}
  Result := ((Value and $FF00) shr 8) or ((Value and $00FF) shl 8);
{$ENDIF}
end;

function SwapLong(Value: Cardinal): Cardinal;
begin
{$IFDEF BIG_ENDIAN}
  Result := Value;
{$ELSE}
  Result := ((Value and $FF000000) shr 24) or ((Value and $00FF0000) shr 8)  or
            ((Value and $0000FF00) shl 8)  or ((Value and $000000FF) shl 24);
{$ENDIF}
end;

function SwapLongLong(Value: Int64): Int64;
begin
{$IFDEF BIG_ENDIAN}
  Result := Value;
{$ELSE}
  PByteArray(@Result)^[0] := PByteArray(@Value)[7];
  PByteArray(@Result)^[1] := PByteArray(@Value)[6];
  PByteArray(@Result)^[2] := PByteArray(@Value)[5];
  PByteArray(@Result)^[3] := PByteArray(@Value)[4];
  PByteArray(@Result)^[4] := PByteArray(@Value)[3];
  PByteArray(@Result)^[5] := PByteArray(@Value)[2];
  PByteArray(@Result)^[6] := PByteArray(@Value)[1];
  PByteArray(@Result)^[7] := PByteArray(@Value)[0];
{$ENDIF}
end;

function GetWord(Value: PByte): Word;
begin
  Result := SwapWord(PWord(Value)^);
end;

function GetLong(Value: PByte): Cardinal;
begin
  Result := SwapLong(PCardinal(Value)^);
end;

function GetLongLong(Value: PByte): Int64;
begin
  Result := SwapLongLong(PInt64(Value)^);
end;

function GetUTCTime(Value: PByte): TUTCTime;
var
  buffer: array[0..4] of Byte;
  t: Int64;
  y: Int64;
  m: Int64;
  d: Int64;
  k: Int64;
begin
{$IFDEF BIG_ENDIAN}
  buffer[0] := PByteArray(Value)[0];
  buffer[1] := PByteArray(Value)[1];
  buffer[2] := PByteArray(Value)[2];
  buffer[3] := PByteArray(Value)[3];
  buffer[4] := PByteArray(Value)[4];
{$ELSE}
  buffer[0] := PByteArray(Value)[4];
  buffer[1] := PByteArray(Value)[3];
  buffer[2] := PByteArray(Value)[2];
  buffer[3] := PByteArray(Value)[1];
  buffer[4] := PByteArray(Value)[0];
{$ENDIF}
  t := buffer[3] or (Cardinal(buffer[4]) shl 8);
  y := Trunc((t - 15078.2) / 365.25);
  m := Trunc((t - 14956.1 - Trunc(y * 365.25)) / 30.6001);
  d := t - 14956 - Trunc(y * 365.25) - Trunc(m * 30.6001);
  if (m = 14) or (m = 15)
    then k := 1
    else k := 0;
  y := y + k + 1900;
  m := m - 1 - k * 12;
  Result.Year := y;
  Result.Month := m;
  Result.Day := d;
  Result.Time.Hour := HexToDec(buffer[2]);
  if Result.Time.Hour > 23
    then Result.Time.Hour := 0;
  Result.Time.Minute := HexToDec(buffer[1]);
  if Result.Time.Minute > 59
    then Result.Time.Minute := 0;
  Result.Time.Second := HexToDec(buffer[0]);
  if Result.Time.Second > 59
    then Result.Time.Second := 0;
end;

function GetMJDTime(Value: PByte): TMJDTime;
var
  t: Int64;
  y: Int64;
  m: Int64;
  d: Int64;
  k: Int64;
begin
{$IFDEF BIG_ENDIAN}
  t := PByteArray(Value)[0] or (Cardinal(PByteArray(Value)[1]) shl 8);
{$ELSE}
  t := PByteArray(Value)[1] or (Cardinal(PByteArray(Value)[0]) shl 8);
{$ENDIF}
  y := Trunc((t - 15078.2) / 365.25);
  m := Trunc((t - 14956.1 - Trunc(y * 365.25)) / 30.6001);
  d := t - 14956 - Trunc(y * 365.25) - Trunc(m * 30.6001);
  if (m = 14) or (m = 15)
    then k := 1
    else k := 0;
  y := y + k + 1900;
  m := m - 1 - k * 12;
  Result.Year := y;
  Result.Month := m;
  Result.Day := d;
end;

function GetBCDTime(Value: PByte): TBCDTime;
begin
{$IFNDEF BIG_ENDIAN}
  Result.Hour   := HexToDec(PByteArray(Value)[0]);
  Result.Minute := HexToDec(PByteArray(Value)[1]);
  Result.Second := HexToDec(PByteArray(Value)[2]);
{$ELSE}
  Result.Hour   := HexToDec(PByteArray(Value)[2]);
  Result.Minute := HexToDec(PByteArray(Value)[1]);
  Result.Second := HexToDec(PByteArray(Value)[0]);
{$ENDIF}
end;

function GetDateTimeFromUTCTime(const ATime: TUTCTime): TDateTime;
begin
  if not TryEncodeDateTime(ATime.Year, ATime.Month, ATime.Day, ATime.Time.Hour, ATime.Time.Minute, ATime.Time.Second, 0, Result)
    then Result := 0;
end;

function GetDateTimeFromMJDTime(const ATime: TMJDTime): TDateTime;
begin
  if not TryEncodeDateTime(ATime.Year, ATime.Month, ATime.Day, 0, 0, 0, 0, Result)
    then Result := 0;
end;

function GetDateTimeFromBCDTime(const ATime: TBCDTime): TDateTime;
begin
  Result := 0;
  Result := IncSecond(Result, ATime.Second);
  Result := IncMinute(Result, ATime.Minute);
  Result := IncHour(Result, ATime.Hour);
end;

function ApplyTimeCorrection(const StartTime: TUTCTime; MinutesCorrection: Integer): TUTCTime;
var
  start: TDateTime;
  tmp: Word;
begin
  start := GetDateTimeFromUTCTime(StartTime);
  start := IncMinute(start, MinutesCorrection);
  DecodeDateTime(start, Result.Year, Result.Month, Result.Day, Result.Time.Hour, Result.Time.Minute, Result.Time.Second, tmp);
end;

function IsCurrentProgram(const ANow: TDateTime; const StartTime: TDateTime; const Stoptime: TDateTime): Boolean;
begin
  Result := (ANow >= StartTime) and (ANow <= Stoptime);
end;

function GetByteBits(Value: PByte; Offset: Byte; Count: Byte): Byte;
begin
  Result := (Value^ and BYTE_BITS_MASK[Offset, Count]) shr (8 - (Offset + Count));
end;

function GetWordBits(Value: PByte; Offset: Byte; Count: Byte): Word;
begin
  Result := (SwapWord(PWord(Value)^) and WORD_BITS_MASK[Offset, Count]) shr (16 - (Offset + Count));
end;

function GetLongBits(Value: PByte; Offset: Byte; Count: Byte): Cardinal;
begin
  Result := (SwapLong(PCardinal(Value)^) and LONG_BITS_MASK[Offset, Count]) shr (32 - (Offset + Count));
end;

function GetLongLongBits(Value: PByte; Offset: Byte; Count: Byte): Int64;
begin
  Result := (SwapLongLong(PInt64(Value)^) and LONGLONG_BITS_MASK[Offset, Count]) shr (64 - (Offset + Count));
end;

const
  MPEG2_PSI_CRC_32_Lookup: array[0..255] of Cardinal = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B, $1A864DB2,
    $1E475005, $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61, $350C9B64, $31CD86D3,
    $3C8EA00A, $384FBDBD, $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9, $5F15ADAC,
    $5BD4B01B, $569796C2, $52568B75, $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011,
    $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD, $9823B6E0, $9CE2AB57, $91A18D8E,
    $95609039, $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5, $BE2B5B58, $BAEA46EF,
    $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D, $D4326D90,
    $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1, $E13EF6F4, $E5FFEB43, $E8BCCD9A,
    $EC7DD02D, $34867077, $30476DC0, $3D044B19, $39C556AE, $278206AB, $23431B1C,
    $2E003DC5, $2AC12072, $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13,
    $054BF6A4, $0808D07D, $0CC9CDCA, $7897AB07, $7C56B6B0, $71159069, $75D48DDE,
    $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02, $5E9F46BF, $5A5E5B08, $571D7DD1,
    $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA, $ACA5C697, $A864DB20,
    $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692, $8AAD2B2F,
    $8E6C3698, $832F1041, $87EE0DF6, $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055,
    $FEF34DE2, $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34,
    $DC3ABDED, $D8FBA05A, $690CE0EE, $6DCDFD59, $608EDB80, $644FC637, $7A089632,
    $7EC98B85, $738AAD5C, $774BB0EB, $4F040D56, $4BC510E1, $46863638, $42472B8F,
    $5C007B8A, $58C1663D, $558240E4, $51435D53, $251D3B9E, $21DC2629, $2C9F00F0,
    $285E1D47, $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B, $0315D626, $07D4CB91,
    $0A97ED48, $0E56F0FF, $1011A0FA, $14D0BD4D, $19939B94, $1D528623, $F12F560E,
    $F5EE4BB9, $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD, $CDA1F604,
    $C960EBB3, $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7, $AE3AFBA2, $AAFBE615,
    $A7B8C0CC, $A379DD7B, $9B3660C6, $9FF77D71, $92B45BA8, $9675461F, $8832161A,
    $8CF30BAD, $81B02D74, $857130C3, $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640,
    $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C, $7B827D21, $7F436096, $7200464F,
    $76C15BF8, $68860BFD, $6C47164A, $61043093, $65C52D24, $119B4BE9, $155A565E,
    $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC, $3793A651,
    $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0, $D6AD50A5, $D26C4D12, $DF2F6BCB,
    $DBEE767C, $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA,
    $F9278673, $FDE69BC4, $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5,
    $9E7D9662, $933EB0BB, $97FFAD0C, $AFB010B1, $AB710D06, $A6322BDF, $A2F33668,
    $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
  );

function CheckPSICRC(ABuffer: PByte; ALength: Cardinal): Boolean;
var
  ulCRCAccum: Cardinal;
  i, j: Cardinal;
begin
  ulCRCAccum := $FFFFFFFF;

  for j := 0 to ALength -1 do
  begin
    i := ((ulCRCAccum shr 24) xor ABuffer^) and $FF;
    ulCRCAccum := ((ulCRCAccum shl 8) xor MPEG2_PSI_CRC_32_Lookup[i]);
    inc(ABuffer);
  end;

  Result := ulCRCAccum = $00000000;
end;

function IsValidPID(APID: Integer): Boolean;
begin
  Result := (APID >= 0) and (APID < 8192);
end;

function HexToDec(AValue: Cardinal): Cardinal;
var
  b: PByteArray;
begin
  b := @AValue;
  Result := b[0] and $0F;
  inc(Result, (b[0] and $F0) shr 4 * 10);
  inc(Result, (b[1] and $0F) * 100);
  inc(Result, (b[1] and $F0) shr 4 * 1000);
  inc(Result, (b[2] and $0F) * 10000);
  inc(Result, (b[2] and $F0) shr 4 * 100000);
  inc(Result, (b[3] and $0F) * 1000000);
  inc(Result, (b[3] and $F0) shr 4 * 10000000);
end;

function GetRunningStatus(AValue: Byte): TRunningStatus;
begin
  case AValue of
    0..4: Result := TRunningStatus(AValue);
    else  Result := rsReserved;
  end;
end;

function GetBandwidth(AValue: Byte): Byte;
begin
  case AValue of
    0:   Result := 8;
    1:   Result := 7;
    2:   Result := 6;
    3:   Result := 5;
    else Result := 0;
  end;
end;

function GetTerrestrialConstellation(AValue: Byte): TTerrestrialConstellation;
begin
  case AValue of
    0..2: Result := TTerrestrialConstellation(AValue);
    else  Result := tcReserved;
  end;
end;

function GetTerrestrialCodeRate(AValue: Byte): TTerrestrialCodeRate;
begin
  case AValue of
    0..4: Result := TTerrestrialCodeRate(AValue);
    else  Result := tcrReserved;
  end;
end;

function GetTerrestrialTransmissionMode(AValue: Byte): TTerrestrialTransmissionMode;
begin
  case AValue of
    0..2: Result := TTerrestrialTransmissionMode(AValue);
    else  Result := ttmReserved;
  end;
end;

function GetISO639LanguageDescriptorAudioType(AValue: Byte): TISO639LanguageDescriptorAudioType;
begin
  case AValue of
    0..3: Result := TISO639LanguageDescriptorAudioType(AValue);
    else  Result := atReserved;
  end;
end;

function GetProgrammeIdentificationLabel(AValue: PByte): TProgrammeIdentificationLabel;
var
  c: Cardinal;
begin
  c := (GetLongBits(AValue, 0, 20) shr 8) and $FFFFF;
  Result.Day := (c and $F8000) shr 15;
  Result.Month := (c and $7800) shr 11;
  Result.Hour := (c and $7C0) shr 6;
  Result.Minute := (c and $3F);
end;

function GetFECInner(AValue: Byte): TFECInner;
begin
  case AValue of
    0:    Result := fecNotDefined;
    1:    Result := fec1_2;
    2:    Result := fec2_3;
    3:    Result := fec3_4;
    4:    Result := fec5_6;
    5:    Result := fec7_8;
    6:    Result := fec8_9;
    7:    Result := fec3_5;
    8:    Result := fec4_5;
    9:    Result := fec9_10;
    15:   Result := fecNoConv;
    else  Result := fecReserved;
  end;
end;

function GetFECOuter(AValue: Byte): TFECOuter;
begin
  case AValue of
    0:    Result := fecNotDefined_;
    1:    Result := fecNoOuter;
    2:    Result := fecRS_204_188;
    else  Result := fecReserved_;
  end;
end;

function GetSatelliteModulation(AValue: Byte): TSatelliteModulation;
begin
  case AValue of
    0:    Result := smNotDefined;
    1:    Result := smQPSK;
    2:    Result := sm8PSK;
    3:    Result := sm16QAM;
    else  Result := smReserved;
  end;
end;

function GetCableModulation(AValue: Byte): TCableModulation;
begin
  case AValue of
    0:    Result := cmNotDefined;
    1:    Result := cm_16_QAM;
    2:    Result := cm_32_QAM;
    3:    Result := cm_64_QAM;
    4:    Result := cm_128_QAM;
    5:    Result := cm_256_QAM;
    else  Result := cmReserved;
  end;
end;

procedure PrintDescriptors(Descriptors: TDescriptorList; Space: String = '');
var
  i, c, k: Integer;
  str: String;
begin
  dbg('|  ' + Space + 'Number of Descriptors ' + inttostr(Descriptors.Count));
  dbg('|');
  for i := 0 to Descriptors.Count -1 do
  begin
    dbg('|  ' + Space + '  Descriptor ' + inttostr(i));
    dbg('|');
    case Descriptors[i].DescriptorType of
      dtDefault:
      begin
        dbg('|  ' + Space + '    Tag: ' + GetDescriptorIDString(Descriptors[i].Tag));
        dbg('|');
        case Descriptors[i].Tag of
          DESCRIPTOR_TAG_SYSTEM_CLOCK:
          begin
            with TSystemClockDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ExternalClockReferenceIndicator: ' + inttostr(ExternalClockReferenceIndicator));
              dbg('|  ' + Space + '      ClockAccuracyInteger: ' + inttostr(ClockAccuracyInteger));
              dbg('|  ' + Space + '      ClockAccuracyExponent: ' + inttostr(ClockAccuracyExponent));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_DATA_STREAM_ALIGNMENT:
          begin
            with TDataStreamAlignementDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      AlignementType: ' + GetDataAlignementString(AlignementType));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_MAXIMUM_BITRATE:
          begin
            with TMaximumBitrateDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      MaximumBitrate: ' + inttostr(MaximumBitrate));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_MULTIPLEX_BUFFER_UTILIZATION:
          begin
            with TMultiplexBufferUtilizationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      BoundValidFlag: ' + inttostr(BoundValidFlag));
              dbg('|  ' + Space + '      LTWOffsetLowerBound: ' + inttostr(LTWOffsetLowerBound));
              dbg('|  ' + Space + '      LTWOffsetUpperBound: ' + inttostr(LTWOffsetUpperBound));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_SMOOTHING_BUFFER_INDICATOR:
          begin
            with TSmoothingBufferDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      SBLeakRate: ' + inttostr(SBLeakRate));
              dbg('|  ' + Space + '      SBSize: ' + inttostr(SBSize));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_VIDEO_STREAM:
          begin
            with TVideoStreamDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      MultipleFrameRateFlag: ' + inttostr(MultipleFrameRateFlag));
              dbg('|  ' + Space + '      FrameRateCode: ' + inttostr(FrameRateCode));
              dbg('|  ' + Space + '      MPEG1OnlyFlag: ' + inttostr(MPEG1OnlyFlag));
              dbg('|  ' + Space + '      ConstrainedParameterFlag: ' + inttostr(ConstrainedParameterFlag));
              dbg('|  ' + Space + '      StillPictureFlag: ' + inttostr(StillPictureFlag));
              dbg('|  ' + Space + '      ProfileAndLevelIndication: ' + inttostr(ProfileAndLevelIndication));
              dbg('|  ' + Space + '      ChromaFormat: ' + inttostr(ChromaFormat));
              dbg('|  ' + Space + '      FrameRateExtensionFlag: ' + inttostr(FrameRateExtensionFlag));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_AUDIO_STREAM:
          begin
            with TAudioStreamDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      FreeFormatFlag: ' + inttostr(FreeFormatFlag));
              dbg('|  ' + Space + '      ID: ' + inttostr(ID));
              dbg('|  ' + Space + '      Layer: ' + inttostr(Layer));
              dbg('|  ' + Space + '      VariableRateAudioIndicator: ' + inttostr(VariableRateAudioIndicator));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_REGISTRATION:
          begin
            with TRegistrationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      FormatIdentifier: ' + inttostr(FormatIdentifier));
              dbg('|  ' + Space + '      AdditionalIdentificationInfoLength: ' + inttostr(AdditionalIdentificationInfoLength));
              if AdditionalIdentificationInfoLength > 0
                then dbg('|  ' + Space + '      Selector: ' + GetHexString(AdditionalIdentificationInfo, AdditionalIdentificationInfoLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_CA:
          begin
            with TCADescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      SystemID: ' + inttostr(SystemID));
              dbg('|  ' + Space + '      PID: ' + inttostr(PID));
              dbg('|  ' + Space + '      PrivateDataLength: ' + inttostr(PrivateDataLength));
              if PrivateDataLength > 0
                then dbg('|  ' + Space + '      PrivateData: ' + GetHexString(PrivateData, PrivateDataLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_ISO639LANGUAGE:
          begin
            with TISO639LanguageDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Languages: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Language: ' + inttostr(c));
                dbg('|');
                with Language[c] do
                begin
                  dbg('|  ' + Space + '          ISO639Language: ' + String(ISO639Language));
                  dbg('|  ' + Space + '          AudioType: ' + GetISO639LanguageDescriptorAudioTypeString(AudioType));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_STD:
          begin
            with TSTDDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      LeakValidFlag: ' + inttostr(LeakValidFlag));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_CAROUSEL_IDENTIFIER:
          begin
            with TCarouselIdentifierDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      CarouselID: ' + inttostr(CarouselID));
              dbg('|  ' + Space + '      FormatID: ' + inttostr(FormatID));
              dbg('|  ' + Space + '      PrivateDataLength: ' + inttostr(PrivateDataLength));
              if PrivateDataLength > 0
                then dbg('|  ' + Space + '      PrivateData: ' + GetHexString(PrivateData, PrivateDataLength));
              dbg('|  ' + Space + '      ModuleVersion: ' + inttostr(ModuleVersion));
              dbg('|  ' + Space + '      ModuleID: ' + inttostr(ModuleID));
              dbg('|  ' + Space + '      BlockSize: ' + inttostr(BlockSize));
              dbg('|  ' + Space + '      ModuleSize: ' + inttostr(ModuleSize));
              dbg('|  ' + Space + '      CompressionMethod: ' + inttostr(CompressionMethod));
              dbg('|  ' + Space + '      OriginalSize: ' + inttostr(OriginalSize));
              dbg('|  ' + Space + '      TimeOut: ' + inttostr(TimeOut));
              dbg('|  ' + Space + '      ObjectKeyLength: ' + inttostr(ObjectKeyLength));
              if ObjectKeyLength > 0
                then dbg('|  ' + Space + '      ObjectKey: ' + GetHexString(ObjectKey, ObjectKeyLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_NETWORK_NAME:
          begin
            with TNetworkNameDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      NetworkName: ' + Name);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_BOUQUET_NAME:
          begin
            with TBouquetNameDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      NetworkName: ' + Name);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_SERVICE_LIST:
          begin
            with TServiceListDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Services: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Service: ' + inttostr(c));
                dbg('|');
                with Service[c] do
                begin
                  dbg('|  ' + Space + '          ServiceID: ' + inttostr(ServiceID));
                  dbg('|  ' + Space + '          ServiceType: ' + GetServiceTypeString(ServiceType));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_VBI_DATA:
          begin
            with TVBIDataDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Lines: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Line: ' + inttostr(c));
                dbg('|');
                with Line[c] do
                begin
                  dbg('|  ' + Space + '          DataServiceID: ' + GetDataServiceIDString(DataServiceID));
                  dbg('|  ' + Space + '          FieldParity: ' + inttostr(FieldParity));
                  dbg('|  ' + Space + '          LineOffset: ' + inttostr(LineOffset));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_SERVICE:
          begin
            with TServiceDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ServiceType: ' + GetServiceTypeString(ServiceType));
              dbg('|  ' + Space + '      ProviderName: ' + ProviderName);
              dbg('|  ' + Space + '      ServiceName ' + ServiceName);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_COUNTRY_AVAILABILITY:
          begin
            with TCountryAvailibilityDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ServiceType: ' + inttostr(CountryAvailibilityFlag));
              dbg('|  ' + Space + '      CountryCode: ' + CountryCode);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_LINKAGE:
          begin
            with TLinkageDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      TransportStreamID: ' + inttostr(TransportStreamID));
              dbg('|  ' + Space + '      OriginalNetworkID: ' + inttostr(OriginalNetworkID));
              dbg('|  ' + Space + '      ServiceID: ' + inttostr(ServiceID));
              dbg('|  ' + Space + '      LinkageType: (0x' + inttohex(LinkageType, 2) + ') ' + GetLinkageTypeString(LinkageType));
              if (LinkageType = $08) then
              begin
                dbg('|  ' + Space + '      PrivateData: ' + inttostr(HandOverType));
                dbg('|  ' + Space + '      PrivateData: ' + inttostr(OriginType));
                dbg('|  ' + Space + '      PrivateData: ' + inttostr(NetworkID));
                dbg('|  ' + Space + '      PrivateData: ' + inttostr(InitialServiceID));
                dbg('|  ' + Space + '      PrivateData: ' + inttostr(HandOverType));
                if (PrivateDataLength > 0) then
                  dbg('|  ' + Space + '      PrivateData: ' + GetHexString(PrivateData, PrivateDataLength));
              end else
              if (LinkageType = LINKAGE_DESCRIPTOR_PREMIERE) then
              begin
                // Premiere Linkage
                str := '';
                if PrivateDataLength > 0 then
                begin
                  SetLength(str, PrivateDataLength);
                  Move(PrivateData^, str[1], PrivateDataLength);
                end;
                dbg('|  ' + Space + '      Premiere Link: ' + str);
              end else
              if (PrivateDataLength > 0) then
              begin
                dbg('|  ' + Space + '      PrivateData: ' + GetHexString(PrivateData, PrivateDataLength));
              end;
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_SHORT_EVENT:
          begin
            with TShortEventDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Language: ' + String(Language));
              dbg('|  ' + Space + '      EventName: ' + Name);
              dbg('|  ' + Space + '      Description ' + Description);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_EXTENDED_EVENT:
          begin
            with TExtendedEventDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      DescriptorNumber: ' + inttostr(DescriptorNumber));
              dbg('|  ' + Space + '      LastDescriptorNumber: ' + inttostr(LastDescriptorNumber));
              dbg('|  ' + Space + '      Language: ' + String(Language));
              dbg('|  ' + Space + '      Text: ' + Text);
              dbg('|');
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                with Item[c] do
                begin
                  dbg('|  ' + Space + '          Description: ' + Description);
                  dbg('|  ' + Space + '          Text: ' + Text);
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_COMPONENT:
          begin
            with TComponentDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Content: ' + GetStreamContentString(StreamContent, ComponentType));
              dbg('|  ' + Space + '      Component Tag: ' + inttostr(ComponentTag));
              dbg('|  ' + Space + '      Description: ' + Description);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_STREAM_IDENTIFIER:
          begin
            with TStreamIdentifierDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ComponentTag: ' + inttostr(ComponentTag));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_CONTENT:
          begin
            with TContentDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                with Item[c] do
                begin
                  dbg('|  ' + Space + '          Content: ' + GetContentDescriptionString(ContentNibbleLevel1, ContentNibbleLevel2));
                  dbg('|  ' + Space + '          User Nibble 1: ' + inttostr(UserNibbleLevel1));
                  dbg('|  ' + Space + '          User Nibble 1: ' + inttostr(UserNibbleLevel2));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_PARENTAL_RATING:
          begin
            with TParentalRatingDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                with Item[c] do
                begin
                  dbg('|  ' + Space + '          CountryCode: ' + String(CountryCode));
                  dbg('|  ' + Space + '          Rating: ' + GetParentalRatingString(Rating));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_TELETEXT:
          begin
            with TTeletextDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Pages: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Page: ' + inttostr(c));
                dbg('|');
                with Page[c] do
                begin
                  dbg('|  ' + Space + '          ISO639Language: ' + String(ISO639Language));
                  dbg('|  ' + Space + '          Type: ' + GetTeletextTypeString(Type_));
                  dbg('|  ' + Space + '          MagazinNumber: ' + inttostr(MagazinNumber));
                  dbg('|  ' + Space + '          PageNumber: ' + inttostr(PageNumber));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_LOCAL_TIME_OFFSET:
          begin
            with TLocalTimeOffsetDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Entrys: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Entry: ' + inttostr(c));
                dbg('|');
                with Entry[c] do
                begin
                  dbg('|  ' + Space + '          CountryCode: ' + String(CountryCode));
                  dbg('|  ' + Space + '          CountryRegionID: ' + inttostr(CountryRegionID));
                  dbg('|  ' + Space + '          LocalTimeOffsetPolarity: ' + inttostr(LocalTimeOffsetPolarity));
                  dbg('|  ' + Space + '          LocalTimeOffset: ' + inttostr(LocalTimeOffset));
                  dbg('|  ' + Space + '          TimeOfChange: ' + GetUTCTimeString(TimeOfChange));
                  dbg('|  ' + Space + '          NextTimeOffset: ' + inttostr(NextTimeOffset));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_TERRESTRIAL_DELIVERY_SYSTEM:
          begin
            with TTerrestrialDeliverySystemDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      CentreFrequency: ' + inttostr(CentreFrequency));
              dbg('|  ' + Space + '      Bandwidth: ' + inttostr(Bandwidth));
              dbg('|  ' + Space + '      Priority: ' + GetTerrestrialPriorityString(Priority));
              dbg('|  ' + Space + '      TimeSlicingIndicator: ' + inttostr(TimeSlicingIndicator));
              dbg('|  ' + Space + '      MPEFECIndicator: ' + inttostr(MPEFECIndicator));
              dbg('|  ' + Space + '      Constellation: ' + GetConstellationString(Constellation));
              dbg('|  ' + Space + '      HierarchyInformation: ' + GetHierarchyInformationString(HierarchyInformation));
              dbg('|  ' + Space + '      CodeRateHPStream: ' + GetTTerrestrialCodeRateString(CodeRateHPStream));
              dbg('|  ' + Space + '      CodeRateLPStream: ' + GetTTerrestrialCodeRateString(CodeRateLPStream));
              dbg('|  ' + Space + '      GuardInterval: ' + GetGuardIntervalString(GuardInterval));
              dbg('|  ' + Space + '      TransmissionMode: ' + GetTransmissionModeString(TransmissionMode));
              dbg('|  ' + Space + '      OtherFrequencyFlag: ' + inttostr(OtherFrequencyFlag));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_SATELLITE_DELIVERY_SYSTEM:
          begin
            with TSatelliteDeliverySystemDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Frequency: ' + inttostr(Frequency));
              dbg('|  ' + Space + '      OrbitalPosition: ' + inttostr(OrbitalPosition));
              dbg('|  ' + Space + '      WestEastFlag: ' + inttostr(WestEastFlag));
              dbg('|  ' + Space + '      Polarization: ' + GetSatellitePolarizationString(Polarization));
              dbg('|  ' + Space + '      Modulation: ' + GetSatelliteModulationString(Modulation));
              dbg('|  ' + Space + '      SymbolRate: ' + inttostr(SymbolRate));
              dbg('|  ' + Space + '      FECInner: ' + GetFECInnerString(FECInner));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_CABLE_DELIVERY_SYSTEM:
          begin
            with TCableDeliverySystemDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Frequency: ' + inttostr(Frequency));
              dbg('|  ' + Space + '      FECOuter: ' + GetFECOuterString(FECOuter));
              dbg('|  ' + Space + '      Modulation: ' + GetCableModulationString(Modulation));
              dbg('|  ' + Space + '      SymbolRate: ' + inttostr(SymbolRate));
              dbg('|  ' + Space + '      FECInner: ' + GetFECInnerString(FECInner));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_PRIVATE_DATA_SPECIFIER:
          begin
            with TPrivateDataSpecifierDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      PrivateDataSpecifier: ' + inttostr(PrivateDataSpecifier));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_DATA_BROADCAST:
          begin
            with TDataBroadcastDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      DataBroadcastID: ' + inttostr(DataBroadcastID));
              dbg('|  ' + Space + '      ComponentTag: ' + inttostr(ComponentTag));
              dbg('|  ' + Space + '      SelectorLength: ' + inttostr(SelectorLength));
              if SelectorLength > 0
                then dbg('|  ' + Space + '      Selector: ' + GetHexString(Selector, SelectorLength));
              dbg('|  ' + Space + '      ISO639Language: ' + String(ISO639LanguageCode));
              dbg('|  ' + Space + '      Description: ' + Description);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_DATA_BROADCAST_ID:
          begin
            with TDataBroadcastIDDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      DataBroadcastID: ' + inttostr(DataBroadcastID));
              dbg('|  ' + Space + '      SelectorLength: ' + inttostr(IDSelectorLength));
              if IDSelectorLength > 0
                then dbg('|  ' + Space + '      Selector: ' + GetHexString(IDSelector, IDSelectorLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_PDC:
          begin
            with TPDCDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Day: ' + inttostr(ProgrammeIdentificationLabel.Day));
              dbg('|  ' + Space + '      Month: ' + inttostr(ProgrammeIdentificationLabel.Month));
              dbg('|  ' + Space + '      Hour: ' + inttostr(ProgrammeIdentificationLabel.Hour));
              dbg('|  ' + Space + '      Minute: ' + inttostr(ProgrammeIdentificationLabel.Minute));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_AC3:
          begin
            with TAC3Descriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ComponentTypeFlag: ' + inttostr(ComponentTypeFlag));
              if ComponentTypeFlag > 0
                then dbg('|  ' + Space + '      ComponentType: ' + inttostr(ComponentType));
              dbg('|  ' + Space + '      BSIDFlag: ' + inttostr(BSIDFlag));
              if ComponentTypeFlag > 0
                then dbg('|  ' + Space + '      BSID: ' + inttostr(BSID));
              dbg('|  ' + Space + '      MainIDFlag: ' + inttostr(MainIDFlag));
              if ComponentTypeFlag > 0
                then dbg('|  ' + Space + '      BSID: ' + inttostr(BSID));
              dbg('|  ' + Space + '      ASVCFlag: ' + inttostr(ASVCFlag));
              if ComponentTypeFlag > 0
                then dbg('|  ' + Space + '      ASVC: ' + inttostr(ASVC));
              dbg('|  ' + Space + '      AdditionalInfoLength: ' + inttostr(AdditionalInfoLength));
              if AdditionalInfoLength > 0
                then dbg('|  ' + Space + '      AdditionalInfo: ' + GetHexString(AdditionalInfo, AdditionalInfoLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_APPLICATION_SIGNALLING:
          begin
            with TApplicationSignallingDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Applications: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Application: ' + inttostr(c));
                dbg('|');
                with Application[c] do
                begin
                  dbg('|  ' + Space + '          ApplicationType: ' + inttostr(ApplicationType));
                  dbg('|  ' + Space + '          AITVersionNumber: ' + inttostr(AITVersionNumber));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_CA_IDENTIFIER:
          begin
            with TCAIdentifierDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      CASystemIDLength: ' + inttostr(CASystemIDLength));
              if CASystemIDLength > 0
                then dbg('|  ' + Space + '      CASystemID: ' + GetHexString(CASystemID, CASystemIDLength));
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_MULTILINGUAL_COMPONENT:
          begin
            with TMultilingualComponentDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ComponentTag: ' + inttostr(ComponentTag));
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                with Item[c] do
                begin
                  dbg('|  ' + Space + '          Language: ' + LanguageCode);
                  dbg('|  ' + Space + '          Text: ' + Text);
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_FREQUENCY_LIST:
          begin
            with TFrequencyListDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      CodingType: ' + inttostr(CodingType));
              dbg('|  ' + Space + '      Number of Frequencies: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                dbg('|  ' + Space + '          Frequency: ' + inttostr(Frequency[c]));
                dbg('|');
              end;
            end;
          end;
          DESCRIPTOR_TAG_SUBTITLING:
          begin
            with TSubtitlingDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Item: ' + inttostr(c));
                dbg('|');
                with Subtitle[c] do
                begin
                  dbg('|  ' + Space + '          Language: ' + LanguageCode);
                  dbg('|  ' + Space + '          SubtitlingType: ' + inttostr(SubtitlingType));
                  dbg('|  ' + Space + '          CompositionPageID: ' + inttostr(CompositionPageID));
                  dbg('|  ' + Space + '          AncillaryPageID: ' + inttostr(AncillaryPageID));
                  dbg('|');
                end;
              end;
            end;
          end;
          DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER:
          begin
            with TPremiereContentOrderDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      OrderNumber: ' + OrderNumber);
              dbg('|  ' + Space + '      OrderPrice: ' + OrderPrice);
              dbg('|  ' + Space + '      OrderPhoneNumber: ' + OrderPhoneNumber);
              dbg('|  ' + Space + '      SMSOrderInformation: ' + SMSOrderInformation);
              dbg('|  ' + Space + '      URLOrderInformation: ' + URLOrderInformation);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION:
          begin
            with TPremiereParentalInformationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Rating: ' + inttostr(Rating));
              dbg('|  ' + Space + '      ControlTime1: ' + ControlTime1);
              dbg('|  ' + Space + '      ControlTime2: ' + ControlTime2);
              dbg('|  ' + Space + '      ParentalInformation: ' + ParentalInformation);
              dbg('|');
            end;
          end;
          DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION:
          begin
            with TPremiereContentTransmissionDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      TSID: ' + inttostr(TSID));
              dbg('|  ' + Space + '      ONID: ' + inttostr(ONID));
              dbg('|  ' + Space + '      SID: ' + inttostr(SID));
              dbg('|');
              dbg('|  ' + Space + '      Number of StartDates: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        StartDate Index: ' + inttostr(c));
                dbg('|');
                with StartDate[c]^ do
                begin
                  dbg('|  ' + Space + '          StartDate: ' + GetMJDTimeString(StartDate));
                  dbg('|');
                  dbg('|  ' + Space + '          Number of StartTimes: ' + inttostr(StartTimesCount));
                  dbg('|');
                  for k := 0 to StartTimesCount -1 do
                  begin
                    dbg('|  ' + Space + '            StartTime ' + inttostr(k) + ': ' + GetBCDTimeString(StartTimes[k]));
                  end;
                  if StartTimesCount > 0
                    then dbg('|');
                end;
              end;
            end;
          end;
        end;
      end;
      dtMHP:
      begin
        dbg('|  ' + Space + '    Tag: ' + GetMHPDescriptorIDString(Descriptors[i].Tag));
        dbg('|');
        case Descriptors[i].Tag of
          MHP_DESCRIPTOR_TAG_APPLICATION:
          begin
            with TMHPApplicationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ServiceBoundFlag: ' + inttostr(ServiceBoundFlag));
              dbg('|  ' + Space + '      Visibility: ' + inttostr(Visibility));
              dbg('|  ' + Space + '      ApplicationPriority: ' + inttostr(ApplicationPriority));
              dbg('|  ' + Space + '      TransportProtocolLabelLength: ' + inttostr(TransportProtocolLabelLength));
              if TransportProtocolLabelLength > 0
                then dbg('|  ' + Space + '      TransportProtocolLabel: ' + GetHexString(TransportProtocolLabel, TransportProtocolLabelLength));
              dbg('|');
              dbg('|  ' + Space + '      Number of Profiles: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Profile: ' + inttostr(c));
                dbg('|');
                with Profiles[c] do
                begin
                  dbg('|  ' + Space + '          ApplicationProfile: ' + inttostr(ApplicationProfile));
                  dbg('|  ' + Space + '          MajorVersion: ' + inttostr(MajorVersion));
                  dbg('|  ' + Space + '          MinorVersion: ' + inttostr(MinorVersion));
                  dbg('|  ' + Space + '          MicroVersion: ' + inttostr(MicroVersion));
                  dbg('|');
                end;
              end;
            end;
          end;
          MHP_DESCRIPTOR_TAG_APPLICATION_NAME:
          begin
            with TMHPApplicationNameDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Names: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Name: ' + inttostr(c));
                dbg('|');
                with Names[c] do
                begin
                  dbg('|  ' + Space + '          Language: ' + Language);
                  dbg('|  ' + Space + '          Name: ' + Name);
                  dbg('|');
                end;
              end;
            end;
          end;
          MHP_DESCRIPTOR_TAG_TRANSPORT_PROTOCOL:
          begin
            with TMHPTransportProtocolDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ProtocolID: ' + inttostr(ProtocolID));
              dbg('|  ' + Space + '      TransportProtocolLabel: ' + inttostr(TransportProtocolLabel));
              dbg('|  ' + Space + '      SelectorLength: ' + inttostr(SelectorLength));
              if SelectorLength > 0
                then dbg('|  ' + Space + '      Selector: ' + GetHexString(Selector, SelectorLength));
              dbg('|');
            end;
          end;
          MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION:
          begin
            with TMHPDVBJApplicationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Parameters: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Parameter: ' + inttostr(c));
                dbg('|');
                with Parameters[c] do
                begin
                  dbg('|  ' + Space + '          Parameter: ' + Parameter);
                  dbg('|');
                end;
              end;
            end;
          end;
          MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION_LOCATION:
          begin
            with TMHPDVBJApplicationLocationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      BaseDirectory: ' + BaseDirectory);
              dbg('|  ' + Space + '      ClassPathExtension: ' + ClassPathExtension);
              dbg('|  ' + Space + '      InitialClass: ' + InitialClass);
              dbg('|');
            end;
          end;
          MHP_DESCRIPTOR_TAG_EXTERNAL_APPLICATION_AUTHORISATION:
          begin
            with TMHPExternalApplicationAuthorisationDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Authorisations: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Authorisation: ' + inttostr(c));
                dbg('|');
                with Authorisations[c] do
                begin
                  dbg('|  ' + Space + '          OrganisationID: ' + inttostr(ApplicationIndentifier.OrganisationID));
                  dbg('|  ' + Space + '          ApplicationID: ' + inttostr(ApplicationIndentifier.ApplicationID));
                  dbg('|  ' + Space + '          ApplicationPriority: ' + inttostr(ApplicationPriority));
                  dbg('|');
                end;
              end;
            end;
          end;
          MHP_DESCRIPTOR_TAG_APPLICATION_SIGNALLING:
          begin
            with TMHPApplicationSignallingDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Number of Items: ' + inttostr(CountItems));
              dbg('|');
              for c := 0 to CountItems -1 do
              begin
                dbg('|  ' + Space + '        Items: ' + inttostr(c));
                dbg('|');
                with Items[c] do
                begin
                  dbg('|  ' + Space + '          ApplicationType: ' + inttostr(ApplicationType));
                  dbg('|  ' + Space + '          AITVersionNumber: ' + inttostr(AITVersionNumber));
                  dbg('|');
                end;
              end;
            end;
          end;
          MHP_DESCRIPTOR_TAG_LABEL:
          begin
            with TMHPLabelDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      Label: ' + Label_);
              dbg('|');
            end;
          end;
          MHP_DESCRIPTOR_TAG_CACHING_PRIORITY:
          begin
            with TMHPCachingPriorityDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      PriorityValue: ' + inttostr(PriorityValue));
              dbg('|  ' + Space + '      TransparencyLevel: ' + inttostr(TransparencyLevel));
              dbg('|');
            end;
          end;
          MHP_DESCRIPTOR_TAG_CONTENT_TYPE:
          begin
            with TMHPContentTypeDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      ContentType: ' + ContentType);
              dbg('|');
            end;
          end;
        end;
      end;
      dtDVB:
      begin
        dbg('|  ' + Space + '    Tag: ' + GetDVBDescriptorIDString(Descriptors[i].Tag));
        dbg('|');
        case Descriptors[i].Tag of
          DVB_DESCRIPTOR_TAG_COMPRESSED_MODULE:
          begin
            with TDVBCompressedModuleDescriptor(Descriptors[i]) do
            begin
              dbg('|  ' + Space + '      CompressionMethod: ' + inttostr(CompressionMethod));
              dbg('|  ' + Space + '      OriginalSize: ' + inttostr(OriginalSize));
              dbg('|');
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure PrintPAT(PAT: TProgramAssociationSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_PAT --------------------------');
  dbg('|');
  dbg('|  TransportStreamID: ' + inttostr(PAT.TransportStreamID));
  dbg('|');
  dbg('|  Number of Programs: ' + inttostr(PAT.CountPrograms));
  dbg('|');
  for i := 0 to PAT.CountPrograms -1 do
  begin
    dbg('|    Program ' + inttostr(i));
    dbg('|');
    dbg('|      Program Number ' + inttostr(PAT.Program_[i].ProgramNumber));
    if PAT.Program_[i].ProgramNumber = 0
      then dbg('|      Network PID ' + inttostr(PAT.Program_[i].NetworkPID))
      else dbg('|      Program Map PID ' + inttostr(PAT.Program_[i].ProgramMapPID));
    dbg('|');
  end;
end;

procedure PrintPMT(PMT: TProgramMapSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_PMT --------------------------');
  dbg('|');
  dbg('|  ProgramNumber: ' + inttostr(PMT.ProgramNumber));
  dbg('|  PCRPID: ' + inttostr(PMT.PCRPID));
  dbg('|');
  dbg('|  Number of Programs: ' + inttostr(PMT.ProgramStreamList.Count));
  dbg('|');
  for i := 0 to PMT.ProgramStreamList.Count -1 do
  begin
    dbg('|    Program ' + inttostr(i));
    dbg('|');
    dbg('|      StreamType ' + GetStreamTypeString(PMT.ProgramStreamList[i].StreamType));
    dbg('|      ElementaryPID ' + inttostr(PMT.ProgramStreamList[i].ElementaryPID));
    dbg('|');
    PrintDescriptors(PMT.ProgramStreamList[i].Descriptors, '    ');
  end;
  PrintDescriptors(PMT.Descriptors, '');
end;

procedure PrintNIT(NIT: TNetworkInformationSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_NIT --------------------------');
  dbg('|');
  dbg('|  NetworkID: ' + inttostr(NIT.NetworkID));
  dbg('|');
  dbg('|  Number of Transport Streams: ' + inttostr(NIT.TransportStreamList.Count));
  dbg('|');
  for i := 0 to NIT.TransportStreamList.Count -1 do
  begin
    dbg('|    Transport Stream ' + inttostr(i));
    dbg('|');
    dbg('|      TransportStreamID ' + inttostr(NIT.TransportStreamList[i].TransportStreamID));
    dbg('|      OriginalNetworkID ' + inttostr(NIT.TransportStreamList[i].OriginalNetworkID));
    dbg('|');
    PrintDescriptors(NIT.TransportStreamList[i].Descriptors, '    ');
  end;
  PrintDescriptors(NIT.Descriptors, '');
end;

procedure PrintSDT(SDT: TServiceDescriptionSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_SDT --------------------------');
  dbg('|');
  dbg('|  TransportStreamID: ' + inttostr(SDT.TransportStreamID));
  dbg('|  OriginalNetworkID: ' + inttostr(SDT.OriginalNetworkID));
  dbg('|');
  dbg('|  Number of Services: ' + inttostr(SDT.ServiceList.Count));
  dbg('|');
  for i := 0 to SDT.ServiceList.Count -1 do
  begin
    dbg('|    Service ' + inttostr(i));
    dbg('|');
    dbg('|      ServiceID: ' + inttostr(SDT.ServiceList[i].ServiceID));
    dbg('|      RunningStatus: ' + GetRunningStatusString(SDT.ServiceList[i].RunningStatus));
    dbg('|      EITScheduleFlag: ' + inttostr(SDT.ServiceList[i].EITScheduleFlag));
    dbg('|      EITPresentFollowingFlag: ' + inttostr(SDT.ServiceList[i].EITPresentFollowingFlag));
    dbg('|      FreeCAMode: ' + inttostr(SDT.ServiceList[i].FreeCAMode));
    dbg('|');
    PrintDescriptors(SDT.ServiceList[i].Descriptors, '    ');
  end;
end;

procedure PrintBAT(BAT: TBouquetAssociationSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_BAT --------------------------');
  dbg('|');
  dbg('|  BouquetID: ' + inttostr(BAT.BouquetID));
  dbg('|');
  dbg('|  Number of Transport Streams: ' + inttostr(BAT.TransportStreamList.Count));
  dbg('|');
  for i := 0 to BAT.TransportStreamList.Count -1 do
  begin
    dbg('|    Transport Stream ' + inttostr(i));
    dbg('|');
    dbg('|      TransportStreamID ' + inttostr(BAT.TransportStreamList[i].TransportStreamID));
    dbg('|      OriginalNetworkID ' + inttostr(BAT.TransportStreamList[i].OriginalNetworkID));
    dbg('|');
    PrintDescriptors(BAT.TransportStreamList[i].Descriptors, '    ');
  end;
  PrintDescriptors(BAT.Descriptors, '');
end;

procedure PrintEIT(EIT: TEventInformationSection);
var
  i: Integer;
begin
  dbg('-------------------------- TABLE_ID_EIT --------------------------');
  dbg('|');
  dbg('|  ServiceID: ' + inttostr(EIT.ServiceID));
  dbg('|  TransportStreamID: ' + inttostr(EIT.TransportStreamID));
  dbg('|  OriginalNetworkID: ' + inttostr(EIT.OriginalNetworkID));
  dbg('|  SegmentLastSectionNumber: ' + inttostr(EIT.SegmentLastSectionNumber));
  dbg('|  LastTableID: ' + inttostr(EIT.LastTableID));
  dbg('|');
  dbg('|  Number of Events: ' + inttostr(EIT.EventList.Count));
  dbg('|');
  for i := 0 to EIT.EventList.Count -1 do
  begin
    dbg('|    Event ' + inttostr(i));
    dbg('|');
    dbg('|      EventID ' + inttostr(EIT.EventList[i].EventID));
    dbg('|      StartTime ' + GetUTCTimeString(EIT.EventList[i].StartTime));
    dbg('|      Duration ' + GetBCDTimeString(EIT.EventList[i].Duration));
    dbg('|      RunningStatus ' + GetRunningStatusString(EIT.EventList[i].RunningStatus));
    dbg('|      FreeCAMode ' + inttostr(EIT.EventList[i].FreeCAMode));
    dbg('|');
    PrintDescriptors(EIT.EventList[i].Descriptors, '    ');
  end;
end;

procedure PrintCAT(CAT: TConditionalAccessSection);
begin
  dbg('-------------------------- TABLE_ID_CAT --------------------------');
  dbg('|');
  PrintDescriptors(CAT.Descriptors, '');
end;

procedure PrintPremiereCIT(CIT: TPremiereContentInformationSection);
begin
  dbg('---------------------- TABLE_ID_PREMIERE_CIT ---------------------');
  dbg('|');
  dbg('|  ContentID: ' + inttostr(CIT.ContentID));
  dbg('|  Duration: ' + inttostr(CIT.Duration) + ' seconds');
  dbg('|  DescriptorLoopLength: ' + inttostr(CIT.DescriptorLoopLength));
  dbg('|');
  PrintDescriptors(CIT.Descriptors, '');
end;

function GetDescriptorIDString(AValue: Byte): String;
begin                                                                 
  Result := 'Descriptor (0x' + IntToHex(AValue, 2) + ') -> ';
  case (AValue) of
    DESCRIPTOR_TAG_RESERVED_0:                      Result := Result + 'DESCRIPTOR_TAG_RESERVED_0';
    DESCRIPTOR_TAG_RESERVED_1:                      Result := Result + 'DESCRIPTOR_TAG_RESERVED_1';
    DESCRIPTOR_TAG_VIDEO_STREAM:                    Result := Result + 'DESCRIPTOR_TAG_VIDEO_STREAM';
    DESCRIPTOR_TAG_AUDIO_STREAM:                    Result := Result + 'DESCRIPTOR_TAG_AUDIO_STREAM';
    DESCRIPTOR_TAG_HIERARCHY:                       Result := Result + 'DESCRIPTOR_TAG_HIERARCHY';
    DESCRIPTOR_TAG_REGISTRATION:                    Result := Result + 'DESCRIPTOR_TAG_REGISTRATION';
    DESCRIPTOR_TAG_DATA_STREAM_ALIGNMENT:           Result := Result + 'DESCRIPTOR_TAG_DATA_STREAM_ALIGNMENT';
    DESCRIPTOR_TAG_TARGET_BACKGROUND_GRID:          Result := Result + 'DESCRIPTOR_TAG_TARGET_BACKGROUND_GRID';
    DESCRIPTOR_TAG_VIDEO_WINDOW:                    Result := Result + 'DESCRIPTOR_TAG_VIDEO_WINDOW';
    DESCRIPTOR_TAG_CA:                              Result := Result + 'DESCRIPTOR_TAG_CA';
    DESCRIPTOR_TAG_ISO639LANGUAGE:                  Result := Result + 'DESCRIPTOR_TAG_ISO639LANGUAGE';
    DESCRIPTOR_TAG_SYSTEM_CLOCK:                    Result := Result + 'DESCRIPTOR_TAG_SYSTEM_CLOCK';
    DESCRIPTOR_TAG_MULTIPLEX_BUFFER_UTILIZATION:    Result := Result + 'DESCRIPTOR_TAG_MULTIPLEX_BUFFER_UTILIZATION';
    DESCRIPTOR_TAG_COPYRIGHT:                       Result := Result + 'DESCRIPTOR_TAG_COPYRIGHT';
    DESCRIPTOR_TAG_MAXIMUM_BITRATE:                 Result := Result + 'DESCRIPTOR_TAG_MAXIMUM_BITRATE';
    DESCRIPTOR_TAG_PRIVATE_DATA_INDICATOR:          Result := Result + 'DESCRIPTOR_TAG_PRIVATE_DATA_INDICATOR';
    DESCRIPTOR_TAG_SMOOTHING_BUFFER_INDICATOR:      Result := Result + 'DESCRIPTOR_TAG_SMOOTHING_BUFFER_INDICATOR';
    DESCRIPTOR_TAG_STD:                             Result := Result + 'DESCRIPTOR_TAG_STD';
    DESCRIPTOR_TAG_IBP:                             Result := Result + 'DESCRIPTOR_TAG_IBP';
    DESCRIPTOR_TAG_CAROUSEL_IDENTIFIER:             Result := Result + 'DESCRIPTOR_TAG_CAROUSEL_IDENTIFIER';
    DESCRIPTOR_TAG_ASSOCIATION:                     Result := Result + 'DESCRIPTOR_TAG_ASSOCIATION';
    DESCRIPTOR_TAG_DEFERRED_ASSOCIATION:            Result := Result + 'DESCRIPTOR_TAG_DEFERRED_ASSOCIATION';
    DESCRIPTOR_TAG_MPEG4_VIDEO:                     Result := Result + 'DESCRIPTOR_TAG_MPEG4_VIDEO';
    DESCRIPTOR_TAG_MPEG4_AUDIO:                     Result := Result + 'DESCRIPTOR_TAG_MPEG4_AUDIO';
    DESCRIPTOR_TAG_IOD:                             Result := Result + 'DESCRIPTOR_TAG_IOD';
    DESCRIPTOR_TAG_FMC:                             Result := Result + 'DESCRIPTOR_TAG_FMC';
    DESCRIPTOR_TAG_SL:                              Result := Result + 'DESCRIPTOR_TAG_SL';
    DESCRIPTOR_TAG_OCR_ES_ID:                       Result := Result + 'DESCRIPTOR_TAG_OCR_ES_ID';
    DESCRIPTOR_TAG_EXTERNAL_ES_ID:                  Result := Result + 'DESCRIPTOR_TAG_EXTERNAL_ES_ID';
    DESCRIPTOR_TAG_NETWORK_NAME:                    Result := Result + 'DESCRIPTOR_TAG_NETWORK_NAME';
    DESCRIPTOR_TAG_SERVICE_LIST:                    Result := Result + 'DESCRIPTOR_TAG_SERVICE_LIST';
    DESCRIPTOR_TAG_STUFFING:                        Result := Result + 'DESCRIPTOR_TAG_STUFFING';
    DESCRIPTOR_TAG_SATELLITE_DELIVERY_SYSTEM:       Result := Result + 'DESCRIPTOR_TAG_SATELLITE_DELIVERY_SYSTEM';
    DESCRIPTOR_TAG_CABLE_DELIVERY_SYSTEM:           Result := Result + 'DESCRIPTOR_TAG_CABLE_DELIVERY_SYSTEM';
    DESCRIPTOR_TAG_VBI_DATA:                        Result := Result + 'DESCRIPTOR_TAG_VBI_DATA';
    DESCRIPTOR_TAG_VBI_TELETEXT:                    Result := Result + 'DESCRIPTOR_TAG_VBI_TELETEXT';
    DESCRIPTOR_TAG_BOUQUET_NAME:                    Result := Result + 'DESCRIPTOR_TAG_BOUQUET_NAME';
    DESCRIPTOR_TAG_SERVICE:                         Result := Result + 'DESCRIPTOR_TAG_SERVICE';
    DESCRIPTOR_TAG_COUNTRY_AVAILABILITY:            Result := Result + 'DESCRIPTOR_TAG_COUNTRY_AVAILABILITY';
    DESCRIPTOR_TAG_LINKAGE:                         Result := Result + 'DESCRIPTOR_TAG_LINKAGE';
    DESCRIPTOR_TAG_NVOD_REFERENCE:                  Result := Result + 'DESCRIPTOR_TAG_NVOD_REFERENCE';
    DESCRIPTOR_TAG_TIME_SHIFTED_SERVICE:            Result := Result + 'DESCRIPTOR_TAG_TIME_SHIFTED_SERVICE';
    DESCRIPTOR_TAG_SHORT_EVENT:                     Result := Result + 'DESCRIPTOR_TAG_SHORT_EVENT';
    DESCRIPTOR_TAG_EXTENDED_EVENT:                  Result := Result + 'DESCRIPTOR_TAG_EXTENDED_EVENT';
    DESCRIPTOR_TAG_TIME_SHIFTED_EVENT:              Result := Result + 'DESCRIPTOR_TAG_TIME_SHIFTED_EVENT';
    DESCRIPTOR_TAG_COMPONENT:                       Result := Result + 'DESCRIPTOR_TAG_COMPONENT';
    DESCRIPTOR_TAG_MOSAIC:                          Result := Result + 'DESCRIPTOR_TAG_MOSAIC';
    DESCRIPTOR_TAG_STREAM_IDENTIFIER:               Result := Result + 'DESCRIPTOR_TAG_STREAM_IDENTIFIER';
    DESCRIPTOR_TAG_CA_IDENTIFIER:                   Result := Result + 'DESCRIPTOR_TAG_CA_IDENTIFIER';
    DESCRIPTOR_TAG_CONTENT:                         Result := Result + 'DESCRIPTOR_TAG_CONTENT';
    DESCRIPTOR_TAG_PARENTAL_RATING:                 Result := Result + 'DESCRIPTOR_TAG_PARENTAL_RATING';
    DESCRIPTOR_TAG_TELETEXT:                        Result := Result + 'DESCRIPTOR_TAG_TELETEXT';
    DESCRIPTOR_TAG_TELEPHONE:                       Result := Result + 'DESCRIPTOR_TAG_TELEPHONE';
    DESCRIPTOR_TAG_LOCAL_TIME_OFFSET:               Result := Result + 'DESCRIPTOR_TAG_LOCAL_TIME_OFFSET';
    DESCRIPTOR_TAG_SUBTITLING:                      Result := Result + 'DESCRIPTOR_TAG_SUBTITLING';
    DESCRIPTOR_TAG_TERRESTRIAL_DELIVERY_SYSTEM:     Result := Result + 'DESCRIPTOR_TAG_TERRESTRIAL_DELIVERY_SYSTEM';
    DESCRIPTOR_TAG_MULTILINGUAL_NETWORK_NAME:       Result := Result + 'DESCRIPTOR_TAG_MULTILINGUAL_NETWORK_NAME';
    DESCRIPTOR_TAG_MULTILINGUAL_BOUQUET_NAME:       Result := Result + 'DESCRIPTOR_TAG_MULTILINGUAL_BOUQUET_NAME';
    DESCRIPTOR_TAG_MULTILINGUAL_SERVICE_NAME:       Result := Result + 'DESCRIPTOR_TAG_MULTILINGUAL_SERVICE_NAME';
    DESCRIPTOR_TAG_MULTILINGUAL_COMPONENT:          Result := Result + 'DESCRIPTOR_TAG_MULTILINGUAL_COMPONENT';
    DESCRIPTOR_TAG_PRIVATE_DATA_SPECIFIER:          Result := Result + 'DESCRIPTOR_TAG_PRIVATE_DATA_SPECIFIER';
    DESCRIPTOR_TAG_SERVICE_MOVE:                    Result := Result + 'DESCRIPTOR_TAG_SERVICE_MOVE';
    DESCRIPTOR_TAG_SHORT_SMOOTHING_BUFFER:          Result := Result + 'DESCRIPTOR_TAG_SHORT_SMOOTHING_BUFFER';
    DESCRIPTOR_TAG_FREQUENCY_LIST:                  Result := Result + 'DESCRIPTOR_TAG_FREQUENCY_LIST';
    DESCRIPTOR_TAG_PARTIAL_TRANSPORT_STREAM:        Result := Result + 'DESCRIPTOR_TAG_PARTIAL_TRANSPORT_STREAM';
    DESCRIPTOR_TAG_DATA_BROADCAST:                  Result := Result + 'DESCRIPTOR_TAG_DATA_BROADCAST';
    DESCRIPTOR_TAG_SCRAMBLING:                      Result := Result + 'DESCRIPTOR_TAG_SCRAMBLING';
    DESCRIPTOR_TAG_DATA_BROADCAST_ID:               Result := Result + 'DESCRIPTOR_TAG_DATA_BROADCAST_ID';
    DESCRIPTOR_TAG_TRANSPORT_STREAM:                Result := Result + 'DESCRIPTOR_TAG_TRANSPORT_STREAM';
    DESCRIPTOR_TAG_DSNG:                            Result := Result + 'DESCRIPTOR_TAG_DSNG';
    DESCRIPTOR_TAG_PDC:                             Result := Result + 'DESCRIPTOR_TAG_PDC';
    DESCRIPTOR_TAG_AC3:                             Result := Result + 'DESCRIPTOR_TAG_AC3';
    DESCRIPTOR_TAG_ANCILLARY_DATA:                  Result := Result + 'DESCRIPTOR_TAG_ANCILLARY_DATA';
    DESCRIPTOR_TAG_CELL_LIST:                       Result := Result + 'DESCRIPTOR_TAG_CELL_LIST';
    DESCRIPTOR_TAG_CELL_FREQUENCY_LINK:             Result := Result + 'DESCRIPTOR_TAG_CELL_FREQUENCY_LINK';
    DESCRIPTOR_TAG_ANNOUNCEMENT_SUPPORT:            Result := Result + 'DESCRIPTOR_TAG_ANNOUNCEMENT_SUPPORT';
    DESCRIPTOR_TAG_APPLICATION_SIGNALLING:          Result := Result + 'DESCRIPTOR_TAG_APPLICATION_SIGNALLING';
    DESCRIPTOR_TAG_ADAPTATION_FIELD_DATA:           Result := Result + 'DESCRIPTOR_TAG_ADAPTATION_FIELD_DATA';
    DESCRIPTOR_TAG_SERVICE_IDENTIFIER:              Result := Result + 'DESCRIPTOR_TAG_SERVICE_IDENTIFIER';
    DESCRIPTOR_TAG_SERVICE_AVAILABILITY:            Result := Result + 'DESCRIPTOR_TAG_SERVICE_AVAILABILITY';
    DESCRIPTOR_TAG_DEFAULT_AUTHORITY:               Result := Result + 'DESCRIPTOR_TAG_DEFAULT_AUTHORITY';
    DESCRIPTOR_TAG_RELATED_CONTENT:                 Result := Result + 'DESCRIPTOR_TAG_RELATED_CONTENT';
    DESCRIPTOR_TAG_TVAID:                           Result := Result + 'DESCRIPTOR_TAG_TVAID';
    DESCRIPTOR_TAG_CONTENT_IDENTIFIER:              Result := Result + 'DESCRIPTOR_TAG_CONTENT_IDENTIFIER';
    DESCRIPTOR_TAG_TIME_SLICE_FEC_IDENTIFIER:       Result := Result + 'DESCRIPTOR_TAG_TIME_SLICE_FEC_IDENTIFIER';
    DESCRIPTOR_TAG_ECM_REPETITION_RATE:             Result := Result + 'DESCRIPTOR_TAG_ECM_REPETITION_RATE_DESCRIPTOR';
    DESCRIPTOR_S2_SATELLITE_DELIVERY:               Result := Result + 'DESCRIPTOR_S2_SATELLITE_DELIVERY';
    DESCRIPTOR_TAG_ENHANCED_AC3:                    Result := Result + 'DESCRIPTOR_TAG_ENHANCED_AC3';
    DESCRIPTOR_TAG_DTS:                             Result := Result + 'DESCRIPTOR_TAG_DTS';
    DESCRIPTOR_TAG_AAC:                             Result := Result + 'DESCRIPTOR_TAG_AAC';

    DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER:          Result := Result + 'DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER';
    DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION:   Result := Result + 'DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION';
    DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION:    Result := Result + 'DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION';

    DESCRIPTOR_TAG_FORBIDDEN:                       Result := Result + 'DESCRIPTOR_TAG_FORBIDDEN';
    else                                            Result := Result + 'Unknown';
  end;
end;

function GetStreamTypeString(AValue: Byte): String;
begin
  Result := '';

  case AValue of
    $00:      Result := 'ITU-T | ISO/IEC Reserved';
    $01:      Result := 'ISO/IEC 11172 Video';
    $02:      Result := 'ITU-T Rec. H.262 | ISO/IEC 13818-2 Video or ISO/IEC 11172-2 constrained parameter video stream';
    $03:      Result := 'ISO/IEC 11172 Audio';
    $04:      Result := 'ISO/IEC 13818-3 Audio';
    $05:      Result := 'ITU-T Rec. H.222.0 | ISO/IEC 13818-1 private_sections';
    $06:      Result := 'ITU-T Rec. H.222.0 | ISO/IEC 13818-1 PES packets containing private data';
    $07:      Result := 'ISO/IEC 13522 MHEG';
    $08:      Result := 'ITU-T Rec. H.222.0 | ISO/IEC 13818-1 Annex A DSM CC';
    $09:      Result := 'ITU-T Rec. H.222.1';
    $0A:      Result := 'ISO/IEC 13818-6 type A';
    $0B:      Result := 'ISO/IEC 13818-6 type B';
    $0C:      Result := 'ISO/IEC 13818-6 type C';
    $0D:      Result := 'ISO/IEC 13818-6 type D';
    $0E:      Result := 'ISO/IEC 13818-1 auxiliary';
    $0F:      Result := 'ISO/IEC 13818-7 Audio with ADTS transport syntax';
    $10:      Result := 'ISO/IEC 14496-2 Video';
    $11:      Result := 'ISO/IEC 14496-2 Audio';
    $12:      Result := 'ISO/IEC 14496 SL-packetized Stream or FlexMux stream carried in PES';
    $13:      Result := 'ISO/IEC 14496 SL-packetized Stream or FlexMux stream carried in ISO/IEC 13818-1 sections';
    $14:      Result := 'ISO/IEC 13818-6 Synchronized Download Protocol';
    $15..$7F: Result := 'ITU-T Rec. H.222.0 | ISO/IEC 13818-1 Reserved';
    $80..$FF: Result := 'User Private';
  end;

  Result := '0x' + inttohex(AValue, 2) + ' ' + Result;
end;

function GetServiceTypeString(AValue: Byte): String;
begin
  case AValue of
    $00:      Result := 'reserved for future use';
    $01:      Result := 'digital television service';
    $02:      Result := 'digital radio sound service';
    $03:      Result := 'Teletext service';
    $04:      Result := 'NVOD reference service';
    $05:      Result := 'NVOD time-shifted service';
    $06:      Result := 'mosaic service';
    $07:      Result := 'PAL coded signal';
    $08:      Result := 'SECAM coded signal';
    $09:      Result := 'D/D2-MAC';
    $0A:      Result := 'FM Radio';
    $0B:      Result := 'NTSC coded signal';
    $0C:      Result := 'data broadcast service';
    $0D:      Result := 'reserved for Common Interface Usage (EN 50221 [16])';
    $0E:      Result := 'RCS Map (see EN 301 790 [34])';
    $0F:      Result := 'RCS FLS (see EN 301 790 [34])';
    $10:      Result := 'DVB MHP service';
    $11:      Result := 'MPEG-2 HD digital television service';
    $12:      Result := 'MPEG-2 HD NVOD time shifted service';
    $13:      Result := 'MPEG-2 HD NVOD reference service';
    $14:      Result := 'AAC audio service';
    $15:      Result := 'AAC v2 audio service';
    $16:      Result := 'AVC SD digital television service';
    $17:      Result := 'AVC SD NVOD time shifted service';
    $18:      Result := 'AVC SD NVOD reference service';
    $19:      Result := 'AVC HD digital television service';
    $1A:      Result := 'AVC HD NVOD time shifted service';
    $1B:      Result := 'AVC HD NVOD reference service';    
    $1C..$7F: Result := 'reserved for future use';
    $80..$FE: Result := 'user defined';
    $FF:      Result := 'reserved for future use';
  end;
end;

function GetISO639LanguageDescriptorAudioTypeString(AValue: TISO639LanguageDescriptorAudioType): String;
begin
  case AValue of
    atUndefined:                Result := 'undefined';
    atCleanEffects:             Result := 'clean effects';
    atHearingImpaired:          Result := 'hearing impaired';
    atVisualImpairedCommentary: Result := 'visual impaired commentary';
    else                        Result := 'Reserved';
  end;
end;

function GetTerrestrialPriorityString(AValue: Byte): String;
begin
  case AValue of
    0:   Result := 'HP (high priority)';
    1:   Result := 'LP (low priority)';
    else Result := 'unknown'
  end;
end;

function GetConstellationString(AValue: TTerrestrialConstellation): String;
begin
  case AValue of
    tcQPSK:     Result := 'QPSK';
    tc16QAM:    Result := '16-QAM';
    tc64QAM:    Result := '64-QAM';
    tcReserved: Result := 'reserved for future use';
  end;
end;

function GetHierarchyInformationString(AValue: TTerrestrialHierarchyInformation): String;
begin
  case AValue of
    thiNonHierarchicalNativeInterleaver:  Result := 'non-hierarchical, native interleaver';
    thi1NativeInterleaver:                Result := 'á = 1, native interleaver';
    thi2NativeInterleaver:                Result := 'á = 2, native interleaver';
    thi4NativeInterleaver:                Result := 'á = 4, native interleaver';
    thiNonHierarchicalInDepthInterleaver: Result := 'non-hierarchical, in-depth interleaver';
    thi1InDepthInterleaver:               Result := 'á = 1, in-depth interleaver';
    thi2InDepthInterleaver:               Result := 'á = 2, in-depth interleaver';
    thi4InDepthInterleaver:               Result := 'á = 4, in-depth interleaver';
  end;
end;

function GetTTerrestrialCodeRateString(AValue: TTerrestrialCodeRate): String;
begin
  case AValue of
    tcr1_2:      Result := '1/2';
    tcr2_3:      Result := '2/3';
    tcr3_4:      Result := '3/4';
    tcr5_6:      Result := '5/6';
    tcr7_8:      Result := '7/8';
    tcrReserved: Result := 'reserved for future use';
  end;
end;

function GetGuardIntervalString(AValue: TTerrestrialGuardInterval): String;
begin
  case AValue of
    tgi1_32: Result := '1/32';
    tgi1_16: Result := '1/16';
    tgi1_8:  Result := '1/8';
    tgi1_4:  Result := '1/4';
  end;
end;

function GetTransmissionModeString(AValue: TTerrestrialTransmissionMode): String;
begin
  case AValue of
    ttm2k:       Result := '2k mode';
    ttm8k:       Result := '8k mode';
    ttm4k:       Result := '4k mode';
    ttmReserved: Result := 'reserved for future use';
  end;
end;

function GetHexString(AValue: PByte; Count: Integer; Seperator: Boolean = False): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count -1 do
  begin
    if Seperator and (i <> 0) and (i mod 4 = 0)
      then Result := Result + '-';
    Result := Result + inttohex(AValue^, 2);
    inc(AValue);
  end;
end;

function GetRunningStatusString(AValue: TRunningStatus): String;
begin
  case AValue of
    rsUndefined: Result := 'undefined';
    rsNotRunning: Result := 'not running';
    rsStartsInFewSeconds: Result := 'starts in a few seconds (e.g. for video recording)';
    rsPausing: Result := 'pausing';
    rsRunning: Result := 'running';
    rsReserved: Result := 'reserved for future use';
  end;
end;

function GetTeletextTypeString(AValue: Byte): String;
begin
  case AValue of
    0:   Result := 'reserved for future use';
    1:   Result := 'initial Teletext page';
    2:   Result := 'Teletext subtitle page';
    3:   Result := 'additional information page';
    4:   Result := 'programme schedule page';
    5:   Result := 'Teletext subtitle page for hearing impaired people';
    else Result := 'reserved for future use';
  end;
end;

function GetDataServiceIDString(AValue: Byte): String;
begin
  case AValue of
    0:   Result := 'reserved for future use';
    1:   Result := 'EBU teletext (Requires additional teletext_descriptor)';
    2:   Result := 'inverted teletext';
    3:   Result := 'reserved';
    4:   Result := 'VPS';
    5:   Result := 'WSS';
    6:   Result := 'Closed Captioning';
    7:   Result := 'monochrome 4:2:2 samples';
    else Result := 'reserved for future use';
  end;
end;

function GetContentDescriptionString(cn1, cn2: Byte): String;
begin
//  Content_nibble_level_1 - Content_nibble_level_2 - Description
  if (cn1 = $1) and (cn2 = $0) then Result := 'movie/drama (general)'
  else if (cn1 = $1) and (cn2 = $1) then Result := 'detective/thriller'
  else if (cn1 = $1) and (cn2 = $2) then Result := 'adventure/western/war'
  else if (cn1 = $1) and (cn2 = $3) then Result := 'science fiction/fantasy/horror'
  else if (cn1 = $1) and (cn2 = $4) then Result := 'comedy'
  else if (cn1 = $1) and (cn2 = $5) then Result := 'soap/melodrama/folkloric'
  else if (cn1 = $1) and (cn2 = $6) then Result := 'romance'
  else if (cn1 = $1) and (cn2 = $7) then Result := 'serious/classical/religious/historical movie/drama'
  else if (cn1 = $1) and (cn2 = $8) then Result := 'adult movie/drama'
  else if (cn1 = $2) and (cn2 = $0) then Result := 'news/current affairs (general)'
  else if (cn1 = $2) and (cn2 = $1) then Result := 'news/weather report'
  else if (cn1 = $2) and (cn2 = $2) then Result := 'news magazine'
  else if (cn1 = $2) and (cn2 = $3) then Result := 'documentary'
  else if (cn1 = $2) and (cn2 = $4) then Result := 'discussion/interview/debate'
  else if (cn1 = $3) and (cn2 = $0) then Result := 'show/game show (general)'
  else if (cn1 = $3) and (cn2 = $1) then Result := 'game show/quiz/contest'
  else if (cn1 = $3) and (cn2 = $2) then Result := 'variety show'
  else if (cn1 = $3) and (cn2 = $3) then Result := 'talk show'
  else if (cn1 = $4) and (cn2 = $0) then Result := 'sports (general)'
  else if (cn1 = $4) and (cn2 = $1) then Result := 'special events (Olympic Games, World Cup, etc.)'
  else if (cn1 = $4) and (cn2 = $2) then Result := 'sports magazines'
  else if (cn1 = $4) and (cn2 = $3) then Result := 'football/soccer'
  else if (cn1 = $4) and (cn2 = $4) then Result := 'tennis/squash'
  else if (cn1 = $4) and (cn2 = $5) then Result := 'team sports (excluding football)'
  else if (cn1 = $4) and (cn2 = $6) then Result := 'athletics'
  else if (cn1 = $4) and (cn2 = $7) then Result := 'motor sport'
  else if (cn1 = $4) and (cn2 = $8) then Result := 'water sport'
  else if (cn1 = $4) and (cn2 = $9) then Result := 'winter sports'
  else if (cn1 = $4) and (cn2 = $A) then Result := 'equestrian'
  else if (cn1 = $4) and (cn2 = $B) then Result := 'martial sports'
  else if (cn1 = $5) and (cn2 = $0) then Result := 'children''s/youth programmes (general)'
  else if (cn1 = $5) and (cn2 = $1) then Result := 'pre-school children''s programmes'
  else if (cn1 = $5) and (cn2 = $2) then Result := 'entertainment programmes for 6 to 14'
  else if (cn1 = $5) and (cn2 = $3) then Result := 'entertainment programmes for 10 to 16'
  else if (cn1 = $5) and (cn2 = $4) then Result := 'informational/educational/school programmes'
  else if (cn1 = $5) and (cn2 = $5) then Result := 'cartoons/puppets'
  else if (cn1 = $6) and (cn2 = $0) then Result := 'music/ballet/dance (general)'
  else if (cn1 = $6) and (cn2 = $1) then Result := 'rock/pop'
  else if (cn1 = $6) and (cn2 = $2) then Result := 'serious music/classical music'
  else if (cn1 = $6) and (cn2 = $3) then Result := 'folk/traditional music'
  else if (cn1 = $6) and (cn2 = $4) then Result := 'jazz'
  else if (cn1 = $6) and (cn2 = $5) then Result := 'musical/opera'
  else if (cn1 = $6) and (cn2 = $6) then Result := 'ballet'
  else if (cn1 = $7) and (cn2 = $0) then Result := 'arts/culture (without music, general)'
  else if (cn1 = $7) and (cn2 = $1) then Result := 'performing arts'
  else if (cn1 = $7) and (cn2 = $2) then Result := 'fine arts'
  else if (cn1 = $7) and (cn2 = $3) then Result := 'religion'
  else if (cn1 = $7) and (cn2 = $4) then Result := 'popular culture/traditional arts'
  else if (cn1 = $7) and (cn2 = $5) then Result := 'literature'
  else if (cn1 = $7) and (cn2 = $6) then Result := 'film/cinema'
  else if (cn1 = $7) and (cn2 = $7) then Result := 'experimental film/video'
  else if (cn1 = $7) and (cn2 = $8) then Result := 'broadcasting/press'
  else if (cn1 = $7) and (cn2 = $9) then Result := 'new media'
  else if (cn1 = $7) and (cn2 = $A) then Result := 'arts/culture magazines'
  else if (cn1 = $7) and (cn2 = $B) then Result := 'fashion'
  else if (cn1 = $8) and (cn2 = $0) then Result := 'social/political issues/economics (general)'
  else if (cn1 = $8) and (cn2 = $1) then Result := 'magazines/reports/documentary'
  else if (cn1 = $8) and (cn2 = $2) then Result := 'economics/social advisory'
  else if (cn1 = $8) and (cn2 = $3) then Result := 'remarkable people'
  else if (cn1 = $9) and (cn2 = $0) then Result := 'education/science/factual topics (general)'
  else if (cn1 = $9) and (cn2 = $1) then Result := 'nature/animals/environment'
  else if (cn1 = $9) and (cn2 = $2) then Result := 'technology/natural sciences'
  else if (cn1 = $9) and (cn2 = $3) then Result := 'medicine/physiology/psychology'
  else if (cn1 = $9) and (cn2 = $4) then Result := 'foreign countries/expeditions'
  else if (cn1 = $9) and (cn2 = $5) then Result := 'social/spiritual sciences'
  else if (cn1 = $9) and (cn2 = $6) then Result := 'further education'
  else if (cn1 = $9) and (cn2 = $7) then Result := 'languages'
  else if (cn1 = $A) and (cn2 = $0) then Result := 'leisure hobbies (general)'
  else if (cn1 = $A) and (cn2 = $1) then Result := 'tourism/travel'
  else if (cn1 = $A) and (cn2 = $2) then Result := 'handicraft'
  else if (cn1 = $A) and (cn2 = $3) then Result := 'motoring'
  else if (cn1 = $A) and (cn2 = $4) then Result := 'fitness and health'
  else if (cn1 = $A) and (cn2 = $5) then Result := 'cooking'
  else if (cn1 = $A) and (cn2 = $6) then Result := 'advertisement/shopping'
  else if (cn1 = $A) and (cn2 = $7) then Result := 'gardening'
  else if (cn1 = $B) and (cn2 = $0) then Result := 'original language'
  else if (cn1 = $B) and (cn2 = $1) then Result := 'black and white'
  else if (cn1 = $B) and (cn2 = $2) then Result := 'unpublished'
  else if (cn1 = $B) and (cn2 = $3) then Result := 'live broadcast'
  else Result := 'Unknown Content (reserved or user defined)';
end;

function GetContentDescriptionValid(cn1, cn2: Byte): Boolean;
begin
//  Content_nibble_level_1 - Content_nibble_level_2 - Description
  if (cn1 = $1) and (cn2 = $0) then Result := True
  else if (cn1 = $1) and (cn2 = $1) then Result := True
  else if (cn1 = $1) and (cn2 = $2) then Result := True 
  else if (cn1 = $1) and (cn2 = $3) then Result := True 
  else if (cn1 = $1) and (cn2 = $4) then Result := True 
  else if (cn1 = $1) and (cn2 = $5) then Result := True  
  else if (cn1 = $1) and (cn2 = $6) then Result := True  
  else if (cn1 = $1) and (cn2 = $7) then Result := True  
  else if (cn1 = $1) and (cn2 = $8) then Result := True
  else if (cn1 = $2) and (cn2 = $0) then Result := True  
  else if (cn1 = $2) and (cn2 = $1) then Result := True  
  else if (cn1 = $2) and (cn2 = $2) then Result := True  
  else if (cn1 = $2) and (cn2 = $3) then Result := True  
  else if (cn1 = $2) and (cn2 = $4) then Result := True  
  else if (cn1 = $3) and (cn2 = $0) then Result := True  
  else if (cn1 = $3) and (cn2 = $1) then Result := True  
  else if (cn1 = $3) and (cn2 = $2) then Result := True  
  else if (cn1 = $3) and (cn2 = $3) then Result := True  
  else if (cn1 = $4) and (cn2 = $0) then Result := True  
  else if (cn1 = $4) and (cn2 = $1) then Result := True  
  else if (cn1 = $4) and (cn2 = $2) then Result := True  
  else if (cn1 = $4) and (cn2 = $3) then Result := True  
  else if (cn1 = $4) and (cn2 = $4) then Result := True  
  else if (cn1 = $4) and (cn2 = $5) then Result := True  
  else if (cn1 = $4) and (cn2 = $6) then Result := True  
  else if (cn1 = $4) and (cn2 = $7) then Result := True  
  else if (cn1 = $4) and (cn2 = $8) then Result := True  
  else if (cn1 = $4) and (cn2 = $9) then Result := True  
  else if (cn1 = $4) and (cn2 = $A) then Result := True  
  else if (cn1 = $4) and (cn2 = $B) then Result := True  
  else if (cn1 = $5) and (cn2 = $0) then Result := True  
  else if (cn1 = $5) and (cn2 = $1) then Result := True  
  else if (cn1 = $5) and (cn2 = $2) then Result := True  
  else if (cn1 = $5) and (cn2 = $3) then Result := True  
  else if (cn1 = $5) and (cn2 = $4) then Result := True  
  else if (cn1 = $5) and (cn2 = $5) then Result := True  
  else if (cn1 = $6) and (cn2 = $0) then Result := True  
  else if (cn1 = $6) and (cn2 = $1) then Result := True  
  else if (cn1 = $6) and (cn2 = $2) then Result := True  
  else if (cn1 = $6) and (cn2 = $3) then Result := True  
  else if (cn1 = $6) and (cn2 = $4) then Result := True 
  else if (cn1 = $6) and (cn2 = $5) then Result := True  
  else if (cn1 = $6) and (cn2 = $6) then Result := True  
  else if (cn1 = $7) and (cn2 = $0) then Result := True  
  else if (cn1 = $7) and (cn2 = $1) then Result := True
  else if (cn1 = $7) and (cn2 = $2) then Result := True  
  else if (cn1 = $7) and (cn2 = $3) then Result := True  
  else if (cn1 = $7) and (cn2 = $4) then Result := True  
  else if (cn1 = $7) and (cn2 = $5) then Result := True  
  else if (cn1 = $7) and (cn2 = $6) then Result := True  
  else if (cn1 = $7) and (cn2 = $7) then Result := True  
  else if (cn1 = $7) and (cn2 = $8) then Result := True  
  else if (cn1 = $7) and (cn2 = $9) then Result := True  
  else if (cn1 = $7) and (cn2 = $A) then Result := True  
  else if (cn1 = $7) and (cn2 = $B) then Result := True  
  else if (cn1 = $8) and (cn2 = $0) then Result := True  
  else if (cn1 = $8) and (cn2 = $1) then Result := True  
  else if (cn1 = $8) and (cn2 = $2) then Result := True  
  else if (cn1 = $8) and (cn2 = $3) then Result := True  
  else if (cn1 = $9) and (cn2 = $0) then Result := True  
  else if (cn1 = $9) and (cn2 = $1) then Result := True  
  else if (cn1 = $9) and (cn2 = $2) then Result := True  
  else if (cn1 = $9) and (cn2 = $3) then Result := True  
  else if (cn1 = $9) and (cn2 = $4) then Result := True  
  else if (cn1 = $9) and (cn2 = $5) then Result := True  
  else if (cn1 = $9) and (cn2 = $6) then Result := True  
  else if (cn1 = $9) and (cn2 = $7) then Result := True  
  else if (cn1 = $A) and (cn2 = $0) then Result := True  
  else if (cn1 = $A) and (cn2 = $1) then Result := True  
  else if (cn1 = $A) and (cn2 = $2) then Result := True  
  else if (cn1 = $A) and (cn2 = $3) then Result := True  
  else if (cn1 = $A) and (cn2 = $4) then Result := True  
  else if (cn1 = $A) and (cn2 = $5) then Result := True  
  else if (cn1 = $A) and (cn2 = $6) then Result := True  
  else if (cn1 = $A) and (cn2 = $7) then Result := True  
  else if (cn1 = $B) and (cn2 = $0) then Result := True  
  else if (cn1 = $B) and (cn2 = $1) then Result := True
  else if (cn1 = $B) and (cn2 = $2) then Result := True
  else if (cn1 = $B) and (cn2 = $3) then Result := True 
  else Result := False;
end;

function GetStreamContentString(cn1, cn2: Byte): String;
begin
  if (cn1 = $01) and (cn2 = $01) then Result := 'video, 4:3 aspect ratio, 25 Hz'
  else if (cn1 = $01) and (cn2 = $02) then Result := 'video, 16:9 aspect ratio with pan vectors, 25 Hz'
  else if (cn1 = $01) and (cn2 = $03) then Result := 'video, 16:9 aspect ratio without pan vectors, 25 Hz'
  else if (cn1 = $01) and (cn2 = $04) then Result := 'video, > 16:9 aspect ratio, 25 Hz'
  else if (cn1 = $01) and (cn2 = $05) then Result := 'video, 4:3 aspect ratio, 30 Hz'
  else if (cn1 = $01) and (cn2 = $06) then Result := 'video, 16:9 aspect ratio with pan vectors, 30 Hz'
  else if (cn1 = $01) and (cn2 = $07) then Result := 'video, 16:9 aspect ratio without pan vectors, 30 Hz'
  else if (cn1 = $01) and (cn2 = $05) then Result := 'video, > 16:9 aspect ratio, 30 Hz'
  else if (cn1 = $01) and (cn2 = $09) then Result := 'high definition video, 4:3 aspect ratio, 25 Hz'
  else if (cn1 = $01) and (cn2 = $0A) then Result := 'high definition video, 16:9 aspect ratio with pan vectors, 25 Hz'
  else if (cn1 = $01) and (cn2 = $0B) then Result := 'high definition video, 16:9 aspect ratio without pan vectors, 25 Hz'
  else if (cn1 = $01) and (cn2 = $0C) then Result := 'high definition video, > 16:9 aspect ratio, 25 Hz'
  else if (cn1 = $01) and (cn2 = $0D) then Result := 'high definition video, 4:3 aspect ratio, 30 Hz'
  else if (cn1 = $01) and (cn2 = $0E) then Result := 'high definition video, 16:9 aspect ratio with pan vectors, 30 Hz'
  else if (cn1 = $01) and (cn2 = $0F) then Result := 'high definition video, 16:9 aspect ratio without pan vectors, 30 Hz'
  else if (cn1 = $01) and (cn2 = $10) then Result := 'high definition video, > 16:9 aspect ratio, 30 Hz'
  else if (cn1 = $02) and (cn2 = $01) then Result := 'audio, single mono channel'
  else if (cn1 = $02) and (cn2 = $02) then Result := 'audio, dual mono channel'
  else if (cn1 = $02) and (cn2 = $03) then Result := 'audio, stereo (2 channel)'
  else if (cn1 = $02) and (cn2 = $04) then Result := 'audio, multi-lingual, multi-channel'
  else if (cn1 = $02) and (cn2 = $05) then Result := 'audio, surround sound'
  else if (cn1 = $02) and (cn2 = $40) then Result := 'audio description for the visually impaired'
  else if (cn1 = $02) and (cn2 = $41) then Result := 'audio for the hard of hearing'
  else if (cn1 = $02) and (cn2 = $42) then Result := 'receiver-mixed supplementary audio as per annex G of TR 101 154 [11]'
  else if (cn1 = $03) and (cn2 = $01) then Result := 'EBU Teletext subtitles'
  else if (cn1 = $03) and (cn2 = $02) then Result := 'associated EBU Teletext'
  else if (cn1 = $03) and (cn2 = $03) then Result := 'VBI data'
  else if (cn1 = $03) and (cn2 = $10) then Result := 'DVB subtitles (normal) with no monitor aspect ratio criticality'
  else if (cn1 = $03) and (cn2 = $11) then Result := 'DVB subtitles (normal) for display on 4:3 aspect ratio monitor'
  else if (cn1 = $03) and (cn2 = $12) then Result := 'DVB subtitles (normal) for display on 16:9 aspect ratio monitor'
  else if (cn1 = $03) and (cn2 = $13) then Result := 'DVB subtitles (normal) for display on 2.21:1 aspect ratio monitor'
  else if (cn1 = $03) and (cn2 = $20) then Result := 'DVB subtitles (for the hard of hearing) with no monitor aspect ratio criticality'
  else if (cn1 = $03) and (cn2 = $21) then Result := 'DVB subtitles (for the hard of hearing) for display on 4:3 aspect ratio monitor'
  else if (cn1 = $03) and (cn2 = $22) then Result := 'DVB subtitles (for the hard of hearing) for display on 16:9 aspect ratio monitor'
  else if (cn1 = $03) and (cn2 = $23) then Result := 'DVB subtitles (for the hard of hearing) for display on 2.21:1 aspect ratio monitor'
  else Result := 'Unknown Stream Content (reserved or user defined)';
end;

function GetStreamContentValid(cn1, cn2: Byte): Boolean;
begin
  if (cn1 = $01) and (cn2 = $01) then Result := True
  else if (cn1 = $01) and (cn2 = $02) then Result := True
  else if (cn1 = $01) and (cn2 = $03) then Result := True
  else if (cn1 = $01) and (cn2 = $04) then Result := True
  else if (cn1 = $01) and (cn2 = $05) then Result := True
  else if (cn1 = $01) and (cn2 = $06) then Result := True
  else if (cn1 = $01) and (cn2 = $07) then Result := True
  else if (cn1 = $01) and (cn2 = $05) then Result := True
  else if (cn1 = $01) and (cn2 = $09) then Result := True
  else if (cn1 = $01) and (cn2 = $0A) then Result := True
  else if (cn1 = $01) and (cn2 = $0B) then Result := True
  else if (cn1 = $01) and (cn2 = $0C) then Result := True
  else if (cn1 = $01) and (cn2 = $0D) then Result := True
  else if (cn1 = $01) and (cn2 = $0E) then Result := True
  else if (cn1 = $01) and (cn2 = $0F) then Result := True
  else if (cn1 = $01) and (cn2 = $10) then Result := True
  else if (cn1 = $02) and (cn2 = $01) then Result := True
  else if (cn1 = $02) and (cn2 = $02) then Result := True
  else if (cn1 = $02) and (cn2 = $03) then Result := True
  else if (cn1 = $02) and (cn2 = $04) then Result := True
  else if (cn1 = $02) and (cn2 = $05) then Result := True
  else if (cn1 = $02) and (cn2 = $40) then Result := True
  else if (cn1 = $02) and (cn2 = $41) then Result := True
  else if (cn1 = $02) and (cn2 = $42) then Result := True
  else if (cn1 = $03) and (cn2 = $01) then Result := True
  else if (cn1 = $03) and (cn2 = $02) then Result := True
  else if (cn1 = $03) and (cn2 = $03) then Result := True
  else if (cn1 = $03) and (cn2 = $10) then Result := True
  else if (cn1 = $03) and (cn2 = $11) then Result := True
  else if (cn1 = $03) and (cn2 = $12) then Result := True
  else if (cn1 = $03) and (cn2 = $13) then Result := True
  else if (cn1 = $03) and (cn2 = $20) then Result := True
  else if (cn1 = $03) and (cn2 = $21) then Result := True
  else if (cn1 = $03) and (cn2 = $22) then Result := True
  else if (cn1 = $03) and (cn2 = $23) then Result := True
  else Result := False;
end;

function GetParentalRatingString(AValue: Byte): String;
begin
  case AValue of
    $00:      Result := 'undefined';
    $01..$0F: Result := 'minimum age: ' + inttostr(AValue + 3);
    else      Result := 'defined by broadcaster'
  end;
end;

function GetLinkageTypeString(AValue: Byte): String;
begin
  case AValue of
    $00:      Result := 'reserved for future use';
    $01:      Result := 'information service';
    $02:      Result := 'EPG service';
    $03:      Result := 'CA replacement service';
    $04:      Result := 'TS containing complete Network/Bouquet SI';
    $05:      Result := 'service replacement service';
    $06:      Result := 'data broadcast service';
    $07:      Result := 'RCS Map';
    $08:      Result := 'mobile hand-over';
    $09:      Result := 'System Software Update Service (TS 102 006 [20])';
    $0A:      Result := 'TS containing SSU BAT or NIT (TS 102 006 [20])';
    $0B:      Result := 'IP/MAC Notification Service (EN 301 192 [37])';
    $0C:      Result := 'TS containing INT BAT or NIT (EN 301 192 [37])';
    $0D..$7F: Result := 'reserved for future use';
    $80..$FE: Result := 'user defined';
    $FF:      Result := 'reserved for future use';
  end;
end;

function GetSatellitePolarizationString(AValue: TSatellitePolarization): String;
begin
  case AValue of
    spUnknown:            Result := 'Unknown';
    spLinearHorizontal:   Result := 'Linear Horizontal';
    spLinearVertical:     Result := 'Linear Vertical';
    spCircularLeft:       Result := 'Circular Left';
    spCircularRight:      Result := 'Circular Right';
  end;
end;

function GetSatelliteModulationString(AValue: TSatelliteModulation): String;
begin
  case AValue of
    smNotDefined: Result := 'Not Defined';
    smQPSK:       Result := 'QPSK';
    sm8PSK:       Result := '8PSK';
    sm16QAM:      Result := '16QAM';
    smReserved:   Result := 'Reserved';
  end;
end;

function GetFECInnerString(AValue: TFECInner): String;
begin
  case AValue of
    fecNotDefined:  Result := 'Not Defined';
    fec1_2:         Result := '1/2 conv. code rate';
    fec2_3:         Result := '2/3 conv. code rate';
    fec3_4:         Result := '3/4 conv. code rate';
    fec5_6:         Result := '5/6 conv. code rate';
    fec7_8:         Result := '7/8 conv. code rate';
    fec8_9:         Result := '8/9 conv. code rate';
    fecNoConv:      Result := 'No conv. coding';
    fecReserved:    Result := 'Reserved';
  end;
end;

function GetFECOuterString(AValue: TFECOuter): String;
begin
  case AValue of
    fecNotDefined_: Result := 'Not Defined';
    fecNoOuter:     Result := 'No outer FEC coding';
    fecRS_204_188:  Result := 'RS(204/188)';
    fecReserved_:   Result := 'Reserved';
  end;
end;

function GetCableModulationString(AValue: TCableModulation): String;
begin
  case AValue of
    cmNotDefined: Result := 'Not Defined';
    cm_16_QAM:    Result := '16 QAM';
    cm_32_QAM:    Result := '32 QAM';
    cm_64_QAM:    Result := '64 QAM';
    cm_128_QAM:   Result := '128 QAM';
    cm_256_QAM:   Result := '256 QAM';
    cmReserved:   Result := 'Reserved';
  end;
end;

function GetDataAlignementString(AValue: Byte): String;
begin
  case AValue of
    1:      Result := 'Slice, or video access unit';
    2:      Result := 'video access unit';
    3:      Result := 'GOP, or SEQ';
    4:      Result := 'SEQ';
    else    Result := 'Reserved';
  end;
end;

function GetCAIDString(AValue: Integer): String;
begin
  case AValue of
    $0B00: Result := 'Conax';
    $0D00,
    $0D02,
    $0D03,
    $0D05,
    $0D07,
    $0D20: Result := 'Cryptoworks';
    $4A70: Result := 'Dreamcrypt';
    $0606: Result := 'Irdeto1 or Irdeta2';
    $0602,
    $0604,
//    $0606,
    $0608,
    $0622,
    $0626: Result := 'Irdeto2';
    $1702,
    $1722,
    $1762: Result := 'NagraAladin';
    $1800: Result := 'Nagravision';
    $1801: Result := 'Nagravision or NagraAladin';
    $4AD4: Result := 'Omnicrypt';
    $0100: Result := 'Seca1 or Seca2';
    $4A60,
    $4A61,
    $4A63: Result := 'SkyCrypt';
    $0500: Result := 'TPSCrypt or Viaccess1 or Viaccess2';
    $0911,
    $0919,
    $0960,
    $0961: Result := 'Videoguard';
    $4AD0,
    $4AD1: Result := 'X-Crypt';
    else   Result := 'Unknown';
  end;
end;

function GetUTCTimeString(const UTCTime: TUTCTime): String;
begin
  try
    Result := DateTimeToStr(GetDateTimeFromUTCTime(UTCTime));
  except
    Result := 'Corrupted Date/Time';
  end;
end;

function GetMJDTimeString(const MJDTime: TMJDTime): String;
begin
  try
    Result := DateTimeToStr(GetDateTimeFromMJDTime(MJDTime));
  except
    Result := 'Corrupted Date/Time';
  end;
end;

function GetBCDTimeString(const BCDTime: TBCDTime): String;
begin
  try
    Result := TimeToStr(GetDateTimeFromBCDTime(BCDTime));
  except
    Result := 'Corrupted Time';
  end;
end;

function IsEqualTransactionID(ID1: Cardinal; ID2: Cardinal): Boolean;
begin
  Result := GetTransactionID(ID1) = GetTransactionID(ID2);
end;

function GetTransactionID(AID: Cardinal): Cardinal;
begin
  Result := AID and $FFFE;
end;

function HammingDecode(Value: PByteArray): Byte;
const
  HammingTable: array[0..255] of Byte = (
    $01, $FF, $81, $01, $FF, $00, $01, $FF, $FF, $02, $01, $FF, $0A, $FF, $FF, $07,
    $FF, $00, $01, $FF, $00, $80, $FF, $00, $06, $FF, $FF, $0B, $FF, $00, $03, $FF,
    $FF, $0C, $01, $FF, $04, $FF, $FF, $07, $06, $FF, $FF, $07, $FF, $07, $07, $87,
    $06, $FF, $FF, $05, $FF, $00, $0D, $FF, $86, $06, $06, $FF, $06, $FF, $FF, $07,
    $FF, $02, $01, $FF, $04, $FF, $FF, $09, $02, $82, $FF, $02, $FF, $02, $03, $FF,
    $08, $FF, $FF, $05, $FF, $00, $03, $FF, $FF, $02, $03, $FF, $03, $FF, $83, $03,
    $04, $FF, $FF, $05, $84, $04, $04, $FF, $FF, $02, $0F, $FF, $04, $FF, $FF, $07,
    $FF, $05, $05, $85, $04, $FF, $FF, $05, $06, $FF, $FF, $05, $FF, $0E, $03, $FF,
    $FF, $0C, $01, $FF, $0A, $FF, $FF, $09, $0A, $FF, $FF, $0B, $8A, $0A, $0A, $FF,
    $08, $FF, $FF, $0B, $FF, $00, $0D, $FF, $FF, $0B, $0B, $8B, $0A, $FF, $FF, $0B,
    $0C, $8C, $FF, $0C, $FF, $0C, $0D, $FF, $FF, $0C, $0F, $FF, $0A, $FF, $FF, $07,
    $FF, $0C, $0D, $FF, $0D, $FF, $8D, $0D, $06, $FF, $FF, $0B, $FF, $0E, $0D, $FF,
    $08, $FF, $FF, $09, $FF, $09, $09, $89, $FF, $02, $0F, $FF, $0A, $FF, $FF, $09,
    $88, $08, $08, $FF, $08, $FF, $FF, $09, $08, $FF, $FF, $0B, $FF, $0E, $03, $FF,
    $FF, $0C, $0F, $FF, $04, $FF, $FF, $09, $0F, $FF, $8F, $0F, $FF, $0E, $0F, $FF,
    $08, $FF, $FF, $05, $FF, $0E, $0D, $FF, $FF, $0E, $0F, $FF, $0E, $8E, $FF, $0E
  );
begin
  Result := (HammingTable[Value[1]] shl 4) or (HammingTable[Value[0]] and $0F);
end;

function GetVTXChar(t: Char; Lang: Integer): Char;
const
  ncs: array[0..7, 0..12] of Char = (
  	( '£','$','@','«','½','»','^','#','-','¼','¦','¾','÷' ),
  	( 'é','ï','à','ë','ê','ù','î','#','è','â','ô','û','ç' ),
  	( '#','¤','É','Ä','Ö','Å','Ü','_','é','ä','ø','å','ü' ),
  	( '#','û','@','t','z','ý','í','r','é','á','ê','ú','s' ),
  	( '#','$','§','Ä','Ö','Ü','^','_','°','ä','ö','ü','ß' ),
  	( 'ç','$','@','á','é','í','ó','ú','¿','ã','ñ','õ','à' ),
  	( '£','$','é','°','ç','»','^','ë','ù','à','ò','è','ì' ),
  	( '#','¤','T','Â','S','Ä','Î','i','t','â','s','â','î' )
  );
begin
  case Byte(t) of
		$23: Result := ncs[Lang][0];
    $24: Result := ncs[Lang][1];
		$40: Result := ncs[Lang][2];
		$5b: Result := ncs[Lang][3];
		$5c: Result := ncs[Lang][4];
		$5d: Result := ncs[Lang][5];
		$5e: Result := ncs[Lang][6];
		$5f: Result := ncs[Lang][7];
		$60: Result := ncs[Lang][8];
		$7b: Result := ncs[Lang][9];
		$7c: Result := ncs[Lang][10];
		$7d: Result := ncs[Lang][11];
		$7e: Result := ncs[Lang][12];
    else Result := t;
  end;
end;

function GetTeletextColor(Index: Integer): TColor;
begin
  case Index of
    0: Result := clBlack;
    1: Result := clRed;
    2: Result := clLime;
    3: Result := clYellow;
    4: Result := clBlue;
    5: Result := clFuchsia;
    6: Result := clAqua;
    7: Result := clWhite;
    else Result := clBlack;
  end;
end;

procedure UnParityTeletextLine(Line: PTeletextLine);
const
  UNPARITY: Cardinal = $7F7F7F7F;
asm
  mov   ecx, UNPARITY

  and   [eax],    ecx
  and   [eax+4],  ecx
  and   [eax+8],  ecx
  and   [eax+12], ecx
  and   [eax+16], ecx
  and   [eax+20], ecx
  and   [eax+24], ecx
  and   [eax+28], ecx
  and   [eax+32], ecx
  and   [eax+36], ecx
end;

procedure SFree(var AClass);
begin
  if Assigned(Pointer(AClass)) then
  begin
    TObject(AClass).Free;
    Pointer(AClass) := nil;
  end;
end;

end.
