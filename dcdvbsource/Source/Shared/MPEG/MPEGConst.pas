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

unit MPEGConst;

{$I Compiler.inc}

interface

uses
  ISO639LanguageCode;
  
const
  // TS Constants
  TS_PACKET_SIZE                                  = 188;
  TS_PACKET_SYNC_BYTE                             = $47;

  // Payload Constants
  PAYLOAD_MAXIMUM_SIZE                            = 4096;

  // PES Constants
  PES_START_CODE_PREFIX                           = $000001;

  STREAM_ID_MIN                                   = $BA;
  STREAM_ID_MAX                                   = $FF;

  STREAM_ID_PROGRAM_STREAM_MAP                    = $BC;
  STREAM_ID_PRIVATE_STREAM_1                      = $BD;
  STREAM_ID_PADDING_STREAM                        = $BE;
  STREAM_ID_PRIVATE_STREAM_2                      = $BF;

  STREAM_ID_AUDIO_STREAM_MIN                      = $C0;
  STREAM_ID_AUDIO_STREAM_MAX                      = $DF;

  STREAM_ID_VIDEO_STREAM_MIN                      = $E0;
  STREAM_ID_VIDEO_STREAM_MAX                      = $EF;

  STREAM_ID_ECM                                   = $F0;
  STREAM_ID_EMM                                   = $F1;
  STREAM_ID_DSMCC_STREAM                          = $F2;
  STREAM_ID_IEC_13522                             = $F3;
  STREAM_ID_H_222_1_TYPE_A                        = $F4;
  STREAM_ID_H_222_1_TYPE_B                        = $F5;
  STREAM_ID_H_222_1_TYPE_C                        = $F6;
  STREAM_ID_H_222_1_TYPE_D                        = $F7;
  STREAM_ID_H_222_1_TYPE_E                        = $F8;
  STREAM_ID_ANCILLARY                             = $F9;
  STREAM_ID_IEC_14496_1_SL_PACKETIZED             = $FA;
  STREAM_ID_IEC_14496_1_FLEXMUX                   = $FB;
  STREAM_ID_RESERVED_DATA_MIN                     = $FC;
  STREAM_ID_RESERVED_DATA_MAX                     = $FE;
  STREAM_ID_PROGRAM_STREAM_DIRECTORY              = $FF;

  // PSI

  PSI_PID_PAT                                     = $0000;
  PSI_PID_CAT                                     = $0001;
  PSI_PID_TSDT                                    = $0002;
  PSI_PID_NIT                                     = $0010;
  PSI_PID_SDT                                     = $0011;
  PSI_PID_EIT                                     = $0012;
  PSI_PID_RST                                     = $0013;
  PSI_PID_TDT                                     = $0014;
  PSI_PID_RNT                                     = $0016;
  PSI_PID_DIT                                     = $001E;
  PSI_PID_SIT                                     = $001F;

  TABLE_ID_PAT                                    = $00;
  TABLE_ID_CAT                                    = $01;
  TABLE_ID_PMT                                    = $02;
  TABLE_ID_NIT_ACTUAL                             = $40;
  TABLE_ID_NIT_OTHER                              = $41;
  TABLE_ID_SDT_ACTUAL                             = $42;
  TABLE_ID_SDT_OTHER                              = $46;
  TABLE_ID_BAT                                    = $4A;
  TABLE_ID_EIT_ACTUAL_PRESENT                     = $4E;
  TABLE_ID_EIT_OTHER_PRESENT                      = $4F;
  TABLE_ID_EIT_ACTUAL_SCHEDULE_MIN                = $50;
  TABLE_ID_EIT_ACTUAL_SCHEDULE_MAX                = $5F;
  TABLE_ID_EIT_OTHER_SCHEDULE_MIN                 = $60;
  TABLE_ID_EIT_OTHER_SCHEDULE_MAX                 = $6F;
  TABLE_ID_TDT                                    = $70;
  TABLE_ID_RST                                    = $71;
  TABLE_ID_ST                                     = $72;
  TABLE_ID_TOT                                    = $73;
  TABLE_ID_RNT                                    = $79;
  TABLE_ID_DIT                                    = $7E;
  TABLE_ID_RESERVED                               = $FF;

  // MPEG
  DESCRIPTOR_TAG_RESERVED_0                       = $00;
  DESCRIPTOR_TAG_RESERVED_1                       = $01;
  DESCRIPTOR_TAG_VIDEO_STREAM                     = $02;
  DESCRIPTOR_TAG_AUDIO_STREAM                     = $03;
  DESCRIPTOR_TAG_HIERARCHY                        = $04;
  DESCRIPTOR_TAG_REGISTRATION                     = $05;
  DESCRIPTOR_TAG_DATA_STREAM_ALIGNMENT            = $06;
  DESCRIPTOR_TAG_TARGET_BACKGROUND_GRID           = $07;
  DESCRIPTOR_TAG_VIDEO_WINDOW                     = $08;
  DESCRIPTOR_TAG_CA                               = $09;
  DESCRIPTOR_TAG_ISO639LANGUAGE                   = $0A;
  DESCRIPTOR_TAG_SYSTEM_CLOCK                     = $0B;
  DESCRIPTOR_TAG_MULTIPLEX_BUFFER_UTILIZATION     = $0C;
  DESCRIPTOR_TAG_COPYRIGHT                        = $0D;
  DESCRIPTOR_TAG_MAXIMUM_BITRATE                  = $0E;
  DESCRIPTOR_TAG_PRIVATE_DATA_INDICATOR           = $0F;
  DESCRIPTOR_TAG_SMOOTHING_BUFFER_INDICATOR       = $10;
  DESCRIPTOR_TAG_STD                              = $11;
  DESCRIPTOR_TAG_IBP                              = $12;

  // DSM-CC
  DESCRIPTOR_TAG_CAROUSEL_IDENTIFIER              = $13;
  DESCRIPTOR_TAG_ASSOCIATION                      = $14;
  DESCRIPTOR_TAG_DEFERRED_ASSOCIATION             = $15;

  // MPEG New
  DESCRIPTOR_TAG_MPEG4_VIDEO                      = $1B;
  DESCRIPTOR_TAG_MPEG4_AUDIO                      = $1C;
  DESCRIPTOR_TAG_IOD                              = $1D;
  DESCRIPTOR_TAG_FMC                              = $1E;
  DESCRIPTOR_TAG_SL                               = $1F;
  DESCRIPTOR_TAG_OCR_ES_ID                        = $20;
  DESCRIPTOR_TAG_EXTERNAL_ES_ID                   = $21;

  // SI
  DESCRIPTOR_TAG_NETWORK_NAME                     = $40;
  DESCRIPTOR_TAG_SERVICE_LIST                     = $41;
  DESCRIPTOR_TAG_STUFFING                         = $42;
  DESCRIPTOR_TAG_SATELLITE_DELIVERY_SYSTEM        = $43;
  DESCRIPTOR_TAG_CABLE_DELIVERY_SYSTEM            = $44;
  DESCRIPTOR_TAG_VBI_DATA                         = $45;
  DESCRIPTOR_TAG_VBI_TELETEXT                     = $46;
  DESCRIPTOR_TAG_BOUQUET_NAME                     = $47;
  DESCRIPTOR_TAG_SERVICE                          = $48;
  DESCRIPTOR_TAG_COUNTRY_AVAILABILITY             = $49;
  DESCRIPTOR_TAG_LINKAGE                          = $4A;
  DESCRIPTOR_TAG_NVOD_REFERENCE                   = $4B;
  DESCRIPTOR_TAG_TIME_SHIFTED_SERVICE             = $4C;
  DESCRIPTOR_TAG_SHORT_EVENT                      = $4D;
  DESCRIPTOR_TAG_EXTENDED_EVENT                   = $4E;
  DESCRIPTOR_TAG_TIME_SHIFTED_EVENT               = $4F;
  DESCRIPTOR_TAG_COMPONENT                        = $50;
  DESCRIPTOR_TAG_MOSAIC                           = $51;
  DESCRIPTOR_TAG_STREAM_IDENTIFIER                = $52;
  DESCRIPTOR_TAG_CA_IDENTIFIER                    = $53;
  DESCRIPTOR_TAG_CONTENT                          = $54;
  DESCRIPTOR_TAG_PARENTAL_RATING                  = $55;
  DESCRIPTOR_TAG_TELETEXT                         = $56;
  DESCRIPTOR_TAG_TELEPHONE                        = $57;
  DESCRIPTOR_TAG_LOCAL_TIME_OFFSET                = $58;
  DESCRIPTOR_TAG_SUBTITLING                       = $59;
  DESCRIPTOR_TAG_TERRESTRIAL_DELIVERY_SYSTEM      = $5A;
  DESCRIPTOR_TAG_MULTILINGUAL_NETWORK_NAME        = $5B;
  DESCRIPTOR_TAG_MULTILINGUAL_BOUQUET_NAME        = $5C;
  DESCRIPTOR_TAG_MULTILINGUAL_SERVICE_NAME        = $5D;
  DESCRIPTOR_TAG_MULTILINGUAL_COMPONENT           = $5E;
  DESCRIPTOR_TAG_PRIVATE_DATA_SPECIFIER           = $5F;
  DESCRIPTOR_TAG_SERVICE_MOVE                     = $60;
  DESCRIPTOR_TAG_SHORT_SMOOTHING_BUFFER           = $61;
  DESCRIPTOR_TAG_FREQUENCY_LIST                   = $62;
  DESCRIPTOR_TAG_PARTIAL_TRANSPORT_STREAM         = $63;
  DESCRIPTOR_TAG_DATA_BROADCAST                   = $64;
  DESCRIPTOR_TAG_SCRAMBLING                       = $65;
  DESCRIPTOR_TAG_DATA_BROADCAST_ID                = $66;
  DESCRIPTOR_TAG_TRANSPORT_STREAM                 = $67;
  DESCRIPTOR_TAG_DSNG                             = $68;
  DESCRIPTOR_TAG_PDC                              = $69;
  DESCRIPTOR_TAG_AC3                              = $6A;
  DESCRIPTOR_TAG_ANCILLARY_DATA                   = $6B;
  DESCRIPTOR_TAG_CELL_LIST                        = $6C;
  DESCRIPTOR_TAG_CELL_FREQUENCY_LINK              = $6D;
  DESCRIPTOR_TAG_ANNOUNCEMENT_SUPPORT             = $6E;
  DESCRIPTOR_TAG_APPLICATION_SIGNALLING           = $6F;
  DESCRIPTOR_TAG_ADAPTATION_FIELD_DATA            = $70;
  DESCRIPTOR_TAG_SERVICE_IDENTIFIER               = $71;
  DESCRIPTOR_TAG_SERVICE_AVAILABILITY             = $72;
  DESCRIPTOR_TAG_DEFAULT_AUTHORITY                = $73;
  DESCRIPTOR_TAG_RELATED_CONTENT                  = $74;
  DESCRIPTOR_TAG_TVAID                            = $75;
  DESCRIPTOR_TAG_CONTENT_IDENTIFIER               = $76;
  DESCRIPTOR_TAG_TIME_SLICE_FEC_IDENTIFIER        = $77;
  DESCRIPTOR_TAG_ECM_REPETITION_RATE              = $78;
  DESCRIPTOR_S2_SATELLITE_DELIVERY                = $79;
  DESCRIPTOR_TAG_ENHANCED_AC3                     = $7A;
  DESCRIPTOR_TAG_DTS                              = $7B;
  DESCRIPTOR_TAG_AAC                              = $7C;

  DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER           = $F0;
  DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION    = $F1;
  DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION     = $F2;

  DESCRIPTOR_TAG_FORBIDDEN                        = $FF;

  STREAM_TYPE_RESERVED                            = $00;
  STREAM_TYPE_11172_VIDEO                         = $01;
  STREAM_TYPE_13818_2_VIDEO                       = $02;
  STREAM_TYPE_11172_AUDIO                         = $03;
  STREAM_TYPE_13818_3_AUDIO                       = $04;
  STREAM_TYPE_13818_PRIVATE_SECTIONS              = $05;
  STREAM_TYPE_13818_PES_PRIVATE_DATA              = $06;
  STREAM_TYPE_13522_MHEG                          = $07;
  STREAM_TYPE_13818_DSMCC                         = $08;
  STREAM_TYPE_H_222_1                             = $09;
  STREAM_TYPE_13818_6_TYPE_A                      = $0A;
  STREAM_TYPE_13818_6_TYPE_B                      = $0B;
  STREAM_TYPE_13818_6_TYPE_C                      = $0C;
  STREAM_TYPE_13818_6_TYPE_D                      = $0D;
  STREAM_TYPE_13818_AUYILIARY                     = $0E;
  STREAM_TYPE_13818_7_AUDIO_ADTS                  = $0F;
  STREAM_TYPE_14496_2_VIDEO                       = $10;
  STREAM_TYPE_14496_3_AUDIO                       = $11;
  STREAM_TYPE_14496_SL_FLEXMUX_PES                = $12;
  STREAM_TYPE_14496_SL_FLEXMUX_ISO                = $13;
  STREAM_TYPE_13818_6_SYNCHRONIZED_DOWNLOAD       = $14;
  STREAM_TYPE_METADATA_PES                        = $15;
  STREAM_TYPE_METADATA_SECTIONS                   = $16;
  STREAM_TYPE_METADATA_DATA_CAROUSEL              = $17;
  STREAM_TYPE_METADATA_OBJECT_CAROUSEL            = $18;
  STREAM_TYPE_METADATA_SYNCHRONIZED               = $19;
  STREAM_TYPE_13818_11_IPMP                       = $1A;
  STREAM_TYPE_14496_10_H264_VIDEO                 = $1B;
  STREAM_TYPE_14496_3_AAC_AUDIO                   = $1C;
  STREAM_TYPE_14496_17_TEXT                       = $1D;

  LINKAGE_DESCRIPTOR_PREMIERE                     = $B0;

  // http://pdc.ro.nu/hamming.html
  HAMMING_TABLE: array[0..255] of Byte =
  (
    $00, $01, $00, $01, $02, $03, $02, $03, $00, $01,
    $00, $01, $02, $03, $02, $03, $04, $05, $04, $05,
    $06, $07, $06, $07, $04, $05, $04, $05, $06, $07,
    $06, $07, $00, $01, $00, $01, $02, $03, $02, $03,
    $00, $01, $00, $01, $02, $03, $02, $03, $04, $05,
    $04, $05, $06, $07, $06, $07, $04, $05, $04, $05,
    $06, $07, $06, $07, $08, $09, $08, $09, $0A, $0B,
    $0A, $0B, $08, $09, $08, $09, $0A, $0B, $0A, $0B,
    $0C, $0D, $0C, $0D, $0E, $0F, $0E, $0F, $0C, $0D,
    $0C, $0D, $0E, $0F, $0E, $0F, $08, $09, $08, $09,
    $0A, $0B, $0A, $0B, $08, $09, $08, $09, $0A, $0B,
    $0A, $0B, $0C, $0D, $0C, $0D, $0E, $0F, $0E, $0F,
    $0C, $0D, $0C, $0D, $0E, $0F, $0E, $0F, $00, $01,
    $00, $01, $02, $03, $02, $03, $00, $01, $00, $01,
    $02, $03, $02, $03, $04, $05, $04, $05, $06, $07,
    $06, $07, $04, $05, $04, $05, $06, $07, $06, $07,
    $00, $01, $00, $01, $02, $03, $02, $03, $00, $01,
    $00, $01, $02, $03, $02, $03, $04, $05, $04, $05,
    $06, $07, $06, $07, $04, $05, $04, $05, $06, $07,
    $06, $07, $08, $09, $08, $09, $0A, $0B, $0A, $0B,
    $08, $09, $08, $09, $0A, $0B, $0A, $0B, $0C, $0D,
    $0C, $0D, $0E, $0F, $0E, $0F, $0C, $0D, $0C, $0D,
    $0E, $0F, $0E, $0F, $08, $09, $08, $09, $0A, $0B,
    $0A, $0B, $08, $09, $08, $09, $0A, $0B, $0A, $0B,
    $0C, $0D, $0C, $0D, $0E, $0F, $0E, $0F, $0C, $0D,
    $0C, $0D, $0E, $0F, $0E, $0F
  );
  
  BIT_REVERSE_TABLE: array[0..255] of Byte =
  (
    $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90,
    $50, $D0, $30, $B0, $70, $F0, $08, $88, $48, $C8,
    $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8,
    $78, $F8, $04, $84, $44, $C4, $24, $A4, $64, $E4,
    $14, $94, $54, $D4, $34, $B4, $74, $F4, $0C, $8C,
    $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC,
    $3C, $BC, $7C, $FC, $02, $82, $42, $C2, $22, $A2,
    $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A,
    $5A, $DA, $3A, $BA, $7A, $FA, $06, $86, $46, $C6,
    $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6,
    $76, $F6, $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
    $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE, $01, $81,
    $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1,
    $31, $B1, $71, $F1, $09, $89, $49, $C9, $29, $A9,
    $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95,
    $55, $D5, $35, $B5, $75, $F5, $0D, $8D, $4D, $CD,
    $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD,
    $7D, $FD, $03, $83, $43, $C3, $23, $A3, $63, $E3,
    $13, $93, $53, $D3, $33, $B3, $73, $F3, $0B, $8B,
    $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB,
    $3B, $BB, $7B, $FB, $07, $87, $47, $C7, $27, $A7,
    $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F,
    $5F, $DF, $3F, $BF, $7F, $FF
  );

type
  TDescriptorType = ( dtDefault, dtMHP, dtDVB );

  TServiceDescriptorService = packed record
    ServiceID: Word;
    ServiceType: Byte;
  end;
  PServiceDescriptorService = ^TServiceDescriptorService;

  TTerrestrialConstellation = ( tcQPSK, tc16QAM, tc64QAM, tcReserved );

  TTerrestrialHierarchyInformation = (
    thiNonHierarchicalNativeInterleaver,
    thi1NativeInterleaver,
    thi2NativeInterleaver,
    thi4NativeInterleaver,
    thiNonHierarchicalInDepthInterleaver,
    thi1InDepthInterleaver,
    thi2InDepthInterleaver,
    thi4InDepthInterleaver
  );

  TTerrestrialCodeRate = ( tcr1_2, tcr2_3, tcr3_4, tcr5_6, tcr7_8, tcrReserved );

  TTerrestrialGuardInterval = ( tgi1_32, tgi1_16, tgi1_8, tgi1_4 );

  TTerrestrialTransmissionMode = ( ttm2k, ttm8k, ttm4k, ttmReserved );

  TProgramAssociationSectionProgram = packed record
    ProgramNumber: Word;
    case Integer of
      0: ( NetworkPID: Word );
      1: ( ProgramMapPID: Word );
  end;
  PProgramAssociationSectionProgram = ^TProgramAssociationSectionProgram;

  TTeletextDescriptorPage = packed record
    ISO639Language: TISO6392LanguageCode;
    Type_: Byte;
    MagazinNumber: Byte;
    PageNumber: Byte;
  end;
  PTeletextDescriptorPage = ^TTeletextDescriptorPage;

  TISO639LanguageDescriptorAudioType = (
    atUndefined, atCleanEffects, atHearingImpaired, atVisualImpairedCommentary, atReserved
  );

  TISO639LanguageDescriptorLanguage = packed record
    ISO639Language: TISO6392LanguageCode;
    AudioType: TISO639LanguageDescriptorAudioType;
  end;
  PISO639LanguageDescriptorLanguage = ^TISO639LanguageDescriptorLanguage;

  TVBIDataDescriptorLine = packed record
    DataServiceID: Byte;
    FieldParity: Byte;
    LineOffset: Byte;
  end;
  PVBIDataDescriptorLine = ^TVBIDataDescriptorLine;

  TRunningStatus = (
    rsUndefined, rsNotRunning, rsStartsInFewSeconds, rsPausing, rsRunning, rsReserved
  );

  TBCDTime = record
    Hour: Word;
    Minute: Word;
    Second: Word;
  end;
  PBCDTime = ^TBCDTime;

  TUTCTime = record
    Year: Word;
    Month: Word;
    Day: Word;
    Time: TBCDTime;
  end;
  PUTCTime = ^TUTCTime;

  TMJDTime = record
    Day: Integer;
    Month: Integer;
    Year: Integer;
  end;
  PMJDTime = ^TMJDTime;

  TCountryCode = array[0..2] of Char;
  PCountryCode = ^TCountryCode;

  TLocalTimeOffsetDescriptorEntry = packed record
    CountryCode: TCountryCode;
    CountryRegionID: Byte;
    LocalTimeOffsetPolarity: Byte;
    LocalTimeOffset: Word;
    TimeOfChange: TUTCTime;
    NextTimeOffset: Word;
  end;
  PLocalTimeOffsetDescriptorEntry = ^TLocalTimeOffsetDescriptorEntry;

  TExtendedEventDescriptorItem = packed record
    Description: String;
    Text: String;
  end;
  PExtendedEventDescriptorItem = ^TExtendedEventDescriptorItem;

  TContentDescriptorItem = packed record
    ContentNibbleLevel1: Byte;
    ContentNibbleLevel2: Byte;
    UserNibbleLevel1: Byte;
    UserNibbleLevel2: Byte;
  end;
  PContentDescriptorItem = ^TContentDescriptorItem;

  TParentalDescriptorItem = packed record
    CountryCode: TCountryCode;
    Rating: Byte;
  end;
  PParentalDescriptorItem = ^TParentalDescriptorItem;

  TProgrammeIdentificationLabel = packed record
    Day: Byte;
    Month: Byte;
    Hour: Byte;
    Minute: Byte;
  end;
  PProgrammeIdentificationLabel = ^TProgrammeIdentificationLabel;

  TApplicationSignallingDescriptorApplication = packed record
    ApplicationType: Word;
    AITVersionNumber: Byte;
  end;
  PApplicationSignallingDescriptorApplication = ^TApplicationSignallingDescriptorApplication;

  TSatelliteModulation = ( smNotDefined, smQPSK, sm8PSK, sm16QAM, smReserved );

  TSatellitePolarization = (
    spUnknown, spLinearHorizontal, spLinearVertical, spCircularLeft, spCircularRight
  );

  TFECInner = (
    fecNotDefined, fec1_2, fec2_3, fec3_4, fec5_6, fec7_8, fec8_9, fec3_5, fec4_5, fec9_10, fecNoConv, fecReserved
  );

  TFECOuter = (
    fecNotDefined_, fecNoOuter, fecRS_204_188, fecReserved_
  );

  TCableModulation = (
    cmNotDefined, cm_16_QAM, cm_32_QAM, cm_64_QAM, cm_128_QAM, cm_256_QAM, cmReserved
  );

  TMultilingualComponentDescriptorItem = record
    LanguageCode: TISO6392LanguageCode;
    Text: String;
  end;
  PMultilingualComponentDescriptorItem = ^TMultilingualComponentDescriptorItem;

  TSubtitlingDescriptorItem = record
    LanguageCode: TISO6392LanguageCode;
    SubtitlingType: Byte;
    CompositionPageID: Word;
    AncillaryPageID: Word;
    // not in ISO. Used by Channel list to choose a default Stream
    DefaultSub: Boolean;
  end;
  PSubtitlingDescriptorItem = ^TSubtitlingDescriptorItem;

  TBufferCallback = procedure(ABuffer: PByte; ASize: Integer) of Object;
  TPacketCallback = procedure(ABuffer: PByte; APID: Integer) of Object;
  TPCRCallback    = procedure(APCR: Int64) of Object;

  TTeletextLine = array[0..39] of Char;
  PTeletextLine = ^TTeletextLine;
  TTeletextLineCallback = procedure(Magazine: Integer; Line: Integer; Text: PTeletextLine) of Object;

  PMyByteArray = ^TMyByteArray;
  TMyByteArray = array[0..High(Integer)-1] of Byte;

  TPremiereContentTransmitionItem = record
    StartDate: TMJDTime;
    StartTimes: array of TBCDTime;
    StartTimesCount: Integer;
  end;
  PPremiereContentTransmitionItem = ^TPremiereContentTransmitionItem;

var
  BYTE_BITS_MASK: array[0..7, 0..7] of Byte;
  WORD_BITS_MASK: array[0..15, 0..15] of Word;
  LONG_BITS_MASK: array[0..31, 0..31] of Cardinal;
  LONGLONG_BITS_MASK: array[0..63, 0..63]of Int64;

implementation

function GetMask(Offset: Byte; Count: Byte; M: Byte): Int64;
var
  i: Integer;
begin
	Result := 0;
  for i := 0 to Count -1
    do Result := Result or (int64(1) shl Int64(M-Offset-i));
end;

procedure GenerateBitMasks();
var
  i, c: Integer;
begin
  for i := 0 to 7 do
    for c := 0 to 7 do
		  BYTE_BITS_MASK[i][c] := GetMask(i,c,7);
  for i := 0 to 15 do
    for c := 0 to 15 do
		  WORD_BITS_MASK[i][c] := GetMask(i,c,15);
  for i := 0 to 31 do
    for c := 0 to 31 do
		  LONG_BITS_MASK[i][c] := GetMask(i,c,31);
  for i := 0 to 63 do
    for c := 0 to 63 do
		  LONGLONG_BITS_MASK[i][c] := GetMask(i,c,63);
end;

initialization
  GenerateBitMasks;

end.
