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

unit MPEGSections;

interface

{$I Compiler.inc}

uses
  Classes, SysUtils, MPEGConst, MPEGDescriptors, Windows;

type
  TBaseSection = class
  protected
    FTableID: Byte;
    FSectionSyntaxIndicator: Byte;
    FReserved1: Byte;
    FReserved2: Byte;
    FSectionLength: Word;

    FValid: Boolean;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte; ASize: Integer); overload; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; virtual;
  published
    property TableID: Byte read FTableID;
    property SectionSyntaxIndicator: Byte read FSectionSyntaxIndicator;
    property Reserved1: Byte read FReserved1;
    property Reserved2: Byte read FReserved2;
    property SectionLength: Word read FSectionLength;

    property Valid: Boolean read FValid;
  end;

  // ---------------------------------------------------------------------------

  TBaseSectionSyntax = class(TBaseSection)
  protected
    FUsesSectionSyntax: Boolean;
    FReserved: Cardinal;
    FVersionNumber: Byte;
    FCurrentNextIndicator: Byte;
    FSectionNumber: Byte;
    FLastSectionNumber: Byte;
    FValidCRC: Boolean;
    FCRC: Cardinal;
  public
    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;
  published
    property VersionNumber: Byte read FVersionNumber;
    property CurrentNextIndicator: Byte read FCurrentNextIndicator;
    property SectionNumber: Byte read FSectionNumber;
    property LastSectionNumber: Byte read FLastSectionNumber;
    property CRC: Cardinal read FCRC;
    property ValidCRC: Boolean read FValidCRC;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x00 Defined in ISO 13818-1
  //
  TProgramAssociationSection = class(TBaseSectionSyntax)
  protected
    FTransportStreamID: Word;
    FProgramList: TList;
    function GetCountPrograms: integer;
    function GetProgram(Index: Integer): TProgramAssociationSectionProgram;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;
  published
    property TransportStreamID: Word read FTransportStreamID;
    property CountPrograms: Integer read GetCountPrograms;
  public
    property Program_[Index: Integer]: TProgramAssociationSectionProgram read GetProgram; default;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x01 Defined in ISO 13818-1
  //
  TConditionalAccessSection = class(TBaseSectionSyntax)
  private
    FDescriptors: TDescriptorList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property Descriptors: TDescriptorList read FDescriptors;
  end;

  // ---------------------------------------------------------------------------

  TProgramStream = class
  private
    FStreamType: Byte;
    FElementaryPID: Word;
    FDescriptors: TDescriptorList;
    FTotalLength: Integer;
    function GetIsTeletext: Boolean;
    function GetIsAudioMP1: Boolean;
    function GetIsAudioMP2: Boolean;
    function GetIsAudioAC3: Boolean;
    function GetIsAudioAAC: Boolean;
    function GetIsAudioDTS: Boolean;
    function GetIsVideoMPEG2: Boolean;
    function GetIsVideoH264: Boolean;
    function GetIsMHPAIT: Boolean;
    function GetIsMHPData: Boolean;
    function GetIsDSMCC: Boolean;
    function GetIsSubtitle: Boolean;
    function GetHasVideo: Boolean;
    function GetHasAudio: Boolean;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property StreamType: Byte read FStreamType;
    property ElementaryPID: Word read FElementaryPID;
    property Descriptors: TDescriptorList read FDescriptors;
    property TotalLength: Integer read FTotalLength;

    property IsVideoMPEG2: Boolean read GetIsVideoMPEG2;
    property IsVideoH264: Boolean read GetIsVideoH264;
    property HasVideo: Boolean read GetHasVideo;

    property IsAudioMP1: Boolean read GetIsAudioMP1;
    property IsAudioMP2: Boolean read GetIsAudioMP2;
    property IsAudioAC3: Boolean read GetIsAudioAC3;
    property IsAudioAAC: Boolean read GetIsAudioAAC;
    property IsAudioDTS: Boolean read GetIsAudioDTS;
    property HasAudio: Boolean read GetHasAudio;

    property IsTeletext: Boolean read GetIsTeletext;
    property IsSubtitle: Boolean read GetIsSubtitle;

    property IsMHPAIT: Boolean read GetIsMHPAIT;
    property IsMHPData: Boolean read GetIsMHPData;
    property IsDSMCC: Boolean read GetIsDSMCC;
  end;

  // ---------------------------------------------------------------------------

  TProgramStreamList = class(TList)
  private
    function Get(Index: Integer): TProgramStream;
    procedure Put(Index: Integer; Item: TProgramStream);
  public
    function Add(Item: TProgramStream): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TProgramStream read Get write Put; default;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x02 Defined in ISO 13818-1
  //
  TProgramMapSection = class(TBaseSectionSyntax)
  private
    FProgramNumber: Word;
    FPCRPID: Word;
    FDescriptors: TDescriptorList;
    FProgramStreamList: TProgramStreamList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property ProgramNumber: Word read FProgramNumber;
    property PCRPID: Word read FPCRPID;
    property Descriptors: TDescriptorList read FDescriptors;
    property ProgramStreamList: TProgramStreamList read FProgramStreamList;
  end;

  // ---------------------------------------------------------------------------
  // TODO transport_stream_description_section TableID 0x03
  //

  // ---------------------------------------------------------------------------

  TTransportStream = class
  private
    FTransportStreamID: Word;
    FOriginalNetworkID: Word;
    FDescriptors: TDescriptorList;
    FTotalLength: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property TransportStreamID: Word read FTransportStreamID;
    property OriginalNetworkID: Word read FOriginalNetworkID;
    property Descriptors: TDescriptorList read FDescriptors;
    property TotalLength: Integer read FTotalLength;
  end;

  // ---------------------------------------------------------------------------

  TTransportStreamList = class(TList)
  private
    function Get(Index: Integer): TTransportStream;
    procedure Put(Index: Integer; Item: TTransportStream);
  public
    function Add(Item: TTransportStream): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TTransportStream read Get write Put; default;
  end;

  // ---------------------------------------------------------------------------

  TBaseTransportStreamSection = class(TBaseSectionSyntax)
  protected
    FUnknownID: Word;
    FDescriptors: TDescriptorList;
    FTransportStreamList: TTransportStreamList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property Descriptors: TDescriptorList read FDescriptors;
    property TransportStreamList: TTransportStreamList read FTransportStreamList;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x40 and 0x41 Defined in EN 300 468
  //
  TNetworkInformationSection = class(TBaseTransportStreamSection)
  public
    property NetworkID: Word read FUnknownID;
  end;

  // ---------------------------------------------------------------------------

  TService = class
  private
    FServiceID: Word;
    FEITScheduleFlag: Byte;
    FEITPresentFollowingFlag: Byte;
    FRunningStatus: TRunningStatus;
    FFreeCAMode: Byte;
    FDescriptors: TDescriptorList;
    FTotalLength: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property ServiceID: Word read FServiceID;
    property EITScheduleFlag: Byte read FEITScheduleFlag;
    property EITPresentFollowingFlag: Byte read FEITPresentFollowingFlag;
    property RunningStatus: TRunningStatus read FRunningStatus;
    property FreeCAMode: Byte read FFreeCAMode;

    property Descriptors: TDescriptorList read FDescriptors;
    property TotalLength: Integer read FTotalLength;
  end;

  // ---------------------------------------------------------------------------

  TServiceList = class(TList)
  private
    function Get(Index: Integer): TService;
    procedure Put(Index: Integer; Item: TService);
  public
    function Add(Item: TService): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TService read Get write Put; default;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x42 and 0x46 Defined in EN 300 468
  //
  TServiceDescriptionSection = class(TBaseSectionSyntax)
  private
    FTransportStreamID: Word;
    FOriginalNetworkID: Word;
    FServiceList: TServiceList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property TransportStreamID: Word read FTransportStreamID;
    property OriginalNetworkID: Word read FOriginalNetworkID;
    property ServiceList: TServiceList read FServiceList;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x4A Defined in EN 300 468
  //
  TBouquetAssociationSection = class(TBaseTransportStreamSection)
  public
    property BouquetID: Word read FUnknownID;
  end;

  // ---------------------------------------------------------------------------

  TEvent = class
  private
    FEventID: Word;
    FStartTime: TUTCTime;
    FDuration: TBCDTime;
    FRunningStatus: TRunningStatus;
    FFreeCAMode: Byte;
    FDescriptors: TDescriptorList;
    FTotalLength: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property EventID: Word read FEventID;
    property StartTime: TUTCTime read FStartTime;
    property Duration: TBCDTime read FDuration;
    property RunningStatus: TRunningStatus read FRunningStatus;
    property FreeCAMode: Byte read FFreeCAMode;

    property Descriptors: TDescriptorList read FDescriptors;
    property TotalLength: Integer read FTotalLength;
  end;

  // ---------------------------------------------------------------------------

  TEventList = class(TList)
  private
    function Get(Index: Integer): TEvent;
    procedure Put(Index: Integer; Item: TEvent);
  public
    function Add(Item: TEvent): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TEvent read Get write Put; default;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x4E .. 0x6F Defined in EN 300 468
  //
  TEventInformationSection = class(TBaseSectionSyntax)
  private
    FServiceID: Word;
    FTransportStreamID: Word;
    FOriginalNetworkID: Word;
    FSegmentLastSectionNumber: Byte;
    FLastTableID: Byte;
    FEventList: TEventList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property ServiceID: Word read FServiceID;
    property TransportStreamID: Word read FTransportStreamID;
    property OriginalNetworkID: Word read FOriginalNetworkID;
    property SegmentLastSectionNumber: Byte read FSegmentLastSectionNumber;
    property LastTableID: Byte read FLastTableID;
    property EventList: TEventList read FEventList;
  end;

  // ---------------------------------------------------------------------------
  // TableID 0x70 .. 0x6F Defined in EN 300 468
  //
  TTimeDateSection = class(TBaseSection)
  private
    FUTCTime: TUTCTime;
  public
    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property UTCTime: TUTCTime read FUTCTime;
  end;

  // ---------------------------------------------------------------------------
  // TODO running_status_section TableID 0x71 .. 0x6F Defined in EN 300 468
  //

  // ---------------------------------------------------------------------------
  // TODO stuffing_section TableID 0x72 .. 0x6F Defined in EN 300 468
  //

  // ---------------------------------------------------------------------------
  // TableID 0x73 .. 0x6F Defined in EN 300 468
  //
  TTimeOffsetSection = class(TBaseSection)
  private
    FUTCTime: TUTCTime;
    FValidCRC: Boolean;
    FCRC: Cardinal;
    FDescriptors: TDescriptorList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property UTCTime: TUTCTime read FUTCTime;
    property Descriptors: TDescriptorList read FDescriptors;
    property CRC: Cardinal read FCRC;
    property ValidCRC: Boolean read FValidCRC;
  end;

  // ---------------------------------------------------------------------------
  // TODO resolution container section section TableID 0x74 .. 0x6F Defined TS 102 323 [36]
  //

  // ---------------------------------------------------------------------------
  // TODO container section TableID 0x75 .. 0x6F Defined in TS 102 323 [36]
  //

  // ---------------------------------------------------------------------------
  // TODO related content section TableID 0x76 .. 0x6F Defined in TS 102 323 [36]
  //

  // ---------------------------------------------------------------------------
  // TODO content identifier section TableID 0x77 .. 0x6F Defined in TS 102 323 [36]
  //

  // ---------------------------------------------------------------------------
  // TODO MPE-FEC section TableID 0x78 .. 0x6F Defined in EN 301 192 [37]
  //

  // ---------------------------------------------------------------------------
  // TableID 0x7E .. 0x6F Defined in EN 300 468
  //
  TDiscontinuityInformationSection = class(TBaseSection)
  private
    FTransitionFlag: Byte;
  public
    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property TransitionFlag: Byte read FTransitionFlag;
  end;

  // ---------------------------------------------------------------------------
  // TODO selection_information_section TableID 0x7F .. 0x6F Defined in EN 300 468
  //

  // ---------------------------------------------------------------------------
  // TableID 0xA0 .. premiere.de Content Information Section
  //
  TPremiereContentInformationSection = class(TBaseSectionSyntax)
  private
    FDescriptors: TDescriptorList;
    FContentID: Cardinal;
    FDuration: Cardinal;
    FDescriptorLoopLength: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property ContentID: Cardinal read FContentID;
    property Duration: Cardinal read FDuration;
    property DescriptorLoopLength: Integer read FDescriptorLoopLength;
    property Descriptors: TDescriptorList read FDescriptors;
  end;

implementation

uses
  MPEGUtils;
  
(*** TBaseSection *********************************************************)

constructor TBaseSection.Create;
begin
  inherited Create;
  Clear;
end;

constructor TBaseSection.Create(ABuffer: PByte; ASize: Integer);
begin
  Create;
  ParseBuffer(ABuffer, ASize);
end;

destructor TBaseSection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBaseSection.Clear;
begin
  FTableID := TABLE_ID_RESERVED;
  FSectionSyntaxIndicator := 0;
  FSectionLength := 0;
  FReserved1 := 0;
  FReserved2 := 0;

  FValid := False;
end;

function TBaseSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
begin
  Clear;
  Result := False;

  if not Assigned(ABuffer) or (ASize < 3)
    then Exit;

  FTableID := ABuffer^;
  inc(ABuffer);

  FSectionSyntaxIndicator := GetByteBits(ABuffer, 0, 1);
  FReserved1 := GetByteBits(ABuffer, 1, 1);
  FReserved2 := GetByteBits(ABuffer, 2, 2);
  FSectionLength := GetWordBits(ABuffer, 4, 12);

  if (FSectionLength + 3 <> ASize)
    then Exit;

  Result := True;
  FValid := True;
end;

(*** TBaseSectionSyntax ***************************************************)

procedure TBaseSectionSyntax.Clear;
begin
  inherited Clear;
  FUsesSectionSyntax := True;
  FReserved := 0;
  FVersionNumber := 0;
  FCurrentNextIndicator := 0;
  FSectionNumber := 0;
  FLastSectionNumber := 0;
  FValidCRC := False;
  FCRC := $FFFFFFFF;
end;

function TBaseSectionSyntax.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  tempBuffer: PByte;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if ((FSectionSyntaxIndicator = 0) and FUsesSectionSyntax) or (ASize < 12) then
  begin
    FValid := False;
    Exit;
  end;

  tempBuffer := ABuffer;

  // Table ID
  inc(ABuffer);

  // section_length
  inc(ABuffer, 2);

  FReserved := GetLongBits(ABuffer, 0, 18);
  inc(ABuffer, 2);

  FVersionNumber := GetByteBits(ABuffer, 2, 5);
  FCurrentNextIndicator := GetByteBits(ABuffer, 7, 1);
  inc(ABuffer, 1);

  FSectionNumber := ABuffer^;
  inc(ABuffer, 1);

  FLastSectionNumber := ABuffer^;
  inc(ABuffer, 1);

  // skip all Section Data and go directly to the CRC
  inc(ABuffer, FSectionLength - 4 - 5);
  FCRC := GetLong(ABuffer);

  if not FUsesSectionSyntax and not CheckPSICRC(tempBuffer, ASize) then
  begin
    FValid := False;
    Exit;
  end;

  FValidCRC := True;
  Result := True;
end;

(*** TProgramAssociationSection ***********************************************)

constructor TProgramAssociationSection.Create;
begin
  inherited Create;
  FProgramList := TList.Create;
end;

destructor TProgramAssociationSection.Destroy;
begin
  Clear;
  FreeAndNil(FProgramList);
  inherited Destroy;
end;

function TProgramAssociationSection.GetCountPrograms: integer;
begin
  Result := FProgramList.Count;
end;

function TProgramAssociationSection.GetProgram(Index: Integer): TProgramAssociationSectionProgram;
begin
  Result.ProgramNumber := 0;
  Result.NetworkPID := 0;
  if (Index >= 0) and (Index < FProgramList.Count)
    then Result := PProgramAssociationSectionProgram(FProgramList[Index])^;
end;

procedure TProgramAssociationSection.Clear;
begin
  inherited Clear;

  FTransportStreamID := 0;
  if Assigned(FProgramList) then
  begin
    while (FProgramList.Count > 0) do
    begin
      Dispose(PProgramAssociationSectionProgram(FProgramList[0]));
      FProgramList.Delete(0);
    end;
  end;
end;

function TProgramAssociationSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  c: Integer;
  prog: PProgramAssociationSectionProgram;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID <> TABLE_ID_PAT) or (FSectionLength > 1021) or (FReserved1 <> 0) then
  begin
    FValid := False;
    Exit;
  end;

  // Skip first 3 Bytes. Already parsed by BaseSection
  inc(ABuffer, 3);

  FTransportStreamID := GetWORD(ABuffer);
  inc(ABuffer, 5);

  c := FSectionLength + 3 - 8 - 4;
  while (c > 0) do
  begin
    new(prog);

    prog.ProgramNumber := GetWord(ABuffer);
    inc(ABuffer, 2);

    if prog.ProgramNumber = 0
      then prog.NetworkPID := GetWordBits(ABuffer, 3, 13)
      else prog.ProgramMapPID := GetWordBits(ABuffer, 3, 13);
    inc(Abuffer, 2);

    dec(c, 4);

    if (prog.ProgramNumber = 0) then
    begin
      if (prog.NetworkPID < $10) or (prog.NetworkPID > $1FFE) then
      begin
        Dispose(prog);
        Continue;
      end;
    end else
    begin
      if (prog.ProgramMapPID < $10) or (prog.ProgramMapPID > $1FFE) then
      begin
        Dispose(prog);
        Continue;
      end;
    end;

    FProgramList.Add(prog);
  end;

  Result := True;
end;

(*** TConditionalAccessSection ********************************************)

constructor TConditionalAccessSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
end;

destructor TConditionalAccessSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  inherited Destroy;
end;

procedure TConditionalAccessSection.Clear;
begin
  inherited Clear;

  if Assigned(FDescriptors)
    then FDescriptors.Clear;
end;

function TConditionalAccessSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  descriptors_loop_length: Integer;
  descriptor: TBaseDescriptor;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID <> TABLE_ID_CAT) or (FSectionLength > 1021) or (FReserved1 <> 0) then
  begin
    FValid := False;
    Exit;
  end;

  // Skip first 8 Bytes. Already parsed by BaseSection
  inc(ABuffer, 8);

  descriptors_loop_length := FSectionLength - 5 - 4;

  while (descriptors_loop_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

(*** TProgramStream *********************************************************)

constructor TProgramStream.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TProgramStream.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TProgramStream.Destroy;
begin
  Clear;
  FDescriptors.Free;
  inherited Destroy;
end;

procedure TProgramStream.Clear;
begin
  FStreamType := 0;
  FElementaryPID := 0;
  FDescriptors.Clear;
  FTotalLength := 0;
end;

function TProgramStream.ParseBuffer(ABuffer: PByte): Boolean;
var
  es_info_length: Integer;
  descriptor: TBaseDescriptor;
begin
  FStreamType := ABuffer^;
  inc(ABuffer);

  FElementaryPID := GetWordBits(ABuffer, 3, 13);
  inc(ABuffer, 2);

  es_info_length := GetWordBits(ABuffer, 4, 12);
  FTotalLength := es_info_length + 5;
  inc(ABuffer, 2);

  while (es_info_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(es_info_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

function TProgramStream.GetHasVideo: Boolean;
begin
  Result := IsVideoMPEG2 or IsVideoH264;
end;

function TProgramStream.GetHasAudio: Boolean;
begin
  Result := IsAudioMP1 or IsAudioMP2 or IsAudioAC3 or
            IsAudioAAC or IsAudioDTS;
end;

function TProgramStream.GetIsVideoMPEG2: Boolean;
begin
  Result := FStreamType = STREAM_TYPE_13818_2_VIDEO;
end;

function TProgramStream.GetIsVideoH264: Boolean;
begin
  Result := FStreamType = STREAM_TYPE_14496_10_H264_VIDEO;
end;

function TProgramStream.GetIsAudioAAC: Boolean;
var
  desc: TBaseDescriptor;
begin
  // TODO check if this is correct
  Result := FDescriptors.GetDescriptor(TAACDescriptor, desc) and
           (FStreamType = STREAM_TYPE_14496_3_AAC_AUDIO);
end;

function TProgramStream.GetIsAudioDTS: Boolean;
var
  desc: TBaseDescriptor;
begin
  // TODO check if this is correct
  Result := FDescriptors.GetDescriptor(TDTSDescriptor, desc);
end;

function TProgramStream.GetIsAudioMP1: Boolean;
begin
  Result := FStreamType = STREAM_TYPE_11172_AUDIO;
end;

function TProgramStream.GetIsAudioMP2: Boolean;
begin
  Result := FStreamType = STREAM_TYPE_13818_3_AUDIO;
end;

function TProgramStream.GetIsAudioAC3: Boolean;
var
  desc: TBaseDescriptor;
begin
  Result := FDescriptors.GetDescriptor(TAC3Descriptor, desc) and
           (FStreamType = STREAM_TYPE_13818_PES_PRIVATE_DATA);
end;

function TProgramStream.GetIsTeletext: Boolean;
var
  desc: TBaseDescriptor;
begin
  Result := FDescriptors.GetDescriptor(TTeletextDescriptor, desc) and
           (FStreamType = STREAM_TYPE_13818_PES_PRIVATE_DATA);
end;

function TProgramStream.GetIsMHPAIT: Boolean;
var
  desc: TBaseDescriptor;
begin
  Result := FDescriptors.GetDescriptor(TApplicationSignallingDescriptor, desc) and
           (FStreamType = STREAM_TYPE_13818_PRIVATE_SECTIONS);
end;

function TProgramStream.GetIsMHPData: Boolean;
var
  desc: TBaseDescriptor;
begin
  Result := FDescriptors.GetDescriptor(TCarouselIdentifierDescriptor, desc) and
           (FStreamType = STREAM_TYPE_13818_6_TYPE_B);
end;

function TProgramStream.GetIsDSMCC: Boolean;
begin
  Result := (FStreamType = STREAM_TYPE_13818_6_TYPE_B) and not GetIsMHPData;
end;

function TProgramStream.GetIsSubtitle: Boolean;
var
  desc: TBaseDescriptor;
begin
  Result := FDescriptors.GetDescriptor(TSubtitlingDescriptor, desc) and
           (FStreamType = STREAM_TYPE_13818_PES_PRIVATE_DATA);
end;

(*** TProgramStreamList *******************************************************)

function TProgramStreamList.Get(Index: Integer): TProgramStream;
begin
  Result := inherited Get(Index);
end;

procedure TProgramStreamList.Put(Index: Integer; Item: TProgramStream);
begin
  inherited Put(Index, Item);
end;

function TProgramStreamList.Add(Item: TProgramStream): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TProgramStreamList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TProgramStream(Items[i]).Free;
  inherited Clear;
end;

procedure TProgramStreamList.Delete(Index: Integer);
begin
  TProgramStream(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TProgramMapSection *******************************************************)

constructor TProgramMapSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  FProgramStreamList := TProgramStreamList.Create;
end;

destructor TProgramMapSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  FreeAndNil(FProgramStreamList);
  inherited Destroy;
end;

procedure TProgramMapSection.Clear;
begin
  inherited Clear;

  FProgramNumber := 0;
  FPCRPID := 0;

  if Assigned(FDescriptors)
    then FDescriptors.Clear;
  if Assigned(FProgramStreamList)
    then FProgramStreamList.Clear;
end;

function TProgramMapSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  program_info_length: Integer;
  stream_loop_length: Integer;
  descriptor: TBaseDescriptor;
  ps: TProgramStream;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID <> TABLE_ID_PMT) or (FSectionLength > 1021) or (FReserved1 <> 0) or (ASize < 16) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);

  if (FSectionSyntaxIndicator = 0) then
  begin
    FValid := False;
    Exit;
  end;

  FProgramNumber := GetWORD(ABuffer);
  inc(ABuffer, 5);

  FPCRPID := GetWordBits(ABuffer, 3, 13);
  inc(ABuffer, 2);

  program_info_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);
  stream_loop_length := (FSectionLength + 3 - 4) - (12 + program_info_length);

  while (program_info_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(program_info_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  while (stream_loop_length > 0) do
  begin
    ps := TProgramStream.Create(ABuffer);
    FProgramStreamList.Add(ps);
    dec(stream_loop_length, ps.TotalLength);
    inc(ABuffer, ps.TotalLength);
  end;

  Result := True;
end;

(*** TTransportStream *********************************************************)

constructor TTransportStream.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TTransportStream.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TTransportStream.Destroy;
begin
  Clear;
  FDescriptors.Free;
  inherited Destroy;
end;

procedure TTransportStream.Clear;
begin
  FTransportStreamID := 0;
  FOriginalNetworkID := 0;
  FDescriptors.Clear;
  FTotalLength := 0;
end;

function TTransportStream.ParseBuffer(ABuffer: PByte): Boolean;
var
  transport_descriptors_length: Integer;
  descriptor: TBaseDescriptor;
begin
  FTransportStreamID := GetWORD(ABuffer);
  inc(ABuffer, 2);

  FOriginalNetworkID := GetWORD(ABuffer);
  inc(ABuffer, 2);

  transport_descriptors_length := GetWordBits(ABuffer, 4, 12);
  FTotalLength := transport_descriptors_length + 6;
  inc(ABuffer, 2);

  while (transport_descriptors_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(transport_descriptors_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

(*** TTransportStreamList *****************************************************)

function TTransportStreamList.Get(Index: Integer): TTransportStream;
begin
  Result := inherited Get(Index);
end;

procedure TTransportStreamList.Put(Index: Integer; Item: TTransportStream);
begin
  inherited Put(Index, Item);
end;

function TTransportStreamList.Add(Item: TTransportStream): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TTransportStreamList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TTransportStream(Items[i]).Free;
  inherited Clear;
end;

procedure TTransportStreamList.Delete(Index: Integer);
begin
  TTransportStream(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TBaseTransportStreamSection **********************************************)

constructor TBaseTransportStreamSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  FTransportStreamList := TTransportStreamList.Create;
end;

destructor TBaseTransportStreamSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  FreeAndNil(FTransportStreamList);
  inherited Destroy;
end;

procedure TBaseTransportStreamSection.Clear;
begin
  inherited Clear;

  FUnknownID := 0;
  if Assigned(FDescriptors)
    then FDescriptors.Clear;
  if Assigned(FTransportStreamList)
    then FTransportStreamList.Clear;
end;

function TBaseTransportStreamSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  network_descriptors_length: Integer;
  transport_stream_loop_length: Integer;
  descriptor: TBaseDescriptor;
  ts: TTransportStream;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if ((FTableID <> TABLE_ID_NIT_ACTUAL) and (FTableID <> TABLE_ID_NIT_OTHER) and (FTableID <> TABLE_ID_BAT)) or
     (FSectionLength > 1021) or (ASize < 16) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);
  FUnknownID := GetWORD(ABuffer);
  inc(ABuffer, 5);

  network_descriptors_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);
  while (network_descriptors_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(network_descriptors_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  transport_stream_loop_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);
  while (transport_stream_loop_length > 0) do
  begin
    ts := TTransportStream.Create(ABuffer);
    FTransportStreamList.Add(ts);
    dec(transport_stream_loop_length, ts.TotalLength);
    inc(ABuffer, ts.TotalLength);
  end;

  Result := True;
end;

(*** TService *****************************************************************)

constructor TService.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TService.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TService.Destroy;
begin
  Clear;
  FDescriptors.Free;
  inherited Destroy;
end;

procedure TService.Clear;
begin
  FServiceID := 0;
  FEITScheduleFlag := 0;
  FEITPresentFollowingFlag := 0;
  FRunningStatus := rsReserved;
  FFreeCAMode := 0;
  FDescriptors.Clear;
  FTotalLength := 0;
end;

function TService.ParseBuffer(ABuffer: PByte): Boolean;
var
  descriptor: TBaseDescriptor;
  descriptors_loop_length: Integer;
begin
  FServiceID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FEITScheduleFlag := GetByteBits(ABuffer, 6, 1);
  FEITPresentFollowingFlag := GetByteBits(ABuffer, 7, 1);
  inc(ABuffer);

  FRunningStatus := GetRunningStatus(GetByteBits(ABuffer, 0, 3));
  FFreeCAMode := GetByteBits(ABuffer, 3, 1);
  descriptors_loop_length := GetWordBits(ABuffer, 4, 12);
  FTotalLength := descriptors_loop_length + 5;
  inc(ABuffer, 2);

  while (descriptors_loop_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

(*** TServiceList *************************************************************)

function TServiceList.Get(Index: Integer): TService;
begin
  Result := inherited Get(Index);
end;

procedure TServiceList.Put(Index: Integer; Item: TService);
begin
  inherited Put(Index, Item);
end;

function TServiceList.Add(Item: TService): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TServiceList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TService(Items[i]).Free;
  inherited Clear;
end;

procedure TServiceList.Delete(Index: Integer);
begin
  TService(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TServiceDescriptionSection ***********************************************)

constructor TServiceDescriptionSection.Create;
begin
  inherited Create;
  FServiceList := TServiceList.Create;
end;

destructor TServiceDescriptionSection.Destroy;
begin
  FreeAndNil(FServiceList);
  inherited Destroy;
end;

procedure TServiceDescriptionSection.Clear;
begin
  inherited Clear;

  FTransportStreamID := 0;
  FOriginalNetworkID := 0;

  if Assigned(FServiceList)
    then FServiceList.Clear;
end;

function TServiceDescriptionSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  program_loop_length: Integer;
  service: TService;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if ((FTableID <> TABLE_ID_SDT_ACTUAL) and (FTableID <> TABLE_ID_SDT_OTHER)) or
     (FSectionLength > 1021) or (ASize < 15) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);
  FTransportStreamID := GetWORD(ABuffer);
  inc(ABuffer, 5);

  FOriginalNetworkID := GetWord(ABuffer);
  inc(ABuffer, 2);
  inc(ABuffer);

  program_loop_length := FSectionLength - 8 - 4;
  while (program_loop_length > 0) do
  begin
    service := TService.Create(ABuffer);
    FServiceList.Add(service);
    dec(program_loop_length, service.TotalLength);
    inc(ABuffer, service.TotalLength);
  end;

  Result := True;
end;

(*** TEvent *******************************************************************)

constructor TEvent.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TEvent.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TEvent.Destroy;
begin
  Clear;
  FDescriptors.Free;
  inherited Destroy;
end;

procedure TEvent.Clear;
begin
  FEventID := 0;
  FillChar(FStartTime, 5, 0);
  FillChar(FDuration, 3, 0);
  FRunningStatus := rsReserved;
  FFreeCAMode := 0;
  FDescriptors.Clear;
  FTotalLength := 0;
end;

function TEvent.ParseBuffer(ABuffer: PByte): Boolean;
var
  descriptor: TBaseDescriptor;
  descriptors_loop_length: Integer;
begin
  FEventID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FStartTime := GetUTCTime(ABuffer);
  inc(ABuffer, 5);

  FDuration := GetBCDTime(ABuffer);
  inc(ABuffer, 3);

  FRunningStatus := GetRunningStatus(GetByteBits(ABuffer, 0, 3));
  FFreeCAMode := GetByteBits(ABuffer, 3, 1);
  descriptors_loop_length := GetWordBits(ABuffer, 4, 12);
  FTotalLength := descriptors_loop_length + 12;
  inc(ABuffer, 2);

  while (descriptors_loop_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

(*** TServiceList *************************************************************)

function TEventList.Get(Index: Integer): TEvent;
begin
  Result := inherited Get(Index);
end;

procedure TEventList.Put(Index: Integer; Item: TEvent);
begin
  inherited Put(Index, Item);
end;

function TEventList.Add(Item: TEvent): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TEventList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TEvent(Items[i]).Free;
  inherited Clear;
end;

procedure TEventList.Delete(Index: Integer);
begin
  TEvent(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TEventInformationSection *************************************************)

constructor TEventInformationSection.Create;
begin
  inherited Create;
  FEventList := TEventList.Create;
end;

destructor TEventInformationSection.Destroy;
begin
  FreeAndNil(FEventList);
  inherited Destroy;
end;

procedure TEventInformationSection.Clear;
begin
  inherited Clear;

  FServiceID := 0;
  FTransportStreamID := 0;
  FOriginalNetworkID := 0;
  FSegmentLastSectionNumber := 0;

  if Assigned(FEventList)
    then FEventList.Clear;
end;

function TEventInformationSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  event_loop_length: Integer;
  event: TEvent;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID < TABLE_ID_EIT_ACTUAL_PRESENT) or (FTableID > TABLE_ID_EIT_OTHER_SCHEDULE_MAX) or
     (FSectionLength > 4093) or (ASize < 18) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);
  FServiceID := GetWORD(ABuffer);
  inc(ABuffer, 5);

  FTransportStreamID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FOriginalNetworkID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FSegmentLastSectionNumber := ABuffer^;
  inc(ABuffer);

  FLastTableID := ABuffer^;
  inc(ABuffer);

  event_loop_length := FSectionLength - 11 - 4;
  while (event_loop_length > 0) do
  begin
    event := TEvent.Create(ABuffer);
    FEventList.Add(event);
    dec(event_loop_length, event.TotalLength);
    inc(ABuffer, event.TotalLength);
  end;

  Result := True;
end;

(*** TTimeDateSection *********************************************************)

procedure TTimeDateSection.Clear;
begin
  inherited Clear;
  FillChar(FUTCTime, 5, 0);
end;

function TTimeDateSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (ASize <> 8) or (FTableID <> TABLE_ID_TDT) then
  begin
    FValid := False;
    Exit;
  end;

  if (FSectionLength <> 5) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);
  FUTCTime := GetUTCTime(ABuffer);

  Result := True;
end;

(*** TTimeOffsetSection *******************************************************)

constructor TTimeOffsetSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
end;

destructor TTimeOffsetSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  inherited Destroy;
end;

procedure TTimeOffsetSection.Clear;
begin
  inherited Clear;

  FillChar(FUTCTime, 5, 0);
  FCRC := $FFFFFFFF;
  FValidCRC := False;

  if Assigned(FDescriptors)
    then FDescriptors.Clear;
end;

function TTimeOffsetSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  descriptors_loop_length: Integer;
  descriptor: TBaseDescriptor;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (ASize < 14) or (FTableID <> TABLE_ID_TOT) or (FSectionLength > 1023) then
  begin
    FValid := False;
    Exit;
  end;

  FValidCRC := CheckPSICRC(ABuffer, ASize);
  if not FValidCRC then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);

  FUTCTime := GetUTCTime(ABuffer);
  inc(ABuffer, 5);

  descriptors_loop_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);

  while (descriptors_loop_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  FCRC := GetLong(ABuffer);

  Result := True;
end;

(*** TDiscontinuityInformationSection *****************************************)

procedure TDiscontinuityInformationSection.Clear;
begin
  inherited Clear;
  FTransitionFlag := 0;
end;

function TDiscontinuityInformationSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (ASize <> 4) or (FTableID <> TABLE_ID_DIT) then
  begin
    FValid := False;
    Exit;
  end;

  if (FSectionLength <> 1) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);
  FTransitionFlag := GetByteBits(ABuffer, 0, 1);

  Result := True;
end;

(*** TDiscontinuityInformationSection *****************************************)

constructor TPremiereContentInformationSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
end;

destructor TPremiereContentInformationSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  inherited Destroy;
end;

procedure TPremiereContentInformationSection.Clear;
begin
  inherited Clear;

  FContentID := 0;
  FDuration := 0;
  FDescriptorLoopLength := 0;

  if Assigned(FDescriptors)
    then FDescriptors.Clear;
end;

function TPremiereContentInformationSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  b: Cardinal;
  len: Cardinal;
  descriptors_loop_length: Integer;
  descriptor: TBaseDescriptor;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID <> $A0) or (ASize < 18) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 8);

  FContentID := GetLong(ABuffer);
  inc(ABuffer, 4);

  len := GetLongBits(ABuffer, 0, 24);
  inc(ABuffer, 3);

  b := HexToDec(len and $FF);
  FDuration := b;
  b := HexToDec((len shr 8) and $FF);
  FDuration := FDuration + (b * 60);
  b := HexToDec((len shr 16) and $FF);
  FDuration := FDuration + (b * 60 * 60);

  FDescriptorLoopLength := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);

  descriptors_loop_length := FDescriptorLoopLength;
  while (descriptors_loop_length > 0) do
  begin
    descriptor := TBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

end.
