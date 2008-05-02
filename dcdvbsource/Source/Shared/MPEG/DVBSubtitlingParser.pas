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

unit DVBSubtitlingParser;

interface

uses
  SysUtils, MPEGUtils, Classes, Graphics, MPEGParser, Windows, Math, MPEGConst,
  BitReader;

type
  TSegmentType = (
    stReserved = $0,
    stPageComposition = $10,
    stRegionComposition,
    stCLUTDefinition,
    stObjectData,
    stEndOfDisplaySet = $80,
    stPrivateDataStart,
    stPrivateDataEnd = $EF,
    stStuffing = $FF
  );

  TPageState = (
    psNormalCase,
    psAcquisitionPoint,
    psModeChange,
    psReserved
  );

  TRegionLevelOfCompatibility = (
    rlcReserved,
    rlc2Bit,
    rlc4Bit,
    rlc8Bit,
    rlcReserved4,
    rlcReserved5,
    rlcReserved6,
    rlcReserved7
  );

  TRegionDepth = (
    rdReserved0,
    rd2Bit,
    rd4Bit,
    rd8Bit,
    rdReserved4,
    rdReserved5,
    rdReserved6,
    rdReserved7
  );

  TObjectType = (
    otBasicObjectBitmap,
    otBasicObjectCharacter,
    otCompositeObject,
    otReserved
  );

  TObjectProviderFlag = (
    opfSubtitleStream,
    opfROM,
    opfReserved2,
    opfReserved3
  );

  TDVBSubtitlingRegion = record
    RegionID: Byte;
    RegionHorizontalAddress: Word;
    RegionVerticalAddress: Word;
  end;
  PDVBSubtitlingRegion = ^TDVBSubtitlingRegion;

  TDVBSubtitlingSegmentObject = record
    ObjectID: Word;
    ObjectType: TObjectType;
    ObjectProviderFlag: TObjectProviderFlag;
    ObjectHorizontalPosition: Word;
    ObjectVerticalPosition: Word;
    ForegroundPixelCode: Byte;
    BackgroundPixelCode: Byte;
  end;
  PDVBSubtitlingSegmentObject = ^TDVBSubtitlingSegmentObject;

  TDVBSubtitlingCLUT = record
    CLUTEntryID: Byte;
    N2BitEntryCLUTFlag: Boolean;
    N4BitEntryCLUTFlag: Boolean;
    N8BitEntryCLUTFlag: Boolean;
    FullRangeFlag: Boolean;
    YValue: Byte;
    CrValue: Byte;
    CbValue: Byte;
    TValue: Byte;
  end;
  PDVBSubtitlingCLUT = ^TDVBSubtitlingCLUT;

  TDVBSubtitlingBaseSegment = class
  protected
    FSyncByte: Byte;
    FSegmentType: TSegmentType;
    FPageID: Word;
    FSegmentLength: Integer;
    FTotalLength: Integer;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte); overload; virtual;
    destructor Destroy; override;

    procedure ParseBuffer(ABuffer: PByte); virtual;
    procedure Clear; virtual;

    property SyncByte: Byte read FSyncByte;
    property SegmentType: TSegmentType read FSegmentType;
    property PageID: Word read FPageID;
    property SegmentLength: Integer read FSegmentLength;
    property TotalLength: Integer read FTotalLength;
  end;

  TDVBSubtitlingCompositionPageSegment = class(TDVBSubtitlingBaseSegment)
  protected
    FPageTimeOut: Byte;
    FPageVersionNumber: Byte;
    FPageState: TPageState;
    FRegions: array of TDVBSubtitlingRegion;
    function GetCountRegions: Integer;
    function GetRegion(AIndex: Integer): PDVBSubtitlingRegion;
  public
    procedure ParseBuffer(ABuffer: PByte); override;
    procedure Clear; override;

    property PageTimeOut: Byte read FPageTimeOut;
    property PageVersionNumber: Byte read FPageVersionNumber;
    property PageState: TPageState read FPageState;

    property CountRegions: Integer read GetCountRegions;
    property Region[AIndex: Integer]: PDVBSubtitlingRegion read GetRegion; default;
  end;

  TDVBSubtitlingCompositionRegionSegment = class(TDVBSubtitlingBaseSegment)
  protected
    FRegionID: Byte;
    FRegionVersionNumber: Byte;
    FRegionFillFlag: Boolean;
    FRegionWidth: Word;
    FRegionHeight: Word;
    FRegionLevelOfCompatibility: TRegionLevelOfCompatibility;
    FRegionDepth: TRegionDepth;
    FCLUTID: Byte;
    FRegion8BitPixelCode: Byte;
    FRegion4BitPixelCode: Byte;
    FRegion2BitPixelCode: Byte;
    FObjects: array of TDVBSubtitlingSegmentObject;
    function GetCountObjects: Integer;
    function GetObject(AIndex: Integer): PDVBSubtitlingSegmentObject;
  public
    procedure ParseBuffer(ABuffer: PByte); override;
    procedure Clear; override;

    property RegionID: Byte read FRegionID write FRegionID;
    property RegionVersionNumber: Byte read FRegionVersionNumber;
    property RegionFillFlag: Boolean read FRegionFillFlag;
    property RegionWidth: Word read FRegionWidth;
    property RegionHeight: Word read FRegionHeight;
    property RegionLevelOfCompatibility: TRegionLevelOfCompatibility read FRegionLevelOfCompatibility;
    property RegionDepth: TRegionDepth read FRegionDepth;
    property CLUTID: Byte read FCLUTID;
    property Region8BitPixelCode: Byte read FRegion8BitPixelCode;
    property Region4BitPixelCode: Byte read FRegion4BitPixelCode;
    property Region2BitPixelCode: Byte read FRegion2BitPixelCode;

    property CountObjects: Integer read GetCountObjects;
    property Objects[AIndex: Integer]: PDVBSubtitlingSegmentObject read GetObject; default;
  end;

  TDVBSubtitlingCLUTDefinitionSegment = class(TDVBSubtitlingBaseSegment)
  protected
    FRGBTableCreated: Boolean;
    FCLUTID: Byte;
    FCLUTVersionNumber: Byte;
    FCLUTs: array of TDVBSubtitlingCLUT;
    FCLUT2: array[0..3] of Cardinal;
    FCLUT4: array[0..15] of Cardinal;
    FCLUT8: array[0..255] of Cardinal;
    function GetCountCLUTs: Integer;
    function GetCLUT(AIndex: Integer): PDVBSubtitlingCLUT;
  public
    procedure ParseBuffer(ABuffer: PByte); override;
    procedure Clear; override;

    procedure CreateRGBTable;
    function GetColor(ADepth: TRegionDepth; AIndex: Integer): Cardinal;

    property CLUTID: Byte read FCLUTID;
    property CLUTVersionNumber: Byte read FCLUTVersionNumber;

    property CountCLUTs: Integer read GetCountCLUTs;
    property CLUT[AIndex: Integer]: PDVBSubtitlingCLUT read GetCLUT; default;
  end;

  TDVBSubtitlingObjectDataSegment = class(TDVBSubtitlingBaseSegment)
  protected
    FObjectID: Word;
    FObjectVersionNumber: Byte;
    FObjectCodingMethod: Byte;
    FNonModifyingColourFlag: Boolean;
    FTopFieldDataBlockLength: Word;
    FBottomFieldDataBlockLength: Word;
    FTopFieldData: PByte;
    FBottomFieldData: PByte;
    FCharacterCodeLength: Integer;
    FCharacterCode: WideString;
  public
    procedure ParseBuffer(ABuffer: PByte); override;
    procedure Clear; override;

    property ObjectID: Word read FObjectID write FObjectID;
    property ObjectVersionNumber: Byte read FObjectVersionNumber;
    property ObjectCodingMethod: Byte read FObjectCodingMethod;
    property NonModifyingColourFlag: Boolean read FNonModifyingColourFlag;
    property TopFieldDataBlockLength: Word read FTopFieldDataBlockLength;
    property BottomFieldDataBlockLength: Word read FBottomFieldDataBlockLength;
    property TopFieldData: PByte read FTopFieldData;
    property BottomFieldData: PByte read FBottomFieldData;
    property CharacterCodeLength: Integer read FCharacterCodeLength;
    property CharacterCode: WideString read FCharacterCode;
  end;

  TDVBSubtitlingEndOfDisplaySetSegment = class(TDVBSubtitlingBaseSegment)
  end;

  TDVBSubtitlingDisplaySet = class
  private
    FPTS: Int64;
    FList: TList;
    FValid: Boolean;
    function GetCount: Integer;
    function GetSegment(AIndex: Integer): TDVBSubtitlingBaseSegment;
    function GetCountRegions: Integer;
    function GetRegion(AIndex: Integer): TDVBSubtitlingCompositionRegionSegment;
    function GetCountObjects: Integer;
    function GetObject(AIndex: Integer): TDVBSubtitlingObjectDataSegment;
    function GetCountCLUTs: Integer;
    function GetCLUT(AIndex: Integer): TDVBSubtitlingCLUTDefinitionSegment;
    function GetCountCompositionPages: Integer;
    function GetCompositionPage2(AIndex: Integer): TDVBSubtitlingCompositionPageSegment;
  protected
    procedure DeleteRegion(ARegionID: Integer);
    procedure DeleteCLUT(ACLUTID: Integer);
    procedure DeleteObject(AObjectID: Integer);
    procedure DeletePageComposition;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear(AKeepSegments: Boolean = False);
    procedure AddSegment(ASegment: TDVBSubtitlingBaseSegment); overload;
    procedure AddDisplaySet(ADisplaySet: TDVBSubtitlingDisplaySet); overload;
    procedure AddAllDisplaySet(ADisplaySet: TDVBSubtitlingDisplaySet);
    function GetCompositionPage: TDVBSubtitlingCompositionPageSegment;
    function GetCLUTForCLUTID(ACLUTID: Integer): TDVBSubtitlingCLUTDefinitionSegment;

    property PTS: Int64 read FPTS write FPTS;
    property Valid: Boolean read FValid;
    property Count: Integer read GetCount;
    property Segment[AIndex: Integer]: TDVBSubtitlingBaseSegment read GetSegment; default;
    property CountRegions: Integer read GetCountRegions;
    property Region[AIndex: Integer]: TDVBSubtitlingCompositionRegionSegment read GetRegion;
    property CountObjects: Integer read GetCountObjects;
    property Object_[AIndex: Integer]: TDVBSubtitlingObjectDataSegment read GetObject;
    property CountCLUTs: Integer read GetCountCLUTs;
    property CLUT[AIndex: Integer]: TDVBSubtitlingCLUTDefinitionSegment read GetCLUT;
    property CountCompositionPages: Integer read GetCountCompositionPages;
    property CompositionPage[AIndex: Integer]: TDVBSubtitlingCompositionPageSegment read GetCompositionPage2;
  end;

  TDVBSubtitlingDisplaySetList = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetDisplaySet(AIndex: Integer): TDVBSubtitlingDisplaySet;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(ADisplaySet: TDVBSubtitlingDisplaySet);
    procedure Delete(AIndex: Integer);

    function GetDisplaySetForPTS(APTS: Int64): TDVBSubtitlingDisplaySet;

    property Count: Integer read GetCount;
    property DisplaySet[AIndex: Integer]: TDVBSubtitlingDisplaySet read GetDisplaySet; default;
  end;

  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..(1 shl 24)] of Cardinal;

  TDVBSubtitlingParser = class
  private
    FEnabled: Boolean;
    FCurrentSet: TDVBSubtitlingDisplaySet;
    FSetList: TDVBSubtitlingDisplaySetList;
    FFont: TFont;
    FCompositionPageID: Integer;
    FAncillaryPageID: Integer;
    FPCRTSParser: TTSParser;
    FSubTSParser: TTSParser;
    FSubtitleParser: TSubtitleParser;
    FPCR: TPESParser;
    FWidth: Integer;
    FHeight: Integer;
    FBufferSize: Integer;
    FBitmap: THandle;
    FDevice: THandle;
    FBuffer: PCardinalArray;
    FTimeOut: Double;
    FBitReader: TBitReader;
    procedure OnPCR(APCR: Int64);
    procedure OnSubtitleData(ABuffer: PByte; ASize: Integer);
    procedure DrawSet(ADisplaySet: TDVBSubtitlingDisplaySet);
    procedure DrawPage(APage: TDVBSubtitlingCompositionPageSegment);
    procedure DrawRegion(APage: TDVBSubtitlingCompositionPageSegment; ARegion:
      PDVBSubtitlingRegion; ARegionSegment: TDVBSubtitlingCompositionRegionSegment);
    procedure DrawBitmap(ARegion: PDVBSubtitlingRegion;
      ARegionSegment: TDVBSubtitlingCompositionRegionSegment;
      ACLUT: TDVBSubtitlingCLUTDefinitionSegment; AObject: PDVBSubtitlingSegmentObject;
      AObjectSegment: TDVBSubtitlingObjectDataSegment);
    procedure DrawString(ARegion: PDVBSubtitlingRegion;
      ARegionSegment: TDVBSubtitlingCompositionRegionSegment;
      ACLUT: TDVBSubtitlingCLUTDefinitionSegment; AObject: PDVBSubtitlingSegmentObject;
      AObjectSegment: TDVBSubtitlingObjectDataSegment);
    procedure FillRect(AX, AY, AWidth, AHeight: Integer; AColor: Cardinal);
    procedure TextOut(AX, AY: Integer; const AText: WideString; AFGColor: Cardinal; ABGColor: Cardinal);
    procedure SetPixel(AX, AY: Integer; AColor: Cardinal);
    procedure SetEnabled(AEnabled: Boolean);
    function GetPID: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ErasePage;
    procedure Flush;

    procedure ParsePCRBuffer(ABuffer: PByte; ASize: Integer);
    procedure ParseSubtitleBuffer(ABuffer: PByte; ASize: Integer);

    property BitmapWidth: Integer read FWidth;
    property BitmapHeight: Integer read FHeight;
    property TimeOut: Double read FTimeOut write FTimeOut;
    property Buffer: PCardinalArray read FBuffer;

    procedure SetPID(APCRPID: Integer; APID: Integer;
      ACompositionPageID: Integer;
      AAncillaryPageID: Integer);

    property Enabled: Boolean read FEnabled write SetEnabled;
    property PID: Integer read GetPID;
  end;

implementation

const
  DEFAULT_WIDTH         = 720;
  DEFAULT_HEIGHT        = 576;
  MAXIMUM_PAGE_SETS     = 100;
  SEGMENT_SYNC_BYTE     = $0F;

  LUT_DEFAULT_2: array[0..3] of Cardinal =
  (
    $00000000, $FFFFFFFF, $FF000000, $FF7F7F7F
  );

  LUT_DEFAULT_4: array[0..15] of Cardinal =
  (
    $00000000, $FFFF0000, $FF00FF00, $FFFFFF00, $FF0000FF, $FFFF00FF,
    $FF00FFFF, $FFFFFFFF, $FF000000, $FF7F0000, $FF007F00, $FF7F7F00,
    $FF00007F, $FF7F007F, $FF007F7F, $FF7F7F7F
  );

  LUT_DEFAULT_8: array[0..255] of Cardinal =
  (
    $00000000, $BF0000FF, $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF,
    $BFFFFF00, $BFFFFFFF, $80000000, $80000055, $80005500, $80005555,
    $80550000, $80550055, $80555500, $80555555, $BF000000, $BF0000FF,
    $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF, $BFFFFF00, $BFFFFFFF,
    $800000AA, $800000FF, $800055AA, $800055FF, $805500AA, $805500FF,
    $805555AA, $805555FF, $BF000000, $BF0000FF, $BF00FF00, $BF00FFFF,
    $BFFF0000, $BFFF00FF, $BFFFFF00, $BFFFFFFF, $8000AA00, $8000AA55,
    $8000FF00, $8000FF55, $8055AA00, $8055AA55, $8055FF00, $8055FF55,
    $BF000000, $BF0000FF, $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF,
    $BFFFFF00, $BFFFFFFF, $8000AAAA, $8000AAFF, $8000FFAA, $8000FFFF,
    $8055AAAA, $8055AAFF, $8055FFAA, $8055FFFF, $BF000000, $BF0000FF,
    $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF, $BFFFFF00, $BFFFFFFF,
    $80AA0000, $80AA0055, $80AA5500, $80AA5555, $80FF0000, $80FF0055,
    $80FF5500, $80FF5555, $BF000000, $BF0000FF, $BF00FF00, $BF00FFFF,
    $BFFF0000, $BFFF00FF, $BFFFFF00, $BFFFFFFF, $80AA00AA, $80AA00FF,
    $80AA55AA, $80AA55FF, $80FF00AA, $80FF00FF, $80FF55AA, $80FF55FF,
    $BF000000, $BF0000FF, $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF,
    $BFFFFF00, $BFFFFFFF, $80AAAA00, $80AAAA55, $80AAFF00, $80AAFF55,
    $80FFAA00, $80FFAA55, $80FFFF00, $80FFFF55, $BF000000, $BF0000FF,
    $BF00FF00, $BF00FFFF, $BFFF0000, $BFFF00FF, $BFFFFF00, $BFFFFFFF,
    $80AAAAAA, $80AAAAFF, $80AAFFAA, $80AAFFFF, $80FFAAAA, $80FFAAFF,
    $80FFFFAA, $80FFFFFF, $FF7F7F7F, $FF7F7FAA, $FF7FAA7F, $FF7FAAAA,
    $FFAA7F7F, $FFAA7FAA, $FFAAAA7F, $FFAAAAAA, $FF000000, $FF00002B,
    $FF002B00, $FF002B2B, $FF2B0000, $FF2B002B, $FF2B2B00, $FF2B2B2B,
    $FF7F7FD4, $FF7F7FFF, $FF7FAAD4, $FF7FAAFF, $FFAA7FD4, $FFAA7FFF,
    $FFAAAAD4, $FFAAAAFF, $FF000055, $FF000080, $FF002B55, $FF002B80,
    $FF2B0055, $FF2B0080, $FF2B2B55, $FF2B2B80, $FF7FD47F, $FF7FD4AA,
    $FF7FFF7F, $FF7FFFAA, $FFAAD47F, $FFAAD4AA, $FFAAFF7F, $FFAAFFAA,
    $FF005500, $FF00552B, $FF008000, $FF00802B, $FF2B5500, $FF2B552B,
    $FF2B8000, $FF2B802B, $FF7FD4D4, $FF7FD4FF, $FF7FFFD4, $FF7FFFFF,
    $FFAAD4D4, $FFAAD4FF, $FFAAFFD4, $FFAAFFFF, $FF005555, $FF005580,
    $FF008055, $FF008080, $FF2B5555, $FF2B5580, $FF2B8055, $FF2B8080,
    $FFD47F7F, $FFD47FAA, $FFD4AA7F, $FFD4AAAA, $FFFF7F7F, $FFFF7FAA,
    $FFFFAA7F, $FFFFAAAA, $FF550000, $FF55002B, $FF552B00, $FF552B2B,
    $FF800000, $FF80002B, $FF802B00, $FF802B2B, $FFD47FD4, $FFD47FFF,
    $FFD4AAD4, $FFD4AAFF, $FFFF7FD4, $FFFF7FFF, $FFFFAAD4, $FFFFAAFF,
    $FF550055, $FF550080, $FF552B55, $FF552B80, $FF800055, $FF800080,
    $FF802B55, $FF802B80, $FFD4D47F, $FFD4D4AA, $FFD4FF7F, $FFD4FFAA,
    $FFFFD47F, $FFFFD4AA, $FFFFFF7F, $FFFFFFAA, $FF555500, $FF55552B,
    $FF558000, $FF55802B, $FF805500, $FF80552B, $FF808000, $FF80802B,
    $FFD4D4D4, $FFD4D4FF, $FFD4FFD4, $FFD4FFFF, $FFFFD4D4, $FFFFD4FF,
    $FFFFFFD4, $FFFFFFFF, $FF555555, $FF555580, $FF558055, $FF558080,
    $FF805555, $FF805580, $FF808055, $FF808080
  );

var
  DefaultCLUT: TDVBSubtitlingCLUTDefinitionSegment;

function yuv2rgb(Y: Integer; Cb: Integer; Cr: Integer): Cardinal;
var
  r, g, b: Integer;
begin
  Cb := Cb - 128;
  Cr := Cr - 128;
  y := y * 256;

  r := (y + 351 * Cr) shr 8;
  g := (y - 179 * Cr - 87 * Cb) shr 8;
  b := (y + 444 * Cb) shr 8;

  if r < 0 then r := 0 else if r > 255 then r := 255;
  if g < 0 then g := 0 else if g > 255 then g := 255;
  if b < 0 then b := 0 else if b > 255 then b := 255;

  Result := (b shl 16) or (g shl 8) or r;
end;

function GetPageState(ABuffer: PByte): TPageState;
begin
  Result := TPageState(GetByteBits(ABuffer, 4, 2));
end;

function GetSegmentType(ABuffer: PByte): TSegmentType;
begin
  Result := TSegmentType(ABuffer^);
end;

function GetRegionLevelOfCompatibility(ABuffer: PByte): TRegionLevelOfCompatibility;
begin
  Result := TRegionLevelOfCompatibility(GetByteBits(ABuffer, 0, 3));
end;

function GetRegionDepth(ABuffer: PByte): TRegionDepth;
begin
  Result := TRegionDepth(GetByteBits(ABuffer, 3, 3));
end;

function GetObjectType(ABuffer: PByte): TObjectType;
begin
  Result := TObjectType(GetByteBits(ABuffer, 0, 2));
end;

function GetObjectProviderFlag(ABuffer: PByte): TObjectProviderFlag;
begin
  Result := TObjectProviderFlag(GetByteBits(ABuffer, 2, 2));
end;

function CreateSubtitleSegment(ABuffer: PByte): TDVBSubtitlingBaseSegment;
begin
  Result := nil;
  
  if not Assigned(ABuffer)
    then Exit;

  if (ABuffer^ <> SEGMENT_SYNC_BYTE)
    then Exit;

  case GetSegmentType(PByte(Cardinal(ABuffer)+1)) of
    stPageComposition:    Result := TDVBSubtitlingCompositionPageSegment.Create(ABuffer);
    stRegionComposition:  Result := TDVBSubtitlingCompositionRegionSegment.Create(ABuffer);
    stCLUTDefinition:     Result := TDVBSubtitlingCLUTDefinitionSegment.Create(ABuffer);
    stObjectData:         Result := TDVBSubtitlingObjectDataSegment.Create(ABuffer);
    stEndOfDisplaySet:    Result := TDVBSubtitlingEndOfDisplaySetSegment.Create(ABuffer);
    else                  Result := TDVBSubtitlingBaseSegment.Create(ABuffer);
  end;
end;

function IsValidSubtitlePage(ABuffer: PByte; ACompositionPageID: Integer; AAncillaryPageID: Integer; out ASize: Integer): Boolean;
var
  page_id: Integer;
  segment_type: TSegmentType;
begin
  Result := False;
  ASize := 0;

  if ABuffer^ <> SEGMENT_SYNC_BYTE
    then Exit;

  inc(ABuffer);

  segment_type := GetSegmentType(ABuffer);
  inc(ABuffer);

  page_id := GetWord(ABuffer);
  inc(ABuffer, 2);

  ASize := GetWord(ABuffer);
  inc(ASize, 6);

  if ((page_id <> ACompositionPageID) and (page_id <> AAncillaryPageID))
    then Exit;

  if ((segment_type = stPageComposition) or (segment_type = stRegionComposition)) and (page_id <> ACompositionPageID)
    then Exit;

  if not (segment_type = stPageComposition) and
     not (segment_type = stRegionComposition) and
     not (segment_type = stCLUTDefinition) and
     not (segment_type = stObjectData) and
     not (segment_type = stEndOfDisplaySet) and
     not (segment_type = stStuffing)
    then Exit;

  if (segment_type = stPageComposition) then
  begin
    inc(ABuffer, 3);
    if GetPageState(ABuffer) = psReserved
      then Exit;
  end;

  Result := True;
end;

(*** TDVBSubtitlingBaseSegment ************************************************)

constructor TDVBSubtitlingBaseSegment.Create;
begin
  inherited Create;
  Clear;
end;

constructor TDVBSubtitlingBaseSegment.Create(ABuffer: PByte);
begin
  Create;
  Clear;
  ParseBuffer(ABuffer);
end;

destructor TDVBSubtitlingBaseSegment.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDVBSubtitlingBaseSegment.ParseBuffer(ABuffer: PByte);
begin
  Clear;

  FSyncByte := ABuffer^;
  inc(ABuffer);

  FSegmentType := GetSegmentType(ABuffer);
  inc(ABuffer);

  FPageID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FSegmentLength := GetWord(ABuffer);
  FTotalLength := FSegmentLength + 6;
end;

procedure TDVBSubtitlingBaseSegment.Clear;
begin
  FSyncByte := 0;
  FSegmentType := stReserved;
  FPageID := 0;
  FSegmentLength := 0;
  FTotalLength := 6;
end;

(*** TDVBSubtitlingCompositionPageSegment *************************************)

procedure TDVBSubtitlingCompositionPageSegment.ParseBuffer(ABuffer: PByte);
var
  processed_length, i: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 6);

  FPageTimeOut := ABuffer^;
  inc(ABuffer);

  FPageVersionNumber := GetByteBits(ABuffer, 0, 4);
  FPageState := GetPageState(ABuffer);
  inc(ABuffer);

  processed_length := FSegmentLength - 2;
  SetLength(FRegions, processed_length div 6);

  for i := 0 to High(FRegions) do
  begin
    FRegions[i].RegionID := ABuffer^;
    inc(ABuffer, 2);

    FRegions[i].RegionHorizontalAddress := GetWord(ABuffer);
    inc(ABuffer, 2);

    FRegions[i].RegionVerticalAddress := GetWord(ABuffer);
    inc(ABuffer, 2);
  end;
end;

procedure TDVBSubtitlingCompositionPageSegment.Clear;
begin
  inherited Clear;
  FPageTimeOut := 0;
  FPageVersionNumber := 0;
  FPageState := psReserved;
  SetLength(FRegions, 0);
end;

function TDVBSubtitlingCompositionPageSegment.GetCountRegions: Integer;
begin
  Result := High(FRegions) + 1;
end;

function TDVBSubtitlingCompositionPageSegment.GetRegion(AIndex: Integer): PDVBSubtitlingRegion;
begin
  if (AIndex < 0) or (AIndex > High(FRegions))
    then Result := nil
    else Result := @FRegions[AIndex];
end;

(*** TDVBSubtitlingCompositionRegionSegment ***********************************)

procedure TDVBSubtitlingCompositionRegionSegment.ParseBuffer(ABuffer: PByte);
var
  processed_length: Integer;
  i: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 6);

  FRegionID := ABuffer^;
  inc(ABuffer);

  FRegionVersionNumber := GetByteBits(ABuffer, 0, 4);
  FRegionFillFlag := GetByteBits(ABuffer, 4, 1) = 1;
  inc(ABuffer);


  FRegionWidth := GetWord(ABuffer);
  inc(ABuffer, 2);

  FRegionHeight := GetWord(ABuffer);
  inc(ABuffer, 2);

  FRegionLevelOfCompatibility := GetRegionLevelOfCompatibility(ABuffer);
  FRegionDepth := GetRegionDepth(ABuffer);
  inc(ABuffer);

  FCLUTID := ABuffer^;
  inc(ABuffer);

  FRegion8BitPixelCode := ABuffer^;
  inc(ABuffer);

  FRegion4BitPixelCode := GetByteBits(ABuffer, 0, 4);
  FRegion2BitPixelCode := GetByteBits(ABuffer, 4, 2);
  inc(ABuffer);

  processed_length := FSegmentLength - 10;
  SetLength(FObjects, processed_length div 6);

  i := 0;
  while (processed_length > 0) do
  begin
    FObjects[i].ObjectID := GetWord(ABuffer);
    inc(ABuffer, 2);
    dec(processed_length, 2);

    FObjects[i].ObjectType := GetObjectType(ABuffer);
    FObjects[i].ObjectProviderFlag := GetObjectProviderFlag(ABuffer);
    FObjects[i].ObjectHorizontalPosition := GetWordBits(ABuffer, 4, 12);

    inc(ABuffer, 2);
    dec(processed_length, 2);

    FObjects[i].ObjectVerticalPosition := GetWordBits(ABuffer, 4, 12);
    inc(ABuffer, 2);
    dec(processed_length, 2);

    if (FObjects[i].ObjectType = otBasicObjectCharacter) or (FObjects[i].ObjectType = otCompositeObject) then
    begin
      FObjects[i].ForegroundPixelCode := ABuffer^;
      inc(ABuffer);
      dec(processed_length);

      FObjects[i].BackgroundPixelCode := ABuffer^;
      inc(ABuffer);
      dec(processed_length);
    end;

    inc(i);
  end;

  SetLength(FObjects, i);
end;

procedure TDVBSubtitlingCompositionRegionSegment.Clear;
begin
  inherited Clear;

  FRegionID := 0;
  FRegionVersionNumber := 0;
  FRegionFillFlag := False;
  FRegionWidth := 0;
  FRegionHeight := 0;
  FRegionLevelOfCompatibility := rlcReserved;
  FRegionDepth := rdReserved0;
  FCLUTID := 0;
  FRegion8BitPixelCode := 0;
  FRegion4BitPixelCode := 0;
  FRegion2BitPixelCode := 0;
  SetLength(FObjects, 0);
end;

function TDVBSubtitlingCompositionRegionSegment.GetCountObjects: Integer;
begin
  Result := High(FObjects) + 1;
end;

function TDVBSubtitlingCompositionRegionSegment.GetObject(AIndex: Integer): PDVBSubtitlingSegmentObject;
begin
  if (AIndex < 0) or (AIndex > High(FObjects))
    then Result := nil
    else Result := @FObjects[AIndex];
end;

(*** TDVBSubtitlingCLUTDefinitionSegment **************************************)

procedure TDVBSubtitlingCLUTDefinitionSegment.ParseBuffer(ABuffer: PByte);
var
  processed_length: Integer;
  i: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 6);

  FCLUTID := ABuffer^;
  inc(ABuffer);

  FCLUTVersionNumber := GetByteBits(ABuffer, 0, 4);
  inc(ABuffer);

  processed_length := FSegmentLength - 2;
  SetLength(FCLUTs, processed_length div 4);

  i := 0;
  while (processed_length > 0) do
  begin
    FCLUTs[i].CLUTEntryID := ABuffer^;
    inc(ABuffer);
    dec(processed_length);

    FCLUTs[i].N2BitEntryCLUTFlag := GetByteBits(ABuffer, 0, 1) = 1;
    FCLUTs[i].N4BitEntryCLUTFlag := GetByteBits(ABuffer, 1, 1) = 1;
    FCLUTs[i].N8BitEntryCLUTFlag := GetByteBits(ABuffer, 2, 1) = 1;
    FCLUTs[i].FullRangeFlag := GetByteBits(ABuffer, 7, 1) = 1;
    inc(ABuffer);
    dec(processed_length);

    if FCLUTs[i].FullRangeFlag then
    begin
      FCLUTs[i].YValue := ABuffer^;
      inc(ABuffer);
      dec(processed_length);

      if FCLUTs[i].YValue = 0 then
      begin
        FCLUTs[i].CrValue := 0;
        FCLUTs[i].CbValue := 0;
        FCLUTs[i].TValue := 255;
        inc(ABuffer, 3);
        dec(processed_length, 3);
      end else
      begin
        FCLUTs[i].CrValue := ABuffer^;
        inc(ABuffer);
        dec(processed_length);

        FCLUTs[i].CbValue := ABuffer^;
        inc(ABuffer);
        dec(processed_length);

        FCLUTs[i].TValue := ABuffer^;
        inc(ABuffer);
        dec(processed_length);
      end;
    end else
    begin
      FCLUTs[i].YValue := GetByteBits(ABuffer, 0, 6);
      if FCLUTs[i].YValue = 0 then
      begin
        FCLUTs[i].CrValue := 0;
        FCLUTs[i].CbValue := 0;
        FCLUTs[i].TValue := 255;
        inc(ABuffer, 2);
        dec(processed_length, 2);
      end else
      begin
        FCLUTs[i].CrValue := GetWordBits(ABuffer, 6, 4) shl 4;
        inc(ABuffer);
        dec(processed_length);

        FCLUTs[i].CbValue := GetByteBits(ABuffer, 2, 4) shl 4;
        FCLUTs[i].TValue := GetByteBits(ABuffer, 6, 2) * 85;

        inc(ABuffer);
        dec(processed_length);
      end;
    end;

    inc(i);
  end;

  SetLength(FCLUTs, i);
end;

procedure TDVBSubtitlingCLUTDefinitionSegment.Clear;
begin
  inherited Clear;

  FCLUTID := 0;
  FCLUTVersionNumber := 0;
  SetLength(FCLUTs, 0);
end;

procedure TDVBSubtitlingCLUTDefinitionSegment.CreateRGBTable;

  procedure FillCLUT(var ACLUT: array of Cardinal; ALUT: PDVBSubtitlingCLUT; AMax: Integer);
  begin
    if ALUT.CLUTEntryID < AMax then
    begin
      if ALUT.YValue = 0
        then ACLUT[ALUT.CLUTEntryID] := 0
        else ACLUT[ALUT.CLUTEntryID] := yuv2rgb(ALUT.YValue, ALUT.CbValue, ALUT.CrValue) or (Cardinal(255 - ALUT.TValue) shl 24);
    end;
  end;

var
  c: Integer;
  lut: PDVBSubtitlingCLUT;
begin
  if FRGBTableCreated
    then Exit;

  Move(LUT_DEFAULT_2[0], FCLUT2[0], 4 shl 2);
  Move(LUT_DEFAULT_4[0], FCLUT4[0], 16 shl 2);
  Move(LUT_DEFAULT_8[0], FCLUT8[0], 256 shl 2);

  c := GetCountCLUTs;

  while (c > 0) do
  begin
    dec(c);

    lut := CLUT[c];

    if lut.N2BitEntryCLUTFlag
      then FillCLUT(FCLUT2, lut, 4);

    if lut.N4BitEntryCLUTFlag
      then FillCLUT(FCLUT4, lut, 16);

    if lut.N8BitEntryCLUTFlag
      then FillCLUT(FCLUT8, lut, 256);
  end;

  FRGBTableCreated := True;
end;

function TDVBSubtitlingCLUTDefinitionSegment.GetCountCLUTs: Integer;
begin
  Result := High(FCLUTs) + 1;
end;

function TDVBSubtitlingCLUTDefinitionSegment.GetCLUT(AIndex: Integer): PDVBSubtitlingCLUT;
begin
  if (AIndex < 0) or (AIndex > High(FCLUTs))
    then Result := nil
    else Result := @FCLUTs[AIndex];
end;

function TDVBSubtitlingCLUTDefinitionSegment.GetColor(ADepth: TRegionDepth; AIndex: Integer): Cardinal;

  function GetLUT(const ACLUT: array of Cardinal; AIndex: Integer; AMax: Integer): Cardinal;
  begin
    if (AIndex < AMax)
      then Result := ACLUT[AIndex]
      else Result := 0;
  end;

begin
  Result := 0;
  if (AIndex < 0)
    then Exit;

  case ADepth of
    rd2Bit: Result := GetLUT(FCLUT2, AIndex, 4);
    rd4Bit: Result := GetLUT(FCLUT4, AIndex, 16);
    rd8Bit: Result := GetLUT(FCLUT8, AIndex, 256);
  end;
end;

(*** TDVBSubtitlingObjectDataSegment ******************************************)

procedure TDVBSubtitlingObjectDataSegment.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 6);

  FObjectID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FObjectVersionNumber := GetByteBits(ABuffer, 0, 4);
  FObjectCodingMethod := GetByteBits(ABuffer, 4, 2);
  FNonModifyingColourFlag := GetByteBits(ABuffer, 6, 1) = 1;
  inc(ABuffer);

  if FObjectCodingMethod = 0 then
  begin
    FTopFieldDataBlockLength := GetWord(ABuffer);
    inc(ABuffer, 2);

    FBottomFieldDataBlockLength := GetWord(ABuffer);
    inc(ABuffer, 2);

    if FTopFieldDataBlockLength > 0 then
    begin
      FTopFieldData := AllocMem(FTopFieldDataBlockLength);
      Move(ABuffer^, FTopFieldData^, FTopFieldDataBlockLength);
      inc(ABuffer, FTopFieldDataBlockLength)
    end;

    if FBottomFieldDataBlockLength > 0 then
    begin
      FBottomFieldData := AllocMem(FBottomFieldDataBlockLength);
      Move(ABuffer^, FBottomFieldData^, FBottomFieldDataBlockLength);
    end;
  end else
  if FObjectCodingMethod = 1 then
  begin
    FCharacterCodeLength := ABuffer^;
    FCharacterCodeLength := FCharacterCodeLength;
    inc(ABuffer);
    if FCharacterCodeLength > 0 then
    begin
      SetLength(FCharacterCode, FCharacterCodeLength);
      Move(ABuffer^, FCharacterCode[1], FCharacterCodeLength shl 1);
    end;
  end;
end;

procedure TDVBSubtitlingObjectDataSegment.Clear;
begin
  inherited Clear;

  FObjectID := 0;
  FObjectVersionNumber := 0;
  FObjectCodingMethod := 0;
  FNonModifyingColourFlag := False;
  FTopFieldDataBlockLength := 0;
  FBottomFieldDataBlockLength := 0;
  FCharacterCodeLength := 0;
  FCharacterCode := '';
  if Assigned(FBottomFieldData) then
  begin
    FreeMem(FBottomFieldData);
    FBottomFieldData := nil;
  end;
  if Assigned(FTopFieldData) then
  begin
    FreeMem(FTopFieldData);
    FTopFieldData := nil;
  end;
end;

(*** TDVBSubtitlingDisplaySet *************************************************)

constructor TDVBSubtitlingDisplaySet.Create;
begin
  inherited Create;
  FList := TList.Create;
  Clear;
end;

destructor TDVBSubtitlingDisplaySet.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDVBSubtitlingDisplaySet.Clear(AKeepSegments: Boolean = False);
var
  i: Integer;
begin
  FValid := False;
  FPTS := -1;
  if not AKeepSegments then
  begin
    for i := 0 to FList.Count -1
      do TDVBSubtitlingBaseSegment(FList[i]).Free;
  end;
  FList.Clear;
end;

function TDVBSubtitlingDisplaySet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBSubtitlingDisplaySet.GetSegment(AIndex: Integer): TDVBSubtitlingBaseSegment;
begin
  Result := FList[AIndex];
end;

function TDVBSubtitlingDisplaySet.GetCountRegions: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stRegionComposition)
      then inc(Result);
  end;
end;

function TDVBSubtitlingDisplaySet.GetRegion(AIndex: Integer): TDVBSubtitlingCompositionRegionSegment;
var
  c, i: Integer;
begin
  Result := nil;
  c := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stRegionComposition) then
    begin
      if (AIndex = c) then
      begin
        Result:= FList[i];
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBSubtitlingDisplaySet.GetCountObjects: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stObjectData)
      then inc(Result);
  end;
end;

function TDVBSubtitlingDisplaySet.GetObject(AIndex: Integer): TDVBSubtitlingObjectDataSegment;
var
  c, i: Integer;
begin
  Result := nil;
  c := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stObjectData) then
    begin
      if (AIndex = c) then
      begin
        Result:= FList[i];
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBSubtitlingDisplaySet.GetCountCLUTs: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stCLUTDefinition)
      then inc(Result);
  end;
end;

function TDVBSubtitlingDisplaySet.GetCLUT(AIndex: Integer): TDVBSubtitlingCLUTDefinitionSegment;
var
  c, i: Integer;
begin
  Result := nil;
  c := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stCLUTDefinition) then
    begin
      if (AIndex = c) then
      begin
        Result:= FList[i];
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBSubtitlingDisplaySet.GetCountCompositionPages: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stPageComposition)
      then inc(Result);
  end;
end;

function TDVBSubtitlingDisplaySet.GetCompositionPage2(AIndex: Integer): TDVBSubtitlingCompositionPageSegment;
var
  c, i: Integer;
begin
  Result := nil;
  c := 0;
  for i := 0 to FList.Count -1 do
  begin
    if (TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stPageComposition) then
    begin
      if (AIndex = c) then
      begin
        Result:= FList[i];
        Exit;
      end;
      inc(c);
    end;
  end;
end;

procedure TDVBSubtitlingDisplaySet.AddSegment(ASegment: TDVBSubtitlingBaseSegment);
begin
  if not FValid
    then FValid := ASegment.SegmentType = stEndOfDisplaySet;
  FList.Add(ASegment);
end;

procedure TDVBSubtitlingDisplaySet.DeleteRegion(ARegionID: Integer);
var
  c: Integer;
  region: TDVBSubtitlingCompositionRegionSegment;
begin
  c := GetCountRegions;
  while (c > 0) do
  begin
    dec(c);
    region := GetRegion(c);
    if (region.RegionID = ARegionID) then
    begin
      FList.Remove(region);
      region.Free;
    end;
  end;
end;

procedure TDVBSubtitlingDisplaySet.DeleteCLUT(ACLUTID: Integer);
var
  c: Integer;
  clut: TDVBSubtitlingCLUTDefinitionSegment;
begin
  c := GetCountCLUTs;
  while (c > 0) do
  begin
    dec(c);
    clut := GetCLUT(c);
    if (clut.CLUTID = ACLUTID) then
    begin
      FList.Remove(clut);
      clut.Free;
    end;
  end;
end;

procedure TDVBSubtitlingDisplaySet.DeleteObject(AObjectID: Integer);
var
  c: Integer;
  obj: TDVBSubtitlingObjectDataSegment;
begin
  c := GetCountObjects;
  while (c > 0) do
  begin
    dec(c);
    obj := GetObject(c);
    if (obj.ObjectID = AObjectID) then
    begin
      FList.Remove(obj);
      obj.Free;
    end;
  end;
end;

procedure TDVBSubtitlingDisplaySet.DeletePageComposition;
var
  c: Integer;
  segment: TDVBSubtitlingBaseSegment;
begin
  c := GetCount;
  while (c > 0) do
  begin
    dec(c);
    segment := GetSegment(c);
    if (segment.SegmentType = stPageComposition) then
    begin
      FList.Remove(segment);
      segment.Free;
    end;
  end;
end;

procedure TDVBSubtitlingDisplaySet.AddAllDisplaySet(ADisplaySet: TDVBSubtitlingDisplaySet);
var
  i: Integer;
begin
  for i := 0 to ADisplaySet.Count -1
    do AddSegment(ADisplaySet.Segment[i]);
end;

procedure TDVBSubtitlingDisplaySet.AddDisplaySet(ADisplaySet: TDVBSubtitlingDisplaySet);
var
  i: Integer;
  segment: TDVBSubtitlingBaseSegment;
begin
  // TODO check Versions and Update if needed
  for i := 0 to ADisplaySet.Count -1 do
  begin
    segment := ADisplaySet.Segment[i];

    case segment.SegmentType of
      stObjectData,
      stRegionComposition: AddSegment(segment);
      stCLUTDefinition:
      begin
        DeleteCLUT(TDVBSubtitlingCLUTDefinitionSegment(segment).CLUTID);
        AddSegment(segment);
      end;
      stPageComposition:
      begin
        DeletePageComposition;
        AddSegment(segment);
      end;
      else
      begin
        segment.Free;
      end;
    end;
  end;
end;

function TDVBSubtitlingDisplaySet.GetCompositionPage: TDVBSubtitlingCompositionPageSegment;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    if TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stPageComposition then
    begin
      Result := FList[i];
      Exit;
    end;
  end;

  Result := nil;
end;

function TDVBSubtitlingDisplaySet.GetCLUTForCLUTID(ACLUTID: Integer): TDVBSubtitlingCLUTDefinitionSegment;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count -1 do
  begin
    if TDVBSubtitlingBaseSegment(FList[i]).SegmentType = stCLUTDefinition then
    begin
      if (TDVBSubtitlingCLUTDefinitionSegment(FList[i]).CLUTID = ACLUTID) then
      begin
        Result := FList[i];
        Exit;
      end;
    end;
  end;
end;

(*** TDVBSubtitlingDisplaySetList *********************************************)

constructor TDVBSubtitlingDisplaySetList.Create;
begin
  inherited Create;
  FList := TList.Create;
  Clear;
end;

destructor TDVBSubtitlingDisplaySetList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDVBSubtitlingDisplaySetList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDVBSubtitlingDisplaySet(FList[i]).Free;
  FList.Clear;
end;

function TDVBSubtitlingDisplaySetList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBSubtitlingDisplaySetList.GetDisplaySet(AIndex: Integer): TDVBSubtitlingDisplaySet;
begin
  Result := FList[AIndex];
end;

procedure TDVBSubtitlingDisplaySetList.Add(ADisplaySet: TDVBSubtitlingDisplaySet);
var
  i: Integer;
  display_set: TDVBSubtitlingDisplaySet;
begin
  for i := 0 to FList.Count -1 do
  begin
    display_set := FList[i];
    if display_set.PTS > ADisplaySet.PTS then
    begin
      FList.Insert(i, ADisplaySet);
      Exit;
    end;
  end;

  FList.Add(ADisplaySet);
end;

procedure TDVBSubtitlingDisplaySetList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TDVBSubtitlingDisplaySetList.GetDisplaySetForPTS(APTS: Int64): TDVBSubtitlingDisplaySet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count -1 do
  begin
    if TDVBSubtitlingDisplaySet(FList[i]).FPTS = APTS then
    begin
      Result := FList[i];
      Exit;
    end;
  end;
end;

(*** TDVBSubtitlingParser *****************************************************)

constructor TDVBSubtitlingParser.Create;
var
  bitmap_info: TBitmapInfo;
  log_font: TLogFont;
begin
  inherited Create;
  FBitReader := TBitReader.Create;

  FFont := TFont.Create;
  FFont.Size := 24;
  FFont.Name := 'Tahoma';
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  GetObject(FFont.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  FFont.Handle := CreateFontIndirect(log_font);

  FCurrentSet := TDVBSubtitlingDisplaySet.Create;
  FSetList := TDVBSubtitlingDisplaySetList.Create;

  FPCRTSParser := TTSParser.Create;
  FSubTSParser := TTSParser.Create;
  FSubtitleParser := TSubtitleParser.Create(FSubTSParser, -1, OnSubtitleData, True);
  FPCR := TPESParser.Create(FPCRTSParser, -1, nil, True);
  FPCR.OnPCRCallback := OnPCR;
  FPCR.ParseData := False;

  FCompositionPageID := -1;
  FAncillaryPageID := -1;

  FWidth := DEFAULT_WIDTH;
  FHeight := DEFAULT_HEIGHT;
  FBufferSize := FWidth * FHeight * 4;

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

  FBitmap := CreateDIBSection(0, bitmap_info, DIB_RGB_COLORS, Pointer(FBuffer), 0, 0);
  FDevice := CreateCompatibleDC(0);
  SelectObject(FDevice, FBitmap);

  ErasePage;
end;

destructor TDVBSubtitlingParser.Destroy;
begin
  DeleteDC(FDevice);
  DeleteObject(FBitmap);

  FreeAndNil(FFont);
  FreeAndNil(FSubtitleParser);
  FreeAndNil(FPCR);
  FreeAndNil(FPCRTSParser);
  FreeAndNil(FSubTSParser);

  FreeAndNil(FCurrentSet);
  FreeAndNil(FSetList);
  FBitReader.Free;
  inherited Destroy;
end;

procedure TDVBSubtitlingParser.ErasePage;
begin
  FillChar(FBuffer[0], FBufferSize, $00);
end;

procedure TDVBSubtitlingParser.ParsePCRBuffer(ABuffer: PByte; ASize: Integer);
begin
  if (ABuffer = nil) or (ASize <= 0) or not FEnabled or (FSubtitleParser.PID = -1)
    then Exit;

  FPCRTSParser.ParseBuffer(ABuffer, ASize);
end;

procedure TDVBSubtitlingParser.ParseSubtitleBuffer(ABuffer: PByte; ASize: Integer);
begin
  if (ABuffer = nil) or (ASize <= 0) or not FEnabled
    then Exit;

  FSubTSParser.ParseBuffer(ABuffer, ASize);
end;

procedure TDVBSubtitlingParser.OnPCR(APCR: Int64);
var
  display_set: TDVBSubtitlingDisplaySet;
begin
  if (APCR <= 0) or (FSetList.Count = 0)
    then Exit;

  display_set := FSetList[0];

  if (display_set.PTS > APCR)
    then Exit;

  DrawSet(display_set);
end;

procedure TDVBSubtitlingParser.OnSubtitleData(ABuffer: PByte; ASize: Integer);
const
  END_OF_PES_MARKER           = $FF;
  SUBTITLE_DATA_IDENTIFIER    = $20;
  PES_SUBTITLE_STREAM_ID      = $00;
var
  data_identifier: Byte;
  subtitle_stream_id: Byte;
  segment: TDVBSubtitlingBaseSegment;
  total_size: Integer;
  display_set: TDVBSubtitlingDisplaySet;
  own_set: Boolean;

  procedure DeleteSet;
  begin
    if own_set then
    begin
      display_set.Free;
      display_set := nil;
    end;
  end;

begin
  if FSubtitleParser.PTS <= 0
    then Exit;

  data_identifier := ABuffer^;
  if (data_identifier <> SUBTITLE_DATA_IDENTIFIER) then
  begin
    Exit;
  end;
  inc(ABuffer);

  subtitle_stream_id := ABuffer^;
  if (subtitle_stream_id <> PES_SUBTITLE_STREAM_ID) then
  begin
    Exit;
  end;
  inc(ABuffer);

  if (ABuffer^ = END_OF_PES_MARKER) or (ABuffer^ <> SEGMENT_SYNC_BYTE) then
  begin
    Exit;
  end;

  display_set := FSetList.GetDisplaySetForPTS(FSubtitleParser.PTS);
  own_set := display_set = nil;
  if not Assigned(display_set) then
  begin
    display_set := TDVBSubtitlingDisplaySet.Create;
    display_set.PTS := FSubtitleParser.PTS;
  end;

  while ((ABuffer^ <> END_OF_PES_MARKER) and (ABuffer^ = SEGMENT_SYNC_BYTE)) do
  begin
    if not IsValidSubtitlePage(ABuffer, FCompositionPageID, FAncillaryPageID, total_size) then
    begin
      if (total_size = 0) then
      begin
        DeleteSet;
        Exit;
      end;

      inc(ABuffer, total_size);
      Continue;
    end;

    segment := CreateSubtitleSegment(ABuffer);
    if not Assigned(segment) then
    begin
      DeleteSet;
      Exit;
    end;
    display_set.AddSegment(segment);
    inc(ABuffer, segment.TotalLength);
  end;

  if display_set.Count > 0 then
  begin
    while FSetList.Count >= MAXIMUM_PAGE_SETS do
    begin
      FSetList.DisplaySet[0].Free;
      FSetList.Delete(0);
    end;
    if own_set
	    then FSetList.Add(display_set);
    Exit;
  end;

  DeleteSet;
end;

procedure TDVBSubtitlingParser.DrawSet(ADisplaySet: TDVBSubtitlingDisplaySet);
var
  comp_page: TDVBSubtitlingCompositionPageSegment;
begin
  comp_page := ADisplaySet.GetCompositionPage;
  if not Assigned(comp_page) or not ADisplaySet.Valid then
  begin
    FSetList.Delete(0);
    ADisplaySet.Free;
    Exit;
  end;

  case comp_page.PageState of
    psNormalCase:
    begin
      if (FCurrentSet.Count = 0) then
      begin
        FSetList.Delete(0);
        ADisplaySet.Free;
        Exit;
      end;
      FCurrentSet.AddDisplaySet(ADisplaySet);
      ADisplaySet.Clear(True);
      FSetList.Delete(0);
      ADisplaySet.Free;
      ErasePage;
    end;
    psAcquisitionPoint,
    psModeChange:
    begin
      FCurrentSet.Clear;
      FCurrentSet.PTS := ADisplaySet.PTS;
      FCurrentSet.AddAllDisplaySet(ADisplaySet);
      ADisplaySet.Clear(True);
      FSetList.Delete(0);
      ADisplaySet.Free;
      ErasePage;
    end;
  end;

  comp_page := FCurrentSet.GetCompositionPage;
  if not Assigned(comp_page)
    then Exit;

  FTimeOut := Cardinal(comp_page.PageTimeOut) * 1000;

  DrawPage(comp_page);
end;

procedure TDVBSubtitlingParser.DrawPage(APage: TDVBSubtitlingCompositionPageSegment);
var
  i, c: Integer;
  region: PDVBSubtitlingRegion;
  region_segment: TDVBSubtitlingCompositionRegionSegment;
begin
  for i := 0 to APage.CountRegions -1 do
  begin
    region := APage.Region[i];
    for c := 0 to FCurrentSet.CountRegions -1 do
    begin
      region_segment := FCurrentSet.Region[c];
      if (region_segment.RegionID = region.RegionID) then
      begin
        DrawRegion(APage, region, region_segment);
      end;
    end;
  end;
end;

procedure TDVBSubtitlingParser.DrawRegion(APage: TDVBSubtitlingCompositionPageSegment;
  ARegion: PDVBSubtitlingRegion; ARegionSegment: TDVBSubtitlingCompositionRegionSegment);
var
  i, c: Integer;
  clut_segment: TDVBSubtitlingCLUTDefinitionSegment;
  obj: PDVBSubtitlingSegmentObject;
  obj_segment: TDVBSubtitlingObjectDataSegment;
begin
  clut_segment := FCurrentSet.GetCLUTForCLUTID(ARegionSegment.CLUTID);
  if not Assigned(clut_segment)
    then clut_segment := DefaultCLUT
    else clut_segment.CreateRGBTable;

  for i := 0 to ARegionSegment.CountObjects -1 do
  begin
    obj := ARegionSegment.Objects[i];
    for c := 0 to FCurrentSet.CountObjects -1 do
    begin
      obj_segment := FCurrentSet.Object_[c];
      if (obj_segment.ObjectID = obj.ObjectID) and (obj.ObjectProviderFlag = opfSubtitleStream) then
      begin
        case obj_segment.ObjectCodingMethod of
          0: DrawBitmap(ARegion, ARegionSegment, clut_segment, obj, obj_segment);
          1: DrawString(ARegion, ARegionSegment, clut_segment, obj, obj_segment);
        end;
      end;
    end;
  end;
end;

procedure TDVBSubtitlingParser.DrawBitmap(ARegion: PDVBSubtitlingRegion;
  ARegionSegment: TDVBSubtitlingCompositionRegionSegment;
  ACLUT: TDVBSubtitlingCLUTDefinitionSegment;
  AObject: PDVBSubtitlingSegmentObject;
  AObjectSegment: TDVBSubtitlingObjectDataSegment);
var
  x1, x2, y1, y2: Integer;
  buffer: PByte;
  data_type: Byte;
  size: Integer;
  next_bits: Byte;
  switch_1: Byte;
  switch_2: Byte;
  run_length_3_9: Integer;
  run_length_4_7: Integer;
  i, c: Integer;
  switch_3: Byte;
  run_length_25_280: Integer;
  run_length_9_24: Integer;
  value: Byte;
  run_length_1_127: Integer;
  run_length_3_127: Integer;
  run_length_3_10: Integer;
  run_length_12_27: Integer;
  run_length_29_284: Integer;
  color: Cardinal;
  clut: TDVBSubtitlingCLUT;
begin
  x1 := ARegion.RegionHorizontalAddress + AObject.ObjectHorizontalPosition;
  y1 := ARegion.RegionVerticalAddress + AObject.ObjectVerticalPosition;

  if (ARegionSegment.RegionFillFlag) then
  begin
    case ARegionSegment.RegionDepth of
      rd2Bit: value := ARegionSegment.Region2BitPixelCode;
      rd4Bit: value := ARegionSegment.Region4BitPixelCode;
      rd8Bit: value := ARegionSegment.Region8BitPixelCode;
      else    value := 0;
    end;

    FillRect(x1, y1, ARegionSegment.RegionWidth, ARegionSegment.RegionHeight,
             ACLUT.GetColor(ARegionSegment.RegionDepth, value));
  end;

  for c := 0 to 1 do
  begin
    if c = 0 then
    begin
      buffer := AObjectSegment.TopFieldData;
      size := AObjectSegment.TopFieldDataBlockLength;
      y2 := y1;
    end else
    begin
      y2 := y1 + 1;
      if AObjectSegment.BottomFieldDataBlockLength = 0 then
      begin
        buffer := AObjectSegment.TopFieldData;
        size := AObjectSegment.TopFieldDataBlockLength;
      end else
      begin
        buffer := AObjectSegment.BottomFieldData;
        size := AObjectSegment.BottomFieldDataBlockLength;
      end;
    end;

    if not Assigned(buffer)
      then Continue;

    while (size > 0) do
    begin
      data_type := buffer^;

      inc(buffer);
      dec(size);

      FBitReader.SetBuffer(buffer, size);

      x2 := x1;

      case data_type of
        $10:
        begin
          repeat
            next_bits := FBitReader.Read(2);
            if (next_bits <> 0) then
            begin
              SetPixel(x2, y2, ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits));
              inc(x2);
            end else
            begin
              switch_1 := FBitReader.Read(1);
              if (switch_1 = 1) then
              begin
                run_length_3_10 := FBitReader.Read(3);
                inc(run_length_3_10, 3);
                value := FBitReader.Read(2);
                color := ACLUT.GetColor(ARegionSegment.RegionDepth, value);
                for i := 0 to run_length_3_10 -1 do
                begin
                  SetPixel(x2, y2, color);
                  inc(x2);
                end;
              end else
              begin
                switch_2 := FBitReader.Read(1);
                if (switch_2 = 0) then
                begin
                  switch_3 := FBitReader.Read(2);
                  case switch_3 of
                    0:
                    begin
                      break;
                    end;
                    1:
                    begin
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, 0);
                      SetPixel(x2, y2, color);
                      inc(x2);
                      SetPixel(x2, y2, color);
                      inc(x2);
                    end;
                    2:
                    begin
                      run_length_12_27 := FBitReader.Read(4);
                      inc(run_length_12_27, 12);
                      value := FBitReader.Read(2);
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, value);
                      for i := 0 to run_length_12_27 -1 do
                      begin
                        SetPixel(x2, y2, color);
                        inc(x2);
                      end;
                    end;
                    3:
                    begin
                      run_length_29_284 := FBitReader.Read(8);
                      inc(run_length_29_284, 29);
                      value := FBitReader.Read(2);
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, value);
                      for i := 0 to run_length_29_284 -1 do
                      begin
                        SetPixel(x2, y2, color);
                        inc(x2);
                      end;
                    end;
                  end;
                end else
                begin
                  SetPixel(x2, y2, ACLUT.GetColor(ARegionSegment.RegionDepth, 0));
                  inc(x2);
                end;
              end;
            end;
          until False;
          FBitReader.Align8;
          inc(buffer, FBitReader.BufferPos);
          dec(size, FBitReader.BufferPos);
        end;
        $11: // 4-bit/pixel code string
        begin
          repeat
            next_bits := FBitReader.Read(4);
            if (next_bits <> 0) then
            begin
              SetPixel(x2, y2, ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits));
              inc(x2);
            end else
            begin
              switch_1 := FBitReader.Read(1);
              if (switch_1 = 0) then
              begin
                next_bits := FBitReader.Read(3);
                if next_bits <> 0 then
                begin
                  run_length_3_9 := next_bits;
                  inc(run_length_3_9, 2);
                  color := ACLUT.GetColor(ARegionSegment.RegionDepth, 0);
                  for i := 0 to run_length_3_9 -1 do
                  begin
                    SetPixel(x2, y2, color);
                    inc(x2);
                  end;
                end else
                begin
                  break;
                end;
              end else
              begin
                switch_2 := FBitReader.Read(1);
                if (switch_2 = 0) then
                begin
                  run_length_4_7 := FBitReader.Read(2);
                  inc(run_length_4_7, 4);
                  next_bits := FBitReader.Read(4);
                  color := ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits);
                  for i := 0 to run_length_4_7 -1 do
                  begin
                    SetPixel(x2, y2, color);
                    inc(x2);
                  end;
                end else
                begin
                  switch_3 := FBitReader.Read(2);
                  case switch_3 of
                    0:
                    begin
                      SetPixel(x2, y2, ACLUT.GetColor(ARegionSegment.RegionDepth, 0));
                      inc(x2);
                    end;
                    1:
                    begin
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, 0);
                      SetPixel(x2, y2, color);
                      inc(x2);
                      SetPixel(x2, y2, color);
                      inc(x2);
                    end;
                    2:
                    begin
                      run_length_9_24 := FBitReader.Read(4);
                      inc(run_length_9_24, 9);
                      next_bits := FBitReader.Read(4);
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits);
                      for i := 0 to run_length_9_24 -1 do
                      begin
                        SetPixel(x2, y2, color);
                        inc(x2);
                      end;
                    end;
                    3:
                    begin
                      run_length_25_280 := FBitReader.Read(8);
                      inc(run_length_25_280, 25);
                      next_bits := FBitReader.Read(4);
                      color := ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits);
                      for i := 0 to run_length_25_280 -1 do
                      begin
                        SetPixel(x2, y2, color);
                        inc(x2);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          until False;
          FBitReader.Align8;
          inc(buffer, FBitReader.BufferPos);
          dec(size, FBitReader.BufferPos);
        end;
        $12: // 8-bit/pixel code string
        begin
          repeat
            next_bits := FBitReader.Read(8);
            if (next_bits <> 0) then
            begin
              SetPixel(x2, y2, ACLUT.GetColor(ARegionSegment.RegionDepth, next_bits));
              inc(x2);
            end else
            begin
              switch_1 := FBitReader.Read(1);
              if (switch_1 = 0) then
              begin
                next_bits := FBitReader.Read(7);
                if (next_bits <> 0) then
                begin
                  run_length_1_127 := next_bits;
                  color := ACLUT.GetColor(ARegionSegment.RegionDepth, 0);
                  for i := 0 to run_length_1_127 -1 do
                  begin
                    SetPixel(x2, y2, color);
                    inc(x2);
                  end;
                end else
                begin
                  break;
                end;
              end else
              begin
                run_length_3_127 := FBitReader.Read(7);
                value := FBitReader.Read(8);;
                if run_length_3_127 < 3
                  then run_length_3_127 := 3;
                color := ACLUT.GetColor(ARegionSegment.RegionDepth, value);
                for i := 0 to run_length_3_127 -1 do
                begin
                  SetPixel(x2, y2, color);
                  inc(x2);
                end;
              end;
            end;
          until False;
          inc(buffer, FBitReader.BufferPos);
          dec(size, FBitReader.BufferPos);
        end;
        $20:
        begin
          inc(buffer, 2);
          dec(size, 2);
        end;
        $21:
        begin
          inc(buffer, 4);
          dec(size, 4);
        end;
        $22:
        begin
          inc(buffer, 16);
          dec(size, 16);
        end;
        $F0:
        begin
          inc(y2, 2);
        end;
        else
        begin
          inc(buffer);
          dec(size);
        end;
      end;
    end;
  end;
end;

procedure TDVBSubtitlingParser.DrawString(ARegion: PDVBSubtitlingRegion;
  ARegionSegment: TDVBSubtitlingCompositionRegionSegment;
  ACLUT: TDVBSubtitlingCLUTDefinitionSegment;
  AObject: PDVBSubtitlingSegmentObject;
  AObjectSegment: TDVBSubtitlingObjectDataSegment);
var
  fx, fy: Integer;
  value: Byte;
begin
  fx := ARegion.RegionHorizontalAddress + AObject.ObjectHorizontalPosition;
  fy := ARegion.RegionVerticalAddress + AObject.ObjectVerticalPosition;

  if (ARegionSegment.RegionFillFlag) then
  begin
    case ARegionSegment.RegionDepth of
      rd2Bit: value := ARegionSegment.Region2BitPixelCode;
      rd4Bit: value := ARegionSegment.Region4BitPixelCode;
      rd8Bit: value := ARegionSegment.Region8BitPixelCode;
      else    value := 0;
    end;

    FillRect(fx, fy, ARegionSegment.RegionWidth, ARegionSegment.RegionHeight,
             ACLUT.GetColor(ARegionSegment.RegionDepth, value));
  end;

  if (AObjectSegment.CharacterCodeLength > 0) then
  begin
    TextOut(fx, fy, AObjectSegment.CharacterCode,
            ACLUT.GetColor(ARegionSegment.RegionDepth, AObject.ForegroundPixelCode),
			      ACLUT.GetColor(ARegionSegment.RegionDepth, AObject.BackgroundPixelCode));
  end;
end;

procedure TDVBSubtitlingParser.FillRect(AX, AY, AWidth, AHeight: Integer; AColor: Cardinal);
var
  j: Integer;
  i: Integer;
  p: PCardinal;
begin
  if (AX >= FWidth) or (AX < 0) or (AY >= FHeight) or (AY < 0) or
     (AWidth < 0) or (AHeight < 0)
    then Exit;

  AWidth := EnsureRange(AWidth, 1, FWidth - AX);
  AHeight := EnsureRange(AHeight, 1, FHeight - AY);

  for j := AY to AY + AHeight - 1 do
  begin
    i := AWidth;
    p := @FBuffer[j * FWidth + AX];
    while (i > 0) do
    begin
      p^ := AColor;
      inc(p);
      dec(i);
    end;
  end;
end;

procedure TDVBSubtitlingParser.TextOut(AX, AY: Integer; const AText: WideString; AFGColor: Cardinal; ABGColor: Cardinal);
var
  i, j: Integer;
  p: PCardinal;
  size: TSize;
begin
  GetTextExtentPoint32W(FDevice, PWideChar(AText), Length(AText), size);
  FillRect(AX, AY, size.cx, size.cy, ABGColor);

  SelectObject(FDevice, FFont.Handle);
  SetBkMode(FDevice, OPAQUE);
  SetTextColor(FDevice, AFGColor and $00FFFFFF);
  SetBkColor(FDevice, ABGColor and $00FFFFFF);
  ExtTextOutW(FDevice, AX, AY, ETO_OPAQUE, nil, PWideChar(AText), Length(AText), nil);

  size.cx := EnsureRange(size.cx, 1, FWidth - AX);
  size.cy := EnsureRange(size.cy, 1, FHeight - AY);

  for j := AY to AY + size.cy - 1 do
  begin
    i := size.cx;
    p := @FBuffer[j * FWidth + AX];
    while (i > 0) do
    begin
      p^ := p^ or $FF000000;
      inc(p);
      dec(i);
    end;
  end;
end;

procedure TDVBSubtitlingParser.SetPixel(AX, AY: Integer; AColor: Cardinal);
begin
  if (AX >= FWidth) or (AX < 0) or (AY >= FHeight) or (AY < 0)
    then Exit;

  FBuffer[AY * FWidth + AX] := AColor;
end;

procedure TDVBSubtitlingParser.Flush;
begin
  FTimeOut := -1;
  FPCRTSParser.Flush;
  FSubTSParser.Flush;
  FSubtitleParser.Flush;
  FPCR.Flush;
  FCurrentSet.Clear;
  FSetList.Clear;
  ErasePage;
end;

procedure TDVBSubtitlingParser.SetPID(APCRPID: Integer; APID: Integer;
  ACompositionPageID: Integer;
  AAncillaryPageID: Integer);
begin
  if (FPCR.PID = APCRPID) and (FSubtitleParser.PID = APID) and
     (FCompositionPageID = ACompositionPageID) and
     (FAncillaryPageID = AAncillaryPageID)
    then Exit;
    
  FPCR.PID := APCRPID;
  FSubtitleParser.PID := APID;
  FCompositionPageID := ACompositionPageID;
  FAncillaryPageID := AAncillaryPageID;

  Flush;
end;

procedure TDVBSubtitlingParser.SetEnabled(AEnabled: Boolean);
begin
  if (FEnabled = AEnabled)
    then Exit;

  FEnabled := AEnabled;
  Flush;
end;

function TDVBSubtitlingParser.GetPID: Integer;
begin
  Result := FSubtitleParser.PID;
end;

initialization
  DefaultCLUT := TDVBSubtitlingCLUTDefinitionSegment.Create;
  DefaultCLUT.CreateRGBTable;

finalization
  DefaultCLUT.Free;

end.
