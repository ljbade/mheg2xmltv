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

unit DSMCCSections;

{$I Compiler.inc}

interface

uses
  Classes, SysUtils, MPEGConst, MPEGDescriptors, MPEGSections, Windows,
  DSMCCConst, MHPDescriptors, MHPUtils, MHPConst, DVBDescriptors, DSMCCBIOP;

type
  TDSMCCBaseHeader = class
  protected
    FType: TDSMCCHeaderType;
    FProtocolDiscriminator: Byte;
    FDSMCCType: Byte;
    FMessageID: Word;
    FUnknownID: Cardinal;
    FAdaptationLength: Byte;
    FMessageLength: Word;

    FAdaptionType: Byte;
    FAdaptionDataLength: Byte;
    FAdaptionData: PByte;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte); overload; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    function ParseBuffer(ABuffer: PByte): Boolean; virtual;

    property ProtocolDiscriminator: Byte read FProtocolDiscriminator;
    property DSMCCType: Byte read FDSMCCType;
    property MessageID: Word read FMessageID;
    property AdaptationLength: Byte read FAdaptationLength;
    property MessageLength: Word read FMessageLength;
    property AdaptionType: Byte read FAdaptionType;
    property AdaptionDataLength: Byte read FAdaptionDataLength;
    property AdaptionData: PByte read FAdaptionData;

    property Type_: TDSMCCHeaderType read FType;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCDownloadDataHeader = class(TDSMCCBaseHeader)
  public
    property DownloadID: Cardinal read FUnknownID;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCMessageHeader = class(TDSMCCBaseHeader)
  public
    property TransactionID: Cardinal read FUnknownID;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCDownloadServerInitiate = class(TDSMCCMessageHeader) // tr_101202v010601p.pdf - Page 52
  private
    FServerID: TDSMCCServerID;
    FPrivateDataLength: Word;
    FPrivateData: PByte;
  public
    constructor Create; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property ServerID: TDSMCCServerID read FServerID;
    property PrivateDataLength: Word read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCDownloadInfoIndicationModule = class
  private
    FModuleId: Word;
    FModuleSize: Cardinal;
    FModuleVersion: Byte;
    FModuleInfoLength: Byte;
    FBIOPModuleInfo: TBIOPModuleInfo;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property ModuleId: Word read FModuleId;
    property ModuleSize: Cardinal read FModuleSize;
    property ModuleVersion: Byte read FModuleVersion;
    property ModuleInfoLength: Byte read FModuleInfoLength;
    property BIOPModuleInfo: TBIOPModuleInfo read FBIOPModuleInfo;
  end;

  TDSMCCDownloadInfoIndicationModuleList = class(TList)
  private
    function Get(Index: Integer): TDSMCCDownloadInfoIndicationModule;
    procedure Put(Index: Integer; Item: TDSMCCDownloadInfoIndicationModule);
  public
    function Add(Item: TDSMCCDownloadInfoIndicationModule): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TDSMCCDownloadInfoIndicationModule read Get write Put; default;
  end;

  TDSMCCDownloadInfoIndication = class(TDSMCCMessageHeader)
  private
    FDownloadID: Cardinal;
    FBlockSize: Word;
    FWindowSize: Byte;
    FACKPPeriod: Byte;
    FTCDownloadWindow: Cardinal;
    FTCDownloadScenario: Cardinal;
    FModuleList: TDSMCCDownloadInfoIndicationModuleList;
    FPrivateDataLength: Word;
    FPrivateData: PByte;
  public
    constructor Create; override;
    constructor Create(ABuffer: PByte); override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property DownloadID: Cardinal read FDownloadID;
    property BlockSize: Word read FBlockSize;
    property WindowSize: Byte read FWindowSize;
    property ACKPPeriod: Byte read FACKPPeriod;
    property TCDownloadWindow: Cardinal read FTCDownloadWindow;
    property TCDownloadScenario: Cardinal read FTCDownloadScenario;
    property ModuleList: TDSMCCDownloadInfoIndicationModuleList read FModuleList;
    property PrivateDataLength: Word read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCDownloadDataBlock = class(TDSMCCDownloadDataHeader)
  private
    FModuleID: Word;
    FModuleVersion: Byte;
    FBlockNumber: Word;
    FBlockDataLength: Integer;
    FBlockData: PByte;
  public
    constructor Create; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property ModuleID: Word read FModuleID;
    property ModuleVersion: Byte read FModuleVersion;
    property BlockNumber: Word read FBlockNumber;
    property BlockDataLength: Integer read FBlockDataLength;
    property BlockData: PByte read FBlockData;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCDownloadCancel = class(TDSMCCMessageHeader)
  private
    FDownloadID: Cardinal;
    FModuleID: Word;
    FBlockNumber: Word;
    FDownloadCancelReason: Byte;
    FPrivateDataLength: Word;
    FPrivateData: PByte;
  public
    constructor Create; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property DownloadID: Cardinal read FDownloadID;
    property ModuleID: Word read FModuleID;
    property BlockNumber: Word read FBlockNumber;
    property DownloadCancelReason: Byte read FDownloadCancelReason;
    property PrivateDataLength: Word read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCUserNetworkMessage = class(TDSMCCDownloadDataHeader)
    class function CreateDownload(ABuffer: PByte): TDSMCCBaseHeader;
  end;

  // ---------------------------------------------------------------------------

  TDSMCCSection = class(TBaseSectionSyntax)
  protected
    FTableIDExtension: Word;
    FDSMCCHeader: TDSMCCBaseHeader;
  public
    constructor Create; override;
    constructor Create(ABuffer: PByte; ASize: Integer); override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property PrivateIndicator: Byte read FReserved1;
    property TableIDExtension: Word read FTableIDExtension;
    property DSMCCHeader: TDSMCCBaseHeader read FDSMCCHeader;
  end;

implementation

uses
  MPEGUtils;

(*** TDSMCCBaseHeader *********************************************************)

constructor TDSMCCBaseHeader.Create;
begin
  inherited Create;
  FType := htBase;
  Clear;
end;

constructor TDSMCCBaseHeader.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TDSMCCBaseHeader.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDSMCCBaseHeader.Clear;
begin
  FProtocolDiscriminator := 0;
  FDSMCCType := 0;
  FMessageID := 0;
  FUnknownID := 0;
  FAdaptationLength := 0;
  FMessageLength := 0;

  FAdaptionType := 0;
  FAdaptionDataLength := 0;
  if Assigned(FAdaptionData) then
  begin
    FreeMem(FAdaptionData);
    FAdaptionData := nil;
  end;
end;

function TDSMCCBaseHeader.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Clear;

  FProtocolDiscriminator := ABuffer^;
  inc(ABuffer);

  FDSMCCType := ABuffer^;
  inc(ABuffer);

  FMessageID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FUnknownID := GetLong(ABuffer);
  inc(ABuffer, 4);

  inc(ABuffer);

  FAdaptationLength := ABuffer^;
  inc(ABuffer);

  FMessageLength := GetWord(ABuffer);
  inc(ABuffer, 2);

  if FAdaptationLength > 0 then
  begin
    FAdaptionDataLength := FAdaptationLength -1;

    FAdaptionType := ABuffer^;
    inc(ABuffer);

    FAdaptionData := AllocMem(FAdaptionDataLength);
    Move(ABuffer^, FAdaptionData^, FAdaptionDataLength);
  end;

  Result := True;
end;

(*** TDSMCCDownloadServerInitiate *********************************************)

constructor TDSMCCDownloadServerInitiate.Create;
begin
  inherited Create;
  FType := htDSI;
end;

procedure TDSMCCDownloadServerInitiate.Clear;
begin
  inherited Clear;

  FillChar(FServerID, 20, $00);
  FPrivateDataLength := 0;
  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
end;

function TDSMCCDownloadServerInitiate.ParseBuffer(ABuffer: PByte): Boolean;
var
  compatibility_descriptor_length: Integer;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;

  inc(ABuffer, 12 + FAdaptationLength);

  Move(ABuffer^, FServerID, 20);
  inc(ABuffer, 20);

  // compatibility descriptor in a_92.pdf P32
  compatibility_descriptor_length := GetWord(ABuffer);
  inc(ABuffer, 2);
  if compatibility_descriptor_length > 0 then
  begin
    // TODO: Parse Descriptor
    // According to Specs, the compatibility descriptor is not used !?
    inc(ABuffer, compatibility_descriptor_length);
  end;

  FPrivateDataLength := GetWord(ABuffer);
  inc(ABuffer, 2);

  if FPrivateDataLength > 0 then
  begin
    // FPrivateData is GroupInfoIndication  en 301192 P59
    FPrivateData := AllocMem(FPrivateDataLength); // TR 101 202 P43
    Move(ABuffer^, FPrivateData^, FPrivateDataLength);

//    with TFileStream.Create('c:\dmp\dmp_' + inttohex(Random($FFFFFFFF), 8) + '.log', fmCreate) do
//    begin
//      Write(FPrivateData^, FPrivateDataLength);
//      Free;
//    end;
  end;

  Result := True;
end;

(*** TDSMCCDownloadInfoIndicationModule ***************************************)

constructor TDSMCCDownloadInfoIndicationModule.Create;
begin
  inherited Create;
  FBIOPModuleInfo := TBIOPModuleInfo.Create;
  Clear;
end;

constructor TDSMCCDownloadInfoIndicationModule.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TDSMCCDownloadInfoIndicationModule.Destroy;
begin
  FBIOPModuleInfo.Free;
  inherited Destroy;
end;

procedure TDSMCCDownloadInfoIndicationModule.Clear;
begin
  FModuleId := 0;
  FModuleSize := 0;
  FModuleVersion := 0;
  FModuleInfoLength := 0;
  FBIOPModuleInfo.Clear;
end;

function TDSMCCDownloadInfoIndicationModule.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Clear;
  Result := True;

  FModuleId := GetWord(ABuffer);
  inc(ABuffer, 2);

  FModuleSize := GetLong(ABuffer);
  inc(ABuffer, 4);

  FModuleVersion := ABuffer^;
  inc(ABuffer);

  FModuleInfoLength := ABuffer^;
  inc(ABuffer);

  if FModuleInfoLength > 0
    then FBIOPModuleInfo.ParseBuffer(ABuffer);
end;

(*** TDSMCCDownloadInfoIndicationModuleList ***********************************)

function TDSMCCDownloadInfoIndicationModuleList.Get(Index: Integer): TDSMCCDownloadInfoIndicationModule;
begin
  Result := inherited Get(Index);
end;

procedure TDSMCCDownloadInfoIndicationModuleList.Put(Index: Integer; Item: TDSMCCDownloadInfoIndicationModule);
begin
  inherited Put(Index, Item);
end;

function TDSMCCDownloadInfoIndicationModuleList.Add(Item: TDSMCCDownloadInfoIndicationModule): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDSMCCDownloadInfoIndicationModuleList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TDSMCCDownloadInfoIndicationModule(Items[i]).Free;
  inherited Clear;
end;

procedure TDSMCCDownloadInfoIndicationModuleList.Delete(Index: Integer);
begin
  TDSMCCDownloadInfoIndicationModule(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TDSMCCDownloadInfoIndication *********************************************)

constructor TDSMCCDownloadInfoIndication.Create;
begin
  inherited Create;
  FType := htDII;
  FModuleList := TDSMCCDownloadInfoIndicationModuleList.Create;
end;

constructor TDSMCCDownloadInfoIndication.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TDSMCCDownloadInfoIndication.Destroy;
begin
  Clear;
  FreeAndNil(FModuleList);
  inherited Destroy;
end;

procedure TDSMCCDownloadInfoIndication.Clear;
begin
  inherited Clear;

  FDownloadID := 0;
  FBlockSize := 0;
  FWindowSize := 0;
  FACKPPeriod := 0;
  FTCDownloadWindow := 0;
  FTCDownloadScenario := 0;

  if Assigned(FModuleList)
    then FModuleList.Clear;

  FPrivateDataLength := 0;
  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
end;

function TDSMCCDownloadInfoIndication.ParseBuffer(ABuffer: PByte): Boolean;
var
  numberOfModules: Integer;
  i: Integer;
  module: TDSMCCDownloadInfoIndicationModule;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;

  inc(ABuffer, 12 + FAdaptationLength);

  FDownloadID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FBlockSize := GetWord(ABuffer);
  inc(ABuffer, 2);

  FWindowSize := ABuffer^;
  inc(ABuffer);

  FACKPPeriod := ABuffer^;
  inc(ABuffer);

  FTCDownloadWindow := GetLong(ABuffer);
  inc(ABuffer, 4);

  FTCDownloadScenario := GetLong(ABuffer);
  inc(ABuffer, 4);

  inc(ABuffer, 2);

  numberOfModules := GetWord(ABuffer);
  inc(ABuffer, 2);

  for i := 0 to numberOfModules -1 do
  begin
    module := TDSMCCDownloadInfoIndicationModule.Create(ABuffer);
    inc(ABuffer, 8 + module.ModuleInfoLength);
    FModuleList.Add(module);
  end;

  FPrivateDataLength := GetWord(ABuffer);
  inc(ABuffer, 2);

  if FPrivateDataLength > 0 then
  begin
    FPrivateData := AllocMem(FPrivateDataLength);
    Move(ABuffer^, FPrivateData^, FPrivateDataLength);
  end;

  Result := True;
end;

(*** TDSMCCDownloadDataBlock **************************************************)

constructor TDSMCCDownloadDataBlock.Create;
begin
  inherited Create;
  FType := htDDB;
end;

procedure TDSMCCDownloadDataBlock.Clear;
begin
  inherited Clear;

  FModuleID := 0;
  FModuleVersion := 0;
  FBlockNumber := 0;
  FBlockDataLength := 0;
  if Assigned(FBlockData) then
  begin
    FreeMem(FBlockData);
    FBlockData := nil;
  end;
end;

function TDSMCCDownloadDataBlock.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;
  inc(ABuffer, 12 + FAdaptationLength);

  FModuleID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FModuleVersion := ABuffer^;
  inc(ABuffer);

  inc(ABuffer);

  FBlockNumber := GetWord(ABuffer);
  inc(ABuffer, 2);

  FBlockDataLength := FMessageLength - FAdaptationLength - 6;
  if FBlockDataLength > 0 then
  begin
    FBlockData := AllocMem(FBlockDataLength);
    Move(ABuffer^, FBlockData^, FBlockDataLength);
  end;

  Result := True;
end;

(*** TDSMCCDownloadCancel *****************************************************)

constructor TDSMCCDownloadCancel.Create;
begin
  inherited Create;
  FType := htDC;
end;

procedure TDSMCCDownloadCancel.Clear;
begin
  inherited Clear;

  FDownloadID := 0;
  FModuleID := 0;
  FBlockNumber := 0;
  FDownloadCancelReason := 0;
  FPrivateDataLength := 0;
  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
end;

function TDSMCCDownloadCancel.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;
  inc(ABuffer, 12 + FAdaptationLength);

  FDownloadID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FModuleID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FBlockNumber := GetWord(ABuffer);
  inc(ABuffer, 2);

  FDownloadCancelReason := ABuffer^;
  inc(ABuffer);

  inc(ABuffer);

  FPrivateDataLength := GetWord(ABuffer);
  inc(ABuffer, 2);

  if FPrivateDataLength > 0 then
  begin
    FPrivateData := AllocMem(FPrivateDataLength);
    Move(ABuffer^, FPrivateData^, FPrivateDataLength);
  end;

  Result := True;
end;

(*** TDSMCCUserNetworkMessage *************************************************)

class function TDSMCCUserNetworkMessage.CreateDownload(ABuffer: PByte): TDSMCCBaseHeader;
begin
  case GetWord(@PByteArray(ABuffer)[2]) of
    $1002: Result := TDSMCCDownloadInfoIndication.Create(ABuffer);
    $1005: Result := TDSMCCDownloadCancel.Create(ABuffer);
    $1006: Result := TDSMCCDownloadServerInitiate.Create(ABuffer);
    else   Result := TDSMCCBaseHeader.Create(ABuffer);
  end;
end;

(*** TDSMCCSection ************************************************************)

constructor TDSMCCSection.Create;
begin
  inherited Create;
  FUsesSectionSyntax := False;
end;

constructor TDSMCCSection.Create(ABuffer: PByte; ASize: Integer);
begin
  Create;
  ParseBuffer(ABuffer, ASize);
end;

destructor TDSMCCSection.Destroy;
begin
  inherited Destroy;
end;

procedure TDSMCCSection.Clear;
begin
  inherited Clear;

  FTableIDExtension := 0;

  if Assigned(FDSMCCHeader)
    then FreeAndNil(FDSMCCHeader);
end;

function TDSMCCSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID < $3A) or (FTableID > $3F) or (FSectionLength > 4093) or (ASize < 12) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer);
  inc(ABuffer, 2);

  FTableIDExtension := GetWORD(ABuffer);
  inc(ABuffer, 5);

  case FTableID of
//    $3A: Reserved;
    $3B: FDSMCCHeader := TDSMCCUserNetworkMessage.CreateDownload(ABuffer);
    $3C: FDSMCCHeader := TDSMCCDownloadDataBlock.Create(ABuffer);
    $3D: OutputDebugString('0x3D - DSMCC Descriptor in TDSMCCSection.ParseBuffer');
//    $3D: Descriptors;
//    $3E: Private Data;
//    $3F: Reserved;
    else FDSMCCHeader := TDSMCCBaseHeader.Create(ABuffer);
  end;

  Result := True;
end;

end.
