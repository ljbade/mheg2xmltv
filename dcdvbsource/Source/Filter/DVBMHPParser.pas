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

unit DVBMHPParser;

interface

uses
  Windows, Classes, SysUtils, MPEGUtils, BDAUtils, MPEGConst, DSMCCBIOP, DSMCCSections,
  MHPConst, MHPDescriptors, DVBConst, DVBDescriptors, ZLibEx, BaseClass, MHPSections,
  MHPUtils, MPEGDescriptors, DSMCCConst, JvSimpleXML, ActiveX;

const
  DII_WRITE_CHECK_DELAY = 60000;

type
  TModule = class;
  TDownload = class;
  TDSMCCParser = class;

  TDecodingThread = class(TThread)
  private
    FOwner: TModule;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TModule);
  end;

  TWriteThread = class(TThread)
  protected
    FParser: TDSMCCParser;
    FDownload: TDownload;
    FProfile: TBIOPProfileBody;
    FPath: WideString;
    procedure Execute; override;
  public
    constructor Create(AParser: TDSMCCParser; ADownload: TDownload; AProfile: TBIOPProfileBody; APath: WideString);
    destructor Destroy; override;
  end;

  TModule = class
  private
    FBIOPList: TList;
    FID: Integer;
    FVersion: Integer;
    FSize: Integer;
    FCompressed: Boolean;
    FFinished: Boolean;
    FDecoded: Boolean;
    FBuffer: PByte;
    FLastBlockNumber: Integer;
    FCurrentBufferPos: Integer;
    FIsDecoding: Boolean;
    FDecodingThread: TDecodingThread;
    function GetBIOPCount: Integer;
    function GetBIOP(AIndex: Integer): TBIOPBaseMessage;
  public
    constructor Create(AFrom: TDSMCCDownloadInfoIndicationModule);
    destructor Destroy; override;

    procedure ParseDownloadDataBlock(ABuffer: TDSMCCDownloadDataBlock);
    procedure Decode;
    procedure DecodeInternal;

    property ID: Integer read FID write FID;
    property Version: Integer read FVersion write FVersion;
    property Size: Integer read FSize write FSize;
    property Compressed: Boolean read FCompressed write FCompressed;
    property Finished: Boolean read FFinished write FFinished;
    property Buffer: PByte read FBuffer write FBuffer;
    property LastBlockNumber: Integer read FLastBlockNumber write FLastBlockNumber;
    property CurrentBufferPos: Integer read FCurrentBufferPos write FCurrentBufferPos;

    property Decoded: Boolean read FDecoded;
    property BIOPCount: Integer read GetBIOPCount;
    property BIOP[AIndex: Integer]: TBIOPBaseMessage read GetBIOP;
  end;

  TDownload = class(TList)
  private
    FDownloadID: Cardinal;
    FTransactionID: Cardinal;
    FIsStandalone: Boolean;
    FDataWritten: Boolean;
    function Get(Index: Integer): TModule;
    procedure Put(Index: Integer; Item: TModule);
    function GetComplete: Boolean;
  public
    constructor Create; overload;
    constructor Create(AFrom: TDSMCCDownloadInfoIndication); overload;
    destructor Destroy; override;

    function Add(Item: TModule): Integer;
    procedure AddModulesFromDII(DII: TDSMCCDownloadInfoIndication);
    procedure Clear; override;
    procedure DeleteWithoutDestroy(Index: Integer);
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TModule read Get write Put; default;
    property DownloadID: Cardinal read FDownloadID write FDownloadID;
    property TransactionID: Cardinal read FTransactionID write FTransactionID;
    property Complete: Boolean read GetComplete;
    property IsStandalone: Boolean read FIsStandalone;
    property DataWritten: Boolean read FDataWritten write FDataWritten;
  end;

  TDownloadList = class(TList)
  private
    function Get(Index: Integer): TDownload;
    procedure Put(Index: Integer; Item: TDownload);
  public
    function Add(Item: TDownload): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function GetModuleForDDB(DDB: TDSMCCDownloadDataBlock; out Download: TDownload; out Module: TModule): Boolean;
    property Items[Index: Integer]: TDownload read Get write Put; default;
  end;

  TDVBMHPDVBJApplication = record
    FBaseDirectory: String;
    FClassPathExtension: String;
    FInitialClass: String;
  end;

  TDVBMHPApplication = class
  private
    FDVBJApplication: TDVBMHPDVBJApplication;
    FTestApplication: Boolean;
    FApplicationType: Integer;
    FOrganisationID: Cardinal;
    FApplicationID: Integer;
    FApplicationControlCode: Integer;
    FApplicationName: String;
    FSavePath: String;
    FParameters: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDVBMHPApplications = class
  private
    FList: TList;
    function GetApplicationCount: Integer;
    function GetApplication(AIndex: Integer): TDVBMHPApplication;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property ApplicationCount: Integer read GetApplicationCount;
    property Application[Index: Integer]: TDVBMHPApplication read GetApplication; default;
  end;

  TWriteDelay = record
    TransactionID: Cardinal;
    NextCheck: Cardinal;
  end;
  PWriteDelay = ^TWriteDelay;

  TDSMCCParser = class
  private
    FWriteList: TList;
    FWriteDelay: TList;
    FApps: TDVBMHPApplications;
    FDSMCCSection: TDSMCCSection;
    FLock: TBCCritSec;
    FSavePath: WideString;
    FCounter: Integer;
    FDownloadList: TDownloadList;
    FAIT: TMHPApplicationInformationSection;
    procedure SetSavePath(APath: WideString);

    procedure ParseDownloadServerInitiate(DSI: TDSMCCDownloadServerInitiate);
    procedure ParseDownloadInfoIndication(DII: TDSMCCDownloadInfoIndication);
    procedure ParseDownloadDataBlock(DDB: TDSMCCDownloadDataBlock);
    procedure ParseDownloadCancel(DC: TDSMCCDownloadCancel);

    procedure ParseAIT;

    procedure WriteData(ADownload: TDownload; AProfile: TBIOPProfileBody; APath: WideString); overload;
    procedure WriteData(ADownload: TDownload; APath: WideString); overload;
    procedure ParseBindings(ABinding: TBinding; APath: WideString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetMHPApps(out AMHP: PChar): Boolean;

    procedure ParsePSIBuffer(ABuffer: PByte; ASize: Integer);
    procedure RemoveFromList(AThread: TWriteThread);
  published
    property SavePath: WideString read FSavePath write SetSavePath;
  end;

implementation

(*** TDecodingThread **********************************************************)

constructor TDecodingThread.Create(AOwner: TModule);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

procedure TDecodingThread.Execute;
begin
  Priority := tpLowest;
  sleep(50);
  FOwner.DecodeInternal;
end;

(*** TModule ******************************************************************)

constructor TModule.Create(AFrom: TDSMCCDownloadInfoIndicationModule);
var
  desc: TBaseDescriptor;
begin
  inherited Create;
  FDecodingThread := TDecodingThread.Create(Self);
  FID := AFrom.ModuleId;
  FVersion := AFrom.ModuleVersion;

  FCompressed := AFrom.BIOPModuleInfo.Descriptors.GetDescriptor(TDVBCompressedModuleDescriptor, desc);

  FFinished  := False;
  FSize := AFrom.ModuleSize;
  FBuffer := AllocMem(FSize);

  FLastBlockNumber := 0;
  FCurrentBufferPos := 0;
  FDecoded := False;

  FBIOPList := TList.Create;
end;

destructor TModule.Destroy;
var
  i: Integer;
begin
  FDecodingThread.Terminate;
  if not FDecodingThread.Terminated
    then FDecodingThread.WaitFor;
  FDecodingThread.Free;
  for i := 0 to FBIOPList.Count -1
    do TBIOPBaseMessage(FBIOPList[i]).Free;
  FBIOPList.Clear;
  FBIOPList.Free;
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TModule.GetBIOPCount: Integer;
begin
  Result := FBIOPList.Count;
end;

function TModule.GetBIOP(AIndex: Integer): TBIOPBaseMessage;
begin
  Result := FBIOPList[AIndex];
end;

procedure TModule.ParseDownloadDataBlock(ABuffer: TDSMCCDownloadDataBlock);
begin
  if (FFinished) or (ABuffer.ModuleID <> FID) or
     (ABuffer.ModuleVersion <> FVersion) or
     (ABuffer.BlockNumber <> FLastBlockNumber)
    then Exit;

  Move(ABuffer.BlockData^, PChar(FBuffer)[FCurrentBufferPos], ABuffer.BlockDataLength);

  inc(FLastBlockNumber);
  inc(FCurrentBufferPos, ABuffer.BlockDataLength);

  FFinished := FCurrentBufferPos = FSize;
  Decode;
end;

procedure TModule.DecodeInternal;
var
  biop: TBIOPBaseMessage;
  buffer: PByte;
  size: Integer;
begin
  buffer := FBuffer;
  size := FSize;

  if FCompressed then
  begin
    try
      ZDecompress(FBuffer, FSize, Pointer(buffer), size);
      FreeMem(FBuffer);
      FBuffer := buffer;
      FSize := size;
    except
      // Decompressing failed -> Exit without doing anything
      Exit;
    end;
  end;

  while (size > 0) do
  begin
    biop := TBIOPBaseMessage.CreateBIOPMessage(buffer);
    if not Assigned(biop) or not biop.ParseBuffer(buffer) then
    begin
      // This should never happen except the Provider is sending out
      // wrong Data. Since we don't know if the following BIOP's are valid,
      // we Exit here.
      Exit;
    end;
    FBIOPList.Add(biop);

    dec(size, biop.TotalLength);
    inc(buffer, biop.TotalLength);
  end;

  FIsDecoding := False;
  FDecoded := True;
end;

procedure TModule.Decode;
begin
  if not FFinished or FDecoded or (FSize <= 0) or not Assigned(FBuffer) or FIsDecoding
    then Exit;

  FIsDecoding := True;
  FDecodingThread.Resume;
end;

(*** TDownload ****************************************************************)

constructor TDownload.Create;
begin
  inherited Create;
  FDownloadID := $FFFFFFFF;
  FTransactionID := $FFFFFFFF;
  FIsStandalone := False;
  FDataWritten := False;
end;

constructor TDownload.Create(AFrom: TDSMCCDownloadInfoIndication);
begin
  Create;
  FDownloadID := AFrom.DownloadID;
  FTransactionID := AFrom.TransactionID;
  FIsStandalone := (AFrom.TransactionID and $FFFF) < 2;
end;

destructor TDownload.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDownload.Get(Index: Integer): TModule;
begin
  Result := inherited Get(Index);
end;

procedure TDownload.Put(Index: Integer; Item: TModule);
begin
  inherited Put(Index, Item);
end;

function TDownload.Add(Item: TModule): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDownload.AddModulesFromDII(DII: TDSMCCDownloadInfoIndication);
var
  i, c: Integer;
  module: TModule;
  found: Boolean;
begin
  for i := 0 to DII.ModuleList.Count -1 do
  begin
    found := False;
    for c := 0 to Count -1 do
    begin
      module := Get(c);
      if module.FID = DII.ModuleList[i].ModuleId then
      begin
        found := True;
        break;
      end;
    end;
    if not found then
    begin
      Add(TModule.Create(DII.ModuleList[i]));
    end;
  end;
end;

procedure TDownload.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TModule(Items[i]).Free;
  inherited Clear;
end;

procedure TDownload.DeleteWithoutDestroy(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TDownload.Delete(Index: Integer);
begin
  TModule(Items[Index]).Free;
  inherited Delete(Index);
end;

function TDownload.GetComplete: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if not Get(i).Finished or not Get(i).Decoded then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

(*** TDownloadList ************************************************************)

function TDownloadList.Get(Index: Integer): TDownload;
begin
  Result := inherited Get(Index);
end;

procedure TDownloadList.Put(Index: Integer; Item: TDownload);
begin
  inherited Put(Index, Item);
end;

function TDownloadList.Add(Item: TDownload): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDownloadList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TDownload(Items[i]).Free;
  inherited Clear;
end;

procedure TDownloadList.Delete(Index: Integer);
begin
  TDownload(Items[Index]).Free;
  inherited Delete(Index);
end;

function TDownloadList.GetModuleForDDB(DDB: TDSMCCDownloadDataBlock; out Download: TDownload; out Module: TModule): Boolean;
var
  i, c: Integer;
begin
  for i := 0 to Count -1 do
  begin
    Download := Get(i);
    for c := 0 to Get(i).Count -1 do
    begin
      Module := Download[c];
      if (Module.ID = DDB.ModuleID) and
         (Download.DownloadID = DDB.DownloadID) and
         (Module.Version = DDB.ModuleVersion) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
  Download := nil;
  Module := nil;
end;

(*** TDVBMHPApplication *******************************************************)

constructor TDVBMHPApplication.Create;
begin
  inherited Create;
  FParameters := TStringList.Create;
end;

destructor TDVBMHPApplication.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

(*** TDVBMHPApplications ******************************************************)

constructor TDVBMHPApplications.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDVBMHPApplications.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDVBMHPApplications.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDVBMHPApplication(FList[i]).Free;
  FList.Clear;
end;

function TDVBMHPApplications.GetApplicationCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBMHPApplications.GetApplication(AIndex: Integer): TDVBMHPApplication;
begin
  if (AIndex < 0) or (AIndex >= FList.Count)
    then Result := nil
    else Result := FList[AIndex];
end;

(*** TDSMCCParser *************************************************************)

constructor TDSMCCParser.Create;
begin
  inherited Create;
  FWriteDelay := TList.Create;
  FWriteList := TList.Create;
  FCounter := 0;
  FSavePath := '';
  FLock := TBCCritSec.Create;
  FApps := TDVBMHPApplications.Create;
  FDSMCCSection := TDSMCCSection.Create;
  FAIT := TMHPApplicationInformationSection.Create;
  FDownloadList := TDownloadList.Create;
end;

destructor TDSMCCParser.Destroy;
var
  i: Integer;
begin
  Clear;
  i := 0;
  while FWriteList.Count > 0 do
  begin
    Sleep(10);
    inc(i);
    if i > 1000
      then break;
  end;
  FWriteList.Free;
  FDSMCCSection.Free;
  FDownloadList.Free;
  FAIT.Free;
  FApps.Free;
  FWriteDelay.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TDSMCCParser.SetSavePath(APath: WideString);
begin
  FLock.Lock;
  try
    FSavePath := AddBackSlash(APath);
  finally
    FLock.UnLock;
  end;
end;

procedure TDSMCCParser.Clear;
var
  i: Integer;
begin
  FLock.Lock;
  try
    FDownloadList.Clear;
    for i := 0 to FWriteDelay.Count -1 do
    begin
      Dispose(PWriteDelay(FWriteDelay[i]));
    end;
    FWriteDelay.Clear;
  finally
    FLock.UnLock;
  end;
end;

procedure TDSMCCParser.ParsePSIBuffer(ABuffer: PByte; ASize: Integer);
begin
  FLock.Lock;
  try
    if (FSavePath = '')
      then Exit;

    case ABuffer^ of
      DSMCC_TABLE_ID_DATA_MIN..DSMCC_TABLE_ID_DATA_MAX:
      begin
        if FDSMCCSection.ParseBuffer(ABuffer, ASize) and FDSMCCSection.Valid then
        begin
          case FDSMCCSection.DSMCCHeader.Type_ of
            htDII: ParseDownloadInfoIndication(TDSMCCDownloadInfoIndication(FDSMCCSection.DSMCCHeader));
            htDC:  ParseDownloadCancel(TDSMCCDownloadCancel(FDSMCCSection.DSMCCHeader));
            htDSI: ParseDownloadServerInitiate(TDSMCCDownloadServerInitiate(FDSMCCSection.DSMCCHeader));
            htDDB: ParseDownloadDataBlock(TDSMCCDownloadDataBlock(FDSMCCSection.DSMCCHeader));
          end;
        end;
      end;
      MHP_TABLE_ID_AIT:
      begin
        if FAIT.ParseBuffer(ABuffer, ASize) and FAIT.Valid
          then ParseAIT;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

procedure TDSMCCParser.ParseDownloadServerInitiate(DSI: TDSMCCDownloadServerInitiate);
var
  buffer: PByte;
  type_id_length: Integer;
  type_id: Cardinal;
  tagged_profiles_count: Integer;
  i, c, k: Integer;
  profile_ID_tag: Cardinal;
  t: PWriteDelay;
  profile_data_length: Integer;
  profile, p2: TBIOPProfileBody;
  download: TDownload;
  delay: Boolean;
  found: Boolean;
begin
  // For DownloadServerInitiate messages the 2 least significant bytes of the
  // transactionId shall be in the range 0x0000 to 0x0001.
  if (DSI.TransactionID and $FFFF) > 1 then
  begin
    Exit;
  end;

  if (DSI.PrivateDataLength <= 8) or (DSI.PrivateData = nil)
    then Exit;

  buffer := DSI.PrivateData;

  type_id_length := GetLong(buffer);
  inc(buffer, 4);
  if (type_id_length <> 4)
    then Exit;

  type_id := GetLong(buffer);
  inc(buffer, 4);

  if (type_id <> BIOP_SRG)
    then Exit;              

  tagged_profiles_count := GetLong(buffer);
  inc(buffer, 4);

  for i := 0 to tagged_profiles_count -1 do
  begin
    profile_ID_tag := GetLong(buffer);
    inc(buffer, 4);

    profile_data_length := GetLong(buffer);
    inc(buffer, 4);

    if (profile_ID_tag = TAG_BIOP) then
    begin
      dec(buffer, 8);
      profile := TBIOPProfileBody.Create(buffer);
      delay := False;
      found := False;
      t := nil;
      for k := 0 to FWriteDelay.Count -1 do
      begin
        t := PWriteDelay(FWriteDelay[k]);
        if IsEqualTransactionID(t.TransactionID, profile.TransactionID) then
        begin
          delay := t.NextCheck < GetTickCount;
          found := True;
          break;
        end;
      end;

      if not found then
      begin
        new(t);
        t.TransactionID := profile.TransactionID;
        t.NextCheck := GetTickCount + DII_WRITE_CHECK_DELAY;
        FWriteDelay.Add(t);
      end;

      if not delay then
      begin
        profile.Free;
        inc(buffer, 8);
        Continue;
      end;

      if Assigned(t) then
      begin
        t.NextCheck := GetTickCount + DII_WRITE_CHECK_DELAY;
        t.TransactionID := profile.TransactionID;
      end;

      for c := 0 to FDownloadList.Count -1 do
      begin
        download := FDownloadList[c];
        if IsEqualTransactionID(download.TransactionID, profile.TransactionID) then
        begin
          p2 := TBIOPProfileBody.Create(buffer);
          FWriteList.Add(TWriteThread.Create(Self, download, p2, FSavePath));
        end;
      end;

      profile.Free;
      inc(buffer, 8);
    end;

    if profile_data_length > 0 then
    begin
      inc(buffer, profile_data_length);
    end;
  end;
end;

procedure TDSMCCParser.ParseDownloadInfoIndication(DII: TDSMCCDownloadInfoIndication);
var
  i: Integer;
  download: TDownload;
begin
  i := FDownloadList.Count;
  while (i > 0) do
  begin
    dec(i);
    download := FDownloadList[i];
    if (download.DownloadID = DII.DownloadID) and
       (IsEqualTransactionID(download.TransactionID, DII.TransactionID)) then
    begin
      if (download.TransactionID <> DII.TransactionID) then
      begin
        FDownloadList.Delete(i);
      end else
      begin
        Exit;
      end;
    end;
  end;

  download := TDownload.Create(DII);
  download.AddModulesFromDII(DII);
  FDownloadList.Add(download);
end;

procedure TDSMCCParser.ParseDownloadDataBlock(DDB: TDSMCCDownloadDataBlock);
var
  module: TModule;
  download: TDownload;
begin
  if FDownloadList.GetModuleForDDB(DDB, download, module) then
  begin
    if download.Complete then
    begin
      if download.IsStandalone then
      begin
        if not download.DataWritten then
        begin
          WriteData(download, FSavePath);
        end;
      end;
    end else
    begin
      module.ParseDownloadDataBlock(DDB);
    end;
  end;
end;

procedure TDSMCCParser.WriteData(ADownload: TDownload; AProfile: TBIOPProfileBody; APath: WideString);
var
  i, c, k: Integer;
  module: TModule;
  biop: TBIOPBaseMessage;
  dir: TBIOPDirectoryMessage;
begin
  if not ADownload.Complete
    then Exit;

  for i := 0 to ADownload.Count -1 do
  begin
    module := ADownload[i];
    if (module.ID = AProfile.ModuleID) then
    begin
      for c := 0 to module.BIOPCount -1 do
      begin
        biop := module.BIOP[c];
        if (AProfile.ObjectKeyLength > 0) and
           (AProfile.ObjectKeyData <> nil) and
           (biop.ObjectKeyLength > 0) and
           (biop.ObjectKeyData <> nil) and
           (CompareMem(AProfile.ObjectKeyData, biop.ObjectKeyData, AProfile.ObjectKeyLength)) then
        begin
          if (biop.Type_ = mtDirectory) or (biop.Type_ = mtServiceGateway) then
          begin
            dir := TBIOPDirectoryMessage(biop);
            for k := 0 to dir.Bindings.Count -1
              do ParseBindings(dir.Bindings[k], APath);
          end;
        end;
      end;
    end;
  end;
end;

procedure TDSMCCParser.WriteData(ADownload: TDownload; APath: WideString);
var
  i, c, k: Integer;
  module: TModule;
  biop: TBIOPBaseMessage;
  dir: TBIOPDirectoryMessage;
begin
  if not ADownload.Complete or ADownload.DataWritten
    then Exit;

  for i := 0 to ADownload.Count -1 do
  begin
    module := ADownload[i];
    for c := 0 to module.BIOPCount -1 do
    begin
      biop := module.BIOP[c];
      if biop.Type_ = mtServiceGateway then
      begin
        dir := TBIOPDirectoryMessage(biop);
        for k := 0 to dir.Bindings.Count -1
          do ParseBindings(dir.Bindings[k], APath);
      end;
    end;
  end;

  ADownload.DataWritten := True;
end;

procedure TDSMCCParser.ParseBindings(ABinding: TBinding; APath: WideString);
var
  i, c, k, z, y: Integer;
  biop: TBIOPBaseMessage;
  profile: TBaseProfileBody;
  dir: TBIOPDirectoryMessage;
  pb: TBIOPProfileBody;
  fil: TBIOPFileMessage;
  download: TDownload;
  module: TModule;
begin
  if not DirectoryExists(APath)
    then CreateDir(APath);
  APath := AddBackSlash(APath);

  if UpperCase(ABinding.Kind) = 'DIR' then
  begin
    APath := APath + ABinding.ID;
    if not DirectoryExists(APath)
      then CreateDir(APath);
    APath := AddBackSlash(APath);

    for i := 0 to ABinding.ProfilesList.Count -1 do
    begin
      profile := ABinding.ProfilesList[i];
      if profile.ProfileType = ptBIOPProfileBody then
      begin
        pb := TBIOPProfileBody(profile);

        for c := 0 to FDownloadList.Count -1 do
        begin
          download := FDownloadList[c];

          for k := 0 to download.Count -1 do
          begin
            module := download[k];

            if (module.ID = pb.ModuleID) and (module.Decoded) then
            begin
              for z := 0 to module.BIOPCount -1 do
              begin
                biop := module.BIOP[z];
                if (biop.Type_ = mtDirectory) or (biop.Type_ = mtServiceGateway) then
                begin
                  dir := TBIOPDirectoryMessage(biop);
                  if (dir.ObjectKeyLength > 0) and
                     (dir.ObjectKeyData <> nil) and
                     (pb.ObjectKeyLength > 0) and
                     (pb.ObjectKeyData <> nil) and
                     (CompareMem(dir.ObjectKeyData, pb.ObjectKeyData, dir.ObjectKeyLength)) then
                  begin
                    for y := 0 to dir.Bindings.Count -1
                      do ParseBindings(dir.Bindings[y], APath);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end else
  if UpperCase(ABinding.Kind) = 'FIL' then
  begin
    for i := 0 to ABinding.ProfilesList.Count -1 do
    begin
      profile := ABinding.ProfilesList[i];
      if profile.ProfileType = ptBIOPProfileBody then
      begin
        pb := TBIOPProfileBody(profile);

        for c := 0 to FDownloadList.Count -1 do
        begin
          download := FDownloadList[c];

          for k := 0 to download.Count -1 do
          begin
            module := download[k];

            if (module.ID = pb.ModuleID) and (module.Decoded) then
            begin
              for z := 0 to module.BIOPCount -1 do
              begin
                biop := module.BIOP[z];
                if (biop.Type_ = mtFile) then
                begin
                  fil := TBIOPFileMessage(biop);
                  if (not fil.Written) and
                     (fil.ObjectKeyLength > 0) and
                     (fil.ObjectKeyData <> nil) and
                     (pb.ObjectKeyLength > 0) and
                     (pb.ObjectKeyData <> nil) and
                     (CompareMem(fil.ObjectKeyData, pb.ObjectKeyData, fil.ObjectKeyLength)) then
                  begin
                    try
                      with TFileStream.Create(APath + ABinding.ID, fmCreate) do
                      begin
                        Write(fil.ContentData^, fil.ContentLength);
                        fil.Written := True;
                        Free;
                      end;
                    except
                      // The File propably has been opened by another Process,
                      // so we skip it.
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDSMCCParser.ParseDownloadCancel(DC: TDSMCCDownloadCancel);
var
  i: Integer;
begin
  i := FDownloadList.Count;
  while (i > 0) do
  begin
    dec(i);
    if (FDownloadList[i].DownloadID = DC.DownloadID) and
       (FDownloadList[i].TransactionID = DC.TransactionID) then
    begin
      FDownloadList.Delete(i);
    end;
  end;
end;

procedure TDSMCCParser.ParseAIT;
var
  i, c: Integer;
  app: TMHPApplication;
  mapp: TDVBMHPApplication;
  found_app: Boolean;
  app_name: TMHPApplicationNameDescriptor;
  dvbjapp: TMHPDVBJApplicationDescriptor;
  dvbjloc: TMHPDVBJApplicationLocationDescriptor;
begin
  if (FAIT.ApplicationType < 1) or (FAIT.ApplicationType > 2)
    then Exit;

  for i := 0 to FAIT.Applications.Count -1 do
  begin
    found_app := False;
    app := FAIT.Applications[i];
    for c := 0 to FApps.ApplicationCount -1 do
    begin
      mapp := FApps[c];
      if (FAIT.ApplicationType = mapp.FApplicationType) and
         (Boolean(FAIT.TestApplicationFlag) = mapp.FTestApplication) and
         (app.ApplicationIDentifier.ApplicationID = mapp.FApplicationID) and
         (app.ApplicationIDentifier.OrganisationID = mapp.FOrganisationID) and
         (app.ApplicationControlCode = mapp.FApplicationControlCode) then
      begin
        found_app := True;
        break;
      end;
    end;

    if found_app
      then Continue;

    mapp := TDVBMHPApplication.Create;
    FApps.FList.Add(mapp);

    mapp.FSavePath := FSavePath;
    mapp.FTestApplication := Boolean(FAIT.TestApplicationFlag);
    mapp.FApplicationType := FAIT.ApplicationType;
    mapp.FApplicationID := app.ApplicationIDentifier.ApplicationID;
    mapp.FOrganisationID := app.ApplicationIDentifier.OrganisationID;
    mapp.FApplicationControlCode := app.ApplicationControlCode;

    // find the Application Name
    if app.Descriptors.GetDescriptor(TMHPApplicationNameDescriptor, app_name) then
    begin
      if app_name.CountItems > 0
        then mapp.FApplicationName := app_name.Names[0].Name;
    end;

    case FAIT.ApplicationType of
      1: // DVB-J Aplication
      begin
        // find the DVB-J Application Descriptor
        if app.Descriptors.GetDescriptor(TMHPDVBJApplicationDescriptor, dvbjapp) then
        begin
          for c := 0 to dvbjapp.CountItems -1
            do mapp.FParameters.Add(dvbjapp.Parameters[c].Parameter)
        end;

        // find the DVB-J Application Location Descriptor
        if app.Descriptors.GetDescriptor(TMHPDVBJApplicationLocationDescriptor, dvbjloc) then
        begin
          mapp.FDVBJApplication.FBaseDirectory := dvbjloc.BaseDirectory;
          mapp.FDVBJApplication.FClassPathExtension := dvbjloc.ClassPathExtension;
          mapp.FDVBJApplication.FInitialClass := dvbjloc.InitialClass;
        end;
      end;
      2: // DVB-HTML
      begin

      end;
    end;
  end;
end;

function TDSMCCParser.GetMHPApps(out AMHP: PChar): Boolean;
var
  xml: TJvSimpleXml;
  str: String;
  l, i, c: Integer;
  app: TDVBMHPApplication;
  i1, i2: TJvSimpleXmlElem;
begin
  Result := False;
  AMHP := nil;

  FLock.Lock;
  try
    if FApps.FList.Count = 0
      then Exit;

    xml := TJvSimpleXml.Create(nil);
    xml.Root.Name := 'mhp';

    for i := 0 to FApps.FList.Count -1 do
    begin
      app := FApps.FList[i];
      i1 := xml.Root.Items.Add('application');
      i1.Properties.Add('applicationid', app.FApplicationID);
      i1.Properties.Add('organisationid', app.FOrganisationID);
      i1.Properties.Add('applicationcontrolcode', app.FApplicationControlCode);
      i1.Properties.Add('applicationname', app.FApplicationName);
      i1.Properties.Add('applicationtype', app.FApplicationType);
      i1.Properties.Add('testapplication', Byte(app.FTestApplication));
      i1.Properties.Add('savepath', app.FSavePath);
      if app.FApplicationType = 1 then
      begin
        i1.Properties.Add('basedirectory', app.FDVBJApplication.FBaseDirectory);
        i1.Properties.Add('classpathextension', app.FDVBJApplication.FClassPathExtension);
        i1.Properties.Add('initialclass', app.FDVBJApplication.FInitialClass);

        if app.FParameters.Count > 0 then
        begin
          i2 := i1.Items.Add('parameters');
          for c := 0 to app.FParameters.Count -1
            do i2.Items.Add('parameter').Properties.Add('name', app.FParameters[c]);
        end;
      end;
    end;

    str := xml.SaveToString;
    l := Length(str) + 1;
    AMHP := CoTaskMemAlloc(l);
    Move(str[1], AMHP^, l);
    xml.Free;

    Result := True;
  finally
    FLock.UnLock;
  end;
end;

(*** TWriteThread *************************************************************)

constructor TWriteThread.Create(AParser: TDSMCCParser; ADownload: TDownload; AProfile: TBIOPProfileBody; APath: WideString);
begin
  inherited Create(True);
  FParser := AParser;
  FDownload := ADownload;
  FProfile := AProfile;
  FPath := APath;
  FreeOnTerminate := True;
  Resume;
end;

destructor TWriteThread.Destroy;
begin
  FProfile.Free;
  try
    FParser.RemoveFromList(Self);
  except
  end;
  inherited Destroy;
end;

procedure TWriteThread.Execute;
begin
  Priority := tpLowest;
  try
    FParser.WriteData(FDownload, FProfile, FPath);
  except
  end;
end;

procedure TDSMCCParser.RemoveFromList(AThread: TWriteThread);
begin
  FWriteList.Remove(AThread);
end;

end.
