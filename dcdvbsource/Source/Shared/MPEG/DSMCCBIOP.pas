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

unit DSMCCBIOP;

{$I Compiler.inc}

interface

uses
  Classes, SysUtils, MPEGConst, MPEGDescriptors, MPEGSections, Windows,
  DSMCCConst, MHPDescriptors, MHPUtils, MHPConst, DVBDescriptors, DVBConst;

type
  TBIOPModuleInfo = class
  private
    FModuleTimeOut: Cardinal;
    FBlockTimeOut: Cardinal;
    FMinBlockTime: Cardinal;
    FTapsList: TList;
    FDescriptors: TDescriptorList;
    function GetTap(AIndex: Integer): TModuleInfoTap;
    function GetTapCount: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property CountTaps: Integer read GetTapCount;
    property Tap[Index: Integer]: TModuleInfoTap read GetTap;

    property ModuleTimeOut: Cardinal read FModuleTimeOut;
    property BlockTimeOut: Cardinal read FBlockTimeOut;
    property MinBlockTime: Cardinal read FMinBlockTime;
    property Descriptors: TDescriptorList read FDescriptors;
  end;

  // ---------------------------------------------------------------------------

const
  BIOP_MAGIC = $42494F50;
  BIOP_FILE  = $66696C00;
  BIOP_DIR   = $64697200;
  BIOP_SRG   = $73726700;
  BIOP_STR   = $73747200;

  TAG_BIOP          = $49534F06;
  TAG_LITE_OPTIONS  = $49534F05;

type
  TBIOPMessageType = ( mtBase, mtFile, mtDirectory, mtServiceGateway, mtServiceGatewayInfo );
  TProfileType = ( ptBase, ptBIOPProfileBody, ptLiteOptionsProfileBody );

  TBIOPMessage = class
  protected
    FType: TBIOPMessageType;
  public
    procedure Clear; virtual; abstract;
    function ParseBuffer(ABuffer: PByte): Boolean; virtual; abstract;
  end;

  TBIOPBaseMessage = class(TBIOPMessage)
  protected
    FMagic: Cardinal;
    FVersionMajor: Byte;
    FVersionMinor: Byte;
    FByteOrder: Byte;
    FMessageType: Byte;
    FMessageSize: Cardinal;
    FObjectKeyLength: Byte;
    FObjectKeyData: PByte;
    FObjectKindLength: Cardinal;
    FObjectKindData: Cardinal;
    FObjectInfoLength: Word;
    FTotalLength: Integer;
    // Used for identification. NOT parsed by Buffer !
    FWritten: Boolean;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte); overload; virtual;
    destructor Destroy; override;

    class function CreateBIOPMessage(ABuffer: PByte): TBIOPBaseMessage;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property Type_: TBIOPMessageType read FType;

    property Magic: Cardinal read FMagic;
    property VersionMajor: Byte read FVersionMajor;
    property VersionMinor: Byte read FVersionMinor;
    property ByteOrder: Byte read FByteOrder;
    property MessageType: Byte read FMessageType;
    property MessageSize: Cardinal read FMessageSize;
    property ObjectKeyLength: Byte read FObjectKeyLength;
    property ObjectKeyData: PByte read FObjectKeyData;
    property ObjectKindLength: Cardinal read FObjectKindLength;
    property ObjectKindData: Cardinal read FObjectKindData;
    property ObjectInfoLength: Word read FObjectInfoLength;
    property TotalLength: Integer read FTotalLength;
    // Used for identification. NOT parsed by Buffer !
    property Written: Boolean read FWritten write FWritten;
  end;

  // ---------------------------------------------------------------------------

  TServiceContext = class
  private
    FContextID: Cardinal;
    FContextDataLength: Word;
    FContextData: PByte;
    FTotalLength: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property ContextID: Cardinal read FContextID;
    property ContextDataLength: Word read FContextDataLength;
    property ContextData: PByte read FContextData;
    property TotalLength: Integer read FTotalLength;
  end;

  TServiceContextList = class(TList)
  private
    function Get(Index: Integer): TServiceContext;
    procedure Put(Index: Integer; Item: TServiceContext);
  public
    function Add(Item: TServiceContext): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TServiceContext read Get write Put; default;
  end;

  TBIOPFileMessage = class(TBIOPBaseMessage)
  private
    FContentSize: Int64;
    FDescriptors: TDescriptorList;
    FServiceContextList: TServiceContextList;
    FMessageBodyLength: Cardinal;
    FContentLength: Cardinal;
    FContentData: PByte;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property ContentSize: Int64 read FContentSize;
    property Descriptors: TDescriptorList read FDescriptors;
    property ServiceContextList: TServiceContextList read FServiceContextList;
    property MessageBodyLength: Cardinal read FMessageBodyLength;
    property ContentLength: Cardinal read FContentLength;
    property ContentData: PByte read FContentData;
  end;

  // ---------------------------------------------------------------------------

  TBaseProfileBody = class
  protected
    FType: TProfileType;
    FProfileIdTag: Cardinal;
    FProfileDataLength: Integer;
    FTotalLength: Integer;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte); overload; virtual;
    destructor Destroy; override;

    class function CreateProfileBody(ABuffer: PByte): TBaseProfileBody;

    procedure Clear; virtual;
    function ParseBuffer(ABuffer: PByte): Boolean; virtual;

    property ProfileType: TProfileType read FType;
    property ProfileIdTag: Cardinal read FProfileIdTag;
    property ProfileDataLength: Integer read FProfileDataLength;
    property TotalLength: Integer read FTotalLength;
  end;

  TBIOPProfileBody = class(TBaseProfileBody)
  protected
    FCarouselID: Cardinal;
    FTransactionID: Cardinal;
    FModuleID: Word;
    FObjectKeyLength: Integer;
    FObjectKeyData: PByte;
  public
    constructor Create; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property ObjectKeyLength: Integer read FObjectKeyLength;
    property ObjectKeyData: PByte read FObjectKeyData;
    property CarouselID: Cardinal read FCarouselID;
    property TransactionID: Cardinal read FTransactionID;
    property ModuleID: Word read FModuleID;
  end;

  TProfileBodyList = class(TList)
  private
    function Get(Index: Integer): TBaseProfileBody;
    procedure Put(Index: Integer; Item: TBaseProfileBody);
  public
    function Add(Item: TBaseProfileBody): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TBaseProfileBody read Get write Put; default;
  end;

  TBinding = class
  private
    FProfiles: TProfileBodyList;
    FID: PChar;
    FKind: PChar;
    FTypeID: PChar;
    FTotalLength: Integer;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property ID: PChar read FID;
    property Kind: PChar read FKind;
    property TypeID: PChar read FTypeID;
    property TotalLength: Integer read FTotalLength;

    property ProfilesList: TProfileBodyList read FProfiles;
  end;

  TBindingsList = class(TList)
  private
    function Get(Index: Integer): TBinding;
    procedure Put(Index: Integer; Item: TBinding);
  public
    function Add(Item: TBinding): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TBinding read Get write Put; default;
  end;

  TBIOPDirectoryMessage = class(TBIOPBaseMessage)
  private
    FObjectInfoData: PByte;
    FServiceContextList: TServiceContextList;
    FMessageBodyLength: Cardinal;
    FBindingsList: TBindingsList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;

    property ObjectInfoData: PByte read FObjectInfoData;
    property ServiceContextList: TServiceContextList read FServiceContextList;
    property MessageBodyLength: Cardinal read FMessageBodyLength;
    property Bindings: TBindingsList read FBindingsList;
  end;

  // ---------------------------------------------------------------------------

  TBIOPServiceGatewayMessage = class(TBIOPDirectoryMessage)
  public
    constructor Create; override;
  end;

  // ---------------------------------------------------------------------------

  TBIOPServiceGatewayInfoMessage = class(TBIOPMessage)
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte): Boolean; override;
  end;

implementation

uses
  MPEGUtils;

(*** TBIOPModuleInfo **********************************************************)

constructor TBIOPModuleInfo.Create;
begin
  inherited Create;
  FTapsList := TList.Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TBIOPModuleInfo.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TBIOPModuleInfo.Destroy;
begin
  Clear;
  FreeAndNil(FTapsList);
  FreeAndNil(FDescriptors);
  inherited Destroy;
end;

function TBIOPModuleInfo.GetTap(AIndex: Integer): TModuleInfoTap;
begin
  Result.ID := 0;
  Result.Use := 0;
  Result.AssociationTag := 0;
  Result.SelectorLength := 0;
  if (AIndex >= 0) and (AIndex < FTapsList.Count)
    then Result := PModuleInfoTap(FTapsList[AIndex])^;
end;

function TBIOPModuleInfo.GetTapCount: Integer;
begin
  Result := FTapsList.Count;
end;

procedure TBIOPModuleInfo.Clear;
begin
  FModuleTimeOut := 0;
  FBlockTimeOut := 0;
  FMinBlockTime := 0;
  while (FTapsList.Count > 0) do
  begin
    Dispose(PModuleInfoTap(FTapsList[0]));
    FTapsList.Delete(0);
  end;
  FDescriptors.Clear;
end;

function TBIOPModuleInfo.ParseBuffer(ABuffer: PByte): Boolean;
var
  taps_count: integer;
  UserInfoLength: Integer;
  tap: PModuleInfoTap;
  desc: TBaseDescriptor;
begin
  Clear;
  Result := True;

  FModuleTimeOut := GetLong(ABuffer);
  inc(ABuffer, 4);

  FBlockTimeOut := GetLong(ABuffer);
  inc(ABuffer, 4);

  FMinBlockTime := GetLong(ABuffer);
  inc(ABuffer, 4);

  taps_count := ABuffer^;
  inc(ABuffer);

  while (taps_count > 0) do
  begin
    new(tap);

    tap.ID := GetWord(ABuffer);
    inc(ABuffer, 2);

    tap.Use := GetWord(ABuffer);
    inc(ABuffer, 2);

    tap.AssociationTag := GetWord(ABuffer);
    inc(ABuffer, 2);

    tap.SelectorLength := ABuffer^;
    inc(ABuffer);

    FTapsList.Add(tap);
    dec(taps_count);
  end;

  UserInfoLength := Abuffer^;
  inc(ABuffer);

  while UserInfoLength > 0 do
  begin
    case ABuffer^ of
      DVB_DESCRIPTOR_TAG_COMPRESSED_MODULE: desc := TDVBCompressedModuleDescriptor.Create(ABuffer);

      MHP_DESCRIPTOR_TAG_LABEL:             desc := TMHPLabelDescriptor.Create(ABuffer);
      MHP_DESCRIPTOR_TAG_CACHING_PRIORITY:  desc := TMHPCachingPriorityDescriptor.Create(ABuffer);
      MHP_DESCRIPTOR_TAG_CONTENT_TYPE:      desc := TMHPContentTypeDescriptor.Create(ABuffer);
      else desc := TBaseDescriptor.Create(ABuffer);
    end;
    inc(ABuffer, desc.TotalLength);
    dec(UserInfoLength, desc.TotalLength);
    FDescriptors.Add(desc);
  end;
end;

(*** TBIOPBaseMessage *********************************************************)

class function TBIOPBaseMessage.CreateBIOPMessage(ABuffer: PByte): TBIOPBaseMessage;
var
  buf: PByte;
begin
  buf := ABuffer;
  Result := nil;
  if GetLong(ABuffer) <> BIOP_MAGIC
    then Exit;

  inc(ABuffer, 12);
  inc(ABuffer, ABuffer^ + 1 + 4);

  case GetLong(ABuffer) of
    BIOP_FILE: Result := TBIOPFileMessage.Create(buf);
    BIOP_DIR:  Result := TBIOPDirectoryMessage.Create(buf);
    BIOP_SRG:  Result := TBIOPServiceGatewayMessage.Create(buf);
    else       Result := TBIOPBaseMessage.Create(buf);
  end;
end;

constructor TBIOPBaseMessage.Create;
begin
  inherited Create;
  FType := mtBase;
  Clear;
end;

constructor TBIOPBaseMessage.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TBIOPBaseMessage.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBIOPBaseMessage.Clear;
begin
  FMagic := 0;
  FVersionMajor := 0;
  FVersionMinor := 0;
  FByteOrder := 0;
  FMessageType := 0;
  FMessageSize := 0;
  FObjectKeyLength := 0;
  FObjectKindLength := 0;
  FObjectKindData := 0;
  FObjectInfoLength := 0;
  FTotalLength := 0;
  if Assigned(FObjectKeyData) then
  begin
    FreeMem(FObjectKeyData);
    FObjectKeyData := nil;
  end;
end;

function TBIOPBaseMessage.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Result := False;
  Clear;

  FMagic := GetLong(ABuffer);
  inc(ABuffer, 4);

  if FMagic <> BIOP_MAGIC
    then Exit;

  FVersionMajor := ABuffer^;
  inc(ABuffer);

  FVersionMinor := ABuffer^;
  inc(ABuffer);

  FByteOrder := ABuffer^;
  inc(ABuffer);

  FMessageType := ABuffer^;
  inc(ABuffer);

  FMessageSize := GetLong(ABuffer);
  inc(ABuffer, 4);

  FTotalLength := FMessageSize + 12;

  FObjectKeyLength := ABuffer^;
  inc(ABuffer);

  if FObjectKeyLength > 0 then
  begin
    FObjectKeyData := AllocMem(FObjectKeyLength);
    Move(ABuffer^, FObjectKeyData^, FObjectKeyLength);
    inc(ABuffer, FObjectKeyLength);
  end;

  FObjectKindLength := GetLong(ABuffer);
  inc(ABuffer, 4);

  FObjectKindData := GetLong(ABuffer);
  inc(ABuffer, 4);

  FObjectInfoLength := GetWord(ABuffer);

  Result := True;
end;

(*** TServiceContext **********************************************************)

constructor TServiceContext.Create;
begin
  inherited Create;
  Clear;
end;

constructor TServiceContext.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TServiceContext.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TServiceContext.Clear;
begin
  FContextID := 0;
  FContextDataLength := 0;
  if Assigned(FContextData) then
  begin
    FreeMem(FContextData);
    FContextData := nil;
  end;
  FTotalLength := 0;
end;

function TServiceContext.ParseBuffer(ABuffer: PByte): Boolean;
begin
  FContextID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FContextDataLength := GetWORD(ABuffer);
  inc(ABuffer, 2);
  FTotalLength := FContextDataLength + 6;

  if FContextDataLength > 0 then
  begin
    FContextData := AllocMem(FContextDataLength);
    Move(ABuffer^, FContextData^, FContextDataLength);
  end;

  Result := True;
end;

(*** TServiceContextList ******************************************************)

function TServiceContextList.Get(Index: Integer): TServiceContext;
begin
  Result := inherited Get(Index);
end;

procedure TServiceContextList.Put(Index: Integer; Item: TServiceContext);
begin
  inherited Put(Index, Item);
end;

function TServiceContextList.Add(Item: TServiceContext): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TServiceContextList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TServiceContext(Items[i]).Free;
  inherited Clear;
end;

procedure TServiceContextList.Delete(Index: Integer);
begin
  TServiceContext(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TBIOPFileMessage *********************************************************)

constructor TBIOPFileMessage.Create;
begin
  inherited Create;
  FType := mtFile;
  FDescriptors := TDescriptorList.Create;
  FServiceContextList := TServiceContextList.Create;
end;

destructor TBIOPFileMessage.Destroy;
begin
  FreeAndNil(FDescriptors);
  FreeAndNil(FServiceContextList);
  inherited Destroy;
end;

procedure TBIOPFileMessage.Clear;
begin
  inherited Clear;

  FContentSize := 0;
  FMessageBodyLength := 0;
  FContentLength := 0;
  if Assigned(FContentData) then
  begin
    FreeMem(FContentData);
    FContentData := nil;
  end;
  if Assigned(FDescriptors)
    then FDescriptors.Clear;
  if Assigned(FServiceContextList)
    then FServiceContextList.Clear;
end;

function TBIOPFileMessage.ParseBuffer(ABuffer: PByte): Boolean;
var
  len: Integer;
  descriptor: TBaseDescriptor;
  sc: TServiceContext;
  serviceContextList_count: Integer;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;
  inc(ABuffer, 23 + FObjectKeyLength);

  len := FObjectInfoLength;
  if len > 0 then
  begin
    FContentSize := GetLongLong(ABuffer);
    inc(ABuffer, 8);
    dec(len, 8);
    while (len > 0) do
    begin
      descriptor := TMHPBaseDescriptor.CreateDescriptor(ABuffer);
      FDescriptors.Add(descriptor);
      dec(len, descriptor.TotalLength);
      inc(ABuffer, descriptor.TotalLength);
    end;
  end;

  serviceContextList_count := ABuffer^;
  inc(ABuffer);

  while (serviceContextList_count > 0) do
  begin
    sc := TServiceContext.Create(ABuffer);
    inc(ABuffer, sc.TotalLength);
    dec(serviceContextList_count, sc.TotalLength);
    FServiceContextList.Add(sc);
  end;

  FMessageBodyLength := GetLong(ABuffer);
  inc(ABuffer, 4);

  FContentLength := GetLong(ABuffer);
  inc(ABuffer, 4);

  if FContentLength > 0 then
  begin
    FContentData := AllocMem(FContentLength);
    Move(ABuffer^, FContentData^, FContentLength);
  end;

  Result := True;
end;

(*** TBaseProfileBody *********************************************************)

constructor TBaseProfileBody.Create;
begin
  inherited Create;
  Clear;
  FType := ptBase;
end;

constructor TBaseProfileBody.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TBaseProfileBody.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function TBaseProfileBody.CreateProfileBody(ABuffer: PByte): TBaseProfileBody;
begin
  case GetLong(ABuffer) of
    TAG_BIOP:         Result := TBIOPProfileBody.Create(ABuffer);
    TAG_LITE_OPTIONS: Result := TBaseProfileBody.Create(ABuffer);
    else              Result := TBaseProfileBody.Create(ABuffer);
  end;
end;

procedure TBaseProfileBody.Clear;
begin
  FProfileIdTag := 0;
  FProfileDataLength := 0;
  FTotalLength := 0;
end;

function TBaseProfileBody.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Clear;

  FProfileIdTag := GetLong(ABuffer);
  inc(ABuffer, 4);
  FProfileDataLength := GetLong(ABuffer);
  FTotalLength := FProfileDataLength + 8;

//  OutputDebugString(pchar(inttostr(FTotalLength) + ' - ' + inttostr(FProfileDataLength) + ' - ' + inttohex(FProfileIdTag, 8)));

  Result := True;
end;

(*** TBIOPProfileBody *********************************************************)

constructor TBIOPProfileBody.Create;
begin
  inherited Create;
  Clear;
  FType := ptBIOPProfileBody;
end;

procedure TBIOPProfileBody.Clear;
begin
  inherited Clear;

  if Assigned(FObjectKeyData) then
  begin
    FreeMem(FObjectKeyData);
    FObjectKeyData := nil;
  end;
  FObjectKeyLength := 0;
  FCarouselID := 0;
  FTransactionID := 0;
  FModuleID := 0;
end;

function TBIOPProfileBody.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Result := inherited ParseBuffer(ABuffer);

  if not Result
    then Exit;

  inc(ABuffer, 4); // profileId_tag
  inc(ABuffer, 4); // profile_data_length
  inc(ABuffer); // profile_data_byte_order
  inc(ABuffer); // lite_component_count
  inc(ABuffer, 4); // componentId_tag
  inc(ABuffer); // component_data_length

  FCarouselID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FModuleID := GetWord(ABuffer);
  inc(ABuffer, 2);

  inc(ABuffer); // version_major
  inc(ABuffer); // version_minor

  FObjectKeyLength := ABuffer^;
  inc(ABuffer);

  if FObjectKeyLength > 0 then
  begin
    FObjectKeyData := AllocMem(FObjectKeyLength);
    Move(ABuffer^, FObjectKeyData^, FObjectKeyLength);
    inc(ABuffer, FObjectKeyLength);
//    OutputDebugString(pchar(GetHexString(FObjectKeyData, FObjectKeyLength)));
  end;

  inc(ABuffer, 4); // componentId_tag
  inc(ABuffer); // component_data_length
  inc(ABuffer); // taps_count
  inc(ABuffer, 2); // id
  inc(ABuffer, 2); // use
  inc(ABuffer, 2); // assoc_tag
  inc(ABuffer); // selector_length
  inc(ABuffer, 2); // selector_type
  FTransactionID := GetLong(ABuffer);
//  inc(ABuffer, 4); // transaction_id
//  inc(ABuffer, 4); // timeout
end;

(*** TProfileBodyList *********************************************************)

function TProfileBodyList.Get(Index: Integer): TBaseProfileBody;
begin
  Result := inherited Get(Index);
end;

procedure TProfileBodyList.Put(Index: Integer; Item: TBaseProfileBody);
begin
  inherited Put(Index, Item);
end;

function TProfileBodyList.Add(Item: TBaseProfileBody): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TProfileBodyList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TBaseProfileBody(Items[i]).Free;
  inherited Clear;
end;

procedure TProfileBodyList.Delete(Index: Integer);
begin
  TBaseProfileBody(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TBinding *****************************************************************)

constructor TBinding.Create;
begin
  inherited Create;
  FProfiles := TProfileBodyList.Create;
  Clear;
end;

constructor TBinding.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TBinding.Destroy;
begin
  Clear;
  FProfiles.Free;
  inherited Destroy;
end;

procedure TBinding.Clear;
begin
  if Assigned(FID) then
  begin
    FreeMem(FID);
    FID := nil;
  end;
  if Assigned(FKind) then
  begin
    FreeMem(FKind);
    FKind := nil;
  end;
  if Assigned(FTypeID) then
  begin
    FreeMem(FTypeID);
    FTypeID := nil;
  end;
  FProfiles.Clear;
  FTotalLength := 0;
end;

function TBinding.ParseBuffer(ABuffer: PByte): Boolean;
var
  id_length, k: Integer;
  kind_length: Integer;
  type_id_length: Cardinal;
  buf: PByte;
  object_info_length: Integer;
  tagged_profiles_count: Integer;
  profile: TBaseProfileBody;
begin
  Clear;
  Result := True;

  buf := ABuffer;

  inc(ABuffer); // name_components_count always == 1
  id_length := ABuffer^;
  inc(ABuffer);
  if id_length > 0 then
  begin
    FID := AllocMem(id_length);
    Move(ABuffer^, FID^, id_length);
    inc(ABuffer, id_length);
  end;

  kind_length := ABuffer^;
  inc(ABuffer);
  if kind_length > 0 then
  begin
    FKind := AllocMem(kind_length);
    Move(ABuffer^, FKind^, kind_length);
//    OutputDebugString(PChar('DVB: ' + FKind));
    inc(ABuffer, kind_length);
  end;

  inc(ABuffer); // BindingType

  type_id_length := GetLong(ABuffer);
  inc(ABuffer, 4);
  if type_id_length > 0 then
  begin
    FTypeID := AllocMem(type_id_length);
    Move(ABuffer^, FTypeID^, type_id_length);
    inc(ABuffer, type_id_length);
  end;

  // tr101202.v1.2.1.pdf
  k := (type_id_length mod 4);
  if (k > 0) then
  begin
    inc(ABuffer, 4 - k);
  end;

  // Private Data of Download Server Initiate starts at position 0x8
  tagged_profiles_count := GetLong(ABuffer);
  inc(ABuffer, 4);

  for k := 0 to tagged_profiles_count -1 do
  begin
    profile := TBaseProfileBody.CreateProfileBody(ABuffer);
    inc(ABuffer, profile.TotalLength);
    FProfiles.Add(profile);
  end;

  object_info_length := GetWord(ABuffer);
  inc(ABuffer, 2);

  // Tested and not used in any Binding so far.
  if object_info_length > 0 then
  begin
    if UpperCase(String(FKind)) = String('FIL') then
    begin
      inc(ABuffer, 8); // content_size
      dec(object_info_length, 8);
      if object_info_length > 0
        then inc(ABuffer, object_info_length);
    end else
    begin
      if object_info_length > 0
        then inc(ABuffer, object_info_length);
    end;
  end;

  FTotalLength := Cardinal(ABuffer) - Cardinal(buf);
end;

(*** TBindingsList ************************************************************)

function TBindingsList.Get(Index: Integer): TBinding;
begin
  Result := inherited Get(Index);
end;

procedure TBindingsList.Put(Index: Integer; Item: TBinding);
begin
  inherited Put(Index, Item);
end;

function TBindingsList.Add(Item: TBinding): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TBindingsList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TBinding(Items[i]).Free;
  inherited Clear;
end;

procedure TBindingsList.Delete(Index: Integer);
begin
  TBinding(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TBIOPDirectoryMessage ****************************************************)

constructor TBIOPDirectoryMessage.Create;
begin
  inherited Create;
  FServiceContextList := TServiceContextList.Create;
  FBindingsList := TBindingsList.Create;
  FType := mtDirectory;
end;

destructor TBIOPDirectoryMessage.Destroy;
begin
  FreeAndNil(FServiceContextList);
  FreeAndNil(FBindingsList);
  inherited Destroy;
end;

procedure TBIOPDirectoryMessage.Clear;
begin
  inherited Clear;

  if Assigned(FObjectInfoData) then
  begin
    FreeMem(FObjectInfoData);
    FObjectInfoData := nil;
  end;
  if Assigned(FServiceContextList)
    then FServiceContextList.Clear;
  if Assigned(FBindingsList)
    then FBindingsList.Clear;
  FMessageBodyLength := 0;
end;

function TBIOPDirectoryMessage.ParseBuffer(ABuffer: PByte): Boolean;
var
  sc: TServiceContext;
  serviceContextList_count: Integer;
  bindings_count: Integer;
  i: Integer;
  binding: TBinding;
begin
  Result := False;
  if not inherited ParseBuffer(ABuffer)
    then Exit;
  inc(ABuffer, 23 + FObjectKeyLength);

  if FObjectInfoLength > 0 then
  begin
    FObjectInfoData := AllocMem(FObjectInfoLength);
    Move(ABuffer^, FObjectInfoData^, FObjectInfoLength);
    inc(ABuffer, FObjectInfoLength);
  end;

  serviceContextList_count := ABuffer^;
  inc(ABuffer);
  while (serviceContextList_count > 0) do
  begin
    sc := TServiceContext.Create(ABuffer);
    inc(ABuffer, sc.TotalLength);
    dec(serviceContextList_count, sc.TotalLength);
    FServiceContextList.Add(sc);
  end;

  FMessageBodyLength := GetLong(ABuffer);
  inc(ABuffer, 4);

  bindings_count := GetWord(ABuffer);
  inc(ABuffer, 2);

  for i := 0 to bindings_count -1 do
  begin
    binding := TBinding.Create(ABuffer);
    inc(ABuffer, binding.TotalLength);
    FBindingsList.Add(binding)
  end;

  Result := True;
end;

(*** TBIOPServiceGatewayMessage ***********************************************)

constructor TBIOPServiceGatewayMessage.Create;
begin
  inherited Create;
  FType := mtServiceGateway;
end;

(*** TBIOPServiceGatewayInfoMessage *******************************************)


constructor TBIOPServiceGatewayInfoMessage.Create;
begin
  inherited Create;
  FType := mtServiceGatewayInfo;
end;

destructor TBIOPServiceGatewayInfoMessage.Destroy;
begin
  inherited Destroy;
end;

procedure TBIOPServiceGatewayInfoMessage.Clear;
begin
  inherited Clear;
end;

function TBIOPServiceGatewayInfoMessage.ParseBuffer(ABuffer: PByte): Boolean;
begin
  Result := False;
end;

end.
