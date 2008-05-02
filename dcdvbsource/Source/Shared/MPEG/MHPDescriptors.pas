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

unit MHPDescriptors;

{$I Compiler.inc}

interface

uses
  SysUtils, Classes, MPEGConst, ISO639LanguageCode, MPEGDescriptors, MHPConst;

type
  TMHPBaseDescriptor = class(TBaseDescriptor)
  public
    constructor Create; override;
    class function CreateDescriptor(ABuffer: PByte): TBaseDescriptor; override;
  end;

  // ---------------------------------------------------------------------------
  // Used in PMT

  TMHPDataBroadcastIDDescriptor = class(TMHPBaseDescriptor)
  protected
    FDataBroadcastID: Word;
    FIDSpecificDataLength: Integer;
    FIDSpecificData: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property DataBroadcastID: Word read FDataBroadcastID;
    property IDSpecificDataLength: Integer read FIDSpecificDataLength;
    property IDSpecificData: PByte read FIDSpecificData;
  end;

  // ---------------------------------------------------------------------------

  TMHPApplicationSignallingDescriptor = class(TMHPBaseDescriptor)
  protected
    function GetItem(Index: Integer): TMHPApplicationSignallingDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Items[Index: Integer]: TMHPApplicationSignallingDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------
  // Used in SDT

  TMHPServiceIdentifierDescriptor = class(TMHPBaseDescriptor)
  protected
    FServiceIdentifier: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ServiceIdentifier: String read FServiceIdentifier;
  end;

  // ---------------------------------------------------------------------------
  // Used in DownloadInfoIndication

  TMHPLabelDescriptor = class(TMHPBaseDescriptor)
  protected
    FLabel: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Label_: String read FLabel;
  end;

  // ---------------------------------------------------------------------------

  TMHPCachingPriorityDescriptor = class(TMHPBaseDescriptor)
  protected
    FPriorityValue: Byte;
    FTransparencyLevel: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property PriorityValue: Byte read FPriorityValue;
    property TransparencyLevel: Byte read FTransparencyLevel;
  end;

  // ---------------------------------------------------------------------------
  // Used in BIOP ObjectInfo

  TMHPContentTypeDescriptor = class(TMHPBaseDescriptor)
  protected
    FContentType: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ContentType: String read FContentType;
  end;

  // ---------------------------------------------------------------------------
  // Used in AIT

  TMHPApplicationNameDescriptor = class(TMHPBaseDescriptor)
  protected
    function GetName(Index: Integer): TMHPApplicationName;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Names[Index: Integer]: TMHPApplicationName read GetName;
  end;

  // ---------------------------------------------------------------------------

  TMHPApplicationDescriptor = class(TMHPBaseDescriptor)
  protected
    FServiceBoundFlag: Byte;
    FVisibility: Byte;
    FApplicationPriority: Byte;
    FTransportProtocolLabelLength: Integer;
    FTransportProtocolLabel: PByte;
    function GetProfile(Index: Integer): TMHPApplicationProfile;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ServiceBoundFlag: Byte read FServiceBoundFlag;
    property Visibility: Byte read FVisibility;
    property ApplicationPriority: Byte read FApplicationPriority;
    property TransportProtocolLabelLength: Integer read FTransportProtocolLabelLength;
    property TransportProtocolLabel: PByte read FTransportProtocolLabel;

    property Profiles[Index: Integer]: TMHPApplicationProfile read GetProfile;
  end;

  // ---------------------------------------------------------------------------

  TMHPTransportProtocolDescriptor = class(TMHPBaseDescriptor)
  protected
    FProtocolID: Word;
    FTransportProtocolLabel: Byte;
    FSelectorLength: Integer;
    FSelector: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ProtocolID: Word read FProtocolID;
    property TransportProtocolLabel: Byte read FTransportProtocolLabel;
    property SelectorLength: Integer read FSelectorLength;
    property Selector: PByte read FSelector;
  end;

  // ---------------------------------------------------------------------------

  TMHPDVBJApplicationDescriptor = class(TMHPBaseDescriptor)
  protected
    function GetParameter(Index: Integer): TDVBJParameter;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Parameters[Index: Integer]: TDVBJParameter read GetParameter;
  end;

  // ---------------------------------------------------------------------------

  TMHPDVBJApplicationLocationDescriptor = class(TMHPBaseDescriptor)
  protected
    FBaseDirectory: String;
    FClassPathExtension: String;
    FInitialClass: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property BaseDirectory: String read FBaseDirectory;
    property ClassPathExtension: String read FClassPathExtension;
    property InitialClass: String read FInitialClass;
  end;

  // ---------------------------------------------------------------------------

  TMHPExternalApplicationAuthorisationDescriptor = class(TMHPBaseDescriptor)
  protected
    function GetAuthorisation(Index: Integer): TMHPApplicationAuthorisation;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Authorisations[Index: Integer]: TMHPApplicationAuthorisation read GetAuthorisation;
  end;

implementation

uses
  MPEGUtils, MHPUtils;

(*** TMHPBaseDescriptor *******************************************************)

constructor TMHPBaseDescriptor.Create;
begin
  inherited Create;
  FDescriptorType := dtMHP;
end;

class function TMHPBaseDescriptor.CreateDescriptor(ABuffer: PByte): TBaseDescriptor;
begin
  case (ABuffer^) of
    MHP_DESCRIPTOR_TAG_APPLICATION:                         Result := TMHPApplicationDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_APPLICATION_NAME:                    Result := TMHPApplicationNameDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_TRANSPORT_PROTOCOL:                  Result := TMHPTransportProtocolDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION:                    Result := TMHPDVBJApplicationDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION_LOCATION:           Result := TMHPDVBJApplicationLocationDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_EXTERNAL_APPLICATION_AUTHORISATION:  Result := TMHPExternalApplicationAuthorisationDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_APPLICATION_SIGNALLING:              Result := TMHPApplicationSignallingDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_LABEL:                               Result := TMHPLabelDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_CACHING_PRIORITY:                    Result := TMHPLabelDescriptor.Create(ABuffer);
    MHP_DESCRIPTOR_TAG_CONTENT_TYPE:                        Result := TMHPContentTypeDescriptor.Create(ABuffer);
    else                                                    Result := TMHPBaseDescriptor.Create(ABuffer);
  end;
end;

(*** TMHPApplicationSignallingDescriptor **************************************)

function TMHPApplicationSignallingDescriptor.GetItem(Index: Integer): TMHPApplicationSignallingDescriptorItem;
begin
  Result.ApplicationType := 0;
  Result.AITVersionNumber := 0;
  if (Index >= 0) and (Index < FList.Count)
    then Result := PMHPApplicationSignallingDescriptorItem(FList[Index])^;
end;

procedure TMHPApplicationSignallingDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PMHPApplicationSignallingDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMHPApplicationSignallingDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  item: PMHPApplicationSignallingDescriptorItem;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(item);
    item.ApplicationType := GetWord(ABuffer);
    inc(ABuffer, 2);
    item.AITVersionNumber := GetByteBits(ABuffer, 3, 5);
    FList.Add(item);
    inc(ABuffer, 1);
    dec(count, 3);
  end;
end;

(*** TMHPApplicationNameDescriptor ********************************************)

function TMHPApplicationNameDescriptor.GetName(Index: Integer): TMHPApplicationName;
begin
  Result.Language := '   ';
  Result.Name := '';
  if (Index >= 0) and (Index < FList.Count)
    then Result := PMHPApplicationName(FList[Index])^;
end;

procedure TMHPApplicationNameDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PMHPApplicationName(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMHPApplicationNameDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  item: PMHPApplicationName;
  application_name_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(item);
    item.Language := PISO6392LanguageCode(ABuffer)^;
    inc(ABuffer, 3);
    dec(count, 3);

    application_name_length := ABuffer^;
    inc(ABuffer);
    dec(count);

    if application_name_length > 0 then
    begin
      SetLength(item.Name, application_name_length);
      Move(ABuffer^, item.Name[1], application_name_length);
      inc(ABuffer, application_name_length);
      dec(count, application_name_length);
    end;

    FList.Add(item);
  end;
end;

(*** TMHPApplicationDescriptor ************************************************)

function TMHPApplicationDescriptor.GetProfile(Index: Integer): TMHPApplicationProfile;
begin
  Result.ApplicationProfile := 0;
  Result.MajorVersion := 0;
  Result.MinorVersion := 0;
  Result.MicroVersion := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PMHPApplicationProfile(FList[Index])^;
end;

procedure TMHPApplicationDescriptor.Clear;
begin
  inherited Clear;

  FServiceBoundFlag := 0;
  FVisibility := 0;
  FApplicationPriority := 0;
  FTransportProtocolLabelLength := 0;
  if Assigned(FTransportProtocolLabel) then
  begin
    FreeMem(FTransportProtocolLabel);
    FTransportProtocolLabel := nil;
  end;

  while (FList.Count > 0) do
  begin
    Dispose(PMHPApplicationProfile(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMHPApplicationDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  item: PMHPApplicationProfile;
  application_profiles_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;

  application_profiles_length := ABuffer^;
  inc(ABuffer);
  dec(count);

  while (application_profiles_length > 0) do
  begin
    new (item);
    item.ApplicationProfile := GetWord(ABuffer);
    inc(ABuffer, 2);
    item.MajorVersion := ABuffer^;
    inc(ABuffer);
    item.MinorVersion := ABuffer^;
    inc(ABuffer);
    item.MicroVersion := ABuffer^;
    inc(ABuffer);

    dec(application_profiles_length, 5);
    dec(count, 5);

    FList.Add(item);
  end;

  FServiceBoundFlag := GetByteBits(ABuffer, 0, 1);
  FVisibility := GetByteBits(ABuffer, 1, 2);
  inc(ABuffer);
  dec(count);

  FApplicationPriority := ABuffer^;
  inc(ABuffer);
  dec(count);

  FTransportProtocolLabelLength := count;

  if FTransportProtocolLabelLength > 0 then
  begin
    FTransportProtocolLabel := AllocMem(FTransportProtocolLabelLength);
    Move(ABuffer^, FTransportProtocolLabel^, FTransportProtocolLabelLength);
  end;
end;

(*** TMHPTransportProtocolDescriptor ******************************************)

procedure TMHPTransportProtocolDescriptor.Clear;
begin
  inherited Clear;

  FProtocolID := 0;
  FTransportProtocolLabel := 0;
  FSelectorLength := 0;
  if Assigned(FSelector) then
  begin
    FreeMem(FSelector);
    FSelector := nil;
  end;
end;

procedure TMHPTransportProtocolDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FProtocolID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FTransportProtocolLabel := ABuffer^;
  inc(ABuffer);

  FSelectorLength := FDescriptorLength - 3;

  if FSelectorLength > 0 then
  begin
    FSelector := AllocMem(FSelectorLength);
    Move(ABuffer^, FSelector^, FSelectorLength);
  end;
end;

(*** TMHPDVBJApplicationDescriptor ********************************************)

function TMHPDVBJApplicationDescriptor.GetParameter(Index: Integer): TDVBJParameter;
begin
  Result.Parameter := '';
  if (Index >= 0) and (Index < FList.Count)
    then Result := PDVBJParameter(FList[Index])^;
end;

procedure TMHPDVBJApplicationDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PDVBJParameter(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMHPDVBJApplicationDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  item: PDVBJParameter;
  parameter_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(item);
    parameter_length := ABuffer^;
    inc(ABuffer);
    dec(count);

    if parameter_length > 0 then
    begin
      SetLength(item.Parameter, parameter_length);
      Move(ABuffer^, item.Parameter[1], parameter_length);
      inc(ABuffer, parameter_length);
      dec(count, parameter_length);
    end;

    FList.Add(item);
  end;
end;

(*** TMHPDVBJApplicationLocationDescriptor ************************************)

procedure TMHPDVBJApplicationLocationDescriptor.Clear;
begin
  inherited Clear;

  FBaseDirectory := '';
  FClassPathExtension := '';
  FInitialClass := '';
end;

procedure TMHPDVBJApplicationLocationDescriptor.ParseBuffer(ABuffer: PByte);
var
  len: Integer;
  base_directory_length: Integer;
  classpath_extension_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  len := FDescriptorLength;

  base_directory_length := ABuffer^;
  inc(ABuffer);
  dec(len);
  if base_directory_length > 0 then
  begin
    SetLength(FBaseDirectory, base_directory_length);
    Move(ABuffer^, FBaseDirectory[1], base_directory_length);
    inc(ABuffer, base_directory_length);
    dec(len, base_directory_length);
  end;

  classpath_extension_length := ABuffer^;
  inc(ABuffer);
  dec(len);
  if classpath_extension_length > 0 then
  begin
    SetLength(FClassPathExtension, classpath_extension_length);
    Move(ABuffer^, FClassPathExtension[1], classpath_extension_length);
    inc(ABuffer, classpath_extension_length);
    dec(len, classpath_extension_length);
  end;

  if len > 0 then
  begin
    SetLength(FInitialClass, len);
    Move(ABuffer^, FInitialClass[1], len);
  end;
end;

(*** TMHPExternalApplicationAuthorisationDescriptor ***************************)

function TMHPExternalApplicationAuthorisationDescriptor.GetAuthorisation(Index: Integer): TMHPApplicationAuthorisation;
begin
  Result.ApplicationIndentifier.OrganisationID := 0;
  Result.ApplicationIndentifier.ApplicationID := 0;
  Result.ApplicationPriority := 0;
  if (Index >= 0) and (Index < FList.Count)
    then Result := PMHPApplicationAuthorisation(FList[Index])^;
end;

procedure TMHPExternalApplicationAuthorisationDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PMHPApplicationAuthorisation(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMHPExternalApplicationAuthorisationDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  item: PMHPApplicationAuthorisation;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(item);

    item.ApplicationIndentifier.OrganisationID := GetLong(ABuffer);
    inc(Abuffer, 4);
    dec(count, 4);

    item.ApplicationIndentifier.ApplicationID := GetWord(ABuffer);
    inc(Abuffer, 2);
    dec(count, 2);

    item.ApplicationPriority := ABuffer^;
    inc(Abuffer);
    dec(count);

    FList.Add(item);
  end;
end;

(*** TMHPLabelDescriptor ******************************************************)

procedure TMHPLabelDescriptor.Clear;
begin
  inherited Clear;

  FLabel := '';
end;

procedure TMHPLabelDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  if FDescriptorLength > 0 then
  begin
    SetLength(FLabel, FDescriptorLength);
    Move(ABuffer^, FLabel[1], FDescriptorLength);
  end;
end;

(*** TMHPDVBJApplicationLocationDescriptor ************************************)

procedure TMHPCachingPriorityDescriptor.Clear;
begin
  inherited Clear;

  FPriorityValue := 0;
  FTransparencyLevel := 0;
end;

procedure TMHPCachingPriorityDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FPriorityValue := ABuffer^;
  inc(ABuffer);

  FTransparencyLevel := ABuffer^;
end;

(*** TMHPContentTypeDescriptor ************************************************)

procedure TMHPContentTypeDescriptor.Clear;
begin
  inherited Clear;

  FContentType := '';
end;

procedure TMHPContentTypeDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  if FDescriptorLength > 0 then
  begin
    SetLength(FContentType, FDescriptorLength);
    Move(ABuffer^, FContentType[1], FDescriptorLength);
  end;
end;

(*** TMHPServiceIdentifierDescriptor ************************************************)

procedure TMHPServiceIdentifierDescriptor.Clear;
begin
  inherited Clear;

  FServiceIdentifier := '';
end;

procedure TMHPServiceIdentifierDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  if FDescriptorLength > 0 then
  begin
    SetLength(FServiceIdentifier, FDescriptorLength);
    Move(ABuffer^, FServiceIdentifier[1], FDescriptorLength);
  end;
end;

(*** TMHPTransportProtocolDescriptor ******************************************)

procedure TMHPDataBroadcastIDDescriptor.Clear;
begin
  inherited Clear;

  FDataBroadcastID := 0;
  FIDSpecificDataLength := 0;
  if Assigned(FIDSpecificData) then
  begin
    FreeMem(FIDSpecificData);
    FIDSpecificData := nil;
  end;
end;

procedure TMHPDataBroadcastIDDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FDataBroadcastID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FIDSpecificDataLength := FDescriptorLength - 2;

  if FIDSpecificDataLength > 0 then
  begin
    FIDSpecificData := AllocMem(FIDSpecificDataLength);
    Move(ABuffer^, FIDSpecificData^, FIDSpecificDataLength);
  end;
end;

end.
