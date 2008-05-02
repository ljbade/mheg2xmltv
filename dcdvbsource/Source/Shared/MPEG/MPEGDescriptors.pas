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

unit MPEGDescriptors;

{$I Compiler.inc}

interface

uses
  SysUtils, Classes, MPEGConst, ISO639LanguageCode;

type
  TBaseDescriptor       = class;
  TBaseDescriptorClass  = class of TBaseDescriptor;

  // ---------------------------------------------------------------------------

  TDescriptorList = class(TList)
  private
    function Get(Index: Integer): TBaseDescriptor;
    procedure Put(Index: Integer; Item: TBaseDescriptor);
  public
    function Add(Item: TBaseDescriptor): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    function GetDescriptor(AClass: TBaseDescriptorClass; out ADescriptor): Boolean;
    property Items[Index: Integer]: TBaseDescriptor read Get write Put; default;
  end;

  // ---------------------------------------------------------------------------

  TBaseDescriptor = class
  protected
    FList: TList;
    FTag: Byte;
    FDescriptorLength: Byte;
    FTotalLength: Integer;
    FDescriptorType: TDescriptorType;
    function GetCountItems: Integer;
  public
    constructor Create; overload; virtual;
    constructor Create(ABuffer: PByte); overload; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ParseBuffer(ABuffer: PByte); virtual;
    class function CreateDescriptor(ABuffer: PByte): TBaseDescriptor; virtual;

    property DescriptorType: TDescriptorType read FDescriptorType;
    property Tag: Byte read FTag;
    property DescriptorLength: Byte read FDescriptorLength;
    property TotalLength: Integer read FTotalLength;
    property CountItems: Integer read GetCountItems;
  end;

  // ---------------------------------------------------------------------------

  TVideoStreamDescriptor = class(TBaseDescriptor)
  protected
    FMultipleFrameRateFlag: Byte;
    FFrameRateCode: Byte;
    FMPEG1OnlyFlag: Byte;
    FConstrainedParameterFlag: Byte;
    FStillPictureFlag: Byte;
    FProfileAndLevelIndication: Byte;
    FChromaFormat: Byte;
    FFrameRateExtensionFlag: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property MultipleFrameRateFlag: Byte read FMultipleFrameRateFlag;
    property FrameRateCode: Byte read FFrameRateCode;
    property MPEG1OnlyFlag: Byte read FMPEG1OnlyFlag;
    property ConstrainedParameterFlag: Byte read FConstrainedParameterFlag;
    property StillPictureFlag: Byte read FStillPictureFlag;
    property ProfileAndLevelIndication: Byte read FProfileAndLevelIndication;
    property ChromaFormat: Byte read FChromaFormat;
    property FrameRateExtensionFlag: Byte read FFrameRateExtensionFlag;
  end;

  // ---------------------------------------------------------------------------

  TAudioStreamDescriptor = class(TBaseDescriptor)
  protected
    FFreeFormatFlag: Byte;
    FID: Byte;
    FLayer: Byte;
    FVariableRateAudioIndicator: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property FreeFormatFlag: Byte read FFreeFormatFlag;
    property ID: Byte read FID;
    property Layer: Byte read FLayer;
    property VariableRateAudioIndicator: Byte read FVariableRateAudioIndicator;
  end;

  // ---------------------------------------------------------------------------

  TNetworkNameDescriptor = class(TBaseDescriptor)
  protected
    FName: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Name: String read FName;
  end;

  // ---------------------------------------------------------------------------

  TShortEventDescriptor = class(TBaseDescriptor)
  protected
    FLanguage: TISO6392LanguageCode;
    FName: String;
    FDescription: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Language: TISO6392LanguageCode read FLanguage;
    property Name: String read FName;
    property Description: String read FDescription;
  end;

  // ---------------------------------------------------------------------------

  TServiceListDescriptor = class(TBaseDescriptor)
  protected
    function GetService(Index: Integer): TServiceDescriptorService;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Service[Index: Integer]: TServiceDescriptorService read GetService;
  end;

  // ---------------------------------------------------------------------------

  TTerrestrialDeliverySystemDescriptor = class(TBaseDescriptor)
  protected
    FCentreFrequency: Int64;
    FBandwidth: Byte;
    FPriority: Byte;
    FTimeSlicingIndicator: Byte;
    FMPEFECIndicator: Byte;
    FConstellation: TTerrestrialConstellation;
    FHierarchyInformation: TTerrestrialHierarchyInformation;
    FCodeRateHPStream: TTerrestrialCodeRate;
    FCodeRateLPStream: TTerrestrialCodeRate;
    FGuardInterval: TTerrestrialGuardInterval;
    FTransmissionMode: TTerrestrialTransmissionMode;
    FOtherFrequencyFlag: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CentreFrequency: Int64 read FCentreFrequency;
    property Bandwidth: Byte read FBandwidth;
    property Priority: Byte read FPriority;
    property TimeSlicingIndicator: Byte read FTimeSlicingIndicator;
    property MPEFECIndicator: Byte read FMPEFECIndicator;
    property Constellation: TTerrestrialConstellation read FConstellation;
    property HierarchyInformation: TTerrestrialHierarchyInformation read FHierarchyInformation;
    property CodeRateHPStream: TTerrestrialCodeRate read FCodeRateHPStream;
    property CodeRateLPStream: TTerrestrialCodeRate read FCodeRateLPStream;
    property GuardInterval: TTerrestrialGuardInterval read FGuardInterval;
    property TransmissionMode: TTerrestrialTransmissionMode read FTransmissionMode;
    property OtherFrequencyFlag: Byte read FOtherFrequencyFlag;
  end;

  // ---------------------------------------------------------------------------

  TSatelliteDeliverySystemDescriptor = class(TBaseDescriptor)
  protected
    FFrequency: Int64;
    FOrbitalPosition: Word;
    FWestEastFlag: Byte;
    FPolarization: TSatellitePolarization;
    FModulation: TSatelliteModulation;
    FSymbolRate: Int64;
    FFECInner: TFECInner;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Frequency: Int64 read FFrequency;
    property OrbitalPosition: Word read FOrbitalPosition;
    property WestEastFlag: Byte read FWestEastFlag;
    property Polarization: TSatellitePolarization read FPolarization;
    property Modulation: TSatelliteModulation read FModulation;
    property SymbolRate: Int64 read FSymbolRate;
    property FECInner: TFECInner read FFECInner;
  end;

  // ---------------------------------------------------------------------------

  TCableDeliverySystemDescriptor = class(TBaseDescriptor)
  protected
    FFrequency: Int64;
    FFECOuter: TFECOuter;
    FModulation: TCableModulation;
    FSymbolRate: Int64;
    FFECInner: TFECInner;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Frequency: Int64 read FFrequency;
    property FECOuter: TFECOuter read FFECOuter;
    property Modulation: TCableModulation read FModulation;
    property SymbolRate: Int64 read FSymbolRate;
    property FECInner: TFECInner read FFECInner;
  end;

  // ---------------------------------------------------------------------------

  TStreamIdentifierDescriptor = class(TBaseDescriptor)
  protected
    FComponentTag: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ComponentTag: Byte read FComponentTag;
  end;

  // ---------------------------------------------------------------------------

  TDataBroadcastIDDescriptor = class(TBaseDescriptor)
  protected
    FDataBroadcastID: Word;
    FIDSelectorLength: Byte;
    FIDSelector: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property DataBroadcastID: Word read FDataBroadcastID;
    property IDSelectorLength: Byte read FIDSelectorLength;
    property IDSelector: PByte read FIDSelector;
  end;

  // ---------------------------------------------------------------------------

  TAC3Descriptor = class(TBaseDescriptor)
  protected
    FComponentTypeFlag: Byte;
    FBSIDFlag: Byte;
    FMainIDFlag: Byte;
    FASVCFlag: Byte;
    FComponentType: Byte;
    FBSID: Byte;
    FMainID: Byte;
    FASVC: Byte;
    FAdditionalInfoLength: Integer;
    FAdditionalInfo: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ComponentTypeFlag: Byte read FComponentTypeFlag;
    property BSIDFlag: Byte read FBSIDFlag;
    property MainIDFlag: Byte read FMainIDFlag;
    property ASVCFlag: Byte read FASVCFlag;
    property ComponentType: Byte read FComponentType;
    property BSID: Byte read FBSID;
    property MainID: Byte read FMainID;
    property ASVC: Byte read FASVC;
    property AdditionalInfoLength: Integer read FAdditionalInfoLength;
    property AdditionalInfo: PByte read FAdditionalInfo;
  end;

  // ---------------------------------------------------------------------------

  TTeletextDescriptor = class(TBaseDescriptor)
  protected
    function GetPage(Index: Integer): TTeletextDescriptorPage;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Page[Index: Integer]: TTeletextDescriptorPage read GetPage;
  end;

  // ---------------------------------------------------------------------------

  TISO639LanguageDescriptor = class(TBaseDescriptor)
  protected
    function GetLanguage(Index: Integer): TISO639LanguageDescriptorLanguage;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Language[Index: Integer]: TISO639LanguageDescriptorLanguage read GetLanguage;
  end;

  // ---------------------------------------------------------------------------

  TSTDDescriptor = class(TBaseDescriptor)
  protected
    FLeakValidFlag: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property LeakValidFlag: Byte read FLeakValidFlag;
  end;

  // ---------------------------------------------------------------------------

  TApplicationSignallingDescriptor = class(TBaseDescriptor)
  protected
    function GetApplication(Index: Integer): TApplicationSignallingDescriptorApplication;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Application[Index: Integer]: TApplicationSignallingDescriptorApplication read GetApplication;
  end;

  // ---------------------------------------------------------------------------

  TCarouselIdentifierDescriptor = class(TBaseDescriptor)
  protected
    FCarouselID: Cardinal;
    FFormatID: Byte;
    FPrivateDataLength: Integer;
    FPrivateData: PByte;
    FModuleVersion: Byte;
    FModuleID: Word;
    FBlockSize: Word;
    FModuleSize: Cardinal;
    FCompressionMethod: Byte;
    FOriginalSize: Cardinal;
    FTimeOut: Byte;
    FObjectKeyLength: Byte;
    FObjectKey: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CarouselID: Cardinal read FCarouselID;
    property FormatID: Byte read FFormatID;
    property PrivateDataLength: Integer read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
    property ModuleVersion: Byte read FModuleVersion;
    property ModuleID: Word read FModuleID;
    property BlockSize: Word read FBlockSize;
    property ModuleSize: Cardinal read FModuleSize;
    property CompressionMethod: Byte read FCompressionMethod;
    property OriginalSize: Cardinal read FOriginalSize;
    property TimeOut: Byte read FTimeOut;
    property ObjectKeyLength: Byte read FObjectKeyLength;
    property ObjectKey: PByte read FObjectKey;
  end;

  // ---------------------------------------------------------------------------

  TRegistrationDescriptor = class(TBaseDescriptor)
  protected
    FFormatIdentifier: Cardinal;
    FAdditionalIdentificationInfoLength: Integer;
    FAdditionalIdentificationInfo: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property FormatIdentifier: Cardinal read FFormatIdentifier;
    property AdditionalIdentificationInfoLength: Integer read FAdditionalIdentificationInfoLength;
    property AdditionalIdentificationInfo: PByte read FAdditionalIdentificationInfo;
  end;

  // ---------------------------------------------------------------------------

  TVBIDataDescriptor = class(TBaseDescriptor)
  protected
    function GetLine(Index: Integer): TVBIDataDescriptorLine;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Line[Index: Integer]: TVBIDataDescriptorLine read GetLine;
  end;

  // ---------------------------------------------------------------------------

  TServiceDescriptor = class(TBaseDescriptor)
  protected
    FServiceType: Byte;
    FProviderName: String;
    FServiceName: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ServiceType: Byte read FServiceType;
    property ProviderName: String read FProviderName;
    property ServiceName: String read FServiceName;
  end;

  // ---------------------------------------------------------------------------

  TDataBroadcastDescriptor = class(TBaseDescriptor)
  protected
    FDataBroadcastID: Word;
    FComponentTag: Byte;
    FSelectorLength: Byte;
    FSelector: PByte;
    FISO639LanguageCode: TISO6392LanguageCode;
    FDescription: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property DataBroadcastID: Word read FDataBroadcastID;
    property ComponentTag: Byte read FComponentTag;
    property SelectorLength: Byte read FSelectorLength;
    property Selector: PByte read FSelector;
    property ISO639LanguageCode: TISO6392LanguageCode read FISO639LanguageCode;
    property Description: String read FDescription;
  end;

  // ---------------------------------------------------------------------------

  TLocalTimeOffsetDescriptor = class(TBaseDescriptor)
  protected
    function GetEntry(Index: Integer): TLocalTimeOffsetDescriptorEntry;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Entry[Index: Integer]: TLocalTimeOffsetDescriptorEntry read GetEntry;
  end;

  // ---------------------------------------------------------------------------

  TExtendedEventDescriptor = class(TBaseDescriptor)
  protected
    FDescriptorNumber: Byte;
    FLastDescriptorNumber: Byte;
    FLanguage: TISO6392LanguageCode;
    FText: String;
    function GetItem(Index: Integer): TExtendedEventDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property DescriptorNumber: Byte read FDescriptorNumber;
    property LastDescriptorNumber: Byte read FLastDescriptorNumber;
    property Language: TISO6392LanguageCode read FLanguage;
    property Text: String read FText;

    property Item[Index: Integer]: TExtendedEventDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TContentDescriptor = class(TBaseDescriptor)
  protected
    function GetItem(Index: Integer): TContentDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Item[Index: Integer]: TContentDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TComponentDescriptor = class(TBaseDescriptor)
  protected
    FStreamContent: Byte;
    FComponentType: Byte;
    FComponentTag: Byte;
    FISO639LanguageCode: TISO6392LanguageCode;
    FDescription: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property StreamContent: Byte read FStreamContent;
    property ComponentType: Byte read FComponentType;
    property ComponentTag: Byte read FComponentTag;
    property ISO639LanguageCode: TISO6392LanguageCode read FISO639LanguageCode;
    property Description: String read FDescription;
  end;

  // ---------------------------------------------------------------------------

  TParentalRatingDescriptor = class(TBaseDescriptor)
  protected
    function GetItem(Index: Integer): TParentalDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Item[Index: Integer]: TParentalDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TPrivateDataSpecifierDescriptor = class(TBaseDescriptor)
  protected
    FPrivateDataSpecifier: Cardinal;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property PrivateDataSpecifier: Cardinal read FPrivateDataSpecifier;
  end;

  // ---------------------------------------------------------------------------

  TLinkageDescriptor = class(TBaseDescriptor)
  protected
    FTransportStreamID: Word;
    FOriginalNetworkID: Word;
    FServiceID: Word;
    FLinkageType: Byte;
    FHandOverType: Byte;
    FOriginType: Byte;
    FNetworkID: Word;
    FInitialServiceID: Word;
    FPrivateDataLength: Integer;
    FPrivateData: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property TransportStreamID: Word read FTransportStreamID;
    property OriginalNetworkID: Word read FOriginalNetworkID;
    property ServiceID: Word read FServiceID;
    property LinkageType: Byte read FLinkageType;
    property HandOverType: Byte read FHandOverType;
    property OriginType: Byte read FOriginType;
    property NetworkID: Word read FNetworkID;
    property InitialServiceID: Word read FInitialServiceID;
    property PrivateDataLength: Integer read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
  end;

  // ---------------------------------------------------------------------------

  TPDCDescriptor = class(TBaseDescriptor)
  protected
    FProgrammeIdentificationLabel: TProgrammeIdentificationLabel;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ProgrammeIdentificationLabel: TProgrammeIdentificationLabel read FProgrammeIdentificationLabel;
  end;

  // ---------------------------------------------------------------------------

  TCADescriptor = class(TBaseDescriptor)
  protected
    FSystemID: Word;
    FPID: Word;
    FPrivateDataLength: Integer;
    FPrivateData: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property SystemID: Word read FSystemID;
    property PID: Word read FPID;
    property PrivateDataLength: Integer read FPrivateDataLength;
    property PrivateData: PByte read FPrivateData;
  end;

  // ---------------------------------------------------------------------------

  TCountryAvailibilityDescriptor = class(TBaseDescriptor)
  protected
    FCountryAvailibilityFlag: Byte;
    FCountryCode: TCountryCode;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CountryAvailibilityFlag: Byte read FCountryAvailibilityFlag;
    property CountryCode: TCountryCode read FCountryCode;
  end;

  // ---------------------------------------------------------------------------

  TCAIdentifierDescriptor = class(TBaseDescriptor)
  protected
    FCASystemIDLength: Integer;
    FCASystemID: PByte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CASystemIDLength: Integer read FCASystemIDLength;
    property CASystemID: PByte read FCASystemID;
  end;

  // ---------------------------------------------------------------------------

  TSystemClockDescriptor = class(TBaseDescriptor)
  protected
    FExternalClockReferenceIndicator: Byte;
    FClockAccuracyInteger: Byte;
    FClockAccuracyExponent: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ExternalClockReferenceIndicator: Byte read FExternalClockReferenceIndicator;
    property ClockAccuracyInteger: Byte read FClockAccuracyInteger;
    property ClockAccuracyExponent: Byte read FClockAccuracyExponent;
  end;

  // ---------------------------------------------------------------------------

  TSmoothingBufferDescriptor = class(TBaseDescriptor)
  protected
    FSBLeakRate: Cardinal;
    FSBSize: Cardinal;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property SBLeakRate: Cardinal read FSBLeakRate;
    property SBSize: Cardinal read FSBSize;
  end;

  // ---------------------------------------------------------------------------

  TDataStreamAlignementDescriptor = class(TBaseDescriptor)
  protected
    FAlignementType: Byte;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property AlignementType: Byte read FAlignementType;
  end;

  // ---------------------------------------------------------------------------

  TMultiplexBufferUtilizationDescriptor = class(TBaseDescriptor)
  protected
    FBoundValidFlag: Byte;
    FLTWOffsetLowerBound: Word;
    FLTWOffsetUpperBound: Word;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property BoundValidFlag: Byte read FBoundValidFlag;
    property LTWOffsetLowerBound: Word read FLTWOffsetLowerBound;
    property LTWOffsetUpperBound: Word read FLTWOffsetUpperBound;
  end;

  // ---------------------------------------------------------------------------

  TMaximumBitrateDescriptor = class(TBaseDescriptor)
  protected
    FMaximumBitrate: Cardinal;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property MaximumBitrate: Cardinal read FMaximumBitrate;
  end;

  // ---------------------------------------------------------------------------

  TMultilingualComponentDescriptor = class(TBaseDescriptor)
  protected
    FComponentTag: Byte;
    function GetItem(Index: Integer): TMultilingualComponentDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property ComponentTag: Byte read FComponentTag;
    property Item[Index: Integer]: TMultilingualComponentDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TFrequencyListDescriptor = class(TBaseDescriptor)
  protected
    FCodingType: Byte;
    function GetItem(Index: Integer): Int64;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CodingType: Byte read FCodingType;
    property Frequency[Index: Integer]: Int64 read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TSubtitlingDescriptor = class(TBaseDescriptor)
  protected
    function GetItem(Index: Integer): TSubtitlingDescriptorItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Subtitle[Index: Integer]: TSubtitlingDescriptorItem read GetItem;
  end;

  // ---------------------------------------------------------------------------

  TBouquetNameDescriptor = class(TBaseDescriptor)
  protected
    FName: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Name: String read FName;
  end;

  // ---------------------------------------------------------------------------

  // TODO implement AAC Descriptor
  TAACDescriptor = class(TBaseDescriptor)
  end;

  // TODO implement DTS Descriptor
  TDTSDescriptor = class(TBaseDescriptor)
  end;

  TPremiereContentOrderDescriptor = class(TBaseDescriptor)
  protected
    FOrderNumber: String;
    FOrderPrice: String;
    FOrderPhoneNumber: String;
    FSMSOrderInformation: String;
    FURLOrderInformation: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property OrderNumber: String read FOrderNumber;
    property OrderPrice: String read FOrderPrice;
    property OrderPhoneNumber: String read FOrderPhoneNumber;
    property SMSOrderInformation: String read FSMSOrderInformation;
    property URLOrderInformation: String read FURLOrderInformation;
  end;

  TPremiereParentalInformationDescriptor = class(TBaseDescriptor)
  protected
    FRating: Byte;
    FControlTime1: String;
    FControlTime2: String;
    FParentalInformation: String;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property Rating: Byte read FRating;
    property ControlTime1: String read FControlTime1;
    property ControlTime2: String read FControlTime2;
    property ParentalInformation: String read FParentalInformation;
  end;

  TPremiereContentTransmissionDescriptor = class(TBaseDescriptor)
  protected
    FTSID: Integer;
    FONID: Integer;
    FSID: Integer;
    function GetItem(AIndex: Integer): PPremiereContentTransmitionItem;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property TSID: Integer read FTSID;
    property ONID: Integer read FONID;
    property SID: Integer read FSID;

    property StartDate[Index: Integer]: PPremiereContentTransmitionItem read GetItem;
  end;



implementation

uses
  MPEGUtils;

(*** TDescriptorList **********************************************************)

function TDescriptorList.Get(Index: Integer): TBaseDescriptor;
begin
  Result := inherited Get(Index);
end;

procedure TDescriptorList.Put(Index: Integer; Item: TBaseDescriptor);
begin
  inherited Put(Index, Item);
end;

function TDescriptorList.Add(Item: TBaseDescriptor): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDescriptorList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TBaseDescriptor(Items[i]).Free;
  inherited Clear;
end;

procedure TDescriptorList.Delete(Index: Integer);
begin
  TBaseDescriptor(Items[Index]).Free;
  inherited Delete(Index);
end;

function TDescriptorList.GetDescriptor(AClass: TBaseDescriptorClass; out ADescriptor): Boolean;
var
  i: Integer;
begin
  Result := False;
  Pointer(ADescriptor) := nil;
  for i := 0 to Count -1 do
  begin
    if Get(i).ClassType = AClass then
    begin
      Result := True;
      Pointer(ADescriptor) := Get(i);
      Exit;
    end;
  end;
end;

(*** TBaseDescriptor ******************************************************)

class function TBaseDescriptor.CreateDescriptor(ABuffer: PByte): TBaseDescriptor;
begin
  if not Assigned(ABuffer) then
  begin
    Result := TBaseDescriptor.Create;
    Exit;
  end;

  case (ABuffer^) of
    DESCRIPTOR_TAG_RESERVED_0:                      Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_RESERVED_1:                      Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_VIDEO_STREAM:                    Result := TVideoStreamDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_AUDIO_STREAM:                    Result := TAudioStreamDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_HIERARCHY:                       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_REGISTRATION:                    Result := TRegistrationDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DATA_STREAM_ALIGNMENT:           Result := TDataStreamAlignementDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TARGET_BACKGROUND_GRID:          Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_VIDEO_WINDOW:                    Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CA:                              Result := TCADescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ISO639LANGUAGE:                  Result := TISO639LanguageDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SYSTEM_CLOCK:                    Result := TSystemClockDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MULTIPLEX_BUFFER_UTILIZATION:    Result := TMultiplexBufferUtilizationDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_COPYRIGHT:                       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MAXIMUM_BITRATE:                 Result := TMaximumBitrateDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PRIVATE_DATA_INDICATOR:          Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SMOOTHING_BUFFER_INDICATOR:      Result := TSmoothingBufferDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_STD:                             Result := TSTDDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_IBP:                             Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CAROUSEL_IDENTIFIER:             Result := TCarouselIdentifierDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ASSOCIATION:                     Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DEFERRED_ASSOCIATION:            Result := TBaseDescriptor.Create(ABuffer);
    // TODO Add missing DSM-CC Descriptors
    DESCRIPTOR_TAG_MPEG4_VIDEO:                     Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MPEG4_AUDIO:                     Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_IOD:                             Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_FMC:                             Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SL:                              Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_OCR_ES_ID:                       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_EXTERNAL_ES_ID:                  Result := TBaseDescriptor.Create(ABuffer);
    // Reserved Descriptor Tags followed by PSI Tags
    DESCRIPTOR_TAG_NETWORK_NAME:                    Result := TNetworkNameDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SERVICE_LIST:                    Result := TServiceListDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_STUFFING:                        Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SATELLITE_DELIVERY_SYSTEM:       Result := TSatelliteDeliverySystemDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CABLE_DELIVERY_SYSTEM:           Result := TCableDeliverySystemDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_VBI_DATA:                        Result := TVBIDataDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_VBI_TELETEXT:                    Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_BOUQUET_NAME:                    Result := TBouquetNameDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SERVICE:                         Result := TServiceDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_COUNTRY_AVAILABILITY:            Result := TCountryAvailibilityDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_LINKAGE:                         Result := TLinkageDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_NVOD_REFERENCE:                  Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TIME_SHIFTED_SERVICE:            Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SHORT_EVENT:                     Result := TShortEventDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_EXTENDED_EVENT:                  Result := TExtendedEventDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TIME_SHIFTED_EVENT:              Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_COMPONENT:                       Result := TComponentDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MOSAIC:                          Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_STREAM_IDENTIFIER:               Result := TStreamIdentifierDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CA_IDENTIFIER:                   Result := TCAIdentifierDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CONTENT:                         Result := TContentDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PARENTAL_RATING:                 Result := TParentalRatingDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TELETEXT:                        Result := TTeletextDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TELEPHONE:                       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_LOCAL_TIME_OFFSET:               Result := TLocalTimeOffsetDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SUBTITLING:                      Result := TSubtitlingDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TERRESTRIAL_DELIVERY_SYSTEM:     Result := TTerrestrialDeliverySystemDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MULTILINGUAL_NETWORK_NAME:       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MULTILINGUAL_BOUQUET_NAME:       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MULTILINGUAL_SERVICE_NAME:       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_MULTILINGUAL_COMPONENT:          Result := TMultilingualComponentDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PRIVATE_DATA_SPECIFIER:          Result := TPrivateDataSpecifierDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SERVICE_MOVE:                    Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SHORT_SMOOTHING_BUFFER:          Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_FREQUENCY_LIST:                  Result := TFrequencyListDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PARTIAL_TRANSPORT_STREAM:        Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DATA_BROADCAST:                  Result := TDataBroadcastDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SCRAMBLING:                      Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DATA_BROADCAST_ID:               Result := TDataBroadcastIDDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TRANSPORT_STREAM:                Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DSNG:                            Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PDC:                             Result := TPDCDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_AC3:                             Result := TAC3Descriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ANCILLARY_DATA:                  Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CELL_LIST:                       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CELL_FREQUENCY_LINK:             Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ANNOUNCEMENT_SUPPORT:            Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_APPLICATION_SIGNALLING:          Result := TApplicationSignallingDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ADAPTATION_FIELD_DATA:           Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SERVICE_IDENTIFIER:              Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_SERVICE_AVAILABILITY:            Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DEFAULT_AUTHORITY:               Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_RELATED_CONTENT:                 Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TVAID:                           Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_CONTENT_IDENTIFIER:              Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_TIME_SLICE_FEC_IDENTIFIER:       Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ECM_REPETITION_RATE:             Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_S2_SATELLITE_DELIVERY:               Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_ENHANCED_AC3:                    Result := TBaseDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_DTS:                             Result := TDTSDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_AAC:                             Result := TAACDescriptor.Create(ABuffer);

    DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER:          Result := TPremiereContentOrderDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION:   Result := TPremiereParentalInformationDescriptor.Create(ABuffer);
    DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION:    Result := TPremiereContentTransmissionDescriptor.Create(ABuffer);

    else                                            Result := TBaseDescriptor.Create(ABuffer);
  end;
end;

constructor TBaseDescriptor.Create;
begin
  inherited Create;
  FDescriptorType := dtDefault;
  FList := TList.Create;
  Clear;
end;

constructor TBaseDescriptor.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TBaseDescriptor.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TBaseDescriptor.GetCountItems: Integer;
begin
  Result := FList.Count;
end;

procedure TBaseDescriptor.Clear;
begin
  FTag := DESCRIPTOR_TAG_FORBIDDEN;
  FDescriptorLength := 0;
  FTotalLength := 2;
end;

procedure TBaseDescriptor.ParseBuffer(ABuffer: PByte);
begin
  Clear;

  if not Assigned(ABuffer)
    then Exit;

  FTag := ABuffer^;
  inc(ABuffer);
  FDescriptorLength := ABuffer^;
  FTotalLength := FDescriptorLength + 2;
end;

(*** TVideoStreamDescriptor ***************************************************)

procedure TVideoStreamDescriptor.Clear;
begin
  inherited Clear;

  FMultipleFrameRateFlag := 0;
  FFrameRateCode := 0;
  FMPEG1OnlyFlag := 0;
  FConstrainedParameterFlag := 0;
  FStillPictureFlag := 0;
  FProfileAndLevelIndication := 0;
  FChromaFormat := 0;
  FFrameRateExtensionFlag := 0;
end;

procedure TVideoStreamDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FMultipleFrameRateFlag := GetByteBits(ABuffer, 0, 1);
  FFrameRateCode := GetByteBits(ABuffer, 1, 4);
  FMPEG1OnlyFlag := GetByteBits(ABuffer, 5, 1);
  FConstrainedParameterFlag := GetByteBits(ABuffer, 6, 1);
  FStillPictureFlag := GetByteBits(ABuffer, 7, 1);
  if FMPEG1OnlyFlag = 1 then
  begin
    inc(ABuffer);
    FProfileAndLevelIndication := ABuffer^;
    inc(ABuffer);
    FChromaFormat := GetByteBits(ABuffer, 0, 2);
    FFrameRateExtensionFlag := GetByteBits(ABuffer, 2, 1);
  end;
end;

(*** TAudioStreamDescriptor ***************************************************)

procedure TAudioStreamDescriptor.Clear;
begin
  inherited Clear;

  FFreeFormatFlag := 0;
  FID := 0;
  FLayer := 0;
  FVariableRateAudioIndicator := 0;
end;

procedure TAudioStreamDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FFreeFormatFlag := GetByteBits(ABuffer, 0, 1);
  FID := GetByteBits(ABuffer, 1, 1);
  FLayer := GetByteBits(ABuffer, 2, 2);
  FVariableRateAudioIndicator := GetByteBits(ABuffer, 4, 1);
end;

(*** TNetworkNameDescriptor ***************************************************)

procedure TNetworkNameDescriptor.Clear;
begin
  inherited Clear;
  FName := '';
end;

procedure TNetworkNameDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  if (FDescriptorLength > 0) then
  begin
    SetLength(FName, FDescriptorLength);
    Move(ABuffer^, FName[1], FDescriptorLength);
  end;
end;

(*** TShortEventDescriptor ****************************************************)

procedure TShortEventDescriptor.Clear;
begin
  inherited Clear;
  FLanguage := '   ';
  FName := '';
  FDescription := '';
end;

procedure TShortEventDescriptor.ParseBuffer(ABuffer: PByte);
var
  event_name_length: Integer;
  text_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FLanguage := PISO6392LanguageCode(ABuffer)^;
  inc(ABuffer, 3);

  event_name_length := ABuffer^;
  inc(ABuffer, 1);
  if (event_name_length > 0) then
  begin
    SetLength(FName, event_name_length);
    Move(ABuffer^, FName[1], event_name_length);
    inc(ABuffer, event_name_length);
  end;

  text_length := ABuffer^;
  inc(ABuffer, 1);
  if (text_length > 0) then
  begin
    SetLength(FDescription, text_length);
    Move(ABuffer^, FDescription[1], text_length);
  end;
end;

(*** TServiceListDescriptor ***************************************************)

function TServiceListDescriptor.GetService(Index: Integer): TServiceDescriptorService;
begin
  Result.ServiceID := 0;
  Result.ServiceType := 0;
  if (Index >= 0) and (Index < FList.Count)
    then Result := PServiceDescriptorService(FList[Index])^;
end;

procedure TServiceListDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PServiceDescriptorService(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TServiceListDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  service: PServiceDescriptorService;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(service);
    service.ServiceID := GetWord(ABuffer);
    inc(ABuffer, 2);
    service.ServiceType := ABuffer^;
    FList.Add(service);
    inc(ABuffer, 1);
    dec(count, 3);
  end;
end;

(*** TTerrestrialDeliverySystemDescriptor *************************************)

procedure TTerrestrialDeliverySystemDescriptor.Clear;
begin
  inherited Clear;

  FCentreFrequency := 0;
  FBandwidth := 0;
  FPriority := 0;
  FTimeSlicingIndicator := 0;
  FMPEFECIndicator := 0;
  FConstellation := tcReserved;
  FHierarchyInformation := thiNonHierarchicalNativeInterleaver;
  FCodeRateHPStream := tcrReserved;
  FCodeRateLPStream := tcrReserved;
  FGuardInterval := tgi1_4;
  FTransmissionMode := ttmReserved;
  FOtherFrequencyFlag := 0;
end;

procedure TTerrestrialDeliverySystemDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCentreFrequency := Int64(GetLong(ABuffer)) * 10;
  inc(ABuffer, 4);

  FBandwidth := GetBandwidth(GetByteBits(ABuffer, 0, 3));
  FPriority := GetByteBits(ABuffer, 3, 1);
  FTimeSlicingIndicator := GetByteBits(ABuffer, 4, 1);
  FMPEFECIndicator := GetByteBits(ABuffer, 5, 1);
  inc(ABuffer, 1);

  FConstellation := GetTerrestrialConstellation(GetByteBits(ABuffer, 0, 2));
  FHierarchyInformation := TTerrestrialHierarchyInformation(GetByteBits(ABuffer, 2, 3));
  FCodeRateHPStream := GetTerrestrialCodeRate(GetByteBits(ABuffer, 5, 3));
  inc(ABuffer, 1);

  FCodeRateLPStream := GetTerrestrialCodeRate(GetByteBits(ABuffer, 0, 3));
  FGuardInterval := TTerrestrialGuardInterval(GetByteBits(ABuffer, 3, 2));
  FTransmissionMode := GetTerrestrialTransmissionMode(GetByteBits(ABuffer, 5, 2));
  FOtherFrequencyFlag := GetByteBits(ABuffer, 7, 1);
end;

(*** TSatelliteDeliverySystemDescriptor ***************************************)

procedure TSatelliteDeliverySystemDescriptor.Clear;
begin
  inherited Clear;

  FFrequency := 0;
  FOrbitalPosition := 0;
  FWestEastFlag := 0;
  FPolarization := spUnknown;
  FModulation := smNotDefined;
  FSymbolRate := 0;
  FFECInner := fecNotDefined;
end;

procedure TSatelliteDeliverySystemDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);


  FFrequency := HexToDec(GetLong(ABuffer)) * 10;
  FFrequency := FFrequency * 1000; 
  inc(ABuffer, 4);

  FOrbitalPosition := HexToDec(GetWord(ABuffer));
  inc(ABuffer, 2);

  FWestEastFlag := GetByteBits(ABuffer, 0, 1);
  FPolarization := TSatellitePolarization(GetByteBits(ABuffer, 1, 2) + 1);
  FModulation := GetSatelliteModulation(GetByteBits(ABuffer, 3, 5));
  inc(ABuffer, 1);

  FSymbolRate := Int64(HexToDec(GetLongBits(ABuffer, 0, 28))) * 100;
  FFECInner := GetFECInner(GetLongBits(ABuffer, 28, 4));
end;

(*** TCableDeliverySystemDescriptor *******************************************)

procedure TCableDeliverySystemDescriptor.Clear;
begin
  inherited Clear;

  FFrequency := 0;
  FFECOuter := fecNotDefined_;
  FModulation := cmNotDefined;
  FSymbolRate := 0;
  FFECInner := fecNotDefined;
end;

procedure TCableDeliverySystemDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FFrequency := HexToDec(GetLong(ABuffer));
  FFrequency := FFrequency * 100;
  inc(ABuffer, 4);

  inc(ABuffer);
  FFECOuter := GetFECOuter(GetByteBits(ABuffer, 4, 4));
  inc(ABuffer);

  FModulation := GetCableModulation(ABuffer^);
  inc(ABuffer);

  FSymbolRate := Int64(HexToDec(GetLongBits(ABuffer, 0, 28))) * 100;
  FFECInner := GetFECInner(GetLongBits(ABuffer, 28, 4));
end;

(*** TStreamIdentifierDescriptor **********************************************)

procedure TStreamIdentifierDescriptor.Clear;
begin
  inherited Clear;
  FComponentTag := 0;
end;

procedure TStreamIdentifierDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FComponentTag := ABuffer^;
end;

(*** TDataBroadcastIDDescriptor ***********************************************)

procedure TDataBroadcastIDDescriptor.Clear;
begin
  inherited Clear;
  FDataBroadcastID := 0;
  FIDSelectorLength := 0;
  if Assigned(FIDSelector) then
  begin
    FreeMem(FIDSelector);
    FIDSelector := nil;
  end;
end;

procedure TDataBroadcastIDDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FDataBroadcastID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FIDSelectorLength := FDescriptorLength - 2;
  if FIDSelectorLength > 0 then
  begin
    FIDSelector := AllocMem(FIDSelectorLength);
    Move(ABuffer^, FIDSelector^, FIDSelectorLength);
  end;
end;

(*** TAC3Descriptor ***********************************************************)

procedure TAC3Descriptor.Clear;
begin
  inherited Clear;

  FComponentTypeFlag := 0;
  FBSIDFlag := 0;
  FMainIDFlag := 0;
  FASVCFlag := 0;
  FComponentType := 0;
  FBSID := 0;
  FMainID := 0;
  FASVC := 0;

  FAdditionalInfoLength := 0;
  if Assigned(FAdditionalInfo) then
  begin
    FreeMem(FAdditionalInfo);
    FAdditionalInfo := nil;
  end;
end;

procedure TAC3Descriptor.ParseBuffer(ABuffer: PByte);
var
  l: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);
  l := 0;

  FComponentTypeFlag := GetByteBits(ABuffer, 0, 1);
  FBSIDFlag := GetByteBits(ABuffer, 1, 1);
  FMainIDFlag := GetByteBits(ABuffer, 2, 1);
  FASVCFlag := GetByteBits(ABuffer, 3, 1);
  inc(ABuffer);

  if (FComponentTypeFlag > 0) then
  begin
    FComponentType := ABuffer^;
    inc(ABuffer);
    inc(l);
  end;

  if (FBSIDFlag > 0) then
  begin
    FBSID := ABuffer^;
    inc(ABuffer);
    inc(l);
  end;

  if (FMainIDFlag > 0) then
  begin
    FMainID := ABuffer^;
    inc(ABuffer);
    inc(l);
  end;

  if (FASVCFlag > 0) then
  begin
    FASVC := ABuffer^;
    inc(ABuffer);
    inc(l);
  end;

  FAdditionalInfoLength := FDescriptorLength - 1 - l;
  if (FAdditionalInfoLength > 0) then
  begin
    FAdditionalInfo := AllocMem(FAdditionalInfoLength);
    Move(ABuffer^, FAdditionalInfo^, FAdditionalInfoLength);
  end;
end;

(*** TTeletextDescriptor ******************************************************)

function TTeletextDescriptor.GetPage(Index: Integer): TTeletextDescriptorPage;
begin
  Result.ISO639Language := '   ';
  Result.Type_ := 0;
  Result.MagazinNumber := 0;
  Result.PageNumber := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PTeletextDescriptorPage(FList[Index])^;
end;

procedure TTeletextDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PTeletextDescriptorPage(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TTeletextDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  page: PTeletextDescriptorPage;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(page);

    page.ISO639Language := PISO6392LanguageCode(ABuffer)^;
    inc(ABuffer, 3);

    page.Type_ := GetByteBits(ABuffer, 0, 5);
    page.Type_ := GetByteBits(ABuffer, 5, 3);
    inc(ABuffer, 1);

    page.PageNumber := ABuffer^;
    inc(ABuffer, 1);
    FList.Add(page);

    dec(count, 5);
  end;
end;

(*** TISO639LanguageDescriptor ************************************************)

function TISO639LanguageDescriptor.GetLanguage(Index: Integer): TISO639LanguageDescriptorLanguage;
begin
  Result.ISO639Language := '   ';
  Result.AudioType := atReserved;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PISO639LanguageDescriptorLanguage(FList[Index])^;
end;

procedure TISO639LanguageDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PISO639LanguageDescriptorLanguage(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TISO639LanguageDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  lang: PISO639LanguageDescriptorLanguage;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(lang);

    lang.ISO639Language := PISO6392LanguageCode(ABuffer)^;
    inc(ABuffer, 3);

    lang.AudioType := GetISO639LanguageDescriptorAudioType(ABuffer^);
    inc(ABuffer, 1);
    FList.Add(lang);

    dec(count, 4);
  end;
end;

(*** TSTDDescriptor ***********************************************************)

procedure TSTDDescriptor.Clear;
begin
  inherited Clear;
  FLeakValidFlag := 0;
end;

procedure TSTDDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FLeakValidFlag := GetByteBits(ABuffer, 7, 1);
end;

(*** TApplicationSignallingDescriptor **********************************************)

function TApplicationSignallingDescriptor.GetApplication(Index: Integer): TApplicationSignallingDescriptorApplication;
begin
  Result.ApplicationType := 0;
  Result.AITVersionNumber := 0;
  if (Index >= 0) and (Index < FList.Count)
    then Result := PApplicationSignallingDescriptorApplication(FList[Index])^;
end;

procedure TApplicationSignallingDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PApplicationSignallingDescriptorApplication(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TApplicationSignallingDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  app: PApplicationSignallingDescriptorApplication;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    new(app);
    app.ApplicationType := GetWord(ABuffer);
    inc(ABuffer, 2);
    app.AITVersionNumber := GetByteBits(ABuffer, 3, 5);
    FList.Add(app);
    inc(ABuffer, 1);
    dec(count, 3);
  end;
end;

(*** TRegistrationDescriptor **************************************************)

procedure TRegistrationDescriptor.Clear;
begin
  inherited Clear;
  FFormatIdentifier := 0;
  FAdditionalIdentificationInfoLength := 0;
  if Assigned(FAdditionalIdentificationInfo) then
  begin
    FreeMem(FAdditionalIdentificationInfo);
    FAdditionalIdentificationInfo := nil;
  end;
end;

procedure TRegistrationDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FFormatIdentifier := GetLong(ABuffer);
  inc(ABuffer, 4);

  FAdditionalIdentificationInfoLength := FDescriptorLength - 4;
  if FAdditionalIdentificationInfoLength > 0 then
  begin
    FAdditionalIdentificationInfo := AllocMem(FAdditionalIdentificationInfoLength);
    Move(ABuffer^, FAdditionalIdentificationInfo^, FAdditionalIdentificationInfoLength);
  end;
end;

(*** TCarouselIdentifierDescriptor ********************************************)

procedure TCarouselIdentifierDescriptor.Clear;
begin
  inherited Clear;

  FCarouselID := 0;
  FFormatID := 0;
  FPrivateDataLength := 0;
  FModuleVersion := 0;
  FModuleID := 0;
  FBlockSize := 0;
  FModuleSize := 0;
  FCompressionMethod := 0;
  FOriginalSize := 0;
  FTimeOut := 0;
  FObjectKeyLength := 0;

  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
  if Assigned(FObjectKey) then
  begin
    FreeMem(FObjectKey);
    FObjectKey := nil;
  end;
end;

procedure TCarouselIdentifierDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCarouselID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FFormatID := ABuffer^;
  inc(ABuffer);

  if FFormatID = 0 then
  begin
    FPrivateDataLength := FDescriptorLength - 5;
    if FPrivateDataLength > 0 then
    begin
      FPrivateData := AllocMem(FPrivateDataLength);
      Move(ABuffer^, FPrivateData^, FPrivateDataLength);
    end;
  end else
  if (FFormatID = $01) then
  begin
    FModuleVersion := ABuffer^;
    inc(ABuffer);

    FModuleID := GetWord(ABuffer);
    inc(ABuffer, 2);

    FBlockSize := GetWord(ABuffer);
    inc(ABuffer, 2);

    FModuleSize := GetLong(ABuffer);
    inc(ABuffer, 4);

    FCompressionMethod := ABuffer^;
    inc(ABuffer);

    FOriginalSize := GetLong(ABuffer);
    inc(ABuffer, 4);

    FTimeOut := ABuffer^;
    inc(ABuffer);

    FObjectKeyLength := ABuffer^;
    inc(ABuffer);

    if FObjectKeyLength > 0 then
    begin
      FObjectKey := AllocMem(FObjectKeyLength);
      Move(ABuffer^, FObjectKey^, FObjectKeyLength);
    end;
  end;
end;

(*** TVBIDataDescriptor *******************************************************)

function TVBIDataDescriptor.GetLine(Index: Integer): TVBIDataDescriptorLine;
begin
  Result.DataServiceID := 0;
  Result.FieldParity := 0;
  Result.LineOffset := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PVBIDataDescriptorLine(FList[Index])^;
end;

procedure TVBIDataDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PVBIDataDescriptorLine(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TVBIDataDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  line: PVBIDataDescriptorLine;
  data_service_id: Integer;
  data_service_descriptor_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;
  while(count > 0) do
  begin
    data_service_id := ABuffer^;
    inc(ABuffer);
    data_service_descriptor_length := ABuffer^;
    inc(ABuffer);

    dec(count, data_service_descriptor_length + 1);

    if (data_service_id = $01) or
       (data_service_id = $02) or
       (data_service_id = $04) or
       (data_service_id = $05) or
       (data_service_id = $06) or
       (data_service_id = $07) then
    begin
      while(data_service_descriptor_length > 0) do
      begin
        new(line);
        line.DataServiceID := data_service_id;
        line.FieldParity := GetByteBits(ABuffer, 2, 1);
        line.LineOffset := GetByteBits(ABuffer, 3, 5);
        dec(data_service_descriptor_length);
        FList.Add(line);
        inc(ABuffer);
      end;
    end;
  end;
end;

(*** TServiceDescriptor *******************************************************)

procedure TServiceDescriptor.Clear;
begin
  inherited Clear;
  FProviderName := '';
  FServiceName := '';
end;

procedure TServiceDescriptor.ParseBuffer(ABuffer: PByte);
var
  service_provider_name_length: Byte;
  service_name_length: Byte;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FServiceType := ABuffer^;
  inc(ABuffer);

  service_provider_name_length := ABuffer^;
  inc(ABuffer);

  if service_provider_name_length > 0 then
  begin
    SetLength(FProviderName, service_provider_name_length);
    Move(ABuffer^, FProviderName[1], service_provider_name_length);
    inc(ABuffer, service_provider_name_length);
  end;

  service_name_length := ABuffer^;
  inc(ABuffer);

  if service_name_length > 0 then
  begin
    SetLength(FServiceName, service_name_length);
    Move(ABuffer^, FServiceName[1], service_name_length);
  end;
end;

(*** TDataBroadcastDescriptor *************************************************)

procedure TDataBroadcastDescriptor.Clear;
begin
  inherited Clear;
  FDataBroadcastID := 0;
  FComponentTag := 0;
  FSelectorLength := 0;
  FISO639LanguageCode := '   ';
  FDescription := '';

  if Assigned(FSelector) then
  begin
    FreeMem(FSelector);
    FSelector := nil;
  end;
end;

procedure TDataBroadcastDescriptor.ParseBuffer(ABuffer: PByte);
var
  text_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FDataBroadcastID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FComponentTag := ABuffer^;
  inc(ABuffer);

  FSelectorLength := ABuffer^;
  inc(ABuffer);

  if FSelectorLength > 0 then
  begin
    FSelector := AllocMem(FSelectorLength);
    Move(ABuffer^, FSelector^, FSelectorLength);
    inc(ABuffer,  FSelectorLength);
  end;

  FISO639LanguageCode := PISO6392LanguageCode(ABuffer)^;
  inc(ABuffer, 3);

  text_length := ABuffer^;
  inc(ABuffer);

  if text_length > 0 then
  begin
    SetLength(FDescription, text_length);
    Move(ABuffer^, FDescription[1], text_length);
  end;
end;

(*** TLocalTimeOffsetDescriptor ***********************************************)

function TLocalTimeOffsetDescriptor.GetEntry(Index: Integer): TLocalTimeOffsetDescriptorEntry;
begin
  Result.CountryCode := '   ';
  Result.CountryRegionID := 0;
  Result.LocalTimeOffsetPolarity := 0;
  Result.LocalTimeOffset := 0;
  FillChar(Result.TimeOfChange, 5, 0);
  Result.NextTimeOffset := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PLocalTimeOffsetDescriptorEntry(FList[Index])^;
end;

procedure TLocalTimeOffsetDescriptor.Clear;
begin
  inherited Clear;
  while (FList.Count > 0) do
  begin
    Dispose(PLocalTimeOffsetDescriptorEntry(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TLocalTimeOffsetDescriptor.ParseBuffer(ABuffer: PByte);
var
  count: Integer;
  entry: PLocalTimeOffsetDescriptorEntry;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  count := FDescriptorLength;

  while(count > 0) do
  begin
    new(entry);

    entry.CountryCode := PCountryCode(ABuffer)^;
    inc(ABuffer, 3);

    entry.CountryRegionID := GetByteBits(ABuffer, 0, 6);
    entry.LocalTimeOffsetPolarity := GetByteBits(ABuffer, 7, 1);
    inc(ABuffer);

    entry.LocalTimeOffset := GetWord(ABuffer);
    inc(ABuffer, 2);

    entry.TimeOfChange := GetUTCTime(ABuffer);
    inc(ABuffer, 5);

    entry.NextTimeOffset := GetWord(ABuffer);
    inc(ABuffer, 2);

    FList.Add(entry);

    dec(count, 13);
  end;
end;

(*** TExtendedEventDescriptor *************************************************)

function TExtendedEventDescriptor.GetItem(Index: Integer): TExtendedEventDescriptorItem;
begin
  Result.Description := '';
  Result.Text := '';

  if (Index >= 0) and (Index < FList.Count)
    then Result := PExtendedEventDescriptorItem(FList[Index])^;
end;

procedure TExtendedEventDescriptor.Clear;
begin
  inherited Clear;

  FDescriptorNumber := 0;
  FLastDescriptorNumber := 0;
  FLanguage := '   ';
  FText := '';

  while (FList.Count > 0) do
  begin
    Dispose(PExtendedEventDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TExtendedEventDescriptor.ParseBuffer(ABuffer: PByte);
var
  item: PExtendedEventDescriptorItem;
  length_of_items: Integer;
  text_length: Integer;
  item_description_length: Integer;
  item_length: integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FDescriptorNumber := GetByteBits(ABuffer, 0, 4);
  FLastDescriptorNumber := GetByteBits(ABuffer, 4, 4);
  inc(ABuffer);

  FLanguage := PISO6392LanguageCode(ABuffer)^;
  inc(ABuffer, 3);

  length_of_items := ABuffer^;
  inc(ABuffer);

  while (length_of_items > 0) do
  begin
    new(item);

    item_description_length := ABuffer^;
    inc(ABuffer);
    dec(length_of_items);

    if item_description_length > 0 then
    begin
      SetLength(item.Description, item_description_length);
      Move(ABuffer^, item.Description[1], item_description_length);
      inc(ABuffer, item_description_length);
      dec(length_of_items, item_description_length);
    end;

    item_length := ABuffer^;
    inc(ABuffer);
    dec(length_of_items);

    if item_length > 0 then
    begin
      SetLength(item.Text, item_length);
      Move(ABuffer^, item.Text[1], item_length);
      inc(ABuffer, item_length);
      dec(length_of_items, item_length);
    end;

    FList.Add(item);
  end;

  text_length := ABuffer^;
  inc(ABuffer);

  if text_length > 0 then
  begin
    SetLength(FText, text_length);
    Move(ABuffer^, FText[1], text_length);
  end;
end;

(*** TContentDescriptor *******************************************************)

function TContentDescriptor.GetItem(Index: Integer): TContentDescriptorItem;
begin
  Result.ContentNibbleLevel1 := 0;
  Result.ContentNibbleLevel2 := 0;
  Result.UserNibbleLevel1 := 0;
  Result.UserNibbleLevel2 := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PContentDescriptorItem(FList[Index])^;
end;

procedure TContentDescriptor.Clear;
begin
  inherited Clear;

  while (FList.Count > 0) do
  begin
    Dispose(PContentDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TContentDescriptor.ParseBuffer(ABuffer: PByte);
var
  item: PContentDescriptorItem;
  length_of_items: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  length_of_items := FDescriptorLength;

  while (length_of_items > 0) do
  begin
    new(item);

    item.ContentNibbleLevel1 := GetByteBits(ABuffer, 0, 4);
    item.ContentNibbleLevel2 := GetByteBits(ABuffer, 4, 4);
    inc(ABuffer);

    item.UserNibbleLevel1 := GetByteBits(ABuffer, 0, 4);
    item.UserNibbleLevel2 := GetByteBits(ABuffer, 4, 4);
    inc(ABuffer);

    dec(length_of_items, 2);
    FList.Add(item);
  end;
end;

(*** TComponentDescriptor *****************************************************)

procedure TComponentDescriptor.Clear;
begin
  inherited Clear;
  FStreamContent := 0;
  FComponentType := 0;
  FComponentTag := 0;
  FISO639LanguageCode := '   ';
  FDescription := '';
end;

procedure TComponentDescriptor.ParseBuffer(ABuffer: PByte);
var
  text_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FStreamContent := GetByteBits(ABuffer, 4, 4);
  inc(ABuffer);

  FComponentType := ABuffer^;
  inc(ABuffer);

  FComponentTag := ABuffer^;
  inc(ABuffer);

  FISO639LanguageCode := PISO6392LanguageCode(ABuffer)^;
  inc(ABuffer, 3);

  text_length := FDescriptorLength - 6;
  if text_length > 0 then
  begin
    SetLength(FDescription, text_length);
    Move(ABuffer^, FDescription[1], text_length);
  end;
end;

(*** TParentalRatingDescriptor ************************************************)

function TParentalRatingDescriptor.GetItem(Index: Integer): TParentalDescriptorItem;
begin
  Result.CountryCode := '   ';
  Result.Rating := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PParentalDescriptorItem(FList[Index])^;
end;

procedure TParentalRatingDescriptor.Clear;
begin
  inherited Clear;

  while (FList.Count > 0) do
  begin
    Dispose(PParentalDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TParentalRatingDescriptor.ParseBuffer(ABuffer: PByte);
var
  item: PParentalDescriptorItem;
  length_of_items: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  length_of_items := FDescriptorLength;

  while (length_of_items > 0) do
  begin
    new(item);

    item.CountryCode := PCountryCode(ABuffer)^;
    inc(ABuffer, 3);
    item.Rating := ABuffer^;
    inc(ABuffer);

    dec(length_of_items, 4);
    FList.Add(item);
  end;
end;

(*** TPrivateDataSpecifierDescriptor ******************************************)

procedure TPrivateDataSpecifierDescriptor.Clear;
begin
  inherited Clear;
  FPrivateDataSpecifier := 0;
end;

procedure TPrivateDataSpecifierDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FPrivateDataSpecifier := GetLong(ABuffer);
end;

(*** TLinkageDescriptor *******************************************************)

procedure TLinkageDescriptor.Clear;
begin
  inherited Clear;

  FTransportStreamID := 0;
  FOriginalNetworkID := 0;
  FServiceID := 0;
  FLinkageType := 0;
  FHandOverType := 0;
  FOriginType := 0;
  FNetworkID := 0;
  FInitialServiceID := 0;
  FPrivateDataLength := 0;

  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
end;

procedure TLinkageDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FTransportStreamID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FOriginalNetworkID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FServiceID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FLinkageType := ABuffer^;
  inc(ABuffer);

  if FLinkageType <> $08 then
  begin
    FPrivateDataLength := FDescriptorLength - 7;
    if FPrivateDataLength > 0 then
    begin
      FPrivateData := AllocMem(FPrivateDataLength);
      Move(ABuffer^, FPrivatedata^, FPrivateDataLength);
    end;
  end else
  begin
    FPrivateDataLength := FDescriptorLength - 8;
    FHandOverType := GetByteBits(ABuffer, 0, 4);
    FOriginType := GetByteBits(ABuffer, 7, 1);
    inc(ABuffer);
    if (FHandOverType = $01) or (FHandOverType = $02) or (FHandOverType = $03) then
    begin
      FNetworkID := GetWord(ABuffer);
      inc(ABuffer, 2);
      dec(FPrivateDataLength, 2);
    end;
    if (FOriginType = $00) then
    begin
      FInitialServiceID := GetWord(ABuffer);
      inc(ABuffer, 2);
      dec(FPrivateDataLength, 2);
    end;
    if FPrivateDataLength > 0 then
    begin
      FPrivateData := AllocMem(FPrivateDataLength);
      Move(ABuffer^, FPrivatedata^, FPrivateDataLength);
    end;
  end;
end;

(*** TPDCDescriptor ***********************************************************)

procedure TPDCDescriptor.Clear;
begin
  inherited Clear;
  FillChar(FProgrammeIdentificationLabel, 4, 0);
end;

procedure TPDCDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FProgrammeIdentificationLabel := GetProgrammeIdentificationLabel(ABuffer);
end;

(*** TCADescriptor ************************************************************)

procedure TCADescriptor.Clear;
begin
  inherited Clear;

  FSystemID := 0;
  FPID := 0;
  FPrivateDataLength := 0;

  if Assigned(FPrivateData) then
  begin
    FreeMem(FPrivateData);
    FPrivateData := nil;
  end;
end;

procedure TCADescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FSystemID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FPID := GetWordBits(ABuffer, 3, 13);
  inc(ABuffer, 2);

  FPrivateDataLength := FDescriptorLength - 4;
  if FPrivateDataLength > 0 then
  begin
    FPrivateData := AllocMem(FPrivateDataLength);
    Move(ABuffer^, FPrivateData^, FPrivateDataLength);
  end;
end;

(*** TCountryAvailibilityDescriptor ************************************************)

procedure TCountryAvailibilityDescriptor.Clear;
begin
  inherited Clear;

  FCountryAvailibilityFlag := 0;
  FCountryCode := '   ';
end;

procedure TCountryAvailibilityDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCountryAvailibilityFlag := GetByteBits(ABuffer, 0, 1);
  inc(ABuffer);

  FCountryCode := PCountryCode(ABuffer)^;
end;

(*** TCAIdentifierDescriptor **************************************************)

procedure TCAIdentifierDescriptor.Clear;
begin
  inherited Clear;

  FCASystemIDLength := 0;
  if Assigned(FCASystemID) then
  begin
    FreeMem(FCASystemID);
    FCASystemID := nil;
  end;
end;

procedure TCAIdentifierDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCASystemIDLength := FDescriptorLength;
  if FCASystemIDLength > 0 then
  begin
    FCASystemID := AllocMem(FCASystemIDLength);
    Move(ABuffer^, FCASystemID^, FCASystemIDLength);
  end;
end;

(*** TMultilingualComponentDescriptor *****************************************)

function TMultilingualComponentDescriptor.GetItem(Index: Integer): TMultilingualComponentDescriptorItem;
begin
  Result.LanguageCode := '   ';
  Result.Text := '';

  if (Index >= 0) and (Index < FList.Count)
    then Result := PMultilingualComponentDescriptorItem(FList[Index])^;
end;

procedure TMultilingualComponentDescriptor.Clear;
begin
  inherited Clear;
  FComponentTag := 0;

  while (FList.Count > 0) do
  begin
    Dispose(PMultilingualComponentDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TMultilingualComponentDescriptor.ParseBuffer(ABuffer: PByte);
var
  item: PMultilingualComponentDescriptorItem;
  length_of_items: Integer;
  text_description_length: Integer;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FComponentTag := ABuffer^;
  inc(ABuffer);

  length_of_items := FDescriptorLength - 1;

  while (length_of_items > 0) do
  begin
    new(item);
    item.Text := '';

    item.LanguageCode := PISO6392LanguageCode(ABuffer)^;
    inc(ABuffer, 3);
    dec(length_of_items, 3);

    text_description_length := ABuffer^;
    inc(ABuffer);
    dec(length_of_items);

    if text_description_length > 0 then
    begin
      SetLength(item.Text, text_description_length);
      Move(ABuffer^, item.Text[1], text_description_length);
      inc(ABuffer, text_description_length);
      dec(length_of_items, text_description_length);
    end;

    FList.Add(item);
  end;
end;

(*** TFrequencyListDescriptor *************************************************)

function TFrequencyListDescriptor.GetItem(Index: Integer): Int64;
begin
  Result := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PInt64(FList[Index])^;
end;

procedure TFrequencyListDescriptor.Clear;
begin
  inherited Clear;
  FCodingType := 0;
  while (FList.Count > 0) do
  begin
    Dispose(PInt64(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TFrequencyListDescriptor.ParseBuffer(ABuffer: PByte);
var
  length_of_items: Integer;
  frequency: PInt64;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCodingType := GetByteBits(ABuffer, 6, 2);
  inc(ABuffer);

  length_of_items := FDescriptorLength - 1;

  while (length_of_items > 0) do
  begin
    new(frequency);
    frequency^ := Int64(GetLong(ABuffer)) * 10;
    inc(ABuffer, 4);
    dec(length_of_items, 4);

    FList.Add(frequency);
  end;
end;

(*** TSubtitlingDescriptor ****************************************************)

function TSubtitlingDescriptor.GetItem(Index: Integer): TSubtitlingDescriptorItem;
begin
  Result.LanguageCode := '   ';
  Result.SubtitlingType := 0;
  Result.CompositionPageID := 0;
  Result.AncillaryPageID := 0;

  if (Index >= 0) and (Index < FList.Count)
    then Result := PSubtitlingDescriptorItem(FList[Index])^;
end;

procedure TSubtitlingDescriptor.Clear;
begin
  inherited Clear;

  while (FList.Count > 0) do
  begin
    Dispose(PSubtitlingDescriptorItem(FList[0]));
    FList.Delete(0);
  end;
end;

procedure TSubtitlingDescriptor.ParseBuffer(ABuffer: PByte);
var
  length_of_items: Integer;
  item: PSubtitlingDescriptorItem;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  length_of_items := FDescriptorLength;

  while (length_of_items > 0) do
  begin
    new(item);
    
    item.LanguageCode := PISO6392LanguageCode(ABuffer)^;
    inc(ABuffer, 3);

    item.SubtitlingType := ABuffer^;
    inc(ABuffer);

    item.CompositionPageID := GetWord(ABuffer);
    inc(ABuffer, 2);

    item.AncillaryPageID := GetWord(ABuffer);
    inc(ABuffer, 2);

    FList.Add(item);

    dec(length_of_items, 8);
  end;
end;

(*** TSystemClockDescriptor ***************************************************)

procedure TSystemClockDescriptor.Clear;
begin
  inherited Clear;

  FExternalClockReferenceIndicator := 0;
  FClockAccuracyInteger := 0;
  FClockAccuracyExponent := 0;
end;

procedure TSystemClockDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FExternalClockReferenceIndicator := GetByteBits(ABuffer, 0, 1);
  FClockAccuracyInteger := GetByteBits(ABuffer, 2, 6);
  inc(ABuffer);

  FClockAccuracyExponent := GetByteBits(ABuffer, 0, 3);
end;

(*** TSmoothingBufferDescriptor ***********************************************)

procedure TSmoothingBufferDescriptor.Clear;
begin
  inherited Clear;

  FSBLeakRate := 0;
  FSBSize := 0;
end;

procedure TSmoothingBufferDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FSBLeakRate := GetLongBits(ABuffer, 2, 22);
  inc(ABuffer, 3);

  FSBSize := GetByteBits(ABuffer, 2, 22);
end;

(*** TDataStreamAlignementDescriptor ******************************************)

procedure TDataStreamAlignementDescriptor.Clear;
begin
  inherited Clear;

  FAlignementType := 0;
end;

procedure TDataStreamAlignementDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FAlignementType := ABuffer^;
end;

(*** TMultiplexBufferUtilizationDescriptor ************************************)

procedure TMultiplexBufferUtilizationDescriptor.Clear;
begin
  inherited Clear;

  FBoundValidFlag := 0;
  FLTWOffsetLowerBound := 0;
  FLTWOffsetUpperBound := 0;
end;

procedure TMultiplexBufferUtilizationDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FBoundValidFlag := GetByteBits(ABuffer, 0, 1);
  FLTWOffsetLowerBound := GetWordBits(ABuffer, 1, 15);
  inc(ABuffer, 2);

  FLTWOffsetUpperBound := GetWordBits(ABuffer, 1, 15);
end;

(*** TMaximumBitrateDescriptor ************************************************)

procedure TMaximumBitrateDescriptor.Clear;
begin
  inherited Clear;

  FMaximumBitrate := 0;
end;

procedure TMaximumBitrateDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FMaximumBitrate := GetLongBits(ABuffer, 2, 22);
end;

(*** TNetworkNameDescriptor ***************************************************)

procedure TBouquetNameDescriptor.Clear;
begin
  inherited Clear;
  FName := '';
end;

procedure TBouquetNameDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  if (FDescriptorLength > 0) then
  begin
    SetLength(FName, FDescriptorLength);
    Move(ABuffer^, FName[1], FDescriptorLength);
  end;
end;


(*** TPremiereContentOrderDescriptor ******************************************)

procedure TPremiereContentOrderDescriptor.Clear;
begin
  inherited Clear;

  FOrderNumber := '';
  FOrderPrice := '';
  FOrderPhoneNumber := '';
  FSMSOrderInformation := '';
  FURLOrderInformation := '';
end;

procedure TPremiereContentOrderDescriptor.ParseBuffer(ABuffer: PByte);
var
  len: Byte;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  len := ABuffer^;
  inc(ABuffer);
  if (len > 0) then
  begin
    SetLength(FOrderNumber, len);
    Move(ABuffer^, FOrderNumber[1], len);
    inc(ABuffer, len);
  end;

  len := ABuffer^;
  inc(ABuffer);
  if (len > 0) then
  begin
    SetLength(FOrderPrice, len);
    Move(ABuffer^, FOrderPrice[1], len);
    inc(ABuffer, len);
  end;

  len := ABuffer^;
  inc(ABuffer);
  if (len > 0) then
  begin
    SetLength(FOrderPhoneNumber, len);
    Move(ABuffer^, FOrderPhoneNumber[1], len);
    inc(ABuffer, len);
  end;

  len := ABuffer^;
  inc(ABuffer);
  if (len > 0) then
  begin
    SetLength(FSMSOrderInformation, len);
    Move(ABuffer^, FSMSOrderInformation[1], len);
    inc(ABuffer, len);
  end;

  len := ABuffer^;
  inc(ABuffer);
  if (len > 0) then
  begin
    SetLength(FURLOrderInformation, len);
    Move(ABuffer^, FURLOrderInformation[1], len);
//    inc(ABuffer, len);
  end;
end;

(*** TPremiereParentalInformationDescriptor ***********************************)

procedure TPremiereParentalInformationDescriptor.Clear;
begin
  inherited Clear;

  FRating := 0;
  FControlTime1 := '';
  FControlTime2 := '';
  FParentalInformation := '';
end;

procedure TPremiereParentalInformationDescriptor.ParseBuffer(ABuffer: PByte);
var
  b1, b2, b3: Byte;
  len: Byte;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FRating := ABuffer^;
  inc(ABuffer);

  b1 := HexToDec(ABuffer^);
  inc(ABuffer);

  b2 := HexToDec(ABuffer^);
  inc(ABuffer);

  b3 := HexToDec(ABuffer^);
  inc(ABuffer);

  FControlTime1 := Format('%.2d:%.2d:%.2d', [b1, b2, b3]);

  b1 := HexToDec(ABuffer^);
  inc(ABuffer);

  b2 := HexToDec(ABuffer^);
  inc(ABuffer);

  b3 := HexToDec(ABuffer^);
  inc(ABuffer);

  FControlTime2 := Format('%.2d:%.2d:%.2d', [b1, b2, b3]);

  len := ABuffer^;
  inc(ABuffer);
  if len > 0 then
  begin
    SetLength(FParentalInformation, len);
    Move(ABuffer^, FParentalInformation[1], len);
  end;
end;

(*** TPremiereContentTransmissionDescriptor ***********************************)

function TPremiereContentTransmissionDescriptor.GetItem(AIndex: Integer): PPremiereContentTransmitionItem;
begin
  Result := FList[AIndex];
end;

procedure TPremiereContentTransmissionDescriptor.Clear;
var
  i: Integer;
  item: PPremiereContentTransmitionItem;
begin
  inherited Clear;

  FTSID := 0;
  FONID := 0;
  FSID := 0;

  for i  := 0 to FList.Count - 1 do
  begin
    item := FList[i];
    SetLength(item.StartTimes, 0);
    Dispose(item);
  end;

  FList.Clear;
end;

procedure TPremiereContentTransmissionDescriptor.ParseBuffer(ABuffer: PByte);
var
  len: Integer;
  starttime_loop_length: Integer;
  item: PPremiereContentTransmitionItem;
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FTSID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FONID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FSID := GetWord(ABuffer);
  inc(ABuffer, 2);

  len := FDescriptorLength;
  len := len - 6;

  while (len > 0) do
  begin
    new(item);
    item.StartTimesCount := 0;
    SetLength(item.StartTimes, 100);

    item.StartDate := GetMJDTime(ABuffer);
    inc(ABuffer, 2);
    dec(len, 2);

    starttime_loop_length := ABuffer^;
    inc(ABuffer);
    dec(len);

    while(starttime_loop_length > 0) do
    begin
       item.StartTimes[item.StartTimesCount] := GetBCDTime(ABuffer);
       inc(ABuffer, 3);
       dec(len, 3);
       dec(starttime_loop_length, 3);
       inc(item.StartTimesCount);
    end;

    SetLength(item.StartTimes, item.StartTimesCount);
    FList.Add(item);
  end;
end;

end.
