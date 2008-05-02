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

unit DVBEPG;

interface

{$I Compiler.inc}

uses
  Windows, DVBSettings, MPEGSections, MPEGUtils, SysUtils, BDAUtils, MPEGConst,
  MPEGDescriptors, BaseClass, DVBChannelList, BDAConst, Classes, DateUtils,
  ActiveX;

const
  EPG_HEADER = $00475045;
  EPG_VERSION = $00000002;
  EPG_CHECK_OLD_ENTRYS_DELAY = 300; //seconds
  EPG_FILE = 'EPG.data';

  EPG_DELAY = 20;

type
  TEPGHeader = packed record
    Header: Cardinal;
    Version: Cardinal;
    ServiceCount: Integer;
    Size: Integer;
  end;

  TDVBEPGComponentDescriptor = record
    StreamContent: Byte;
    ComponentType: Byte;
    ComponentTag: Byte;
    LanguageCode: String;
    Description: String;
  end;
  PDVBEPGComponentDescriptor = ^TDVBEPGComponentDescriptor;

  TDVBEPGParentalRating = record
    Country: String;
    Rating: Integer;
  end;
  PDVBEPGParentalRating = ^TDVBEPGParentalRating;

  TDVBEPGShortDescription = record
    Name: String;
    Description: String;
    Language: String;
    Valid: Boolean;
  end;

  TDVBEPGExtendedDescriptionKeyValue = record
    Key: String;
    Value: String;
  end;
  PDVBEPGExtendedDescriptionKeyValue = ^TDVBEPGExtendedDescriptionKeyValue;

  TDVBEPGPremiereLinkage = record
    TSID: Integer;
    ONID: Integer;
    SID: Integer;
    Name: String;
  end;
  PDVBEPGPremiereLinkage = ^TDVBEPGPremiereLinkage;

  TDVBEPGExtendedDescription = class
  private
    FDescription: String;
    FLanguage: String;
    FValid: Boolean;
    FList: TList;
    function GetKeyValueCount: Integer;
    function GetKeyValue(AIndex: Integer): PDVBEPGExtendedDescriptionKeyValue;
    procedure AddKeyValue(AKey: String; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseKeyValues(ADescriptor: TExtendedEventDescriptor);

    property Description: String read FDescription write FDescription;
    property Language: String read FLanguage write FLanguage;
    property Valid: Boolean read FValid write FValid;
    property KeyValueCount: Integer read GetKeyValueCount;
    property KeyValue[Index: Integer]: PDVBEPGExtendedDescriptionKeyValue read GetKeyValue; default;
  end;

  TDVBEPGEvent = class
  private
    FEventID: Integer;
    FStartTime: TDateTime;
    FDuration: TDateTime;
    FFreeToAir: Boolean;
    FRunningStatus: TRunningStatus;
    FShortDescription: TDVBEPGShortDescription;
    FExtendedDescription: TDVBEPGExtendedDescription;
    FContent: TList;
    FParentalRating: TList;
    FComponents: TList;
    FPremiereLinks: TList;
    function GetComponentsCount: Integer;
    function GetComponent(AIndex: Integer): PDVBEPGComponentDescriptor;
    function GetParentalRatingCount: Integer;
    function GetParentalRating(AIndex: Integer): PDVBEPGParentalRating;
    function GetContentCount: Integer;
    function GetContent(AIndex: Integer): PContentDescriptorItem;
    function GetPremiereLinkCount: Integer;
    function GetPremiereLink(AIndex: Integer): PDVBEPGPremiereLinkage;
    procedure AddContent(cn1, cn2, un1, un2: Byte);
    procedure AddRating(ACountry: String; ARating: Integer);
    procedure AddComponent(AStreamContent: Byte; AComponentType: Byte; AComponentTag: Byte; ALanguageCode: String; ADescription: String);
    procedure AddPremiereLink(ATSID: Integer; AONID: Integer; ASID: Integer; AName: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseEvent(AEvent: TEvent; AStartTime: TDateTime; ADuration: TDateTime);
    procedure ParseEventPremiere(AEvent: TPremiereContentInformationSection; AStartTime: TDateTime; ADuration: TDateTime);

    property EventID: Integer read FEventID write FEventID;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property Duration: TDateTime read FDuration write FDuration;
    property FreeToAir: Boolean read FFreeToAir write FFreeToAir;
    property RunningStatus: TRunningStatus read FRunningStatus write FRunningStatus;
    property ShortDescription: TDVBEPGShortDescription read FShortDescription;
    property ExtendedDescription: TDVBEPGExtendedDescription read FExtendedDescription;

    property ComponentsCount: Integer read GetComponentsCount;
    property Component[Index: Integer]: PDVBEPGComponentDescriptor read GetComponent;
    property ParentalRatingCount: Integer read GetParentalRatingCount;
    property ParentalRating[Index: Integer]: PDVBEPGParentalRating read GetParentalRating;
    property ContentCount: Integer read GetContentCount;
    property Content[Index: Integer]: PContentDescriptorItem read GetContent;
    property PremiereLinkCount: Integer read GetPremiereLinkCount;
    property PremiereLink[Index: Integer]: PDVBEPGPremiereLinkage read GetPremiereLink;
  end;

  TDVBEPGService = class
  private
    FSID: Integer;
    FTSID: Integer;
    FONID: Integer;
    FList: TList;
    function GetEventCount: Integer;
    function GetEvent(AIndex: Integer): TDVBEPGEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseEvent(AEvent: TEvent; ATimeOffset: Integer);
    procedure ParsePremiereEvent(AEvent: TPremiereContentInformationSection; ACont: TPremiereContentTransmissionDescriptor; ATimeOffset: Integer);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    property SID: Integer read FSID write FSID;
    property TSID: Integer read FTSID write FTSID;
    property ONID: Integer read FONID write FONID;
    property EventCount: Integer read GetEventCount;
    property Event[Index: Integer]: TDVBEPGEvent read GetEvent; default;
  end;

  TDVBEPGList = class
  private
    FList: TList;
    FTimeOffset: Integer;
    FNextGarbageCheck: Cardinal;
    procedure RunGarbageCollector;
    procedure RemoveOldEntrys;
    function GetServiceCount: Integer;
    function GetService(AIndex: Integer): TDVBEPGService;
  public
    constructor Create(ATimeOffset: Integer);
    destructor Destroy; override;

    procedure Clear;
    procedure Parse(ASID: Integer; ATSID: Integer; AONID: Integer; AEvent: TEvent);
    procedure ParsePremiere(ASID: Integer; ATSID: Integer; AONID: Integer; AEvent: TPremiereContentInformationSection; ACont: TPremiereContentTransmissionDescriptor);

    procedure LoadFromFile(AFilename: WideString);
    procedure SaveToFile(AFilename: WideString);

    property ServiceCount: Integer read GetServiceCount;
    property Service[Index: Integer]: TDVBEPGService read GetService; default;
  end;

  TDVBEPG = class
  private
    FLastTime: Cardinal;
    FLock: TBCCritSec;
    FSettings: TDVBSettings;
    FEPGList: TDVBEPGList;
    FEPG: TEventInformationSection;
    function GetEvent(AService: TDVBEPGService; ANow: TDateTime; out AText: String; out AStart, AEnd: TDateTime): Boolean;
    function GetEarliestEvent(AService: TDVBEPGService; ANow: TDateTime; out AText: String; out AStart, AEnd: TDateTime): Boolean;
  public
    constructor Create(ASettings: TDVBSettings);
    destructor Destroy; override;

    function GetCurrentProgram(ProgramIndex: Integer; out AName: String; out Description: String; out AStart: String; out AEnd: String): Boolean; overload;
    function GetCurrentProgram(ProgramIndex: Integer; out Description: String): Boolean; overload;
    function GetCurrentProgram(ProgramIndex: Integer; out AEvent: TDVBEPGEvent): Boolean; overload;
    function GetNowNext(ProgramIndex: Integer; out ANow: String; out ANowStart, ANowEnd: TDateTime; out ANext: String; out ANextStart, ANextEnd: TDateTime): Boolean; overload;
    function GetNowNext(ProgramIndex: Integer; out ANow: String; out ANext: String): Boolean; overload;
    function GetEPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): Boolean;
    procedure Clear;

    procedure Load;
    procedure ParseBuffer(ABuffer: PByte; ASize: Integer);
    procedure ParsePremiereCIT(ACIT: TPremiereContentInformationSection);
  end;

implementation

(*** TDVBEPGExtendedDescription ***********************************************)

constructor TDVBEPGExtendedDescription.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDVBEPGExtendedDescription.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TDVBEPGExtendedDescription.GetKeyValueCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBEPGExtendedDescription.GetKeyValue(AIndex: Integer): PDVBEPGExtendedDescriptionKeyValue;
begin
  if (AIndex < 0) or (AIndex >= FList.Count)
    then Result := nil
    else Result := FList[AIndex];
end;

procedure TDVBEPGExtendedDescription.AddKeyValue(AKey: String; AValue: String);
var
  val: PDVBEPGExtendedDescriptionKeyValue;
begin
  new(val);
  val.Key := AKey;
  val.Value := AValue;
  FList.Add(val);
end;

procedure TDVBEPGExtendedDescription.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do Dispose(PDVBEPGExtendedDescriptionKeyValue(FList[i]));
  FList.Clear;
end;

procedure TDVBEPGExtendedDescription.ParseKeyValues(ADescriptor: TExtendedEventDescriptor);
var
  i: Integer;
  it: PDVBEPGExtendedDescriptionKeyValue;
begin
  with ADescriptor do
  begin
    for i := 0 to CountItems -1 do
    begin
      new (it);
      with Item[i] do
      begin
        it.Key := Description;
        it.Value := Text;
      end;
      FList.Add(it);
    end;
  end;
end;

(*** TDVBEPGEvent *************************************************************)

constructor TDVBEPGEvent.Create;
begin
  inherited Create;
  FEventID := -1;
  FExtendedDescription := TDVBEPGExtendedDescription.Create;
  FContent := TList.Create;
  FParentalRating := TList.Create;
  FComponents := TList.Create;
  FPremiereLinks := TList.Create;
  Clear;
end;

destructor TDVBEPGEvent.Destroy;
begin
  Clear;
  FExtendedDescription.Free;
  FContent.Free;
  FParentalRating.Free;
  FComponents.Free;
  FPremiereLinks.Free;
  inherited Destroy;
end;

procedure TDVBEPGEvent.Clear;
var
  i: Integer;
begin
  for i := 0 to FContent.Count -1
    do Dispose(PContentDescriptorItem(FContent[i]));
  FContent.Clear;
  for i := 0 to FParentalRating.Count -1
    do Dispose(PDVBEPGParentalRating(FParentalRating[i]));
  FParentalRating.Clear;
  for i := 0 to FComponents.Count -1
    do Dispose(PDVBEPGComponentDescriptor(FComponents[i]));
  for i := 0 to FPremiereLinks.Count -1
    do Dispose(PDVBEPGPremiereLinkage(FPremiereLinks[i]));
  FComponents.Clear;
  FShortDescription.Valid := False;
  FExtendedDescription.Clear;
  FPremiereLinks.Clear;
end;

function TDVBEPGEvent.GetComponentsCount: Integer;
begin
  Result := FComponents.Count;
end;

function TDVBEPGEvent.GetComponent(AIndex: Integer): PDVBEPGComponentDescriptor;
begin
  if (AIndex < 0) or (AIndex >= FComponents.Count)
    then Result := nil
    else Result := FComponents[AIndex];
end;

function TDVBEPGEvent.GetContentCount: Integer;
begin
  Result := FContent.Count;
end;

function TDVBEPGEvent.GetContent(AIndex: Integer): PContentDescriptorItem;
begin
  if (AIndex < 0) or (AIndex >= FContent.Count)
    then Result := nil
    else Result := FContent[AIndex];
end;

function TDVBEPGEvent.GetPremiereLinkCount: Integer;
begin
  Result := FPremiereLinks.Count;
end;

function TDVBEPGEvent.GetPremiereLink(AIndex: Integer): PDVBEPGPremiereLinkage;
begin
  if (AIndex < 0) or (AIndex >= FPremiereLinks.Count)
    then Result := nil
    else Result := FPremiereLinks[AIndex];
end;

procedure TDVBEPGEvent.AddContent(cn1, cn2, un1, un2: Byte);
var
  val: PContentDescriptorItem;
begin
  new(val);
  val.ContentNibbleLevel1 := cn1;
  val.ContentNibbleLevel2 := cn2;
  val.UserNibbleLevel1 := un1;
  val.UserNibbleLevel2 := un2;
  FContent.Add(val);
end;

procedure TDVBEPGEvent.AddRating(ACountry: String; ARating: Integer);
var
  val: PDVBEPGParentalRating;
begin
  new(val);
  val.Country := ACountry;
  val.Rating := ARating;
  FParentalRating.Add(val);
end;

procedure TDVBEPGEvent.AddComponent(AStreamContent: Byte; AComponentType: Byte; AComponentTag: Byte; ALanguageCode: String; ADescription: String);
var
  val: PDVBEPGComponentDescriptor;
begin
  new(val);
  val.StreamContent := AStreamContent;
  val.ComponentType := AComponentType;
  val.ComponentTag := AComponentTag;
  val.LanguageCode := ALanguageCode;
  val.Description := ADescription;
  FComponents.Add(val);
end;

procedure TDVBEPGEvent.AddPremiereLink(ATSID: Integer; AONID: Integer; ASID: Integer; AName: String);
var
  val: PDVBEPGPremiereLinkage;
begin
  new(val);
  val.TSID := ATSID;
  val.ONID := AONID;
  val.SID := ASID;
  val.Name := AName;
  FPremiereLinks.Add(val);
end;

function TDVBEPGEvent.GetParentalRatingCount: Integer;
begin
  Result := FParentalRating.Count;
end;

function TDVBEPGEvent.GetParentalRating(AIndex: Integer): PDVBEPGParentalRating;
begin
  if (AIndex < 0) or (AIndex >= FParentalRating.Count)
    then Result := nil
    else Result := FParentalRating[AIndex];
end;

procedure TDVBEPGEvent.ParseEvent(AEvent: TEvent; AStartTime: TDateTime; ADuration: TDateTime);
var
  i, c: Integer;
  desc: TBaseDescriptor;
  content: PContentDescriptorItem;
  str: String;
begin
  Clear;

  FEventID := AEvent.EventID;
  FStartTime := AStartTime;
  FDuration := ADuration;
  FFreeToAir := AEvent.FreeCAMode = 0;
  FRunningStatus := AEvent.RunningStatus;

  // Parse Short Event Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_SHORT_EVENT then
    begin
      with TShortEventDescriptor(AEvent.Descriptors[i]) do
      begin
        FShortDescription.Name := Name;
        FShortDescription.Description := Description;
        FShortDescription.Language := String(Language);
        FShortDescription.Valid := True;
      end;
      break;
    end;
  end;

  // Parse Extended Event Descriptor
  FExtendedDescription.Description := '';
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_EXTENDED_EVENT then
    begin
      desc := AEvent.Descriptors[i];
      with TExtendedEventDescriptor(desc) do
      begin
        FExtendedDescription.Description := FExtendedDescription.Description + Text;
        if DescriptorNumber = LastDescriptorNumber then
        begin
          FExtendedDescription.Language := String(Language);
          FExtendedDescription.Valid := True;
          FExtendedDescription.ParseKeyValues(TExtendedEventDescriptor(desc));
          break;
        end;
      end;
    end;
  end;

  // Parse Content Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_CONTENT then
    begin
      with TContentDescriptor(AEvent.Descriptors[i]) do
      begin
        for c := 0 to CountItems -1 do
        begin
          new(content);
          content^ := Item[c];
          FContent.Add(content);
        end;
      end;
    end;
  end;

  // Parse Parental Rating Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_PARENTAL_RATING then
    begin
      with TParentalRatingDescriptor(AEvent.Descriptors[i]) do
      begin
        for c := 0 to CountItems -1 do
        begin
          with Item[c] do
          begin
            AddRating(String(CountryCode), Rating);
          end;
        end;
      end;
    end;
  end;

  // Parse Component Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_COMPONENT then
    begin
      with TComponentDescriptor(AEvent.Descriptors[i]) do
      begin
        AddComponent(StreamContent, ComponentType, ComponentTag, String(ISO639LanguageCode), Description);
      end;
    end;
  end;

  // Parse Premiere Linkage Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if (AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_LINKAGE) and
       (TLinkageDescriptor(AEvent.Descriptors[i]).LinkageType = LINKAGE_DESCRIPTOR_PREMIERE) then
    begin
      with TLinkageDescriptor(AEvent.Descriptors[i]) do
      begin
        str := '';
        if TLinkageDescriptor(AEvent.Descriptors[i]).PrivateDataLength > 0 then
        begin
          SetLength(str, TLinkageDescriptor(AEvent.Descriptors[i]).PrivateDataLength);
          Move(TLinkageDescriptor(AEvent.Descriptors[i]).PrivateData^, str[1], TLinkageDescriptor(AEvent.Descriptors[i]).PrivateDataLength);
        end;

        AddPremiereLink(TransportStreamID, OriginalNetworkID, ServiceID, str);
      end;
    end;
  end;
end;

procedure TDVBEPGEvent.ParseEventPremiere(AEvent: TPremiereContentInformationSection; AStartTime: TDateTime; ADuration: TDateTime);
var
  i, c: Integer;
  desc: TBaseDescriptor;
  content: PContentDescriptorItem;
begin
  Clear;

  FEventID := AEvent.ContentID;
  FStartTime := AStartTime;
  FDuration := ADuration;
  FFreeToAir := False;
  FRunningStatus := rsUndefined;

  // Parse Short Event Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_SHORT_EVENT then
    begin
      with TShortEventDescriptor(AEvent.Descriptors[i]) do
      begin
        FShortDescription.Name := Name;
        FShortDescription.Description := Description;
        FShortDescription.Language := String(Language);
        FShortDescription.Valid := True;
      end;
      break;
    end;
  end;

  // Parse Extended Event Descriptor
  FExtendedDescription.Description := '';
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_EXTENDED_EVENT then
    begin
      desc := AEvent.Descriptors[i];
      with TExtendedEventDescriptor(desc) do
      begin
        FExtendedDescription.Description := FExtendedDescription.Description + Text;
        if DescriptorNumber = LastDescriptorNumber then
        begin
          FExtendedDescription.Language := String(Language);
          FExtendedDescription.Valid := True;
          FExtendedDescription.ParseKeyValues(TExtendedEventDescriptor(desc));
          break;
        end;
      end;
    end;
  end;

  // Parse Content Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_CONTENT then
    begin
      with TContentDescriptor(AEvent.Descriptors[i]) do
      begin
        for c := 0 to CountItems -1 do
        begin
          new(content);
          content^ := Item[c];
          FContent.Add(content);
        end;
      end;
    end;
  end;

  // Parse Parental Rating Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_PARENTAL_RATING then
    begin
      with TParentalRatingDescriptor(AEvent.Descriptors[i]) do
      begin
        for c := 0 to CountItems -1 do
        begin
          with Item[c] do
          begin
            AddRating(String(CountryCode), Rating);
          end;
        end;
      end;
    end;
  end;

  // Parse Component Descriptor
  for i := 0 to AEvent.Descriptors.Count -1 do
  begin
    if AEvent.Descriptors[i].Tag = DESCRIPTOR_TAG_COMPONENT then
    begin
      with TComponentDescriptor(AEvent.Descriptors[i]) do
      begin
        AddComponent(StreamContent, ComponentType, ComponentTag, String(ISO639LanguageCode), Description);
      end;
    end;
  end;
end;

(*** TDVBEPGService ***********************************************************)

constructor TDVBEPGService.Create;
begin
  inherited Create;
  FSID := -1;
  FTSID := -1;
  FONID := -1;
  FList := TList.Create;
end;

destructor TDVBEPGService.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TDVBEPGService.GetEventCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBEPGService.GetEvent(AIndex: Integer): TDVBEPGEvent;
begin
  if (AIndex < 0) or (AIndex >= FList.Count)
    then Result := nil
    else Result := FList[AIndex];
end;

procedure TDVBEPGService.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDVBEPGEvent(FList[i]).Free;
  FList.Clear;
end;

procedure TDVBEPGService.ParseEvent(AEvent: TEvent; ATimeOffset: Integer);
var
  event: TDVBEPGEvent;
  i: Integer;
  start_time, duration: TDateTime;
{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  n: TDateTime;
{$ENDIF}
begin
{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  n := IncMinute(Now, -ATimeOffset);
{$ENDIF}
  start_time := GetDateTimeFromUTCTime(AEvent.StartTime);
  duration := GetDateTimeFromBCDTime(AEvent.Duration);

{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  if (start_time + duration < n)
    then Exit;
{$ENDIF}

  for i := 0 to FList.Count -1 do
  begin
    event := FList[i];
    if (event.FEventID = AEvent.EventID) then
    begin
      event.ParseEvent(AEvent, start_time, duration);
      Exit;
    end;
  end;

{$IFDEF EPG_CHECK_DUPLICATE_STARTTIME}
  for i := 0 to FList.Count -1 do
  begin
    event := FList[i];
    if (event.FStartTime = start_time) then
    begin
      FList.Delete(i);
      event.Free;
      break;
    end;
  end;
{$ENDIF}

  event := TDVBEPGEvent.Create;
  event.ParseEvent(AEvent, start_time, duration);
  FList.Add(event);
end;

procedure TDVBEPGService.ParsePremiereEvent(AEvent: TPremiereContentInformationSection; ACont: TPremiereContentTransmissionDescriptor; ATimeOffset: Integer);
var
  event: TDVBEPGEvent;
  i: Integer;
  c, k: Integer;
  start_time, start_time2, duration: TDateTime;
  found: Boolean;
{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  n: TDateTime;
{$ENDIF}
begin
{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  n := IncMinute(Now, -ATimeOffset);
{$ENDIF}

  for i := 0 to ACont.CountItems - 1 do
  begin
    start_time := GetDateTimeFromMJDTime(ACont.StartDate[i].StartDate);
    duration := IncSecond(0, AEvent.Duration);
    for c := 0 to ACont.StartDate[i].StartTimesCount - 1 do
    begin
      start_time2 := start_time + GetDateTimeFromBCDTime(ACont.StartDate[i].StartTimes[c]);

{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
      if (start_time2 + duration < n)
        then Continue;
{$ENDIF}

      found := False;

      for k := 0 to FList.Count -1 do
      begin
        event := FList[k];
        if (Cardinal(event.FEventID) = AEvent.ContentID) and (event.FStartTime = start_time) then
        begin
          event.ParseEventPremiere(AEvent, start_time2, duration);
          found := True;
          break;
        end;
      end;

{$IFDEF EPG_CHECK_DUPLICATE_STARTTIME}
      for k := 0 to FList.Count -1 do
      begin
        event := FList[k];
        if (event.FStartTime = start_time2) then
        begin
          FList.Delete(k);
          event.Free;
          break;
        end;
      end;
{$ENDIF}

      if not found then
      begin
        event := TDVBEPGEvent.Create;
        event.ParseEventPremiere(AEvent, start_time2, duration);
        FList.Add(event);
      end;
    end;
  end;
end;

procedure TDVBEPGService.LoadFromStream(AStream: TStream);
var
  i, k, l: Integer;
  event: TDVBEPGEvent;
  b1, b2, b3, b4: Byte;
  s1, s2: String;
  i1, i2, i3: Integer;

  function ReadInteger: Integer;
  begin
    AStream.Read(Result, SizeOf(Integer));
  end;

  function ReadDateTime: TDateTime;
  begin
    AStream.Read(Result, SizeOf(TDateTime));
  end;

  function ReadByte: Byte;
  begin
    AStream.Read(Result, SizeOf(Byte));
  end;

  function ReadString: String;
  begin
    AStream.Read(l, SizeOf(Integer));
    if l > 0 then
    begin
      SetLength(Result, l);
      AStream.Read(Result[1], l);
    end else
    begin
      Result := '';
    end;
  end;

begin
  AStream.Read(FSID, SizeOf(Integer));
  AStream.Read(FTSID, SizeOf(Integer));
  AStream.Read(FONID, SizeOf(Integer));
  for i := 0 to ReadInteger -1 do
  begin
    event := TDVBEPGEvent.Create;
    FList.Add(event);
    event.FEventID := ReadInteger;
    event.FStartTime := ReadDateTime;
    event.FDuration := ReadDateTime;
    event.FRunningStatus := TRunningStatus(ReadByte);
    event.FFreeToAir := Boolean(ReadByte);

    AStream.Read(event.FShortDescription.Valid, SizeOf(Byte));
    if event.FShortDescription.Valid then
    begin
      event.FShortDescription.Name := ReadString;
      event.FShortDescription.Description := ReadString;
      event.FShortDescription.Language := ReadString;
    end;

    AStream.Read(event.FExtendedDescription.FValid, SizeOf(Byte));
    if event.FExtendedDescription.FValid then
    begin
      event.FExtendedDescription.FLanguage := ReadString;
      event.FExtendedDescription.FDescription := ReadString;
      for k := 0 to ReadInteger -1 do
      begin
        s1 := ReadString;
        s2 := ReadString;
        event.FExtendedDescription.AddKeyValue(s1, s2);
      end;
    end;

    for k := 0 to ReadInteger -1 do
    begin
      b1 := ReadByte;
      b2 := ReadByte;
      b3 := ReadByte;
      b4 := ReadByte;
      event.AddContent(b1, b2, b3, b4);
    end;

    for k := 0 to ReadInteger -1 do
    begin
      s1 := ReadString;
      i1 := ReadInteger;
      event.AddRating(s1, i1);
    end;

    for k := 0 to ReadInteger -1 do
    begin
      b1 := ReadByte;
      b2 := ReadByte;
      b3 := ReadByte;
      s1 := ReadString;
      s2 := ReadString;
      event.AddComponent(b1, b2, b3, s1, s2);
    end;

    for k := 0 to ReadInteger -1 do
    begin
      i1 := ReadInteger;
      i2 := ReadInteger;
      i3 := ReadInteger;
      s1 := ReadString;
      event.AddPremiereLink(i1, i2, i3, s1);
    end;
  end;
end;

procedure TDVBEPGService.SaveToStream(AStream: TStream);
var
  c, k, l: Integer;
  event: TDVBEPGEvent;

  procedure WriteString(AString: String);
  begin
    l := Length(AString);
    AStream.Write(l, SizeOf(Integer));
    if l > 0 then AStream.Write(AString[1], l);
  end;

begin
  AStream.Write(FSID, SizeOf(Integer));
  AStream.Write(FTSID, SizeOf(Integer));
  AStream.Write(FONID, SizeOf(Integer));
  AStream.Write(FList.Count, SizeOf(Integer));
  for c := 0 to FList.Count -1 do
  begin
    event := FList[c];
    AStream.Write(event.FEventID, SizeOf(Integer));
    AStream.Write(event.FStartTime, SizeOf(TDateTime));
    AStream.Write(event.FDuration, SizeOf(TDateTime));
    AStream.Write(event.FRunningStatus, SizeOf(Byte));
    AStream.Write(event.FFreeToAir, SizeOf(Byte));

    AStream.Write(event.FShortDescription.Valid, SizeOf(Byte));
    if event.FShortDescription.Valid then
    begin
      WriteString(event.FShortDescription.Name);
      WriteString(event.FShortDescription.Description);
      WriteString(event.FShortDescription.Language);
    end;

    AStream.Write(event.FExtendedDescription.FValid, SizeOf(Byte));
    if event.FExtendedDescription.FValid then
    begin
      WriteString(event.FExtendedDescription.FLanguage);
      WriteString(event.FExtendedDescription.FDescription);
      AStream.Write(event.FExtendedDescription.FList.Count, SizeOf(Integer));
      for k := 0 to event.FExtendedDescription.FList.Count -1 do
      begin
        with PDVBEPGExtendedDescriptionKeyValue(event.FExtendedDescription.FList[k])^ do
        begin
          WriteString(Key);
          WriteString(Value);
        end;
      end;
    end;

    AStream.Write(event.FContent.Count, SizeOf(Integer));
    for k := 0 to event.FContent.Count -1 do
    begin
      with PContentDescriptorItem(event.FContent[k])^ do
      begin
        AStream.Write(ContentNibbleLevel1, SizeOf(ContentNibbleLevel1));
        AStream.Write(ContentNibbleLevel2, SizeOf(ContentNibbleLevel2));
        AStream.Write(UserNibbleLevel1, SizeOf(UserNibbleLevel1));
        AStream.Write(UserNibbleLevel2, SizeOf(UserNibbleLevel2));
      end;
    end;

    AStream.Write(event.FParentalRating.Count, SizeOf(Integer));
    for k := 0 to event.FParentalRating.Count -1 do
    begin
      with PDVBEPGParentalRating(event.FParentalRating[k])^ do
      begin
        WriteString(Country);
        AStream.Write(Rating, SizeOf(Rating));
      end;
    end;

    AStream.Write(event.FComponents.Count, SizeOf(Integer));
    for k := 0 to event.FComponents.Count -1 do
    begin
      with PDVBEPGComponentDescriptor(event.FComponents[k])^ do
      begin
        AStream.Write(StreamContent, SizeOf(StreamContent));
        AStream.Write(ComponentType, SizeOf(ComponentType));
        AStream.Write(ComponentTag, SizeOf(ComponentTag));
        WriteString(LanguageCode);
        WriteString(Description);
      end;
    end;

    AStream.Write(event.FPremiereLinks.Count, SizeOf(Integer));
    for k := 0 to event.FPremiereLinks.Count -1 do
    begin
      with PDVBEPGPremiereLinkage(event.FPremiereLinks[k])^ do
      begin
        AStream.Write(TSID, SizeOf(Integer));
        AStream.Write(ONID, SizeOf(Integer));
        AStream.Write(SID, SizeOf(Integer));
        WriteString(Name);
      end;
    end;
  end;
end;

(*** TDVBEPGList **************************************************************)

constructor TDVBEPGList.Create(ATimeOffset: Integer);
begin
  inherited Create;
  FNextGarbageCheck := 0;
  FTimeOffset := ATimeOffset;
  FList := TList.Create;
end;

destructor TDVBEPGList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TDVBEPGList.GetServiceCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBEPGList.GetService(AIndex: Integer): TDVBEPGService;
begin
  if (AIndex < 0) or (AIndex >= FList.Count)
    then Result := nil
    else Result := FList[AIndex];
end;

procedure TDVBEPGList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDVBEPGService(FList[i]).Free;
  FList.Clear;
end;

procedure TDVBEPGList.Parse(ASID: Integer; ATSID: Integer; AONID: Integer; AEvent: TEvent);
var
  i: Integer;
  service: TDVBEPGService;
begin
  RunGarbageCollector;

  for i := 0 to FList.Count -1 do
  begin
    service := FList[i];
    if (service.FSID = ASID) and (service.FTSID = ATSID) and (service.FONID = AONID) then
    begin
      service.ParseEvent(AEvent, FTimeOffset);
      Exit;
    end;
  end;

  service := TDVBEPGService.Create;
  service.FSID := ASID;
  service.FTSID := ATSID;
  service.FONID := AONID;

  FList.Add(service);
  service.ParseEvent(AEvent, FTimeOffset);
end;

procedure TDVBEPGList.ParsePremiere(ASID: Integer; ATSID: Integer; AONID: Integer; AEvent: TPremiereContentInformationSection; ACont: TPremiereContentTransmissionDescriptor);
var
  i: Integer;
  service: TDVBEPGService;
begin
  RunGarbageCollector;

  for i := 0 to FList.Count -1 do
  begin
    service := FList[i];
    if (service.FSID = ASID) and (service.FTSID = ATSID) and (service.FONID = AONID) then
    begin
      service.ParsePremiereEvent(AEvent, ACont, FTimeOffset);
      Exit;
    end;
  end;

  service := TDVBEPGService.Create;
  service.FSID := ASID;
  service.FTSID := ATSID;
  service.FONID := AONID;

  FList.Add(service);
  service.ParsePremiereEvent(AEvent, ACont, FTimeOffset);
end;

procedure TDVBEPGList.RunGarbageCollector;
begin
{$IFDEF EPG_DONT_ADD_VALUES_FROM_PAST}
  if FNextGarbageCheck < GetTickCount then
  begin
    FNextGarbageCheck := GetTickCount + (EPG_CHECK_OLD_ENTRYS_DELAY * 1000);
    RemoveOldEntrys;
  end;
{$ENDIF}
end;

procedure TDVBEPGList.RemoveOldEntrys;
var
  i, c: Integer;
  service: TDVBEPGService;
  event: TDVBEPGEvent;
  n: TDateTime;
begin
  n := IncMinute(Now, -FTimeOffset);

  for i := 0 to FList.Count -1 do
  begin
    service := FList[i];
    for c := service.FList.Count -1 downto 0 do
    begin
      event := service.FList[c];
      if (event.FStartTime + event.FDuration < n) then
      begin
        event.Free;
        service.FList.Delete(c);
      end;
    end;
  end;
end;

procedure TDVBEPGList.LoadFromFile(AFilename: WideString);
var
  fs: TFileStream;
  header: TEPGHeader;
  i: Integer;
  service: TDVBEPGService;
  stream: TMemoryStream;
begin
  Clear;

  try
    fs := TFileStream.Create(AFilename, fmOpenRead);
  except
    Exit;
  end;

  if fs.Size < SizeOf(header) then
  begin
    fs.Free;
    Exit;
  end;

  stream := TMemoryStream.Create;
  stream.SetSize(fs.Size);
  fs.Read(PChar(stream.Memory)^, stream.Size);
  fs.Free;

  stream.Read(header, SizeOf(header));
  if (header.Header <> EPG_HEADER) or
     (header.Version <> EPG_VERSION) or
     (header.ServiceCount = 0) or
     (header.Size <> stream.Size) then
  begin
    stream.Free;
    Exit;
  end;

  for i := 0 to header.ServiceCount -1 do
  begin
    service := TDVBEPGService.Create;
    FList.Add(service);
    service.LoadFromStream(stream);
  end;

  stream.Free;
end;

procedure TDVBEPGList.SaveToFile(AFilename: WideString);
var
  fs: TFileStream;
  stream: TMemoryStream;
  i: Integer;
  service: TDVBEPGService;
  header: TEPGHeader;
begin
  stream := TMemoryStream.Create;
  header.Header := EPG_HEADER;
  header.Version := EPG_VERSION;
  header.ServiceCount := FList.Count;
  stream.Write(header, SizeOf(header));

  for i:= 0 to FList.Count -1 do
  begin
    service := FList[i];
    service.SaveToStream(stream);
  end;

  stream.Seek(0, soFromBeginning);
  header.Size := stream.Size;
  stream.Write(header, SizeOf(header)) ;

  try
    fs := TFileStream.Create(AFilename, fmCreate);
    fs.Write(PChar(stream.Memory)^, stream.Size);
    fs.Free;
  except
  end;

  stream.Free;
end;

(*** TDVBEPG ******************************************************************)

constructor TDVBEPG.Create(ASettings: TDVBSettings);
begin
  inherited Create;
  FSettings := ASettings;
  FEPGList := TDVBEPGList.Create(FSettings.EPGTimeOffset);
  FLock := TBCCritSec.Create;
  FEPG := TEventInformationSection.Create;
  FLastTime := GetTickCount;
end;

destructor TDVBEPG.Destroy;
begin
  if not FSettings.GraphFailed then
  begin
    FEPGList.SaveToFile(FSettings.EPGDirectory + EPG_FILE);
  end;
  FEPG.Free;
  FEPGList.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TDVBEPG.Load;
begin
  if FileExists(FSettings.EPGDirectory + EPG_FILE) then
  begin
    FEPGList.LoadFromFile(FSettings.EPGDirectory + EPG_FILE);
    FEPGList.RemoveOldEntrys;
  end;
end;

function TDVBEPG.GetCurrentProgram(ProgramIndex: Integer; out AName: String; out Description: String; out AStart: String; out AEnd: String): Boolean;
var
  channel: TDVBChannel;
  i, c: Integer;
  service: TDVBEPGService;
  event: TDVBEPGEvent;
  n, start_time, stop_time: TDateTime;
begin
  Result := False;
  Description := '';
  AName := '';
  AStart := '';
  AEnd := '';

  if (ProgramIndex < 0) or (ProgramIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ProgramIndex];
  n := IncMinute(Now, -FSettings.EPGTimeOffset);

  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        for c := 0 to service.FList.Count -1 do
        begin
          event := service.FList[c];
          start_time := event.FStartTime;
          stop_time := start_time + event.FDuration;
          if (n >= start_time) and (n <= stop_time) then
          begin
            AName := RemoveUnwantedChars(event.FShortDescription.Name);
            Description := RemoveUnwantedChars(event.FExtendedDescription.FDescription);
            AStart := FormatDateTime('hh:nn', IncMinute(start_time, FSettings.EPGTimeOffset));
            AEnd := FormatDateTime('hh:nn', IncMinute(stop_time, FSettings.EPGTimeOffset));
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBEPG.GetCurrentProgram(ProgramIndex: Integer; out Description: String): Boolean;
var
  channel: TDVBChannel;
  i, c: Integer;
  service: TDVBEPGService;
  event: TDVBEPGEvent;
  n, start_time, stop_time: TDateTime;
begin
  Result := False;
  Description := '';
  if (ProgramIndex < 0) or (ProgramIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ProgramIndex];
  n := IncMinute(Now, -FSettings.EPGTimeOffset);

  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        for c := 0 to service.FList.Count -1 do
        begin
          event := service.FList[c];
          start_time := event.FStartTime;
          stop_time := start_time + event.FDuration;
          if (n >= start_time) and (n <= stop_time) then
          begin
            Description := '[' + FormatDateTime('hh:nn', IncMinute(start_time, FSettings.EPGTimeOffset)) + ' - ' +
                                 FormatDateTime('hh:nn', IncMinute(stop_time, FSettings.EPGTimeOffset)) + ']  ' + event.FShortDescription.Name;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBEPG.GetCurrentProgram(ProgramIndex: Integer; out AEvent: TDVBEPGEvent): Boolean;
var
  channel: TDVBChannel;
  i, c: Integer;
  service: TDVBEPGService;
  event: TDVBEPGEvent;
  n, start_time, stop_time: TDateTime;
begin
  Result := False;
  AEvent := nil;

  if (ProgramIndex < 0) or (ProgramIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ProgramIndex];
  n := IncMinute(Now, -FSettings.EPGTimeOffset);

  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        for c := 0 to service.FList.Count -1 do
        begin
          event := service.FList[c];
          start_time := event.FStartTime;
          stop_time := start_time + event.FDuration;
          if (n >= start_time) and (n <= stop_time) then
          begin
            AEvent := event;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBEPG.GetEvent(AService: TDVBEPGService; ANow: TDateTime; out AText: String; out AStart, AEnd: TDateTime): Boolean;
var
  i: Integer;
  event: TDVBEPGEvent;
  start_time, stop_time: TDateTime;
begin
  Result := False;

  for i := 0 to AService.FList.Count -1 do
  begin
    event := AService.FList[i];
    start_time := event.FStartTime;
    stop_time := start_time + event.FDuration;
    if ((ANow >= start_time) and (ANow <= stop_time)) then
    begin
      Result := True;
      AStart := IncMinute(start_time, FSettings.EPGTimeOffset);
      AEnd := IncMinute(stop_time, FSettings.EPGTimeOffset);
      AText := RemoveUnwantedChars(event.FShortDescription.Name);
      Exit;
    end;
  end;
end;

function TDVBEPG.GetEarliestEvent(AService: TDVBEPGService; ANow: TDateTime; out AText: String; out AStart, AEnd: TDateTime): Boolean;
var
  i: Integer;
  idx: Integer;
  event: TDVBEPGEvent;
  start_time, stop_time, s2: TDateTime;
begin
  idx := -1;
  s2 := 0;
  for i := 0 to AService.FList.Count -1 do
  begin
    event := AService.FList[i];
    start_time := event.StartTime;
    if start_time >= ANow then
    begin
      if s2 = 0 then
      begin
        idx := i;
        s2 := start_time;
      end else
      begin
        if start_time < s2 then
        begin
          idx := i;
          s2 := start_time;
        end;
      end;
    end;
  end;

  Result := idx <> -1;
  if not Result
    then Exit;

  event := AService.FList[idx];
  start_time := event.FStartTime;
  stop_time := start_time + event.FDuration;
  AStart := IncMinute(start_time, FSettings.EPGTimeOffset);
  AEnd := IncMinute(stop_time, FSettings.EPGTimeOffset);
  AText := RemoveUnwantedChars(event.FShortDescription.Name);
end;

function TDVBEPG.GetNowNext(ProgramIndex: Integer; out ANow: String; out ANowStart, ANowEnd: TDateTime; out ANext: String; out ANextStart, ANextEnd: TDateTime): Boolean;
var
  channel: TDVBChannel;
  i: Integer;
  service: TDVBEPGService;
  n: TDateTime;
begin
  Result := False;
  ANow := '';
  ANext := '';
  ANowStart := 0;
  ANowEnd := 0;
  ANextStart := 0;
  ANextEnd := 0;

  if (ProgramIndex < 0) or (ProgramIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ProgramIndex];
  n := IncMinute(Now, -FSettings.EPGTimeOffset);

  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        if GetEvent(service, n, ANow, ANowStart, ANowEnd) then
        begin
          Result := True;
          GetEarliestEvent(service, IncMinute(ANowEnd, -FSettings.EPGTimeOffset-1), ANext, ANextStart, ANextEnd);
          Exit;
        end else
        begin
          if GetEarliestEvent(service, n, ANext, ANextStart, ANextEnd) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBEPG.GetNowNext(ProgramIndex: Integer; out ANow: String; out ANext: String): Boolean;
var
  channel: TDVBChannel;
  i: Integer;
  service: TDVBEPGService;
  n, start_time, stop_time: TDateTime;
begin
  Result := False;
  ANow := '';
  ANext := '';

  if (ProgramIndex < 0) or (ProgramIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ProgramIndex];
  n := IncMinute(Now, -FSettings.EPGTimeOffset);

  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        if GetEvent(service, n, ANow, start_time, stop_time) then
        begin
          Result := True;
          ANow := FormatDateTime('hh:nn', start_time) + ' - ' + FormatDateTime('hh:nn', stop_time) + '  ' + ANow;
          if GetEarliestEvent(service, IncMinute(stop_time, -FSettings.EPGTimeOffset-1), ANext, start_time, stop_time) then
          begin
            ANext := FormatDateTime('hh:nn', start_time) + ' - ' + FormatDateTime('hh:nn', stop_time) + '  ' + ANext;
          end;
          Exit;
        end else
        begin
          if GetEarliestEvent(service, n, ANext, start_time, stop_time) then
          begin
            ANext := FormatDateTime('hh:nn', start_time) + ' - ' + FormatDateTime('hh:nn', stop_time) + '  ' + ANext;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

function TDVBEPG.GetEPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): Boolean;
var
  channel: TDVBChannel;
  i: Integer;
  service: TDVBEPGService;
  stream: TMemoryStream;
  header: TEPGHeader;
begin
  Result := False;
  EPG := nil;
  Size := 0;
  if (ChannelIndex < 0) or (ChannelIndex >= FSettings.Channels.Count)
    then Exit;

  channel := FSettings.Channels[ChannelIndex];
  FLock.Lock;
  try
    for i := 0 to FEPGList.FList.Count -1 do
    begin
      service := FEPGList.FList[i];
      if (service.FSID = channel.SID) and
         (service.FTSID = channel.Network.TSID) and
         (service.FONID = channel.Network.ONID) then
      begin
        stream := TMemoryStream.Create;
        header.Header := EPG_HEADER;
        header.Version := EPG_VERSION;
        header.ServiceCount := 1;
        stream.Write(header, SizeOf(header));

        service.SaveToStream(stream);

        stream.Seek(0, soFromBeginning);
        header.Size := stream.Size;
        stream.Write(header, SizeOf(header));
        stream.Seek(0, soFromBeginning);

        Size := stream.Size;
        EPG := CoTaskMemAlloc(Size);
        stream.Read(EPG^, Size);
        stream.Free;

        Result := True;
        Exit;
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

procedure TDVBEPG.Clear;
begin
  FLock.Lock;
  try
    FEPGList.Clear;
  finally
    FLock.UnLock;
  end;
end;

(*** IEPGNIFilterEPGCallBack **************************************************)

procedure TDVBEPG.ParseBuffer(ABuffer: PByte; ASize: Integer);
var
  i: Integer;
begin
  if GetTickCount > FLastTime then
  begin
    FLastTime := GetTickCount + EPG_DELAY;

    FLock.Lock;
    try
      if FEPG.ParseBuffer(ABuffer, ASize) and FEPG.Valid then
      begin
//        PrintEIT(FEPG);
        for i := 0 to FEPG.EventList.Count -1 do
        begin
          FEPGList.Parse(FEPG.ServiceID, FEPG.TransportStreamID, FEPG.OriginalNetworkID, FEPG.EventList[i]);
        end;
      end;
    finally
      FLock.UnLock;
    end;
  end;
end;

procedure TDVBEPG.ParsePremiereCIT(ACIT: TPremiereContentInformationSection);
var
  i: Integer;
  cont: TPremiereContentTransmissionDescriptor;
begin
//[1320] ---------------------- TABLE_ID_PREMIERE_CIT ---------------------
//[1320] |
//[1320] |  ContentID: 2759056
//[1320] |  Duration: 7821 seconds
//[1320] |  DescriptorLoopLength: 826
//[1320] |
//[1320] |  Number of Descriptors 10
//[1320] |
//[1320] |    Descriptor 0
//[1320] |
//[1320] |      Tag: Descriptor (0x54) -> DESCRIPTOR_TAG_CONTENT
//[1320] |
//[1320] |        Number of Items: 1
//[1320] |
//[1320] |          Item: 0
//[1320] |
//[1320] |            Content: Unknown Content (reserved or user defined)
//[1320] |            User Nibble 1: 0
//[1320] |            User Nibble 1: 1
//[1320] |
//[1320] |    Descriptor 1
//[1320] |
//[1320] |      Tag: Descriptor (0x4D) -> DESCRIPTOR_TAG_SHORT_EVENT
//[1320] |
//[1320] |        Language: DEU
//[1320] |        EventName: Die Insel
//[1320] |        Description Science-Fiction
//[1320] |
//[1320] |    Descriptor 2
//[1320] |
//[1320] |      Tag: Descriptor (0x4E) -> DESCRIPTOR_TAG_EXTENDED_EVENT
//[1320] |
//[1320] |        DescriptorNumber: 0
//[1320] |        LastDescriptorNumber: 1
//[1320] |        Language: DEU
//[1320] |        Text: Lincoln Six-Echo (Ewan McGregor) lebt mit tausenden anderen in einer futuristischen Wohneinheit. Angeblich darf man von hier irgendwann auf "Die Insel". Aber Lincoln entdeckt, dass alles nur Lüge ist: Die Bewohner dienen als Ersatzteilspender für ih
//[1320] |
//[1320] |        Number of Items: 0
//[1320] |
//[1320] |    Descriptor 3
//[1320] |
//[1320] |      Tag: Descriptor (0x4E) -> DESCRIPTOR_TAG_EXTENDED_EVENT
//[1320] |
//[1320] |        DescriptorNumber: 1
//[1320] |        LastDescriptorNumber: 1
//[1320] |        Language: DEU
//[1320] |        Text: re menschlichen Originale. Mit der schönen Jordan Two-Delta (Scarlett Johansson) wagt er die Flucht. - Mitreißendes Sci-Fi-Spektakel von Action-Großmeister Michael Bay.
//[1320] |
//[1320] |        Number of Items: 0
//[1320] |
//[1320] |    Descriptor 4
//[1320] |
//[1320] |      Tag: Descriptor (0xF0) -> DESCRIPTOR_TAG_PREMIERE_CONTENT_ORDER
//[1320] |
//[1320] |        OrderNumber: 465
//[1320] |        OrderPrice: ¤ 3
//[1320] |        OrderPhoneNumber: Orderline D:0180-5530000 A:01-49168800 (Preis s. Programm-Magazin)
//[1320] |        SMSOrderInformation: 
//[1320] |        URLOrderInformation: 
//[1320] |
//[1320] |    Descriptor 5
//[1320] |
//[1320] |      Tag: Descriptor (0xF1) -> DESCRIPTOR_TAG_PREMIERE_PARENTAL_INFORMATION
//[1320] |
//[1320] |        Rating: 13
//[1320] |        ControlTime1: 05:00:00
//[1320] |        ControlTime2: 21:00:00
//[1320] |        ParentalInformation: ab 16 Jahren
//[1320] |
//[1320] |    Descriptor 6
//[1320] |
//[1320] |      Tag: Descriptor (0xF2) -> DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION
//[1320] |
//[1320] |        TSID: 1
//[1320] |        ONID: 133
//[1320] |        SID: 212
//[1320] |
//[1320] |        Number of StartDates: 4
//[1320] |
//[1320] |          StartDate Index: 0
//[1320] |
//[1320] |            StartDate: 08.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 8
//[1320] |
//[1320] |              StartTime 0: 06:00:24
//[1320] |              StartTime 1: 08:30:24
//[1320] |              StartTime 2: 11:00:24
//[1320] |              StartTime 3: 13:30:24
//[1320] |              StartTime 4: 16:00:24
//[1320] |              StartTime 5: 18:30:24
//[1320] |              StartTime 6: 21:00:24
//[1320] |              StartTime 7: 23:30:24
//[1320] |
//[1320] |          StartDate Index: 1
//[1320] |
//[1320] |            StartDate: 09.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 8
//[1320] |
//[1320] |              StartTime 0: 06:00:24
//[1320] |              StartTime 1: 08:30:24
//[1320] |              StartTime 2: 11:00:24
//[1320] |              StartTime 3: 13:30:24
//[1320] |              StartTime 4: 16:00:24
//[1320] |              StartTime 5: 18:30:24
//[1320] |              StartTime 6: 21:00:24
//[1320] |              StartTime 7: 23:30:24
//[1320] |
//[1320] |          StartDate Index: 2
//[1320] |
//[1320] |            StartDate: 10.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 6
//[1320] |
//[1320] |              StartTime 0: 05:30:24
//[1320] |              StartTime 1: 12:00:24
//[1320] |              StartTime 2: 14:30:24
//[1320] |              StartTime 3: 17:00:24
//[1320] |              StartTime 4: 19:30:24
//[1320] |              StartTime 5: 22:00:24
//[1320] |
//[1320] |          StartDate Index: 3
//[1320] |
//[1320] |            StartDate: 11.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 1
//[1320] |
//[1320] |              StartTime 0: 00:30:24
//[1320] |
//[1320] |    Descriptor 7
//[1320] |
//[1320] |      Tag: Descriptor (0xF2) -> DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION
//[1320] |
//[1320] |        TSID: 3
//[1320] |        ONID: 133
//[1320] |        SID: 241
//[1320] |
//[1320] |        Number of StartDates: 2
//[1320] |
//[1320] |          StartDate Index: 0
//[1320] |
//[1320] |            StartDate: 08.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 5
//[1320] |
//[1320] |              StartTime 0: 09:30:24
//[1320] |              StartTime 1: 12:00:24
//[1320] |              StartTime 2: 14:30:24
//[1320] |              StartTime 3: 17:00:24
//[1320] |              StartTime 4: 19:30:24
//[1320] |
//[1320] |          StartDate Index: 1
//[1320] |
//[1320] |            StartDate: 10.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 1
//[1320] |
//[1320] |              StartTime 0: 09:30:24
//[1320] |
//[1320] |    Descriptor 8
//[1320] |
//[1320] |      Tag: Descriptor (0xF2) -> DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION
//[1320] |
//[1320] |        TSID: 3
//[1320] |        ONID: 133
//[1320] |        SID: 244
//[1320] |
//[1320] |        Number of StartDates: 3
//[1320] |
//[1320] |          StartDate Index: 0
//[1320] |
//[1320] |            StartDate: 08.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 5
//[1320] |
//[1320] |              StartTime 0: 10:30:24
//[1320] |              StartTime 1: 13:00:24
//[1320] |              StartTime 2: 15:30:24
//[1320] |              StartTime 3: 18:00:24
//[1320] |              StartTime 4: 20:30:24
//[1320] |
//[1320] |          StartDate Index: 1
//[1320] |
//[1320] |            StartDate: 09.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 1
//[1320] |
//[1320] |              StartTime 0: 19:00:24
//[1320] |
//[1320] |          StartDate Index: 2
//[1320] |
//[1320] |            StartDate: 10.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 3
//[1320] |
//[1320] |              StartTime 0: 16:00:24
//[1320] |              StartTime 1: 18:30:24
//[1320] |              StartTime 2: 21:00:24
//[1320] |
//[1320] |    Descriptor 9
//[1320] |
//[1320] |      Tag: Descriptor (0xF2) -> DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION
//[1320] |
//[1320] |        TSID: 4
//[1320] |        ONID: 133
//[1320] |        SID: 209
//[1320] |
//[1320] |        Number of StartDates: 4
//[1320] |
//[1320] |          StartDate Index: 0
//[1320] |
//[1320] |            StartDate: 08.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 8
//[1320] |
//[1320] |              StartTime 0: 04:00:24
//[1320] |              StartTime 1: 06:30:24
//[1320] |              StartTime 2: 09:00:24
//[1320] |              StartTime 3: 11:30:24
//[1320] |              StartTime 4: 14:00:24
//[1320] |              StartTime 5: 16:30:24
//[1320] |              StartTime 6: 19:00:24
//[1320] |              StartTime 7: 21:30:24
//[1320] |
//[1320] |          StartDate Index: 1
//[1320] |
//[1320] |            StartDate: 09.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 9
//[1320] |
//[1320] |              StartTime 0: 00:00:24
//[1320] |              StartTime 1: 04:30:24
//[1320] |              StartTime 2: 07:00:24
//[1320] |              StartTime 3: 09:30:24
//[1320] |              StartTime 4: 12:00:24
//[1320] |              StartTime 5: 14:30:24
//[1320] |              StartTime 6: 17:00:24
//[1320] |              StartTime 7: 19:30:24
//[1320] |              StartTime 8: 22:00:24
//[1320] |
//[1320] |          StartDate Index: 2
//[1320] |
//[1320] |            StartDate: 10.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 6
//[1320] |
//[1320] |              StartTime 0: 00:30:24
//[1320] |              StartTime 1: 05:00:24
//[1320] |              StartTime 2: 14:00:24
//[1320] |              StartTime 3: 16:30:24
//[1320] |              StartTime 4: 19:00:24
//[1320] |              StartTime 5: 21:30:24
//[1320] |
//[1320] |          StartDate Index: 3
//[1320] |
//[1320] |            StartDate: 11.06.2006
//[1320] |
//[1320] |            Number of StartTimes: 1
//[1320] |
//[1320] |              StartTime 0: 00:00:24
//[1320] |

  PrintPremiereCIT(ACIT);
  for i := 0 to ACIT.Descriptors.Count - 1 do
  begin
    if (ACIT.Descriptors[i].Tag = DESCRIPTOR_TAG_PREMIERE_CONTENT_TRANSMITION) then
    begin
      cont := TPremiereContentTransmissionDescriptor(ACIT.Descriptors[i]);
      FEPGList.ParsePremiere(cont.SID, cont.TSID, cont.ONID, ACIT, cont);
    end;
  end;
end;

end.
