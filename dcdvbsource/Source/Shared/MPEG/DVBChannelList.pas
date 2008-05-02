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

unit DVBChannelList;

interface

uses
  Windows, SysUtils, Classes, Math, MPEGConst, ISO639LanguageCode, DVBConst,
  BDATuning, MPEGDescriptors;

type
  TDVBChannelStreamType = ( stUnknown, stVideo, stAudio, stTeletext, stMHPAIT, stMHPData, stDSMCC, stSubtitle );
  TDVBAudioStreamCoding = ( acUnknown, acMP1, acMP2, acAC3, acAAC );
  TDVBVideoStreamCoding = ( vcUnknown, vcMPEG, vcH264 );

  //----------------------------------------------------------------------------

  TDVBBaseStream = class
  private
    FStreamType: TDVBChannelStreamType;
    FPID: Integer;
    FName: WideString;
    FDefault: Boolean;
    FTag: Integer;
  public
    constructor Create; overload;
    constructor Create(ASource: TDVBBaseStream); overload;
    procedure Assign(ASource: TDVBBaseStream); virtual;
    procedure Clear; virtual;

    class function CreateStream(AStreamType: TDVBChannelStreamType): TDVBBaseStream;
    class function CopyStream(AStream: TDVBBaseStream): TDVBBaseStream;

    property Name: WideString read FName write FName;
    property StreamType: TDVBChannelStreamType read FStreamType write FStreamType;
    property Default: Boolean read FDefault write FDefault;
    property PID: Integer read FPID write FPID;
    property Tag: Integer read FTag write FTag;
  end;

  TDVBVideoStream = class(TDVBBaseStream)
  private
    FCoding: TDVBVideoStreamCoding;
  public
    procedure Assign(ASource: TDVBBaseStream); override;
    procedure Clear; override;

    property Coding: TDVBVideoStreamCoding read FCoding write FCoding;
  end;

  TDVBAudioStream = class(TDVBBaseStream)
  private
    FLanguage: TISO6392LanguageCode;
    FCoding: TDVBAudioStreamCoding;
    FAudioType: TISO639LanguageDescriptorAudioType;
  public
    procedure Assign(ASource: TDVBBaseStream); override;
    procedure Clear; override;

    function GetName: String;
    function GetExtension: String;

    property Language: TISO6392LanguageCode read FLanguage write FLanguage;
    property Coding: TDVBAudioStreamCoding read FCoding write FCoding;
    property AudioType: TISO639LanguageDescriptorAudioType read FAudioType write FAudioType;
  end;

  TDVBTeletextStream = class(TDVBBaseStream)
  private
    FLanguage: TISO6392LanguageCode;
  public
    procedure Assign(ASource: TDVBBaseStream); override;
    procedure Clear; override;

    property Language: TISO6392LanguageCode read FLanguage write FLanguage;
  end;

  TDVBSubtitleStream = class(TDVBBaseStream)
  private
    FSubtitles: array of TSubtitlingDescriptorItem;
    function GetSubtitlesCount: Integer;
    function GetSubtitle(AIndex: Integer): TSubtitlingDescriptorItem;
  public
    procedure Assign(ASource: TDVBBaseStream); override;
    procedure Clear; override;

    procedure AddFromSubsDescriptor(ADescriptor: TSubtitlingDescriptor);
    procedure AddItem(lng: String; tp: integer; cp: Integer; ap: Integer; def: Boolean);

    property SubtitlesCount: Integer read GetSubtitlesCount;
    property Subtitle[Index: Integer]: TSubtitlingDescriptorItem read GetSubtitle;
  end;

  //----------------------------------------------------------------------------

  TDVBStreamList = class(TList)
  private
    function Get(Index: Integer): TDVBBaseStream;
    procedure Put(Index: Integer; Item: TDVBBaseStream);
  public
    procedure Assign(ASource: TDVBStreamList);

    function Add(Item: TDVBBaseStream): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TDVBBaseStream read Get write Put; default;
  end;

  //----------------------------------------------------------------------------

  TDVBNetwork = class
  private
    FType: TDVBNetworkType;
    FName: WideString;
    FNetworkID: Integer;

    FONID: Integer;
    FTSID: Integer;

    FNetworkTerrestrial: TBDADVBTTuningSpace;
    FNetworkSatellite: TBDADVBSTuningSpace;
    FNetworkCable: TBDADVBCTuningSpace;
    FNetworkATSC: TBDAATSCTuningSpace;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TDVBNetwork);
    procedure Clear;

    property Type_: TDVBNetworkType read FType write FType;
    property Name: WideString read FName write FName;
    property NetworkID: Integer read FNetworkID write FNetworkID;

    property TSID: Integer read FTSID write FTSID;
    property ONID: Integer read FONID write FONID;

    property Terrestrial: TBDADVBTTuningSpace read FNetworkTerrestrial write FNetworkTerrestrial;
    property Satellite: TBDADVBSTuningSpace read FNetworkSatellite write FNetworkSatellite;
    property Cable: TBDADVBCTuningSpace read FNetworkCable write FNetworkCable;
    property ATSC: TBDAATSCTuningSpace read FNetworkATSC write FNetworkATSC;
  end;

  //----------------------------------------------------------------------------

  TDVBChannel = class
  private
    FStreamList: TDVBStreamList;
    FNetwork: TDVBNetwork;

    FName: WideString;
    FOriginalName: WideString;
    FProvider: WideString;

    FSID: Integer;
    FPCRPID: Integer;
    FProgramMapPID: Integer;
    FServiceType: Integer;

    FParsedPAT: Boolean;
    FParsedSDT: Boolean;
    FParsedPMT: Boolean;
    FParsedNIT: Boolean;
    
    function GetAudioStreamCount: Integer;
    function GetSubtitleStreamCount: Integer;
    function GetDefaultAudioStreamIndex: Integer;
    function GetDefaultSubtitleStreamIndex: Integer;
  public
    constructor Create; overload;
    constructor Create(ASource: TDVBChannel); overload;
    destructor Destroy; override;

    procedure Clear;
    function GetAudioStreamName(AIndex: Integer): String;
    function GetAudioStreamExtension(AIndex: Integer): String;
    function GetSubtitleStreamName(AIndex: Integer): String;
    function GetSubtitleStream(AIndex: Integer; out SubStream: TSubtitlingDescriptorItem; out pid: Integer): Boolean;

    procedure Assign(ASource: TDVBChannel);
  published
    property Name: WideString read FName write FName;
    property OriginalName: WideString read FOriginalName write FOriginalName;
    property Provider: WideString read FProvider write FProvider;

    property SID: Integer read FSID write FSID;
    property PCRPID: Integer read FPCRPID write FPCRPID;
    property ProgramMapPID: Integer read FProgramMapPID write FProgramMapPID;
    property ServiceType: Integer read FServiceType write FServiceType;

    property Streams: TDVBStreamList read FStreamList write FStreamList;
    property Network: TDVBNetwork read FNetwork write FNetwork;

    property AudioStreamCount: Integer read GetAudioStreamCount;
    property SubtitleStreamCount: Integer read GetSubtitleStreamCount;
    property DefaultAudioStreamIndex: Integer read GetDefaultAudioStreamIndex;
    property DefaultSubtitleStreamIndex: Integer read GetDefaultSubtitleStreamIndex;

    property ParsedPAT: Boolean read FParsedPAT write FParsedPAT;
    property ParsedNIT: Boolean read FParsedNIT write FParsedNIT;
    property ParsedSDT: Boolean read FParsedSDT write FParsedSDT;
    property ParsedPMT: Boolean read FParsedPMT write FParsedPMT;
  end;

  TDVBChannelsList = class(TList)
  private
    function Get(Index: Integer): TDVBChannel;
    procedure Put(Index: Integer; Item: TDVBChannel);
  public
    function Add(Item: TDVBChannel): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TDVBChannel read Get write Put; default;
  end;

  //----------------------------------------------------------------------------

  function GetDVBChannelStreamTypeString(AStreamType: TDVBChannelStreamType): String;

implementation

function GetDVBChannelStreamTypeString(AStreamType: TDVBChannelStreamType): String;
begin
  case AStreamType of
    stVideo:        Result := 'Video';
    stAudio:        Result := 'Audio';
    stTeletext:     Result := 'Teletext';
    stMHPAIT:       Result := 'Carousel (AIT)';
    stMHPData:      Result := 'Carousel (Data)';
    stDSMCC:        Result := 'DSMCC';
    stSubtitle:     Result := 'Subtitle';
    else            Result := 'Unknown';
  end
end;

(*** TDVBBaseStream ***********************************************************)

constructor TDVBBaseStream.Create;
begin
  inherited Create;
  Clear;
end;

constructor TDVBBaseStream.Create(ASource: TDVBBaseStream);
begin
  inherited Create;
  Clear;
  Assign(ASource);
end;

class function TDVBBaseStream.CreateStream(AStreamType: TDVBChannelStreamType): TDVBBaseStream;
begin
  case AStreamType of
    stVideo:    Result := TDVBVideoStream.Create;
    stAudio:    Result := TDVBAudioStream.Create;
    stTeletext: Result := TDVBTeletextStream.Create;
    stSubtitle: Result := TDVBSubtitleStream.Create;
    else        Result := TDVBBaseStream.Create;
  end;
end;

class function TDVBBaseStream.CopyStream(AStream: TDVBBaseStream): TDVBBaseStream;
begin
  Result := CreateStream(AStream.StreamType);
  Result.Assign(AStream);
end;

procedure TDVBBaseStream.Assign(ASource: TDVBBaseStream);
begin
  FName := ASource.FName;
  FStreamType := ASource.FStreamType;
  FDefault := ASource.FDefault;
  FPID := ASource.FPID;
  FTag := ASource.FTag;
end;

procedure TDVBBaseStream.Clear;
begin
  FName := '';
  FStreamType := stUnknown;
  FDefault := False;
  FPID := -1;
  FTag := -1;
end;

(*** TDVBVideoStream **********************************************************)

procedure TDVBVideoStream.Assign(ASource: TDVBBaseStream);
begin
  inherited Assign(ASource);

  with TDVBVideoStream(ASource) do
  begin
    Self.FCoding := Coding;
  end;
end;

procedure TDVBVideoStream.Clear;
begin
  inherited Clear;

  FCoding := vcUnknown;
end;

(*** TDVBAudioStream **********************************************************)

procedure TDVBAudioStream.Assign(ASource: TDVBBaseStream);
begin
  inherited Assign(ASource);

  with TDVBAudioStream(ASource) do
  begin
    Self.FLanguage := Language;
    Self.FCoding := Coding;
    Self.FAudioType := AudioType;
  end;
end;

procedure TDVBAudioStream.Clear;
begin
  inherited Clear;

  FLanguage := '   ';
  FCoding := acUnknown;
  FAudioType := atUndefined;
end;

function TDVBAudioStream.GetName: String;
begin
  Result := GetFullISO639Name(FLanguage);

  case FCoding of
    acMP1: Result := Result + ' - MP1';
    acMP2: Result := Result + ' - MP2';
    acAC3: Result := Result + ' - AC3';
  end;
end;

function TDVBAudioStream.GetExtension: String;
begin
  case FAudioType of
    atCleanEffects: Result := 'Clean Effects';
    atHearingImpaired: Result := 'Hearing Impaired';
    atVisualImpairedCommentary: Result := 'Visual Impaired Commentary';
    else Result := '';
  end;
end;

(*** TDVBTeletextStream *******************************************************)

procedure TDVBTeletextStream.Assign(ASource: TDVBBaseStream);
begin
  inherited Assign(ASource);

  with TDVBTeletextStream(ASource) do
  begin
    Self.FLanguage := Language;
  end;
end;

procedure TDVBTeletextStream.Clear;
begin
  inherited Clear;

  FLanguage := '   ';
end;

(*** TDVBSubtitleStream *******************************************************)

function TDVBSubtitleStream.GetSubtitlesCount: Integer;
begin
  Result := High(FSubtitles) + 1;
end;

function TDVBSubtitleStream.GetSubtitle(AIndex: Integer): TSubtitlingDescriptorItem;
begin
  Result.LanguageCode := '   ';
  Result.SubtitlingType := 0;
  Result.CompositionPageID := 0;
  Result.AncillaryPageID := 0;

  if (AIndex < 0) or (AIndex >= GetSubtitlesCount)
    then Exit;

  Result := FSubtitles[AIndex];
end;

procedure TDVBSubtitleStream.AddFromSubsDescriptor(ADescriptor: TSubtitlingDescriptor);
var
  i: Integer;
begin
  SetLength(FSubtitles, ADescriptor.CountItems);
  for i := 0 to ADescriptor.CountItems -1 do
  begin
    FSubtitles[i] := ADescriptor.Subtitle[i];
    FSubtitles[i].DefaultSub := False;
  end;
end;

procedure TDVBSubtitleStream.AddItem(lng: String; tp: integer; cp: Integer; ap: Integer; def: Boolean);
begin
  SetLength(FSubtitles, GetSubtitlesCount + 1);
  with FSubtitles[GetSubtitlesCount -1] do
  begin
    SetLength(lng, 3);
    Move(lng[1], LanguageCode[0], 3);
    SubtitlingType := tp;
    CompositionPageID := cp;
    AncillaryPageID := ap;
    DefaultSub := def;
  end;
end;

procedure TDVBSubtitleStream.Assign(ASource: TDVBBaseStream);
var
  sub: TDVBSubtitleStream;
  i: Integer;
begin
  inherited Assign(ASource);

  sub := TDVBSubtitleStream(ASource);

  SetLength(FSubtitles, sub.GetSubtitlesCount);
  for i := 0 to sub.GetSubtitlesCount -1 do
  begin
    FSubtitles[i] := sub.FSubtitles[i];
  end;
end;

procedure TDVBSubtitleStream.Clear;
begin
  inherited Clear;

  SetLength(FSubtitles, 0);
end;

(*** TDVBStreamList ***********************************************************)

function TDVBStreamList.Get(Index: Integer): TDVBBaseStream;
begin
  Result := inherited Get(Index);
end;

procedure TDVBStreamList.Put(Index: Integer; Item: TDVBBaseStream);
begin
  inherited Put(Index, Item);
end;

function TDVBStreamList.Add(Item: TDVBBaseStream): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDVBStreamList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TDVBBaseStream(Items[i]).Free;
  inherited Clear;
end;

procedure TDVBStreamList.Delete(Index: Integer);
begin
  TDVBBaseStream(Items[Index]).Free;
  inherited Delete(Index);
end;

procedure TDVBStreamList.Assign(ASource: TDVBStreamList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to ASource.Count -1
    do Add(ASource[i].CopyStream(ASource[i]));
end;

(*** TDVBNetwork **************************************************************)

constructor TDVBNetwork.Create;
begin
  FNetworkSatellite := TBDADVBSTuningSpace.Create;
  FNetworkCable := TBDADVBCTuningSpace.Create;
  FNetworkATSC := TBDAATSCTuningSpace.Create;
  FNetworkTerrestrial := TBDADVBTTuningSpace.Create;
end;

destructor TDVBNetwork.Destroy;
begin
  FNetworkSatellite.Free;
  FNetworkCable.Free;
  FNetworkATSC.Free;
  FNetworkTerrestrial.Free;
end;

procedure TDVBNetwork.Assign(ASource: TDVBNetwork);
begin
  FName := ASource.Name;
  FNetworkID := ASource.NetworkID;

  FONID := ASource.ONID;
  FTSID := ASource.TSID;
  FType := ASource.Type_;

  FNetworkTerrestrial.CentreFrequency := ASource.Terrestrial.CentreFrequency;
  FNetworkTerrestrial.Bandwidth := ASource.Terrestrial.Bandwidth;
  FNetworkTerrestrial.Priority := ASource.Terrestrial.Priority;
  FNetworkTerrestrial.TimeSlicingIndicator := ASource.Terrestrial.TimeSlicingIndicator;
  FNetworkTerrestrial.MPEFECIndicator := ASource.Terrestrial.MPEFECIndicator;
  FNetworkTerrestrial.Constellation := ASource.Terrestrial.Constellation;
  FNetworkTerrestrial.HierarchyInformation := ASource.Terrestrial.HierarchyInformation;
  FNetworkTerrestrial.CodeRateHPStream := ASource.Terrestrial.CodeRateHPStream;
  FNetworkTerrestrial.CodeRateLPStream := ASource.Terrestrial.CodeRateLPStream;
  FNetworkTerrestrial.GuardInterval := ASource.Terrestrial.GuardInterval;
  FNetworkTerrestrial.TransmissionMode := ASource.Terrestrial.TransmissionMode;
  FNetworkTerrestrial.OtherFrequencyFlag := ASource.Terrestrial.OtherFrequencyFlag;

  FNetworkSatellite.Frequency := ASource.Satellite.Frequency;
  FNetworkSatellite.OrbitalPosition := ASource.Satellite.OrbitalPosition;
  FNetworkSatellite.WestEastFlag := ASource.Satellite.WestEastFlag;
  FNetworkSatellite.Polarization := ASource.Satellite.Polarization;
  FNetworkSatellite.Modulation := ASource.Satellite.Modulation;
  FNetworkSatellite.SymbolRate := ASource.Satellite.SymbolRate;
  FNetworkSatellite.FECInner := ASource.Satellite.FECInner;

  FNetworkCable.Frequency := ASource.Cable.Frequency;
  FNetworkCable.FECOuter := ASource.Cable.FECOuter;
  FNetworkCable.Modulation := ASource.Cable.Modulation;
  FNetworkCable.SymbolRate := ASource.Cable.SymbolRate;
  FNetworkCable.FECInner := ASource.Cable.FECInner;
end;

procedure TDVBNetwork.Clear;
begin
  FName := '';
  FNetworkID := -1;

  FONID := -1;
  FTSID := -1;
  FType := ntUnknown;

  FNetworkTerrestrial.CentreFrequency := -1;
  FNetworkTerrestrial.Bandwidth := -1;
  FNetworkTerrestrial.Priority := -1;
  FNetworkTerrestrial.TimeSlicingIndicator := -1;
  FNetworkTerrestrial.MPEFECIndicator := -1;
  FNetworkTerrestrial.Constellation := tcReserved;
  FNetworkTerrestrial.HierarchyInformation := thiNonHierarchicalNativeInterleaver;
  FNetworkTerrestrial.CodeRateHPStream := tcrReserved;
  FNetworkTerrestrial.CodeRateLPStream := tcrReserved;
  FNetworkTerrestrial.GuardInterval := tgi1_8;
  FNetworkTerrestrial.TransmissionMode := ttmReserved;
  FNetworkTerrestrial.OtherFrequencyFlag := -1;
end;

(*** TDVBChannel **************************************************************)

constructor TDVBChannel.Create;
begin
  inherited Create;
  FStreamList := TDVBStreamList.Create;
  FNetwork := TDVBNetwork.Create;
  Clear;
end;

constructor TDVBChannel.Create(ASource: TDVBChannel);
begin
  inherited Create;
  FStreamList := TDVBStreamList.Create;
  FNetwork := TDVBNetwork.Create;
  Clear;
  Assign(ASource);
end;

destructor TDVBChannel.Destroy;
begin
  Clear;
  FNetwork.Free;
  FStreamList.Free;
  inherited Destroy;
end;

procedure TDVBChannel.Clear;
begin
  FName := '';
  FProvider := '';

  FSID := -1;
  FPCRPID := -1;
  FServiceType := -1;
  FProgramMapPID := -1;

  FNetwork.Clear;
  FStreamList.Clear;
end;

procedure TDVBChannel.Assign(ASource: TDVBChannel);
begin
  Clear;

  FName := ASource.Name;
  FProvider := ASource.Provider;
  FSID := ASource.SID;
  FPCRPID := ASource.PCRPID;
  FProgramMapPID := ASource.ProgramMapPID;
  FServiceType := ASource.ServiceType;

  FNetwork.Assign(ASource.Network);
  FStreamList.Assign(ASource.Streams);
end;

function TDVBChannel.GetAudioStreamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FStreamList.Count -1 do
    if FStreamList[i].StreamType = stAudio
      then inc(Result);
end;

function TDVBChannel.GetSubtitleStreamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stSubtitle then
    begin
      Inc(Result, TDVBSubtitleStream(FStreamList[i]).SubtitlesCount);
    end;
  end;
end;

function TDVBChannel.GetAudioStreamName(AIndex: Integer): String;
var
  i, c: Integer;
begin
  Result := '';
  c := 0;

  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stAudio then
    begin
      if AIndex = c then
      begin
        Result := TDVBAudioStream(FStreamList[i]).GetName;
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBChannel.GetAudioStreamExtension(AIndex: Integer): String;
var
  i, c: Integer;
begin
  Result := '';
  c := 0;

  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stAudio then
    begin
      if AIndex = c then
      begin
        Result := TDVBAudioStream(FStreamList[i]).GetExtension;
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBChannel.GetSubtitleStreamName(AIndex: Integer): String;
var
  i, c, k: Integer;
  str: TDVBSubtitleStream;
begin
  c := 0;
  Result := '';

  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stSubtitle then
    begin
      str := TDVBSubtitleStream(FStreamList[i]);
      for k := 0 to str.SubtitlesCount -1 do
      begin
        if AIndex = c then
        begin
          Result := GetFullISO639Name(str.Subtitle[k].LanguageCode);
          Exit;
        end;
        inc(c);
      end;
    end;
  end;
end;

function TDVBChannel.GetSubtitleStream(AIndex: Integer; out SubStream: TSubtitlingDescriptorItem; out pid: Integer): Boolean;
var
  i, c, k: Integer;
  str: TDVBSubtitleStream;
begin
  Result := False;
  c := 0;

  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stSubtitle then
    begin
      str := TDVBSubtitleStream(FStreamList[i]);
      for k := 0 to str.SubtitlesCount -1 do
      begin
        if AIndex = c then
        begin
          Result := True;
          SubStream := str.Subtitle[k];
          pid := str.PID;
          Exit;
        end;
        inc(c);
      end;
    end;
  end;
end;

function TDVBChannel.GetDefaultAudioStreamIndex: Integer;
var
  i, c: Integer;
begin
  Result := 0;
  c := 0;
  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stAudio then
    begin
      if TDVBAudioStream(FStreamList[i]).Default then
      begin
        Result := c;
        Exit;
      end;
      inc(c);
    end;
  end;
end;

function TDVBChannel.GetDefaultSubtitleStreamIndex: Integer;
var
  i, c, k: Integer;
  str: TDVBSubtitleStream;
begin
  c := 0;
  Result := -1;

  for i := 0 to FStreamList.Count -1 do
  begin
    if FStreamList[i].StreamType = stSubtitle then
    begin
      str := TDVBSubtitleStream(FStreamList[i]);
      for k := 0 to str.SubtitlesCount -1 do
      begin
        if str.Subtitle[k].DefaultSub then
        begin
          Result := c;
          Exit;
        end;
        inc(c);
      end;
    end;
  end;
end;

(*** TDVBChannelsList *********************************************************)

function TDVBChannelsList.Get(Index: Integer): TDVBChannel;
begin
  Result := inherited Get(Index);
end;

procedure TDVBChannelsList.Put(Index: Integer; Item: TDVBChannel);
begin
  inherited Put(Index, Item);
end;

function TDVBChannelsList.Add(Item: TDVBChannel): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDVBChannelsList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TDVBChannel(Items[i]).Free;
  inherited Clear;
end;

procedure TDVBChannelsList.Delete(Index: Integer);
begin
  TDVBChannel(Items[Index]).Free;
  inherited Delete(Index);
end;

end.
