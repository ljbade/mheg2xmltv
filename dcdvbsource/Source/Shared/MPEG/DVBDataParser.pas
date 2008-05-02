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

unit DVBDataParser;

interface

uses
  Windows, BaseClass, ActiveX, DirectShow9, SysUtils, Classes, MPEGSections,
  MPEGUtils, MPEGConst, MPEGDescriptors, DVBChannelList, ISO639LanguageCode,
  BDAUtils, DVBConst, DVBFrequencyList, Logger;

type
  TDVBDataParser = class
  private
    FLog: TLogger;
    FBufferPos: Int64;
    FWriteData: Boolean;
    FWritePath: String;

    FPrintPAT: Boolean;
    FPrintPMT: Boolean;
    FPrintNIT: Boolean;
    FPrintSDT: Boolean;
    FPrintCAT: Boolean;
    FPrintBAT: Boolean;
    FPrintEIT: Boolean;

    FChannelList: TDVBChannelsList;
    FNIT: TNetworkInformationSection;
    FPAT: TProgramAssociationSection;
    FPMT: TProgramMapSection;
    FSDT: TServiceDescriptionSection;
    FCAT: TConditionalAccessSection;
    FBAT: TBouquetAssociationSection;
    FEIT: TEventInformationSection;
    function GetChannelList: TDVBChannelsList;
    function GetValid: Boolean;
    procedure ParseNIT;
    procedure ParsePAT;
    procedure ParsePMT;
    procedure ParseSDT;
    procedure ParseCAT;
    procedure ParseBAT;
    function GetStreamFromProgram(AProgram: TProgramStream): TDVBBaseStream;
  public
    constructor Create(ALog: TLogger);
    destructor Destroy; override;

    procedure Clear;
    procedure Parse(ABuffer: PByte; ASize: Integer);

    // Workaround: Set Frequency and Bandwidth from the external List
    procedure SetManual(AFrequency: TDVBFrequency);
  published
    property ChannelList: TDVBChannelsList read GetChannelList;
    property Valid: Boolean read GetValid;
    property PAT: TProgramAssociationSection read FPAT;

    property WriteData: Boolean read FWriteData write FWriteData;
    property WritePath: String read FWritePath write FWritePath;

    property PrintPAT: Boolean read FPrintPAT write FPrintPAT;
    property PrintPMT: Boolean read FPrintPMT write FPrintPMT;
    property PrintNIT: Boolean read FPrintNIT write FPrintNIT;
    property PrintSDT: Boolean read FPrintSDT write FPrintSDT;
    property PrintCAT: Boolean read FPrintCAT write FPrintCAT;
    property PrintBAT: Boolean read FPrintBAT write FPrintBAT;
    property PrintEIT: Boolean read FPrintEIT write FPrintEIT;
  end;

implementation

constructor TDVBDataParser.Create(ALog: TLogger);
begin
  inherited Create;
  FLog := ALog;
  FChannelList := TDVBChannelsList.Create;
  FPAT := TProgramAssociationSection.Create;
  FPMT := TProgramMapSection.Create;
  FNIT := TNetworkInformationSection.Create;
  FSDT := TServiceDescriptionSection.Create;
  FCAT := TConditionalAccessSection.Create;
  FBAT := TBouquetAssociationSection.Create;
  FEIT := TEventInformationSection.Create;
end;

destructor TDVBDataParser.Destroy;
begin
  Clear;
  FEIT.Free;
  FBAT.Free;
  FCAT.Free;
  FPAT.Free;
  FPMT.Free;
  FNIT.Free;
  FSDT.Free;
  FChannelList.Free;
end;

procedure TDVBDataParser.Clear;
begin
  FPAT.Clear;
  FPMT.Clear;
  FNIT.Clear;
  FSDT.Clear;
  FBAT.Clear;
  FEIT.Clear;
  FChannelList.Clear;
end;

procedure TDVBDataParser.Parse(ABuffer: PByte; ASize: Integer);
var
  s: String;
begin
  if not Assigned(ABuffer) or (ASize <= 0)
    then Exit;

  if FWriteData and (FWritePath <> '') then
  begin
    if not DirectoryExists(FWritePath) then
    begin
      CreateDir(FWritePath);
    end;
    s := AddBackSlash(FWritePath);
    s := s + Format('%.16d', [FBufferPos]) + '.bin';
    try
      with TFileStream.Create(s, fmCreate) do
      begin
        Write(ABuffer^, ASize);
        Free;
      end;
    except
    end;
    inc(FBufferPos);
  end;

  case ABuffer^ of
    TABLE_ID_NIT_ACTUAL:
//    TABLE_ID_NIT_OTHER:
    begin
      if FNIT.ParseBuffer(ABuffer, ASize) and FNIT.Valid then
      begin
        ParseNIT;
      end;
    end;
    TABLE_ID_PAT:
    begin
      if FPAT.ParseBuffer(ABuffer, ASize) and FPAT.Valid then
      begin
        ParsePAT;
      end;
    end;
    TABLE_ID_SDT_ACTUAL:
//    TABLE_ID_SDT_OTHER:
    begin
      if FSDT.ParseBuffer(ABuffer, ASize) and FSDT.Valid then
      begin
        ParseSDT;
      end;
    end;
    TABLE_ID_PMT:
    begin
      if (FPMT.ParseBuffer(ABuffer, ASize) and FPMT.Valid) then
      begin
        ParsePMT;
      end;
    end;
    TABLE_ID_CAT:
    begin
      if (FCAT.ParseBuffer(ABuffer, ASize) and FCAT.Valid) then
      begin
        ParseCAT;
      end;
    end;
    TABLE_ID_BAT:
    begin
      if (FBAT.ParseBuffer(ABuffer, ASize) and FBAT.Valid) then
      begin
        ParseBAT;
      end;
    end;
    TABLE_ID_EIT_ACTUAL_PRESENT,
    TABLE_ID_EIT_OTHER_PRESENT,
    TABLE_ID_EIT_ACTUAL_SCHEDULE_MIN..TABLE_ID_EIT_ACTUAL_SCHEDULE_MAX,
    TABLE_ID_EIT_OTHER_SCHEDULE_MIN..TABLE_ID_EIT_OTHER_SCHEDULE_MAX:
    begin
      if (FEIT.ParseBuffer(ABuffer, ASize) and FEIT.Valid) then
      begin
        if FPrintEIT
          then MPEGUtils.PrintEIT(FEIT);
      end;
    end;
  end;
end;

procedure TDVBDataParser.ParsePAT;
var
  i, c: Integer;
  channel: TDVBChannel;
  prog: TProgramAssociationSectionProgram;
  in_list: Boolean;
begin
  if FPrintPAT
    then MPEGUtils.PrintPAT(FPAT);
    
  for i := 0 to FPAT.CountPrograms -1 do
  begin
    prog := FPAT.Program_[i];
    in_list := False;

    // ProgramNumber 0 is the Network PID, but we only need the
    // Program Map PID here.
    if (prog.ProgramNumber <> 0) then
    begin
      for c := 0 to FChannelList.Count -1 do
      begin
        channel := FChannelList[c];
        if (channel.ProgramMapPID = prog.ProgramMapPID) and
           (channel.Network.TSID = FPAT.TransportStreamID) and
           (channel.SID = prog.ProgramNumber) then
        begin
          in_list := True;
          break;
        end;
      end;

      if not in_list then
      begin
        channel := TDVBChannel.Create;
        channel.ParsedPAT := True;
        channel.Network.TSID := FPAT.TransportStreamID;
        channel.SID := prog.ProgramNumber;
        channel.ProgramMapPID := prog.ProgramMapPID;
        FChannelList.Add(channel);
      end;
    end;
  end;
end;

procedure TDVBDataParser.ParseNIT;
var
  i, c, k: Integer;
  ts: TTransportStream;
  desc: TBaseDescriptor;
  channel: TDVBChannel;
  serv: TServiceDescriptorService;
  network_name: String;
begin
  if FPrintNIT
    then MPEGUtils.PrintNIT(FNIT);

  Log(FLog, Self, 'ParsePMT', 'parsing NIT with ' + inttostr(FNIT.TransportStreamList.Count) + ' Streams');
  network_name := '';
  if FNIT.Descriptors.GetDescriptor(TNetworkNameDescriptor, desc)
    then network_name := TNetworkNameDescriptor(desc).Name;

  for i := 0 to FNIT.TransportStreamList.Count -1 do
  begin
    ts := FNIT.TransportStreamList[i];
    for c := 0 to FChannelList.Count -1 do
    begin
      channel := FChannelList[c];
      channel.Network.Name := network_name;

      if ts.Descriptors.GetDescriptor(TTerrestrialDeliverySystemDescriptor, desc) then
      begin
        Log(FLog, Self, 'ParsePMT', 'found Terrestrial Delivery System Descriptor');
        channel.Network.Type_ := ntDVBT;
      end else
      if ts.Descriptors.GetDescriptor(TSatelliteDeliverySystemDescriptor, desc) then
      begin
        Log(FLog, Self, 'ParsePMT', 'found Satellite Delivery System Descriptor');
        channel.Network.Type_ := ntDVBS;
      end else
      if ts.Descriptors.GetDescriptor(TCableDeliverySystemDescriptor, desc) then
      begin
        Log(FLog, Self, 'ParsePMT', 'found Cable Delivery System Descriptor');
        channel.Network.Type_ := ntDVBC;
      end;

      // TODO Workaround for Milan/Italy doesn't work with Astra Transponders !!
      if channel.Network.TSID = ts.TransportStreamID then
      begin
        channel.ParsedNIT := True;
        channel.Network.NetworkID := FNIT.NetworkID;
        channel.Network.ONID := ts.OriginalNetworkID;

        if ts.Descriptors.GetDescriptor(TServiceListDescriptor, desc) then
        begin
          with TServiceListDescriptor(desc) do
          begin
            for k := 0 to CountItems -1 do
            begin
              serv := Service[k];
              if channel.SID = serv.ServiceID then
              begin
                channel.ServiceType := serv.ServiceType;
              end;
            end;
          end;
        end;

        if ts.Descriptors.GetDescriptor(TTerrestrialDeliverySystemDescriptor, desc) then
        begin
          with TTerrestrialDeliverySystemDescriptor(desc) do
          begin
            channel.Network.Type_ := ntDVBT;
            channel.Network.Terrestrial.CentreFrequency := CentreFrequency div 1000;
            channel.Network.Terrestrial.Bandwidth := Bandwidth;
            channel.Network.Terrestrial.Priority := Priority;
            channel.Network.Terrestrial.TimeSlicingIndicator := TimeSlicingIndicator;
            channel.Network.Terrestrial.MPEFECIndicator := MPEFECIndicator;
            channel.Network.Terrestrial.Constellation := Constellation;
            channel.Network.Terrestrial.HierarchyInformation := HierarchyInformation;
            channel.Network.Terrestrial.CodeRateHPStream := CodeRateHPStream;
            channel.Network.Terrestrial.CodeRateLPStream := CodeRateLPStream;
            channel.Network.Terrestrial.GuardInterval := GuardInterval;
            channel.Network.Terrestrial.TransmissionMode := TransmissionMode;
            channel.Network.Terrestrial.OtherFrequencyFlag := OtherFrequencyFlag;
          end;
        end;

        if ts.Descriptors.GetDescriptor(TSatelliteDeliverySystemDescriptor, desc) then
        begin
          with TSatelliteDeliverySystemDescriptor(desc) do
          begin
            channel.Network.Type_ := ntDVBS;
            channel.Network.Satellite.Frequency := Frequency div 1000;
            channel.Network.Satellite.OrbitalPosition := OrbitalPosition;
            channel.Network.Satellite.WestEastFlag := WestEastFlag;
            channel.Network.Satellite.Polarization := Polarization;
            channel.Network.Satellite.Modulation := Modulation;
            channel.Network.Satellite.SymbolRate := SymbolRate div 1000;
            channel.Network.Satellite.FECInner := FECInner;
          end;
        end;

        if ts.Descriptors.GetDescriptor(TCableDeliverySystemDescriptor, desc) then
        begin
          with TCableDeliverySystemDescriptor(desc) do
          begin
            channel.Network.Type_ := ntDVBC;
            channel.Network.Cable.Frequency := Frequency div 1000;
            channel.Network.Cable.FECOuter := FECOuter;
            channel.Network.Cable.Modulation := Modulation;
            channel.Network.Cable.SymbolRate := SymbolRate div 1000;
            channel.Network.Cable.FECInner := FECInner;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDVBDataParser.ParseSDT;
var
  i, c: Integer;
  channel: TDVBChannel;
  service: TService;
  desc: TServiceDescriptor;
begin
  if FPrintSDT
    then MPEGUtils.PrintSDT(FSDT);

  Log(FLog, Self, 'ParsePMT', 'parsing SDT with TransportStreamID = ' + inttostr(FSDT.TransportStreamID));
  for i := 0 to FSDT.ServiceList.Count -1 do
  begin
    service := FSDT.ServiceList[i];
    if service.Descriptors.GetDescriptor(TServiceDescriptor, desc) then
    begin
      Log(FLog, Self, 'ParsePMT', 'found Service Descriptor');
      for c := 0 to FChannelList.Count -1 do
      begin
        channel := FChannelList[c];
        if (* (channel.Network.TSID = FSDT.TransportStreamID) and (channel.Network.ONID = FSDT.OriginalNetworkID) and *)
           (channel.SID = service.ServiceID) then
        begin
          channel.Name := desc.ServiceName;
          channel.Provider := desc.ProviderName;
          channel.ServiceType := desc.ServiceType;
          channel.ParsedSDT := True;
        end;
      end;
    end;
  end;
end;

procedure TDVBDataParser.ParsePMT;
var
  i, c, k: Integer;
  channel: TDVBChannel;
  in_list: Boolean;
  prog: TProgramStream;
  str: TDVBBaseStream;
begin
  if FPrintPMT
    then MPEGUtils.PrintPMT(FPMT);

  for i := 0 to FChannelList.Count -1 do
  begin
    channel := FChannelList[i];
    if channel.SID = FPMT.ProgramNumber then
    begin
      channel.PCRPID := FPMT.PCRPID;
      channel.ParsedPMT := True;
      for c := 0 to FPMT.ProgramStreamList.Count -1 do
      begin
        prog := FPMT.ProgramStreamList[c];
        in_list := False;
        for k := 0 to channel.Streams.Count -1 do
        begin
          str := channel.Streams[k];
          if str.PID = prog.ElementaryPID then
          begin
            in_list := True;
            break;
          end;
        end;
        if not in_list then
        begin
          Log(FLog, Self, 'ParsePMT', 'found Elementary PID ' + inttostr(prog.ElementaryPID) + ' for ProgramNumber = ' + inttostr(FPMT.ProgramNumber));
          channel.Streams.Add(GetStreamFromProgram(prog));
        end;
      end;
    end;
  end;
end;

procedure TDVBDataParser.ParseCAT;
begin
  if FPrintCAT
    then MPEGUtils.PrintCAT(FCAT);
  // ToDo
end;

procedure TDVBDataParser.ParseBAT;
begin
  if FPrintBAT
    then MPEGUtils.PrintBAT(FBAT);
  // ToDo
end;

function TDVBDataParser.GetStreamFromProgram(AProgram: TProgramStream): TDVBBaseStream;
var
  i: Integer;
  // Delphi 2005 workaround. Doesn't compile with Direct Assignement from Descriptor
  // so we copy it temporary to another variable. Seems stupid, but it's the only
  // way to bypass the internal error;
  lang: TISO639LanguageDescriptorLanguage;
begin
  if AProgram.HasVideo then
  begin
    Result := TDVBVideoStream.Create;
    with TDVBVideoStream(Result) do
    begin
      if (AProgram.IsVideoMPEG2)
        then Coding := vcMPEG
      else if (AProgram.IsVideoH264)
        then Coding := vcH264;

      StreamType := stVideo;
    end;
  end else
  if AProgram.IsAudioMP1 then
  begin
    Result := TDVBAudioStream.Create;
    with TDVBAudioStream(Result) do
    begin
      StreamType := stAudio;
      Coding := acMP1;
      // find language
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_ISO639LANGUAGE then
        begin
          if TISO639LanguageDescriptor(AProgram.Descriptors[i]).CountItems > 0 then
          begin
            lang := TISO639LanguageDescriptor(AProgram.Descriptors[i]).Language[0];
            Language := lang.ISO639Language;
            AudioType := lang.AudioType;
            break;
          end;
        end;
      end;
    end;
  end else
  if AProgram.IsAudioMP2 then
  begin
    Result := TDVBAudioStream.Create;
    with TDVBAudioStream(Result) do
    begin
      StreamType := stAudio;
      Coding := acMP2;
      // find language
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_ISO639LANGUAGE then
        begin
          if TISO639LanguageDescriptor(AProgram.Descriptors[i]).CountItems > 0 then
          begin
            lang := TISO639LanguageDescriptor(AProgram.Descriptors[i]).Language[0];
            Language := lang.ISO639Language;
            AudioType := lang.AudioType;
            break;
          end;
        end;
      end;
    end;
  end else
  if AProgram.IsAudioAC3 then
  begin
    Result := TDVBAudioStream.Create;
    with TDVBAudioStream(Result) do
    begin
      StreamType := stAudio;
      Coding := acAC3;
      // find language
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_ISO639LANGUAGE then
        begin
          if TISO639LanguageDescriptor(AProgram.Descriptors[i]).CountItems > 0 then
          begin
            lang := TISO639LanguageDescriptor(AProgram.Descriptors[i]).Language[0];
            Language := lang.ISO639Language;
            AudioType := lang.AudioType;
            break;
          end;
        end;
      end;
    end;
  end else
  if AProgram.IsAudioAAC then
  begin
    Result := TDVBAudioStream.Create;
    with TDVBAudioStream(Result) do
    begin
      StreamType := stAudio;
      Coding := acAAC;
      // find language
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_ISO639LANGUAGE then
        begin
          if TISO639LanguageDescriptor(AProgram.Descriptors[i]).CountItems > 0 then
          begin
            lang := TISO639LanguageDescriptor(AProgram.Descriptors[i]).Language[0];
            Language := lang.ISO639Language;
            AudioType := lang.AudioType;
            break;
          end;
        end;
      end;
    end;
  end else
  if AProgram.IsTeletext then
  begin
    Result := TDVBTeletextStream.Create;
    with TDVBTeletextStream(Result) do
    begin
      StreamType := stTeletext;
      // find language
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_TELETEXT then
        begin
          if TTeletextDescriptor(AProgram.Descriptors[i]).CountItems > 0 then
          begin
            Language := TTeletextDescriptor(AProgram.Descriptors[i]).Page[0].ISO639Language;
            break;
          end;
        end;
      end;
    end;
  end else
  if AProgram.IsMHPAIT then
  begin
    Result := TDVBBaseStream.Create;
    Result.StreamType := stMHPAIT;
  end else
  if AProgram.IsMHPData then
  begin
    Result := TDVBBaseStream.Create;
    Result.StreamType := stMHPData;
  end else
  if AProgram.IsSubtitle then
  begin
    Result := TDVBSubtitleStream.Create;
    with TDVBSubtitleStream(Result) do
    begin
      StreamType := stSubtitle;
      // find Subtitle Descriptor
      for i := 0 to AProgram.Descriptors.Count -1 do
      begin
        if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_SUBTITLING then
        begin
          AddFromSubsDescriptor(TSubtitlingDescriptor(AProgram.Descriptors[i]));
        end;
      end;
    end;
  end else
  if AProgram.IsDSMCC then
  begin
    Result := TDVBBaseStream.Create;
    Result.StreamType := stDSMCC;
  end else
  begin
    Result := TDVBBaseStream.Create;
    Result.StreamType := stUnknown;
  end;

  for i := 0 to AProgram.Descriptors.Count -1 do
  begin
    if AProgram.Descriptors[i].Tag = DESCRIPTOR_TAG_STREAM_IDENTIFIER then
    begin
      Result.Tag := TStreamIdentifierDescriptor(AProgram.Descriptors[i]).ComponentTag;
      break;
    end;
  end;

  Result.PID := AProgram.ElementaryPID;
  Result.Name := GetStreamTypeString(AProgram.StreamType);
end;

function TDVBDataParser.GetValid: Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FChannelList.Count -1 do
  begin
    with FChannelList[i] do
    begin
      if not ParsedNIT or not ParsedPAT or not ParsedSDT or not ParsedPMT
        then Exit;
    end;
  end;

  Result := FChannelList.Count > 0;
end;

function TDVBDataParser.GetChannelList: TDVBChannelsList;
begin
  Result := FChannelList;
end;

procedure TDVBDataParser.SetManual(AFrequency: TDVBFrequency);
var
  i: Integer;
begin
  for i := 0 to FChannelList.Count -1 do
  begin
    with (FChannelList[i].Network) do
    begin
      case Type_ of
        ntDVBT:
        begin
          Terrestrial.CentreFrequency := AFrequency.Frequency;
          Terrestrial.Bandwidth := AFrequency.Bandwidth;
        end;
        ntDVBC:
        begin
          Cable.Frequency := AFrequency.Frequency;
          Cable.SymbolRate := AFrequency.Symbolrate;
        end;
        ntDVBS:
        begin
          Satellite.Frequency := AFrequency.Frequency;
          Satellite.SymbolRate := AFrequency.Symbolrate;
          case AFrequency.Polarization of
            0: Satellite.Polarization := spLinearHorizontal;
            1: Satellite.Polarization := spLinearVertical;
          end;
        end;
        ntATSC:
        begin
          ATSC.CentreFrequency := AFrequency.Frequency;
          ATSC.Bandwidth := AFrequency.Bandwidth;
        end;
      end;
    end;
  end;
end;

end.
