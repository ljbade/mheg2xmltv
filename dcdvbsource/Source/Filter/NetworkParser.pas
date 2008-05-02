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

unit NetworkParser;

interface

uses
  Windows, Classes, DirectShow9, DVBInterface, MPEGConst, MPEGParser, MPEGUtils,
  SysUtils, DVBEPG, MPEGSections, DVBMHPParser, MHPConst, DVBConst, DSMCCCOnst,
  MPEGDescriptors;

const
  PMT_CHECK_DELAY = 60000;

type
  TNetworkParser = class
  private
    FTSParser: TTSParser;

    FTXTPID: Integer;
    FTXTCallback: ITeletextCallBack;

    FPCRCallback: IPCRCallback;
    FPCRPID: Integer;
    FSubtitleCallback: ISubtitleCallback;
    FSubtitlePID: Integer;

    FPSIParser: TList;
    FPremiereCIT: TPSIParser;
    FCIT: TPremiereContentInformationSection;

    FEPG: TDVBEPG;
    FPMTCheck: Cardinal;
    FEnableDSMCC: Boolean;
    FPMTCallback: TPMTCallback;
    FDSMCC: TDSMCCParser;
    FDSMCCCallback: TDSMCCBytesCallback;
    FMappings: array of Cardinal;
    FDSMCCList: TList;
    FPMTBufferCallback: TBufferCallback;

    procedure CheckPremiereCIT(APMT: TProgramMapSection);
    procedure OnMPEGTSPacket(APacket: PByte; APID: Integer);
    procedure OnPSI(ABuffer: PByte; ASize: Integer);
    procedure OnPremiereCIT(ABuffer: PByte; ASize: Integer);
    procedure OnDSMCC(ABuffer: PByte; ASize: Integer);
    procedure CheckMappings(APMT: TProgramMapSection);
    procedure RemapPIDs(APIDs: array of Cardinal);
    procedure ClearMappings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessBuffer(ABuffer: PByte; ASize: Integer);

    procedure SetTeletextCallback(CallBack: ITeletextCallBack);
    procedure SetTeletextPID(APID: Integer);

    procedure SetPCRCallBack(CallBack: IPCRCallback);
    procedure SetPCRPID(APID: Integer);
    procedure SetSubtitleCallBack(CallBack: ISubtitleCallback);
    procedure SetSubtitlePID(APID: Integer);

    procedure RemovePSIMappings;
    procedure SetPSIMappings(AMappings: PCardinal; ACount: Integer);

    procedure SetEPG(AEPG: TDVBEPG);
    procedure Reset;
    procedure SetEnableDSMCC(AEnable: Boolean);
    procedure SetPMTCallback(ACallback: TPMTCallback);
    procedure SetPMTBufferCallback(ACallback: TBufferCallback);
    procedure SetDSMCC(ADSMCC: TDSMCCParser);
    procedure SetDSMCCCallback(ADSMCCCallback: TDSMCCBytesCallback);
  end;

implementation

constructor TNetworkParser.Create;
begin
  inherited Create;
  SetLength(FMappings, 0);
  FPSIParser := TList.Create;
  FDSMCCList := TList.Create;
  FTXTPID := -1;
  FTXTCallback := nil;
  FTSParser := TTSParser.Create(True);
  FTSParser.OnMPEGTSPacket := OnMPEGTSPacket;
  FPMTCheck := 0;
  FEnableDSMCC := False;
  FPremiereCIT := TPSIParser.Create(FTSParser, -1, OnPremiereCIT, True);
  FCIT := TPremiereContentInformationSection.Create;
end;

destructor TNetworkParser.Destroy;
begin
  RemovePSIMappings;
  ClearMappings;
  FPSIParser.Free;
  FDSMCCList.Free;
  FTSParser.Free;
  FPremiereCIT.Free;
  FCIT.Free;
  SetLength(FMappings, 0);
  inherited Destroy;
end;

procedure TNetworkParser.OnMPEGTSPacket(APacket: PByte; APID: Integer);
begin
  if (FTXTCallback <> nil) and (APID = FTXTPID) then
  begin
    FTXTCallback.OnTeletextBuffer(APacket, TS_PACKET_SIZE);
  end;

  if (FSubtitleCallback <> nil) and (APID = FSubtitlePID) then
  begin
    FSubtitleCallback.OnSubtitleData(APacket, TS_PACKET_SIZE);
  end;

  if (FPCRCallback <> nil) and (APID = FPCRPID) then
  begin
    FPCRCallback.OnPCRData(APacket, TS_PACKET_SIZE);
  end;
end;

procedure TNetworkParser.ProcessBuffer(ABuffer: PByte; ASize: Integer);
begin
  FTSParser.ParseBuffer(ABuffer, ASize);
end;

procedure TNetworkParser.SetTeletextCallback(CallBack: ITeletextCallBack);
begin
  FTXTCallback := CallBack;
end;

procedure TNetworkParser.SetTeletextPID(APID: Integer);
begin
  FTXTPID := APID;
end;

procedure TNetworkParser.SetPCRCallBack(CallBack: IPCRCallback);
begin
  FPCRCallback := Callback;
end;

procedure TNetworkParser.SetPCRPID(APID: Integer);
begin
  FPCRPID := APID;
end;

procedure TNetworkParser.SetSubtitleCallBack(CallBack: ISubtitleCallback);
begin
  FSubtitleCallback := Callback;
end;

procedure TNetworkParser.SetSubtitlePID(APID: Integer);
begin
  FSubtitlePID := APID;
end;

procedure TNetworkParser.RemovePSIMappings;
var
  i: Integer;
begin
  for i := 0 to FPSIParser.Count -1 do
  begin
    TPSIParser(FPSIParser[i]).Free;
  end;
  FPSIParser.Clear;
end;

procedure TNetworkParser.SetPSIMappings(AMappings: PCardinal; ACount: Integer);
var
  i: Integer;
  parser: TPSIParser;
begin
  for i := 0 to ACount -1 do
  begin
    parser := TPSIParser.Create(FTSParser, PInteger(AMappings)^, OnPSI, True);
    FPSIParser.Add(parser);
    inc(AMappings);
  end;
end;

procedure TNetworkParser.OnPSI(ABuffer: PByte; ASize: Integer);
var
//  sdt: TServiceDescriptionSection;
//  cat: TConditionalAccessSection;
  pmt: TProgramMapSection;
begin
  case ABuffer^ of
    TABLE_ID_EIT_ACTUAL_PRESENT,
    TABLE_ID_EIT_OTHER_PRESENT,
    TABLE_ID_EIT_ACTUAL_SCHEDULE_MIN..TABLE_ID_EIT_ACTUAL_SCHEDULE_MAX,
    TABLE_ID_EIT_OTHER_SCHEDULE_MIN..TABLE_ID_EIT_OTHER_SCHEDULE_MAX:
    begin
      if Assigned(FEPG)
        then FEPG.ParseBuffer(ABuffer, ASize);
    end;
    TABLE_ID_RNT:
    begin
      // TODO TV Anytime
//        OutputDebugString(PChar('DVB: TABLE_ID_RNT'));
    end;
    TABLE_ID_SDT_ACTUAL,
    TABLE_ID_SDT_OTHER:
    begin
//        sdt := TServiceDescriptionSection.Create;
//        if sdt.ParseBuffer(ABuffer, ASize)
//          then printsdt(sdt);
//        sdt.Free;
      // TODO Service Description
//        OutputDebugString(PChar('DVB: TABLE_ID_SDT'));
    end;
    TABLE_ID_BAT:
    begin
      // TODO Bouquet Association
//        OutputDebugString(PChar('DVB: TABLE_ID_BAT'));
    end;
    TABLE_ID_CAT:
    begin
//        cat := TConditionalAccessSection.Create(ABuffer, ASize);
//        if cat.Valid
//          then PrintCAT(cat);
//        cat.Free;
    end;
    TABLE_ID_PMT:
    begin
      if assigned(FPMTBufferCallback) then
        FPMTBufferCallback(ABuffer, ASize);
      if (FPMTCheck < GetTickCount) then
      begin

        pmt := TProgramMapSection.Create(ABuffer, ASize);
        if pmt.Valid then
        begin
          FPMTCheck := GetTickCount + PMT_CHECK_DELAY;
          if FEnableDSMCC
            then CheckMappings(pmt);
          if Assigned(FPMTCallback)
            then FPMTCallback(pmt);
          CheckPremiereCIT(pmt);
          // FTeletextPin.CheckMapping(pmt);
        end;
        pmt.Free;
      end;
    end;
  end;
end;

procedure TNetworkParser.SetEPG(AEPG: TDVBEPG);
begin
  FEPG := AEPG;
end;

procedure TNetworkParser.Reset;
begin
  FPMTCheck := 0;
  ClearMappings;
end;

procedure TNetworkParser.SetEnableDSMCC(AEnable: Boolean);
begin
  FEnableDSMCC := AEnable;
end;

procedure TNetworkParser.SetPMTCallback(ACallback: TPMTCallback);
begin
  FPMTCallback := ACallback;
end;

procedure TNetworkParser.SetPMTBufferCallback(ACallback: TBufferCallback);
begin
  FPMTBufferCallback := ACallback;
end;

procedure TNetworkParser.SetDSMCC(ADSMCC: TDSMCCParser);
begin
  FDSMCC := ADSMCC;
end;

procedure TNetworkParser.OnDSMCC(ABuffer: PByte; ASize: Integer);
begin
  case ABuffer^ of
    DSMCC_TABLE_ID_DATA_MIN..DSMCC_TABLE_ID_DATA_MAX,
    MHP_TABLE_ID_AIT:
    begin
      if Assigned(FDSMCCCallback)
        then FDSMCCCallback(ASize);
      if FEnableDSMCC and Assigned(FDSMCC) then
      begin
//OutputDebugString('OnDSMCC');
        FDSMCC.ParsePSIBuffer(ABuffer, ASize);
      end;
    end;
  end;
end;

procedure TNetworkParser.SetDSMCCCallback(ADSMCCCallback: TDSMCCBytesCallback);
begin
  FDSMCCCallback := ADSMCCCallback;
end;

procedure TNetworkParser.CheckMappings(APMT: TProgramMapSection);
var
  i, c: Integer;
  mappings: array of Cardinal;
  p: Integer;
  has_mapping: Boolean;
begin
  p := 0;
  for i := 0 to APMT.ProgramStreamList.Count -1 do
  begin
    if (APMT.ProgramStreamList[i].IsDSMCC) or
       (APMT.ProgramStreamList[i].IsMHPAIT) or
       (APMT.ProgramStreamList[i].IsMHPData) then
    begin
      inc(p);
    end;
  end;

  SetLength(mappings, p);

  p := 0;
  for i := 0 to APMT.ProgramStreamList.Count -1 do
  begin
    if (APMT.ProgramStreamList[i].IsDSMCC) or
       (APMT.ProgramStreamList[i].IsMHPAIT) or
       (APMT.ProgramStreamList[i].IsMHPData) then
    begin
      mappings[p] := APMT.ProgramStreamList[i].ElementaryPID;
      inc(p);
    end;
  end;

  // now check the mappings from the PMT with the current mappings.

  if High(FMappings) <> High(mappings) then
  begin
    RemapPIDs(mappings);
    Exit;
  end;

  for i := 0 to High(FMappings) do
  begin
    has_mapping := False;
    for c := 0 to High(mappings) do
    begin
      if (mappings[c] = FMappings[i])
        then has_mapping := True;
    end;
    if not has_mapping then
    begin
      RemapPIDs(mappings);
      Exit;
    end;
  end;

  for i := 0 to High(mappings) do
  begin
    has_mapping := False;
    for c := 0 to High(FMappings) do
    begin
      if (FMappings[c] = mappings[i])
        then has_mapping := True;
    end;
    if not has_mapping then
    begin
      RemapPIDs(mappings);
      Exit;
    end;
  end;
end;

procedure TNetworkParser.RemapPIDs(APIDs: array of Cardinal);
var
  i: Integer;
  parser: TPSIParser;
begin
  ClearMappings;
  SetLength(FMappings, High(APIDs)+1);
  for i := 0 to High(APIDs)
    do FMappings[i] := APIDs[i];
  for i := 0 to High(FMappings) do
  begin
    parser := TPSIParser.Create(FTSParser, FMappings[i], OnDSMCC, True);
//OutputDebugString(PChar('Mapped DSMCC at PID: ' + inttostr(parser.PID)));
    FDSMCCList.Add(parser);
  end;
end;

procedure TNetworkParser.ClearMappings;
var
  i: Integer;
begin
  SetLength(FMappings, 0);
  for i := 0 to FDSMCCList.Count -1 do
  begin
//OutputDebugString(PChar('Removing DSMCC at PID: ' + inttostr(TPSIParser(FDSMCCList[i]).PID)));
    TPSIParser(FDSMCCList[i]).Free;
  end;
  FDSMCCList.Clear;
end;

procedure TNetworkParser.CheckPremiereCIT(APMT: TProgramMapSection);
var
  i, c: Integer;
  pdesc: TPrivateDataSpecifierDescriptor;
begin

  for i := 0 to APMT.ProgramStreamList.Count - 1 do
  begin
    if (APMT.ProgramStreamList[i].StreamType = $05) and
       (APMT.ProgramStreamList[i].Descriptors.GetDescriptor(TPrivateDataSpecifierDescriptor, pdesc)) then
    begin
      for c := 0 to APMT.ProgramStreamList[i].Descriptors.Count - 1 do
      begin
        if (APMT.ProgramStreamList[i].Descriptors[c].Tag = $90) then
        begin
          if (APMT.ProgramStreamList[i].ElementaryPID <> FPremiereCIT.PID) then
          begin
            FPremiereCIT.PID := APMT.ProgramStreamList[i].ElementaryPID;
            FPremiereCIT.Flush;
          end;
          
          Exit;
        end;
      end;
    end;
  end;

  FPremiereCIT.Flush;
  FPremiereCIT.PID := -1;
end;

procedure TNetworkParser.OnPremiereCIT(ABuffer: PByte; ASize: Integer);
begin
  if FCIT.ParseBuffer(ABuffer, ASize) then
  begin
    FEPG.ParsePremiereCIT(FCIT);
  end;
end;


end.
