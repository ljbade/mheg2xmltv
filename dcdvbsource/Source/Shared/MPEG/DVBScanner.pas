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

unit DVBScanner;

interface

uses
  Windows, Classes, MPEGParser, DVBDataParser, MPEGConst, MPEGSections,
  DVBChannelList, Math, Logger, SysUtils;

type
  TDVBScanner = class
  private
    FLog: TLogger;
    FReceivedData: Boolean;
    FTSParser: TTSParser;
    FPATPSI: TPSIParser;
    FNITPSI: TPSIParser;
    FSDTPSI: TPSIParser;
    FCATPSI: TPSIParser;
    FEITPSI: TPSIParser;
    FDataParser: TDVBDataParser;
    FList: TList;
    FRemapPAT: Boolean;
    procedure Remap(APAT: TProgramAssociationSection);
    procedure ClearPMT;
    procedure OnPSI(ABuffer: PByte; ASize: Integer);
    function GetChannelList: TDVBChannelsList;
  public
    constructor Create(ALog: TLogger);
    destructor Destroy; override;

    procedure Clear;
    procedure ParseBuffer(ABuffer: PByte; ASize: Integer);
    procedure ParsePSIBuffer(ABuffer: PByte; ASize: Integer);
    property DataParser: TDVBDataParser read FDataParser;
    property ChannelList: TDVBChannelsList read GetChannelList;
    property ReceivedData: Boolean read FReceivedData;
  end;

implementation

constructor TDVBScanner.Create(ALog: TLogger);
begin
  inherited Create;
  FLog := ALog;
  FTSParser := TTSParser.Create;
  FList := TList.Create;

  FPATPSI := TPSIParser.Create(FTSParser, PSI_PID_PAT, OnPSI);
  FNITPSI := TPSIParser.Create(FTSParser, PSI_PID_NIT, OnPSI);
  FSDTPSI := TPSIParser.Create(FTSParser, PSI_PID_SDT, OnPSI);
  FCATPSI := TPSIParser.Create(FTSParser, PSI_PID_CAT, OnPSI);
  FEITPSI := TPSIParser.Create(FTSParser, PSI_PID_EIT, OnPSI);

  FDataParser := TDVBDataParser.Create(FLog);
  Clear;
end;

destructor TDVBScanner.Destroy;
begin
  Clear;
  FList.Free;
  FDataParser.Free;
  FEITPSI.Free;
  FPATPSI.Free;
  FNITPSI.Free;
  FSDTPSI.Free;
  FCATPSI.Free;
  FTSParser.Free;

  inherited Destroy;
end;

function TDVBScanner.GetChannelList: TDVBChannelsList;
begin
  Result := FDataParser.ChannelList;
end;

procedure TDVBScanner.ParseBuffer(ABuffer: PByte; ASize: Integer);
begin
  FReceivedData := True;
  FTSParser.ParseBuffer(ABuffer, ASize);
end;

procedure TDVBScanner.ParsePSIBuffer(ABuffer: PByte; ASize: Integer);
begin
  FDataParser.Parse(ABuffer, ASize);
end;

procedure TDVBScanner.ClearPMT;
begin
  Log(FLog, Self, 'ClearPMT', 'removing mapped PMT PID''s');
  while (FList.Count > 0) do
  begin
    TPSIParser(FList[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TDVBScanner.Clear;
begin
  Log(FLog, Self, 'Clear', 'Flushing PSI Parser');
  FReceivedData := False;
  ClearPMT;
  FCATPSI.Flush;
  FSDTPSI.Flush;
  FNITPSI.Flush;
  FPATPSI.Flush;
  FTSParser.Flush;
  FDataParser.Clear;
  FRemapPAT := True;
end;

procedure TDVBScanner.OnPSI(ABuffer: PByte; ASize: Integer);
begin
  FDataParser.Parse(ABuffer, ASize);

  if FDataParser.PAT.Valid and FRemapPAT then
  begin
    Remap(FDataParser.PAT);
  end;
end;

procedure TDVBScanner.Remap(APAT: TProgramAssociationSection);
var
  item: TPSIParser;
  i: Integer;
begin
  ClearPMT;

  Log(FLog, Self, 'Remap', 'PAT has ' + inttostr(APAT.CountPrograms) + ' Programs');

  if APAT.CountPrograms = 0 then
  begin
    Exit;
  end;

  FRemapPAT := False;

  for i := 0 to APAT.CountPrograms -1 do
  begin
    item := TPSIParser.Create(FTSParser, IfThen(APAT.Program_[i].ProgramNumber = 0, APAT.Program_[i].NetworkPID, APAT.Program_[i].ProgramMapPID), OnPSI);
    Log(FLog, Self, 'Remap', 'Mapping PAT Entry ' + inttostr(i) + ' to PMT PID ' + inttostr(item.PID) + '. ProgramNumber = ' + inttostr(APAT.Program_[i].ProgramNumber));
    FList.Add(item);
  end;
end;

end.
