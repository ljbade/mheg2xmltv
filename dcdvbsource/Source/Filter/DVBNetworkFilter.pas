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

unit DVBNetworkFilter;

interface

{$I Compiler.inc}

uses
  Windows, DirectShow9, Classes, ActiveX, BaseClass, DVBInterface, SysUtils,
  MPEGSections, MPEGUtils, MPEGConst, DSUtil, MPEGParser, BDAConst, BDAUtils,
  MHPSections, MHPConst, MHPUtils, DSMCCSections, DSMCCConst, DVBMHPParser,
  DVBEPG;

const
  PMT_CHECK_DELAY = 60000;

type
  TDVBDSMCCInputPin = Class (TBCRenderedInputPin)
  private
    FMappings: array of Cardinal;
  public
    constructor Create(ObjectName: string; pUnk: IUnKnown; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT);
    destructor Destroy; override;

    procedure RemapPIDs(APIDs: array of Cardinal);
    procedure CheckMappings(APMT: TProgramMapSection);
    procedure ClearMappings;

    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function Receive(pSample: IMediaSample): HRESULT; override;
  end;

implementation

(*** TDVBDSMCCInputPin **********************************************************)

constructor TDVBDSMCCInputPin.Create(ObjectName: string; pUnk: IUnKnown; Filter: TBCBaseFilter; Lock: TBCCritSec; out hr: HRESULT);
begin
  inherited Create(ObjectName, Filter, Lock, hr, 'DSMCC');
  SetLength(FMappings, 0);
end;

destructor TDVBDSMCCInputPin.Destroy;
begin
  SetLength(FMappings, 0);
  inherited Destroy;
end;

function TDVBDSMCCInputPin.Receive(pSample: IMediaSample): HRESULT;
var
  buffer: PByte;
  size: Integer;
begin
  Result := S_OK;

  pSample.GetPointer(buffer);
  size := pSample.GetActualDataLength;

  if (size < 8) or not Assigned(buffer)
    then Exit;

//  TDVBNetworkFilter(FFilter).OnDSMCCData(buffer, size);
end;

function TDVBDSMCCInputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

procedure TDVBDSMCCInputPin.CheckMappings(APMT: TProgramMapSection);
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

procedure TDVBDSMCCInputPin.RemapPIDs(APIDs: array of Cardinal);
var
  pin: IPin;
  map: IMPEG2PIDMap;
  i: Integer;
begin
  pin := GetConnected;
  if Assigned(pin) then
  begin
    if pin.QueryInterface(IID_IMPEG2PIDMap, map) = S_OK then
    begin
      SetLength(FMappings, High(APIDs)+1);
      for i := 0 to High(APIDs)
        do FMappings[i] := APIDs[i];

      UnmapPIDs(map, nil);
      map.MapPID(High(FMappings)+1, Pointer(FMappings), MEDIA_MPEG2_PSI);
    end;
  end;
end;

procedure TDVBDSMCCInputPin.ClearMappings;
var
  pin: IPin;
  map: IMPEG2PIDMap;
begin
  SetLength(FMappings, 0);
  pin := GetConnected;
  if Assigned(pin) then
  begin
    if pin.QueryInterface(IID_IMPEG2PIDMap, map) = S_OK then
    begin
      UnmapPIDs(map, nil);
    end;
  end;
end;


end.
