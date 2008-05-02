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

unit DVBFrequencyList;

interface

uses
  Classes, JvSimpleXml, SysUtils, DVBChannelList, DVBConst;

type
  TDVBFrequency = class
  private
    FFrequency: Int64;
    FBandwidth: Integer;
    FPolarization: Integer;
    FSymbolrate: Cardinal;
    FNetworkType: TDVBNetworkType;
    FName: WideString;
  published
    property Frequency: Int64 read FFrequency write FFrequency;
    property Bandwidth: Integer read FBandwidth write FBandwidth;
    property Polarization: Integer read FPolarization write FPolarization;
    property Symbolrate: Cardinal read FSymbolrate write FSymbolrate;
    property NetworkType: TDVBNetworkType read FNetworkType write FNetworkType;
    property Name: WideString read FName write FName;
  end;

  TDVBFrequencys = class
  private
    FList: TList;
    FName: WideString;
    FNetworkType: TDVBNetworkType;
    function GetCount: Integer;
    function GetFrequency(AIndex: Integer): TDVBFrequency;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddFrom(AFrequency: TDVBFrequency);
    procedure Delete(AIndex: Integer);
    procedure DeleteAll;

    property NetworkType: TDVBNetworkType read FNetworkType;
    property Name: WideString read FName;

    property Count: Integer read GetCount;
    property Frequency[Index: Integer]: TDVBFrequency read GetFrequency; default;
  end;

  TDVBFrequencyList = class
  private
    FDVBTList: TList;
    FDVBCList: TList;
    FDVBSList: TList;
    FATSCList: TList;
    function GetDVBTCount: Integer;
    function GetDVBCCount: Integer;
    function GetDVBSCount: Integer;
    function GetATSCCount: Integer;
    function GetDVBTFrequency(AIndex: Integer): TDVBFrequencys;
    function GetDVBCFrequency(AIndex: Integer): TDVBFrequencys;
    function GetDVBSFrequency(AIndex: Integer): TDVBFrequencys;
    function GetATSCFrequency(AIndex: Integer): TDVBFrequencys;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(AFileName: WideString);
    procedure Clear;

    procedure DeleteDVBT(AIndex: Integer);
    procedure DeleteDVBC(AIndex: Integer);
    procedure DeleteDVBS(AIndex: Integer);
    procedure DeleteATSC(AIndex: Integer);

    procedure AddDVBT(AName: String);
    procedure AddDVBC(AName: String);
    procedure AddDVBS(AName: String);
    procedure AddATSC(AName: String);

    property DVBTCount: Integer read GetDVBTCount;
    property DVBCCount: Integer read GetDVBCCount;
    property DVBSCount: Integer read GetDVBSCount;
    property ATSCCount: Integer read GetATSCCount;

    property DVBTFrequency[Index: Integer]: TDVBFrequencys read GetDVBTFrequency;
    property DVBCFrequency[Index: Integer]: TDVBFrequencys read GetDVBCFrequency;
    property DVBSFrequency[Index: Integer]: TDVBFrequencys read GetDVBSFrequency;
    property ATSCFrequency[Index: Integer]: TDVBFrequencys read GetATSCFrequency;
  end;

implementation

(*** TDVBFrequencys ***********************************************************)

constructor TDVBFrequencys.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDVBFrequencys.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TDVBFrequencys.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDVBFrequencys.GetFrequency(AIndex: Integer): TDVBFrequency;
begin
  if (AIndex < 0) or (AIndex >= FList.Count)
    then Result := nil
    else Result := FList[AIndex];
end;

procedure TDVBFrequencys.Clear;
begin
  while FList.Count > 0 do
  begin
    TDVBFrequency(FList[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TDVBFrequencys.AddFrom(AFrequency: TDVBFrequency);
var
  freq: TDVBFrequency;
begin
  freq := TDVBFrequency.Create;
  freq.FFrequency := AFrequency.FFrequency;
  freq.FBandwidth := AFrequency.FBandwidth;
  freq.FPolarization := AFrequency.FPolarization;
  freq.FSymbolrate := AFrequency.FSymbolrate;
  freq.FNetworkType := AFrequency.FNetworkType;
  freq.FName := AFrequency.FName;
  FList.Add(freq);
end;

procedure TDVBFrequencys.Delete(AIndex: Integer);
begin
  TDVBFrequency(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

procedure TDVBFrequencys.DeleteAll;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TDVBFrequency(FList[i]).Free;
  FList.Clear;
end;

(*** TDVBFrequencyList ********************************************************)

constructor TDVBFrequencyList.Create;
begin
  inherited Create;
  FDVBTList := TList.Create;
  FDVBCList := TList.Create;
  FDVBSList := TList.Create;
  FATSCList := TList.Create;
end;

destructor TDVBFrequencyList.Destroy;
begin
  Clear;
  FDVBTList.Free;
  FDVBCList.Free;
  FDVBSList.Free;
  FATSCList.Free;
  inherited Destroy;
end;

function TDVBFrequencyList.GetDVBTCount: Integer;
begin
  Result := FDVBTList.Count;
end;

function TDVBFrequencyList.GetDVBCCount: Integer;
begin
  Result := FDVBCList.Count;
end;

function TDVBFrequencyList.GetDVBSCount: Integer;
begin
  Result := FDVBSList.Count;
end;

function TDVBFrequencyList.GetATSCCount: Integer;
begin
  Result := FATSCList.Count;
end;

function TDVBFrequencyList.GetDVBTFrequency(AIndex: Integer): TDVBFrequencys;
begin
  if (AIndex < 0) or (AIndex >= FDVBTList.Count)
    then Result := nil
    else Result := FDVBTList[AIndex];
end;

function TDVBFrequencyList.GetDVBCFrequency(AIndex: Integer): TDVBFrequencys;
begin
  if (AIndex < 0) or (AIndex >= FDVBCList.Count)
    then Result := nil
    else Result := FDVBCList[AIndex];
end;

function TDVBFrequencyList.GetDVBSFrequency(AIndex: Integer): TDVBFrequencys;
begin
  if (AIndex < 0) or (AIndex >= FDVBSList.Count)
    then Result := nil
    else Result := FDVBSList[AIndex];
end;

function TDVBFrequencyList.GetATSCFrequency(AIndex: Integer): TDVBFrequencys;
begin
  if (AIndex < 0) or (AIndex >= FATSCList.Count)
    then Result := nil
    else Result := FATSCList[AIndex];
end;

procedure TDVBFrequencyList.Clear;

  procedure ClearList(AList: TList);
  begin
    while AList.Count > 0 do
    begin
      TDVBFrequencys(AList[0]).Free;
      AList.Delete(0);
    end;
  end;

begin
  ClearList(FDVBTList);
  ClearList(FDVBCList);
  ClearList(FDVBSList);
  ClearList(FATSCList);
end;

procedure TDVBFrequencyList.LoadFromFile(AFileName: WideString);
var
  xml: TJvSimpleXml;
  i1, i2, i3: TJvSimpleXmlElem;
  p1: TJvSimpleXmlProp;
  i, c, k: Integer;
  network_type: TDVBNetworkType;
  freqs: TDVBFrequencys;
  freq: TDVBFrequency;
begin
  Clear;

  if not FileExists(AFileName)
    then Exit;


  xml := TJvSimpleXml.Create(nil);
  xml.LoadFromFile(AFileName);

  for i := 0 to xml.Root.Items.Count -1 do
  begin
    network_type := ntUnknown;
    i1 := xml.Root.Items[i];

    p1 := i1.Properties.ItemNamed['name'];
    if Assigned(p1) then
    begin
      if UpperCase(p1.Value) = 'DVBT'
        then network_type := ntDVBT
      else if UpperCase(p1.Value) = 'DVBC'
        then network_type := ntDVBC
      else if UpperCase(p1.Value) = 'DVBS'
        then network_type := ntDVBS
      else if UpperCase(p1.Value) = 'ATSC'
        then network_type := ntATSC;

      if network_type = ntUnknown
        then Continue;

      for c := 0 to i1.Items.Count -1 do
      begin
        i2 := i1.Items[c];
        freqs := TDVBFrequencys.Create;
        freqs.FNetworkType := network_type;
        freqs.FName := i2.Properties.Value('name');

        case network_type of
          ntDVBT:
          begin
            FDVBTList.Add(freqs);
            for k := 0 to i2.Items.Count -1 do
            begin
              i3 := i2.Items[k];
              freq := TDVBFrequency.Create;
              freqs.FList.Add(freq);
              freq.FNetworkType := freqs.FNetworkType;
              freq.FFrequency := i3.Properties.IntValue('frequency');
              freq.FBandwidth := i3.Properties.IntValue('bandwidth');
            end;
          end;
          ntDVBC:
          begin
            FDVBCList.Add(freqs);
            for k := 0 to i2.Items.Count -1 do
            begin
              i3 := i2.Items[k];
              freq := TDVBFrequency.Create;
              freqs.FList.Add(freq);
              freq.FNetworkType := freqs.FNetworkType;
              freq.FFrequency := i3.Properties.IntValue('frequency');
              freq.FSymbolrate := i3.Properties.IntValue('symbolrate');
            end;
          end;
          ntDVBS:
          begin
            FDVBSList.Add(freqs);
            for k := 0 to i2.Items.Count -1 do
            begin
              i3 := i2.Items[k];
              freq := TDVBFrequency.Create;
              freqs.FList.Add(freq);
              freq.FNetworkType := freqs.FNetworkType;
              freq.FFrequency := i3.Properties.IntValue('frequency');
              freq.FPolarization := i3.Properties.IntValue('polarization');
              freq.FSymbolrate := i3.Properties.IntValue('symbolrate');
            end;
          end;
          ntATSC:
          begin
            FATSCList.Add(freqs);
            for k := 0 to i2.Items.Count -1 do
            begin
              i3 := i2.Items[k];
              freq := TDVBFrequency.Create;
              freqs.FList.Add(freq);
              freq.FNetworkType := freqs.FNetworkType;
              freq.FFrequency := i3.Properties.IntValue('frequency');
            end;
          end;
        end;

      end;
    end;
  end;

  xml.Free;
end;

procedure TDVBFrequencyList.DeleteDVBT(AIndex: Integer);
begin
  TDVBFrequencys(FDVBTList[AIndex]).Free;
  FDVBTList.Delete(AIndex);
end;

procedure TDVBFrequencyList.DeleteDVBC(AIndex: Integer);
begin
  TDVBFrequencys(FDVBCList[AIndex]).Free;
  FDVBCList.Delete(AIndex);
end;

procedure TDVBFrequencyList.DeleteDVBS(AIndex: Integer);
begin
  TDVBFrequencys(FDVBSList[AIndex]).Free;
  FDVBSList.Delete(AIndex);
end;

procedure TDVBFrequencyList.DeleteATSC(AIndex: Integer);
begin
  TDVBFrequencys(FATSCList[AIndex]).Free;
  FATSCList.Delete(AIndex);
end;

procedure TDVBFrequencyList.AddDVBT(AName: String);
var
  l: TDVBFrequencys;
begin
  l := TDVBFrequencys.Create;
  l.FName := AName;
  l.FNetworkType := ntDVBT;
  FDVBTList.Add(l);
end;

procedure TDVBFrequencyList.AddDVBC(AName: String);
var
  l: TDVBFrequencys;
begin
  l := TDVBFrequencys.Create;
  l.FName := AName;
  l.FNetworkType := ntDVBC;
  FDVBCList.Add(l);
end;

procedure TDVBFrequencyList.AddDVBS(AName: String);
var
  l: TDVBFrequencys;
begin
  l := TDVBFrequencys.Create;
  l.FName := AName;
  l.FNetworkType := ntDVBS;
  FDVBSList.Add(l);
end;

procedure TDVBFrequencyList.AddATSC(AName: String);
var
  l: TDVBFrequencys;
begin
  l := TDVBFrequencys.Create;
  l.FName := AName;
  l.FNetworkType := ntATSC;
  FATSCList.Add(l);
end;

end.
