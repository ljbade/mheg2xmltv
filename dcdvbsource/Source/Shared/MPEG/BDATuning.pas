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

unit BDATuning;

interface

uses
  MPEGConst, BDAConst, DVBConst;

type
  TBDATuningSpace = class
  private
    FDeviceType: TDVBNetworkType;
  published
    property DeviceType: TDVBNetworkType read FDeviceType;
  end;

  TBDADVBTTuningSpace = class(TBDATuningSpace)
  private
    FCentreFrequency: Int64;
    FBandwidth: Integer;
    FConstellation: TTerrestrialConstellation;
    FHierarchyInformation: TTerrestrialHierarchyInformation;
    FCodeRateHPStream: TTerrestrialCodeRate;
    FCodeRateLPStream: TTerrestrialCodeRate;
    FGuardInterval: TTerrestrialGuardInterval;
    FTransmissionMode: TTerrestrialTransmissionMode;
    FPriority: Integer;
    FTimeSlicingIndicator: Integer;
    FMPEFECIndicator: Integer;
    FOtherFrequencyFlag: Integer;
  public
    constructor Create;
  published
    property CentreFrequency: Int64 read FCentreFrequency write FCentreFrequency;
    property Bandwidth: Integer read FBandwidth write FBandwidth;
    property Constellation: TTerrestrialConstellation read FConstellation write FConstellation;
    property HierarchyInformation: TTerrestrialHierarchyInformation read FHierarchyInformation write FHierarchyInformation;
    property CodeRateHPStream: TTerrestrialCodeRate read FCodeRateHPStream write FCodeRateHPStream;
    property CodeRateLPStream: TTerrestrialCodeRate read FCodeRateLPStream write FCodeRateLPStream;
    property GuardInterval: TTerrestrialGuardInterval read FGuardInterval write FGuardInterval;
    property TransmissionMode: TTerrestrialTransmissionMode read FTransmissionMode write FTransmissionMode;
    property Priority: Integer read FPriority write FPriority;
    property TimeSlicingIndicator: Integer read FTimeSlicingIndicator write FTimeSlicingIndicator;
    property MPEFECIndicator: Integer read FMPEFECIndicator write FMPEFECIndicator;
    property OtherFrequencyFlag: Integer read FOtherFrequencyFlag write FOtherFrequencyFlag;
  end;

  TBDADVBCTuningSpace = class(TBDATuningSpace)
  private
    FFrequency: Int64;
    FFECOuter: TFECOuter;
    FModulation: TCableModulation;
    FSymbolRate: Int64;
    FFECInner: TFECInner;
  public
    constructor Create;
  published
    property Frequency: Int64 read FFrequency write FFrequency;
    property FECOuter: TFECOuter read FFECOuter write FFECOuter;
    property Modulation: TCableModulation read FModulation write FModulation;
    property SymbolRate: Int64 read FSymbolRate write FSymbolRate;
    property FECInner: TFECInner read FFECInner write FFECInner;
  end;

  TBDADVBSTuningSpace = class(TBDATuningSpace)
  private
    FFrequency: Int64;
    FOrbitalPosition: Word;
    FWestEastFlag: Byte;
    FPolarization: TSatellitePolarization;
    FModulation: TSatelliteModulation;
    FSymbolRate: Int64;
    FFECInner: TFECInner;
  public
    constructor Create;
  published
    property Frequency: Int64 read FFrequency write FFrequency;
    property OrbitalPosition: Word read FOrbitalPosition write FOrbitalPosition;
    property WestEastFlag: Byte read FWestEastFlag write FWestEastFlag;
    property Polarization: TSatellitePolarization read FPolarization write FPolarization;
    property Modulation: TSatelliteModulation read FModulation write FModulation;
    property SymbolRate: Int64 read FSymbolRate write FSymbolRate;
    property FECInner: TFECInner read FFECInner write FFECInner;
  end;

  TBDAATSCTuningSpace = class(TBDATuningSpace)
  private
    FCentreFrequency: Int64;
    FBandwidth: Integer;
  public
    constructor Create;
  published
    property CentreFrequency: Int64 read FCentreFrequency write FCentreFrequency;
    property Bandwidth: Integer read FBandwidth write FBandwidth;
  end;

implementation

(*** TBDADVBTTuningSpace ******************************************************)

constructor TBDADVBTTuningSpace.Create;
begin
  inherited Create;
  FDeviceType := ntDVBt;
  FCentreFrequency := -1;
  FBandwidth := -1;
  FConstellation := tcReserved;
  FHierarchyInformation := thiNonHierarchicalNativeInterleaver;
  FCodeRateHPStream := tcrReserved;
  FCodeRateLPStream := tcrReserved;
  FGuardInterval := tgi1_4;
  FTransmissionMode := ttmReserved;
end;

(*** TBDADVBCTuningSpace ******************************************************)

constructor TBDADVBCTuningSpace.Create;
begin
  inherited Create;
  FDeviceType := ntDVBc;
  FFrequency := -1;
  FFECOuter := fecNotDefined_;
  FModulation := cmNotDefined;
  FSymbolRate := -1;
  FFECInner := fecNotDefined;
end;

(*** TBDADVBSTuningSpace ******************************************************)

constructor TBDADVBSTuningSpace.Create;
begin
  inherited Create;
  FDeviceType := ntDVBs;
  FFrequency := -1;
  FOrbitalPosition := 0;
  FWestEastFlag := 0;
  FPolarization := spUnknown;
  FModulation := smNotDefined;
  FSymbolRate := -1;
  FFECInner := fecNotDefined;
end;

(*** TBDAATSCTuningSpace ******************************************************)

constructor TBDAATSCTuningSpace.Create;
begin
  inherited Create;
  FDeviceType := ntATSC;
  FCentreFrequency := -1;
  FBandwidth := -1;
end;

end.
