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

unit DCDVBTuningPluginTransportStream;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils, DCDVBTuningPlugins, Logger, TransportStream;

type
  TDCDVBTuningPluginTransportStream = class(TTransportStream, IDCDVBTuningPluginApplicationCallback)
  protected
    FDevice: IDCDVBTuningPluginDevice;
    function IDCDVBTuningPluginApplicationCallback.Log = OnLog;
    function GetActive: Boolean; override;
    procedure SetActive(AActive: Boolean); override;
    procedure SetDevice(ADevice: IDCDVBTuningPluginDevice);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IApplicationCallback
    function ReceiveTSStream(ABuffer: PByte; ASize: Integer): Integer; stdcall;
    function OnLog(AMethod: PChar; AText: PChar): Integer; stdcall;
  public
    constructor Create(ALogger: TLogger = nil); override;
    destructor Destroy; override;
    procedure TuneIn(ATuningSpace: Pointer);
    function GetSignalStatistic(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): Boolean;
  published
    property Device: IDCDVBTuningPluginDevice read FDevice write SetDevice;
  end;

implementation

uses Math;

(*** TDCDVBTuningPluginTransportStream ****************************************)

constructor TDCDVBTuningPluginTransportStream.Create(ALogger: TLogger = nil);
begin
  inherited Create(ALogger);
end;

destructor TDCDVBTuningPluginTransportStream.Destroy;
begin
  SetDevice(nil);
  inherited Destroy;
end;

function TDCDVBTuningPluginTransportStream.GetActive: Boolean;
begin
  Result := Assigned(FDevice) and (FDevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_GET_ACTIVATE_STATE, 0, 0) = DCDVB_TUNING_PLUGIN_ACTIVE);
//  Log(FLogger, Self, 'GetActive', 'Returned: ' + IfThen(Result, 'True', 'False'));
end;

procedure TDCDVBTuningPluginTransportStream.SetActive(AActive: Boolean);
begin
  Log(FLogger, Self, 'SetActive', 'SetStatus: ' + IfThen(AActive, 'True', 'False') + ' Current Status: ' + IfThen(GetActive, 'True', 'False'));
  if (GetActive = AActive)
    then Exit;

  if GetActive then
  begin
    FDevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE, 0, 0);
  end;

  if AActive and Assigned(FDevice) then
  begin
    FDevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_ACTIVATE, 0, 0);
  end;
end;

procedure TDCDVBTuningPluginTransportStream.SetDevice(ADevice: IDCDVBTuningPluginDevice);
var
  was_active: Boolean;
begin
  Log(FLogger, Self, 'SetDevice', 'Device: ' + inttohex(Integer(ADevice), 8));
  was_active := GetActive;
  SetActive(False);
  if Assigned(FDevice) then
  begin
    FDevice.put_ApplicationCallback(nil);
    FDevice := nil;
  end;
  if Assigned(ADevice) then
  begin
    FDevice := ADevice;
    FDevice.put_ApplicationCallback(Self);
    SetActive(was_active);
  end;
end;

procedure TDCDVBTuningPluginTransportStream.TuneIn(ATuningSpace: Pointer);
begin
  Log(FLogger, Self, 'TuneIn', 'Request to change the Frequency');

  if (FDevice = nil)
    then Exit;

  if (ATuningSpace = nil)
    then Exit;

  FDevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST, Integer(ATuningSpace), PBaseTuneRequest(ATuningSpace).DeviceType);
end;

function TDCDVBTuningPluginTransportStream.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TDCDVBTuningPluginTransportStream._AddRef: Integer;
begin
  Result := 2;
end;

function TDCDVBTuningPluginTransportStream._Release: Integer;
begin
  Result := 2;
end;

function TDCDVBTuningPluginTransportStream.ReceiveTSStream(ABuffer: PByte; ASize: Integer): Integer;
begin
  PushData(ABuffer, ASize);
  Result := S_OK;
end;

function TDCDVBTuningPluginTransportStream.OnLog(AMethod: PChar; AText: PChar): Integer;
begin
  Log(FLogger, Self, 'TuningPlugin::' + AMethod, AText);
  Result := S_OK;
end;

function TDCDVBTuningPluginTransportStream.GetSignalStatistic(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): Boolean;
var
  statistics: TSignalStatistics;
begin
  Result := False;

  if not Assigned(FDevice)
    then Exit;

  statistics.Size := SizeOf(TSignalStatistics);

  if FDevice.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_GET_SIGNAL_STATISTICS, Integer(@statistics), 0) = S_OK then
  begin
    Strength := statistics.SignalStrength;
    Quality := statistics.SignalQuality;
    SignalPresent := statistics.SignalPresent;
    SignalLocked := statistics.SignalLocked;
    Result := True;
  end;
end;

end.
