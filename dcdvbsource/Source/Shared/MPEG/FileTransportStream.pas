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

unit FileTransportStream;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils,

  Logger, TransportStream, BDAUtils, BDATSDumpFilter;

type
  TFileTransportStream = class(TTransportStream)
  protected
    FGraphBuild: Boolean;
    FGraph: IFilterGraph2;
    FControl: IMediaControl;
    FSource: IBaseFilter;
    FDump: IBaseFilter;
    FFileName: String;
    function GetActive: Boolean; override;
    procedure SetActive(AActive: Boolean); override;
    procedure SetFileName(AFileName: String);
    procedure RenderGraph;
    procedure ClearGraph;
  published
    property FileName: String read FFileName write SetFileName;
  end;

implementation

(*** TFileTransportStream *****************************************************)

function TFileTransportStream.GetActive: Boolean;
begin
  Result := FGraphBuild;
//  Log(FLogger, Self, 'GetActive', 'Returned: ' + IfThen(FGraphBuild, 'True', 'False'));
end;

procedure TFileTransportStream.SetActive(AActive: Boolean);
begin
  Log(FLogger, Self, 'SetActive', 'SetStatus: ' + IfThen(AActive, 'True', 'False') + ' Current Status: ' + IfThen(GetActive, 'True', 'False'));
  if (GetActive = AActive)
    then Exit;

  ClearGraph;
  if AActive
    then RenderGraph;
end;

procedure TFileTransportStream.SetFileName(AFileName: String);
var
  was_active: Boolean;
begin
  Log(FLogger, Self, 'SetFileName', 'FileName: ' + AFileName);
  was_active := GetActive;
  SetActive(False);
  ClearGraph;
  FFileName := AFileName;
  SetActive(was_active);
end;

procedure TFileTransportStream.RenderGraph;
const
  CLSID_TSFileSource: TGuid = '{4F8BF30C-3BEB-43a3-8BF2-10096FD28CF2}';
var
  hr: HRESULT;
begin
  Log(FLogger, Self, 'RenderGraph', 'init');
  ClearGraph;

  if (FFileName = '') or not FileExists(FFileName) then
  begin
    Log(FLogger, Self, 'RenderGraph', 'File does not exist');
    Exit;
  end;

  Log(FLogger, Self, 'RenderGraph', 'creating FilterGraph');
  hr := CoCreateInstance(CLSID_FilterGraphNoThread, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FGraph);
  if (hr <> S_OK) then
  begin
    Log(FLogger, Self, 'RenderGraph', 'creating FilterGraph failed');
    ClearGraph;
    Exit;
  end;

  Log(FLogger, Self, 'RenderGraph', 'creating Source Filter');
  FSource := GetFilterFromSystem(CLSID_TSFileSource);
  if not Assigned(FSource) then
  begin
    Log(FLogger, Self, 'RenderGraph', 'failed creating TS File Source');
    ClearGraph;
    Exit;
  end;

  Log(FLogger, Self, 'RenderGraph', 'adding TS File Source to the Graph');
  hr := FGraph.AddFilter(FSource, StringToOleStr('TS File Source'));
  Log(FLogger, Self, 'RenderGraph', 'Graph::AddFilter returned ' + inttohex(hr, 8));

  Log(FLogger, Self, 'RenderGraph', 'loading File');
  hr := (FSource as IFileSourceFilter).Load(StringToOleStr(FFileName), nil);
  Log(FLogger, Self, 'RenderGraph', 'IFileSourceFilter::Load returned ' + inttohex(hr, 8));

  Log(FLogger, Self, 'RenderGraph', 'creating TS-Dump Filter');
  FDump := TBDATSDumpFilter.Create(FLogger);
  Log(FLogger, Self, 'RenderGraph', 'setting up Callback on TS-Dump Filter');
  (FDump as IBDATSDump).SetCallback(PushData);
  Log(FLogger, Self, 'RenderGraph', 'adding TS-Dump Filter to the Graph');
  hr := FGraph.AddFilter(FDump, PWideChar(WideString('TS Dump')));
  Log(FLogger, Self, 'RenderGraph', 'Graph::AddFilter (TS-Dump) returned ' + inttohex(hr, 8));
  Log(FLogger, Self, 'RenderGraph', 'connecting TS-Source to TS-Dump Filter');
  if (FGraph.ConnectDirect(GetOutPin(FSource, 0), GetInPin(FDump, 0), nil) <> S_OK) then
  begin
    Log(FLogger, Self, 'RenderGraph', 'connecting TS-Source to TS-Dump Filter failed');
    ClearGraph;
    Exit;
  end;

  Log(FLogger, Self, 'RenderGraph', 'searching for IMediaControl');
  hr := FGraph.QueryInterface(IID_IMediaControl, FControl);
  if hr <> S_OK then
  begin
    Log(FLogger, Self, 'RenderGraph', 'searching for IMediaControl failed');
    ClearGraph;
    Exit;
  end;

  Log(FLogger, Self, 'RenderGraph', 'starting the Graph');
  hr := FControl.Run;
  Log(FLogger, Self, 'RenderGraph', 'Control::Run returned ' + inttohex(hr, 8));

  FGraphBuild := True;
  Log(FLogger, Self, 'RenderGraph', 'Graph build');
end;

procedure TFileTransportStream.ClearGraph;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
  enum_pins: TPinList;
  i: Integer;
begin
  Log(FLogger, Self, 'ClearGraph', 'Request to kill the Graph');
  FGraphBuild := False;

  if Assigned(FGraph) then
  begin
    if FGraph.EnumFilters(enum) = S_OK then
    begin
      while enum.Next(1, filter, nil) = S_OK do
      begin
        try
          filter.Stop;
        except
        end;
        try
          enum_pins := TPinList.Create(filter);
          for i := 0 to enum_pins.Count -1 do
          begin
            enum_pins[i].Disconnect;
          end;
          enum_pins.Free;
          filter := nil;
        except
        end;
      end;
    end;
    if FGraph.EnumFilters(enum) = S_OK then
    begin
      while enum.Next(1, filter, nil) = S_OK do
      begin
        try
          FGraph.RemoveFilter(filter);
          enum.Reset;
          filter := nil;
        except
        end;
      end;
    end;
  end;

  if Assigned(FControl) then
  begin
    try
      Log(FLogger, Self, 'ClearGraph', 'Stopping the Graph');
      FControl.Stop;
      FControl := nil;
    except on E: Exception do
    begin
      Pointer(FControl) := nil;
      Log(FLogger, Self, 'ClearGraph', 'Stopping the Graph raised an Exception: ' + e.Message);
    end;
    end;
  end;

  FLock.Enter;
  try
    if Assigned(FDump) then
    begin
      Log(FLogger, Self, 'ClearGraph', 'Removing Callback on TS-Dump Filter');
      (FDump as IBDATSDump).SetCallback(nil);
      FDump := nil;
    end;

    FSource := nil;
    FGraph := nil;
    Log(FLogger, Self, 'ClearGraph', 'Graph cleared');
  finally
    FLock.Leave;
  end;
end;

end.
