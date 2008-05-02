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

unit UPIDCounter;

interface

uses
  ActiveX, Windows, Classes, SysUtils, PropSettings, PropAbout, DCDVBDataPlugins,
  SyncObjs, DCDVBShared;

const
  CLSID_PIDCounter: TGuid = '{0F7DCB37-B2CF-46BE-869D-3C08CA4CFFE3}';

type
  TPIDItem = record
    PID: Integer;
    Count: Integer;
  end;
  PPIDItem = ^TPIDItem;

  TPIDCounter = class(TDCDVBDataPlugin, IPIDCounter)
  protected
    FLock: TCriticalSection;
    FList: TList;
    procedure Initialize; override;
    // IPIDCounter
    function Reset: HRESULT; stdcall;
    function GetStatistics(out AStatistics: Pointer; out ACount: Integer): HRESULT; stdcall;
  public
    constructor Create(APluginName: String; APluginID: TGUID); override;
    destructor Destroy; override;

    function ProcessBuffer(AInBuffer: PByte; var AInSize: Integer): Integer; override; stdcall;
  end;

implementation

constructor TPIDCounter.Create(APluginName: String; APluginID: TGUID);
begin
  inherited Create(APluginName, APluginID);
  FList := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TPIDCounter.Destroy;
begin
  Reset;
  FList.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TPIDCounter.Initialize;
begin
  AddPropertyPage(CLSID_PIDCounterPropertyPage);
  AddPropertyPage(CLSID_PIDCounterPropertyPageAbout);
end;

function TPIDCounter.Reset: HRESULT;
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FList.Count -1
      do Dispose(PPIDItem(FList[i]));
    FList.Clear;
  finally
    FLock.Leave;
  end;

  Result := E_FAIL;
end;

function TPIDCounter.GetStatistics(out AStatistics: Pointer; out ACount: Integer): HRESULT;
var
  pid: PPIDItem;
  i: Integer;
begin
  Result := S_FALSE;
  
  FLock.Enter;
  try
    ACount := FList.Count;

    if ACount = 0 then
    begin
      Result := S_FALSE;
      Exit;
    end;

    AStatistics := CoTaskMemAlloc(SizeOf(TPIDItem) * FList.Count);
    pid := PPIDItem(AStatistics);

    for i := 0 to FList.Count -1 do
    begin
      pid^ := PPIDItem(FList[i])^;
      inc(pid);
    end;
  finally
    FLock.Leave;
  end;

  Result := S_OK;
end;

function TPIDCounter.ProcessBuffer(AInBuffer: PByte; var AInSize: Integer): Integer;
var
  size: Integer;
  pid: Integer;
  found: Boolean;
  i: Integer;
  pid_item: PPIDItem;
  error: Integer;
begin
  size :=  AInSize;

  FLock.Enter;
  try
    while (size > 188) do
    begin
      if (AInBuffer^ = $47) and (PByte(Cardinal(AInBuffer) + 188)^ = $47) then
      begin
        inc(AInBuffer);
        pid := AInBuffer^;
        error := pid shr 7;
        inc(AInBuffer);
        pid := AInBuffer^ or (pid shl 8);
        pid := pid and $1FFF;

        if error = 0 then
        begin
          found := False;
          for i := 0 to FList.Count -1 do
          begin
            pid_item := PPIDItem(FList[i]);
            if pid_item.PID = pid then
            begin
              found := True;
              inc(pid_item.Count);
              break;
            end;
          end;

          if not found then
          begin
            new(pid_item);
            pid_item.PID := pid;
            pid_item.Count := 1;
            FList.Add(pid_item);
          end;
        end;

        inc(AInBuffer, 186);
        dec(size, 188);
      end else
      begin
        inc(AInBuffer);
        dec(size);
      end;
    end;
  finally
    FLock.Leave;
  end;

  Result := S_OK;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TPIDCounter,
    CLSID_PIDCounter,
    'PID Counter',
    'PID Counter',
    'Milenko Mitrovic',
    '0.0.0.1'
  );

end.
