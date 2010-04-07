(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004, 2005 Milenko Mitrovic                                *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  SDK for DC-DVB Filter Version 0.1.6                                      *
 *                                                                           *
 *  The Source Code is given "as is" without warranty of any kind. The       *
 *  Author is not responsible for any damage due to the use of this Code.    *
 *  The complete Source Code remains property of the Author and must be      *
 *  used only for creating Plugins for the DC-DVB Filter.                    *
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
