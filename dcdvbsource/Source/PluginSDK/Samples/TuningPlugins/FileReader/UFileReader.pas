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

unit UFileReader;

interface

uses
  DirectShow9, ActiveX, DSUtil, Windows, SysUtils, Classes, DCDVBTuningPlugins,
  PropSettings, PropAbout, DCDVBShared, SyncObjs, MMSystem;

const
  CLSID_DCDVB_FileReader: TGuid  = '{36339372-4C98-48BE-B956-0072E3973065}';

type
  TFileReaderThread = class;

  TFileReaderDevice = class(TDCDVBTuningPluginDevice, IFileReaderDevice)
  protected
    FFileThread: TFileReaderThread;
    FFileLock: TCriticalSection;
    FActive: Boolean;
    FMappingList: TList;
    FSignalStats: TSignalStatistics;
    procedure ClearMappings;
    procedure ClearThread;
    function Activate: Integer;
    function DeActivate: Integer;
    function GetSignalStatistics(AStatistics: Pointer): Integer;
    function LoadSettings(ABuffer: PByte; ASize: Integer): Integer;
    function SaveSettings(out ABuffer: PByte; out ASize: Integer): Integer;
    function TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;
    function OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer; override;
    procedure Initialize; override;
    procedure ResetSignalStats(ASignalFound: Boolean);
    // IFileReaderDevice
    function SetFileMappings(AMappings: PFileMapping; ACountMappings: Integer): HRESULT; stdcall;
    function GetFileMappings(out AMappings: PFileMapping; out ACountMappings: Integer): HRESULT; stdcall;
  public
    constructor Create(ADeviceName: String; ADeviceType: Integer; ADeviceID: String); override;
    destructor Destroy; override;
  end;

  TFileReader = class(TDCDVBTuningPlugin)
  protected
    procedure Initialize; override;
  end;

  TFileReaderThread = class(TThread)
  protected
    FOwner: TFileReaderDevice;
    FFile: TFileStream;
    procedure Execute; override;
  public
    constructor Create(AOwner: TFileReaderDevice; AFilename: WideString);
    destructor Destroy; override;
  end;

implementation

function SwapLongLong(Value: Int64): Int64;
begin
{$IFDEF BIG_ENDIAN}
  Result := Value;
{$ELSE}
  PByteArray(@Result)^[0] := PByteArray(@Value)[7];
  PByteArray(@Result)^[1] := PByteArray(@Value)[6];
  PByteArray(@Result)^[2] := PByteArray(@Value)[5];
  PByteArray(@Result)^[3] := PByteArray(@Value)[4];
  PByteArray(@Result)^[4] := PByteArray(@Value)[3];
  PByteArray(@Result)^[5] := PByteArray(@Value)[2];
  PByteArray(@Result)^[6] := PByteArray(@Value)[1];
  PByteArray(@Result)^[7] := PByteArray(@Value)[0];
{$ENDIF}
end;

constructor TFileReaderThread.Create(AOwner: TFileReaderDevice; AFilename: WideString);
begin
  inherited Create(True);
  FOwner := AOwner;
  try
    FFile := TFileStream.Create(AFilename, fmOpenRead);
  except
    FFile := nil;
  end;

  FOwner.ResetSignalStats(False);

  Resume;
end;

destructor TFileReaderThread.Destroy;
begin
  if Assigned(FFile) then
  begin
    FFile.Free;
    FFile := nil;
  end;
  inherited Destroy;
end;

function GetPCRPID(AFile: TFileStream): Integer;
var
  buffer: PByte;
  buffer2: PByte;
  buffer3: PByte;
  buffer_size: Integer;
  tmp: Integer;
  pid: Integer;
  adap_field_ctrl: Integer;
  adap_field_len: Integer;
  pcrflag: Integer;
const
  NUM_PACKETS = 2000;
begin
  Result := -1;
  buffer_size := 188 * NUM_PACKETS;
  buffer := AllocMem(buffer_size);
  buffer2 := buffer;

  AFile.Seek(0, soFromBeginning);
  buffer_size := AFile.Read(buffer^, buffer_size);

  while buffer_size > 188 do
  begin
    if (buffer2^ <> $47) then
    begin
      inc(buffer2);
      dec(buffer_size);
    end;

    inc(buffer2, 188);
    tmp := buffer^;
    dec(buffer2, 188);

    if (tmp <> $47) then
    begin
      inc(buffer2);
      dec(buffer_size);
    end;

    buffer3 := buffer2;

    // read PID
    inc(buffer3);
    pid := buffer3^ and $1F;
    pid := pid shl 8;
    inc(buffer3);
    pid := pid or buffer3^;

    inc(buffer3);

    // check for PCR
    adap_field_ctrl := (buffer3^ and $30) shr 4;
    inc(buffer3);

    if (adap_field_ctrl > 1) then
    begin
      adap_field_len := buffer3^;
      inc(buffer3);

      if (adap_field_len > 0) then
      begin
        pcrflag := (buffer3^ and $10) shr 4;
        if (pcrflag = 1) then
        begin
          Result := pid;
          break;
        end;
      end;
    end;

    inc(buffer2, 188);
    dec(buffer_size, 188);
  end;

  FreeMem(buffer);
end;

procedure TFileReaderThread.Execute;
var
  buffer: PByte;
  buffer2: PByte;
  buffer3: PByte;
  readen: Integer;
  pcrpid: Integer;
  FFirstPCR: Int64;
  FFirstTime: Int64;
  readen_size: Integer;
  tmp: Integer;
  pid: Integer;
  adap_field_ctrl: Integer;
  adap_field_len: Integer;
  pcrflag: Integer;
  pcr, t1: Int64;
  difference: Int64;
  syst, pcrt, diff: Int64;
const
  READ_SIZE = 188 * 50;
begin
  Priority := tpHighest;
  
  if FFile = nil then
  begin
    FOwner.ResetSignalStats(False);
    Exit;
  end;

  // search PCR PID
  pcrpid := GetPCRPID(FFile);
  if (pcrpid = -1) then
  begin
    FOwner.ResetSignalStats(False);
    Exit;
  end;

//  OutputDebugString(PChar('Found PCR PID at PID: ' + inttostr(pcrpid)));

  FOwner.ResetSignalStats(True);
  buffer := AllocMem(READ_SIZE);
  FFirstPCR := -1;
  difference := 0;
  FFirstTime := -1;

  while not Terminated do
  begin
    readen := FFile.Read(buffer^, READ_SIZE);

    if readen = 0 then
    begin
      FFile.Seek(0, soFromBeginning);
      FFirstPCR := -1;
      Continue;
    end;

    readen_size := readen;
    buffer2 := buffer;

    while readen_size > 188 do
    begin
      if (buffer2^ <> $47) then
      begin
        dec(readen_size);
        inc(buffer2);
      end;
      
      inc(buffer2, 188);
      tmp := buffer^;
      dec(buffer2, 188);

      if (tmp <> $47) then
      begin
        inc(buffer2);
        dec(readen_size);
      end;

      buffer3 := buffer2;

      // read PID
      inc(buffer3);
      pid := buffer3^ and $1F;
      pid := pid shl 8;
      inc(buffer3);
      pid := pid or buffer3^;
      if pid = pcrpid then
      begin
        inc(buffer3);

        // check for PCR
        adap_field_ctrl := (buffer3^ and $30) shr 4;
        inc(buffer3);

        if (adap_field_ctrl > 1) then
        begin
          adap_field_len := buffer3^;
          inc(buffer3);

          if (adap_field_len > 0) then
          begin
            pcrflag := (buffer3^ and $10) shr 4;
            if (pcrflag = 1) then
            begin
              inc(buffer3);

              pcr := buffer3^;
              pcr := pcr shl 8;
              inc(buffer3);

              pcr := pcr or buffer3^;
              pcr := pcr shl 8;
              inc(buffer3);

              pcr := pcr or buffer3^;
              pcr := pcr shl 8;
              inc(buffer3);

              pcr := pcr or buffer3^;
              pcr := pcr shl 8;
              inc(buffer3);

              pcr := pcr shl 1;

              if (buffer3^ and $80 > 0)
                then pcr := pcr + 1;

//              t1 := buffer3^ and $01;
//              t1 := t1 shl 8;
//              inc(buffer3);
//              t1 := t1 or buffer3^;

//              OutputDebugString(PChar(inttostr(pcr div 90)));

// PCR_ base(i) = ((system_ clock_ frequency ´ t(i)) DIV 300) % 2^33
// PCR_ ext(i)  = ((system_ clock_ frequency ´ t(i)) DIV 1) % 300
// PCR(i)       = PCR_ base(i) ´ 300 + PCR_ ext(i)

//              pcr := (pcr * 300) + t1;
//              pcr := pcr div 27000;
              pcr := pcr div 27000;

//              OutputDebugString(PChar(inttostr(pcr)));

              if (FFirstPCR = -1) then
              begin
                FFirstTime := timeGetTime;
                FFirstPCR := pcr; // MSec
                difference := FFirstPCR - FFirstTime;
//                OutputDebugString(PChar('D1: ' + IntToStr(FFirstPCR)));
              end else
              begin
                syst := timeGetTime;
                pcrt := pcr;
                diff := pcrt - syst;

//                OutputDebugString(PChar('--------------------------'));
//                OutputDebugString(PChar('PCR: ' + inttostr(pcrt - FFirstPCR)));
//                OutputDebugString(PChar('SYS: ' + inttostr(syst - FFirstTime)));



                if pcrt < FFirstPCR then
                begin
//                  OutputDebugString('AAAAAAAAAAAAAAAAAAAAAAA');
                  FFirstPCR := -1; // pcr changed
                  break;
                end;

//                OutputDebugString(PChar('D2: ' + inttostr(diff) + ' - ' + inttostr(diff - difference)));
//                OutputDebugString(PChar('D2: ' + inttostr(diff - difference)));

//                while (diff > difference) do
                while (pcrt - FFirstPCR > syst - FFirstTime) do
                begin
                  if Terminated
                    then break;

                  sleep(100);
                  syst := timeGetTime;
                  diff := pcrt - syst;
//                  if diff < 0
//                    then diff := diff * -1;
//                  OutputDebugString(PChar('D3: ' + inttostr(diff) + ' - ' + inttostr(diff - difference)));

                end;

              end;
            end;
          end;
        end;
      end;

      inc(buffer2, 188);
      dec(readen_size, 188);
    end;

    FOwner.PushData(buffer, readen);
  end;

  FreeMem(buffer);
end;

(*** TFileReaderDevice ********************************************************)

constructor TFileReaderDevice.Create(ADeviceName: String; ADeviceType: Integer; ADeviceID: String);
begin
  inherited Create(ADeviceName, ADeviceType, ADeviceID);
  FFileLock := TCriticalSection.Create;
  FMappingList := TList.Create;
end;

destructor TFileReaderDevice.Destroy;
begin
  DeActivate;
  ClearMappings;
  FMappingList.Free;
  FFileLock.Free;
  inherited Destroy;
end;

procedure TFileReaderDevice.ClearMappings;
var
  i: Integer;
  mapping: PFileMapping;
begin
  for i := 0 to FMappingList.Count - 1 do
  begin
    mapping := FMappingList[i];
    Dispose(mapping);
  end;

  FMappingList.Clear;
end;

procedure TFileReaderDevice.Initialize;
begin
  AddPropertyPage(CLSID_FileReaderPropertyPageSettings);
  AddPropertyPage(CLSID_FileReaderPropertyPageAbout);
  FActive := False;
  ResetSignalStats(False);
end;

function TFileReaderDevice.OnControlMessage(AMessage: Integer; AParam1: Integer; AParam2: Integer): Integer;
begin
  case AMessage of
    DCDVB_TUNING_PLUGIN_CTRL_ACTIVATE:                Result := Activate;
    DCDVB_TUNING_PLUGIN_CTRL_DEACTIVATE:              Result := DeActivate;
    DCDVB_TUNING_PLUGIN_CTRL_GET_ACTIVATE_STATE:      Result := _IF(not FActive, DCDVB_TUNING_PLUGIN_INACTIVE, DCDVB_TUNING_PLUGIN_ACTIVE);
    DCDVB_TUNING_PLUGIN_CTRL_LOAD_SETTINGS:           Result := LoadSettings(PByte(AParam1), AParam2);
    DCDVB_TUNING_PLUGIN_CTRL_SAVE_SETTINGS:           Result := SaveSettings(PPByte(AParam1)^, PInteger(AParam2)^);
    DCDVB_TUNING_PLUGIN_CTRL_GET_SIGNAL_STATISTICS:   Result := GetSignalStatistics(Pointer(AParam1));
    DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST:             Result := TuneRequest(Pointer(AParam1), AParam2);
    else                                              Result := E_NOTIMPL;
  end;
end;

function TFileReaderDevice.Activate: Integer;
begin
  if FActive then
  begin
    Result := S_FALSE;
    Exit;
  end;

  FActive := True;

  Result := S_OK;
end;

function TFileReaderDevice.DeActivate: Integer;
begin
  if not FActive then
  begin
    Result := S_FALSE;
    Exit;
  end;

  ClearThread;

  FActive := False;
  Result := S_OK;
end;

procedure TFileReaderDevice.ClearThread;
begin
  if Assigned(FFileThread) then
  begin
    FFileThread.Terminate;
    FFileThread.WaitFor;
    FFileThread.Free;
    FFileThread := nil;
  end;
end;

procedure TFileReaderDevice.ResetSignalStats(ASignalFound: Boolean);
begin
  FSignalStats.Size := SizeOf(TSignalStatistics);
  FSignalStats.SignalLocked := ASignalFound;
  FSignalStats.SignalPresent := ASignalFound;
  if ASignalFound then
  begin
    FSignalStats.SignalStrength := 10000;
    FSignalStats.SignalQuality := 100;
  end else
  begin
    FSignalStats.SignalStrength := 0;
    FSignalStats.SignalQuality := 0;
  end;
end;

function TFileReaderDevice.LoadSettings(ABuffer: PByte; ASize: Integer): Integer;
var
  count_mappings: Integer;
  i: Integer;
  mapping: PFileMapping;
begin
  Result := S_OK;

  if ASize > 0 then
  begin
    count_mappings := PInteger(ABuffer)^;
    inc(ABuffer, 4);

    if (ASize <> ((count_mappings * sizeof(TFileMapping)) + sizeof(Integer))) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

    for i := 0 to count_mappings - 1 do
    begin
      new(mapping);

      mapping^ := PFileMapping(ABuffer)^;
      inc(ABuffer, sizeof(TFileMapping));

      FMappingList.Add(mapping);
    end;
  end;
end;

function TFileReaderDevice.SaveSettings(out ABuffer: PByte; out ASize: Integer): Integer;
var
  buffer_size: Integer;
  buffer: PByte;
  i: Integer;
begin
  buffer_size := sizeof(TFileMapping) * FMappingList.Count;
  buffer_size := buffer_size + sizeof(Integer);

  buffer := CoTaskMemAlloc(buffer_size);
  ABuffer := buffer;
  ASize := buffer_size;

  PInteger(buffer)^ := FMappingList.Count;
  inc(buffer, 4);

  for i := 0 to FMappingList.Count - 1 do
  begin
    PFileMapping(buffer)^ := PFileMapping(FMappingList[i])^;
    inc(buffer, sizeof(TFileMapping));
  end;

  Result := S_OK;
end;

function TFileReaderDevice.SetFileMappings(AMappings: PFileMapping; ACountMappings: Integer): HRESULT;
var
  i: Integer;
  mapping: PFileMapping;
begin
  FFileLock.Enter;
  try
    ClearMappings;

    if (AMappings = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    for i := 0 to ACountMappings - 1 do
    begin
      new(mapping);
      mapping^ := AMappings^;

      FMappingList.Add(mapping);

      inc(AMappings);
    end;

    Result := S_OK;
  finally
    FFileLock.Leave;
  end;
end;

function TFileReaderDevice.GetFileMappings(out AMappings: PFileMapping; out ACountMappings: Integer): HRESULT; stdcall;
var
  i: Integer;
  mapping: PFileMapping;
begin
  if (@AMappings = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if (@ACountMappings = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  AMappings := nil;
  ACountMappings := FMappingList.Count;

  if (ACountMappings = 0) then
  begin
    Result := E_FAIL;
    Exit;
  end;


  AMappings := CoTaskMemAlloc(ACountMappings * SizeOf(TFileMapping));
  mapping := AMappings;

  for i := 0 to FMappingList.Count - 1 do
  begin
    mapping^ := PFileMapping(FMappingList[i])^;
    inc(mapping);
  end;

  Result := S_OK;
end;

function TFileReaderDevice.TuneRequest(ATuneRequest: Pointer; ATuneRequestType: Integer): Integer;
var
  freq: Int64;
  mapping: PFileMapping;
  i: Integer;
  filename: WideString;
begin
  Log('TuneRequest', 'Request to change the Frequency/Transponder');

  freq := -1;
  filename := '';
  ClearThread;

  if (ATuneRequest = nil) then
  begin
    Log('TuneRequest', 'No TuningSpace present');
    Result := E_POINTER;
    Exit;
  end;

  if (ATuneRequestType = DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN) then
  begin
    Log('TuneRequest', 'TuneRequestType is Unknown');
    Result := E_INVALIDARG;
    Exit;
  end;

  case ATuneRequestType of
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:
    begin
      with PDVBTTuneRequest(ATuneRequest)^ do
      begin
        freq := Frequency;
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:
    begin
      with PDVBCTuneRequest(ATuneRequest)^ do
      begin
        freq := Frequency;
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:
    begin
      with PDVBSTuneRequest(ATuneRequest)^ do
      begin
        freq := Frequency;
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:
    begin
      with PATSCTuneRequest(ATuneRequest)^ do
      begin
        freq := Frequency;
      end;
    end;
  end;

  ResetSignalStats(False);

  if (freq = -1) then
  begin
    Result := S_OK;
    Exit;
  end;

  for i := 0 to FMappingList.Count - 1 do
  begin
    mapping := FMappingList[i];
    if mapping.AFrequency = freq then
    begin
      filename := mapping.AFilename;
      break;
    end;
  end;

  if (filename = '') then
  begin
    Result := S_OK;
    Exit;
  end;

  if not FileExists(filename) then
  begin
    Result := S_OK;
    Exit;
  end;

  FFileThread := TFileReaderThread.Create(Self, filename);

  Result := S_OK;
end;

function TFileReaderDevice.GetSignalStatistics(AStatistics: Pointer): Integer;
begin
  Log('GetSignalStatistics', 'Receiving Statistics');
  if not Assigned(AStatistics) then
  begin
    Log('GetSignalStatistics', 'Statistics is NULL');
    Result := E_POINTER;
    Exit;
  end;

  if PSignalStatistics(AStatistics)^.Size <> SizeOf(TSignalStatistics) then
  begin
    Log('GetSignalStatistics', 'Statistics definition is unknown');
    Result := E_FAIL;
    Exit;
  end;

  PSignalStatistics(AStatistics)^ := FSignalStats;

  with PSignalStatistics(AStatistics)^ do
  begin
    Log('GetSignalStatistics', 'SignalPresent: ' + inttostr(Integer(SignalPresent)) +
        ' SignalLocked: ' + inttostr(Integer(SignalLocked)) +
        ' SignalStrength: ' + inttostr(SignalStrength) +
        ' SignalQuality: ' + inttostr(SignalQuality));
  end;

  Result := S_OK;
end;

(*** TBDATuner ****************************************************************)

procedure TFileReader.Initialize;
begin
  FDeviceList.Add(TFileReaderDevice.Create('File Reader', DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT, GUIDToString(CLSID_DCDVB_FileReader)));
end;

initialization

  TDCDVBClassFactory.Create
  (
    TFileReader,                              // Tuner Class Name
    CLSID_DCDVB_FileReader,                   // Tuner CLSID
    'File Reader',                            // Name
    'File Reader Plugin for DC-DVB Source',   // Description
    'Milenko Mitrovic <dcoder@dsp-worx.de>',  // Author
    '0.0.0.2'                                 // Version
  );

end.
