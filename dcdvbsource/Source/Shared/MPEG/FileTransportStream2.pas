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

unit FileTransportStream2;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils,

  Logger, TransportStream, BDAUtils, MPEGParser;

type
  TFileTransportStream2 = class;

  TStreamThread = class(TThread)
  private
    FOwner: TFileTransportStream2;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFileTransportStream2);
  end;

  TFileTransportStream2 = class(TTransportStream)
  protected
    FTSParser: TTSParser;
    FPCR: TPayloadParser;
    FBuffer: PByte;
    FBufferSize: Integer;
    FStreamThread: TStreamThread;
    FPCRPID: Integer;
    FFileName: String;
    FFileStream: TFileStream;
    function GetActive: Boolean; override;
    procedure SetActive(AActive: Boolean); override;
    procedure SetFileName(AFileName: String);
    procedure ReadData;
    procedure SetPCRPID(APCRPID: Integer);
  public
    constructor Create(ALogger: TLogger = nil); override;
    destructor Destroy; override;
  published
    property PCRPID: Integer read FPCRPID write SetPCRPID;
    property FileName: String read FFileName write SetFileName;
  end;

implementation

(*** TStreamThread ************************************************************)

constructor TStreamThread.Create(AOwner: TFileTransportStream2);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

procedure TStreamThread.Execute;
begin
  Priority := tpHighest;
  while not Terminated do
  begin
    FOwner.ReadData;
    Sleep(10);
  end;
end;

(*** TFileTransportStream2 ****************************************************)

constructor TFileTransportStream2.Create(ALogger: TLogger = nil);
begin
  inherited Create(ALogger);
  FTSParser := TTSParser.Create(True);
  FPCR := TPayloadParser.Create(FTSParser, -1, nil, True);
  FBufferSize := 272 * 188;
  FBuffer := AllocMem(FBufferSize);
  FStreamThread := TStreamThread.Create(Self);
  FFileStream := nil;
end;

destructor TFileTransportStream2.Destroy;
begin
  SetActive(False);
  Sleep(10);
  FStreamThread.Resume;
  FStreamThread.Terminate;
  FStreamThread.Free;
  FreeMemory(FBuffer);
  FPCR.Free;
  FTSParser.Free;
  inherited Destroy;
end;

function TFileTransportStream2.GetActive: Boolean;
begin
  Result := Assigned(FFileStream);
//  Log(FLogger, Self, 'GetActive', 'Returned: ' + IfThen(Assigned(FFileStream), 'True', 'False'));
end;

procedure TFileTransportStream2.SetActive(AActive: Boolean);
begin
  FLock.Enter;
  try
    Log(FLogger, Self, 'SetActive', 'SetStatus: ' + IfThen(AActive, 'True', 'False') + ' Current Status: ' + IfThen(GetActive, 'True', 'False'));
    if (GetActive = AActive)
      then Exit;

    if Assigned(FFileStream) then
    begin
      FFileStream.Free;
      FFileStream := nil;
    end;

    if not FStreamThread.Suspended
      then FStreamThread.Suspend;

    if AActive and (FFileName <> '') then
    begin
      try
        FFileStream := TFileStream.Create(FFileName, fmOpenRead);
      except
        FFileStream := nil;
        Exit;
      end;

      FStreamThread.Resume;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFileTransportStream2.SetFileName(AFileName: String);
var
  was_active: Boolean;
begin
  FLock.Enter;
  try
    Log(FLogger, Self, 'SetFileName', 'FileName: ' + AFileName);
    was_active := GetActive;
    SetActive(False);
    FFileName := AFileName;
    SetActive(was_active);
  finally
    FLock.Leave;
  end;
end;

procedure TFileTransportStream2.ReadData;
var
  c: Integer;
begin
  FLock.Enter;
  try
    if Assigned(FFileStream) and Assigned(FBuffer) then
    begin
      c := FFileStream.Read(FBuffer^, FBufferSize);
      if c > 0 then
      begin
        PushData(FBuffer, c);
      end else
      begin
        FFileStream.Seek(0, soFromBeginning);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFileTransportStream2.SetPCRPID(APCRPID: Integer);
begin
  FLock.Enter;
  try
    FPCRPID := APCRPID;
    FPCR.PID := FPCRPID;
  finally
    FLock.Leave;
  end;
end;

end.
