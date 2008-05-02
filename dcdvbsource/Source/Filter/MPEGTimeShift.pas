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

unit MPEGTimeShift;

interface

uses
  Windows, Classes, SysUtils, MPEGConst;

const
  UNITS         = 10000000;
  DATA_OVERHEAD = 65536;

type
  TTimeShift = class
  private
    FCallback: TBufferCallback;
    FFileWriter: THandle;
    FWrapped: Boolean;
    FStopped: Boolean;
    FReadBuffer: PByte;
    FReadBufferSize: Int64;
    FBufferSize: Int64;
    FBufferPos: Int64;
    FReadPos: Int64;
    FMaximumTime: Int64;
    FMaximumSize: Int64;
    FBytesPerSecond: Int64;
    FStartPos: Int64;
    FStartBytes: Int64;
    FStartReadBytes: Int64;
    FWritten: Int64;
    FReaden: Int64;
    procedure PushData(ASize: Integer);
    function GetDuration: Int64;
    function GetStart: Int64;
    function GetReadStart: Int64;
    function Seek(AOffset: Int64; AOrigin: Cardinal): Int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure SetFilename(AFilename: String);
    procedure ParseBuffer(ABuffer: PByte; ASize: Integer);
    procedure SetBytesPerSecond(ABytes: Int64);
    procedure SetMaximumTime(ATime: Int64);
    procedure SetPosition(APosition: Int64);
    property Stopped: Boolean read FSTopped write FStopped;
    property OnData: TBufferCallback read FCallback write FCallback;
    property Duration: Int64 read GetDuration;
    property Start: Int64 read GetStart;
    property ReadStart: Int64 read GetReadStart;
  end;

implementation

procedure DBG(AText: String);
begin
  OutputDebugString(PChar(AText));
end;

constructor TTimeShift.Create;
begin
  inherited Create;
  FCallback := nil;
  FFileWriter := 0;
  FStopped := True;
  FBytesPerSecond := 1875000;
  FReadBuffer := nil;
  FReadBufferSize := 0;
  FStartBytes := 0;
end;

destructor TTimeShift.Destroy;
begin
  if (FFileWriter <> 0) then
  begin
    CloseHandle(FFileWriter);
    FFileWriter := 0;
  end;
  if Assigned(FReadBuffer) then
  begin
    FreeMem(FReadBuffer);
    FReadBuffer := nil;
  end;
  inherited Destroy;
end;

procedure TTimeShift.SetFilename(AFilename: String);
var
  attr: Cardinal;
begin
  if (FFileWriter <> 0) then
  begin
    CloseHandle(FFileWriter);
    FFileWriter := 0;
  end;
  attr := FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_DELETE_ON_CLOSE;

  FFileWriter := CreateFile(PChar(AFilename), GENERIC_WRITE or GENERIC_READ, 0, nil, CREATE_ALWAYS, attr, 0);
  if (FFileWriter = $FFFFFFFF)
    then FFileWriter := 0;
end;

function TTimeShift.Seek(AOffset: Int64; AOrigin: Cardinal): Int64;
var
  li: LARGE_INTEGER;
begin
  Result := -1;
  if (FFileWriter <> 0) then
  begin
    li.QuadPart := AOffset;
    li.LowPart := SetFilePointer (FFileWriter, li.LowPart, @li.HighPart, AOrigin);
    if ((li.LowPart = DWORD(-1)) and (GetLastError <> NO_ERROR))
      then li.QuadPart := -1;
    Result := li.QuadPart;
  end;
end;

procedure TTimeShift.Reset;
begin
  FBufferPos := 0;
  FBufferSize := 0;
  FReadPos := 0;
  FStartPos := 0;
  FWrapped := False;
  FStartBytes := 0;
  FStartReadBytes := 0;
  FWritten := 0;
  FReaden := 0;
end;

procedure TTimeShift.SetMaximumTime(ATime: Int64);
begin
  if ATime < 5
    then ATime := 5;
  FMaximumTime := ATime;
  SetBytesPerSecond(FBytesPerSecond);
end;

procedure TTimeShift.SetBytesPerSecond(ABytes: Int64);
begin
  FBytesPerSecond := ABytes;
  FMaximumSize := FMaximumTime * FBytesPerSecond + DATA_OVERHEAD;
end;

function TTimeShift.GetDuration: Int64;
begin
  if (FBytesPerSecond <= 0) then
  begin
    Result := 0;
  end else
  begin
    Result := FBufferSize * UNITS div FBytesPerSecond
  end;
end;

function TTimeShift.GetStart: Int64;
begin
  if (FBytesPerSecond > 0)
    then Result := FStartBytes * UNITS div FBytesPerSecond
    else Result := 0;
end;

procedure TTimeShift.ParseBuffer(ABuffer: PByte; ASize: Integer);
var
  written: Cardinal;
begin
  if (FFileWriter = 0) or not Assigned(FCallback) or
     (ASize <= 0) or (ABuffer = nil)
    then Exit;

  if (FBufferPos + ASize > FMaximumSize) then
  begin
    Seek(0, FILE_BEGIN);
    WriteFile(FFileWriter, ABuffer^, ASize, written, nil);
    FBufferSize := FBufferPos;
    FBufferPos := ASize;
    FWrapped := True;
  end else
  begin
    Seek(FBufferPos, FILE_BEGIN);
    WriteFile(FFileWriter, ABuffer^, ASize, written, nil);
    inc(FBufferPos, ASize);
    if FBufferSize < FBufferPos
      then FBufferSize := FBufferPos;
  end;

  Sleep(1);

  if FWrapped then
  begin
    FStartBytes := FStartBytes + ASize;
  end;

  FWritten := FWritten + ASize;

  PushData(ASize);
end;

procedure TTimeShift.PushData(ASize: Integer);
var
  readen: Cardinal;
  pos: Int64;
  totalsize: Int64;
  size: Int64;
begin
  if FStopped
    then Exit;

  if (FReadBufferSize < ASize) then
  begin
    if (FReadBuffer = nil)
      then FReadBuffer := AllocMem(ASize)
      else FReadBuffer := ReallocMemory(FReadBuffer, ASize);
    FReadBufferSize := ASize;
  end;

  if FReadPos < FWritten - FBufferSize
    then FReadPos := FWritten - FBufferSize;

  if FReadPos > FWritten - ASize
    then FReadPos := FWritten - ASize;

  pos := FReadPos;
  totalsize := FStartBytes + FBufferSize;

  pos := pos - totalsize + FBufferPos;
  if pos < 0
    then pos := pos + FBufferSize;

  size := FBufferSize - pos;

  if size > ASize
    then size := ASize;

  Seek(pos, FILE_BEGIN);
  ReadFile(FFileWriter, FReadBuffer^, size, readen, nil);

  if size < ASize then
  begin
    inc(FReadBuffer, size);
    Seek(0, FILE_BEGIN);
    ReadFile(FFileWriter, FReadBuffer^, ASize - size, readen, nil);
    dec(FReadBuffer, size);
  end;

  FCallback(FReadBuffer, ASize);
  inc(FReadPos, ASize);
end;

procedure TTimeShift.SetPosition(APosition: Int64);
begin
  if (FBytesPerSecond = 0)
    then Exit;

  if (APosition < GetStart)
    then APosition := GetStart
  else if (APosition > GetStart + GetDuration)
    then APosition := GetStart + GetDuration;

  FReadPos := APosition * FBytesPerSecond div UNITS;
end;

function TTimeShift.GetReadStart: Int64;
var
  time: Int64;
begin
  time := GetStart;

  if (FBytesPerSecond = 0) then
  begin
    Result := time;
    Exit;
  end;

  Result := FReadPos * UNITS div FBytesPerSecond;

  if Result < time
    then Result := time;
end;

end.
