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

unit TCPTransportStream;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils,

  TCPClientServer, Logger, TransportStream;

type
  TTCPTransportStream = class;

  TTCPThread = class(TThread)
  protected
    FOwner: TTCPTransportStream;
    FClient: TTCPClientSocket;
    procedure Execute; override;
  public
    constructor Create(AOwner: TTCPTransportStream);
  end;

  TTCPTransportStream = class(TTransportStream)
  protected
    FThread: TTCPThread;
//    FClient: TTCPClientSocket;
    function GetActive: Boolean; override;
    procedure SetActive(AActive: Boolean); override;
    function GetPort: Word;
    procedure SetPort(APort: Word);
    function GetIP: String;
    procedure SetIP(AIP: String);
  public
    constructor Create(ALogger: TLogger = nil); override;
    destructor Destroy; override;
  published
    property IP: String read GetIP write SetIP;
    property Port: Word read GetPort write SetPort;
  end;

implementation

(*** TTCPTransportStream ******************************************************)

const
  UM_CLOSE_CLIENT = WM_USER + 100;
  UM_CONNECT_CLIENT = WM_USER + 101;

constructor TTCPThread.Create(AOwner: TTCPTransportStream);
begin
  inherited Create(True);
  FOwner := AOwner;
//  FreeOnTerminate := True;
  Resume;
end;

procedure TTCPThread.Execute;
var
  msg: TMsg;
begin
  FClient := TTCPClientSocket.Create;
  FClient.OnReadData := FOwner.PushData;

  while (GetMessage(msg, 0, 0, 0)) do
  begin
    case msg.message of
      UM_CLOSE_CLIENT: FClient.Close;
      UM_CONNECT_CLIENT: FClient.Connect;
    end;

    if Terminated
      then break;

    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

constructor TTCPTransportStream.Create(ALogger: TLogger = nil);
begin
  inherited Create(ALogger);
  Log(FLogger, Self, 'Create', 'constructor');
  FThread := TTCPThread.Create(Self);
  while (FThread.FClient = nil)
    do sleep(5);
//  FClient := TTCPClientSocket.Create();
//  FClient.OnReadData := PushData;
end;

destructor TTCPTransportStream.Destroy;
begin
  Log(FLogger, Self, 'Destroy', 'destructor');
  SetActive(False);
  FThread.FClient.Free;
  FThread.FClient := nil;
  FThread.Terminate;
  PostThreadMessage(FThread.ThreadID, WM_NULL, 0, 0);
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;
  inherited Destroy;
end;

function TTCPTransportStream.GetActive: Boolean;
begin
  FLock.Enter;
  try
    if not Assigned(FThread) or not Assigned(FThread.FClient)
      then Result := False
      else Result := FThread.FClient.IsConnected;
//    Log(FLogger, Self, 'GetActive', 'Returned: ' + IfThen(Result, 'True', 'False'));
  finally
    FLock.Leave;
  end;
end;

procedure TTCPTransportStream.SetActive(AActive: Boolean);
var
  i: Integer;
begin
  FLock.Enter;
  try
    Log(FLogger, Self, 'SetActive', 'SetStatus: ' + IfThen(AActive, 'True', 'False') + ' Current Status: ' + IfThen(GetActive, 'True', 'False'));
    if (GetActive = AActive)
      then Exit;

    if not Assigned(FThread) or not Assigned(FThread.FClient)
      then Exit;

    if AActive then
    begin
      PostThreadMessage(FThread.ThreadID, UM_CONNECT_CLIENT, 0, 0);
      for i := 0 to 100 do
      begin
        if FThread.FClient.IsConnected
          then break;
        sleep(20);
      end;
//      FThread.FClient.Connect;
    end else
    begin
      PostThreadMessage(FThread.ThreadID, UM_CLOSE_CLIENT, 0, 0);
//      FThread.FClient.Close;
    end;
  finally
    FLock.Leave;
  end;
end;

function TTCPTransportStream.GetPort: Word;
begin
  FLock.Enter;
  try
    Result := FThread.FClient.Port;
    Log(FLogger, Self, 'GetPort', 'Returned: ' + inttostr(GetPort));
  finally
    FLock.Leave;
  end;
end;

procedure TTCPTransportStream.SetPort(APort: Word);
var
  was_active: Boolean;
begin
  FLock.Enter;
  try
    Log(FLogger, Self, 'SetPort', 'to ' + inttostr(APort));
    was_active := GetActive;
    SetActive(False);
    FThread.FClient.Port := APort;
    SetActive(was_active);
  finally
    FLock.Leave;
  end;
end;

function TTCPTransportStream.GetIP: String;
begin
  FLock.Enter;
  try
    Result := FThread.FClient.Host;
    Log(FLogger, Self, 'GetIP', 'returning ' + Result);
  finally
    FLock.Leave;
  end;
end;

procedure TTCPTransportStream.SetIP(AIP: String);
var
  was_active: Boolean;
begin
  FLock.Enter;
  try
    Log(FLogger, Self, 'SetIP', 'to ' + AIP);
    was_active := GetActive;
    SetActive(False);
    FThread.FClient.Host := AIP;
    SetActive(was_active);
  finally
    FLock.Leave;
  end;
end;

end.
