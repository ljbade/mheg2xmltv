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

unit MulticastClientServer;

interface

uses
  Windows, WinSock, Classes, Messages, SysUtils, SyncObjs, StrUtils,

  Logger;

type
  TMulticastServer = class
  private
    FLogger: TLogger;
    FActive: Boolean;
    FLock: TCriticalSection;
    FWSAOK: Boolean;
    FWSAData: TWSAData;
    FSocket: Integer;
    FSockAddr: sockaddr_in;
    FPort: Word;
    FMulticastIP: String;
    procedure SetActive(AActive: Boolean);
  public
    constructor Create(ALogger: TLogger = nil);
    destructor Destroy; override;

    procedure SendBuffer(ABuffer: PByte; ASize: Integer);
  published
    property MulticastIP: String read FMulticastIP write FMulticastIP;
    property Port: Word read FPort write FPort;
    property Active: Boolean read FActive write SetActive;
    property Logger: TLogger read FLogger write FLogger;
  end;

  TOnReadData = procedure(ABuffer: PByte; ASize: Integer) of Object;

  TMulticastClient = class
  private
    FLogger: TLogger;
    FBuffer: PByte;
    FWindow: THandle;
    FWSAData: TWSAData;
    FWSAOK: Boolean;
    FSocket: Integer;
    FLock: TCriticalSection;
    FOnReadData: TOnReadData;
    FActive: Boolean;
    FPort: Word;
    FMulticastGroup: String;
    procedure OnMessage(var Message: TMessage);
    procedure SetActive(AActive: Boolean);
  public
    constructor Create(ALogger: TLogger = nil);
    destructor Destroy; override;
  published
    property MulticastGroup: String read FMulticastGroup write FMulticastGroup;
    property Port: Word read FPort write FPort;
    property Active: Boolean read FActive write SetActive;
    property OnReadData: TOnReadData read FOnReadData write FOnReadData;
    property Logger: TLogger read FLogger write FLogger;
  end;

implementation

const
  UM_SOCKET = WM_USER + $1000;

(*** TMulticastServer *********************************************************)

constructor TMulticastServer.Create(ALogger: TLogger = nil);
begin
  inherited Create;
  FLogger := ALogger;
  Log(FLogger, Self, 'Create', 'constructor');
  FActive := False;
  FLock := TCriticalSection.Create;
  FPort := 12345;
  FMulticastIP := '225.1.1.1';
  FWSAOK := WSAStartup(MAKEWORD(1,1), FWSAData) = 0;
end;

destructor TMulticastServer.Destroy;
begin
  Log(FLogger, Self, 'Destroy', 'destructor');
  SetActive(False);
  WSACleanup;
  FLock.Free;
  inherited Destroy;
end;

procedure TMulticastServer.SetActive(AActive: Boolean);
var
  sock_addr: sockaddr_in;
  res: Integer;
begin
  Log(FLogger, Self, 'SetActive', IfThen(AActive, 'True', 'False'));
  FLock.Enter;
  try
    if not FWSAOK
      then Exit;

    if AActive then
    begin
      SetActive(False);

      FSocket := socket(AF_INET, SOCK_DGRAM, 0);
      if (FSocket = INVALID_SOCKET)
        then Exit;

      FillChar(sock_addr, SizeOf(sockaddr_in), 0);
      sock_addr.sin_family := AF_INET;
      sock_addr.sin_port := htons(FPort);
      sock_addr.sin_addr.s_addr := INADDR_ANY;

      bind(FSocket, sock_addr, SizeOf(sockaddr_in));
      res := 0;
      setsockopt(FSocket, IPPROTO_IP, IP_MULTICAST_LOOP, @res, SizeOf(Integer));

      FSockAddr.sin_family := PF_INET;
      FSockAddr.sin_addr.s_addr := inet_addr(PChar(FMulticastIP));
      FSockAddr.sin_port := htons(FPort);

      FActive := True;
    end else
    begin
      if FSocket <> INVALID_SOCKET then
      begin
        closesocket(FSocket);
        FSocket := INVALID_SOCKET;
      end;
      FActive := False;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMulticastServer.SendBuffer(ABuffer: PByte; ASize: Integer);
begin
  if not FActive
    then Exit;

  FLock.Enter;
  try
    Log(FLogger, Self, 'SendBuffer', 'BufferSize ' + inttostr(ASize));
    sendto(FSocket, ABuffer^, ASize, 0, FSockAddr, SizeOf(sockaddr_in));
  finally
    FLock.Leave;
  end;
end;

(*** TMulticastClient *********************************************************)

const
  PACKET_SIZE = 512 * 1024;

constructor TMulticastClient.Create(ALogger: TLogger = nil);
begin
  inherited Create;
  FLogger := ALogger;
  Log(FLogger, Self, 'Create', 'constructor');
  FMulticastGroup := '225.1.1.1';
  FPort := 12345;
  FLock := TCriticalSection.Create;
  FActive := False;
  FBuffer := AllocMem(PACKET_SIZE);
  FWSAOK := WSAStartup(MAKEWORD(1,1), FWSAData) = 0;
  if FWSAOK
    then FWindow := Classes.AllocateHWnd(OnMessage);
end;

destructor TMulticastClient.Destroy;
begin
  Log(FLogger, Self, 'Destroy', 'destructor');
  SetActive(False);
  if FWindow <> 0
    then DeallocateHWnd(FWindow);
  WSACleanup;
  FLock.Free;
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TMulticastClient.SetActive(AActive: Boolean);
type
  ip_mreq = record
    imr_multiaddr : in_addr;
    imr_interface : in_addr;
  end;
var
  sock_ip: ip_mreq;
  sock_addr: sockaddr_in;
  res: Integer;
begin
  Log(FLogger, Self, 'SetActive', IfThen(AActive, 'True', 'False'));
  FLock.Enter;
  try
    if not FWSAOK
      then Exit;

    if AActive then
    begin
      SetActive(False);

      FSocket := WinSock.Socket(AF_INET, SOCK_DGRAM, 0);
      if FSocket = INVALID_SOCKET then
      begin
        SetActive(False);
        Exit;
      end;

      WSAAsyncSelect(FSocket, FWindow, UM_SOCKET, FD_READ);

      res := 1;
      setsockopt(FSocket, SOL_SOCKET, SO_REUSEADDR, @res, SizeOf(Integer));

      FillChar(sock_addr, SizeOf(sockaddr_in), 0);
      sock_addr.sin_family := AF_INET;
      sock_addr.sin_addr.s_addr := htonl(INADDR_ANY);
      sock_addr.sin_port := htons(FPort);
      bind(FSocket, sock_addr, SizeOf(sockaddr_in));

      res := 0;
      setsockopt(FSocket, IPPROTO_IP, IP_MULTICAST_LOOP, @res, SizeOf(Integer));
      
      FillChar(sock_ip, SizeOf(ip_mreq), 0);
      sock_ip.imr_multiaddr.s_addr := inet_addr(PChar(FMulticastGroup));
      sock_ip.imr_interface.s_addr := htonl(INADDR_ANY);

      setsockopt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP, @sock_ip, SizeOf(ip_mreq));

      FActive := True;
    end else
    begin
      if FSocket <> INVALID_SOCKET then
      begin
        closesocket(FSocket);
        FSocket := INVALID_SOCKET;
      end;
      FActive := False;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMulticastClient.OnMessage(var Message: TMessage);
var
  msg: Word;
  rec: Integer;
begin
  inherited;

  if Message.Msg = UM_SOCKET then
  begin
    msg := WSAGetSelectEvent(Message.LParam);
    case msg of
      FD_READ:
      begin
//        Log(FLogger, Self, 'OnMessage', 'received FD_READ');
        rec := recv(FSocket, FBuffer^, PACKET_SIZE, 0);
        FLock.Enter;
        try
          if Assigned(FOnReadData)
            then FOnReadData(FBuffer, rec);
        finally
          FLock.Leave;
        end;
      end;
    end;
  end;
end;

end.
