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

unit TCPClientServer;

interface

uses
  Windows, WinSock, Classes, Messages, SysUtils, SyncObjs;

type
  TTCPServerSocket = class
  private
    FIsListening: Boolean;
    FLock: TCriticalSection;
    FClients: TList;
    FWSAOK: Boolean;
    FWSAData: TWSAData;
    FSocket: Integer;
    FWindow: THandle;
    FPort: Word;
    procedure OnMessage(var Message: TMessage);
    procedure DisconnectClients;
  public
    procedure Listen;
    procedure Close;

    procedure SendBuffer(ABuffer: PByte; ASize: Integer);

    constructor Create;
    destructor Destroy; override;
  published
    property Port: Word read FPort write FPort;
    property IsListening: Boolean read FIsListening;
  end;

  TOnReadData = procedure(ABuffer: PByte; ASize: Integer) of Object;

  TTCPClientSocket = class
  private
    FBuffer: PByte;
    FWindow: THandle;
    FWSAData: TWSAData;
    FWSAOK: Boolean;
    FSocket: Integer;
    FLock: TCriticalSection;
    FOnReadData: TOnReadData;
    FIsConnected: Boolean;
    FPort: Word;
    FHost: String;
    procedure OnMessage(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Close;
  published
    property Host: String read FHost write FHost;
    property Port: Word read FPort write FPort;
    property IsConnected: Boolean read FIsConnected;
    property OnReadData: TOnReadData read FOnReadData write FOnReadData;
    property Lock: TCriticalSection read FLock;
  end;

implementation

const
  UM_SOCKET = WM_USER + $1000;

type
  TTCPClient = class
  private
    FSocket: Integer;
    FOwner: TTCPServerSocket;
  public
    constructor Create(ASocket: Integer; AOwner: TTCPServerSocket);
    destructor Destroy; override;
    procedure SendBuffer(ABuffer: PByte; ASize: Integer);
  end;

//procedure Dbg(const Msg: String);
//begin
//  OutputDebugString(PChar(Msg));
//end;

(*** TTCPServerSocket *********************************************************)

constructor TTCPServerSocket.Create;
begin
  inherited Create;
  FIsListening := False;
  FLock := TCriticalSection.Create;
  FClients := TList.Create;
  FPort := 12345;
  FWSAOK := WSAStartup(MAKEWORD(1,1), FWSAData) = 0;
  if FWSAOK
    then FWindow := Classes.AllocateHWnd(OnMessage);
end;

destructor TTCPServerSocket.Destroy;
begin
  DisconnectClients;
  Close;
  if FWindow <> 0
    then DeallocateHWnd(FWindow);
  WSACleanup;
  FLock.Free;
  FClients.Free;
  inherited Destroy;
end;

procedure TTCPServerSocket.DisconnectClients;
var
  client: TTCPClient;
begin
  FLock.Enter;
  try
    while FClients.Count > 0 do
    begin
      client := FClients[0];
//      Dbg('DisconnectClients -> Closing Client Socket ' + inttostr(client.FSocket));
      closesocket(client.FSocket);
      client.Free;
      FClients.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TTCPServerSocket.OnMessage(var Message: TMessage);
var
  msg: Word;
  addr: TSockAddrIn;
  len: Integer;
  socket: Integer;
  client: TTCPClient;
  i: Integer;
begin
  inherited;

  if Message.Msg = UM_SOCKET then
  begin
    msg := WSAGetSelectEvent(Message.LParam);
    case msg of
      FD_CLOSE:
      begin
//        Dbg('FD_CLOSE -> Socket ' + inttostr(message.WParam));
        FLock.Enter;
        try
          for i := 0 to FClients.Count -1 do
          begin
            client := FClients[i];
            if client.FSocket = message.WParam then
            begin
//              Dbg('FD_CLOSE -> Found Socket ' + inttostr(message.WParam) + ' -> deleting');
              FClients.Delete(i);
              client.Free;
              Exit;
            end;
          end;
        finally
          FLock.Leave;
        end;
      end;
      FD_ACCEPT:
      begin
        len := SizeOf(TSockAddrIn);
        socket := Accept(FSocket, @addr, @len);
        if socket <> INVALID_SOCKET then
        begin
//          Dbg('FD_ACCEPT -> New Socket ' + inttostr(socket));
          client := TTCPClient.Create(socket, Self);
          FLock.Enter;
          try
            FClients.Add(client);
          finally
            FLock.Leave;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTCPServerSocket.Listen;
var
  sock_addr: sockaddr_in;
  res: Integer;
begin
  Close;
  if not FWSAOK
    then Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocket = INVALID_SOCKET)
    then Exit;

  res := 1;
  ioctlsocket(FSocket, FIONBIO, res);

  FillChar(sock_addr, SizeOf(sockaddr_in), 0);
  sock_addr.sin_family := AF_INET;
  sock_addr.sin_port := htons(FPort);
  sock_addr.sin_addr.s_addr := INADDR_ANY;

  res := bind(FSocket, sock_addr, SizeOf(sockaddr_in));
  if(res = SOCKET_ERROR) then
  begin
    Close;
    Exit;
  end;

  res := WinSock.listen(FSocket, 1);
  if(res = SOCKET_ERROR) then
  begin
    Close;
    Exit;
  end;

  WSAAsyncSelect(FSocket, FWindow, UM_SOCKET, FD_CLOSE or FD_ACCEPT);
  FIsListening := True;
end;

procedure TTCPServerSocket.Close;
begin
  if FSocket <> -1 then
  begin
    closesocket(FSocket);
    FSocket := -1;
  end;
  FIsListening := False;
end;

procedure TTCPServerSocket.SendBuffer(ABuffer: PByte; ASize: Integer);
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FClients.Count -1
      do TTCPClient(FClients[i]).SendBuffer(ABuffer, ASize);
  finally
    FLock.Leave;
  end;
end;

(*** TTCPClient ***************************************************************)

constructor TTCPClient.Create(ASocket: Integer; AOwner: TTCPServerSocket);
begin
  inherited Create;
  FSocket := ASocket;
  FOwner := AOwner;
end;

destructor TTCPClient.Destroy;
begin
  inherited Destroy;
end;

procedure TTCPClient.SendBuffer(ABuffer: PByte; ASize: Integer);
var
  snd: Integer;
  send_bytes: Integer;
begin
  send_bytes := 1024;
  while ASize > 0 do
  begin
//    Dbg('Sending ' + inttostr(ASize) + ' bytes to ' + inttostr(FSocket));
    snd := send(FSocket, ABuffer^, send_bytes, 0);
    if snd = SOCKET_ERROR then
    begin
      if WSAGetLastError = WSAEWOULDBLOCK then
      begin
//        Dbg('WSAEWOULDBLOCK');
        Continue;
      end;
//      Dbg('Send Error in Socket ' + inttostr(FSocket) + ' - WSAGetLastError = 0x' + inttohex(WSAGetLastError, 8));
      FOwner.FClients.Remove(Self);
      closesocket(FSocket);
//      Free;
      Exit;
    end;
    dec(ASize, snd);
    inc(ABuffer, snd);
  end;
end;

(*** TTCPClientSocket *********************************************************)

const
  PACKET_SIZE = 65536;

constructor TTCPClientSocket.Create;
begin
  inherited Create;
  FHost := 'localhost';
  FPort := 12345;
  FLock := TCriticalSection.Create;
  FIsConnected := False;
  FBuffer := AllocMem(PACKET_SIZE);
  FWSAOK := WSAStartup(MAKEWORD(1,1), FWSAData) = 0;
  if FWSAOK
    then FWindow := Classes.AllocateHWnd(OnMessage);
end;

destructor TTCPClientSocket.Destroy;
begin
  Close;
  if FWindow <> 0
    then DeallocateHWnd(FWindow);
  WSACleanup;
  FLock.Free;
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TTCPClientSocket.Connect;
var
  host: PHostEnt;
  ip: String;
  sock_addr: sockaddr_in;
  res: Integer;
begin
  Close;
  if not FWSAOK
    then Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocket = INVALID_SOCKET)
    then Exit;

  res := 1;
  ioctlsocket(FSocket, FIONBIO, res);

  host := gethostbyname(PChar(FHost));
  if not Assigned(host) then
  begin
    Close;
    Exit;
  end;

  with host^ do ip := Format('%d.%d.%d.%d', [Ord(h_addr^[0]), Ord(h_addr^[1]), Ord(h_addr^[2]), Ord(h_addr^[3])]);

  FillChar(sock_addr, SizeOf(sockaddr_in), 0);
  sock_addr.sin_family := AF_INET;
  sock_addr.sin_port := htons(FPort);
  sock_addr.sin_addr.s_addr := inet_addr(PChar(ip));

  res := WinSock.connect(FSocket, sock_addr, SizeOf(sockaddr_in));
  if(res = SOCKET_ERROR) and (WSAGetLastError <> WSAEWOULDBLOCK) then
  begin
    Close;
    Exit;
  end;

  WSAAsyncSelect(FSocket, FWindow, UM_SOCKET, FD_READ);
  FIsConnected := True;
end;

procedure TTCPClientSocket.Close;
begin
  FLock.Enter;
  try
    if FSocket <> -1 then
    begin
      closesocket(FSocket);
      FSocket := -1;
    end;
    FIsConnected := False;
  finally
    FLock.Leave;
  end;
end;

procedure TTCPClientSocket.OnMessage(var Message: TMessage);
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
        rec := recv(FSocket, FBuffer^, PACKET_SIZE, 0);
//        Dbg('FD_READ -> Socket ' + inttostr(rec));
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
