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

unit MulticastTransportStream;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils,

  MulticastClientServer, Logger, TransportStream;

type
  TMulticastTransportStream = class;

  TMulticastTransportThread = class(TThread)
  protected
    FOwner: TMulticastTransportStream;
    FClient: TMulticastClient;
    procedure Execute; override;
  public
    constructor Create(AOwner: TMulticastTransportStream);
  end;

  TMulticastTransportStream = class(TTransportStream)
  protected
    FThread: TMulticastTransportThread;
//    FClient: TMulticastClient;
    function GetActive: Boolean; override;
    procedure SetActive(AActive: Boolean); override;
    function GetPort: Word;
    procedure SetPort(APort: Word);
    function GetMulticastGroup: String;
    procedure SetMulticastGroup(AGroup: String);
  public
    constructor Create(ALogger: TLogger = nil); override;
    destructor Destroy; override;
  published
    property MulticastGroup: String read GetMulticastGroup write SetMulticastGroup;
    property Port: Word read GetPort write SetPort;
  end;

implementation

const
  UM_CLOSE_CLIENT = WM_USER + 100;
  UM_CONNECT_CLIENT = WM_USER + 101;

(*** TMulticastTransportStream ************************************************)

constructor TMulticastTransportThread.Create(AOwner: TMulticastTransportStream);
begin
  inherited Create(True);
  FOwner := AOwner;
//  FreeOnTerminate := True;
  Resume;
end;

procedure TMulticastTransportThread.Execute;
var
  msg: TMsg;
begin
  FClient := TMulticastClient.Create(FOwner.FLogger);
  FClient.OnReadData := FOwner.PushData;

  while (GetMessage(msg, 0, 0, 0)) do
  begin
    case msg.message of
      UM_CLOSE_CLIENT: FClient.Active := False;
      UM_CONNECT_CLIENT: FClient.Active := True;
    end;

    if Terminated
      then break;

    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

constructor TMulticastTransportStream.Create(ALogger: TLogger = nil);
begin
  inherited Create(ALogger);
  Log(FLogger, Self, 'Create', 'constructor');
//  FClient := TMulticastClient.Create(ALogger);
//  FClient.OnReadData := PushData;
  FThread := TMulticastTransportThread.Create(Self);
  while (FThread.FClient = nil)
    do sleep(5);
end;

destructor TMulticastTransportStream.Destroy;
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

function TMulticastTransportStream.GetActive: Boolean;
begin
  if not Assigned(FThread) or not Assigned(FThread.FClient)
    then Result := False
    else Result := FThread.FClient.Active;
//  Log(FLogger, Self, 'GetActive', 'Returned: ' + IfThen(Result, 'True', 'False'));
end;

procedure TMulticastTransportStream.SetActive(AActive: Boolean);
var
  i: Integer;
begin
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
        if FThread.FClient.Active
          then break;
        sleep(20);
      end;
    end else
    begin
      PostThreadMessage(FThread.ThreadID, UM_CLOSE_CLIENT, 0, 0);
    end;
//  FThread.FClient.Active := AActive;
end;

function TMulticastTransportStream.GetPort: Word;
begin
  Result := FThread.FClient.Port;
  Log(FLogger, Self, 'GetPort', 'Returned: ' + inttostr(GetPort));
end;

procedure TMulticastTransportStream.SetPort(APort: Word);
var
  was_active: Boolean;
begin
  Log(FLogger, Self, 'SetPort', 'to ' + inttostr(APort));
  was_active := GetActive;
  SetActive(False);
  FThread.FClient.Port := APort;
  SetActive(was_active);
end;

function TMulticastTransportStream.GetMulticastGroup: String;
begin
  Result := FThread.FClient.MulticastGroup;
  Log(FLogger, Self, 'GetMulticastGroup', 'returning ' + Result);
end;

procedure TMulticastTransportStream.SetMulticastGroup(AGroup: String);
var
  was_active: Boolean;
begin
  Log(FLogger, Self, 'SetMulticastGroup', 'to ' + AGroup);
  was_active := GetActive;
  SetActive(False);
  FThread.FClient.MulticastGroup := AGroup;
  SetActive(was_active);
end;

end.
