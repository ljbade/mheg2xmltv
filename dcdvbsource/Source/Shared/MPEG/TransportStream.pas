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

unit TransportStream;

interface

uses
  DirectShow9, SyncObjs, WinSock, Windows, Messages, SysUtils, Classes, DSUtil,
  ActiveX, StrUtils,

  Logger;

type
  TOnTSData = procedure(ABuffer: PByte; ASize: Integer) of Object;

  TTransportStream = class
  protected
    FLogger: TLogger;
    FLock: TCriticalSection;
    FOnTSData: TOnTSData;
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(AActive: Boolean); virtual; abstract;
    procedure SetOnTSData(AData: TOnTSData);
    procedure PushData(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ALogger: TLogger = nil); virtual;
    destructor Destroy; override;
  published
    property OnTSData: TOnTSData read FOnTSData write SetOnTSData;
    property Active: Boolean read GetActive write SetActive;
    property Logger: TLogger read FLogger write FLogger;
  end;

implementation

(*** TTransportStream *********************************************************)

constructor TTransportStream.Create(ALogger: TLogger = nil);
begin
  inherited Create;
  FLogger := ALogger;
  Log(FLogger, Self, 'Create', 'constructor');
  FLock := TCriticalSection.Create;
end;

destructor TTransportStream.Destroy;
begin
  Log(FLogger, Self, 'Destroy', 'destructor');
  SetOnTSData(nil);
  SetActive(False);
  FLock.Free;
  inherited Destroy;
end;

procedure TTransportStream.SetOnTSData(AData: TOnTSData);
begin
  Log(FLogger, Self, 'SetOnTSData', inttohex(Integer(@AData), 8));
  FLock.Enter;
  try
    FOnTSData := AData;
  finally
    FLock.Leave;
  end;
end;

procedure TTransportStream.PushData(ABuffer: PByte; ASize: Integer);
begin
//  Log(FLogger, Self, 'PushData', 'Size: ' + inttostr(ASize));
  try
    FLock.Enter;
    try
      if Assigned(FOnTSData)
        then FOnTSData(ABuffer, ASize);
    finally
      FLock.Leave;
    end;
  except
  end;
end;

end.
