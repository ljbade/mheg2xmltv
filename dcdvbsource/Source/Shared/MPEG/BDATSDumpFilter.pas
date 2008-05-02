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

unit BDATSDumpFilter;

interface

uses
  Windows, DirectShow9, Classes, ActiveX, BaseClass, SysUtils, DSUtil,

  Logger;

const
  CLSID_BDATSDump: TGuid = '{4C1CC017-E66B-41F8-9667-B19AADE1C98C}';

type
  TOnTSDump = procedure(ABuffer: PByte; ASize: Integer) of Object;

  TBDATSDumpFilter = class;

  IBDATSDump = interface(IUnknown)
  ['{D0645887-124D-4FD3-80C5-F8E3501A51E6}']
    procedure SetCallback(ACallback: TOnTSDump); stdcall;
  end;

  TBDATSDumpInputPin = Class (TBCRenderedInputPin)
  private
    FLogger: TLogger;
  public
    constructor Create(ObjectName: string; pUnk: IUnKnown; Filter: TBDATSDumpFilter; Lock: TBCCritSec; out hr: HRESULT; ALogger: TLogger);
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function Receive(pSample: IMediaSample): HRESULT; override;
  end;

  TBDATSDumpFilter = class (TBCBaseFilter, IBDATSDump, ITuneRequestInfo)
  private
    FLogger: TLogger;
    FCallbackLock: TBCCritSec;
    FInputPin: TBDATSDumpInputPin;
    FCallback: TOnTSDump;
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create(ALogger: TLogger = nil);
    destructor Destroy; override;

    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: integer; override;
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;

    // IBDATSDump
    procedure SetCallback(ACallback: TOnTSDump); stdcall;
    // ITuneRequestInfo
    function GetLocatorData(Request: ITuneRequest): HResult; stdcall;
    function GetComponentData(CurrentRequest: ITuneRequest): HResult; stdcall;
    function CreateComponentList(CurrentRequest: ITuneRequest): HResult; stdcall;
    function GetNextProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult; stdcall;
    function GetPreviousProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult; stdcall;
    function GetNextLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult; stdcall;
    function GetPreviousLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult; stdcall;
  end;

implementation

(*** TBDATSDumpInputPin *******************************************************)

constructor TBDATSDumpInputPin.Create(ObjectName: string;pUnk: IUnKnown; Filter: TBDATSDumpFilter; Lock: TBCCritSec; out hr: HRESULT; ALogger: TLogger);
begin
  inherited Create(ObjectName, Filter, Lock, hr, 'BDA-TS Dump Input Pin');
  FLogger := ALogger;
  Log(FLogger, Self, 'Create', 'constructor');
end;

function TBDATSDumpInputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  Log(FLogger, Self, 'CheckMediaType', 'init');
  if not Assigned(mt) then
  begin
    Log(FLogger, Self, 'CheckMediaType', 'MediaType = NULL');
    Result := E_POINTER;
    Exit;
  end;

  if not IsEqualGUID(mt.majortype, MEDIATYPE_Stream) then
  begin
    Log(FLogger, Self, 'CheckMediaType', 'MajorType (' + GUIDToString(mt.majortype) + ') is not MEDIATYPE_Stream');
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;

  if not IsEqualGUID(mt.subtype, KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT) and
     not IsEqualGUID(mt.subtype, KSDATAFORMAT_TYPE_MPEG2_TRANSPORT) then
  begin
    Log(FLogger, Self, 'CheckMediaType', 'MajorType (' + GUIDToString(mt.subtype) + ') is not KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT or KSDATAFORMAT_TYPE_MPEG2_TRANSPORT');
    Result := VFW_E_INVALIDSUBTYPE;
    Exit;
  end;

  Log(FLogger, Self, 'CheckMediaType', 'MediaType valid for connection');
  Result := S_OK;
end;

function TBDATSDumpInputPin.Receive(pSample: IMediaSample): HRESULT;
var
  buffer: PByte;
  size: Integer;
begin
  if Assigned(pSample) then
  begin
    size := pSample.GetActualDataLength;
    pSample.GetPointer(buffer);

//    Log(FLogger, Self, 'Receive', 'Received ' + inttostr(size) + ' bytes');

    if Assigned(buffer) and (size > 0)
      then TBDATSDumpFilter(FFilter).OnBuffer(buffer, size);
  end;

  Result := S_OK;
end;

(*** TBDATSDumpFilter *********************************************************)

constructor TBDATSDumpFilter.Create(ALogger: TLogger = nil);
var
  hr: HRESULT;
begin
  inherited Create('BDA-TS Dump Filter', nil, TBCCritSec.Create, CLSID_BDATSDump);
  FLogger := ALogger;
  Log(FLogger, Self, 'Create', 'constructor');
  FInputPin := TBDATSDumpInputPin.Create('BDA-TS Dump Input Pin', GetOwner, Self, FLock, hr, FLogger);
  FCallbackLock := TBCCritSec.Create;
end;

destructor TBDATSDumpFilter.Destroy;
begin
  Log(FLogger, Self, 'Destroy', 'destructor');
  FreeAndNil(FInputPin);
  FreeAndNil(FCallbackLock);
  inherited Destroy;
end;

function TBDATSDumpFilter.GetPinCount: integer;
begin
  Result := 1;
  Log(FLogger, Self, 'GetPinCount', 'Returning ' + inttostr(Result));
end;

function TBDATSDumpFilter.GetPin(n: Integer): TBCBasePin;
begin
  Log(FLogger, Self, 'GetPin', 'Request for Index ' + inttostr(n));
  case n of
    0: Result := FInputPin;
    else Result := nil;
  end;
  Log(FLogger, Self, 'GetPin', 'Returning ' + inttohex(integer(Result), 8));
end;

procedure TBDATSDumpFilter.OnBuffer(ABuffer: PByte; ASize: Integer);
begin
  FCallbackLock.Lock;
  try
    if Assigned(FCallback)
      then FCallback(ABuffer, ASize);
  finally
    FCallbackLock.UnLock;
  end;
end;

function TBDATSDumpFilter.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IID_ITuneRequestInfo) then
  begin
    if GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited NonDelegatingQueryInterface(IID, Obj);
  end;
end;

procedure TBDATSDumpFilter.SetCallback(ACallback: TOnTSDump);
begin
  Log(FLogger, Self, 'SetCallback', 'Setting up Callback to ' + inttohex(integer(@ACallback), 8));
  FCallbackLock.Lock;
  try
    FCallback := ACallback;
  finally
    FCallbackLock.UnLock;
  end;
end;

function TBDATSDumpFilter.GetLocatorData(Request: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TBDATSDumpFilter.GetComponentData(CurrentRequest: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TBDATSDumpFilter.CreateComponentList(CurrentRequest: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TBDATSDumpFilter.GetNextProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBDATSDumpFilter.GetPreviousProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBDATSDumpFilter.GetNextLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBDATSDumpFilter.GetPreviousLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

end.
