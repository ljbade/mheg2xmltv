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

unit DCDVBTSDumpFilter;

interface

uses
  Windows, DirectShow9, BaseClass, ActiveX;

const
  CLSID_DCDVBTSDump: TGuid = '{4C1CC017-E66B-41F8-9667-B19AADE1C98C}';
  IID_IDCDVBTSDump: TGuid  = '{D0645887-124D-4FD3-80C5-F8E3501A51E6}';

type
  TOnTSDump = procedure(ABuffer: PByte; ASize: Integer) of Object;

  TDCDVBTSDumpFilter = class;

  IDCDVBTSDump = interface(IUnknown)
  ['{D0645887-124D-4FD3-80C5-F8E3501A51E6}']
    procedure SetCallback(ACallback: TOnTSDump); stdcall;
  end;

  TDCDVBTSDumpInputPin = Class (TBCRenderedInputPin)
  public
    constructor Create(ObjectName: string; pUnk: IUnKnown; Filter: TDCDVBTSDumpFilter; Lock: TBCCritSec; out hr: HRESULT);
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function Receive(pSample: IMediaSample): HRESULT; override;
  end;

  TDCDVBTSDumpFilter = class (TBCBaseFilter, IDCDVBTSDump, ITuneRequestInfo)
  private
    FCallbackLock: TBCCritSec;
    FInputPin: TDCDVBTSDumpInputPin;
    FCallback: TOnTSDump;
    procedure OnBuffer(ABuffer: PByte; ASize: Integer);
  public
    constructor Create;
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

(*** TDCDVBTSDumpInputPin *****************************************************)

constructor TDCDVBTSDumpInputPin.Create(ObjectName: string;pUnk: IUnKnown; Filter: TDCDVBTSDumpFilter; Lock: TBCCritSec; out hr: HRESULT);
begin
  inherited Create(ObjectName, Filter, Lock, hr, 'DC-DVB TS Dump Input Pin');
end;

function TDCDVBTSDumpInputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  if not Assigned(mt) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not IsEqualGUID(mt.majortype, MEDIATYPE_Stream) then
  begin
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;

  if not IsEqualGUID(mt.subtype, KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT) and
     not IsEqualGUID(mt.subtype, MEDIASUBTYPE_MPEG2_TRANSPORT) and
     not IsEqualGUID(mt.subtype, KSDATAFORMAT_TYPE_MPEG2_TRANSPORT) then
  begin
    Result := VFW_E_INVALIDSUBTYPE;
    Exit;
  end;

  Result := S_OK;
end;

function TDCDVBTSDumpInputPin.Receive(pSample: IMediaSample): HRESULT;
var
  buffer: PByte;
  size: Integer;
begin
  if Assigned(pSample) then
  begin
    size := pSample.GetActualDataLength;
    pSample.GetPointer(buffer);

    if Assigned(buffer) and (size > 0)
      then TDCDVBTSDumpFilter(FFilter).OnBuffer(buffer, size);
  end;

  Result := S_OK;
end;

(*** TDCDVBTSDumpFilter *******************************************************)

constructor TDCDVBTSDumpFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create('DC-DVB TS Dump Filter', nil, TBCCritSec.Create, CLSID_DCDVBTSDump);
  FInputPin := TDCDVBTSDumpInputPin.Create('DC-DVB TS Dump Input Pin', GetOwner, Self, FLock, hr);
  FCallbackLock := TBCCritSec.Create;
end;

destructor TDCDVBTSDumpFilter.Destroy;
begin
  FInputPin.Free;
  FCallbackLock.Free;
  inherited Destroy;
end;

function TDCDVBTSDumpFilter.GetPinCount: integer;
begin
  Result := 1;
end;

function TDCDVBTSDumpFilter.GetPin(n: Integer): TBCBasePin;
begin
  case n of
    0: Result := FInputPin;
    else Result := nil;
  end;
end;

procedure TDCDVBTSDumpFilter.OnBuffer(ABuffer: PByte; ASize: Integer);
begin
  FCallbackLock.Lock;
  try
    if Assigned(FCallback)
      then FCallback(ABuffer, ASize);
  finally
    FCallbackLock.UnLock;
  end;
end;

function TDCDVBTSDumpFilter.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IID_ITuneRequestInfo) or
     IsEqualGUID(IID, IID_IDCDVBTSDump) then
  begin
    if GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited NonDelegatingQueryInterface(IID, Obj);
  end;
end;

procedure TDCDVBTSDumpFilter.SetCallback(ACallback: TOnTSDump);
begin
  FCallbackLock.Lock;
  try
    FCallback := ACallback;
  finally
    FCallbackLock.UnLock;
  end;
end;

function TDCDVBTSDumpFilter.GetLocatorData(Request: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TDCDVBTSDumpFilter.GetComponentData(CurrentRequest: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TDCDVBTSDumpFilter.CreateComponentList(CurrentRequest: ITuneRequest): HResult;
begin
  Result := S_OK;
end;

function TDCDVBTSDumpFilter.GetNextProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBTSDumpFilter.GetPreviousProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBTSDumpFilter.GetNextLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBTSDumpFilter.GetPreviousLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HResult;
begin
  Result := E_NOTIMPL;
end;

end.
