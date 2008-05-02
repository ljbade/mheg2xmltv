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

unit UDemuxControl;

interface

uses
  ActiveX, Windows, Classes, SysUtils, DCDVBDataPlugins, SyncObjs, DirectShow9,
  PropSettings, PropAbout, IDVBSource, DCDVBShared;

const
  CLSID_DemuxControl: TGuid = '{5B9DF5EA-592A-4972-8306-E7E2CCBDA2DA}';

type
  TDemuxControl = class(TDCDVBDataPlugin, IDmxControl)
  protected
    procedure Initialize; override;
  public
    // IDmxControl
    function get_DemuxControl(out AControl: IDemuxControl): HRESULT; stdcall;
  end;

implementation

procedure TDemuxControl.Initialize;
begin
  AddPropertyPage(CLSID_DemuxControlPropertyPage);
  AddPropertyPage(CLSID_DemuxControlPropertyPageAbout);
end;

function TDemuxControl.get_DemuxControl(out AControl: IDemuxControl): HRESULT;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
begin
  if not Assigned(@AControl) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not Assigned(FFilterGraph) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if FFilterGraph.EnumFilters(enum) = S_OK then
  begin
    while enum.Next(1, filter, nil) = S_OK do
    begin
      if filter.QueryInterface(IID_IDemuxControl, AControl) = S_OK then
      begin
        Result := S_OK;
        Exit;
      end;
    end;
  end;

  Result := E_FAIL;
end;


initialization

  TDCDVBClassFactory.Create
  (
    TDemuxControl,
    CLSID_DemuxControl,
    'Demux Control',
    'Demux Control',
    'Milenko Mitrovic',
    '0.0.0.1'
  );

end.
