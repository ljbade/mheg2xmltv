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

unit UEPG2XML;

interface

uses
  ActiveX, Windows, Classes, SysUtils, DCDVBDataPlugins, SyncObjs, DirectShow9,
  PropSettings, DCDVBShared;

const
  CLSID_EPG2XML: TGuid = '{53F2B9C3-E782-4A0D-B4E8-DCF3D56545F8}';

type
  TEPG2XML = class(TDCDVBDataPlugin, IEPG2XML)
  protected
    procedure Initialize; override;
  public
    // IEPG2XML
    function get_Graph(out AGraph: IFilterGraph): HRESULT; stdcall;
  end;

implementation

procedure TEPG2XML.Initialize;
begin
  AddPropertyPage(CLSID_EPG2XMLPropertyPage);
end;

function TEPG2XML.get_Graph(out AGraph: IFilterGraph): HRESULT;
begin
  if not Assigned(@AGraph) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not Assigned(FFilterGraph) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  AGraph := FFilterGraph;
  Result := S_OK;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TEPG2XML,
    CLSID_EPG2XML,
    'EPG2XML',
    'EPG2XML',
    'Milenko Mitrovic',
    '0.0.0.1'
  );

end.
