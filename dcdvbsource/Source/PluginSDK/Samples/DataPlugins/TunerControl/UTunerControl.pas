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

unit UTunerControl;

interface

uses
  ActiveX, Windows, Classes, SysUtils, DCDVBTuningPlugins, DCDVBDataPlugins, SyncObjs, DirectShow9,
  PropDVBT, PropDVBC, PropDVBS, PropATSC, PropAbout, DCDVBShared;

const
  CLSID_TunerControl: TGuid = '{FBD0C36E-E66C-4734-B14A-11558BFB103C}';

type
  TTunerControl = class(TDCDVBDataPlugin, ITunerControl)
  protected
    FLastTuned: TLastTuner;
    FTuner: IDCDVBTuningPluginDevice;
    procedure Initialize; override;
  public
    // ITunerControl
    function get_TunerInterface(out ATunerInterface: IDCDVBTuningPluginDevice): HRESULT; stdcall;
    function get_LastTuned(out ALastTuned: TLastTuner): HRESULT; stdcall;
    function put_LastTuned(ALastTuned: TLastTuner): HRESULT; stdcall;
  end;

implementation

procedure TTunerControl.Initialize;
begin
  AddPropertyPage(CLSID_TunerControlPropertyPageDVBT);
  AddPropertyPage(CLSID_TunerControlPropertyPageDVBC);
  AddPropertyPage(CLSID_TunerControlPropertyPageDVBS);
  AddPropertyPage(CLSID_TunerControlPropertyPageATSC);
  AddPropertyPage(CLSID_TunerControlPropertyPageAbout);
end;

function TTunerControl.get_TunerInterface(out ATunerInterface: IDCDVBTuningPluginDevice): HRESULT;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
begin
  if not Assigned(@ATunerInterface) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not Assigned(FFilterGraph) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if Assigned(FTuner) then
  begin
    ATunerInterface := FTuner;
    Result := S_OK;
    Exit;
  end;

  if FFilterGraph.EnumFilters(enum) = S_OK then
  begin
    while enum.Next(1, filter, nil) = S_OK do
    begin
      if filter.QueryInterface(IID_IDCDVBTuningPluginDevice, FTuner) = S_OK then
      begin
        ATunerInterface := FTuner;
        Result := S_OK;
        Exit;
      end;
    end;
  end;

  Result := E_FAIL;
end;

function TTunerControl.get_LastTuned(out ALastTuned: TLastTuner): HRESULT;
begin
  ALastTuned := FLastTuned;
  Result := S_OK;
end;

function TTunerControl.put_LastTuned(ALastTuned: TLastTuner): HRESULT;
begin
  FLastTuned := ALastTuned;
  Result := S_OK;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TTunerControl,
    CLSID_TunerControl,
    'Tuner Control',
    'Tuner Control',
    'Milenko Mitrovic',
    '0.0.0.1'
  );

end.
