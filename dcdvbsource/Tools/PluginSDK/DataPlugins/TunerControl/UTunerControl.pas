(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004, 2005 Milenko Mitrovic                                *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  SDK for DC-DVB Filter Version 0.1.6                                      *
 *                                                                           *
 *  The Source Code is given "as is" without warranty of any kind. The       *
 *  Author is not responsible for any damage due to the use of this Code.    *
 *  The complete Source Code remains property of the Author and must be      *
 *  used only for creating Plugins for the DC-DVB Filter.                    *
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
