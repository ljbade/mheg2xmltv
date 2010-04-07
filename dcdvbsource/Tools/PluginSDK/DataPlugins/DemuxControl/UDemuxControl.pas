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
