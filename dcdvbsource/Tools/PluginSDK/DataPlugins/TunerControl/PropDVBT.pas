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

unit PropDVBT;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBTuningPlugins, DCDVBDataPlugins, ComCtrls, Menus, Spin, DCDVBShared;

const
  CLSID_TunerControlPropertyPageDVBT: TGuid = '{31D3F0E3-C4F4-4B65-B90E-82424D511E85}';
  IID_ITunerControl: TGuid = '{50D3ADCF-FB42-47F8-A0B7-023B7CD39DB8}';

type
  TLastTuner = packed record
    DVBTFrequency: Integer;
    DVBTBandwidth: Integer;
    DVBCFrequency: Integer;
    DVBCSymbolrate: Integer;
    DVBSFrequency: Integer;
    DVBSSymbolrate: Integer;
    DVBSPolarization: Integer;
    ATSCFrequency: Integer;
  end;

  ITunerControl = interface(IUnknown)
  ['{50D3ADCF-FB42-47F8-A0B7-023B7CD39DB8}']
    function get_TunerInterface(out ATunerInterface: IDCDVBTuningPluginDevice): HRESULT; stdcall;
    function get_LastTuned(out ALastTuned: TLastTuner): HRESULT; stdcall;
    function put_LastTuned(ALastTuned: TLastTuner): HRESULT; stdcall;
  end;

  TfrmPropDVBT = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    Label14: TLabel;
    Label15: TLabel;
    SpinEdit7: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FTuner: IDCDVBTuningPluginDevice;
    FTuning: ITunerControl;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

  function GetTunerFromUnknown(Unknown: IUnknown): IDCDVBTuningPluginDevice;

implementation

{$R *.DFM}

function GetTunerFromUnknown(Unknown: IUnknown): IDCDVBTuningPluginDevice;
var
  tuner: ITunerControl;
begin
  if Unknown.QueryInterface(IID_ITunerControl, tuner) = S_OK then
  begin
    if tuner.get_TunerInterface(Result) = S_OK then
    begin
      Exit;
    end;
  end;

  Result := nil;
end;

function TfrmPropDVBT.OnConnect(Unknown: IInterface): HRESULT;
var
  last_tuned: TLastTuner;
begin
  FTuner := GetTunerFromUnknown(Unknown);
  if Unknown.QueryInterface(IID_ITunerControl, FTuning) = S_OK then
  begin
    FTuning.get_LastTuned(last_tuned);
    SpinEdit7.Value := last_tuned.DVBTFrequency;
    SpinEdit1.Value := last_tuned.DVBTBandwidth;
  end;
  Result := S_OK;
end;

function TfrmPropDVBT.OnDisconnect: HRESULT;
begin
  FTuning := nil;
  FTuner := nil;
  Result := S_OK;
end;

procedure TfrmPropDVBT.Button1Click(Sender: TObject);
var
  tune_request: TDVBTTuneRequest;
  last_tuned: TLastTuner;
begin
  if Assigned(FTuner) then
  begin
    tune_request.Size := SizeOf(TDVBTTuneRequest);
    tune_request.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
    tune_request.Frequency := SpinEdit7.Value;
    tune_request.Bandwidth := SpinEdit1.Value;
    FTuner.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST, Integer(@tune_request), tune_request.DeviceType);
    if Assigned(FTuning) then
    begin
      FTuning.get_LastTuned(last_tuned);
      last_tuned.DVBTFrequency := SpinEdit7.Value;
      last_tuned.DVBTBandwidth := SpinEdit1.Value;
      FTuning.put_LastTuned(last_tuned);
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropDVBT,
    CLSID_TunerControlPropertyPageDVBT
  );

end.
