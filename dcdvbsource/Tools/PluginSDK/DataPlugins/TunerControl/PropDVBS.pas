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

unit PropDVBS;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBTuningPlugins, DCDVBDataPlugins, ComCtrls, Menus, Spin, DCDVBShared, PropDVBT;

const
  CLSID_TunerControlPropertyPageDVBS: TGuid = '{A23DF423-1908-4A7C-A257-2DE9A76A2631}';

type
  TfrmPropDVBS = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    Label14: TLabel;
    Label15: TLabel;
    SpinEdit7: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    Label3: TLabel;
    ComboBox2: TComboBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FTuner: IDCDVBTuningPluginDevice;
    FTuning: ITunerControl;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

function TfrmPropDVBS.OnConnect(Unknown: IInterface): HRESULT;
var
  last_tuned: TLastTuner;
begin
  FTuner := GetTunerFromUnknown(Unknown);
  if Unknown.QueryInterface(IID_ITunerControl, FTuning) = S_OK then
  begin
    FTuning.get_LastTuned(last_tuned);
    SpinEdit7.Value := last_tuned.DVBSFrequency;
    SpinEdit1.Value := last_tuned.DVBSSymbolrate;
    ComboBox2.ItemIndex := last_tuned.DVBSPolarization;
  end;
  Result := S_OK;
end;

function TfrmPropDVBS.OnDisconnect: HRESULT;
begin
  FTuning := nil;
  FTuner := nil;
  Result := S_OK;
end;

procedure TfrmPropDVBS.Button1Click(Sender: TObject);
var
  tune_request: TDVBSTuneRequest;
  last_tuned: TLastTuner;
begin
  if Assigned(FTuner) then
  begin
    tune_request.Size := SizeOf(TDVBSTuneRequest);
    tune_request.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
    tune_request.Frequency := SpinEdit7.Value;
    tune_request.SymbolRate := SpinEdit1.Value;
    tune_request.Polarization := _IF(ComboBox2.ItemIndex = 0, DVBS_POLARIZATION_HORIZONTAL, DVBS_POLARIZATION_VERTICAL);
    FTuner.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST, Integer(@tune_request), tune_request.DeviceType);
    if Assigned(FTuning) then
    begin
      FTuning.get_LastTuned(last_tuned);
      last_tuned.DVBSFrequency := SpinEdit7.Value;
      last_tuned.DVBSSymbolrate := SpinEdit1.Value;
      last_tuned.DVBSPolarization := ComboBox2.ItemIndex;
      FTuning.put_LastTuned(last_tuned);
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropDVBS,
    CLSID_TunerControlPropertyPageDVBS
  );

end.
