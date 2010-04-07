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

unit PropDVBC;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBTuningPlugins, DCDVBDataPlugins, ComCtrls, Menus, Spin,
  DCDVBShared, PropDVBT;

const
  CLSID_TunerControlPropertyPageDVBC: TGuid = '{6961619E-67D0-48FD-B722-173DCA2D68F9}';

type
  TfrmPropDVBC = class(TDCDVBFormPropertyPage)
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

implementation

{$R *.DFM}

function TfrmPropDVBC.OnConnect(Unknown: IInterface): HRESULT;
var
  last_tuned: TLastTuner;
begin
  FTuner := GetTunerFromUnknown(Unknown);
  if Unknown.QueryInterface(IID_ITunerControl, FTuning) = S_OK then
  begin
    FTuning.get_LastTuned(last_tuned);
    SpinEdit7.Value := last_tuned.DVBCFrequency;
    SpinEdit1.Value := last_tuned.DVBCSymbolrate;
  end;
  Result := S_OK;
end;

function TfrmPropDVBC.OnDisconnect: HRESULT;
begin
  FTuning := nil;
  FTuner := nil;
  Result := S_OK;
end;

procedure TfrmPropDVBC.Button1Click(Sender: TObject);
var
  tune_request: TDVBCTuneRequest;
  last_tuned: TLastTuner;
begin
  if Assigned(FTuner) then
  begin
    tune_request.Size := SizeOf(TDVBCTuneRequest);
    tune_request.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
    tune_request.Frequency := SpinEdit7.Value;
    tune_request.SymbolRate := SpinEdit1.Value;
    FTuner.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST, Integer(@tune_request), tune_request.DeviceType);
    if Assigned(FTuning) then
    begin
      FTuning.get_LastTuned(last_tuned);
      last_tuned.DVBCFrequency := SpinEdit7.Value;
      last_tuned.DVBCSymbolrate := SpinEdit1.Value;
      FTuning.put_LastTuned(last_tuned);
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropDVBC,
    CLSID_TunerControlPropertyPageDVBC
  );

end.
