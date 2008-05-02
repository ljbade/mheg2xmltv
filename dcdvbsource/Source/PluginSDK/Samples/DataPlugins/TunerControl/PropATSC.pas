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

unit PropATSC;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBTuningPlugins, DCDVBDataPlugins, ComCtrls, Menus, Spin, DCDVBShared, PropDVBT;

const
  CLSID_TunerControlPropertyPageATSC: TGuid = '{31969042-FCA7-4F6C-B3D2-5153DBCF63DF}';

type
  TfrmPropATSC = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    Label14: TLabel;
    Label15: TLabel;
    SpinEdit7: TSpinEdit;
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

function TfrmPropATSC.OnConnect(Unknown: IInterface): HRESULT;
var
  last_tuned: TLastTuner;
begin
  FTuner := GetTunerFromUnknown(Unknown);
  if Unknown.QueryInterface(IID_ITunerControl, FTuning) = S_OK then
  begin
    FTuning.get_LastTuned(last_tuned);
    SpinEdit7.Value := last_tuned.ATSCFrequency;
  end;
  Result := S_OK;
end;

function TfrmPropATSC.OnDisconnect: HRESULT;
begin
  FTuning := nil;
  FTuner := nil;
  Result := S_OK;
end;

procedure TfrmPropATSC.Button1Click(Sender: TObject);
var
  tune_request: TATSCTuneRequest;
  last_tuned: TLastTuner;
begin
  if Assigned(FTuner) then
  begin
    tune_request.Size := SizeOf(TATSCTuneRequest);
    tune_request.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC;
    tune_request.Frequency := SpinEdit7.Value;
    FTuner.put_ControlMessage(DCDVB_TUNING_PLUGIN_CTRL_TUNEREQUEST, Integer(@tune_request), tune_request.DeviceType);
    if Assigned(FTuning) then
    begin
      FTuning.get_LastTuned(last_tuned);
      last_tuned.ATSCFrequency := SpinEdit7.Value;
      FTuning.put_LastTuned(last_tuned);
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropATSC,
    CLSID_TunerControlPropertyPageATSC
  );

end.
