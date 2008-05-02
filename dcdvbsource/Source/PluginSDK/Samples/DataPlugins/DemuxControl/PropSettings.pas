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

unit PropSettings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBDataPlugins, ComCtrls, Menus, Spin, IDVBSource, DCDVBShared;

const
  CLSID_DemuxControlPropertyPage: TGuid = '{B75D0B49-5A32-4C8E-8D41-92BEE8035E97}';
  IID_IDmxControl:                TGuid = '{8C3742C2-C355-4841-A691-74FDBE0FFC89}';

type
  IDmxControl = interface(IUnknown)
  ['{8C3742C2-C355-4841-A691-74FDBE0FFC89}']
    function get_DemuxControl(out AControl: IDemuxControl): HRESULT; stdcall;
  end;

  TfrmPropSettings = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    SpinEdit1: TSpinEdit;
    ComboBox1: TComboBox;
    Button1: TButton;
    SpinEdit2: TSpinEdit;
    ComboBox2: TComboBox;
    Button2: TButton;
    SpinEdit3: TSpinEdit;
    Button3: TButton;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button4: TButton;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FIntf: IDemuxControl;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

function TfrmPropSettings.OnConnect(Unknown: IInterface): HRESULT;
var
  intf: IDmxControl;
  pid: Integer;
  pcrpid: Integer;
  cpid: Integer;
  apid: Integer;
  vtype: TVideoType;
  atype: TAudioType;
begin
  if Unknown.QueryInterface(IID_IDmxControl, intf) = S_OK then
  begin
    intf.get_DemuxControl(FIntf);
    if Assigned(FIntf) then
    begin
      FIntf.get_VideoPID(pid, vtype);
      SpinEdit1.Value := pid;
      ComboBox1.ItemIndex := integer(vtype);

      FIntf.get_AudioPID(pid, atype);
      SpinEdit2.Value := pid;
      ComboBox2.ItemIndex := integer(atype);

      FIntf.get_TeletextPID(pid);
      SpinEdit3.Value := pid;

      FIntf.get_SubtitlePID(pid, pcrpid, cpid, apid);
      SpinEdit4.Value := pid;
      SpinEdit5.Value := pcrpid;
      SpinEdit6.Value := cpid;
      SpinEdit7.Value := apid;
    end;
  end;

  Result := S_OK;
end;

function TfrmPropSettings.OnDisconnect: HRESULT;
begin
  FIntf := nil;
  Result := S_OK;
end;

procedure TfrmPropSettings.Button1Click(Sender: TObject);
begin
  if Assigned(FIntf) then
  begin
    FIntf.put_VideoPID(SpinEdit1.Value, TVideoType(ComboBox1.ItemIndex))
  end;
end;

procedure TfrmPropSettings.Button2Click(Sender: TObject);
begin
  if Assigned(FIntf) then
  begin
    FIntf.put_AudioPID(SpinEdit2.Value, TAudioType(ComboBox2.ItemIndex))
  end;
end;

procedure TfrmPropSettings.Button3Click(Sender: TObject);
begin
  if Assigned(FIntf) then
  begin
    FIntf.put_TeletextPID(SpinEdit3.Value)
  end;
end;

procedure TfrmPropSettings.Button4Click(Sender: TObject);
begin
  if Assigned(FIntf) then
  begin
    FIntf.put_SubtitlePID(
      SpinEdit4.Value,
      SpinEdit5.Value,
      SpinEdit6.Value,
      SpinEdit7.Value
    );
  end;

end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropSettings,
    CLSID_DemuxControlPropertyPage
  );

end.
