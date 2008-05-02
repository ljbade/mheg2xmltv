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

unit PropertyPages;

interface

uses
  Windows, SysUtils, Messages, DCDVBShared, DCDVBTuningPlugins;

const
  CLSID_PropPageSettings: TGuid = '{C6452ABE-2532-4561-91B4-740A3AF6429B}';
  IID_IBDADriver        : TGuid = '{38E1D039-B7CB-4221-82DE-6E8A4B804B43}';

type
  IBDADriver = interface(IUnknown)
  ['{38E1D039-B7CB-4221-82DE-6E8A4B804B43}']
    function get_DirectTuning(out ADirectTuning: BOOL): HRESULT; stdcall;
    function put_DirectTuning(ADirectTuning: BOOL): HRESULT; stdcall;
    function get_TuneDVBS2(out ADVBS2: BOOL): HRESULT; stdcall;
    function put_TuneDVBS2(ADVBS2: BOOL): HRESULT; stdcall;
  end;

  TPropPageSettings = class(TDCDVBPropertyPage)
  private
    FBDADriver: IBDADriver;
    FBDATuning: THandle;
    FTuneDVBS2: THandle;
    FTuneRequests: THandle;
  public
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
    function OnReceiveMessage(hwndDlg: Cardinal; uMsg: Cardinal; wParam: Integer; lParam: Integer): Integer; override;
  end;

implementation

const
  IDC_RADIO_BDAINTERFACE = 1001;
  IDC_RADIO_TUNEREQUESTS = 1002;
  IDC_CHECK_DVBS2        = 1003;

function TPropPageSettings.OnConnect(Unknown: IInterface): HRESULT;
begin
  Unknown.QueryInterface(IID_IBDADriver, FBDADriver);
  Result := S_OK;
end;

function TPropPageSettings.OnDisconnect: HRESULT;
begin
  FBDADriver := nil;
  Result := S_OK;
end;

function TPropPageSettings.OnReceiveMessage(hwndDlg: Cardinal; uMsg: Cardinal; wParam: Integer; lParam: Integer): Integer;
var
  direct_tuning: BOOL;
  tune_dvbs2: BOOL;
begin
  case uMsg of
    WM_INITDIALOG:
    begin
      FBDATuning := GetDlgItem(hwndDlg, IDC_RADIO_BDAINTERFACE);
      FTuneRequests := GetDlgItem(hwndDlg, IDC_RADIO_TUNEREQUESTS);
      FTuneDVBS2 := GetDlgItem(hwndDlg, IDC_CHECK_DVBS2);
      if Assigned(FBDADriver) then
      begin
        FBDADriver.get_DirectTuning(direct_tuning);
        if direct_tuning
          then SendMessage(FBDATuning, BM_SETCHECK, BST_CHECKED, 0)
          else SendMessage(FTuneRequests, BM_SETCHECK, BST_CHECKED, 0);

        FBDADriver.get_TuneDVBS2(tune_dvbs2);
        if tune_dvbs2
          then SendMessage(FTuneDVBS2, BM_SETCHECK, BST_CHECKED, 0)
          else SendMessage(FTuneDVBS2, BM_SETCHECK, BST_UNCHECKED, 0);

      end;
    end;
    WM_COMMAND:
    begin
      if Assigned(FBDADriver) then
      begin
        case LOWORD(wParam) of
          IDC_RADIO_BDAINTERFACE: FBDADriver.put_DirectTuning(True);
          IDC_RADIO_TUNEREQUESTS: FBDADriver.put_DirectTuning(False);
          IDC_CHECK_DVBS2:
          begin
            tune_dvbs2 := SendMessage(FTuneDVBS2, BM_GETCHECK, 0, 0) = BST_CHECKED;
            FBDADriver.put_TuneDVBS2(tune_dvbs2);
          end;
        end;
      end;
    end;
  end;

  Result := inherited OnReceiveMessage(hwndDlg, uMsg, wParam, lParam);
end;

initialization

  TDCDVBClassFactory.Create(TPropPageSettings, CLSID_PropPageSettings, 104, 'Settings');

end.
