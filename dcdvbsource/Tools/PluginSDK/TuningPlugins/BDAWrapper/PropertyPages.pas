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
  end;

  TPropPageSettings = class(TDCDVBPropertyPage)
  private
    FBDADriver: IBDADriver;
    FBDATuning: THandle;
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
begin
  case uMsg of
    WM_INITDIALOG:
    begin
      FBDATuning := GetDlgItem(hwndDlg, IDC_RADIO_BDAINTERFACE);
      FTuneRequests := GetDlgItem(hwndDlg, IDC_RADIO_TUNEREQUESTS);
      if Assigned(FBDADriver) then
      begin
        FBDADriver.get_DirectTuning(direct_tuning);
        if direct_tuning
          then SendMessage(FBDATuning, BM_SETCHECK, BST_CHECKED, 0)
          else SendMessage(FTuneRequests, BM_SETCHECK, BST_CHECKED, 0);
      end;
    end;
    WM_COMMAND:
    begin
      if Assigned(FBDADriver) then
      begin
        case LOWORD(wParam) of
          IDC_RADIO_BDAINTERFACE: FBDADriver.put_DirectTuning(True);
          IDC_RADIO_TUNEREQUESTS: FBDADriver.put_DirectTuning(False);
        end;
      end;
    end;
  end;

  Result := inherited OnReceiveMessage(hwndDlg, uMsg, wParam, lParam);
end;

initialization

  TDCDVBClassFactory.Create(TPropPageSettings, CLSID_PropPageSettings, 104, 'Settings');

end.
