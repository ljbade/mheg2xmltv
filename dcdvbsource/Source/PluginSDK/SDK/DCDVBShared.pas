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

unit DCDVBShared;

interface

uses
  Windows, ActiveX, Classes, Messages {$IFDEF FORMS}, Forms {$ENDIF};

type
  PPByte = ^PByte;

  TProgram = packed record
    ProgramNumber: Integer;
    PCRPID: Integer;
    PMTPID: Integer;
    NumPIDs: Integer;
    PIDs: array[0..63] of Integer;
  end;
  PProgram = ^TProgram;

(*** Base Interfaced Object ***************************************************)

type
  TDCDVBInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

(*** Base Property Pages ******************************************************)

type
{$IFDEF FORMS}
  TDCDVBFormPropertyPage = class;
  TDCDVBFormPropertyPageClass = class of TDCDVBFormPropertyPage;
{$ENDIF}
  TDCDVBPropertyPage = class;
  TDCDVBPropertyPageClass = class of TDCDVBPropertyPage;

  TDCDVBPropertyPage = class(TDCDVBInterfacedObject, IPropertyPage)
  private
    FObjectSet: Boolean;
  protected
    FPageSite: IPropertyPageSite;
  {$IFDEF FORMS}
    FForm: TDCDVBFormPropertyPage;
  {$ELSE}
    FWindow: THandle;
    FDialog: THandle;
    FDialogID: Integer;
  {$ENDIF}
    FDirty: Boolean;
    FTitle: WideString;
  public
  {$IFDEF FORMS}
    constructor Create(AFormClass: TDCDVBFormPropertyPageClass); virtual;
  {$ELSE}
    constructor Create(ADialogID: Integer; ATitle: WideString); virtual;
  {$ENDIF}
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _Release: Integer; override; stdcall;
    procedure SetPageDirty;
    // Override these
    function OnConnect(pUnknown: IUnknown): HRESULT; virtual;
    function OnDisconnect: HRESULT; virtual;
    function OnActivate: HRESULT; virtual;
    function OnDeactivate: HRESULT; virtual;
    function OnApplyChanges: HRESULT; virtual;
  {$IFNDEF FORMS}
    function OnReceiveMessage(hwndDlg: Thandle; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): Integer; virtual;
  {$ENDIF}
    // IPropertyPage
    function SetPageSite(const pageSite: IPropertyPageSite): HResult; stdcall;
    function Activate(hwndParent: HWnd; const rc: TRect; bModal: BOOL): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function GetPageInfo(out pageInfo: TPropPageInfo): HResult; stdcall;
    function SetObjects(cObjects: Longint; pUnkList: PUnknownList): HResult; stdcall;
    function Show(nCmdShow: Integer): HResult; stdcall;
    function Move(const rect: TRect): HResult; stdcall;
    function IsPageDirty: HResult; stdcall;
    function Apply: HResult; stdcall;
    function Help(pszHelpDir: POleStr): HResult; stdcall;
    function TranslateAccelerator(msg: PMsg): HResult; stdcall;
  end;

{$IFDEF FORMS}
  TDCDVBFormPropertyPage = class(TForm)
  private
    FPropertyPage: TDCDVBPropertyPage;
  protected
    procedure WndProc(var Message: TMessage); override;
  published
    function OnConnect(Unknown: IUnknown): HRESULT; virtual;
    function OnDisconnect: HRESULT; virtual;
    function OnApplyChanges: HRESULT; virtual;

    property PropertyPage: TDCDVBPropertyPage read FPropertyPage write FPropertyPage;
  end;
{$ENDIF}

type
  TDCDVBBasePlugin = class(TDCDVBInterfacedObject, ISpecifyPropertyPages)
  protected
    FPages: TList;
    procedure Initialize; virtual; abstract;
    procedure AddPropertyPage(const AGuid: TGuid);
  public
    constructor Create;
    destructor Destroy; override;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    // ISpecifyPropertyPages
    function GetPages(out pages: TCAGUID): HResult; stdcall;
  end;

(*** Utilities ****************************************************************)

  function _IF(AValue: Boolean; IFTrue: Integer; IFFalse: Integer): Integer;

implementation

(*** Utilities ****************************************************************)

function _IF(AValue: Boolean; IFTrue: Integer; IFFalse: Integer): Integer;
begin
  if AValue
    then Result := IFTrue
    else Result := IFFalse;
end;

(*** TDCDVBInterfacedObject ***************************************************)

procedure TDCDVBInterfacedObject.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

class function TDCDVBInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TDCDVBInterfacedObject(Result).FRefCount := 1;
end;

function TDCDVBInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TDCDVBInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TDCDVBInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

(*** TDCDVBPropertyPage *******************************************************)

{$IFNDEF FORMS}

function GetDialogSize(iResourceID: Integer; pDlgProc: Pointer; lParam: LPARAM; out Size: TSize): Boolean;
var
  rc: TRect;
  wnd: THandle;
begin
  wnd := CreateDialogParam(HInstance, MAKEINTRESOURCE(iResourceID), GetDesktopWindow(), pDlgProc, lParam);

  if (wnd = 0) then
  begin
    Result := False;
    Exit;
  end;

  GetWindowRect(wnd, rc);
  Size.cx := rc.right - rc.left;
  Size.cy := rc.bottom - rc.top;

  DestroyWindow(wnd);
  Result := True;
end;

function DialogProc(hwndDlg: Thandle; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): Integer; stdcall;
var
  PropertyPage: TDCDVBPropertyPage;
begin
  case uMsg of
    WM_INITDIALOG:
    begin
      SetWindowLong(hwndDlg, GWL_USERDATA, lParam);
      PropertyPage := TDCDVBPropertyPage(lParam);
      if (PropertyPage = nil) then
      begin
        Result := LRESULT(1);
        Exit;
      end;
      PropertyPage.FDialog := hwndDlg;
    end;
  end;

  PropertyPage := TDCDVBPropertyPage(GetWindowLong(hwndDlg, GWL_USERDATA));
  if (PropertyPage = nil) then
  begin
    Result := LRESULT(1);
    Exit;
  end;

  Result := PropertyPage.OnReceiveMessage(hwndDlg, uMsg, wParam, lParam);
end;

constructor TDCDVBPropertyPage.Create(ADialogID: Integer; ATitle: WideString);
begin
  inherited Create;
  FDialogID := ADialogID;
  FTitle := ATitle;
  FWindow := 0;
  FDialog := 0;
  FPageSite := nil;
  FObjectSet := False;
  FDirty := False;
end;

function TDCDVBPropertyPage.OnReceiveMessage(hwndDlg: Thandle; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): Integer;
var
  PropertyPage: TDCDVBPropertyPage;
  lpss: PStyleStruct;
begin
  PropertyPage := TDCDVBPropertyPage(GetWindowLong(hwndDlg, GWL_USERDATA));
  if (PropertyPage.FWindow = 0) then
  begin
    Result := 0;
    Exit;
  end;

  case uMsg of
    WM_STYLECHANGING:
    begin
      if (wParam = GWL_EXSTYLE) then
      begin
        lpss := PStyleStruct(lParam);
        lpss.styleNew := lpss.styleNew or WS_EX_CONTROLPARENT;
        Result := 0;
        Exit;
      end;
    end;
  end;

  Result := DefWindowProc(hwndDlg, uMsg, wParam, lParam);
end;

{$ELSE}

constructor TDCDVBPropertyPage.Create(AFormClass: TDCDVBFormPropertyPageClass);
var
  w, h: Integer;
begin
  inherited Create;

  FPageSite := nil;

  FForm := AFormClass.Create(nil);
  w := FForm.ClientWidth;
  h := FForm.ClientHeight;
  FForm.BorderStyle := bsNone;
  FForm.ClientWidth := w;
  FForm.ClientHeight := h;

  FTitle := FForm.Caption;
  FObjectSet := False;
  FDirty := False;
end;

{$ENDIF}

destructor TDCDVBPropertyPage.Destroy;
begin
{$IFDEF FORMS}
  if Assigned(FForm) then
  begin
    FForm.Free;
    FForm := nil;
  end;
{$ENDIF}

  inherited Destroy;
end;

function TDCDVBPropertyPage._Release: Integer;
begin
  if (InterlockedDecrement(FRefCount) = 0) then
  begin
    inc(FRefCount);
    SetPageSite(nil);
    SetObjects(0, nil);
    Result := 0;
    Free;
  end else
  begin
    Result := FRefCount;
  end;
end;

function TDCDVBPropertyPage.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IPropertyPage) then
  begin
    if GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited QueryInterface(IID, Obj)
  end;
end;

procedure TDCDVBPropertyPage.SetPageDirty;
begin
  FDirty := True;
  if Assigned(FPageSite) then FPageSite.OnStatusChange(PROPPAGESTATUS_DIRTY);
end;

function TDCDVBPropertyPage.OnConnect(pUnknown: IUnknown): HRESULT;
begin
  Result := NOERROR;
end;

function TDCDVBPropertyPage.OnDisconnect: HRESULT;
begin
  Result := NOERROR;
end;

function TDCDVBPropertyPage.OnActivate: HRESULT;
begin
  Result := NOERROR;
end;

function TDCDVBPropertyPage.OnDeactivate: HRESULT;
begin
  Result := NOERROR;
end;

function TDCDVBPropertyPage.OnApplyChanges: HRESULT;
begin
  Result := NOERROR;
end;

function TDCDVBPropertyPage.SetPageSite(const pageSite: IPropertyPageSite): HResult;
begin
  if Assigned(pageSite) then
  begin
    if Assigned(FPageSite) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

    FPageSite := PageSite;
  end else
  begin
    if not Assigned(FPageSite) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

    FPageSite := nil;
  end;
  Result := NOERROR;
end;

function TDCDVBPropertyPage.Activate(hwndParent: HWnd; const rc: TRect; bModal: BOOL): HResult;
begin
  if not Assigned(@rc) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if (FObjectSet = False) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

{$IFDEF FORMS}
  if (FForm = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
{$ELSE}
  if (FWindow > 0) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
{$ENDIF}

{$IFDEF FORMS}
  SetParent(FForm.Handle, hwndParent);
  if Assigned(FForm.OnActivate)
    then FForm.OnActivate(FForm);
{$ELSE}
  FWindow := CreateDialogParam(HInstance, MAKEINTRESOURCE(FDialogID), hwndParent, @DialogProc, LPARAM(Self));
  if (FWindow = 0) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  OnActivate;
{$ENDIF}

  Move(rc);
  Result := Show(SW_SHOWNORMAL);
end;

function TDCDVBPropertyPage.Deactivate: HResult;
var
  dwStyle: Cardinal;
{$IFNDEF FORMS}
  h: THandle;
{$ENDIF}
begin
{$IFDEF FORMS}
  if (FForm = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  dwStyle := GetWindowLong(FForm.Handle, GWL_EXSTYLE);
  dwStyle := dwStyle and not WS_EX_CONTROLPARENT;

  SetWindowLong(FForm.Handle, GWL_EXSTYLE, dwStyle);
  if Assigned(FForm.OnDeactivate)
    then FForm.OnDeactivate(FForm);
{$ELSE}
  if (FWindow = 0) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  dwStyle := GetWindowLong(FWindow, GWL_EXSTYLE);
  dwStyle := dwStyle and not WS_EX_CONTROLPARENT;

  h := FWindow;
  FWindow := 0;
  SetWindowLong(h, GWL_EXSTYLE, dwStyle);
  FWindow := h;

  OnDeactivate;

  DestroyWindow(FWindow);
  FWindow := 0;
{$ENDIF}

  Result := NOERROR;
end;

function TDCDVBPropertyPage.GetPageInfo(out pageInfo: TPropPageInfo): HResult;
var
  pszTitle: PWideChar;
  len: Integer;
begin
  if not Assigned(@pageInfo) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  len := sizeof(WCHAR) * (length(FTitle)+1);
  pszTitle := CoTaskMemAlloc(len);
  if (pszTitle = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  CopyMemory(pszTitle, PWideChar(FTitle), len);

  pageInfo.cb               := sizeof(TPropPageInfo);
  pageInfo.pszTitle         := pszTitle;
  pageInfo.pszDocString     := nil;
  pageInfo.pszHelpFile      := nil;
  pageInfo.dwHelpContext    := 0;

  pageInfo.size.cx          := 340;
  pageInfo.size.cy          := 150;

{$IFDEF FORMS}
  if Assigned(FForm) then
  begin
    pageInfo.size.cx := FForm.ClientWidth;
    pageInfo.size.cy := FForm.ClientHeight;
  end;
{$ELSE}
  GetDialogSize(FDialogID, @DialogProc, 0, pageInfo.size);
{$ENDIF}

  Result := NOERROR;
end;

function TDCDVBPropertyPage.SetObjects(cObjects: Longint; pUnkList: PUnknownList): HResult;
begin
  if (cObjects = 1) then
  begin
    if (pUnkList = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    FObjectSet := True;
  {$IFDEF FORMS}
    if (FForm = nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;
    Result := FForm.OnConnect(pUnkList[0]);
  {$ELSE}
    Result := OnConnect(pUnkList[0]);
  {$ENDIF}
    Exit;
  end else
  if (cObjects = 0) then
  begin
    FObjectSet := False;
  {$IFDEF FORMS}
    if (FForm = nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;
    Result := FForm.OnDisconnect;
  {$ELSE}
    Result := OnDisconnect;
  {$ENDIF}
    Exit;
  end;

  Result := E_UNEXPECTED;
end;

function TDCDVBPropertyPage.Show(nCmdShow: Integer): HResult;
begin
{$IFDEF FORMS}
  if (FForm = nil) then
  begin
    Result := E_UNEXPECTED;
    exit;
  end;
{$ELSE}
  if (FWindow = 0) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
{$ENDIF}

  if ((nCmdShow <> SW_SHOW) and (nCmdShow <> SW_SHOWNORMAL) and (nCmdShow <> SW_HIDE)) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

{$IFDEF FORMS}
  if nCmdShow in [SW_SHOW,SW_SHOWNORMAL]
    then FForm.Show
    else FForm.Hide;
  InvalidateRect(FForm.Handle, nil, True);
{$ELSE}
  ShowWindow(FWindow, nCmdShow);
  InvalidateRect(FWindow, nil, True);
{$ENDIF}
  Result := NOERROR;
end;

{$IFDEF FORMS}
{$ELSE}
{$ENDIF}
function TDCDVBPropertyPage.Move(const rect: TRect): HResult;
begin
  if (@rect = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

{$IFDEF FORMS}
  if (FForm = nil) then
  begin
    result := E_UNEXPECTED;
    exit;
  end;
{$ELSE}
  if (FWindow = 0) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
{$ENDIF}

{$IFDEF FORMS}
  MoveWindow(FForm.Handle, rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top, True);
{$ELSE}
  MoveWindow(FWindow, rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top, True);
{$ENDIF}

  Result := NOERROR;
end;

function TDCDVBPropertyPage.IsPageDirty: HResult;
begin
  Result := _IF(FDirty, S_OK, S_FALSE);
end;

function TDCDVBPropertyPage.Apply: HResult;
begin
  if (FObjectSet = False) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  if not Assigned(FPageSite) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  if (FDirty = False) then
  begin
    Result := NOERROR;
    Exit;
  end;

{$IFDEF FORMS}
  if (FForm = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
  Result := FForm.OnApplyChanges;
{$ELSE}
  Result := OnApplyChanges;
{$ENDIF}

  if (SUCCEEDED(Result))
    then FDirty := False;
end;

function TDCDVBPropertyPage.Help(pszHelpDir: POleStr): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDCDVBPropertyPage.TranslateAccelerator(msg: PMsg): HResult;
begin
  Result := E_NOTIMPL;
end;

(*** TDCDVBFormPropertyPage ***************************************************)

{$IFDEF FORMS}

procedure TDCDVBFormPropertyPage.WndProc(var Message: TMessage);
var
  lpss : PStyleStruct;
begin
  if (Message.Msg = WM_STYLECHANGING) and (Message.WParam = GWL_EXSTYLE) then
  begin
    lpss := PStyleStruct(Message.LParam);
    lpss.styleNew := lpss.styleNew or WS_EX_CONTROLPARENT;
    Message.Result := 0;
    Exit;
  end;

  inherited WndProc(Message);
end;

function TDCDVBFormPropertyPage.OnConnect(Unknown: IUnknown): HRESULT;
begin
  Result := S_OK;
end;

function TDCDVBFormPropertyPage.OnDisconnect: HRESULT;
begin
  Result := S_OK;
end;

function TDCDVBFormPropertyPage.OnApplyChanges: HRESULT;
begin
  Result := S_OK;
end;

{$ENDIF}

(*** TDCDVBBasePlugin *********************************************************)

constructor TDCDVBBasePlugin.Create;
begin
  inherited Create;
  FPages := TList.Create;
end;

destructor TDCDVBBasePlugin.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPages.Count -1
    do Dispose(PGuid(FPages[i]));
  FPages.Clear;
  FPages.Free;
  inherited Destroy;
end;

function TDCDVBBasePlugin.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, ISpecifyPropertyPages) and (FPages.Count = 0) then
  begin
    Result := E_NOINTERFACE;
    Exit;
  end;

  Result := inherited QueryInterface(IID, Obj);
end;

procedure TDCDVBBasePlugin.AddPropertyPage(const AGuid: TGuid);
var
  pg: PGuid;
begin
  new(pg);
  pg^ := AGuid;
  FPages.Add(pg);
end;

function TDCDVBBasePlugin.GetPages(out pages: TCAGUID): HResult;
var
  i: Integer;
begin
  if FPages.Count = 0 then
  begin
    Result := E_FAIL;
    Exit;
  end;

  pages.cElems := FPages.Count;
  pages.pElems := CoTaskMemAlloc(sizeof(TGUID) * pages.cElems);

  if (pages.pElems = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  for i := 0 to FPages.Count -1
    do pages.pElems^[i] := PGuid(FPages[i])^;

  Result := S_OK;
end;

end.



