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

unit formPropertyPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComCtrls, StdCtrls, ExtCtrls, Buttons, DSUtil;

type
  TfrmPropertyPage = class(TForm, IPropertyPageSite)
    pnlControls: TPanel;
    btClose: TButton;
    pgPages: TPageControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    FRefCount: Longint;
    FNumPages: integer;
    FPages: TList;
    FPanels: TList;
  protected
    // IPropertyPageSite
    function OnStatusChange(flags: Longint): HResult; stdcall;
    function GetLocaleID(out localeID: TLCID): HResult; stdcall;
    function GetPageContainer(out unk: IUnknown): HResult; stdcall;
    function TranslateAccelerator(msg: PMsg): HResult; stdcall;
    function CreatePage(AUnknown: IUnknown; ACaption: WideString): Boolean;
  public
    class function CreateInstance(AOwner: TComponent; AUnknown: IUnknown; ACaption: WideString): TfrmPropertyPage;
  end;

implementation

{$R *.dfm}

class function TfrmPropertyPage.CreateInstance(AOwner: TComponent; AUnknown: IUnknown; ACaption: WideString): TfrmPropertyPage;
begin
  Result := TfrmPropertyPage.Create(AOwner);
  if not Result.CreatePage(AUnknown, ACaption) then
  begin
    Result.Free;
    Result := nil;
    Exit;
  end;
end;

function TfrmPropertyPage.CreatePage(AUnknown: IUnknown; ACaption: WideString): Boolean;
var
  i: integer;
  rect: TRect;
  Info: TPropPageInfo;
  sheet: TTabSheet;
  mWidth, mHeight: integer;
  CAGUID: TCAGUID;
  Prop: ISpecifyPropertyPages;
  Page: IPropertyPage;
  Panel: TPanel;
begin
  Result := False;
  CAGUID.cElems := 0;
  Caption := ACaption;

  if not Assigned(AUnknown) or (AUnknown.QueryInterface(IID_ISpecifyPropertyPages, Prop) <> S_OK) or
     not Assigned(Prop) then Exit;
  if Prop.GetPages(CAGUID) <> S_OK then
  begin
    Prop := nil;
    Exit;
  end;
  if CAGUID.cElems <= 0  then
  begin
    Exit;
  end;
  mWidth := 0;
  mHeight := 0;

  fNumPages := CAGUID.cElems;

  for i := 0 to CAGUID.cElems -1 do
  begin
    CoCreateInstance(CAGUID.pElems[i], nil, CLSCTX_INPROC_SERVER, IPropertyPage, Page);
    Page._AddRef;
    fPages.Add(Pointer(Page));

    Page.GetPageInfo(Info);
    Page.SetObjects(1, @AUnknown);
    Page.SetPageSite(Self as IPropertyPageSite);

    sheet := TTabSheet.Create(pgPages);
    sheet.PageControl := pgPages;
    sheet.Name := 'prop' + inttostr(i);
    sheet.Caption := Info.pszTitle;

    Panel := TPanel.CreateParented(sheet.Handle);
    Panel.Parent := sheet;
    Panel.Name := 'proppanel' + inttostr(i);
    Panel.BorderStyle := bsNone;
    Panel.Caption := '';
    Panel.BevelOuter := bvNone;
    Panel.BevelInner := bvNone;
    Panel.Left := 0;
    Panel.Top := 0;
    Panel.ParentBackground := False;
    fPanels.Add(Panel);

    rect.Left := 0;
    rect.Top := 0;
    rect.Right := info.size.cx;
    rect.Bottom := info.size.cy;
    rect.TopLeft.X := rect.Left;
    rect.TopLeft.Y := rect.Top;
    rect.BottomRight.X := rect.Right;
    rect.BottomRight.Y := rect.Bottom;

    if mWidth < info.size.cx then mWidth := info.size.cx;
    if mHeight < info.size.cy then mHeight := info.size.cy;
    Page.Activate(Panel.Handle, rect, False);
    Page.Show(SW_SHOWNORMAL);
    Page := nil;
  end;

  pgPages.TabIndex := 0;

  pgPages.Width := mWidth + (pgPages.Width - pgPages.Pages[0].Width) + pgPages.Pages[0].Left div 2;
  pgPages.Height := mHeight + (pgPages.Height - pgPages.Pages[0].Height) + pgPages.Pages[0].Left div 2;

  for i := 0 to CAGUID.cElems -1 do
  begin
    TPanel(fPanels.Items[i]).Width := mWidth;
    TPanel(fPanels.Items[i]).Height := mHeight;
  end;

  ClientWidth := pgPages.Width + 18;
  ClientHeight := pgPages.Height + pnlControls.Height + 18;

  Prop := nil;
  if (CAGUID.cElems > 0) and Assigned(CAGUID.pElems)
    then CoTaskMemFree(CAGUID.pElems);

  Result := True;
end;

function TfrmPropertyPage.OnStatusChange(flags: Longint): HResult;
begin
  Result := S_OK;
end;

function TfrmPropertyPage.GetLocaleID(out localeID: TLCID): HResult;
begin
  if not Assigned(@localeID) then
  begin
    Result := E_POINTER;
  end else
  begin
    LocaleID := GetUserDefaultLCID;
    Result := S_OK;
  end;
end;

function TfrmPropertyPage.GetPageContainer(out unk: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;

function TfrmPropertyPage.TranslateAccelerator(msg: PMsg): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TfrmPropertyPage.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
  Page: IPropertyPage;
begin
  for i := 0 to fNumPages -1 do
  begin
    Page := IPropertyPage(fPages.Items[i]);
    Page.Show(SW_HIDE);
    Page.Deactivate;
    Page.SetObjects(0,nil);
    Page._Release;
    Page := nil;
    TPanel(fPanels.Items[i]).Destroy;
  end;

  fPages.Clear;
  FreeAndNil(fPages);
  fPanels.Clear;
  FreeAndNil(fPanels);
  Action := caFree;
end;

procedure TfrmPropertyPage.FormCreate(Sender: TObject);
begin
  fNumPages := 0;
  fRefCount := 0;
  pnlControls.Align := alBottom;
  fPages := TList.Create;
  fPanels := TList.Create;
  BorderIcons := [biSystemMenu];
end;

procedure TfrmPropertyPage.btCloseClick(Sender: TObject);
begin
  Close;
end;

end.
