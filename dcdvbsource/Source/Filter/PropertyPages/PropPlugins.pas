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

unit PropPlugins;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  DVBInterface, ShellAPI, ComCtrls, CheckLst, IDVBSource, DCDVBDataPlugins, DSUtil,
  DCDVBShared;

type
  TFormPropPlugins = class(TFormPropertyPage, IPropertyPageSite)
    TabControl1: TTabControl;
    CheckListBox1: TCheckListBox;
    PageControl1: TPageControl;
    procedure FormShow(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FIntf: IDCDVBSource;
    FCurrentIndex: Integer;
    FPages: TInterfaceList;
    FPanels: TList;
    procedure ClearPluginPages;
    // IPropertyPageSite
    function OnStatusChange(flags: Longint): HResult; stdcall;
    function GetLocaleID(out localeID: TLCID): HResult; stdcall;
    function GetPageContainer(out unk: IUnknown): HResult; stdcall;
    function TranslateAccelerator(msg: PMsg): HResult; stdcall;
  public
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;
  end;

implementation

{$R *.DFM}

function TFormPropPlugins.OnConnect(Unknown: IInterface): HRESULT;
var
  c, i: Integer;
  plugin: IDCDVBDataPlugin;
  enabled: Boolean;
  name: PChar;
begin
  Unknown.QueryInterface(IID_IDCDVBSource, FIntf);
  if Assigned(FIntf) then
  begin
    CheckListBox1.Clear;
    FIntf.get_PluginsCount(c);
    for i := 0 to c -1 do
    begin
      FIntf.get_Plugin(i, IUnknown(plugin));
      FIntf.get_PluginEnabled(i, enabled);
      plugin.get_PluginName(name);
      CheckListBox1.Items.Add(name);
      CheckListBox1.Checked[i] := enabled;
    end;
  end;

  Result := S_OK;
end;

procedure TFormPropPlugins.FormCreate(Sender: TObject);
begin
  FCurrentIndex := -1;
  FPages := TInterfaceList.Create;
  FPanels := TList.Create;
end;

procedure TFormPropPlugins.FormDestroy(Sender: TObject);
begin
  ClearPluginPages;
  FPages.Free;
  FPanels.Free;
end;

procedure TFormPropPlugins.FormShow(Sender: TObject);
begin
  if (CheckListBox1.Count > 0) and (CheckListBox1.ItemIndex < 0) then
  begin
    CheckListBox1.ItemIndex := 0;
    CheckListBox1Click(Self);
  end;
end;

procedure TFormPropPlugins.CheckListBox1ClickCheck(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FIntf) then
  begin
    for i := 0 to CheckListBox1.Items.Count -1 do
    begin
      FIntf.put_PluginEnabled(i, CheckListBox1.Checked[i]);
    end;
  end;
end;

procedure TFormPropPlugins.ClearPluginPages;
var
  i: Integer;
begin
  for i := 0 to FPages.Count -1 do
  begin
    with IPropertyPage(FPages[i]) do
    begin
      Show(SW_HIDE);
      Deactivate;
      SetObjects(0, nil);
    end;
  end;
  FPages.Clear;

  for i := 0 to FPanels.Count -1
    do TPanel(FPanels[i]).Free;
  FPanels.Clear;

  for i := 0 to PageControl1.PageCount -1 do
  begin
    try
      PageControl1.Pages[0].Free;
    except
    end;
  end;
end;

procedure TFormPropPlugins.CheckListBox1Click(Sender: TObject);
var
  plugin: IDCDVBDataPlugin;
  prop: ISpecifyPropertyPages;
  CAGUID: TCAGUID;
  page: IPropertyPage;
  i: Integer;
  hr: HRESULT;
  info: TPropPageInfo;
  sheet: TTabSheet;
  panel: TPanel;
  page_site: IPropertyPageSite;
begin
  if CheckListBox1.ItemIndex = FCurrentIndex
    then Exit;

  // clear all Pages
  ClearPluginPages;
  FCurrentIndex := CheckListBox1.ItemIndex;

  if FCurrentIndex < 0
    then Exit;

  // show the current page
  if Assigned(FIntf) then
  begin
    plugin := nil;
    FIntf.get_Plugin(FCurrentIndex, IUnknown(plugin));

    if not Assigned(plugin) or
       not (plugin.QueryInterface(IID_ISpecifyPropertyPages, prop) = S_OK) or
       not Assigned(prop)
      then Exit;

    if prop.GetPages(CAGUID) <> S_OK
      then Exit;

    if CAGUID.cElems <= 0
      then Exit;

    for i := 0 to CAGUID.cElems -1 do
    begin
      hr := CoCreateInstance(CAGUID.pElems[i], nil, CLSCTX_INPROC_SERVER, IPropertyPage, page);
      if hr = S_OK then
      begin
        FPages.Add(page);

        page.GetPageInfo(info);
        page.SetObjects(1, @plugin);
        if QueryInterface(IPropertyPageSite, page_site) = S_OK
          then page.SetPageSite(page_site);

        sheet := TTabSheet.Create(PageControl1);
        sheet.Name := 'prop' + inttostr(i);
        sheet.Caption := Info.pszTitle;
        sheet.PageControl := PageControl1;



        panel := TPanel.CreateParented(sheet.Handle);
        panel.Visible := False;
        panel.Parent := sheet;
        panel.Name := 'proppanel' + inttostr(i);
        panel.BorderStyle := bsNone;
        panel.Caption := '';
        panel.BevelOuter := bvNone;
        panel.BevelInner := bvNone;
        panel.Align := alClient;
        panel.Align := alNone;
        panel.Width := info.size.cx;
        panel.Height := info.size.cy;
        panel.ParentBackground := False;
        panel.Visible := True;
        FPanels.Add(panel);

        page.Activate(panel.Handle, panel.ClientRect, False);
        page.Show(SW_SHOWNORMAL);
      end;
    end;

    if (CAGUID.cElems > 0) and Assigned(CAGUID.pElems)
      then CoTaskMemFree(CAGUID.pElems);
  end;
end;

function TFormPropPlugins.OnStatusChange(flags: Longint): HResult;
begin
  Result := S_OK;
end;

function TFormPropPlugins.GetLocaleID(out localeID: TLCID): HResult;
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

function TFormPropPlugins.GetPageContainer(out unk: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFormPropPlugins.TranslateAccelerator(msg: PMsg): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFormPropPlugins.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IPropertyPageSite) then
  begin
    if GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Result := inherited QueryInterface(IID, Obj);
  end;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropPlugins, CLSID_PropertyPagePlugins);

end.
