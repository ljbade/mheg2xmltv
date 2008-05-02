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

unit PropMHP;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, BDAUtils,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  DVBInterface, ComCtrls, IDVBSource, JvSimpleXml, ActiveX, MHPUtils, ShellAPI;

type
  TFormPropMHP = class(TFormPropertyPage)
    TabControl1: TTabControl;
    Button1: TButton;
    Button2: TButton;
    TreeView1: TTreeView;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFilter: IDCDVBSource;
    FXML: TJvSimpleXml;
  published
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

uses Math, StrUtils;

{$R *.DFM}

function TFormPropMHP.OnConnect(Unknown: IUnknown): HRESULT;
begin
  Unknown.QueryInterface(IID_IDCDVBSource, FFilter);
  Button1Click(Self);
  Result := S_OK;
end;

procedure TFormPropMHP.FormDestroy(Sender: TObject);
begin
  FFilter := nil;
  FXML.Free;
end;

procedure TFormPropMHP.Button1Click(Sender: TObject);
var
  mhp: PChar;
  i, c: Integer;
  item, i2: TJvSimpleXmlElem;
  ti, ta: TTreeNode;
begin
  if not Assigned(FFilter)
    then Exit;

  TreeView1.Items.Clear;

  Button2.Enabled := False;
  
  if FFilter.get_MHPRoot(mhp) <> S_OK
    then Exit;

  FXML.LoadFromString(mhp);
  CoTaskMemFree(mhp);

  for i := 0 to FXML.Root.Items.Count -1 do
  begin
    item := FXML.Root.Items[i];

    ti := TreeView1.Items.AddChild(nil, item.Properties.Value('applicationname'));
    TreeView1.Items.AddChild(ti, 'Application Type: ' + GetMHPAplicationTypeString(strtoint(item.Properties.Value('applicationtype'))));
    TreeView1.Items.AddChild(ti, 'Application ID: ' + item.Properties.Value('applicationid'));
    TreeView1.Items.AddChild(ti, 'Organisation ID: ' + item.Properties.Value('organisationid'));
    TreeView1.Items.AddChild(ti, 'Application Control Code: ' + item.Properties.Value('applicationcontrolcode'));
    TreeView1.Items.AddChild(ti, 'Test Mode: ' +  IfThen(strtoint(item.Properties.Value('testapplication')) = 1, 'True', 'False'));
    ta := TreeView1.Items.AddChild(ti, 'Path');
    if strtoint(item.Properties.Value('applicationtype')) = 1 then
    begin
      TreeView1.Items.AddChild(ta, 'Location: ' + item.Properties.Value('savepath'));
      TreeView1.Items.AddChild(ta, 'Base Directory: ' + item.Properties.Value('basedirectory'));
      TreeView1.Items.AddChild(ta, 'Classpath Extension: ' + item.Properties.Value('classpathextension'));
      TreeView1.Items.AddChild(ta, 'Initial Class: ' + item.Properties.Value('initialclass'));
      i2 := item.Items.ItemNamed['parameters'];
      if Assigned(i2) then
      begin
        ta := TreeView1.Items.AddChild(ti, 'Parameters');
        for c := 0 to i2.Items.Count -1
          do TreeView1.Items.AddChild(ta, i2.Items[c].Properties.Value('name'));
      end;
    end;
  end;
end;

procedure TFormPropMHP.FormPaint(Sender: TObject);
begin
  InvalidateRect(TreeView1.Handle, nil, True);
  SendMessage(TreeView1.Handle,WM_NCPAINT,0,0);
end;

procedure TFormPropMHP.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button2.Enabled := TreeView1.Selected <> nil;
end;

procedure TFormPropMHP.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  Button2.Enabled := TreeView1.Selected <> nil;
end;

procedure TFormPropMHP.Button2Click(Sender: TObject);
var
  mhp_p: array[0..MAX_PATH-1] of Char;
  mhp: String;
  item: TTreeNode;
  index: Integer;
  i1: TJvSimpleXmlElem;
  batch_file: String;
  str: TStringList;
  s: String;
  execute_dir: String;
begin
  item := TreeView1.Selected;
  if item = nil
    then Exit;

  while item.Parent <> nil do
  begin
    item := item.Parent;
  end;
  index := item.Index;

  if index >= FXML.Root.Items.Count
    then Exit;

  GetModuleFileName(HInstance, mhp_p, MAX_PATH);
  mhp := AddBackSlash(ExtractFilePath(StrPas(mhp_p))) + 'MHP\';

  i1 := FXML.Root.Items[index];

  batch_file := AddBackSlash(i1.Properties.Value('savepath')) + 'Execute_' + GetValidDirectoryName(i1.Properties.Value('applicationname')) + '.bat';
  str := TStringList.Create;

  str.Add('@echo off');
  str.Add('set BASE_DIR="' + RemoveBackSlash(GetShortName(trim(mhp))) + '"');

  s := StringReplace(i1.Properties.Value('basedirectory'), '/', '\', [rfReplaceAll]);
  if s = ''
    then execute_dir := RemoveBackSlash(GetShortName(i1.Properties.Value('savepath')))
    else execute_dir := RemoveBackSlash(GetShortName(StringReplace(trim(AddBackSlash(i1.Properties.Value('savepath')) + s), '\\', '\', [rfReplaceAll])));
  str.Add('set XLET_PATH="' +  execute_dir + '"');
  str.Add('set XLET_CLASS=' + i1.Properties.Value('initialclass'));
  str.Add('set JAVATV_JAR=%BASE_DIR%\javatv_fcs\javatv.jar');
  str.Add('set JMF_JAR=%BASE_DIR%\JMF2.1.1e\lib\jmf.jar');
  str.Add('set COMPILED_OPENMHP_CLASSES_DIR=%BASE_DIR%;%BASE_DIR%\xml.jar');
  str.Add('cd %XLET_PATH%');
  str.Add('javaw -classpath .;%XLET_PATH%;%COMPILED_OPENMHP_CLASSES_DIR%;%JAVATV_JAR%;%JMF_JAR% org.openmhp.system.RunXlet %BASE_DIR% %XLET_CLASS% 1 1 %BASE_DIR%');
  str.SaveToFile(batch_file);

  str.Free;

  ShellExecute(GetDesktopWindow, 'open', PChar(batch_file), nil, PChar(AddBackSlash(execute_dir)), SW_HIDE);
end;

procedure TFormPropMHP.FormCreate(Sender: TObject);
begin
  FXML := TJvSimpleXML.Create(nil);
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropMHP, CLSID_PropertyPageMHP);

end.
