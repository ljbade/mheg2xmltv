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
  ExtCtrls, Forms, ComCtrls, ShellAPI, DCDVBShared, DCDVBTuningPlugins,
  Dialogs;

const
  CLSID_FileReaderPropertyPageSettings: TGuid = '{A7BDD222-07A7-42DF-91FA-AC1D0CF98AD9}';
  IID_IFileReaderDevice: TGuid =                '{2B75A476-0C00-469B-85F3-CCE535EA9546}';

type
  TFileMapping = record
    AFrequency: Integer;
    AFilename: array[0..MAX_PATH-1] of WideChar;
  end;
  PFileMapping = ^TFileMapping;

  IFileReaderDevice = interface(IUnknown)
  ['{2B75A476-0C00-469B-85F3-CCE535EA9546}']
    function SetFileMappings(AMappings: PFileMapping; ACountMappings: Integer): HRESULT; stdcall;
    function GetFileMappings(out AMappings: PFileMapping; out ACountMappings: Integer): HRESULT; stdcall;
  end;

  TfrmPropSettings = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    GroupBox1: TGroupBox;
    OpenDialog1: TOpenDialog;
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure ListView1Edited(Sender: TObject; Item: TListItem; var S: string);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Button2Click(Sender: TObject);
  private
    FIntf: IFileReaderDevice;
    procedure SendList;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
  end;

implementation

{$R *.DFM}

function TfrmPropSettings.OnConnect(Unknown: IInterface): HRESULT;
var
  mappings: PFileMapping;
  mappings2: PFileMapping;
  mapcount: Integer;
  item: TListItem;
  i: Integer;
begin
  Unknown.QueryInterface(IID_IFileReaderDevice, FIntf);
  if Assigned(FIntf) then
  begin
    if FIntf.GetFileMappings(mappings, mapcount) = S_OK then
    begin
      mappings2 := mappings;

      for i := 0 to mapcount - 1 do
      begin
        item := ListView1.Items.Add;
        item.Caption := inttostr(mappings2.AFrequency);
        item.SubItems.Add(mappings2.AFilename);
        inc(mappings2);
      end;

      CoTaskMemFree(mappings);
    end;
  end;
  Result := S_OK;
end;

procedure TfrmPropSettings.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Button2.Enabled := ListView1.Selected <> nil;
  Button3.Enabled := ListView1.Items.Count > 0;
end;

procedure TfrmPropSettings.ListView1Edited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  Item.Caption := S;
  SendList;
end;

procedure TfrmPropSettings.Button3Click(Sender: TObject);
begin
  FIntf.SetFileMappings(nil, 0);
  ListView1.Clear;
  Button3.Enabled := ListView1.Items.Count > 0;
end;

procedure TfrmPropSettings.Button1Click(Sender: TObject);
var
  i: Integer;
  item: TListItem;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      item := ListView1.Items.Add;
      item.Caption := '000000';
      item.SubItems.Add(OpenDialog1.Files.Strings[i]);
    end;
  end;

  SendList;
end;

procedure TfrmPropSettings.Button2Click(Sender: TObject);
begin
  if ListView1.Selected = nil
    then Exit;

  ListView1.Selected.Delete;
  Button3.Enabled := ListView1.Items.Count > 0;
  SendList;
end;

procedure TfrmPropSettings.SendList;
var
  i: Integer;
  mappings, mappings2: PFileMapping;
  str: WideString;
begin
  if ListView1.Items.Count = 0 then
  begin
    FIntf.SetFileMappings(nil, 0);
    ListView1.Clear;
  end;

  mappings := CoTaskMemAlloc(sizeof(TFileMapping) * ListView1.Items.Count);
  mappings2 := mappings;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    mappings2^.AFrequency := strtoint(ListView1.Items.Item[i].Caption);
    str := WideString(ListView1.Items.Item[i].Subitems.Strings[0]);
    Move(str[1], mappings2^.AFilename[0], sizeof(WideChar) * (Length(str) + 1));
    inc(mappings2);
  end;

  FIntf.SetFileMappings(mappings, ListView1.Items.Count);

  CoTaskMemFree(mappings);
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropSettings,
    CLSID_FileReaderPropertyPageSettings
  );

end.
