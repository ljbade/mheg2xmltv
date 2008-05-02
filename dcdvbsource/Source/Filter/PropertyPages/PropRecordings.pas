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

unit PropRecordings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  DVBInterface, ComCtrls, formAddRecording, IDVBSource, ActiveX, BDAUtils,
  DVBRecordings, DVBSettings;

type
  TFormPropRecordings = class(TFormPropertyPage)
    TabControl1: TTabControl;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ListView1: TListView;
    procedure FormPaint(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListView1DblClick(Sender: TObject);
  private
    FDVBFilter: IDCDVBSource;
    procedure CheckButtons;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
  end;

implementation

{$R *.DFM}

function TFormPropRecordings.OnConnect(Unknown: IInterface): HRESULT;
begin
  Unknown.QueryInterface(IID_IDCDVBSource, FDVBFilter);
  Button2Click(Self);
  Result := S_OK;
end;

procedure TFormPropRecordings.FormPaint(Sender: TObject);
const
  LOOPCOUNT = 4;
var
  i: Integer;
  w: array[0..LOOPCOUNT -1] of Integer;
begin
  InvalidateRect(ListView1.Handle, nil, True);
  SendMessage(ListView1.Handle,WM_NCPAINT,0,0);

  // This is so stupid ...
  ListView1.Columns.BeginUpdate;

  for i := 0 to LOOPCOUNT -1 do
    w[i] := ListView1.Columns[i].Width;

  for i := 0 to LOOPCOUNT -1 do
  begin
    ListView1.Columns[i].Width := ListView1.Columns[i].Width - 1;
    ListView1.Columns[i].Width := ListView1.Columns[i].Width + 1;
  end;

  for i := 0 to LOOPCOUNT -1 do
    ListView1.Columns[i].Width := w[i];

  ListView1.Columns.EndUpdate;
end;

procedure TFormPropRecordings.Button2Click(Sender: TObject);
var
  i, c: Integer;
  rec: PDVBRecordingSetting;
  item: TListItem;
  chan: PChar;
begin
  ListView1.Items.Clear;

  if not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.get_RecordingsCount(c);
  for i := 0 to c -1 do
  begin
    FDVBFilter.get_Recording(i, rec);
    FDVBFilter.get_ChannelInfo(rec.ChannelIndex, chan);
    item := ListView1.Items.Add;
    item.Caption := rec.Name;
    item.SubItems.Add(FormatDateTime('dddd, d/m hh:nn', GetDate(rec.StartTime)));
    item.SubItems.Add(FormatDateTime('dddd, d/m hh:nn', GetDate(rec.EndTime)));
    item.SubItems.Add(RemoveUnwantedChars(chan));
    case rec.Status of
      rsWaiting: item.SubItems.Add('Waiting');
      rsRecording: item.SubItems.Add('Recording');
      rsStopped: item.SubItems.Add('Stopped');
      rsFailed: item.SubItems.Add('Failed');
      rsInvalid: item.SubItems.Add('Invalid');
    end;
  end;
  CheckButtons;
end;

procedure TFormPropRecordings.FormDestroy(Sender: TObject);
begin
  FDVBFilter := nil;
end;

procedure TFormPropRecordings.Button3Click(Sender: TObject);
var
  dateForm: TfrmAddRecording;
  p: TPoint;
  i: Integer;
  channels: Integer;
  channel_name: PChar;
  s, e: TDateTime;
  rec: TDVBRecordingSetting;
begin
  if not Assigned(FDVBFilter)
    then Exit;

  dateForm := TfrmAddRecording.Create(Self);
  try
    p.X := 0;
    p.Y := 0;
    Windows.ClientToScreen(Handle, p);
    dateForm.Left := p.X + ((Width - dateForm.Width) div 2);
    dateForm.Top := p.Y + ((Height - dateForm.Height) div 2);

    FDVBFilter.get_ChannelCount(channels);
    for i := 0 to channels -1 do
    begin
      FDVBFilter.get_ChannelInfo(i, channel_name);
      dateForm.ComboBox1.Items.Add((channel_name));
      CoTaskMemFree(channel_name);
    end;
    FDVBFilter.get_ChannelSelected(channels);
    dateForm.ComboBox1.ItemIndex := channels;

    dateForm.ShowModal;
    if not dateForm.IsValid
      then Exit;

    dateForm.GetTime(s, e);
    rec.StartTime := GetDate(s);
    rec.EndTime := GetDate(e);
    rec.Name := dateForm.Edit2.Text;
    rec.Location := dateForm.Edit1.Text;
    rec.ChannelIndex := dateForm.ComboBox1.ItemIndex;
    FDVBFilter.put_Recording(@rec);
    sleep(100);
    Button2Click(Self);
  finally
    dateForm.Free;
  end;
  CheckButtons;
end;

procedure TFormPropRecordings.Button1Click(Sender: TObject);
var
  idx: Integer;
  rec: PDVBRecordingSetting;
begin
  if (ListView1.Selected = nil) or (FDVBFilter = nil)
    then Exit;
  idx := ListView1.Selected.Index;

  FDVBFilter.get_Recording(idx, rec);
  FDVBFilter.put_DeleteRecording(rec);
  Button2Click(Self);
  CheckButtons;
end;

procedure TFormPropRecordings.Button5Click(Sender: TObject);
var
  c: Integer;
  rec: PDVBRecordingSetting;
begin
  if (ListView1.Items.Count = 0) or (FDVBFilter = nil)
    then Exit;

  FDVBFilter.get_RecordingsCount(c);
  while (c > 0) do
  begin
    FDVBFilter.get_Recording(0, rec);
    FDVBFilter.put_DeleteRecording(rec);
    dec(c);
  end;
  Button2Click(Self);
end;

procedure TFormPropRecordings.Button4Click(Sender: TObject);
var
  idx: Integer;
  rec: PDVBRecordingSetting;
  dateForm: TfrmAddRecording;
  p: TPoint;
  i: Integer;
  channels: Integer;
  channel_name: PChar;
  s, e: TDateTime;
begin
  if (ListView1.Selected = nil) or (FDVBFilter = nil)
    then Exit;
  idx := ListView1.Selected.Index;

  dateForm := TfrmAddRecording.Create(Self);
  try
    p.X := 0;
    p.Y := 0;
    Windows.ClientToScreen(Handle, p);
    dateForm.Left := p.X + ((Width - dateForm.Width) div 2);
    dateForm.Top := p.Y + ((Height - dateForm.Height) div 2);

    FDVBFilter.get_ChannelCount(channels);
    for i := 0 to channels -1 do
    begin
      FDVBFilter.get_ChannelInfo(i, channel_name);
      dateForm.ComboBox1.Items.Add((channel_name));
      CoTaskMemFree(channel_name);
    end;
    FDVBFilter.get_ChannelSelected(channels);
    dateForm.ComboBox1.ItemIndex := channels;

    FDVBFilter.get_Recording(idx, rec);

    dateForm.Edit1.Enabled := rec.Status = rsWaiting;
    dateForm.ComboBox1.Enabled := rec.Status = rsWaiting;
    dateForm.SpeedButton1.Enabled := rec.Status = rsWaiting;
    dateForm.Edit1.Text := rec.Location;
    dateForm.Edit2.Text := rec.Name;
    dateForm.SetTime(GetDate(rec.StartTime), GetDate(rec.EndTime));
    dateForm.ComboBox1.ItemIndex := rec.ChannelIndex;

    dateForm.ShowModal;
    if not dateForm.IsValid
      then Exit;

    dateForm.GetTime(s, e);
    rec.StartTime := GetDate(s);
    rec.EndTime := GetDate(e);
    rec.Name := dateForm.Edit2.Text;
    rec.Location := dateForm.Edit1.Text;
    rec.ChannelIndex := dateForm.ComboBox1.ItemIndex;
    FDVBFilter.put_EditRecording(rec);
    Button2Click(Self);
  finally
    dateForm.Free;
  end;
  CheckButtons;
end;

procedure TFormPropRecordings.CheckButtons;
begin
  Button1.Enabled := ListView1.Selected <> nil;
  Button4.Enabled := ListView1.Selected <> nil;
  Button5.Enabled := ListView1.Items.Count > 0;
end;

procedure TFormPropRecordings.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  CheckButtons;
end;

procedure TFormPropRecordings.ListView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CheckButtons;
end;

procedure TFormPropRecordings.ListView1DblClick(Sender: TObject);
begin
  if (ListView1.Selected = nil) or (FDVBFilter = nil)
    then Exit;
  Button4Click(Self);
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropRecordings, CLSID_PropertyPageRecordings);

end.
