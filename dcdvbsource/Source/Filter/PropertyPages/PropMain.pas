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

unit PropMain;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  ComCtrls, DSUtil, DVBInterface, Buttons, Spin, BDAUtils, IDVBSource;

type
  TFormPropSettings = class;

  TMyThread = class(TThread)
  private
    FOwner: TFormPropSettings;
    FLock: TBCCritSec;
    FEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFormPropSettings; ALock: TBCCritSec);
  end;

  TFormPropSettings = class(TFormPropertyPage)
    SaveDialog1: TSaveDialog;
    TabControl1: TTabControl;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    SpeedButton2: TSpeedButton;
    TrackBar1: TTrackBar;
    GroupBox2: TGroupBox;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    ComboBox2: TComboBox;
    GroupBox6: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    CheckBox2: TCheckBox;
    Button1: TButton;
    GroupBox7: TGroupBox;
    ComboBox3: TComboBox;
    SpeedButton6: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
  private
    FLock: TBCCritSec;
    FMouseDown: Boolean;
    FCanChange: Boolean;
    FDVBFilter: IDCDVBSource;
    FSeeking: IStreamBufferMediaSeeking;
    FTimer: TMyThread;
    procedure ChangePosition(id: Integer; x: Integer);
    procedure SetPosition;
    procedure UpdatePosition(Start, Stop, Pos: Int64);
    procedure UpdateAudioStreams;
    procedure UpdateSubtitleStreams;
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

uses Math;

{$R *.DFM}

constructor TMyThread.Create(AOwner: TFormPropSettings; ALock: TBCCritSec);
begin
  inherited Create(True);
  FOwner := AOwner;
  FLock := ALock;
  FEvent := CreateEvent(nil, False, False, nil);
  Resume;
end;

procedure TMyThread.Execute;
begin
  Priority := tpLowest;
  while not Terminated do
  begin
    FLock.Lock;
    try
      FOwner.Timer1Timer(nil);
    finally
      FLock.UnLock;
    end;
    if WaitForSingleObject(FEvent, 1000) = WAIT_OBJECT_0
      then break;
  end;
  
  CloseHandle(FEvent);
end;

function TFormPropSettings.OnConnect(Unknown: IUnKnown): HRESULT;
var
  FilterInfo: TFilterInfo;
  EnumFilters: IEnumFilters;
  Filter: IBaseFilter;
  ClassID: TGuid;
  rate: Double;
  channels: Integer;
  i: Integer;
  channel_name: PChar;
  show: LongBool;
  page: Integer;
  sub_page: Integer;
begin
  Result := S_OK;
  FLock.Lock;
  try
    Unknown.QueryInterface(IID_IDCDVBSource, FDVBFilter);

    Unknown.QueryInterface(IID_IBaseFilter, Filter);
    if Assigned(Filter) then
    begin
      Filter.QueryFilterInfo(FilterInfo);
      Filter := nil;

      if Assigned(FilterInfo.pGraph) then
      begin
        if (FilterInfo.pGraph.EnumFilters(EnumFilters) = S_OK) then
        begin
          while (EnumFilters.Next(1, Filter, nil) = S_OK) do
          begin
            Filter.GetClassID(ClassID);
            if IsEqualGUID(ClassID, CLSID_StreamBufferSource) then
            begin
              Filter.QueryInterface(IID_IStreamBufferMediaSeeking, FSeeking);
              Filter := nil;
              break;
            end;
            Filter := nil;
          end;
          EnumFilters := nil;
        end;
        FilterInfo.pGraph := nil;
      end;
    end;

    if not Assigned(FSeeking) then
    begin
      GroupBox1.Enabled := False;
      GroupBox1.Caption := ' Time Shift (Timeshifting disabled) ';
      GroupBox5.Enabled := False;
      GroupBox5.Caption := ' Playrate (Timeshifting disabled) ';
    end else
    begin
      FCanChange := False;
      FSeeking.GetRate(rate);
      TrackBar1.Position := Round(rate * 1000);
      TrackBar1Change(Self);
      FCanChange := True;
    end;

    if Assigned(FDVBFilter) then
    begin
      FCanChange := False;

      FDVBFilter.get_ChannelCount(Channels);
      for i := 0 to Channels -1 do
      begin
        FDVBFilter.get_ChannelInfo(i, channel_name);
        ComboBox1.Items.Add((channel_name));
        CoTaskMemFree(channel_name);
      end;

      FDVBFilter.get_ChannelSelected(channels);
      ComboBox1.ItemIndex := channels;

      FDVBFilter.get_TeletextShow(show);
      CheckBox1.Checked := show;

      FDVBFilter.get_TeletextTransparent(show);
      CheckBox2.Checked := show;

      FDVBFilter.get_TeletextPage(page, sub_page);
      SpinEdit1.Value := page;
      SpinEdit2.Value := sub_page;

      FCanChange := True;
    end;

    FCanChange := False;
    UpdateAudioStreams;
    UpdateSubtitleStreams;
    FCanChange := True;
  finally
    FLock.UnLock;
  end;
end;

procedure TFormPropSettings.FormShow(Sender: TObject);
begin
  UpdateTrackbars(Self);
end;

procedure TFormPropSettings.TrackBar1Change(Sender: TObject);
var
  rate: Double;
begin
  if FCanChange and Assigned(FSeeking) then
  begin
    FSeeking.SetRate(TrackBar1.Position / 1000);
    FSeeking.GetRate(rate);
    label1.Caption := format('%.3fx', [rate]);
  end else
    label1.Caption := format('%.3fx', [TrackBar1.Position / 1000]);
end;

procedure TFormPropSettings.SpeedButton2Click(Sender: TObject);
begin
  TrackBar1.Position := 1000;
  TrackBar1Change(Self);
end;

procedure TFormPropSettings.Timer1Timer(Sender: TObject);

  function GetTime(ft: Int64): String;
  begin
    Result := Format('%s', [TimeToStr(RefTimeToMiliSec(ft) / MiliSecPerDay)])
  end;

var
  start, stop, pos: Int64;
begin
  if Assigned(FSeeking) then
  begin
    FSeeking.GetCurrentPosition(pos);
    FSeeking.GetAvailable(start, stop);

    label4.Caption := GetTime(pos);

    label7.Caption := GetTime(start);
    label8.Caption := GetTime(stop);

    if not FMouseDown
      then UpdatePosition(start, stop, pos);
  end;
end;

procedure TFormPropSettings.FormCreate(Sender: TObject);
begin
  FMouseDown := False;
  Panel2.Width := 0;
  FLock := TBCCritSec.Create;
  FTimer := TMyThread.Create(Self, FLock);
end;

procedure TFormPropSettings.FormDestroy(Sender: TObject);
begin
  SetEvent(FTimer.FEvent);
  FTimer.Terminate;
  FTimer.WaitFor;
  FTimer.Free;
  FLock.Free;
  FSeeking := nil;
  FDVBFilter := nil;
end;

procedure TFormPropSettings.ChangePosition(id: Integer; x: Integer);
var
  mx: Int64;
begin
  mx := Panel1.Width - 4;

  if id = 1
    then x := x - Panel2.Left;

  if x < 0
    then x := 0
  else if x > mx
    then x := mx;

  Panel2.Width := x;
end;

procedure TFormPropSettings.SetPosition;
var
  mx: Int64;
  start, stop: Int64;
  dur: Int64;
  pos: Int64;
begin
  if Assigned(FSeeking) then
  begin
    mx := Panel1.Width - 4;

    FSeeking.GetAvailable(start, stop);
    dur := stop - start;

    pos := Panel2.Width;
    pos := pos * dur div mx;
    pos := pos + start;

    FSeeking.SetPositions(pos, AM_SEEKING_AbsolutePositioning, pos, AM_SEEKING_NoPositioning);
  end;
end;

procedure TFormPropSettings.UpdatePosition(Start, Stop, Pos: Int64);
var
  mx: Int64;
  p: Int64;
  dur: Int64;
begin
  mx := Panel1.Width - 4;

  dur := Stop - Start;

  if dur = 0 then
  begin
    Panel2.Width := 0;
    Exit;
  end;

  if Pos < Start
    then Pos := Start
  else if Pos > Stop
    then Pos := Stop;

  p := Pos - Start;

  p := p * mx div dur;
  Panel2.Width := p;
end;

procedure TFormPropSettings.Panel2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    ChangePosition(0, x);
  end;
end;

procedure TFormPropSettings.Panel2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    ChangePosition(0, x);
  end;
end;

procedure TFormPropSettings.Panel2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ChangePosition(0, x);
    SetPosition;
    FMouseDown := False;
  end;
end;

procedure TFormPropSettings.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    ChangePosition(1, x);
  end;
end;

procedure TFormPropSettings.Panel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    ChangePosition(1, x);
  end;
end;

procedure TFormPropSettings.Panel1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ChangePosition(1, x);
    SetPosition;
    FMouseDown := False;
  end;
end;

procedure TFormPropSettings.ComboBox1Change(Sender: TObject);
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.put_ChannelSelected(ComboBox1.ItemIndex);
  FCanChange := False;
  UpdateAudioStreams;
  UpdateSubtitleStreams;
  FCanChange := True;
end;

procedure TFormPropSettings.ComboBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DrawComboBox(Control as TComboBox, Index, Rect, State);
end;

procedure TFormPropSettings.UpdateAudioStreams;
var
  Channels: Integer;
  i: Integer;
  Name: PChar;
begin
  ComboBox2.Clear;
  if Assigned(FDVBFilter) then
  begin
    Channels := 0;
    FDVBFilter.get_AudioStreamCount(Channels);
    for i := 0 to Channels -1 do
    begin
      FDVBFilter.get_AudioStreamInfo(i, Name);
      ComboBox2.Items.Add(Name);
      CoTaskMemFree(Name);
    end;
    FDVBFilter.get_AudioStreamSelected(i);
    ComboBox2.ItemIndex := i;
  end;
end;

procedure TFormPropSettings.ComboBox2Change(Sender: TObject);
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.put_AudioStreamSelected(ComboBox2.ItemIndex);
end;

procedure TFormPropSettings.CheckBox1Click(Sender: TObject);
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.put_TeletextShow(CheckBox1.Checked);
end;

procedure TFormPropSettings.SpinEdit1Change(Sender: TObject);
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.put_TeletextPage(SpinEdit1.Value, SpinEdit2.Value);
end;

procedure TFormPropSettings.SpeedButton3Click(Sender: TObject);
var
  selected: Integer;
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;
  FCanChange := False;
  FDVBFilter.put_NextChannel;
  FDVBFilter.get_ChannelSelected(selected);
  ComboBox1.ItemIndex := selected;
  UpdateAudioStreams;
  UpdateSubtitleStreams;
  FCanChange := True;
end;

procedure TFormPropSettings.SpeedButton4Click(Sender: TObject);
var
  selected: Integer;
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;
  FCanChange := False;
  FDVBFilter.put_PreviousChannel;
  FDVBFilter.get_ChannelSelected(selected);
  ComboBox1.ItemIndex := selected;
  UpdateAudioStreams;
  UpdateSubtitleStreams;
  FCanChange := True;
end;

procedure TFormPropSettings.CheckBox2Click(Sender: TObject);
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.put_TeletextTransparent(CheckBox2.Checked);
end;

procedure TFormPropSettings.Button1Click(Sender: TObject);
var
  ext: String;
  dest: PWideChar;
begin
  if not Assigned(FDVBFilter)
    then Exit;

  if SaveDialog1.Execute then
  begin
    ext := UpperCase(ExtractFileExt(SaveDialog1.FileName));
    if Length(ext) > 0
      then Delete(ext, 1, 1);

    AMGetWideString(SaveDialog1.FileName, dest);

    if ext = 'VTX' then
    begin
      FDVBFilter.put_SaveTeletext(ttVTX, dest);
    end else
    if ext = 'TXT' then
    begin
      FDVBFilter.put_SaveTeletext(ttTXT, dest);
    end else
      FDVBFilter.put_SaveTeletext(ttBitmap, dest);
  end;
end;

procedure TFormPropSettings.SpeedButton5Click(Sender: TObject);
begin
  FCanChange := False;
  UpdateAudioStreams;
  FCanChange := True;
end;

procedure TFormPropSettings.UpdateSubtitleStreams;
var
  Channels: Integer;
  i: Integer;
  Name: PChar;
begin
  ComboBox3.Clear;
  if Assigned(FDVBFilter) then
  begin
    Channels := 0;
    FDVBFilter.get_SubtitleStreamCount(Channels);
    ComboBox3.Items.Add('No Subtitles');
    for i := 0 to Channels -1 do
    begin
      FDVBFilter.get_SubtitleStreamInfo(i, Name);
      ComboBox3.Items.Add((Name));
      CoTaskMemFree(Name);
    end;
    FDVBFilter.get_SubtitleStreamSelected(channels);
    ComboBox3.ItemIndex := channels+1;
  end;
end;

procedure TFormPropSettings.SpeedButton6Click(Sender: TObject);
begin
  FCanChange := False;
  UpdateSubtitleStreams;
  FCanChange := True;
end;

procedure TFormPropSettings.ComboBox3Change(Sender: TObject);
var
  selected: Integer;
begin
  if not FCanChange or not Assigned(FDVBFilter)
    then Exit;

  FDVBFilter.get_SubtitleStreamSelected(selected);

  if (ComboBox3.ItemIndex = 0) and (selected >= 0) then
  begin
    FDVBFilter.put_SubtitleStreamSelected(selected);
  end else
  if (ComboBox3.ItemIndex - 1 = selected) then
  begin

  end else
  begin
    FDVBFilter.put_SubtitleStreamSelected(ComboBox3.ItemIndex - 1);
  end;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropSettings, CLSID_PropertyPageSettings);

end.
