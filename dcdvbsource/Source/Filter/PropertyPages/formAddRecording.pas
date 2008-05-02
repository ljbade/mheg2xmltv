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

unit formAddRecording;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, Calendar, Spin, XPMan, ComCtrls, StdCtrls, BDAUtils,
  Buttons;

type
  TfrmAddRecording = class(TForm)
    TabControl1: TTabControl;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    MonthCalendar1: TMonthCalendar;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    MonthCalendar2: TMonthCalendar;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Button1: TButton;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    SpeedButton1: TSpeedButton;
    Edit1: TEdit;
    GroupBox4: TGroupBox;
    Edit2: TEdit;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FIsOK: Boolean;
  public
    procedure GetTime(out StartTime: TDateTime; out EndTime: TDateTime);
    procedure SetTime(StartTime: TDateTime; EndTime: TDateTime);
  published
    property IsValid: Boolean read FIsOK;
  end;

implementation

uses DateUtils;

{$R *.dfm}

procedure TfrmAddRecording.FormCreate(Sender: TObject);
var
  n: TDateTime;
begin
  FIsOK := False;
  n := Now;
  SetTime(n, IncMinute(n, 60));
end;

procedure TfrmAddRecording.SetTime(StartTime: TDateTime; EndTime: TDateTime);
begin
  MonthCalendar1.Date := EncodeDateTime(YearOf(StartTime), MonthOf(StartTime), DayOf(StartTime), 0, 0, 0, 0);
  SpinEdit1.Value := HourOfTheDay(StartTime);
  SpinEdit2.Value := MinuteOfTheHour(StartTime);
  MonthCalendar2.Date := EncodeDateTime(YearOf(EndTime), MonthOf(EndTime), DayOf(EndTime), 0, 0, 0, 0);
  SpinEdit3.Value := HourOfTheDay(EndTime);
  SpinEdit4.Value := MinuteOfTheHour(EndTime);
end;

procedure TfrmAddRecording.ComboBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DrawComboBox(Control as TComboBox, Index, Rect, State);
end;

procedure TfrmAddRecording.GetTime(out StartTime: TDateTime; out EndTime: TDateTime);
begin
  StartTime := IncHour(IncMinute(EncodeDateTime(YearOf(MonthCalendar1.Date), MonthOf(MonthCalendar1.Date), DayOf(MonthCalendar1.Date), 0, 0, 0, 0), SpinEdit2.Value), SpinEdit1.Value);
  EndTime := IncHour(IncMinute(EncodeDateTime(YearOf(MonthCalendar2.Date), MonthOf(MonthCalendar2.Date), DayOf(MonthCalendar2.Date), 0, 0, 0, 0), SpinEdit4.Value), SpinEdit3.Value);
end;

procedure TfrmAddRecording.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute
    then Edit1.Text := SaveDialog1.FileName;
end;

procedure ShowError(ACaption: String; AOwner: TForm);
var
  p: TPoint;
begin
  p.X := 0;
  p.Y := 0;
  Windows.ClientToScreen(AOwner.Handle, p);
  with CreateMessageDialog(ACaption, mtError, [mbOK]) do
  begin
    Left := p.X + ((AOwner.Width - Width) div 2);
    Top := p.Y + ((AOwner.Height - Height) div 2);
    ShowModal;
    Free;
  end;
end;

procedure TfrmAddRecording.Button3Click(Sender: TObject);
var
  b, e: TDateTime;
begin
  if Edit2.Text = '' then
  begin
    ShowError('Please Enter a valid Name', Self);
    Edit2.SetFocus;
    Exit;
  end;

  if Edit1.Text = '' then
  begin
    ShowError('Please choose a valid Filename', Self);
    Edit1.SetFocus;
    Exit;
  end;

  GetTime(b, e);

  if b >= e then
  begin
    ShowError('Start Date cannot be Larger than End Date', Self);
    Exit;
  end;

  if e < Now then
  begin
    ShowError('End Date cannot in the Past', Self);
    Exit;
  end;

  if ComboBox1.ItemIndex < 0 then
  begin
    ShowError('Please Select a Channel to Record', Self);
    ComboBox1.SetFocus;
    Exit;
  end;

  FIsOK := True;
  Close;
end;

procedure TfrmAddRecording.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
