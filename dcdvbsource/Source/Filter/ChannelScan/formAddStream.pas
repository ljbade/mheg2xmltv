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

unit formAddStream;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, DVBChannelList, ISO639LanguageCode, ComCtrls,
  Menus;

type
  TfrmAddStream = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    ComboBox2: TComboBox;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    ComboBox3: TComboBox;
    Label6: TLabel;
    ComboBox4: TComboBox;
    Label7: TLabel;
    ComboBox5: TComboBox;
    Label8: TLabel;
    SpinEdit2: TSpinEdit;
    GroupBox5: TGroupBox;
    ListView1: TListView;
    GroupBox6: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    GroupBox7: TGroupBox;
    Label13: TLabel;
    ComboBox11: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  public
    procedure SetStream(AStream: TDVBBaseStream);
  end;

var
  frmAddStream: TfrmAddStream;

implementation

uses MPEGConst, StrUtils;

{$R *.dfm}

procedure TfrmAddStream.SetStream(AStream: TDVBBaseStream);
var
  i, c: Integer;
  item: TListItem;
begin
  Edit1.Text := AStream.Name;
  ComboBox1.ItemIndex := Integer(AStream.StreamType);
  CheckBox1.Checked := AStream.Default;
  SpinEdit1.Value := AStream.PID;
  SpinEdit2.Value := AStream.Tag;
  case AStream.StreamType of
    stAudio:
    begin
      with TDVBAudioStream(AStream) do
      begin
        ComboBox4.ItemIndex := Integer(Coding);
        ComboBox5.ItemIndex := Integer(AudioType);
        for i := 0 to ISO_639_2_LANGUAGES_COUNT -1 do
        begin
          if LowerCase(String(Language)) = LowerCase(String(ISO_639_2_LANGUAGES[i].Code)) then
          begin
            ComboBox3.ItemIndex := i;
            break;
          end;
        end;
        if ComboBox3.ItemIndex < 0
          then ComboBox3.Text := String(Language);
      end;
      ComboBox1.ItemIndex := 2;
      ComboBox1Change(Self);
    end;
    stVideo:
    begin
      with TDVBVideoStream(AStream) do
      begin
        ComboBox11.ItemIndex := Integer(Coding);
      end;
      ComboBox1.ItemIndex := 1;
      ComboBox1Change(Self);
    end;
    stTeletext:
    begin
      with TDVBTeletextStream(AStream) do
      begin
        for i := 0 to ISO_639_2_LANGUAGES_COUNT -1 do
        begin
          if LowerCase(String(Language)) = LowerCase(String(ISO_639_2_LANGUAGES[i].Code)) then
          begin
            ComboBox2.ItemIndex := i;
            break;
          end;
        end;
      end;
      ComboBox1.ItemIndex := 3;
      ComboBox1Change(Self);
    end;
    stSubtitle:
    begin
      with TDVBSubtitleStream(AStream) do
      begin
        for i := 0 to SubtitlesCount -1 do
        begin
          item := ListView1.Items.Add;
          item.Caption := String(Subtitle[i].LanguageCode);
          item.SubItems.Add(inttostr(Subtitle[i].SubtitlingType));
          item.SubItems.Add(inttostr(Subtitle[i].CompositionPageID));
          item.SubItems.Add(inttostr(Subtitle[i].AncillaryPageID));
          item.SubItems.Add(IfThen(Subtitle[i].DefaultSub, 'Yes', 'No'));

          for c := 0 to ISO_639_2_LANGUAGES_COUNT -1 do
          begin
            if LowerCase(String(Subtitle[i].LanguageCode)) = LowerCase(String(ISO_639_2_LANGUAGES[c].Code)) then
            begin
              item.Caption := ISO_639_2_LANGUAGES[c].Name;
              break;
            end;
          end;
        end;
      end;
      ComboBox1.ItemIndex := 7;
      ComboBox1Change(Self);
    end;
  end;
end;

procedure TfrmAddStream.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
  ComboBox1Change(Self);
end;

procedure TfrmAddStream.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  BorderIcons := [biSystemMenu];
  for i := 0 to ISO_639_2_LANGUAGES_COUNT -1 do
  begin
    ComboBox2.Items.Add(ISO_639_2_LANGUAGES[i].Name);
    ComboBox3.Items.Add(ISO_639_2_LANGUAGES[i].Name);
  end;
end;

procedure TfrmAddStream.ComboBox1Change(Sender: TObject);
begin
  GroupBox3.Visible := False;
  GroupBox4.Visible := False;
  GroupBox7.Visible := False;
  GroupBox5.Visible := False;

  if ComboBox1.ItemIndex = 1 then
  begin
    ClientHeight := 313;
    GroupBox7.Visible := True;
  end else
  if ComboBox1.ItemIndex = 3 then
  begin
    ClientHeight := 305;
    GroupBox3.Visible := True;
  end else
  if ComboBox1.ItemIndex = 2 then
  begin
    ClientHeight := 329;
    GroupBox4.Visible := True;
  end else
  if ComboBox1.ItemIndex = 7 then
  begin
    ClientHeight := 379;
    GroupBox5.Visible := True;
  end else
  begin
    ClientHeight := 233;
  end;

  CheckBox1.Visible := ComboBox1.ItemIndex <> 7;
end;

procedure TfrmAddStream.ListView1DblClick(Sender: TObject);
begin
  if ListView1.Selected = nil
    then Exit;

  if Length(ListView1.Selected.SubItems[3]) > 2
    then ListView1.Selected.SubItems[3] := 'No'
    else ListView1.Selected.SubItems[3] := 'Yes';
end;

end.
