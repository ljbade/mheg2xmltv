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

unit formAddChannel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls, DVBChannelList, Menus, MPEGConst,
  ISO639LanguageCode, DVBConst, BDAUtils;

type
  TfrmAddChannel = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    SpinEdit3: TSpinEdit;
    Label6: TLabel;
    SpinEdit4: TSpinEdit;
    Label7: TLabel;
    SpinEdit5: TSpinEdit;
    Label8: TLabel;
    GroupBox3: TGroupBox;
    ListView1: TListView;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button3: TButton;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Button9: TButton;
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChannel: TDVBChannel;
    procedure AddItem(AName, AStreamType, APID, ADefault: WideString);
    procedure EnableButtons;
  public
    constructor Create(AOwner: TComponent; AChannel: TDVBChannel); reintroduce;
  end;

var
  frmAddChannel: TfrmAddChannel;

implementation

uses formAddStream, formNetwork;

{$R *.dfm}

constructor TfrmAddChannel.Create(AOwner: TComponent; AChannel: TDVBChannel);
var
  i: Integer;
  s: String;
  f: String;
begin
  inherited Create(AOwner);
  FChannel := AChannel;

  Edit1.Text :=  RemoveUnwantedChars(FChannel.Name);
  Edit2.Text := RemoveUnwantedChars(FChannel.Provider);

  SpinEdit1.Value := FChannel.ServiceType;
  SpinEdit3.Value := FChannel.SID;
  SpinEdit4.Value := FChannel.ProgramMapPID;
  SpinEdit5.Value := FChannel.PCRPID;

  for i := 0 to FChannel.Streams.Count -1 do
  begin
    with FChannel.Streams[i] do
    begin
      if Default then s := 'yes' else s := 'no';
      f := GetDVBChannelStreamTypeString(StreamType);
      if StreamType = stAudio then
      begin
        case TDVBAudioStream(FChannel.Streams[i]).Coding of
          acUnknown:  f := f + ' (UNK)';
          acMP1:      f := f + ' (MP1)';
          acMP2:      f := f + ' (MP2)';
          acAC3:      f := f + ' (AC3)';
          acAAC:      f := f + ' (AAC)';
          else        f := f + ' (UNK)';
        end;
      end;
      if StreamType = stVideo then
      begin
        case TDVBVideoStream(FChannel.Streams[i]).Coding of
          vcUnknown:  f := f + ' (UNK)';
          vcMPEG:     f := f + ' (MPEG)';
          vcH264:     f := f + ' (H264)';
          else        f := f + ' (UNK)';
        end;
      end;
      AddItem(Name, f, inttostr(PID), s);
    end;
  end;
end;

procedure TfrmAddChannel.Button4Click(Sender: TObject);
var
  form: TfrmAddStream;
  stream: TDVBBaseStream;
  s, f: String;
begin
  form := TfrmAddStream.Create(Self);
  form.Icon := Icon;
  form.ShowModal;
  if form.ModalResult = mrCancel then
  begin
    form.Free;
    Exit;
  end;
  case TDVBChannelStreamType(form.ComboBox1.ItemIndex) of
    stAudio:    stream := TDVBAudioStream.Create;
    stTeletext: stream := TDVBTeletextStream.Create;
    stSubtitle: stream := TDVBSubtitleStream.Create;
    stVideo:    stream := TDVBVideoStream.Create;
    else        stream := TDVBBaseStream.Create;
  end;
  stream.Name := form.Edit1.Text;
  stream.StreamType := TDVBChannelStreamType(form.ComboBox1.ItemIndex);
  stream.Default := form.CheckBox1.Checked;
  stream.PID := form.SpinEdit1.Value;
  stream.Tag := form.SpinEdit2.Value;

  case stream.StreamType of
    stAudio:
    begin
      with TDVBAudioStream(stream) do
      begin
        Coding := TDVBAudioStreamCoding(form.ComboBox4.ItemIndex);
        AudioType := TISO639LanguageDescriptorAudioType(form.ComboBox5.ItemIndex);
        if form.ComboBox3.ItemIndex > 0 then
        begin
          Language := ISO_639_2_LANGUAGES[form.ComboBox3.ItemIndex].Code;
        end;
      end;
    end;
    stTeletext:
    begin
      with TDVBTeletextStream(stream) do
      begin
        if form.ComboBox2.ItemIndex > 0 then
        begin
          Language := ISO_639_2_LANGUAGES[form.ComboBox2.ItemIndex].Code;
        end;
      end;
    end;
    stVideo:
    begin
      with TDVBVideoStream(stream) do
      begin
        Coding := TDVBVideoStreamCoding(form.ComboBox11.ItemIndex);
      end;
    end;
  end;

  FChannel.Streams.Add(stream);

  if stream.Default then s := 'yes' else s := 'no';
  with stream do
  begin
    if Default then s := 'yes' else s := 'no';
    f := GetDVBChannelStreamTypeString(StreamType);
    if StreamType = stAudio then
    begin
      case TDVBAudioStream(stream).Coding of
        acUnknown:  f := f + ' (UNK)';
        acMP1:      f := f + ' (MP1)';
        acMP2:      f := f + ' (MP2)';
        acAC3:      f := f + ' (AC3)';
        acAAC:      f := f + ' (AAC)';
        else        f := f + ' (UNK)';
      end;
    end else
    if StreamType = stVideo then
    begin
      case TDVBVideoStream(stream).Coding of
        vcUnknown:  f := f + ' (UNK)';
        vcMPEG:     f := f + ' (MPEG)';
        vcH264:     f := f + ' (H264)';
        else        f := f + ' (UNK)';
      end;
    end;
    AddItem(stream.Name, f, inttostr(stream.PID), s);
  end;

  form.Free;
  EnableButtons;
end;

procedure TfrmAddChannel.Button1Click(Sender: TObject);
begin
  FChannel.Name := Edit1.Text;
  FChannel.Provider := Edit2.Text;

  FChannel.SID := SpinEdit3.Value;
  FChannel.PCRPID := SpinEdit5.Value;
  FChannel.ProgramMapPID := SpinEdit4.Value;
  FChannel.ServiceType := SpinEdit1.Value;
end;

procedure TfrmAddChannel.AddItem(AName, AStreamType, APID, ADefault: WideString);
var
  item: TListItem;
begin
  item := ListView1.Items.Add;
  item.Caption := AName;
  item.SubItems.Add(AStreamType);
  item.SubItems.Add(APID);
  item.SubItems.Add(ADefault);
end;

procedure TfrmAddChannel.Button8Click(Sender: TObject);
begin
  if ListView1.SelCount = 0
    then Exit;

  FChannel.Streams.Delete(ListView1.Selected.Index);
  ListView1.DeleteSelected;

  EnableButtons;
end;

procedure TfrmAddChannel.EnableButtons;
begin
  Button3.Enabled := ListView1.Items.Count > 0;
  Button5.Enabled := ListView1.SelCount > 0;
  Button8.Enabled := ListView1.SelCount > 0;
  Button7.Enabled := (ListView1.SelCount > 0) and (ListView1.Selected.Index < ListView1.Items.Count -1);
  Button6.Enabled := (ListView1.SelCount > 0) and (ListView1.Selected.Index > 0);
end;

procedure TfrmAddChannel.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  EnableButtons;
end;

procedure ExchangeItems(lv: TListView; const i, j: Integer);
var
  tempLI: TListItem;
begin
  lv.Items.BeginUpdate;
  try
    tempLI := TListItem.Create(lv.Items);
    tempLI.Assign(lv.Items.Item[i]);
    lv.Items.Item[i].Assign(lv.Items.Item[j]);
    lv.Items.Item[j].Assign(tempLI);
    tempLI.Free;
  finally
    lv.Items.EndUpdate
  end;
end;

procedure TfrmAddChannel.Button7Click(Sender: TObject);
var
  idx: Integer;
begin
  if ListView1.SelCount = 0
    then Exit;

  idx := ListView1.Selected.Index;

  ExchangeItems(ListView1, idx, idx+1);
  FChannel.Streams.Move(idx, idx+1);
  ListView1.Items[idx+1].Selected := True;
  EnableButtons;
end;

procedure TfrmAddChannel.Button6Click(Sender: TObject);
var
  idx: Integer;
begin
  if ListView1.SelCount = 0
    then Exit;

  idx := ListView1.Selected.Index;

  ExchangeItems(ListView1, idx, idx-1);
  ListView1.Items[idx-1].Selected := True;
  FChannel.Streams.Move(idx, idx-1);
  EnableButtons;
end;

procedure TfrmAddChannel.Button5Click(Sender: TObject);
var
  form: TfrmAddStream;
  stream, stream2: TDVBBaseStream;
  f: String;
  lang: TISO6392LanguageCode;
  sub: TDVBSubtitleStream;
  i: Integer;
begin
  form := TfrmAddStream.Create(Self);
  form.Icon := Icon;
  stream := FChannel.Streams[ListView1.Selected.Index];
  form.SetStream(stream);
  form.ShowModal;
  if form.ModalResult = mrCancel then
  begin
    form.Free;
    Exit;
  end;

  case TDVBChannelStreamType(form.ComboBox1.ItemIndex) of
    stAudio:    stream2 := TDVBAudioStream.Create;
    stTeletext: stream2 := TDVBTeletextStream.Create;
    stSubtitle: stream2 := TDVBSubtitleStream.Create;
    else        stream2 := TDVBBaseStream.Create;
  end;

  stream2.Name := form.Edit1.Text;
  stream2.StreamType := TDVBChannelStreamType(form.ComboBox1.ItemIndex);
  stream2.Default := form.CheckBox1.Checked;
  stream2.PID := form.SpinEdit1.Value;
  stream2.Tag := form.SpinEdit2.Value;
  sub := TDVBSubtitleStream(stream2);

  case stream2.StreamType of
    stAudio:
    begin
      with TDVBAudioStream(stream2) do
      begin
        Coding := TDVBAudioStreamCoding(form.ComboBox4.ItemIndex);
        AudioType := TISO639LanguageDescriptorAudioType(form.ComboBox5.ItemIndex);
        if form.ComboBox3.ItemIndex > 0 then
        begin
          Language := ISO_639_2_LANGUAGES[form.ComboBox3.ItemIndex].Code;
        end else
        begin
          f := form.ComboBox3.Text;
          SetLength(f, 3);
          Move(f[1], lang, 3);
          Language := lang;
        end;
      end;
    end;
    stVideo:
    begin
      with TDVBVideoStream(stream2) do
      begin
        Coding := TDVBVideoStreamCoding(form.ComboBox11.ItemIndex);
      end;
    end;
    stTeletext:
    begin
      with TDVBTeletextStream(stream2) do
      begin
        if form.ComboBox2.ItemIndex > 0 then
        begin
          Language := ISO_639_2_LANGUAGES[form.ComboBox2.ItemIndex].Code;
        end else
        begin
          f := form.ComboBox2.Text;
          SetLength(f, 3);
          Move(f[1], lang, 3);
          Language := lang;
        end;
      end;
    end;
    stSubtitle:
    begin
      with TDVBSubtitleStream(stream) do
      begin
        for i := 0 to SubtitlesCount -1 do
        begin
          sub.AddItem(Subtitle[i].LanguageCode, Subtitle[i].SubtitlingType, Subtitle[i].CompositionPageID, Subtitle[i].AncillaryPageID, Length(form.ListView1.Items[i].SubItems[3]) > 2);
        end;
      end;
    end;
  end;

  f := GetDVBChannelStreamTypeString(stream2.StreamType);
  if stream2.StreamType = stAudio then
  begin
    case TDVBAudioStream(stream2).Coding of
      acUnknown:  f := f + ' (UNK)';
      acMP1:      f := f + ' (MP1)';
      acMP2:      f := f + ' (MP2)';
      acAC3:      f := f + ' (AC3)';
      acAAC:      f := f + ' (AAC)';
      else        f := f + ' (UNK)';
    end;
  end else
  if stream2.StreamType = stVideo then
  begin
    case TDVBVideoStream(stream2).Coding of
      vcUnknown:  f := f + ' (UNK)';
      vcMPEG:     f := f + ' (MPEG)';
      vcH264:     f := f + ' (H264)';
      else        f := f + ' (UNK)';
    end;
  end;

  ListView1.Selected.Caption := stream2.Name;
  ListView1.Selected.SubItems.Strings[0] := f;
  ListView1.Selected.SubItems.Strings[1] := inttostr(stream2.PID);
  if stream2.Default
    then ListView1.Selected.SubItems.Strings[2] := 'yes'
    else ListView1.Selected.SubItems.Strings[2] := 'no';

  FChannel.Streams[ListView1.Selected.Index] := stream2;
  stream.Free;

  form.Free;
end;

procedure TfrmAddChannel.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TfrmAddChannel.Button3Click(Sender: TObject);
begin
  FChannel.Streams.Clear;
  ListView1.Clear;
end;

procedure TfrmAddChannel.Button9Click(Sender: TObject);
var
  form: TfrmNetwork;
begin
  form := TfrmNetwork.Create(Self);
  form.Icon := Icon;

  form.Edit1.Text := FChannel.Network.Name;
  form.SpinEdit4.Value := FChannel.Network.ONID;
  form.SpinEdit2.Value := FChannel.Network.TSID;
  form.ComboBox7.ItemIndex := Integer(FChannel.Network.Type_);
  form.SpinEdit1.Value := FChannel.Network.NetworkID;
  form.SpinEdit9.Value := FChannel.Network.Terrestrial.Priority;
  form.SpinEdit3.Value := FChannel.Network.Terrestrial.CentreFrequency;
  form.SpinEdit5.Value := FChannel.Network.Terrestrial.Bandwidth;
  form.SpinEdit6.Value := FChannel.Network.Terrestrial.MPEFECIndicator;
  form.ComboBox1.ItemIndex := Integer(FChannel.Network.Terrestrial.Constellation);
  form.ComboBox3.ItemIndex := Integer(FChannel.Network.Terrestrial.CodeRateHPStream);
  form.ComboBox4.ItemIndex := Integer(FChannel.Network.Terrestrial.CodeRateLPStream);
  form.ComboBox5.ItemIndex := Integer(FChannel.Network.Terrestrial.GuardInterval);
  form.ComboBox6.ItemIndex := Integer(FChannel.Network.Terrestrial.TransmissionMode);
  form.SpinEdit7.Value := FChannel.Network.Terrestrial.TimeSlicingIndicator;
  form.SpinEdit13.Value := FChannel.Network.Terrestrial.OtherFrequencyFlag;
  form.ComboBox2.ItemIndex := Integer(FChannel.Network.Terrestrial.HierarchyInformation);

  form.SpinEdit10.Value := FChannel.Network.Cable.Frequency;
  form.SpinEdit16.Value := FChannel.Network.Cable.SymbolRate;
  form.ComboBox11.ItemIndex := Integer(FChannel.Network.Cable.Modulation);
  form.ComboBox9.ItemIndex := Integer(FChannel.Network.Cable.FECOuter);
  form.ComboBox13.ItemIndex := Integer(FChannel.Network.Cable.FECInner);

  form.SpinEdit8.Value := FChannel.Network.Satellite.Frequency;
  form.SpinEdit14.Value := FChannel.Network.Satellite.WestEastFlag;
  form.SpinEdit11.Value := FChannel.Network.Satellite.OrbitalPosition;
  form.SpinEdit12.Value := FChannel.Network.Satellite.SymbolRate;
  form.ComboBox10.ItemIndex := Integer(FChannel.Network.Satellite.Modulation);
  form.ComboBox8.ItemIndex := Integer(FChannel.Network.Satellite.Polarization);
  form.ComboBox12.ItemIndex := Integer(FChannel.Network.Satellite.FECInner);

  form.ShowModal;
  if form.ModalResult = mrCancel then
  begin
    form.Free;
    Exit;
  end;

  FChannel.Network.Satellite.Frequency := form.SpinEdit8.Value;
  FChannel.Network.Satellite.WestEastFlag := form.SpinEdit14.Value;
  FChannel.Network.Satellite.OrbitalPosition := form.SpinEdit11.Value;
  FChannel.Network.Satellite.SymbolRate := form.SpinEdit12.Value;
  FChannel.Network.Satellite.Modulation := TSatelliteModulation(form.ComboBox10.ItemIndex);
  FChannel.Network.Satellite.Polarization := TSatellitePolarization(form.ComboBox8.ItemIndex);
  FChannel.Network.Satellite.FECInner := TFECInner(form.ComboBox12.ItemIndex);

  FChannel.Network.Cable.Frequency := form.SpinEdit10.Value;
  FChannel.Network.Cable.SymbolRate := form.SpinEdit16.Value;
  FChannel.Network.Cable.Modulation := TCableModulation(form.ComboBox11.ItemIndex);
  FChannel.Network.Cable.FECOuter := TFECOuter(form.ComboBox9.ItemIndex);
  FChannel.Network.Cable.FECInner := TFECInner(form.ComboBox13.ItemIndex);

  FChannel.Network.Name := form.Edit1.Text;
  FChannel.Network.ONID := form.SpinEdit4.Value;
  FChannel.Network.TSID := form.SpinEdit2.Value;
  FChannel.Network.Type_ := TDVBNetworkType(form.ComboBox7.ItemIndex);
  FChannel.Network.NetworkID := form.SpinEdit1.Value;
  FChannel.Network.Terrestrial.Priority := form.SpinEdit9.Value;
  FChannel.Network.Terrestrial.CentreFrequency := form.SpinEdit3.Value;
  FChannel.Network.Terrestrial.Bandwidth := form.SpinEdit5.Value;
  FChannel.Network.Terrestrial.MPEFECIndicator := form.SpinEdit6.Value;
  FChannel.Network.Terrestrial.Constellation := TTerrestrialConstellation(form.ComboBox1.ItemIndex);
  FChannel.Network.Terrestrial.CodeRateHPStream := TTerrestrialCodeRate(form.ComboBox3.ItemIndex);
  FChannel.Network.Terrestrial.CodeRateLPStream := TTerrestrialCodeRate(form.ComboBox4.ItemIndex);
  FChannel.Network.Terrestrial.GuardInterval := TTerrestrialGuardInterval(form.ComboBox5.ItemIndex);
  FChannel.Network.Terrestrial.TransmissionMode := TTerrestrialTransmissionMode(form.ComboBox6.ItemIndex);
  FChannel.Network.Terrestrial.TimeSlicingIndicator := form.SpinEdit7.Value;
  FChannel.Network.Terrestrial.OtherFrequencyFlag := form.SpinEdit13.Value;
  FChannel.Network.Terrestrial.HierarchyInformation := TTerrestrialHierarchyInformation(form.ComboBox2.ItemIndex);

  form.Free;
end;

procedure TfrmAddChannel.ListView1DblClick(Sender: TObject);
begin
  if ListView1.SelCount = 0
    then Exit;
  Button5Click(Self);
end;

procedure TfrmAddChannel.FormCreate(Sender: TObject);
begin
  BorderIcons := [biSystemMenu];
end;

end.
