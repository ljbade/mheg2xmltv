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

unit formScan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, XPMan, DSUtil, DirectShow9, Menus, ComCtrls,
  Spin, Buttons, formDebug, ActiveX, JVSimpleXml, DVBChannelList, ShellAPI,
  AppEvnts, BDAConst, BDAUtils, Logger, MPEGConst, ISO639LanguageCode,
  DVBFrequencyList, DVBScanner, MulticastClientServer, DCDVBTuningPluginTransportStream,
  BDATuning, TCPClientServer, DVBConst, formFrequency, DCDVBPluginManager,
  formPropertyPage, formOSDColor, DCDVBTuningPlugins;

type
  TfrmScan = class;

  TScanningThread = class(TThread)
  private
    FOwner: TfrmScan;
    FFrequencies: TDVBFrequencys;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TfrmScan);
    destructor Destroy; override;
  end;

  TfrmScan = class(TForm)
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Edit1: TEdit;
    SpinEdit7: TSpinEdit;
    TabSheet4: TTabSheet;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    ComboBox3: TComboBox;
    TabSheet3: TTabSheet;
    ListView1: TListView;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button1: TButton;
    TabSheet2: TTabSheet;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N3: TMenuItem;
    Reset1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ComboBox2: TComboBox;
    Label8: TLabel;
    ComboBox6: TComboBox;
    Label9: TLabel;
    DebugLog1: TMenuItem;
    SpeedButton2: TSpeedButton;
    OpenDialog3: TOpenDialog;
    ListView2: TListView;
    ListView3: TListView;
    ListView4: TListView;
    ListView5: TListView;
    Button2: TButton;
    Button3: TButton;
    Button9: TButton;
    Button11: TButton;
    Button12: TButton;
    Button10: TButton;
    Button13: TButton;
    N2: TMenuItem;
    TabControl1: TTabControl;
    TabSheet5: TTabSheet;
    GroupBox3: TGroupBox;
    SpeedButton1: TSpeedButton;
    Label10: TLabel;
    ComboBox7: TComboBox;
    RadioButton3: TRadioButton;
    Edit3: TEdit;
    SpinEdit11: TSpinEdit;
    Edit4: TEdit;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    ComboBox8: TComboBox;
    Button14: TButton;
    ProgressBar1: TProgressBar;
    Label24: TLabel;
    Label26: TLabel;
    ComboBox9: TComboBox;
    ComboBox11: TComboBox;
    RichEdit1: TRichEdit;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Debug1: TMenuItem;
    ScanfromPSIDump1: TMenuItem;
    OpenDialog4: TOpenDialog;
    ApplicationEvents1: TApplicationEvents;
    GroupBox9: TGroupBox;
    Label6: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    CheckBox6: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    CheckBox13: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox8: TCheckBox;
    Label1: TLabel;
    ComboBox5: TComboBox;
    SpeedButton4: TSpeedButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    GroupBox5: TGroupBox;
    CheckBox4: TCheckBox;
    Edit2: TEdit;
    SpinEdit6: TSpinEdit;
    ComboBox10: TComboBox;
    PrintEIT1: TMenuItem;
    PrintNIT1: TMenuItem;
    PrintSDT1: TMenuItem;
    PrintPMT1: TMenuItem;
    PrintPAT1: TMenuItem;
    N5: TMenuItem;
    PrintCAT1: TMenuItem;
    PrintBAT1: TMenuItem;
    N4: TMenuItem;
    PrintAll1: TMenuItem;
    PrintNone1: TMenuItem;
    Label11: TLabel;
    SpeedButton5: TSpeedButton;
    ComboBox12: TComboBox;
    TabSheet6: TTabSheet;
    GroupBox4: TGroupBox;
    Label29: TLabel;
    Label18: TLabel;
    Label27: TLabel;
    Label23: TLabel;
    SpinEdit15: TSpinEdit;
    SpinEdit10: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    GroupBox6: TGroupBox;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    SpinEdit19: TSpinEdit;
    SpinEdit9: TSpinEdit;
    SpinEdit5: TSpinEdit;
    GroupBox7: TGroupBox;
    Label28: TLabel;
    Label33: TLabel;
    CheckBox7: TCheckBox;
    SpinEdit8: TSpinEdit;
    Button18: TButton;
    GroupBox8: TGroupBox;
    Label2: TLabel;
    CheckBox5: TCheckBox;
    SpinEdit12: TSpinEdit;
    GroupBox10: TGroupBox;
    Label5: TLabel;
    SpeedButton3: TSpeedButton;
    ComboBox4: TComboBox;
    Label12: TLabel;
    SpeedButton6: TSpeedButton;
    ComboBox13: TComboBox;
    Label13: TLabel;
    SpeedButton7: TSpeedButton;
    ComboBox14: TComboBox;
    Label14: TLabel;
    CheckBox9: TCheckBox;
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure PrintNone1Click(Sender: TObject);
    procedure PrintAll1Click(Sender: TObject);
    procedure PrintEIT1Click(Sender: TObject);
    procedure PrintBAT1Click(Sender: TObject);
    procedure PrintCAT1Click(Sender: TObject);
    procedure PrintSDT1Click(Sender: TObject);
    procedure PrintNIT1Click(Sender: TObject);
    procedure PrintPMT1Click(Sender: TObject);
    procedure PrintPAT1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DebugLog1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DblClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckBox9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label10Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ScanfromPSIDump1Click(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure ListView4Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListView4DblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
  private
    FAbort: Boolean;
    FDevices: TDCDVBTPluginList;
    FDebug: TfrmDebug;
    FLog: TCallbackLogger;
    FVideoGuids: TStringList;
    FAudioGuids: TStringList;
    FAACGuids: TStringList;
    FAC3Guids: TStringList;
    FH264Guids: TStringList;
    FPPGuids: TStringList;
    FChannelList: TDVBChannelsList;
    FFrequencys: TDVBFrequencyList;
    FIsScanning: Boolean;
    FMulticastClient: TMulticastClient;
    FScanner: TDVBScanner;
    FDCDVBScanner: TDCDVBTuningPluginTransportStream;
    FThread: TScanningThread;
    FTCPClient: TTCPClientSocket;
    FWriteData: Boolean;
    FOSDColor: TOSDColor;
    procedure SaveToFile(AFile: WideString);
    procedure LoadFromFile(AFile: WideString);
    procedure EnableButtons;
    procedure EnableButtons2;
    procedure UpdateChannelNumber;
    procedure OnChannelFound(AChannel: TDVBChannel);
    procedure OnMulticastData(ABuffer: PByte; ASize: Integer);
    procedure OnBDAData(ABuffer: PByte; ASize: Integer);
    procedure CheckCountryButton;
    procedure AddText(AText: String);
    procedure AddBoldText(AText, AText2:  String);
    procedure AddPoint;
    procedure ReplaceText(AText: String);
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WndProc(var Message: TMessage); override;
  public
  end;

var
  frmScan: TfrmScan;

implementation

uses formAddChannel, formAbout, StrUtils, formLNBSelection, DVBDataParser;

{$R *.dfm}

const
  UM_BDA          = WM_USER + $11234;

  PUT_POSITION    = 1;
  SCANNING_ENDED  = 2;
  FOUND_CHANNEL   = 3;
  ADD_EMPTY       = 4;
  ADD_TEXT        = 5;
  ADD_POINT       = 6;
  REPLACE_TEXT    = 7;

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

function GetCurrentDirectory: String;
var
  buffer: array[0..255] of Char;
begin
  GetModuleFileName(HInstance, @buffer, 256);
  Result := ExtractFilePath(String(buffer));
end;

(*** TScanningThread **********************************************************)

constructor TScanningThread.Create(AOwner: TfrmScan);
begin
  inherited Create(True);
  FOwner := AOwner;
  FFrequencies := TDVBFrequencys.Create;
  FreeOnTerminate := True;
end;

destructor TScanningThread.Destroy;
begin
  if (FFrequencies <> nil) then
  begin
    FFrequencies.Free;
    FFrequencies := nil;
  end;

  inherited Destroy;
end;

function GetString(AString: String): PChar;
begin
  Result := AllocMem(Length(AString) + 1);
  Move(AString[1], Result^, Length(AString) + 1);
end;

procedure TScanningThread.Execute;
var
  i, c: Integer;
  t: Cardinal;
  received: Boolean;
  chan: TDVBChannel;
  freq: Int64;
  dvbt_tunerequest: TDVBTTuneRequest;
  dvbs_tunerequest: TDVBSTuneRequest;
  dvbc_tunerequest: TDVBCTuneRequest;
  atsc_tunerequest: TATSCTuneRequest;
  tune_request: Pointer;
  a1, a2: Integer;
  b1, b2: LongBool;
  channels_added: Integer;
begin
  if Terminated
    then Exit;

  dvbt_tunerequest.Size := SizeOf(TDVBTTuneRequest);
  dvbs_tunerequest.Size := SizeOf(TDVBSTuneRequest);
  dvbc_tunerequest.Size := SizeOf(TDVBCTuneRequest);
  atsc_tunerequest.Size := SizeOf(TATSCTuneRequest);

  dvbt_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT;
  dvbs_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS;
  dvbc_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC;
  atsc_tunerequest.DeviceType := DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC;

  channels_added := 0;
  
  FOwner.FScanner.DataParser.WriteData := False;
  try
    for i := 0 to FFrequencies.Count -1 do
    begin
      channels_added := 0;

      FOwner.FScanner.DataParser.WriteData := False;
      if Terminated then Exit;
      PostMessage(FOwner.Handle, UM_BDA, PUT_POSITION, i+1);
      tune_request := nil;
      freq:= 0;

      case FFrequencies[i].NetworkType of
        ntDVBT:
        begin
          tune_request := @dvbt_tunerequest;
          with dvbt_tunerequest do
          begin
            Frequency := FFrequencies[i].Frequency;
            Bandwidth := FFrequencies[i].Bandwidth;
            freq := Frequency;
          end;
        end;
        ntDVBC:
        begin
          tune_request := @dvbc_tunerequest;
          with dvbc_tunerequest do
          begin
            Frequency := FFrequencies[i].Frequency;
            SymbolRate := FFrequencies[i].Symbolrate;
            freq := Frequency;
          end;
        end;
        ntDVBS:
        begin
          tune_request := @dvbs_tunerequest;
          with dvbs_tunerequest do
          begin
            Frequency := FFrequencies[i].Frequency;
            SymbolRate := FFrequencies[i].Symbolrate;
            case FFrequencies[i].Polarization of
              0: Polarization := DVBS_POLARIZATION_HORIZONTAL;
              1: Polarization := DVBS_POLARIZATION_VERTICAL;
            end;
            freq := Frequency;
          end;
        end;
        ntATSC:
        begin
          tune_request := @atsc_tunerequest;
          with atsc_tunerequest do
          begin
            Frequency := FFrequencies[i].Frequency;
            freq := Frequency;
          end;
        end;
      end;

      FOwner.FScanner.DataParser.WritePath := GetCurrentDirectory + inttostr(freq);
      PostMessage(FOwner.Handle, UM_BDA, ADD_TEXT, integer(GetString('Tuning to ' + inttostr(freq) + ' ')));

      if Assigned(tune_request) then
      begin
        FOwner.FDCDVBScanner.TuneIn(tune_request);
      end;

      FOwner.FScanner.Clear;
      received := False;

      t := GetTickCount + 2000;
      while (t > GetTickCount) do
      begin
        PostMessage(FOwner.Handle, UM_BDA, ADD_POINT, 0);
        if Terminated then Exit;
        sleep(40);

        if FOwner.FDCDVBScanner.GetSignalStatistic(a1, a2, b1, b2) and b2 then
        begin
          received := True;
          break;
        end;
      end;

      if not received then
      begin
        PostMessage(FOwner.Handle, UM_BDA, REPLACE_TEXT, integer(GetString('Tuning to ' + inttostr(freq) + ' ................................................ no Signallock')));
        Continue;
      end;

      FOwner.FScanner.DataParser.WriteData := FOwner.FWriteData;

      // Clear again, just to be sure
      FOwner.FScanner.Clear;
      PostMessage(FOwner.Handle, UM_BDA, REPLACE_TEXT, integer(GetString('Tuning to ' + inttostr(freq) + ' ... Signal locked. Waiting for Services (max. 1 Minute)')));
      t := GetTickCount + 60000;
      while (t > GetTickCount) do
      begin
        if Terminated then Exit;
        sleep(100);
        if FOwner.FScanner.DataParser.Valid then
        begin
          FOwner.FScanner.DataParser.SetManual(FFrequencies[i]);
          if FOwner.FScanner.DataParser.ChannelList.Count > 0 then
          begin
            PostMessage(FOwner.Handle, UM_BDA, ADD_TEXT, integer(GetString(' ')));
          end;
          for c := 0 to FOwner.FScanner.DataParser.ChannelList.Count -1 do
          begin
            chan := TDVBChannel.Create;
            chan.Assign(FOwner.FScanner.DataParser.ChannelList[c]);

            if chan.Network.Type_ = ntUnknown then
            begin
              case FFrequencies[i].NetworkType of
                ntDVBT: chan.Network.Terrestrial.CentreFrequency := dvbt_tunerequest.Frequency;
                ntDVBC: chan.Network.Cable.Frequency := dvbc_tunerequest.Frequency;
                ntDVBS: chan.Network.Satellite.Frequency := dvbs_tunerequest.Frequency;
                ntATSC: chan.Network.ATSC.CentreFrequency := atsc_tunerequest.Frequency;
              end;
            end;

            PostMessage(FOwner.Handle, UM_BDA, FOUND_CHANNEL, Integer(chan));
            channels_added := 1;
          end;
          if FOwner.FScanner.DataParser.ChannelList.Count > 0 then
          begin
            PostMessage(FOwner.Handle, UM_BDA, ADD_EMPTY, 0);
          end;
          break;
        end;
      end;

      // Add Channels if DataParser.Valid == False
      if not FOwner.FScanner.DataParser.Valid then
      begin
        FOwner.FScanner.DataParser.SetManual(FFrequencies[i]);
        if FOwner.FScanner.DataParser.ChannelList.Count > 0 then
        begin
          PostMessage(FOwner.Handle, UM_BDA, ADD_TEXT, integer(GetString(' ')));
        end;
        for c := 0 to FOwner.FScanner.DataParser.ChannelList.Count -1 do
        begin
          chan := TDVBChannel.Create;
          chan.Assign(FOwner.FScanner.DataParser.ChannelList[c]);
          PostMessage(FOwner.Handle, UM_BDA, FOUND_CHANNEL, Integer(chan));
        end;
        if FOwner.FScanner.DataParser.ChannelList.Count > 0 then
        begin
          PostMessage(FOwner.Handle, UM_BDA, ADD_EMPTY, 0);
        end;
      end;
    end;
  finally
    FOwner.FScanner.DataParser.WriteData := False;
//    FOwner.FDCDVBScanner.Active := False;
    if not Terminated
      then PostMessage(FOwner.Handle, UM_BDA, SCANNING_ENDED, channels_added);
  end;
end;

(*** TfrmScan *****************************************************************)

procedure TfrmScan.WndProc(var Message: TMessage);
var
  p: PChar;
begin
  if Message.Msg = UM_BDA then
  begin
    case Message.WParam of
      PUT_POSITION:
      begin
        ProgressBar1.Position := Message.LParam;
      end;
      SCANNING_ENDED:
      begin
        Button14.Caption := 'Start Scanning';
        ProgressBar1.Position := 0;
        if Message.LParam = 0
          then AddText(' ');
        AddText('Scanning finished');
        FIsScanning := False;
        FAbort := False;
        FDCDVBScanner.Active := False;
      end;
      FOUND_CHANNEL:
      begin
        OnChannelFound(TDVBChannel(Message.LParam));
        AddBoldText('Found', RemoveUnwantedChars(TDVBChannel(Message.LParam).Name));
        TDVBChannel(Message.LParam).Free;
      end;
      ADD_EMPTY:
      begin
        AddText(' ');
      end;
      ADD_TEXT:
      begin
        p := PChar(Message.LParam);
        AddText(p);
        FreeMem(p);
      end;
      ADD_POINT:
      begin
        AddPoint;
      end;
      REPLACE_TEXT:
      begin
        p := PChar(Message.LParam);
        ReplaceText(p);
        FreeMem(p);
      end;
    end;
    Message.Result := 0;
    Exit;
  end;

  inherited WndProc(Message);
end;

procedure TfrmScan.WMDropFiles(var Message: TWMDropFiles);
var
  fc: Integer;
  res: Integer;
  buffer: array[0..MAX_PATH -1] of char;
begin
  res := -1;
  fc := DragQueryFile(Message.Drop, res, nil, 0);
  if fc > 0 then
  begin
    DragQueryFile(Message.Drop, 0, @buffer, MAX_PATH);
    if uppercase(ExtractFileExt(String(buffer))) = '.DVB' then
    begin
      LoadFromFile(String(buffer));
      OpenDialog1.FileName := String(buffer);
      SaveDialog1.FileName := String(buffer);
    end;
  end;
end;

procedure TfrmScan.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Application.Title := Caption;
  DragAcceptFiles(Handle, True);
  FThread := nil;
  CoInitialize(nil);
  FChannelList := TDVBChannelsList.Create;
  FFrequencys := TDVBFrequencyList.Create;
  FFrequencys.LoadFromFile(GetCurrentDirectory + 'Frequencies.xml');
  FDebug := TfrmDebug.Create(Self);
  FLog := TCallbackLogger.Create;
  FLog.OnLog := FDebug.Add;
  FLog.Enabled := True;

  FVideoGuids := TStringList.Create;
  FAudioGuids := TStringList.Create;
  FAACGuids := TStringList.Create;
  FAC3Guids := TStringList.Create;
  FH264Guids := TStringList.Create;
  FPPGuids := TStringList.Create;

  FDevices := TDCDVBTPluginList.Create(FLog);
  RadioButton3Click(Self);

  for i := 0 to FDevices.Count -1 do
  begin
    ComboBox1.Items.Add(FDevices.DeviceName[i]);
    ComboBox7.Items.Add(FDevices.DeviceName[i]);
  end;

  if ComboBox7.Items.Count > 0
    then ComboBox7.ItemIndex := 0;
  ComboBox7Change(Self);

  Reset1Click(Self);
  FScanner := TDVBScanner.Create(FLog);
  FMulticastClient := TMulticastClient.Create(FLog);

  FDCDVBScanner := TDCDVBTuningPluginTransportStream.Create(FLog);
  FDCDVBScanner.OnTSData := OnBDAData;

  FTCPClient := TTCPClientSocket.Create;
  FTCPClient.OnReadData := OnMulticastData;

  ComboBox2.ItemIndex := 0;
  ComboBox2Change(Self);

  FOSDColor.StartColor := $00B07D44;
  FOSDColor.EndColor := $00754923;
  FOSDColor.FontColor := clWhite;
  FOSDColor.FontShadowColor := clBlack;
end;

procedure TfrmScan.FormDestroy(Sender: TObject);
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
//    FThread.WaitFor;
//    FThread.Free;
    FThread := nil;
  end;

  FMulticastClient.Free;
  FTCPClient.Free;
  FDCDVBScanner.Free;
  FScanner.Free;
  FVideoGuids.Free;
  FAudioGuids.Free;
  FAACGuids.Free;
  FAC3Guids.Free;
  FH264Guids.Free;
  FPPGuids.Free;
  FDevices.Free;
  FChannelList.Free;
  FLog.Free;
  FDebug.Free;
  FFrequencys.Free;
  CoUninitialize;
end;

procedure TfrmScan.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_MAXIMIZEBOX);
  ClientWidth := PageControl1.Width + (PageControl1.Left * 2);
  ClientHeight := PageControl1.Height + (PageControl1.Top * 2);
end;

procedure TfrmScan.RadioButton1Click(Sender: TObject);
begin
  Edit1.Enabled := False;
  SpinEdit7.Enabled := False;
  ComboBox1.Enabled := True;
  RadioButton1.Checked := True;
  RadioButton1.Font.Style := RadioButton1.Font.Style + [fsBold];
  RadioButton2.Font.Style := RadioButton2.Font.Style - [fsBold];
  ComboBox9.Enabled := False;
  if ComboBox1.ItemIndex < 0 then
  begin
    Button16.Enabled := False;
  end else
  begin
    Button16.Enabled := FDevices.HasPropertyPage(ComboBox1.ItemIndex);
  end;
end;

procedure TfrmScan.RadioButton2Click(Sender: TObject);
begin
  RadioButton2.Font.Style := RadioButton2.Font.Style + [fsBold];
  RadioButton1.Font.Style := RadioButton1.Font.Style - [fsBold];
  Edit1.Enabled := True;
  SpinEdit7.Enabled := True;
  ComboBox1.Enabled := False;
  ComboBox9.Enabled := True;
  RadioButton2.Checked := True;
  Button16.Enabled := False;
end;

procedure TfrmScan.CheckBox1Click(Sender: TObject);
begin
  SpinEdit1.Enabled := CheckBox1.Checked;
end;

procedure TfrmScan.CheckBox4Click(Sender: TObject);
begin
  SpinEdit6.Enabled := CheckBox4.Checked;
  Edit2.Enabled := CheckBox4.Checked;
  ComboBox10.Enabled := CheckBox4.Checked;
end;

procedure TfrmScan.About1Click(Sender: TObject);
var
  frm: TfrmAbout;
begin
  frm := TfrmAbout.Create(Self);
  frm.Icon := Icon;
  frm.ShowModal;
  frm.Free;
end;

procedure TfrmScan.RadioButton3Click(Sender: TObject);
begin
  ComboBox11.Enabled := False;
  ComboBox7.Enabled := True;
  ComboBox8.Enabled := True;
  Edit3.Enabled := False;
  Edit4.Enabled := False;
  SpinEdit11.Enabled := False;
  SpeedButton1.Enabled := False;
  RadioButton3.Font.Style := RadioButton3.Font.Style + [fsBold];
  RadioButton4.Font.Style := RadioButton4.Font.Style - [fsBold];
  RadioButton5.Font.Style := RadioButton5.Font.Style - [fsBold];
  label10.Font.Style := label10.Font.Style + [fsBold];
  if Visible then RadioButton3.SetFocus;
  if ComboBox7.ItemIndex < 0 then
  begin
    Button15.Enabled := False;
    Button17.Enabled := False;
  end else
  begin
    Button17.Enabled := FDevices.HasPropertyPage(ComboBox7.ItemIndex);
    case FDevices.DeviceType[ComboBox7.ItemIndex] of
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT: // DVB-t
      begin
        Button15.Enabled := False;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC: // DVB-c
      begin
        Button15.Enabled := False;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS: // DVB-s
      begin
        Button15.Enabled := True;
      end;
      DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC: // ATSC
      begin
        Button15.Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmScan.RadioButton4Click(Sender: TObject);
begin
  ComboBox11.Enabled := True;
  RadioButton4.Font.Style := RadioButton4.Font.Style + [fsBold];
  RadioButton3.Font.Style := RadioButton3.Font.Style - [fsBold];
  RadioButton5.Font.Style := RadioButton5.Font.Style - [fsBold];
  label10.Font.Style := label10.Font.Style - [fsBold];
  ComboBox7.Enabled := False;
  ComboBox8.Enabled := False;
  Edit3.Enabled := True;
  Edit4.Enabled := False;
  SpinEdit11.Enabled := True;
  SpeedButton1.Enabled := False;
  Button17.Enabled := False;
  Button15.Enabled := False;
end;

procedure TfrmScan.RadioButton5Click(Sender: TObject);
begin
  ComboBox11.Enabled := False;
  RadioButton5.Font.Style := RadioButton5.Font.Style + [fsBold];
  label10.Font.Style := label10.Font.Style - [fsBold];
  RadioButton4.Font.Style := RadioButton4.Font.Style - [fsBold];
  RadioButton3.Font.Style := RadioButton3.Font.Style - [fsBold];
  ComboBox7.Enabled := False;
  ComboBox8.Enabled := False;
  Edit3.Enabled := False;
  Edit4.Enabled := True;
  SpinEdit11.Enabled := False;
  SpeedButton1.Enabled := True;
  Button17.Enabled := False;
  Button15.Enabled := False;
end;

procedure TfrmScan.DebugLog1Click(Sender: TObject);
begin
  FDebug.Show;
end;

procedure TfrmScan.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex < 0 then
  begin
    Button16.Enabled := False;
    RadioButton1.Caption := 'Device';
    Exit;
  end;

  Button16.Enabled := FDevices.HasPropertyPage(ComboBox1.ItemIndex);
  RadioButton1.Caption := 'Device (' + FDevices.DeviceTypeString[ComboBox1.ItemIndex] + ')';
end;

procedure TfrmScan.ComboBox7Change(Sender: TObject);
var
  i: Integer;
begin
  if ComboBox7.ItemIndex < 0 then
  begin
    Button15.Enabled := False;
    Button17.Enabled := False;
    RadioButton3.Caption := 'Device';
    Exit;
  end;

  RadioButton3.Caption := 'Device (' + FDevices.DeviceTypeString[ComboBox7.ItemIndex] + ')';
  ComboBox8.Clear;
  Button17.Enabled := FDevices.HasPropertyPage(ComboBox7.ItemIndex);

  case FDevices.DeviceType[ComboBox7.ItemIndex] of
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT: // DVB-t
    begin
      Button15.Enabled := False;
      for i := 0 to FFrequencys.DVBTCount -1 do
      begin
        ComboBox8.Items.Add(FFrequencys.DVBTFrequency[i].Name);
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC: // DVB-c
    begin
      Button15.Enabled := False;
      for i := 0 to FFrequencys.DVBCCount -1 do
      begin
        ComboBox8.Items.Add(FFrequencys.DVBCFrequency[i].Name);
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS: // DVB-s
    begin
      Button15.Enabled := True;
      for i := 0 to FFrequencys.DVBSCount -1 do
      begin
        ComboBox8.Items.Add(FFrequencys.DVBSFrequency[i].Name);
      end;
    end;
    DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC: // ATSC
    begin
      Button15.Enabled := False;
      for i := 0 to FFrequencys.ATSCCount -1 do
      begin
        ComboBox8.Items.Add(FFrequencys.ATSCFrequency[i].Name);
      end;
    end;
  end;

  if ComboBox8.Items.Count > 0 then
  begin
    ComboBox8.ItemIndex := 0;
  end;
end;

procedure TfrmScan.Reset1Click(Sender: TObject);
begin
  // Stream Device
  if ComboBox1.Items.Count > 0
    then ComboBox1.ItemIndex := 0;
  ComboBox1Change(Self);
  RadioButton1Click(Self);
  Edit1.Text := '225.1.1.1';
  SpinEdit7.Value := 12345;
  ComboBox9.ItemIndex := 0;

  // TS multicast Server
  CheckBox4.Checked := False;
  Edit2.Text := '225.1.1.1';
  SpinEdit6.Value := 12345;
  ComboBox10.ItemIndex := 0;

  // Misc Settings
  CheckBox2.Checked := False;
  CheckBox6.Checked := False;
  CheckBox8.Checked := False;
  CheckBox3.Checked := True;
  CheckBox1.Checked := False;
  CheckBox1Click(Self);
  SpinEdit2.Value := 0;
  SpinEdit1.Value := 30;

  // Video Screen
  SpinEdit3.Value := 3;

  // Teletext Screen
  SpinEdit4.Value := 2;
  SpinEdit5.Value := 10;

  // OSD
  SpinEdit10.Value := 33;
  SpinEdit9.Value := 10;
  SpinEdit8.Value := 5;
  CheckBox7.Checked := True;

  // Decoders and Post Processor
  SpeedButton2Click(Self);
  SpeedButton3Click(Self);
  SpeedButton4Click(Self);
  SpeedButton6Click(Self);
  SpeedButton7Click(Self);
  SpeedButton5Click(Self);

  CheckBox10.Checked := False;
  CheckBox13.Checked := False;
  CheckBox5.Checked := True;

  SpinEdit19.Value := 10;
  SpinEdit15.Value := 0;
  SpinEdit12.Value := 2;

  // Channels;
  ListView1.Clear;
  EnableButtons;
  FChannelList.Clear;

  // OSD Color
  FOSDColor.StartColor := $00B07D44;
  FOSDColor.EndColor := $00754923;
  FOSDColor.FontColor := clWhite;
  FOSDColor.FontShadowColor := clBlack;

  // Ignore Device ID
  CheckBox9.Checked := False;
end;

procedure TfrmScan.SpeedButton2Click(Sender: TObject);
const
  CLSID_FFDShowVideoDecoder: TGuid = '{04fe9017-f873-410e-871e-ab91661a4ef7}';
var
  Mapper: IFilterMapper2;
  Types: array[0..1] of TGuid;
  filter: IBaseFilter;
  in_list: Boolean;
  i: Integer;
begin
  ComboBox3.Clear;
  FVideoGuids.Clear;

  if CoCreateInstance(CLSID_FilterMapper2, nil, CLSCTX_INPROC, IID_IFilterMapper2, Mapper) = S_OK then
  begin
    Types[0] := MEDIATYPE_Video;
    Types[1] := MEDIASUBTYPE_MPEG2_VIDEO;
    FillList(Mapper, @Types, ComboBox3, FVideoGuids, MERIT_DO_NOT_USE + 1, 1);
    Mapper := nil;
  end;

  in_list := false;
  for i := 0 to FVideoGuids.Count -1 do
  begin
    if IsEqualGUID(StringToGUID(FVideoGuids[i]), CLSID_FFDShowVideoDecoder) then
    begin
      in_list := True;
      break;
    end;
  end;

  if not in_list then
  begin
    if CoCreateInstance(CLSID_FFDShowVideoDecoder, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, filter) = S_OK then
    begin
      ComboBox3.Items.Add('ffdshow MPEG4 Video Decoder');
      FVideoGuids.Add(GUIDToString(CLSID_FFDShowVideoDecoder));
    end;
  end;

  if ComboBox3.Items.Count > 0
    then ComboBox3.ItemIndex := 0;
end;

procedure TfrmScan.SpeedButton3Click(Sender: TObject);
var
  Mapper: IFilterMapper2;
  Types: array[0..1] of TGuid;
begin
  ComboBox4.Clear;
  FAudioGuids.Clear;

  if CoCreateInstance(CLSID_FilterMapper2, nil, CLSCTX_INPROC, IID_IFilterMapper2, Mapper) = S_OK then
  begin
    Types[0] := MEDIATYPE_Audio;
    Types[1] := MEDIASUBTYPE_MPEG2_AUDIO;
    FillList(Mapper, @Types, ComboBox4, FAudioGuids, MERIT_DO_NOT_USE + 1, 1);
    Mapper := nil;
  end;

  if ComboBox4.Items.Count > 0
    then ComboBox4.ItemIndex := 0;
end;

procedure TfrmScan.SpeedButton4Click(Sender: TObject);
begin
  ComboBox5.Clear;
  FPPGuids.Clear;

  ComboBox5.Items.Add('No Postprocessor');

  if IsFilterAvailable(StringToGUID('{0B390488-D80F-4A68-8408-48DC199F0E97}')) then
  begin
    FPPGuids.Add('{0B390488-D80F-4A68-8408-48DC199F0E97}');
    ComboBox5.Items.Add('ffdshow raw video filter');
  end;

  if IsFilterAvailable(StringToGUID('{0524CEA7-7E7D-4804-8A7B-D17A99807D3C}')) then
  begin
    FPPGuids.Add('{0524CEA7-7E7D-4804-8A7B-D17A99807D3C}');
    ComboBox5.Items.Add('NVidia Video Post Processor');
  end;
  ComboBox5.ItemIndex := 0; 
end;

procedure TfrmScan.SpeedButton5Click(Sender: TObject);
var
  Mapper: IFilterMapper2;
  Types: array[0..7] of TGuid;
begin
  ComboBox12.Clear;
  FH264Guids.Clear;

  if CoCreateInstance(CLSID_FilterMapper2, nil, CLSCTX_INPROC, IID_IFilterMapper2, Mapper) = S_OK then
  begin
    Types[0]  := MEDIATYPE_Video;
    Types[1]  := FOURCCMap(PCardinal(PChar('h264'))^);
    Types[2]  := MEDIATYPE_Video;
    Types[3]  := FOURCCMap(PCardinal(PChar('H264'))^);
    Types[4]  := MEDIATYPE_Video;
    Types[5]  := FOURCCMap(PCardinal(PChar('avc1'))^);
    Types[6]  := MEDIATYPE_Video;
    Types[7]  := FOURCCMap(PCardinal(PChar('AVC1'))^);
    FillList(Mapper, @Types, ComboBox12, FH264Guids, MERIT_DO_NOT_USE + 1, 4);
    Mapper := nil;
  end;

  if ComboBox12.Items.Count > 0
    then ComboBox12.ItemIndex := 0;
end;

procedure TfrmScan.SpeedButton6Click(Sender: TObject);
var
  Mapper: IFilterMapper2;
  Types: array[0..1] of TGuid;
begin
  ComboBox13.Clear;
  FAC3Guids.Clear;

  if CoCreateInstance(CLSID_FilterMapper2, nil, CLSCTX_INPROC, IID_IFilterMapper2, Mapper) = S_OK then
  begin
    Types[0] := MEDIATYPE_Audio;
    Types[1] := MEDIASUBTYPE_DOLBY_AC3;
    FillList(Mapper, @Types, ComboBox13, FAC3Guids, MERIT_DO_NOT_USE + 1, 1);
    Mapper := nil;
  end;

  if ComboBox13.Items.Count > 0
    then ComboBox13.ItemIndex := 0;
end;

procedure TfrmScan.SpeedButton7Click(Sender: TObject);
var
  Mapper: IFilterMapper2;
  Types: array[0..7] of TGuid;
begin
  ComboBox14.Clear;
  FAACGuids.Clear;

  if CoCreateInstance(CLSID_FilterMapper2, nil, CLSCTX_INPROC, IID_IFilterMapper2, Mapper) = S_OK then
  begin
    Types[0] := MEDIATYPE_Audio;
    Types[1] := FOURCCMap($0000AAC0);
    Types[2] := MEDIATYPE_Audio;
    Types[3] := FOURCCMap($000000FF);
    Types[4] := MEDIATYPE_Audio;
    Types[5] := FOURCCMap($4134504D);
    Types[6] := MEDIATYPE_Audio;
    Types[7] := FOURCCMap($0000706D);
    FillList(Mapper, @Types, ComboBox14, FAACGuids, MERIT_DO_NOT_USE + 1, 4);
    Mapper := nil;
  end;

  if ComboBox14.Items.Count > 0
    then ComboBox14.ItemIndex := 0;
end;

procedure TfrmScan.Save1Click(Sender: TObject);
begin
  if SaveDialog1.Execute
    then SaveToFile(SaveDialog1.FileName);
end;

function GetLastSavedChannel(AFile: WideString): Integer;
var
  xml: TJvSimpleXml;
  e1, e2: TJvSimpleXmlElem;
  p1: TJvSimpleXmlProp;
begin
  Result := -1;

  if not FileExists(AFile)
    then Exit;

  xml := TJvSimpleXml.Create(nil);
  xml.LoadFromFile(AFile);
  e1 := xml.Root.Items.ItemNamed['settings'];
  if Assigned(e1) then
  begin
    // Save Last Channel
    e2 := e1.Items.ItemNamed['channel'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['last'];
      if Assigned(p1)
        then Result := p1.IntValue;
    end;
  end;

  xml.Free;
end;

procedure TfrmScan.SaveToFile(AFile: WideString);
var
  xml: TJvSimpleXml;
  e1, e2, e3, e4, e5: TJvSimpleXmlElem;
  i, c, k: Integer;
  chan: TDVBChannel;
  stream: TDVBBaseStream;
  str: String;
  last: Integer;
begin
  xml := TJvSimpleXml.Create(nil);
  xml.Root.Name := 'dvb';

  e1 := xml.Root.Items.Add('settings');
  last := GetLastSavedChannel(AFile);

  // Stream Source
  if RadioButton2.Checked or (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('tuner');
    if RadioButton1.Checked then
    begin
      e2.Properties.Add('name', ComboBox1.Text);
      e2.Properties.Add('id', FDevices.DeviceID[ComboBox1.ItemIndex]);
      e2.Properties.Add('type', FDevices.DeviceType[ComboBox1.ItemIndex]);
      e2.Properties.Add('ignoreid', Byte(CheckBox9.Checked));
      str := FDevices.DeviceSettings[ComboBox1.ItemIndex];
      if str <> ''
        then e2.Properties.Add('settings', str);
    end else
    begin
      e2.Properties.Add('host', Edit1.Text);
      e2.Properties.Add('port', SpinEdit7.Value);
      e2.Properties.Add('type', ComboBox9.ItemIndex);
    end;
  end;

  // Video Decoder
  if (ComboBox3.Items.Count > 0) and (ComboBox3.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('mpeg2videodecoder');
    e2.Properties.Add('name', ComboBox3.Text);
    e2.Properties.Add('clsid', FVideoGuids[ComboBox3.ItemIndex]);
  end;

  if (ComboBox12.Items.Count > 0) and (ComboBox12.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('h264videodecoder');
    e2.Properties.Add('name', ComboBox12.Text);
    e2.Properties.Add('clsid', FH264Guids[ComboBox12.ItemIndex]);
  end;

  // Audio Decoder
  if (ComboBox4.Items.Count > 0) and (ComboBox4.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('mpegaudiodecoder');
    e2.Properties.Add('name', ComboBox4.Text);
    e2.Properties.Add('clsid', FAudioGuids[ComboBox4.ItemIndex]);
  end;

  if (ComboBox13.Items.Count > 0) and (ComboBox13.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('ac3audiodecoder');
    e2.Properties.Add('name', ComboBox13.Text);
    e2.Properties.Add('clsid', FAC3Guids[ComboBox13.ItemIndex]);
  end;

  if (ComboBox14.Items.Count > 0) and (ComboBox14.ItemIndex >= 0) then
  begin
    e2 := e1.Items.Add('aacaudiodecoder');
    e2.Properties.Add('name', ComboBox14.Text);
    e2.Properties.Add('clsid', FAACGuids[ComboBox14.ItemIndex]);
  end;

  // Video Post Processor
  if (ComboBox5.Items.Count > 0) and (ComboBox5.ItemIndex > 0) then
  begin
    e2 := e1.Items.Add('postprocessor');
    e2.Properties.Add('name', ComboBox5.Text);
    e2.Properties.Add('clsid', FPPGuids[ComboBox5.ItemIndex -1]);
  end;

  // EPG
  e2 := e1.Items.Add('epg');
  e2.Properties.Add('timeoffset', SpinEdit2.Value);

  // Timeshift
  e2 := e1.Items.Add('timeshift');
  e2.Properties.Add('enabled', Byte(CheckBox1.Checked));
  e2.Properties.Add('duration', SpinEdit1.Value);

  // Video Screen Offset
  e2 := e1.Items.Add('video');
  e2.Properties.Add('offset', SpinEdit3.Value);

  // Teletext
  e2 := e1.Items.Add('teletext');
  e2.Properties.Add('offset', SpinEdit4.Value);
  e2.Properties.Add('alpha', SpinEdit5.Value);
  e2.Properties.Add('ar', Byte(CheckBox5.Checked));
  e2.Properties.Add('fps', SpinEdit12.Value);

  e2 := e1.Items.Add('channel');
  e2.Properties.Add('save', Byte(CheckBox3.Checked));
  e2.Properties.Add('last', last);

  e2 := e1.Items.Add('debug');
  e2.Properties.Add('writelog', Byte(CheckBox2.Checked));
  e2.Properties.Add('disableosd', Byte(CheckBox8.Checked));
  e2.Properties.Add('disableepg', Byte(CheckBox13.Checked));
  e2.Properties.Add('disabledsmcc', Byte(CheckBox10.Checked));
  e2.Properties.Add('packetsize', -1);

  e2 := e1.Items.Add('streaming');
  e2.Properties.Add('enabled', Byte(CheckBox4.Checked));
  e2.Properties.Add('group', Edit2.Text);
  e2.Properties.Add('port', SpinEdit6.Value);
  e2.Properties.Add('type', ComboBox10.ItemIndex);

  e2 := e1.Items.Add('clock');
  e2.Properties.Add('change', Byte(CheckBox6.Checked));

  e2 := e1.Items.Add('osd');
  e2.Properties.Add('enabled', Byte(CheckBox7.Checked));
  e2.Properties.Add('alpha', SpinEdit9.Value);
  e2.Properties.Add('duration', SpinEdit8.Value);
  e2.Properties.Add('offset', SpinEdit10.Value);
  e2.Properties.Add('startcolor', Cardinal(FOSDColor.StartColor));
  e2.Properties.Add('endcolor', Cardinal(FOSDColor.EndColor));
  e2.Properties.Add('fontcolor', Cardinal(FOSDColor.FontColor));
  e2.Properties.Add('fontshadowcolor', Cardinal(FOSDColor.FontShadowColor));

  e2 := e1.Items.Add('subtitle');
  e2.Properties.Add('alpha', SpinEdit19.Value);
  e2.Properties.Add('offset', SpinEdit15.Value);

  if FChannelList.Count > 0 then
  begin
    e1 := xml.Root.Items.Add('channels');
    for i := 0 to FChannelList.Count -1 do
    begin
      chan := FChannelList[i];

      e2 := e1.Items.Add('channel');
      e2.Properties.Add('name', chan.Name);
      e2.Properties.Add('provider', chan.Provider);
      e2.Properties.Add('sid', chan.SID);
      e2.Properties.Add('pcrpid', chan.PCRPID);
      e2.Properties.Add('pmpid', chan.ProgramMapPID);
      e2.Properties.Add('st', chan.ServiceType);

      e3 := e2.Items.Add('network');
      e3.Properties.Add('type', Integer(chan.Network.Type_));
      e3.Properties.Add('name', chan.Network.Name);
      e3.Properties.Add('nid', chan.Network.NetworkID);
      e3.Properties.Add('onid', chan.Network.ONID);
      e3.Properties.Add('tsid', chan.Network.TSID);

      case chan.Network.Type_ of
        ntDVBT:
        begin
          e3.Properties.Add('cf', chan.Network.Terrestrial.CentreFrequency);
          e3.Properties.Add('bw', chan.Network.Terrestrial.Bandwidth);
          e3.Properties.Add('prio', chan.Network.Terrestrial.Priority);
          e3.Properties.Add('tsi', chan.Network.Terrestrial.TimeSlicingIndicator);
          e3.Properties.Add('fec', chan.Network.Terrestrial.MPEFECIndicator);
          e3.Properties.Add('con', Integer(chan.Network.Terrestrial.Constellation));
          e3.Properties.Add('hi', Integer(chan.Network.Terrestrial.HierarchyInformation));
          e3.Properties.Add('crhp', Integer(chan.Network.Terrestrial.CodeRateHPStream));
          e3.Properties.Add('crlp', Integer(chan.Network.Terrestrial.CodeRateLPStream));
          e3.Properties.Add('gi', Integer(chan.Network.Terrestrial.GuardInterval));
          e3.Properties.Add('tm', Integer(chan.Network.Terrestrial.TransmissionMode));
          e3.Properties.Add('off', chan.Network.Terrestrial.OtherFrequencyFlag);
        end;
        ntDVBS:
        begin
          e3.Properties.Add('f', chan.Network.Satellite.Frequency);
          e3.Properties.Add('op', chan.Network.Satellite.OrbitalPosition);
          e3.Properties.Add('wef', chan.Network.Satellite.WestEastFlag);
          e3.Properties.Add('p', Integer(chan.Network.Satellite.Polarization));
          e3.Properties.Add('mod', Integer(chan.Network.Satellite.Modulation));
          e3.Properties.Add('sr', chan.Network.Satellite.SymbolRate);
          e3.Properties.Add('feci', Integer(chan.Network.Satellite.FECInner));
        end;
        ntDVBC:
        begin
          e3.Properties.Add('f', chan.Network.Cable.Frequency);
          e3.Properties.Add('feco', Integer(chan.Network.Cable.FECOuter));
          e3.Properties.Add('mod', Integer(chan.Network.Cable.Modulation));
          e3.Properties.Add('sr', chan.Network.Cable.SymbolRate);
          e3.Properties.Add('feci', Integer(chan.Network.Cable.FECInner));
        end;
      end;
      if chan.Streams.Count > 0 then
      begin
        e3 := e2.Items.Add('streams');
        for c := 0 to chan.Streams.Count -1 do
        begin
          stream := chan.Streams[c];

          e4 := e3.Items.Add('stream');
          e4.Properties.Add('name', stream.Name);
          e4.Properties.Add('type', Integer(stream.StreamType));
          e4.Properties.Add('pid', stream.PID);
          e4.Properties.Add('tag', stream.Tag);
          e4.Properties.Add('default', Byte(stream.Default));
          case stream.StreamType of
            stAudio:
            begin
              with TDVBAudioStream(stream) do
              begin
                e4.Properties.Add('lng', String(Language));
                e4.Properties.Add('code', Integer(Coding));
                e4.Properties.Add('at', Integer(AudioType));
              end;
            end;
            stVideo:
            begin
              with TDVBVideoStream(stream) do
              begin
                e4.Properties.Add('code', Integer(Coding));
              end;
            end;
            stTeletext:
            begin
              with TDVBTeletextStream(stream) do
              begin
                e4.Properties.Add('lng', String(Language));
              end;
            end;
            stSubtitle:
            begin
              with TDVBSubtitleStream(stream) do
              begin
                for k := 0 to SubtitlesCount -1 do
                begin
                  e5 := e4.Items.Add('sub');
                  with Subtitle[k] do
                  begin
                    e5.Properties.Add('lng', String(LanguageCode));
                    e5.Properties.Add('st', SubtitlingType);
                    e5.Properties.Add('cp', CompositionPageID);
                    e5.Properties.Add('ap', AncillaryPageID);
                    e5.Properties.Add('default', Byte(DefaultSub));
                  end;
                end;
              end;
            end;
          end;
        end
      end;
    end;
  end;

  xml.SaveToFile(AFile);
  xml.Free;
end;

procedure TfrmScan.LoadFromFile(AFile: WideString);
var
  xml: TJvSimpleXml;
  e1, e2, e3, e4, e5: TJvSimpleXmlElem;
  p1, p2, p3, p4, p5: TJvSimpleXmlProp;
  i, c, k: Integer;
  chan: TDVBChannel;
  stream: TDVBBaseStream;
  item: TListItem;
begin
  Reset1Click(Self);
  xml := TJvSimpleXml.Create(nil);
  xml.LoadFromFile(AFile);

  e1 := xml.Root.Items.ItemNamed['settings'];
  if Assigned(e1) then
  begin
    // Tuner
    e2 := e1.Items.ItemNamed['tuner'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      if Assigned(p1) then
      begin
        RadioButton1Click(Self);
        p1 := e2.Properties.ItemNamed['name'];
        p2 := e2.Properties.ItemNamed['id'];
        p3 := e2.Properties.ItemNamed['type'];
        p4 := e2.Properties.ItemNamed['ignoreid'];

        if Assigned(p1) and Assigned(p2) and Assigned(p3) then
        begin
          for i := 0 to ComboBox1.Items.Count -1 do
          begin
            if (ComboBox1.Items[i] = p1.Value) and (FDevices.DeviceID[i] = p2.Value) and (Integer(FDevices.DeviceType[i]) = p3.IntValue) then
            begin
              ComboBox1.ItemIndex := i;
              break;
            end;
          end;
        end;
        if ComboBox1.ItemIndex > -1 then
        begin
          p3 := e2.Properties.ItemNamed['settings'];
          if Assigned(p3) then
          begin
            FDevices.DeviceSettings[ComboBox1.ItemIndex] := p3.Value;
          end;
        end;

        if Assigned(p4) then
        begin
          CheckBox9.Checked := Boolean(p4.IntValue)
        end;
      end else
      begin
        RadioButton2Click(Self);
        p1 := e2.Properties.ItemNamed['host'];
        p2 := e2.Properties.ItemNamed['port'];
        p3 := e2.Properties.ItemNamed['type'];
        if Assigned(p1)
          then Edit1.Text := p1.Value;
        if Assigned(p2)
          then SpinEdit7.Value := p2.IntValue;
        if Assigned(p3)
          then ComboBox9.ItemIndex := p3.IntValue;
      end;
    end;

    // Video Decoder
    e2 := e1.Items.ItemNamed['mpeg2videodecoder'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox3.Items.Count -1 do
        begin
          if (ComboBox3.Items[i] = p1.Value) and (LowerCase(FVideoGuids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox3.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;

    e2 := e1.Items.ItemNamed['h264videodecoder'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox12.Items.Count -1 do
        begin
          if (ComboBox12.Items[i] = p1.Value) and (LowerCase(FH264Guids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox12.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;

    // Audio Decoder
    e2 := e1.Items.ItemNamed['mpegaudiodecoder'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox4.Items.Count -1 do
        begin
          if (ComboBox4.Items[i] = p1.Value) and (LowerCase(FAudioGuids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox4.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;

    e2 := e1.Items.ItemNamed['ac3audiodecoder'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox13.Items.Count -1 do
        begin
          if (ComboBox13.Items[i] = p1.Value) and (LowerCase(FAC3Guids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox13.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;

    e2 := e1.Items.ItemNamed['aacaudiodecoder'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox14.Items.Count -1 do
        begin
          if (ComboBox14.Items[i] = p1.Value) and (LowerCase(FAACGuids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox14.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;

    // Post Processor
    e2 := e1.Items.ItemNamed['postprocessor'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['name'];
      p2 := e2.Properties.ItemNamed['clsid'];
      if Assigned(p1) and Assigned(p2) then
      begin
        for i := 0 to ComboBox5.Items.Count -1 do
        begin
          if (ComboBox5.Items[i+1] = p1.Value) and (LowerCase(FPPGuids[i]) = LowerCase(p2.Value)) then
          begin
            ComboBox5.ItemIndex := i+1;
            break;
          end;
        end;
      end;
    end;

    // Time Shifting
    e2 := e1.Items.ItemNamed['timeshift'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['enabled'];
      if Assigned(p1)
        then CheckBox1.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['duration'];
      if Assigned(p1)
        then SpinEdit1.Value := p1.IntValue;
      SpinEdit1.Enabled := CheckBox1.Checked;
    end;

    // EPG
    e2 := e1.Items.ItemNamed['epg'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['timeoffset'];
      if Assigned(p1)
        then SpinEdit2.Value := p1.IntValue;
    end;

    // Video Screen
    e2 := e1.Items.ItemNamed['video'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['offset'];
      if Assigned(p1)
        then SpinEdit3.Value := p1.IntValue;
    end;

    // Teletext Screen
    e2 := e1.Items.ItemNamed['teletext'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['offset'];
      if Assigned(p1)
        then SpinEdit4.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['alpha'];
      if Assigned(p1)
        then SpinEdit5.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['ar'];
      if Assigned(p1)
        then CheckBox5.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['fps'];
      if Assigned(p1)
        then SpinEdit12.Value := p1.IntValue;
    end;

    // Save Last Channel
    e2 := e1.Items.ItemNamed['channel'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['save'];
      if Assigned(p1)
        then CheckBox3.Checked := Boolean(p1.IntValue);
    end;

    // Debug
    e2 := e1.Items.ItemNamed['debug'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['writelog'];
      if Assigned(p1)
        then CheckBox2.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['disableosd'];
      if Assigned(p1)
        then CheckBox8.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['disableepg'];
      if Assigned(p1)
        then CheckBox13.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['disabledsmcc'];
      if Assigned(p1)
        then CheckBox10.Checked := Boolean(p1.IntValue);
    end;

    // Streaming
    e2 := e1.Items.ItemNamed['streaming'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['enabled'];
      if Assigned(p1)
        then CheckBox4.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['group'];
      if Assigned(p1)
        then Edit2.Text := p1.Value;
      p1 := e2.Properties.ItemNamed['port'];
      if Assigned(p1)
        then SpinEdit6.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['type'];
      if Assigned(p1)
        then ComboBox10.ItemIndex := p1.IntValue;
    end;
    SpinEdit6.Enabled := CheckBox4.Checked;

    // Clock
    e2 := e1.Items.ItemNamed['clock'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['change'];
      if Assigned(p1)
        then CheckBox6.Checked := Boolean(p1.IntValue);
    end;

    // OSD
    e2 := e1.Items.ItemNamed['osd'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['enabled'];
      if Assigned(p1)
        then CheckBox7.Checked := Boolean(p1.IntValue);
      p1 := e2.Properties.ItemNamed['alpha'];
      if Assigned(p1)
        then SpinEdit9.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['duration'];
      if Assigned(p1)
        then SpinEdit8.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['offset'];
      if Assigned(p1)
        then SpinEdit10.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['startcolor'];
      if Assigned(p1)
        then FOSDColor.StartColor := p1.IntValue;
      p1 := e2.Properties.ItemNamed['endcolor'];
      if Assigned(p1)
        then FOSDColor.EndColor := p1.IntValue;
      p1 := e2.Properties.ItemNamed['fontcolor'];
      if Assigned(p1)
        then FOSDColor.FontColor := p1.IntValue;
      p1 := e2.Properties.ItemNamed['fontshadowcolor'];
      if Assigned(p1)
        then FOSDColor.FontShadowColor := p1.IntValue;
    end;

    // Subtitle
    e2 := e1.Items.ItemNamed['subtitle'];
    if Assigned(e2) then
    begin
      p1 := e2.Properties.ItemNamed['alpha'];
      if Assigned(p1)
        then SpinEdit19.Value := p1.IntValue;
      p1 := e2.Properties.ItemNamed['offset'];
      if Assigned(p1)
        then SpinEdit15.Value := p1.IntValue;
    end;
  end;

  e1 := xml.root.Items.ItemNamed['channels'];
  if Assigned(e1) then
  begin
    for i := 0 to e1.Items.Count -1 do
    begin
      e2 := e1.Items[i];
      chan := TDVBChannel.Create;

      p1 := e2.Properties.ItemNamed['name'];
      if Assigned(p1) then chan.Name := p1.Value;
      p1 := e2.Properties.ItemNamed['provider'];
      if Assigned(p1) then chan.Provider := p1.Value;
      p1 := e2.Properties.ItemNamed['sid'];
      if Assigned(p1) then chan.SID := p1.IntValue;
      p1 := e2.Properties.ItemNamed['pcrpid'];
      if Assigned(p1) then chan.PCRPID := p1.IntValue;
      p1 := e2.Properties.ItemNamed['pmpid'];
      if Assigned(p1) then chan.ProgramMapPID := p1.IntValue;
      p1 := e2.Properties.ItemNamed['st'];
      if Assigned(p1) then chan.ServiceType := p1.IntValue;

      e3 := e2.Items.ItemNamed['network'];
      if Assigned(e3) then
      begin
        p1 := e3.Properties.ItemNamed['name'];
        if Assigned(p1) then chan.Network.Name := p1.Value;
        p1 := e3.Properties.ItemNamed['type'];
        if Assigned(p1) then chan.Network.Type_ := TDVBNetworkType(p1.IntValue);
        p1 := e3.Properties.ItemNamed['nid'];
        if Assigned(p1) then chan.Network.NetworkID := p1.IntValue;
        p1 := e3.Properties.ItemNamed['onid'];
        if Assigned(p1) then chan.Network.ONID := p1.IntValue;
        p1 := e3.Properties.ItemNamed['tsid'];
        if Assigned(p1) then chan.Network.TSID := p1.IntValue;
        case chan.Network.Type_ of
          ntDVBT:
          begin
            p1 := e3.Properties.ItemNamed['cf'];
            if Assigned(p1) then chan.Network.Terrestrial.CentreFrequency := p1.IntValue;
            p1 := e3.Properties.ItemNamed['bw'];
            if Assigned(p1) then chan.Network.Terrestrial.Bandwidth := p1.IntValue;
            p1 := e3.Properties.ItemNamed['prio'];
            if Assigned(p1) then chan.Network.Terrestrial.Priority := p1.IntValue;
            p1 := e3.Properties.ItemNamed['tsi'];
            if Assigned(p1) then chan.Network.Terrestrial.TimeSlicingIndicator := p1.IntValue;
            p1 := e3.Properties.ItemNamed['fec'];
            if Assigned(p1) then chan.Network.Terrestrial.MPEFECIndicator := p1.IntValue;
            p1 := e3.Properties.ItemNamed['con'];
            if Assigned(p1) then chan.Network.Terrestrial.Constellation := TTerrestrialConstellation(p1.IntValue);
            p1 := e3.Properties.ItemNamed['hi'];
            if Assigned(p1) then chan.Network.Terrestrial.HierarchyInformation := TTerrestrialHierarchyInformation(p1.IntValue);
            p1 := e3.Properties.ItemNamed['crhp'];
            if Assigned(p1) then chan.Network.Terrestrial.CodeRateHPStream := TTerrestrialCodeRate(p1.IntValue);
            p1 := e3.Properties.ItemNamed['crlp'];
            if Assigned(p1) then chan.Network.Terrestrial.CodeRateLPStream := TTerrestrialCodeRate(p1.IntValue);
            p1 := e3.Properties.ItemNamed['gi'];
            if Assigned(p1) then chan.Network.Terrestrial.GuardInterval := TTerrestrialGuardInterval(p1.IntValue);
            p1 := e3.Properties.ItemNamed['tm'];
            if Assigned(p1) then chan.Network.Terrestrial.TransmissionMode := TTerrestrialTransmissionMode(p1.IntValue);
            p1 := e3.Properties.ItemNamed['off'];
            if Assigned(p1) then chan.Network.Terrestrial.OtherFrequencyFlag := p1.IntValue;
          end;
          ntDVBS:
          begin
            p1 := e3.Properties.ItemNamed['f'];
            if Assigned(p1) then chan.Network.Satellite.Frequency := p1.IntValue;
            p1 := e3.Properties.ItemNamed['op'];
            if Assigned(p1) then chan.Network.Satellite.OrbitalPosition := p1.IntValue;
            p1 := e3.Properties.ItemNamed['wef'];
            if Assigned(p1) then chan.Network.Satellite.WestEastFlag := p1.IntValue;
            p1 := e3.Properties.ItemNamed['p'];
            if Assigned(p1) then chan.Network.Satellite.Polarization := TSatellitePolarization(p1.IntValue);
            p1 := e3.Properties.ItemNamed['mod'];
            if Assigned(p1) then chan.Network.Satellite.Modulation := TSatelliteModulation(p1.IntValue);
            p1 := e3.Properties.ItemNamed['sr'];
            if Assigned(p1) then chan.Network.Satellite.SymbolRate := p1.IntValue;
            p1 := e3.Properties.ItemNamed['feci'];
            if Assigned(p1) then chan.Network.Satellite.FECInner := TFECInner(p1.IntValue);
          end;
          ntDVBC:
          begin
            p1 := e3.Properties.ItemNamed['f'];
            if Assigned(p1) then chan.Network.Cable.Frequency := p1.IntValue;
            p1 := e3.Properties.ItemNamed['feco'];
            if Assigned(p1) then chan.Network.Cable.FECOuter := TFECOuter(p1.IntValue);
            p1 := e3.Properties.ItemNamed['mod'];
            if Assigned(p1) then chan.Network.Cable.Modulation := TCableModulation(p1.IntValue);
            p1 := e3.Properties.ItemNamed['sr'];
            if Assigned(p1) then chan.Network.Cable.SymbolRate := p1.IntValue;
            p1 := e3.Properties.ItemNamed['feci'];
            if Assigned(p1) then chan.Network.Cable.FECInner := TFECInner(p1.IntValue);
          end;
        end;
      end;

      e3 := e2.Items.ItemNamed['streams'];
      if Assigned(e3) then
      begin
        for c := 0 to e3.Items.Count -1 do
        begin
          e4 := e3.Items[c];
          p1 := e4.Properties.ItemNamed['type'];
          if not Assigned(p1)
            then Continue;

          case TDVBChannelStreamType(p1.IntValue) of
            stAudio:    stream := TDVBAudioStream.Create;
            stTeletext: stream := TDVBTeletextStream.Create;
            stSubtitle: stream := TDVBSubtitleStream.Create;
            stVideo   : stream := TDVBVideoStream.Create;
            else        stream := TDVBBaseStream.Create;
          end;
          stream.StreamType := TDVBChannelStreamType(p1.IntValue);

          p1 := e4.Properties.ItemNamed['name'];
          if Assigned(p1) then stream.Name := p1.Value;
          p1 := e4.Properties.ItemNamed['default'];
          if Assigned(p1) then stream.Default := Boolean(p1.IntValue);
          p1 := e4.Properties.ItemNamed['pid'];
          if Assigned(p1) then stream.PID := p1.IntValue;
          p1 := e4.Properties.ItemNamed['tag'];
          if Assigned(p1) then stream.Tag := p1.IntValue;

          case stream.StreamType of
            stAudio:
            begin
              with TDVBAudioStream(stream) do
              begin
                p1 := e4.Properties.ItemNamed['lng'];
                if Assigned(p1) then Language := PISO6392LanguageCode(PChar(String(p1.Value)))^;
                p1 := e4.Properties.ItemNamed['code'];
                if Assigned(p1) then Coding := TDVBAudioStreamCoding(p1.IntValue);
                p1 := e4.Properties.ItemNamed['at'];
                if Assigned(p1) then AudioType := TISO639LanguageDescriptorAudioType(p1.IntValue);
              end;
            end;
            stVideo:
            begin
              with TDVBVideoStream(stream) do
              begin
                p1 := e4.Properties.ItemNamed['code'];
                if Assigned(p1) then Coding := TDVBVideoStreamCoding(p1.IntValue);
              end;
            end;
            stTeletext:
            begin
              with TDVBTeletextStream(stream) do
              begin
                p1 := e4.Properties.ItemNamed['lng'];
                if Assigned(p1) then Language := PISO6392LanguageCode(PChar(String(p1.Value)))^;
              end;
            end;
            stSubtitle:
            begin
              with TDVBSubtitleStream(stream) do
              begin
                for k := 0 to e4.Items.Count -1 do
                begin
                  e5 := e4.Items[k];
                  p1 := e5.Properties.ItemNamed['lng'];
                  p2 := e5.Properties.ItemNamed['st'];
                  p3 := e5.Properties.ItemNamed['cp'];
                  p4 := e5.Properties.ItemNamed['ap'];
                  p5 := e5.Properties.ItemNamed['default'];
                  if Assigned(p1) and Assigned(p2) and Assigned(p3) and Assigned(p4) and Assigned(p5) then
                  begin
                    AddItem(p1.Value, p2.IntValue, p3.IntValue, p4.IntValue, Boolean(p5.IntValue));
                  end;
                end;
              end;
            end;
          end;

          chan.Streams.Add(stream);
        end;
      end;

      FChannelList.Add(chan);
      item := ListView1.Items.Add;
      item.Caption := inttostr(ListView1.Items.Count);
      item.SubItems.Add(RemoveUnwantedChars(chan.Name));
      item.SubItems.Add(RemoveUnwantedChars(chan.Network.Name));
      item.SubItems.Add(RemoveUnwantedChars(chan.Provider));
    end;
  end;

  xml.Free;
  ComboBox1Change(Self);
end;

procedure TfrmScan.Load1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmScan.EnableButtons;
begin
  Button1.Enabled := ListView1.Items.Count > 0;
  Button5.Enabled := ListView1.SelCount > 0;
  Button8.Enabled := ListView1.SelCount > 0;
  Button7.Enabled := (ListView1.SelCount > 0) and (ListView1.Selected.Index < ListView1.Items.Count -1);
  Button6.Enabled := (ListView1.SelCount > 0) and (ListView1.Selected.Index > 0);
end;

procedure TfrmScan.UpdateChannelNumber;
var
  i: Integer;
begin
  for i := 0 to ListView1.Items.Count -1 do
  begin
    ListView1.Items[i].Caption := inttostr(i+1);
  end;
end;

procedure TfrmScan.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  EnableButtons;
end;

procedure TfrmScan.ListView1DblClick(Sender: TObject);
begin
  Button5Click(Self);
end;

procedure TfrmScan.Button4Click(Sender: TObject);
var
  form: TfrmAddChannel;
  chan: TDVBChannel;
  item: TListItem;
begin
  chan := TDVBChannel.Create;
  form := TfrmAddChannel.Create(Self, chan);
  form.Caption := 'Add Channel';
  form.Icon := Icon;
  form.ShowModal;
  if form.ModalResult = mrCancel then
  begin
    chan.Free;
    form.Free;
    Exit;
  end;

  FChannelList.Add(chan);
  item := ListView1.Items.Add;
  item.Caption := inttostr(ListView1.Items.Count);
  item.SubItems.Add(RemoveUnwantedChars(chan.Name));
  item.SubItems.Add(RemoveUnwantedChars(chan.Network.Name));
  item.SubItems.Add(RemoveUnwantedChars(chan.Provider));

  form.Free;
  EnableButtons;
end;

procedure TfrmScan.Button5Click(Sender: TObject);
var
  form: TfrmAddChannel;
  chan: TDVBChannel;
  idx: Integer;
begin
  if ListView1.SelCount = 0
    then Exit;

  idx := ListView1.Selected.Index;
  chan := TDVBChannel.Create;
  chan.Assign(FChannelList[idx]);
  form := TfrmAddChannel.Create(Self, chan);
  form.Caption := 'Edit Channel';
  form.Icon := Icon;
  try
    form.ShowModal;
    if form.ModalResult = mrCancel
      then Exit;

    FChannelList[idx].Assign(chan);

    ListView1.Selected.SubItems.Strings[0] := RemoveUnwantedChars(chan.Name);
    ListView1.Selected.SubItems.Strings[1] := RemoveUnwantedChars(chan.Network.Name);
    ListView1.Selected.SubItems.Strings[2] := RemoveUnwantedChars(chan.Provider);
  finally
    chan.Free;
    form.Free;
  end;
end;

procedure TfrmScan.Button8Click(Sender: TObject);
begin
  if ListView1.SelCount = 0
    then Exit;

  FChannelList.Delete(ListView1.Selected.Index);
  ListView1.Items.Delete(ListView1.Selected.Index);

  EnableButtons;
  UpdateChannelNumber;
end;

procedure TfrmScan.Button1Click(Sender: TObject);
begin
  FChannelList.Clear;
  ListView1.Clear;
  EnableButtons;
end;

procedure TfrmScan.Button7Click(Sender: TObject);
var
  idx: Integer;
begin
  if ListView1.SelCount = 0
    then Exit;

  idx := ListView1.Selected.Index;

  ExchangeItems(ListView1, idx, idx+1);
  FChannelList.Move(idx, idx+1);
  ListView1.Items[idx+1].Selected := True;
  EnableButtons;
  UpdateChannelNumber;
end;

procedure TfrmScan.Button6Click(Sender: TObject);
var
  idx: Integer;
begin
  if ListView1.SelCount = 0
    then Exit;

  idx := ListView1.Selected.Index;

  ExchangeItems(ListView1, idx, idx-1);
  ListView1.Items[idx-1].Selected := True;
  FChannelList.Move(idx, idx-1);
  EnableButtons;
  UpdateChannelNumber;
end;

procedure TfrmScan.ComboBox2Change(Sender: TObject);
var
  i: Integer;
begin
  ComboBox6.Clear;
  ListView2.Visible := False;
  ListView3.Visible := False;
  ListView4.Visible := False;
  ListView5.Visible := False;

  ListView2.Clear;
  ListView3.Clear;
  ListView4.Clear;
  ListView5.Clear;

  if ComboBox2.ItemIndex < 0
    then Exit;

  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      for i := 0 to FFrequencys.DVBTCount -1 do
      begin
        ComboBox6.Items.Add(FFrequencys.DVBTFrequency[i].Name);
      end;
      ListView2.Visible := True;
    end;
    1: // DVB-c
    begin
      for i := 0 to FFrequencys.DVBCCount -1 do
      begin
        ComboBox6.Items.Add(FFrequencys.DVBCFrequency[i].Name);
      end;
      ListView3.Visible := True;
    end;
    2: // DVB-s
    begin
      for i := 0 to FFrequencys.DVBSCount -1 do
      begin
        ComboBox6.Items.Add(FFrequencys.DVBSFrequency[i].Name);
      end;
      ListView4.Visible := True;
    end;
    3: // ATSC
    begin
      for i := 0 to FFrequencys.ATSCCount -1 do
      begin
        ComboBox6.Items.Add(FFrequencys.ATSCFrequency[i].Name);
      end;
      ListView5.Visible := True;
    end;
  end;

  if ComboBox6.Items.Count > 0 then
  begin
    ComboBox6.ItemIndex := 0;
    ComboBox6Change(Self);
  end else
  begin
    Button2.Enabled := False;
    Button12.Enabled := False;
  end;

  CheckCountryButton;
end;

procedure TfrmScan.ComboBox6Change(Sender: TObject);
var
  i: Integer;
  item: TListItem;
begin
  ListView2.Clear;
  ListView3.Clear;
  ListView4.Clear;
  ListView5.Clear;

  EnableButtons2;
  Button12.Enabled := ComboBox6.ItemIndex >= 0;
  Button2.Enabled := ComboBox6.ItemIndex >= 0;

  if (ComboBox6.ItemIndex < 0) or (ComboBox2.ItemIndex < 0)
    then Exit;

  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      for i := 0 to FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].Count -1 do
      begin
        item := ListView2.Items.Add;
        item.Caption := inttostr(FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].Frequency[i].Frequency) + ' kHz';
        item.SubItems.Add(inttostr(FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].Frequency[i].Bandwidth) + ' MHz');
      end;
    end;
    1: // DVB-c
    begin
      for i := 0 to FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].Count -1 do
      begin
        item := ListView3.Items.Add;
        item.Caption := inttostr(FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].Frequency[i].Frequency) + ' kHz';
        item.SubItems.Add(inttostr(FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].Frequency[i].Symbolrate));
      end;
    end;
    2: // DVB-s
    begin
      for i := 0 to FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Count -1 do
      begin
        item := ListView4.Items.Add;
        item.Caption := inttostr(FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Frequency[i].Frequency) + ' kHz';
        item.SubItems.Add(inttostr(FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Frequency[i].Symbolrate));
        item.SubItems.Add(IfThen(FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Frequency[i].Polarization = 0, 'Horizontal', 'Vertical'));
      end;
    end;
    3: // ATSC
    begin
      for i := 0 to FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].Count -1 do
      begin
        item := ListView5.Items.Add;
        item.Caption := inttostr(FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].Frequency[i].Frequency) + ' kHz';
      end;
    end;
  end;

  EnableButtons2;
end;

procedure TfrmScan.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog3.Execute
    then Edit4.Text := OpenDialog3.FileName;
end;

procedure TfrmScan.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmScan.Button14Click(Sender: TObject);
var
  stream: TFileStream;
  buffer: array[0..1024] of Byte;
  readen: Integer;
  scanner: TDVBScanner;
  i: Integer;
begin
  if FIsScanning then
  begin
    if FTCPClient.IsConnected then
    begin
      Button14.Caption := 'Start Scanning';
      ProgressBar1.Position := 0;
      AddText(' ');
      AddText('Scanning finished');
      FTCPClient.Close;
      FIsScanning := False;
      FAbort := False;
    end else
    if FMulticastClient.Active then
    begin
      Button14.Caption := 'Start Scanning';
      ProgressBar1.Position := 0;
      AddText(' ');
      AddText('Scanning finished');
      FMulticastClient.Active := False;
      FIsScanning := False;
      FAbort := False;
    end else
    if FDCDVBScanner.Active and Assigned(FThread) then
    begin
      FThread.Terminate;
//      FThread.WaitFor;
//      FThread.Free;
      FThread := nil;
      Button14.Caption := 'Start Scanning';
      ProgressBar1.Position := 0;
      AddText(' ');
      AddText('Scanning aborted');
      FDCDVBScanner.Active := False;
      FIsScanning := False;
      FAbort := False;
    end else
    begin
      Button14.Caption := 'Start Scanning';
      ProgressBar1.Position := 0;
      AddText(' ');
      AddText('Scanning finished');
      FTCPClient.Close;
      FIsScanning := False;
      FAbort := True;
    end;
  end else
  begin
    RichEdit1.Clear;
    ProgressBar1.Position := 0;

    if RadioButton5.Checked then // File
    begin
      ProgressBar1.Max := 100;
      FIsScanning := True;
      FAbort := False;
      Button14.Caption := 'Stop Scanning';

      AddText('Scanning from File');
      if not FileExists(Edit4.Text) then
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: File does not exist');
        FIsScanning := False;
        Exit;
      end;

      try
        stream := TFileStream.Create(Edit4.Text, fmOpenRead or fmShareDenyNone);
      except on E: Exception do
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: ' + E.Message);
        FIsScanning := False;
        Exit;
      end;
      end;
      scanner := TDVBScanner.Create(FLog);

      scanner.DataParser.PrintPAT := FScanner.DataParser.PrintPAT;
      scanner.DataParser.PrintPMT := FScanner.DataParser.PrintPMT;
      scanner.DataParser.PrintNIT := FScanner.DataParser.PrintNIT;
      scanner.DataParser.PrintSDT := FScanner.DataParser.PrintSDT;
      scanner.DataParser.PrintCAT := FScanner.DataParser.PrintCAT;
      scanner.DataParser.PrintBAT := FScanner.DataParser.PrintBAT;
      scanner.DataParser.PrintEIT := FScanner.DataParser.PrintEIT;

      readen := stream.Read(buffer, 1024);
      while (readen > 0) and not FAbort and not scanner.DataParser.Valid do
      begin
        if stream.Size > 0
          then ProgressBar1.Position := stream.Position * ProgressBar1.Max div stream.Size;
        Application.ProcessMessages;
        scanner.ParseBuffer(@buffer, readen);
        readen := stream.Read(buffer, 1024);
      end;

      if scanner.ChannelList.Count = 0 then
      begin
        AddText('No Channels found');
      end else
      begin
        AddText(' ');
      end;

      for i := 0 to scanner.ChannelList.Count -1 do
      begin
        OnChannelFound(scanner.ChannelList[i]);
        AddBoldText('Found', RemoveUnwantedChars(scanner.ChannelList[i].Name));
      end;

      scanner.Free;
      stream.Free;
      AddText(' ');
      if FAbort
        then AddText('Scanning aborted')
        else AddText('Scanning finished');

      Button14.Caption := 'Start Scanning';
      ProgressBar1.Position := 0;
      FIsScanning := False;
    end else
    if RadioButton4.Checked then
    begin
      if ComboBox11.ItemIndex = 0 then  // Multicast
      begin
        ProgressBar1.Max := 100;
        FIsScanning := True;
        FScanner.Clear;
        FAbort := False;
        Button14.Caption := 'Stop Scanning';
        AddText('Scanning from Multicast Stream');

        FMulticastClient.MulticastGroup := Edit3.Text;
        FMulticastClient.Port := SpinEdit11.Value;
        FMulticastClient.OnReadData := OnMulticastData;
        FMulticastClient.Active := True;

        if not FMulticastClient.Active then
        begin
          Button14.Caption := 'Start Scanning';
          AddText('ERROR: Could not join Multicast Group');
          FIsScanning := False;
          Exit;
        end;
      end else // TCP
      begin
        ProgressBar1.Max := 100;
        FIsScanning := True;
        FScanner.Clear;
        FAbort := False;
        Button14.Caption := 'Stop Scanning';
        AddText('Scanning from TCP Stream');

        FTCPClient.Host := Edit3.Text;
        FTCPClient.Port := SpinEdit11.Value;
        FTCPClient.OnReadData := OnMulticastData;
        FTCPClient.Connect;

        if not FTCPClient.IsConnected then
        begin
          Button14.Caption := 'Start Scanning';
          AddText('ERROR: Could not connect to Server');
          FIsScanning := False;
          Exit;
        end;
      end;
    end else
    begin
      FIsScanning := True;
      FScanner.Clear;
      FAbort := False;
      Button14.Caption := 'Stop Scanning';

      AddText('Scanning from Device');

      if ComboBox8.ItemIndex < 0 then
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: No Frequency List selected');
        FIsScanning := False;
        Exit;
      end;

      if ComboBox7.ItemIndex < 0 then
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: No BDA Device selected');
        FIsScanning := False;
        Exit;
      end;

      if FDevices.DeviceType[ComboBox7.ItemIndex] = DCDVB_TUNING_PLUGIN_DEVICE_TYPE_UNKNOWN then
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: BDA Device Type is Unknown');
        FIsScanning := False;
        Exit;
      end;

      FThread := TScanningThread.Create(Self);

      case FDevices.DeviceType[ComboBox7.ItemIndex] of
        DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBT:
        begin
          if FFrequencys.DVBTFrequency[ComboBox8.ItemIndex].Count = 0 then
          begin
            Button14.Caption := 'Start Scanning';
            AddText('ERROR: Frequency List is empty');
            FreeAndNil(FThread);
            FIsScanning := False;
            Exit;
          end;
          for i := 0 to FFrequencys.DVBTFrequency[ComboBox8.ItemIndex].Count -1 do
          begin
            FThread.FFrequencies.AddFrom(FFrequencys.DVBTFrequency[ComboBox8.ItemIndex].Frequency[i]);
          end;
        end;
        DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBC:
        begin
          if FFrequencys.DVBCFrequency[ComboBox8.ItemIndex].Count = 0 then
          begin
            Button14.Caption := 'Start Scanning';
            AddText('ERROR: Frequency List is empty');
            FreeAndNil(FThread);
            FIsScanning := False;
            Exit;
          end;
          for i := 0 to FFrequencys.DVBCFrequency[ComboBox8.ItemIndex].Count -1 do
          begin
            FThread.FFrequencies.AddFrom(FFrequencys.DVBCFrequency[ComboBox8.ItemIndex].Frequency[i]);
          end;
        end;
        DCDVB_TUNING_PLUGIN_DEVICE_TYPE_DVBS:
        begin
          if FFrequencys.DVBSFrequency[ComboBox8.ItemIndex].Count = 0 then
          begin
            Button14.Caption := 'Start Scanning';
            AddText('ERROR: Frequency List is empty');
            FIsScanning := False;
            FreeAndNil(FThread);
            Exit;
          end;
          for i := 0 to FFrequencys.DVBSFrequency[ComboBox8.ItemIndex].Count -1 do
          begin
            FThread.FFrequencies.AddFrom(FFrequencys.DVBSFrequency[ComboBox8.ItemIndex].Frequency[i]);
          end;
        end;
        DCDVB_TUNING_PLUGIN_DEVICE_TYPE_ATSC:
        begin
          if FFrequencys.ATSCFrequency[ComboBox8.ItemIndex].Count = 0 then
          begin
            Button14.Caption := 'Start Scanning';
            AddText('ERROR: Frequency List is empty');
            FIsScanning := False;
            FreeAndNil(FThread);
            Exit;
          end;
          for i := 0 to FFrequencys.ATSCFrequency[ComboBox8.ItemIndex].Count -1 do
          begin
            FThread.FFrequencies.AddFrom(FFrequencys.ATSCFrequency[ComboBox8.ItemIndex].Frequency[i]);
          end;
        end;
      end;

      ProgressBar1.Max := FThread.FFrequencies.Count;

      AddText('Activating the Tuning Plugin');
      FDCDVBScanner.Device := FDevices[ComboBox7.ItemIndex];
      FDCDVBScanner.Active := True;
      if not FDCDVBScanner.Active then
      begin
        Button14.Caption := 'Start Scanning';
        AddText('ERROR: Activating the Tuning Plugin failed');
        FreeAndNil(FThread);
        FIsScanning := False;
        Exit;
      end;

      AddText('Starting the Scan');
      AddText(' ');
      FThread.Resume;
//      sleep(500);
    end;
  end;
end;

procedure TfrmScan.OnChannelFound(AChannel: TDVBChannel);
var
  item: TListItem;
begin
  FChannelList.Add(TDVBChannel.Create(AChannel));

  item := ListView1.Items.Add;
  item.Caption := inttostr(ListView1.Items.Count);
  item.SubItems.Add(RemoveUnwantedChars(AChannel.Name));
  item.SubItems.Add(RemoveUnwantedChars(AChannel.Network.Name));
  item.SubItems.Add(RemoveUnwantedChars(AChannel.Provider));
end;

procedure TfrmScan.OnMulticastData(ABuffer: PByte; ASize: Integer);
var
  i: Integer;
begin
  FScanner.ParseBuffer(ABuffer, ASize);
  if FScanner.DataParser.Valid then
  begin
    if FScanner.ChannelList.Count > 0
      then AddText(' ');
      
    for i := 0 to FScanner.ChannelList.Count -1 do
    begin
      OnChannelFound(FScanner.ChannelList[i]);
      AddBoldText('Found', RemoveUnwantedChars(FScanner.ChannelList[i].Name));
    end;
    FScanner.Clear;
  end;
end;

procedure TfrmScan.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FIsScanning;
end;

procedure TfrmScan.CheckBox9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  if CheckBox9.Checked
//    then ShowMessage('Direct Tuning enables faster Tuning to Frequencies,'+#13#10+'but doesn''t work with every Hardware!');
end;

procedure TfrmScan.Label10Click(Sender: TObject);
begin
  RadioButton3.Checked := True;
  RadioButton3Click(Self);
end;

procedure TfrmScan.OnBDAData(ABuffer: PByte; ASize: Integer);
begin
  FScanner.ParseBuffer(ABuffer, ASize);
end;

procedure TfrmScan.CheckCountryButton;
begin
  Button13.Enabled := ComboBox6.ItemIndex > -1;
end;

procedure TfrmScan.Button10Click(Sender: TObject);
var
  lname: String;
begin
  lname := InputBox('New Frequecy List', 'Please enter the Name for the List', '');
  if lname = ''
    then Exit;

  case ComboBox2.ItemIndex of
    0: FFrequencys.AddDVBT(lname);
    1: FFrequencys.AddDVBC(lname);
    2: FFrequencys.AddDVBS(lname);
    3: FFrequencys.AddATSC(lname);
  end;

  ComboBox7Change(Self);
  ComboBox2Change(Self);
  CheckCountryButton;
end;

procedure TfrmScan.Button13Click(Sender: TObject);
begin
  if MessageDlg('Really delete this Frequency List?', mtConfirmation, [mbYes, mbNo], 0) <> MRYES
    then Exit;

  case ComboBox2.ItemIndex of
    0: FFrequencys.DeleteDVBT(ComboBox6.ItemIndex);
    1: FFrequencys.DeleteDVBC(ComboBox6.ItemIndex);
    2: FFrequencys.DeleteDVBS(ComboBox6.ItemIndex);
    3: FFrequencys.DeleteATSC(ComboBox6.ItemIndex);
  end;

  ComboBox7Change(Self);
  ComboBox2Change(Self);
  CheckCountryButton;
end;

procedure TfrmScan.Button9Click(Sender: TObject);
var
  xml: TJvSimpleXml;
  a1, a2, a3: TJvSimpleXmlElem;
  c, i: Integer;
begin
  if MessageDlg('This will override the List. Do you really want to do this?', mtConfirmation, [mbYes, mbNo], 0) <> MRYES
    then Exit;

  xml := TJvSimpleXml.Create(nil);
  xml.Root.Name := 'networks';

  a1 := xml.Root.Items.Add('network');
  a1.Properties.Add('name', 'DVBT');
  for i := 0 to FFrequencys.DVBTCount -1 do
  begin
    a2:= a1.Items.Add('frequencies');
    a2.Properties.Add('name', FFrequencys.DVBTFrequency[i].Name);
    for c := 0 to FFrequencys.DVBTFrequency[i].Count -1 do
    begin
      a3 := a2.Items.Add('frequency');
      a3.Properties.Add('frequency', FFrequencys.DVBTFrequency[i].Frequency[c].Frequency);
      a3.Properties.Add('bandwidth', FFrequencys.DVBTFrequency[i].Frequency[c].Bandwidth);
    end;
  end;

  a1 := xml.Root.Items.Add('network');
  a1.Properties.Add('name', 'DVBS');
  for i := 0 to FFrequencys.DVBSCount -1 do
  begin
    a2:= a1.Items.Add('frequencies');
    a2.Properties.Add('name', FFrequencys.DVBSFrequency[i].Name);
    for c := 0 to FFrequencys.DVBSFrequency[i].Count -1 do
    begin
      a3 := a2.Items.Add('frequency');
      a3.Properties.Add('frequency', FFrequencys.DVBSFrequency[i].Frequency[c].Frequency);
      a3.Properties.Add('polarization', FFrequencys.DVBSFrequency[i].Frequency[c].Polarization);
      a3.Properties.Add('symbolrate', FFrequencys.DVBSFrequency[i].Frequency[c].Symbolrate);
    end;
  end;

  a1 := xml.Root.Items.Add('network');
  a1.Properties.Add('name', 'DVBC');
  for i := 0 to FFrequencys.DVBCCount -1 do
  begin
    a2:= a1.Items.Add('frequencies');
    a2.Properties.Add('name', FFrequencys.DVBCFrequency[i].Name);
    for c := 0 to FFrequencys.DVBCFrequency[i].Count -1 do
    begin
      a3 := a2.Items.Add('frequency');
      a3.Properties.Add('frequency', FFrequencys.DVBCFrequency[i].Frequency[c].Frequency);
      a3.Properties.Add('symbolrate', FFrequencys.DVBCFrequency[i].Frequency[c].Symbolrate);
    end;
  end;

  a1 := xml.Root.Items.Add('network');
  a1.Properties.Add('name', 'ATSC');
  for i := 0 to FFrequencys.ATSCCount -1 do
  begin
    a2:= a1.Items.Add('frequencies');
    a2.Properties.Add('name', FFrequencys.ATSCFrequency[i].Name);
    for c := 0 to FFrequencys.ATSCFrequency[i].Count -1 do
    begin
      a3 := a2.Items.Add('frequency');
      a3.Properties.Add('frequency', FFrequencys.ATSCFrequency[i].Frequency[c].Frequency);
    end;
  end;

  xml.SaveToFile(GetCurrentDirectory + 'Frequencies.xml');
  xml.Free;
end;

procedure TfrmScan.Copy1Click(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

procedure TfrmScan.PopupMenu1Popup(Sender: TObject);
begin
  Copy1.Enabled := RichEdit1.SelLength > 0;
end;

procedure TfrmScan.PrintAll1Click(Sender: TObject);
begin
  PrintPAT1.Checked := True;
  PrintPMT1.Checked := True;
  PrintNIT1.Checked := True;
  PrintSDT1.Checked := True;
  PrintCAT1.Checked := True;
  PrintBAT1.Checked := True;
  PrintEIT1.Checked := True;

  FScanner.DataParser.PrintPAT := True;
  FScanner.DataParser.PrintPMT := True;
  FScanner.DataParser.PrintNIT := True;
  FScanner.DataParser.PrintSDT := True;
  FScanner.DataParser.PrintCAT := True;
  FScanner.DataParser.PrintBAT := True;
  FScanner.DataParser.PrintEIT := True;
end;

procedure TfrmScan.PrintBAT1Click(Sender: TObject);
begin
  PrintBAT1.Checked := not PrintBAT1.Checked;
  FScanner.DataParser.PrintBAT := PrintBAT1.Checked;
end;

procedure TfrmScan.PrintCAT1Click(Sender: TObject);
begin
  PrintCAT1.Checked := not PrintCAT1.Checked;
  FScanner.DataParser.PrintCAT := PrintCAT1.Checked;
end;

procedure TfrmScan.PrintEIT1Click(Sender: TObject);
begin
  PrintEIT1.Checked := not PrintEIT1.Checked;
  FScanner.DataParser.PrintEIT := PrintEIT1.Checked;
end;

procedure TfrmScan.PrintNIT1Click(Sender: TObject);
begin
  PrintNIT1.Checked := not PrintNIT1.Checked;
  FScanner.DataParser.PrintNIT := PrintNIT1.Checked;
end;

procedure TfrmScan.PrintNone1Click(Sender: TObject);
begin
  PrintPAT1.Checked := False;
  PrintPMT1.Checked := False;
  PrintNIT1.Checked := False;
  PrintSDT1.Checked := False;
  PrintCAT1.Checked := False;
  PrintBAT1.Checked := False;
  PrintEIT1.Checked := False;

  FScanner.DataParser.PrintPAT := False;
  FScanner.DataParser.PrintPMT := False;
  FScanner.DataParser.PrintNIT := False;
  FScanner.DataParser.PrintSDT := False;
  FScanner.DataParser.PrintCAT := False;
  FScanner.DataParser.PrintBAT := False;
  FScanner.DataParser.PrintEIT := False;
end;

procedure TfrmScan.PrintPAT1Click(Sender: TObject);
begin
  PrintPAT1.Checked := not PrintPAT1.Checked;
  FScanner.DataParser.PrintPAT := PrintPAT1.Checked;
end;

procedure TfrmScan.PrintPMT1Click(Sender: TObject);
begin
  PrintPMT1.Checked := not PrintPMT1.Checked;
  FScanner.DataParser.PrintPMT := PrintPMT1.Checked;
end;

procedure TfrmScan.PrintSDT1Click(Sender: TObject);
begin
  PrintSDT1.Checked := not PrintSDT1.Checked;
  FScanner.DataParser.PrintSDT := PrintSDT1.Checked;
end;

procedure TfrmScan.AddText(AText: String);
begin
  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := 0;
  RichEdit1.SelAttributes.Style := richedit1.SelAttributes.Style - [fsBold];
  RichEdit1.Lines.Add(' ' + AText);
  RichEdit1.Perform(EM_SCROLL,SB_LINEDOWN,0);
end;

procedure TfrmScan.AddBoldText(AText, AText2:  String);
begin
  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := 0;
  RichEdit1.SelAttributes.Style := RichEdit1.SelAttributes.Style - [fsBold];
  richedit1.Lines.Add('   ' + AText + ' ... ' + AText2);
  if Length(AText2) > 0 then
  begin
    RichEdit1.SelStart := Length(RichEdit1.Text) - Length(AText2)- 2;
    RichEdit1.SelLength := Length(AText2);
    RichEdit1.SelAttributes.Style := RichEdit1.SelAttributes.Style + [fsBold];
  end;
  RichEdit1.Perform(EM_SCROLL, SB_LINEDOWN, 0);
end;

procedure TfrmScan.AddPoint;
begin
  RichEdit1.SelStart := RichEdit1.GetTextLen -1;
  richedit1.SelText := '.';
  RichEdit1.SelAttributes.Style := RichEdit1.SelAttributes.Style - [fsBold];
  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := 0;
end;

procedure TfrmScan.ReplaceText(AText: String);
begin
  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := 0;
  RichEdit1.SelAttributes.Style := RichEdit1.SelAttributes.Style - [fsBold];
  RichEdit1.Lines[RichEdit1.Lines.Count -1] := ' ' + AText;
end;

procedure TfrmScan.ScanfromPSIDump1Click(Sender: TObject);
const
  SCAN_TIMES = 1;
var
  path: String;
  rec: TSearchRec;
  stream: TFileStream;
  buf: PByte;
  i: Integer;
begin
  if OpenDialog4.Execute then
  begin
    path := ExtractFilePath(OpenDialog4.FileName);
    FScanner.Clear;

    for i := 0 to SCAN_TIMES -1 do
    begin
      if FindFirst(path + '*.bin', faAnyFile - faDirectory, rec) = 0 then
      begin
        repeat
          stream := TFileStream.Create(path + rec.Name, fmOpenRead);
          buf := AllocMem(stream.Size);
          stream.Read(buf^, stream.Size);
          FScanner.ParsePSIBuffer(buf, stream.Size);
          FreeMem(Buf);
          stream.Free;
        until (FindNext(rec) <> 0);
      end;
    end;

//    if FScanner.DataParser.Valid then
//    begin
      for i := 0 to FScanner.ChannelList.Count -1 do
      begin
        OnChannelFound(FScanner.ChannelList[i]);
        AddBoldText('Found', RemoveUnwantedChars(FScanner.ChannelList[i].Name));
      end;
//    end;
  end;
end;

procedure TfrmScan.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
begin
  Log(FLog, Self, 'Exception', E.Message);
end;

procedure TfrmScan.EnableButtons2;
var
  list: TListView;
begin
  case ComboBox2.ItemIndex of
    0: list := ListView2;
    1: list := ListView3;
    2: list := ListView4;
    3: list := ListView5;
    else
    begin
      Button3.Enabled := False;
      Button11.Enabled := False;
      Button12.Enabled := False;
      Exit;
    end;
  end;

  Button12.Enabled := list.Items.Count > 0;
  Button3.Enabled := list.SelCount > 0;
  Button11.Enabled := list.SelCount > 0;
end;

procedure TfrmScan.ListView4Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  EnableButtons2;
end;

procedure TfrmScan.Button11Click(Sender: TObject);
var
  list: TListView;
begin
  case ComboBox2.ItemIndex of
    0: list := ListView2;
    1: list := ListView3;
    2: list := ListView4;
    3: list := ListView5;
    else Exit
  end;

  if list.Selected = nil
    then Exit;

  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].Delete(list.Selected.Index);
    end;
    1: // DVB-c
    begin
      FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].Delete(list.Selected.Index);
    end;
    2: // DVB-s
    begin
      FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Delete(list.Selected.Index);
    end;
    3: // ATSC
    begin
      FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].Delete(list.Selected.Index);
    end;
  end;

  ComboBox6Change(Self);
end;

procedure TfrmScan.Button12Click(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].DeleteAll;
    end;
    1: // DVB-c
    begin
      FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].DeleteAll;
    end;
    2: // DVB-s
    begin
      FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].DeleteAll;
    end;
    3: // ATSC
    begin
      FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].DeleteAll;
    end;
  end;

  ComboBox6Change(Self);
end;

procedure TfrmScan.ListView4DblClick(Sender: TObject);
begin
  Button3Click(Self);
end;

procedure TfrmScan.Button2Click(Sender: TObject);
var
  frm: TfrmFrequency;
  freq: TDVBFrequency;
begin
  if ComboBox6.ItemIndex < 0
    then Exit;

  frm := TfrmFrequency.Create(Self);
  frm.SetupType(ComboBox2.ItemIndex);
  frm.Icon := Icon;
  frm.ShowModal;
  if frm.ModalResult = mrCancel then
  begin
    frm.Free;
    Exit;
  end;

  freq := TDVBFrequency.Create;
  freq.Frequency := frm.SpinEdit10.Value;
  freq.Bandwidth := frm.SpinEdit1.Value;
  freq.Polarization := frm.ComboBox1.ItemIndex;
  freq.Symbolrate := frm.SpinEdit2.Value;

  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      freq.NetworkType := ntDVBT;
      FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].AddFrom(freq);
    end;
    1: // DVB-c
    begin
      freq.NetworkType := ntDVBC;
      FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].AddFrom(freq);
    end;
    2: // DVB-s
    begin
      freq.NetworkType := ntDVBS;
      FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].AddFrom(freq);
    end;
    3: // ATSC
    begin
      freq.NetworkType := ntATSC;
      FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].AddFrom(freq);
    end;
  end;

  freq.Free;
  frm.Free;
  ComboBox6Change(Self);
end;

procedure TfrmScan.Button3Click(Sender: TObject);
var
  frm: TfrmFrequency;
  freq: TDVBFrequency;
  list: TListView;
begin
  case ComboBox2.ItemIndex of
    0: list := ListView2;
    1: list := ListView3;
    2: list := ListView4;
    3: list := ListView5;
    else Exit
  end;

  if ComboBox6.ItemIndex < 0
    then Exit;

  frm := TfrmFrequency.Create(Self);
  frm.SetupType(ComboBox2.ItemIndex);
  frm.Icon := Icon;
  freq := nil;
  
  case ComboBox2.ItemIndex of
    0: // DVB-t
    begin
      freq := FFrequencys.DVBTFrequency[ComboBox6.ItemIndex].Frequency[list.Selected.Index];
    end;
    1: // DVB-c
    begin
      freq := FFrequencys.DVBCFrequency[ComboBox6.ItemIndex].Frequency[list.Selected.Index];
    end;
    2: // DVB-s
    begin
      freq := FFrequencys.DVBSFrequency[ComboBox6.ItemIndex].Frequency[list.Selected.Index];
    end;
    3: // ATSC
    begin
      freq := FFrequencys.ATSCFrequency[ComboBox6.ItemIndex].Frequency[list.Selected.Index];
    end;
  end;

  frm.SpinEdit10.Value := freq.Frequency;
  frm.SpinEdit1.Value := freq.Bandwidth;
  frm.ComboBox1.ItemIndex := freq.Polarization;
  if frm.ComboBox1.ItemIndex < 0
    then frm.ComboBox1.ItemIndex := 0;
  frm.SpinEdit2.Value := freq.Symbolrate;

  frm.ShowModal;
  if frm.ModalResult = mrCancel then
  begin
    frm.Free;
    Exit;
  end;

  freq.Frequency := frm.SpinEdit10.Value;
  freq.Bandwidth := frm.SpinEdit1.Value;
  freq.Polarization := frm.ComboBox1.ItemIndex;
  freq.Symbolrate := frm.SpinEdit2.Value;

  frm.Free;
  ComboBox6Change(Self);
end;

procedure TfrmScan.Button15Click(Sender: TObject);
var
  frm: TfrmLNBSelection;
begin
  frm := TfrmLNBSelection.Create(Self);
  frm.Icon := Icon;
  frm.ShowModal;
  frm.Free;
end;

procedure TfrmScan.Button16Click(Sender: TObject);
var
  device: IDCDVBTuningPluginDevice;
  frm: TfrmPropertyPage;
  device_name: PChar;
begin
  if ComboBox1.ItemIndex < 0
    then Exit;

  device := FDevices.Device[ComboBox1.ItemIndex];
  if not Assigned(device)
    then Exit;

  device.get_DeviceName(device_name);
  frm := TfrmPropertyPage.CreateInstance(Self, device, device_name);
  frm.Icon := Icon;
  frm.ShowModal;
  frm.Free;
end;

procedure TfrmScan.Button17Click(Sender: TObject);
var
  device: IDCDVBTuningPluginDevice;
  frm: TfrmPropertyPage;
  device_name: PChar;
begin
  if ComboBox7.ItemIndex < 0
    then Exit;

  device := FDevices.Device[ComboBox7.ItemIndex];
  if not Assigned(device)
    then Exit;

  device.get_DeviceName(device_name);
  frm := TfrmPropertyPage.CreateInstance(Self, device, device_name);
  frm.Icon := Icon;
  frm.ShowModal;
  frm.Free;
end;

procedure TfrmScan.Button18Click(Sender: TObject);
var
  frm: TfrmOSDColor;
begin
  frm := TfrmOSDColor.Create(Self);
  frm.SetOSDColor(FOSDColor);
  frm.Icon := Icon;
  frm.ShowModal;

  if frm.ModalResult = mrCancel then
  begin
    frm.Free;
    Exit;
  end;

  FOSDColor := frm.GetOSDColor;
  frm.Free;
end;

end.
