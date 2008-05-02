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

unit formQuickAccess;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DirectShow9, Buttons, BDAUtils, ExtCtrls,
  BaseClass, DVBInterface, IDVBSource, ActiveX, Menus;

type
  TfrmQuickAccess = class(TForm)
    TabControl1: TTabControl;
    Image1: TImage;
    Timer1: TTimer;
    SpeedButton5: TSpeedButton;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    ComboBox2: TComboBox;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    SpeedButton27: TSpeedButton;
    Shape9: TShape;
    Label5: TLabel;
    Shape10: TShape;
    SpeedButton28: TSpeedButton;
    SpeedButton30: TSpeedButton;
    procedure SpeedButton27Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TabControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GroupBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GroupBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton18Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton24Click(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure DoubleLower1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    FMouseDown: Boolean;
    FX, FY: Integer;
    FTempWindow: THandle;
    FChannel: String;
    procedure TempPrc(var Message: TMessage);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
  public
    CanChangeChannels: Boolean;
    Filter: Pointer;
  end;

implementation

uses
  DVBFilter, StrUtils;

{$R *.dfm}
procedure TfrmQuickAccess.ComboBox1Change(Sender: TObject);
begin
  if not CanChangeChannels
    then Exit;

  TDCDVBSource(Filter).Enable(ComboBox1.ItemIndex, AMSTREAMSELECTENABLE_ENABLE);
end;

procedure TfrmQuickAccess.Button1Click(Sender: TObject);
begin
  if SpeedButton23.Down then
  begin
    TDCDVBSource(Filter).put_TeletextNumber((Sender as TSpeedButton).Tag);
  end else
  begin
    if ((Sender as TSpeedButton).Tag = 0) and (Length(FChannel) = 0)
      then Exit;

    Timer1.Enabled := False;

    if Length(FChannel) > 3 then
    begin
      if ((Sender as TSpeedButton).Tag = 0) then
      begin
        FChannel := '';
        Exit;
      end;
      FChannel := inttostr((Sender as TSpeedButton).Tag);
    end else
    begin
      FChannel := FChannel + inttostr((Sender as TSpeedButton).Tag);
    end;

    TDCDVBSource(Filter).put_OSDChannel(FChannel);
    Timer1.Enabled := True;
  end;
end;

procedure TfrmQuickAccess.TabControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
//  if Button = mbLeft then
//  begin
//    FX := X;
//    FY := Y;
//    FMouseDown := True;
//  end;
end;

procedure TfrmQuickAccess.GroupBox3MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  if FMouseDown then
  begin
    GetWindowRect(Handle, r);
    SetWindowpos(Handle, 0, r.Left + (X - FX), r.Top + (Y - FY), 0, 0, SWP_NOSIZE);
  end;
end;

procedure TfrmQuickAccess.GroupBox3MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;

function GetBytesString(ACountByte: Int64): String;
begin
  if ACountByte > 1073741824 then
  begin
    Result := Format('%.2f TB', [ACountByte / 1073741824]);
  end else
  if ACountByte > 1048576 then
  begin
    Result := Format('%.2f GB', [ACountByte / 1048576]);
  end else
  if ACountByte > 1024 then
  begin
    Result := Format('%.2f MB', [ACountByte / 1024]);
  end else
  begin
    Result := inttostr(ACountByte) + '.00 KB';
  end;
end;

procedure TfrmQuickAccess.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_BORDER;
end;

procedure TfrmQuickAccess.FormDestroy(Sender: TObject);
begin
  Classes.DeallocateHWnd(FTempWindow);
end;

procedure TfrmQuickAccess.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmQuickAccess.FormCreate(Sender: TObject);
begin
  // Temp Window acts as the Owner of the Main Form. That's the only way to
  // remove the Taskbar Entry without having to set the form to WS_EX_TOOLWINDOW
  FTempWindow := Classes.AllocateHWnd(TempPrc);
  ParentWindow := FTempWindow;
//  FCheckBoxes
end;

procedure TfrmQuickAccess.TempPrc(var Message: TMessage);
begin
  DefWindowProc(FTempWindow, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TfrmQuickAccess.SpeedButton1Click(Sender: TObject);
begin
  if SpeedButton23.Down
    then TDCDVBSource(Filter).put_TeletextFastext(TTeletextFastext((Sender as TSpeedButton).Tag))
end;

procedure TfrmQuickAccess.SpeedButton5Click(Sender: TObject);
begin
  Hide;
end;

procedure TfrmQuickAccess.SpeedButton2Click(Sender: TObject);
var
  str: TFileStream;
  bfh: TBitmapFileHeader;
  bi: TBitmapInfo;
  buffer_size: Integer;
  buffer: PByte;
  buf: PByte;
  bih: PBitmapInfoHeader;
  enum: IEnumFilters;
  video: IBasicVideo;
  filter: IBaseFilter;
begin
  if TDCDVBSource(Self.Filter).Graph.EnumFilters(enum) = S_OK then
  begin
    while enum.Next(1, filter, nil) = S_OK do
    begin
      if filter.QueryInterface(IID_IBasicVideo, video) = S_OK then
      begin
        buffer_size := 0;
        buffer := nil;
        if video.GetCurrentImage(buffer_size, PInteger(buffer)) = S_OK then
        begin
          buffer := CoTaskMemAlloc(buffer_size);
          if video.GetCurrentImage(buffer_size, PInteger(buffer)) = S_OK then
          begin
            bih := PBitmapInfoHeader(buffer);
            buf := buffer;
            inc(buf, SizeOf(TBitmapInfoHeader));

            if SaveDialog1.Execute then
            begin
              str := TFileStream.Create(SaveDialog1.FileName, fmCreate);

              FillChar(bfh, SizeOf(TBitmapFileHeader), 0);
              FillChar(bi, SizeOf(TBitmapInfo), 0);

              bi.bmiHeader := bih^;

              bfh.bfReserved1 := 0;
              bfh.bfReserved2 := 0;
              bfh.bfSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + bi.bmiHeader.biSizeImage;
              bfh.bfType := $4D42;
              bfh.bfOffBits := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);

              str.Write(bfh, SizeOf(TBitmapFileHeader));
              str.Write(bi.bmiHeader, SizeOf(TBitmapInfoHeader));
              str.Write(buf^,bih.biWidth * bih.biHeight * (bih.biBitCount shr 3));

              str.Free;
            end;
          end;
          CoTaskMemFree(buffer);
        end;
        break;
      end;
    end;
  end;
end;

procedure TfrmQuickAccess.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := False;
end;

procedure TfrmQuickAccess.Timer1Timer(Sender: TObject);
var
  nr: Integer;
begin
  if not TryStrToInt(FChannel, nr) then
  begin
    Timer1.Enabled := False;
    FChannel := '';
    Exit;
  end;

  if (nr < 1) or (nr > ComboBox1.Items.Count) then
  begin
    Timer1.Enabled := False;
    FChannel := '';
    Exit;
  end;

  Timer1.Enabled := False;
  FChannel := '';
  CanChangeChannels := False;
  ComboBox1.ItemIndex := nr - 1;
  CanChangeChannels := True;
  ComboBox1Change(ComboBox1);
end;

procedure TfrmQuickAccess.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_NUMPAD0,
    48: Button1Click(SpeedButton1);
    VK_NUMPAD1,
    49: Button1Click(SpeedButton2);
    VK_NUMPAD2,
    50: Button1Click(SpeedButton3);
    VK_NUMPAD3,
    51: Button1Click(SpeedButton4);
    VK_NUMPAD4,
    52: Button1Click(SpeedButton5);
    VK_NUMPAD5,
    53: Button1Click(SpeedButton6);
    VK_NUMPAD6,
    54: Button1Click(SpeedButton7);
    VK_NUMPAD7,
    55: Button1Click(SpeedButton8);
    VK_NUMPAD8,
    56: Button1Click(SpeedButton9);
    VK_NUMPAD9,
    57: Button1Click(SpeedButton10);
    84:
    begin
      SpeedButton23.Down := not SpeedButton23.Down;
      SpeedButton23Click(SpeedButton23);
    end;
    79: SpeedButton22Click(SpeedButton22);
    189,
    VK_SUBTRACT: // minus
    begin
      if SpeedButton23.Down
        then SpeedButton19Click(SpeedButton19)
        else SpeedButton21Click(SpeedButton21);
    end;
    187,
    VK_ADD: // plus
    begin
      if SpeedButton23.Down
        then SpeedButton20Click(SpeedButton20)
        else SpeedButton18Click(SpeedButton18);
    end;
  end;

//  OutputDebugString(PChar('Key: ' + inttostr(Key)));
end;

procedure TfrmQuickAccess.SpeedButton21Click(Sender: TObject);
begin
  if not CanChangeChannels
    then Exit;

  TDCDVBSource(Filter).put_PreviousChannel;
end;

procedure TfrmQuickAccess.SpeedButton18Click(Sender: TObject);
begin
  if not CanChangeChannels
    then Exit;

  TDCDVBSource(Filter).put_NextChannel;
end;

procedure TfrmQuickAccess.SpeedButton20Click(Sender: TObject);
var
  p, s: Integer;
begin
  TDCDVBSource(Filter).get_TeletextPage(p, s);
  inc(p);
  if p > 899
    then Exit;
  TDCDVBSource(Filter).put_TeletextPage(p, s);
end;

procedure TfrmQuickAccess.SpeedButton19Click(Sender: TObject);
var
  p, s: Integer;
begin
  TDCDVBSource(Filter).get_TeletextPage(p, s);
  dec(p);
  if p < 100
    then Exit;
  TDCDVBSource(Filter).put_TeletextPage(p, s);
end;

procedure TfrmQuickAccess.SpeedButton22Click(Sender: TObject);
begin
  TDCDVBSource(Filter).ShowOSD;
end;

procedure TfrmQuickAccess.SpeedButton23Click(Sender: TObject);
begin
  TDCDVBSource(Filter).put_TeletextShow(SpeedButton23.Down);
end;

procedure TfrmQuickAccess.SpeedButton24Click(Sender: TObject);
begin
  TDCDVBSource(Filter).WriteTSStream(SpeedButton24.Down);
end;

procedure TfrmQuickAccess.SpeedButton25Click(Sender: TObject);
begin
  TDCDVBSource(Filter).put_TeletextTransparent(SpeedButton25.Down);
end;

procedure TfrmQuickAccess.SpeedButton27Click(Sender: TObject);
begin
  if SpeedButton23.Down
    then TDCDVBSource(Filter).put_CycleTeletext // Cycle Teletext Page
    else TDCDVBSource(Filter).put_CycleChannels;
end;

procedure TfrmQuickAccess.DoubleLower1Click(Sender: TObject);
begin
  TDCDVBSource(Filter).put_TeletextSizeMode(TTeletextSizeMode((Sender as TMenuItem).Tag));
  SpeedButton26.Caption := (Sender as TMenuItem).Caption;
end;

procedure TfrmQuickAccess.ComboBox2Change(Sender: TObject);
begin
  TDCDVBSource(Filter).put_TeletextSizeMode(TTeletextSizeMode(ComboBox2.ItemIndex));
end;

const
  DVB_KEY_0 = 0;
  DVB_KEY_1 = 1;
  DVB_KEY_2 = 2;
  DVB_KEY_3 = 3;
  DVB_KEY_4 = 4;
  DVB_KEY_5 = 5;
  DVB_KEY_6 = 6;
  DVB_KEY_7 = 7;
  DVB_KEY_8 = 8;
  DVB_KEY_9 = 9;

  DVB_KEY_TELETEXT_RED    = 10;
  DVB_KEY_TELETEXT_GREEN  = 11;
  DVB_KEY_TELETEXT_YELLOW = 12;
  DVB_KEY_TELETEXT_BLUE   = 13;
  DVB_KEY_TELETEXT_INDEX  = 14;

procedure TfrmQuickAccess.WndProc(var Message: TMessage);
begin
  if Message.Msg = 12345 then
  begin
    case Message.WParam of
      DVB_KEY_0: Button1Click(SpeedButton1);
      DVB_KEY_1: Button1Click(SpeedButton2);
      DVB_KEY_2: Button1Click(SpeedButton3);
      DVB_KEY_3: Button1Click(SpeedButton4);
      DVB_KEY_4: Button1Click(SpeedButton5);
      DVB_KEY_5: Button1Click(SpeedButton6);
      DVB_KEY_6: Button1Click(SpeedButton7);
      DVB_KEY_7: Button1Click(SpeedButton8);
      DVB_KEY_8: Button1Click(SpeedButton9);
      DVB_KEY_9: Button1Click(SpeedButton10);

      DVB_KEY_TELETEXT_RED,
      DVB_KEY_TELETEXT_GREEN,
      DVB_KEY_TELETEXT_YELLOW,
      DVB_KEY_TELETEXT_BLUE,
      DVB_KEY_TELETEXT_INDEX: TDCDVBSource(Filter).put_TeletextFastext(TTeletextFastext(Message.WParam - DVB_KEY_TELETEXT_RED));

            
    end;
  end else
  begin
    inherited WndProc(Message);
  end;
end;

end.
