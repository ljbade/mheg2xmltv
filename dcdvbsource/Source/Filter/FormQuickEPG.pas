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

unit FormQuickEPG;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DirectShow9, Buttons, BDAUtils, ExtCtrls, DVBEPG,
  BaseClass, DVBInterface, IDVBSource, ActiveX, Menus, Grids, EPGWindow,
  OleCtrls, SHDocVw;

type
  TEPGExtendedDescription = record
    Valid: Boolean;
    Text: String;
  end;

  TEPGItem = record
    ChannelName: String;
    Event: String;
    Extended: TEPGExtendedDescription;
    StartTime: TDateTime;
    Duration: TDateTime;
  end;
  PEPGItem = ^TEPGItem;

  TfrmQuickEPG = class(TForm)
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Update2Click(Sender: TObject);
    procedure ListBox1MeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FList: TList;
    FHTML: String;
    FStream: TMemoryStream;
    procedure ClearList;
  protected
    FPopup: HMENU;
    FProc: TWndMethod;
    procedure LBWndProc(var Message: TMessage);
  public
    Kill: Boolean;
    Filter: Pointer;
    EPG: TEPGWindow;
  end;

implementation

uses
  DVBFilter, StrUtils, DVBSettings, DateUtils;

{$R *.dfm}

procedure TfrmQuickEPG.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Kill
    then Action := caFree;
end;

procedure TfrmQuickEPG.FormCreate(Sender: TObject);
begin
  FStream := TMemoryStream.Create;
  FList := TList.Create;
  EPG := TEPGWindow.Create(Self);
  EPG.Parent := Self;
  EPG.Align := alClient;
  EPG.Visible := False;
  ListBox1.Align := alClient;
  ListBox1.DoubleBuffered := True;
  FPopup := CreatePopupMenu;
  AppendMenu(FPopup, MF_STRING, 12345, 'Update');
  FProc := ListBox1.WindowProc;
  ListBox1.WindowProc := LBWndProc;


//  WebBrowser1.Align := alClient;
//  WebBrowser1.Navigate('about:blank');
//  WebBrowser1.Navigate('about:blank', navNoHistory or navNoReadFromCache or navNoWriteToCache);
end;

procedure TfrmQuickEPG.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  ClearList;
  EPG.Free;
  FList.Free;
  FStream.Free;
end;

function ItemSort(AItem1: Pointer; AItem2: Pointer): Integer;
var
  it1, it2: PEPGItem;
begin
  it1 := PEPGItem(AItem1);
  it2 := PEPGItem(AItem2);

  if it1.StartTime < it2.StartTime
    then Result := -1
  else if it2.StartTime < it1.StartTime
    then Result := 1
  else Result := 0;
end;

procedure TfrmQuickEPG.Update2Click(Sender: TObject);
var
  i, k: Integer;
  epgb: PByte;
  size: Integer;
  stream: TMemoryStream;
  header: TEPGHeader;
  event: TDVBEPGEvent;
  epg: TDVBEPGService;
  it: PEPGItem;
  offset: Integer;
begin
  ClearList;
  ListBox1.Items.BeginUpdate;
  ListBox1.Clear;

  TDCDVBSource(Filter).get_EPGTimeOffset(offset);

  for i := 0 to TDCDVBSource(Filter).Settings.Channels.Count -1 do
  begin
    if TDCDVBSource(Filter).get_EPG(i, epgb, size)= S_OK then
    begin
      stream := TMemoryStream.Create;
      stream.SetSize(size);
      stream.Write(epgb^, size);
      CoTaskMemFree(epgb);
      stream.Seek(0, soFromBeginning);
      stream.Read(header, SizeOf(header));
      if header.ServiceCount > 0 then
      begin
        epg := TDVBEPGService.Create;
        epg.LoadFromStream(stream);
        for k := 0 to epg.EventCount -1 do
        begin
          event := epg.Event[k];
          if event.ShortDescription.Valid then
          begin
            new(it);
            it.ChannelName := RemoveUnwantedChars(TDCDVBSource(Filter).Settings.Channels[i].Name);
            it.Event := RemoveUnwantedChars(event.ShortDescription.Name);
            it.StartTime := IncMinute(event.StartTime, offset);
            it.Duration := event.Duration;
            it.Extended.Valid := False;
//            it.Extended.Valid :=  event.ExtendedDescription.Valid;
//            if it.Extended.Valid
//              then it.Extended.Text := RemoveUnwantedChars(event.ExtendedDescription.Description);
            FList.Add(it);
          end;
        end;
        epg.Free;
      end;
      stream.Free;
    end;
  end;

  FList.Sort(ItemSort);

  for i := 0 to FList.Count -1 do
  begin
//    it := FList[i];
    ListBox1.Items.Add('');
  end;

  if FList.Count > 0
    then ListBox1.ItemIndex := -0;

  ListBox1.Items.EndUpdate;
end;

procedure TfrmQuickEPG.Button1Click(Sender: TObject);
var
  header: String;
  footer: String;
  body: String;
begin
//  WebBrowser1.Navigate('about:blank', navNoHistory or navNoReadFromCache or navNoWriteToCache);

  header := '<html><head><style type="text/css">';
  header := header + 'body {background-color: #e9e9df;	color: #000;	font-family: Verdana, Tahoma, Arial, Trebuchet MS, Sans-Serif, Georgia, Courier, Times New Roman, Serif;	font-size: 12px;	margin: 5px;	padding: 0px;}';
  header := header + 'table,tr,td{	background-color: transparent;	color: #000;	font-size: 12px;}#header{	font-size: 18px;	font-style: bold;}#eventtext{	text-align: justify;}#navi{	font-size: 10px;}';
  header := header + '</style></head><body>';

  body := 'ARGH';

  footer := '</body></html>';

  FHTML := 'ARGH';//header + body + footer;

  FStream.SetSize(Length(FHTML) + 1);
  FStream.Seek(0, 0);
  FStream.Write(FHTML[1], Length(FHTML) + 1);
  FStream.Seek(0, 0);

  Timer1.Enabled := True;
end;

procedure TfrmQuickEPG.ClearList;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do Dispose(PEPGItem(FList[i]));
  FList.Clear;
end;

procedure TfrmQuickEPG.ListBox1MeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  it: PEPGItem;
begin
  it := FList[Index];
  if it.Extended.Valid
    then Height := 57
    else Height := 40;
end;

procedure TfrmQuickEPG.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  it: PEPGItem;
  bmp: TBitmap;
  sc: TColor;
begin
  it := FList[Index];
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Width := Rect.Right - Rect.Left;
  bmp.Height := Rect.Bottom - Rect.Top;
  bmp.Canvas.Font := ListBox1.Canvas.Font;

  if (odFocused in State) or (odSelected in State) then
  begin
    sc := clHighlight;
    bmp.Canvas.Brush.Color := clHighlight;
    bmp.Canvas.Pen.Color := clHighlight;
    bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  end else
  begin
    sc := clGray;
    bmp.Canvas.Brush.Color := clGray;
    bmp.Canvas.Pen.Color := clGray;
    bmp.Canvas.Rectangle(0, 0, 85, bmp.Height);
    bmp.Canvas.Brush.Color := clWindow;
    bmp.Canvas.Pen.Color := clWindow;
    bmp.Canvas.Rectangle(85, 0, bmp.Width, bmp.Height);
  end;

  bmp.Canvas.Pen.Color := clGrayText;
  bmp.Canvas.MoveTo(0, bmp.Height -1);
  bmp.Canvas.LineTo(bmp.Width, bmp.Height -1);

  bmp.Canvas.Font.Style := [fsBold];
  bmp.Canvas.TextOut(90, 5, it.Event);

  bmp.Canvas.Font.Style := [];
  bmp.Canvas.TextOut(90, 20, it.ChannelName);

  bmp.Canvas.Font.Style := [fsBold];
  bmp.Canvas.Brush.Color := sc;
  bmp.Canvas.Pen.Color := sc;
  bmp.Canvas.TextOut(5, 5, FormatDateTime('dddd', it.StartTime));
  bmp.Canvas.TextOut(5, 20, FormatDateTime('hh:nn', it.StartTime) + ' - ' + FormatDateTime('hh:nn', it.StartTime + it.Duration));

  ListBox1.Canvas.Draw(Rect.Left, Rect.Top, bmp);
  bmp.Free;
end;

procedure TfrmQuickEPG.LBWndProc(var Message: TMessage);
begin
  case MEssage.Msg of
    WM_COMMAND:
    begin
      if Message.WParam = 12345 then
      begin
        Update2Click(nil);
        //Timer1.Enabled := True;
      end;
    end;
  end;

  FProc(Message);
end;

procedure TfrmQuickEPG.ListBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if Button = mbright then
  begin
    p := ClientToScreen(Point(x,y));
    TrackPopupMenu(FPopup, 0, p.X, p.Y, 0, ListBox1.Handle, nil);
  end;
end;

procedure TfrmQuickEPG.Timer1Timer(Sender: TObject);
begin
//  if WebBrowser1.ReadyState < READYSTATE_INTERACTIVE
//    then Exit;

//  if WebBrowser1.Document <> nil then
//    (WebBrowser1.Document as IPersistStreamInit).Load(TStreamAdapter.Create(FStream));

  Timer1.Enabled := False;
end;

end.
