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

unit formWMPPluginInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, WMPPluginAPI, DVBFilter, ActiveX,
  BDAUtils, ComCtrls, RichEdit, OleCtrls, SHDocVw, MSHTML;

type
  TfrmWMPPluginInfo = class(TWMPUIPluginForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WebBrowser1NavigateComplete2(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    FFilter: TDCDVBSource;
    procedure OnChannelChange(AIndex: Integer);
    procedure LoadText(AText: String);
  protected
  public
    procedure ClearInfo;
    procedure UpdateInfo;
    procedure UpdateText;
  end;

implementation

{$R *.dfm}

procedure WB_SetBorderStyle(Sender: TObject; BorderStyle: String);
var
  Document : IHTMLDocument2;
  Element : IHTMLElement;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  if Assigned(Document) then
  begin
    Element := Document.Body;
    if Element <> nil then
    begin
      Element.Style.BorderStyle := BorderStyle;
    end;
  end;
end;

procedure WB_Set3DBorderStyle(Sender: TObject; bValue: Boolean);
var
  Document : IHTMLDocument2;
  Element : IHTMLElement;
  StrBorderStyle: string;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  if Assigned(Document) then
  begin
    Element := Document.Body;
    if Element <> nil then
    begin
      case BValue of
        False: StrBorderStyle := 'none';
        True: StrBorderStyle := '';
      end;
      Element.Style.BorderStyle := StrBorderStyle;
    end;
  end;
end;

procedure TfrmWMPPluginInfo.FormCreate(Sender: TObject);
var
  log_font: TLogFont;
begin
//  DoubleBuffered := True;
  LoadText('');

  ClearInfo;
  Label3.Align := alRight;
  Label1.Align := alClient;
  GetObject(Label1.Font.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  Label1.Font.Handle := CreateFontIndirect(log_font);
  GetObject(Label3.Font.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  Label3.Font.Handle := CreateFontIndirect(log_font);
  UpdateInfo;
  Timer1.Enabled := True;
end;

procedure TfrmWMPPluginInfo.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Assigned(FFilter)
    then FFilter.OnChannelChange2 := nil;
  Pointer(FFilter) := nil;
end;

procedure TfrmWMPPluginInfo.ClearInfo;
begin
  Label3.Caption := '';
  LoadText('');
  Label1.Caption := 'No Info available for this Channel';
  if Assigned(FFilter)
    then FFilter.OnChannelChange2 := nil;
  Pointer(FFilter) := nil;
end;

procedure TfrmWMPPluginInfo.UpdateInfo;
var
  map: THandle;
  view: Pointer;
  tmp: Cardinal;
begin
  if Assigned(FFilter)
    then FFilter.OnChannelChange2 := nil;
  Pointer(FFilter) := nil;
  map := OpenFileMapping(FILE_MAP_READ, True, 'DCDVBSource2');
  if (map <> 0) then
  begin
    view := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
    if (view <> nil) then
    begin
      tmp := PCardinal(view)^;
      Pointer(FFilter) := Pointer(tmp);
      UnmapViewOfFile(view);

      if (FFilter <> nil) then
      begin
        FFilter.OnChannelChange2 := OnChannelChange;
        UpdateText;
      end;
    end;

    CloseHandle(map);
  end;
end;

procedure TfrmWMPPluginInfo.OnChannelChange(AIndex: Integer);
begin
  UpdateText;
end;

procedure TfrmWMPPluginInfo.UpdateText;
var
  s1, s2, s3, s4: String;
begin
  if not Assigned(FFilter) or (FFilter.Settings.CurrentChannel < 0) then
  begin
    LoadText('');
    Label1.Caption := 'No Info available for this Channel';
    Label3.Caption := '';
    Exit;
  end;

  s1 := '';
  s2 := '';
  s3 := '';
  s4 := '';

  if not FFilter.EPG.GetCurrentProgram(FFilter.Settings.CurrentChannel, s1, s2, s3, s4) then
  begin
    LoadText('');
    Label1.Caption := 'No Info available for this Channel';
    Label3.Caption := '';
    Exit;
  end;

  if (s3 <> '') and (s4 <> '')
    then Label3.Caption := '(' + s3 + ' - ' + s4 + ')';

  Label1.Caption := s1;
  LoadText(s2);
end;

procedure TfrmWMPPluginInfo.Timer1Timer(Sender: TObject);
var
  s1, s2, s3, s4: String;
begin
  if not Assigned(FFilter) or (FFilter.Settings.CurrentChannel < 0) then
  begin
    LoadText('');
    Label1.Caption := 'No Info available for this Channel';
    Label3.Caption := '';
    Exit;
  end;

  s1 := '';
  s2 := '';
  s3 := '';
  s4 := '';

  if not FFilter.EPG.GetCurrentProgram(FFilter.Settings.CurrentChannel, s1, s2, s3, s4) then
  begin
    LoadText('');
    Label1.Caption := 'No Info available for this Channel';
    Label3.Caption := '';
    Exit;
  end;

  s3 := '(' + s3 + ' - ' + s4 + ')';
  if s3 = Label3.Caption
    then Exit;

  Label3.Caption := s3;

  Label1.Caption := s1;
  LoadText(s2);
//  FText.Text := s1;
end;

procedure TfrmWMPPluginInfo.WebBrowser1NavigateComplete2(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  WB_Set3DBorderStyle(Sender, False);
  WB_SetBorderStyle(Sender, 'none');
end;

procedure TfrmWMPPluginInfo.LoadText(AText: String);
var
  str: String;
begin
  str := '<html><head><style type="text/css">';
  str := str + 'body {';
  str := str + 'scrollbar-face-color: #000000;';
  str := str + 'scrollbar-highlight-color: #333333;';
  str := str + 'scrollbar-3dlight-color: #333333;';
  str := str + 'scrollbar-shadow-color: #333333;';
  str := str + 'scrollbar-darkshadow-color: #000000;';
  str := str + 'scrollbar-arrow-color: #CCCCCC;';
  str := str + 'scrollbar-track-color: #333333;';
  str := str + 'background-color: #000;	color: #FFFFFF;	font-family: Tahoma, Verdana, Arial, Trebuchet MS, Sans-Serif, Georgia, Courier, Times New Roman, Serif;	font-size: 10px;	margin: 3px;	padding: 0px;}';
  str := str + 'table,tr,td{	background-color: transparent;	color: #FFFFFF;	font-size: 12px; font-family: Tahoma;}#eventtext{	text-align: justify;}';
  str := str + '</style></head><body><table width="100%" cellspacing="0" cellpadding="0"><tr valign="top"><td id="eventtext">';
  str := str + AText;
  str := str + '</td></tr></table></body></html>';

  Label2.Caption :=    AText;
//  WBLoadHTML(WebBrowser1, str);
end;

end.
