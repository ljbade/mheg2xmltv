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

unit PropEPG;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  ComCtrls, DSUtil, DVBInterface, BDAUtils, Menus, Buttons, OleCtrls,
  SHDocVw, MPEGUtils, IDVBSource, DVBEPG, ISO639LanguageCode, JvSimpleXML;

type
  TFormPropEPG = class(TFormPropertyPage)
    TabControl1: TTabControl;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    WebBrowser1: TWebBrowser;
    ListView1: TListView;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure WebBrowser1BeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure FormShow(Sender: TObject);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure Button3Click(Sender: TObject);
  private
    FDVBFilter: IDCDVBSource;
    FCanChange: Boolean;
    FTimeOffset: Integer;
    FEPG: TDVBEPGService;
    FHTML: String;
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

uses DateUtils;

{$R *.DFM}

function TFormPropEPG.OnConnect(Unknown: IUnKnown): HRESULT;
var
  channels: Integer;
  i: Integer;
  channel_name: PChar;
begin
  Result := S_OK;
  Unknown.QueryInterface(IID_IDCDVBSource, FDVBFilter);

  if Assigned(FDVBFilter) then
  begin
    FCanChange := False;

    FDVBFilter.get_EPGTimeOffset(FTimeOffset);

    FDVBFilter.get_ChannelCount(Channels);
    for i := 0 to Channels -1 do
    begin
      FDVBFilter.get_ChannelInfo(i, channel_name);
      ComboBox1.Items.Add((channel_name));
      CoTaskMemFree(channel_name);
    end;

    FDVBFilter.get_ChannelSelected(channels);
    ComboBox1.ItemIndex := channels;

    FCanChange := True;
  end;

  ComboBox1Change(Self);
end;

procedure TFormPropEPG.Timer1Timer(Sender: TObject);
var
  sl: TStringList;
  ms: TMemoryStream;
  tmp: IStream;
begin
  if WebBrowser1.ReadyState < READYSTATE_INTERACTIVE
    then Exit;

  sl := TStringList.Create;
  try
    ms := TMemoryStream.Create;
    try
      sl.Text := FHTML;
      sl.SaveToStream(ms);
      ms.Seek(0, 0);
      tmp := TStreamAdapter.Create(ms);
      (WebBrowser1.Document as IPersistStreamInit).Load(tmp);
      tmp := nil;
    finally
      ms.Free;
    end;
  finally
    sl.Free;
  end;

  Timer1.Enabled := False;
  WebBrowser1.BringToFront;
  WebBrowser1.SetFocus;
end;

procedure TFormPropEPG.Button1Click(Sender: TObject);
begin
  ComboBox1Change(Self);
end;

procedure TFormPropEPG.FormCreate(Sender: TObject);
begin
  WebBrowser1.Navigate('about:blank');
  WebBrowser1.Width := ListView1.Width;
  ListView1.DoubleBuffered := True;
  FCanChange := False;
  FEPG := TDVBEPGService.Create;
end;

procedure TFormPropEPG.ComboBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DrawComboBox(Control as TComboBox, Index, Rect, State);
end;

procedure TFormPropEPG.ComboBox1Change(Sender: TObject);
var
  i: Integer;
  item: TListItem;
  hr: HRESULT;
  epg: PByte;
  size: Integer;
  stream: TMemoryStream;
  header: TEPGHeader;
  event: TDVBEPGEvent;
  start_time: TDateTime;
begin
  if not FCanChange or not Assigned(FDVBFilter) or (ComboBox1.ItemIndex < 0)
    then Exit;

  ListView1.Items.BeginUpdate;
  ListView1.Clear;
  FEPG.Clear;

  hr := FDVBFilter.get_EPG(ComboBox1.ItemIndex, epg, size);
  if hr <> S_OK then
  begin
    ListView1.Items.EndUpdate;
    ListView1.BringToFront;
    Exit;
  end;

  stream := TMemoryStream.Create;
  stream.SetSize(size);
  stream.Write(epg^, size);
  CoTaskMemFree(epg);
  stream.Seek(0, soFromBeginning);
  stream.Read(header, SizeOf(header));

  if header.ServiceCount = 0 then
  begin
    ListView1.Items.EndUpdate;
    ListView1.BringToFront;
    stream.Free;
    Exit;
  end;
  FEPG.LoadFromStream(stream);
  stream.Free;

  for i := 0 to FEPG.EventCount -1 do
  begin
    event := FEPG.Event[i];
    item := ListView1.Items.Add;
    start_time := IncMinute(event.StartTime, FTimeOffset);
    item.Caption := FormatDateTime('dddd, d/m', start_time);
    item.SubItems.Add(FormatDateTime('hh:nn', start_time));
    item.SubItems.Add(FormatDateTime('hh:nn', start_time + event.Duration));

    if event.ShortDescription.Valid
      then item.SubItems.Add(RemoveUnwantedChars(event.ShortDescription.Name))
      else item.SubItems.Add('Unknown Event');

    item.SubItems.Add(inttostr(PInt64(@event.StartTime)^));
    item.SubItems.Add(inttostr(i));
  end;

  ListView1.AlphaSort;
  ListView1.Items.EndUpdate;
  ListView1.BringToFront;
end;

procedure TFormPropEPG.FormDestroy(Sender: TObject);
begin
  FDVBFilter := nil;
  FEPG.Free;
end;

procedure TFormPropEPG.Button2Click(Sender: TObject);
begin
  if Assigned(FDVBFilter) then
  begin
    FEPG.Clear;
    FDVBFilter.put_EPGClearAll;
    ListView1.Clear;
    ListView1.BringToFront;
  end;
end;

procedure TFormPropEPG.FormPaint(Sender: TObject);
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

procedure TFormPropEPG.ListView1DblClick(Sender: TObject);
var
//  root, e1, e2, e3, e4, e5: IXMLDOMNode;
  i, c: Integer;
  str: String;
  start_time: TDateTime;
  p, n: String;
  found: Boolean;
  ww: Integer;
  tmp: String;
  idx: Integer;
  event: TDVBEPGEvent;
begin
  if (ListView1.Selected = nil)
    then Exit;

  try
    idx := StrToInt(ListView1.Selected.SubItems[4]);
  except
    Exit;
  end;

  if idx >= FEPG.EventCount
    then Exit;

  event := FEPG.Event[idx];

  ww := 50;
  str := '<html><head><style type="text/css">';
  str := str + 'body {background-color: #e9e9df;	color: #000;	font-family: Verdana, Tahoma, Arial, Trebuchet MS, Sans-Serif, Georgia, Courier, Times New Roman, Serif;	font-size: 12px;	margin: 5px;	padding: 0px;}';
  str := str + 'table,tr,td{	background-color: transparent;	color: #000;	font-size: 12px;}#header{	font-size: 18px;	font-style: bold;}#eventtext{	text-align: justify;}#navi{	font-size: 10px;}';
  str := str + '</style></head><body>';

  str := str + '<table width="100%">';
  if ListView1.Selected.Index = 0
    then p := 'Previous'
    else p := '<a href="prev">Previous</a>';

  if ListView1.Selected.Index = ListView1.Items.Count -1
    then n := 'Next'
    else n := '<a href="next">Next</a>';

  str := str + '<tr valign="top"><td align="left" width="33%" id="navi">' + p + '</td><td align="center" width="34%" id="navi"><a href="close">Close</a></td><td align="right" width="33%" id="navi">' + n + '</td></tr>';
  str := str + '</table>';

  str := str + '<table width="100%">';

  tmp := '';
  if event.ShortDescription.Valid then
  begin
    tmp := ReplaceUnwantedCharsHTML(event.ShortDescription.Name)
  end;
  str := str + '<tr valign="top"><td id="header" colspan="2">' + tmp + '</td></tr>';

  if event.ShortDescription.Valid then
  begin
    tmp := ReplaceUnwantedCharsHTML(event.ShortDescription.Description);
    if Length(tmp) > 0
      then str := str + '<tr valign="top"><td colspan="2">"' + tmp + '"</td></tr>';
  end;

  str := str + '<tr valign="top"><td colspan="2">&nbsp;</td></tr>';

  start_time := IncMinute(event.StartTime, FTimeOffset);
  str := str + '<tr valign="top"><td><b>Starts at:</b> ' + FormatDateTime('dddd, d/m hh:nn', start_time) + '</td><td align="right"><b>Duration:</b> ' + FormatDateTime('hh:nn', event.Duration) + '</td></tr>';

  if event.ExtendedDescription.Valid then
  begin
    str := str + '<tr valign="top"><td colspan="2">&nbsp;</td></tr>';
    str := str + '<tr valign="top"><td colspan="2" id="eventtext">' + ReplaceUnwantedCharsHTML(event.ExtendedDescription.Description) + '</td></tr>';
  end;

  str := str + '</table>';

  // add content
  found := False;
  for i := 0 to event.ContentCount -1 do
  begin
    if GetContentDescriptionValid(event.Content[i].ContentNibbleLevel1, event.Content[i].ContentNibbleLevel2) then
    begin
      found := True;
      break;
    end;
  end;
  if found then
  begin
    str := str + '<table width="100%">';
    str := str + '<tr valign="top"><td colspan="2">&nbsp;</td></tr>';
    str := str + '<tr valign="top"><td align="left" width="' + inttostr(ww) + '"><b>Genre:</b></td><td align="left">';
    c := 0;
    for i := 0 to event.ContentCount -1 do
    begin
      if GetContentDescriptionValid(event.Content[i].ContentNibbleLevel1, event.Content[i].ContentNibbleLevel2) then
      begin
        if c = 0
          then str := str + GetContentDescriptionString(event.Content[i].ContentNibbleLevel1, event.Content[i].ContentNibbleLevel2)
          else str := str + '<br>' + GetContentDescriptionString(event.Content[i].ContentNibbleLevel1, event.Content[i].ContentNibbleLevel2);
        inc(c);
      end;
    end;
    str := str + '</td></tr></table>';
  end;

  // add component
  found := False;
  if event.ComponentsCount > 0 then
  begin
    for i := 0 to event.ComponentsCount -1 do
    begin
      if GetStreamContentValid (event.Component[i].StreamContent, event.Component[i].ComponentType) then
      begin
        found := True;
        break;
      end
    end;
    if found then
    begin
      str := str + '<table width="100%">';
      str := str + '<tr valign="top"><td colspan="2">&nbsp;</td></tr>';
      str := str + '<tr valign="top"><td align="left" width="' + inttostr(ww) + '"><b>Content:</b></td><td align="left">';
      c := 0;
      for i := 0 to event.ComponentsCount -1 do
      begin
        if GetStreamContentValid (event.Component[i].StreamContent, event.Component[i].ComponentType) then
        begin
          if c = 0
            then str := str + GetFullISO639Name(event.Component[i].LanguageCode) + ': ' + GetStreamContentString(event.Component[i].StreamContent, event.Component[i].ComponentType)
            else str := str + '<br>' + GetFullISO639Name(event.Component[i].LanguageCode) + ': ' + GetStreamContentString(event.Component[i].StreamContent, event.Component[i].ComponentType);
          if Length(ReplaceUnwantedCharsHTML(event.Component[i].Description)) > 0
            then str := str + ' (' + ReplaceUnwantedCharsHTML(event.Component[i].Description) + ')';
          inc(c);
        end;
      end;
      str := str + '</td></tr></table>';
    end;
  end;

  // Add Key/Value
  if event.ExtendedDescription.KeyValueCount > 0 then
  begin
    str := str + '<table width="100%">';
    str := str + '<tr valign="top"><td>&nbsp;</td></tr>';
    str := str + '<tr valign="top"><td align="left">';
    for i := 0 to event.ExtendedDescription.KeyValueCount -1 do
    begin
      if i = 0 then
      begin
        str := str + '<b>' + event.ExtendedDescription.KeyValue[i].Key + ':</b> ' + event.ExtendedDescription.KeyValue[i].Value;
      end else
      begin
        str := str + '<br><b>' + event.ExtendedDescription.KeyValue[i].Key + ':</b> ' + event.ExtendedDescription.KeyValue[i].Value;
      end;
    end;
    str := str + '</td></tr></table>';
  end;

  // Add Parental Rating
  if event.ParentalRatingCount > 0 then
  begin
    str := str + '<table width="100%">';
    str := str + '<tr valign="top"><td>&nbsp;</td></tr>';
    str := str + '<tr valign="top"><td align="left" width="' + inttostr(ww) + '"><b>Parental Rating:</b></td><td align="left">';
    for i := 0 to event.ParentalRatingCount -1 do
    begin
      if i = 0 then
      begin
        str := str + GetFullISO639Name(event.ParentalRating[i].Country) + ': ' + GetParentalRatingString(event.ParentalRating[i].Rating);
      end else
      begin
        str := str + '<br>' + GetFullISO639Name(event.ParentalRating[i].Country) + ': ' + GetParentalRatingString(event.ParentalRating[i].Rating);
      end;
    end;
    str := str + '</td></tr></table>';
  end;

  str := str + '</body></html>';

  // WBLoadHTML(WebBrowser1, str);
  // WebBrowser1.BringToFront;
  // WebBrowser1.SetFocus;
  FHTML := str;
  WebBrowser1.Navigate('about:blank', navNoHistory or navNoReadFromCache or navNoWriteToCache);
  Timer1.Enabled := True;
end;

procedure TFormPropEPG.WebBrowser1BeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  i: Integer;
begin
  if Pos('close', String(URL)) > 0 then
  begin
    Cancel := True;
    ListView1.BringToFront;
    ListView1.SetFocus;
    Exit;
  end;
  if Pos('next', String(URL)) > 0 then
  begin
    Cancel := True;
    i := ListView1.Selected.Index;
    ListView1.ClearSelection;
    ListView1.Items.Item[i+1].Selected := True;
    ListView1DblClick(Self);
    Exit;
  end;
  if Pos('prev', String(URL)) > 0 then
  begin
    Cancel := True;
    i := ListView1.Selected.Index;
    ListView1.ClearSelection;
    ListView1.Items.Item[i-1].Selected := True;
    ListView1DblClick(Self);
    Exit;
  end;
end;

procedure TFormPropEPG.FormShow(Sender: TObject);
begin
  ListView1.Repaint;
end;

procedure TFormPropEPG.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  d1, d2: Int64;
begin
  try
    d1 := StrToInt64(item1.SubItems[3])
  except
    d1 := 0;
  end;
  try
    d2 := StrToInt64(item2.SubItems[3])
  except
    d2 := 0;
  end;

  if PDateTime(@d1)^ < PDateTime(@d2)^
    then Compare := -1
    else Compare := 1;
end;

procedure TFormPropEPG.Button3Click(Sender: TObject);
var
  xml: TJvSimpleXML;
  c, i, k, z: Integer;
  channel_name: PChar;
  it, it2, it3, it4: TJvSimpleXMLElem;
  epg: TDVBEPGService;
  hr: HRESULT;
  epgb: PByte;
  size: Integer;
  stream: TMemoryStream;
  header: TEPGHeader;
  event: TDVBEPGEvent;
//  start_time: TDateTime;
begin
  if SaveDialog1.Execute then
  begin
    xml := TJvSimpleXML.Create(nil);
    xml.Root.Name := 'epg';
    c := 0;
    FDVBFilter.get_ChannelCount(c);
    for i := 0 to c -1 do
    begin
      FDVBFilter.get_ChannelInfo(i, channel_name);
      it := xml.Root.Items.Add('channel');
      it.Properties.Add('name', RemoveUnwantedChars(channel_name));

      hr := FDVBFilter.get_EPG(i, epgb, size);
      if hr = S_OK then
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
            it2 := it.Items.Add('item');
            it2.Properties.Add('event_id', event.EventID);
            it2.Properties.Add('start', FormatDateTime('yyyymmddhhnn', event.StartTime));
            it2.Properties.Add('end', FormatDateTime('yyyymmddhhnn', event.StartTime + event.Duration));
            it2.Properties.Add('FreeToAir', Byte(event.FreeToAir));
            
            it3 := it2.Items.Add('short');
            it3.Properties.Add('name', RemoveUnwantedChars(event.ShortDescription.Name));
            it3.Properties.Add('description', RemoveUnwantedChars(event.ShortDescription.Description));
            it3.Properties.Add('language', RemoveUnwantedChars(event.ShortDescription.Language));

            it3 := it2.Items.Add('extended');
            it3.Properties.Add('description', RemoveUnwantedChars(event.ExtendedDescription.Description));
            it3.Properties.Add('language', RemoveUnwantedChars(event.ExtendedDescription.Language));
            for z := 0 to event.ExtendedDescription.KeyValueCount -1 do
            begin
              it4 := it3.Items.Add('item');
              it4.Properties.Add('key', RemoveUnwantedChars(event.ExtendedDescription.KeyValue[z].Key));
              it4.Properties.Add('value', RemoveUnwantedChars(event.ExtendedDescription.KeyValue[z].Value));
            end;

            it3 := it2.Items.Add('content');
            for z := 0 to event.ContentCount -1 do
            begin
              if GetContentDescriptionValid(event.Content[z].ContentNibbleLevel1, event.Content[z].ContentNibbleLevel2) then
              begin
                it4 := it3.Items.Add('item');
                it4.Properties.Add('value', GetContentDescriptionString(event.Content[z].ContentNibbleLevel1, event.Content[z].ContentNibbleLevel2));
              end;
            end;

            it3 := it2.Items.Add('parentalrating');
            for z := 0 to event.ParentalRatingCount -1 do
            begin
              it4 := it3.Items.Add('item');
              it4.Properties.Add('rating', GetParentalRatingString(event.ParentalRating[z].Rating));
              it4.Properties.Add('language', RemoveUnwantedChars(event.ParentalRating[z].Country));
            end;

            it3 := it2.Items.Add('components');
            for z := 0 to event.ComponentsCount -1 do
            begin
              if GetStreamContentValid (event.Component[z].StreamContent, event.Component[z].ComponentType) then
              begin
                it4 := it3.Items.Add('item');
                it4.Properties.Add('content', RemoveUnwantedChars(GetStreamContentString(event.Component[z].StreamContent, event.Component[z].ComponentType)));
                it4.Properties.Add('description', RemoveUnwantedChars(event.Component[z].Description));
                it4.Properties.Add('language', RemoveUnwantedChars(event.Component[z].LanguageCode));
              end;
            end;
          end;
          epg.Free;
        end;
        stream.Free;
      end;
      CoTaskMemFree(channel_name);
    end;
    xml.SaveToFile(SaveDialog1.FileName);
    xml.Free;
  end;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropEPG, CLSID_PropertyPageEPG);

end.
