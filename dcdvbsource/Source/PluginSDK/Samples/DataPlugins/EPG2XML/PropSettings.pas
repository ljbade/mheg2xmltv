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

unit PropSettings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBDataPlugins, ComCtrls, Menus, Spin, DCDVBShared, ShellAPI,
  DirectShow9, Dialogs, IDVBSource, JvSimpleXML, DVBEPG, BDAUtils, MPEGUtils;

const
  CLSID_EPG2XMLPropertyPage:        TGuid = '{9AFB4521-ABEA-4A7B-9DB1-AC92504FCC58}';
  IID_IEPG2XML:                     TGuid = '{077FA71E-565B-46E5-B8F0-C5D087408352}';

type
  IEPG2XML = interface(IUnknown)
  ['{077FA71E-565B-46E5-B8F0-C5D087408352}']
    function get_Graph(out AGraph: IFilterGraph): HRESULT; stdcall;
  end;

  TfrmPropSettings = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    label6: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    label4: TLabel;
    statictext1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure TabControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure statictext1Click(Sender: TObject);
    procedure label4Click(Sender: TObject);
    procedure statictext1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FIntf: IEPG2XML;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

function TfrmPropSettings.OnConnect(Unknown: IInterface): HRESULT;
begin
  if Unknown.QueryInterface(IID_IEPG2XML, FIntf) <> S_OK
    then FIntf := nil;

  Result := S_OK;
end;

function TfrmPropSettings.OnDisconnect: HRESULT;
begin
  FIntf := nil;
  Result := S_OK;
end;

procedure TfrmPropSettings.Button1Click(Sender: TObject);
var
  enum: IEnumFilters;
  filter: IBaseFilter;
  graph: IFilterGraph;
  intf: IDCDVBSource;
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
begin
  if SaveDialog1.Execute then
  begin
    if Assigned(FIntf) then
    begin
      if FIntf.get_Graph(graph) = S_OK then
      begin
        if graph.EnumFilters(enum) = S_OK then
        begin
          while enum.Next(1, filter, nil) = S_OK do
          begin
            if filter.QueryInterface(IID_IDCDVBSource, intf) = S_OK then
            begin
              xml := TJvSimpleXML.Create(nil);
              xml.Root.Name := 'epg';
              c := 0;
              intf.get_ChannelCount(c);
              for i := 0 to c -1 do
              begin
                intf.get_ChannelInfo(i, channel_name);
                it := xml.Root.Items.Add('channel');
                it.Properties.Add('name', RemoveUnwantedChars(channel_name));

                hr := intf.get_EPG(i, epgb, size);
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

              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmPropSettings.TabControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font := label1.Font;
end;

procedure TfrmPropSettings.statictext1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPropSettings.label4Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:dcoder@dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPropSettings.statictext1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font.Color := clHotLight;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TfrmPropSettings.label4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StaticText1.Font := label1.Font;
  label4.Font.Color := clHotLight;
  label4.Font.Style := [fsUnderline];
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropSettings,
    CLSID_EPG2XMLPropertyPage
  );

end.
