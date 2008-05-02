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

unit formWMPPlugin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, WMPPluginAPI, DVBFilter, ActiveX,
  BDAUtils, ComCtrls;

type
  TfrmWMPPlugin = class(TWMPUIPluginForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Resize(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    FFilter: TDCDVBSource;
    procedure OnChannelChange(AIndex: Integer);
  protected
    FListViewWndProc: TWndMethod;
    procedure ListViewWndProc(var Msg: TMessage);
  public
    procedure UpdateList;
    procedure ClearList;
  end;

implementation

{$R *.dfm}

procedure TfrmWMPPlugin.ListViewWndProc(var Msg: TMessage);
begin
  ShowScrollBar(ListView1.Handle, SB_HORZ, False);
  FListViewWndProc(Msg);
end;

procedure TfrmWMPPlugin.UpdateList;
var
  map: THandle;
  view: Pointer;
  tmp: Cardinal;
  channels: Integer;
  i: Integer;
  cn: PChar;
  li: TListItem;
begin
  ClearList;
  ListView1.Items.BeginUpdate;

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
        FFilter.OnChannelChange1 := OnChannelChange;
        channels := 0;
        if FFilter.get_ChannelCount(channels) = S_OK then
        begin
          for i := 0 to channels -1 do
          begin
            FFilter.get_ChannelInfo(i, cn);
            li := ListView1.Items.Add;
            li.Caption := inttostr(i+1);
            li.SubItems.Add(RemoveUnwantedChars(cn));
            CoTaskMemFree(cn);
          end;
        end;
        channels := -1;
        FFilter.get_ChannelSelected(channels);

        if channels = -1 then
        begin
          ListView1.ItemFocused := nil;
        end else
        if (channels < ListView1.Items.Count) then
        begin
          ListView1.Selected := ListView1.Items[channels];
        end;

      end;
    end;

    CloseHandle(map);
  end;

  ListView1.Items.EndUpdate;
end;

procedure TfrmWMPPlugin.ClearList;
begin
  ListView1.Clear;
end;

procedure TfrmWMPPlugin.FormCreate(Sender: TObject);
begin
  FListViewWndProc := ListView1.WindowProc;
  ListView1.WindowProc := ListViewWndProc;
  ListView1.Align := alClient;
  UpdateList;
end;

procedure TfrmWMPPlugin.FormDestroy(Sender: TObject);
begin
  if Assigned(FFilter)
    then FFilter.OnChannelChange1 := nil;
  Pointer(FFilter) := nil;
  ListView1.WindowProc := FListViewWndProc; 
  FListViewWndProc := nil;
end;

procedure TfrmWMPPlugin.ListView1Resize(Sender: TObject);
begin
  ListView1.Columns[1].Width := ListView1.Width - ListView1.Columns[0].Width
end;

procedure TfrmWMPPlugin.ListView1DblClick(Sender: TObject);
begin
  if not Assigned(FFilter) or (ListView1.Selected = nil)
    then Exit;

  FFilter.put_ChannelSelected(ListView1.Selected.Index);
end;

procedure TfrmWMPPlugin.OnChannelChange(AIndex: Integer);
begin
  ListView1.Selected := ListView1.Items[AIndex];
end;

end.
