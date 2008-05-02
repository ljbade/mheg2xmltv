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
  ExtCtrls, Forms, DCDVBDataPlugins, ComCtrls, Menus, DCDVBShared;

const
  CLSID_PIDCounterPropertyPage: TGuid = '{3683F4F7-6927-49B2-AD0D-7DD994E8015E}';
  IID_IPIDCounter:              TGuid = '{47B412FA-24B1-4283-8205-F8F9D5F58DA7}';

type
  IPIDCounter = interface(IUnknown)
  ['{47B412FA-24B1-4283-8205-F8F9D5F58DA7}']
    function Reset: HRESULT; stdcall;
    function GetStatistics(out AStatistics: Pointer; out ACount: Integer): HRESULT; stdcall;
  end;

  TfrmPropSettings = class(TDCDVBFormPropertyPage)
    Timer1: TTimer;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Reset1: TMenuItem;
    procedure Reset1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    FIntf: IPIDCounter;
    FUpSort: Boolean;
    FSortIndex: Integer;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

uses
  UPIDCounter, Math;

function TfrmPropSettings.OnConnect(Unknown: IInterface): HRESULT;
begin
  Unknown.QueryInterface(IID_IPIDCounter, FIntf);
  FUpSort := True;
  Timer1.Enabled := True;
  Timer1Timer(Self);
  Result := S_OK;
end;

function TfrmPropSettings.OnDisconnect: HRESULT;
begin
  Timer1.Enabled := False;
  FIntf := nil;
  Result := S_OK;
end;

procedure TfrmPropSettings.Reset1Click(Sender: TObject);
begin
  if Assigned(FIntf) then
  begin
    FIntf.Reset;
    ListView1.Clear;
    Timer1Timer(Self);
  end;
end;

procedure TfrmPropSettings.Timer1Timer(Sender: TObject);

  function GetItemFromPID(APID: Integer): TListItem;
  var
    i: Integer;
    li: TListItem;
    s: String;
  begin
    for i := 0 to ListView1.Items.Count -1 do
    begin
      li := ListView1.Items[i];
      s := '$' + Copy(li.Caption, 3, 4);
      if StrToInt(s) = APID then
      begin
        Result := li;
        Exit;
      end;
    end;

    Result := nil;
  end;

var
  pt: Pointer;
  count: Integer;
  pid: PPIDItem;
  i: Integer;
  li: TListItem;
begin
  if Assigned(FIntf) then
  begin
//    ListView1.Clear;
    if FIntf.GetStatistics(pt, count) = S_OK then
    begin
      pid := PPIDItem(pt);
      ListView1.Items.BeginUpdate;
      for i := 0 to count -1 do
      begin
        if pid.Count > 1 then
        begin
          li := GetItemFromPID(pid.PID);
          if Assigned(li) then
          begin
            li.SubItems.Strings[0] := inttostr(pid.Count);
          end else
          begin
            li := ListView1.Items.Add;
            li.Caption := '0x' + inttohex(pid.PID, 4) + ' (' + inttostr(pid.PID) + ')';
            li.SubItems.Add(inttostr(pid.Count));
          end;
        end;
        inc(pid);
      end;
      ListView1.Items.EndUpdate;
      CoTaskMemFree(pt);
    end;
  end;
end;

function ByFourth(Item1, Item2: Integer; Data: integer):
  integer; stdcall;
var
  it1, it2: TListItem;
begin
  Result := 0;

  it1 := TListItem(Item1);
  it2 := TListItem(Item2);

  case LoWord(Data) of
    0:
    begin
      Result := CompareStr(it1.Caption, it2.Caption) * IfThen(HiWord(Data) <> 0, 1, -1);
    end;
    1:
    begin
      if (it1.SubItems.Count = 0) or (it2.SubItems.Count = 0)
        then Exit;
      Result := (strtoint(it1.SubItems[0]) - strtoint(it2.SubItems[0])) * IfThen(HiWord(Data) <> 0, 1, -1);
    end;
  end;
end;

procedure TfrmPropSettings.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  FUpSort := not FUpSort;
  FSortIndex := Column.Index;
  ListView1.CustomSort(ByFourth, MakeLong(FSortIndex, Word(FUpSort)));
end;

procedure TfrmPropSettings.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  case FSortIndex of
    0:
    begin
      Compare := CompareStr(Item1.Caption, Item2.Caption) * IfThen(FUpSort, 1, -1);
    end;
    1:
    begin
      if (Item1.SubItems.Count = 0) or (Item2.SubItems.Count = 0)
        then Exit;
      Compare := (strtoint(Item1.SubItems[0]) - strtoint(Item2.SubItems[0])) * IfThen(FUpSort, 1, -1);
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropSettings,
    CLSID_PIDCounterPropertyPage
  );

end.
