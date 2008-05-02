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

unit formNetwork;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmNetwork = class(TForm)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    SpinEdit3: TSpinEdit;
    SpinEdit5: TSpinEdit;
    Label6: TLabel;
    SpinEdit4: TSpinEdit;
    Label3: TLabel;
    SpinEdit2: TSpinEdit;
    Label5: TLabel;
    SpinEdit9: TSpinEdit;
    Label2: TLabel;
    SpinEdit7: TSpinEdit;
    Label4: TLabel;
    SpinEdit6: TSpinEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    SpinEdit13: TSpinEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label17: TLabel;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Label19: TLabel;
    ComboBox6: TComboBox;
    Label9: TLabel;
    SpinEdit1: TSpinEdit;
    ComboBox7: TComboBox;
    Label15: TLabel;
    GroupBox1: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label16: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    SpinEdit8: TSpinEdit;
    SpinEdit11: TSpinEdit;
    SpinEdit12: TSpinEdit;
    SpinEdit14: TSpinEdit;
    ComboBox8: TComboBox;
    ComboBox10: TComboBox;
    ComboBox12: TComboBox;
    Label22: TLabel;
    Label25: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    SpinEdit10: TSpinEdit;
    SpinEdit16: TSpinEdit;
    ComboBox9: TComboBox;
    ComboBox11: TComboBox;
    ComboBox13: TComboBox;
    procedure ComboBox7Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNetwork: TfrmNetwork;

implementation

{$R *.dfm}

procedure TfrmNetwork.ComboBox7Change(Sender: TObject);
begin
  GroupBox1.Visible := False;
  GroupBox3.Visible := False;
  GroupBox4.Visible := False;
  GroupBox5.Visible := False;
  case ComboBox7.ItemIndex of
    0:
    begin
      ClientHeight := 193;
    end;
    1:
    begin
      GroupBox3.Visible := True;
      ClientHeight := 401;
    end;
    2:
    begin
      GroupBox5.Visible := True;
      ClientHeight := 401 - 72;
    end;
    3:
    begin
      GroupBox4.Visible := True;
      ClientHeight := 401 - 94;
    end;
    4:
    begin
      GroupBox1.Visible := True;
      ClientHeight := 401;
    end;
  end;
end;

procedure TfrmNetwork.FormShow(Sender: TObject);
begin
  ComboBox7Change(Self);
end;

procedure TfrmNetwork.FormCreate(Sender: TObject);
begin
  BorderIcons := [biSystemMenu];
end;

end.
