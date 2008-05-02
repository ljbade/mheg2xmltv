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

unit formFrequency;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls;

type
  TfrmFrequency = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    SpinEdit10: TSpinEdit;
    GroupBox2: TGroupBox;
    SpinEdit1: TSpinEdit;
    GroupBox3: TGroupBox;
    SpinEdit2: TSpinEdit;
    GroupBox4: TGroupBox;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetupType(AType: Integer);
  end;

var
  frmFrequency: TfrmFrequency;

implementation

{$R *.dfm}

procedure TfrmFrequency.SetupType(AType: Integer);
begin
  case AType of
    0:
    begin
      GroupBox3.Visible := False;
      GroupBox4.Visible := False;
      ClientHeight := 135;
    end;
    1:
    begin
      GroupBox2.Visible := False;
      GroupBox4.Visible := False;
      GroupBox3.Left := GroupBox2.Left;
      GroupBox3.Top := GroupBox2.Top;
      ClientHeight := 135;
    end;
    2:
    begin
      GroupBox2.Visible := False;
      GroupBox4.Left := GroupBox3.Left;
      GroupBox4.Top := GroupBox3.Top;
      GroupBox3.Left := GroupBox2.Left;
      GroupBox3.Top := GroupBox2.Top;
    end;
    3:
    begin
      GroupBox3.Visible := False;
      GroupBox4.Visible := False;
      GroupBox2.Visible := False;
      ClientHeight := 135;
    end;
  end;
end;

procedure TfrmFrequency.FormCreate(Sender: TObject);
begin
  BorderIcons := [biSystemMenu];
end;

end.
