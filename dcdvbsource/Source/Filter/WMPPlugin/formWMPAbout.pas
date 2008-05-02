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

unit formWMPAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, ShellAPI, ComCtrls, PropAbout;

type
  TfrmWMPAbout = class(TForm)
    TabControl1: TTabControl;
    Image1: TImage;
    Label1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    label4: TLabel;
    statictext1: TLabel;
    label6: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    procedure statictext1Click(Sender: TObject);
    procedure label4Click(Sender: TObject);
    procedure statictext1MouseEnter(Sender: TObject);
    procedure label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure statictext1MouseLeave(Sender: TObject);
    procedure label4MouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWMPAbout: TfrmWMPAbout;

implementation

{$R *.dfm}

procedure TfrmWMPAbout.statictext1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmWMPAbout.label4Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:dcoder@dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmWMPAbout.statictext1MouseEnter(Sender: TObject);
begin
  StaticText1.Font.Color := clHotLight;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TfrmWMPAbout.label4MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  label4.Font.Color := clHotLight;
  label4.Font.Style := [fsUnderline];
end;

procedure TfrmWMPAbout.statictext1MouseLeave(Sender: TObject);
begin
  StaticText1.Font := label1.Font;
end;

procedure TfrmWMPAbout.label4MouseLeave(Sender: TObject);
begin
  label4.Font := label1.Font;
end;

procedure TfrmWMPAbout.FormCreate(Sender: TObject);
var
  frm: TFormPropAbout;
begin
  frm := TFormPropAbout.Create(nil);
  Image1.Picture.Assign(frm.Image1.Picture);
  frm.Free;
  
  Screen.Cursors[1] := LoadCursor(0, IDC_HAND);
  Image1.Cursor := 1;
  statictext1.Cursor := 1;
  label4.Cursor := 1;
end;

end.
