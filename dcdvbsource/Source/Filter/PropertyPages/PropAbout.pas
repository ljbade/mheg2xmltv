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

unit PropAbout;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  DVBInterface, ShellAPI, pngimage, ComCtrls;

type
  TFormPropAbout = class(TFormPropertyPage)
    TabControl1: TTabControl;
    Image1: TImage;
    label6: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    label4: TLabel;
    statictext1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox2: TGroupBox;
    Label11: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure statictext1Click(Sender: TObject);
    procedure label4Click(Sender: TObject);
    procedure statictext1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  end;

implementation

{$R *.DFM}

procedure TFormPropAbout.statictext1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TFormPropAbout.label4Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:dcoder@dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TFormPropAbout.statictext1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font.Color := clHotLight;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TFormPropAbout.label4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StaticText1.Font := label1.Font;
  label4.Font.Color := clHotLight;
  label4.Font.Style := [fsUnderline];
end;

procedure TFormPropAbout.FormCreate(Sender: TObject);
begin
  Screen.Cursors[1] := LoadCursor(0, IDC_HAND);
  Image1.Cursor := 1;
  statictext1.Cursor := 1;
  label4.Cursor := 1;
end;

procedure TFormPropAbout.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font := label1.Font;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropAbout, CLSID_PropertyPageAbout);
  
end.
