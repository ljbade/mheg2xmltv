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
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, ComCtrls, ShellAPI, DCDVBShared, DCDVBTuningPlugins,
  Dialogs;

const
  CLSID_FileReaderPropertyPageAbout: TGuid = '{831982D5-78EF-41C1-809F-DC2423492BBD}';

type
  TfrmPropAbout = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    label6: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    label4: TLabel;
    statictext1: TLabel;
    Label2: TLabel;
    procedure TabControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure statictext1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure statictext1Click(Sender: TObject);
    procedure label4Click(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TfrmPropAbout.TabControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font := label1.Font;
end;

procedure TfrmPropAbout.statictext1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label1.Font;
  StaticText1.Font.Color := clHotLight;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TfrmPropAbout.label4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StaticText1.Font := label1.Font;
  label4.Font.Color := clHotLight;
  label4.Font.Style := [fsUnderline];
end;

procedure TfrmPropAbout.statictext1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPropAbout.label4Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:dcoder@dsp-worx.de',nil, nil, SW_SHOWNORMAL);
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropAbout,
    CLSID_FileReaderPropertyPageAbout
  );

end.
