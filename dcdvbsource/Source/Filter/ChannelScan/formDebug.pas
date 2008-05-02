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

unit formDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ClipBrd;

type
  TfrmDebug = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Clear1: TMenuItem;
    Save1: TMenuItem;
    Copy1: TMenuItem;
    Edit1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
  private
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Add(AMsg: String);
  end;

var
  frmDebug: TfrmDebug;

implementation

{$R *.dfm}

procedure TfrmDebug.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  params.ExStyle := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := GetDesktopWindow;
end;

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  Memo1.Align := alClient;
end;

procedure TfrmDebug.Add(AMsg: String);
begin
  memo1.Lines.Add(AMsg);
  Memo1.Perform(EM_LINESCROLL, 0, Memo1.Lines.count);
end;

procedure TfrmDebug.Clear1Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TfrmDebug.Save1Click(Sender: TObject);
begin
  if SaveDialog1.Execute
    then Memo1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmDebug.Copy1Click(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Text;
end;

end.
