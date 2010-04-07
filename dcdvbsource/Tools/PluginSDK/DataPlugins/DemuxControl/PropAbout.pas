(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004, 2005 Milenko Mitrovic                                *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  SDK for DC-DVB Filter Version 0.1.6                                      *
 *                                                                           *
 *  The Source Code is given "as is" without warranty of any kind. The       *
 *  Author is not responsible for any damage due to the use of this Code.    *
 *  The complete Source Code remains property of the Author and must be      *
 *  used only for creating Plugins for the DC-DVB Filter.                    *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit PropAbout;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBDataPlugins, ComCtrls, ShellAPI, DCDVBShared;

const
  CLSID_DemuxControlPropertyPageAbout: TGuid = '{39E03FBC-9049-41CB-B046-AF80EEB86A0C}';

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
    CLSID_DemuxControlPropertyPageAbout
  );

end.
