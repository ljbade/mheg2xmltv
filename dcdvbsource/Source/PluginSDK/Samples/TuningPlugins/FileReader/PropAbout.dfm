object frmPropAbout: TfrmPropAbout
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 260
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnMouseMove = TabControl1MouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = -8
    Top = -8
    Width = 801
    Height = 745
    TabOrder = 0
    OnMouseMove = TabControl1MouseMove
    object label6: TLabel
      Left = 102
      Top = 52
      Width = 120
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'TS-File Reader'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 91
      Top = 130
      Width = 140
      Height = 13
      Caption = '(C) Milenko "DCoder" Mitrovic'
      Transparent = True
      OnMouseMove = TabControl1MouseMove
    end
    object Label5: TLabel
      Left = 90
      Top = 162
      Width = 23
      Height = 13
      Caption = 'Mail'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label7: TLabel
      Left = 90
      Top = 146
      Width = 25
      Height = 13
      Caption = 'Web'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object label4: TLabel
      Left = 118
      Top = 162
      Width = 104
      Height = 13
      Cursor = crHandPoint
      Caption = 'dcoder@dsp-worx.de'
      Transparent = True
      OnClick = label4Click
      OnMouseMove = label4MouseMove
    end
    object statictext1: TLabel
      Left = 118
      Top = 146
      Width = 121
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://www.dsp-worx.de'
      Transparent = True
      OnClick = statictext1Click
      OnMouseMove = statictext1MouseMove
    end
    object Label2: TLabel
      Left = 102
      Top = 68
      Width = 120
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Version 0.0.0.2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
