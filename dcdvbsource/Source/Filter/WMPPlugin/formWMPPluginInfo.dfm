object frmWMPPluginInfo: TfrmWMPPluginInfo
  Left = 194
  Top = 134
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'TV Info'
  ClientHeight = 326
  ClientWidth = 360
  Color = clBlack
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 0
    Top = 31
    Width = 360
    Height = 295
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 5
    ExplicitHeight = 18
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    Color = clBlack
    TabOrder = 0
    object Label1: TLabel
      Left = 184
      Top = -8
      Width = 6
      Height = 23
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ShowAccelChar = False
    end
    object Label3: TLabel
      Left = 192
      Top = -8
      Width = 6
      Height = 23
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ShowAccelChar = False
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 16
    Top = 64
  end
end
