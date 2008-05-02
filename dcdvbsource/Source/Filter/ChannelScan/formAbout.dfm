object frmAbout: TfrmAbout
  Left = 445
  Top = 245
  BorderStyle = bsSingle
  Caption = '[Info]'
  ClientHeight = 289
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 398
    Height = 273
    TabOrder = 0
    object Image1: TImage
      Left = 23
      Top = 29
      Width = 140
      Height = 80
      Cursor = crHandPoint
      AutoSize = True
      Transparent = True
      OnClick = statictext1Click
    end
    object Label1: TLabel
      Left = 207
      Top = 66
      Width = 140
      Height = 13
      Caption = '(C) Milenko "DCoder" Mitrovic'
      Transparent = True
    end
    object Label5: TLabel
      Left = 206
      Top = 98
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
      Left = 206
      Top = 82
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
      Left = 234
      Top = 98
      Width = 104
      Height = 13
      Cursor = crHandPoint
      Caption = 'dcoder@dsp-worx.de'
      Transparent = True
      OnClick = label4Click
      OnMouseMove = label4MouseMove
      OnMouseLeave = label4MouseLeave
    end
    object statictext1: TLabel
      Left = 234
      Top = 82
      Width = 121
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://www.dsp-worx.de'
      Transparent = True
      OnClick = statictext1Click
      OnMouseEnter = statictext1MouseEnter
      OnMouseLeave = statictext1MouseLeave
    end
    object label6: TLabel
      Left = 197
      Top = 30
      Width = 161
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'DVB/ATSC Channel Scan'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 197
      Top = 46
      Width = 161
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Version 0.1.7'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 133
      Width = 376
      Height = 128
      Caption = ' License '
      TabOrder = 0
      object Label8: TLabel
        Left = 16
        Top = 24
        Width = 349
        Height = 44
        AutoSize = False
        Caption = 
          'This software is provided '#39'as-is'#39', without any expressed or impl' +
          'ied warranty. In no event will the author(s) be held liable for ' +
          'any damages arising from the use of this software.'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 16
        Top = 72
        Width = 345
        Height = 40
        AutoSize = False
        Caption = 
          'Permission is granted to anyone to use this software for persona' +
          'l purpose. If you use this software in a product, an acknowledgm' +
          'ent by the author(s) is needed.'
        WordWrap = True
      end
    end
  end
end
