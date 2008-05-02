object FormPropStatistics: TFormPropStatistics
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Statistics'
  ClientHeight = 369
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 497
    Height = 353
    TabOrder = 0
    object GroupBox4: TGroupBox
      Left = 336
      Top = 208
      Width = 145
      Height = 129
      Caption = ' Signal '
      TabOrder = 0
      object Label6: TLabel
        Left = 16
        Top = 20
        Width = 50
        Height = 13
        Caption = 'Strength'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 16
        Top = 51
        Width = 40
        Height = 13
        Caption = 'Quality'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label8: TLabel
        Left = 80
        Top = 20
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '-1'
      end
      object Label1: TLabel
        Left = 80
        Top = 51
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '-1'
      end
      object Label10: TLabel
        Left = 16
        Top = 100
        Width = 40
        Height = 13
        Caption = 'Locked'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 16
        Top = 84
        Width = 44
        Height = 13
        Caption = 'Present'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label12: TLabel
        Left = 80
        Top = 100
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'False'
      end
      object Label13: TLabel
        Left = 80
        Top = 84
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'False'
      end
      object ProgressBar1: TProgressBar
        Left = 14
        Top = 36
        Width = 116
        Height = 13
        Smooth = True
        TabOrder = 0
      end
      object ProgressBar2: TProgressBar
        Left = 14
        Top = 68
        Width = 116
        Height = 13
        TabOrder = 1
      end
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 465
      Height = 183
      Caption = ' Transportstream '
      TabOrder = 1
      object Image1: TImage
        Left = 17
        Top = 24
        Width = 431
        Height = 141
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 208
      Width = 145
      Height = 129
      Caption = ' Video '
      TabOrder = 2
      object Label2: TLabel
        Left = 16
        Top = 20
        Width = 20
        Height = 13
        Caption = 'PID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 16
        Top = 36
        Width = 28
        Height = 13
        Caption = 'Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 16
        Top = 52
        Width = 23
        Height = 13
        Caption = 'Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 16
        Top = 100
        Width = 60
        Height = 13
        Caption = 'Framerate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 16
        Top = 84
        Width = 39
        Height = 13
        Caption = 'Bitrate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label21: TLabel
        Left = 64
        Top = 20
        Width = 65
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0x0311'
      end
      object Label22: TLabel
        Left = 64
        Top = 36
        Width = 65
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'MPEG-2'
      end
      object Label23: TLabel
        Left = 64
        Top = 52
        Width = 65
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '720x576'
      end
      object Label24: TLabel
        Left = 80
        Top = 68
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '16:9'
      end
      object Label25: TLabel
        Left = 80
        Top = 100
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '25 fps'
      end
      object Label26: TLabel
        Left = 56
        Top = 84
        Width = 73
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '6500 kbps'
      end
      object Label5: TLabel
        Left = 16
        Top = 68
        Width = 66
        Height = 13
        Caption = 'Aspectratio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object GroupBox3: TGroupBox
      Left = 176
      Top = 208
      Width = 145
      Height = 129
      Caption = ' Audio '
      TabOrder = 3
      object Label15: TLabel
        Left = 16
        Top = 20
        Width = 20
        Height = 13
        Caption = 'PID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label16: TLabel
        Left = 16
        Top = 40
        Width = 28
        Height = 13
        Caption = 'Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label17: TLabel
        Left = 16
        Top = 100
        Width = 39
        Height = 13
        Caption = 'Bitrate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label18: TLabel
        Left = 80
        Top = 20
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0x0322'
      end
      object Label19: TLabel
        Left = 56
        Top = 40
        Width = 73
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'AC3'
      end
      object Label20: TLabel
        Left = 80
        Top = 60
        Width = 49
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '6'
      end
      object Label27: TLabel
        Left = 16
        Top = 60
        Width = 51
        Height = 13
        Caption = 'Channels'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label28: TLabel
        Left = 56
        Top = 100
        Width = 73
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '384 kbps'
      end
      object Label29: TLabel
        Left = 16
        Top = 80
        Width = 66
        Height = 13
        Caption = 'Samplerate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label31: TLabel
        Left = 84
        Top = 80
        Width = 45
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '48000'
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 990
    OnTimer = Timer1Timer
    Left = 16
    Top = 16
  end
end
