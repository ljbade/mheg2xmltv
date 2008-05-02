object frmParameters: TfrmParameters
  Left = 490
  Top = 287
  BorderStyle = bsSingle
  Caption = 'Commandline Parameters'
  ClientHeight = 145
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 398
    Height = 129
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 38
      Height = 13
      Caption = 'Usage:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 80
      Top = 16
      Width = 136
      Height = 13
      Caption = 'ChannelScan.exe [switches]'
    end
    object Label3: TLabel
      Left = 80
      Top = 48
      Width = 177
      Height = 13
      Caption = 'Enables scanning from RAW PSI Files'
    end
    object Label4: TLabel
      Left = 16
      Top = 48
      Width = 48
      Height = 13
      Caption = '/psiscan'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 80
      Top = 64
      Width = 180
      Height = 13
      Caption = 'Writes RAW PSI Files during Scanning'
    end
    object Label6: TLabel
      Left = 16
      Top = 64
      Width = 51
      Height = 13
      Caption = '/writepsi'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 80
      Top = 80
      Width = 193
      Height = 13
      Caption = 'Writes PSI Tables to OutputDebugString'
    end
    object Label8: TLabel
      Left = 16
      Top = 80
      Width = 49
      Height = 13
      Caption = '/printpsi'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label9: TLabel
      Left = 80
      Top = 96
      Width = 51
      Height = 13
      Caption = 'This Dialog'
    end
    object Label10: TLabel
      Left = 16
      Top = 96
      Width = 12
      Height = 13
      Caption = '/?'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
