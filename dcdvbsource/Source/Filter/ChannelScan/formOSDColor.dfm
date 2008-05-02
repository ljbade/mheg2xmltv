object frmOSDColor: TfrmOSDColor
  Left = 378
  Top = 162
  BorderStyle = bsSingle
  Caption = 'OSD Color'
  ClientHeight = 427
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 383
    Height = 411
    TabOrder = 0
    object Button1: TButton
      Left = 292
      Top = 368
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 204
      Top = 368
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 351
      Height = 193
      Caption = ' Colors '
      TabOrder = 2
      object Label1: TLabel
        Left = 16
        Top = 34
        Width = 48
        Height = 13
        Caption = 'Font Color'
      end
      object Label2: TLabel
        Left = 16
        Top = 74
        Width = 90
        Height = 13
        Caption = 'Font Shadow Color'
      end
      object Label3: TLabel
        Left = 16
        Top = 114
        Width = 65
        Height = 13
        Caption = 'Gradient Start'
      end
      object Label4: TLabel
        Left = 16
        Top = 154
        Width = 62
        Height = 13
        Caption = 'Gradient End'
      end
      object Panel1: TPanel
        Left = 120
        Top = 104
        Width = 121
        Height = 33
        ParentBackground = False
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 120
        Top = 24
        Width = 121
        Height = 33
        ParentBackground = False
        TabOrder = 1
      end
      object Panel3: TPanel
        Left = 120
        Top = 144
        Width = 121
        Height = 33
        ParentBackground = False
        TabOrder = 2
      end
      object Panel4: TPanel
        Left = 120
        Top = 64
        Width = 121
        Height = 33
        ParentBackground = False
        TabOrder = 3
      end
      object Button3: TButton
        Left = 258
        Top = 27
        Width = 75
        Height = 25
        Caption = 'Select'
        TabOrder = 4
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 258
        Top = 67
        Width = 75
        Height = 25
        Caption = 'Select'
        TabOrder = 5
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 258
        Top = 107
        Width = 75
        Height = 25
        Caption = 'Select'
        TabOrder = 6
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 258
        Top = 147
        Width = 75
        Height = 25
        Caption = 'Select'
        TabOrder = 7
        OnClick = Button6Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 220
      Width = 351
      Height = 133
      Caption = ' Preview '
      TabOrder = 3
      object Image1: TImage
        Left = 10
        Top = 17
        Width = 331
        Height = 105
      end
    end
    object Button7: TButton
      Left = 16
      Top = 368
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 4
      OnClick = Button7Click
    end
  end
  object ColorDialog1: TColorDialog
    Left = 16
    Top = 16
  end
end
