object frmLNBSelection: TfrmLNBSelection
  Left = 310
  Top = 266
  BorderStyle = bsSingle
  Caption = 'LNB Selection'
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
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 398
    Height = 273
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Default'
      object Label8: TLabel
        Left = 65
        Top = 81
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 64
        Top = 80
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 8
        Width = 361
        Height = 103
        Caption = ' Settings '
        TabOrder = 0
        Visible = False
        object Label1: TLabel
          Left = 16
          Top = 32
          Width = 25
          Height = 13
          Caption = 'LOF1'
        end
        object Label2: TLabel
          Left = 136
          Top = 64
          Width = 20
          Height = 13
          Caption = 'MHz'
        end
        object Label3: TLabel
          Left = 16
          Top = 64
          Width = 25
          Height = 13
          Caption = 'LOF2'
        end
        object Label4: TLabel
          Left = 136
          Top = 32
          Width = 20
          Height = 13
          Caption = 'MHz'
        end
        object Label5: TLabel
          Left = 192
          Top = 32
          Width = 31
          Height = 13
          Caption = 'Switch'
        end
        object Label6: TLabel
          Left = 320
          Top = 32
          Width = 20
          Height = 13
          Caption = 'MHz'
        end
        object SpinEdit1: TSpinEdit
          Left = 48
          Top = 60
          Width = 81
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 10600
        end
        object SpinEdit2: TSpinEdit
          Left = 48
          Top = 28
          Width = 81
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 9750
        end
        object SpinEdit3: TSpinEdit
          Left = 232
          Top = 28
          Width = 81
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 11700
        end
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Power On LNB'
        TabOrder = 1
        Visible = False
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Tone'
      ImageIndex = 2
      object Label9: TLabel
        Left = 64
        Top = 80
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label10: TLabel
        Left = 65
        Top = 81
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label15: TLabel
        Left = 64
        Top = 80
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'DiSEqC'
      ImageIndex = 1
      object Label11: TLabel
        Left = 64
        Top = 80
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label12: TLabel
        Left = 65
        Top = 81
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label13: TLabel
        Left = 64
        Top = 80
        Width = 284
        Height = 29
        Caption = 'Not yet implemented !!!'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
    end
  end
  object Button1: TButton
    Left = 316
    Top = 240
    Width = 73
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 231
    Top = 240
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
