object frmFrequency: TfrmFrequency
  Left = 437
  Top = 278
  BorderStyle = bsSingle
  Caption = 'Frequency'
  ClientHeight = 201
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    353
    201)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 265
    Top = 159
    Width = 72
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 185
    Top = 159
    Width = 72
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 153
    Height = 59
    Caption = ' Frequency '
    TabOrder = 2
    object SpinEdit10: TSpinEdit
      Left = 16
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
  end
  object GroupBox2: TGroupBox
    Left = 184
    Top = 16
    Width = 153
    Height = 59
    Caption = ' Bandwidth '
    TabOrder = 3
    object SpinEdit1: TSpinEdit
      Left = 16
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 88
    Width = 153
    Height = 59
    Caption = 'Symbolrate '
    TabOrder = 4
    object SpinEdit2: TSpinEdit
      Left = 16
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
  end
  object GroupBox4: TGroupBox
    Left = 184
    Top = 88
    Width = 153
    Height = 59
    Caption = ' Polarisation '
    TabOrder = 5
    object ComboBox1: TComboBox
      Left = 16
      Top = 24
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Horizontal'
      Items.Strings = (
        'Horizontal'
        'Vertical')
    end
  end
end
