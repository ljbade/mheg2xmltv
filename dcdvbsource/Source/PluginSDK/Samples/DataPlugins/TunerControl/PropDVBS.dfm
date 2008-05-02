object frmPropDVBS: TfrmPropDVBS
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'DVB-s'
  ClientHeight = 288
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = -16
    Top = -16
    Width = 801
    Height = 745
    TabOrder = 0
    object Label14: TLabel
      Left = 32
      Top = 40
      Width = 51
      Height = 13
      Caption = 'Frequency'
    end
    object Label15: TLabel
      Left = 288
      Top = 40
      Width = 17
      Height = 13
      Caption = 'kHz'
    end
    object Label1: TLabel
      Left = 32
      Top = 72
      Width = 54
      Height = 13
      Caption = 'Symbolrate'
    end
    object Label2: TLabel
      Left = 288
      Top = 72
      Width = 25
      Height = 13
      Caption = 'ksb/s'
    end
    object Label3: TLabel
      Left = 32
      Top = 104
      Width = 55
      Height = 13
      Caption = 'Polarization'
    end
    object SpinEdit7: TSpinEdit
      Left = 96
      Top = 37
      Width = 177
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object SpinEdit1: TSpinEdit
      Left = 96
      Top = 69
      Width = 177
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object ComboBox2: TComboBox
      Left = 96
      Top = 101
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'Horizontal'
      Items.Strings = (
        'Horizontal'
        'Vertical')
    end
    object Button1: TButton
      Left = 236
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Submit'
      TabOrder = 3
      OnClick = Button1Click
    end
  end
end
