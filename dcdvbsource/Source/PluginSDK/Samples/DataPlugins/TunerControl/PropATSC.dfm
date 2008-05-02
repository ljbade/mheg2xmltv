object frmPropATSC: TfrmPropATSC
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'ATSC'
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
    object Button1: TButton
      Left = 236
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Submit'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
