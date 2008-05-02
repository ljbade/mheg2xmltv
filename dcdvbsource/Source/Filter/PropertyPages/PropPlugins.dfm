object FormPropPlugins: TFormPropPlugins
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Plugins'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 497
    Height = 353
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 160
      Top = 20
      Width = 321
      Height = 318
      TabOrder = 1
    end
    object CheckListBox1: TCheckListBox
      Left = 16
      Top = 20
      Width = 129
      Height = 317
      OnClickCheck = CheckListBox1ClickCheck
      ItemHeight = 13
      TabOrder = 0
      OnClick = CheckListBox1Click
    end
  end
end
