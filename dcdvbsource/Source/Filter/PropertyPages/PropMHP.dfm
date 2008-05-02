object FormPropMHP: TFormPropMHP
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Applications'
  ClientHeight = 369
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 497
    Height = 353
    TabOrder = 0
    object Button1: TButton
      Left = 406
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Update'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 16
      Width = 121
      Height = 21
      Caption = 'Execute Application'
      Enabled = False
      TabOrder = 1
      OnClick = Button2Click
    end
    object TreeView1: TTreeView
      Left = 16
      Top = 48
      Width = 465
      Height = 289
      HideSelection = False
      HotTrack = True
      Indent = 19
      ReadOnly = True
      TabOrder = 2
      OnChange = TreeView1Change
      OnMouseDown = TreeView1MouseDown
    end
  end
end
