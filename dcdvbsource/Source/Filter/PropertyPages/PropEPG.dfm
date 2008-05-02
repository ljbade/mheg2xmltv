object FormPropEPG: TFormPropEPG
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'EPG'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 497
    Height = 353
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 39
      Height = 13
      Caption = 'Channel'
    end
    object WebBrowser1: TWebBrowser
      Left = 16
      Top = 48
      Width = 465
      Height = 289
      TabOrder = 3
      OnBeforeNavigate2 = WebBrowser1BeforeNavigate2
      ControlData = {
        4C0000000F300000DE1D00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126202000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
    object ListView1: TListView
      Left = 16
      Top = 48
      Width = 465
      Height = 289
      Columns = <
        item
          Caption = 'Date'
          Width = 100
        end
        item
          Alignment = taCenter
          Caption = 'Start'
          Width = 60
        end
        item
          Alignment = taCenter
          Caption = 'End'
          Width = 60
        end
        item
          Caption = 'Event'
          Width = 224
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
      ViewStyle = vsReport
      OnCompare = ListView1Compare
      OnDblClick = ListView1DblClick
    end
    object Button1: TButton
      Left = 214
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Update'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 294
      Top = 16
      Width = 107
      Height = 21
      Caption = 'Clear all EPG Data'
      TabOrder = 1
      OnClick = Button2Click
    end
    object ComboBox1: TComboBox
      Left = 64
      Top = 16
      Width = 145
      Height = 21
      Style = csOwnerDrawFixed
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 15
      ParentFont = False
      TabOrder = 2
      OnChange = ComboBox1Change
      OnDrawItem = ComboBox1DrawItem
    end
    object Button3: TButton
      Left = 406
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Save as XML'
      TabOrder = 5
      OnClick = Button3Click
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML File (*.xml)|*.xml'
    Left = 24
    Top = 24
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 20
    OnTimer = Timer1Timer
    Left = 56
    Top = 24
  end
end
