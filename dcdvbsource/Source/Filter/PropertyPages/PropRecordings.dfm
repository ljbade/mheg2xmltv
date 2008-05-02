object FormPropRecordings: TFormPropRecordings
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Recordings'
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
      Left = 301
      Top = 16
      Width = 81
      Height = 21
      Caption = 'Delete'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 16
      Width = 81
      Height = 21
      Caption = 'Update'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 111
      Top = 16
      Width = 81
      Height = 21
      Caption = 'Add'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 206
      Top = 16
      Width = 81
      Height = 21
      Caption = 'Edit'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 397
      Top = 16
      Width = 82
      Height = 21
      Caption = 'Delete All'
      TabOrder = 4
      OnClick = Button5Click
    end
    object ListView1: TListView
      Left = 16
      Top = 48
      Width = 465
      Height = 289
      Columns = <
        item
          Caption = 'Name'
          Width = 77
        end
        item
          Alignment = taCenter
          Caption = 'Start'
          Width = 123
        end
        item
          Alignment = taCenter
          Caption = 'End'
          Width = 123
        end
        item
          Caption = 'Channel'
          Width = 74
        end
        item
          Caption = 'Status'
          Width = 64
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ParentShowHint = False
      ShowHint = False
      TabOrder = 5
      ViewStyle = vsReport
      OnChange = ListView1Change
      OnDblClick = ListView1DblClick
      OnMouseDown = ListView1MouseDown
    end
  end
end
