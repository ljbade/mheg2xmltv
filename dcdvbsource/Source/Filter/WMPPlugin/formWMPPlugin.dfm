object frmWMPPlugin: TfrmWMPPlugin
  Left = 194
  Top = 134
  BorderStyle = bsNone
  BorderWidth = 10
  Caption = 'TV Channels'
  ClientHeight = 323
  ClientWidth = 392
  Color = clBlack
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 104
    Top = 64
    Width = 250
    Height = 150
    BorderStyle = bsNone
    Color = clBlack
    Columns = <
      item
        Width = 30
      end
      item
        Width = 122
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
    OnResize = ListView1Resize
  end
end
