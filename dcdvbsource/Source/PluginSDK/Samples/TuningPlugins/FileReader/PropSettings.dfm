object frmPropSettings: TfrmPropSettings
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Settings'
  ClientHeight = 260
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
    Left = -8
    Top = -8
    Width = 801
    Height = 745
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 297
      Height = 243
      Caption = ' Frequency/File Mappings '
      TabOrder = 0
      object ListView1: TListView
        Left = 8
        Top = 16
        Width = 279
        Height = 193
        Columns = <
          item
            Caption = 'Frequency'
            Width = 70
          end
          item
            AutoSize = True
            Caption = 'File'
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ListView1Change
        OnEdited = ListView1Edited
      end
      object Button1: TButton
        Left = 8
        Top = 215
        Width = 75
        Height = 19
        Caption = 'Add'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 89
        Top = 215
        Width = 75
        Height = 19
        Caption = 'Remove'
        Enabled = False
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 192
        Top = 215
        Width = 95
        Height = 19
        Caption = 'Remove All'
        Enabled = False
        TabOrder = 3
        OnClick = Button3Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'ts'
    Filter = 'Transport Stream (*.ts)|*.ts'
    Options = [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 224
  end
end
