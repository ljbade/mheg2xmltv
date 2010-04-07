object frmPropSettings: TfrmPropSettings
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Settings'
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
    object GroupBox1: TGroupBox
      Left = 24
      Top = 24
      Width = 295
      Height = 57
      Caption = ' Video PID '
      TabOrder = 0
      object SpinEdit1: TSpinEdit
        Left = 14
        Top = 20
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 0
        Value = -1
      end
      object ComboBox1: TComboBox
        Left = 91
        Top = 20
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'MPEG-2'
        Items.Strings = (
          'MPEG-2'
          'AVC')
      end
      object Button1: TButton
        Left = 205
        Top = 20
        Width = 75
        Height = 21
        Caption = 'Set'
        TabOrder = 2
        OnClick = Button1Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 24
      Top = 88
      Width = 295
      Height = 57
      Caption = ' Audio PID '
      TabOrder = 1
      object SpinEdit2: TSpinEdit
        Left = 14
        Top = 20
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 0
        Value = -1
      end
      object ComboBox2: TComboBox
        Left = 91
        Top = 20
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'MPEG-1 Audio'
        Items.Strings = (
          'MPEG-1 Audio'
          'MPEG-2 Audio'
          'AC3'
          'DTS'
          'AAC')
      end
      object Button2: TButton
        Left = 205
        Top = 20
        Width = 75
        Height = 21
        Caption = 'Set'
        TabOrder = 2
        OnClick = Button2Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 24
      Top = 152
      Width = 295
      Height = 57
      Caption = ' Teletext PID '
      TabOrder = 2
      object SpinEdit3: TSpinEdit
        Left = 14
        Top = 20
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 0
        Value = -1
      end
      object Button3: TButton
        Left = 205
        Top = 20
        Width = 75
        Height = 21
        Caption = 'Set'
        TabOrder = 1
        OnClick = Button3Click
      end
    end
    object GroupBox4: TGroupBox
      Left = 24
      Top = 216
      Width = 295
      Height = 81
      Caption = ' Subtitle PID '
      TabOrder = 3
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 17
        Height = 13
        Caption = 'PID'
      end
      object Label2: TLabel
        Left = 16
        Top = 50
        Width = 37
        Height = 13
        Caption = 'PCRPID'
      end
      object Label3: TLabel
        Left = 136
        Top = 24
        Width = 24
        Height = 13
        Caption = 'CPID'
      end
      object Label4: TLabel
        Left = 136
        Top = 50
        Width = 24
        Height = 13
        Caption = 'APID'
      end
      object SpinEdit4: TSpinEdit
        Left = 62
        Top = 20
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 0
        Value = -1
      end
      object SpinEdit5: TSpinEdit
        Left = 62
        Top = 46
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 1
        Value = -1
      end
      object Button4: TButton
        Left = 248
        Top = 20
        Width = 32
        Height = 47
        Caption = 'Set'
        TabOrder = 2
        OnClick = Button4Click
      end
      object SpinEdit6: TSpinEdit
        Left = 174
        Top = 20
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 3
        Value = -1
      end
      object SpinEdit7: TSpinEdit
        Left = 174
        Top = 46
        Width = 67
        Height = 22
        MaxValue = 8191
        MinValue = -1
        TabOrder = 4
        Value = -1
      end
    end
  end
end
