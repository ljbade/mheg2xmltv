object frmAddRecording: TfrmAddRecording
  Left = 192
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Add Recording'
  ClientHeight = 465
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 449
    Height = 449
    TabOrder = 0
    object Label5: TLabel
      Left = 16
      Top = 413
      Width = 39
      Height = 13
      Caption = 'Channel'
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 176
      Width = 201
      Height = 217
      Caption = ' Start recording '
      TabOrder = 0
      object Label2: TLabel
        Left = 83
        Top = 179
        Width = 6
        Height = 13
        Caption = 'h'
      end
      object Label3: TLabel
        Left = 155
        Top = 179
        Width = 8
        Height = 13
        Caption = 'm'
      end
      object MonthCalendar1: TMonthCalendar
        Left = 16
        Top = 24
        Width = 169
        Height = 136
        Date = 38466.801443530090000000
        ShowToday = False
        TabOrder = 0
      end
      object SpinEdit1: TSpinEdit
        Left = 40
        Top = 176
        Width = 41
        Height = 22
        MaxValue = 23
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object SpinEdit2: TSpinEdit
        Left = 112
        Top = 176
        Width = 41
        Height = 22
        MaxValue = 59
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 232
      Top = 176
      Width = 201
      Height = 217
      Caption = ' End Recording '
      TabOrder = 1
      object Label1: TLabel
        Left = 83
        Top = 179
        Width = 6
        Height = 13
        Caption = 'h'
      end
      object Label4: TLabel
        Left = 155
        Top = 179
        Width = 8
        Height = 13
        Caption = 'm'
      end
      object MonthCalendar2: TMonthCalendar
        Left = 16
        Top = 24
        Width = 169
        Height = 136
        Date = 38466.801443530090000000
        ShowToday = False
        TabOrder = 0
      end
      object SpinEdit3: TSpinEdit
        Left = 40
        Top = 176
        Width = 41
        Height = 22
        MaxValue = 23
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object SpinEdit4: TSpinEdit
        Left = 112
        Top = 176
        Width = 41
        Height = 22
        MaxValue = 59
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
    end
    object Button1: TButton
      Left = 263
      Top = 408
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = Button1Click
    end
    object ComboBox1: TComboBox
      Left = 64
      Top = 411
      Width = 177
      Height = 21
      Style = csOwnerDrawFixed
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 15
      ParentFont = False
      TabOrder = 3
      OnDrawItem = ComboBox1DrawItem
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 96
      Width = 417
      Height = 65
      Caption = ' Destination '
      TabOrder = 4
      object SpeedButton1: TSpeedButton
        Left = 344
        Top = 24
        Width = 57
        Height = 21
        Caption = 'Choose'
        OnClick = SpeedButton1Click
      end
      object Edit1: TEdit
        Left = 16
        Top = 24
        Width = 321
        Height = 21
        TabOrder = 0
      end
    end
    object GroupBox4: TGroupBox
      Left = 16
      Top = 16
      Width = 417
      Height = 65
      Caption = ' Name '
      TabOrder = 5
      object Edit2: TEdit
        Left = 16
        Top = 24
        Width = 385
        Height = 21
        TabOrder = 0
      end
    end
    object Button3: TButton
      Left = 358
      Top = 408
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 6
      OnClick = Button3Click
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'dvr-ms'
    Filter = 'Digital Video Recording (*.dvr-ms)|*.dvr-ms'
    Left = 24
    Top = 112
  end
end
