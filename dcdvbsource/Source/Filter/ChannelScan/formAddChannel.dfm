object frmAddChannel: TfrmAddChannel
  Left = 410
  Top = 167
  BorderStyle = bsSingle
  Caption = 'Add Channel'
  ClientHeight = 457
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 409
    Height = 81
    Caption = ' Description '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Label2: TLabel
      Left = 16
      Top = 48
      Width = 40
      Height = 13
      Caption = 'Provider'
    end
    object Edit1: TEdit
      Left = 72
      Top = 21
      Width = 321
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 72
      Top = 48
      Width = 321
      Height = 21
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 112
    Width = 409
    Height = 81
    Caption = ' Tuning '
    TabOrder = 1
    object Label6: TLabel
      Left = 16
      Top = 48
      Width = 41
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'PMT-PID'
    end
    object Label7: TLabel
      Left = 208
      Top = 24
      Width = 41
      Height = 13
      Caption = 'PCR-PID'
    end
    object Label8: TLabel
      Left = 16
      Top = 24
      Width = 17
      Height = 13
      Hint = 'Service ID'
      Caption = 'SID'
    end
    object Label3: TLabel
      Left = 208
      Top = 48
      Width = 57
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Servicetype'
    end
    object SpinEdit3: TSpinEdit
      Left = 80
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
    object SpinEdit4: TSpinEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 2
      Value = -1
    end
    object SpinEdit5: TSpinEdit
      Left = 272
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 1
      Value = -1
    end
    object SpinEdit1: TSpinEdit
      Left = 272
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 3
      Value = -1
    end
  end
  object Button1: TButton
    Left = 352
    Top = 416
    Width = 73
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 264
    Top = 416
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 208
    Width = 409
    Height = 193
    Caption = ' Streams '
    TabOrder = 2
    object ListView1: TListView
      Left = 16
      Top = 24
      Width = 377
      Height = 121
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'PID'
        end
        item
          Caption = 'Default'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = ListView1Change
      OnDblClick = ListView1DblClick
    end
    object Button4: TButton
      Left = 16
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 72
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Edit'
      Enabled = False
      TabOrder = 2
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 319
      Top = 152
      Width = 73
      Height = 25
      Caption = 'Move Up'
      Enabled = False
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 240
      Top = 152
      Width = 72
      Height = 25
      Caption = 'Move Down'
      Enabled = False
      TabOrder = 4
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 128
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Delete'
      Enabled = False
      TabOrder = 3
      OnClick = Button8Click
    end
    object Button3: TButton
      Left = 184
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Clear'
      Enabled = False
      TabOrder = 6
      OnClick = Button3Click
    end
  end
  object Button9: TButton
    Left = 16
    Top = 416
    Width = 75
    Height = 25
    Caption = 'Network'
    TabOrder = 5
    OnClick = Button9Click
  end
end
