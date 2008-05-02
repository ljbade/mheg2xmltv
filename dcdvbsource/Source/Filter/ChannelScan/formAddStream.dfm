object frmAddStream: TfrmAddStream
  Left = 650
  Top = 196
  BorderStyle = bsSingle
  Caption = 'Add Stream'
  ClientHeight = 313
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
  DesignSize = (
    441
    313)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox5: TGroupBox
    Left = 16
    Top = 192
    Width = 409
    Height = 129
    Caption = ' Subtitle Streams (doubleclick on a Stream to make it default) '
    TabOrder = 6
    Visible = False
    object ListView1: TListView
      Left = 16
      Top = 24
      Width = 377
      Height = 89
      Columns = <
        item
          Caption = 'Language'
          Width = 110
        end
        item
          Caption = 'Type'
        end
        item
          Caption = 'Comp. ID'
          Width = 70
        end
        item
          Caption = 'Anc. ID'
          Width = 70
        end
        item
          Caption = 'Default'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = ListView1DblClick
    end
  end
  object GroupBox7: TGroupBox
    Left = 16
    Top = 192
    Width = 409
    Height = 61
    Caption = ' Video Settings '
    TabOrder = 7
    Visible = False
    object Label13: TLabel
      Left = 16
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object ComboBox11: TComboBox
      Left = 56
      Top = 21
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Unknown'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Unknown'
        'MPEG-2'
        'H264')
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 192
    Width = 409
    Height = 57
    Caption = ' Teletext Settings '
    TabOrder = 4
    Visible = False
    object Label4: TLabel
      Left = 16
      Top = 24
      Width = 47
      Height = 13
      Caption = 'Language'
    end
    object ComboBox2: TComboBox
      Left = 80
      Top = 21
      Width = 313
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 352
    Top = 272
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 264
    Top = 272
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 16
    Width = 409
    Height = 57
    Caption = ' Description '
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Edit1: TEdit
      Left = 72
      Top = 21
      Width = 321
      Height = 21
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 88
    Width = 409
    Height = 89
    Caption = ' Settings '
    TabOrder = 3
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 17
      Height = 13
      Caption = 'PID'
    end
    object Label3: TLabel
      Left = 208
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object Label8: TLabel
      Left = 16
      Top = 56
      Width = 18
      Height = 13
      Caption = 'Tag'
    end
    object SpinEdit1: TSpinEdit
      Left = 72
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
    object CheckBox1: TCheckBox
      Left = 336
      Top = 63
      Width = 65
      Height = 17
      Caption = 'Default'
      TabOrder = 2
    end
    object ComboBox1: TComboBox
      Left = 248
      Top = 21
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBox1Change
      Items.Strings = (
        'Unknown'
        'Video'
        'Audio'
        'Teletext'
        'Carousel (AIT)'
        'Carousel (Data)'
        'DSMCC'
        'Subtitle')
    end
    object SpinEdit2: TSpinEdit
      Left = 72
      Top = 53
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 3
      Value = -1
    end
  end
  object GroupBox4: TGroupBox
    Left = 16
    Top = 192
    Width = 409
    Height = 81
    Caption = ' Audio Settings '
    TabOrder = 5
    Visible = False
    object Label5: TLabel
      Left = 16
      Top = 24
      Width = 47
      Height = 13
      Caption = 'Language'
    end
    object Label6: TLabel
      Left = 40
      Top = 48
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object Label7: TLabel
      Left = 224
      Top = 48
      Width = 33
      Height = 13
      Caption = 'Coding'
    end
    object ComboBox3: TComboBox
      Left = 80
      Top = 21
      Width = 313
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object ComboBox4: TComboBox
      Left = 264
      Top = 45
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Unknown'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Unknown'
        'MP1'
        'MP2'
        'AC3')
    end
    object ComboBox5: TComboBox
      Left = 80
      Top = 45
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'Undefined'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Undefined'
        'Clean Effects'
        'Hearing Impaired'
        'Visual Impaired Commentary'
        'Reserved')
    end
    object GroupBox6: TGroupBox
      Left = 0
      Top = 0
      Width = 409
      Height = 81
      Caption = ' Audio Settings '
      TabOrder = 3
      Visible = False
      object Label9: TLabel
        Left = 16
        Top = 24
        Width = 47
        Height = 13
        Caption = 'Language'
      end
      object Label10: TLabel
        Left = 40
        Top = 48
        Width = 24
        Height = 13
        Caption = 'Type'
      end
      object Label11: TLabel
        Left = 224
        Top = 48
        Width = 33
        Height = 13
        Caption = 'Coding'
      end
      object ComboBox6: TComboBox
        Left = 80
        Top = 21
        Width = 313
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object ComboBox7: TComboBox
        Left = 264
        Top = 45
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ComboBox1Change
        Items.Strings = (
          'Unknown'
          'MP1'
          'MP2'
          'AC3'
          'AAC')
      end
      object ComboBox8: TComboBox
        Left = 80
        Top = 45
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 2
        Text = 'Undefined'
        OnChange = ComboBox1Change
        Items.Strings = (
          'Undefined'
          'Clean Effects'
          'Hearing Impaired'
          'Visual Impaired Commentary'
          'Reserved')
      end
    end
  end
end
