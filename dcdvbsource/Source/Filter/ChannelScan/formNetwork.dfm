object frmNetwork: TfrmNetwork
  Left = 290
  Top = 80
  BorderStyle = bsSingle
  Caption = 'Network'
  ClientHeight = 401
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
    401)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 144
    Width = 409
    Height = 201
    Caption = ' Tuning '
    TabOrder = 4
    object Label16: TLabel
      Left = 16
      Top = 24
      Width = 177
      Height = 13
      Caption = 'ATSC Tuning not implemented yet ...'
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 144
    Width = 409
    Height = 201
    Caption = ' Tuning '
    TabOrder = 3
    object Label7: TLabel
      Left = 208
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Bandwidth'
    end
    object Label8: TLabel
      Left = 16
      Top = 24
      Width = 51
      Height = 13
      Hint = 'Service ID'
      Caption = 'Frequency'
    end
    object Label5: TLabel
      Left = 208
      Top = 24
      Width = 34
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Priority'
    end
    object Label2: TLabel
      Left = 16
      Top = 120
      Width = 54
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Time Slicing'
    end
    object Label4: TLabel
      Left = 16
      Top = 48
      Width = 42
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'MPE FEC'
    end
    object Label10: TLabel
      Left = 208
      Top = 72
      Width = 62
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Constellation'
    end
    object Label11: TLabel
      Left = 16
      Top = 168
      Width = 46
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Hierarchy'
    end
    object Label12: TLabel
      Left = 16
      Top = 72
      Width = 41
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'HP Code'
    end
    object Label13: TLabel
      Left = 16
      Top = 96
      Width = 29
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Guard'
    end
    object Label14: TLabel
      Left = 16
      Top = 144
      Width = 53
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Other Freq'
    end
    object Label17: TLabel
      Left = 208
      Top = 96
      Width = 39
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'LP Code'
    end
    object Label19: TLabel
      Left = 208
      Top = 120
      Width = 26
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Mode'
    end
    object SpinEdit3: TSpinEdit
      Left = 80
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
    object SpinEdit5: TSpinEdit
      Left = 272
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 1
      Value = -1
    end
    object SpinEdit9: TSpinEdit
      Left = 272
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 2
      Value = -1
    end
    object SpinEdit7: TSpinEdit
      Left = 80
      Top = 117
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 3
      Value = -1
    end
    object SpinEdit6: TSpinEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 4
      Value = -1
    end
    object SpinEdit13: TSpinEdit
      Left = 80
      Top = 141
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 5
      Value = -1
    end
    object ComboBox1: TComboBox
      Left = 272
      Top = 69
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 6
      Text = 'Reserved'
      Items.Strings = (
        'QPSK'
        '16QAM'
        '64QAM'
        'Reserved')
    end
    object ComboBox2: TComboBox
      Left = 80
      Top = 165
      Width = 313
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 7
      Text = 'Non Hierarchical Native Interleaver'
      Items.Strings = (
        'Non Hierarchical Native Interleaver'
        '1 Native Interleaver'
        '2 Native Interleaver'
        '4 Native Interleaver'
        'Non Hierarchical In Depth Interleaver'
        '1 In Depth Interleaver'
        '2 In Depth Interleaver'
        '4 In Depth Interleaver')
    end
    object ComboBox3: TComboBox
      Left = 80
      Top = 69
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 5
      TabOrder = 8
      Text = 'Reserved'
      Items.Strings = (
        '1/2'
        '2/3'
        '3/4'
        '5/6'
        '7/8'
        'Reserved')
    end
    object ComboBox4: TComboBox
      Left = 272
      Top = 93
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 5
      TabOrder = 9
      Text = 'Reserved'
      Items.Strings = (
        '1/2'
        '2/3'
        '3/4'
        '5/6'
        '7/8'
        'Reserved')
    end
    object ComboBox5: TComboBox
      Left = 80
      Top = 93
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 10
      Text = '1/8'
      Items.Strings = (
        '1/32'
        '1/16'
        '1/8'
        '1/4 ')
    end
    object ComboBox6: TComboBox
      Left = 272
      Top = 117
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 11
      Text = 'Reserved'
      Items.Strings = (
        '2k'
        '8k'
        '4k'
        'Reserved')
    end
  end
  object GroupBox4: TGroupBox
    Left = 16
    Top = 144
    Width = 409
    Height = 105
    Caption = ' Tuning '
    TabOrder = 5
    object Label22: TLabel
      Left = 208
      Top = 48
      Width = 50
      Height = 13
      Caption = 'FEC Outer'
    end
    object Label25: TLabel
      Left = 16
      Top = 24
      Width = 51
      Height = 13
      Hint = 'Service ID'
      Caption = 'Frequency'
    end
    object Label30: TLabel
      Left = 16
      Top = 48
      Width = 54
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Symbolrate'
    end
    object Label31: TLabel
      Left = 208
      Top = 24
      Width = 52
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Modulation'
    end
    object Label32: TLabel
      Left = 208
      Top = 72
      Width = 48
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'FEC Inner'
    end
    object SpinEdit10: TSpinEdit
      Left = 80
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
    object SpinEdit16: TSpinEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 1
      Value = -1
    end
    object ComboBox9: TComboBox
      Left = 272
      Top = 45
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'Unknown'
      Items.Strings = (
        'Unknown'
        'Linear Horizontal'
        'Linear Vertical'
        'Circular Left'
        'Circular Right')
    end
    object ComboBox11: TComboBox
      Left = 272
      Top = 21
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'Not Defined'
      Items.Strings = (
        'Not Defined'
        '16 QAM'
        '32 QAM'
        '64 QAM'
        '128 QAM'
        '256 QAM'
        'Reserved')
    end
    object ComboBox13: TComboBox
      Left = 272
      Top = 69
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'Not Defined'
      Items.Strings = (
        'Not Defined'
        'No Outer'
        'RS 204/188'
        'Reserved')
    end
  end
  object GroupBox5: TGroupBox
    Left = 16
    Top = 144
    Width = 409
    Height = 129
    Caption = ' Tuning '
    TabOrder = 6
    object Label18: TLabel
      Left = 208
      Top = 48
      Width = 55
      Height = 13
      Caption = 'Polarization'
    end
    object Label20: TLabel
      Left = 16
      Top = 24
      Width = 51
      Height = 13
      Hint = 'Service ID'
      Caption = 'Frequency'
    end
    object Label21: TLabel
      Left = 16
      Top = 72
      Width = 62
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Orb. Position'
    end
    object Label23: TLabel
      Left = 16
      Top = 48
      Width = 43
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'W/E Flag'
    end
    object Label24: TLabel
      Left = 16
      Top = 96
      Width = 54
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Symbolrate'
    end
    object Label26: TLabel
      Left = 208
      Top = 24
      Width = 52
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Modulation'
    end
    object Label27: TLabel
      Left = 208
      Top = 72
      Width = 48
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'FEC Inner'
    end
    object SpinEdit8: TSpinEdit
      Left = 80
      Top = 21
      Width = 121
      Height = 22
      MaxValue = 999999999
      MinValue = -1
      TabOrder = 0
      Value = -1
    end
    object SpinEdit11: TSpinEdit
      Left = 80
      Top = 69
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 1
      Value = -1
    end
    object SpinEdit12: TSpinEdit
      Left = 80
      Top = 93
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 2
      Value = -1
    end
    object SpinEdit14: TSpinEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 3
      Value = -1
    end
    object ComboBox8: TComboBox
      Left = 272
      Top = 45
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'Unknown'
      Items.Strings = (
        'Unknown'
        'Linear Horizontal'
        'Linear Vertical'
        'Circular Left'
        'Circular Right')
    end
    object ComboBox10: TComboBox
      Left = 272
      Top = 21
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = 'Not Defined'
      Items.Strings = (
        'Not Defined'
        'QPSK'
        '8PSK'
        '16QAM'
        'Reserved')
    end
    object ComboBox12: TComboBox
      Left = 272
      Top = 69
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = 'Not Defined'
      Items.Strings = (
        'Not Defined'
        '1/2'
        '2/3'
        '3/4'
        '5/6'
        '7/8'
        '8/9'
        '3/5'
        '4/5'
        '9/10'
        'No Conv'
        'Reserved')
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 16
    Width = 409
    Height = 121
    Caption = ' Description '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Label6: TLabel
      Left = 16
      Top = 56
      Width = 26
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'ONID'
    end
    object Label3: TLabel
      Left = 208
      Top = 56
      Width = 23
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'TSID'
    end
    object Label9: TLabel
      Left = 208
      Top = 88
      Width = 54
      Height = 13
      Hint = 'Service ID'
      Caption = 'Network ID'
    end
    object Label15: TLabel
      Left = 16
      Top = 88
      Width = 24
      Height = 13
      Hint = 'Program Map PID'
      Caption = 'Type'
    end
    object Edit1: TEdit
      Left = 72
      Top = 21
      Width = 321
      Height = 21
      TabOrder = 0
    end
    object SpinEdit4: TSpinEdit
      Left = 72
      Top = 53
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 1
      Value = -1
    end
    object SpinEdit2: TSpinEdit
      Left = 272
      Top = 53
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 2
      Value = -1
    end
    object SpinEdit1: TSpinEdit
      Left = 272
      Top = 85
      Width = 121
      Height = 22
      MaxValue = 999999
      MinValue = -1
      TabOrder = 3
      Value = -1
    end
    object ComboBox7: TComboBox
      Left = 72
      Top = 85
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'Unknown'
      OnChange = ComboBox7Change
      Items.Strings = (
        'Unknown'
        'DVB-T'
        'DVB-S'
        'DVB-C'
        'ATSC')
    end
  end
  object Button1: TButton
    Left = 352
    Top = 360
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 264
    Top = 360
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
