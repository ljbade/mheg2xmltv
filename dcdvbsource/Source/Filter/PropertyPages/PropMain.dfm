object FormPropSettings: TFormPropSettings
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Settings'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 497
    Height = 353
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 465
      Height = 81
      Caption = ' Time Shift '
      TabOrder = 0
      object Label2: TLabel
        Left = 16
        Top = 24
        Width = 37
        Height = 13
        Caption = 'Position'
      end
      object Label4: TLabel
        Left = 64
        Top = 24
        Width = 44
        Height = 13
        Caption = '00:00:00'
      end
      object Label5: TLabel
        Left = 176
        Top = 24
        Width = 66
        Height = 13
        Caption = 'Content Start'
      end
      object Label6: TLabel
        Left = 328
        Top = 24
        Width = 64
        Height = 13
        Caption = 'Content Stop'
      end
      object Label7: TLabel
        Left = 248
        Top = 24
        Width = 44
        Height = 13
        Caption = '00:00:00'
      end
      object Label8: TLabel
        Left = 400
        Top = 24
        Width = 44
        Height = 13
        Caption = '00:00:00'
      end
      object Panel1: TPanel
        Left = 16
        Top = 48
        Width = 433
        Height = 17
        BevelOuter = bvLowered
        BiDiMode = bdLeftToRight
        Color = clBackground
        Ctl3D = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentBiDiMode = False
        ParentBackground = False
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnMouseDown = Panel1MouseDown
        OnMouseMove = Panel1MouseMove
        OnMouseUp = Panel1MouseUp
        object Panel2: TPanel
          Left = 2
          Top = 2
          Width = 127
          Height = 13
          BevelOuter = bvNone
          BiDiMode = bdLeftToRight
          Color = clSkyBlue
          Ctl3D = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentBiDiMode = False
          ParentBackground = False
          ParentCtl3D = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          OnMouseDown = Panel2MouseDown
          OnMouseMove = Panel2MouseMove
          OnMouseUp = Panel2MouseUp
        end
      end
    end
    object GroupBox5: TGroupBox
      Left = 255
      Top = 104
      Width = 226
      Height = 89
      Caption = ' Playrate '
      TabOrder = 1
      object Label1: TLabel
        Left = 174
        Top = 64
        Width = 34
        Height = 13
        Alignment = taRightJustify
        Caption = '1.000x'
      end
      object SpeedButton2: TSpeedButton
        Left = 16
        Top = 64
        Width = 47
        Height = 14
        Caption = 'Reset'
        OnClick = SpeedButton2Click
      end
      object TrackBar1: TTrackBar
        Left = 8
        Top = 24
        Width = 209
        Height = 33
        Max = 4000
        Min = -4000
        Frequency = 200
        Position = 1000
        TabOrder = 0
        ThumbLength = 27
        OnChange = TrackBar1Change
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 104
      Width = 226
      Height = 89
      Caption = ' Channels '
      TabOrder = 2
      object SpeedButton3: TSpeedButton
        Left = 120
        Top = 53
        Width = 89
        Height = 22
        Caption = 'next Channel'
        Layout = blGlyphBottom
        OnClick = SpeedButton3Click
      end
      object SpeedButton4: TSpeedButton
        Left = 16
        Top = 53
        Width = 97
        Height = 22
        Caption = 'previous Channel'
        Layout = blGlyphBottom
        OnClick = SpeedButton4Click
      end
      object ComboBox1: TComboBox
        Left = 16
        Top = 24
        Width = 193
        Height = 21
        Style = csOwnerDrawFixed
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 0
        OnChange = ComboBox1Change
        OnDrawItem = ComboBox1DrawItem
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 200
      Width = 226
      Height = 65
      Caption = ' Audio Streams '
      TabOrder = 3
      object SpeedButton5: TSpeedButton
        Left = 186
        Top = 24
        Width = 23
        Height = 22
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080000080000080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800000800000800000
          8000008000008000808000808000808000808000808000808000808000808000
          8080008080008080008080000080000080008080008080000080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000008000808000808000808000808000808000808000808000
          8080008080000080008080008080008080008080008080000080008080008080
          0080800080800080800080800080800080800080800000800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080000080008080008080000080000080008080008080008080008080
          0080800080800080800080800080800080800080800080800000800000800000
          8000008000008000808000808000808000808000808000808000808000808000
          8080008080008080008080008080000080000080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000}
        Layout = blGlyphTop
        OnClick = SpeedButton5Click
      end
      object ComboBox2: TComboBox
        Left = 16
        Top = 25
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBox2Change
      end
    end
    object GroupBox6: TGroupBox
      Left = 16
      Top = 272
      Width = 465
      Height = 65
      Caption = ' Teletext '
      TabOrder = 4
      object Label10: TLabel
        Left = 195
        Top = 28
        Width = 24
        Height = 13
        Caption = 'Page'
      end
      object Label11: TLabel
        Left = 339
        Top = 28
        Width = 42
        Height = 13
        Caption = 'Subpage'
        Visible = False
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 27
        Width = 65
        Height = 17
        Caption = 'Show'
        TabOrder = 0
        OnClick = CheckBox1Click
      end
      object SpinEdit1: TSpinEdit
        Left = 227
        Top = 24
        Width = 73
        Height = 22
        MaxValue = 899
        MinValue = 100
        TabOrder = 1
        Value = 100
        OnChange = SpinEdit1Change
      end
      object SpinEdit2: TSpinEdit
        Left = 374
        Top = 24
        Width = 73
        Height = 22
        MaxValue = 50
        MinValue = 0
        TabOrder = 2
        Value = 0
        Visible = False
        OnChange = SpinEdit1Change
      end
      object CheckBox2: TCheckBox
        Left = 80
        Top = 27
        Width = 89
        Height = 17
        Caption = 'Transparent'
        TabOrder = 3
        OnClick = CheckBox2Click
      end
      object Button1: TButton
        Left = 320
        Top = 24
        Width = 129
        Height = 21
        Caption = 'Save current Page'
        TabOrder = 4
        OnClick = Button1Click
      end
    end
    object GroupBox7: TGroupBox
      Left = 255
      Top = 200
      Width = 226
      Height = 65
      Caption = ' Subtitle Streams '
      TabOrder = 5
      object SpeedButton6: TSpeedButton
        Left = 186
        Top = 24
        Width = 23
        Height = 22
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080000080000080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800000800000800000
          8000008000008000808000808000808000808000808000808000808000808000
          8080008080008080008080000080000080008080008080000080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000008000808000808000808000808000808000808000808000
          8080008080000080008080008080008080008080008080000080008080008080
          0080800080800080800080800080800080800080800000800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080000080008080008080000080000080008080008080008080008080
          0080800080800080800080800080800080800080800080800000800000800000
          8000008000008000808000808000808000808000808000808000808000808000
          8080008080008080008080008080000080000080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800000
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000}
        Layout = blGlyphTop
        OnClick = SpeedButton6Click
      end
      object ComboBox3: TComboBox
        Left = 16
        Top = 25
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBox3Change
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Text (*.txt)|*.txt|VTX (*.vtx)|*.vtx'
    FilterIndex = 0
    Left = 136
    Top = 32
  end
end
