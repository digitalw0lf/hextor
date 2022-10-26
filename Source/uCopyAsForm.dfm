object CopyAsForm: TCopyAsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Copy data as'
  ClientHeight = 324
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 27
    Width = 47
    Height = 13
    Caption = 'Elements:'
  end
  object Label2: TLabel
    Left = 16
    Top = 107
    Width = 37
    Height = 13
    Caption = 'Layout:'
  end
  object Label3: TLabel
    Left = 16
    Top = 67
    Width = 45
    Height = 13
    Caption = 'Notation:'
  end
  object Label4: TLabel
    Left = 16
    Top = 147
    Width = 73
    Height = 13
    Caption = 'Values per line:'
  end
  object BtnEditLayouts: TSpeedButton
    Left = 295
    Top = 102
    Width = 25
    Height = 25
    Hint = 'Edit layouts (re-open this window after changing files)'
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000F7F6F5F7F6F5
      F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6
      F5F7F6F5F7F6F5F7F6F5F7F6F5474645818283F7F6F5F7F6F5F7F6F5F7F6F5F7
      F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5919292
      6F95BA5E7B9886898CF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6
      F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F57594B38EC8FF6997C86F8BA1F7F6F5F7
      F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5
      B6B9BB7EB0DD9BD1FF83BAF25C83AAF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6
      F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F57BA4C074B1DA8EE0FE67D5F534
      6A8CF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5
      F7F6F5F7F6F56F93B860B0D78CEBFF6ED8F9346A8CF7F6F5F7F6F5F7F6F5F7F6
      F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5457AA16AB7DE91
      ECFF79DCF5346A8CF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5
      F7F6F5F7F6F5F7F6F5F7F6F54A89B16FBDE3A8F5FF86E0F6346A8CF7F6F5F7F6
      F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F54E
      8FB582CEEEAFF8FF89E2F5346A8CF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5
      F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5519BC487D5F3C1FFFFA4B2B93C68
      35F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7
      F6F5F7F6F560A9D1A4B2B9447431829682AEAFAFF7F6F5F7F6F5F7F6F5F7F6F5
      F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F56785688FA58BEAE9
      EA9999D56C6CB5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7
      F6F5F7F6F5F7F6F5F7F6F5C4C7C7898AD96D6EDD4D4DADB5B4D7F7F6F5F7F6F5
      F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F57978
      BF6262B9A3A2CEF7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7
      F6F5F7F6F5F7F6F5F7F6F5F7F6F5F7F6F5CAC9E3F7F6F5F7F6F5}
    ParentShowHint = False
    PopupMenu = OpenLayoutFolderMenu
    ShowHint = True
    OnClick = BtnEditLayoutsClick
  end
  object CBElemType: TComboBox
    Left = 112
    Top = 24
    Width = 177
    Height = 21
    Style = csDropDownList
    DropDownCount = 20
    ItemIndex = 1
    TabOrder = 0
    Text = 'uint8'
    OnChange = CBElemTypeChange
    Items.Strings = (
      'int8'
      'uint8'
      'int16'
      'uint16'
      'int32'
      'uint32'
      'int64'
      'uint64'
      'float'
      'double'
      'ansi'
      'unicode')
  end
  object CBLayout: TComboBox
    Left = 112
    Top = 104
    Width = 177
    Height = 21
    Style = csDropDownList
    DropDownCount = 20
    TabOrder = 1
    OnChange = CBElemTypeChange
  end
  object BtnCopy: TButton
    Left = 72
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Copy'
    Default = True
    ImageIndex = 10
    ImageName = 'Copy'
    ImageMargins.Left = 8
    Images = MainForm.VirtualImageList1
    TabOrder = 2
    OnClick = BtnCopyClick
  end
  object BtnCancel: TButton
    Left = 184
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object CBNotation: TComboBox
    Left = 112
    Top = 64
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemIndex = 3
    TabOrder = 4
    Text = 'hex'
    OnChange = CBElemTypeChange
    Items.Strings = (
      'bin'
      'oct'
      'dec'
      'hex'
      'ansi char'
      'wide char')
  end
  object CBValuesPerLine: TComboBox
    Left = 112
    Top = 144
    Width = 177
    Height = 21
    AutoComplete = False
    TabOrder = 5
    Text = '16'
    OnChange = CBElemTypeChange
    Items.Strings = (
      '1'
      '2'
      '4'
      '8'
      '16'
      '32'
      'All in one line')
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 184
    Width = 297
    Height = 81
    Caption = 'Preview'
    Padding.Left = 10
    Padding.Top = 5
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 6
    object LblPreview: TLabel
      Left = 12
      Top = 20
      Width = 273
      Height = 49
      Align = alClient
      AutoSize = False
      Caption = 'Preview'
      ExplicitLeft = 24
      ExplicitTop = 24
      ExplicitWidth = 31
      ExplicitHeight = 13
    end
  end
  object OpenLayoutFolderMenu: TPopupMenu
    Left = 304
    Top = 128
    object MIUserLayoutsFolder: TMenuItem
      Caption = 'Open User layouts folder'
      OnClick = MIUserLayoutsFolderClick
    end
    object MIBuiltInLayoutsFolder: TMenuItem
      Caption = 'Open Built-In layouts folder'
      OnClick = MIUserLayoutsFolderClick
    end
  end
end
