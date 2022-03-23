object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 229
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 27
    Width = 92
    Height = 13
    Caption = 'Check for updates:'
  end
  object LblOpenSettingsFolder: TLabel
    Left = 16
    Top = 120
    Width = 98
    Height = 13
    Cursor = crHandPoint
    Caption = 'Open settings folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LblOpenSettingsFolderClick
  end
  object Label2: TLabel
    Left = 16
    Top = 67
    Width = 109
    Height = 13
    Caption = 'Additional code pages:'
  end
  object ImageProxy1: THintedImageProxy
    Left = 287
    Top = 67
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 'CodePage numbers, separated by space'
  end
  object CBUpdateCheckInterval: TComboBox
    Left = 136
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'Once a week'
      'Once a month'
      'Never')
  end
  object BtnOK: TButton
    Left = 168
    Top = 177
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 265
    Top = 177
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object CBCodePages: TComboBox
    Left = 136
    Top = 64
    Width = 145
    Height = 21
    TabOrder = 3
  end
end
