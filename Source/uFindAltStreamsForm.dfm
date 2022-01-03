object FindAltStreamsForm: TFindAltStreamsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Alternate data streams'
  ClientHeight = 187
  ClientWidth = 521
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
    Left = 8
    Top = 16
    Width = 285
    Height = 13
    Caption = 'Find files with alternate NTFS streams in following folder(s):'
  end
  object BtnSelectFolder: TSpeedButton
    Left = 490
    Top = 34
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = BtnSelectFolderClick
  end
  object Label2: TLabel
    Left = 8
    Top = 74
    Width = 132
    Height = 13
    Caption = 'Only streams with name(s):'
  end
  object ImageProxy1: THintedImageProxy
    Left = 495
    Top = 96
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 
      'Semicolon-separated list of stream name masks.<br>You can use "*' +
      '" and "?" wildcards.<br>You can specify a list of excluded masks' +
      ' after a vertical line "|".'
  end
  object EditFolders: TComboBox
    Left = 8
    Top = 35
    Width = 481
    Height = 21
    TabOrder = 0
    Items.Strings = (
      'C:\')
  end
  object EditStreamNames: TComboBox
    Left = 8
    Top = 93
    Width = 481
    Height = 21
    Hint = 
      'Semicolon-separated list of stream name masks. You can use "*" a' +
      'nd "?" wildcards. You can specify a list of excluded masks after' +
      ' a vertical line "|".'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    Items.Strings = (
      'Zone.Identifier'
      '|Zone.Identifier')
  end
  object BtnCancel: TButton
    Left = 280
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object BtnFind: TButton
    Left = 160
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Find'
    Default = True
    TabOrder = 1
    OnClick = BtnFindClick
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoAllowMultiSelect]
    Left = 400
    Top = 8
  end
end
