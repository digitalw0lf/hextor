object FileSplitForm: TFileSplitForm
  Left = 0
  Top = 0
  Caption = 'Split file'
  ClientHeight = 256
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    531
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 54
    Height = 13
    Caption = 'Source file:'
  end
  object SpeedButton1: TSpeedButton
    Left = 487
    Top = 16
    Width = 23
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 16
    Top = 59
    Width = 67
    Height = 13
    Caption = 'Target folder:'
  end
  object SpeedButton2: TSpeedButton
    Left = 487
    Top = 56
    Width = 23
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = SpeedButton2Click
  end
  object Label3: TLabel
    Left = 16
    Top = 91
    Width = 70
    Height = 13
    Caption = 'Target names:'
  end
  object Label4: TLabel
    Left = 312
    Top = 91
    Width = 194
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '"???" replaced with consecutive numbers'
  end
  object Label5: TLabel
    Left = 16
    Top = 131
    Width = 39
    Height = 13
    Caption = 'Split by:'
  end
  object LblFilesCount: TLabel
    Left = 312
    Top = 131
    Width = 53
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '0 files total'
  end
  object EditSourceFile: TEdit
    Left = 88
    Top = 16
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EditSourceFileChange
  end
  object EditTargetFolder: TComboBox
    Left = 88
    Top = 56
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object EditTargetName: TComboBox
    Left = 88
    Top = 88
    Width = 201
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object EditSplitBy: TComboBox
    Left = 88
    Top = 128
    Width = 201
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 3
    Text = '1 MB'
    OnChange = EditSplitByChange
    Items.Strings = (
      '1 MB'
      '1,44 MB (3.5")'
      '100 MB'
      '650 MB (CD)'
      '1 GB')
  end
  object CBCreateCRCFile: TCheckBox
    Left = 16
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Create CRC file'
    TabOrder = 4
  end
  object BtnOk: TButton
    Left = 168
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 290
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 8
  end
  object DropFileCatcher1: TDropFileCatcher
    Control = Owner
    OnDropFiles = DropFileCatcher1DropFiles
    Left = 440
    Top = 152
  end
  object SelectFolderDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 384
    Top = 44
  end
end
