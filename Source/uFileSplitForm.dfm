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
    Width = 190
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '"%" replaced with consecutive numbers'
  end
  object Label5: TLabel
    Left = 16
    Top = 131
    Width = 39
    Height = 13
    Caption = 'Split by:'
  end
  object Label6: TLabel
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
  object ComboBox1: TComboBox
    Left = 88
    Top = 128
    Width = 201
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 3
    Text = '1 MiB'
    Items.Strings = (
      '1 MiB'
      '1,44 MiB (3.5")'
      '100 MiB'
      '650 MiB (CD)'
      '1 GiB')
  end
  object CheckBox1: TCheckBox
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
    TabOrder = 5
  end
  object BtnCancel: TButton
    Left = 298
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Cancel'
    TabOrder = 6
  end
  object OpenDialog1: TOpenDialog
    Left = 304
    Top = 8
  end
  object DropFileCatcher1: TDropFileCatcher
    Control = Owner
    OnDropFiles = DropFileCatcher1DropFiles
    Left = 440
    Top = 152
  end
end
