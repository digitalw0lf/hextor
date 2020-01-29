object SetFileSizeForm: TSetFileSizeForm
  Left = 0
  Top = 0
  ActiveControl = EditNewSize
  BorderStyle = bsDialog
  Caption = 'Resize file'
  ClientHeight = 203
  ClientWidth = 257
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
    Top = 19
    Width = 62
    Height = 13
    Caption = 'Current size:'
  end
  object Label2: TLabel
    Left = 16
    Top = 59
    Width = 46
    Height = 13
    Caption = 'New size:'
  end
  object LblFillValue: TLabel
    Left = 16
    Top = 99
    Width = 68
    Height = 13
    Caption = 'Fill with value:'
    Enabled = False
  end
  object ImageProxy1: TImageProxy
    Left = 231
    Top = 58
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 'Use 0x or $ for hex, + or - for relative change'
  end
  object EditOldSize: TEdit
    Left = 104
    Top = 16
    Width = 121
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = '0'
  end
  object EditNewSize: TEdit
    Left = 104
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = EditNewSizeChange
  end
  object EditFillValue: TComboBox
    Left = 104
    Top = 96
    Width = 121
    Height = 21
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = '0x00'
    Items.Strings = (
      '0x00'
      '0xFF')
  end
  object BtnOK: TButton
    Left = 40
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 142
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
