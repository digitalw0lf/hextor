object PasteAsForm: TPasteAsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Paste as...'
  ClientHeight = 305
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 161
    Top = 90
    Width = 47
    Height = 13
    Caption = 'Elements:'
  end
  object RBValueArray: TRadioButton
    Left = 18
    Top = 88
    Width = 141
    Height = 17
    Caption = 'Array (delimited text)'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RBTextClick
  end
  object BtnOk: TButton
    Left = 108
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Paste'
    Default = True
    TabOrder = 6
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 212
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object RBText: TRadioButton
    Left = 18
    Top = 16
    Width = 141
    Height = 17
    Caption = 'Text'
    TabOrder = 0
    OnClick = RBTextClick
  end
  object RBHex: TRadioButton
    Left = 18
    Top = 40
    Width = 141
    Height = 17
    Caption = 'Hex'
    TabOrder = 1
    OnClick = RBTextClick
  end
  object Panel1: TPanel
    Left = 32
    Top = 111
    Width = 329
    Height = 41
    BevelOuter = bvNone
    TabOrder = 8
    Visible = False
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 47
      Height = 13
      Caption = 'Elements:'
    end
    object RBElemByte: TRadioButton
      Left = 0
      Top = 16
      Width = 63
      Height = 17
      Caption = 'Byte'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBTextClick
    end
    object RBElemWord: TRadioButton
      Left = 64
      Top = 16
      Width = 63
      Height = 17
      Caption = 'Word'
      TabOrder = 1
      OnClick = RBTextClick
    end
    object RBElemDWord: TRadioButton
      Left = 123
      Top = 16
      Width = 63
      Height = 17
      Caption = 'DWord'
      TabOrder = 2
      OnClick = RBTextClick
    end
    object RBElemFloat: TRadioButton
      Left = 192
      Top = 16
      Width = 63
      Height = 17
      Caption = 'Float'
      TabOrder = 3
      OnClick = RBTextClick
    end
    object RBElemDouble: TRadioButton
      Left = 256
      Top = 16
      Width = 63
      Height = 17
      Caption = 'Double'
      TabOrder = 4
      OnClick = RBTextClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 136
    Width = 369
    Height = 81
    Caption = 'Preview'
    TabOrder = 5
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 30
      Height = 13
      Caption = 'Input:'
    end
    object Label3: TLabel
      Left = 16
      Top = 47
      Width = 38
      Height = 13
      Caption = 'Output:'
    end
    object LblInputPreview: TLabel
      Left = 80
      Top = 24
      Width = 15
      Height = 13
      Caption = '???'
    end
    object LblOutputPreview: TLabel
      Left = 80
      Top = 47
      Width = 15
      Height = 13
      Caption = '???'
    end
  end
  object CBElemType: TComboBox
    Left = 214
    Top = 86
    Width = 89
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = RBTextClick
  end
  object RBBase64: TRadioButton
    Left = 18
    Top = 64
    Width = 141
    Height = 17
    Caption = 'Base64'
    TabOrder = 2
    OnClick = RBTextClick
  end
end
