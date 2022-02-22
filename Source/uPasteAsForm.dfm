object PasteAsForm: TPasteAsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Paste as...'
  ClientHeight = 314
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
    Top = 130
    Width = 47
    Height = 13
    Caption = 'Elements:'
  end
  object Label5: TLabel
    Left = 161
    Top = 34
    Width = 53
    Height = 13
    Caption = 'CodePage:'
  end
  object ImageProxy1: THintedImageProxy
    Left = 345
    Top = 33
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 
      'Unicode text from clipboard is converted to bytes using selected' +
      ' CodePage'
  end
  object Label6: TLabel
    Left = 18
    Top = 8
    Width = 72
    Height = 13
    Caption = 'Source format:'
  end
  object RBValueArray: TRadioButton
    Left = 18
    Top = 128
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
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Paste'
    Default = True
    TabOrder = 6
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 212
    Top = 272
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object RBText: TRadioButton
    Left = 18
    Top = 32
    Width = 141
    Height = 17
    Caption = 'Text'
    TabOrder = 0
    OnClick = RBTextClick
  end
  object RBHex: TRadioButton
    Left = 18
    Top = 56
    Width = 141
    Height = 17
    Caption = 'Hex'
    TabOrder = 1
    OnClick = RBTextClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 168
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
    Left = 224
    Top = 126
    Width = 113
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = RBTextClick
  end
  object RBBase64: TRadioButton
    Left = 18
    Top = 80
    Width = 141
    Height = 17
    Caption = 'Base64'
    TabOrder = 2
    OnClick = RBTextClick
  end
  object RBURLEncode: TRadioButton
    Left = 18
    Top = 104
    Width = 141
    Height = 17
    Caption = 'URLEncode'
    TabOrder = 8
    OnClick = RBTextClick
  end
  object CBCodePage: TComboBox
    Left = 224
    Top = 30
    Width = 113
    Height = 21
    Style = csDropDownList
    TabOrder = 9
    OnChange = RBTextClick
  end
end
