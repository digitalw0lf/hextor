object ModifyWithExpressionForm: TModifyWithExpressionForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Modify data with expression'
  ClientHeight = 374
  ClientWidth = 321
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
  object Label1: TLabel
    Left = 16
    Top = 28
    Width = 142
    Height = 13
    Caption = 'Apply expression to selected:'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 56
    Height = 13
    Caption = 'Expression:'
  end
  object Label3: TLabel
    Left = 16
    Top = 133
    Width = 69
    Height = 13
    Caption = 'Pattern (hex):'
  end
  object LblSizeWarning: TLabel
    Left = 16
    Top = 304
    Width = 40
    Height = 13
    Caption = 'Warning'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object CBElementType: TComboBox
    Left = 216
    Top = 24
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'bytes'
    OnChange = CBElementTypeChange
    Items.Strings = (
      'bytes'
      'words'
      'dwords')
  end
  object EditExpression: TComboBox
    Left = 16
    Top = 88
    Width = 289
    Height = 23
    AutoComplete = False
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'x+1'
    OnChange = CBElementTypeChange
    Items.Strings = (
      '~x      // Invert bits'
      'x+1'
      'x<<1'
      'i       // Sequential values'
      'x^p     // XOR with pattern')
  end
  object EditPattern: TComboBox
    Left = 16
    Top = 152
    Width = 289
    Height = 23
    AutoComplete = False
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnChange = CBElementTypeChange
  end
  object BtnOk: TButton
    Left = 56
    Top = 328
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 192
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object Memo1: TMemo
    Left = 16
    Top = 200
    Width = 289
    Height = 97
    Lines.Strings = (
      'Available variables:'
      ''
      'x - original value'
      'p - pattern value'
      'i - index in selection'
      'a - address in file')
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
  end
end
