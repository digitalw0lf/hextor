object FillBytesForm: TFillBytesForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Insert bytes / Fill selection'
  ClientHeight = 313
  ClientWidth = 278
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
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 278
    Height = 313
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Insert bytes'
      'Fill/Alter selection')
    TabIndex = 0
    OnChange = TabControl1Change
    OnChanging = TabControl1Changing
    object LblCount: TLabel
      Left = 16
      Top = 224
      Width = 33
      Height = 13
      Caption = 'Count:'
    end
    object ImageProxy1: THintedImageProxy
      Left = 254
      Top = 118
      Width = 16
      Height = 16
      Image = MainForm.HintImage
      ImageIndex = 0
      HintFmt = 
        'Available variables:<br>x - original data byte<br>p - pattern by' +
        'te<br>i - index in selection<br>a - adress in file'
    end
    object EditPattern: TComboBox
      Left = 16
      Top = 59
      Width = 232
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = '00'
      Items.Strings = (
        '00'
        'FF'
        '00 01 02 03')
    end
    object RBPattern: TRadioButton
      Left = 16
      Top = 36
      Width = 177
      Height = 17
      Caption = 'Hex Pattern:'
      TabOrder = 1
    end
    object RBRandomBytes: TRadioButton
      Left = 16
      Top = 148
      Width = 177
      Height = 17
      Caption = 'Random bytes from range:'
      TabOrder = 2
    end
    object EditRandomMin: TSpinEdit
      Left = 16
      Top = 171
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object EditRandomMax: TSpinEdit
      Left = 104
      Top = 171
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 4
      Value = 255
    end
    object BtnOK: TButton
      Left = 40
      Top = 276
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object BtnCancel: TButton
      Left = 158
      Top = 276
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object EditCount: TEdit
      Left = 72
      Top = 221
      Width = 121
      Height = 21
      TabOrder = 7
      Text = '1'
    end
    object RBExpression: TRadioButton
      Left = 16
      Top = 92
      Width = 177
      Height = 17
      Caption = 'Expression:'
      Checked = True
      TabOrder = 8
      TabStop = True
    end
    object EditExpression: TComboBox
      Left = 16
      Top = 115
      Width = 232
      Height = 21
      ItemIndex = 0
      TabOrder = 9
      Text = 'x'
      Items.Strings = (
        'x'
        'x xor p'
        'i')
    end
  end
end
