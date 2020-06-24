object FillBytesForm: TFillBytesForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Insert bytes / Fill selection'
  ClientHeight = 267
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
    Height = 267
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Insert bytes'
      'Fill selection')
    TabIndex = 0
    OnChange = TabControl1Change
    OnChanging = TabControl1Changing
    object LblCount: TLabel
      Left = 16
      Top = 168
      Width = 33
      Height = 13
      Caption = 'Count:'
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
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object RBRandomBytes: TRadioButton
      Left = 16
      Top = 92
      Width = 177
      Height = 17
      Caption = 'Random bytes from range:'
      TabOrder = 2
    end
    object EditRandomMin: TSpinEdit
      Left = 16
      Top = 115
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object EditRandomMax: TSpinEdit
      Left = 104
      Top = 115
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 4
      Value = 255
    end
    object BtnOK: TButton
      Left = 40
      Top = 220
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 5
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 158
      Top = 220
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object EditCount: TEdit
      Left = 72
      Top = 165
      Width = 121
      Height = 21
      TabOrder = 7
      Text = '1'
    end
  end
end
