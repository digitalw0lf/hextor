object FindReplaceForm: TFindReplaceForm
  Left = 0
  Top = 0
  Caption = 'Find/Replace'
  ClientHeight = 240
  ClientWidth = 540
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 540
    Height = 121
    Align = alTop
    Caption = 'Find'
    TabOrder = 0
    ExplicitWidth = 634
    DesignSize = (
      540
      121)
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 24
      Height = 13
      Caption = 'Find:'
    end
    object EditFindText: TComboBox
      Left = 87
      Top = 16
      Width = 436
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 530
    end
    object CBFindHex: TCheckBox
      Left = 16
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Hex'
      TabOrder = 1
    end
    object CBFindRegEx: TCheckBox
      Left = 106
      Top = 48
      Width = 84
      Height = 17
      Caption = 'RegEx'
      Enabled = False
      TabOrder = 2
    end
    object CBMatchCase: TCheckBox
      Left = 286
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Match case'
      Enabled = False
      TabOrder = 3
    end
    object CBWholeWords: TCheckBox
      Left = 376
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Whole words'
      Enabled = False
      TabOrder = 4
    end
    object BtnFindNext: TButton
      Tag = 1
      Left = 119
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Find next >>'
      TabOrder = 5
      OnClick = BtnFindNextClick
    end
    object BtnFindPrev: TButton
      Tag = -1
      Left = 16
      Top = 80
      Width = 90
      Height = 25
      Caption = '<< Find prev'
      TabOrder = 6
      OnClick = BtnFindNextClick
    end
    object BtnFindCount: TButton
      Left = 247
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Count'
      TabOrder = 7
    end
    object CBUnicode: TCheckBox
      Left = 196
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Unicode'
      TabOrder = 8
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 121
    Width = 540
    Height = 119
    Align = alClient
    Caption = 'Replace'
    TabOrder = 1
    ExplicitTop = 153
    ExplicitWidth = 634
    ExplicitHeight = 153
    DesignSize = (
      540
      119)
    object Label2: TLabel
      Left = 16
      Top = 19
      Width = 65
      Height = 13
      Caption = 'Replace with:'
    end
    object EditReplaceText: TComboBox
      Left = 87
      Top = 16
      Width = 436
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 530
    end
    object CBReplaceHex: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Hex'
      TabOrder = 1
    end
    object BtnReplaceNext: TButton
      Left = 16
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Replace next'
      TabOrder = 2
    end
    object BtnReplaceAll: TButton
      Left = 119
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Replace all'
      TabOrder = 3
    end
  end
end
