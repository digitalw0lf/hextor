object FindReplaceForm: TFindReplaceForm
  Left = 900
  Top = 200
  Caption = 'Find/Replace'
  ClientHeight = 255
  ClientWidth = 540
  Color = clBtnFace
  Constraints.MinHeight = 294
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object GBFind: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 534
    Height = 121
    Align = alTop
    Caption = 'Find'
    TabOrder = 0
    DesignSize = (
      534
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
      Width = 430
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object CBFindHex: TCheckBox
      Left = 16
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Hex'
      TabOrder = 1
    end
    object CBWildcards: TCheckBox
      Left = 106
      Top = 48
      Width = 84
      Height = 17
      Caption = '? for any'
      Enabled = False
      TabOrder = 2
    end
    object CBMatchCase: TCheckBox
      Left = 286
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Match case'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 4
    end
    object BtnFindNext: TButton
      Tag = 1
      Left = 119
      Top = 80
      Width = 90
      Height = 25
      Hint = 'Alt+Right arrow'
      Caption = 'Find next >>'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = BtnFindNextClick
    end
    object BtnFindPrev: TButton
      Tag = -1
      Left = 16
      Top = 80
      Width = 90
      Height = 25
      Hint = 'Alt+Left arrow'
      Caption = '<< Find prev'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = BtnFindNextClick
    end
    object BtnFindCount: TButton
      Left = 247
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Count'
      TabOrder = 7
      OnClick = BtnFindCountClick
    end
    object CBUnicode: TCheckBox
      Left = 196
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Unicode'
      TabOrder = 3
    end
    object CBFindInSelection: TCheckBox
      Left = 447
      Top = 84
      Width = 84
      Height = 17
      Caption = 'In selection'
      TabOrder = 8
      OnClick = CBFindInSelectionClick
    end
    object BtnFindList: TButton
      Left = 350
      Top = 80
      Width = 90
      Height = 25
      Caption = 'List'
      TabOrder = 9
      OnClick = BtnFindCountClick
    end
  end
  object GBReplace: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 130
    Width = 534
    Height = 122
    Align = alClient
    Caption = 'Replace'
    TabOrder = 1
    DesignSize = (
      534
      122)
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
      Width = 430
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object CBReplaceHex: TCheckBox
      Left = 16
      Top = 48
      Width = 84
      Height = 17
      Caption = 'Hex'
      TabOrder = 1
    end
    object BtnReplaceAll: TButton
      Left = 16
      Top = 80
      Width = 90
      Height = 25
      Caption = 'Replace'
      TabOrder = 2
      OnClick = BtnReplaceAllClick
    end
    object CBReplaceInSelection: TCheckBox
      Left = 112
      Top = 84
      Width = 84
      Height = 17
      Caption = 'In selection'
      TabOrder = 3
      OnClick = CBReplaceInSelectionClick
    end
    object CBAskReplace: TCheckBox
      Left = 106
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Ask each replace'
      TabOrder = 4
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 488
    Top = 73
  end
end
