object FindReplaceForm: TFindReplaceForm
  Left = 900
  Top = 200
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'Find/Replace'
  ClientHeight = 554
  ClientWidth = 542
  Color = clBtnFace
  Constraints.MinHeight = 294
  Constraints.MinWidth = 500
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CategoryPanelGroup1: TCategoryPanelGroup
    Left = 0
    Top = 0
    Width = 542
    Height = 554
    HorzScrollBar.Visible = False
    VertScrollBar.Tracking = True
    VertScrollBar.Visible = False
    Align = alClient
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = []
    HeaderStyle = hsThemed
    TabOrder = 0
    object CPFindInFiles: TCategoryPanel
      Top = 289
      Height = 224
      Caption = 'Find in Files'
      TabOrder = 0
      OnCollapse = CPFindExpand
      OnExpand = CPFindExpand
      object Label3: TLabel
        Left = 16
        Top = 19
        Width = 48
        Height = 13
        Caption = 'Search in:'
      end
      object BtnSelectDirectory: TSpeedButton
        Left = 496
        Top = 81
        Width = 23
        Height = 23
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = BtnSelectDirectoryClick
      end
      object LblFileMasks: TLabel
        Left = 16
        Top = 128
        Width = 60
        Height = 13
        Caption = 'File mask(s):'
        PopupMenu = ClearHistMenu
      end
      object Label5: TLabel
        Left = 16
        Top = 171
        Width = 66
        Height = 13
        Caption = 'Search mode:'
      end
      object HintedImageProxy1: THintedImageProxy
        Left = 307
        Top = 170
        Width = 16
        Height = 16
        Image = MainForm.HintImage
        ImageIndex = 0
        HintFmt = 
          '"Find first occurrence in each file" mode is useful to find all ' +
          'files containing some pattern.<br>It also works with "Replace al' +
          'l" command to replace first occurrence of pattern in each file.'
      end
      object RBInCurrentEditor: TRadioButton
        Left = 16
        Top = 38
        Width = 130
        Height = 17
        Caption = 'Current file'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RBInCurrentEditorClick
      end
      object RBInAllOpenFiles: TRadioButton
        Left = 16
        Top = 61
        Width = 130
        Height = 17
        Caption = 'All open files'
        TabOrder = 1
        OnClick = RBInCurrentEditorClick
      end
      object RBInSelectedDirectories: TRadioButton
        Left = 16
        Top = 84
        Width = 130
        Height = 17
        Caption = 'Selected directories:'
        PopupMenu = ClearHistMenu
        TabOrder = 2
        OnClick = RBInCurrentEditorClick
      end
      object EditInDirectories: TComboBox
        Left = 152
        Top = 82
        Width = 338
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnKeyDown = EditFindTextKeyDown
      end
      object EditFileNameMask: TComboBox
        Left = 87
        Top = 125
        Width = 432
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnKeyDown = EditFindTextKeyDown
      end
      object CBFilesSearchMode: TComboBox
        Left = 88
        Top = 168
        Width = 209
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'Find all occurrences'
        Items.Strings = (
          'Find all occurrences'
          'Find first occurrence in each file'
          'Find files NOT containing this pattern')
      end
    end
    object CPReplace: TCategoryPanel
      Top = 145
      Height = 144
      Caption = 'Replace'
      TabOrder = 1
      OnCollapse = CPFindExpand
      OnExpand = CPFindExpand
      object LblReplaceWith: TLabel
        Left = 16
        Top = 19
        Width = 65
        Height = 13
        Caption = 'Replace with:'
        PopupMenu = ClearHistMenu
      end
      object EditReplaceText: TComboBox
        Left = 87
        Top = 16
        Width = 432
        Height = 22
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyDown = EditFindTextKeyDown
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
        Caption = 'Replace all'
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
    object CPFind: TCategoryPanel
      Top = 0
      Height = 145
      Caption = 'Find'
      TabOrder = 2
      OnCollapse = CPFindExpand
      OnExpand = CPFindExpand
      object LblFind: TLabel
        Left = 16
        Top = 19
        Width = 24
        Height = 13
        Caption = 'Find:'
        PopupMenu = ClearHistMenu
      end
      object ImageProxy1: THintedImageProxy
        Left = 407
        Top = 48
        Width = 16
        Height = 16
        Image = MainForm.HintImage
        ImageIndex = 0
        HintFmt = 
          '<b>?</b> matches any byte (<b>??</b> in hex mode)<br><b>\xAA</b>' +
          ' includes hex byte in text pattern<br><b>{i32:1000}</b> matches ' +
          '32-bit value 1000<br>See help for more options'
      end
      object EditFindText: TComboBox
        Left = 87
        Top = 16
        Width = 432
        Height = 22
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyDown = EditFindTextKeyDown
      end
      object CBExtSyntax: TCheckBox
        Left = 302
        Top = 48
        Width = 107
        Height = 17
        Caption = 'Extended syntax'
        Checked = True
        ParentShowHint = False
        ShowHint = False
        State = cbChecked
        TabOrder = 1
      end
      object CBIgnoreCase: TCheckBox
        Left = 212
        Top = 48
        Width = 84
        Height = 17
        Caption = 'Ignore case'
        TabOrder = 2
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
        TabOrder = 3
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
        TabOrder = 4
        OnClick = BtnFindNextClick
      end
      object BtnFindCount: TButton
        Left = 247
        Top = 80
        Width = 90
        Height = 25
        Caption = 'Count'
        TabOrder = 5
        OnClick = BtnFindCountClick
      end
      object CBFindInSelection: TCheckBox
        Left = 447
        Top = 84
        Width = 84
        Height = 17
        Caption = 'In selection'
        TabOrder = 6
        OnClick = CBFindInSelectionClick
      end
      object BtnFindList: TButton
        Left = 350
        Top = 80
        Width = 90
        Height = 25
        Caption = 'List'
        TabOrder = 7
        OnClick = BtnFindListClick
      end
      object CBFindEncoding: TComboBox
        Left = 117
        Top = 46
        Width = 71
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 8
        Text = 'ansi'
        Items.Strings = (
          'ansi'
          'oem'
          'ucs2'
          'utf8')
      end
      object RBFindHex: TRadioButton
        Left = 16
        Top = 48
        Width = 48
        Height = 17
        Caption = 'HEX'
        TabOrder = 9
      end
      object RBFindText: TRadioButton
        Left = 70
        Top = 48
        Width = 47
        Height = 17
        Caption = 'Text:'
        Checked = True
        TabOrder = 10
        TabStop = True
      end
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 488
    Top = 73
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoAllowMultiSelect]
    Left = 480
    Top = 329
  end
  object DropFileCatcher1: TDropFileCatcher
    Control = Owner
    OnDropFiles = DropFileCatcher1DropFiles
    Left = 336
    Top = 344
  end
  object ClearHistMenu: TPopupMenu
    Left = 352
    Top = 8
    object Clearhistory1: TMenuItem
      Caption = 'Clear history'
      OnClick = Clearhistory1Click
    end
  end
end
