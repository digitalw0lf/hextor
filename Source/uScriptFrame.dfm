object ScriptFrame: TScriptFrame
  Left = 0
  Top = 0
  Width = 401
  Height = 644
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 508
    Width = 401
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
    ExplicitTop = 504
  end
  object OutputPanel: TPanel
    Left = 0
    Top = 512
    Width = 401
    Height = 132
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object MemoOutput: TRichEdit
      Left = 0
      Top = 20
      Width = 401
      Height = 112
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object OutputToolPanel: TPanel
      Left = 0
      Top = 0
      Width = 401
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object BtnClearOutput: TSpeedButton
        Left = 344
        Top = 0
        Width = 57
        Height = 20
        Align = alRight
        Caption = 'Clear'
        OnClick = BtnClearOutputClick
        ExplicitLeft = 328
        ExplicitTop = 6
        ExplicitHeight = 21
      end
    end
  end
  object ScriptEdit: TSynEdit
    Left = 0
    Top = 26
    Width = 401
    Height = 482
    Align = alClient
    CaseSensitive = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.DigitCount = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.ShowLineNumbers = True
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = SynJScriptSyn1
    SelectedColor.Alpha = 0.449999988079071000
    TabWidth = 2
    VisibleSpecialChars = []
    WantTabs = True
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 401
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Caption = 'ToolBar1'
    Images = MainForm.VirtualImageList1
    TabOrder = 2
    object BtnNew: TToolButton
      Left = 0
      Top = 0
      Hint = 'New script'
      Caption = 'BtnNew'
      ImageIndex = 0
      ImageName = 'EmptyFile'
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnNewClick
    end
    object BtnLoad: TToolButton
      Left = 26
      Top = 0
      Hint = 'Open script'
      Caption = 'BtnLoad'
      ImageIndex = 1
      ImageName = 'Open'
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnLoadClick
    end
    object BtnSave: TToolButton
      Left = 52
      Top = 0
      Hint = 'Save script'
      Caption = 'BtnSave'
      DropdownMenu = SaveAsMenu
      ImageIndex = 2
      ImageName = 'Save'
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
      OnClick = MISaveAsClick
    end
    object BtnRun: TToolButton
      Left = 97
      Top = 0
      Hint = 'Run'
      Caption = 'BtnRun'
      ImageIndex = 8
      ImageName = 'GoArrow'
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnRunClick
    end
    object LblScriptName: TLabel
      Left = 123
      Top = 0
      Width = 64
      Height = 26
      Caption = '    Unnamed'
      Transparent = True
      Layout = tlCenter
    end
  end
  object Timer1: TTimer
    Left = 144
    Top = 296
  end
  object SynJScriptSyn1: TSynJScriptSyn
    Left = 104
    Top = 184
  end
  object SavedScriptsMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = MainForm.VirtualImageList1
    Left = 40
    Top = 104
    object MIBuiltinItemsMenu: TMenuItem
      Caption = 'Built-in'
      ImageIndex = 23
      ImageName = 'Folder'
    end
    object MIAfterFileItems: TMenuItem
      Caption = '-'
    end
    object MIOrganizeFiles: TMenuItem
      Caption = 'Organize scripts'
      OnClick = MIOrganizeFilesClick
    end
    object MIDummyScript: TMenuItem
      Caption = 'MIDummyScript'
      Visible = False
      OnClick = MIDummyScriptClick
    end
  end
  object SaveAsMenu: TPopupMenu
    Left = 128
    Top = 104
    object MISaveAs: TMenuItem
      Caption = 'Save as...'
      OnClick = MISaveAsClick
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ds'
    Filter = 'All files|*|Scripts (*.js)|*.js'
    FilterIndex = 2
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 40
    Top = 168
  end
end
