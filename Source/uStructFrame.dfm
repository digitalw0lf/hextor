object StructFrame: TStructFrame
  Left = 0
  Top = 0
  Width = 297
  Height = 629
  HelpType = htKeyword
  HelpKeyword = 'Structure-analyzer'
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  OnResize = FrameResize
  object PnlButtonBar2: TPanel
    Left = 0
    Top = 254
    Width = 297
    Height = 41
    Cursor = crVSplit
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnMouseDown = PnlButtonBar2MouseDown
    OnMouseMove = PnlButtonBar2MouseMove
    OnMouseUp = PnlButtonBar2MouseUp
    object BtnInterpret: TButton
      Left = 8
      Top = 8
      Width = 98
      Height = 25
      Caption = 'Interpret'
      DropDownMenu = InterpretRangeMenu
      ImageIndex = 8
      Images = MainForm.ImageList16
      Style = bsSplitButton
      TabOrder = 0
      OnClick = BtnInterpretClick
    end
    object BtnCopyValue: TButton
      Left = 128
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Copy JSON'
      Enabled = False
      ImageIndex = 10
      Images = MainForm.ImageList16
      TabOrder = 1
      OnClick = BtnCopyValueClick
    end
    object Panel1: TPanel
      Left = 256
      Top = 0
      Width = 41
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object BtnHelp: TButton
        Left = 8
        Top = 8
        Width = 25
        Height = 25
        HelpType = htKeyword
        HelpKeyword = 'Structure-analyzer'
        ImageAlignment = iaCenter
        ImageIndex = 20
        Images = MainForm.ImageList16
        TabOrder = 0
        OnClick = BtnHelpClick
      end
    end
  end
  object DSDescrEdit: TSynEdit
    Left = 0
    Top = 26
    Width = 297
    Height = 228
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.DigitCount = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    Gutter.ShowModification = True
    Highlighter = SynCppSyn1
    MaxScrollWidth = 512
    Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    TabWidth = 2
    WantTabs = True
    FontSmoothing = fsmNone
  end
  object DSTreeView: TVirtualStringTree
    Left = 0
    Top = 295
    Width = 297
    Height = 334
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    HintMode = hmHint
    ParentShowHint = False
    PopupMenu = DSFieldPopupMenu
    ShowHint = True
    TabOrder = 3
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    OnBeforeItemErase = DSTreeViewBeforeItemErase
    OnChange = DSTreeViewChange
    OnEnter = DSTreeViewEnter
    OnExit = DSTreeViewExit
    OnFreeNode = DSTreeViewFreeNode
    OnGetText = DSTreeViewGetText
    OnGetHint = DSTreeViewGetHint
    OnInitChildren = DSTreeViewInitChildren
    OnInitNode = DSTreeViewInitNode
    OnNodeDblClick = DSTreeViewNodeDblClick
    Columns = <>
  end
  object EditFieldValue: TEdit
    Left = 144
    Top = 304
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'EditFieldValue'
    Visible = False
    OnExit = EditFieldValueExit
    OnKeyDown = EditFieldValueKeyDown
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 297
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Caption = 'ToolBar1'
    Images = MainForm.ImageList16
    TabOrder = 4
    object BtnNewDescr: TToolButton
      Left = 0
      Top = 0
      Hint = 'New structure description'
      Caption = 'BtnNewDescr'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnNewDescrClick
    end
    object BtnLoadDescr: TToolButton
      Left = 26
      Top = 0
      Hint = 'Open structure description'
      Caption = 'BtnLoadDescr'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnLoadDescrClick
    end
    object BtnSaveDescr: TToolButton
      Left = 52
      Top = 0
      Hint = 'Save structure description'
      Caption = 'BtnSaveDescr'
      DropdownMenu = SaveAsMenu
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
      OnClick = MISaveAsClick
    end
    object LblStructName: TLabel
      Left = 93
      Top = 0
      Width = 57
      Height = 26
      Caption = '    Unnamed'
      Transparent = True
      Layout = tlCenter
    end
  end
  object SavedDescrsMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = MainForm.ImageList16
    Left = 40
    Top = 104
    object MIBuiltinDSMenu: TMenuItem
      Caption = 'Built-in'
      ImageIndex = 23
    end
    object MIAfterDSItems: TMenuItem
      Caption = '-'
    end
    object MIOrganizeFiles: TMenuItem
      Caption = 'Organize descriptions'
      OnClick = MIOrganizeFilesClick
    end
    object MIDummyDataStruct: TMenuItem
      Caption = 'MIDummyDataStruct'
      Visible = False
      OnClick = MIDummyDataStructClick
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ds'
    Filter = 'All files|*|DataStruct (*.ds)|*.ds'
    FilterIndex = 2
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 40
    Top = 168
  end
  object SynCppSyn1: TSynCppSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clGreen
    Left = 224
    Top = 104
  end
  object SaveAsMenu: TPopupMenu
    Left = 128
    Top = 104
    object MISaveAs: TMenuItem
      Caption = 'Save as...'
      OnClick = MISaveAsClick
    end
  end
  object InterpretRangeMenu: TPopupMenu
    OnPopup = InterpretRangeMenuPopup
    Left = 72
    Top = 288
    object MIRangeEntireFile: TMenuItem
      Caption = 'Entire file'
      Checked = True
      RadioItem = True
      OnClick = MIRangeEntireFileClick
    end
    object MIRangeSelection: TMenuItem
      Tag = 1
      Caption = 'Selection'
      RadioItem = True
      OnClick = MIRangeEntireFileClick
    end
  end
  object DSFieldPopupMenu: TPopupMenu
    OnPopup = DSFieldPopupMenuPopup
    Left = 72
    Top = 408
    object MISelectInEditor: TMenuItem
      Caption = 'Select in editor'
      OnClick = MISelectInEditorClick
    end
    object MIGotoAddr: TMenuItem
      Caption = 'Go to X'
      OnClick = MIGotoAddrClick
    end
    object MICopyFieldName: TMenuItem
      Caption = 'Copy name'
      OnClick = MICopyFieldNameClick
    end
    object MICopyFieldFullName: TMenuItem
      Caption = 'Copy full name'
      OnClick = MICopyFieldFullNameClick
    end
    object MICopyFieldValue: TMenuItem
      Caption = 'Copy value'
      OnClick = MICopyFieldValueClick
    end
  end
end
