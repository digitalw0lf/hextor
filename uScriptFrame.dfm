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
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 33
    Align = alTop
    TabOrder = 0
    object BtnRun: TSpeedButton
      Left = 4
      Top = 4
      Width = 26
      Height = 26
      Hint = 'Run script'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1587211EB73D158721FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF15872128C14E23BC461EB73D158721FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15872128C14E23BC461F
        B83E158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF15872128C14E23BC4620B940158721FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15872128
        C14E23BC4620B940158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15872128C14E23BC4620B940158721FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF15872128C14E23BC4620B940158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1587212BC45728C14E23BC461587
        21FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15
        872133CC662FC85F2BC457158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF15872138D16B35CE6833CC66158721FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15872140D9733C
        D56F38D16B158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF15872147E07A44DD7740D973158721FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF15872150E9834CE57F47E07A15
        8721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF15872150E983158721FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF158721FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnRunClick
    end
  end
  object OutputPanel: TPanel
    Left = 0
    Top = 512
    Width = 401
    Height = 132
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
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
      Zoom = 100
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
    Top = 33
    Width = 401
    Height = 475
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
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
    Gutter.ShowLineNumbers = True
    FontSmoothing = fsmNone
  end
  object Timer1: TTimer
    Left = 104
    Top = 288
  end
end
