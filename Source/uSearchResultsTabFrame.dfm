object SearchResultsTabFrame: TSearchResultsTabFrame
  Left = 0
  Top = 0
  Width = 327
  Height = 408
  TabOrder = 0
  object ResultsList: TVirtualStringTree
    Left = 0
    Top = 28
    Width = 327
    Height = 380
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnDrawText = ResultsListDrawText
    OnFreeNode = ResultsListFreeNode
    OnGetText = ResultsListGetText
    OnNodeDblClick = ResultsListNodeDblClick
    Columns = <
      item
        Position = 0
        Text = 'Address'
        Width = 80
      end
      item
        Position = 1
        Text = 'Hex'
        Width = 117
      end
      item
        Position = 2
        Text = 'Text'
        Width = 130
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 327
    Height = 28
    Align = alTop
    TabOrder = 1
    object LblFoundCount: TLabel
      Left = 8
      Top = 8
      Width = 72
      Height = 13
      Caption = 'LblFoundCount'
    end
    object CBHighlightResults: TCheckBox
      Left = 144
      Top = 5
      Width = 97
      Height = 17
      Caption = 'Highlight'
      Checked = True
      State = cbChecked
      TabOrder = 0
      Visible = False
      OnClick = CBHighlightResultsClick
    end
  end
end
