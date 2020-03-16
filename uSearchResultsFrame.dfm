object SearchResultsFrame: TSearchResultsFrame
  Left = 0
  Top = 0
  Width = 347
  Height = 547
  TabOrder = 0
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 347
    Height = 547
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object ResultsList: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 339
        Height = 500
        Align = alClient
        Header.AutoSizeIndex = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toThemeAware, toUseBlendedImages]
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
            Width = 125
          end
          item
            Position = 2
            Text = 'Text'
            Width = 130
          end>
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 500
        Width = 339
        Height = 19
        Panels = <
          item
            Width = 50
          end>
      end
    end
  end
end
