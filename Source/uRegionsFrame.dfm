object RegionsFrame: TRegionsFrame
  Left = 0
  Top = 0
  Width = 411
  Height = 595
  TabOrder = 0
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 411
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object RegionsTreeView: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 411
    Height = 554
    Align = alClient
    Header.AutoSizeIndex = 2
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible, hoAutoResizeInclCaption]
    TabOrder = 1
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnFreeNode = RegionsTreeViewFreeNode
    OnGetText = RegionsTreeViewGetText
    OnNodeDblClick = RegionsTreeViewNodeDblClick
    Columns = <
      item
        Position = 0
        Text = 'Address'
        Width = 151
      end
      item
        Position = 1
        Text = 'Size'
        Width = 100
      end
      item
        Position = 2
        Text = 'Description'
        Width = 441
      end>
  end
end
