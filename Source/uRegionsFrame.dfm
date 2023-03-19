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
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = 2
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible, hoAutoResizeInclCaption]
    TabOrder = 1
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnDrawText = RegionsTreeViewDrawText
    OnFreeNode = RegionsTreeViewFreeNode
    OnGetText = RegionsTreeViewGetText
    OnNodeDblClick = RegionsTreeViewNodeDblClick
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
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
