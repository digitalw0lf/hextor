object ValueFrame: TValueFrame
  Left = 0
  Top = 0
  Width = 328
  Height = 598
  TabOrder = 0
  object ValuesGrid: TKGrid
    Left = 0
    Top = 0
    Width = 328
    Height = 598
    Align = alClient
    ColCount = 2
    Options = [goAlignLastCol, goColSizing, goDrawFocusSelected, goEditing, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseOverCells, goThemes, goThemedCells, goVertLine]
    RowCount = 2
    TabOrder = 0
    OnClick = ValuesGridClick
    OnEditorDataToGrid = ValuesGridEditorDataToGrid
    OnEditorSelect = ValuesGridEditorSelect
    OnExit = ValuesGridExit
    OnMouseDown = ValuesGridMouseDown
    OnMouseUp = ValuesGridMouseUp
    ColWidths = (
      64
      263)
    RowHeights = (
      10
      21)
  end
  object ValuePopupMenu: TPopupMenu
    Left = 96
    Top = 264
    object MICopyValue: TMenuItem
      Caption = 'Copy'
      OnClick = MICopyValueClick
    end
  end
end
