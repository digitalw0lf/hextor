object ValueFrame: TValueFrame
  Left = 0
  Top = 0
  Width = 328
  Height = 598
  TabOrder = 0
  object ValuesTreeView: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 328
    Height = 598
    Align = alClient
    DefaultNodeHeight = 20
    Header.AutoSizeIndex = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    HintMode = hmHint
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toWheelPanning, toEditOnDblClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    OnEditing = ValuesTreeViewEditing
    OnExit = ValuesTreeViewExit
    OnFocusChanged = ValuesTreeViewFocusChanged
    OnFocusChanging = ValuesTreeViewFocusChanging
    OnFreeNode = ValuesTreeViewFreeNode
    OnGetText = ValuesTreeViewGetText
    OnGetHint = ValuesTreeViewGetHint
    OnGetPopupMenu = ValuesTreeViewGetPopupMenu
    OnNewText = ValuesTreeViewNewText
    Columns = <
      item
        Position = 0
        Text = 'Type'
        Width = 67
      end
      item
        Position = 1
        Text = 'Value'
        Width = 257
      end>
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
