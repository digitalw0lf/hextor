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
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    OnChanging = PageControlChanging
    OnMouseUp = PageControlMouseUp
  end
  object ResultsTabMenu: TPopupMenu
    Left = 120
    Top = 104
    object MIClose: TMenuItem
      Caption = 'Close'
      OnClick = MICloseClick
    end
    object MICloseOtherTabs: TMenuItem
      Caption = 'Close other tabs'
      OnClick = MICloseOtherTabsClick
    end
    object MICloseAll: TMenuItem
      Caption = 'Close all'
      OnClick = MICloseAllClick
    end
  end
end
