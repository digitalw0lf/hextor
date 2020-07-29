object BookmarksFrame: TBookmarksFrame
  Left = 0
  Top = 0
  Width = 390
  Height = 561
  TabOrder = 0
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 390
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Caption = 'ToolBar1'
    Images = MainForm.ImageList16
    TabOrder = 0
    object BtnAddFolder: TToolButton
      Left = 0
      Top = 0
      Hint = 'Add folder'
      Caption = 'BtnAddFolder'
      ImageIndex = 23
      ParentShowHint = False
      ShowHint = True
      Visible = False
    end
    object BtnAddBookmark: TToolButton
      Left = 26
      Top = 0
      Hint = 'Add bookmark'
      Caption = 'BtnAddBookmark'
      ImageIndex = 24
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddBookmarkClick
    end
    object BtnDelete: TToolButton
      Left = 52
      Top = 0
      Hint = 'Delete selected bookmarks and folders'
      Caption = 'BtnDelete'
      ImageIndex = 25
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnDeleteClick
    end
  end
  object BookmarksTreeView: TVirtualStringTree
    Left = 0
    Top = 26
    Width = 390
    Height = 535
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
    TreeOptions.StringOptions = [toSaveCaptions, toShowStaticText, toAutoAcceptEditChange]
    OnFreeNode = BookmarksTreeViewFreeNode
    OnGetText = BookmarksTreeViewGetText
    OnPaintText = BookmarksTreeViewPaintText
    OnNewText = BookmarksTreeViewNewText
    OnNodeDblClick = BookmarksTreeViewNodeDblClick
    Columns = <
      item
        Position = 0
        Text = 'Caption'
        Width = 140
      end
      item
        Position = 1
        Text = 'Position'
        Width = 80
      end
      item
        Position = 2
        Text = 'File'
        Width = 180
      end>
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 64
    Top = 112
  end
end
