object HashFrame: THashFrame
  Left = 0
  Top = 0
  Width = 414
  Height = 575
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 321
    Width = 414
    Height = 3
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 100
    ResizeStyle = rsUpdate
    ExplicitTop = 185
    ExplicitWidth = 390
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 414
    Height = 321
    Align = alTop
    TabOrder = 0
    DesignSize = (
      414
      321)
    object RGDataRange: TRadioGroup
      Left = 207
      Top = 8
      Width = 137
      Height = 73
      Caption = 'Range'
      ItemIndex = 0
      Items.Strings = (
        'Entire file'
        'Selection')
      TabOrder = 0
    end
    object BtnCalculate: TButton
      Left = 208
      Top = 96
      Width = 81
      Height = 25
      Caption = 'Calculate'
      ImageIndex = 8
      Images = MainForm.ImageList16
      TabOrder = 1
      OnClick = BtnCalculateClick
    end
    object AlgorithmsListBox: TListBox
      Left = 8
      Top = 8
      Width = 185
      Height = 305
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 2
      OnClick = AlgorithmsListBoxClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 324
    Width = 414
    Height = 251
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object StaticText1: TStaticText
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 408
      Height = 17
      Align = alTop
      Caption = ' '
      TabOrder = 0
    end
    object ResultListView: TListView
      Left = 0
      Top = 23
      Width = 414
      Height = 228
      Align = alClient
      Columns = <
        item
          Caption = 'Algorithm'
          Width = 120
        end
        item
          AutoSize = True
          Caption = 'Value'
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = ResultPopupMenu
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
  object ResultPopupMenu: TPopupMenu
    Left = 56
    Top = 432
    object MICopy: TMenuItem
      Caption = 'Copy'
      OnClick = MICopyClick
    end
  end
end
