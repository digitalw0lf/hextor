object MediaFrame: TMediaFrame
  Left = 0
  Top = 0
  Width = 523
  Height = 488
  TabOrder = 0
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 523
    Height = 41
    Align = alTop
    TabOrder = 0
    object BtnShow: TSpeedButton
      Left = 8
      Top = 8
      Width = 73
      Height = 27
      Hint = 'Display selected data as image/multimedia'
      Caption = 'Show'
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
      OnClick = BtnShowClick
    end
    object LblDetectedType: TLabel
      Left = 96
      Top = 14
      Width = 81
      Height = 13
      Caption = 'Detected type: ?'
    end
  end
  object ContentPanel: TPanel
    Left = 0
    Top = 41
    Width = 523
    Height = 447
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 523
      Height = 447
      Align = alClient
      PopupMenu = PopupMenu1
      Proportional = True
      Stretch = True
      Visible = False
      ExplicitLeft = 152
      ExplicitTop = 88
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object MediaPlayerPanel: TPanel
      Left = 0
      Top = 0
      Width = 523
      Height = 447
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object Panel1: TPanel
        Left = 0
        Top = 406
        Width = 523
        Height = 41
        Align = alBottom
        TabOrder = 0
        object MediaPlayer1: TMediaPlayer
          Left = 8
          Top = 6
          Width = 253
          Height = 30
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 523
        Height = 406
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object ErrorMemo: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 517
      Height = 441
      Align = alClient
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 120
    Top = 129
    object MICopyImage: TMenuItem
      Caption = 'Copy image'
      OnClick = MICopyImageClick
    end
    object MIDumpData: TMenuItem
      Caption = 'Dump data to file...'
      Visible = False
      OnClick = MIDumpDataClick
    end
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 216
    Top = 128
  end
end
