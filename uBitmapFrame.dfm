object BitmapFrame: TBitmapFrame
  Left = 0
  Top = 0
  Width = 749
  Height = 751
  DoubleBuffered = False
  ParentDoubleBuffered = False
  TabOrder = 0
  OnMouseWheel = FrameMouseWheel
  object MainPaintBox: TPaintBox
    Left = 81
    Top = 49
    Width = 651
    Height = 702
    Align = alClient
    OnMouseDown = MainPaintBoxMouseDown
    OnPaint = MainPaintBoxPaint
    ExplicitLeft = 79
    ExplicitTop = 53
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 49
    Width = 81
    Height = 702
    Align = alLeft
    TabOrder = 0
    object Label3: TLabel
      Left = 8
      Top = 152
      Width = 29
      Height = 13
      Caption = 'Scale:'
    end
    object Label4: TLabel
      Left = 8
      Top = 8
      Width = 49
      Height = 13
      Caption = 'Bytes/pix:'
    end
    object Label5: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = 'Palette:'
    end
    object BtnFlipVert: TSpeedButton
      Left = 8
      Top = 208
      Width = 23
      Height = 22
      Hint = 'Flip vertically'
      AllowAllUp = True
      GroupIndex = 1
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBE6D2BBBD8A4CFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBE6D2BBBC8849B5721EAB6A
        1AFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBE6
        D2BBBB8646B67623C5842BD08A2DAB6A1AFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBE5D1B8BB8646B87B29C98F3AD49A40D39439D18F33AB6A
        1AFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBE4CFB5BB8646B9802FCD9C49D9
        AB56D8A54FD6A048D49A40D39439AB6A1AFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        BB8646AB6A1AAB6A1AAB6A1AAB6A1AAB6A1AAB6A1AAB6A1AAB6A1AAB6A1AAB6A
        1AFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBEDECECDADADAD2D2D2CBCBCBC4C4C4BD
        BDBDB6B6B6B0B0B0ACACACA8A8A8A8A8A8FDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFAF9F8E8E7E7D3D3D3CBCBCBC4C4C4BDBDBDB6B6B6B0B0B0ACACACA8A8
        A8FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF8F7F6E3E2E2CB
        CBCBC4C4C4BDBDBDB6B6B6B0B0B0ACACACFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF6F5F4DDDCDCC4C4C4BDBDBDB6B6B6B0B0
        B0FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBF3F2F2D7D7D6BDBDBDB6B6B6FDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF1F0F0D0D0
        CFFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnFlipVertClick
    end
    object BtnFlipHorz: TSpeedButton
      Left = 37
      Top = 208
      Width = 23
      Height = 22
      Hint = 'Flip horizontally'
      AllowAllUp = True
      GroupIndex = 2
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBCDCDCC
        B6B6B6B0B0B0ACACACA8A8A8A8A8A8FDFCFBFDFCFBAB6A1AAB6A1AAB6A1AAB6A
        1AAB6A1ABF8F54FDFCFBFDFCFBEEEEEDBDBDBDB6B6B6B0B0B0ACACACA8A8A8FD
        FCFBFDFCFBAB6A1AD39439D18F33D08A2DB4711EE8D6C0FDFCFBFDFCFBFDFCFB
        D4D4D3BDBDBDB6B6B6B0B0B0ACACACFDFCFBFDFCFBAB6A1AD49A40D39439C483
        2BBF8D51FDFCFBFDFCFBFDFCFBFDFCFBF2F1F1C4C4C4BDBDBDB6B6B6B0B0B0FD
        FCFBFDFCFBAB6A1AD6A048D49A40B57522E8D6C0FDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBDADAD9C4C4C4BDBDBDB6B6B6FDFCFBFDFCFBAB6A1AD8A54FC88E39BE8C
        4EFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF4F3F3CBCBCBC4C4C4BDBDBDFD
        FCFBFDFCFBAB6A1AD9AB56B77928E8D6C0FDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBE0E0DFCBCBCBC4C4C4FDFCFBFDFCFBAB6A1ACC9A48BE8C4EFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF7F6F5D3D3D3CBCBCBFD
        FCFBFDFCFBAB6A1AB87E2EE7D4BEFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBE7E6E6D2D2D2FDFCFBFDFCFBAB6A1ABE8C4EFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBF9F8F7DADADAFD
        FCFBFDFCFBAB6A1AE7D4BEFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBEBEAEAFBFAF9FDFCFBBD8A4CFDFCFBFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB
        FDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFC
        FBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFD
        FCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFBFDFCFB}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnFlipHorzClick
    end
    object Label6: TLabel
      Left = 8
      Top = 56
      Width = 50
      Height = 13
      Caption = 'Byte shift:'
    end
    object EditScale: TComboBox
      Left = 8
      Top = 168
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = '1'
      OnChange = EditScaleChange
      Items.Strings = (
        '1/4'
        '1/2'
        '1'
        '2'
        '4')
    end
    object EditBPP: TComboBox
      Left = 8
      Top = 24
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 1
      Text = '1 byte'
      OnChange = EditBPPChange
      Items.Strings = (
        '1 bit'
        '2 bits'
        '4 bits'
        '1 byte'
        '2 bytes'
        '3 bytes'
        '4 bytes')
    end
    object EditPalette: TComboBox
      Left = 8
      Top = 120
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemIndex = 6
      TabOrder = 2
      Text = 'RGB (2^24)'
      OnChange = EditPaletteChange
      Items.Strings = (
        'B/W (2)'
        'BBRW (4)'
        'DOS (16)'
        'Red (256)'
        'Green (256)'
        'Blue (256)'
        'RGB (2^24)'
        'BGR (2^24)')
    end
    object EditByteShift: TSpinEdit
      Left = 8
      Top = 72
      Width = 65
      Height = 22
      Hint = '0..3 bytes shift of first pixel in case of multy-byte pixels'
      MaxValue = 3
      MinValue = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Value = 0
      OnChange = EditByteShiftChange
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 749
    Height = 49
    Align = alTop
    TabOrder = 1
    OnResize = TopPanelResize
    DesignSize = (
      749
      49)
    object Label1: TLabel
      Left = 2
      Top = 3
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label2: TLabel
      Left = 2
      Top = 28
      Width = 29
      Height = 13
      Caption = 'Scroll:'
    end
    object TrackBarWidth: TTrackBar
      Left = 78
      Top = 0
      Width = 671
      Height = 27
      Anchors = [akLeft, akTop, akRight]
      Max = 640
      Min = 1
      PageSize = 1
      Position = 256
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBarWidthChange
    end
    object TrackBarHScroll: TTrackBar
      Left = 78
      Top = 25
      Width = 671
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Max = 640
      PageSize = 1
      TabOrder = 3
      TickStyle = tsNone
      OnChange = TrackBarHScrollChange
    end
    object EditHScroll: TSpinEdit
      Left = 32
      Top = 25
      Width = 50
      Height = 22
      MaxValue = 16384
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = EditHScrollChange
    end
    object EditWidth: TSpinEdit
      Left = 32
      Top = 0
      Width = 50
      Height = 22
      MaxValue = 16384
      MinValue = 1
      TabOrder = 0
      Value = 256
      OnChange = EditWidthChange
    end
  end
  object VertScrollBar: TScrollBar
    Left = 732
    Top = 49
    Width = 17
    Height = 702
    Align = alRight
    Kind = sbVertical
    Max = 1000
    PageSize = 0
    TabOrder = 2
    OnChange = VertScrollBarChange
  end
end
