object CompareFrame: TCompareFrame
  Left = 0
  Top = 0
  Width = 497
  Height = 607
  DoubleBuffered = True
  ParentBackground = False
  ParentDoubleBuffered = False
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 497
    Height = 607
    ActivePage = ComparisonTab
    Align = alClient
    TabOrder = 1
    object InitialTab: TTabSheet
      Caption = 'InitialTab'
      object BtnStartCompare: TButton
        Left = 8
        Top = 8
        Width = 105
        Height = 25
        Caption = 'Compare files...'
        TabOrder = 0
        OnClick = BtnStartCompareClick
      end
    end
    object ComparisonTab: TTabSheet
      Caption = 'ComparisonTab'
      ImageIndex = 1
      object DiffBar: TPaintBox
        Left = 0
        Top = 0
        Width = 58
        Height = 579
        Align = alLeft
        OnMouseDown = DiffBarMouseDown
        OnMouseMove = DiffBarMouseMove
        OnPaint = DiffBarPaint
      end
      object BtnCloseComparison: TSpeedButton
        Left = 248
        Top = 8
        Width = 27
        Height = 25
        Hint = 'Exit compare mode'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          BFDDE02D595E52A1A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF52A1A92D595EBFDD
          E0FFFFFFFFFFFFFFFFFFFFFFFFBFDDE02D595E2D595E2D595E52A1A9FFFFFFFF
          FFFFFFFFFF52A1A92D595E2D595E2D595EBFDDE0FFFFFFFFFFFFFFFFFF8DC2C8
          2D595E4282894282892D595E52A1A98DC2C852A1A92D595E4282894282892D59
          5E8DC2C8FFFFFFFFFFFFFFFFFFFFFFFF52A1A92D595E4282894282892D595E2D
          595E2D595E4282894282892D595E52A1A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF52A1A92D595E4282894282894282894282894282892D595E52A1A9FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2D595E42828942828942
          82894282894282892D595EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF2D595E42828942828952A1A94282894282892D595EFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF52A1A92D595E52A1A952A1A952
          A1A952A1A952A1A92D595E52A1A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          72B4BB45878E71B3BA71B3BA52A1A945878E52A1A971B3BA71B3BA45878E72B4
          BBFFFFFFFFFFFFFFFFFFFFFFFF8DC2C845878E71B3BA71B3BA45878E52A1A98D
          C2C852A1A945878E71B3BA71B3BA45878E8DC2C8FFFFFFFFFFFFFFFFFFBFDDE0
          45878E52A1A945878E72B4BBFFFFFFFFFFFFFFFFFF72B4BB45878E52A1A94587
          8EBFDDE0FFFFFFFFFFFFFFFFFFFFFFFFBFDDE045878E72B4BBFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF72B4BB45878EBFDDE0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnCloseComparisonClick
      end
      object BtnPrevDiff: TSpeedButton
        Tag = -1
        Left = 64
        Top = 48
        Width = 30
        Height = 30
        Hint = 'Previous difference (from cursor)'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808000
          8080008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF00808005BEC406BBC1008080FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808004C5C905C2C700
          8080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF00808003CDD103CACE04C7CB008080FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808002D5D703D1D403CED200808000
          8080008080008080008080008080008080008080008080008080FFFFFF008080
          01DDDE01DADC02D6D903D2D504CFD203CCD004C8CC05C5C905C2C705BEC406BB
          C107B8BF07B5BC00808000808000E5E500E1E201DDDE02DADC02D7D902D4D603
          D0D303CCD004C9CD05C6CB05C3C806BFC506BCC207B9BF00808000808000E4E4
          00E5E501E2E201DEDE01DBDC02D8DA03D4D603D1D403CED204CBCF05C7CC05C3
          C805C1C606BCC2008080FFFFFF00808000E4E400E5E501E3E300E0E101DCDE02
          D8DA03D5D703D2D503CED104CBCE04C8CC05C4C805C1C6008080FFFFFFFFFFFF
          00808000E4E400E5E500E3E30080800080800080800080800080800080800080
          80008080008080008080FFFFFFFFFFFFFFFFFF00808000E4E400E5E500E5E500
          8080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF00808000E5E500E4E4008080FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808000E4E400
          E4E4008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF008080008080008080FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnNextDiffClick
      end
      object BtnNextDiff: TSpeedButton
        Tag = 1
        Left = 103
        Top = 48
        Width = 30
        Height = 30
        Hint = 'Next difference (from cursor)'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          8080008080008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808006BBC105BEC4008080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00808005C2C704C5C9008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808004C7CB03CACE03CDD10080
          80FFFFFFFFFFFFFFFFFF00808000808000808000808000808000808000808000
          808000808000808003CED203D1D402D5D7008080FFFFFFFFFFFF00808007B5BC
          07B8BF06BBC105BEC405C2C705C5C904C8CC03CCD004CFD203D2D502D6D901DA
          DC01DDDE008080FFFFFF00808007B9BF06BCC206BFC505C3C805C6CB04C9CD03
          CCD003D0D302D4D602D7D902DADC01DDDE00E1E200E5E500808000808006BCC2
          05C1C605C3C805C7CC04CBCF03CED203D1D403D4D602D8DA01DBDC01DEDE01E2
          E200E5E500E4E400808000808005C1C605C4C804C8CC04CBCE03CED103D2D503
          D5D702D8DA01DCDE00E0E101E3E300E5E500E4E4008080FFFFFF008080008080
          00808000808000808000808000808000808000808000808000E3E300E5E500E4
          E4008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00808000E5E500E5E500E4E4008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00808000E4E400E5E5008080FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          808000E4E400E4E4008080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080008080008080FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnNextDiffClick
      end
      object BtnSyncCaret: TSpeedButton
        Left = 159
        Top = 48
        Width = 30
        Height = 30
        Hint = 'Sync caret position. Ctrl+click in editor for inversed behavior'
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFF
          FFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000000000000000
          FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFF
          FFFFFFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00000000FFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFF
          FFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000000000000000
          FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFF
          FFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnSyncCaretClick
      end
      object BtnRecompare: TButton
        Left = 64
        Top = 8
        Width = 89
        Height = 25
        Caption = 'Recompare'
        ImageIndex = 12
        ImageName = 'Refresh'
        Images = MainForm.VirtualImageList1
        TabOrder = 0
        OnClick = BtnRecompareClick
      end
      object BtnAbort: TButton
        Left = 159
        Top = 8
        Width = 74
        Height = 25
        Caption = 'Abort'
        TabOrder = 1
        Visible = False
        OnClick = BtnAbortClick
      end
      object MemoDiffStats: TMemo
        Left = 64
        Top = 96
        Width = 267
        Height = 89
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object CompareSelectFormPanel: TPanel
    Left = 12
    Top = 272
    Width = 457
    Height = 305
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object Label3: TLabel
      Left = 16
      Top = 212
      Width = 75
      Height = 13
      Caption = 'Sync block size:'
    end
    object Label4: TLabel
      Left = 184
      Top = 212
      Width = 27
      Height = 13
      Caption = 'bytes'
    end
    object ImageProxy1: THintedImageProxy
      Left = 231
      Top = 211
      Width = 16
      Height = 16
      Image = MainForm.HintImage
      ImageIndex = 0
      HintFmt = 
        'When comparing different data, detect similarities if they are a' +
        't least this size'
    end
    object BtnCompare: TButton
      Left = 136
      Top = 256
      Width = 75
      Height = 25
      Caption = 'Compare'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      Left = 248
      Top = 256
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object CBSyncBlockSize: TComboBox
      Left = 112
      Top = 209
      Width = 57
      Height = 21
      Style = csDropDownList
      ItemIndex = 4
      TabOrder = 2
      Text = '16'
      Items.Strings = (
        '1'
        '2'
        '4'
        '8'
        '16'
        '32'
        '64')
    end
    object GBFile1: TGroupBox
      Left = 8
      Top = 3
      Width = 441
      Height = 89
      Caption = 'Left'
      TabOrder = 3
      object LblRange1Start: TLabel
        Left = 120
        Top = 56
        Width = 23
        Height = 13
        Caption = 'start'
        Enabled = False
      end
      object LblRange1End: TLabel
        Left = 280
        Top = 56
        Width = 18
        Height = 13
        Caption = 'end'
        Enabled = False
      end
      object CBCmpEditor1: TComboBox
        Left = 8
        Top = 21
        Width = 417
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = CBCmpEditor1Change
      end
      object CBRange1: TCheckBox
        Left = 8
        Top = 54
        Width = 97
        Height = 17
        Hint = 'Range is pre-filled with current selection'
        Caption = 'Specify range:'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = CBRange1Click
      end
      object EditRange1Start: TEdit
        Left = 150
        Top = 52
        Width = 121
        Height = 21
        Hint = 'Range is pre-filled with current selection'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = '0'
      end
      object EditRange1End: TEdit
        Left = 304
        Top = 52
        Width = 121
        Height = 21
        Hint = 'Range is pre-filled with current selection'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '0'
      end
    end
    object GBFile2: TGroupBox
      Left = 8
      Top = 106
      Width = 441
      Height = 90
      Caption = 'Right'
      TabOrder = 4
      object LblRange2Start: TLabel
        Left = 120
        Top = 56
        Width = 23
        Height = 13
        Caption = 'start'
        Enabled = False
      end
      object LblRange2End: TLabel
        Left = 280
        Top = 56
        Width = 18
        Height = 13
        Caption = 'end'
        Enabled = False
      end
      object CBCmpEditor2: TComboBox
        Left = 8
        Top = 21
        Width = 417
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = CBCmpEditor1Change
      end
      object CBRange2: TCheckBox
        Left = 8
        Top = 54
        Width = 97
        Height = 17
        Hint = 'Range is pre-filled with current selection'
        Caption = 'Specify range:'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = CBRange2Click
      end
      object EditRange2Start: TEdit
        Left = 150
        Top = 52
        Width = 121
        Height = 21
        Hint = 'Range is pre-filled with current selection'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = '0'
      end
      object EditRange2End: TEdit
        Left = 304
        Top = 52
        Width = 121
        Height = 21
        Hint = 'Range is pre-filled with current selection'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '0'
      end
    end
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
