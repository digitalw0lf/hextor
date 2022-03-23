object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = EditByteCols
  Caption = 'Hextor'
  ClientHeight = 639
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 604
    Top = 47
    Width = 4
    Height = 592
    Align = alRight
    AutoSnap = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 840
    ExplicitTop = 49
    ExplicitHeight = 590
  end
  object HintImage: TImage
    Left = 31
    Top = 163
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D617036030000424D360300000000000036000000280000001000
      000010000000010018000000000000030000130B0000130B0000000000000000
      000000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
      00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00F8DE
      C9D6BAA2B6845AAC7445AB7243B27E53D2B59CF8DEC900FF0000FF0000FF0000
      FF0000FF0000FF0000FF00E7D5C6BA895FD7BBA3E9DACAECE0D1ECE0D1E8D8C8
      D3B59CB07A4DE2CFBE00FF0000FF0000FF0000FF0000FF00EAD9CBBE8C62E7D5
      C4E5D2BFC9A685B88E67B68A65C5A180E0CCBAE3D0BEAF7648E3D0C000FF0000
      FF0000FF00F8DEC9C99D79EAD8C9E3CDBAC0946BBA8C62CFB094CFB094B7895F
      B28761DAC0AAE4D1C0B68359F8DEC900FF0000FF00E6CFBCE4CCB9EAD6C5C799
      71BF9066BF9066F7F1ECF6F0EAB7895FB7895FB58963E2CEBBD9BDA6D9BEA700
      FF0000FF00D9B395EFE1D3D9B595C7986CC39569C19367BF9066BF9066BB8B63
      B98A63B88A62CBA786EADCCCC2956F00FF0000FF00DAB393F2E4D9D1A57AC599
      6BC4976AC49669FAF6F2F3EAE1C2956DBE8F65BE8F64C0956DEFE3D5C1906700
      FF0000FF00E1BB9DF2E5DAD1A67ECC9D71C79A6CC5986BE2CCB6F8F3EEF6EEE8
      D9BDA1C29468C59B71F0E2D6C7997100FF0000FF00EACAB0F3E5D9DFBB9ECFA0
      75CD9E72F5EBE3E4CBB4E7D3BFFBF8F6E5D3BFC4986BD6B491EEE0D2D3AC8B00
      FF0000FF00F5E4D6F4E3D4EFDCCDD5A87ED0A077FBF8F5FCF8F5FCF8F5FBF8F5
      D1A881CFA47BEAD5C3EAD4C2E9D4C200FF0000FF00F8DEC9F1D3BBF6E9DDECD8
      C6D7AC81DCBB9AF6ECE3F5ECE2E4C8AED2A77BE6CEBAF1E2D5DFBB9CF8DEC900
      FF0000FF0000FF00F8DEC9F3D4BBF7EADFEEDED0E3C1A7D8AE89D7AC86DDBB9C
      EBD6C7F3E6D9E4C1A3F8DEC900FF0000FF0000FF0000FF0000FF00F8DEC9F8DE
      C9F9E9DCF6E8DDF3E5DAF3E5DAF5E7DCF5E4D6EDCDB4F8DEC900FF0000FF0000
      FF0000FF0000FF0000FF0000FF00F8DEC9F8DEC9F8DEC9F6D9C1F5D7BFF5D9C3
      F8DEC9F8DEC900FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF
      0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
      FF00}
    Transparent = True
    Visible = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1028
    Height = 22
    AutoSize = True
    ButtonWidth = 26
    Caption = 'ToolBar1'
    Images = ImageList16
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = ActionNew
    end
    object ToolButton2: TToolButton
      Left = 26
      Top = 0
      Action = ActionOpen
      DropdownMenu = RecentFilesMenu
      Style = tbsDropDown
    end
    object ToolButton3: TToolButton
      Left = 67
      Top = 0
      Action = ActionSave
    end
    object ToolButton4: TToolButton
      Left = 93
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object EditByteCols: TComboBox
      Left = 101
      Top = 0
      Width = 74
      Height = 21
      Hint = 'Byte column count'
      AutoComplete = False
      TabOrder = 0
      Text = 'Auto'
      OnKeyDown = EditByteColsKeyDown
      OnSelect = EditByteColsSelect
      Items.Strings = (
        'Auto'
        '8'
        '16'
        '32'
        'Line breaks')
    end
    object ToolButton15: TToolButton
      Left = 175
      Top = 0
      Width = 8
      Caption = 'ToolButton15'
      ImageIndex = 22
      Style = tbsSeparator
      Visible = False
    end
    object ToolButton14: TToolButton
      Left = 183
      Top = 0
      Action = ActionShowPaneAddr
      Visible = False
    end
    object ToolButton16: TToolButton
      Left = 209
      Top = 0
      Action = ActionShowPaneHex
      Visible = False
    end
    object ToolButton17: TToolButton
      Left = 235
      Top = 0
      Action = ActionShowPaneText
      Visible = False
    end
    object ToolButton5: TToolButton
      Left = 261
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton6: TToolButton
      Left = 269
      Top = 0
      Action = ActionCut
    end
    object ToolButton7: TToolButton
      Left = 295
      Top = 0
      Action = ActionCopy
    end
    object ToolButton8: TToolButton
      Left = 321
      Top = 0
      Action = ActionPaste
    end
    object ToolButton9: TToolButton
      Left = 347
      Top = 0
      Action = ActionUndo
    end
    object ToolButton10: TToolButton
      Left = 373
      Top = 0
      Action = ActionRedo
    end
    object ToolButton11: TToolButton
      Left = 399
      Top = 0
      Width = 8
      Caption = 'ToolButton11'
      ImageIndex = 15
      Style = tbsSeparator
    end
    object ToolButton12: TToolButton
      Left = 407
      Top = 0
      Action = ActionFind
    end
    object ToolButton13: TToolButton
      Left = 433
      Top = 0
      Width = 8
      Caption = 'ToolButton13'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object BtnCheckUpdate: TToolButton
      Left = 441
      Top = 0
      Hint = 'New update available'
      Action = ActionCheckUpdate
      Visible = False
    end
  end
  object MDITabs: TTabControl
    Left = 0
    Top = 22
    Width = 1028
    Height = 25
    Align = alTop
    DoubleBuffered = True
    Images = ImageList16
    MultiLine = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnChange = MDITabsChange
    OnGetImageIndex = MDITabsGetImageIndex
    OnMouseMove = MDITabsMouseMove
    OnMouseUp = MDITabsMouseUp
  end
  object RightPanel: TPanel
    Left = 608
    Top = 47
    Width = 420
    Height = 592
    Align = alRight
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    OnResize = RightPanelResize
    object RightPanelPageControl: TPageControl
      Left = 0
      Top = 0
      Width = 420
      Height = 551
      ActivePage = PgValue
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = RightPanelPageControlChange
      object PgValue: TTabSheet
        Caption = 'Value'
        inline ValueFrame: TValueFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited ValuesTreeView: TVirtualStringTree
            Width = 412
            Height = 505
            ExplicitWidth = 412
            ExplicitHeight = 505
            Columns = <
              item
                Position = 0
                Text = 'Type'
                Width = 86
              end
              item
                Position = 1
                Text = 'Value'
                Width = 322
              end>
          end
        end
      end
      object PgStruct: TTabSheet
        Caption = 'Struct'
        ImageIndex = 1
        inline StructFrame: TStructFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          HelpType = htKeyword
          HelpKeyword = 'Structure-analyzer'
          Align = alClient
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited PnlButtonBar2: TPanel
            Top = 242
            Width = 412
            TabOrder = 1
            ExplicitTop = 242
            ExplicitWidth = 412
            inherited BtnInterpret: TButton
              Images = nil
            end
            inherited BtnCopyValue: TButton
              Images = nil
            end
            inherited Panel1: TPanel
              Left = 371
              ExplicitLeft = 371
              inherited BtnHelp: TButton
                Images = nil
              end
            end
          end
          inherited DSDescrEdit: TSynEdit
            Width = 412
            Height = 216
            TabOrder = 0
            ExplicitWidth = 412
            ExplicitHeight = 216
          end
          inherited DSTreeView: TVirtualStringTree
            Top = 283
            Width = 412
            Height = 222
            TabOrder = 2
            ExplicitTop = 283
            ExplicitWidth = 412
            ExplicitHeight = 222
          end
          inherited EditFieldValue: TEdit
            TabOrder = 3
          end
          inherited ToolBar1: TToolBar
            Width = 412
            Images = nil
            ExplicitWidth = 412
          end
          inherited SavedDescrsMenu: TPopupMenu
            Images = nil
          end
        end
      end
      object PgCompare: TTabSheet
        Hint = 'Compare two opened files'
        Caption = 'Compare'
        ImageIndex = 2
        inline CompareFrame: TCompareFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          DoubleBuffered = True
          ParentBackground = False
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited PageControl1: TPageControl
            Width = 412
            Height = 505
            ExplicitWidth = 412
            ExplicitHeight = 505
            inherited InitialTab: TTabSheet
              ExplicitLeft = 4
              ExplicitTop = 24
              ExplicitWidth = 489
              ExplicitHeight = 579
            end
            inherited ComparisonTab: TTabSheet
              ExplicitLeft = 4
              ExplicitTop = 24
              ExplicitWidth = 404
              ExplicitHeight = 477
              inherited DiffBar: TPaintBox
                Height = 477
                ExplicitHeight = 491
              end
              inherited BtnRecompare: TButton
                Images = nil
              end
            end
          end
          inherited CompareSelectFormPanel: TPanel
            inherited ImageProxy1: THintedImageProxy
              Image = nil
            end
            inherited CBSyncBlockSize: TComboBox
              ItemIndex = -1
              Text = ''
            end
          end
        end
      end
      object PgScript: TTabSheet
        Caption = 'Script'
        ImageIndex = 3
        inline ScriptFrame: TScriptFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited Splitter1: TSplitter
            Top = 369
            Width = 412
            ExplicitTop = 387
            ExplicitWidth = 412
          end
          inherited OutputPanel: TPanel
            Top = 373
            Width = 412
            ExplicitTop = 373
            ExplicitWidth = 412
            inherited MemoOutput: TRichEdit
              Width = 412
              ExplicitWidth = 412
            end
            inherited OutputToolPanel: TPanel
              Width = 412
              ExplicitWidth = 412
              inherited BtnClearOutput: TSpeedButton
                Left = 355
                ExplicitLeft = 355
              end
            end
          end
          inherited ScriptEdit: TSynEdit
            Width = 412
            Height = 343
            ExplicitTop = 26
            ExplicitWidth = 412
            ExplicitHeight = 343
          end
          inherited ToolBar1: TToolBar
            Width = 412
            Images = nil
            ExplicitWidth = 412
          end
          inherited SavedScriptsMenu: TPopupMenu
            Images = nil
          end
        end
      end
      object PgBitmap: TTabSheet
        Caption = 'Bitmap'
        ImageIndex = 4
        inline BitmapFrame: TBitmapFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          DoubleBuffered = False
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited MainPaintBox: TPaintBox
            Width = 314
            Height = 456
            ExplicitWidth = 299
            ExplicitHeight = 474
          end
          inherited LeftPanel: TPanel
            Height = 456
            ExplicitHeight = 456
          end
          inherited TopPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited VertScrollBar: TScrollBar64
            Left = 395
            Height = 456
            ExplicitLeft = 395
            ExplicitHeight = 456
          end
        end
      end
      object PgSearchResult: TTabSheet
        Caption = 'SearchResult'
        ImageIndex = 5
        inline SearchResultsFrame: TSearchResultsFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited PageControl: TPageControl
            Width = 412
            Height = 505
            ExplicitWidth = 412
            ExplicitHeight = 505
          end
        end
      end
      object PgHash: TTabSheet
        Caption = 'Hash'
        ImageIndex = 6
        inline HashFrame: THashFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited Splitter1: TSplitter
            Width = 412
            ExplicitWidth = 412
          end
          inherited Panel1: TPanel
            Width = 412
            ExplicitWidth = 412
            inherited BtnCalculate: TButton
              Images = nil
            end
          end
          inherited Panel2: TPanel
            Width = 412
            Height = 181
            ExplicitWidth = 412
            ExplicitHeight = 181
            inherited StaticText1: TStaticText
              Width = 406
              ExplicitWidth = 406
            end
            inherited ResultListView: TListView
              Width = 412
              Height = 158
              ExplicitWidth = 412
              ExplicitHeight = 158
            end
          end
        end
      end
      object PgAsm: TTabSheet
        Caption = 'Asm'
        ImageIndex = 7
        inline AsmFrame: TAsmFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited ToolPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited SynEdit1: TSynEdit
            Width = 412
            Height = 464
            ExplicitWidth = 412
            ExplicitHeight = 464
          end
        end
      end
      object PgBookmarks: TTabSheet
        Caption = 'Bookmarks'
        ImageIndex = 8
        inline BookmarksFrame: TBookmarksFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited ToolBar1: TToolBar
            Width = 412
            Images = nil
            ExplicitWidth = 412
          end
          inherited BookmarksTreeView: TVirtualStringTree
            Width = 412
            Height = 479
            ExplicitWidth = 412
            ExplicitHeight = 479
          end
        end
      end
      object PgRegions: TTabSheet
        Caption = 'Regions'
        ImageIndex = 9
        inline RegionsFrame: TRegionsFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited ToolPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited RegionsTreeView: TVirtualStringTree
            Width = 412
            Height = 464
            ExplicitWidth = 412
            ExplicitHeight = 464
          end
        end
      end
      object PgMedia: TTabSheet
        Caption = 'Media'
        ImageIndex = 10
        inline MediaFrame1: TMediaFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 505
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 505
          inherited ToolPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited ContentPanel: TPanel
            Width = 412
            Height = 464
            ExplicitWidth = 412
            ExplicitHeight = 464
            inherited Image1: TImage
              Width = 412
              Height = 464
              ExplicitWidth = 412
              ExplicitHeight = 460
            end
            inherited MediaPlayerPanel: TPanel
              Width = 412
              Height = 464
              ExplicitWidth = 412
              ExplicitHeight = 464
              inherited Panel1: TPanel
                Top = 423
                Width = 412
                ExplicitTop = 423
                ExplicitWidth = 412
              end
              inherited Panel2: TPanel
                Width = 412
                Height = 423
                ExplicitWidth = 412
                ExplicitHeight = 423
              end
            end
            inherited ErrorMemo: TMemo
              Width = 406
              Height = 458
              ExplicitWidth = 406
              ExplicitHeight = 458
            end
          end
        end
      end
    end
    object MsgPanel: TPanel
      Left = 0
      Top = 551
      Width = 420
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Color = 8454143
      ParentBackground = False
      TabOrder = 1
      Visible = False
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 25
        Height = 41
        Align = alLeft
        Center = True
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          0000100000000100180000000000000300000000000000000000000000000000
          0000DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC81B2C81695C51695C51695
          C51695C51695C51695C51695C51695C51695C51695C51695C51695C59DB5C6DC
          DCDCBBCED51695C592E0F07BDFEE66DFEF51E2F03BE5F324DDE913E6ED00FAFC
          00FFFF00FFFF00FFFF00FFFF1695C5C2CED4B7CDD61695C58BDBEC76DAEC64DB
          EC4FDDED35D2E3254D5437656705E9EF00FAFC00FFFF00FEFE00FEFE1695C5B2
          D1D6D5D8DA1695C596DFEE7BDAEB67DAEC55DCEC3AD4E61D4B552B626B08E8F2
          00F6FA00FCFD00FEFE00FFFF1695C5CED2D6DCDCDCB1C8D41695C583DCEB6DDA
          EB5BDBEC49DDED1FD9EB14DDEC14EBF404F0F600F7FA00FCFC1695C58EB3C6DC
          DCDCDCDCDCDCDCDC1695C581D8EB75D9EB64DAEB4CDFF1326F763A73771CEAF6
          12EBF405F0F600F5F91695C5DCDCDCDCDCDCDCDCDCDCDCDCC3D3D91695C581DB
          EB6DD9EB52D0E32D454A3B55572BDBE822E6F117E9F21695C5B6CCD5DCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDC1695C584D9EC7ADAEA43BAD2272E31373E3F2BC9DB
          34E0EE29E7F31695C5DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC1695
          C58ADBEC37A9C6201F1E312D2C28BAD048E0EE1695C5D4D5D8DCDCDCDCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDCDCDCDC1695C57AD1E85DBCD316333B22414942C5DC
          4FD6E91695C5DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDADA
          DB1695C5A0E1F076CEE26BCEE274DCEC1695C5D2D7D9DCDCDCDCDCDCDCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC1695C562C2DF9ADDED8EDBEC6ED1E6
          1695C5DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
          DCDCDCDC1695C5A3DCEDA2DFEF1695C5DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD5D8DA1695C51695C5CFD5D9
          DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
          DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
          DCDC}
        Transparent = True
      end
      object MsgTextBox: TStaticText
        AlignWithMargins = True
        Left = 28
        Top = 3
        Width = 389
        Height = 35
        Align = alClient
        Caption = 'Saving your changes requires temporary file of size X'
        TabOrder = 0
      end
    end
  end
  object SelectRangeFormPanel: TPanel
    Left = -193
    Top = 384
    Width = 256
    Height = 161
    TabOrder = 3
    Visible = False
    object LblSelRangeStart: TLabel
      Left = 24
      Top = 27
      Width = 61
      Height = 13
      Caption = 'Range start:'
    end
    object LblSelRangeEnd: TLabel
      Left = 24
      Top = 67
      Width = 56
      Height = 13
      Caption = 'Range end:'
    end
    object ImageProxy1: THintedImageProxy
      Left = 224
      Top = 48
      Width = 16
      Height = 16
      Image = HintImage
      ImageIndex = 0
      HintFmt = 
        'Use $ or 0x for hex value, + or - to select relative to current ' +
        'selection start/end<br><br>Supports script expressions'
    end
    object EditSelRangeStart: TEdit
      Left = 104
      Top = 24
      Width = 105
      Height = 21
      TabOrder = 0
    end
    object EditSelRangeEnd: TEdit
      Left = 104
      Top = 64
      Width = 105
      Height = 21
      TabOrder = 1
    end
    object BtnSelRangeOk: TButton
      Left = 40
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Select'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object BtnSelRangeCancel: TButton
      Left = 144
      Top = 120
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList16
    Left = 144
    Top = 72
    object MIFile: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = ActionNew
      end
      object Open1: TMenuItem
        Action = ActionOpen
      end
      object MIRecentFilesMenu: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Open Recent'
        OnClick = MIRecentFilesMenuClick
        object MIDummyRecentFile: TMenuItem
          Caption = 'MIDummyRecentFile'
          Visible = False
          OnClick = MIDummyRecentFileClick
        end
      end
      object Save1: TMenuItem
        Action = ActionSave
      end
      object Saveas1: TMenuItem
        Action = ActionSaveAs
      end
      object SaveAll1: TMenuItem
        Action = ActionSaveAll
      end
      object Saveselectionas1: TMenuItem
        Action = ActionSaveSelectionAs
      end
      object Revert1: TMenuItem
        Action = ActionRevert
      end
      object Close1: TMenuItem
        Action = ActionClose
      end
      object CloseAll1: TMenuItem
        Action = ActionCloseAll
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MIOpenDisk: TMenuItem
        Action = ActionOpenDisk
      end
      object OpenProcessMemory1: TMenuItem
        Action = ActionOpenProcMemory
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Settings1: TMenuItem
        Action = ActionSettings
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ActionExit
      end
    end
    object MIEdit: TMenuItem
      Caption = 'Edit'
      object MIUndo: TMenuItem
        Action = ActionUndo
      end
      object MIRedo: TMenuItem
        Action = ActionRedo
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MICut: TMenuItem
        Action = ActionCut
      end
      object MICopy: TMenuItem
        Action = ActionCopy
      end
      object MICopyAsMenu: TMenuItem
        Caption = 'Copy as'
        object MICopyAsArray: TMenuItem
          Action = ActionCopyAsArray
          Caption = 'Array (delimited text)'
        end
        object Base641: TMenuItem
          Action = ActionCopyAsBase64
        end
        object URLEncode1: TMenuItem
          Action = ActionCopyAsURLEncode
        end
        object Cstringconstant1: TMenuItem
          Action = ActionCopyAsCppString
        end
        object Delphistringconstant1: TMenuItem
          Action = ActionCopyAsDelphiString
        end
      end
      object MIPaste: TMenuItem
        Action = ActionPaste
      end
      object MIPasteAs: TMenuItem
        Action = ActionPasteAs
      end
      object MISelectAll: TMenuItem
        Action = ActionSelectAll
      end
      object MISelectRange: TMenuItem
        Action = ActionSelectRange
      end
    end
    object MISearchMenu: TMenuItem
      Caption = 'Search'
      object MIFindReplace: TMenuItem
        Action = ActionFind
      end
      object FindNext1: TMenuItem
        Action = ActionFindNext
      end
      object FindPrevious1: TMenuItem
        Action = ActionFindPrev
      end
      object FindReplaceinfiles1: TMenuItem
        Action = ActionFindInFiles
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Findalternatefilestreams1: TMenuItem
        Action = ActionFindAltStreams
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object GoToaddress1: TMenuItem
        Action = ActionGoToAddr
      end
    end
    object MIView: TMenuItem
      Caption = 'View'
      OnClick = MIViewClick
      object MIThemesMenu: TMenuItem
        Caption = 'Theme'
        object MIThemeDark: TMenuItem
          Action = ActionThemeDark
          AutoCheck = True
        end
        object MIThemeLight: TMenuItem
          Action = ActionThemeLight
          AutoCheck = True
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object MIEncodingMenu: TMenuItem
        Caption = 'Text encoding'
        OnClick = MIEncodingMenuClick
        object ANSI1: TMenuItem
          Caption = 'ANSI default (0)'
          RadioItem = True
          OnClick = ANSI1Click
        end
        object ASCII1: TMenuItem
          Tag = 1
          Caption = 'OEM default (1)'
          RadioItem = True
          OnClick = ANSI1Click
        end
      end
      object MIHighlightMatches: TMenuItem
        Caption = 'Highlight matches'
        OnClick = MIHighlightMatchesClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object MIAutoRefresh: TMenuItem
        Caption = 'Auto-refresh'
        Hint = 
          ' Refresh view every second to show changes made by other program' +
          's'
        OnClick = MIAutoRefreshClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ShowAddrpane1: TMenuItem
        Action = ActionShowPaneAddr
        AutoCheck = True
      end
      object ShowHexpane1: TMenuItem
        Action = ActionShowPaneHex
        AutoCheck = True
      end
      object ShowHexpane2: TMenuItem
        Action = ActionShowPaneText
        AutoCheck = True
      end
    end
    object Operations1: TMenuItem
      Caption = 'Operations'
      object MISetFileSize: TMenuItem
        Action = ActionSetFileSize
      end
      object MIInsertBytes: TMenuItem
        Action = ActionFillBytes
      end
      object MIModifyWithExpr: TMenuItem
        Action = ActionModifyWithExpr
      end
      object MIInvertByteOrder: TMenuItem
        Action = ActionInvertByteOrder
      end
      object Compare1: TMenuItem
        Action = ActionCompare
      end
    end
    object Filetools1: TMenuItem
      Caption = 'File utils'
      object Splitfile1: TMenuItem
        Action = ActionFileSplit
      end
      object Concatenatefiles1: TMenuItem
        Action = ActionFileConcat
      end
    end
    object MIDebug: TMenuItem
      Caption = 'Debug'
      Visible = False
      object DbgToolsForm1: TMenuItem
        Caption = 'DbgToolsForm'
        OnClick = DbgToolsForm1Click
      end
      object Regions1: TMenuItem
        Caption = 'Regions'
        OnClick = Regions1Click
      end
      object Undostack1: TMenuItem
        Caption = 'Undo stack'
        OnClick = Undostack1Click
      end
      object estchangespeed1: TMenuItem
        Caption = 'Test change speed'
        OnClick = estchangespeed1Click
      end
      object CreateTestFile1: TMenuItem
        Caption = 'Create Test File'
        OnClick = CreateTestFile1Click
      end
      object Openemulatedsource1: TMenuItem
        Caption = 'Open emulated source'
        OnClick = Openemulatedsource1Click
      end
      object Something1: TMenuItem
        Caption = 'Something'
        OnClick = Something1Click
      end
      object Loadplugin1: TMenuItem
        Caption = 'Load plugin'
        OnClick = Loadplugin1Click
      end
      object Openpath1: TMenuItem
        Caption = 'Open path...'
        OnClick = Openpath1Click
      end
    end
    object MIHelp: TMenuItem
      Caption = 'Help'
      object Help1: TMenuItem
        Action = ActionHelpContents
      end
      object AboutHextor1: TMenuItem
        Action = ActionAboutBox
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Checkforupdates1: TMenuItem
        Action = ActionCheckUpdate
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 248
    Top = 72
  end
  object ActionList1: TActionList
    Images = ImageList16
    Left = 48
    Top = 69
    object ActionUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 13
      SecondaryShortCuts.Strings = (
        'Alt+BkSp')
      ShortCut = 16474
      OnExecute = ActionUndoExecute
    end
    object ActionShowPaneText: TAction
      Tag = 2
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Text pane'
      Checked = True
      Hint = 'Show/hide Text pane'
      ImageIndex = 28
      OnExecute = ActionShowPaneAddrExecute
    end
    object ActionRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 14
      SecondaryShortCuts.Strings = (
        'Shift+Alt+BkSp')
      ShortCut = 24666
      OnExecute = ActionRedoExecute
    end
    object ActionCopyAsDelphiString: TAction
      Category = 'Edit'
      Caption = 'Delphi string constant'
      Hint = 'Copy data as Delphi string constant (escaped)'
      OnExecute = ActionCopyExecute
    end
    object ActionNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'New file'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = ActionNewExecute
    end
    object ActionOpen: TAction
      Category = 'File'
      Caption = 'Open'
      Hint = 'Open file...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = ActionOpenExecute
    end
    object ActionSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save file'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = ActionSaveExecute
    end
    object ActionCopyAsCppString: TAction
      Category = 'Edit'
      Caption = 'C++ string constant'
      Hint = 'Copy data as C++ string constant (escaped)'
      OnExecute = ActionCopyExecute
    end
    object ActionSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      Hint = 'Save file as...'
      ImageIndex = 2
      OnExecute = ActionSaveAsExecute
    end
    object ActionCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 9
      SecondaryShortCuts.Strings = (
        'Shift+Del')
      ShortCut = 16472
      OnExecute = ActionCopyExecute
    end
    object ActionCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy'
      ImageIndex = 10
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      ShortCut = 16451
      OnExecute = ActionCopyExecute
    end
    object ActionCopyAsBase64: TAction
      Category = 'Edit'
      Caption = 'Base64'
      Hint = 'Copy data as Base64'
      OnExecute = ActionCopyExecute
    end
    object ActionCopyAsURLEncode: TAction
      Category = 'Edit'
      Caption = 'URLEncode'
      Hint = 'Copy data as URL encode'
      OnExecute = ActionCopyExecute
    end
    object ActionCopyAsArray: TAction
      Category = 'Edit'
      Caption = 'Copy as array'
      Hint = 'Copy as...'
      OnExecute = ActionCopyAsArrayExecute
    end
    object ActionPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste'
      ImageIndex = 11
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      ShortCut = 16470
      OnExecute = ActionPasteExecute
    end
    object ActionPasteAs: TAction
      Category = 'Edit'
      Caption = 'Paste as...'
      Hint = 'Paste as...'
      OnExecute = ActionPasteExecute
    end
    object ActionSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select all'
      Hint = 'Select all'
      ShortCut = 16449
      OnExecute = ActionSelectAllExecute
    end
    object ActionGoToStart: TAction
      Category = 'Search'
      Caption = 'Go to start of file'
      Hint = 'Go to start of file'
      SecondaryShortCuts.Strings = (
        'Ctrl+Shift+Home')
      ShortCut = 16420
      OnExecute = ActionGoToStartExecute
    end
    object ActionGoToEnd: TAction
      Category = 'Search'
      Caption = 'Go to end of file'
      Hint = 'Go to end of file'
      SecondaryShortCuts.Strings = (
        'Ctrl+Shift+End')
      ShortCut = 16419
      OnExecute = ActionGoToEndExecute
    end
    object ActionSaveAll: TAction
      Category = 'File'
      Caption = 'Save All'
      Hint = 'Save all files'
      ImageIndex = 2
      ShortCut = 24659
      OnExecute = ActionSaveAllExecute
    end
    object ActionRevert: TAction
      Category = 'File'
      Caption = 'Revert'
      Hint = 'Revert unsaved changes'
      OnExecute = ActionRevertExecute
    end
    object ActionFind: TAction
      Category = 'Search'
      Caption = 'Find/Replace...'
      Hint = 'Find/Replace text or data'
      ImageIndex = 3
      ShortCut = 16454
      OnExecute = ActionFindExecute
    end
    object ActionFindNext: TAction
      Category = 'Search'
      Caption = 'Find Next'
      Hint = 'Find next occurrence'
      ShortCut = 114
      OnExecute = ActionFindNextExecute
    end
    object ActionFindPrev: TAction
      Category = 'Search'
      Caption = 'Find Previous'
      Hint = 'Find previous occurrence'
      ShortCut = 8306
      OnExecute = ActionFindPrevExecute
    end
    object ActionGoToAddr: TAction
      Category = 'Search'
      Caption = 'Go To address...'
      Hint = 'Go To address...'
      ShortCut = 16455
      OnExecute = ActionGoToAddrExecute
    end
    object ActionSaveSelectionAs: TAction
      Category = 'File'
      Caption = 'Save selection as...'
      Hint = 'Save selected data as...'
      OnExecute = ActionSaveSelectionAsExecute
    end
    object ActionOpenDisk: TAction
      Category = 'File'
      Caption = 'Open Disk...'
      Hint = 'Open logical volume'
      ImageIndex = 4
      OnExecute = ActionOpenDiskExecute
    end
    object ActionOpenProcMemory: TAction
      Category = 'File'
      Caption = 'Open Process Memory...'
      Hint = 'Open process memory...'
      ImageIndex = 5
      OnExecute = ActionOpenProcMemoryExecute
    end
    object ActionExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Close Hextor'
      ImageIndex = 6
      OnExecute = ActionExitExecute
    end
    object ActionBitsEditor: TAction
      Category = 'Operations'
      Caption = 'Edit Bits'
      Hint = 'Open bits editor'
      OnExecute = ActionBitsEditorExecute
    end
    object ActionCompare: TAction
      Category = 'Operations'
      Caption = 'Compare...'
      Hint = 'Compare open files...'
      OnExecute = ActionCompareExecute
    end
    object ActionSetFileSize: TAction
      Category = 'Operations'
      Caption = 'Set file size'
      Hint = 'Set file size'
      OnExecute = ActionSetFileSizeExecute
    end
    object ActionFillBytes: TAction
      Category = 'Operations'
      Caption = 'Insert bytes / Fill selection'
      Hint = 'Insert bytes / Fill selection'
      ImageIndex = 15
      OnExecute = ActionFillBytesExecute
    end
    object ActionSelectRange: TAction
      Category = 'Edit'
      Caption = 'Select Range...'
      Hint = 'Select Range...'
      OnExecute = ActionSelectRangeExecute
    end
    object ActionDebugMode: TAction
      Category = 'Operations'
      Caption = 'Switch Debug Mode'
      Hint = 'Switch Debug Mode'
      ShortCut = 24644
      Visible = False
      OnExecute = ActionDebugModeExecute
    end
    object ActionAboutBox: TAction
      Category = 'Help'
      Caption = 'About Hextor'
      Hint = 'About Hextor'
      OnExecute = ActionAboutBoxExecute
    end
    object ActionModifyWithExpr: TAction
      Category = 'Operations'
      Caption = 'Modify with expression'
      Hint = 'Modify with expression'
      ImageIndex = 16
      OnExecute = ActionModifyWithExprExecute
    end
    object ActionClose: TAction
      Category = 'File'
      Caption = 'Close'
      Hint = 'Close file'
      ImageIndex = 19
      OnExecute = ActionCloseExecute
    end
    object ActionCloseAll: TAction
      Category = 'File'
      Caption = 'Close All'
      Hint = 'Close all files'
      OnExecute = ActionCloseAllExecute
    end
    object ActionFindInFiles: TAction
      Category = 'Search'
      Caption = 'Find/Replace in files...'
      Hint = 'Find/Replace in files'
      ImageIndex = 17
      ShortCut = 24646
      OnExecute = ActionFindInFilesExecute
    end
    object ActionHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Hint = 'Help Contents'
      ImageIndex = 20
      ShortCut = 112
    end
    object ActionCheckUpdate: TAction
      Category = 'Help'
      Caption = 'Check for updates'
      Hint = 'Check for updates'
      ImageIndex = 21
      OnExecute = ActionCheckUpdateExecute
    end
    object ActionSettings: TAction
      Category = 'File'
      Caption = 'Settings'
      Hint = 'Settings'
      ImageIndex = 22
      OnExecute = ActionSettingsExecute
    end
    object ActionInvertByteOrder: TAction
      Category = 'Operations'
      Caption = 'Invert byte order'
      Hint = 'Invert byte order'
      OnExecute = ActionInvertByteOrderExecute
    end
    object ActionFileSplit: TAction
      Category = 'File utils'
      Caption = 'Split file...'
      Hint = 'Split a file into several parts'
      OnExecute = ActionFileSplitExecute
    end
    object ActionFileConcat: TAction
      Category = 'File utils'
      Caption = 'Concatenate files...'
      Hint = 'Combine several files into one'
      OnExecute = ActionFileConcatExecute
    end
    object ActionShowPaneAddr: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Address pane'
      Checked = True
      Hint = 'Show/hide Address pane'
      ImageIndex = 26
      OnExecute = ActionShowPaneAddrExecute
    end
    object ActionShowPaneHex: TAction
      Tag = 1
      Category = 'View'
      AutoCheck = True
      Caption = 'Show Hex pane'
      Checked = True
      Hint = 'Show/hide Hex pane'
      ImageIndex = 27
      OnExecute = ActionShowPaneAddrExecute
    end
    object ActionThemeDark: TAction
      Tag = 1
      Category = 'View'
      AutoCheck = True
      Caption = 'Dark'
      GroupIndex = 1
      Hint = 'Switch to Dark theme'
      OnExecute = ActionThemeDarkExecute
    end
    object ActionThemeLight: TAction
      Tag = 2
      Category = 'View'
      AutoCheck = True
      Caption = 'Light'
      Checked = True
      GroupIndex = 1
      Hint = 'Switch to Dark theme'
      OnExecute = ActionThemeDarkExecute
    end
    object ActionFindAltStreams: TAction
      Category = 'Search'
      Caption = 'Find alternate file streams'
      Hint = 'Find alternate NTFS file streams'
      OnExecute = ActionFindAltStreamsExecute
    end
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 248
    Top = 136
  end
  object ImageList16: TImageList
    Left = 364
    Top = 69
    Bitmap = {
      494C01011D004001040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000008000000001002000000000000080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000072000000720000006F0000006D0000006C00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000007400003FD8720023BC420023BC4200006E00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000075000079E997003DD6700024BD4300006F00000000
      0000000000000000000000000000000000000000000000000000C3CAEE002D24
      C8005E65E40000000000000000000000000000000000000000005E65E4002D24
      C800C3CAEE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000007700007AEA98003FD8720025BE4400007100000000
      00000000000000000000000000000000000000000000C3CAEE002D24C8002D24
      C8002D24C8005E65E4000000000000000000000000005E65E4002D24C8002D24
      C8002D24C800C3CAEE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000007B00007CEC9B0042DB750027C04600007400000000
      00000000000000000000000000000000000000000000989FE6002D24C8004E43
      EF004E43EF002D24C8005E65E400989FE6005E65E4002D24C8004E43EF004E43
      EF002D24C800989FE60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007F0000007F0000007F
      0000007E0000007D0000007D00007DED9B0044DD770028C14700007500000074
      000000710000006F0000006E0000006C000000000000000000005E65E4002D24
      C8004E43EF004E43EF002D24C8002D24C8002D24C8004E43EF004E43EF002D24
      C8005E65E4000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000008100004DE680002EC7
      4D002EC74C002EC74C002DC64C004AE37D0045DE780029C2470028C1470027C0
      460025BE440024BD430023BC4200006D00000000000000000000000000005E65
      E4002D24C8004E43EF004E43EF004E43EF004E43EF004E43EF002D24C8005E65
      E400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000083000081F1A0004FE8
      82004EE781004EE781004DE680004CE57F0047E07A0045DE780044DD770042DB
      75003FD872003DD6700023BC4200006F00000000000000000000000000000000
      00002D24C8004E43EF004E43EF004E43EF004E43EF004E43EF002D24C8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000008B000086F7A50086F7
      A50086F6A40085F5A40084F4A30083F4A2004CE57F004AE37D007DED9B007CEC
      9B007AEA980079E997003FD87200007200000000000000000000000000000000
      00002D24C8004E43EF004E43EF005E65E4004E43EF004E43EF002D24C8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000008D0000008D0000008D
      0000008B0000008900000088000084F4A3004DE680002DC64C00007D0000007B
      0000007700000075000000740000007200000000000000000000000000005E65
      E4002D24C8005E65E4005E65E4005E65E4005E65E4005E65E4002D24C8005E65
      E400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000089000085F5A4004EE781002EC74C00007D00000000
      0000000000000000000000000000000000000000000000000000738DCE004354
      C9007081EE007081EE005E65E4004354C9005E65E4007081EE007081EE004354
      C900738DCE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000008B000086F6A4004EE781002EC74C00007E00000000
      00000000000000000000000000000000000000000000989FE6004354C9007081
      EE007081EE004354C9005E65E400989FE6005E65E4004354C9007081EE007081
      EE004354C900989FE60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000008D000086F7A5004FE882002EC74D00007F00000000
      00000000000000000000000000000000000000000000C3CAEE004354C9005E65
      E4004354C900738DCE00000000000000000000000000738DCE004354C9005E65
      E4004354C900C3CAEE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000008D000086F7A50081F1A0004DE68000007F00000000
      0000000000000000000000000000000000000000000000000000C3CAEE004354
      C900738DCE000000000000000000000000000000000000000000738DCE004354
      C900C3CAEE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000008D0000008B00000083000000810000007F00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000855B6400855B6400855B6400855B64000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFE9E900C3A8A800C3ABAB00F9F7F70000000000FEFE
      FE00FDFDFD00000000000000000000000000000000000000000048C7EB0048C6
      EA0048C6EA0047C5EA0047C4E90047C4E90046C3E80046C2E80046C2E70046C1
      E70045C1E60046C1E70051C5E800000000000000000000000000000000000000
      0000EAE1D400C0A07500A16D2A0094570B0094570B00A16D2A00C0A07500EAE1
      D400000000000000000000000000000000000000000000000000B49B9D00BEAE
      AE00BDBABA00C5A6A600B18C8F00A4878D00855B64008F6D7600000000000000
      000000000000000000000000000000000000000000000000000000000000F7F4
      F400EFE9E90000000000DECECE00B6888800A0737300DCCECE00F9F7F700D2BD
      BD00C9B2B200F7F4F4000000000000000000000000000000000049C8EB008FE7
      FF008EE6FF008CE6FF008BE6FF0089E5FF0088E5FF0086E4FF0085E4FF0083E4
      FF0046C1E70083E4FF0046C1E70000000000000000000000000000000000D2BA
      9A009D631900AB630600C36E0300D37B0D00D37B0D00C36E0300AB6306009D63
      1900D2BA9A0000000000000000000000000000000000D9CACB00E1E0E000E1E0
      E000DAD4D400D3C2C200B8A2A200956C7200CC8A8A00855B6400000000000000
      0000000000000000000000000000000000000000000000000000F5F0F000CCB1
      B100AE868600C8AFAF00CDAEAE00D4ACAC00BC939300AD868600C5A2A200C89A
      9A00AE7F7F00D9C9C9000000000000000000000000000000000049C8EC0091E7
      FF008FE7FF008EE6FF008CE6FF008BE6FF0089E5FF0088E5FF0086E4FF0085E4
      FF0046C2E70085E4FF0046C2E700000000000000000000000000D2BA9A00A164
      1500C36D0100D2750000DA861C00F8E6CF00F8E6CF00DA861C00D2750000C36D
      0100A1641500D2BA9A000000000000000000DAD4D400ECEAEA0000000000ECEA
      EA00E1E0E000D3C2C200CC8A8A00855B6400B18C8F00E2CDCB00000000000000
      0000BDBABA000000000000000000000000000000000000000000E2D4D400CFB1
      B100C29A9A00BA8B8B00D6AAAA00DFBCBC00D4B1B100BD919100CA9C9C00DCB4
      B400BE959500E2D5D5000000000000000000000000000000000049C9EC0092E7
      FF0091E7FF008FE7FF008EE6FF008CE6FF008BE6FF0089E5FF0088E5FF0086E4
      FF0046C2E80086E4FF0046C2E8000000000000000000EAE1D4009D631900C26D
      0100D5760000D7780000DB861B00F7E4CC00F7E4CC00DB861B00D7780000D576
      0000C26D01009D631900EAE1D4000000000000000000ECEAEA00EBE4E3000000
      0000E1E0E000D4B6B600956C7200855B6400E8C5C20000000000DE4C2D00DE4C
      2D0000000000B49B9D0000000000000000000000000000000000E4D7D700CFB0
      B000E2C7C700DBBCBC00E1C3C300E0C5C500E0C4C400DDC0C000DBBABA00D8B4
      B400BB949400D9CBCB00F0E9E900FEFEFE00000000000000000049C9ED0094E8
      FF0092E7FF0091E7FF008FE7FF008EE6FF008CE6FF008BE6FF0089E5FF0088E5
      FF0046C3E80088E5FF0046C3E8000000000000000000C1A07400AC630600D275
      0000D7780000D7780000D87A0500DC8C2500DD8E2900D87B0600D7780000D778
      0000D2750000AC630600C1A074000000000000000000DAD4D400DE9A9100DCB1
      B100D0A5A500CD909000855B6400EFA08B0000000000EE4E2000F2501F00F250
      1F00DE4C2D0000000000BDBABA000000000000000000F3ECEC00E2D2D200C2A1
      A100DEC3C300E2C8C800E2C8C800E1C6C600E0C3C300DFC2C200DFC2C200DBBC
      BC00BB8E8E00C0949400C59C9C00DED1D10000000000000000004ACAED0096E8
      FF0094E8FF0092E7FF0091E7FF008FE7FF008EE6FF008CE6FF008BE6FF0089E5
      FF0047C4E90081E1FC0047C4E9000000000000000000A16D2B00C26E0200D577
      0000D7780000D7780000D97F0D00E8B06900EEC59300DB851800D7780000D778
      0000D5770000C26E0200A16D2B00000000000000000000000000DA945F00FFB3
      3500FCD39000DE9A9100EBE4E30000000000DE4C2D00EE4E2000F2501F00F250
      1F00EE4E2000DE4C2D00ECEAEA00BEAEAE00FBFBFB00D8BCBC00CCA0A000C8A3
      A300E2C8C800E4CBCB00E3CBCB00DDC0C000E4C7C700E2C2C200DEC0C000DFC2
      C200D6B3B300D7B0B000C79C9C00CDB7B70000000000000000004ACBEE0097E8
      FF0096E8FF0094E8FF0092E7FF0091E7FF008FE7FF008EE6FF008CE6FF008BE6
      FF0058CCEF004FC8EC0074D3EF00000000000000000094580B00D1740000D778
      0000D7780000D7780000D97E0C00E7AF6700F5DCBE00E3A14C00D97D0A00D778
      0000D7780000D174000094580B000000000000000000DBC9C700F3B46400FFB3
      3500FFB33500FFD3740000000000EE4E2000DE4C2D00F75C2300F75C2300EE4E
      2000EE4E2000DE4C2D00DE4C2D00E1E0E000F6F1F100D4BBBB00E9D3D300E7D2
      D200E3CBCB00E8D3D300D4BABA00E8DADA00FCF9F900F3E2E200E1C1C100DFC2
      C200DEBEBE00D5B1B100C39F9F00DBCBCB0000000000000000004ACBEE0099E9
      FF0097E8FF0096E8FF0094E8FF0092E7FF0091E7FF008FE7FF008EE6FF008CE6
      FF0087E4FE0047C4E900F3FBFE00000000000000000094580B00D1740000D778
      0000D7780000D7780000D7790200DB871D00EDC38D00F5DBB900E0953600D87A
      0400D7780000D174000094580B000000000000000000D3C2C200EDAC4E00FFBF
      4D00FCC05500FFD3740000000000E8DDDE00EBE4E300EE4E2000F75C2300F250
      1F00DE4C2D00E8DDDE00ECEAEA0000000000F6F2F200DAC5C500DEC8C800E9D7
      D700E4CBCB00E9D7D700C8AAAA00EADFDF00FDFDFD00EEE1E100DFC2C200E0C3
      C300DDBFBF00BC989800CFBCBC00FDFDFD0000000000000000004BCCEF009AE9
      FF0099E9FF0097E8FF0096E8FF0094E8FF0092E7FF0091E7FF008FE7FF008EE6
      FF008CE6FF0047C5EA000000000000000000000000009F6B2900C26E0200D577
      0000D7780000D7790100D87C0700D87B0500DE8E2900F3D6B200E9B57200D97E
      0B00D5770000C26E0200A16D2B000000000000000000CAA3A300FCC05500FFC8
      5E00FFCD6800FFCC6600FFD87D00FCD3900000000000DE4C2D00FF772000F75C
      2300EE4E200000000000000000000000000000000000F7F4F400E4D3D300DFC8
      C800E7D0D000EBD9D900CBADAD00BF9A9A00D1B2B200DCC0C000E2C9C900E0C5
      C500DDBEBE00B88E8E00BC9A9A00F7F3F30000000000000000004BCDEF009CEA
      FF009AE9FF0099E9FF0097E8FF0096E8FF0094E8FF0092E7FF0091E7FF008FE7
      FF008EE6FF0048C6EA00000000000000000000000000BB976900AC630500D275
      0000D7780000D87C0800E1983D00DF8F2B00E1973C00F4D8B400E9B67400D97E
      0B00D2750000AC630600C1A074000000000000000000E8C5C200FFCD6800FFD6
      7900FFD87D00FFD87D00FFD87D00FFD6790000000000DE4C2D00FF772000F75C
      2300EF53260000000000000000000000000000000000FDFCFC00E5D0D000E5CD
      CD00EBD9D900E7D2D200EDDDDD00E2CACA00E5CACA00E9D2D200E4CBCB00E0C5
      C500DFC2C200DABABA00C09D9D00F2EDED0000000000000000004BCDEF009DEA
      FF009CEAFF009AE9FF0099E9FF0097E8FF0096E8FF0094E8FF0092E7FF0091E7
      FF008FE7FF0048C6EA00000000000000000000000000EAE0D3009D631900C26D
      0100D5760000D87D0900E5A85A00F5DFC100F8E7D100F6DFC200E1993F00D579
      0500C26D01009D641900EAE1D40000000000DFCCCA00DBC79E00FFD87D00FFDD
      8900FFE79C00FFEAA700FEDC9F00FEE2940000000000DE4C2D00E3583800DE4C
      2D00DE4C2D0000000000000000000000000000000000FBF8F800E2CCCC00F3E9
      E900EEE1E100E8D4D400E7D2D200E7D2D200E7D1D100E4CCCC00E3C8C800D3B3
      B300D3B7B700CAAAAA00D6C1C100FDFDFD0000000000000000004CCEF0009FEA
      FF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8FF0094E8FF0092E7
      FF0091E7FF0048C7EB0000000000000000000000000000000000D2BA9A00A164
      1500C36D0100D2760100D77B0800DA821200DA821400D8801100D3780500C36D
      0100A1641500D2BA9A000000000000000000DAD4D400FCD39000FFDD8900FFEA
      A700FFEEAB00FFF1B100FFEEAB00FFEAA7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EADBDB00E7D1
      D100E5D0D000E5D0D000EBDADA00EAD6D600E3CBCB00E3CBCB00E5CECE00D0B2
      B200F0E7E700EFE8E800F9F6F6000000000000000000000000004CCFF000A0EB
      FF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8FF0094E8
      FF0092E7FF0049C8EB000000000000000000000000000000000000000000D2BA
      9A009D631900AB630600C26E0200D1740000D1740000C26E0200AB6205009D62
      1900D2BA9A00000000000000000000000000D9CACB00AC9D9000C7B19100DBC7
      9E00FDFBC700FDFBC700FDFBC700FFEEAB00FFE79C00956C7200000000000000
      000000000000000000000000000000000000000000000000000000000000FBF8
      F800FDFDFD00EBDBDB00EADADA00E1CACA00E0C9C900DDC5C500E1C8C800D0B1
      B100F5F0F00000000000000000000000000000000000000000004CCFF100A2EB
      FF00A0EBFF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8
      FF0094E8FF0049C8EC0000000000000000000000000000000000000000000000
      0000EAE1D400C0A07500A16D2A0094570B0094570B00A06B2900BB976900EAE0
      D20000000000000000000000000000000000DAD4D400DAD4D400C9C5C500DBC9
      C700C4ADAD00C5A6A600CFB09D00DBC79E00DDBA8E00A4878D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F0E5E500E2CCCC00E7D4D400FCFAFA00EFE5E500E1CECE00E7D9
      D9000000000000000000000000000000000000000000000000004CD0F100A3EB
      FF00A2EBFF00A0EBFF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8
      FF0096E8FF0049C9EC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEDDDD00DBC9C700D3C2C200BF99990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FAF6F600FCFAFA000000000000000000FBFAFA000000
      00000000000000000000000000000000000000000000000000004DD1F2004CD0
      F1004CCFF1004CCFF0004CCEF0004BCDEF004BCDEF004BCCEF004ACBEE004ACB
      EE004ACAED006AD2F00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000048C7EB0048C6
      EA0048C6EA0047C5EA0047C4E90047C4E90046C3E80046C2E80046C2E70046C1
      E70045C1E60046C1E70051C5E800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049C8EB008FE7
      FF008EE6FF008CE6FF008BE6FF0089E5FF0088E5FF0086E4FF00C0CBD2004A67
      8B006177900083E4FF0046C1E70000000000000000000000000000000000FEFE
      FE00E5E5E500B7B7B8009A9C9C00898B8D00888A8C009A9B9C00B7B7B800E4E4
      E400FDFDFD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049C8EC0091E7
      FF008FE7FF008EE6FF008CE6FF008BE6FF0089E5FF00C1DCE8004D698A002E53
      7B005E758D0085E4FF0046C2E700000000000000000000000000FCFCFC00C8C8
      C90095969700A9A3A100E0D4D000E1D2CE00E0D3CE00DDD4D100ADA2A1009495
      9600C6C6C600FCFCFC0000000000000000000000000000000000BFDDE0002D59
      5E0052A1A900000000000000000000000000000000000000000052A1A9002D59
      5E00BFDDE0000000000000000000000000000000000000000000FFB666000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049C9EC0092E7
      FF0091E7FF008FE7FF008EE6FF008CE6FF00C5E2EE0059718B003A5E85004D64
      7C00A8C1D20086E4FF0046C2E800000000000000000000000000C8C8C9009695
      9600D2C3BF00E4C0B500C05E40009B4329009A432900A3523A00E3BDB400D5C2
      BE0096969600C7C7C700000000000000000000000000BFDDE0002D595E002D59
      5E002D595E0052A1A90000000000000000000000000052A1A9002D595E002D59
      5E002D595E00BFDDE0000000000000000000B6B6B6006666B60066666600FFFF
      B6000000000000000000DBDBFF00DBDBDB00FFFFDB0000000000000000000000
      000000000000000000000000000000000000000000000000000049C9ED0094E8
      FF00C4CDCE00A9A8A500ABACAA00B0B1B1006C7C8E0072859B004C637E00BED8
      E50046C3E80088E5FF0046C3E8000000000000000000E5E5E50094959600D1C3
      BF00C16E5200B1371100C98C7800DDBCB400DDBCB40095381D00C7432200D06A
      5200D4C2BE0093959500E4E4E40000000000000000008DC2C8002D595E004282
      8900428289002D595E0052A1A9008DC2C80052A1A9002D595E00428289004282
      89002D595E008DC2C80000000000000000000000000066B6FF0000000000FFB6
      66000000000090DBFF003A3A6600FFB690000000000000000000000000000000
      000000000000DBDBFF00DBDBDB00FFFFDB0000000000000000004ACAED00C6CF
      CF00B9B1A400E5E3DA00F8F7F100E7E4DF00B5B0AD006E829800C7E2EE0089E5
      FF0047C4E90081E1FC0047C4E9000000000000000000B7B8B800A9A3A100E4C4
      B800B0431E00AF381100D7AA9B00F9F9F900FCFCFC0095381D00C0412000BB41
      1F00E5C0B600A9A2A000B6B6B70000000000000000000000000052A1A9002D59
      5E0042828900428289002D595E002D595E002D595E0042828900428289002D59
      5E0052A1A9000000000000000000000000000000000090DBFF0000003A00DB90
      3A00000000009090DB0090909000B6FFB6000000660000000000000000000000
      000000000000FFB66600B6B6B600FFB6B60000000000000000004ACBEE00B6AF
      A600E9DFCE00FCF6EB00FCF4EB00FCF7ED00E9DDD500B0ADAB008CE6FF008BE6
      FF0058CCEF004FC8EC0074D3EF0000000000000000009A9B9D00E0D8D400C466
      4300AE441F00AC381100D7AA9B00E8E8E800FCFCFC0095381D00B6401D00B23F
      1D00B74F2E00DFD5D2009A9C9D000000000000000000000000000000000052A1
      A9002D595E0042828900428289004282890042828900428289002D595E0052A1
      A900000000000000000000000000000000000000000090DBFF0000003A00DB90
      3A00000000009090B60090909000FFFFDB00000000003A90DB00000000006600
      0000FFFFB60000000000B6B6B600DBB6B60000000000000000004ACBEE00B8AE
      A300FAEAD600FDEFE200FDF0E200FCF2E500F9EBDD00B3ADAB008EE6FF008CE6
      FF0087E4FE0047C4E900F3FBFE00000000000000000087898B00E9DCD700CA63
      3B00C0583100AC3B1200D7AA9B00E8E8E800FBFCFC0095381D00B0401B00AD3F
      1B00B0492600E6D8D300888B8D00000000000000000000000000000000000000
      00002D595E0042828900428289004282890042828900428289002D595E000000
      0000000000000000000000000000000000000000000090DBFF0000003A00B666
      000000000000B6B6DB00B6B6B600FFFFDB000000000066B6FF00000000006600
      0000FFFFB6000000000090909000B690900000000000000000004BCCEF00BFB2
      A200ECD9C000FDEDDA00FDEAD600FDEEDE00ECDCC900B6B0AB008FE7FF008EE6
      FF008CE6FF0047C5EA0000000000000000000000000087898B00EADCD800D06A
      4200D16B4400C75A3000D7AA9B00E8E8E800FDFCFC00A7472500AC401A00A940
      1A00B0502D00E6D8D300888B8D00000000000000000000000000000000000000
      00002D595E00428289004282890052A1A90042828900428289002D595E000000
      000000000000000000000000000000000000DBFFFF00003A9000000000000000
      000066000000B6B6B600B6B6B600FFFFB6003A90DB0000000000000000000000
      000000000000DB903A0090909000DBB6900000000000000000004BCDEF00CDCF
      CB00C4B29C00EEE2D000FDEDDB00EFE1CF00C7B8AB00CAD4D70091E7FF008FE7
      FF008EE6FF0048C6EA000000000000000000000000009A9B9C00E2D9D600D97F
      5900D7724A00D6704800D7795500CD613800C05A3600C05A3600AC401A00B54E
      2800BB5A3800DFD6D2009A9C9C000000000000000000000000000000000052A1
      A9002D595E0052A1A90052A1A90052A1A90052A1A90052A1A9002D595E0052A1
      A9000000000000000000000000000000000000000000B6FFFF0066666600B666
      660000000000DBFFFF00B6B6B600DBB6B6000000000000000000000000000000
      0000000000009090DB0066666600FFFFB60000000000000000004BCDEF009DEA
      FF00D0D3D100BFB3A800B0AAA400B7B2AE00CCD4D70094E8FF0092E7FF0091E7
      FF008FE7FF0048C6EA00000000000000000000000000B7B7B800A9A3A100EED2
      C700DF7A5000DC744B00F9A48200FFFFFF00FFFFFF00EC937100C0614100C560
      3800E7C9BD00A9A2A000B7B6B70000000000000000000000000072B4BB004587
      8E0071B3BA0071B3BA0052A1A90045878E0052A1A90071B3BA0071B3BA004587
      8E0072B4BB0000000000000000000000000000000000B6FFFF0066666600B666
      6600000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CCEF0009FEA
      FF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8FF0094E8FF0092E7
      FF0091E7FF0048C7EB00000000000000000000000000E6E6E60093929400D5C8
      C500E69E8200E87B4F00EFB19800FFFFFF00FFFFFF00EC937100C0614100D589
      6C00D4C5C10093949400E5E5E50000000000000000008DC2C80045878E0071B3
      BA0071B3BA0045878E0052A1A9008DC2C80052A1A90045878E0071B3BA0071B3
      BA0045878E008DC2C800000000000000000000000000000000006666B6006666
      6600FFFFB6000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CCFF000A0EB
      FF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8FF0094E8
      FF0092E7FF0049C8EB0000000000000000000000000000000000C9C9CA009594
      9500D4C8C500F2D5CA00F0936D00F8B79C00F8B79B00E4825B00EBCEC200D4C6
      C20095959500C8C8C900000000000000000000000000BFDDE00045878E0052A1
      A90045878E0072B4BB0000000000000000000000000072B4BB0045878E0052A1
      A90045878E00BFDDE00000000000000000000000000000000000B6FFFF000000
      66000000000000000000FFB66600000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CCFF100A2EB
      FF00A0EBFF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8FF0096E8
      FF0094E8FF0049C8EC0000000000000000000000000000000000FCFCFC00C9C9
      CA0094949500A8A3A200E3DAD500ECDCD600ECDCD500E2D8D300A9A3A2009495
      9600C8C8C800FCFCFC0000000000000000000000000000000000BFDDE0004587
      8E0072B4BB00000000000000000000000000000000000000000072B4BB004587
      8E00BFDDE0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CD0F100A3EB
      FF00A2EBFF00A0EBFF009FEAFF009DEAFF009CEAFF009AE9FF0099E9FF0097E8
      FF0096E8FF0049C9EC000000000000000000000000000000000000000000FEFE
      FE00E6E6E600B8B8B800999A9C0085878A0085888A009A9A9D00B7B7B800E5E5
      E500FEFEFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004DD1F2004CD0
      F1004CCFF1004CCFF0004CCEF0004BCDEF004BCDEF004BCCEF004ACBEE004ACB
      EE004ACAED006AD2F00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCF0D8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCF0D80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EADFD400C79E7000EADFD4000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFCFC00FEFCFC000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00F4C36500EFAF3900F7D3
      8B00000000000000000000000000000000000000000000000000000000000000
      0000F7D38B00EFAF3900F3C26200FEFEFE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EADFD400C79E7000DAA26D00C79E7000EADF
      D400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4DBC3001B7815001B781500FDFC
      FC00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F5C76E00E7A62E00D28A1800EDAF
      3800F6D59400000000000000000000000000000000000000000000000000F8D6
      9300EFAF3800D48C1900E8A72F00F5C76E000000000000000000000000000000
      00000000000000000000000000000000000086A2E80000000000000000000000
      00000000000000000000EADFD400C79E7000DCA67300DFAE8100E2BB9200C79E
      7000EADFD4000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFCFB001B7A18008CBC
      8A00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F3BA4D00E7A32700D58E1A00D28B
      1900E9AC3800FBF2E00000000000000000000000000000000000FDF3E000EFB1
      3700D48E1A00D68F1B00E7A32700F3BA4D000000000000000000000000000000
      000000000000000000000000000000000000164EDC0086A2E800000000000000
      000000000000EADFD400C79E7000DCA87600E0B38600E3BF9900E7CBAC00ECD8
      BD00C79E7000EADFD40000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008CBD8D001B7D
      1D00C4DDC4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEF9EF00F2B54000DD972000D48D
      1A00D18B1900E3AA3B0000000000000000000000000000000000F2B33900D58E
      1B00D7901B00DE992000F3B64000FEF9EF000000000000000000000000000000
      000000000000000000000000000000000000164EDC00164EDC00000000000000
      0000EADFD400C79E7000DDAC7B00E0B68C00E4C39E00E9CFB200ECDAC300EDE3
      D100FAF9F300C79E7000EADFD400000000000000000000000000000000000000
      0000FDFBFB00000000000000000000000000000000000000000099C59C001B80
      22008CBE8F000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDF8ED00ECAC3200DA94
      1E00D48D1A00DA9D3200EBCE95000000000000000000F8D69300E8A73100D790
      1B00DE972000F3B13100FDF8ED00000000000000000000000000000000000000
      000000000000000000000000000000000000164EDC00164EDC0000000000EADF
      D400C79E7000DFB08100E2BA9200E6C7A400E9D4B600EDDFC900EEE6D500FAF9
      F300FFFFFF00FAF9F300C79E7000EADFD400000000000000000000000000FDFB
      FB001B832700FDFBFB000000000000000000FDFBFB00FDFBFB007AB681001B83
      27001B832700FDFBFB00FDFBFB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8E5C000E3A8
      3900D9921C00D5932100DFB668000000000000000000F4C36500E09B2300DC96
      1E00EFAF3800FBE7C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001C57F000164EDC00EADFD400C79E
      7000E0B28600E3BE9800E7CBAC00EAD6BD00DFD2BF00E2DCCE00ECEAE400FFFF
      FF00FFFFFF00FAF9F300EDDDC700C79E70000000000000000000FDFBFB001C87
      2E001C872E001C872E00FDFBFB00FDFBFB001C872E001C872E001C872E001C87
      2E001C872E001C872E001C872E00FDFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCF8EE00E0A7
      3700DE971F00D7932000D4A549000000000000000000F2B84600E09B2100E19B
      2100F2B23500FDF8EE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002763FF00164EDC00164EDC00E0B7
      8C00E4C39F00E9CFB100ECDAC200C3BEB700ADACAB00D5D4D200EEEEEE00FFFF
      FF00FAF9F300EDDDC700E7CDAE00D0B1880000000000FDFCFC00248F3700248F
      3700248F3700248F3700248F3700FDFCFC00FDFCFC00248F3700248F3700248F
      3700248F3700248F3700FDFCFC00000000000000000000000000D4AB5900CD9F
      4600CD9F4600CC9F4500CC9E4500CA9E4500CAA14E0000000000FCF9F100DBA4
      3800E39D2200DB982200CFA147000000000000000000F3B74400E49F2300E6A1
      2400F2B33500FEF9F10000000000CAA14E00C89C4600C69C4600C59B4600C49A
      4700C2994700C8A45B0000000000000000007CA0FC002763FF00164EDC00E7C9
      A400EAD4B700ECDDC700EEE6D500ADACAB00F3F3F000C1C0BE00FFFFFF00F9F6
      F100ECDDC700E9CEB000D0B18800EDE4DA00FDFCFC00329A4500329A4500329A
      4500329A4500329A4500329A4500329A4500FDFCFC00FDFCFC00329A4500329A
      4500329A4500FDFCFC0000000000000000000000000000000000F3B43A00FBBE
      4100FBC04600FABE4100F9BC3900EFB43C00EEC87C0000000000EBCD9100DEA6
      3A00E8A22500DDA03200D2AB5E000000000000000000F4BF5B00EBAA3200EBA6
      2700F0B13700F5D2900000000000EEC87C00E9B03C00F3B73800F2BA3F00F0B9
      4500EEB84800C99C40000000000000000000000000005383FF002763FF00164E
      DC00ECE0CB00EEE7D800EEE7D800EAE9E70091918F00D5D4D200FAF9F300ECDC
      C600E9CEB000D0B18800EDE4DA000000000000000000FDFCFC00FDFCFC0040A4
      520040A452008FCA9A00FDFCFC00FDFCFC000000000000000000FDFCFC0040A4
      5200FDFCFC000000000000000000000000000000000000000000F3B43A00FDBE
      3A00FEC44A00FEC64F00F1B74000EFC97D00F8EDD600EBCC8E00D9A53D00F0AF
      3300EEB24000DBAD5700DEC696000000000000000000F8D69300F3BD5500F1B4
      4100F3B23300EBB03B00F1CE8D00F8EDD600E9C57D00E5B04000FEC64C00FEC6
      5000F9BF4400C6994000000000000000000000000000000000007CA0FC002763
      FF00E1BF9300EEE7D800FAF9F300FFFFFF0099989600FAF9F300EDDDC700E7CE
      AE00D0B18800EDE4DA0000000000000000000000000000000000FEFEFE00A2D5
      AB0049AB5A00AEDAB6000000000000000000000000000000000000000000FDFD
      FC00FEFEFE000000000000000000000000000000000000000000F4B94600FDC1
      4400FEC14300FEC54E00FEC75100EBB44500E2AD4400E6B14500F8BD4800F7BE
      4F00F0BA5100C99F4E00FEFEFE000000000000000000FEFEFE00F1B94A00F5BE
      5100F8C05000FABE4800EBB54400E2AD4400E4B04500FEC74F00FEC75300FEC4
      4C00F8BF4700C49D4C0000000000000000000000000000000000000000000000
      0000F1E7DC00E1BF9300FFFFFF00FFFFFF00A1A09F00EDDDC700E9CEB000D0B1
      8800EDE4DA00000000000000000000000000000000000000000000000000D1EA
      D60051B16100A6D8AF00FDFDFD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F3BC4F00FDC9
      5E00F8C35600FECA5C00FECE6700FECE6800FECD5F00FEC95300FCC34D00F6BD
      4C00CCA14A00F5EFE30000000000000000000000000000000000FCF3E200ECB4
      4600F9C14C00FDC44E00FECA5500FECD5F00FECE6700FECF6A00FDCB6200ECBC
      5800F9C86000C49F550000000000000000000000000000000000000000000000
      000000000000F1E7DC00E1BF9300FAF9F300A8A7A600E9CEB000D0B18800EDE4
      DA0000000000000000000000000000000000000000000000000000000000FCFC
      FC00AADAB30058B56700FDFDFD00FDFDFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F1BA5000F3BF
      5800EEC36D00E3AC4000ECBA5500F0BD5700F1C05700ECBA5000E0B04C00C89B
      4400F5EFE300000000000000000000000000000000000000000000000000FBF3
      E200E4AE4200EBB74C00F0BC5000F1C05700ECBC5900E1B35700C69A4400D3B2
      7000D9AF5B00C29E550000000000000000000000000000000000000000000000
      00000000000000000000F1E7DC00E1BF9300ADACAB00D0B18800999896000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFD005CB96B005CB96B00D4EDD9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EEBC5600F0C7
      79000000000000000000EED8AB00E5C48400DBB56900DDBD7E00E5D1AA00FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00EED7A900E4C27D00DBB56900DFC08400E5D2AD00000000000000
      0000D2B57D00C3A15B0000000000000000000000000000000000000000000000
      0000000000000000000000000000F1E7DC00ADACAB00D3D2D000A1A09F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FAFBFB00FDFDFD00FDFDFD00FAFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E6E5E300A1A09F00E6E5E3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001587210000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000092929100929291009292910092929100929291009292
      9100929291009292910092929100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000158721001EB73D0015872100000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E6C2
      8900C67D0D00C0770D00E0BE920000000000EACB9A00C7801100C1760A00DDB7
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000092929100FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00929291000000000000000000000000006E8EAC003562
      8B00335F8900315D87002F5A85002E5883002C5681002A547F0029527D002750
      7C00264E7A005A77990000000000000000000000000000000000000000001587
      210028C14E0023BC46001EB73D00158721000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D692
      1C00F0DAB900E6C69800C178100000000000D6921D00F2E2C700EDD8B700C077
      0F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000092929100FFFFFF00D6B28A00D5AB7F00D3A26D00D199
      5C00D0925100FFFFFF0092929100000000000000000000000000446F95005189
      B300528AB3005189B1005087AF005086AE004F84AC004E83AA004D82A9004D81
      A7004C80A600264E7A0000000000000000000000000000000000000000000000
      00001587210028C14E0023BC46001FB83E001587210000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DE9A
      1B00FAF4EA0000000000C67D0D00F6EAD700DC971A0000000000F9F3EA00C47A
      0C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000088888800FFFFFF00F9F9FA00F8F9F900F7F8F900F7F8
      F800F6F8F800FFFFFF009292910000000000000000000000000045719700528B
      B500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004D81A700274F7C0000000000000000000000000000000000000000000000
      0000000000001587210028C14E0023BC460020B9400015872100000000000000
      000000000000000000000000000000000000000000000000000000000000E8AE
      3B00E8BD6C0000000000CF891800ECCD9700E2A1240000000000E0B26400D192
      2E0000000000000000000000000000000000A7A7A700A7A7A700A7A7A700A7A7
      A700A7A7A700B0B0B0007F7F7F00FFFFFF00D6B28A00D5AC7E00D3A26D00D198
      5D00D0925100FFFFFF009292910000000000000000000000000047739900538C
      B700E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6
      C8004D82A90028517D0000000000000000000000000000000000000000000000
      000000000000000000001587210028C14E0023BC460020B94000158721000000
      000000000000000000000000000000000000000000000000000000000000FBF5
      EB00E6A62A00ECC88300D99A2C00E7BA6900E8AD3A00ECC78000DB982400F9F3
      EA0000000000000000000000000000000000A7A7A700FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007F7F7F00FFFFFF00FCFBFB00FAFBFA00F9FAFA00F9FA
      F900F8F9F900FFFFFF009292910000000000000000000000000049769B00548E
      B900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004E83AA002A537F0000000000000000000000000000000000000000000000
      00000000000000000000000000001587210028C14E0023BC460020B940001587
      2100000000000000000000000000000000000000000000000000000000000000
      0000F8EBCF00E5A32300DE9B1F00E6AE4800EAA92500E5A32300F7EAD3000000
      000000000000000000000000000000000000A7A7A700FFFFFF00EBDFD200E9D8
      C500E7D0B700E6C9AA007F7F7F00FFFFFF00D6B28A00D4AB7E00D3A26D00D199
      5D00D0925100FFFFFF00929291000000000000000000000000004B789D00558F
      BA00E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6
      C8004F84AC002B55800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001587210028C14E0023BC460020B9
      4000158721000000000000000000000000000000000000000000000000000000
      000000000000F4D79D00E5A21F00E5A93400EAA82200F4D8A000000000000000
      000000000000000000000000000000000000A7A7A700FFFFFF00FDFDFD00FCFD
      FC00FCFCFC00FBFCFB007F7F7F00FFFFFF00FDFDFD00FDFDFD00FCFCFC00FFFF
      FF00EDEEED00EDEEED00929291000000000000000000000000004C7A9F005690
      BC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF005086AE002D57820000000000000000000000000000000000000000000000
      000000000000000000000000000000000000158721002BC4570028C14E0023BC
      4600158721000000000000000000000000000000000000000000000000000000
      00000000000000000000AA9A8F00BEAFA600C9BCB50000000000000000000000
      000000000000000000000000000000000000A7A7A700FFFFFF00EBE0D200EAD8
      C600E8D0B800E6C9AB007F7F7F00FFFFFF00D6B28A00D4AB7E00FDFDFC008888
      88009292910092929100929291000000000000000000000000004E7CA0005792
      BE00E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6
      C8005087AF002F59840000000000000000000000000000000000000000000000
      00000000000000000000000000001587210033CC66002FC85F002BC457001587
      2100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C3B5AD00BCAEA400B0A0970000000000000000000000
      000000000000000000000000000000000000A7A7A700FFFFFF00FDFEFE00FDFD
      FE00FCFDFC00FCFCFC007F7F7F00FFFFFF00FEFEFE00FEFDFE00FDFDFD007F7F
      7F00FFFFFF0092929100C2C1C1000000000000000000000000004F7EA1005893
      BF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF005189B100305B860000000000000000000000000000000000000000000000
      000000000000000000001587210038D16B0035CE680033CC6600158721000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EBE7E400B8A99F00B3A49A008B776900E7E3DF00000000000000
      000000000000000000000000000000000000A7A7A700FFFFFF00ECE0D300EADA
      C600E8D1B800E7CAAC007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      7F007F7F7F00C2C1C10000000000000000000000000000000000507FA3005894
      C100E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6C800E0D6
      C800528AB300325E880000000000000000000000000000000000000000000000
      0000000000001587210040D973003CD56F0038D16B0015872100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C7BDB700B5A59C00D6CDC8008B776900B1A49B00000000000000
      000000000000000000000000000000000000A7A7A700FFFFFF00FFFFFF00FEFF
      FE00FEFEFE00FFFFFF007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F
      7F00BABAB90000000000000000000000000000000000000000005181A4005995
      C200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00538BB50034608A0000000000000000000000000000000000000000000000
      00001587210047E07A0044DD770040D973001587210000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F1EEEC00AEA09600C6BBB40000000000A39287008C786A00EFECE9000000
      000000000000000000000000000000000000A7A7A700FFFFFF00ECE1D300EBDA
      C600FEFEFF00BBBBBB00BABABA00B9B9B900B8B8B80000000000000000000000
      00000000000000000000000000000000000000000000000000005282A6005996
      C300FDFFFE005D5E5C005F5D5C005F5D5C005D5D5D005B5D5D005B5D5E00F7F4
      F000548DB70035628C0000000000000000000000000000000000000000001587
      210050E983004CE57F0047E07A00158721000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CDC4BD00AB9C9200EBE8E50000000000DED8D3008B776900C1B7AF000000
      000000000000000000000000000000000000A7A7A700FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A7A7A700FFFFFF00BABABA00E3E2E10000000000000000000000
      0000000000000000000000000000000000000000000000000000739AB600487B
      A10047799F00AFB0AE00ACAEAF00DDDFDF00DFDFDF00B0AEAE00B6B4B3003B6A
      9200396790005F82A30000000000000000000000000000000000000000000000
      00001587210050E9830015872100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B1A39A00C0B4AD00000000000000000000000000A39287009E8D80000000
      000000000000000000000000000000000000A7A7A700FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A7A7A700BCBDBD00E3E2E1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006662610062626200605C5B005B5D5D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001587210000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B8AAA200EAE6E300000000000000000000000000DED8D300A49489000000
      000000000000000000000000000000000000A7A7A700A7A7A700A7A7A700A7A7
      A700A7A7A700A7A7A700E3E2E100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000091908F008F8F
      8D008E8D8C008D8C8B008B8A89008A8988008888860087868500868583008484
      82008382800082817F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097928F0097928F0097928F009792
      8F0097928F0097928F0097928F0097928F0097928F0097928F0097928F009792
      8F0097928F0097928F0097928F0097928F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000092929000FBFB
      FA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFB
      FA00FBFBFA008382800000000000000000000000000000000000000000000000
      0000000000000000000000000000F6F6F600EEEEEE00F4F4F400FDFDFD000000
      00000000000000000000000000000000000097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093939200FCFB
      FB00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00FBFBFA008484820000000000000000000000000000000000000000000000
      0000FDFDFD00F1F1F100D9D9D900ABABAB008A8A8A00A3A3A300D0D0D000EDED
      ED00FAFAFA0000000000000000000000000097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000095949300FCFB
      FB00F8F7F600F8F7F600F8F7F600F8F7F600F7F7F600F7F6F500F7F6F500F7F6
      F500FBFBFA008685830000000000000000000000000000000000FAFAFA00EDED
      ED00CFCFCF0093939300676767008E848400524D4D004A4A4A005D5D5D008989
      8900C0C0C000E5E5E500F6F6F6000000000097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F00676E74005C63640065696D00676A
      6E0063676B006367690065666B0062656A005B6162006569690063676C00666B
      6F00696C71006B6C6F00686C6E006D727600000000000000000096969500FCFC
      FB00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00FBFBFA0087868500000000000000000000000000EBEBEB00C0C0C0008181
      8100757575008D8D8D00A5A5A500757070003F3C3C00494444005A5252004E4C
      4C005656560072727200A8A8A800DCDCDC0097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F002C2F360038353700383734003937
      3400393734003837350037373700383838003536340037383200363731003737
      3500363531003836340038383500262A2E00000000000000000097979600FCFC
      FB00F9F8F700F9F8F700F9F8F700F8F8F700F8F7F600F8F7F600F8F7F600F7F6
      F600FBFBFA00888886000000000000000000F1F1F1008989890089898900C9C9
      C900A3A3A30080808000828282007D7C7C004E4E4E00717171007D7D7D00746E
      6E007465650056515100525252007B7B7B0097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F00414648000000000000000A000000
      0B0000000B0000000D00000000000000000000000A0000002A00000027000000
      000000002100000021000000000041414400000000000000000099989800FCFC
      FC00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00FBFBFB008A8988000000000000000000A6A6A600AEAEAE00989898008282
      8200A4A4A400B9B9B900D1D1D1008E8E8E00797979006C6C6C00818181006262
      620034343400B79898008A7777006B6B6B0097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F003D4345000100230004046C000304
      5E0006055C00030352000405600000002D0004035C0002016B00030674000000
      260006056E0001005100000000003F3F430000000000000000009A9A9900FDFC
      FC00F9F9F800F9F9F800F9F8F800F9F8F700F9F8F700F9F8F700F8F7F700F8F7
      F600FCFBFB008B8A89000000000000000000A0A0A0009C9C9C00B5B5B500C7C7
      C700E1E1E100E6E6E600D0D0D000D8D8D800D1D1D100C3C3C3009A9999007D78
      78004643430093818100847676007D7D7D0097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F004043460000002900040477000100
      6800040455000000000003015700060693000102650000005600040692000000
      40000203890002005800000000003E41420000000000000000009B9B9B00FDFC
      FC00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00FCFBFB008D8C8B000000000000000000BABABA00D3D3D300DEDEDE00C7C7
      C700E1E1E100DFDFDF00F2F2F200FCFCFC00ECECEC00DFDFDF00F7F7F700EEEA
      EA00DECCCC008F8A8A008D898900E7E7E70097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F00454642000000330007078E000202
      8100020368000405450006048A000302920003038400000070000605AA000000
      54000604A10002027700040331003C3D420000000000000000009D9D9C00FDFC
      FC00FAF9F900FAF9F800FAF9F800FAF9F800F9F9F800F9F8F700F9F8F700F8F8
      F700FCFBFB008E8D8C000000000000000000E3E3E300E0E0E000BBBBBB007979
      79006C6C6C00B8B8B800F2F2F200FCFCFC00ECECEC00DFDFDF00F7F7F700CACA
      CA00C1C1C100CBCBCB00F9F9F9000000000097928F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000097928F0046454100000038000604A0000303
      A8000203A7000000A40001017F0000005700010080000202B0000406AB000502
      A9000304BA000202B4000B088E003A39430000000000000000009E9E9E00FDFD
      FC00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00FCFBFB008F8F8D000000000000000000000000000000000000000000ECEC
      EC00DBDBDB00C8C8C800D5D5D500E6E6E600CDCDCD00D4D4D400D4D4D400FCFC
      FC000000000000000000000000000000000097928F00CDCCCA00CDCCCA00CDCC
      CA00CDCCCA00CDCCCA00CDCCCA00CDCCCA00CDCCCA00CDCCCA00CDCCCA00CDCC
      CA00CDCCCA00CDCCCA00CDCCCA0097928F0042424200181922001A1B37001C1A
      3C001C1B3D001D1C3C00171827001A19220018182A001719250014140C001616
      0F0015150D0015150D0012140F003F3F3F000000000000000000A09F9F00FDFD
      FD00FAFAF900FAFAF900FAF9F900FAF9F800FAF9F800F9F9F800F9F8F800F9F8
      F700FCFBFB0091908F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097928F00E0D9D300E0D9D300E0D9
      D300E0D9D300E0D9D300E0D9D300E0D9D300E0D9D300E0D9D30091796800E0D9
      D30091796800E0D9D3009179680097928F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A100FDFD
      FD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00F9F9F800A6A6A6008C8C
      8C008C8C8C009292900000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097928F0097928F0097928F009792
      8F0097928F0097928F0097928F0097928F0097928F0097928F0097928F009792
      8F0097928F0097928F0097928F0097928F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A2A2A200FDFD
      FD00FBFBFA00FBFAFA00FAFAF900FAF9F900FAF9F800FAF9F800A6A6A600EAEA
      EA00D5D5D5009F9E9E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4A4A300FDFD
      FD00FDFDFD00FDFDFD00FDFDFC00FDFCFC00FDFCFC00FDFCFC00A6A6A600D6D6
      D600A1A1A000EBEBEB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5A5A500A4A4
      A300A2A2A200A1A1A100A09F9F009E9E9E009D9D9C009B9B9B009A9A9900A3A3
      A300EBEBEB00000000000000000000000000000000000000000091908F008F8F
      8D008E8D8C008D8C8B008B8A89008A8988008888860087868500868583008484
      82008382800082817E00000000000000000000000000F1F8FC00D8EBF500BDDD
      EF009FCDE70085BFE00053A1CB00C7D6DD00EDF2F40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000092929200929292009292
      9200929292009292920092929200929292009292920092929200929292009292
      920092929200929292000000000000000000000000000000000092929000FBFB
      FA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFBFA00FBFB
      FA00FBFBFA00838280000000000000000000000000004CABD70043ACD80044B1
      DB0048B9DF004BBFE3002C8FC300358CB300388FB600389ECD00379DCD00369C
      CC0045A3CF008DC6E100000000000000000000000000B99FAB00BA698E00A666
      7E009D9B9A00969291008B8381008A8685008582810082807F0087858500726E
      6E00874B68009E5B7C008A627400000000000000000092929200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C5C8CC004A67
      8B0061779000929292000000000000000000000000000000000093939200FCFB
      FB00F8F7F600F8F7F600F7F6F600F7F6F500F7F6F500F7F6F500F7F6F500F7F6
      F500FBFBFA00848482000000000000000000000000003CA6D5006AD8EF0064D5
      EE005FD2EC005ACFEB003396C7004EA7C00052AAC30060CAE30063CBE30067CD
      E40066C8E20045A3CF00000000000000000000000000D175A000CB6C9800C75A
      8300C5C3C200AF799200B04A7B009A959400979493008986850085828100706A
      680096436D00B45F8900905A7300000000000000000092929200FFFFFF00F6F7
      F700F6F7F600F6F6F700F6F6F700F5F6F700F5F6F600D1D8DF004D698A002E53
      7B005E758D00929292000000000000000000000000000000000095949300FCFB
      FB00F8F7F600F8F7F600F8F7F600F8F7F600F7F7F600F7F6F500F7F6F500F7F6
      F500FBFBFA008685830000000000000000000000000038A6D60073DDF1006CD9
      F00066D6EE0061D3ED003398C8004AA6BF004CA8C20057C5E0005AC7E10060C9
      E200D9B66C00379CCC00000000000000000000000000CE7AA100C76D9600C75A
      8300DDDBDB00BD8BA200B34E7D00B0ABAA00AFADAC009C99980092908E00726D
      6B0095426C00B05D8400925D7500000000000000000092929200FFFFFF00F6F8
      F800F6F7F700F6F8F800F4F5F500F6F6F700D9E0E50059718B003A5E85004D64
      7C00B4C1CE00929292000000000000000000000000000000000096969500FCFC
      FB00F8F7F700F8F7F700F8F7F600F8F7F600F8F7F600F8F7F600F7F6F500F7F6
      F500FBFBFA00878685000000000000000000000000003AA9D8007CE2F40075DE
      F2006EDBF00068D7EF00359ACA004CA7C0004DA8C30055C4E00054C4E0005AC7
      E100E1C47600389DCD00000000000000000000000000CF7DA300C9719A00C75A
      8300F4F2F200CC9BB100B7528100D7D3D200E2E2E200C4C2C200B4B2B2008580
      7F0098426C00B05B8500945F7700000000000000000092929200FFFFFF00F6F8
      F700CAC9C600A9A8A500ABACAA00B0B1B1006C7C8E0072859B004C637E00CCD4
      DC00FFFFFF00929292000000000000000000000000000000000097979600FCFC
      FB00F9F8F700F9F8F700F9F8F700F8F8F700F8F7F600F8F7F600F8F7F600F7F6
      F600FBFBFA00888886000000000000000000000000003BACDA0084E7F6007EE3
      F40077DFF20071DCF100379DCC0052AAC10052ABC4005BC7E10059C6E1005BC7
      E100EEEFEF00399FCE00000000000000000000000000D081A500CA759C00C167
      9200C193A900B889A100A5668600C49DB000C6A6B700BB99A900B391A1009C68
      800094446C00AD58810096607900000000000000000092929200FFFFFF00CCCB
      C700B9B1A400E5E3DA00F8F7F100E7E4DF00B5B0AD006E829800D9E0E600F7F7
      F700FFFFFF00929292000000000000000000000000000000000099989800FCFC
      FC00F9F8F800F9F8F700F9F8F700F9F8F700F8F8F700F8F7F700F8F7F600F8F7
      F600FBFBFB008A8988000000000000000000000000003DB0DC008DEBF80087E8
      F60080E4F50079E1F3003AA1CE005AAEC4005AAFC60092DAEA00AFE2EE0090D9
      E900EEEFEF003BA0CF00000000000000000000000000D384AA00C4769900C27D
      9D00B9709200B96D8E00B9709200B6668900B4628700B3618600B15F8300B161
      8300B2648600B656830097617A00000000000000000092929200FFFFFF00B6AF
      A600E9DFCE00FCF6EB00FCF4EB00FCF7ED00E9DDD500B0ADAB00F7F8F800F7F8
      F800FFFFFF0092929200000000000000000000000000000000009A9A9900FDFC
      FC00F9F9F800F9F9F800F9F8F800F9F8F700F9F8F700F9F8F700F8F7F700F8F7
      F600FCFBFB008B8A89000000000000000000000000003FB2DE0095F0FA008FEC
      F80089E9F70082E5F5003EA5D00064B2C60090C9D700B7E6F000AF683900B5E5
      EF00EEEFEF003CA2D000000000000000000000000000D283A700C98FA800B076
      8E00DDC6CB00FBFCFC00FAFAFB00F9FAFA00F8F9F800F7F8F800F6F7F800DDC6
      CB00BE889800C2669100985F7900000000000000000092929200FFFFFF00B8AE
      A300FAEAD600FDEFE200FDF0E200FCF2E500F9EBDD00B3ADAB00F7F8F800F7F9
      F900FFFFFF0092929200000000000000000000000000000000009B9B9B00FDFC
      FC00FAF9F800FAF9F800FAF9F800F9F9F800F9F8F800F9F8F700F8F8F700F8F7
      F600FCFBFB008D8C8B0000000000000000000000000040B5DF009CF4FC0097F1
      FA0091EEF9008BEAF70041A9D20097CBD700B4D9E200AF683900AF683900BBE8
      F100A3E1EE003EA4D100000000000000000000000000D387A900CB96AE00B076
      8E00FDFDFC00FBFCFC00FBFBFB00FAFAFA00F9FAFA00F7F8F900F6F8F800FCFD
      FB00BE889800C66B95009A617C00000000000000000092929200FFFFFF00BFB2
      A200ECD9C000FDEDDA00FDEAD600FDEEDE00ECDCC900B6B0AB00F8FAFA00F8F9
      FA00FFFFFF0092929200000000000000000000000000000000009D9D9C00FDFC
      FC00FAF9F900FAF9F800FAF9F800FAF9F800F9F9F800F9F8F700F9F8F700F8F8
      F700FCFBFB008E8D8C0000000000000000000000000043B8E100A3F7FD009EF5
      FC0099F2FB0093EFFA007BC3DF00B8DBE200B9743900B9743900B9743900B974
      39009E9F8E0088ACB500000000000000000000000000D58CAD00CC9AB100B076
      8E00EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEA
      E500BE889800C96F98009D657F00000000000000000092929200FFFFFF00D3CB
      C200C4B29C00EEE2D000FDEDDB00EFE1CF00C7B8AB00D1D1D100F9FAFA00F9FA
      FA00FFFFFF0092929200000000000000000000000000000000009E9E9E00FDFD
      FC00FAFAF900FAF9F900FAF9F800FAF9F800FAF9F800F9F8F800F9F8F700F9F8
      F700FCFBFB008F8F8D0000000000000000000000000045BBE200A8FAFF00A4F8
      FE00A0F6FD009BF3FB00A2D5E700D9983900D9983900D9983900D9983900D998
      3900D9983900D9983900ECCB9C000000000000000000D790B000CE9CB400B076
      8E00FDFCFC00FDFEFE00FCFDFD00FBFCFC00FBFBFB00F9FAFA00F8F9FA00FDFC
      FB00BE889800CD729C00A0678200000000000000000092929200FFFFFF00FBFD
      FB00D6CFC900BFB3A800B0AAA400B7B2AE00D2D1D000FAFBFC00FAFBFB00FAFB
      FB00FFFFFF009292920000000000000000000000000000000000A09F9F00FDFD
      FD00FAFAF900FAFAF900FAF9F900FAF9F800FAF9F800F9F9F800F9F8F800F9F8
      F700FCFBFB0091908F0000000000000000000000000054C3E700A9FBFF00A9FA
      FF00A5F9FE00A1F6FD007FC9E100C1DFE400F7BC3900F7BC3900F7BC3900DDD1
      880093B29100F7BC3900F7BC3900F7BC390000000000D794B400D09FB500B076
      8E00EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEAE500EEEA
      E500BE889800D0769F00A26B8500000000000000000092929200FFFFFF00FCFD
      FD00FBFDFD00FCFDFD00FBFDFD00FCFDFD00FBFCFC00FBFCFC00FBFCFB00E5E5
      E600DFDFE0009292920000000000000000000000000000000000A1A1A100FDFD
      FD00FBFAFA00FAFAF900FAFAF900FAF9F900FAF9F800F9F9F800A6A6A6008C8C
      8C008C8C8C00929290000000000000000000000000005BC5E700A9FBFF00A9FB
      FF00A9FBFF00A7F9FE004EB7DA00ADD6DD00C4E1E600FBC83700FBC83700D7F5
      F80046ACD7000000000000000000FBC8370000000000D99CB800CDCDCD00B076
      8E00FDFDFC00FDFEFE00FDFEFE00FDFDFD00FCFDFD00FAFCFC00FAFAFB00FDFD
      F900BE889800CDCDCD00A9768E00000000000000000092929200FFFFFF00FCFE
      FD00FCFEFE00FCFDFE00FCFDFD00FBFDFD00FBFDFC00FCFDFD00929292009292
      9200929292009292920000000000000000000000000000000000A2A2A200FDFD
      FD00FBFBFA00FBFAFA00FAFAF900FAF9F900FAF9F800FAF9F800A6A6A600EAEA
      EA00D5D5D5009F9E9D000000000000000000000000005CC6E700A9FBFF00A9FB
      FF009DF3FC0083E2F4004FB9DB0094CCD500CAEFF300DEF9FA00DAB53300DDF7
      F90048AED8000000000000000000F5ECCB0000000000BD9AAA009B678100B076
      8E00F9F8F700F7F5F300F5F4EF00F3F1EC00F2F1EA00F2F1E900EEEAE400EEEA
      E400BE8898009B678100A6889600000000000000000092929200FFFFFF00FDFE
      FE00FDFEFE00FCFEFE00FCFDFD00FDFEFD00FCFEFD00FCFDFD0092929200FFFF
      FF0092929200CFCFCF0000000000000000000000000000000000A4A4A300FDFD
      FD00FDFDFD00FDFDFD00FDFDFC00FDFCFC00FDFCFC00FDFCFC00A6A6A600D6D6
      D600A1A1A000EBEAEA000000000000000000000000005FC8E8008DE6F5007EDA
      F0007EDAF00095E3F40096D4DE00C4F7FA00C8FBFD00D7FAFB00E2F9FA00CAF2
      F80058B5DB0000000000000000000000000000000000E5B7CE00D887AD00D887
      AD00D887AD00D887AD00D887AD00D887AD00D887AD00D887AD00D887AD00D887
      AD00D887AD00D887AD00B1879A00000000000000000092929200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00929292009292
      9200CACACA000000000000000000000000000000000000000000A5A5A500A4A4
      A300A2A2A200A1A1A100A09F9F009E9E9E009D9D9C009B9B9B009A9A9900A3A3
      A300EBEBEB000000000000000000000000000000000079D0EB0069C9E9005FC3
      E6005EC1E5005CC0E4005ABEE30058BCE10055BAE00054B8DF0051B6DD005CB9
      DE009AD2E9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000092929200929292009292
      920092929200929292009292920092929200929292009292920092929200C2C2
      C20000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000800000000100010000000000000400000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FC3F000000000000FE7F000000000000FE7F000000000000FE7F000000000000
      FE7F000000000000FE7F000000000000FE7F000000000000FE7F000000000000
      FE7F000000000000E667000000000000E007000000000000E007000000000000
      FFFF000000000000FFFF000000000000FC1FFFFFFFFFFFFFFC1FFFFFFFFFFFFF
      FC1FC7C7FFFF9F9FFC1F8383FFFF9F9FFC1F8003FFFF9F9F8000C007DDDD9F9F
      8000E00FAAAA9F9F8000F01FAAAA83838000F01FAAAA83838000E00FAAAA9F9F
      FC1FC007DDDD9F9FFC1F8003FFFF9F9FFC1F8383FFFF8181FC1FC7C7FFFF8181
      FC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FFFC27C001F00FC03FE403C001
      E007803FC003C001C0032037C003C0018001904BC000C001800180858000C001
      8001C1000000C001800182000000C001800182010000C003800180878000C003
      800180878000C003800100878000C003C00300FFC001C003E007003FE007C003
      F00F003FF80FC003FFFFF87FFCDFC003FFFFC001FFFFFFFFFFFFC001E007FFFF
      FFFFC001C003C7C71FFFC001C00383830C7FC0018001800388F8C0018001C007
      8800C0018001E00F8884C0018001F01F8884C0038001F01F0000C0038001E00F
      88F8C0038001C0078FFFC00380018003C7FFC003C0038383C1FFC003C003C7C7
      FFFFC003E007FFFFFFFFC003FFFFFFFFFFFFFFDFFBFFFF1FFF9FFF0FF0FFFE0F
      FF0FFF07E0FF7C07FF8FFF03C0FF3803FFC7FF03C0FF3001F7C7FF8181FF2000
      E301FFC183FF0000C000FFC183FF00008001C041820300000003C04182038001
      80C7C0018003C003C3E7C0018003F007E1FFC003C003F80FE0FFC007E003FC1F
      F0FFCC0FF033FE1FF0FFFFFFFFFFFF1FFBFFFFFFFC01FFFFF1FFE10FFC01C003
      E0FFE10FFC01C003F07FE44FFC01C003F83FE44F0001C003FC1FE00F0001C003
      FE0FF01F0001C003FF07F83F0001C003FF07FC7F0001C003FE0FFC7F0001C003
      FC1FF83F0003C003F83FF83F0007C003F07FF11F007FC003E0FFF11F007FC003
      F1FFF39F00FFFC3FFBFFF39F01FFFFFFFFFFFFFFFFFFC003FFFF0000FFFFC003
      FE1F0000FFFFC003F0070000FFFFC003C00100000000C003800000000000C003
      000000000000C003000000000000C003000000000000C003000000000000C003
      000100000000C003E00F00000000C003FFFF0000FFFFC003FFFF0000FFFFC003
      FFFFFFFFFFFFC003FFFFFFFFFFFFC007C003807FFFFF8003C003800380018003
      C003800380018003C003800380018003C003800380018003C003800380018003
      C003800380018003C003800380018003C003800380018003C003800380018003
      C003800180018003C003800080018003C003800680018003C003800680018003
      C003800780018007C0078007FFFF800F00000000000000000000000000000000
      000000000000}
  end
  object RecentFilesMenu: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = RecentFilesMenuPopup
    Left = 144
    Top = 136
    object MIDummyRecentFile1: TMenuItem
      Caption = 'MIDummyRecentFile'
      Visible = False
    end
  end
  object AfterEventTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = AfterEventTimerTimer
    Left = 48
    Top = 224
  end
  object Timer1: TTimer
    Interval = 100
    Left = 48
    Top = 296
  end
  object EditorTabMenu: TPopupMenu
    Images = ImageList16
    OnPopup = EditorTabMenuPopup
    Left = 144
    Top = 200
    object Fileinfo1: TMenuItem
      Caption = 'File info'
      ImageIndex = 18
      OnClick = Fileinfo1Click
    end
    object Copyfullname1: TMenuItem
      Caption = 'Copy full name'
      ImageIndex = 10
      OnClick = Copyfullname1Click
    end
    object Showinfolder1: TMenuItem
      Caption = 'Show in folder'
      ImageIndex = 1
      OnClick = Showinfolder1Click
    end
    object Closeothertabs1: TMenuItem
      Caption = 'Close other tabs'
      OnClick = Closeothertabs1Click
    end
    object MICloseEditorTab: TMenuItem
      Caption = 'Close'
      ImageIndex = 19
      OnClick = MICloseEditorTabClick
    end
  end
  object DropFileCatcher1: TDropFileCatcher
    Control = Owner
    OnDropFiles = DropFileCatcher1DropFiles
    Left = 248
    Top = 200
  end
  object ApplicationEvents1: TApplicationEvents
    OnHint = ApplicationEvents1Hint
    Left = 144
    Top = 288
  end
end
