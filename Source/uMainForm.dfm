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
    Images = VirtualImageList1
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
      ImageName = 'Search'
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
      ImageName = 'Gear'
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
      ImageName = 'Drive'
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
      ImageName = 'Fill'
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
      ImageName = 'Drive'
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
    Images = VirtualImageList1
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
            inherited BtnSaveDescr: TToolButton
              ExplicitWidth = 26
            end
            inherited LblStructName: TLabel
              Height = 13
              ExplicitHeight = 13
            end
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
            inherited ComparisonTab: TTabSheet
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
            ExplicitWidth = 412
            ExplicitHeight = 343
          end
          inherited ToolBar1: TToolBar
            Width = 412
            Images = nil
            ExplicitWidth = 412
            inherited BtnSave: TToolButton
              ExplicitWidth = 26
            end
            inherited LblScriptName: TLabel
              Height = 13
              ExplicitHeight = 13
            end
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
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
                inherited MediaPlayer1: TMediaPlayer
                  Width = -8
                  ExplicitWidth = -8
                end
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
    Images = VirtualImageList1
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
    Images = VirtualImageList1
    Left = 48
    Top = 69
    object ActionUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 13
      ImageName = 'Undo'
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
      ImageName = 'PaneText'
      OnExecute = ActionShowPaneAddrExecute
    end
    object ActionRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 14
      ImageName = 'Redo'
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
      ImageName = 'EmptyFile'
      ShortCut = 16462
      OnExecute = ActionNewExecute
    end
    object ActionOpen: TAction
      Category = 'File'
      Caption = 'Open'
      Hint = 'Open file...'
      ImageIndex = 1
      ImageName = 'Open'
      ShortCut = 16463
      OnExecute = ActionOpenExecute
    end
    object ActionSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save file'
      ImageIndex = 2
      ImageName = 'Save'
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
      ImageName = 'Save'
      OnExecute = ActionSaveAsExecute
    end
    object ActionCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 9
      ImageName = 'Cut'
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
      ImageName = 'Copy'
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
      ImageName = 'Paste'
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
      ImageName = 'Save'
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
      ImageName = 'Search'
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
      ImageName = 'Drive'
      OnExecute = ActionOpenDiskExecute
    end
    object ActionOpenProcMemory: TAction
      Category = 'File'
      Caption = 'Open Process Memory...'
      Hint = 'Open process memory...'
      ImageIndex = 5
      ImageName = 'IconProcess'
      OnExecute = ActionOpenProcMemoryExecute
    end
    object ActionExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Close Hextor'
      ImageIndex = 6
      ImageName = 'Exit'
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
      ImageName = 'Fill'
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
      ImageName = 'Function'
      OnExecute = ActionModifyWithExprExecute
    end
    object ActionClose: TAction
      Category = 'File'
      Caption = 'Close'
      Hint = 'Close file'
      ImageIndex = 19
      ImageName = 'Delete_gray'
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
      ImageName = 'FindInFiles'
      ShortCut = 24646
      OnExecute = ActionFindInFilesExecute
    end
    object ActionHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Hint = 'Help Contents'
      ImageIndex = 20
      ImageName = 'Help'
      ShortCut = 112
    end
    object ActionCheckUpdate: TAction
      Category = 'Help'
      Caption = 'Check for updates'
      Hint = 'Check for updates'
      ImageIndex = 21
      ImageName = 'Update'
      OnExecute = ActionCheckUpdateExecute
    end
    object ActionSettings: TAction
      Category = 'File'
      Caption = 'Settings'
      Hint = 'Settings'
      ImageIndex = 22
      ImageName = 'Gear'
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
      ImageName = 'PaneAddr'
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
      ImageName = 'PaneHex'
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
    Images = VirtualImageList1
    OnPopup = EditorTabMenuPopup
    Left = 144
    Top = 200
    object Fileinfo1: TMenuItem
      Caption = 'File info'
      ImageIndex = 18
      ImageName = 'Info'
      OnClick = Fileinfo1Click
    end
    object Copyfullname1: TMenuItem
      Caption = 'Copy full name'
      ImageIndex = 10
      ImageName = 'Copy'
      OnClick = Copyfullname1Click
    end
    object Showinfolder1: TMenuItem
      Caption = 'Show in folder'
      ImageIndex = 1
      ImageName = 'Open'
      OnClick = Showinfolder1Click
    end
    object Closeothertabs1: TMenuItem
      Caption = 'Close other tabs'
      OnClick = Closeothertabs1Click
    end
    object MICloseEditorTab: TMenuItem
      Caption = 'Close'
      ImageIndex = 19
      ImageName = 'Delete_gray'
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
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'EmptyFile'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001D14944415478DA8DD3DD3602511407F07FAB93B5B8C673F4105DC41B
              502A51642D84C847586E90A49A46918FC8C75A2C3C878FF7E8A6473033DB3933
              4DD3890BE7A2397BAFB57FB377B55D0482388F0F8F64108F0C03C49F86C1EF64
              C0CE89E7407F3FFC7E3F0687065D681F970DDCDF3FD0F8D8989DB7B2D409F1FC
              FC04AFD78BCFAF2F8C8E8C6268D8423AC05DE38E02810074FE369122EBA363BC
              BDBEC0E7F3A1D56AE1FDE313D16844061AB70D0A0683D0750376CE018803AF68
              369B66C4FAFAB0B8302F0337F55B0A8526A0E97A4FB1138898B9DD504F4F914C
              2EC840FDBA4EA1709877A0F7148BFA4E4F26A0A8652C2F2565E0EAEA9A22E188
              0948C5622072EECCCD505214ACAC2CC9C0E5C5254D4E469D117A5AB7210F6328
              964A48A59665A056BBA0A9E81407B43FBE03670C019C140A585B4BC9C0F9598D
              A6A739A0FD1EA11BF33037F22705A4D3AB3250AD9E512C163381BFE7B7BA601E
              86E37C1E1BEB6919A854AA148FC7A17D6BCE3F51AAB5321E0EE472C7D8DC5C97
              0155ADD0ECCC0CBE35ADB754EA4200D9A31C325B1B32502EAB94984D3840D76F
              DF7D047098CD623BB3250325A54C738904FE73F60F0EB1BB9391816251E1EBCC
              F7C05C63B1CEEDB56EE7CCB516EBDDBEEFED6E9BC00FCA402C00E54B6B720000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'Open'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002D94944415478DA75935F4853511CC7BF77774EE7DA442D37735B92A2
              82B54CF1CF32B5A42213ADC4B917B5C5ECA98720822808A297A0877A881EA277
              2DEAC18AD44AAAB19C3E58484526D3E6664E332535D3FDBBF774EE9979F1C1C3
              3D1CF8F1FB7E7EDF737EBFCB1110CC8F5C2562781142641544A41122D2538428
              4AA7C062223D395E034D6635322CE738AC2F4E02FC1C7210ADD982E8CA120B2A
              D57ACC2D7130E658C02B9510490A856A20866731E5BE8DDCFAAECD80594F1B31
              9436B080105A41E8771033A3C388A41F8539A70C89C9DB61ED8FC153CBC3D77B
              1EF94DBD0C303CBF4AE280815662286BFC0F65F6C7DF7481DF75066A8D1EA767
              CA59DCB5779C022EA0A0F92503540CC5D6016E1B3154D8A8927E240622C4F0E3
              F377F019B5B04D946D804FEE0842EDEFC1C3E40E6CBAC2D4EB63C454E3A49563
              6C8B4214F3814434B9CBB1D57296A4C3599ACE3140A0EF0831D638A8384AAB4B
              3B82E92FB3E00D55B0DFE736096D85D3C8D7CEE144F3656EC3C1E48BC3D4412B
              AB2C89C55804DEB73D48D8DD028DCE04FB9D2813F75DDF068175E2012CAD2E19
              E07B768818AB5B98300E08C3FBAE1FABDA3CA4EAF720D3948FBA1B21F45C5301
              C956F89F37625FBB5B064C74571363E529EA8002A8581424808B02B2A1A069B9
              8515502699105DF34395D18EC9A70D2872BC9701DE270729A08E3990AA4B2EBC
              2E0FFE6AB3C0D33493512DF596662BA02BB80B5F773DF69FF5C880B14795C468
              AD6562A93A91DEC0FD813AD05300470149D274D05156E19E7013F6313B8A9D83
              3260B4D34AB2CA2BD7ED475827BC039FB0A6A300850266938616E7E956E1D6C2
              153882ED28E91892015F3BABC8CE624BFC0185782BC707C7104EA557E095C8CE
              D6D14C0940FF8B28C182D787A2B65732E0DBE306C2716BE01338FA584A28553C
              2647FC5814D414C0439F22DD40FA4B09F8C414A4E51C87F9C0451920849649F8
              CF0C22CBD3082D06105E0EE257E023D24A2F416FCCA3B390C66D3591FF00E7BB
              75955A9E3CDC0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Save'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002954944415478DAA5914B4854511CC6BFEBBDE318538D94930E8A1541
              8BC8850B6B55AD5A4414B5E9B19020901A2225482841C82C0D75D1A2A0974E56
              485644D1436A51688B20B0199FA336A3E3739C978D779C7BEFE8BDE774E61ED7
              517838877316E7FFFB7FDFF7172828D6B2840CC0D3354727BCC9FF2A8C631415
              578F0A26E0E5832FD4D73B8E0D76075CD70F413708744A60B09B806B5C49EB66
              E1FB362FC2130138B6DBE0BA729A033E3D1AA38ED22574774EA3B2F108B204DE
              859AEED80F06631B4414E16EEE46D9413BFBEBC7A5A6131CF0E1CE08156C698C
              FA82387979FF5FA5BF630AAC0B9B3097EA47CDDD720E787B7B844AEB558C8D4D
              A1B28129C8CA74A7AB07A682CC43102D686DE98614B133C0006AEF9FE180D72D
              C354DAA822E09FF9270508D910520671EDE1590E78D138482D760DC1E0AC9941
              C630EF4C79068447295AB2D1DADC03633AC704D4BB2B38E079FD00159885B950
              08FB8E95A0744F312BA26611E124F30C7842E8ED99447A5CC27C7A180DEDE756
              C7786390FE96172159095249958D11904440550C109D984A0CA6022C1B511460
              C81254DB246EBACF7340475D1F559796A1C82AF277039AA2623610C7AEBD5B61
              5D9705399982A66998F08511F36B70E439A1E584D1F0D8C501CF6ABD26201C26
              A8BA5782AF9D23F0764571BC6E1B72ED3696CD141289047A7FF621DEB3194E67
              01646906B7DA2F70C0931A0F4D25D388C78153755BF0FD4D10BFBE29282937D8
              482962B13814554120108430B40305858558C4149A9E5EE480F6EA7EAAA9CB88
              2EE838E022F07E8C20DC27C07938C2FC1B906519BAAE23341F856574278A8A8A
              1159F1A3A5A38A035EB93FD3E91F0212460C45652A54358DC4628A054959E10A
              0831906DB1308B512C071CC8CBCD87668DA1B9AD9A03D6B2FE00098F86007F50
              C9FE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Search'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001F44944415478DA8DD3CB6B13411CC0F1EF6CB3BBB155B4A610B4B442
              054D1FC2D6B42507D74311044505EFF6EA412278F7D08B7F400DEDD583A2070F
              8A39484550547CA6D196A052DB86B6A226252991421E6477DDDD9850E8267476
              6186617E1F7EBF79080B8BD8ED98C52E9AA669E8A775B17D4ED4816834DA3238
              168BB97D241261746C5478029665FE9FB6679D9C8470C7CE3F3D3D83B3C68174
              5D471BD6C40EC0B40C048D981AE42CB23F07D8DEA2D7A31E8069D4A2118DDEB2
              EA146C64B2542A159EC4E3DE40D5AC925CF841FA67866AB140BB5FA5EF483743
              0321EABBECF3C96E199EC0EB448AC5DF0546B4013A3AF6B3B5F597A56FDF3976
              A89DFEFE900BC8B2D21CB81B7F49EFF1131CEEEAA46A57E3F7D969E737594B7D
              E4D2B971B70E59519B03F71ECD123E35CE5EB9CDDD8636C9A2543648BE7AC685
              F367DC4351147F73E0C1E3A784B4303DC100D8272AD9CE9F5F39BE7E79670367
              DD1254B505F03E31C7B21D307672886057807C6E936422C9C6C22C1727AED119
              ECC5EF6F0194CA453ECFA748A7D3C83E09559659FEF49C7D7B14D6571699B831
              C9D1C17073A06C03CEB13BB51A86892424F2D975EE4F4D52C9AD5254025CBE72
              9537F32B4D3228156BD7A7F1B46A3731B3B6C4C3A99B14B2AB1CECD3907B46BC
              81566DEEC35B5EDCB9857C2088D43DBC13D8CD73F67A0BFF008D28240094C0F5
              610000000049454E44AE426082}
          end>
      end
      item
        Name = 'Drive'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001DA4944415478DADD933B8B1A5114C7FF775CC7F7828AA0A28D225150
              2B51B058FC00E60BA4D86ABF49BE84A5957D5A4141B0316E91F8D8054506C7C2
              D7888EE3667CCDDC9DB9294276D3A508E4C06DCEBDE777CEF99F730905C5DF18
              F9CF001B6943B7DB2D369B0D3C1E0FBC5E2FE6F3396C361BBB4F2693B0DC58C8
              6F80D96C46354DC3783C46B3D984A2282897CB28168B703A9DD8ED76CC170C06
              D1EFF7A1AA2AF367B359F0369E907ABD4E43A1105C2E1796CB25C2E130745D87
              DFEF7F07501415A2B833127D01A53AEEEF3F8154AB557ABD5E61427C3E1F0358
              AD56AC562BD8ED769C4E27ECF77B3C3D3D431066E8763BB8BBFB889717D968ED
              0452ABD5A8F9D00C304F2C1643201060191B8D06D6EB35ABC0F44DA753F47A3D
              3C3C7C36607D1C0E5310599669A5526182944A258C46234C2613701C87F3F962
              047C473C1E87DBED862CAB860680248920E48A542AF5730AD25AA2C3E110AD56
              CBA01E5876F374BBDFF0F8F8D568C589DB5BAF013C18BAD811894490CFE791CB
              E57E8DF172BE50B3545114D16EB731180C108D7E40A7D386A61D99A8E9741A85
              42018944828DD8E17490778BA46B3A35AB582C16AC5F411098C0994CC60046D9
              7E58792BF9E322BD35F5874A8FC723789E87C3E10067E1C8DB37FFFE2FBC029E
              68F06854CA6CF30000000049454E44AE426082}
          end>
      end
      item
        Name = 'IconProcess'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000004E4944415478DA63FCCFF09F8112C048B101FD93A6536402D8001777
              4F820AA72ED8C8909DE08F42AB4BB2410CB0B63223CBF6A3C74E410C28CCCB24
              CB00A0DE5103460D401840966E28A03837020040005056617CC7650000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'Exit'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000019E4944415478DADD924B6B14511085BFBAFD9899204A16EE75292A13
              3722211A7BC61992856E5DF80374E5CA1F250A0A822013822EDC8AE022BEF001
              5964E1036742D474DF87A775F00FB8506C6E75F72DAACE3975EEB544E24F1EFB
              0F000683417AFF7C8BC3CE5124871924BD3EF89AC632BA29EA9BD86902996A62
              FA45E85260E9F849AC1A8DD3B78D09D7F252C58156510891D70279A1E263184B
              CE6872357B63D145EA987855F6B87FAA2F05E3519A4E36B8D2E9B0DB34EC48C1
              1B318DA4A61B0333ED3F86C0725172ABA9B928A2ADE899986371F90C361E9E4F
              DB9B8FB894E74C7DE0539E71DB7B2E4BFE4A91F1CC374CC5B852E6DCA93DEB99
              E369809B4406D5396C343C9BDE6E3E665D8D33C98F6AFC2AE68392FDA4F1F4B3
              9C4331D22B8C7B75604D4AB6A5F4AE6A87ABABD8DAB84A0F260F59681D9D3BEB
              14BBF3FF367F4421525E2A8ECEF3EF1417AA0A3BDD3F91AC3C4070FC8630A13B
              85C9482F35519E24199AD3EE1399E6D7627FF619BB71FD6AFAB2B7474FAE9AB2
              21784DD71E53DB14F8EEF7E97417C83446A6319D468C3FE103DDB2F8176EE25F
              07F801194EB7429539B8C40000000049454E44AE426082}
          end>
      end
      item
        Name = 'TextFile'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001CD4944415478DA8D93C94E024110867FC260A2677D0F1E820371C1B3
              82AC8262A2A222B8E0725211111846505C505C128D1AE02DD0F7E0C223383365
              0F0CA36D345887EEAE9AEEAFFFEAC96F2210B478B87F209558A6AA2036AB2A5B
              938A6E4D9B07FAFB61B7DB31383468821EA62EE0EEEE9E262726BAF54E958C14
              4F4F8FB05AAD787B7FC7C8F0B0013100B7955B723A9D50D86D5A893A83C1787D
              7986CD6643ABD542A3F1069FDFCB032A371572B95CA856ABF81923A3A30CF082
              66B3D9CE85BE3E2C2ECCF380EBF20DB9DD539015C5E881BE37A4E782D90CE9E4
              0491C8020F285F95C9EDF1A0F68B82EFE1708C4394F2585E8AF080CBCB2BF27A
              BC509802E28E687FE36B2D9805E444112B2B4B3CE0E2FC827C3E3F6AB5DE0AB2
              B91CA2D1651E502A9D53C01F606F20FFF2065ADED96911041C673288C5A23CE0
              ECB444D3D301A6A0D6438103E9E30CE2F1551E502C9E523018842C2B7FF4DF51
              2158041CA5D3585F8BF38042A148A15008F51E0AC6988254EA081B1B6B3C4092
              0A343B33830F59FE71845761610A9287292436D779403E2F5178368C7ABDB782
              8364125B894D1E9013F334170EE33FB1B77F809DED040FC86645666766E5B68D
              353BEBB6D66B6D5B6BF6D6BFEFEE6CB5019F154B1100E2233D74000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'GoArrow'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000DB4944415478DA63FCCFF09F011D28B58BFDBF57F98A918108C0886E
              004873529235C3BC794719883104C50098E63F7FFE30FCFEFD9B61F9F273040D
              C1EA82C8482386BF7FFF820D5AB7EE2A5E4318718541509036D80010DEB6ED2E
              4E43B01A0033C4CB4B19EC927FFFFE31ECDEFD08AB21380D8019E2EA2AC7F013
              E88A1F407CEAF02B0C43F01A0033C4CC568CE13BD0253F80017BFBF427144388
              3240D74A18ACF917D09087E7BE126F0021CD0403919066BCD1488C66AC06206B
              0685FCE3F3DF884F48C89ABF03F1930BDFC94BCA32069C4469C61B06C4666700
              387BD1F16339B6A60000000049454E44AE426082}
          end>
      end
      item
        Name = 'Cut'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002044944415478DA9DD15D4893611407F0FF036A921FB3DCAB73BD5B6B
              411858981742A44ED3FC4A52AA8B4AC1A28282ECA6B0286A205A1A0461D08552
              127AE342D1C1C2F0E342C4C251BA341C88CD8869B20F3F96ABF77DA5D3765184
              F53AF37F7978CEEF70CEC308843FD36A7A45391969E0D51CFB559BB03BE865DF
              30AAAF9433AC095B0B34B79A297E9B02C74B0CBF1FD737B6912049305E3B171A
              18187A471FEC1F71A2241B6A959205A73F69E9407E763ACA8AB242031EEF3299
              CCFDD0EE50E1E891832C38FDF3EC3C8CD7CF234119171A08A6A7FF0D39BF7891
              92BC0BCF4D16A4EDDB830B15C7FE6A96059C736EEA1D7C0B29B0F7E8B83D70BC
              0AE834AA8D03C1745A86E89B20C0B7FC1597CE96FEB3795DE051D30B4AE49450
              C444A1382FFDFF8067932B54635D02B705F05238EEA66E456572D4C656187309
              74B2C783D5EF02F6C647625A0AC30F514257C176A42444AEFF0BEE95553274BB
              705A1F81F6F71E64EA6261F331E4ABC3D0665BC070B9165C4C049305323AE7E9
              8002789C9BC8921AC6C898A7C1C3093FA62A35ECA2F9138DCC2CC17675BF3CC0
              373968F40C0F2E3A9CC5DE794D2355A9C8ED9885F3F26EE6F289A4BF6F85EFDE
              217940D338494F0BD570B8FDB8699EC2427D16D3066AA6321E8B7E09A75AC6B1
              D86090071E0CCE516DEF0C208AA82BD0A1EAF04ED66C75D10DCB3404BF885B39
              3C6E17E9E581CDE4275A3AE8F1CEFC30FD0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Copy'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001824944415478DA8D90BB4E02411486FF090964698D54860A8354D00A
              5E128C606DB52FB1B47486B01412ACA0D11E03958DD10679060A30B6628D4010
              05579C716658962C179793CCED64CE37DF1C52A954185644347600BF7F87C021
              8800A8AA6A4B56AB55F87CDB08EC061D216B012257ABD510DC0BFD0BB100ADB7
              777C7C33EC07B6F054AF8331668DE459D21920825F17130821C864322B0BA2B1
              18128953B26CD0E60606C5604431E426CD87EB25C8EC1C8F9FE0E8F890D8008C
              51F93A352D745D9705E20BD31443369B95B962A9844838222116A0D9EE603866
              188C29EF0545EBF1465E2EDCBF70238A8BF310AE0A05285EAF65944A697300A5
              D4EA8158737A4E02445EF6879BB85C2E747B5D792E976FED80E66B47BE3C3560
              785E30E88D18FA667FEEB4B0FC860DB018A2588CC9E497FB88FE483FB9BADD1E
              3B60B1585114341A8DB981C1D0FBA2E8F321F6360393BB14F9CB3C4BA7D3F831
              0CB337B39B0C1E8FE20C28164B2CA569D2E093FFBB3B127DA072BF91C10CB02E
              3602C02104E00FAF9B27006D8429980000000049454E44AE426082}
          end>
      end
      item
        Name = 'Paste'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001C14944415478DAA5D2494B5B511806E0F7069C6255A4365A4BAE3BC1
              5D8542174590D6A184D679E9840B275AD238B575362AAD1D5CB8704224C6C428
              318A22F62708168DCABDD83A1003820B7F411DCED79B1B3767E159E8599CF73B
              03CFE1C0271108F719D26D404D4D1D49B8C2BF0B06837409E7AC4B1202BF265B
              E83CA8823142C2F37A48EC1A4F3333B51303FCFE5D44444622B03E0876CDF0F0
              493ACABB1D120778ECA5644E8AC28B06172AABAA0132845F00D36686D0ED91B1
              296C4C54405583689EDEE401775F09C949D1C86A9C13FE79FD47990EB43A7EF3
              80ABA798E447D1887AF955089CAF3541554EF0D1B9C503CEEE220D8841F63B8F
              1058FD5602752F80CF6E3F0FCC7416926C8A81316748089CAD7CD0810ECF0E0F
              38DADF92D964C42BEB821058FE520C65F7185D0B7B3C30FDE90D99938D88CFFF
              2E044E97AC1A70841EAFC203536D169253629167F30A01EF4021D49D23F4FA54
              1E986C7E4DF2E30748B4FC1402C1C5F750FC87B02FEFF3C0B82D8FE4D438585A
              7C4260DE5E0075FB00FD2B7F7860D49AAB0390B4DE23D25B5A0B3D436B0AED69
              EB502A1A30B8FA97077CC3560A281B7AAF33C66E92C2350BD774936919CF503B
              E4E681BB8EFF9E5CE6F1C6FAFEA50000000049454E44AE426082}
          end>
      end
      item
        Name = 'Refresh'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000016D4944415478DA63FCCFF09F0119FCF9FDFB3F13133398FDEFDF5F06
              16565646063C8011DD807F7FFFFDCFDB130F66E7995430A8096B136FC0DF3F7F
              FF6FBFB78E61CBED750CD33C9682C59898998837E0FAAB4BFFBB8F3730F8A987
              32F8A98613D48C62C0FF7FFFFFAFBDBE8461FDB5E50C31FAA90C9E6A81189AFF
              02BDC7C4C8C8C00822900DF8FBE71FD894B015CE60410F357F8624A33C309B99
              05E20A901A90FCAA88BD7031B0017F80FEF69C678AD389DB934E836964352031
              98217017584F56C7D07C34F72698C6250732041E067F7EFFFB6FDC230F5770B6
              E4219846164307203528B1003244A7438EE14AC52360028238112406A2DB76D7
              322C3BBB10AC0E240F0220358C9829F1DF7F98666430F7E8ACFF6DBB9B1872ED
              0A190A9C8A51638118D0B7B7F7FF8403FD0CCB135632582B5B9366C01F6020CB
              D6C9311439153294BA14A3B88EA001476E1FFD1F34371CCC7EDEF208250D10ED
              82BF7FFE831531B33062840DD161800B0000ED7FABE5B2ECCD35000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'Undo'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000021F4944415478DAA593CB6B135114C6BF3B93263313D3262121160B69
              6DAB9436A042C107A21B0DBA2856AA5215B27021EAC28D1BD7827F801B7125EE
              940A5528524B168A8A2B573E306E229A409AC9B3994C662633739C0E9852DB92
              801F5CB8E7DCF3FD2EE7702F2310FE47EC2FE052BA4AA37E0EF78F0CB0F5F8F9
              F72A654A2DE4EB2AA20287A98880B9C42018C7D8B680D9953512FA38443D6D7C
              5E6D22DA2F82E33D00CFA01B6D98AA024D2EE0F15C024321896D019C4B2B3410
              90A039C55E9F177EC7BB5ED5728E1513300C1B6418B073DFF06866A203E90092
              AF9B640BA21331D7CC738EB15C06E92D308F07FCAE307C4E3567EAB0F219BC48
              4DBBED7400C79614822421E865703A41BDDEC0F1B08DF93111CBD906163315A8
              520C01DE46AC5DC689411E378E8E6C06F824C135B76D82E62C5E53713242B877
              38C41E7C2AD1B34C03B22921603731E56FE2C985C406E0F2C2572A2BBABB7773
              6EDA86C8032F53D36C4DB728B9904551179C1B348C8B0A9653073600DD54D52C
              3AB39843A16683397398085B787565B277C0DBBC4E77DFC9F85D54D1C74C2447
              253C3C3BDC1BA0E20CE4D45215A54205A66E6077D08B6B0743B87928D21D50D3
              8966D30A7EFE9261A82D906D616F3C8CF7178736BF839D34B3A2D08F55050DB9
              02B22CC44762787A3A8C78BFA737C0ED8F2ABDC9AEA19893313CBE0757F789B8
              35296E7DCA3B896CA2EB1F547C29B5717E4CC09D84B0FD67EAA64CCDA2FD419E
              FD9BFF033D030100B649471A0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Redo'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000021E4944415478DAA592CD6B136110C69F773FE3EE4692346D4C8A5A2F
              9682F4A8562B3D88298220F42242C1AF528FFE057AEEC18BE05104A1225450B0
              884A4BA3F622540F52B18DD14A5D4B1B134DD26DBA9BEE66C7773D98AA970507
              E6F0F23EF363E6996104C2FF04FB1B403ED1FD7C15EF4A9B285B363A75093D09
              0543BD1916FC5F7B3C4F4BE51AC6CFF5B37F00A6E5D1F9A922B41D2A2459862A
              31089E0BDFAEE3FBEA0A0EEC8AA2D454B0552962E2E2B13F01A6D5A491175528
              D128045182AA30682210E13291A7E311366B15448C28EA0B7398B870B40508DA
              3E35654195259020C2F10578768D57D960A2002D9581C4C58D26E0BA5B90975E
              63F2D236C0CDF70D7AB9EAA26CFB58E7C511DFC1D01E0527F7AAB8B758C36CC9
              83D191FE05D968B8F03EBC41EE725F0B309CABD342C946DD0362BA8CB3FB645C
              E9D5D8D557157ABEC60B540DAA4850C887C8D3F95A406EE4600B70E2C9067D5E
              AB810902E20903D359033B55C64EDF9923C7F5B98C27111753B026247505E3C3
              875A80C1A71615CC1FC113C9541CCFB23AE2118185BE83D1D93ACD2C96B9411E
              32E9368C1D36309096C20302136FBFADE25BB102595511CFB4637A5043224417
              BFEFE0C8A4455F0A26C07D90F92175ED6EC7C3E33A62DC8B508065CBA73333EB
              303FAD80310146328EFD29038FB246384010C12877F3369639A4A3338981AE28
              6EF469E101415C9F77E8C147073D6D126EF5EB7CB52147D81EF96A93BA6362A8
              2DFC048C5DF6F1A328EE1C0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Fill'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002224944415478DA8DD36F4813711807F0EF6FB7F967DE9DDEDD76B9B6
              041B73835EF62EE8CFD0D59BE84508BD29991491D08BDE46F94AE87D1144D18B
              25444620912F82142422A9C849B496A8AB60936B9A39A752AC1B3F7F77EB2E67
              327DE0388EE3FB79EE797E1CA1A0A855B9798D8E8D8EA22B1643C0EF235BDF93
              5A40462BD2A9D763087574602A99443CDEB37BC0083F9F2EA075E93D32AE2044
              ED2DFA2E5DDC1D6085E3877C18191E863B1CC57456C3E983FB100AC8A4266085
              2F1F6B339F130F0611397C12F9157D5BC406CA897ECABD7A82DB675F201611E0
              9779F0752EDCB97B0FE77A7B91CAAD425B2961269BAF422AC04037C5B714328B
              CB58BE958650EF30D166878EA1A147E8ECEC427B28848FD922168AE5AA2F217A
              E2BAD9D908BFB9F6014A13879646074476A527C6113B711CC9C924A2478FA0F8
              AB84B4B6CAC6F987107A3E4C8DB0514FFBDE41E19D9079172406ADE73E63EDE7
              02C29108027B7D9024096BA53F95710A6C9C5C1EA47026407F30F9F7CD14EE8F
              67A07A7C0CE0E0119C26E2D0D7D1ECE6A08A6EF8BD8AD9C8421EBF9C01993BA5
              9A5B0C3ECB934F5F17A98148722B24169219220B1C1BCB09BE9EA04DA92CD600
              FA1F4EE24234580D18770B6912551BF1FC1D496C24D8AF0A76F840BB979800DF
              7D057B7AAEDA676B21756E2F5A5877858D64EC46125C189998B5C3E6317E1FBC
              413787B7225C83176203C7C21CBE64E7ABC23BFE4C1652762AE0F4A5FFC23B02
              9B91EDC2466D00460A0A2A00B94D410000000049454E44AE426082}
          end>
      end
      item
        Name = 'Function'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001154944415478DAC592BD71C3300C851F77215564846802CA8DAAB4E9
              C4521AC01BB8114B6984546A424E608FE022E42E084829FE39DB89F373173494
              0EC48707E20902E13721FE0E409E8CA8302E89C611860AE26E806F0C4D7CD6C3
              002DBE2E3C07A4EE26976318F5DDC5B70114C9FBC0FF0ABA9217C0D85BDAECF7
              59AD200A644B856EF7916EE05C3D7F6A0D654BA86332C7631FF08457A0582D0A
              82A55275D8350EC40AA2F394FB3340A6F770864435CE709ADFC8B20A269C03C0
              E46D2785E7E41B5F5FB56D06C4BEA459C511E0B9491AF12A20CDC80251B405A6
              93D51E86E415D7F069C605B0483CEC9E81E6F9050FEB2DDA6B5E0891623AA584
              708EABA609171E38B9243FF1454AD0779C77D3893F8DFF07BC0335CDA52D0A75
              C2310000000049454E44AE426082}
          end>
      end
      item
        Name = 'FindInFiles'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002514944415478DA85D34B4C13411807F0FFB65DA8166D615B9728A5A6
              C6500D358D35420135E160C4A2076D6A8C31F1E64525217AF3E4C9F88817BDE9
              C5B3F14290160D414C047994874A55A0F421B565BB2D0592FA681967DB585AA8
              6166936F7667BE5F26DFCE300404525B993C4B6803724F2E92DC5CE1580A72E5
              5EA80F3D61A477E61FB03C7186ECDC7321B71A6BB99552CC66174731D005AEE1
              D506609C0235CE2D93A5EFA2BF1B5CE306204901758D63CB64691C0BF440BB09
              F048C0F92D93A571CC4F015B4F31B034D64E34FA73988A5AE1F527904E25B15D
              598E03461D4CDAA12248987743D754029895DDC164288523968350A9D4585D5D
              C6ACF70BEA6BE4307183F9FA08FEDE12C0683BE90ADF446D9D19BBB595486700
              A50210E209043F0DC3D1F0355F1F61FE3574CDAE6220316227DDA11BB0B6B4A2
              8295D319FABF65043F7F65E019E885C336934D266B7FE80EFAA16B780186AD60
              8A00D7F76B3059ACD0F35C76B732EA44C222A6270629309BADC152780C99740A
              72D57E68CCF7D781F8B09D04D9DBF81C5CC1D1C3F5E0B51CE262029E510F8429
              379C572EA2523E80C5F93E70FA2688A1F7D8D5E22A003E9C2655063BC6172CF0
              CE2C8055C850CEB2981B79831DDBCA10F27DC3D55B9D900B77C11B5B11F5F581
              3FE65E07440A70B56D9B8E72226DC3B3478FF15B0C2055C6C179F912CCFB7C14
              E8077FBC1018A280E154C9A31C4D35E2F9C307482E065065B4A0B3A30E11DF5B
              541702310A680D27FF7B943FFE6882FBE93DB01A1E1DD78D88CCBD43F5894260
              B02D7F7B25247F93375E71DA19A92B54E09B5F327F01E87967000381A3710000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'Info'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002C44944415478DAA5D35F4853611806F0E7EC3875DA74D389E95CA048
              69A66E6E6C96D32228B434331182A00BB5C40C04218230BC884A2BECA22052F2
              A242A2B23F74D14CCA8CC8F46C5354FA4316E8749B3A53373733DDF93A3B6666
              7655DFEDF7F283EFF99E972220F89F43FD0E109610BBDD8EBEBE3E8C0C0FE1EB
              A4839B00C2C322205728909C9282A8A82850028A5A037817BDC46C3281611888
              436550EB32B841117FCB2E7AD0C37462D6398134B5061A8D06B41F4DAD0298AE
              6EC230462835DB21146FC0AD172C52E303F9DB978C0365B901C0B731F4F77440
              A356439BAE5B016C561B7970BF052AED4E3C7F2785E9B30039DA0094ED93F078
              D5D549F49A0790941081223DE1907614141C40B43C9AE281D6A70632E6706263
              EA2ED4DD99C1C2772F744A054E1FF25F05CC1121CE96C7C1F2F115226521C8DE
              93BD04DC686C24DBB2B271A6790E332E162C4B201651B0D83D60E75D3C326275
              20244404C93A7F3454C7A3FB4D1B4A4A8F2C0197EA6A495E61098ED55BE10BD8
              31E541C57E09F2B2A490C92290B8F72EDCF3425FD4089704A0B9568B674F6EA2
              EAC4C91520B7B018A5E787E0717B306471A0AE32E927108E587D03D84039F7CF
              5E0407D168B9FC07D0C43D4197B91B95F52318FC320EE72C8B0B55CB808C03AE
              0381B11CB008A94488DBE712C074B6A178F909AD062EC4092722633370B4C608
              979B7040020EE7C781A6691EA04549DCE402AED56CC6F8F06B2E44311762CE12
              601DB592C78F1E2231251357EE4DA3BD6B1A6545EBA157CAF800CBAB0D1006A7
              41B94984E307FDF0BEBF1DF9F90590C7C8A95F45EA7EDB454C263387E8E1F186
              E162D320C6463FF90A0E69A808A72A762048E8C487810EA8542AA46F4DA7D654
              D96434C26C3621482C43AA4AC77D27C54FD09497EB412FDC2E1B94AA3468B5DA
              B5555E5E269BCD867EDF328D5A30333505420824D230C4C428B02539995F2601
              2DA0FEBA8DFF727E0034A83E00B268498D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Delete_gray'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000010E4944415478DA63FCCFF09F8112C04835031EDE3DF07FDDAA36065D
              5D5D06179F5E4674857BB614FFBF7CF9324350581583BCB203238A0130CDD252
              BC0C4F9F7DC63004A619268F6C08D8809387FAFE1F3BBA8341474707ACE1CA95
              2B60C5F28AD60C0FEF1F056B4296B3B2F66030B72B6244F102CC1698C2F7EF1E
              8235820C121492876B46771D4A20AE5A18FC7FEB8EDB0CDE1EAA188105130F8B
              5F8B123E18B1101FA9F75F5B5314CC5690176078F0F003987DF5FA6B8685CB2F
              61042E5E0390015106C0BC80CB00BC5E40D70CD20003C862E886C0A371DAF405
              180A61D1886E705666026A348212525D4D1EDC46745B60AE8381A69649A80909
              9694418660F327B221C89AB1C602A980620300B9B8C5F15E9ABB9B0000000049
              454E44AE426082}
          end>
      end
      item
        Name = 'Help'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002474944415478DAA5934B6813511486FF3B934C26EF340F0D3434F509
              A60912A4B8B1621716458AB8B105A57623A2457123085245A50B115C28524417
              B12B7527C54D150D1645F1B5B02ABEDA465288C6C4A4793699CCF5CEA444499A
              82783617EEE17CF7FCE7FC975050FC4F907AC0BB489CDE7CF41EF75E4510F991
              51EFDA9C266CF3B7E2F0CE8D08B4BB4853C0F589B7F4CCED1748C002C9B812B2
              6055EFB9521A7CF63B9C98C7D9FE4E1CEC09900680527CEACE1BFCD4AF06F456
              E8B45A18743C3896ABC832B2C532A45C0ACEC23446F6066B101530351BA73DE7
              C71133AC03D53BA067857B3ACC38B4D9069BC8235D94313C11C7D3D92CA46C02
              EEC267DC1FEE4587D74554C091D107F4C6CB34CAF6F500AF85A821E8F4E8912C
              C86811399CEC76203C9DC3E8B31472850508C94F38D6E5C2C5C1AD55806F2844
              3FF26B20B3D7413810D69CC013C84CDD5A87800B3B5C08CFE471ED39032C48E0
              0A096CA03398BA32500598FAAFD29CA70BD008B581720CD26AD1E0F8163BFC6E
              5195F07AAE0849A14A2518A393C8DE1A6A0E30091CF6052DE80B5870E949120F
              BFE4912FCBD5643DC07F748C7E20AB6A129430EB38EC0F5AE1730918092710CB
              48D5622A374A38117A4C2F4FC6515A1CA2125AA6A1DDAE81869D5F1365942A8B
              7EA9941B87A8B86FFBB93F6B04C7A9FAEF0E78D49ADD6351CCCDB30E981F087B
              DD9D676B3CFDD71AEB8D44452B3C2D22C607DBD45C6FE81BA2BF8A20C5F4D246
              5ACACA15A31B06B34DBDCF6752E0733138142BF735B1F2729FC9BBC28C5D9BBC
              38D0ED53DB5EF637FE6BFC06F1B31D000E2C67100000000049454E44AE426082}
          end>
      end
      item
        Name = 'Update'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002804944415478DA7D925B4853711CC7BFFF9D69F3B8C92EBA5979A788
              32034BE8A1A40BD843450F3D058390C282F550442FF59E10D54324BD48BE7479
              EB21BBD045834AD3A641EA71A266D3A1C2DCF2B24DDD71E7ECD739FF3163E2FA
              3EFE7EFC3EBFEFEFC208848DFA3DE1A7B9B08A841CC7C84818972E1F65C82226
              49639CB0B4B40083D18ED9E9206499615514C15C662C460DA01F3DB87ED3BD29
              84F57947A97B6808AC703B2CCE42ECAEAB5A4F4E6BC54B32E0BBFD08F75B3C2C
              AB830EAA822C0A28B3004623902B006B2AA028C03927B2DAE7006FBF9FA48402
              BF6D07CC39804304F235403C4258CE05AE9431B6EFB49FF2B5E4722CC18B065F
              57AE4399BEC4AFDD417A6E72C196A73930FCA32B49A0790F38205A5D0961260E
              61358ED117D64CC0D3277DD45EBA1F792601A2E622A9AD55DB1F1C0915576B84
              0C801A5985BFDDB6C1C11789EECD6D0339EC303115E50E01776BC0FC1341CAD9
              22A0DE234028C883A28D50A084F1F28108B3C58822A7837140EFF75FF438548C
              013273EA910A7040E59905B22417B162DDCAE37A775D7A4CD7AB1621E5405763
              CB38F9CA77C25DA182F50470F6A40525258529FB062BC8604C594E2A1CD0D956
              80AEAEC11460581AA7CE6F4654B89288C562088554487DEFB0ABC1831B8D56EE
              240DD047D0AFF0F1C3276A38713C354268EE0FC96BC43B4E4D06C8E78BA2E3ED
              6744161671DE7311F5875CDC49FA846DAD6FE842D329B6FE484EA79D2F4407AD
              AC2C231088A0B757C6D8C07B54D7EEC5E163B5A83B50CA0BD29D33AED0EF1DA3
              22A78907623119F3F332262767F0D33B81E2B2831CD4FAECD6E6AF9C5EA26ECB
              A6BDA1D94C08874D08CE86914C4431224DC16E1771E7E1B5FF03D26A72376704
              B2754EEB2F85B034BD8A7AFB2F0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Gear'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000029B4944415478DA7553CB4F1A411CFE06769767D5AA14AAA0C628C66A
              3C34410F35F171341EBC1AFF532FC68B871EAA118C12AB8008883C6481E5B58F
              E96F86A4B6D54E42C8ECCE7CCFDF320E8EF756B76372457141D5DC4CECCD81C5
              554D61FF9E63EF01140B3A2F959AF07A55C4E321542A2D94CB4D04021AE24B11
              F65F807E8F585417EEEFEBE09CA3D1E8CA7FCBB2E1762B04F282ADAD458C8EF9
              D91B809FB715DEE9D874C1917BBF5FA38B1C86D1977BB7DB85C7C70A12893930
              BA3532E287CFAF3109209853A91262B18FE8F54CB45A0338CE109804C0346D09
              56A934A028A0F75D84C3416C6E2E0D15D89643004FB06D2018D4D0EF5BD26FAD
              66086ECA42A1672606038BCE704992CF177170F0750860B47B3C93A9915C930E
              7BE1F32948A5B204D04624324E7203F44C587208C8C6C5C52D1129383CFC06D6
              6C767832F928431A1D0DA0DD36E9A58A6432437EE731333341DE75E4723A7967
              32D0743A83858509ECECAC81E97A877FFF7E4F4C2119A0902FEA2B95AA989B9B
              C4ECEC24A5DF452653270047662132A8D7EB585D8D8099A6C94F4FEF88A54D2A
              2C2C2E4EC9C421AC1903783C1E0AD1A1502DB85C0CBADEA51C18AAD506A6A7B5
              6106B56A9B178B2F383FCF627E3E0A55556403A209119A9805B1448897975968
              9A2A5526129F5FE7E0E6BAC0D3E90AD53321BBF7783462E4B246E17B3070E867
              239BCD63636386B20923141A7905B84AE5F9DD5D8DFCB648668FBC47313EEE23
              30832E95A9D60FB2915CEE01DBDB0BF8B212637F4DA2DEE85016D7B8BA7AC0F2
              F21431AB64C143613E51683AA2D108F5EF46A150C4DEDE0AD637E2ECCDB720E6
              C1307AF8141E63E73F32FCF8F886A6EF1947479B041AC3D9599A1475B1BBBB86
              40D0FB16E0CFF55C6EF09393143502ECEFAFC3EBD31877B85870B95DBF3FA65F
              891187C5E30AEDAE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Folder'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001684944415478DA85935B4B02411886BFC182C04873DD14D2358C2EEB
              D6D2CCE822B2D5A403F5A3BAEE5F4437516A811841673BFC80562508CDDDB2BA
              E8A2729AAF21DD35716616DE6F0ECFC32CCC100A14B0BDDF2D51D600F8C793F2
              35738D61EB1B01C7C416C131F913BCDD26E9C0F006DF0D0DBE13F397B6A651DE
              0529B4DF26B86102DFBA10C679A3B407D2649BE095091CBE35218CB55E4E83FB
              9FE01A05AB42186BBDC4045369ABA05E4850A77F4508635D2B66410E77142C0B
              61FCC55AE9A083E00A052921CC4F700872246315BC5CAA7450490961DAF86427
              C8831CDA06D2DB4FAC027FB22B8CE3FA6301BEBF3EC0661F03E7F8664BF07CA1
              529792E80AE3FC533107923F0CC6C3090C4D674C82F345EA0AA85D61CCAA9603
              4F708E6734DB12184C202971E155AE6A792698E53963169C3141604178952BDA
              117883319E6681CE04EEC0BCF02A57EE8FC13B1AE519330B4EE3CDD78B92E64B
              6E7FE2AC13EC3D76F04476C80FB4A55E0097EB5F6A0000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'Plus'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001744944415478DAA5D1494A03411400D05F5BEF10EF201EC013780BDD
              68231A23E28489D18038A1080ED9089A03986E9CD080E01412A7646386261DC9
              B07204516CBBBF35744CC06451FA9BDF55D4A71E55BF0802C27F823402C81C2D
              D974F241D316893E245240A82B00E6A709C65B1CBC6BEADF00FD250AB15C0ED4
              BD8424304370AB9B011710330CD076293021034C53409902FD352A801D0AF81B
              00BC6195701AC6DC4D659203717E82A458FFAAD6D928007ADC9012E0FB11C50F
              59D28FF5C0B66D686E6AE5EB169DB35AE6E90CE6D78F1DC04F70A3C707F7EF57
              BC28208198A6C9013667A365597C4C944A10517507F011F476B6C3653E5FED08
              DDD0E27271E0BA50F83955056319D9AE00E3353D402769047BC720FB7C0E3714
              D8D7EEC4BA55A707755F618460B06F14D2F4AEB7C5221C6829C05999671C24B8
              DA3F0CE9C7537EDF433503B820030C105CF10C41EAE10492E5321C85B3808B32
              8087A0BBA38DDF9F4524AC032EC9006EA7B1354DC3E5DFC03770AE1A311141D7
              490000000049454E44AE426082}
          end>
      end
      item
        Name = 'Delete'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000010F4944415478DA63FCCFF09F8112C0483503DE9F3AF2FF76570303AB
              A5018361710F23BAC2F3BD25FF7F1FBFC0A05AD6C0206866C38862004CB3B4A0
              34C3D3F74F310C816986C9231B0236E0F9FC99FF1F6F5DCDA0AAAB0BD670FBF2
              65B062262B2B867FC78E413421C9C97A87324826A633A27801EE44A8C26F4FDE
              8135820CE29211826B46771D4A203E4D8DFFFF64FF79061947438CC082894BCF
              5E88123E18B1705245EFBFBABC2298FD55498081FBDE0730FBE6C3FB0CE6772E
              61042E5E0390015106C0BC80CB00BC5E40D70CD20003C862E886C0A3F151EB54
              0C85B068443758AE3A1B351A4109E9565416DC46745B60AE8301B565D3501312
              2C29830CC1E64F64439035638D055201C5060000E814C5F1A456830200000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'PaneAddr'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000002F4944415478DA63FCCFF09F8112C0386A00C200462013C866844B10
              C9071B00E790410F1703280EC4C1910E86AE010013538FF5668551A500000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'PaneHex'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000003E4944415478DA63FCCFF09F8112C048150318411416009463042BC2
              234F5D03601A309C89477E901980CDFFB8C2001E3E543560E0C360081B4009A0
              D80000639167F16E02F8250000000049454E44AE426082}
          end>
      end
      item
        Name = 'PaneText'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000374944415478DA63FCCFF09F8112C048550318415C2200500F236D0D
              802920C4271806D834901488A3060C7903D053252E43A89B99C80100A9C249F1
              341F46D50000000049454E44AE426082}
          end>
      end>
    Left = 456
    Top = 72
  end
  object VirtualImageList1: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'EmptyFile'
        Disabled = False
        Name = 'EmptyFile'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Disabled = False
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Save'
        Disabled = False
        Name = 'Save'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Search'
        Disabled = False
        Name = 'Search'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Drive'
        Disabled = False
        Name = 'Drive'
      end
      item
        CollectionIndex = 5
        CollectionName = 'IconProcess'
        Disabled = False
        Name = 'IconProcess'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Exit'
        Disabled = False
        Name = 'Exit'
      end
      item
        CollectionIndex = 7
        CollectionName = 'TextFile'
        Disabled = False
        Name = 'TextFile'
      end
      item
        CollectionIndex = 8
        CollectionName = 'GoArrow'
        Disabled = False
        Name = 'GoArrow'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Cut'
        Disabled = False
        Name = 'Cut'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Copy'
        Disabled = False
        Name = 'Copy'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Paste'
        Disabled = False
        Name = 'Paste'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Refresh'
        Disabled = False
        Name = 'Refresh'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Undo'
        Disabled = False
        Name = 'Undo'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Redo'
        Disabled = False
        Name = 'Redo'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Fill'
        Disabled = False
        Name = 'Fill'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Function'
        Disabled = False
        Name = 'Function'
      end
      item
        CollectionIndex = 17
        CollectionName = 'FindInFiles'
        Disabled = False
        Name = 'FindInFiles'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Info'
        Disabled = False
        Name = 'Info'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Delete_gray'
        Disabled = False
        Name = 'Delete_gray'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Help'
        Disabled = False
        Name = 'Help'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Update'
        Disabled = False
        Name = 'Update'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Gear'
        Disabled = False
        Name = 'Gear'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Folder'
        Disabled = False
        Name = 'Folder'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Plus'
        Disabled = False
        Name = 'Plus'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Delete'
        Disabled = False
        Name = 'Delete'
      end
      item
        CollectionIndex = 26
        CollectionName = 'PaneAddr'
        Disabled = False
        Name = 'PaneAddr'
      end
      item
        CollectionIndex = 27
        CollectionName = 'PaneHex'
        Disabled = False
        Name = 'PaneHex'
      end
      item
        CollectionIndex = 28
        CollectionName = 'PaneText'
        Disabled = False
        Name = 'PaneText'
      end>
    ImageCollection = ImageCollection1
    Left = 368
    Top = 136
  end
end
