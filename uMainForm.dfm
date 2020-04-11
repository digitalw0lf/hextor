object MainForm: TMainForm
  Left = 0
  Top = 0
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 604
    Top = 51
    Width = 4
    Height = 588
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
    Height = 26
    AutoSize = True
    ButtonHeight = 26
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
      Top = 2
      Width = 74
      Height = 21
      Hint = 'Byte column count'
      AutoComplete = False
      ItemIndex = 0
      TabOrder = 0
      Text = 'Auto'
      OnKeyDown = EditByteColsKeyDown
      OnSelect = EditByteColsSelect
      Items.Strings = (
        'Auto'
        '8'
        '16'
        '32')
    end
    object ToolButton5: TToolButton
      Left = 175
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton6: TToolButton
      Left = 183
      Top = 0
      Action = ActionCut
    end
    object ToolButton7: TToolButton
      Left = 209
      Top = 0
      Action = ActionCopy
    end
    object ToolButton8: TToolButton
      Left = 235
      Top = 0
      Action = ActionPaste
    end
    object ToolButton9: TToolButton
      Left = 261
      Top = 0
      Action = ActionUndo
    end
    object ToolButton10: TToolButton
      Left = 287
      Top = 0
      Action = ActionRedo
    end
    object ToolButton11: TToolButton
      Left = 313
      Top = 0
      Width = 8
      Caption = 'ToolButton11'
      ImageIndex = 15
      Style = tbsSeparator
    end
    object ToolButton12: TToolButton
      Left = 321
      Top = 0
      Action = ActionFind
    end
  end
  object MDITabs: TTabControl
    Left = 0
    Top = 26
    Width = 1028
    Height = 25
    Align = alTop
    DoubleBuffered = True
    Images = ImageList16
    ParentDoubleBuffered = False
    TabOrder = 1
    OnChange = MDITabsChange
    OnGetImageIndex = MDITabsGetImageIndex
    OnMouseUp = MDITabsMouseUp
  end
  object RightPanel: TPanel
    Left = 608
    Top = 51
    Width = 420
    Height = 588
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
      Height = 547
      ActivePage = PgHash
      Align = alClient
      TabOrder = 0
      OnChange = RightPanelPageControlChange
      object PgValue: TTabSheet
        Caption = 'Value'
        inline ValueFrame: TValueFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 519
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
          inherited ValuesGrid: TKGrid
            Width = 412
            Height = 519
            ExplicitWidth = 412
            ExplicitHeight = 519
            ColWidths = (
              64
              343)
            RowHeights = (
              21
              21)
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
          Height = 519
          Align = alClient
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
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
            Height = 236
            TabOrder = 2
            ExplicitTop = 283
            ExplicitWidth = 412
            ExplicitHeight = 236
          end
          inherited EditFieldValue: TEdit
            TabOrder = 3
          end
          inherited ToolBar1: TToolBar
            Width = 412
            Images = nil
            ExplicitWidth = 412
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
          Height = 519
          Align = alClient
          DoubleBuffered = True
          ParentBackground = False
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
          inherited DiffBar: TPaintBox
            Height = 519
            ExplicitHeight = 523
          end
          inherited BtnRecompare: TButton
            Images = nil
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
          Height = 519
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
          inherited Splitter1: TSplitter
            Top = 383
            Width = 412
            ExplicitTop = 387
            ExplicitWidth = 412
          end
          inherited ToolPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited OutputPanel: TPanel
            Top = 387
            Width = 412
            ExplicitTop = 387
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
            Height = 350
            ExplicitWidth = 412
            ExplicitHeight = 350
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
          Height = 519
          Align = alClient
          DoubleBuffered = False
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
          inherited MainPaintBox: TPaintBox
            Width = 314
            Height = 470
            ExplicitWidth = 299
            ExplicitHeight = 474
          end
          inherited LeftPanel: TPanel
            Height = 470
            ExplicitHeight = 470
          end
          inherited TopPanel: TPanel
            Width = 412
            ExplicitWidth = 412
          end
          inherited VertScrollBar: TScrollBar
            Left = 395
            Height = 470
            ExplicitLeft = 395
            ExplicitHeight = 470
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
          Height = 519
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
          inherited PageControl: TPageControl
            Width = 412
            Height = 519
            ExplicitWidth = 412
            ExplicitHeight = 519
            inherited TabSheet1: TTabSheet
              ExplicitLeft = 4
              ExplicitTop = 24
              ExplicitWidth = 404
              ExplicitHeight = 491
              inherited ResultsList: TVirtualStringTree
                Width = 404
                Height = 472
                TabOrder = 1
                ExplicitWidth = 404
                ExplicitHeight = 472
                Columns = <
                  item
                    Position = 0
                    Text = 'Address'
                    Width = 70
                  end
                  item
                    Position = 1
                    Text = 'Hex'
                    Width = 230
                  end
                  item
                    Position = 2
                    Text = 'Text'
                    Width = 100
                  end>
              end
              inherited StatusBar1: TStatusBar
                Top = 472
                Width = 404
                ExplicitTop = 472
                ExplicitWidth = 404
              end
            end
          end
        end
      end
      object PgHash: TTabSheet
        Caption = 'Hash'
        ImageIndex = 6
        inline HashFrame1: THashFrame
          Left = 0
          Top = 0
          Width = 412
          Height = 519
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 412
          ExplicitHeight = 519
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
          inherited ResultListView: TListView
            Width = 412
            Height = 172
            ExplicitWidth = 412
            ExplicitHeight = 172
          end
          inherited StaticText1: TStaticText
            Width = 406
            ExplicitWidth = 406
          end
        end
      end
    end
    object MsgPanel: TPanel
      Left = 0
      Top = 547
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
        'selection start/end'
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
      object Saveselectionas1: TMenuItem
        Action = ActionSaveSelectionAs
      end
      object Revert1: TMenuItem
        Action = ActionRevert
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MIOpenDisk: TMenuItem
        Action = ActionOpenDisk
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object OpenProcessMemory1: TMenuItem
        Action = ActionOpenProcMemory
      end
      object N5: TMenuItem
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
      object MICopyAs: TMenuItem
        Action = ActionCopyAs
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
      object N2: TMenuItem
        Caption = '-'
      end
      object MIFindReplace: TMenuItem
        Action = ActionFind
      end
      object FindNext1: TMenuItem
        Action = ActionFindNext
      end
      object FindPrevious1: TMenuItem
        Action = ActionFindPrev
      end
      object GoToaddress1: TMenuItem
        Action = ActionGoToAddr
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Setfilesize1: TMenuItem
        Action = ActionSetFileSize
      end
      object Insertbytes1: TMenuItem
        Action = ActionFillBytes
      end
    end
    object MIView: TMenuItem
      Caption = 'View'
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
    end
    object MITools: TMenuItem
      Caption = 'Tools'
      object Compare1: TMenuItem
        Action = ActionCompare
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
      object Something1: TMenuItem
        Caption = 'Something'
        OnClick = Something1Click
      end
      object Loadplugin1: TMenuItem
        Caption = 'Load plugin'
        OnClick = Loadplugin1Click
      end
    end
    object MIHelp: TMenuItem
      Caption = 'Help'
      object AboutHextor1: TMenuItem
        Action = ActionAboutBox
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
    object ActionCopyAs: TAction
      Category = 'Edit'
      Caption = 'Copy as...'
      Hint = 'Copy as...'
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
      Category = 'Navigation'
      Caption = 'Go to start of file'
      Hint = 'Go to start of file'
      SecondaryShortCuts.Strings = (
        'Ctrl+Shift+Home')
      ShortCut = 16420
      OnExecute = ActionGoToStartExecute
    end
    object ActionGoToEnd: TAction
      Category = 'Navigation'
      Caption = 'Go to end of file'
      Hint = 'Go to end of file'
      SecondaryShortCuts.Strings = (
        'Ctrl+Shift+End')
      ShortCut = 16419
      OnExecute = ActionGoToEndExecute
    end
    object ActionRevert: TAction
      Category = 'File'
      Caption = 'Revert'
      Hint = 'Revert unsaved changes'
      OnExecute = ActionRevertExecute
    end
    object ActionFind: TAction
      Category = 'Edit'
      Caption = 'Find/Replace...'
      Hint = 'Find/Replace text or data'
      ImageIndex = 3
      ShortCut = 16454
      OnExecute = ActionFindExecute
    end
    object ActionFindNext: TAction
      Category = 'Edit'
      Caption = 'Find Next'
      Hint = 'Find next occurrence'
      ShortCut = 114
      OnExecute = ActionFindNextExecute
    end
    object ActionFindPrev: TAction
      Category = 'Edit'
      Caption = 'Find Previous'
      Hint = 'Find previous occurrence'
      ShortCut = 8306
      OnExecute = ActionFindPrevExecute
    end
    object ActionGoToAddr: TAction
      Category = 'Navigation'
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
      Category = 'Edit'
      Caption = 'Edit Bits'
      Hint = 'Open bits editor'
      OnExecute = ActionBitsEditorExecute
    end
    object ActionCompare: TAction
      Category = 'Tools'
      Caption = 'Compare...'
      Hint = 'Compare open files...'
      OnExecute = ActionCompareExecute
    end
    object ActionSetFileSize: TAction
      Category = 'Edit'
      Caption = 'Set file size...'
      Hint = 'Set file size...'
      OnExecute = ActionSetFileSizeExecute
    end
    object ActionFillBytes: TAction
      Category = 'Edit'
      Caption = 'Insert bytes / Fill selection'
      Hint = 'Insert bytes / Fill selection'
      OnExecute = ActionFillBytesExecute
    end
    object ActionSelectRange: TAction
      Category = 'Edit'
      Caption = 'Select Range...'
      Hint = 'Select Range...'
      OnExecute = ActionSelectRangeExecute
    end
    object ActionDebugMode: TAction
      Category = 'Tools'
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
      494C01010F004001540210001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCF0D8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCF0D80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFCFC00FEFCFC000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00F4C36500EFAF3900F7D3
      8B00000000000000000000000000000000000000000000000000000000000000
      0000F7D38B00EFAF3900F3C26200FEFEFE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4DBC3001B7815001B781500FDFC
      FC00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F5C76E00E7A62E00D28A1800EDAF
      3800F6D59400000000000000000000000000000000000000000000000000F8D6
      9300EFAF3800D48C1900E8A72F00F5C76E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFCFB001B7A18008CBC
      8A00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F3BA4D00E7A32700D58E1A00D28B
      1900E9AC3800FBF2E00000000000000000000000000000000000FDF3E000EFB1
      3700D48E1A00D68F1B00E7A32700F3BA4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008CBD8D001B7D
      1D00C4DDC4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEF9EF00F2B54000DD972000D48D
      1A00D18B1900E3AA3B0000000000000000000000000000000000F2B33900D58E
      1B00D7901B00DE992000F3B64000FEF9EF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFBFB00000000000000000000000000000000000000000099C59C001B80
      22008CBE8F000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDF8ED00ECAC3200DA94
      1E00D48D1A00DA9D3200EBCE95000000000000000000F8D69300E8A73100D790
      1B00DE972000F3B13100FDF8ED00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFB
      FB001B832700FDFBFB000000000000000000FDFBFB00FDFBFB007AB681001B83
      27001B832700FDFBFB00FDFBFB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8E5C000E3A8
      3900D9921C00D5932100DFB668000000000000000000F4C36500E09B2300DC96
      1E00EFAF3800FBE7C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FDFBFB001C87
      2E001C872E001C872E00FDFBFB00FDFBFB001C872E001C872E001C872E001C87
      2E001C872E001C872E001C872E00FDFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCF8EE00E0A7
      3700DE971F00D7932000D4A549000000000000000000F2B84600E09B2100E19B
      2100F2B23500FDF8EE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFCFC00248F3700248F
      3700248F3700248F3700248F3700FDFCFC00FDFCFC00248F3700248F3700248F
      3700248F3700248F3700FDFCFC00000000000000000000000000D4AB5900CD9F
      4600CD9F4600CC9F4500CC9E4500CA9E4500CAA14E0000000000FCF9F100DBA4
      3800E39D2200DB982200CFA147000000000000000000F3B74400E49F2300E6A1
      2400F2B33500FEF9F10000000000CAA14E00C89C4600C69C4600C59B4600C49A
      4700C2994700C8A45B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDFCFC00329A4500329A4500329A
      4500329A4500329A4500329A4500329A4500FDFCFC00FDFCFC00329A4500329A
      4500329A4500FDFCFC0000000000000000000000000000000000F3B43A00FBBE
      4100FBC04600FABE4100F9BC3900EFB43C00EEC87C0000000000EBCD9100DEA6
      3A00E8A22500DDA03200D2AB5E000000000000000000F4BF5B00EBAA3200EBA6
      2700F0B13700F5D2900000000000EEC87C00E9B03C00F3B73800F2BA3F00F0B9
      4500EEB84800C99C400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFCFC00FDFCFC0040A4
      520040A452008FCA9A00FDFCFC00FDFCFC000000000000000000FDFCFC0040A4
      5200FDFCFC000000000000000000000000000000000000000000F3B43A00FDBE
      3A00FEC44A00FEC64F00F1B74000EFC97D00F8EDD600EBCC8E00D9A53D00F0AF
      3300EEB24000DBAD5700DEC696000000000000000000F8D69300F3BD5500F1B4
      4100F3B23300EBB03B00F1CE8D00F8EDD600E9C57D00E5B04000FEC64C00FEC6
      5000F9BF4400C699400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFE00A2D5
      AB0049AB5A00AEDAB6000000000000000000000000000000000000000000FDFD
      FC00FEFEFE000000000000000000000000000000000000000000F4B94600FDC1
      4400FEC14300FEC54E00FEC75100EBB44500E2AD4400E6B14500F8BD4800F7BE
      4F00F0BA5100C99F4E00FEFEFE000000000000000000FEFEFE00F1B94A00F5BE
      5100F8C05000FABE4800EBB54400E2AD4400E4B04500FEC74F00FEC75300FEC4
      4C00F8BF4700C49D4C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D1EA
      D60051B16100A6D8AF00FDFDFD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F3BC4F00FDC9
      5E00F8C35600FECA5C00FECE6700FECE6800FECD5F00FEC95300FCC34D00F6BD
      4C00CCA14A00F5EFE30000000000000000000000000000000000FCF3E200ECB4
      4600F9C14C00FDC44E00FECA5500FECD5F00FECE6700FECF6A00FDCB6200ECBC
      5800F9C86000C49F550000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00AADAB30058B56700FDFDFD00FDFDFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F1BA5000F3BF
      5800EEC36D00E3AC4000ECBA5500F0BD5700F1C05700ECBA5000E0B04C00C89B
      4400F5EFE300000000000000000000000000000000000000000000000000FBF3
      E200E4AE4200EBB74C00F0BC5000F1C05700ECBC5900E1B35700C69A4400D3B2
      7000D9AF5B00C29E550000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFD005CB96B005CB96B00D4EDD9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EEBC5600F0C7
      79000000000000000000EED8AB00E5C48400DBB56900DDBD7E00E5D1AA00FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00EED7A900E4C27D00DBB56900DFC08400E5D2AD00000000000000
      0000D2B57D00C3A15B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FAFBFB00FDFDFD00FDFDFD00FAFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFDFFBFF0000FF9FFF0FF0FF0000
      FF0FFF07E0FF0000FF8FFF03C0FF0000FFC7FF03C0FF0000F7C7FF8181FF0000
      E301FFC183FF0000C000FFC183FF00008001C041820300000003C04182030000
      80C7C00180030000C3E7C00180030000E1FFC003C0030000E0FFC007E0030000
      F0FFCC0FF0330000F0FFFFFFFFFF0000FBFFFFFFFC01FFFFF1FFE10FFC01C003
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
    Left = 144
    Top = 200
    object MICloseEditorTab: TMenuItem
      Caption = 'Close'
      OnClick = MICloseEditorTabClick
    end
  end
end
