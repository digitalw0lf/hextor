object AsmFrame: TAsmFrame
  Left = 0
  Top = 0
  Width = 342
  Height = 519
  TabOrder = 0
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 342
    Height = 41
    Align = alTop
    TabOrder = 0
    object BtnLockDisasm: TSpeedButton
      Left = 8
      Top = 8
      Width = 57
      Height = 27
      Hint = 'Lock disasm and don'#39't move with caret'
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Lock'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000A449A3996B30
        8F5A148F5A14905A13905A13915A12925A12925B11935B11935B108F5C108C5C
        0F8C5C0FA06E2DA449A3996B317A6248A17A5CB58662B68762B88863BA8963BC
        8A64BD8B64BB8860BC8A62BC8A62BC8B63B4885DAA773FA36F2E9C6215B1835C
        9B82649F6F2EC07D21C27E21C27E21C27E21C27E21BF7E24BF7F30C3863FC191
        3EBD963CB285379C7123A0671BCE945FA57432C1B4A2956D38BC7E2CC4822AB1
        81428C7F6EAD893BBF883AC18537C28835C1963CC39845987622A36B20CE9358
        C584319E723AE2E0DD8C6A3E9F78468F8271ADADADA18D48C18C3AC08835C78F
        3FCA9440C297469F7424A66F25CF9455C98A39C28639967040D5D5D5928675A4
        A4A4A9A9A98D8070C98D3CC7923EC9943FC9933EC99649A77026A97228D09550
        CC8E3CCC8E3CA67F4D958978B6B6B69A9A9AA0A0A0908372CC8E3CCA933DCF94
        41C4943CD0954DA97228AD762AD1964BCF923FBC9054988C7BCCCCCCB0B0B0A4
        A4A4A1A1A1968874B88C50CF923FCF923FCF923FD1964AAD762AB07A2DD29746
        D196439A8E7ED2D2D2C7C7C7C2C2C2B9B9B9AFAFAFABABAB8F8372B98F53D196
        43D19643D29746B07A2DB37D30D49A46D49A46B79A71AD9A7FAD9A7FAC9A7EAA
        987ECDCDCDAFAFAFABABAB9588769286748F8372BB9255B37D30B78134D79F4A
        D79F4AD79F4AD79F4AD79F4AD79F4AC89E5DAE9C80CDCDCDAFAFAFA0A0A0A7A7
        A7ADADAD8F8473B78134BA8638DAA350DAA350DAA350DAA350DAA350DAA350DA
        A350CBA161AD9C82BFBFBFA6A6A6A7A7A7988B78C0995DBA8638BD8A3CDDA959
        DDA856DDA856DDA856DDA856DDA856DDA856DDA856AF9E84C6C6C6C8C8C8A596
        7FC7A063DDA959BD8A3CBA8435E4BC85E3B46CE3B46CE3B46CE3B46CE3B46CE3
        B46CE3B46CB2A38BD2D2D2AFA08ACFAB75E3B46CE4BC85BC8738BB8741E4C597
        F2DABDF2DABDF2DABDF2DABDF2DABDF2DABDF2DABDDDCBB5B7AFA6DCCAB4F2DA
        BDF2DABDE4C597BB8741A449A3BD8942BD8635BD8635BD8635BD8635BD8635BD
        8635BD8635BD8635BD8635BD8635BD8635BD8635BD8942A449A3}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnLockDisasmClick
    end
    object CBArchitecture: TComboBox
      Left = 88
      Top = 11
      Width = 97
      Height = 21
      Hint = 'Instruction set'
      Style = csDropDownList
      ItemIndex = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'x86'
      OnChange = CBArchitectureChange
      Items.Strings = (
        'x86'
        'x64')
    end
  end
  object SynEdit1: TSynEdit
    Left = 0
    Top = 41
    Width = 342
    Height = 478
    Align = alClient
    ActiveLineColor = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = DisasmPopupMenu
    TabOrder = 1
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynAsmSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoRightMouseMovesCursor, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    ReadOnly = True
    OnStatusChange = SynEdit1StatusChange
    FontSmoothing = fsmNone
  end
  object SynAsmSyn1: TSynAsmSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 104
    Top = 176
  end
  object DisasmPopupMenu: TPopupMenu
    OnPopup = DisasmPopupMenuPopup
    Left = 104
    Top = 240
    object MIGoToAddr: TMenuItem
      Caption = 'Go to 0xXXXXXXXX'
      OnClick = MIGoToAddrClick
    end
  end
end
