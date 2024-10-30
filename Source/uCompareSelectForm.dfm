object CompareSelectForm: TCompareSelectForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Compare'
  ClientHeight = 338
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object Label3: TLabel
    Left = 16
    Top = 212
    Width = 82
    Height = 15
    Caption = 'Sync block size:'
  end
  object Label4: TLabel
    Left = 200
    Top = 212
    Width = 28
    Height = 15
    Caption = 'bytes'
  end
  object ImageProxy1: THintedImageProxy
    Left = 239
    Top = 211
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 
      'When comparing different data, detect similarities if they are a' +
      't least this size'
  end
  object HintedImageProxy1: THintedImageProxy
    Left = 239
    Top = 244
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 'If unchecked, simple compare bytes one-by-one'
  end
  object BtnCompare: TButton
    Left = 136
    Top = 288
    Width = 89
    Height = 33
    Caption = 'Compare'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 272
    Top = 288
    Width = 89
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object CBSyncBlockSize: TComboBox
    Left = 128
    Top = 209
    Width = 57
    Height = 23
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
    Width = 473
    Height = 97
    Caption = 'Left'
    TabOrder = 3
    object LblRange1Start: TLabel
      Left = 136
      Top = 56
      Width = 23
      Height = 15
      Caption = 'start'
      Enabled = False
    end
    object LblRange1End: TLabel
      Left = 304
      Top = 56
      Width = 20
      Height = 15
      Caption = 'end'
      Enabled = False
    end
    object CBCmpEditor1: TComboBox
      Left = 8
      Top = 21
      Width = 449
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = CBCmpEditor1Change
    end
    object CBRange1: TCheckBox
      Left = 8
      Top = 57
      Width = 121
      Height = 17
      Hint = 'Range is pre-filled with current selection'
      Caption = 'Specify range:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CBRange1Click
    end
    object EditRange1Start: TEdit
      Left = 174
      Top = 52
      Width = 121
      Height = 23
      Hint = 'Range is pre-filled with current selection'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = '0'
    end
    object EditRange1End: TEdit
      Left = 336
      Top = 52
      Width = 121
      Height = 23
      Hint = 'Range is pre-filled with current selection'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = '0'
    end
  end
  object CBDetectInsertions: TCheckBox
    Left = 16
    Top = 244
    Width = 193
    Height = 17
    Hint = 'Range is pre-filled with current selection'
    Caption = 'Detect block insertion/deletion'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 4
  end
  object GBFile2: TGroupBox
    Left = 8
    Top = 106
    Width = 473
    Height = 97
    Caption = 'Right'
    TabOrder = 5
    object LblRange2Start: TLabel
      Left = 136
      Top = 56
      Width = 23
      Height = 15
      Caption = 'start'
      Enabled = False
    end
    object LblRange2End: TLabel
      Left = 304
      Top = 56
      Width = 20
      Height = 15
      Caption = 'end'
      Enabled = False
    end
    object CBCmpEditor2: TComboBox
      Left = 8
      Top = 21
      Width = 449
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = CBCmpEditor1Change
    end
    object CBRange2: TCheckBox
      Left = 8
      Top = 57
      Width = 121
      Height = 17
      Hint = 'Range is pre-filled with current selection'
      Caption = 'Specify range:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CBRange2Click
    end
    object EditRange2Start: TEdit
      Left = 174
      Top = 52
      Width = 121
      Height = 23
      Hint = 'Range is pre-filled with current selection'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = '0'
    end
    object EditRange2End: TEdit
      Left = 336
      Top = 52
      Width = 121
      Height = 23
      Hint = 'Range is pre-filled with current selection'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = '0'
    end
  end
end
