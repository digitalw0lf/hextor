object FileConcatForm: TFileConcatForm
  Left = 0
  Top = 0
  Caption = 'Concatenate files'
  ClientHeight = 366
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    535
    366)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 157
    Height = 13
    Caption = 'Files (in order of concatenation):'
  end
  object SpeedButton1: TSpeedButton
    Left = 334
    Top = 12
    Width = 105
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 'Add files...'
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000A449A3F1F8FC
      D8EBF5BDDDEF9FCDE785BFE053A1CBC7D6DDEDF2F4A449A3A449A3A449A3A449
      A3A449A3A449A3A449A3A449A34CABD743ACD844B1DB48B9DF4BBFE32C8FC335
      8CB3388FB6389ECD379DCD369CCC45A3CF8DC6E1A449A3A449A3A449A33CA6D5
      6AD8EF64D5EE5FD2EC5ACFEB3396C74EA7C052AAC360CAE363CBE367CDE466C8
      E245A3CFA449A3A449A3A449A338A6D673DDF16CD9F066D6EE61D3ED3398C84A
      A6BF4CA8C257C5E05AC7E160C9E2D9B66C379CCCA449A3A449A3A449A33AA9D8
      7CE2F475DEF26EDBF068D7EF359ACA4CA7C04DA8C355C4E054C4E05AC7E1E1C4
      76389DCDA449A3A449A3A449A33BACDA84E7F67EE3F477DFF271DCF1379DCC52
      AAC152ABC45BC7E159C6E15BC7E1EEEFEF399FCEA449A3A449A3A449A33DB0DC
      8DEBF887E8F680E4F579E1F33AA1CE5AAEC45AAFC692DAEAAFE2EE90D9E9EEEF
      EF3BA0CFA449A3A449A3A449A33FB2DE95F0FA8FECF889E9F782E5F53EA5D064
      B2C690C9D7B7E6F0AF6839B5E5EFEEEFEF3CA2D0A449A3A449A3A449A340B5DF
      9CF4FC97F1FA91EEF98BEAF741A9D297CBD7B4D9E2AF6839AF6839BBE8F1A3E1
      EE3EA4D1A449A3A449A3A449A343B8E1A3F7FD9EF5FC99F2FB93EFFA7BC3DFB8
      DBE2B97439B97439B97439B974399E9F8E88ACB5A449A3A449A3A449A345BBE2
      A8FAFFA4F8FEA0F6FD9BF3FBA2D5E7D99839D99839D99839D99839D99839D998
      39D99839ECCB9CA449A3A449A354C3E7A9FBFFA9FAFFA5F9FEA1F6FD7FC9E1C1
      DFE4F7BC39F7BC39F7BC39DDD18893B291F7BC39F7BC39F7BC39A449A35BC5E7
      A9FBFFA9FBFFA9FBFFA7F9FE4EB7DAADD6DDC4E1E6FBC837FBC837D7F5F846AC
      D7A449A3A449A3FBC837A449A35CC6E7A9FBFFA9FBFF9DF3FC83E2F44FB9DB94
      CCD5CAEFF3DEF9FADAB533DDF7F948AED8A449A3A449A3F5ECCBA449A35FC8E8
      8DE6F57EDAF07EDAF095E3F496D4DEC4F7FAC8FBFDD7FAFBE2F9FACAF2F858B5
      DBA449A3A449A3A449A3A449A379D0EB69C9E95FC3E65EC1E55CC0E45ABEE358
      BCE155BAE054B8DF51B6DD5CB9DE9AD2E9A449A3A449A3A449A3}
    OnClick = SpeedButton1Click
    ExplicitLeft = 264
  end
  object Label2: TLabel
    Left = 8
    Top = 229
    Width = 91
    Height = 13
    Hint = 
      'CRC file is used to check if combined file is equal to original ' +
      'file before splitting. CRC file format is compatible with Total ' +
      'Commander.'
    Anchors = [akLeft, akBottom]
    Caption = 'CRC file (optional):'
    ParentShowHint = False
    ShowHint = True
    ExplicitTop = 304
  end
  object SpeedButton2: TSpeedButton
    Left = 504
    Top = 226
    Width = 21
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = '...'
    OnClick = SpeedButton2Click
    ExplicitLeft = 434
    ExplicitTop = 301
  end
  object Label3: TLabel
    Left = 8
    Top = 277
    Width = 42
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Save as:'
    ParentShowHint = False
    ShowHint = False
    ExplicitTop = 352
  end
  object SpeedButton3: TSpeedButton
    Left = 504
    Top = 274
    Width = 21
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = '...'
    OnClick = SpeedButton3Click
    ExplicitLeft = 434
    ExplicitTop = 349
  end
  object BtnClearList: TSpeedButton
    Left = 446
    Top = 12
    Width = 80
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 'Clear list'
    OnClick = BtnClearListClick
    ExplicitLeft = 376
  end
  object ImageProxy1: THintedImageProxy
    Left = 107
    Top = 228
    Width = 16
    Height = 16
    Image = MainForm.HintImage
    ImageIndex = 0
    HintFmt = 
      'CRC file is used to check if combined file is equal to original ' +
      'file before splitting. <br>CRC file format is compatible with To' +
      'tal Commander.'
  end
  object EditResultFileName: TComboBox
    Left = 128
    Top = 274
    Width = 367
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
  end
  object EditCRCFileName: TComboBox
    Left = 128
    Top = 226
    Width = 367
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object BtnOk: TButton
    Left = 162
    Top = 324
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Concatenate'
    Default = True
    TabOrder = 2
    OnClick = BtnOkClick
    ExplicitLeft = 136
    ExplicitTop = 399
  end
  object BtnCancel: TButton
    Left = 302
    Top = 324
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 258
    ExplicitTop = 399
  end
  object FilesList: TListBox
    Left = 8
    Top = 48
    Width = 519
    Height = 158
    DragMode = dmAutomatic
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 4
    OnDragOver = FilesListDragOver
    OnMouseDown = FilesListMouseDown
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 96
    Top = 88
  end
  object DropFileCatcher1: TDropFileCatcher
    Control = Owner
    OnDropFiles = DropFileCatcher1DropFiles
    Left = 176
    Top = 88
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'crc'
    Filter = 'CRC file|*.crc'
    Left = 400
    Top = 216
  end
  object SaveDialog1: TSaveDialog
    Left = 400
    Top = 272
  end
  object PopupMenu1: TPopupMenu
    Left = 320
    Top = 128
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object Movetotop1: TMenuItem
      Caption = 'Move to top'
      OnClick = Movetotop1Click
    end
    object Movetobottom1: TMenuItem
      Caption = 'Move to bottom'
      OnClick = Movetobottom1Click
    end
  end
end
