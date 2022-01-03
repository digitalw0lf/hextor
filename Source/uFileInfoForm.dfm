object FileInfoForm: TFileInfoForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'File information'
  ClientHeight = 401
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Full name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 61
    Height = 13
    Caption = 'Original size:'
  end
  object Label4: TLabel
    Left = 16
    Top = 104
    Width = 55
    Height = 13
    Caption = 'Edited size:'
  end
  object Label3: TLabel
    Left = 16
    Top = 144
    Width = 43
    Height = 13
    Caption = 'Created:'
  end
  object Label5: TLabel
    Left = 16
    Top = 168
    Width = 44
    Height = 13
    Caption = 'Modified:'
  end
  object EditFullName: TEdit
    Left = 16
    Top = 35
    Width = 409
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = 'EditFullName'
  end
  object EditOrigSize: TEdit
    Left = 96
    Top = 77
    Width = 209
    Height = 21
    ReadOnly = True
    TabOrder = 1
    Text = 'EditFullName'
  end
  object EditEditedSize: TEdit
    Left = 96
    Top = 101
    Width = 209
    Height = 21
    ReadOnly = True
    TabOrder = 2
    Text = 'EditFullName'
  end
  object BtnClose: TButton
    Left = 184
    Top = 360
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 8
    TabOrder = 3
  end
  object EditCreationTime: TEdit
    Left = 96
    Top = 141
    Width = 209
    Height = 21
    ReadOnly = True
    TabOrder = 4
    Text = 'EditFullName'
  end
  object EditModificationTime: TEdit
    Left = 96
    Top = 165
    Width = 209
    Height = 21
    ReadOnly = True
    TabOrder = 5
    Text = 'EditFullName'
  end
  object GBStreams: TGroupBox
    Left = 16
    Top = 208
    Width = 409
    Height = 137
    Caption = 'NTFS Streams'
    TabOrder = 6
    object StaticText1: TStaticText
      AlignWithMargins = True
      Left = 10
      Top = 23
      Width = 389
      Height = 104
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = 'N/A'
      TabOrder = 1
    end
    object StreamsPanel: TPanel
      Left = 2
      Top = 15
      Width = 405
      Height = 120
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object StreamsListView: TListView
        Left = 3
        Top = 8
        Width = 310
        Height = 104
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end
          item
            Caption = 'Size'
            Width = 80
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = BtnStreamEditClick
      end
      object BtnStreamAdd: TButton
        Left = 319
        Top = 8
        Width = 75
        Height = 25
        Hint = 'Create alternate stream in file'
        Caption = 'Add'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = BtnStreamAddClick
      end
      object BtnStreamEdit: TButton
        Left = 319
        Top = 35
        Width = 75
        Height = 25
        Hint = 'Edit selected stream'
        Caption = 'Edit'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = BtnStreamEditClick
      end
      object BtnStreamDelete: TButton
        Left = 319
        Top = 62
        Width = 75
        Height = 25
        Hint = 'Delete selected stream'
        Caption = 'Delete'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = BtnStreamDeleteClick
      end
    end
  end
end
