object UpdaterForm: TUpdaterForm
  Left = 0
  Top = 0
  Caption = 'Software update'
  ClientHeight = 294
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 470
    Height = 294
    ActivePage = PgInfo
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    ExplicitHeight = 245
    object PgProgress: TTabSheet
      Caption = 'PgProgress'
      ExplicitHeight = 214
      object Panel1: TPanel
        Left = 0
        Top = 222
        Width = 462
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 173
        DesignSize = (
          462
          41)
        object Gauge1: TGauge
          Left = 8
          Top = 10
          Width = 345
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          ForeColor = clLime
          Progress = 0
        end
        object BtnCancel: TButton
          Left = 372
          Top = 8
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 0
          OnClick = BtnCancelClick
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 462
        Height = 222
        Align = alClient
        Lines.Strings = (
          'Checking for available update...')
        ReadOnly = True
        TabOrder = 1
        ExplicitHeight = 173
      end
    end
    object PgInfo: TTabSheet
      Caption = 'PgInfo'
      ImageIndex = 1
      ExplicitHeight = 214
      object Panel2: TPanel
        Left = 0
        Top = 222
        Width = 462
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 173
        DesignSize = (
          462
          41)
        object LblDownloadSize: TLabel
          Left = 8
          Top = 16
          Width = 80
          Height = 13
          Caption = 'Download size: ?'
        end
        object BtnCancel2: TButton
          Left = 372
          Top = 8
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 0
        end
        object BtnUpdate: TButton
          Left = 274
          Top = 8
          Width = 89
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Update'
          Default = True
          ImageIndex = 8
          ImageMargins.Left = 10
          Images = MainForm.ImageList16
          TabOrder = 1
          OnClick = BtnUpdateClick
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 462
        Height = 105
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object LblNewVersion: TLabel
          Left = 16
          Top = 16
          Width = 89
          Height = 13
          Caption = 'LblNewVersion'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 105
        Width = 462
        Height = 117
        Align = alClient
        BevelOuter = bvLowered
        TabOrder = 2
        ExplicitLeft = 136
        ExplicitTop = 112
        ExplicitWidth = 185
        ExplicitHeight = 41
        object WebBrowser1: TWebBrowser
          Left = 1
          Top = 1
          Width = 460
          Height = 115
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 0
          ExplicitTop = 4
          ExplicitHeight = 137
          ControlData = {
            4C0000008B2F0000E30B00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 160
    Top = 40
  end
end
