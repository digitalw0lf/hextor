object UpdaterForm: TUpdaterForm
  Left = 0
  Top = 0
  Caption = 'Software update'
  ClientHeight = 443
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 470
    Height = 443
    ActivePage = PgInfo
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    ExplicitWidth = 468
    ExplicitHeight = 374
    object PgProgress: TTabSheet
      Caption = 'PgProgress'
      object Panel1: TPanel
        Left = 0
        Top = 371
        Width = 462
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 369
        DesignSize = (
          462
          41)
        object Gauge1: TGauge
          Left = 8
          Top = 10
          Width = 341
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          ForeColor = clLime
          Progress = 0
          ExplicitWidth = 345
        end
        object BtnCancel: TButton
          Left = 368
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
        Height = 371
        Align = alClient
        Lines.Strings = (
          'Checking for available update...')
        ReadOnly = True
        TabOrder = 1
        ExplicitHeight = 369
      end
    end
    object PgInfo: TTabSheet
      Caption = 'PgInfo'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 371
        Width = 462
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 302
        ExplicitWidth = 460
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
          ExplicitLeft = 370
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
          ImageName = 'GoArrow'
          ImageMargins.Left = 10
          Images = MainForm.VirtualImageList1
          TabOrder = 1
          OnClick = BtnUpdateClick
          ExplicitLeft = 272
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
        ExplicitWidth = 460
        object LblNewVersion: TLabel
          Left = 16
          Top = 16
          Width = 89
          Height = 14
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
        Height = 266
        Align = alClient
        BevelOuter = bvLowered
        TabOrder = 2
        ExplicitWidth = 460
        ExplicitHeight = 197
        object WebBrowser1: TWebBrowser
          Left = 1
          Top = 1
          Width = 460
          Height = 264
          Align = alClient
          TabOrder = 0
          SelectedEngine = EdgeIfAvailable
          OnNewWindow3 = WebBrowser1NewWindow3
          ExplicitWidth = 458
          ExplicitHeight = 195
          ControlData = {
            4C00000009260000D41500000000000000000000000000000000000000000000
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
