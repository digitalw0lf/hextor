object UpdaterForm: TUpdaterForm
  Left = 0
  Top = 0
  Caption = 'Software update'
  ClientHeight = 141
  ClientWidth = 455
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
  object Panel1: TPanel
    Left = 0
    Top = 100
    Width = 455
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
    DesignSize = (
      455
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
      Left = 368
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 455
    Height = 100
    Align = alClient
    Lines.Strings = (
      'Checking for available update...')
    ReadOnly = True
    TabOrder = 1
  end
  object mxWebUpdate1: TmxWebUpdate
    TagInfo.Author = 'mxAuthor'
    TagInfo.Email = 'mxEmail'
    TagInfo.ClientFileName = 'mxClientSideName'
    TagInfo.Download = 'mxDownload'
    TagInfo.ProductName = 'mxProduct'
    TagInfo.Redirection = 'mxRedirection'
    TagInfo.RunParameters = 'mxRunParameters'
    TagInfo.Version = 'mxVersion'
    ProductInfo.Version = '1.0'
    InfoCaption.OkButton = '&OK'
    InfoCaption.CancelButton = '&Cancel'
    InfoCaption.CheckForUpdate = 'C&heck for updates in the future'
    Options = [uoRunUpdate, uoOverwrite]
    Version = '1.21'
    HTTPPort = 80
    OnBeforeDownload = mxWebUpdate1BeforeDownload
    OnAfterDownload = mxWebUpdate1AfterDownload
    OnUpdateAvailable = mxWebUpdate1UpdateAvailable
    OnBeforeGetInfo = mxWebUpdate1BeforeGetInfo
    OnNoUpdateFound = mxWebUpdate1NoUpdateFound
    OnDownload = mxWebUpdate1Download
    OnDownloadError = mxWebUpdate1DownloadError
    Left = 56
    Top = 40
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 160
    Top = 40
  end
end
