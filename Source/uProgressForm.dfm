object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Please wait...'
  ClientHeight = 126
  ClientWidth = 393
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    393
    126)
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressGauge: TGauge
    Left = 24
    Top = 43
    Width = 345
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    ForeColor = clLime
    Progress = 0
  end
  object ProgressTextLabel: TLabel
    Left = 24
    Top = 24
    Width = 345
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ExplicitWidth = 353
  end
  object BusyLabel: TLabel
    Left = 24
    Top = 43
    Width = 345
    Height = 22
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '.|......'
    Layout = tlCenter
    Visible = False
  end
  object BtnAbort: TButton
    Left = 159
    Top = 84
    Width = 75
    Height = 25
    Anchors = [akTop]
    Caption = 'Abort'
    TabOrder = 0
    OnClick = BtnAbortClick
  end
  object Taskbar1: TTaskbar
    TaskBarButtons = <>
    TabProperties = []
    Left = 48
    Top = 72
  end
end
