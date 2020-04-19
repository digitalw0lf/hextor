object BitsEditorForm: TBitsEditorForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Bits Editor'
  ClientHeight = 143
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    466
    143)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 8
    Top = 8
    Width = 449
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnPaint = PaintBox1Paint
  end
  object BtnOk: TButton
    Left = 16
    Top = 106
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 110
    Top = 106
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EditDec: TEdit
    Left = 16
    Top = 79
    Width = 145
    Height = 21
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
    Text = 'EditDec'
  end
  object EditHex: TEdit
    Left = 176
    Top = 79
    Width = 145
    Height = 21
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
    Text = 'EditHex'
  end
end
