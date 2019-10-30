object StructFrame: TStructFrame
  Left = 0
  Top = 0
  Width = 297
  Height = 629
  TabOrder = 0
  DesignSize = (
    297
    629)
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 281
    Height = 177
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'int8 x;'
      'uint16 a, b;'
      'float list[2];'
      '{'
      '  int32 f1;'
      '  double f2;'
      '} rec;')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Interpret'
    TabOrder = 1
    OnClick = Button1Click
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 264
    Width = 281
    Height = 257
    Anchors = [akLeft, akTop, akRight]
    Indent = 19
    TabOrder = 2
  end
end
