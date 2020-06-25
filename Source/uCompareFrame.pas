{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uCompareFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Math,
  Generics.Collections, System.Types, Vcl.StdCtrls, Vcl.Buttons,
  Generics.Defaults,

  uHextorTypes, uEditorForm, uEditedData, uHextorGUI, Vcl.ComCtrls{, uLogFile};

type
  TCompareFrame = class(TFrame)
    CompareSelectFormPanel: TPanel;
    Label1: TLabel;
    CBCmpEditor1: TComboBox;
    Label2: TLabel;
    CBCmpEditor2: TComboBox;
    BtnCompare: TButton;
    BtnCancel: TButton;
    PageControl1: TPageControl;
    InitialTab: TTabSheet;
    ComparisonTab: TTabSheet;
    BtnStartCompare: TButton;
    DiffBar: TPaintBox;
    LblDiffsCount: TLabel;
    BtnCloseComparison: TSpeedButton;
    BtnRecompare: TButton;
    BtnAbort: TButton;
    BtnPrevDiff: TSpeedButton;
    BtnNextDiff: TSpeedButton;
    procedure DiffBarPaint(Sender: TObject);
    procedure DiffBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DiffBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnRecompareClick(Sender: TObject);
    procedure CBCmpEditor1Change(Sender: TObject);
    procedure BtnAbortClick(Sender: TObject);
    procedure BtnCloseComparisonClick(Sender: TObject);
    procedure BtnStartCompareClick(Sender: TObject);
    procedure BtnNextDiffClick(Sender: TObject);
  private type
    TDiff = TFileRange;
  private
    { Private declarations }
    MaxSize, ProcessedSize: TFilePointer;
    Diffs: TList<TDiff>;
    ScrBmp: TBitmap;
    FAborted: Boolean;
    FOurScrolling: Boolean;
    function AddDiff(const ARange: TFileRange): Integer;
    function ScrToPos(Y: Integer): TFilePointer;
    function PosToScr(P: TFilePointer): Integer;
    procedure UpdateInfo();
    procedure DrawDiffBarInternal();
    procedure UnsubscribeFromEditor(Editor: TEditorForm);
    procedure CloseComparison;
    procedure EditorVisRangeChanged(Sender: TEditorForm);
    procedure EditorByteColsChanged(Sender: TEditorForm);
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure DataChanged(Sender: TEditedData; Addr: TFilePointer; OldSize, NewSize: TFilePointer; Value: PByteArray);
  public
    { Public declarations }
    Editors: array[0..1] of TEditorForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure StartCompare(Editor1, Editor2: TEditorForm);
    function ShowCompareDialog(): TModalResult;
  end;

implementation

uses
  uMainForm, uBitmapFrame;

{$R *.dfm}

{ TCompareFrame }

function TCompareFrame.AddDiff(const ARange: TFileRange): Integer;
// Add diff range to Diffs list.
// Combine with last diff if adjusent.
var
  ADiff: TDiff;
begin
  if (Diffs.Count > 0) then
  begin
    Result := Diffs.Count - 1;
    ADiff := Diffs[Result];
    if ADiff.AEnd = ARange.Start then
    begin
      ADiff.AEnd := ARange.AEnd;
      Diffs[Result] := ADiff;
      Exit;
    end;
  end;

  ADiff.Start := ARange.Start;
  ADiff.AEnd := ARange.AEnd;
  Result := Diffs.Add(ADiff);
end;

procedure TCompareFrame.BtnAbortClick(Sender: TObject);
begin
  FAborted := True;
end;

procedure TCompareFrame.BtnRecompareClick(Sender: TObject);
begin
  if (Editors[0] <> nil) and (Editors[1] <> nil) then
    StartCompare(Editors[0], Editors[1]);
end;

procedure TCompareFrame.BtnStartCompareClick(Sender: TObject);
begin
  ShowCompareDialog();
end;

procedure TCompareFrame.CBCmpEditor1Change(Sender: TObject);
begin
  BtnCompare.Enabled := (CBCmpEditor1.ItemIndex <> CBCmpEditor2.ItemIndex);
end;

constructor TCompareFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  ScrBmp := TBitmap.Create();
  Diffs := TList<TDiff>.Create();
  for i:=0 to PageControl1.PageCount-1 do
    PageControl1.Pages[i].TabVisible := False;
  PageControl1.ActivePage := InitialTab;
end;

procedure TCompareFrame.DataChanged(Sender: TEditedData; Addr, OldSize,
  NewSize: TFilePointer; Value: PByteArray);
// Adjust regions positions
//var
//  i: Integer;
//  Range: TFileRange;
begin
  // TODO: implement this when there will be support for Diffs with different positions in compared files

//  if NewSize = OldSize then Exit;
//  for i:=0 to Diffs.Count-1 do
//  begin
//    Range := Diffs[i];
//    AdjustPositionInData(Range, Addr, OldSize, NewSize);
//    Diffs[i] := Range;
//  end;
end;

destructor TCompareFrame.Destroy;
begin
  ScrBmp.Free;
  Diffs.Free;
  inherited;
end;

procedure TCompareFrame.DiffBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DiffBarMouseMove(Sender, Shift, X, Y);
end;

procedure TCompareFrame.DiffBarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TFilePointer;
begin
  if Editors[0] = nil then Exit;
  if ssLeft in Shift then
  begin
    p := ScrToPos(Y) - Editors[0].VisibleBytesCount div 2;
    Editors[0].TopVisibleRow := p div Editors[0].ByteColumns;
  end;
end;

procedure TCompareFrame.DiffBarPaint(Sender: TObject);
var
  RAll, R: TRect;
begin
  if MaxSize = 0 then Exit;

  if (DiffBar.Width <> ScrBmp.Width) or (DiffBar.Height <> ScrBmp.Height) then
  begin
    ScrBmp.SetSize(DiffBar.Width, DiffBar.Height);
    DrawDiffBarInternal();
  end;

  DiffBar.Canvas.Draw(0, 0, ScrBmp);

  // Editor view frame
  RAll := Rect(0, 0, ScrBmp.Width, ScrBmp.Height);
  R.Left := 0;
  R.Right := RAll.Right - 1;
  R.Top := PosToScr(Editors[0].FirstVisibleAddr);
  R.Bottom := PosToScr(Editors[0].FirstVisibleAddr + Editors[0].VisibleBytesCount) - 1;
  DiffBar.Canvas.Pen.Color := clBlack;
  DrawEditorViewFrame(DiffBar.Canvas, R);
end;

procedure TCompareFrame.DrawDiffBarInternal;
// Draw Diff map to internal buffer
const
  ClrUnknown = clDkGray;
  ClrEqual   = clMoneyGreen;
  ClrDiff    = clRed;
var
  i, PrevY: Integer;
  RAll, R: TRect;
begin
//  StartTimeMeasure();
  RAll := Rect(0, 0, ScrBmp.Width, ScrBmp.Height);
  with ScrBmp.Canvas do
  begin
    // Unprocessed / Processed background
    Brush.Color := ClrUnknown;
    FillRect(RAll);

    R := RAll;
    R.Bottom := PosToScr(ProcessedSize);
    Brush.Color := ClrEqual;
    FillRect(R);

    // Differences
    Brush.Color := ClrDiff;
    PrevY := -1;
    for i:=0 to Diffs.Count-1 do
    begin
      R := RAll;
      R.Top := PosToScr(Diffs[i].Start);
      R.Bottom := PosToScr(Diffs[i].AEnd);
      if R.Bottom = R.Top then Inc(R.Bottom);
      if R.Bottom = PrevY then Continue;  // Do not paint single row multiple times
      PrevY := R.Bottom;
      FillRect(R);
    end;
  end;
//  EndTimeMeasure('DrawDiffBarInternal', True);
end;

procedure TCompareFrame.EditorByteColsChanged(Sender: TEditorForm);
// Sync col count
var
  n: Integer;
begin
  if Sender = Editors[0] then n := 0
  else if Sender = Editors[1] then n := 1
  else Exit;

  Editors[1-n].ByteColumnsSetting := Sender.ByteColumnsSetting;
end;

procedure TCompareFrame.EditorClosed(Sender: TEditorForm);
begin
  CloseComparison();
end;

procedure TCompareFrame.EditorGetTaggedRegions(Editor: TEditorForm; Start,
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
// Colorize diffs in text
var
  i: Integer;
begin
  for i:=0 to Diffs.Count-1 do
    Regions.AddRegion(Self, Diffs[i].Start, Diffs[i].AEnd, clNone, Color_DiffBg, clNone);
end;

procedure TCompareFrame.EditorVisRangeChanged(Sender: TEditorForm);
// Sunc scroll
var
  n: Integer;
begin
  if FOurScrolling then Exit;
  FOurScrolling := True;
  try
    if Sender = Editors[0] then n := 0
    else if Sender = Editors[1] then n := 1
    else Exit;

    Editors[1-n].BeginUpdate();
    try
      Editors[1-n].TopVisibleRow := Sender.TopVisibleRow;
      Editors[1-n].HorzScrollPos := Sender.HorzScrollPos;
    finally
      Editors[1-n].EndUpdate();
    end;
    DiffBarPaint(nil);
  finally
    FOurScrolling := False;
  end;
end;



function TCompareFrame.PosToScr(P: TFilePointer): Integer;
begin
  Result := Floor(P / MaxSize * DiffBar.Height);
  Result := BoundValue(Result, 0, DiffBar.Height);
end;

function TCompareFrame.ScrToPos(Y: Integer): TFilePointer;
begin
  Result := Floor(Y / DiffBar.Height * MaxSize);
  Result := BoundValue(Result, 0, MaxSize);
end;

function TCompareFrame.ShowCompareDialog: TModalResult;
var
  n: Integer;
begin
  CBCmpEditor1.Items.Clear();
  for n:=0 to MainForm.EditorCount-1 do
    CBCmpEditor1.Items.Add(MainForm.Editors[n].Caption);
  CBCmpEditor2.Items.Assign(CBCmpEditor1.Items);

  n := MainForm.GetEditorIndex(MainForm.ActiveEditor);
  if n = 0 then
  begin
    CBCmpEditor1.ItemIndex := 0;
    CBCmpEditor2.ItemIndex := 1;
  end
  else
  begin
    CBCmpEditor1.ItemIndex := n - 1;
    CBCmpEditor2.ItemIndex := n;
  end;

  with MakeFormWithContent(CompareSelectFormPanel, bsDialog, 'Compare') do
  begin
    Result := ShowModal();
    if Result <> mrOk then Exit;
  end;

  StartCompare(MainForm.Editors[CBCmpEditor1.ItemIndex], MainForm.Editors[CBCmpEditor2.ItemIndex]);
end;

procedure TCompareFrame.CloseComparison;
var
  i: Integer;
begin
  FAborted := True;
  for i:=0 to 1 do
  if Editors[i] <> nil then
  begin
    UnsubscribeFromEditor(Editors[i]);
    Editors[i] := nil;
  end;
  MaxSize := 0;
  Diffs.Clear();
  //Refresh();
  PageControl1.ActivePage := InitialTab;
  MainForm.ActiveEditor.WindowState := wsMaximized;
end;

procedure TCompareFrame.BtnCloseComparisonClick(Sender: TObject);
begin
  CloseComparison();
end;

procedure TCompareFrame.BtnNextDiffClick(Sender: TObject);
// Move caret in editors to next/prev difference
var
  n, CurDiff, Direction: Integer;
  Tmp: TFileRange;
begin
  if Diffs.Count = 0 then Exit;
  if MainForm.ActiveEditor = Editors[1] then
    n := 1
  else
    n := 0;
  Tmp := TFileRange.Create(Editors[n].CaretPos, Editors[n].CaretPos + 1);
  Direction := (Sender as TControl).Tag;
  // Find "current" difference under cursor
  if not Diffs.BinarySearch(Tmp, CurDiff, TComparer<TFileRange>.Construct(
    function (const Left, Right: TFileRange): Integer
    begin
      if Left.Intersects(Right) then Result := 0
      else Result := CompareValue(Left.Start, Right.Start);
    end))
  then
    if Direction = 1 then Dec(CurDiff);

  CurDiff := BoundValue(CurDiff + Direction, 0, Diffs.Count-1);

  for n:=0 to 1 do
    Editors[n].MoveCaret(Diffs[CurDiff].Start, []);
end;

procedure TCompareFrame.StartCompare(Editor1, Editor2: TEditorForm);
const
  BlockSize = 10*MByte;
var
  P: TFilePointer;
  Size1, Size2, MinSize: TFilePointer;
  Buf: array[0..1] of TBytes;
  i, Portion: Integer;
  MDIRect: TRect;
begin
  for i:=0 to 1 do
    UnsubscribeFromEditor(Editors[i]);

  Editors[0] := Editor1;
  Editors[1] := Editor2;
  PageControl1.ActivePage := ComparisonTab;

  // Show side-by-side
  Winapi.Windows.GetClientRect(MainForm.ClientHandle, MDIRect);
  Dec(MDIRect.Right, 4);
  Dec(MDIRect.Bottom, 4);
  for i:=0 to MainForm.EditorCount-1 do
    MainForm.Editors[i].WindowState := wsNormal;
  for i:=0 to 1 do
  begin
    Editors[i].WindowState := wsNormal;
    Editors[i].SetBounds((MDIRect.Width div 2) * i, 0, MDIRect.Width div 2, MDIRect.Height);
    Editors[i].ByteColumnsSetting := -1;
    Editors[i].BringToFront();

    Editors[i].OnClosed.Add(EditorClosed, Self);
    Editors[i].OnVisibleRangeChanged.Add(EditorVisRangeChanged, Self);
    Editors[i].OnByteColsChanged.Add(EditorByteColsChanged, Self);
    Editors[i].OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
    Editors[i].Data.OnDataChanged.Add(DataChanged, Self);
  end;

  Size1 := Editors[0].Data.GetSize();
  Size2 := Editors[1].Data.GetSize();
  MaxSize := Max(Size1, Size2);
  MinSize := Min(Size1, Size2);
  ProcessedSize := 0;
  Diffs.Clear();

  BtnRecompare.Enabled := False;
  BtnAbort.Visible := True;
  FAborted := False;
  P := 0;

  try
    while (P < MinSize) do
    begin
      // Get data block from sources
      Portion := Min(BlockSize, MinSize - P);
  //    StartTimeMeasure();
      for i:=0 to 1 do
        Buf[i] := Editors[i].Data.Get(P, Portion, False);
  //    EndTimeMeasure('Cmp.Read', True);

      // Compare
  //    StartTimeMeasure();
      for i:=0 to Min(Length(Buf[0]), Length(Buf[1]))-1 do
      begin
        if Buf[0][i] <> Buf[1][i] then
          AddDiff(TFileRange.Create(P+i, P+i+1));
      end;
  //    EndTimeMeasure('Cmp.Compare', True);

      P := P + Portion;
      ProcessedSize := P;

  //    StartTimeMeasure();
      UpdateInfo();
  //    EndTimeMeasure('Cmp.BarPaint', True);
      Application.ProcessMessages();
      if (FAborted) or (Application.Terminated) then Break;
    end;

    // Sources of different size
    if MinSize <> MaxSize then
    begin
      AddDiff(TFileRange.Create(MinSize, MaxSize));
      ProcessedSize := MaxSize;
    end;

    UpdateInfo();
  finally
    BtnRecompare.Enabled := True;
    BtnAbort.Visible := False;
  end;
end;

procedure TCompareFrame.UnsubscribeFromEditor(Editor: TEditorForm);
// Remove our callbacks from given Editor
begin
  if Editor = nil then Exit;
  Editor.OnClosed.Remove(Self);
  Editor.OnVisibleRangeChanged.Remove(Self);
  Editor.OnByteColsChanged.Remove(Self);
  Editor.OnGetTaggedRegions.Remove(Self);
  Editor.Data.OnDataChanged.Remove(Self);
end;

procedure TCompareFrame.UpdateInfo;
begin
  LblDiffsCount.Caption := 'Differences: ' + IntToStr(Diffs.Count);
  DrawDiffBarInternal();
  DiffBar.Refresh();
//  DiffBarPaint(nil);
  if Editors[0]<>nil then
    Editors[0].UpdatePanes();
  if Editors[1]<>nil then
    Editors[1].UpdatePanes();
end;

end.
