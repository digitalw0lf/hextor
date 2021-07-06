{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
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
  TComparedRange = record
    Differs: Boolean;
    Range: array[0..1] of TFileRange;
  end;

  TFilePointerPair = array[0..1] of TFilePointer;

  TDataComparer = class
  protected
    Ptr: TFilePointerPair;  // Current position in files
    // Cached data, up to MaxReadAhead bytes
    Cache: array[0..1] of TBytes;
    CacheAddr: TFilePointerPair;
    procedure AddRange(Differs: Boolean; Size0, Size1: TFilePointer);
    procedure ForceCache(DataIndex: Integer; Start, AEnd: TFilePointer);
    function GetByte(n: Integer; Addr: TFilePointer): Byte;
    function GetBytes(n: Integer; Addr: TFilePointer; Size: Integer): TBytes;
    function FindNextCommonSeq(var SyncPtrs: TFilePointerPair): Boolean;
  public
    // Settings
    MaxWorkTime: Cardinal;   // Milliseconds
    SyncBlockSize: Integer;  // Detect common sequence if it is at least this size. Must be a power of 2
    MaxReadAhead: Integer;   // Search ahead for sequence resync up to this distance
    // Data
    Data: array[0..1] of TEditedData;
    // Result
    Working: Boolean;
    Progress: Double;
    Ranges: TList<TComparedRange>;
    procedure Clear();
    procedure StartCompare(AData0, AData1: TEditedData);
    procedure Work();
    constructor Create();
    destructor Destroy(); override;
    function DiffCount(): Int64;
    function ProcessedTill(DataIndex: Integer): TFilePointer;
    function DataIndex(AData: TEditedData): Integer;
    function GetRangeIndex(DataIndex: Integer; Pos: TFilePointer): Integer;
    function GetCorrespondingPosition(FromData: Integer; FromPos: TFilePointer): TFilePointer;
    procedure AdjustRanges(DataIndex: Integer; Addr: TFilePointer; OldSize, NewSize: TFilePointer);
  end;

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
    Timer1: TTimer;
    Label3: TLabel;
    CBSyncBlockSize: TComboBox;
    Label4: TLabel;
    ImageProxy1: THintedImageProxy;
    CBRange1: TCheckBox;
    EditRange1Start: TEdit;
    LblRange1Start: TLabel;
    LblRange1End: TLabel;
    EditRange1End: TEdit;
    CBRange2: TCheckBox;
    EditRange2Start: TEdit;
    LblRange2Start: TLabel;
    LblRange2End: TLabel;
    EditRange2End: TEdit;
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
    procedure Timer1Timer(Sender: TObject);
    procedure CBRange1Click(Sender: TObject);
    procedure CBRange2Click(Sender: TObject);
  private type
    TDiff = TFileRange;
  private
    { Private declarations }
    DrawnSize: TFilePointer;
    ScrBmp: TBitmap;
    FOurScrolling: Boolean;
    StartTime: Cardinal;
    function ScrToPos(Y: Integer): TFilePointer;
    function PosToScr(P: TFilePointer): Integer;
    function DiffBarRect(EditorIndex: Integer): TRect;
    procedure UpdateInfo();
    procedure DrawDiffBarInternal();
    procedure CompareDone();
    procedure UnsubscribeFromEditor(Editor: TEditorForm);
    procedure CloseComparison;
    procedure EditorVisRangeChanged(Sender: TEditorForm);
    procedure EditorSelectionChanged(Sender: TEditorForm);
    procedure EditorByteColsChanged(Sender: TEditorForm);
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure DataChanged(Sender: TEditedData; Addr: TFilePointer; OldSize, NewSize: TFilePointer; Value: PByteArray);
    function EditorIndex(Editor: TEditorForm): Integer;
  public
    { Public declarations }
    Editors: array[0..1] of TEditorForm;
    Comparer: TDataComparer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure StartCompare(Editor1, Editor2: TEditorForm; SyncBlockSize: Integer);
    function ShowCompareDialog(): TModalResult;
  end;

implementation

uses
  uMainForm, uBitmapFrame;

{$R *.dfm}

{ TCompareFrame }

procedure TCompareFrame.BtnAbortClick(Sender: TObject);
begin
  Comparer.Working := False;
  CompareDone();
end;

procedure TCompareFrame.BtnRecompareClick(Sender: TObject);
begin
  if (Editors[0] <> nil) and (Editors[1] <> nil) then
    StartCompare(Editors[0], Editors[1], Comparer.SyncBlockSize);
end;

procedure TCompareFrame.BtnStartCompareClick(Sender: TObject);
begin
  ShowCompareDialog();
end;

procedure TCompareFrame.CBCmpEditor1Change(Sender: TObject);
begin
  BtnCompare.Enabled := (CBCmpEditor1.ItemIndex <> CBCmpEditor2.ItemIndex);
end;

procedure TCompareFrame.CBRange1Click(Sender: TObject);
begin
  LblRange1Start.Enabled := CBRange1.Checked;
  EditRange1Start.Enabled := CBRange1.Checked;
  LblRange1End.Enabled := CBRange1.Checked;
  EditRange1End.Enabled := CBRange1.Checked;
end;

procedure TCompareFrame.CBRange2Click(Sender: TObject);
begin
  LblRange2Start.Enabled := CBRange2.Checked;
  EditRange2Start.Enabled := CBRange2.Checked;
  LblRange2End.Enabled := CBRange2.Checked;
  EditRange2End.Enabled := CBRange2.Checked;
end;

constructor TCompareFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  ScrBmp := TBitmap.Create();
  Comparer := TDataComparer.Create();
  for i:=0 to PageControl1.PageCount-1 do
    PageControl1.Pages[i].TabVisible := False;
  PageControl1.ActivePage := InitialTab;
end;

procedure TCompareFrame.DataChanged(Sender: TEditedData; Addr, OldSize,
  NewSize: TFilePointer; Value: PByteArray);
// Adjust regions positions
var
  n: Integer;
begin
  n := Comparer.DataIndex(Sender);
  if n < 0 then Exit;
  Comparer.AdjustRanges(n, Addr, OldSize, NewSize);
end;

destructor TCompareFrame.Destroy;
begin
  ScrBmp.Free;
  Comparer.Free;
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
  n: Integer;
begin
  if Editors[0] = nil then Exit;
  if ssLeft in Shift then
  begin
    if X < DiffBar.Width div 2 then n := 0
                               else n := 1;
    p := ScrToPos(Y) - Editors[n].VisibleBytesCount div 2;
    Editors[n].TopVisibleRow := p div Editors[n].ByteColumns;
  end;
end;

procedure TCompareFrame.DiffBarPaint(Sender: TObject);
var
  RAll, R: TRect;
  Redraw: Boolean;
  n: Integer;
begin
  Redraw := False;
  if (DiffBar.Width <> ScrBmp.Width) or (DiffBar.Height <> ScrBmp.Height) then
  begin
    ScrBmp.SetSize(DiffBar.Width, DiffBar.Height);
    Redraw := True;
  end;

  if Redraw then
    DrawDiffBarInternal();

  DiffBar.Canvas.Draw(0, 0, ScrBmp);

  // Editor view frames
  for n := 0 to 1 do
  begin
    RAll := DiffBarRect(n);
    R.Left := RAll.Left;
    R.Right := RAll.Right - 1;
    R.Top := PosToScr(Editors[n].FirstVisibleAddr);
    R.Bottom := PosToScr(Editors[n].FirstVisibleAddr + Editors[n].VisibleBytesCount) - 1;
    DiffBar.Canvas.Pen.Color := clBlack;
    DrawEditorViewFrame(DiffBar.Canvas, R);
  end;
end;

function TCompareFrame.DiffBarRect(EditorIndex: Integer): TRect;
begin
  Result.Top := 0;
  Result.Bottom := ScrBmp.Height;
  if EditorIndex = 0 then
  begin
    Result.Left := 0;
    Result.Right := ScrBmp.Width * 4 div 10;
  end
  else
  begin
    Result.Left := ScrBmp.Width * 6 div 10;
    Result.Right := ScrBmp.Width;
  end;
end;

procedure TCompareFrame.DrawDiffBarInternal;
// Draw Diff map to internal buffer
const
  ClrUnknown = clDkGray;
  ClrEqual   = TColor($C0DCC0);
  ClrMixed   = TColor($6070E0);
  ClrDiff    = TColor($0000FF);
var
  n, i, SY, y1, y2, y: Integer;
  RAll: TRect;
  Range: TComparedRange;
  w: Int64;
  EqualWeight, DiffWeight: TArray<Int64>;  // Size of Equal and Diff data in every pixel
  c: TColor;
begin
  SY := ScrBmp.Height;
  SetLength(EqualWeight, SY);
  SetLength(DiffWeight, SY);

  RAll := Rect(0, 0, ScrBmp.Width, ScrBmp.Height);
  with ScrBmp.Canvas do
  begin
    // Unprocessed background
    Brush.Color := ClrUnknown;
    FillRect(RAll);
  end;

  if (Comparer.Data[0] = nil) or (Comparer.Data[1] = nil) then Exit;
  DrawnSize := Max(Comparer.Data[0].GetSize(), Comparer.Data[1].GetSize());

//  StartTimeMeasure();
  for n := 0 to 1 do
  begin
    ZeroMemory(@EqualWeight[0], Length(EqualWeight) * SizeOf(EqualWeight[0]));
    ZeroMemory(@DiffWeight[0], Length(DiffWeight) * SizeOf(DiffWeight[0]));
    // Ranges
    for i:=0 to Comparer.Ranges.Count-1 do
    begin
      Range := Comparer.Ranges[i];
      y1 := PosToScr(Range.Range[n].Start);
      y2 := PosToScr(Range.Range[n].AEnd);
      w := Max(Range.Range[n].Size, 1);
      for y := y1 to y2 do
        if Range.Differs then
          Inc(DiffWeight[y], w)
        else
          Inc(EqualWeight[y], w);
    end;

    // Draw color bar according to DiffMarks
    RAll := DiffBarRect(n);
    RandSeed := 0;
    for y := 0 to SY - 1 do
    begin
      if (EqualWeight[y] = 0) and (DiffWeight[y] = 0) then
        c := ClrUnknown
      else
      if (DiffWeight[y] = 0) then
        c := ClrEqual
      else
      if (EqualWeight[y] = 0) then
        c := ClrDiff
      else
      begin
        {// If we have single Diff surrounded by Equal, or single Equal surrounded by Diff,
        // we don't want to visually miss it
        if ((y-1 < 0) or (DiffWeight[y-1] = 0)) and ((y+1 >= SY) or (DiffWeight[y+1] = 0)) then
          c := ClrDiff
        else
        if ((y-1 < 0) or (EqualWeight[y-1] = 0)) and ((y+1 >= SY) or (EqualWeight[y+1] = 0)) then
          c := ClrEqual
        else
        begin
          // When we have both Diff and Equal ranges in this pixel,
          // Colorize it pseudo-randomly, proportionally to Diff/Equal ratio
          if Random(Min(EqualWeight[y], MaxInt)) < Random(Min(DiffWeight[y], MaxInt)) then
            c := ClrDiff
          else
            c := ClrEqual;
        end;}
        c := ClrMixed;
      end;

      with ScrBmp.Canvas do
      begin
        Pen.Color := c;
        MoveTo(RAll.Left, y);
        LineTo(RAll.Right, y);
      end;
    end;
  end;
//  EndTimeMeasure('DrawDiffBarInternal', True);
end;

procedure TCompareFrame.EditorByteColsChanged(Sender: TEditorForm);
// Sync col count
var
  n: Integer;
begin
  n := EditorIndex(Sender);
  if n < 0 then Exit;

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
  i, i1, i2, n: Integer;
begin
  n := EditorIndex(Editor);
  if n < 0 then Exit;
  i1 := Comparer.GetRangeIndex(n, Start);
  i2 := Comparer.GetRangeIndex(n, AEnd);
  for i:=i1 to Min(i2, Comparer.Ranges.Count - 1) do
    if Comparer.Ranges[i].Differs then
      Regions.AddRegion(Self, Comparer.Ranges[i].Range[n].Start, Comparer.Ranges[i].Range[n].AEnd, clNone, Color_DiffBg, Color_DiffFr);
end;

function TCompareFrame.EditorIndex(Editor: TEditorForm): Integer;
begin
  if Editor = Editors[0] then Result := 0
  else if Editor = Editors[1] then Result := 1
  else Result := -1;
end;

procedure TCompareFrame.EditorSelectionChanged(Sender: TEditorForm);
// Sync caret pos
var
  n: Integer;
  CaretPos: TFilePointer;
begin
  if FOurScrolling then Exit;
  FOurScrolling := True;
  try
    n := EditorIndex(Sender);
    if n < 0 then Exit;

    Editors[1-n].BeginUpdate();
    try
      CaretPos := Sender.CaretPos;
      CaretPos := Comparer.GetCorrespondingPosition(n, CaretPos);
      Editors[1-n].MoveCaret(CaretPos, []);
    finally
      Editors[1-n].EndUpdate();
    end;
  finally
    FOurScrolling := False;
  end;
end;

procedure TCompareFrame.EditorVisRangeChanged(Sender: TEditorForm);
// Sync scroll
var
  n: Integer;
  CenterPos: TFilePointer;
begin
  if not FOurScrolling then
  begin
    FOurScrolling := True;
    try
      n := EditorIndex(Sender);
      if n < 0 then Exit;

      Editors[1-n].BeginUpdate();
      try
        CenterPos := Sender.FirstVisibleAddr() + Sender.VisibleBytesCount() div 2;
        CenterPos := Comparer.GetCorrespondingPosition(n, CenterPos);
        Editors[1-n].TopVisibleRow := CenterPos div Editors[1-n].ByteColumns - Editors[1-n].GetVisibleRowsCount() div 2;

        Editors[1-n].HorzScrollPos := Sender.HorzScrollPos;
      finally
        Editors[1-n].EndUpdate();
      end;
    finally
      FOurScrolling := False;
    end;
  end;
  DiffBarPaint(nil);
end;



function TCompareFrame.PosToScr(P: TFilePointer): Integer;
begin
  if DrawnSize = 0 then Exit(0);
  Result := P * DiffBar.Height div DrawnSize;
  Result := BoundValue(Result, 0, DiffBar.Height - 1);
end;

function TCompareFrame.ScrToPos(Y: Integer): TFilePointer;
begin
  if DiffBar.Height = 0 then Exit(0);
  Result := Y * DrawnSize div DiffBar.Height;
  Result := BoundValue(Result, 0, DrawnSize);
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

  StartCompare(MainForm.Editors[CBCmpEditor1.ItemIndex],
    MainForm.Editors[CBCmpEditor2.ItemIndex], StrToInt(CBSyncBlockSize.Text));
end;

procedure TCompareFrame.CloseComparison;
var
  i: Integer;
begin
  for i:=0 to 1 do
  begin
    UnsubscribeFromEditor(Editors[i]);
    Editors[i] := nil;
  end;
  DrawnSize := 0;
  if Comparer.Working then
    CompareDone;
  Comparer.Clear();
  PageControl1.ActivePage := InitialTab;
  MainForm.ActiveEditor.WindowState := wsMaximized;
end;

procedure TCompareFrame.CompareDone;
begin
  WriteLogFmt('Compare done, %d ms', [GetTickCount() - StartTime]);
  BtnRecompare.Enabled := True;
  BtnAbort.Visible := False;
end;

procedure TCompareFrame.BtnCloseComparisonClick(Sender: TObject);
begin
  CloseComparison();
end;

procedure TCompareFrame.BtnNextDiffClick(Sender: TObject);
// Move caret in editors to next/prev difference
var
  n, i, CurRange, Direction: Integer;
begin
  if Comparer.Ranges.Count <= 1 then Exit;
  n := EditorIndex(MainForm.ActiveEditor);
  if n < 0 then Exit;

  CurRange := Comparer.GetRangeIndex(n, Editors[n].CaretPos);
  Direction := (Sender as TControl).Tag;

  repeat
    Inc(CurRange, Direction);
  until (CurRange < 0) or (CurRange >= Comparer.Ranges.Count) or (Comparer.Ranges[CurRange].Differs);

  if (CurRange >= 0) and (CurRange < Comparer.Ranges.Count) then
  begin
    FOurScrolling := True;
    try
      for i := 0 to 1 do
        Editors[i].SelectAndShow(Comparer.Ranges[CurRange].Range[i].Start, Comparer.Ranges[CurRange].Range[i].Start);
    finally
      FOurScrolling := False;
    end;
  end;
end;

procedure TCompareFrame.StartCompare(Editor1, Editor2: TEditorForm; SyncBlockSize: Integer);
var
  i: Integer;
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
    Editors[i].OnSelectionChanged.Add(EditorSelectionChanged, Self);
    Editors[i].OnByteColsChanged.Add(EditorByteColsChanged, Self);
    Editors[i].OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
    Editors[i].Data.OnDataChanged.Add(DataChanged, Self);
  end;

  WriteLog('Start compare: ' + Editors[0].Data.DataSource.DisplayName + ' - ' + Editors[1].Data.DataSource.DisplayName);
  StartTime := GetTickCount();

  Comparer.SyncBlockSize := SyncBlockSize;
  Comparer.StartCompare(Editors[0].Data, Editors[1].Data);

  BtnRecompare.Enabled := False;
  BtnAbort.Visible := True;
end;

procedure TCompareFrame.Timer1Timer(Sender: TObject);
begin
  if Comparer.Working then
  begin
    Comparer.Work();

    UpdateInfo();

    if not Comparer.Working then
      CompareDone();
  end;
end;

procedure TCompareFrame.UnsubscribeFromEditor(Editor: TEditorForm);
// Remove our callbacks from given Editor
begin
  if Editor = nil then Exit;
  Editor.RemoveEventListener(Self);
end;

procedure TCompareFrame.UpdateInfo;
begin
  LblDiffsCount.Caption := 'Differences: ' + IntToStr(Comparer.DiffCount());
  DrawDiffBarInternal();
  DiffBar.Refresh();
  if Editors[0]<>nil then
    Editors[0].UpdatePanes();
  if Editors[1]<>nil then
    Editors[1].UpdatePanes();
end;

{ TDataComparer }

procedure TDataComparer.AddRange(Differs: Boolean; Size0, Size1: TFilePointer);
// Add different/identical range.
// Combine with previous range if same type.
var
  Range: TComparedRange;
begin
  if (Ranges.Count > 0) then
  begin
    Range := Ranges.Last;
    if Range.Differs = Differs then
    begin
      // Enlarge last range
      Range.Range[0].AEnd := Range.Range[0].AEnd + Size0;
      Range.Range[1].AEnd := Range.Range[1].AEnd + Size1;
      Ranges[Ranges.Count-1] := Range;
      Exit;
    end;
  end;
  // Add new range
  if (Ranges.Count > 0) then
  begin
    Range.Range[0].Start :=Ranges.Last.Range[0].AEnd;
    Range.Range[1].Start :=Ranges.Last.Range[1].AEnd;
  end
  else
  begin
    Range.Range[0].Start := 0;
    Range.Range[1].Start := 0;
  end;
  Range.Differs := Differs;
  Range.Range[0].Size := Size0;
  Range.Range[1].Size := Size1;

  Ranges.Add(Range);
end;

procedure TDataComparer.AdjustRanges(DataIndex: Integer; Addr, OldSize,
  NewSize: TFilePointer);
// Adjust found ranges when bytes inserted/deleted in original data
var
  i1, i: Integer;
  R: TComparedRange;
begin
  i1 := GetRangeIndex(DataIndex, Addr);
  for i := i1 to Ranges.Count - 1 do
  begin
    R := Ranges[i];
    AdjustPositionInData(R.Range[DataIndex], Addr, OldSize, NewSize);
    Ranges[i] := R;
  end;
end;

procedure TDataComparer.Clear;
var
  n: Integer;
begin
  Working := False;
  Ranges.Clear();
  for n := 0 to 1 do
  begin
    Data[n] := nil;
    Cache[n] := nil;
    CacheAddr[n] := 0;
  end;
end;

constructor TDataComparer.Create;
begin
  MaxWorkTime := 100;
  SyncBlockSize := 16;
  MaxReadAhead := 10*MByte;
  Ranges := TList<TComparedRange>.Create();
end;

function TDataComparer.DataIndex(AData: TEditedData): Integer;
begin
  if AData = Data[0] then Result := 0
  else if AData = Data[1] then Result := 1
  else Result := -1;
end;

destructor TDataComparer.Destroy;
begin
  Ranges.Free;
  inherited;
end;

function TDataComparer.DiffCount: Int64;
begin
  Result := Ranges.Count div 2;
  if Ranges.First.Differs and Ranges.Last.Differs then
    Inc(Result);
end;

type
  TRollingChecksum = Cardinal;

procedure AddToChecksum(var Sum: TRollingChecksum; AByte:Byte);
begin
  Sum:=Sum xor AByte;
  Sum:=(Sum shl 1) or (Sum shr 31);  // ROL
end;

procedure SubFromChecksum(var Sum: TRollingChecksum; AByte:Byte; WindowSize: Integer);
// WindowSize must be a power of 2
begin
  if WindowSize < 32 then
    Sum := Sum xor (AByte shl WindowSize)
  else
    Sum:=Sum xor AByte;
end;

function CompareBytes(const b1, b2: TBytes): Boolean;
begin
  Result := (Length(b1) = Length(b2)) and
            CompareMem(@b1[0], @b2[0], Length(b1));
end;

function EqualBytesCount( p1, p2: PByte; MaxLength: Integer): Integer;
// Find a position at which two memory blocks differs
var
  i: Integer;
begin
  for i := 0 to MaxLength - 1 do
    if p1[i] <> p2[i] then Exit(i);
  Result := MaxLength;
end;

function TDataComparer.FindNextCommonSeq(var SyncPtrs: TFilePointerPair): Boolean;
// Find next common sequence after current positions (Ptr[0], Ptr[1]).
// Returns a beginning of common sequence; entire sequence may be larger
// than returned CommonSeqSize.
var
  n, i: Integer;
  AEnd: TFilePointerPair;
  p: array[0..1] of TFilePointer;  // Current window: [p-SyncBlockSize .. p)
  CS: array[0..1] of TRollingChecksum;  // Checksum of current window
  CSPos: array[0..1] of TDictionary<TRollingChecksum, TArray<TFilePointer>>;  // Checksums of prev windows
  Ptrs: TArray<TFilePointer>;
  FoundMatch: TFilePointerPair;
begin
  for n := 0 to 1 do
  begin
    p[n] := Ptr[n];
    CS[n] := 0;
    CSPos[n] := TDictionary<TRollingChecksum, TArray<TFilePointer>>.Create();
    // Search either till end of file or MaxReadAhead from curent position
    AEnd[n] := Min(Data[n].GetSize, Ptr[n] + MaxReadAhead);
  end;

  try
    while True do
    begin
      // Reached the end of both files?
      if (p[0] = AEnd[0]) and (p[1] = AEnd[1]) then
      begin
        SyncPtrs[0] := p[0];
        SyncPtrs[1] := p[1];
        Exit(False);
      end;

      // Advance window by 1 byte in both streams
      for n := 0 to 1 do
        if p[n] < AEnd[n] then
        begin
          // Update rolling checksum
          if p[n] - SyncBlockSize >= Ptr[n] then
            SubFromChecksum(CS[n], GetByte(n, p[n] - SyncBlockSize), SyncBlockSize);
          AddToChecksum(CS[n], GetByte(n, p[n]));
          Inc(p[n]);
          // Add checksum and position to dictionary
          if p[n] - Ptr[n] >= SyncBlockSize then
            // When in large changed block, only add to dictionary once in SyncBlockSize bytes
            if (p[n] - Ptr[n] <= 256) or   // Add every address at the beginning of changed range
               (p[n] mod SyncBlockSize = 0) then
            begin
              if not CSPos[n].TryGetValue(CS[n], Ptrs) then
                Ptrs := nil;
              Ptrs := Ptrs + [p[n] - SyncBlockSize];
              CSPos[n].AddOrSetValue(CS[n], Ptrs);
            end;
        end;

      SyncPtrs[0] := -1;
      // Compare (by checksum) current window in one stream with all accumulated
      // windows from another stream
      for n := 0 to 1 do
      begin
        if p[n] - Ptr[n] >= SyncBlockSize then
        begin
          if CSPos[1-n].TryGetValue(CS[n], Ptrs) then
          begin
            // Found checksum match, compare actual data of all possible matches
            for i := 0 to Length(Ptrs) - 1 do
              if CompareBytes(GetBytes(n, p[n] - SyncBlockSize, SyncBlockSize),
                              GetBytes(1-n, Ptrs[i], SyncBlockSize)) then
              begin
                // Found matching block
                FoundMatch[n] := p[n] - SyncBlockSize;
                FoundMatch[1-n] := Ptrs[i];
                // If at this point we found cross-matches in both directions
                // (e.g. "abcd0" and "xy000"), select one with lower addresses
                if (SyncPtrs[0] < 0) or (FoundMatch[0] + FoundMatch[1] < SyncPtrs[0] + SyncPtrs[1]) then
                begin
                  SyncPtrs := FoundMatch;
                end;
                Break;
              end;
          end;
        end;
      end;
      if SyncPtrs[0] >= 0 then
        Exit(True);
    end;
  finally
    for n := 0 to 1 do
      CSPos[n].Free;
    // Maybe we missed some matching bytes before this block?
    while (SyncPtrs[0] > Ptr[0]) and (SyncPtrs[1] > Ptr[1]) and
          (GetByte(0, SyncPtrs[0] - 1) = GetByte(1, SyncPtrs[1] - 1)) do
    begin
      Dec(SyncPtrs[0]);
      Dec(SyncPtrs[1]);
    end;
  end;
end;

procedure TDataComparer.ForceCache(DataIndex: Integer; Start, AEnd: TFilePointer);
// Check that requested range is in our cache.
// If not, read up to MaxReadAhead bytes starting from current Ptr.
// Comparison algorithm always requests data from this range.
var
  Size: Integer;
begin
  if (Start < CacheAddr[DataIndex]) or (AEnd > CacheAddr[DataIndex] + Length(Cache[DataIndex])) then
  // Cache miss
  begin
    if (Start < Ptr[DataIndex]) or (AEnd > Ptr[DataIndex] + MaxReadAhead) or (AEnd > Data[DataIndex].GetSize()) then
      raise Exception.Create('Error during file compare');  // Check for some bugs?
    CacheAddr[DataIndex] := Ptr[DataIndex];
    Size := Min(MaxReadAhead, Data[DataIndex].GetSize() - CacheAddr[DataIndex]);
    Cache[DataIndex] := Data[DataIndex].Get(CacheAddr[DataIndex], Size);
  end;
end;

function TDataComparer.GetByte(n: Integer; Addr: TFilePointer): Byte;
// Get byte from Data, using cache window of MaxReadAhead
begin
  ForceCache(n, Addr, Addr + 1);

  Result := Cache[n][Addr - CacheAddr[n]];
end;

function TDataComparer.GetBytes(n: Integer; Addr: TFilePointer;
  Size: Integer): TBytes;
begin
  ForceCache(n, Addr, Addr + Size);

  Result := Copy(Cache[n], Addr - CacheAddr[n], Size)
end;

function TDataComparer.GetCorrespondingPosition(FromData: Integer;
  FromPos: TFilePointer): TFilePointer;
// Given data index (0 or 1) and position in that data,
// returns corresponding position in another data
var
  i: Integer;
begin
  if FromPos = Data[FromData].GetSize() then
    Exit(Data[1 - FromData].GetSize());
  i := GetRangeIndex(FromData, FromPos);
  if i >= Ranges.Count then
    Exit(FromPos);  // Maybe this position is not processed yet

  if Ranges[i].Differs then
  begin
    if Ranges[i].Range[FromData].Size = 0 then
      Result := Ranges[i].Range[1 - FromData].Start
    else  // Linear interpolation
      Result := Floor((FromPos - Ranges[i].Range[FromData].Start) / Ranges[i].Range[FromData].Size *
                      Ranges[i].Range[1 - FromData].Size)  + Ranges[i].Range[1 - FromData].Start;
  end
  else
  begin
    Result := FromPos - Ranges[i].Range[FromData].Start + Ranges[i].Range[1 - FromData].Start;
  end;
end;

function TDataComparer.GetRangeIndex(DataIndex: Integer;
  Pos: TFilePointer): Integer;
// Get range index at given address
var
  Tmp: TComparedRange;
begin
  Tmp.Range[DataIndex] := TFileRange.Create(Pos, Pos + 1);

  Ranges.BinarySearch(Tmp, Result, TComparer<TComparedRange>.Construct(
    function (const Left, Right: TComparedRange): Integer
    var
      L, R: TFileRange;
    begin
      L := Left.Range[DataIndex];
      R := Right.Range[DataIndex];
      if L.Intersects(R) then Result := 0
      else Result := CompareValue(L.Start, R.Start);
    end));
end;

function TDataComparer.ProcessedTill(DataIndex: Integer): TFilePointer;
begin
  if Ranges.Count = 0 then Result := 0
  else Result := Ranges.Last.Range[DataIndex].AEnd;
end;

procedure TDataComparer.StartCompare(AData0, AData1: TEditedData);
begin
  Clear();
  Working := True;
  Progress := 0;
  Ptr[0] := 0;
  Ptr[1] := 0;
  Data[0] := AData0;
  Data[1] := AData1;
end;

function MinInt64Value(const Data: array of Int64): Int64;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

procedure TDataComparer.Work;
// Compare next portion of files (not longer then MaxWorkTime milliseconds)
const
  BlockSize = 512;
var
  Ticks: Cardinal;
  SizeLeft: array[0..1] of TFilePointer;
  n, L: Integer;
  p: TFilePointerPair;
  b0, b1: TBytes;
begin
  if not Working then Exit;

  Ticks := GetTickCount();
  repeat
    for n := 0 to 1 do
      SizeLeft[n] := Data[n].GetSize() - Ptr[n];
    // End of data?
    for n := 0 to 1 do
      if SizeLeft[n] = 0 then
      begin
        if SizeLeft[0] <> SizeLeft[1] then
        begin
          AddRange(True, SizeLeft[0], SizeLeft[1]);
        end;
        Working := False;
        Exit;
      end;

    L := BlockSize;
    if SizeLeft[0] < L then L := SizeLeft[0];
    if SizeLeft[1] < L then L := SizeLeft[1];
    b0 := GetBytes(0, Ptr[0], L);
    b1 := GetBytes(1, Ptr[1], L);
    L := EqualBytesCount(@b0[0], @b1[0], L);
    if L > 0 then
    begin
      AddRange(False, L, L);
      Inc(Ptr[0], L);
      Inc(Ptr[1], L);
    end
    else
    begin
      // Found different bytes. Search for re-sync
      FindNextCommonSeq(p);
      AddRange(True, p[0] - Ptr[0], p[1] - Ptr[1]);
      Ptr[0] := p[0];
      Ptr[1] := p[1];
    end;

  until GetTickCount() - Ticks >= MaxWorkTime;
end;

end.
