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
    function GetByte(n: Integer; Addr: TFilePointer): Byte;
    function GetBytes(n: Integer; Addr: TFilePointer; Size: Integer): TBytes;
    function FindNextCommonSeq(var SyncPtrs: TFilePointerPair): Boolean;
  public
    // Settings
    MaxWorkTime: Cardinal;   // Milliseconds
    SyncBlockSize: Integer;  // Detect common sequence if it is at least this size
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
  private type
    TDiff = TFileRange;
  private
    { Private declarations }
    DrawnSize: TFilePointer;
    ScrBmp: TBitmap;
    FOurScrolling: Boolean;
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
    procedure StartCompare(Editor1, Editor2: TEditorForm);
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
  ClrEqual   = clMoneyGreen;
  ClrDiff    = clRed;
var
  n, i, PrevY: Integer;
  RAll, R: TRect;
  Range: TComparedRange;
begin
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

    RAll := DiffBarRect(n);


    with ScrBmp.Canvas do
    begin
      // Processed background
      R := RAll;
      R.Bottom := PosToScr(Comparer.ProcessedTill(n));
      Brush.Color := ClrEqual;
      FillRect(R);

      // Differences
      Brush.Color := ClrDiff;
      PrevY := -1;
      for i:=0 to Comparer.Ranges.Count-1 do
      begin
        Range := Comparer.Ranges[i];
        if Range.Differs then
        begin
          R := RAll;
          R.Top := PosToScr(Range.Range[n].Start);
          R.Bottom := PosToScr(Range.Range[n].AEnd);
          if R.Bottom = R.Top then Inc(R.Bottom);
          if R.Bottom = PrevY then Continue;  // Do not paint single row multiple times
          PrevY := R.Bottom;
          FillRect(R);
        end;
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
  i, n: Integer;
begin
  n := EditorIndex(Editor);
  if n < 0 then Exit;
  for i:=0 to Comparer.Ranges.Count - 1 do
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
  Result := Floor(P / DrawnSize * DiffBar.Height);
  Result := BoundValue(Result, 0, DiffBar.Height);
end;

function TCompareFrame.ScrToPos(Y: Integer): TFilePointer;
begin
  if DiffBar.Height = 0 then Exit(0);
  Result := Floor(Y / DiffBar.Height * DrawnSize);
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

  StartCompare(MainForm.Editors[CBCmpEditor1.ItemIndex], MainForm.Editors[CBCmpEditor2.ItemIndex]);
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

procedure TCompareFrame.StartCompare(Editor1, Editor2: TEditorForm);
const
  BlockSize = 10*MByte;
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
  SyncBlockSize := 32;
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

procedure SubFromChecksum(var Sum: TRollingChecksum; AByte:Byte);
begin
  Sum:=Sum xor AByte;
end;

function CompareBytes(const b1, b2: TBytes): Boolean;
begin
  Result := (Length(b1) = Length(b2)) and
            CompareMem(@b1[0], @b2[0], Length(b1));
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
begin
  for n := 0 to 1 do
  begin
    p[n] := Ptr[n];
    CS[n] := 0;
    CSPos[n] := TDictionary<TRollingChecksum, TArray<TFilePointer>>.Create();
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
      begin
        // Update rolling checksum
        if p[n] - SyncBlockSize >= Ptr[n] then
          SubFromChecksum(CS[n], GetByte(n, p[n] - SyncBlockSize));
        if p[n] < AEnd[n] then
        begin
          AddToChecksum(CS[n], GetByte(n, p[n]));
          Inc(p[n]);
          // Add checksum and position to dictionary
          if p[n] - SyncBlockSize >= Ptr[n] then
          begin
            if not CSPos[n].TryGetValue(CS[n], Ptrs) then
              Ptrs := nil;
            Ptrs := Ptrs + [p[n] - SyncBlockSize];
            CSPos[n].AddOrSetValue(CS[n], Ptrs);
          end;
        end;
      end;

      // Compare (by checksum) current window in one stream with all accumulated
      // windows from another stream
      for n := 0 to 1 do
      if p[n] - SyncBlockSize >= Ptr[n] then
      begin
        if CSPos[1-n].TryGetValue(CS[n], Ptrs) then
        begin
          // Found checksum match, compare actual data of all possible matches
          for i := 0 to Length(Ptrs) - 1 do
            if CompareBytes(GetBytes(n, p[n] - SyncBlockSize, SyncBlockSize),
                            GetBytes(1-n, Ptrs[i], SyncBlockSize)) then
            begin
              // Found matching block
              SyncPtrs[n] := p[n] - SyncBlockSize;
              SyncPtrs[1-n] := Ptrs[i];
              Exit(True);
            end;
        end;
      end;
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

function TDataComparer.GetByte(n: Integer; Addr: TFilePointer): Byte;
// Get byte from Data, using cache window of MaxReadAhead
var
  Size: Integer;
begin
  if (Addr < CacheAddr[n]) or (Addr >= CacheAddr[n] + Length(Cache[n])) then
  // Cache miss
  begin
    if (Addr < Ptr[n]) or (Addr >= Ptr[n] + MaxReadAhead) or (Addr >= Data[n].GetSize()) then
      raise Exception.Create('Error during file coimpare');  // Check for some bugs?
    CacheAddr[n] := Ptr[n];
    Size := Min(MaxReadAhead, Data[n].GetSize() - CacheAddr[n]);
    Cache[n] := Data[n].Get(CacheAddr[n], Size);
  end;

  Result := Cache[n][Addr - CacheAddr[n]];
end;

function TDataComparer.GetBytes(n: Integer; Addr: TFilePointer;
  Size: Integer): TBytes;
begin
  if (Addr >= CacheAddr[n]) and (Addr + Size <= CacheAddr[n] + Length(Cache[n])) then
    Result := Copy(Cache[n], Addr - CacheAddr[n], Size)
  else
    Result := Data[n].Get(Addr, Size);
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
  BlockSize = 10*MByte;
var
  Ticks: Cardinal;
  SizeLeft: array[0..1] of TFilePointer;
  n: Integer;
  p: TFilePointerPair;
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

    if GetByte(0, Ptr[0]) = GetByte(1, Ptr[1]) then
    begin
      AddRange(False, 1, 1);
      Inc(Ptr[0]);
      Inc(Ptr[1]);
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
