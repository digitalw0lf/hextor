unit uCompareFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Math,
  Generics.Collections, System.Types, Vcl.StdCtrls,

  uDWHexTypes, uEditorForm, uEditedData, uLogFile, ColoredPanel, Vcl.Buttons;

type
  TCompareFrame = class(TFrame)
    DiffBar: TPaintBox;
    BtnRecompare: TButton;
    LblDiffsCount: TLabel;
    CompareSelectFormPanel: TPanel;
    Label1: TLabel;
    CBCmpEditor1: TComboBox;
    Label2: TLabel;
    CBCmpEditor2: TComboBox;
    BtnCompare: TButton;
    BtnCancel: TButton;
    BtnAbort: TButton;
    BtnCloseComparsion: TSpeedButton;
    procedure DiffBarPaint(Sender: TObject);
    procedure DiffBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DiffBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnRecompareClick(Sender: TObject);
    procedure CBCmpEditor1Change(Sender: TObject);
    procedure BtnAbortClick(Sender: TObject);
    procedure BtnCloseComparsionClick(Sender: TObject);
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
    procedure CloseComparsion;
    procedure EditorVisRangeChanged(Sender: TEditorForm);
    procedure EditorByteColsChanged(Sender: TEditorForm);
  public
    { Public declarations }
    Editors: array[0..1] of TEditorForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure StartCompare(Editor1, Editor2: TEditorForm);
    function GetDataColors(Editor: TEditorForm; Addr: TFilePointer;
      Size: Integer; Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
    function ShowCompareDialog(): TModalResult;
  end;

implementation

uses
  uMainForm;

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

procedure TCompareFrame.CBCmpEditor1Change(Sender: TObject);
begin
  BtnCompare.Enabled := (CBCmpEditor1.ItemIndex <> CBCmpEditor2.ItemIndex);
end;

constructor TCompareFrame.Create(AOwner: TComponent);
begin
  inherited;
  ScrBmp := TBitmap.Create();
  Diffs := TList<TDiff>.Create();
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
// Draw Diff map
const
  ClrUnknown = clDkGray;
  ClrEqual   = clMoneyGreen;
  ClrDiff    = clRed;
var
  i: Integer;
  RAll, R: TRect;

//  function ScrCoord(P: TFilePointer): Integer;
//  begin
//    Result := Round(RAll.Top + RAll.Height*(P / MaxSize));
//  end;
//
begin
  if MaxSize = 0 then Exit;

  if (DiffBar.Width <> ScrBmp.Width) or (DiffBar.Height <> ScrBmp.Height) then
    ScrBmp.SetSize(DiffBar.Width, DiffBar.Height);

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
    for i:=0 to Diffs.Count-1 do
    begin
      R := RAll;
      R.Top := PosToScr(Diffs[i].Start);
      R.Bottom := PosToScr(Diffs[i].AEnd);
      if R.Bottom = R.Top then Inc(R.Bottom);

      FillRect(R);
    end;

    // Editor view frame
    R.Left := 0;
    R.Right := RAll.Right - 1;
    R.Top := PosToScr(Editors[0].FirstVisibleAddr);
    R.Bottom := PosToScr(Editors[0].FirstVisibleAddr + Editors[0].VisibleBytesCount) - 1;
    Pen.Color := clBlack;
    Polyline([Point(R.Left + 3, R.Top),
              Point(R.Left, R.Top),
              Point(R.Left, R.Bottom),
              Point(R.Left + 4, R.Bottom)]);
    Polyline([Point(R.Right - 3, R.Top),
              Point(R.Right, R.Top),
              Point(R.Right, R.Bottom),
              Point(R.Right - 4, R.Bottom)]);
  end;

  DiffBar.Canvas.Draw(0, 0, ScrBmp);
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

    Editors[1-n].BeginUpdatePanes();
    try
      Editors[1-n].TopVisibleRow := Sender.TopVisibleRow;
      Editors[1-n].HorzScrollPos := Sender.HorzScrollPos;
    finally
      Editors[1-n].EndUpdatePanes();
    end;
    DiffBarPaint(nil);
  finally
    FOurScrolling := False;
  end;
end;

function TCompareFrame.GetDataColors(Editor: TEditorForm; Addr: TFilePointer;
  Size: Integer; Data: PByteArray; var TxColors,
  BgColors: TColorArray): Boolean;
// Colorize diffs in text
var
  i: Integer;
begin
  Result := False;
  if (Editor <> Editors[0]) and (Editor <> Editors[1]) then Exit;

  for i:=0 to Diffs.Count-1 do
    Result := FillRangeInColorArray(BgColors, Addr,
      Diffs[i].Start, Diffs[i].AEnd, Color_DiffBg) or Result;
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

procedure TCompareFrame.CloseComparsion;
var
  i: Integer;
begin
  FAborted := True;
  for i:=0 to 1 do
  begin
    Editors[i].OnVisibleRangeChanged.Remove(EditorVisRangeChanged);
    Editors[i].OnByteColsChanged.Remove(EditorByteColsChanged);
    Editors[i] := nil;
  end;
  MaxSize := 0;
  Diffs.Clear;
  Refresh;
  MainForm.ActiveEditor.WindowState := wsMaximized;
end;

procedure TCompareFrame.BtnCloseComparsionClick(Sender: TObject);
begin
  CloseComparsion();
end;

procedure TCompareFrame.StartCompare(Editor1, Editor2: TEditorForm);
const
  BlockSize = 1*MByte;
var
  P: TFilePointer;
  Size1, Size2, MinSize: TFilePointer;
  Buf: array[0..1] of TBytes;
  i, Portion: Integer;
  MDIRect: TRect;
begin
  Editors[0] := Editor1;
  Editors[1] := Editor2;

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

    Editors[i].OnClosed.Add(procedure (Sender: TEditorForm)
      begin
        CloseComparsion();
      end);
    Editors[i].OnVisibleRangeChanged.Add(EditorVisRangeChanged);
    Editors[i].OnByteColsChanged.Add(EditorByteColsChanged);
  end;

  Size1 := Editors[0].EditedData.GetSize();
  Size2 := Editors[1].EditedData.GetSize();
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
        Buf[i] := Editors[i].EditedData.Get(P, Portion, False);
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

procedure TCompareFrame.UpdateInfo;
begin
  LblDiffsCount.Caption := 'Diffs: ' + IntToStr(Diffs.Count);
  DiffBarPaint(nil);
  if Editors[0]<>nil then
    Editors[0].UpdatePanes();
  if Editors[1]<>nil then
    Editors[1].UpdatePanes();
end;

end.
