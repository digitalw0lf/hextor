unit uEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Generics.Collections, Math, System.Types, Vcl.Menus,
  System.Win.ComObj, System.TypInfo, Winapi.ActiveX,

  uUtil, uHextorTypes, uHextorDataSources, uEditorPane, uEditedData,
  uCallbackList, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  uDataSearcher, uUndoStack, uLogFile, uOleAutoAPIWrapper;

type
  TEditorForm = class;

  // Callback that is used by tools to colorise displayed data in editor
  TGetDataColors = function (Editor: TEditorForm; Addr: TFilePointer; Size: Integer; AData: PByteArray; var TxColors, BgColors: TColorArray): Boolean;

  TFFSkipSearcher = class(TDataSearcher)
  // Searcher for skipping given byte value
  public
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; override;
  end;

  TEditorForm = class(TForm)
    PaneHex: TEditorPane;
    PaneAddr: TEditorPane;
    PaneText: TEditorPane;
    VertScrollBar: TScrollBar;
    StatusBar: TStatusBar;
    EditorPopupMenu: TPopupMenu;
    PMICut: TMenuItem;
    PMICopy: TMenuItem;
    PMIPaste: TMenuItem;
    PMISelectAll: TMenuItem;
    N6: TMenuItem;
    PMIBitsEditor: TMenuItem;
    Shape1: TShape;
    Shape2: TShape;
    HorzScrollBar: TScrollBar;
    BtnSkipFFBack: TSpeedButton;
    BtnSkipFFFwd: TSpeedButton;
    Image1: TImage;
    ImgListSkipBtn: TImageList;
    TypingActionChangeTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaneHexEnter(Sender: TObject);
    procedure PaneHexKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaneHexKeyPress(Sender: TObject; var Key: Char);
    procedure PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaneHexMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaneTextKeyPress(Sender: TObject; var Key: Char);
    procedure VertScrollBarChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure HorzScrollBarChange(Sender: TObject);
    procedure BtnSkipFFBackClick(Sender: TObject);
    procedure TypingActionChangeTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FData: TEditedData;
    FDestroyed: Boolean;  // Some events (e.g. FormResize) are oddly called after form destruction
    FCaretPos: TFilePointer;
    FSelStart, FSelLength: TFilePointer;
    SelDragStart, SelDragEnd: TFilePointer;
    WasLeftMouseDown: Boolean;
    FCaretInByte: Integer;
    TypingActionCode: Integer;  // To combine sequentially typed chars into one "Undo" action
    FHasUnsavedChanges: Boolean;
    FTopVisibleRow: TFilePointer;
    FUpdating: Integer;
    FNeedUpdatePanes: Boolean;
    FByteColumns: Integer;
    FLinesPerScrollBarTick: Integer;
    FInsertMode: Boolean;
    FByteColumnsSetting: Integer;
    FPrevVisibleRange: TFileRange;
    FPrevHorzScroll: Integer;
    FHorzScrollPos: Integer;
    FFSkipSearcher: TFFSkipSearcher;
    FFSkipBackByte, FFSkipFwdByte: Byte;
    FTextEncoding: Integer;
//    FAutoObject: TAutoObject;
    procedure SetCaretPos(Value: TFilePointer);
    procedure UpdatePanesCarets();
    procedure PaneMouseMove(Sender: TObject; IsMouseDown: Boolean; Shift: TShiftState; X, Y: Integer);
    procedure SetCaretInByte(const Value: Integer);
    procedure SetHasUnsavedChanges(const Value: Boolean);
    procedure UpdateFormCaption();
    procedure SetTopVisibleRow(Value: TFilePointer);
    procedure SetByteColumns(Value: Integer);
    procedure UpdatePaneWidths();
    procedure ShowSelectionInfo();
    procedure AddCurrentFileToRecentFiles();
    procedure SelectionChanged();
    procedure CopyDataRegion(Source, Dest: THextorDataSource; SourceAddr, DestAddr, Size: TFilePointer);
    procedure SetInsertMode(Value: Boolean);
    function AdjustPositionInData(var Pos: TFilePointer; OpAddr, OpSize: TFilePointer): Boolean;
    procedure AdjustPointersPositions(OpAddr, OpSize: TFilePointer);
    procedure DataChanged(Addr: TFilePointer; OldSize, NewSize: TFilePointer; Value: PByteArray);
    procedure SomeDataChanged();
    procedure UndoActionCreating(Action: TUndoStackAction);
    procedure UndoActionReverted(Action: TUndoStackAction; Direction: TUndoStack.TUndoDirection);
    procedure SetByteColumnsSetting(const Value: Integer);
    procedure SetHorzScrollPos(Value: Integer);
    procedure BreakCurrentTypingAction();
    procedure SetTextEncoding(const Value: Integer);
//  public type
//    TSaveMethod = (smUnknown, smPartialInplace, smFull, smTempFile);
  public
    { Public declarations }
    DataSource: THextorDataSource;
    UndoStack: TUndoStack;
    OnClosed: TCallbackListP1<TEditorForm>;
    OnVisibleRangeChanged: TCallbackListP1<TEditorForm>;
    OnSelectionChanged: TCallbackListP1<TEditorForm>;  // Called when either selection moves or data in selected range changes
    OnByteColsChanged: TCallbackListP1<TEditorForm>;
//    property AutoObject: TAutoObject read FAutoObject implements IDispatch;
    [API]
    property Data: TEditedData read FData write FData;
    destructor Destroy(); override;
    function AskSaveChanges(): TModalResult;
    procedure OpenNewEmptyFile;
    procedure SaveFile(DataSourceType: THextorDataSourceType; const APath: string);
    procedure NewFileOpened(ResetCaret: Boolean);
    function GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;
    function GetOrigFileSize(): TFilePointer;
    [API]
    function GetFileSize(): TFilePointer;
    procedure UpdatePanes();
    procedure UpdateScrollBars();
    procedure UpdateSkipFFButtons(const AData: TBytes);
    function GetVisibleRowsCount(): Integer;
    function GetVisibleColsCount(IncludePartial: Boolean = True): Integer;
    function FirstVisibleAddr(): TFilePointer;
    function VisibleBytesCount(): Integer;
    procedure ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
    function DeleteSelected(): TFilePointer;
    procedure ReplaceSelected(NewSize: TFilePointer; Value: PByteArray);
    [API]
    property CaretPos: TFilePointer read FCaretPos write SetCaretPos;
    property CaretInByte: Integer read FCaretInByte write SetCaretInByte;
    [API]
    property SelStart: TFilePointer read FSelStart;
    [API]
    property SelLength: TFilePointer read FSelLength;
    [API]
    property InsertMode: Boolean read FInsertMode write SetInsertMode;
    procedure MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
    [API]
    procedure SetSelection(AStart, AEnd: TFilePointer);
    procedure ScrollToShow(Addr: TFilePointer; RowsFromBorder: Integer = 0; ColsFromBorder: Integer = 0);
    procedure ScrollToCaret();
    [API]
    procedure BeginUpdatePanes();
    [API]
    procedure EndUpdatePanes();
    property HasUnsavedChanges: Boolean read FHasUnsavedChanges write SetHasUnsavedChanges;
    function ChooseSaveMethod(DataSourceType: THextorDataSourceType; const APath: string;
      var InplaceSaving, UseTempFile: Boolean): Boolean;
    property TopVisibleRow: TFilePointer read FTopVisibleRow write SetTopVisibleRow;
    property HorzScrollPos: Integer read FHorzScrollPos write SetHorzScrollPos;
    property ByteColumns: Integer read FByteColumns write SetByteColumns;
    property ByteColumnsSetting: Integer read FByteColumnsSetting write SetByteColumnsSetting;
    procedure CalculateByteColumns();
    function GetSelectedOrAfterCaret(DefaultSize, MaxSize: Integer; var Addr: TFilePointer; NothingIfMore: Boolean = False): TBytes;
    property TextEncoding: Integer read FTextEncoding write SetTextEncoding;
  end;

procedure ConfigureScrollbar(AScrollBar: TScrollBar; AMax, APageSize: Integer);

var
  EditorForm: TEditorForm;

implementation

uses
  uMainForm, uValueFrame;

{$R *.dfm}

procedure ConfigureScrollbar(AScrollBar: TScrollBar; AMax, APageSize: Integer);
begin
  APageSize := Min(APageSize, AMax);
  if AMax < AScrollBar.PageSize then
  // Order matters
  begin
    AScrollBar.PageSize := APageSize;
    AScrollBar.Max := AMax;
  end
  else
  begin
    AScrollBar.Max := AMax;
    AScrollBar.PageSize := APageSize;
  end;
end;

procedure TEditorForm.AddCurrentFileToRecentFiles;
var
  Recent: THextorSettings.TRecentFileRec;
  n, i: Integer;
begin
  if (DataSource.ClassType <> TFileDataSource) or (ExtractFilePath(DataSource.Path) = '') then Exit;

  n := -1;
  for i:=0 to Length(AppSettings.RecentFiles)-1 do
    if SameFileName(AppSettings.RecentFiles[i].FileName, DataSource.Path) then
    begin
      n := i;
      Break;
    end;
  if n < 0 then
  begin
    Recent.FileName := DataSource.Path;
  end
  else
  begin
    Recent := AppSettings.RecentFiles[n];
    Delete(AppSettings.RecentFiles, n, 1);
  end;
  Insert([Recent], AppSettings.RecentFiles, 0);
  if Length(AppSettings.RecentFiles) > 20 then
    SetLength(AppSettings.RecentFiles, 20);

  MainForm.SaveSettings();
end;

procedure TEditorForm.AdjustPointersPositions(OpAddr, OpSize: TFilePointer);
// Adjust Caret position, bookmarks etc. after operation that inserted or deleted data
var
  ASelEnd: TFilePointer;
begin
  AdjustPositionInData(FCaretPos, OpAddr, OpSize);
  ASelEnd := SelStart + SelLength;
  AdjustPositionInData(FSelStart, OpAddr, OpSize);
  AdjustPositionInData(ASelEnd, OpAddr, OpSize);
  FSelLength := ASelEnd - FSelStart;

  AdjustPositionInData(SelDragStart, OpAddr, OpSize);
  AdjustPositionInData(SelDragEnd, OpAddr, OpSize);
end;

function TEditorForm.AdjustPositionInData(var Pos: TFilePointer; OpAddr,
  OpSize: TFilePointer): Boolean;
// Adjust position Pos according to operation that inserted or deleted data at position OpAddr.
// OpSize < 0 for deletion
begin
  if Pos < OpAddr then Exit(False);
  Pos := Max(Pos + OpSize, OpAddr);  // Works for both deletion and insertion
  Result := True;
end;

function TEditorForm.AskSaveChanges: TModalResult;
begin
  Result := mrNo;
  if HasUnsavedChanges then
  begin
    Result := Application.MessageBox(PChar('Save changes to '#13#10+DataSource.Path+'?'), 'Closing', MB_YESNOCANCEL);
    case Result of
      mrYes: MainForm.ActionSaveExecute(nil);
    end;
  end;
end;

procedure TEditorForm.BeginUpdatePanes;
begin
  Inc(FUpdating);
  PaneAddr.BeginUpdate();
  PaneHex.BeginUpdate();
  PaneText.BeginUpdate();
end;

procedure TEditorForm.BreakCurrentTypingAction;
begin
  // If there is a delay in typing characters, then it we be another action in "Undo" stack
  TypingActionCode := (TypingActionCode + 1) mod 10000;
  TypingActionChangeTimer.Enabled := False;
end;

procedure TEditorForm.BtnSkipFFBackClick(Sender: TObject);
// Skip repeating bytes and scroll to where values changes
var
  Start, Ptr: TFilePointer;
  Dir, Size: Integer;
begin
  FFSkipSearcher.Haystack := Data;
  FFSkipSearcher.Params.Range := EntireFile;

  Dir := (Sender as TSpeedButton).Tag;
  if Dir < 0 then
  begin
    FFSkipSearcher.Params.Needle := [FFSkipBackByte];
    Start := FirstVisibleAddr() - 1;
  end
  else
  begin
    FFSkipSearcher.Params.Needle := [FFSkipFwdByte];
    Start := FirstVisibleAddr() + VisibleBytesCount();
  end;

  try
    FFSkipSearcher.Find(Start, Dir, Ptr, Size);
  finally
    MainForm.OperationDone(FFSkipSearcher);
  end;
  BeginUpdatePanes();
  try
    ScrollToShow(Ptr, -1, -1);
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.CalculateByteColumns();
// Choose byte columns count based on width of editor panes
var
  Cols: Integer;
begin
  if ByteColumnsSetting > 0 then
    ByteColumns := ByteColumnsSetting
  else
  begin
    Cols := (VertScrollBar.Left - PaneHex.Left - PaneHex.CharWidth - PaneText.CharWidth) div (PaneHex.CharWidth*3 + PaneText.CharWidth);
    if Cols < 1 then Cols := 1;
    ByteColumns :=  Cols;
  end;
end;

procedure TEditorForm.ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
begin
  Data.Change(Addr, Length(Value), @Value[0]);
end;

function TEditorForm.ChooseSaveMethod(DataSourceType: THextorDataSourceType; const APath: string;
  var InplaceSaving, UseTempFile: Boolean): Boolean;
// How can we save our modified data to given target:
// Can we only write changed parts, or maybe we'll have to use temp file?
var
  SameSource: Boolean;
begin
  SameSource := (DataSourceType = DataSource.ClassType) and
                (SameFileName(APath, DataSource.Path));

  InplaceSaving := SameSource and (not Data.HasMovements());
  UseTempFile := (SameSource) and (not InplaceSaving);
  Result := True;
end;

procedure TEditorForm.CopyDataRegion(Source, Dest: THextorDataSource; SourceAddr,
  DestAddr, Size: TFilePointer);
const
  BlockSize = 10*MByte;
var
  Buf: TBytes;
  Pos: TFilePointer;
  PortionSize: Integer;
begin
  SetLength(Buf, BlockSize);
  Pos := 0;

  while Pos < Size do
  begin
    PortionSize := Min(BlockSize, Size - Pos);
    Source.GetData(SourceAddr + Pos, PortionSize, Buf[0]);
    Dest.ChangeData(DestAddr + Pos, PortionSize, Buf[0]);
    Pos := Pos + PortionSize;
  end;
end;

procedure TEditorForm.DataChanged(Addr: TFilePointer; OldSize,
  NewSize: TFilePointer; Value: PByteArray);
begin
  if OldSize <> NewSize then
    AdjustPointersPositions(Addr, NewSize - OldSize);

  SomeDataChanged();
  if (Addr <= SelStart + SelLength) and (Addr + Max(OldSize, NewSize) >= SelStart) then
    SelectionChanged();
end;

function TEditorForm.DeleteSelected(): TFilePointer;
begin
  Result := SelLength;
  if Result > 0 then
    ReplaceSelected(0, nil);
end;

destructor TEditorForm.Destroy;
begin
  FDestroyed := True;
  MainForm.RemoveEditor(Self);
  UndoStack.Free;
  Data.Free;
  DataSource.Free;
  FFSkipSearcher.Free;
//  FreeAndNil(FAutoObject);
  inherited;
end;

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  if DataSource <> nil then
    MainForm.ActiveEditorChanged();
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  OnClosed.Call(Self);
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AskSaveChanges() = IDCANCEL then
    CanClose := False;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
//  FAutoObject := TAutoObject.Create();
  Data := TEditedData.Create({Self});
  Data.OnDataChanged.Add(DataChanged);
  UndoStack := TUndoStack.Create(Data);
  UndoStack.OnActionCreating.Add(UndoActionCreating);
  UndoStack.OnActionReverted.Add(UndoActionReverted);
  UndoStack.OnProgress.Add(MainForm.ShowProgress);
  UndoStack.OnOperationDone.Add(MainForm.OperationDone);
  CalculateByteColumns();
  FFSkipSearcher := TFFSkipSearcher.Create();
  FFSkipSearcher.OnProgress.Add(MainForm.ShowProgress);
  MainForm.AddEditor(Self);
end;

procedure TEditorForm.FormDeactivate(Sender: TObject);
begin
  if FDestroyed then Exit;
  UpdatePanesCarets();  // Gray caret
end;

procedure TEditorForm.FormResize(Sender: TObject);
begin
  if FDestroyed then Exit;
  BeginUpdatePanes();
  try
    CalculateByteColumns();
    UpdatePaneWidths();
    UpdatePanes();
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.PaneHexEnter(Sender: TObject);
begin
//  MainForm.CheckEnabledActions();

end;

procedure TEditorForm.PaneHexKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure AMoveCaret(dx, dy: Integer; StartOfByte: Boolean = False);
  var
    cp, cp2: TFilePointer;
    cb: Integer;
  begin
    cp := CaretPos;
    cb := CaretInByte;

    if (Sender=PaneHex) then
    begin
      cp2 := cp*2+cb+dx;
      cp2 := BoundValue(cp2, 0, GetFileSize()*2);
      cp := cp2 div 2;
      cb := cp2 mod 2;
    end
    else
    begin
      cp := cp + dx;
    end;
    cp := cp + ByteColumns*dy;

    if StartOfByte then cb := 0;

    MoveCaret(cp, Shift);
    CaretInByte := cb;
  end;

begin
  BeginUpdatePanes();
  try
    case Key of
      VK_HOME:
        if ssCtrl in Shift then
          begin end //MainForm.ActionGoToStart.Execute()
        else
        begin
          MoveCaret((CaretPos div ByteColumns)*ByteColumns, KeyboardStateToShiftState());
          CaretInByte := 0;
        end;

      VK_END:
        if ssCtrl in Shift then
          begin end //MainForm.ActionGoToEnd.Execute()
        else
        begin
          MoveCaret((CaretPos div ByteColumns)*ByteColumns + ByteColumns - 1, KeyboardStateToShiftState());
          CaretInByte := 0;
        end;

      VK_LEFT: AMoveCaret(-1, 0);
      VK_RIGHT: AMoveCaret(1, 0);
      VK_UP: AMoveCaret(0, -1);
      VK_DOWN: AMoveCaret(0, 1);

      VK_PRIOR: AMoveCaret(0, -GetVisibleRowsCount());
      VK_NEXT: AMoveCaret(0, GetVisibleRowsCount());

      VK_TAB:
        begin
          if ActiveControl = PaneHex then
            ActiveControl := PaneText
          else
          if ActiveControl = PaneText then
            ActiveControl := PaneHex;
          UpdatePanesCarets();
        end;

      VK_INSERT:
        if Shift = [] then
          InsertMode := not InsertMode;

      VK_DELETE:
        if (Shift = []) and (InsertMode) then
          begin
            if SelLength > 0 then
              DeleteSelected()
            else
            if (Sender = PaneText) and (CaretPos < Data.GetSize()) then
              Data.Delete(CaretPos, 1);
          end;

      VK_BACK:
        begin
          if InsertMode then
          begin
            if SelLength > 0 then
              DeleteSelected()
            else
            if (Sender = PaneText) and (CaretPos > 0) then
              Data.Delete(CaretPos - 1, 1);
          end
          else
          begin
            // In overwrite mode, allow to backspace-delete from end of file
            if (Sender = PaneText) and (CaretPos > 0) and (CaretPos = Data.GetSize()) and (Data.Resizable) then
              Data.Delete(CaretPos - 1, 1);
          end;
        end;
    end;

//    if ssCtrl in Shift then
//    begin
//      case Key of
//        Ord('A'): MainForm.ActionSelectAll.Execute();
//        Ord('X'): MainForm.ActionCut.Execute();
////        Ord('C'), VK_INSERT: MainForm.ActionCopy.Execute();
//        Ord('V'): MainForm.ActionPaste.Execute();
//        Ord('F'): MainForm.ActionFind.Execute();
//      end;
//      Key := 0;
//    end;
//
//    if (ssShift in Shift) and (Key = VK_INSERT) then
//    begin
//      MainForm.ActionPaste.Execute();
//      Key := 0;
//    end;

  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.PaneHexKeyPress(Sender: TObject; var Key: Char);
const
  Zero: Byte = 0;
var
  APos: TFilePointer;
  Buf: TBytes;
  x: Byte;
  Digit: Integer;
begin
  if CharInSet(Key, HexCharsSet) then
  begin
    BeginUpdatePanes();
    UndoStack.BeginAction('type_'+IntToStr(TypingActionCode), 'Typing');
    try
      if InsertMode then
      begin
        if DeleteSelected() > 0 then
          CaretInByte := 0;
      end;

      APos := CaretPos;
      if (InsertMode or (APos = Data.GetSize())) and (CaretInByte = 0) then
      begin
        Data.Insert(APos, 1, @Zero);
        FCaretPos := APos;  // Keep caret pos in same byte
      end;
      Buf := GetEditedData(APos, 1, dspResizable in DataSource.GetProperties());
      if Length(Buf)<>1 then Exit;
      x := Buf[0];
      Digit := StrToInt('$'+Key);
      if CaretInByte=0 then
        x := (x and $0F) or (Digit shl 4)
      else
        x := (x and $F0) or (Digit);
      ChangeBytes(APos, [x]);

      if CaretInByte=0 then
        CaretInByte := 1
      else
      begin
        MoveCaret(CaretPos + 1, []);
        CaretInByte := 0;
      end;
      ScrollToCaret();
    finally
      UndoStack.EndAction();
      TypingActionChangeTimer.Enabled := False;
      TypingActionChangeTimer.Enabled := True;
      EndUpdatePanes();
    end;
  end;
end;

procedure TEditorForm.PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ActiveControl := (Sender as TWinControl);
  if not ActiveControl.Focused then
    ActiveControl.SetFocus();
  if (Sender <> PaneAddr) and (Button = mbLeft) then
  begin
    WasLeftMouseDown := True;
    PaneMouseMove(Sender, True, Shift+[ssLeft], X, Y);
  end;
end;

procedure TEditorForm.PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Sender <> PaneAddr) and (ssLeft in Shift) and (WasLeftMouseDown) then
    PaneMouseMove(Sender, False, Shift, X, Y);
end;

procedure TEditorForm.PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    WasLeftMouseDown := False;
end;

procedure TEditorForm.PaneHexMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TopVisibleRow := TopVisibleRow - WheelDelta div 120 * AppSettings.ScrollWithWheel;
end;

function TEditorForm.GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;
begin
  Result := Data.Get(Addr, Size, ZerosBeyondEoF);
end;

function TEditorForm.GetFileSize: TFilePointer;
// Edited file size (including appended region)
begin
  Result := Data.GetSize();
end;

function TEditorForm.GetOrigFileSize: TFilePointer;
// Original file size
begin
  if DataSource <> nil then
    Result := DataSource.GetSize()
  else
    Result := 0;
end;

function TEditorForm.GetSelectedOrAfterCaret(DefaultSize, MaxSize: Integer; var Addr: TFilePointer;
  NothingIfMore: Boolean): TBytes;
// Return selected data or data block after caret, if nothing selected.
// NothingIfMore: return nothing if selection length is more then specified.
var
  Size: Integer;
begin
  if SelLength > 0 then
  begin
    if (NothingIfMore) and (SelLength > MaxSize) then
      Exit(nil);
    Size := Min(MaxSize, SelLength);
    Addr := SelStart;
  end
  else
  begin
    Size := Min(DefaultSize, GetFileSize()-CaretPos);
    Addr := CaretPos;
  end;

  Result := GetEditedData(Addr, Size);
end;

//function TEditorForm.GetTypeInfo(Index, LocaleID: Integer;
//  out TypeInfo): HResult;
//begin
//  Nop();
//end;
//
//function TEditorForm.GetTypeInfoCount(out Count: Integer): HResult;
//begin
//  Nop();
//end;

function TEditorForm.FirstVisibleAddr: TFilePointer;
begin
  Result := TopVisibleRow * ByteColumns;
end;

function TEditorForm.GetVisibleColsCount(IncludePartial: Boolean = True): Integer;
begin
  if IncludePartial then
    Result := DivRoundUp(PaneHex.Width, PaneHex.CharWidth()*3)
  else
    Result := PaneHex.Width div (PaneHex.CharWidth()*3);
  if Result < 1 then Result := 1;
end;

function TEditorForm.GetVisibleRowsCount: Integer;
begin
  Result := PaneHex.Height div PaneHex.CharHeight();
end;

procedure TEditorForm.HorzScrollBarChange(Sender: TObject);
begin
  HorzScrollPos := HorzScrollBar.Position;
end;

//function TEditorForm.Invoke(DispID: Integer; const IID: TGUID;
//  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
//  ArgErr: Pointer): HResult;
//begin
//  OleVariant(VarResult^) := 2345;
////  Result := inherited;
//end;

procedure TEditorForm.MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
// Move caret and adjust/remove selection
begin
  NewPos := BoundValue(NewPos, 0, Data.GetSize());
  if ssShift in Shift then
    SelDragEnd := NewPos
  else
  begin
    SelDragStart := NewPos;
    SelDragEnd := -1;
  end;
  SetSelection(SelDragStart, SelDragEnd);

  CaretPos := NewPos;
  ScrollToCaret();
end;

procedure TEditorForm.NewFileOpened(ResetCaret: Boolean);
begin
  BeginUpdatePanes();
  try
    Data.DataSource := DataSource;
    Data.Resizable := (dspResizable in DataSource.GetProperties());
    Data.ResetParts();
    HasUnsavedChanges := False;
    UndoStack.Clear();

    MainForm.ImageList16.GetIcon(MainForm.GetIconIndex(DataSource), Icon);
    UpdateFormCaption();
    UpdateScrollBars();
    if ResetCaret then
      MoveCaret(0, [])
    else
      CaretPos := CaretPos;  // Limit by file size
    InsertMode := InsertMode;  // Only for resizable sources
    UpdatePanes();

    SelectionChanged();
  finally
    EndUpdatePanes();
  end;

  AddCurrentFileToRecentFiles();
end;

procedure TEditorForm.PaneMouseMove(Sender: TObject; IsMouseDown: Boolean;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  ACaretPos: TFilePointer;
  ACaretInByte: Integer;
  ss: TShiftState;
begin
  BeginUpdatePanes();
  try
    if (Sender as TEditorPane).GetCharAt(X, Y, p) then
    begin
      if Sender = PaneHex then
      begin
        ACaretInByte := BoundValue(p.X mod 3, 0, 1);
        p.X := p.X div 3;
      end
      else
        ACaretInByte := 0;
      ACaretPos := FirstVisibleAddr() + (p.Y*ByteColumns + p.X);
      ss := Shift;
      if not IsMouseDown then
        ss := ss + [ssShift];
      MoveCaret(ACaretPos, ss);
      CaretInByte := ACaretInByte;
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.PaneTextKeyPress(Sender: TObject; var Key: Char);
var
  s: RawByteString;
  c: AnsiChar;
begin
  if Key >= ' ' then
  begin
    BeginUpdatePanes();
    UndoStack.BeginAction('type_'+IntToStr(TypingActionCode), 'Typing');
    try
      // Maybe some better way to convert single unicode char to selected CP char?
      s := RawByteString(string(Key));  // Converts to system CP
      SetCodePage(s, TextEncoding, True);
      c := s[Low(s)];
      if InsertMode then
      begin
//        DeleteSelected();
//        EditedData.Insert(CaretPos, 1, @c);
        ReplaceSelected(1, @c);
      end
      else
      begin
        ChangeBytes(CaretPos, [Byte(c)]);
        MoveCaret(CaretPos + 1, []);
      end;
    finally
      UndoStack.EndAction();
      TypingActionChangeTimer.Enabled := False;
      TypingActionChangeTimer.Enabled := True;
      EndUpdatePanes();
    end;
  end;
end;

procedure TEditorForm.ReplaceSelected(NewSize: TFilePointer; Value: PByteArray);
var
  NewCaretPos: TFilePointer;
begin
  BeginUpdatePanes();
  try
    NewCaretPos := SelStart + NewSize;
    Data.Change(SelStart, SelLength, NewSize, Value);
    MoveCaret(NewCaretPos, []);
    ScrollToCaret();
  finally
    EndUpdatePanes();
  end;
end;

//function TEditorForm.QueryInterface(const IID: TGUID; out Obj): HResult;
//begin
//  Result := inherited;
//end;

procedure TEditorForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := TFilePointer(VertScrollBar.Position) * FLinesPerScrollBarTick;
end;

procedure TEditorForm.SaveFile(DataSourceType: THextorDataSourceType; const APath: string);
var
  Dest: THextorDataSource;
  InplaceSaving, UseTempFile: Boolean;
  TempFileName: string;
  APart: TEditedData.TDataPart;
begin
  if (DataSourceType=nil) or (APath='') then Exit;

  if not ChooseSaveMethod(DataSourceType, APath, InplaceSaving, UseTempFile) then
    raise Exception.Create('Cannot save this data to this target');

  if InplaceSaving then
  // If saving to same file, re-open it for writing
  begin
    try
      DataSource.Open(fmOpenReadWrite);
    except
      // If failed to open for write (e.g. used by other app) - re-open for reading
      DataSource.Open(fmOpenRead);
      raise;
    end;
    Dest := DataSource;
  end
  else
  // If saving to another file, create another DataSource
  begin
    if UseTempFile then
    // Overwrite using temporary file
    begin
      if DataSourceType = TFileDataSource then
        TempFileName := APath+'_temp'+IntToStr(Random(10000))
      else
        TempFileName := TempPath + 'save'+IntToStr(Random(10000));
      Dest := TFileDataSource.Create(TempFileName);
    end
    else
      // Save to new file
      Dest := DataSourceType.Create(APath);
    Dest.Open(fmCreate);
  end;

  // Write regions
  try
    for APart in Data.Parts do
    begin
      if (InplaceSaving) and (APart.PartType = ptSource) then Continue;
      case APart.PartType of
        ptSource:
          CopyDataRegion(DataSource, Dest, APart.SourceAddr, APart.Addr, APart.Size);
        ptBuffer:
          Dest.ChangeData(APart.Addr, APart.Size, APart.Data[0]);
      end;

      MainForm.ShowProgress(Self, APart.Addr + APart.Size, Data.GetSize, '-');
    end;
  finally
    MainForm.OperationDone(Self);
  end;
  // TODO: Handle write errors

  // If saving in-place, we may have to truncate file
  if (InplaceSaving) and (dspResizable in Dest.GetProperties()) and
     (Data.GetSize() <> Dest.GetSize) then
    Dest.SetSize(Data.GetSize());


  if UseTempFile then
  // Saving throught temporary file
  begin
    if DataSourceType = TFileDataSource then
    // Destination is file - rename TempFile to target filename
    begin
      DataSource.Free;
      Dest.Free;
      DeleteFile(APath);
      RenameFile(TempFileName, APath);
      DataSource := TFileDataSource.Create(APath);
    end
    else
    // Not a file - copy tempfile content to destination
    begin
      DataSource.Free;
      DataSource := DataSourceType.Create(APath);
      DataSource.Open(fmCreate);
      DataSource.CopyContentFrom(Dest);
      Dest.Free;
      DeleteFile(TempFileName);
    end;
  end
  else
  begin
    if not InplaceSaving then
    // Switch to new file
    begin
      DataSource.Free;
      DataSource := Dest;
    end;
  end;

  // Open new saved file for reading
  DataSource.Open(fmOpenRead);

  NewFileOpened(False);
end;

procedure TEditorForm.ScrollToCaret;
begin
  ScrollToShow(CaretPos, 0, 0);
end;

procedure TEditorForm.ScrollToShow(Addr: TFilePointer;
  RowsFromBorder: Integer = 0; ColsFromBorder: Integer = 0);
// Scroll view to show specified file position.
// RowsFromBorder, ColsFromBorder ensures enough distance from window corners
var
  TargetRow: TFilePointer;
  TargetCol: Integer;
begin
  BeginUpdatePanes();
  try
    if RowsFromBorder = -1 then RowsFromBorder := 8;
    if ColsFromBorder = -1 then ColsFromBorder := 8;

    // Vertical scroll
    RowsFromBorder := BoundValue(RowsFromBorder, 0, GetVisibleRowsCount() div 2);
    TargetRow := Addr div ByteColumns;
    if TargetRow < TopVisibleRow + RowsFromBorder then
      TopVisibleRow := TargetRow - RowsFromBorder
    else
    if TargetRow > TopVisibleRow + GetVisibleRowsCount() - RowsFromBorder - 1 then
      TopVisibleRow := TargetRow - GetVisibleRowsCount() + RowsFromBorder + 1;

    // Horizontal scroll
    ColsFromBorder := BoundValue(ColsFromBorder, 0, GetVisibleColsCount(False) div 2);
    TargetCol := Addr mod ByteColumns;
    if (TargetCol < HorzScrollPos + ColsFromBorder) then
      HorzScrollPos := TargetCol - ColsFromBorder
    else
    if TargetCol > HorzScrollPos + GetVisibleColsCount(False) - ColsFromBorder - 1 then
      HorzScrollPos := TargetCol - GetVisibleColsCount(False) + ColsFromBorder + 1;
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.SelectionChanged;
begin
  ShowSelectionInfo();
  OnSelectionChanged.Call(Self);
  if Self = MainForm.GetActiveEditorNoEx() then
    MainForm.SelectionChanged();
end;

procedure TEditorForm.SetByteColumns(Value: Integer);
var
  NewTopRow: TFilePointer;
  CaretLineOnScreen: TFilePointer;
begin
  Value := BoundValue(Value, 1, 16384);
  if Value <> FByteColumns then
  begin
    BeginUpdatePanes();
    try
      if ByteColumns > 0 then
        CaretLineOnScreen := CaretPos div ByteColumns - TopVisibleRow
      else
        CaretLineOnScreen := 0;
      NewTopRow := Round((TopVisibleRow * ByteColumns) / Value);

      FByteColumns := Value;

      UpdatePaneWidths();
      // Keep caret in view, if it is now
      if (CaretLineOnScreen >= 0) and (CaretLineOnScreen < GetVisibleRowsCount()) then
        TopVisibleRow := CaretPos div ByteColumns - CaretLineOnScreen
      // else approximately track view position in file
      else
        TopVisibleRow := NewTopRow;
      UpdatePanes();

      OnByteColsChanged.Call(Self);
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TEditorForm.SetByteColumnsSetting(const Value: Integer);
begin
  if FByteColumnsSetting <> Value then
  begin
    FByteColumnsSetting := Value;
    CalculateByteColumns();
  end;
end;

procedure TEditorForm.SetCaretInByte(const Value: Integer);
begin
  if Value<>FCaretInByte then
  begin
    Assert((Value>=0) and (Value<=1), 'SetCaretInByte: '+IntToStr(Value));
    FCaretInByte := Value;
    UpdatePanesCarets();
  end;
end;

procedure TEditorForm.SetCaretPos(Value: TFilePointer);
begin
  Value := BoundValue(Value, 0, GetFileSize());
  if Value <> FCaretPos then
  begin
    BeginUpdatePanes();
    try
      // If caret moved away, start new "Typing" action in undo stack
      if (Value < FCaretPos) or (Value > FCaretPos + 1) then
        BreakCurrentTypingAction();

      FCaretPos := Value;
      ScrollToCaret();
      UpdatePanesCarets();
      SelectionChanged();
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TEditorForm.SetHasUnsavedChanges(const Value: Boolean);
begin
  if Value<>FHasUnsavedChanges then
  begin
    FHasUnsavedChanges := Value;
    MainForm.CheckEnabledActions();
    UpdateFormCaption();
    MainForm.UpdateMsgPanel();
  end;
end;

procedure TEditorForm.SetHorzScrollPos(Value: Integer);
begin
  // Validate
  if HorzScrollBar.Max = 0 then
    Value := 0
  else
    Value := BoundValue(Value, 0, HorzScrollBar.Max - HorzScrollBar.PageSize + 1);
  if FHorzScrollPos <> Value then
  begin
    FHorzScrollPos := Value;

    // Move scroll bar if scrolled programmatically
    if HorzScrollBar.Position<>FHorzScrollPos then
    begin
      HorzScrollBar.OnChange := nil;
      try
        HorzScrollBar.Position:=FHorzScrollPos;
      finally
        HorzScrollBar.OnChange := HorzScrollBarChange;
      end;
    end;
    UpdatePanes();
  end;
end;

procedure TEditorForm.SetInsertMode(Value: Boolean);
begin
  if not (dspResizable in DataSource.GetProperties) then Value := False;
  if FInsertMode <> Value then
  begin
    FInsertMode := Value;
    UpdatePanesCarets();
  end;
end;

procedure TEditorForm.SetSelection(AStart, AEnd: TFilePointer);
// AEnd is after last byte of selection
begin
  if AEnd=-1 then
  begin
    FSelStart := AStart;
    FSelLength := 0;
  end
  else
  begin
    if AStart>AEnd then
      Swap8Bytes(AStart, AEnd);
    FSelStart := AStart;
    FSelLength := AEnd-AStart;
  end;
  UpdatePanes();
  SelectionChanged();
end;

procedure TEditorForm.SetTextEncoding(const Value: Integer);
begin
  if Value <> FTextEncoding then
  begin
    FTextEncoding := Value;
    UpdatePanes();
  end;
end;

procedure TEditorForm.SetTopVisibleRow(Value: TFilePointer);
var
  MaxTopRow: TFilePointer;
begin
  // Validate
  MaxTopRow := DivRoundUp(GetFileSize()+1, ByteColumns) - GetVisibleRowsCount();
  if Value > MaxTopRow then Value := MaxTopRow;
  if Value < 0 then Value := 0;
  if Value<>FTopVisibleRow then
  begin

    FTopVisibleRow := Value;

    // Move scroll bar if scrolled programmatically
    if VertScrollBar.Position<>FTopVisibleRow div FLinesPerScrollBarTick then
    begin
      VertScrollBar.OnChange := nil;
      try
        VertScrollBar.Position:=FTopVisibleRow div FLinesPerScrollBarTick;
      finally
        VertScrollBar.OnChange := VertScrollBarChange;
      end;
    end;
    UpdatePanes();
  end;
end;

procedure TEditorForm.ShowSelectionInfo();
// Show info in statusbar about values under caret
var
  AData: TBytes;
  x: Int64;
begin
  if SelLength = 0 then
  begin
    StatusBar.Panels[0].Text := 'Addr: ' + IntToStr(CaretPos) + '( '+'0x' + IntToHex(CaretPos, 2) + ')';
    AData := GetEditedData(CaretPos, 1);
    if Length(AData)>=1 then
      StatusBar.Panels[1].Text := 'Byte: ' + IntToStr(AData[0])
    else
      StatusBar.Panels[1].Text := '';
  end
  else
  begin
    StatusBar.Panels[0].Text := 'Selected: ' + IntToStr(SelLength) + ' bytes';
    if (SelLength <= 8) then
    begin
      AData := GetEditedData(SelStart, SelLength);
      x := 0;
      Move(AData[0], x, Length(AData));
      StatusBar.Panels[1].Text := 'As '+IntToStr(Length(AData))+'-byte value: ' + IntToStr(x);
    end
    else
      StatusBar.Panels[1].Text := '';
  end;
end;

procedure TEditorForm.SomeDataChanged;
begin
  HasUnsavedChanges := Data.HasChanges();
  UpdateScrollBars();
  UpdatePanes();
  MainForm.UpdateMsgPanel();
  if Self = MainForm.ActiveEditor then
    MainForm.CheckEnabledActions();
end;

procedure TEditorForm.TypingActionChangeTimerTimer(Sender: TObject);
begin
  BreakCurrentTypingAction();
end;

procedure TEditorForm.EndUpdatePanes;
begin
  if FUpdating=0 then Exit;
  Dec(FUpdating);
  if FUpdating=0 then
  begin
    if FNeedUpdatePanes then
      UpdatePanes();
  end;

  PaneAddr.EndUpdate();
  PaneHex.EndUpdate();
  PaneText.EndUpdate();
end;

procedure TEditorForm.UndoActionCreating(Action: TUndoStackAction);
begin
  Action.SelStart := SelStart;
  Action.SelLength := SelLength;
end;

procedure TEditorForm.UndoActionReverted(Action: TUndoStackAction; Direction: TUndoStack.TUndoDirection);
begin
  // Restore selection when Undo'ing operation
  BeginUpdatePanes();
  try
    if Direction = udUndo then
    begin
      SetSelection(Action.SelStart, Action.SelStart + Action.SelLength);
      CaretPos := Action.SelStart + Action.SelLength;
      ScrollToCaret;
    end
    else
      MoveCaret(Action.SelStart + Action.SelLength, []);
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.UpdateFormCaption;
var
  s: string;
begin
  s := '';
  if DataSource.Path <> '' then
    s := s + DataSource.Path
  else
    s := s + '(unnamed)';
  if HasUnsavedChanges then
    s := s + ' *';
  Self.Caption := s;

  MainForm.UpdateMDITabs();
end;

procedure ByteToHex(x: Byte; Buf: PChar);
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';
begin
  Buf[0] := HexChars[(x shr 4) and $0F];
  Buf[1] := HexChars[(x) and $0F];
end;

procedure TEditorForm.UpdatePanes;
var
  AData: TBytes;
  i, len: Integer;
  Rows, AddrChars: Integer;
  Lines: TStringList;
  //s: AnsiString;
  s: RawByteString;
  c: AnsiChar;
  FirstVisibleAddress, VisibleRangeEnd: TFilePointer;
  FileSize: TFilePointer;
  IncludesFileEnd: Boolean;
  ws: string;
begin
  if FUpdating>0 then
  begin
    FNeedUpdatePanes := True;
    Exit;
  end;
  FNeedUpdatePanes := False;

//  StartTimeMeasure();

  BeginUpdatePanes();
  try
    Rows := GetVisibleRowsCount();
    FirstVisibleAddress := FirstVisibleAddr();

    // Get visible data
//    StartTimeMeasure();
    AData := GetEditedData(FirstVisibleAddress, Rows * ByteColumns);
//    EndTimeMeasure('GetData', True);

    VisibleRangeEnd := FirstVisibleAddress + Length(AData);
    IncludesFileEnd := (Length(AData) < Rows * ByteColumns);

    // Address
    Lines := PaneAddr.Lines;
    Lines.Clear();
    FileSize := GetFileSize();
    if FileSize > TFilePointer($FFFFFFFF) then
      AddrChars := 10
    else
    if FileSize > TFilePointer($FFFF) then
      AddrChars := 8
    else
      AddrChars := 4;
    for i:=0 to DivRoundUp(Length(AData), ByteColumns)-1 do
    begin
      Lines.Add(IntToHex(FirstVisibleAddress + i*ByteColumns, AddrChars));
    end;
    PaneAddr.Refresh();

    // Hex
//    StartTimeMeasure();
    Lines := PaneHex.Lines;
    Lines.Clear();
    ws := StringOfChar(' ', ByteColumns * 3);
    len := 0;
    for i:=0 to Length(AData)-1 do
    begin
      ByteToHex(AData[i], @ws[Low(ws) + len]);
      Inc(len, 3);
      if ((i+1) mod ByteColumns)=0 then
      begin
        Lines.Add(Copy(ws, Low(ws), len));
        len := 0;
      end;
    end;
    if (len>0) or (IncludesFileEnd) then
      Lines.Add(Copy(ws, Low(ws), len));
    PaneHex.HorzScrollPos := HorzScrollPos * 3;
//    EndTimeMeasure('Hex', True);

    // Text
//    StartTimeMeasure();
    Lines := PaneText.Lines;
    Lines.Clear();
    SetLength(s, ByteColumns);
    //s := StringOfChar(AnsiChar(' '), ByteColumns);
    SetLength(s, ByteColumns);
    FillChar(s[Low(s)], Length(s), AnsiChar(' '));
    SetCodePage(s, TextEncoding, False);
    len := 0;
    for i:=0 to Length(AData)-1 do
    begin
      if (AData[i] < Ord(' ')) or (AData[i] = $7F) or (AData[i] = $98) then
        c := '.'
      else
        c := AnsiChar(AData[i]);
      s[Low(s) + len] := c;
      Inc(len);
      if ((i+1) mod ByteColumns)=0 then
      begin
        Lines.Add(string(Copy(s, Low(s), len)));
        len := 0;
      end;
    end;
    if (len>0) or (IncludesFileEnd) then
      Lines.Add(string(Copy(s, Low(s), len)));
    PaneText.HorzScrollPos := HorzScrollPos;
//    EndTimeMeasure('Text', True);

//    StartTimeMeasure();
    UpdatePanesCarets();
//    EndTimeMeasure('UpdatePanesCarets', True);

    // Call events when visible range changes
    if (FirstVisibleAddress <> FPrevVisibleRange.Start) or
       (VisibleRangeEnd <> FPrevVisibleRange.AEnd) or
       (HorzScrollPos <> FPrevHorzScroll) then
    begin
      OnVisibleRangeChanged.Call(Self);
      if Self = MainForm.GetActiveEditorNoEx() then
        MainForm.VisibleRangeChanged();
      FPrevVisibleRange.Start := FirstVisibleAddress;
      FPrevVisibleRange.AEnd := VisibleRangeEnd;
      FPrevHorzScroll := HorzScrollPos;
    end;
  finally
//    StartTimeMeasure();
    EndUpdatePanes();
//    EndTimeMeasure('EndUpdatePanes', True);
  end;

  UpdateSkipFFButtons(AData);

//  EndTimeMeasure('UpdatePanes', True);
end;

procedure TEditorForm.UpdatePanesCarets;
// Update panes carets and text/background colors
var
  cp: TPoint;
  p, FirstVis: TFilePointer;
  VisSize: Integer;
  Parts: TEditedData.TDataPartList;
  TxColors, BgColors: TColorArray;
  DefBgColor, DefTxColor: TColor;
  i: Integer;

  procedure Update(Pane: TEditorPane; CharsPerByte: Integer);
  var
    L, i, n: Integer;
  begin
    L := VisSize*CharsPerByte;
    if Length(Pane.BgColors)<>L then
      SetLength(Pane.BgColors, L);
    if Length(Pane.TxColors)<>L then
      SetLength(Pane.TxColors, L);
//    StartTimeMeasure();
//    if CharsPerByte = 1 then
//    begin
//      Move(TxColors[0], Pane.TxColors[0], L*SizeOf(TxColors[0]));
//      Move(BgColors[0], Pane.BgColors[0], L*SizeOf(BgColors[0]));
//    end
//    else
    for i:=0 to L-1 do
    begin
      n := i div CharsPerByte;
      Pane.TxColors[i] := TxColors[n];
      Pane.BgColors[i] := BgColors[n];
    end;
//    EndTimeMeasure('Update pane colors', True);
    // Caret position
    Pane.CaretPos := Point(cp.X*CharsPerByte + IfThen(CharsPerByte>1, CaretInByte, 0), cp.Y);
    Pane.InsertModeCaret := InsertMode;
  end;

begin
  FirstVis := FirstVisibleAddr();
  VisSize := VisibleBytesCount();

  p := FCaretPos - FirstVis;
  cp := Point(p mod ByteColumns, p div ByteColumns);

  SetLength(TxColors, VisSize);
  SetLength(BgColors, VisSize);

  // Background colors
  DefBgColor := PaneHex.Color;
  DefTxColor := PaneHex.Font.Color;
  for i:=0 to VisSize-1 do
  begin
    BgColors[i] := DefBgColor;
    TxColors[i] := DefTxColor;
  end;

  // Changed bytes
  Parts := TEditedData.TDataPartList.Create(False);
  try
    Data.GetOverlappingParts(FirstVis, VisSize, Parts);
    for i:=0 to Parts.Count-1 do
      if Parts[i].PartType = ptBuffer then
        FillRangeInColorArray(BgColors, FirstVis, Parts[i].Addr, Parts[i].Addr+Parts[i].Size, Color_ChangedByte);
  finally
    Parts.Free;
  end;

  // Selection background
  FillRangeInColorArray(TxColors, FirstVis, SelStart, SelStart+SelLength, Color_SelectionTx);
  FillRangeInColorArray(BgColors, FirstVis, SelStart, SelStart+SelLength, Color_SelectionBg);

  // Tools
  MainForm.CompareFrame.GetDataColors(Self, FirstVis, VisSize, nil, TxColors, BgColors);
  MainForm.StructFrame.GetDataColors(Self, FirstVis, VisSize, nil, TxColors, BgColors);
  MainForm.ValueFrame.GetDataColors(Self, FirstVis, VisSize, nil, TxColors, BgColors);

  Update(PaneHex, 3);
  Update(PaneText, 1);
end;

procedure TEditorForm.UpdatePaneWidths;
// Calculate HexPane width
var
  HexWidth: Integer;
begin
  HexWidth := (FByteColumns * 3 + 1) * PaneHex.CharWidth;
  HexWidth := Min(HexWidth, (VertScrollBar.Left - PaneHex.Left - Shape2.Width) * 3 div 4 );
  HexWidth := Max(HexWidth, PaneHex.CharWidth * 3);
  PaneHex.Width := HexWidth;
  UpdateScrollBars();
end;

procedure TEditorForm.UpdateScrollBars;
var
  FileRows: TFilePointer;
  ScreenRows: Integer;
begin
  // Horizontal scroll bar
  if ByteColumns > GetVisibleColsCount(False) then
  begin
    HorzScrollBar.Visible := True;
    HorzScrollBar.Max := ByteColumns - 1;
    HorzScrollBar.PageSize := GetVisibleColsCount(False);
    HorzScrollBar.LargeChange := HorzScrollBar.PageSize;
  end
  else
  begin
    HorzScrollBar.Visible := False;
    HorzScrollBar.PageSize := 0;
    HorzScrollBar.Max := 0;
  end;

  // Vertical scroll bar
  FileRows := DivRoundUp(GetFileSize()+1, ByteColumns);
  ScreenRows := GetVisibleRowsCount();
  if (FileRows > ScreenRows) then
  begin
    // ScrollBar.Position is limited by 2^32, so for large files make
    // one scrollbar step more then one line
    FLinesPerScrollBarTick := Max(FileRows div 100000000{100 M lines}, 1);
    VertScrollBar.Max := (FileRows div FLinesPerScrollBarTick) - 1;
    VertScrollBar.PageSize := (ScreenRows div FLinesPerScrollBarTick);
    VertScrollBar.LargeChange := VertScrollBar.PageSize;
  end
  else
  begin
    FLinesPerScrollBarTick := 1;
    VertScrollBar.PageSize := 0;
    VertScrollBar.Max := 0;
  end;
end;

procedure TEditorForm.UpdateSkipFFButtons(const AData: TBytes);
// Show "Skip FF" buttons if there is a lot of repeating bytes
// in start/end of viewed area

  function ShowBtn(Btn: TSpeedButton; n1, n2: Integer; var AByte: Byte; HintTemplate: string): Boolean;
  var
    Vis: Boolean;
    i: Integer;
  begin
    if (Length(AData) = 0) or
       ((Btn.Tag = -1) and (n1 + FirstVisibleAddr() = 0)) or
       ((Btn.Tag =  1) and (n2 + FirstVisibleAddr() = GetFileSize() - 1)) then
      // Don't show if we are at file start/end
      Vis := False
    else
    begin
      Vis := True;
      for i:=n1+1 to n2 do
        if AData[i] <> AData[n1] then
        begin
          Vis := False;
          Break;
        end;
    end;
    Btn.Visible := Vis;
    if Vis then
    begin
      AByte := AData[n1];
      Btn.Caption := 'Skip '+IntToHex(AByte, 2);
      Btn.Hint := HintTemplate.Replace('%', IntToHex(AByte, 2));
    end;
    Result := Vis;
  end;

begin
  ShowBtn(BtnSkipFFBack, 0, Min(Length(AData) div 4, 4096), FFSkipBackByte, 'Skip backward to previous non-% byte');
  ShowBtn(BtnSkipFFFwd, High(AData) - Min(Length(AData) div 4, 4096), High(AData), FFSkipFwdByte, 'Skip forward to next non-% byte');
end;

function TEditorForm.VisibleBytesCount: Integer;
begin
  Result := GetVisibleRowsCount() * ByteColumns;
end;

procedure TEditorForm.OpenNewEmptyFile;
begin
  DataSource := TFileDataSource.Create('New file');

  NewFileOpened(True);
end;

//function TEditorForm.GetFileSizeOle: TFilePointer;
//begin
//  Result := GetFileSize();
//end;

//function TEditorForm.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
//  LocaleID: Integer; DispIDs: Pointer): HResult;
////var
////  S: string;
////  Info: PPropInfo;
//begin
//   Result := S_OK;
////   // Получаем имя функции или свойства
////   S := PPChar(Names)^;
////   // Проверяем, есть ли VCL-свойство с таким же именем
////   Info := GetPropInfo(ClassInfo, S);
////   if Assigned(Info) then
////     // Свойство есть, возвращаем в качестве DispId
////     // адрес структуры PropInfo
//     PDispIDList(DispIds)[0] := 1;// Integer(Info);
//end;

{ TFFSkipSearcher }

function TFFSkipSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
// Return True if we found different byte
begin
  if Data^ <> Params.Needle[0] then
  begin
    Size := 1;
    Result := True;
  end
  else
    Result := False;
end;

end.

