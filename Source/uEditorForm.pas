{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Generics.Collections, Math, System.Types, Vcl.Menus,
  System.Win.ComObj, System.TypInfo, Winapi.ActiveX, Vcl.Buttons,
  System.ImageList, Vcl.ImgList, System.IOUtils,

  uHextorTypes, uHextorDataSources, uEditorPane, uEditedData,
  uCallbackList, uDataSearcher, uUndoStack, {uLogFile,} uOleAutoAPIWrapper,
  uDataSaver, uModuleSettings, uHextorGUI;

type
  TEditorForm = class;

  TEditorSettings = class (TModuleSettings)
  public
    ScrollWithWheel: Integer;
    ByteColumns: Integer;  // -1 - auto
    HighlightMatches: Boolean;
    procedure InitDefault(); override;
  end;

  TFFSkipSearcher = class(TCustomDataSearcher)
  // Searcher for skipping given byte value
  public
    SkipByte: Byte;
    constructor Create();
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; override;
  end;

  TEditorForm = class(TForm)
    PaneHex: TEditorPane;
    PaneAddr: TEditorPane;
    PaneText: TEditorPane;
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
    VertScrollBar: TScrollBar64;
    PMIShowDSField: TMenuItem;
    PMIResyncCompare: TMenuItem;
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
    procedure PaneHexBeforeDraw(Sender: TEditorPane; Canvas: TCanvas);
    procedure PaneHexAfterDraw(Sender: TEditorPane; Canvas: TCanvas);
    procedure PaneHexContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PMIShowDSFieldClick(Sender: TObject);
    procedure PMIResyncCompareClick(Sender: TObject);
  private
    { Private declarations }
    FData: TEditedData;
    FClosed: Boolean;  // Some events (e.g. FormResize) are oddly called after form destruction
    FCaretPos: TFilePointer;
    FSelStart, FSelLength: TFilePointer;
    SelDragStart, SelDragEnd: TFilePointer;
    WasLeftMouseDown: Boolean;
    FCaretInByte: Integer;
    TypingActionCode: Integer;  // To combine sequentially typed chars into one "Undo" action
    FHasUnsavedChanges: Boolean;
    FTopVisibleRow: TFilePointer;
    FUpdating: Integer;
    FNeedUpdatePanes: Boolean;          // We need to call UpdatePanes() when long operation ends
    FNeedCallSomeDataChanged: Boolean;  // We need to call SomeDataChanged() when long operation ends
    FNeedCallSelectionChanged: Boolean; // We need to call SelectionChanged() when long operation ends
    FByteColumns: Integer;
    FInsertMode: Boolean;
    FByteColumnsSetting: Integer;
    FPrevVisibleRange: TFileRange;
    FPrevHorzScroll: Integer;
    FHorzScrollPos: Integer;
    FFSkipSearcher: TFFSkipSearcher;
    FFSkipBackByte, FFSkipFwdByte: Byte;
    FTextEncoding: Integer;
    FNowSelecting: Boolean;
    class var FSettings: TEditorSettings;
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
    procedure SetInsertMode(Value: Boolean);
    procedure AdjustPointersPositions(Addr, OldSize, NewSize: TFilePointer);
    procedure DataChanged(Sender: TEditedData; Addr: TFilePointer; OldSize, NewSize: TFilePointer; Value: PByteArray);
    procedure SomeDataChanged();
    procedure UndoActionCreating(Action: TUndoStackAction);
    procedure UndoActionReverted(Action: TUndoStackAction; Direction: TUndoStack.TUndoDirection);
    procedure SetByteColumnsSetting(const Value: Integer);
    procedure SetHorzScrollPos(Value: Integer);
    procedure BreakCurrentTypingAction();
    procedure SetTextEncoding(const Value: Integer);
    function GetSelectedRange: TFileRange;
    procedure SetSelectedRange(const Value: TFileRange);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
//  public type
//    TSaveMethod = (smUnknown, smPartialInplace, smFull, smTempFile);
  public type
    TCaretInSelection = (CaretNoMove, CaretAtStart, CaretAtEnd);
  public const
    MaxHighlightedSelMatch = 65536;
  public
    { Public declarations }
    DataSource: THextorDataSource;
    UndoStack: TUndoStack;
    OnClosed: TCallbackListP1<TEditorForm>;
    OnVisibleRangeChanged: TCallbackListP1<TEditorForm>;
    OnSelectionChanged: TCallbackListP1<TEditorForm>;  // Called when either selection moves or data in selected range changes
    OnByteColsChanged: TCallbackListP1<TEditorForm>;
    OnGetTaggedRegions: TCallbackListP5<{Editor: }TEditorForm, {Start: }TFilePointer,
      {AEnd: }TFilePointer, {AData: }PByteArray, {Regions: }TTaggedDataRegionList>;
    OnBeforeDrawPane: TCallbackListP3<{Editor: }TEditorForm, {Pane: }TEditorPane, {Canvas: }TCanvas>;
    OnAfterDrawPane: TCallbackListP3<{Editor: }TEditorForm, {Pane: }TEditorPane, {Canvas: }TCanvas>;
    [API]
    property Data: TEditedData read FData write FData;
    destructor Destroy(); override;
    function AskSaveChanges(): TModalResult;
    procedure Open(DataSourceType: THextorDataSourceType; const APath: string; ResetCaret: Boolean);
    procedure OpenNewEmptyFile(const FileName: string);
    function CloseCurrentFile(AskSave: Boolean): TModalResult;
    procedure SaveAs(DataSourceType: THextorDataSourceType; APath: string);
    [API]
    procedure SaveAsFile(APath: string);
    [API]
    procedure Save();
    procedure NewFileOpened(ResetCaret: Boolean);
    function GetEditedData(Addr, Size: TFilePointer): TBytes;
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
    function GetByteScreenPosition(Pane: TEditorPane; Addr: TFilePointer; var Pos: TPoint): Boolean;
    function GetByteScreenRect(Pane: TEditorPane; Addr: TFilePointer; var Rect: TRect): Boolean;
    function GetByteAtScreenCoord(Pane: TEditorPane; Coord: TPoint; var Addr: TFilePointer; {out} CaretInByte: PInteger = nil): Boolean;
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
    property SelectedRange: TFileRange read GetSelectedRange write SetSelectedRange;
    property NowSelecting: Boolean read FNowSelecting;
    [API]
    property InsertMode: Boolean read FInsertMode write SetInsertMode;
    procedure MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
    [API]
    procedure SetSelection(AStart, AEnd: TFilePointer; MoveCaret: TCaretInSelection = CaretNoMove);
    procedure ScrollToShow(Addr: TFilePointer; RowsFromBorder: Integer = 0; ColsFromBorder: Integer = 0);
    procedure ScrollToCaret();
    procedure SelectAndShow(AStart, AEnd: TFilePointer; MoveCaret: TCaretInSelection = CaretAtStart);
    [API]
    procedure BeginUpdate();
    [API]
    procedure EndUpdate();
    property HasUnsavedChanges: Boolean read FHasUnsavedChanges write SetHasUnsavedChanges;
    property TopVisibleRow: TFilePointer read FTopVisibleRow write SetTopVisibleRow;
    property HorzScrollPos: Integer read FHorzScrollPos write SetHorzScrollPos;
    property ByteColumns: Integer read FByteColumns write SetByteColumns;
    property ByteColumnsSetting: Integer read FByteColumnsSetting write SetByteColumnsSetting;
    procedure CalculateByteColumns();
    function GetSelectedOrAfterCaret(DefaultSize, MaxSize: Integer; var Addr: TFilePointer; NothingIfMore: Boolean = False): TBytes;
    property TextEncoding: Integer read FTextEncoding write SetTextEncoding;
    [API]
    procedure InsertDataFromFile(const FileName: string; Addr: TFilePointer; Overwrite: Boolean);
    [API]
    procedure InsertHex(const HexText: string; Addr: TFilePointer; Overwrite: Boolean);
    class function Settings(): TEditorSettings;
    function HasEventListener(Id: Pointer): Boolean;
    procedure RemoveEventListener(Id: Pointer);
  end;

procedure ConfigureScrollbar(AScrollBar: TScrollBar64; AMax, APageSize: Int64);

var
  EditorForm: TEditorForm;

implementation

uses
  uMainForm, uValueFrame, uDbgToolsForm, uDataStruct;

{$R *.dfm}

procedure ConfigureScrollbar(AScrollBar: TScrollBar64; AMax, APageSize: Int64);
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
  Recent: TMainFormSettings.TRecentFileRec;
  n, i: Integer;
begin
  if (DataSource.ClassType <> TFileDataSource) or (ExtractFilePath(DataSource.Path) = '') then Exit;

  n := -1;
  for i:=0 to Length(MainForm.Settings.RecentFiles)-1 do
    if SameFileName(MainForm.Settings.RecentFiles[i].FileName, DataSource.Path) then
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
    Recent := MainForm.Settings.RecentFiles[n];
    Delete(MainForm.Settings.RecentFiles, n, 1);
  end;
  Insert([Recent], MainForm.Settings.RecentFiles, 0);
  if Length(MainForm.Settings.RecentFiles) > 20 then
    SetLength(MainForm.Settings.RecentFiles, 20);

  MainForm.Settings.Changed(True);
end;

procedure TEditorForm.AdjustPointersPositions(Addr, OldSize, NewSize: TFilePointer);
// Adjust Caret position, bookmarks etc. after operation that inserted or deleted data
var
  ASelEnd: TFilePointer;
begin
  AdjustPositionInData(FCaretPos, Addr, OldSize, NewSize);
  ASelEnd := SelStart + SelLength;
  AdjustPositionInData(FSelStart, Addr, OldSize, NewSize);
  AdjustPositionInData(ASelEnd, Addr, OldSize, NewSize);
  FSelLength := ASelEnd - FSelStart;

  AdjustPositionInData(SelDragStart, Addr, OldSize, NewSize);
  AdjustPositionInData(SelDragEnd, Addr, OldSize, NewSize);
end;

function TEditorForm.AskSaveChanges: TModalResult;
begin
  Result := mrNo;
  if HasUnsavedChanges then
  begin
    Result := Application.MessageBox(PChar('Save changes to '+sLineBreak+DataSource.DisplayName+'?'), 'Closing', MB_YESNOCANCEL);
    case Result of
      mrYes: MainForm.ActionSaveExecute(nil);
    end;
  end;
end;

procedure TEditorForm.BeginUpdate;
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
    FFSkipSearcher.SkipByte := FFSkipBackByte;
    Start := FirstVisibleAddr() - 1;
  end
  else
  begin
    FFSkipSearcher.SkipByte := FFSkipFwdByte;
    Start := FirstVisibleAddr() + VisibleBytesCount();
  end;

  Progress.TaskStart(FFSkipSearcher);
  try
    FFSkipSearcher.FindNext(Start, Dir, Ptr, Size);
  finally
    Progress.TaskEnd();
  end;
  BeginUpdate();
  try
    ScrollToShow(Ptr, -1, -1);
  finally
    EndUpdate();
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

function TEditorForm.CloseCurrentFile(AskSave: Boolean): TModalResult;
begin
  if (AskSave) then
  begin
    Result := AskSaveChanges();
    if Result = mrCancel then Exit;
  end;
  Result := mrNo;

  FreeAndNil(DataSource);
end;

procedure TEditorForm.DataChanged(Sender: TEditedData; Addr: TFilePointer; OldSize,
  NewSize: TFilePointer; Value: PByteArray);
begin
  if OldSize <> NewSize then
    AdjustPointersPositions(Addr, OldSize, NewSize);

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
  MainForm.RemoveEditor(Self);
  UndoStack.Free;
  Data.Free;
  DataSource.Free;
  FFSkipSearcher.Free;
  inherited;
end;

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  if FClosed then Exit;
  if DataSource <> nil then
    MainForm.CheckActiveEditorChanged();
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  OnClosed.Call(Self);
  FClosed := True;
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AskSaveChanges() = IDCANCEL then
    CanClose := False;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  Data := TEditedData.Create();
  Data.OnDataChanged.Add(DataChanged);
  UndoStack := TUndoStack.Create(Data);
  UndoStack.OnActionCreating.Add(UndoActionCreating);
  UndoStack.OnActionReverted.Add(UndoActionReverted);

  ByteColumnsSetting := Settings.ByteColumns;
  CalculateByteColumns();
  FFSkipSearcher := TFFSkipSearcher.Create();
  OnGetTaggedRegions.Add(EditorGetTaggedRegions);

  MainForm.AddEditor(Self);
end;

procedure TEditorForm.FormDeactivate(Sender: TObject);
begin
  if FClosed then Exit;
  UpdatePanesCarets();  // Gray caret
end;

procedure TEditorForm.FormResize(Sender: TObject);
begin
  if FClosed then Exit;
  BeginUpdate();
  try
    CalculateByteColumns();
    UpdatePaneWidths();
    UpdatePanes();
  finally
    EndUpdate();
  end;
end;

procedure TEditorForm.PaneHexAfterDraw(Sender: TEditorPane; Canvas: TCanvas);
begin
  OnAfterDrawPane.Call(Self, Sender, Canvas);
end;

procedure TEditorForm.PaneHexBeforeDraw(Sender: TEditorPane; Canvas: TCanvas);
begin
  OnBeforeDrawPane.Call(Self, Sender, Canvas);
end;

procedure TEditorForm.PaneHexContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Addr: TFilePointer;
  TaggedRegions: TTaggedDataRegionList;
  i: Integer;
begin
  if GetByteAtScreenCoord(Sender as TEditorPane, MousePos, Addr) then
  begin
    // DataStruct element under cursor?
    PMIShowDSField.Visible := False;
    TaggedRegions := TTaggedDataRegionList.Create(TFileRange.Create(Addr, Addr + 1));
    try
      OnGetTaggedRegions.Call(Self, Addr, Addr + 1, nil, TaggedRegions);

      for i := TaggedRegions.Count-1 downto 0 do
        if TaggedRegions[i].Owner = MainForm.StructFrame then
        begin
          PMIShowDSField.Tag := NativeUInt(TaggedRegions[i].Data);
          PMIShowDSField.Caption := TDSField(TaggedRegions[i].Data).FullName();
          PMIShowDSField.Visible := True;
          break;
        end;
    finally
      TaggedRegions.Free;
    end;

  end;

  // Resync comparison from here
  if (MainForm.CompareFrame.Editors[0] = Self) or (MainForm.CompareFrame.Editors[1] = Self) then
    PMIResyncCompare.Visible := True
  else
    PMIResyncCompare.Visible := False;
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
  BeginUpdate();
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
    EndUpdate();
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
    BeginUpdate();
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
      Buf := GetEditedData(APos, 1);
      // Allow typing beyong end of file
      if (dspResizable in DataSource.GetProperties()) and
         (Length(Buf) = 0) and (APos = Data.GetSize()) then
        Buf := [0];
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
      EndUpdate();
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
    FNowSelecting := True;
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
  begin
    WasLeftMouseDown := False;
    FNowSelecting := False;
    UpdatePanesCarets();
  end;
end;

procedure TEditorForm.PaneHexMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TopVisibleRow := TopVisibleRow - WheelDelta div 120 * Settings.ScrollWithWheel;
end;

function TEditorForm.GetByteAtScreenCoord(Pane: TEditorPane; Coord: TPoint;
  var Addr: TFilePointer; {out} CaretInByte: PInteger = nil): Boolean;
// Get address of byte at given screen coordinates (in pixels)
var
  Index, ACaretInByte: Integer;
  p: TPoint;
begin
  if Pane.GetCharAt(Coord.X, Coord.Y, p, Index) then
  begin
    if Pane = PaneHex then
    begin
      ACaretInByte := BoundValue(p.X mod 3, 0, 1);
      Index := Index div 3;
    end
    else
      ACaretInByte := 0;
    Addr := FirstVisibleAddr() + Index;
    if CaretInByte <> nil then
      CaretInByte^ := ACaretInByte;
    Result := True;
  end
  else
    Result := False;
end;

function TEditorForm.GetByteScreenPosition(Pane: TEditorPane; Addr: TFilePointer;
  var Pos: TPoint): Boolean;
// Returns Row/Column of given byte on Pane, if this byte is visible on screen now.
// Horizontal scroll does not affects it (it is handled by Pane itself).
// Pos is in chars.
var
  FirstVis, VisCount: TFilePointer;
  N: Integer;
begin
  FirstVis := FirstVisibleAddr();
  VisCount := VisibleBytesCount();
  if (Addr < FirstVis) or (Addr >= FirstVis + VisCount) then Exit(False);
  N := Addr - FirstVis;
  Pos.Y := N div ByteColumns;
  Pos.X := N mod ByteColumns;
  if Pane = PaneAddr then Pos.X := 0
  else if Pane = PaneHex then Pos.X := Pos.X * 3;
  Result := True;
end;

function TEditorForm.GetByteScreenRect(Pane: TEditorPane; Addr: TFilePointer;
  var Rect: TRect): Boolean;
// Returns screen position of given byte on Pane, if this byte is visible on screen now.
// Horizontal scroll does not affects it (it is handled by Pane itself)
var
  p: TPoint;
begin
  Result := GetByteScreenPosition(Pane, Addr, p);
  if not Result then Exit;
  Rect := Pane.GetCharRect(p);
  if Pane = PaneHex then
    Rect.Width := Rect.Width * 3;
end;

function TEditorForm.GetEditedData(Addr, Size: TFilePointer): TBytes;
begin
  Result := Data.Get(Addr, Size);
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

function TEditorForm.GetSelectedRange: TFileRange;
begin
  Result.Start := SelStart;
  Result.AEnd := SelStart + SelLength;
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

function TEditorForm.HasEventListener(Id: Pointer): Boolean;
// True if this listener is subscribed on some events of this Editor
begin
  Result :=
    OnClosed.HasListener(Id) or
    OnVisibleRangeChanged.HasListener(Id) or
    OnSelectionChanged.HasListener(Id) or
    OnByteColsChanged.HasListener(Id) or
    OnGetTaggedRegions.HasListener(Id) or
    OnBeforeDrawPane.HasListener(Id) or
    OnAfterDrawPane.HasListener(Id);
end;

procedure TEditorForm.HorzScrollBarChange(Sender: TObject);
begin
  HorzScrollPos := HorzScrollBar.Position;
end;

procedure TEditorForm.InsertDataFromFile(const FileName: string; Addr: TFilePointer;
  Overwrite: Boolean);
var
  Buf: TBytes;
begin
  Buf := TFile.ReadAllBytes(FileName);
  if Overwrite then
    Data.Change(Addr, Length(Buf), @Buf[0])
  else
    Data.Insert(Addr, Length(Buf), @Buf[0]);
end;

procedure TEditorForm.InsertHex(const HexText: string; Addr: TFilePointer;
  Overwrite: Boolean);
var
  Buf: TBytes;
begin
  Buf := Hex2Data(HexText);
  if Overwrite then
    Data.Change(Addr, Length(Buf), @Buf[0])
  else
    Data.Insert(Addr, Length(Buf), @Buf[0]);
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
  SetSelection(SelDragStart, SelDragEnd, CaretNoMove);

  CaretPos := NewPos;
  ScrollToCaret();
end;

procedure TEditorForm.NewFileOpened(ResetCaret: Boolean);
begin
  BeginUpdate();
  try
    Data.DataSource := DataSource;
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
    EndUpdate();
  end;

  AddCurrentFileToRecentFiles();
end;

procedure TEditorForm.PaneMouseMove(Sender: TObject; IsMouseDown: Boolean;
  Shift: TShiftState; X, Y: Integer);
// Called only while left mouse button is pressed
var
  ACaretPos: TFilePointer;
  ACaretInByte: Integer;
  ss: TShiftState;
begin
  BeginUpdate();
  try
    if GetByteAtScreenCoord(Sender as TEditorPane, Point(X, Y), ACaretPos, @ACaretInByte) then
    begin
      ss := Shift;
      if not IsMouseDown then
        ss := ss + [ssShift];
      MoveCaret(ACaretPos, ss);
      CaretInByte := ACaretInByte;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TEditorForm.PaneTextKeyPress(Sender: TObject; var Key: Char);
var
  s: RawByteString;
  c: AnsiChar;
begin
  if Key >= ' ' then
  begin
    BeginUpdate();
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
      EndUpdate();
    end;
  end;
end;

procedure TEditorForm.PMIResyncCompareClick(Sender: TObject);
begin
  MainForm.CompareFrame.ResyncFromCursors();
end;

procedure TEditorForm.PMIShowDSFieldClick(Sender: TObject);
begin
  MainForm.StructFrame.SelectFieldInTree(Pointer(PMIShowDSField.Tag));
end;

procedure TEditorForm.RemoveEventListener(Id: Pointer);
// Remove given listener from all events of this Editor
begin
  OnClosed.Remove(Id);
  OnVisibleRangeChanged.Remove(Id);
  OnSelectionChanged.Remove(Id);
  OnByteColsChanged.Remove(Id);
  OnGetTaggedRegions.Remove(Id);
  OnBeforeDrawPane.Remove(Id);
  OnAfterDrawPane.Remove(Id);
  Data.OnDataChanged.Remove(Id);
  Data.OnBeforePartsReplace.Remove(Id);
end;

procedure TEditorForm.ReplaceSelected(NewSize: TFilePointer; Value: PByteArray);
var
  NewCaretPos: TFilePointer;
begin
  BeginUpdate();
  try
    NewCaretPos := SelStart + NewSize;
    Data.Change(SelStart, SelLength, NewSize, Value);
    MoveCaret(NewCaretPos, []);
    ScrollToCaret();
  finally
    EndUpdate();
  end;
end;

//function TEditorForm.QueryInterface(const IID: TGUID; out Obj): HResult;
//begin
//  Result := inherited;
//end;

procedure TEditorForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := TFilePointer(VertScrollBar.Position);
end;

procedure TEditorForm.Save;
begin
  if not DataSource.CanBeSaved() then
    raise EInvalidUserInput.Create('Cannot save to this target');
  SaveAs(THextorDataSourceType(DataSource.ClassType), DataSource.Path);
end;

procedure TEditorForm.SaveAs(DataSourceType: THextorDataSourceType; APath: string);
begin
  if SameDataSource(DataSource, DataSourceType, APath) then
  begin
    // Save to same file
    TDataSaver.Save(Data, DataSource);
    DataSource := Data.DataSource;
    NewFileOpened(False);
  end
  else
  begin
    // Save to another file
    TDataSaver.Save(Data, DataSourceType, APath);
    Open(DataSourceType, APath, False);
  end;
end;

procedure TEditorForm.SaveAsFile(APath: string);
begin
  SaveAs(TFileDataSource, APath);
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
  BeginUpdate();
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
    EndUpdate();
  end;
end;

procedure TEditorForm.SelectAndShow(AStart, AEnd: TFilePointer;
  MoveCaret: TCaretInSelection = CaretAtStart);
// Select specified range and scroll it to view.
// Caret is placed at start or at end of range.
begin
  BeginUpdate();
  try
    ScrollToShow(AStart, -1, -1);
    SetSelection(AStart, AEnd, MoveCaret);
  finally
    EndUpdate();
  end;
end;

procedure TEditorForm.SelectionChanged;
begin
  if FUpdating>0 then
  begin
    FNeedCallSelectionChanged := True;
    Exit;
  end;
  FNeedCallSelectionChanged := False;

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
    BeginUpdate();
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
      EndUpdate();
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
    BeginUpdate();
    try
      // If caret moved away, start new "Typing" action in undo stack
      if (Value < FCaretPos) or (Value > FCaretPos + 1) then
        BreakCurrentTypingAction();

      FCaretPos := Value;
      ScrollToCaret();
      UpdatePanesCarets();
      SelectionChanged();
    finally
      EndUpdate();
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

procedure TEditorForm.SetSelectedRange(const Value: TFileRange);
begin
  SetSelection(Value.Start, Value.AEnd);
end;

procedure TEditorForm.SetSelection(AStart, AEnd: TFilePointer; MoveCaret: TCaretInSelection = CaretNoMove);
// AEnd is after last byte of selection
var
  Tmp: TFilePointer;
begin
  if AEnd=-1 then
  begin
    FSelStart := AStart;
    FSelLength := 0;
  end
  else
  begin
    if AStart>AEnd then
    begin
      Tmp := AStart;
      AStart := AEnd;
      AEnd := Tmp;
    end;
    FSelStart := AStart;
    FSelLength := AEnd-AStart;
  end;
  BeginUpdate();
  try
    case MoveCaret of
      CaretAtStart: CaretPos := FSelStart;
      CaretAtEnd:   CaretPos := FSelStart + FSelLength;
    end;
    UpdatePanes();
    SelectionChanged();
  finally
    EndUpdate();
  end;
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
    if VertScrollBar.Position<>FTopVisibleRow then
    begin
      VertScrollBar.OnChange := nil;
      try
        VertScrollBar.Position:=FTopVisibleRow;
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
  if FUpdating>0 then
  begin
    FNeedCallSomeDataChanged := True;
    Exit;
  end;
  FNeedCallSomeDataChanged := False;

  HasUnsavedChanges := Data.HasChanges();
  UpdateScrollBars();
  UpdatePanes();
  if Self = MainForm.ActiveEditor then
    MainForm.CheckEnabledActions();
end;

procedure TEditorForm.TypingActionChangeTimerTimer(Sender: TObject);
begin
  BreakCurrentTypingAction();
end;

procedure TEditorForm.EditorGetTaggedRegions(Editor: TEditorForm; Start,
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  SrcRegions: TSourceRegionArray;
  Parts: TEditedData.TDataPartList;
  i, Len: Integer;
  Sel: TFileRange;
  Pattern: TBytes;
begin
  // Data Source native regions
  if DataSource <> nil then
  begin
    // TODO: Regions may move when data changed?
    SrcRegions := DataSource.GetRegions(TFileRange.Create(Start, AEnd));
    try
      for i := 0 to Length(SrcRegions)-1 do
      begin
        Regions.AddRegion(Self, SrcRegions[i].Range.Start, SrcRegions[i].Range.AEnd, IfThen(SrcRegions[i].HasData, clNone, Color_NoDataTx), clNone, Color_SrcRegionFr);
      end;
    finally
      SrcRegions.Free;
    end;
  end;

  // Changed bytes
  Parts := TEditedData.TDataPartList.Create(False);
  try
    Data.GetOverlappingParts(Start, AEnd - Start, Parts);
    for i:=0 to Parts.Count-1 do
      if (Parts[i].PartType = ptBuffer) then
        Regions.AddRegion(Self, Parts[i].Addr, Parts[i].Addr+Parts[i].Size, clNone, Color_ChangedByte, clNone);
  finally
    Parts.Free;
  end;

  // Blocks matching selection
  if Settings.HighlightMatches and not NowSelecting and (AData <> nil) then
  begin
    Sel := Editor.SelectedRange;
    if (Sel.Size > 0) and (Sel.Size <= MaxHighlightedSelMatch) then
    begin
      Pattern := Editor.Data.Get(Sel.Start, Sel.Size);
      Len := Length(Pattern);
      if Len > 0 then
      begin
        i := 0;
        while i <= AEnd - Start - Len do
        begin
          if CompareMem(@AData[i], @Pattern[0], Len) then
          begin
            if Start + i <> Sel.Start then
              Regions.AddRegion(Self, Start + i, Start + i + Len, clNone, Color_FoundItemBg, Color_FoundItemFr);
            Inc(i, Len);
          end
          else
            Inc(i);
        end;
      end;
    end;
  end;

end;

class function TEditorForm.Settings: TEditorSettings;
// Returns single Settings instance for all editor forms
begin
  if FSettings = nil then
    FSettings := TEditorSettings.Create();
  Result := FSettings;
end;

procedure TEditorForm.EndUpdate;
begin
  if FUpdating=0 then Exit;
  Dec(FUpdating);
  if FUpdating=0 then
  begin
    if FNeedUpdatePanes then
      UpdatePanes();
    if FNeedCallSomeDataChanged then
      SomeDataChanged();
    if FNeedCallSelectionChanged then
      SelectionChanged();
  end;

  PaneAddr.EndUpdate();
  PaneHex.EndUpdate();
  PaneText.EndUpdate();
end;

procedure TEditorForm.UndoActionCreating(Action: TUndoStackAction);
begin
  Action.SelBefore.CaretPos := CaretPos;
  Action.SelBefore.SelStart := SelStart;
  Action.SelBefore.SelLength := SelLength;
end;

procedure TEditorForm.UndoActionReverted(Action: TUndoStackAction; Direction: TUndoStack.TUndoDirection);
begin
  // Restore selection when Undo'ing operation
  BeginUpdate();
  try
    if Direction = udUndo then
    begin
      SetSelection(Action.SelBefore.SelStart, Action.SelBefore.SelStart + Action.SelBefore.SelLength, CaretNoMove);
      CaretPos := Action.SelBefore.CaretPos;
    end
    else
      // TODO: Save and restore selection after action (add TEditedData.OnAfterPartsReplace?)
      MoveCaret(Action.SelBefore.SelStart + Action.SelBefore.SelLength, []);
  finally
    EndUpdate();
  end;
end;

procedure TEditorForm.UpdateFormCaption;
var
  s: string;
begin
  s := '';
  if DataSource.DisplayName <> '' then
    s := s + DataSource.DisplayName
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
  t: Cardinal;

  function ToDisplayedString(Buf: RawByteString): string;
  var
    i: Integer;
  begin
    for i:=Low(Buf) to High(Buf) do
    begin
      if ((Buf[i] < ' ') {and (Buf[i] <> #$0A) and (Buf[i] <> #$0D)}) or
         (Buf[i] = #$7F) or (Buf[i] = #$98) then
        Buf[i] := '.';
    end;
    Result := string(Buf);
//    // Carriage return symbols
//    for i:=Low(Result) to High(Result) do
//    begin
//      if (Result[i] = #$000D) or (Result[i] = #$000A) then
//        Result[i] := #$21B5;
//    end;
  end;

begin
  if FUpdating>0 then
  begin
    FNeedUpdatePanes := True;
    Exit;
  end;
  FNeedUpdatePanes := False;

//  StartTimeMeasure();
  t := GetTickCount();

  BeginUpdate();
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
      c := AnsiChar(AData[i]);
      s[Low(s) + len] := c;
      Inc(len);
      if (((i+1) mod ByteColumns) = 0) or
         (i = Length(AData)-1) then
      begin
        Lines.Add(ToDisplayedString(Copy(s, Low(s), len)));
        len := 0;
      end;
    end;
    if (IncludesFileEnd) and
       ((Lines.Count = 0) or (Length(Lines[Lines.Count-1]) = ByteColumns)) then
      Lines.Add('');
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
    EndUpdate();
//    EndTimeMeasure('EndUpdatePanes', True);
    t := GetTickCount() - t;
    DbgToolsForm.LblUpdateTime.Caption := 'Update: ' + IntToStr(t) + ' ms';
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
  TaggedRegions: TTaggedDataRegionList;
  AData: TBytes;

  procedure Update(Pane: TEditorPane; CharsPerByte: Integer);
  var
    i: Integer;
    TextRegions: TEditorPane.TVisualTextRegionArray;
  begin
    // Convert TaggedRegions on data to VisualTextRegions on pane chars
    SetLength(TextRegions, TaggedRegions.Count);
    for i:=0 to TaggedRegions.Count-1 do
    begin
      TextRegions[i].Range.Start := (TaggedRegions[i].Range.Start - FirstVis) * CharsPerByte;
      TextRegions[i].Range.AEnd := (TaggedRegions[i].Range.AEnd - FirstVis) * CharsPerByte;
      TextRegions[i].TextColor := TaggedRegions[i].TextColor;
      TextRegions[i].BgColor := TaggedRegions[i].BgColor;
      TextRegions[i].FrameColor := TaggedRegions[i].FrameColor;
    end;
    Pane.SetVisRegions(TextRegions);

    // Caret position
    Pane.InsertModeCaret := InsertMode;
    Pane.CaretPos := Point(cp.X*CharsPerByte + IfThen(CharsPerByte>1, CaretInByte, 0), cp.Y);
  end;

begin
  FirstVis := FirstVisibleAddr();
  VisSize := VisibleBytesCount();

  p := FCaretPos - FirstVis;
  cp := Point(p mod ByteColumns, p div ByteColumns);

  // TODO: cache this inside EditorForm?
  AData := Data.Get(FirstVis, VisSize);

  TaggedRegions := TTaggedDataRegionList.Create(TFileRange.Create(FirstVis, FirstVis + VisSize));
  try
    // Collect tagged regions (Bookmarks, Structure fields etc.) from tools
    OnGetTaggedRegions.Call(Self, FirstVis, FirstVis + VisSize, @AData[0], TaggedRegions);

    // Selection background - on top
    if SelLength > 0 then
      TaggedRegions.AddRegion(Self, SelStart, SelStart + SelLength, Color_SelectionTx, Color_SelectionBg, Color_SelectionFr);


    Update(PaneHex, 3);
    Update(PaneText, 1);
  finally
    TaggedRegions.Free;
  end;
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
    VertScrollBar.Max := FileRows - 1;
    VertScrollBar.PageSize := ScreenRows;
    VertScrollBar.LargeChange := VertScrollBar.PageSize;
  end
  else
  begin
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

procedure TEditorForm.Open(DataSourceType: THextorDataSourceType;
  const APath: string; ResetCaret: Boolean);
var
  DS: THextorDataSource;
begin
  FreeAndNil(DataSource);
  DS := DataSourceType.Create(APath);
  try
    DS.Open(fmOpenRead);
  except
    DS.Free;
    raise;
  end;
  DataSource := DS;
  NewFileOpened(ResetCaret);
end;

procedure TEditorForm.OpenNewEmptyFile(const FileName: string);
begin
  Open(TFileDataSource, FileName, True);
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
////   // �������� ��� ������� ��� ��������
////   S := PPChar(Names)^;
////   // ���������, ���� �� VCL-�������� � ����� �� ������
////   Info := GetPropInfo(ClassInfo, S);
////   if Assigned(Info) then
////     // �������� ����, ���������� � �������� DispId
////     // ����� ��������� PropInfo
//     PDispIDList(DispIds)[0] := 1;// Integer(Info);
//end;

{ TFFSkipSearcher }

constructor TFFSkipSearcher.Create;
begin
  inherited;
  MinMatchSize := 1;
  MaxMatchSize := 1;
end;

function TFFSkipSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
// Return True if we found different byte
begin
  if Data^ <> SkipByte then
  begin
    Size := 1;
    Result := True;
  end
  else
    Result := False;
end;

{ TEditorSettings }

procedure TEditorSettings.InitDefault;
begin
  inherited;
  ScrollWithWheel := 3;
  ByteColumns := -1;
  HighlightMatches := True;
end;

initialization

finalization
  FreeAndNil(TEditorForm.FSettings);
end.

