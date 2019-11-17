unit uEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Generics.Collections, Math, System.Types, Vcl.Menus,

  uUtil, uDWHexTypes, uDWHexDataSources, uEditorPane, uEditedData,
  uCallbackList;

type
  TEditorForm = class;

  // Callback that is used by tools to colorise displayed data in editor
  TGetDataColors = function (Editor: TEditorForm; Addr: TFilePointer; Size: Integer; Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;

  TEditorForm = class(TForm)
    PaneHex: TEditorPane;
    PaneLnNum: TEditorPane;
    PaneText: TEditorPane;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
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
    procedure Splitter1Moved(Sender: TObject);
    procedure VertScrollBarChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    FDestroyed: Boolean;  // Some events (e.g. FormResize) are oddly called after form destruction
    FCaretPos: TFilePointer;
    SelDragStart, SelDragEnd: TFilePointer;
    WasLeftMouseDown: Boolean;
    FCaretInByte: Integer;
    FHasUnsavedChanges: Boolean;
    FTopVisibleRow: TFilePointer;
    FUpdating: Integer;
    FNeedUpdatePanes: Boolean;
    FByteColumns: Integer;
    FLinesPerScrollBarTick: Integer;
    FInsertMode: Boolean;
    FByteColumnsSetting: Integer;
    FPrevVisibleRange: TFileRange;
    procedure SetCaretPos(Value: TFilePointer);
    procedure UpdatePanesCarets();
    procedure PaneMouseMove(Sender: TObject; IsMouseDown: Boolean; Shift: TShiftState; X, Y: Integer);
    procedure SetCaretInByte(const Value: Integer);
    procedure SetHasUnsavedChanges(const Value: Boolean);
    procedure UpdateFormCaption();
    procedure ScrollToCaret();
    procedure SetTopVisibleRow(Value: TFilePointer);
    procedure SetByteColumns(Value: Integer);
    procedure ShowSelectionInfo();
    procedure AddCurrentFileToRecentFiles();
    procedure SelectionChanged();
    procedure CopyDataRegion(Source, Dest: TDWHexDataSource; SourceAddr, DestAddr, Size: TFilePointer);
    procedure SetInsertMode(Value: Boolean);
    function AdjustPositionInData(var Pos: TFilePointer; OpAddr, OpSize: TFilePointer): Boolean;
    procedure AdjustPointersPositions(OpAddr, OpSize: TFilePointer);
    procedure SomeDataChanged();
    procedure SetByteColumnsSetting(const Value: Integer);
//  public type
//    TSaveMethod = (smUnknown, smPartialInplace, smFull, smTempFile);
  public
    { Public declarations }
    DataSource: TDWHexDataSource;
    EditedData: TEditedData;
//    CachedRegions: TCachedRegionsList;
    SelStart, SelLength: TFilePointer;
    OnClosed: TCallbackListP1<TEditorForm>;
    OnVisibleRangeChanged: TCallbackListP1<TEditorForm>;
    OnByteColsChanged: TCallbackListP1<TEditorForm>;
    destructor Destroy(); override;
    function AskSaveChanges(): TModalResult;
    procedure OpenNewEmptyFile;
    procedure SaveFile(DataSourceType: TDWHexDataSourceType; const APath: string);
    procedure NewFileOpened(ResetCaret: Boolean);
    function GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;
    function GetOrigFileSize(): TFilePointer;
    function GetFileSize(): TFilePointer;
    procedure UpdatePanes();
    procedure UpdateScrollBar();
    function GetVisibleRowsCount(): Integer;
    function FirstVisibleAddr(): TFilePointer;
    function VisibleBytesCount(): Integer;
    procedure ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
    function DeleteSelected(): TFilePointer;
    property CaretPos: TFilePointer read FCaretPos write SetCaretPos;
    property CaretInByte: Integer read FCaretInByte write SetCaretInByte;
    property InsertMode: Boolean read FInsertMode write SetInsertMode;
    procedure MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
    procedure SetSelection(AStart, AEnd: TFilePointer);
    procedure BeginUpdatePanes();
    procedure EndUpdatePanes();
    property HasUnsavedChanges: Boolean read FHasUnsavedChanges write SetHasUnsavedChanges;
    function ChooseSaveMethod(DataSourceType: TDWHexDataSourceType; const APath: string;
      var InplaceSaving, UseTempFile: Boolean): Boolean;
    property TopVisibleRow: TFilePointer read FTopVisibleRow write SetTopVisibleRow;
    property ByteColumns: Integer read FByteColumns write SetByteColumns;
    property ByteColumnsSetting: Integer read FByteColumnsSetting write SetByteColumnsSetting;
    procedure CalculateByteColumns();
    function GetSelectedOrAfterCaret(DefaultSize, MaxSize: Integer; var Addr: TFilePointer; NothingIfMore: Boolean = False): TBytes;
    procedure DataChanged(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure DataInserted(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure DataDeleted(Addr: TFilePointer; Size: TFilePointer);
  end;

var
  EditorForm: TEditorForm;

implementation

uses
  uMainForm, uValueFrame;

{$R *.dfm}

procedure TEditorForm.AddCurrentFileToRecentFiles;
var
  Recent: TDWHexSettings.TRecentFileRec;
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
  AdjustPositionInData(SelStart, OpAddr, OpSize);
  AdjustPositionInData(ASelEnd, OpAddr, OpSize);
  SelLength := ASelEnd - SelStart;

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
  PaneLnNum.BeginUpdate();
  PaneHex.BeginUpdate();
  PaneText.BeginUpdate();
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
//    Cols := Min( PaneHex.ClientWidth div (PaneHex.CharWidth*3),
//                 PaneText.ClientWidth div (PaneText.CharWidth));
    //Cols := (PaneHex.ClientWidth + PaneText.ClientWidth - PaneText.CharWidth) div (PaneHex.CharWidth*3 + PaneText.CharWidth);
    Cols := (VertScrollBar.Left - PaneHex.Left - PaneHex.CharWidth - PaneText.CharWidth) div (PaneHex.CharWidth*3 + PaneText.CharWidth);
    if Cols < 1 then Cols := 1;
    ByteColumns :=  Cols;
  end;
end;

procedure TEditorForm.ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
begin
  EditedData.Change(Addr, Length(Value), @Value[0]);
end;

function TEditorForm.ChooseSaveMethod(DataSourceType: TDWHexDataSourceType; const APath: string;
  var InplaceSaving, UseTempFile: Boolean): Boolean;
// How can we save our modified data to given target:
// Can we only write changed parts, or maybe we'll have to use temp file?
var
  SameSource: Boolean;
begin
  SameSource := (DataSourceType = DataSource.ClassType) and
                (SameFileName(APath, DataSource.Path));

  InplaceSaving := SameSource and (not EditedData.HasMovements());
  UseTempFile := (SameSource) and (not InplaceSaving);
  Result := True;
end;

procedure TEditorForm.CopyDataRegion(Source, Dest: TDWHexDataSource; SourceAddr,
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

procedure TEditorForm.DataChanged(Addr, Size: TFilePointer;
  const Value: PByteArray);
begin
  SomeDataChanged();
  if (Addr <= SelStart + SelLength) and (Addr + Size >= SelStart) then
    SelectionChanged();
end;

procedure TEditorForm.DataDeleted(Addr, Size: TFilePointer);
begin
  AdjustPointersPositions(Addr, -Size);

  SomeDataChanged();

  if (Addr <= SelStart + SelLength) and (Addr + Size >= SelStart) then
    SelectionChanged();
end;

procedure TEditorForm.DataInserted(Addr, Size: TFilePointer;
  const Value: PByteArray);
begin
  AdjustPointersPositions(Addr, Size);
  DataChanged(Addr, Size, Value);
end;

function TEditorForm.DeleteSelected(): TFilePointer;
begin
  if (not InsertMode) or (SelLength = 0) then Exit(0);

  Result := SelLength;
  EditedData.Delete(SelStart, SelLength);
end;

destructor TEditorForm.Destroy;
begin
  FDestroyed := True;
  EditedData.Free;
  DataSource.Free;
  MainForm.RemoveEditor(Self);
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
  EditedData := TEditedData.Create(Self);
  CalculateByteColumns();
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
    UpdateScrollBar();
    UpdatePanes();
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.PaneHexEnter(Sender: TObject);
begin
  MainForm.CheckEnabledActions();

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
          MainForm.ActionGoToStart.Execute()
        else
        begin
          MoveCaret((CaretPos div ByteColumns)*ByteColumns, KeyboardStateToShiftState());
          CaretInByte := 0;
        end;

      VK_END:
        if ssCtrl in Shift then
          MainForm.ActionGoToEnd.Execute()
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
            if (Sender = PaneText) and (CaretPos < EditedData.GetSize()) then
              EditedData.Delete(CaretPos, 1);
          end;

      VK_BACK:
        begin
          if InsertMode then
          begin
            if SelLength > 0 then
              DeleteSelected()
            else
            if (Sender = PaneText) and (CaretPos > 0) then
              EditedData.Delete(CaretPos - 1, 1);
          end
          else
          begin
            // In overwrite mode, allow to backspace-delete appended portion of file
            if (Sender = PaneText) and (CaretPos > GetOrigFileSize()) then
              EditedData.Delete(CaretPos - 1, 1);
          end;
        end;
    end;

    if ssCtrl in Shift then
    begin
      case Key of
        Ord('A'): MainForm.ActionSelectAll.Execute();
        Ord('X'): MainForm.ActionCut.Execute();
        Ord('C'): MainForm.ActionCopy.Execute();
        Ord('V'): MainForm.ActionPaste.Execute();
        Ord('F'): MainForm.ActionFind.Execute();
      end;
      Key := 0;
    end;

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
    try
      if InsertMode then
      begin
        if DeleteSelected() > 0 then
          CaretInByte := 0;
      end;

      APos := CaretPos;
      if InsertMode and (CaretInByte = 0) then
      begin
        EditedData.Insert(APos, 1, @Zero);
        FCaretPos := FCaretPos - 1;
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
    finally
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
  if (Sender <> PaneLnNum) and (Button = mbLeft) then
  begin
    WasLeftMouseDown := True;
    PaneMouseMove(Sender, True, Shift+[ssLeft], X, Y);
  end;
end;

procedure TEditorForm.PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Sender <> PaneLnNum) and (ssLeft in Shift) and (WasLeftMouseDown) then
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
  Result := EditedData.Get(Addr, Size, ZerosBeyondEoF);
end;

function TEditorForm.GetFileSize: TFilePointer;
// Edited file size (including appended region)
begin
  Result := EditedData.GetSize();
end;

function TEditorForm.GetOrigFileSize: TFilePointer;
// Original file size
begin
//  if FileDataLoaded then
//    Result := Length(FileData)
//  else
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

function TEditorForm.FirstVisibleAddr: TFilePointer;
begin
  Result := TopVisibleRow * ByteColumns;
end;

function TEditorForm.GetVisibleRowsCount: Integer;
begin
  Result := PaneHex.Height div PaneHex.CharHeight();
end;

procedure TEditorForm.MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
// Move caret and adjust/remove selection
begin
  NewPos := BoundValue(NewPos, 0, EditedData.GetSize());
  if ssShift in Shift then
    SelDragEnd := NewPos
  else
  begin
    SelDragStart := NewPos;
    SelDragEnd := -1;
  end;
  SetSelection(SelDragStart, SelDragEnd);

  CaretPos := NewPos;
end;

procedure TEditorForm.NewFileOpened(ResetCaret: Boolean);
begin
  BeginUpdatePanes();
  try
    EditedData.DataSource := DataSource;
    EditedData.Resizable := (dspResizable in DataSource.GetProperties());
    EditedData.ResetParts();
    HasUnsavedChanges := False;

    MainForm.ImageList16.GetIcon(MainForm.GetIconIndex(DataSource), Icon);
    UpdateFormCaption();
    UpdateScrollBar();
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
  c: AnsiChar;
begin
  BeginUpdatePanes();
  try
    if Key >= ' ' then
    begin
      // Maybe some better way to convert single unicode char to current locale ansi char?
      c := AnsiString(string(Key))[Low(AnsiString)];
      if InsertMode then
      begin
        DeleteSelected();
        EditedData.Insert(CaretPos, 1, @c);
      end
      else
      begin
        ChangeBytes(CaretPos, [Byte(c)]);
        MoveCaret(CaretPos + 1, []);
      end;
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.Splitter1Moved(Sender: TObject);
begin
//  CalculateByteColumns();
end;

procedure TEditorForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := VertScrollBar.Position * FLinesPerScrollBarTick;
end;

procedure TEditorForm.SaveFile(DataSourceType: TDWHexDataSourceType; const APath: string);
var
  i: Integer;
  Dest: TDWHexDataSource;
  InplaceSaving, UseTempFile: Boolean;
  TempFileName: string;
begin
  if (DataSourceType=nil) or (APath='') then Exit;

  if not ChooseSaveMethod(DataSourceType, APath, InplaceSaving, UseTempFile) then
    raise Exception.Create('Cannot save this data to this target');

  if InplaceSaving then
  // If saving to same file, re-open it for writing
  begin
    DataSource.Open(fmOpenReadWrite);
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

  // Write changed regions
  for i:=0 to EditedData.Parts.Count-1 do
  begin
    if (InplaceSaving) and (EditedData.Parts[i].PartType = ptSource) then Continue;
    case EditedData.Parts[i].PartType of
      ptSource:
        CopyDataRegion(DataSource, Dest, EditedData.Parts[i].SourceAddr, EditedData.Parts[i].Addr, EditedData.Parts[i].Size);
      ptBuffer:
        Dest.ChangeData(EditedData.Parts[i].Addr, EditedData.Parts[i].Size, EditedData.Parts[i].Data[0]);
    end;
  end;
  // TODO: Handle write errors

  // If saving in-place, we may have to truncate file
  if (InplaceSaving) and (dspResizable in Dest.GetProperties()) and
     (EditedData.GetSize() <> Dest.GetSize) then
    Dest.SetSize(EditedData.GetSize());


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
var
  CaretRow: TFilePointer;
begin
  CaretRow := CaretPos div ByteColumns;
  if CaretRow < TopVisibleRow then
    TopVisibleRow := CaretRow
  else
  if CaretRow > TopVisibleRow+GetVisibleRowsCount()-1 then
    TopVisibleRow := CaretRow-GetVisibleRowsCount()+1;
end;

procedure TEditorForm.SelectionChanged;
begin
  ShowSelectionInfo();
  if Self = MainForm.ActiveEditor then
    MainForm.SelectionChanged();
end;

procedure TEditorForm.SetByteColumns(Value: Integer);
var
  NewTopRow: TFilePointer;
  CaretLineOnScreen: TFilePointer;
begin
  Value := BoundValue(Value, 1, 1024);
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

      PaneHex.Width := (FByteColumns * 3 + 1) * PaneHex.CharWidth;

      UpdateScrollBar();
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
    SelStart := AStart;
    SelLength := 0;
  end
  else
  begin
    if AStart>AEnd then
      Swap8Bytes(AStart, AEnd);
    SelStart := AStart;
    SelLength := AEnd-AStart;
  end;
  UpdatePanes();
  SelectionChanged();
end;

procedure TEditorForm.SetTopVisibleRow(Value: TFilePointer);
var
  MaxTopRow: TFilePointer;
begin
  if Value<>FTopVisibleRow then
  begin
    MaxTopRow := DivRoundUp(GetFileSize()+1, ByteColumns) - GetVisibleRowsCount();
    if Value > MaxTopRow then Value := MaxTopRow;
    if Value < 0 then Value := 0;

    FTopVisibleRow := Value;
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
  Data: TBytes;
  x: Int64;
begin
  if SelLength = 0 then
  begin
    StatusBar.Panels[0].Text := 'Addr: ' + IntToStr(CaretPos) + '( '+'0x' + IntToHex(CaretPos, 2) + ')';
    Data := GetEditedData(CaretPos, 1);
    if Length(Data)>=1 then
      StatusBar.Panels[1].Text := 'Byte: ' + IntToStr(Data[0])
    else
      StatusBar.Panels[1].Text := '';
  end
  else
  begin
    StatusBar.Panels[0].Text := 'Selected: ' + IntToStr(SelLength) + ' bytes';
    if (SelLength <= 8) then
    begin
      Data := GetEditedData(SelStart, SelLength);
      x := 0;
      Move(Data[0], x, Length(Data));
      StatusBar.Panels[1].Text := 'As '+IntToStr(Length(Data))+'-byte value: ' + IntToStr(x);
    end
    else
      StatusBar.Panels[1].Text := '';
  end;
end;

procedure TEditorForm.SomeDataChanged;
begin
  HasUnsavedChanges := True;
  UpdatePanes();
  MainForm.UpdateMsgPanel();
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

  PaneLnNum.EndUpdate();
  PaneHex.EndUpdate();
  PaneText.EndUpdate();
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

procedure TEditorForm.UpdatePanes;
var
  Data: TBytes;
  i: Integer;
  Rows: Integer;
  sb: TStringBuilder;
  Lines: TStringList;
  s: AnsiString;
  c: AnsiChar;
  FirstVisibleAddress, VisibleRangeEnd: TFilePointer;
  IncludesFileEnd: Boolean;
begin
  if FUpdating>0 then
  begin
    FNeedUpdatePanes := True;
    Exit;
  end;
  FNeedUpdatePanes := False;

  BeginUpdatePanes();
  try
    Rows := GetVisibleRowsCount();
    FirstVisibleAddress := FirstVisibleAddr();
    Data := GetEditedData(FirstVisibleAddress, Rows * ByteColumns);
    VisibleRangeEnd := FirstVisibleAddress + Length(Data);
    IncludesFileEnd := (Length(Data) < Rows * ByteColumns);
    sb := TStringBuilder.Create();
    Lines := TStringList.Create();

    // Line numbers
    for i:=0 to DivRoundUp(Length(Data), ByteColumns)-1 do
    begin
      //sb.Append(IntToHex(FirstVisibleAddress + i*ByteColumns, 8)+#13#10);
      Lines.Add(IntToHex(FirstVisibleAddress + i*ByteColumns, 8));
    end;
    //PaneLnNum.Text := sb.ToString;
    PaneLnNum.Lines.Assign(Lines);

    // Hex
  //  sb.Clear();
    Lines.Clear();
    for i:=0 to Length(Data)-1 do
    begin
      sb.Append(IntToHex(Data[i], 2)+' ');
      if ((i+1) mod ByteColumns)=0 then
      begin
        Lines.Add(sb.ToString());
        sb.Clear();
      end;
    end;
    if (sb.Length>0) or (IncludesFileEnd) then
      Lines.Add(sb.ToString());
    PaneHex.Lines.Assign(Lines);

    // Text
    //sb.Clear();
    s := '';
    Lines.Clear();
    for i:=0 to Length(Data)-1 do
    begin
      if (Data[i] < Ord(' ')) or (Data[i] = $7F) or (Data[i] = $98) then
        c := '.'
      else
        c := AnsiChar(Data[i]);
      //sb.Append(c);
      s := s + c;
      if ((i+1) mod ByteColumns)=0 then
        //sb.Append(#13#10);
      begin
        //Lines.Add(sb.ToString());
        //sb.Clear();
        Lines.Add(string(s));
        s := '';
      end;
    end;
  //  if (sb.Length>0) or (IncludesFileEnd) then
  //    Lines.Add(sb.ToString());
    if (s<>'') or (IncludesFileEnd) then
      Lines.Add(string(s));
    PaneText.Lines.Assign(Lines);

    sb.Free;
    Lines.Free;

    UpdatePanesCarets();

    if (FirstVisibleAddress <> FPrevVisibleRange.Start) or (VisibleRangeEnd <> FPrevVisibleRange.AEnd) then
    begin
      OnVisibleRangeChanged.Call(Self);
      FPrevVisibleRange.Start := FirstVisibleAddress;
      FPrevVisibleRange.AEnd := VisibleRangeEnd;
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.UpdatePanesCarets;
// Update panes carets and text/background colors
var
  cp: TPoint;
  p, FirstVis: TFilePointer;
  VisSize: Integer;
  Parts: TEditedData.TDataPartList;
  TxColors, BgColors: TColorArray;
  i: Integer;

  procedure Update(Pane: TEditorPane; CharsPerByte: Integer);
  var
    L, i: Integer;
  begin
    L := VisSize*CharsPerByte;
    if Length(Pane.BgColors)<>L then
      SetLength(Pane.BgColors, L);
    if Length(Pane.TxColors)<>L then
      SetLength(Pane.TxColors, L);
    for i:=0 to L-1 do
    begin
      Pane.TxColors[i] := TxColors[i div CharsPerByte];
      Pane.BgColors[i] := BgColors[i div CharsPerByte];
    end;

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
  for i:=0 to VisSize-1 do
  begin
    BgColors[i] := PaneHex.Color;
    TxColors[i] := PaneHex.Font.Color;
  end;

  // Changed bytes
  Parts := TEditedData.TDataPartList.Create(False);
  try
    EditedData.GetOverlappingParts(FirstVis, VisSize, Parts);
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

procedure TEditorForm.UpdateScrollBar;
var
  FileRows: TFilePointer;
  ScreenRows: Integer;
begin
  FileRows := DivRoundUp(GetFileSize()+1, ByteColumns);
  ScreenRows := GetVisibleRowsCount();
  if (FileRows <= ScreenRows) then
  begin
    FLinesPerScrollBarTick := 1;
    VertScrollBar.PageSize := 0;
    VertScrollBar.Max := 0;
  end
  else
  begin
    // ScrollBar.Position is limited by 2^32, so for large files make
    // one scrollbar step more then one line
    FLinesPerScrollBarTick := Max(FileRows div 100000000{100 M lines}, 1);
    VertScrollBar.Max := (FileRows div FLinesPerScrollBarTick) - 1;
    VertScrollBar.PageSize := (ScreenRows div FLinesPerScrollBarTick);
  end;
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

end.

