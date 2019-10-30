unit uEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Generics.Collections, Math, System.Types,

  uUtil, uDWHexTypes, uDWHexDataSources, uEditorPane, Vcl.Menus;

type
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  private
    { Private declarations }
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
  public
    { Public declarations }
    DataSource: TDWHexDataSource;
    CachedRegions: TCachedRegionsList;
    SelStart, SelLength: TFilePointer;
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
    function GetOverlappingRegions(Addr, Size: TFilePointer; var Index1, Index2: Integer): TCachedRegionsList; overload;
    function GetOverlappingRegions(Addr, Size: TFilePointer): TCachedRegionsList; overload;
    function FindCachedRegion(Addr: TFilePointer; var Index: Integer): Boolean;
    function CreateCachedRegion(Addr, Size: TFilePointer): TCachedRegion;
    function StartChanges(Addr, Size: TFilePointer): TCachedRegion;
    property CaretPos: TFilePointer read FCaretPos write SetCaretPos;
    property CaretInByte: Integer read FCaretInByte write SetCaretInByte;
    procedure MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
    procedure SetSelection(AStart, AEnd: TFilePointer);
    procedure BeginUpdatePanes();
    procedure EndUpdatePanes();
    property HasUnsavedChanges: Boolean read FHasUnsavedChanges write SetHasUnsavedChanges;
    property TopVisibleRow: TFilePointer read FTopVisibleRow write SetTopVisibleRow;
    property ByteColumns: Integer read FByteColumns write SetByteColumns;
    procedure CalculateByteColumns();
    function GetSelectedOrAfterCaret(MaxSize: Integer; var Addr: TFilePointer; NothingIfMore: Boolean = False): TBytes;
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
  if AppSettings.ByteColumns > 0 then
    ByteColumns := AppSettings.ByteColumns
  else
  begin
    Cols := Min( PaneHex.ClientWidth div (PaneHex.CharWidth*3),
                 PaneText.ClientWidth div (PaneText.CharWidth));
    if Cols < 1 then Cols := 1;
    ByteColumns :=  Cols;
  end;
end;

procedure TEditorForm.ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
var
  Region: TCachedRegion;
begin
  Region := StartChanges(Addr, Length(Value));
  Move(Value[0], Region.Data[Addr-Region.Addr], Length(Value));
  UpdatePanes();

  if (Addr <= SelStart + SelLength) and (Addr + Length(Value) >= SelStart) then
    SelectionChanged();
end;

function TEditorForm.CreateCachedRegion(Addr, Size: TFilePointer): TCachedRegion;
var
  n: Integer;
begin
  Result := TCachedRegion.Create();
  Result.Addr := Addr;
  Result.Data := GetEditedData(Addr, Size, True);
  if FindCachedRegion(Addr, n) then
    raise Exception.Create('Trying to create overlapping region');
  CachedRegions.Insert(n, Result);
end;

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  MainForm.ActiveEditorChanged();
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AskSaveChanges() = IDCANCEL then
    CanClose := False;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
//  FByteColumns := 16;
  CachedRegions := TObjectList<TCachedRegion>.Create(True);
  CalculateByteColumns();
  MainForm.AddEditor(Self);
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  CachedRegions.Free;
  MainForm.RemoveEditor(Self);
end;

procedure TEditorForm.FormResize(Sender: TObject);
begin
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
    end;

    if ssCtrl in Shift then
    begin
      case Key of
        Ord('A'): MainForm.ActionSelectAll.Execute();
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
      APos := CaretPos;
      Buf := GetEditedData(APos, 1, True);
      if Length(Buf)<>1 then Exit;
      x := Buf[0];
      Digit := StrToInt('$'+Key);
      if CaretInByte=0 then
        x := (x and $0F) or (Digit shl 4)
      else
        x := (x and $F0) or (Digit);
      ChangeBytes(CaretPos, [x]);

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
var
  OrigSize, CurrSize: TFilePointer;
  ReadSize, ReturnSize: Integer;
  i: Integer;
  oPos, oSize: TFilePointer;
  Regions: TCachedRegionsList;
begin
  OrigSize := GetOrigFileSize();
  CurrSize := GetFileSize();
  if (Addr<0) or (Addr>CurrSize) then Exit(nil);
  if (not ZerosBeyondEoF) and (Addr>=CurrSize) then Exit(nil);

  if ZerosBeyondEoF then
    ReturnSize := Size
  else
  begin
    ReturnSize := Min(Size, CurrSize-Addr);
  end;
  SetLength(Result, ReturnSize);


  // Original data
  ReadSize := Size;
  if Addr + ReadSize > OrigSize then
    ReadSize := OrigSize - Addr;
  if ReadSize > 0 then
  begin
//    if FileDataLoaded then
//      //Result := Copy(FileData, Addr, ReadSize)
//      Move(FileData[Addr], Result[0], ReadSize)
//    else
//    begin
      DataSource.GetData(Addr, ReadSize, Result);
      // TODO: check result
//    end;
  end;

  // Edited data
  Regions := GetOverlappingRegions(Addr, Size);
  for i:=0 to Regions.Count-1 do
  begin
    oPos := Max(Addr, Regions[i].Addr);
    oSize := Min(Regions[i].Addr+Regions[i].Size, Addr+Size) - oPos;
    Move(Regions[i].Data[oPos-Regions[i].Addr], Result[oPos-Addr], oSize);
  end;

  if (ZerosBeyondEoF) and (Addr+ReturnSize > CurrSize) then
  // Fill with zeros beyond end of file
  begin
    ZeroMemory(@Result[CurrSize-Addr], Addr+ReturnSize-CurrSize);
  end;
end;

function TEditorForm.GetFileSize: TFilePointer;
// Edited file size (including appended region)
var
  p: TFilePointer;
begin
  Result := GetOrigFileSize();
  if (CachedRegions.Count>0) then
  with CachedRegions.Last do
  begin
    p := Addr + Size;
    if p > Result then
      Result := p;
  end;
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

function TEditorForm.GetOverlappingRegions(Addr,
  Size: TFilePointer): TCachedRegionsList;
var
  Index1, Index2: Integer;
begin
  Result := GetOverlappingRegions(Addr, Size, Index1, Index2);
end;

function TEditorForm.GetSelectedOrAfterCaret(MaxSize: Integer; var Addr: TFilePointer;
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
    Size := Min(MaxSize, GetFileSize()-CaretPos);
    Addr := CaretPos;
  end;

  Result := GetEditedData(Addr, Size);
end;

function TEditorForm.FindCachedRegion(Addr: TFilePointer; var Index: Integer): Boolean;
var
  i: Integer;
begin
  for i:=0 to CachedRegions.Count-1 do
  begin
    if CachedRegions[i].Addr>Addr then
    begin
      Index := i;
      Exit(False);
    end;
    if CachedRegions[i].Addr+CachedRegions[i].Size>Addr then
    begin
      Index := i;
      Exit(True);
    end;
  end;
  Index := CachedRegions.Count;
  Result := False;
end;

function TEditorForm.FirstVisibleAddr: TFilePointer;
begin
  Result := TopVisibleRow * ByteColumns;
end;

function TEditorForm.GetOverlappingRegions(Addr,
  Size: TFilePointer; var Index1, Index2: Integer): TCachedRegionsList;
var
  i: Integer;
begin
  Result := TCachedRegionsList.Create(False);
  Index1 := MaxInt;
  Index2 := -1;
  for i:=0 to CachedRegions.Count-1 do
    if (CachedRegions[i].Addr<Addr+Size) and (CachedRegions[i].Addr+CachedRegions[i].Size>Addr) then
    begin
      Result.Add(CachedRegions[i]);
      if i < Index1 then Index1 := i;
      if i > Index2 then Index2 := i;
    end;
end;

function TEditorForm.GetVisibleRowsCount: Integer;
begin
  Result := PaneHex.Height div PaneHex.CharHeight();
end;

procedure TEditorForm.MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
// Move caret and adjust/remove selection
begin
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
    HasUnsavedChanges := False;
    CachedRegions.Clear();

    MainForm.ImageList16.GetIcon(MainForm.GetIconIndex(DataSource), Icon);
    UpdateFormCaption();
    UpdateScrollBar();
    if ResetCaret then
      MoveCaret(0, [])
    else
      CaretPos := CaretPos;  // Limit by file size
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
      ChangeBytes(CaretPos, [Byte(c)]);
      MoveCaret(CaretPos + 1, []);
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.Splitter1Moved(Sender: TObject);
begin
  CalculateByteColumns();
end;

procedure TEditorForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := VertScrollBar.Position * FLinesPerScrollBarTick;
end;

procedure TEditorForm.SaveFile(DataSourceType: TDWHexDataSourceType; const APath: string);
var
  i: Integer;
  //FS: TFileStream;
  Dest: TDWHexDataSource;
  SameSource: Boolean;
begin
  if (DataSourceType=nil) or (APath='') then Exit;

  SameSource := (DataSourceType = DataSource.ClassType) and (SameFileName(APath, DataSource.Path));

  if SameSource then
  // If saving to same file, re-open it for writing
  begin
    DataSource.Open(fmOpenReadWrite);
    Dest := DataSource;
  end
  else
  // If saving to another file, create another DataSource and copy original contents first
  begin
    Dest := DataSourceType.Create(APath);
    Dest.Open(fmCreate);
    Dest.CopyContentFrom(DataSource);
  end;

  // Write changed regions
  for i:=0 to CachedRegions.Count-1 do
  begin
    Dest.ChangeData(CachedRegions[i].Addr, CachedRegions[i].Data[0], CachedRegions[i].Size());
  end;

  // Open new saved file
  if not SameSource then
  begin
    DataSource.Free;
    DataSource := Dest;
  end
  else
  begin

  end;
  DataSource.Open(fmOpenRead);

  NewFileOpened(not SameSource);
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
      // Keep caret in view, if it is now
      if ByteColumns > 0 then
        CaretLineOnScreen := CaretPos div ByteColumns - TopVisibleRow
      else
        CaretLineOnScreen := 0;
      // else approximately track view position in file
      NewTopRow := Round((TopVisibleRow * ByteColumns) / Value);

      FByteColumns := Value;

      UpdateScrollBar();
      if (CaretLineOnScreen >= 0) and (CaretLineOnScreen < GetVisibleRowsCount()) then
        TopVisibleRow := CaretPos div ByteColumns - CaretLineOnScreen
      else
        TopVisibleRow := NewTopRow;
      UpdatePanes();
    finally
      EndUpdatePanes();
    end;
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
  end;
end;

procedure TEditorForm.SetSelection(AStart, AEnd: TFilePointer);
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
    SelLength := AEnd-AStart+1;
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

function TEditorForm.StartChanges(Addr, Size: TFilePointer): TCachedRegion;
// Find or create CachedRegion covering given range.
// May combine several old regions that are overlapped by range.
var
  i1, i2: Integer;
  Regions: TCachedRegionsList;
  p1, p2: TFilePointer;
  Data: TBytes;
begin
  Regions := GetOverlappingRegions(Addr-1, Size+2, i1, i2);
  try
    if Regions.Count = 0 then
    begin
      Result := CreateCachedRegion(Addr, Size);
    end
    else
    begin
      p1 := Min(Addr, Regions.First.Addr);
      p2 := Max(Addr+Size, Regions.Last.Addr+Regions.Last.Size);
      Data := GetEditedData(p1, p2-p1, True);
      Result := Regions[0];
      Result.Data := Data;
      Result.Addr := p1;
      if i2>i1 then
        CachedRegions.DeleteRange(i1+1, i2-i1);
    end;
  finally
    Regions.Free;
  end;
  HasUnsavedChanges := True;
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
  FirstVisibleAddress: TFilePointer;
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
      if (Data[i] < Ord(' ')) or (Data[i] = $98) then
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
  finally
    EndUpdatePanes();
  end;
end;

procedure TEditorForm.UpdatePanesCarets;
var
  cp: TPoint;
  p, FirstVis: TFilePointer;
  VisSize: Integer;
  Cached: TCachedRegionsList;

  procedure Update(Pane: TEditorPane; CharsPerByte: Integer);
  var
    L, i: Integer;
    j: TFilePointer;
    N0: TFilePointer;
  begin
    // Background colors
    N0 := FirstVis;
    L := VisSize*CharsPerByte;
    if Length(Pane.BgColors)<>L then
      SetLength(Pane.BgColors, L);
    if Length(Pane.TxColors)<>L then
      SetLength(Pane.TxColors, L);
    for i:=0 to L-1 do
    begin
      Pane.BgColors[i] := Pane.Color;
      Pane.TxColors[i] := Pane.Font.Color;
    end;
    // Changed bytes
    for i:=0 to Cached.Count-1 do
    begin
      for j:=Max(FirstVis, Cached[i].Addr)*CharsPerByte to Min(FirstVis+VisSize, Cached[i].Addr+Cached[i].Size)*CharsPerByte-1 do
        Pane.BgColors[j - FirstVis*CharsPerByte] := Color_ChangedByte;
    end;
    // Selection background
    for i:=0 to L-1 do
    begin
      if (N0+(i div CharsPerByte)>=SelStart) and (N0+(i div CharsPerByte)<SelStart+SelLength) then
      begin
        Pane.BgColors[i] := Color_SelectionBg;
        Pane.TxColors[i] := Color_SelectionTx;
      end;
    end;

    // Caret position
    Pane.CaretPos := Point(cp.X*CharsPerByte + IfThen(CharsPerByte>1, CaretInByte, 0), cp.Y);
  end;

begin
  FirstVis := FirstVisibleAddr();
  VisSize := VisibleBytesCount();

  p := FCaretPos - FirstVis;
  cp := Point(p mod ByteColumns, p div ByteColumns);
  Cached := GetOverlappingRegions(FirstVis, VisSize);

  try
    Update(PaneHex, 3);
    Update(PaneText, 1);
  finally
    Cached.Free;
  end;
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

