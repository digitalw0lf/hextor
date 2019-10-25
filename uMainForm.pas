unit uMainForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
//{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  System.Math, Generics.Collections, Clipbrd, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.Types, System.ImageList,
  Vcl.ImgList, System.UITypes, Winapi.SHFolder, System.Rtti, Winapi.ShellAPI,

  uUtil, uLargeStr, uEditorPane, uLogFile, superobject,
  uDWHexTypes, uDWHexDataSources{, uPathCompressTest};

const
  Color_ChangedByte = $B0FFFF;
  Color_SelectionBg = clHighlight;
  Color_SelectionTx = clHighlightText;

type
  TCachedRegion = class
    Addr: TFilePointer;
    Data: TBytes;
    function Size(): TFilePointer;
  end;

  TCachedRegionsList = TObjectList<TCachedRegion>;

  TDWHexSettings = class
  public type
    TRecentFileRec = record
      FileName: string;
    end;
  public
    ScrollWithWheel: Integer;
    ByteColumns: Integer;  // -1 - auto
    RecentFiles: array of TRecentFileRec;
  end;

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    ToolBar1: TToolBar;
    OpenDialog1: TOpenDialog;
    MainPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    VertScrollBar: TScrollBar;
    PaneLnNum: TEditorPane;
    PaneHex: TEditorPane;
    PaneText: TEditorPane;
    est1: TMenuItem;
    Copyas6Nwords1: TMenuItem;
    N1: TMenuItem;
    Decompress1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ActionList1: TActionList;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    View1: TMenuItem;
    Columnscount1: TMenuItem;
    MIColumns8: TMenuItem;
    MIColumns16: TMenuItem;
    MIColumns32: TMenuItem;
    MIColumnsByWidth: TMenuItem;
    Regions1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ActionCut: TAction;
    ActionCopy: TAction;
    ActionPaste: TAction;
    ActionCopyAs: TAction;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MICopyAs: TMenuItem;
    MIPaste: TMenuItem;
    ActionSelectAll: TAction;
    MISelectAll: TMenuItem;
    ActionGoToStart: TAction;
    ActionGoToEnd: TAction;
    ImageList16: TImageList;
    ActionRevert: TAction;
    Revert1: TMenuItem;
    N2: TMenuItem;
    ActionFind: TAction;
    MIFindReplace: TMenuItem;
    EditorPopupMenu: TPopupMenu;
    PMICut: TMenuItem;
    PMICopy: TMenuItem;
    PMIPaste: TMenuItem;
    PMISelectAll: TMenuItem;
    StatusBar: TStatusBar;
    ActionFindNext: TAction;
    ActionFindPrev: TAction;
    FindNext1: TMenuItem;
    FindPrevious1: TMenuItem;
    ActionGoToAddr: TAction;
    GoToaddress1: TMenuItem;
    ActionSaveSelectionAs: TAction;
    Saveselectionas1: TMenuItem;
    MIRecentFilesMenu: TMenuItem;
    MIDummyRecentFile: TMenuItem;
    ActionExit: TAction;
    N3: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    ActionOpenDisk: TAction;
    MIOpenDisk: TMenuItem;
    N5: TMenuItem;
    ActionOpenProcMemory: TAction;
    OpenProcessMemory1: TMenuItem;
    ActionBitsEditor: TAction;
    N6: TMenuItem;
    BitsEditor1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure VertScrollBarChange(Sender: TObject);
    procedure Copyas6Nwords1Click(Sender: TObject);
    procedure Decompress1Click(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure Regions1Click(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ActionCopyExecute(Sender: TObject);
    procedure PaneHexEnter(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionGoToStartExecute(Sender: TObject);
    procedure ActionGoToEndExecute(Sender: TObject);
    procedure PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaneHexKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaneHexKeyPress(Sender: TObject; var Key: Char);
    procedure PaneTextKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure PaneHexMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Splitter1Moved(Sender: TObject);
    procedure MIColumns8Click(Sender: TObject);
    procedure Columnscount1Click(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPrevExecute(Sender: TObject);
    procedure ActionGoToAddrExecute(Sender: TObject);
    procedure ActionSaveSelectionAsExecute(Sender: TObject);
    procedure MIRecentFilesMenuClick(Sender: TObject);
    procedure MIDummyRecentFileClick(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionOpenDiskExecute(Sender: TObject);
    procedure ActionOpenProcMemoryExecute(Sender: TObject);
    procedure ActionBitsEditorExecute(Sender: TObject);
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
//    PrevClickPos: TPoint;
//    PrevClickTime: Cardinal;
//    PrevClickCount: Integer;
    procedure SetCaretPos(Value: TFilePointer);
    procedure UpdatePanesCarets();
    procedure PaneMouseMove(Sender: TObject; IsMouseDown: Boolean; Shift: TShiftState; X, Y: Integer);
    procedure CheckEnabledActions();
    procedure NewFileOpened(ResetCaret: Boolean);
    procedure SetCaretInByte(const Value: Integer);
    procedure SetHasUnsavedChanges(const Value: Boolean);
    procedure UpdateFormCaption();
    procedure ScrollToCaret();
    procedure SetTopVisibleRow(Value: TFilePointer);
    procedure SetByteColumns(Value: Integer);
    procedure CalculateByteColumns();
    procedure ShowSelectionInfo();
    function AskSaveChanges(): TModalResult;
    procedure InitDefaultSettings();
    procedure SaveSettings();
    procedure LoadSettings();
    procedure AddCurrentFileToRecentFiles();
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    { Public declarations }
//    FileName: string;
//    FileStream: TFileStream;
    DataSource: TDWHexDataSource;
//    FileData: TBytes;
//    FileDataLoaded: Boolean;
    CachedRegions: TCachedRegionsList;
    SelStart, SelLength: TFilePointer;
    Settings: TDWHexSettings;
    SettingsFolder, SettingsFile: string;
    procedure OpenFile(DataSourceType: TDWHexDataSourceType; const AFileName: string);
    procedure SaveFile(const AFileName: string);
    function CloseCurrentFile(AskSave: Boolean): TModalResult;
    procedure OpenNewEmptyFile();
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
  end;

const
  EntireFile: TFileRange = (Start: 0; AEnd: -1);

function DivRoundUp(A, B: Int64): Int64; inline;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses uFindReplaceForm, uDiskSelectForm, uProcessSelectForm, uBitsEditorForm;

function DivRoundUp(A, B: Int64): Int64; inline;
begin
  Result := (A-1) div B + 1;
end;

function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
// Ограничивает X в диапазон [MinX,MaxX]
var
  t: TFilePointer;
begin
  Result:=X;
  if MinX>MaxX then
  begin
    t:=MinX;
    MinX:=MaxX;
    MaxX:=t;
  end;
  if Result<MinX then Result:=MinX;
  if Result>MaxX then Result:=MaxX;
end;

{ TMainForm }

procedure TMainForm.ActionBitsEditorExecute(Sender: TObject);
var
  Addr, Size: TFilePointer;
  Buf: TBytes;
  x: Int64;
begin
  if SelLength > 4 then Exit;
  if SelLength > 0 then
  begin
    Addr := SelStart;
    Size := SelLength;
  end
  else
  begin
    Addr := CaretPos;
    Size := 1;
  end;
  Buf := GetEditedData(Addr, Size);
//  if Length(Buf) < Size then Exit;
  BitsEditorForm.OkEnabled := (Length(Buf) = Size);

  x := 0;
  Move(Buf[0], x, Length(Buf));
  BitsEditorForm.Value := x;
  BitsEditorForm.ValueSize := Size;

  if BitsEditorForm.ShowModal() <> mrOk then Exit;

  if Length(Buf) > 0 then
  begin
    x := BitsEditorForm.Value;
    Move(x, Buf[0], Length(Buf));
    ChangeBytes(Addr, Buf);
  end;
end;

procedure TMainForm.ActionCopyExecute(Sender: TObject);
var
  Buf: TBytes;
  s: string;
begin
  if SelLength > 100*MByte then
    if Application.MessageBox(PChar('Try to copy '+IntToStr(SelLength)+' bytes to system clipboard?'), PChar('Copy'), MB_YESNO) <> IDYES then Exit;
  Buf := GetEditedData(SelStart, SelLength);
  if ActiveControl=PaneHex then
    s := Data2Hex(Buf, True)
  else
    s := MakeStr(Buf);
  Clipboard.AsText := s;
end;

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.ActionFindExecute(Sender: TObject);
begin
  FindReplaceForm.Show();
end;

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
begin
  if FindReplaceForm.ParamsDefined() then
    FindReplaceForm.FindNext(1)
  else
    ActionFindExecute(Sender);
end;

procedure TMainForm.ActionFindPrevExecute(Sender: TObject);
begin
  if FindReplaceForm.ParamsDefined() then
    FindReplaceForm.FindNext(-1)
  else
    ActionFindExecute(Sender);
end;

procedure TMainForm.ActionGoToAddrExecute(Sender: TObject);
var
  s: string;
  Pos: TFilePointer;
begin
  s := IntToStr(CaretPos);
  if not InputQuery('Go to address', 'Go to address (use $ or 0x for hex value, + or - for relative jump):', s) then Exit;
  s := Trim(s);
  if s = '' then Exit;
  s := s.Replace('0x', '$');
  s := s.Replace('x', '$');

  if (s[Low(s)] = '+') or (s[Low(s)] = '-') then
    Pos := CaretPos + StrToInt64(s)
  else
    Pos := StrToInt64(s);

  MoveCaret(Pos, []);
end;

procedure TMainForm.ActionGoToEndExecute(Sender: TObject);
begin
  BeginUpdatePanes();
  try
    MoveCaret(GetFileSize(), KeyboardStateToShiftState());
    CaretInByte := 0;
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.ActionGoToStartExecute(Sender: TObject);
begin
  BeginUpdatePanes();
  try
    MoveCaret(0, KeyboardStateToShiftState());
    CaretInByte := 0;
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
  if CloseCurrentFile(True) = mrCancel then Exit;
  OpenNewEmptyFile();
end;

procedure TMainForm.ActionOpenDiskExecute(Sender: TObject);
var
  s: string;
begin
  if CloseCurrentFile(True) = mrCancel then Exit;

  if DiskSelectForm.ShowModal() <> mrOk then Exit;
  s := DiskSelectForm.SelectedDrive;
  if Length(s)<2 then Exit;

  OpenFile(TDiskDataSource, s);
end;

procedure TMainForm.ActionOpenExecute(Sender: TObject);
begin
  if AskSaveChanges() = mrCancel then Exit;
  if not OpenDialog1.Execute() then Exit;
  OpenFile(TFileDataSource, OpenDialog1.FileName);
end;

procedure TMainForm.ActionOpenProcMemoryExecute(Sender: TObject);
var
  s: string;
begin
  if CloseCurrentFile(True) = mrCancel then Exit;

  if ProcessSelectForm.ShowModal() <> mrOk then Exit;
  s := ProcessSelectForm.SelectedPID;
  if Length(s)=0 then Exit;

  OpenFile(TProcMemDataSource, s);
end;

procedure TMainForm.ActionPasteExecute(Sender: TObject);
var
  s: string;
  Buf: TBytes;
begin
  s := Clipboard.AsText;
  if ActiveControl=PaneHex then
    Buf := HexToData(s)
  else
    Buf := Str2Bytes(AnsiString(s));
  if Length(Buf)=0 then Exit;

  BeginUpdatePanes();
  try
    ChangeBytes(CaretPos, Buf);
    MoveCaret(CaretPos + Length(Buf), []);
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.ActionRevertExecute(Sender: TObject);
begin
  if Application.MessageBox('Revert unsaved changes?', 'Revert', MB_OKCANCEL) <> IDOK then Exit;
  OpenFile(TDWHexDataSourceType(DataSource.ClassType), DataSource.Path);
end;

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
var
  fn: string;
begin
  fn := DataSource.Path;
  if DataSource.ClassType <> TFileDataSource then
    fn := MakeValidFileName(fn);
  SaveDialog1.FileName := fn;
  if not SaveDialog1.Execute() then Exit;
  SaveFile(SaveDialog1.FileName);
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  if ExtractFilePath(DataSource.Path) = '' then
    ActionSaveAsExecute(Sender)
  else
    SaveFile(DataSource.Path);
end;

procedure TMainForm.ActionSaveSelectionAsExecute(Sender: TObject);
// Save selected part to another file.
// If same file is chosen, re-open it with new content
var
  SameFile: Boolean;
  Data: TBytes;
  fn: string;
begin
  if SelLength > MaxInt then
    raise EInvalidUserInput.Create('This command is not supported for selection larger then 2 GBytes');

  fn := DataSource.Path;
  if DataSource.ClassType <> TFileDataSource then
    fn := MakeValidFileName(fn);
  fn := ChangeFileExt(fn, '_part'+ExtractFileExt(fn));

  SaveDialog1.FileName := fn;
  if not SaveDialog1.Execute() then Exit;
  fn := SaveDialog1.FileName;

  SameFile := SameFileName(fn, DataSource.Path);
  if SameFile then
    if Application.MessageBox('Current file will be overwritten and re-opened with new content', 'Replace file', MB_OKCANCEL) <> IDOK then Exit;

  Data := GetEditedData(SelStart, SelLength);

  if SameFile then CloseCurrentFile(False);

  SaveEntireFile(fn, Data);

  if SameFile then
    OpenFile(TDWHexDataSourceType(DataSource.ClassType), fn);
end;

procedure TMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  SetSelection(0, GetFileSize()-1);
  UpdatePanes();
end;

procedure TMainForm.AddCurrentFileToRecentFiles;
var
  Recent: TDWHexSettings.TRecentFileRec;
  n, i: Integer;
begin
  if (DataSource.ClassType <> TFileDataSource) or (ExtractFilePath(DataSource.Path) = '') then Exit;

  n := -1;
  for i:=0 to Length(Settings.RecentFiles)-1 do
    if SameFileName(Settings.RecentFiles[i].FileName, DataSource.Path) then
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
    Recent := Settings.RecentFiles[n];
    Delete(Settings.RecentFiles, n, 1);
  end;
  Insert([Recent], Settings.RecentFiles, 0);
  if Length(Settings.RecentFiles) > 20 then
    SetLength(Settings.RecentFiles, 20);

  SaveSettings();
end;

function TMainForm.AskSaveChanges: TModalResult;
begin
  Result := mrNo;
  if HasUnsavedChanges then
  begin
    Result := Application.MessageBox(PChar('Save changes to '#13#10+DataSource.Path+'?'), 'Closing', MB_YESNOCANCEL);
    case Result of
      mrYes: ActionSaveExecute(nil);
    end;
  end;
end;

procedure TMainForm.BeginUpdatePanes;
begin
  Inc(FUpdating);
  PaneLnNum.BeginUpdate();
  PaneHex.BeginUpdate();
  PaneText.BeginUpdate();
end;

procedure TMainForm.CalculateByteColumns();
// Choose byte columns count based on width of editor panes
var
  Cols: Integer;
begin
  if Settings.ByteColumns > 0 then
    ByteColumns := Settings.ByteColumns
  else
  begin
    Cols := Min( PaneHex.ClientWidth div (PaneHex.CharWidth*3),
                 PaneText.ClientWidth div (PaneText.CharWidth));
    if Cols < 1 then Cols := 1;
    ByteColumns :=  Cols;
  end;
end;

procedure TMainForm.ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
var
  Region: TCachedRegion;
begin
  Region := StartChanges(Addr, Length(Value));
  Move(Value[0], Region.Data[Addr-Region.Addr], Length(Value));
  UpdatePanes();
end;

procedure TMainForm.CheckEnabledActions;
var
  FocusInEditor: Boolean;
begin
  FocusInEditor := (Screen.ActiveControl=PaneHex) or (Screen.ActiveControl=PaneText);

  ActionSave.Enabled := (dspWritable in DataSource.GetProperties()) and ((DataSource.Path='') or (HasUnsavedChanges));
  ActionRevert.Enabled := (HasUnsavedChanges);

  ActionCopy.Enabled := (FocusInEditor) and (SelLength > 0);
  ActionCut.Enabled := ActionCopy.Enabled;
  ActionPaste.Enabled := FocusInEditor;

  ActionSelectAll.Enabled := FocusInEditor;

  ActionBitsEditor.Enabled := (SelLength<=4);
end;

function TMainForm.CloseCurrentFile(AskSave: Boolean): TModalResult;
begin
  if (AskSave) then
  begin
    Result := AskSaveChanges();
    if Result = mrCancel then Exit;
  end;
  Result := mrNo;

  FreeAndNil(DataSource);
//  FileDataLoaded := False;
end;

procedure TMainForm.Columnscount1Click(Sender: TObject);
begin
  case Settings.ByteColumns of
    8: MIColumns8.Checked := True;
    16: MIColumns16.Checked := True;
    32: MIColumns32.Checked := True;
    else MIColumnsByWidth.Checked := True;
  end;
end;

procedure TMainForm.Copyas6Nwords1Click(Sender: TObject);
//var
//  i: Integer;
//  s: TStringBuilder;
begin
//  s := TStringBuilder.Create();
//  for i:=0 to Length(FileData) div 2-1 do
//  begin
//    s.Append(IntToStr(pSmallInt(@FileData[i*2])^)+#9);
//    if (i+1) mod 6=0 then
//      s.Append(#13#10);
//  end;
//  Clipboard.AsText := s.ToString;
//  s.Free;

end;

function TMainForm.CreateCachedRegion(Addr, Size: TFilePointer): TCachedRegion;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AskSaveChanges() = IDCANCEL then
    CanClose := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ws: string;
  i: Integer;
begin
  bWriteLogFile := True;
  Settings := TDWHexSettings.Create();

  // Get path to settings folder (AppData...)
  SetLength(ws, MAX_PATH);
  i:=SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, @ws[Low(ws)]);
  if i=0 then  SettingsFolder := AddSlash(PChar(ws)) + 'DWHex\'
         else  SettingsFolder := ExePath + 'Settings\';
  SettingsFile := SettingsFolder + 'Settings.json';

  InitDefaultSettings();
  LoadSettings();

//  FByteColumns := 16;
  CachedRegions := TObjectList<TCachedRegion>.Create(True);
  CalculateByteColumns();

  // Будем ловить файлы, бросаемые в программу
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings();
  CachedRegions.Free;
  Settings.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
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

procedure TMainForm.FormShow(Sender: TObject);
begin
//  OpenFile('d:\DWF\Delphi\Tools\DWHex\Test\Unit1.pas');
  if ParamCount()>0 then
    OpenFile(TFileDataSource, ParamStr(1))
  else
    OpenNewEmptyFile();
end;

function TMainForm.GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;
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

function TMainForm.GetFileSize: TFilePointer;
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

function TMainForm.GetOrigFileSize: TFilePointer;
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

function TMainForm.GetOverlappingRegions(Addr,
  Size: TFilePointer): TCachedRegionsList;
var
  Index1, Index2: Integer;
begin
  Result := GetOverlappingRegions(Addr, Size, Index1, Index2);
end;

function TMainForm.FindCachedRegion(Addr: TFilePointer; var Index: Integer): Boolean;
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

function TMainForm.FirstVisibleAddr: TFilePointer;
begin
  Result := TopVisibleRow * ByteColumns;
end;

function TMainForm.GetOverlappingRegions(Addr,
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

function TMainForm.GetVisibleRowsCount: Integer;
begin
  Result := PaneHex.Height div PaneHex.CharHeight();
end;

procedure TMainForm.InitDefaultSettings;
begin
  Settings.ScrollWithWheel := 3;
  Settings.ByteColumns := -1;
end;

procedure TMainForm.LoadSettings;
var
  ctx: TSuperRttiContext;
  json: ISuperObject;
  Value: TValue;
begin
  if FileExists(SettingsFile) then
    json := TSuperObject.ParseFile(SettingsFile, False)
  else
    json := SO('');

  ctx := TSuperRttiContext.Create;
  Value := TValue.From<TDWHexSettings>(Settings);

  ctx.FromJson(TypeInfo(TDWHexSettings), json, Value);

  ctx.Free;
end;

procedure TMainForm.MIColumns8Click(Sender: TObject);
var
  n: Integer;
begin
  n := (Sender as TMenuItem).Tag;
  Settings.ByteColumns := n;
  CalculateByteColumns();
  SaveSettings();
end;

procedure TMainForm.MIDummyRecentFileClick(Sender: TObject);
// Open file from "Recent files" menu
begin
  if AskSaveChanges() = mrCancel then Exit;
  OpenFile(TFileDataSource, (Sender as TMenuItem).Caption);
end;

procedure TMainForm.MIRecentFilesMenuClick(Sender: TObject);
// Show menu items for "Recent files"
var
  i: Integer;
  mi: TMenuItem;
begin
  for i:=MIRecentFilesMenu.Count-1 downto 1 do
    MIRecentFilesMenu.Items[i].Free;

  for i:=0 to Length(Settings.RecentFiles)-1 do
  begin
    mi := TMenuItem.Create(Self);
    mi.Caption := Settings.RecentFiles[i].FileName;
    mi.OnClick := MIDummyRecentFileClick;
    MIRecentFilesMenu.Add(mi);
  end;
end;

procedure TMainForm.MoveCaret(NewPos: TFilePointer; Shift: TShiftState);
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

procedure TMainForm.N1Click(Sender: TObject);
begin
//  TestCompress();
end;

procedure TMainForm.NewFileOpened(ResetCaret: Boolean);
begin
  BeginUpdatePanes();
  try
    HasUnsavedChanges := False;
    CachedRegions.Clear();

    UpdateFormCaption();
    UpdateScrollBar();
//    TopVisibleRow := 0;
    if ResetCaret then
      MoveCaret(0, []);
    UpdatePanes();

    CheckEnabledActions();
  finally
    EndUpdatePanes();
  end;

  AddCurrentFileToRecentFiles();
end;

procedure TMainForm.OpenFile(DataSourceType: TDWHexDataSourceType; const AFileName: string);
begin
  CloseCurrentFile(False);

  DataSource := DataSourceType.Create(AFileName);
  DataSource.Open(fmOpenRead);

  NewFileOpened(True);
end;

procedure TMainForm.OpenNewEmptyFile;
begin
  DataSource := TFileDataSource.Create('New file');
//  FileDataLoaded := False;

  NewFileOpened(True);
end;

procedure TMainForm.PaneHexEnter(Sender: TObject);
begin
  CheckEnabledActions();
end;

procedure TMainForm.PaneHexKeyDown(Sender: TObject; var Key: Word;
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
          ActionGoToStart.Execute()
        else
        begin
          MoveCaret((CaretPos div ByteColumns)*ByteColumns, KeyboardStateToShiftState());
          CaretInByte := 0;
        end;

      VK_END:
        if ssCtrl in Shift then
          ActionGoToEnd.Execute()
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
        Ord('A'): ActionSelectAll.Execute();
        Ord('C'): ActionCopy.Execute();
        Ord('V'): ActionPaste.Execute();
        Ord('F'): ActionFind.Execute();
      end;
      Key := 0;
    end;

  finally
    EndUpdatePanes();
  end;

end;

procedure TMainForm.PaneHexKeyPress(Sender: TObject; var Key: Char);
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

procedure TMainForm.PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ActiveControl := (Sender as TWinControl);
  if (Sender <> PaneLnNum) and (Button = mbLeft) then
  begin
    WasLeftMouseDown := True;
    PaneMouseMove(Sender, True, Shift+[ssLeft], X, Y);
  end;
end;

procedure TMainForm.PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Sender <> PaneLnNum) and (ssLeft in Shift) and (WasLeftMouseDown) then
    PaneMouseMove(Sender, False, Shift, X, Y);
end;

procedure TMainForm.PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    WasLeftMouseDown := False;
end;

procedure TMainForm.PaneHexMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TopVisibleRow := TopVisibleRow - WheelDelta div 120 * Settings.ScrollWithWheel;
end;

procedure TMainForm.PaneMouseMove(Sender: TObject; IsMouseDown: Boolean;
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

procedure TMainForm.PaneTextKeyPress(Sender: TObject; var Key: Char);
begin
  BeginUpdatePanes();
  try
    if Key >= ' ' then
    begin
      ChangeBytes(CaretPos, [Byte(Key)]);
      MoveCaret(CaretPos + 1, []);
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.Regions1Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '';
  for i:=0 to CachedRegions.Count-1 do
    s := s + IntToStr(CachedRegions[i].Addr)+' '+IntToStr(CachedRegions[i].Size)+' '+RemUnprintable(MakeStr(CachedRegions[i].Data))+#13#10;
  Application.MessageBox(PChar(s),'');
end;

procedure TMainForm.SaveFile(const AFileName: string);
var
  i: Integer;
  //FS: TFileStream;
  Dest: TDWHexDataSource;
begin
  if (AFileName='') then Exit;

  if SameFileName(AFileName, DataSource.Path) then
  // If saving to same file, re-open it for writing
  begin
    DataSource.Open(fmOpenReadWrite);
    Dest := DataSource;
  end
  else
  // If saving to another file, create another DataSource and copy original contents first
  begin
    Dest := TFileDataSource.Create(AFileName);
    Dest.Open(fmCreate);
//    if FileDataLoaded then
//      Dest.ChangeData(0, FileData)
//    else
      Dest.CopyContentFrom(DataSource);
  end;

  // Write changed regions
  for i:=0 to CachedRegions.Count-1 do
  begin
    Dest.ChangeData(CachedRegions[i].Addr, CachedRegions[i].Data[0], CachedRegions[i].Size());
  end;

  // Open new saved file
  if Dest <> DataSource then
  begin
    DataSource.Free;
    DataSource := Dest;
  end
  else
  begin

  end;
  DataSource.Open(fmOpenRead);

  NewFileOpened(False);

end;

procedure TMainForm.SaveSettings;
const
  BOM: array[0..1] of Byte = ($FF, $FE);
var
  ctx: TSuperRttiContext;
  fs: TFileStream;
begin
  ForceDirectories(SettingsFolder);
  fs := TFileStream.Create(SettingsFile, fmCreate);
  try
    fs.WriteBuffer(BOM, SizeOf(BOM));

    ctx := TSuperRttiContext.Create;
    ctx.AsJson<TDWHexSettings>(Settings).SaveTo(fs, True, False);
    ctx.Free;
  finally
    fs.Free;
  end;
end;

procedure TMainForm.ScrollToCaret;
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

procedure TMainForm.SetByteColumns(Value: Integer);
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

procedure TMainForm.SetCaretInByte(const Value: Integer);
begin
  if Value<>FCaretInByte then
  begin
    Assert((Value>=0) and (Value<=1), 'SetCaretInByte: '+IntToStr(Value));
    FCaretInByte := Value;
    UpdatePanesCarets();
  end;
end;

procedure TMainForm.SetCaretPos(Value: TFilePointer);
begin
  Value := BoundValue(Value, 0, GetFileSize());
  if Value <> FCaretPos then
  begin
    BeginUpdatePanes();
    try
      FCaretPos := Value;
      ScrollToCaret();
      UpdatePanesCarets();
      ShowSelectionInfo();
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.SetHasUnsavedChanges(const Value: Boolean);
begin
  if Value<>FHasUnsavedChanges then
  begin
    FHasUnsavedChanges := Value;
    CheckEnabledActions();
    UpdateFormCaption();
  end;
end;

procedure TMainForm.SetSelection(AStart, AEnd: TFilePointer);
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
  CheckEnabledActions();
end;

procedure TMainForm.SetTopVisibleRow(Value: TFilePointer);
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

procedure TMainForm.ShowSelectionInfo();
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
      StatusBar.Panels[1].Text := 'As number: ' + IntToStr(x);
    end
    else
      StatusBar.Panels[1].Text := '';
  end;
end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  CalculateByteColumns();
end;

function TMainForm.StartChanges(Addr, Size: TFilePointer): TCachedRegion;
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

procedure TMainForm.Decompress1Click(Sender: TObject);
begin
//  TestDecompress();
end;

procedure TMainForm.EndUpdatePanes;
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

procedure TMainForm.UpdateFormCaption;
var
  s: string;
begin
  s := 'DWHex - [';
  if DataSource.Path <> '' then
    s := s + DataSource.Path
  else
    s := s + '(unnamed)';
  if HasUnsavedChanges then
    s := s + ' *';
  s := s + ']';
  Self.Caption := s;
end;

procedure TMainForm.UpdatePanes;
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
        Lines.Add(s);
        s := '';
      end;
    end;
  //  if (sb.Length>0) or (IncludesFileEnd) then
  //    Lines.Add(sb.ToString());
    if (s<>'') or (IncludesFileEnd) then
      Lines.Add(s);
    PaneText.Lines.Assign(Lines);

    sb.Free;
    Lines.Free;

    UpdatePanesCarets();
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.UpdatePanesCarets;
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

procedure TMainForm.UpdateScrollBar;
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

procedure TMainForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := VertScrollBar.Position * FLinesPerScrollBarTick;
end;

function TMainForm.VisibleBytesCount: Integer;
begin
  Result := GetVisibleRowsCount() * ByteColumns;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  i:Integer;
  Catcher: TDropFileCatcher;
  s:string;
begin
  inherited;
  Catcher := TDropFileCatcher.Create(Msg.Drop);
  try
    for I := 0 to Catcher.FileCount-1 do
    begin
      s:=Catcher.Files[i];
      if FileExists(s) then OpenFile(TFileDataSource, s);
    end;
  finally
    Catcher.Free;
  end;
  Msg.Result := 0;
end;

{ TCachedRegion }

function TCachedRegion.Size: TFilePointer;
begin
  Result := Length(Data);
end;

end.
