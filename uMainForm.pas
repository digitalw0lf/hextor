unit uMainForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN SYMBOL_PLATFORM OFF}
//{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  System.Math, Generics.Collections, Clipbrd, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.Types, System.ImageList,
  Vcl.ImgList, System.UITypes, Winapi.SHFolder, System.Rtti, Winapi.ShellAPI,
  Vcl.FileCtrl,

  uUtil, uLargeStr, uEditorPane, uLogFile, superobject,
  uDWHexTypes, uDWHexDataSources, uEditorForm, KControls, KGrids,
  uValueFrame, uStructFrame, uCRC
  {, uPathCompressTest};

const
  Color_ChangedByte = $B0FFFF;
  Color_SelectionBg = clHighlight;
  Color_SelectionTx = clHighlightText;
  Color_ValueHighlightBg = $FFD0A0;

//  MAX_TAB_WIDTH = 200;

type
  TDWHexSettings = class
  public type
    TRecentFileRec = record
      FileName: string;
    end;
  public
    ScrollWithWheel: Integer;
    ByteColumns: Integer;  // -1 - auto
    RecentFiles: array of TRecentFileRec;
//    Colors: record
//      ValueHighlightBg: TColor;
//    end;
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
    MDITabs: TTabControl;
    abs1: TMenuItem;
    RecentFilesMenu: TPopupMenu;
    MIDummyRecentFile1: TMenuItem;
    RightPanel: TPanel;
    RightPanelPageControl: TPageControl;
    PgValue: TTabSheet;
    Splitter1: TSplitter;
    PgStruct: TTabSheet;
    ValueFrame: TValueFrame;
    StructFrame: TStructFrame;
    EditorClosedTimer: TTimer;
    Tools1: TMenuItem;
    CRC321: TMenuItem;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Copyas6Nwords1Click(Sender: TObject);
    procedure Decompress1Click(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure Regions1Click(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionGoToStartExecute(Sender: TObject);
    procedure ActionGoToEndExecute(Sender: TObject);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
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
    procedure abs1Click(Sender: TObject);
    procedure MDITabsChange(Sender: TObject);
    procedure MDITabsGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
    procedure FormShow(Sender: TObject);
    procedure MDITabsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RecentFilesMenuPopup(Sender: TObject);
    procedure EditorClosedTimerTimer(Sender: TObject);
    procedure CRC321Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FEditors: TObjectList<TEditorForm>;
    FInitialFilesOpened: Boolean;
    procedure InitDefaultSettings();
    procedure LoadSettings();
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    function GetActiveEditor: TEditorForm;
    procedure SetActiveEditor(const Value: TEditorForm);
    function CreateNewEditor(): TEditorForm;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): TEditorForm;
    procedure OpenInitialFiles();
    procedure GenerateRecentFilesMenu(Menu: TMenuItem);
  public
    { Public declarations }
    SettingsFolder, SettingsFile: string;
    procedure OpenFile(DataSourceType: TDWHexDataSourceType; const AFileName: string);
    function CloseCurrentFile(AskSave: Boolean): TModalResult;
    procedure SaveSettings();
    procedure CheckEnabledActions();
    procedure UpdateMDITabs();
    procedure ActiveEditorChanged();
    procedure SelectionChanged();
    function GetIconIndex(DataSource: TDWHexDataSource): Integer;
    property ActiveEditor: TEditorForm read GetActiveEditor write SetActiveEditor;
    property EditorCount: Integer read GetEditorCount;
    property Editors[Index: Integer]: TEditorForm read GetEditor;
    procedure AddEditor(AEditor: TEditorForm);
    procedure RemoveEditor(AEditor: TEditorForm);
  end;

const
  EntireFile: TFileRange = (Start: 0; AEnd: -1);

var
  MainForm: TMainForm;
  AppSettings: TDWHexSettings;

implementation

{$R *.dfm}

uses uFindReplaceForm, uDiskSelectForm, uProcessSelectForm, uBitsEditorForm;

{ TMainForm }

procedure TMainForm.abs1Click(Sender: TObject);
begin
  UpdateMDITabs();
end;

procedure TMainForm.ActionBitsEditorExecute(Sender: TObject);
var
  Addr{, Size}: TFilePointer;
  Buf: TBytes;
  x: Int64;
begin
  with ActiveEditor do
  begin
//    if SelLength > 4 then Exit;
//    if SelLength > 0 then
//    begin
//      Addr := SelStart;
//      Size := SelLength;
//    end
//    else
//    begin
//      Addr := CaretPos;
//      Size := 1;
//    end;
//    Buf := GetEditedData(Addr, Size);

    Buf := GetSelectedOrAfterCaret(4, Addr, True);


  //  if Length(Buf) < Size then Exit;
    BitsEditorForm.OkEnabled := (Length(Buf) > 0);

    x := 0;
    Move(Buf[0], x, Length(Buf));
    BitsEditorForm.Value := x;
    BitsEditorForm.ValueSize := Max(Length(Buf), 1);

    if BitsEditorForm.ShowModal() <> mrOk then Exit;

    if Length(Buf) > 0 then
    begin
      x := BitsEditorForm.Value;
      Move(x, Buf[0], Length(Buf));
      ChangeBytes(Addr, Buf);
    end;
  end;
end;

procedure TMainForm.ActionCopyExecute(Sender: TObject);
var
  Buf: TBytes;
  s: string;
begin
  with ActiveEditor do
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
  with ActiveEditor do
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
end;

procedure TMainForm.ActionGoToEndExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    BeginUpdatePanes();
    try
      MoveCaret(GetFileSize(), KeyboardStateToShiftState());
      CaretInByte := 0;
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.ActionGoToStartExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    BeginUpdatePanes();
    try
      MoveCaret(0, KeyboardStateToShiftState());
      CaretInByte := 0;
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
//  if CloseCurrentFile(True) = mrCancel then Exit;
//  OpenNewEmptyFile();
  CreateNewEditor().OpenNewEmptyFile();
end;

procedure TMainForm.ActionOpenDiskExecute(Sender: TObject);
var
  s: string;
begin
  if DiskSelectForm.ShowModal() <> mrOk then Exit;
  s := DiskSelectForm.SelectedDrive;

  OpenFile(TDiskDataSource, s);
end;

procedure TMainForm.ActionOpenExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute() then Exit;

  OpenFile(TFileDataSource, OpenDialog1.FileName);
end;

procedure TMainForm.ActionOpenProcMemoryExecute(Sender: TObject);
var
  s: string;
begin
  if ProcessSelectForm.ShowModal() <> mrOk then Exit;
  s := ProcessSelectForm.SelectedPID;

  OpenFile(TProcMemDataSource, s);
end;

procedure TMainForm.ActionPasteExecute(Sender: TObject);
var
  s: string;
  Buf: TBytes;
begin
  with ActiveEditor do
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
end;

procedure TMainForm.ActionRevertExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    if Application.MessageBox('Revert unsaved changes?', 'Revert', MB_OKCANCEL) <> IDOK then Exit;
    NewFileOpened(False);
  end;
end;

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
var
  fn: string;
begin
  with ActiveEditor do
  begin
    fn := DataSource.Path;
    if DataSource.ClassType <> TFileDataSource then
      fn := MakeValidFileName(fn);
    SaveDialog1.FileName := fn;
    if not SaveDialog1.Execute() then Exit;
    SaveFile(TFileDataSource, SaveDialog1.FileName);
  end;
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    if DataSource.CanBeSaved() then
      SaveFile(TDWHexDataSourceType(DataSource.ClassType), DataSource.Path)
    else
      ActionSaveAsExecute(Sender);
  end;
end;

procedure TMainForm.ActionSaveSelectionAsExecute(Sender: TObject);
// Save selected part to another file.
// If same file is chosen, re-open it with new content
var
  SameFile: Boolean;
  Data: TBytes;
  fn: string;
begin
  with ActiveEditor do
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
end;

procedure TMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    SetSelection(0, GetFileSize()-1);
    UpdatePanes();
  end;
end;

procedure TMainForm.ActiveEditorChanged;
begin
  UpdateMDITabs();
//  CheckEnabledActions();
  SelectionChanged();
end;

procedure TMainForm.AddEditor(AEditor: TEditorForm);
begin
  FEditors.Add(AEditor);
//  UpdateMDITabs();
//  Tabs will be updated when editor changes caption
end;

procedure TMainForm.CheckEnabledActions;
var
  FocusInEditor: Boolean;
begin
  try
    with ActiveEditor do
    begin
      FocusInEditor := (Screen.ActiveControl=PaneHex) or (Screen.ActiveControl=PaneText);

      ActionSave.Enabled := (DataSource <> nil) and (dspWritable in DataSource.GetProperties()) and ((DataSource.Path='') or (HasUnsavedChanges));
      ActionRevert.Enabled := (HasUnsavedChanges);

      ActionCopy.Enabled := (FocusInEditor) and (SelLength > 0);
      ActionCut.Enabled := ActionCopy.Enabled;
      ActionPaste.Enabled := FocusInEditor;

      ActionSelectAll.Enabled := FocusInEditor;

      ActionBitsEditor.Enabled := (SelLength<=4);
    end;
  except
    on E: ENoActiveEditor do
    begin
      ActionSave.Enabled := False;
      ActionRevert.Enabled := False;

      ActionCopy.Enabled := False;
      ActionCut.Enabled := False;
      ActionPaste.Enabled := False;

      ActionSelectAll.Enabled := False;

      ActionBitsEditor.Enabled := False;
    end;
  end;
end;

function TMainForm.CloseCurrentFile(AskSave: Boolean): TModalResult;
begin
  with ActiveEditor do
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
end;

procedure TMainForm.Columnscount1Click(Sender: TObject);
begin
  case AppSettings.ByteColumns of
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

procedure TMainForm.CRC321Click(Sender: TObject);
var
  AData: TBytes;
  crc: Cardinal;
  s: string;
begin
  with ActiveEditor do
  begin
    AData := GetEditedData(SelStart, SelLength);
  end;

//  crc := 0;
//  crc := AddToCRC32(crc, @AData[0], Length(AData));
  crc := CalcCRC32(@AData[0], Length(AData));

  s := IntToHex(crc, 8);
  InputQuery('CRC32', 'CRC32:', s);
end;

function TMainForm.CreateNewEditor: TEditorForm;
begin
  Result := TEditorForm.Create(Application);
  Result.Visible := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ws: string;
  i: Integer;
begin
  bWriteLogFile := True;
  bThreadedLogWrite := False;
  AppSettings := TDWHexSettings.Create();

  // Get path to settings folder (AppData\DWHex)
  SetLength(ws, MAX_PATH);
  i:=SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, @ws[Low(ws)]);
  if i=0 then  SettingsFolder := AddSlash(PChar(ws)) + 'DWHex\'
         else  SettingsFolder := ExePath + 'Settings\';
  SettingsFile := SettingsFolder + 'Settings.json';

  InitDefaultSettings();
  LoadSettings();

  // We will catch drag'n'dropped files
  DragAcceptFiles(Handle, True);

  FEditors := TObjectList<TEditorForm>.Create(False);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings();
  AppSettings.Free;
  FEditors.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not FInitialFilesOpened then
    OpenInitialFiles();
end;

procedure TMainForm.GenerateRecentFilesMenu(Menu: TMenuItem);
// Show menu items for "Recent files"
var
  i: Integer;
  mi: TMenuItem;
begin
  for i:=Menu.Count-1 downto 1 do
    Menu.Items[i].Free;

  for i:=0 to Length(AppSettings.RecentFiles)-1 do
  begin
    mi := TMenuItem.Create(Self);
    mi.Caption := AppSettings.RecentFiles[i].FileName;
    mi.OnClick := MIDummyRecentFileClick;
    Menu.Add(mi);
  end;
end;

function TMainForm.GetActiveEditor: TEditorForm;
var
  Frm: TForm;
begin
  Frm := ActiveMDIChild;
  if (Frm <> nil) and (Frm is TEditorForm) then
    Result := TEditorForm(Frm)
  else
    // Silently abort operation that requires active editor
    raise ENoActiveEditor.Create('No active editor');
end;

function TMainForm.GetEditor(Index: Integer): TEditorForm;
begin
  Result := FEditors[Index];
end;

function TMainForm.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TMainForm.GetIconIndex(DataSource: TDWHexDataSource): Integer;
// Index in ImageList16 for given DataSource
var
  DSType: TDWHexDataSourceType;
begin
  DSType := TDWHexDataSourceType(DataSource.ClassType);
  if DSType = TDiskDataSource then  Result := 4
  else if DSType = TProcMemDataSource then  Result := 5
  else Result := 7;
end;

procedure TMainForm.InitDefaultSettings;
begin
  AppSettings.ScrollWithWheel := 3;
  AppSettings.ByteColumns := -1;

//  AppSettings.Colors.ValueHighlightBg := $FFD0A0;
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
  Value := TValue.From<TDWHexSettings>(AppSettings);

  ctx.FromJson(TypeInfo(TDWHexSettings), json, Value);

  ctx.Free;
end;

procedure TMainForm.MDITabsChange(Sender: TObject);
var
  N: Integer;
begin
  N := MDITabs.TabIndex;
  Editors[N].BringToFront;
end;

procedure TMainForm.MDITabsGetImageIndex(Sender: TObject; TabIndex: Integer;
  var ImageIndex: Integer);
begin
  if (TabIndex >= EditorCount) or (Editors[TabIndex].DataSource = nil) then
  begin
    ImageIndex := 0;
    Exit;
  end;
  ImageIndex := GetIconIndex(Editors[TabIndex].DataSource);
end;

procedure TMainForm.MDITabsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  n: Integer;
begin
  n := MDITabs.IndexOfTabAt(X, Y);
  if n >= 0 then
  begin
    case Button of
      mbMiddle:
        begin
          Editors[n].Close();
        end;
    end;
  end;
end;

procedure TMainForm.MIColumns8Click(Sender: TObject);
var
  n: Integer;
begin
  with ActiveEditor do
  begin
    n := (Sender as TMenuItem).Tag;
    AppSettings.ByteColumns := n;
    CalculateByteColumns();
    SaveSettings();
  end;
end;

procedure TMainForm.MIDummyRecentFileClick(Sender: TObject);
// Open file from "Recent files" menu
begin
  OpenFile(TFileDataSource, (Sender as TMenuItem).Caption);
end;

procedure TMainForm.MIRecentFilesMenuClick(Sender: TObject);
begin
  GenerateRecentFilesMenu(MIRecentFilesMenu);
end;

procedure TMainForm.N1Click(Sender: TObject);
begin
//  TestCompress();
end;

procedure TMainForm.OpenFile(DataSourceType: TDWHexDataSourceType; const AFileName: string);
begin
  with CreateNewEditor() do
  begin
    DataSource := DataSourceType.Create(AFileName);
    DataSource.Open(fmOpenRead);

    NewFileOpened(True);
  end;
end;

procedure TMainForm.OpenInitialFiles;
begin
  FInitialFilesOpened := True;
  if ParamCount()>0 then
    OpenFile(TFileDataSource, ParamStr(1))
  else
    ActionNewExecute(nil);
end;

procedure TMainForm.RecentFilesMenuPopup(Sender: TObject);
begin
  GenerateRecentFilesMenu(RecentFilesMenu.Items);
end;

procedure TMainForm.Regions1Click(Sender: TObject);
//var
//  s: string;
//  i: Integer;
begin
//  s := '';
//  for i:=0 to CachedRegions.Count-1 do
//    s := s + IntToStr(CachedRegions[i].Addr)+' '+IntToStr(CachedRegions[i].Size)+' '+RemUnprintable(MakeStr(CachedRegions[i].Data))+#13#10;
//  Application.MessageBox(PChar(s),'');
end;

procedure TMainForm.RemoveEditor(AEditor: TEditorForm);
begin
  FEditors.Remove(AEditor);
  // Update tabs and interface when editor form will be destroyed
  EditorClosedTimer.Enabled := True;
end;

procedure TMainForm.SaveSettings;
const
  BOM: array[0..1] of Byte = ($FF, $FE);
var
  ctx: TSuperRttiContext;
  fs: TFileStream;
begin
  System.SysUtils.ForceDirectories(SettingsFolder);
  fs := TFileStream.Create(SettingsFile, fmCreate);
  try
    fs.WriteBuffer(BOM, SizeOf(BOM));

    ctx := TSuperRttiContext.Create;
    ctx.AsJson<TDWHexSettings>(AppSettings).SaveTo(fs, True, False);
    ctx.Free;
  finally
    fs.Free;
  end;
end;

procedure TMainForm.SelectionChanged;
begin
  CheckEnabledActions();
  ValueFrame.UpdateInfo();
end;

procedure TMainForm.SetActiveEditor(const Value: TEditorForm);
begin
  if Value <> nil then
    Value.BringToFront();
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
//  Caption := Screen.ActiveControl.Name;
end;

procedure TMainForm.EditorClosedTimerTimer(Sender: TObject);
begin
  EditorClosedTimer.Enabled := False;
  ActiveEditorChanged();
end;

procedure TMainForm.UpdateMDITabs;
var
  i, Current: Integer;
  st: TStringList;
  s: string;
begin
  Current := -1;
  st := TStringList.Create();
  for i:=0 to EditorCount-1 do
  begin
//    s := MinimizeName(Editors[i].Caption, MDITabs.Canvas, MAX_TAB_WIDTH);
    s := ExtractFileName(Editors[i].Caption);
    if s = '' then s := Editors[i].Caption;
    st.Add(s);
    if Editors[i] = ActiveMDIChild then
      Current := i;
  end;

  MDITabs.Tabs.Assign(st);
  st.Free;

  MDITabs.TabIndex := Current;

  RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TMainForm.Decompress1Click(Sender: TObject);
begin
//  TestDecompress();
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

end.
