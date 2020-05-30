{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uMainForm;

//{$WARN IMPLICIT_STRING_CAST OFF}
//{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
//{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  System.Math, Generics.Collections, Clipbrd, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.Types, System.ImageList,
  Vcl.ImgList, System.UITypes, Winapi.SHFolder, System.Rtti, Winapi.ShellAPI,
  Vcl.FileCtrl, KControls, KGrids, Vcl.Buttons, Vcl.Samples.Gauges,
  System.StrUtils, MSScriptControl_TLB, System.IOUtils, Vcl.HtmlHelpViewer,

  uEditorPane, {uLogFile,} superobject,
  uHextorTypes, uHextorDataSources, uEditorForm,
  uValueFrame, uStructFrame, uCompareFrame, uScriptFrame,
  uBitmapFrame, uCallbackList, uHextorGUI, uOleAutoAPIWrapper,
  uSearchResultsFrame, uHashFrame, uDataSaver, Vcl.StdActns, uAsmFrame;

const
  Color_ChangedByte = $B0FFFF;
  Color_SelectionBg = $F5DDBF; //clHighlight;
  Color_SelectionTx = clNone; //clHighlightText;
  Color_SelectionFr = $D77800;
  Color_ValueHighlightBg = $FFD0A0;
  Color_DiffBg = $05CBEF;

//  MAX_TAB_WIDTH = 200;

type
  THextorSettings = class
  public type
    TRecentFileRec = record
      FileName: string;
    end;
  public
    ScrollWithWheel: Integer;
    ByteColumns: Integer;  // -1 - auto
    ActiveRightPage: Integer;
    RightPanelWidth: Integer;
    Struct: record
      Range: TStructFrame.TInterpretRange;
    end;
    Script: record
      Text: string;
    end;
    RecentFiles: array of TRecentFileRec;
//    Colors: record
//      ValueHighlightBg: TColor;
//    end;
  end;

  [API]
  THextorUtils = class
  public
    procedure Sleep(milliseconds: Cardinal);
    procedure Alert(V: Variant);
  end;

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MIFile: TMenuItem;
    MIEdit: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    ToolBar1: TToolBar;
    OpenDialog1: TOpenDialog;
    MIDebug: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ActionList1: TActionList;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    Regions1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ActionCut: TAction;
    ActionCopy: TAction;
    ActionPaste: TAction;
    ActionCopyAsArray: TAction;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MICopyAsArray: TMenuItem;
    MIPaste: TMenuItem;
    ActionSelectAll: TAction;
    MISelectAll: TMenuItem;
    ActionGoToStart: TAction;
    ActionGoToEnd: TAction;
    ImageList16: TImageList;
    ActionRevert: TAction;
    Revert1: TMenuItem;
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
    RecentFilesMenu: TPopupMenu;
    MIDummyRecentFile1: TMenuItem;
    RightPanel: TPanel;
    RightPanelPageControl: TPageControl;
    PgValue: TTabSheet;
    Splitter1: TSplitter;
    PgStruct: TTabSheet;
    ValueFrame: TValueFrame;
    StructFrame: TStructFrame;
    AfterEventTimer: TTimer;
    MITools: TMenuItem;
    estchangespeed1: TMenuItem;
    MsgPanel: TPanel;
    Image1: TImage;
    MsgTextBox: TStaticText;
    ActionCompare: TAction;
    Compare1: TMenuItem;
    PgCompare: TTabSheet;
    CompareFrame: TCompareFrame;
    ToolButton4: TToolButton;
    EditByteCols: TComboBox;
    Timer1: TTimer;
    PgScript: TTabSheet;
    ScriptFrame: TScriptFrame;
    DbgToolsForm1: TMenuItem;
    ToolButton5: TToolButton;
    ActionUndo: TAction;
    ActionRedo: TAction;
    N1: TMenuItem;
    MIUndo: TMenuItem;
    MIRedo: TMenuItem;
    Undostack1: TMenuItem;
    CreateTestFile1: TMenuItem;
    PgBitmap: TTabSheet;
    BitmapFrame: TBitmapFrame;
    Something1: TMenuItem;
    MISetFileSize: TMenuItem;
    MIInsertBytes: TMenuItem;
    HintImage: TImage;
    ActionSetFileSize: TAction;
    ActionFillBytes: TAction;
    Loadplugin1: TMenuItem;
    PgSearchResult: TTabSheet;
    SearchResultsFrame: TSearchResultsFrame;
    MIView: TMenuItem;
    MIEncodingMenu: TMenuItem;
    ANSI1: TMenuItem;
    ASCII1: TMenuItem;
    ActionSelectRange: TAction;
    SelectRangeFormPanel: TPanel;
    LblSelRangeStart: TLabel;
    EditSelRangeStart: TEdit;
    LblSelRangeEnd: TLabel;
    EditSelRangeEnd: TEdit;
    BtnSelRangeOk: TButton;
    BtnSelRangeCancel: TButton;
    ImageProxy1: THintedImageProxy;
    MISelectRange: TMenuItem;
    EditorTabMenu: TPopupMenu;
    MICloseEditorTab: TMenuItem;
    ActionPasteAs: TAction;
    MIPasteAs: TMenuItem;
    ActionDebugMode: TAction;
    MIHelp: TMenuItem;
    ActionAboutBox: TAction;
    AboutHextor1: TMenuItem;
    PgHash: TTabSheet;
    HashFrame: THashFrame;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    Operations1: TMenuItem;
    ActionModifyWithExpr: TAction;
    MIModifyWithExpr: TMenuItem;
    ActionSaveAll: TAction;
    SaveAll1: TMenuItem;
    MISearchMenu: TMenuItem;
    MICopyAsMenu: TMenuItem;
    ActionClose: TAction;
    ActionCloseAll: TAction;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    ActionFindInFiles: TAction;
    FindReplaceinfiles1: TMenuItem;
    N2: TMenuItem;
    Copyfullname1: TMenuItem;
    Showinfolder1: TMenuItem;
    Fileinfo1: TMenuItem;
    Closeothertabs1: TMenuItem;
    Help1: TMenuItem;
    ActionHelpContents: THelpContents;
    PgAsm: TTabSheet;
    AsmFrame: TAsmFrame;
    procedure FormCreate(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure Regions1Click(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionGoToStartExecute(Sender: TObject);
    procedure ActionGoToEndExecute(Sender: TObject);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
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
    procedure MDITabsChange(Sender: TObject);
    procedure MDITabsGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
    procedure FormShow(Sender: TObject);
    procedure MDITabsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RecentFilesMenuPopup(Sender: TObject);
    procedure AfterEventTimerTimer(Sender: TObject);
    procedure estchangespeed1Click(Sender: TObject);
    procedure ActionCompareExecute(Sender: TObject);
    procedure EditByteColsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditByteColsSelect(Sender: TObject);
    procedure RightPanelPageControlChange(Sender: TObject);
    procedure RightPanelResize(Sender: TObject);
    procedure DbgToolsForm1Click(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure Undostack1Click(Sender: TObject);
    procedure CreateTestFile1Click(Sender: TObject);
    procedure Something1Click(Sender: TObject);
    procedure ActionSetFileSizeExecute(Sender: TObject);
    procedure ActionFillBytesExecute(Sender: TObject);
    procedure Loadplugin1Click(Sender: TObject);
    procedure ANSI1Click(Sender: TObject);
    procedure MIEncodingMenuClick(Sender: TObject);
    procedure ActionSelectRangeExecute(Sender: TObject);
    procedure MICloseEditorTabClick(Sender: TObject);
    procedure ActionDebugModeExecute(Sender: TObject);
    procedure ActionAboutBoxExecute(Sender: TObject);
    procedure ActionModifyWithExprExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionCopyAsArrayExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionCloseAllExecute(Sender: TObject);
    procedure ActionFindInFilesExecute(Sender: TObject);
    procedure Copyfullname1Click(Sender: TObject);
    procedure Showinfolder1Click(Sender: TObject);
    procedure Fileinfo1Click(Sender: TObject);
    procedure Closeothertabs1Click(Sender: TObject);
  private type
    TShortCutSet = record
      ShortCut: TShortCut;
      //SecondaryShortCuts: TCustomShortCutList;
      //SecondaryShortCuts: TArray<string>;
      SecondaryShortCuts: string;  // = Action.SecondaryShortCuts.Text
    end;
  private
    { Private declarations }
    FEditors: TObjectList<TEditorForm>;
    FInitialFilesOpened: Boolean;
    FDoAfterEvent: array of TProc;
    OldOnActiveControlChange: TNotifyEvent;
    EditorActionShortcuts: TDictionary<TContainedAction, TShortCutSet>;
    EditorForTabMenu: TEditorForm;
    procedure InitDefaultSettings();
    procedure LoadSettings();
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    function GetActiveEditor: TEditorForm;
    procedure SetActiveEditor(const Value: TEditorForm);
    function CreateNewEditor(): TEditorForm;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): TEditorForm;
    procedure OpenInitialFiles();
    procedure GenerateRecentFilesMenu(Menu: TMenuItem);
    procedure ApplyByteColEdit();
    procedure UpdateByteColEdit();
    procedure ActiveControlChanged(Sender: TObject);
    procedure ShortCutsWhenEditorActive(const AActions: array of TContainedAction);
  public
    { Public declarations }
    SettingsFolder, SettingsFile: string;
    TempPath: string;
//    HextorOle: TCoHextor;
    APIEnv: TAPIEnvironment;
    Utils: THextorUtils;
    OnVisibleRangeChanged: TCallbackListP1<TEditorForm>;
    OnSelectionChanged: TCallbackListP1<TEditorForm>;  // Called when either selection moves or data in selected range changes
    function OpenFile(DataSourceType: THextorDataSourceType; const AFileName: string): TEditorForm;
    procedure SaveSettings();
    procedure CheckEnabledActions();
    procedure UpdateMDITabs();
    procedure UpdateMsgPanel();
    procedure ActiveEditorChanged();
    procedure SelectionChanged();
    procedure VisibleRangeChanged();
    function GetIconIndex(DataSource: THextorDataSource): Integer;
    [API]
    property ActiveEditor: TEditorForm read GetActiveEditor write SetActiveEditor;
    function GetActiveEditorNoEx: TEditorForm;
    [API]
    property EditorCount: Integer read GetEditorCount;
    [API]
    property Editors[Index: Integer]: TEditorForm read GetEditor;
    procedure AddEditor(AEditor: TEditorForm);
    procedure RemoveEditor(AEditor: TEditorForm);
    function GetEditorIndex(AEditor: TEditorForm): Integer;
    function FindEditorWithSource(DataSourceType: THextorDataSourceType; const APath: string): TEditorForm;
    procedure ShowToolFrame(Frame: TFrame);
    procedure DoAfterEvent(Proc: TProc);
    function ParseFilePointer(Text: string; OldValue: TFilePointer): TFilePointer;

    [API]
    procedure DoTest(x: Integer);
  end;

var
  MainForm: TMainForm;
  AppSettings: THextorSettings;

implementation

{$R *.dfm}

uses
  System.SysConst,

  uFindReplaceForm, uDiskSelectForm, uProcessSelectForm, uBitsEditorForm,
  uDbgToolsForm, uEditedData, uProgressForm, uSetFileSizeForm, uFillBytesForm,
  uPasteAsForm, uAboutForm, uModifyWithExpressionForm, uCopyAsForm,
  uFileInfoForm;

{ TMainForm }

procedure TMainForm.ActionAboutBoxExecute(Sender: TObject);
begin
  AboutForm.ShowModal();
end;

procedure TMainForm.ActionBitsEditorExecute(Sender: TObject);
var
  Addr: TFilePointer;
  Buf: TBytes;
  x: Int64;
begin
  with ActiveEditor do
  begin
    Buf := GetSelectedOrAfterCaret(1, 4, Addr, True);

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

procedure TMainForm.ActionCloseAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i:=EditorCount-1 downto 0 do
    Editors[i].Close();
end;

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  ActiveEditor.Close();
end;

procedure TMainForm.ActionCompareExecute(Sender: TObject);
begin
  ShowToolFrame(CompareFrame);
  CompareFrame.ShowCompareDialog();
end;

procedure TMainForm.ActionCopyAsArrayExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    CopyAsForm.Data := Data.Get(SelStart, SelLength);
    CopyAsForm.ShowModal();
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
      if Application.MessageBox(PChar('Try to copy '+IntToStr(SelLength div MByte)+' megabytes to system clipboard?'), PChar('Copy'), MB_YESNO) <> IDYES then Exit;
    Buf := GetEditedData(SelStart, SelLength);
    if ActiveControl=PaneHex then
      s := Data2Hex(Buf, True)
    else
      s := Data2String(Buf, TextEncoding);
    Clipboard.AsText := s;

    if (Sender = ActionCut) and (InsertMode) then
      DeleteSelected();
  end;
end;

procedure TMainForm.ActionDebugModeExecute(Sender: TObject);
begin
  // Show Debug menu
  MIDebug.Visible := not MIDebug.Visible;
end;

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.ActionFillBytesExecute(Sender: TObject);
// Insert bytes / Fill selection
begin
  FillBytesForm.FEditor := ActiveEditor;
  FillBytesForm.Range := ActiveEditor.SelectedRange;
  FillBytesForm.FInsertPos := ActiveEditor.CaretPos;
  FillBytesForm.ShowModal();
end;

procedure TMainForm.ActionFindExecute(Sender: TObject);
begin
  // Show Find/Replace dialog and select "Find in current editor" mode
  FindReplaceForm.RBInCurrentEditor.Checked := True;
  FindReplaceForm.CPFindInFiles.Collapsed := True;
  FindReplaceForm.Show();
end;

procedure TMainForm.ActionFindInFilesExecute(Sender: TObject);
begin
  // Show Find/Replace dialog and select "Find in directories" mode
  if FindReplaceForm.RBInCurrentEditor.Checked then
    FindReplaceForm.RBInSelectedDirectories.Checked := True;
  FindReplaceForm.CPFindInFiles.Collapsed := False;
  FindReplaceForm.Show();
end;

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
begin
  if FindReplaceForm.Searcher.ParamsDefined() then
    FindReplaceForm.FindNext(1)
  else
    ActionFindExecute(Sender);
end;

procedure TMainForm.ActionFindPrevExecute(Sender: TObject);
begin
  if FindReplaceForm.Searcher.ParamsDefined() then
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
    if not InputQuery('Go to address', 'Go to address (use $ or 0x for hex value, + or - for relative jump; supports expressions):', s) then Exit;
    Pos := ParseFilePointer(s, CaretPos);

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

procedure TMainForm.ActionModifyWithExprExecute(Sender: TObject);
// Modify data with expression
begin
  ModifyWithExpressionForm.FEditor := ActiveEditor;
  ModifyWithExpressionForm.Range := ActiveEditor.SelectedRange;
  ModifyWithExpressionForm.ShowModal();
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
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
var
  i: Integer;
begin
  if not OpenDialog1.Execute() then Exit;

  for i:=0 to OpenDialog1.Files.Count-1 do
    OpenFile(TFileDataSource, OpenDialog1.Files[i]);
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
    if Sender = ActionPasteAs then
    begin
      if PasteAsForm.ShowModal() <> mrOk then Exit;
      Buf := PasteAsForm.ResultData;
    end
    else
    begin
      s := Clipboard.AsText;
      if ActiveControl=PaneHex then
        Buf := Hex2Data(s)
      else
        Buf := String2Data(s, TextEncoding);
    end;
    if Length(Buf)=0 then Exit;

    BeginUpdatePanes();
    UndoStack.BeginAction('', 'Paste');
    try
      if InsertMode then
      begin
        ReplaceSelected(Length(Buf), @Buf[0]);
      end
      else
      begin
        ChangeBytes(CaretPos, Buf);
        MoveCaret(CaretPos + Length(Buf), []);
      end;
    finally
      UndoStack.EndAction();
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.ActionRedoExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    BeginUpdatePanes();
    try
      UndoStack.Redo();
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

procedure TMainForm.ActionSaveAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to EditorCount-1 do
  begin
    // TODO: handle never saved editors
    with Editors[i] do
      SaveFile(THextorDataSourceType(DataSource.ClassType), DataSource.Path)
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
      SaveFile(THextorDataSourceType(DataSource.ClassType), DataSource.Path)
    else
      ActionSaveAsExecute(Sender);
  end;
end;

procedure TMainForm.ActionSaveSelectionAsExecute(Sender: TObject);
// Save selected part to another file.
// If same file is chosen, re-open it with new content
var
  SameFile: Boolean;
  AData: TBytes;
  fn: string;
  fs: TFileStream;
  E: EInOutError;
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
      raise EInvalidUserInput.Create('Cannot save file part under same name');
//      if Application.MessageBox('Current file will be overwritten and re-opened with new content', 'Replace file', MB_OKCANCEL) <> IDOK then Exit;

    AData := GetEditedData(SelStart, SelLength);

//    if SameFile then CloseCurrentFile(False);

    if not System.SysUtils.ForceDirectories(ExtractFilePath(fn)) then
    begin
      E := EInOutError.CreateRes(@SCannotCreateDir);
      E.ErrorCode := 3;
      raise E;
    end;

    fs := TFileStream.Create(fn, fmCreate);
    try
      fs.WriteBuffer(AData, Length(AData));
    finally
      fs.Free;
    end;

//    if SameFile then
//    begin
//      OpenFile(TFileDataSource, fn);
//    end;
  end;
end;

procedure TMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  with ActiveEditor do
    SetSelection(0, GetFileSize());
end;

procedure TMainForm.ActionSelectRangeExecute(Sender: TObject);
var
  Range: TFileRange;
begin
  with ActiveEditor do
  begin
    Range := TFileRange.Create(SelStart, SelStart + SelLength);

    EditSelRangeStart.Text := IntToStr(Range.Start);
    EditSelRangeEnd.Text := IntToStr(Range.AEnd);
    with MakeFormWithContent(SelectRangeFormPanel, bsDialog, 'Select range') do
    begin
      if ShowModal() <> mrOk then Exit;
    end;

    Range.Start := ParseFilePointer(EditSelRangeStart.Text, Range.Start);
    Range.AEnd := ParseFilePointer(EditSelRangeEnd.Text, Range.AEnd);

    SetSelection(Range.Start, Range.AEnd, CaretAtStart);
  end;
end;

procedure TMainForm.ActionSetFileSizeExecute(Sender: TObject);
var
  OldSize, NewSize: TFilePointer;
  Value: Byte;
  Buf: TBytes;
begin
  with ActiveEditor do
  begin
    OldSize := Data.GetSize();
    SetFileSizeForm.EditOldSize.Text := IntToStr(OldSize);
    SetFileSizeForm.EditNewSize.Text := IntToStr(OldSize);
    if SetFileSizeForm.ShowModal() <> mrOk then Exit;

    NewSize := ParseFilePointer(SetFileSizeForm.EditNewSize.Text, OldSize);
    if NewSize < 0 then
      raise EInvalidUserInput.Create('Size can not be negative');
    if NewSize > OldSize + MaxInt then
      raise EInvalidUserInput.Create('Can not add more than 2 GBytes in one operation');
    Value := StrToInt(SetFileSizeForm.EditFillValue.Text);

    UndoStack.BeginAction('', 'Set file size');
    try
      if (NewSize = OldSize) then
      begin
        // Nothing changed
      end
      else
      if NewSize < OldSize then
      begin
        Data.Delete(NewSize, OldSize - NewSize);
      end
      else
      begin
        SetLength(Buf, NewSize - OldSize);
        FillChar(Buf[0], Length(Buf), Value);
        Data.Insert(OldSize, Length(Buf), @Buf[0]);
      end;
    finally
      UndoStack.EndAction();
    end;
  end;
end;

procedure TMainForm.ActionUndoExecute(Sender: TObject);
begin
  with ActiveEditor do
  begin
    BeginUpdatePanes();
    try
      UndoStack.Undo();
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.ActiveControlChanged(Sender: TObject);
begin
  if Assigned(OldOnActiveControlChange) then
    OldOnActiveControlChange(Sender);
  if Application.Terminated then Exit;
  CheckEnabledActions();
end;

procedure TMainForm.ActiveEditorChanged;
begin
  UpdateMDITabs();
  UpdateByteColEdit();
  VisibleRangeChanged();
  SelectionChanged();
  UpdateMsgPanel();
end;

procedure TMainForm.AddEditor(AEditor: TEditorForm);
begin
  FEditors.Add(AEditor);
//  UpdateMDITabs();
//  Tabs will be updated when editor changes caption
end;

procedure TMainForm.ApplyByteColEdit;
// Apply byte column count from edit field
var
  n: Integer;
begin
  n := StrToIntDef(EditByteCols.Text, -1);
  if n <> -1 then
    n := BoundValue(n, 1, 16384);
  AppSettings.ByteColumns := n;
  SaveSettings();
  with ActiveEditor do
    ByteColumnsSetting := n;
  DoAfterEvent(UpdateByteColEdit);
end;

procedure TMainForm.CheckEnabledActions;
// Enable/disable actions based on active window, selection etc.
var
  FocusInEditor: Boolean;
  S: string;
  AShortCut: TPair<TContainedAction, TShortCutSet>;
  i: Integer;
  AEditor: TEditorForm;
begin
  FocusInEditor := False;
  ActionCompare.Enabled := (EditorCount >= 2);

  AEditor := GetActiveEditorNoEx();
  if AEditor <> nil then
  begin
    with AEditor do
    begin
      FocusInEditor := (Screen.ActiveControl=PaneHex) or (Screen.ActiveControl=PaneText);

      ActionSave.Enabled := (DataSource <> nil) and (dspWritable in DataSource.GetProperties()) and ((HasUnsavedChanges) or (DataSource.Path=''));
      ActionSaveAs.Enabled := True;
      ActionSaveAll.Enabled := True;
      ActionSaveSelectionAs.Enabled := (SelLength > 0);
      ActionRevert.Enabled := (HasUnsavedChanges);
      ActionClose.Enabled := True;
      ActionCloseAll.Enabled := True;

      for i:=0 to ActionList1.ActionCount-1 do
        if (ActionList1.Actions[i].Category = 'Edit') or
           (ActionList1.Actions[i].Category = 'Search') or
           (ActionList1.Actions[i].Category = 'Operations') then
          ActionList1.Actions[i].Enabled := True;

      ActionUndo.Enabled := UndoStack.CanUndo(S);
      ActionUndo.Caption := 'Undo ' + S;
      ActionUndo.Hint := ActionUndo.Caption;
      ActionRedo.Enabled := UndoStack.CanRedo(S);
      ActionRedo.Caption := 'Redo ' + S;
      ActionRedo.Hint := ActionRedo.Caption;

      ActionCopy.Enabled := {(FocusInEditor) and} (SelLength > 0);
      ActionCopyAsArray.Enabled := ActionCopy.Enabled;
      ActionCut.Enabled := (ActionCopy.Enabled) and (dspResizable in DataSource.GetProperties());
      ActionPaste.Enabled := Clipboard.HasFormat(CF_UNICODETEXT) and (DataSource <> nil) and (dspWritable in DataSource.GetProperties());
      ActionPasteAs.Enabled := ActionPaste.Enabled;

      ActionSelectAll.Enabled := True; //FocusInEditor;

      ActionSetFileSize.Enabled := (DataSource <> nil) and (dspResizable in DataSource.GetProperties());
      ActionFillBytes.Enabled := (DataSource <> nil) and (dspWritable in DataSource.GetProperties());
      ActionModifyWithExpr.Enabled := (DataSource <> nil) and (dspWritable in DataSource.GetProperties()) and (SelLength > 0);

      ActionBitsEditor.Enabled := (SelLength<=4);
    end;
  end
  else
  begin
    ActionSave.Enabled := False;
    ActionSaveAs.Enabled := False;
    ActionSaveAll.Enabled := False;
    ActionSaveSelectionAs.Enabled := False;
    ActionRevert.Enabled := False;
    ActionClose.Enabled := False;
    ActionCloseAll.Enabled := False;

    for i:=0 to ActionList1.ActionCount-1 do
      if (ActionList1.Actions[i].Category = 'Edit') or
         ((ActionList1.Actions[i].Category = 'Search') and (ActionList1.Actions[i] <> ActionFindInFiles)) or
         (ActionList1.Actions[i].Category = 'Operations') then
        ActionList1.Actions[i].Enabled := False;

    ActionUndo.Caption := 'Undo';
    ActionRedo.Caption := 'Redo';
  end;

  // Register/unregister action shortcuts
  for AShortCut in EditorActionShortcuts do
    if FocusInEditor then
    begin
      AShortCut.Key.ShortCut := AShortCut.Value.ShortCut;
      AShortCut.Key.SecondaryShortCuts.Text := AShortCut.Value.SecondaryShortCuts;
    end
    else
    begin
      AShortCut.Key.ShortCut := 0;
      AShortCut.Key.SecondaryShortCuts.Clear;
    end;
end;

procedure TMainForm.MICloseEditorTabClick(Sender: TObject);
begin
  if EditorForTabMenu <> nil then
    EditorForTabMenu.Close();
end;

procedure TMainForm.Closeothertabs1Click(Sender: TObject);
var
  i: Integer;
begin
  if EditorForTabMenu <> nil then
  begin
    for i:=EditorCount-1 downto 0 do
      if Editors[i] <> EditorForTabMenu then
        Editors[i].Close();
  end;
end;

procedure TMainForm.Copyfullname1Click(Sender: TObject);
begin
  if EditorForTabMenu <> nil then
    Clipboard.AsText := EditorForTabMenu.DataSource.Path;
end;

function TMainForm.CreateNewEditor: TEditorForm;
begin
  Result := TEditorForm.Create(Application);
  Result.ByteColumnsSetting := AppSettings.ByteColumns;
  Result.OnByteColsChanged.Add(procedure (Sender: TEditorForm)
    begin
      if Sender = GetActiveEditorNoEx() then
        UpdateByteColEdit();
    end);
  Result.OnClosed.Add(procedure(Sender: TEditorForm)
    begin
      if Sender = EditorForTabMenu then
        EditorForTabMenu := nil;
    end);
  // Call ActiveEditorChanged when this editor has DataSource etc.
  DoAfterEvent(ActiveEditorChanged);
end;

procedure TMainForm.CreateTestFile1Click(Sender: TObject);
const
  BlockSize = 1*MByte;
var
  Size: Int64;
  i: Integer;
  Block: TBytes;
  s: string;
  fs: TFileStream;
  s1: AnsiString;
begin
  s := '1 GB';
  if not InputQuery('Create test file', 'Create test file of size:', s) then Exit;
  Size := Str2FileSize(s);

  fs := TFileStream.Create(ExtractFilePath(Application.ExeName) + 'TestFile_'+s.Replace(' ','_')+'.dat', fmCreate);
  Progress.TaskStart(Sender);
  try
    SetLength(Block, BlockSize);
    for i:=0 to BlockSize div 4-1 do
      pCardinal(@Block[i*4])^ := Sqr(i);

    for i:=0 to Size div BlockSize-1 do
    begin
      s1 := AnsiString(Format('-------- Block %8d --------', [i]));
      Move(s1[Low(s1)], Block[0], Length(s1));
      fs.WriteBuffer(Block[0], BlockSize);
      Progress.Show(i+1, Size div BlockSize);
    end;

  finally
    Progress.TaskEnd();
    fs.Free;
  end;

end;

procedure TMainForm.DbgToolsForm1Click(Sender: TObject);
begin
  DbgToolsForm.Show();
end;

procedure TMainForm.DoAfterEvent(Proc: TProc);
// Proc will be called by timer just after processing of current event
begin
  FDoAfterEvent := FDoAfterEvent + [TProc(Proc)];
  AfterEventTimer.Enabled := True;
end;

procedure TMainForm.DoTest(x: Integer);
begin
  Application.MessageBox(PChar(IntToStr(x)), 'DoTest', MB_OK);
end;

procedure TMainForm.Fileinfo1Click(Sender: TObject);
begin
  if EditorForTabMenu <> nil then
  begin
    FileInfoForm.ShowInfo(EditorForTabMenu);
    FileInfoForm.ShowModal();
  end;
end;

function TMainForm.FindEditorWithSource(DataSourceType: THextorDataSourceType;
  const APath: string): TEditorForm;
// If we have requested data source (file) open in some editor, return it
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to EditorCount-1 do
    if (Editors[i].DataSource.ClassType = DataSourceType) and
       (SameFileName(Editors[i].DataSource.Path, APath)) then
      Exit(Editors[i]);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  // MDI childs' FormClose event is not called by VCL when main form closes
  for i:=EditorCount-1 downto 0 do
    Editors[i].OnClosed.Call(Editors[i]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ws: string;
  i: Integer;
begin
//  bWriteLogFile := True;
//  bThreadedLogWrite := False;
  Progress := TProgressTracker.Create();
  Progress.OnDisplay.Add(ProgressForm.ProgressDisplay);
  Progress.OnTaskEnd.Add(ProgressForm.ProgressTaskEnd);

  TempPath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(TPath.GetTempPath())+ChangeFileExt(ExtractFileName(Application.ExeName),''));

  AppSettings := THextorSettings.Create();

  // Get path to settings folder (AppData\Hextor)
  SetLength(ws, MAX_PATH);
  i:=SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, @ws[Low(ws)]);
  if i=0 then  SettingsFolder := IncludeTrailingPathDelimiter(PChar(ws)) + 'Hextor\'
         else  SettingsFolder := ExtractFilePath(Application.ExeName) + 'Settings\';
  SettingsFile := SettingsFolder + 'Settings.json';

  InitDefaultSettings();
  LoadSettings();

  // We will catch drag'n'dropped files
  DragAcceptFiles(Handle, True);

  // Listen for clipboard changes
  AddClipboardFormatListener(Handle);

  FEditors := TObjectList<TEditorForm>.Create(False);
  RightPanelPageControl.ActivePageIndex := AppSettings.ActiveRightPage;
  if AppSettings.RightPanelWidth > 0 then
    RightPanel.Width := AppSettings.RightPanelWidth;

  OldOnActiveControlChange := Screen.OnActiveControlChange;
  Screen.OnActiveControlChange := ActiveControlChanged;

  // Remember actions whose shortcuts should only be active in main editors
  EditorActionShortcuts := TDictionary<TContainedAction, TShortCutSet>.Create();
  for i:=0 to ActionList1.ActionCount-1 do
    if (ActionList1.Actions[i].Category = 'Edit') or
       ((ActionList1.Actions[i].Category = 'Search') and (ActionList1.Actions[i] <> ActionFindInFiles)) then
      ShortCutsWhenEditorActive([ActionList1.Actions[i]]);
  ShortCutsWhenEditorActive([ActionSave, ActionSaveAs, ActionRevert]);

  Utils := THextorUtils.Create();

//  HextorOle := TCoHextor.Create();
  APIEnv := TAPIEnvironment.Create();

  ScriptFrame.Init();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  RemoveClipboardFormatListener(Handle);

  ScriptFrame.Uninit();

  SaveSettings();
  AppSettings.Free;
  EditorActionShortcuts.Free;
  FEditors.Free;

  Utils.Free;
  APIEnv.Free;
  FreeAndNil(Progress);
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
begin
  Result := GetActiveEditorNoEx();
  if Result = nil then
    // Silently abort operation that requires active editor
    raise ENoActiveEditor.Create('No active editor');
end;

function TMainForm.GetActiveEditorNoEx: TEditorForm;
var
  Frm: TForm;
begin
  Frm := ActiveMDIChild;
  if (Frm <> nil) and (Frm is TEditorForm) then
    Result := TEditorForm(Frm)
  else
    Result := nil;
end;

function TMainForm.GetEditor(Index: Integer): TEditorForm;
begin
  Result := FEditors[Index];
end;

function TMainForm.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TMainForm.GetEditorIndex(AEditor: TEditorForm): Integer;
begin
  Result := FEditors.IndexOf(AEditor);
end;

function TMainForm.GetIconIndex(DataSource: THextorDataSource): Integer;
// Index in ImageList16 for given DataSource
var
  DSType: THextorDataSourceType;
begin
  DSType := THextorDataSourceType(DataSource.ClassType);
  if DSType = TDiskDataSource then  Result := 4
  else if DSType = TProcMemDataSource then  Result := 5
  else Result := 7;
end;

procedure TMainForm.InitDefaultSettings;
var
  Files: TStringDynArray;
  Source, Target: string;
  i: Integer;
begin
  AppSettings.ScrollWithWheel := 3;
  AppSettings.ByteColumns := -1;

//  AppSettings.Colors.ValueHighlightBg := $FFD0A0;

  // Copy default configuration from App_Folder\DefaultSettings to AppData
  try
    Source := TPath.Combine(ExtractFilePath(Application.ExeName), 'DefaultSettings');
    if System.SysUtils.DirectoryExists(Source) then
    begin
      Files := TDirectory.GetDirectories(Source, '*', TSearchOption.soTopDirectoryOnly);
      for i:=0 to Length(Files)-1 do
      begin
        Target := TPath.Combine(SettingsFolder, ExtractFileName(Files[i]));
        if not System.SysUtils.DirectoryExists(Target) then
          TDirectory.Copy(Files[i], Target);
      end;
    end;
  except
  end;
end;

type
  TInitPluginProc = procedure(App: OleVariant {IHextorApp}); stdcall;
//  TInsertProc = procedure(Addr: TFilePointer; Value: Byte) of Object;
//  TInitPluginProc = procedure(InsProc: TInsertProc); stdcall;

procedure TMainForm.Loadplugin1Click(Sender: TObject);
var
  hLib: HMODULE;
  pInit: TInitPluginProc;
  //app: IHextorApp;
  app: OleVariant;
begin
  hLib := LoadLibrary(PChar(ExtractFilePath(Application.ExeName) + 'Plugins\PluginTest.dll'));
  pInit := GetProcAddress(hLib, 'init');
  app := APIEnv.GetAPIWrapper(MainForm);//as IHextorApp;
  pInit(app);
//  pInit(MainForm.ActiveEditor.Data.Insert);

  FreeLibrary(hLib);
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
  Value := TValue.From<THextorSettings>(AppSettings);

  ctx.FromJson(TypeInfo(THextorSettings), json, Value);

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
  Pt: TPoint;
begin
  n := MDITabs.IndexOfTabAt(X, Y);
  if n >= 0 then
  begin
    case Button of
      mbMiddle:
        begin
          Editors[n].Close();
        end;
      mbRight:
        begin
          EditorForTabMenu := Editors[n];
          Pt := (Sender as TControl).ClientToScreen(Point(X,Y));
          EditorTabMenu.Popup(Pt.X, Pt.Y);
        end;
    end;
  end;
end;

procedure TMainForm.MIDummyRecentFileClick(Sender: TObject);
// Open file from "Recent files" menu
begin
  OpenFile(TFileDataSource, (Sender as TMenuItem).Caption);
end;

procedure TMainForm.MIEncodingMenuClick(Sender: TObject);
// Show current editor encoding
var
  AEncoding, i: Integer;
begin
  AEncoding := ActiveEditor.TextEncoding;
  for i:=0 to MIEncodingMenu.Count-1 do
    MIEncodingMenu.Items[i].Checked := (MIEncodingMenu.Items[i].Tag = AEncoding);
end;

procedure TMainForm.MIRecentFilesMenuClick(Sender: TObject);
begin
  GenerateRecentFilesMenu(MIRecentFilesMenu);
end;

function TMainForm.OpenFile(DataSourceType: THextorDataSourceType; const AFileName: string): TEditorForm;
var
  DS: THextorDataSource;
begin
  DS := DataSourceType.Create(AFileName);
  try
    DS.Open(fmOpenRead);
  except
    DS.Free;
    raise;
  end;
  Result := CreateNewEditor();
  with Result do
  begin
    DataSource := DS;
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

function TMainForm.ParseFilePointer(Text: string; OldValue: TFilePointer): TFilePointer;
// Parse file pointer from Text. Text can contain script expressions or just a number.
// If Text starts with '+' or '-', it is treated as relative to OldValue
var
  Rel: Integer;
begin
  Text := Trim(Text);
  if Text = '' then
    raise EInvalidUserInput.Create('"" is not a valid offset');
  Text := Text.Replace('$', '0x');

  if Text[Low(Text)] = '-' then
    Rel := -1
  else
  if Text[Low(Text)] = '+' then
    Rel := 1
  else
    Rel := 0;
  if Rel <> 0 then
    Delete(Text, Low(Text), 1);

  if not TryStrToInt64(Text, Result) then
    Result := ScriptFrame.Eval(Text);

  if Rel <> 0 then
    Result := OldValue + Rel * Result;
end;

procedure TMainForm.RecentFilesMenuPopup(Sender: TObject);
begin
  GenerateRecentFilesMenu(RecentFilesMenu.Items);
end;

procedure TMainForm.Regions1Click(Sender: TObject);
var
  s: string;
begin
  s := ActiveEditor.Data.GetDebugDescr();
  Application.MessageBox(PChar(s),'');
end;

procedure TMainForm.RemoveEditor(AEditor: TEditorForm);
begin
  FEditors.Remove(AEditor);
  // Update tabs and interface when editor form will be destroyed
  DoAfterEvent(ActiveEditorChanged);
end;

procedure TMainForm.RightPanelPageControlChange(Sender: TObject);
var
  AFrame: IHextorToolFrame;
begin
  AppSettings.ActiveRightPage := RightPanelPageControl.ActivePageIndex;
  if Supports(RightPanelPageControl.ActivePage.Controls[0], IHextorToolFrame, AFrame) then
    AFrame.OnShown();
end;

procedure TMainForm.RightPanelResize(Sender: TObject);
begin
  AppSettings.RightPanelWidth := RightPanel.Width;
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
    ctx.AsJson<THextorSettings>(AppSettings).SaveTo(fs, True, False);
    ctx.Free;
  finally
    fs.Free;
  end;
end;

procedure TMainForm.SelectionChanged;
begin
  CheckEnabledActions();
  OnSelectionChanged.Call(GetActiveEditorNoEx());

  {}
//  ScriptFrame.MemoOutput.Text := ActiveEditor.EditedData.GetDebugDescr();
  {}
end;

procedure TMainForm.SetActiveEditor(const Value: TEditorForm);
begin
  if (Value <> nil) and (Value <> GetActiveEditorNoEx()) then
    Value.BringToFront();
end;

procedure TMainForm.ShortCutsWhenEditorActive(const AActions: array of TContainedAction);
// Passed Actions should have ShortCuts active only when focus is in main editor.
// Just setting Action.Enabled to False does not works, because even disabled
// actions make their shortcuts not work in other windows and controls
var
  i: Integer;
  ASet: TShortCutSet;
begin
  for i:=0 to Length(AActions)-1 do
  begin
    ASet.ShortCut := AActions[i].ShortCut;
    ASet.SecondaryShortCuts := AActions[i].SecondaryShortCuts.Text;
    EditorActionShortcuts.AddOrSetValue(AActions[i], ASet);
  end;
end;

procedure TMainForm.Showinfolder1Click(Sender: TObject);
begin
  if (EditorForTabMenu <> nil) and (EditorForTabMenu.DataSource is TFileDataSource) then
  begin
    ShellExecute(0,'','explorer.exe',PChar('/select,"'+EditorForTabMenu.DataSource.Path+'"'),'',SW_SHOW);
  end;
end;

procedure TMainForm.ShowToolFrame(Frame: TFrame);
//var
//  i: Integer;
begin
//  for i:=0 to RightPanelPageControl.PageCount-1 do
  if Frame.Parent is TTabSheet then
    RightPanelPageControl.ActivePage := TTabSheet(Frame.Parent);
end;

procedure TMainForm.Something1Click(Sender: TObject);
var
  s: string;
begin
  s := {IntToStr(Low(Int64) + 1) + ' ' +} IntToStr(Low(Int64));
  Application.MessageBox(PChar(s), '', MB_OK);
end;

procedure TMainForm.EditByteColsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ApplyByteColEdit();
end;

procedure TMainForm.EditByteColsSelect(Sender: TObject);
begin
  ApplyByteColEdit();
end;

procedure TMainForm.AfterEventTimerTimer(Sender: TObject);
var
  i: Integer;
  AProc: TProc;
begin
  AfterEventTimer.Enabled := False;

  for i:=0 to Length(FDoAfterEvent)-1 do
  begin
    AProc := FDoAfterEvent[0];
    Delete(FDoAfterEvent, 0, 1);
    AProc();
  end;
end;

procedure TMainForm.ANSI1Click(Sender: TObject);
// Change current editor encoding
begin
  ActiveEditor.TextEncoding := (Sender as TMenuItem).Tag;
end;

procedure TMainForm.estchangespeed1Click(Sender: TObject);
const
  WriteCount = 100000;
var
  i: Integer;
  b: TBytes;
begin
  b := String2Data(AnsiString('#SPEEDTEST'));

  Progress.TaskStart(Sender);
  with ActiveEditor do
  begin
    BeginUpdatePanes();
    try
      for i:=1 to WriteCount do
      begin
//        StartTimeMeasure();
        ChangeBytes(Random(GetFileSize() - Length(b)), b);
//        EndTimeMeasure('change', True, 'Timing');
        Progress.Show(i, WriteCount);
      end;
    finally
      EndUpdatePanes();
    end;
  end;
  Progress.TaskEnd();
end;

procedure TMainForm.Undostack1Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '';
  with ActiveEditor.UndoStack do
    for i:=0 to Actions.Count-1 do
    begin
      if i = CurPointer then
        s := s + '<-- pointer' + sLineBreak;
      s := s + Actions[i].Code + ' ' + Actions[i].Caption+' ('+IntToStr(Actions[i].Changes.Count)+' changes) '+sLineBreak;
    end;
  Application.MessageBox(PChar(s),'');
end;

procedure TMainForm.UpdateByteColEdit;
var
  Editor: TEditorForm;
begin
  Editor := GetActiveEditorNoEx();
  if Editor <> nil then
  begin
    EditByteCols.Enabled := True;
    if Editor.ByteColumnsSetting <= 0 then
      EditByteCols.Text := 'Auto (' + IntToStr(Editor.ByteColumns) + ')'
    else
      EditByteCols.Text := IntToStr(Editor.ByteColumns);
  end
  else
    EditByteCols.Enabled := False;
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

procedure TMainForm.UpdateMsgPanel();
// Show warning for active editor if there is any
const
  WarnSize = 100*MByte;
var
  InplaceSaving, UseTempFile: Boolean;
  ShowMsg: Boolean;
  ASize: TFilePointer;
begin
  with ActiveEditor do
  begin
    ShowMsg := False;
    if (DataSource <> nil) then
    begin
      if TDataSaver.ChooseSaveMethod(Data, THextorDataSourceType(DataSource.ClassType), DataSource.Path, InplaceSaving, UseTempFile) then
      begin
        ASize := Data.GetSize();
        if ASize > WarnSize then
        begin
          if UseTempFile then
          begin
            ShowMsg := True;
            MsgTextBox.Caption := 'Saving your changes will require temporary file of ' + FileSize2Str(ASize);
          end;
        end;
      end
      else
      begin
        ShowMsg := True;
        MsgTextBox.Caption := 'Cannot save such changes to original source';
      end;
    end;
    MsgPanel.Visible := ShowMsg;
  end;
end;

procedure TMainForm.VisibleRangeChanged;
begin
  OnVisibleRangeChanged.Call(GetActiveEditorNoEx());
end;

procedure TMainForm.WMClipboardUpdate(var Msg: TMessage);
begin
  CheckEnabledActions();
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  i:Integer;
  Catcher: TDropFileCatcher;
  s:string;
begin
  inherited;
//  StartTimeMeasure();
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
//  EndTimeMeasure('DropFiles', True);
  Msg.Result := 0;
end;

{ THextorUtils }

procedure THextorUtils.Alert(V: Variant);
begin
  Application.MessageBox(PChar(string(V)), 'Alert', MB_OK);
end;

procedure THextorUtils.Sleep(milliseconds: Cardinal);
begin
  System.SysUtils.Sleep(milliseconds);
end;

end.
