{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math, Vcl.ExtCtrls, Vcl.Samples.Gauges,
  System.UITypes, System.IOUtils, System.Types, Vcl.Buttons, System.StrUtils,
  System.Masks, Winapi.ShellAPI, Generics.Collections, Vcl.Clipbrd, Vcl.Menus,

  uHextorTypes, uMainForm, uEditorForm, uEditedData, uCallbackList,
  uDataSearcher, uSearchResultsTabFrame, uHextorDataSources, uDataSaver,
  uHextorGUI, uModuleSettings;

type
  TSearchSettings = class (TModuleSettings)
  public
    EditFindTextItems,
    EditReplaceTextItems,
    EditInDirectoriesItems,
    EditFileNameMaskItems: TArray<string>;
  end;

  TFindReplaceForm = class(TForm)
    Timer1: TTimer;
    CategoryPanelGroup1: TCategoryPanelGroup;
    CPFind: TCategoryPanel;
    CPReplace: TCategoryPanel;
    LblFind: TLabel;
    EditFindText: TComboBox;
    CBExtSyntax: TCheckBox;
    CBIgnoreCase: TCheckBox;
    BtnFindNext: TButton;
    BtnFindPrev: TButton;
    BtnFindCount: TButton;
    CBFindInSelection: TCheckBox;
    BtnFindList: TButton;
    LblReplaceWith: TLabel;
    EditReplaceText: TComboBox;
    CBReplaceHex: TCheckBox;
    BtnReplaceAll: TButton;
    CBReplaceInSelection: TCheckBox;
    CBAskReplace: TCheckBox;
    CPFindInFiles: TCategoryPanel;
    RBInCurrentEditor: TRadioButton;
    Label3: TLabel;
    RBInAllOpenFiles: TRadioButton;
    RBInSelectedDirectories: TRadioButton;
    EditInDirectories: TComboBox;
    BtnSelectDirectory: TSpeedButton;
    LblFileMasks: TLabel;
    EditFileNameMask: TComboBox;
    FileOpenDialog1: TFileOpenDialog;
    ImageProxy1: THintedImageProxy;
    Label5: TLabel;
    CBFilesSearchMode: TComboBox;
    HintedImageProxy1: THintedImageProxy;
    CBFindEncoding: TComboBox;
    RBFindHex: TRadioButton;
    RBFindText: TRadioButton;
    DropFileCatcher1: TDropFileCatcher;
    EditOptionsMenu: TPopupMenu;
    Clearhistory1: TMenuItem;
    BtnEditFindTextOptions: TSpeedButton;
    Pasteescaped1: TMenuItem;
    BtnEditReplaceTextOptions: TSpeedButton;
    HintedImageProxy2: THintedImageProxy;
    procedure BtnFindNextClick(Sender: TObject);
    procedure BtnFindCountClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnReplaceAllClick(Sender: TObject);
    procedure CBFindInSelectionClick(Sender: TObject);
    procedure CBReplaceInSelectionClick(Sender: TObject);
    procedure CPFindExpand(Sender: TObject);
    procedure BtnSelectDirectoryClick(Sender: TObject);
    procedure RBInCurrentEditorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnFindListClick(Sender: TObject);
    procedure DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
      Control: TWinControl; Files: TStrings; DropPoint: TPoint);
    procedure Clearhistory1Click(Sender: TObject);
    procedure EditFindTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnEditFindTextOptionsClick(Sender: TObject);
    procedure Pasteescaped1Click(Sender: TObject);
  private type
    TSearchAction = (saCount, saList, saReplace);
    TFindWhereType = (fwCurrentFile, fwAllOpenFiles, fwSelectedDirectories);
    TFileSearchMode = (fsAllItems, fsFirstItem, fsFilesNotContaining);
    TFindWhere = record
      AType: TFindWhereType;
      Directories: TArray<string>;
      FileMasks: TNameFilter;
      SearchMode: TFileSearchMode;
    end;
  private
    { Private declarations }
    FindWhere: TFindWhere;
    Inited: Boolean;
    procedure FillParams(aReplace, aCanFindInSel: Boolean);
    function GetTargetEditor: TEditorForm;
    function GetTargetEditorNoEx: TEditorForm;
    procedure AutosizeForm();
    procedure EnsureControlsNotClipped();
    procedure CheckEnabledControls();
    function GetCorrespondingEditControl(Sender: TComponent): TComboBox;
    procedure FindInData(AEditor: TEditorForm; AData: TEditedData; Action: TSearchAction; OldCount: Integer; var NewCount: Integer; ResultsFrame: TSearchResultsTabFrame);
    procedure FindInOpenFiles(Action: TSearchAction; ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
    procedure FindInDirectories(Action: TSearchAction; ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
    function ConfirmReplace(AEditor: TEditorForm; Ptr, Size: TFilePointer; var YesToAll: Boolean): TModalResult;
    procedure StoreToSettings();
  public
    { Public declarations }
    Searcher: TExtPatternDataSearcher;
    Settings: TSearchSettings;
    function FindNext(Direction: Integer): Boolean;
    function FindAll(Action: TSearchAction): Integer;
    property TargetEditor: TEditorForm read GetTargetEditor;
    property TargetEditorNoEx: TEditorForm read GetTargetEditorNoEx;
  end;

var
  FindReplaceForm: TFindReplaceForm;

implementation

{$R *.dfm}

{ TFindReplaceForm }

procedure TFindReplaceForm.AutosizeForm();
var
  i, h: Integer;
begin
  h := 0;
  for i:=0 to CategoryPanelGroup1.Panels.Count-1 do
    Inc(h, TCustomCategoryPanel(CategoryPanelGroup1.Panels[i]).Height);
  h := h + (Height - ClientHeight) + 2;
  Constraints.MinHeight := h;
  Constraints.MaxHeight := h;
end;

procedure TFindReplaceForm.BtnEditFindTextOptionsClick(Sender: TObject);
begin
  PopupFromControl(EditOptionsMenu, Sender as TControl);
end;

procedure TFindReplaceForm.BtnFindCountClick(Sender: TObject);
begin
  FindAll(saCount);
end;

procedure TFindReplaceForm.BtnFindListClick(Sender: TObject);
begin
  FindAll(saList);
end;

procedure TFindReplaceForm.BtnFindNextClick(Sender: TObject);
begin
  FindNext((Sender as TButton).Tag);
end;

procedure TFindReplaceForm.BtnReplaceAllClick(Sender: TObject);
begin
  FindAll(saReplace);
end;

procedure TFindReplaceForm.BtnSelectDirectoryClick(Sender: TObject);
// Directory select dialog
var
  sl: TStringList;
begin
  if not FileOpenDialog1.Execute then Exit;
  sl := TStringList.Create();
  try
    sl.Assign(FileOpenDialog1.Files);
    sl.Delimiter := PathSep;
    sl.QuoteChar := '"';
    EditInDirectories.Text := sl.DelimitedText;
  finally
    sl.Free;
  end;
end;

procedure TFindReplaceForm.CBFindInSelectionClick(Sender: TObject);
begin
  CBReplaceInSelection.Checked := CBFindInSelection.Checked;
end;

procedure TFindReplaceForm.CBReplaceInSelectionClick(Sender: TObject);
begin
  CBFindInSelection.Checked := CBReplaceInSelection.Checked;
end;

procedure TFindReplaceForm.CheckEnabledControls;
// Enable/disable some controls based on selected search options
var
  FindInEditor: Boolean;
  ATargetEditor: TEditorForm;
begin
  ATargetEditor := TargetEditorNoEx;

  RBInCurrentEditor.Enabled := (ATargetEditor <> nil);
  RBInAllOpenFiles.Enabled := (MainForm.EditorCount > 0);

  FindInEditor := (RBInCurrentEditor.Checked) and (ATargetEditor <> nil);
  BtnFindPrev.Enabled := FindInEditor;
  BtnFindNext.Enabled := FindInEditor;
  CBFindInSelection.Enabled := FindInEditor and (ATargetEditor <> nil) and (ATargetEditor.SelLength > 0);
  CBReplaceInSelection.Enabled := CBFindInSelection.Enabled;
  CBAskReplace.Enabled := not RBInSelectedDirectories.Checked;

  EditInDirectories.Enabled := RBInSelectedDirectories.Checked;
  BtnSelectDirectory.Enabled := EditInDirectories.Enabled;
  EditFileNameMask.Enabled := RBInSelectedDirectories.Checked;
  CBFilesSearchMode.Enabled := RBInAllOpenFiles.Checked or RBInSelectedDirectories.Checked;
end;

procedure TFindReplaceForm.Clearhistory1Click(Sender: TObject);
var
  CB: TComboBox;
begin
  CB := GetCorrespondingEditControl(EditOptionsMenu.PopupComponent);
  if CB = nil then Exit;
  CB.Items.Clear();
  CB.Text := '';
  StoreToSettings();
end;

function TFindReplaceForm.ConfirmReplace(AEditor: TEditorForm; Ptr,
  Size: TFilePointer; var YesToAll: Boolean): TModalResult;
// Show found item in editor and display replacement confirmation
begin
  // Show found occurrence in editor
  MainForm.ActiveEditor := AEditor;
  AEditor.SelectAndShow(Ptr, Ptr + Size);

  AEditor.EndUpdate();  // Redraw editor when showing confirmation
  try
    Result := MessageDlg('Replace this Occurrence?'+sLineBreak+'"'+Searcher.Params.Text+'"  ->  "'+Searcher.Params.Replace+'"',
                      mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel, TMsgDlgBtn.mbYesToAll], 0);
  finally
    AEditor.BeginUpdate();
  end;

  case Result of
    mrCancel:   Abort();
    mrYesToAll: YesToAll := True;
  end;
end;

procedure TFindReplaceForm.CPFindExpand(Sender: TObject);
begin
  EnsureControlsNotClipped();
  AutosizeForm();
end;

procedure TFindReplaceForm.DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
  Control: TWinControl; Files: TStrings; DropPoint: TPoint);
// Add drag'n'dropped directories to "Search in:" edit field
var
  i:Integer;
  s:string;
begin
  for I := 0 to Files.Count-1 do
  begin
    s:=Files[i];
    if DirectoryExists(s) then
    begin
      if EditInDirectories.Text <> '' then
        EditInDirectories.Text := EditInDirectories.Text + ';';
      EditInDirectories.Text := EditInDirectories.Text + s;
      RBInSelectedDirectories.Checked := True;
      CPFindInFiles.Expand();
    end;
  end;
end;

procedure TFindReplaceForm.EditFindTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CB: TComboBox;
  i: Integer;
begin
  CB := Sender as TComboBox;
  // Shift+Del in list deletes selected item
  if (Key = VK_DELETE) and (ssShift in Shift) and (CB.DroppedDown) then
  begin
    i := CB.ItemIndex;
    if i >= 0 then
    begin
      Key := 0;
      CB.Items.Delete(i);
      if i < CB.Items.Count then
        CB.ItemIndex := i;
      StoreToSettings();
    end;
  end;
end;

procedure TFindReplaceForm.EnsureControlsNotClipped();
// Workaround for incorrect panel height on non-primary monitor with different DPI
var
  i, j, MaxExtent: Integer;
  Pnl: TCustomCategoryPanel;
  Sfc: TCategoryPanelSurface;
begin
  for i:=0 to CategoryPanelGroup1.Panels.Count-1 do
  if not TCustomCategoryPanel(CategoryPanelGroup1.Panels[i]).Collapsed then
  begin
    MaxExtent := 0;
    Pnl := TCustomCategoryPanel(CategoryPanelGroup1.Panels[i]);
    Sfc := TCategoryPanelSurface(Pnl.Controls[0]);
    for j := 0 to Sfc.ControlCount - 1 do
    begin
      MaxExtent := Max(MaxExtent, Sfc.Controls[j].Top + Sfc.Controls[j].Height);
    end;
    Inc(MaxExtent, 10);
    if MaxExtent > Pnl.ClientHeight then
      Pnl.ClientHeight := MaxExtent;
  end;
end;

procedure TFindReplaceForm.FillParams(aReplace, aCanFindInSel: Boolean);
// Collect search parameters from input controls to internal structure
begin
  // What to search for
  with Searcher do
  begin
    FreeAndNil(Pattern);
    FreeAndNil(ReplacePattern);

    // Search text
    Params.Text := EditFindText.Text;
    AddComboBoxHistory(EditFindText);
    Params.bHex := RBFindHex.Checked;
    Params.FindCodePage := IfThen(RBFindText.Checked, Integer(CBFindEncoding.Items.Objects[CBFindEncoding.ItemIndex]) {SearchCodePages[CBFindEncoding.ItemIndex]}, 0);
    Params.bExtSyntax := CBExtSyntax.Checked;
    Params.bIgnoreCase := CBIgnoreCase.Checked;
    Params.bFindInSel := aCanFindInSel and CBFindInSelection.Enabled and CBFindInSelection.Checked;

    // Replace
    Params.bReplace := aReplace;
    Params.Replace := EditReplaceText.Text;
    AddComboBoxHistory(EditReplaceText);
    Params.bRepHex := CBReplaceHex.Checked;
    Params.ReplaceCodePage := Params.FindCodePage;
    Params.bAskEachReplace := CBAskReplace.Checked;

    // Range inside data
    if Params.bFindInSel then
    begin
      Params.Range.Start := TargetEditor.SelStart;
      Params.Range.AEnd := TargetEditor.SelStart+TargetEditor.SelLength;
    end
    else
      Params.Range := EntireFile;

    try
      // Parse replace pattern first, determine HasPlaceholders
      if Params.bReplace then
      begin
        ReplacePattern := TExtReplacePattern.Create(Params.Replace, Params.bRepHex, Params.bExtSyntax, Params.ReplaceCodePage);
      end;

      // Parse search pattern
      Pattern := TExtMatchPattern.Create(Params.Text, Params.bHex, Params.bIgnoreCase, Params.bExtSyntax, Params.FindCodePage,
        Assigned(ReplacePattern) and (ReplacePattern.HasPlaceholders));
      if Pattern.IsEmpty() then
        raise EMatchPatternException.Create('Specify search string');
      Pattern.CalcMinMaxMatchSize(MinMatchSize, MaxMatchSize);
      if MinMatchSize <= 0 then
        raise EMatchPatternException.Create('Expression can match empty buffer');
      if MaxMatchSize > 16 * KByte then  // Limited by BlockOverlap in DataSearcher
        raise EMatchPatternException.Create('Max allowed match size is 16 KBytes');
    except
      FreeAndNil(Pattern);
      FreeAndNil(ReplacePattern);
      raise;
    end;
  end;

  // Where to search - editor or files
  Finalize(FindWhere);
  FillChar(FindWhere, SizeOf(FindWhere), 0);
  with FindWhere do
  begin
    if RBInCurrentEditor.Checked       then  AType := fwCurrentFile
    else
    if RBInAllOpenFiles.Checked        then  AType := fwAllOpenFiles
    else
    if RBInSelectedDirectories.Checked then  AType := fwSelectedDirectories;

    if AType = fwSelectedDirectories then
    begin
      Directories := SplitPathList(EditInDirectories.Text);
      if Length(Directories) = 0 then
        raise EInvalidUserInput.Create('Specify search directories');
      AddComboBoxHistory(EditInDirectories);
    end;

    if AType = fwSelectedDirectories then
    begin
      FileMasks := TNameFilter.FromString(EditFileNameMask.Text);
      AddComboBoxHistory(EditFileNameMask);
    end;

    if AType in [fwAllOpenFiles, fwSelectedDirectories] then
    begin
      SearchMode := TFileSearchMode(CBFilesSearchMode.ItemIndex);
    end;
  end;

  StoreToSettings();
end;

function TFindReplaceForm.FindAll(Action: TSearchAction): Integer;
// Universal top-level function for "Count", "List" and "Replace"
var
  ResultsFrame: TSearchResultsTabFrame;
  ResultBoundToEditor: TEditorForm;
  InFilesCount: Integer;
  s: string;
  t: Cardinal;
begin
  ResultsFrame := nil;
  // Collect search parameters from input controls
  FillParams(Action = saReplace, True);
  Result := 0;
  InFilesCount := 0;

  if (Action = saReplace) and ( FindWhere.SearchMode = fsFilesNotContaining) then
    raise EInvalidUserInput.Create('"Find files NOT containing..." option is not applicable to "Replace" command');

  // Confirmation for "Replace in directories" - in contrast to "replace in open editor(s)", this is
  // saved immediately to files and can't be easily undone
  if (Action = saReplace) and (FindWhere.AType = fwSelectedDirectories) then
  begin
    s := 'Replace ' + IfThen(FindWhere.SearchMode = fsFirstItem, 'first occurrence', 'all occurances') + ' of "' + Searcher.Params.Text + '" to "' +
         Searcher.Params.Replace + '" in selected directories/files?' + sLineBreak + sLineBreak +
         'This can''t be undone.';
    if Application.MessageBox(PChar(s), 'Replace', MB_OKCANCEL) <> IDOK then Exit;
  end;

  if Action in [saList, saReplace] then
  // Prepare result list
  begin
    if FindWhere.AType = fwCurrentFile then
      ResultBoundToEditor := TargetEditor
    else
      ResultBoundToEditor := nil;
    ResultsFrame := MainForm.SearchResultsFrame.StartNewList(ResultBoundToEditor, Searcher.Params.Text);
  end;

  Progress.TaskStart(Self);
  try
    t := GetTickCount();

    case FindWhere.AType of
      fwCurrentFile:  // Search in current editor
        begin
          FindInData(TargetEditor, TargetEditor.Data, Action, 0, Result, ResultsFrame);
        end;
      fwAllOpenFiles:  // Search in all open files
        begin
          FindInOpenFiles(Action, ResultsFrame, Result, InFilesCount);
        end;
      fwSelectedDirectories:  // Search in specified directories
        begin
          FindInDirectories(Action, ResultsFrame, Result, InFilesCount);
        end;
    end;

    t := GetTickCount() - t;
    WriteLog('Search: ' + IntToStr(Result) + ' items, ' + IntToStr(t) + ' ms');

  finally
    if Action in [saList, saReplace] then
    begin
      ResultsFrame.EndUpdateList();
      MainForm.ShowToolFrame(MainForm.SearchResultsFrame);
    end;
    Progress.TaskEnd();
  end;

  // Result count message box
  if FindWhere.SearchMode = fsFilesNotContaining then
    s := 'Found ' + IntToStr(InFilesCount) + ' file(s) which do NOT contain given pattern'
  else
  begin
    if Result = 0 then
      s := 'Search pattern not found'
    else
    begin
      s := 'Search pattern ' + IfThen(Action = saReplace, 'replaced', 'found');
      if FindWhere.SearchMode <> fsFirstItem then
        s := s + ' '+IntToStr(Result)+' time(s)';
      if (FindWhere.AType <> fwCurrentFile) then
        s := s + ' in ' + IntToStr(InFilesCount) + ' file(s)';
    end;
  end;

  Application.MessageBox(PChar(s), 'Search', MB_OK);
end;

procedure TFindReplaceForm.FindInData(AEditor: TEditorForm; AData: TEditedData;
  Action: TSearchAction; OldCount: Integer; var NewCount: Integer; ResultsFrame: TSearchResultsTabFrame);
// Find all occurances of requested item in specified EditedData and do specified Action.
// OldCount is used to display total progress
var
  ResultsGroupNode: Pointer;  // PResultTreeNode type is private to TSearchResultsTabFrame
  Start, Ptr: TFilePointer;
  Size, NewSize: Integer;
  YesToAll: Boolean;
  ActionCode: Integer;
  SelectAfterOperation: TFileRange;  // This range will be selected after operation complete (e.g. last replaced item)

  function ProgressText(): string;
  begin
    Result := 'Found '+IntToStr(OldCount + NewCount)+' time(s)  (' + AData.DataSource.DisplayName + ')';
  end;

begin
  Searcher.Haystack := AData;
  ResultsGroupNode := nil;
  if Action in [saList, saReplace] then
  begin
    ResultsGroupNode := ResultsFrame.AddListGroup(AEditor, Searcher.Haystack);
  end;
  NewCount := 0;
  Start := Searcher.Params.Range.Start;
  YesToAll := False;
  ActionCode := Random(1000000);
  SelectAfterOperation := NoRange;

  Progress.TaskStart(Searcher);
  if Assigned(AEditor) then
    AEditor.BeginUpdate();
  try
    Progress.Show(0, ProgressText());
    while Searcher.FindNext(Start, 1, Ptr, Size) do  // <--
    begin
      NewSize := Size;

      if Action = saReplace then
      begin
        // Confirm replace if it is needed
        if (Assigned(AEditor)) and (Searcher.Params.bAskEachReplace) and (not YesToAll) then
        begin
          SelectAfterOperation := NoRange;  // To keep selection set by "ConfirmReplace()" if search cancelled
          if ConfirmReplace(AEditor, Ptr, NewSize, YesToAll) = mrNo then
          begin
            Start := Ptr + Size;
            Continue;
          end;

          ActionCode := Random(1000000);  // Confirmed changes will be separate "undo" steps
        end;

        // Replace
        if Assigned(AEditor) then
          AEditor.UndoStack.BeginAction('Replace_'+IntToStr(ActionCode), 'Replace');
        try
          if Searcher.ReplaceLastFound(NewSize) then
          begin
            SelectAfterOperation := TFileRange.Create(Searcher.LastFoundRange.Start, Searcher.LastFoundRange.Start + NewSize);
            // Adjust search range
            Searcher.Range.AEnd := Searcher.Range.AEnd + (NewSize - Searcher.LastFoundRange.Size);
          end;
        finally
          if Assigned(AEditor) then
            AEditor.UndoStack.EndAction();
        end;
      end;

      Inc(NewCount);

      // "Find files NOT containing..." mode
      if (FindWhere.SearchMode = fsFilesNotContaining) then
      begin
        Break;
      end;

      // Add found item to list. We always add items to list when replacing
      if Action in [saList, saReplace] then
      begin
        ResultsFrame.AddListItem(ResultsGroupNode, Searcher.Haystack, TFileRange.Create(Ptr, Ptr + NewSize), Searcher.Params.FindCodePage);
      end;

      Start := Ptr + NewSize;
      Progress.Show(Start - Searcher.Range.Start, Searcher.Range.Size, ProgressText());

      // "Find first occurrence in each file" mode
      if FindWhere.SearchMode = fsFirstItem then
      begin
        Break;
      end;
    end;
  finally
    // Move caret to where operation ended
    if (FindWhere.AType = fwCurrentFile) and (Assigned(AEditor)) and
       (SelectAfterOperation <> NoRange) then
    begin
      AEditor.SelectAndShow(SelectAfterOperation.AEnd, SelectAfterOperation.AEnd);
    end;
    if Assigned(AEditor) then
      AEditor.EndUpdate();

    // In "Find files NOT containing...", we invert "NewCount": it will be 1 if we found nothing
    if FindWhere.SearchMode = fsFilesNotContaining then
    begin
      if NewCount = 0 then
        NewCount := 1
      else
        NewCount := 0;
    end;

    if (NewCount = 0) then
    begin
      ResultsFrame.DeleteListGroup(ResultsGroupNode);
    end;
    Progress.TaskEnd();
  end;
end;

function TFindReplaceForm.FindNext(Direction: Integer): Boolean;
// Find requested item in active editor starting from current position in specified direction.
var
  Start, Ptr: TFilePointer;
  Dir, Size: Integer;
  MoveCaret: TEditorForm.TCaretInSelection;
begin
  // Collect search parameters from input controls
  FillParams(False, False);
  Searcher.Haystack := TargetEditor.Data;
  Dir := Direction;
  if Dir > 0 then
    Start := TargetEditor.SelStart + TargetEditor.SelLength
  else
  begin
    Start := TargetEditor.SelStart - Searcher.MinMatchSize;
    if TargetEditor.SelLength=0 then Inc(Start);
    if Start > TargetEditor.GetFileSize()-1 then Start := TargetEditor.GetFileSize()-1;
  end;

  Progress.TaskStart(Searcher);
  try
    if Searcher.FindNext(Start, Dir, Ptr, Size) then
    begin
      // Select found item. Move caret to it's start/end depending on search direction
      if Dir > 0 then MoveCaret := CaretAtEnd
                 else MoveCaret := CaretAtStart;
      TargetEditor.SelectAndShow(Ptr, Ptr + Size, MoveCaret);
      Result := True;
    end
    else
    begin
      // Nothing more found - remove selection and move caret to start/end of last found item
      TargetEditor.SetSelection(IfThen(Dir > 0, TargetEditor.SelStart + TargetEditor.SelLength, TargetEditor.SelStart), -1, CaretAtStart);
      Result := False;
    end;
  finally
    Progress.TaskEnd();
  end;
end;

procedure TFindReplaceForm.FormCreate(Sender: TObject);
var
  Sizes: TDictionary<TControl, TRect>;
  i: Integer;
  CS: TPair<TControl, TRect>;
begin
  Searcher := TExtPatternDataSearcher.Create();

  Settings := TSearchSettings.Create();

  // Crunch for a bug in TCategoryPanel: after first Collapse, right-aligned child controls disappear.
  // We save their sizes and restore it afterwards.
  Sizes := TDictionary<TControl, TRect>.Create();
  for i:=0 to CPFindInFiles.ControlCount-1 do
    Sizes.AddOrSetValue(CPFindInFiles.Controls[i], CPFindInFiles.Controls[i].BoundsRect);
  for i:=0 to CPReplace.ControlCount-1 do
    Sizes.AddOrSetValue(CPReplace.Controls[i], CPReplace.Controls[i].BoundsRect);

  CPReplace.Collapsed := True;
  CPFindInFiles.Collapsed := True;

  for CS in Sizes do
    CS.Key.BoundsRect := CS.Value;
  Sizes.Free;

  EnsureControlsNotClipped();
  AutosizeForm();

  // We will catch drag'n'dropped directories and add them to "Search in:" edit field
  DragAcceptFiles(Handle, True);
end;

procedure TFindReplaceForm.FormDestroy(Sender: TObject);
begin
  Settings.Free;
  Searcher.Free;
end;

procedure TFindReplaceForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssAlt in Shift then
  begin
    if Key = VK_LEFT then
    begin
      FindNext(-1);
      Key := 0;
    end;
    if Key = VK_RIGHT then
    begin
      FindNext(1);
      Key := 0;
    end;
  end;
  if Key = VK_ESCAPE then
  begin
    if (ActiveControl is TComboBox) and ((ActiveControl as TComboBox).DroppedDown) then
    begin
    end
    else
      Close();
  end;
end;

procedure TFindReplaceForm.FormShow(Sender: TObject);
var
  n: Integer;
begin
  if not Inited then
  begin
    with Settings do
    begin
      EditFindText.Items.AddStrings(EditFindTextItems);
      EditReplaceText.Items.AddStrings(EditReplaceTextItems);
      EditInDirectories.Items.AddStrings(EditInDirectoriesItems);
      EditFileNameMask.Items.AddStrings(EditFileNameMaskItems);
    end;
    Inited := True;
  end;

  CheckEnabledControls();

  n := CBFindEncoding.ItemIndex;
  GetUsedEncodings(CBFindEncoding.Items, False);
  if n < 0 then n := 0;
  CBFindEncoding.ItemIndex := n;

  EnsureControlsNotClipped();
end;

function TFindReplaceForm.GetCorrespondingEditControl(
  Sender: TComponent): TComboBox;
begin
  if (Sender = LblFind) or (Sender = BtnEditFindTextOptions) then
    Result := EditFindText
  else if (Sender = LblReplaceWith) or (Sender = BtnEditReplaceTextOptions) then
    Result := EditReplaceText
  else if Sender = RBInSelectedDirectories then
    Result := EditInDirectories
  else if Sender = LblFileMasks then
    Result := EditFileNameMask
  else
    Result := nil;
end;

function TFindReplaceForm.GetTargetEditor: TEditorForm;
begin
  Result := MainForm.ActiveEditor;
end;

function TFindReplaceForm.GetTargetEditorNoEx: TEditorForm;
begin
  Result := MainForm.GetActiveEditorNoEx;
end;

function EscapeSearchChars(const Text: string): string;
var
  sb: TStringBuilder;
  i: Integer;
  c: Char;
  s: string;
begin
  sb := TStringBuilder.Create(Length(Text));
  for i := Low(Text) to High(Text) do
  begin
    c := Text[i];
    case c of
      #9: s := '\t';
      #10: s := '\n';
      #13: s := '\r';
      '?', '\', '{', '(', ')': s := '\' + c;
      NullCharClipboardReplace: s := '\x00';
      else
        begin
          if c < ' ' then
            s := '\x' + IntToHex(Ord(c), 2)
          else
            s := c;
        end;
    end;
    sb.Append(s);
  end;
  Result := sb.ToString();
  sb.Free;
end;

procedure TFindReplaceForm.Pasteescaped1Click(Sender: TObject);
var
  Text, SubText: string;
  CB: TComboBox;
  SelStart, SelEnd: Integer;
begin
  CB := GetCorrespondingEditControl(EditOptionsMenu.PopupComponent);
  if CB = nil then Exit;
  SubText := EscapeSearchChars(Clipboard.AsText);
  Text := CB.Text;
  SelStart := CB.SelStart;
  SelEnd := CB.SelStart + CB.SelLength;
  Text := Text.Substring(0, SelStart) + SubText + Text.Substring(SelEnd, MaxInt);
  CB.Text := Text;
  CB.SelStart := SelStart + Length(SubText);
end;

procedure TFindReplaceForm.RBInCurrentEditorClick(Sender: TObject);
begin
  CheckEnabledControls();
end;

procedure TFindReplaceForm.StoreToSettings;
begin
  with Settings do
  begin
    EditFindTextItems := EditFindText.Items.ToStringArray;
    EditReplaceTextItems := EditReplaceText.Items.ToStringArray;
    EditInDirectoriesItems := EditInDirectories.Items.ToStringArray;
    EditFileNameMaskItems := EditFileNameMask.Items.ToStringArray;
  end;
  Settings.Changed();
end;

procedure TFindReplaceForm.FindInDirectories(Action: TSearchAction;
  ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
// Search for requested item in specified directories
var
  FileNames, FilesInDir: TStringDynArray;
  i, j, NewCount: Integer;
  Data: TEditedData;
  DataSource: THextorDataSource;
  TotalSize: Double;  // We can overflow Int64 here
begin
  Count := 0;
  InFilesCount := 0;
  TotalSize := 0;

  // Collect a list of files matching requested directory/mask
  FileNames := [];
  Progress.TaskStart(Self, 0);
  try
    for i:=0 to Length(FindWhere.Directories)-1 do
    begin
      FilesInDir := TDirectory.GetFiles(FindWhere.Directories[i], '*', TSearchOption.soAllDirectories,
        function(const Path: string; const SearchRec: TSearchRec): Boolean
        // Compare with all masks from list
        begin
          Progress.Show(-1, 'Scanning folders... (' + Path + ')');
          if (SearchRec.Attr and faDirectory) <> 0 then Exit(True);
          Result := FindWhere.FileMasks.Matches(SearchRec.Name, TPath.Combine(Path, SearchRec.Name));
          if Result then
            TotalSize := TotalSize + SearchRec.Size;
        end);
      // Cannot operate on opened file
      if Action = saReplace then
        for j:=0 to Length(FilesInDir)-1 do
          if MainForm.FindEditorWithSource(TFileDataSource, FilesInDir[j]) <> nil then
            raise EInvalidUserInput.Create('"Replace in directory" cannot operate on a file that is open in editor now. ' +
              'Close this file before proceeding:' + sLineBreak + sLineBreak + FilesInDir[j]);
      FileNames := FileNames + FilesInDir;
    end;
    if TotalSize = 0 then TotalSize := 1;
  finally
    Progress.TaskEnd();
  end;

  // Search in files
  for i:=0 to Length(FileNames)-1 do
  begin
    DataSource := TFileDataSource.Create(FileNames[i]);
    try
      DataSource.Open(fmOpenRead, fmShareDenyNone);
      Progress.TaskStart(Searcher, DataSource.GetSize() / TotalSize);
      Data := TEditedData.Create();
      try
        Data.DataSource := DataSource;

        // Do search in loaded data
        FindInData(nil, Data, Action, Count, NewCount, ResultsFrame);  // <--

        Inc(Count, NewCount);
        if NewCount > 0 then
          Inc(InFilesCount);

        // If something was replaced, save file
        if (Action = saReplace) and (NewCount > 0) then
        begin
          TDataSaver.Save(Data, DataSource);
        end;

      finally
        Data.Free;
        Progress.TaskEnd();
      end;
    finally
      DataSource.Free;
    end;
  end;
end;

procedure TFindReplaceForm.FindInOpenFiles(Action: TSearchAction;
  ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
// Search in all open files
var
  i, NewCount: Integer;
  TotalSize: Double;  // We can overflow Int64 here
begin
  // Calculate sum of files sizes to display total progress
  TotalSize := 0;
  for i:=0 to MainForm.EditorCount-1 do
    TotalSize := TotalSize + MainForm.Editors[i].GetFileSize();
  if TotalSize = 0 then TotalSize := 1;

  // Search
  Count := 0;
  InFilesCount := 0;
  for i:=0 to MainForm.EditorCount-1 do
  begin
    Progress.TaskStart(Searcher, MainForm.Editors[i].GetFileSize() / TotalSize);
    try
      FindInData(MainForm.Editors[i], MainForm.Editors[i].Data, Action, Count, NewCount, ResultsFrame);
      Inc(Count, NewCount);
      if NewCount > 0 then
        Inc(InFilesCount);
    finally
      Progress.TaskEnd();
    end;
  end;
end;

procedure TFindReplaceForm.Timer1Timer(Sender: TObject);
begin
  if not Visible then Exit;
  CheckEnabledControls();
end;

end.
