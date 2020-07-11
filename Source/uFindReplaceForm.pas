{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
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
  System.Masks,

  uHextorTypes, uMainForm, uEditorForm, uEditedData, uCallbackList,
  uDataSearcher, uSearchResultsTabFrame, uHextorDataSources, uDataSaver,
  uHextorGUI;

type
  TFindReplaceForm = class(TForm)
    Timer1: TTimer;
    CategoryPanelGroup1: TCategoryPanelGroup;
    CPFind: TCategoryPanel;
    CPReplace: TCategoryPanel;
    Label1: TLabel;
    EditFindText: TComboBox;
    CBFindHex: TCheckBox;
    CBWildcards: TCheckBox;
    CBMatchCase: TCheckBox;
    BtnFindNext: TButton;
    BtnFindPrev: TButton;
    BtnFindCount: TButton;
    CBUnicode: TCheckBox;
    CBFindInSelection: TCheckBox;
    BtnFindList: TButton;
    Label2: TLabel;
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
    Label4: TLabel;
    EditFileNameMask: TComboBox;
    FileOpenDialog1: TFileOpenDialog;
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
  private type
    TSearchAction = (saCount, saList, saReplace);
    TFindWhereType = (fwCurrentFile, fwAllOpenFiles, fwSelectedDirectories);
    TFindWhere = record
      AType: TFindWhereType;
      Directories: TArray<string>;
      FileMasks: TArray<string>;
    end;
  private
    { Private declarations }
    FindWhere: TFindWhere;
    procedure FillParams(aReplace, aCanFindInSel: Boolean);
    function GetTargetEditor: TEditorForm;
    function GetTargetEditorNoEx: TEditorForm;
    procedure AutosizeForm();
    procedure CheckEnabledControls();
    procedure FindInData(AEditor: TEditorForm; AData: TEditedData; Action: TSearchAction; OldCount: Integer; var NewCount: Integer; ResultsFrame: TSearchResultsTabFrame);
    procedure FindInOpenFiles(Action: TSearchAction; ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
    procedure FindInDirectories(Action: TSearchAction; ResultsFrame: TSearchResultsTabFrame; var Count, InFilesCount: Integer);
    function ConfirmReplace(AEditor: TEditorForm; Ptr, Size: TFilePointer; var YesToAll: Boolean): TModalResult;
  public
    { Public declarations }
    Searcher: TDataSearcher;
    function FindNext(Direction: Integer): Boolean;
    function FindAll(Action: TSearchAction): Integer;
    property TargetEditor: TEditorForm read GetTargetEditor;
    property TargetEditorNoEx: TEditorForm read GetTargetEditorNoEx;
  end;

var
  FindReplaceForm: TFindReplaceForm;

implementation

{$R *.dfm}

function SplitPathList(const Text: string): TArray<string>;
// Split ';'-separated and '"'-quoted list of paths to string array
var
  sl: TStringList;
begin
  sl := TStringList.Create();
  try
    sl.Delimiter := PathSep;
    sl.StrictDelimiter := True;
    sl.QuoteChar := '"';
    sl.DelimitedText := Text;
    Result := sl.ToStringArray();
  finally
    sl.Free;
  end;
end;

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

  EditInDirectories.Enabled := (RBInSelectedDirectories.Checked);
  BtnSelectDirectory.Enabled := EditInDirectories.Enabled;
  EditFileNameMask.Enabled := (RBInSelectedDirectories.Checked);
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
  AutosizeForm();
end;

procedure TFindReplaceForm.FillParams(aReplace, aCanFindInSel: Boolean);
// Collect search parameters from input controls to internal structure
begin
  // What to search for
  with Searcher do
  begin
    // Search text
    Params.Text := EditFindText.Text;
    AddComboBoxHistory(EditFindText);
    Params.bHex := CBFindHex.Checked;
    Params.bWildcards := CBWildcards.Checked;
    Params.bUnicode := CBUnicode.Checked;
    Params.bMatchCase := CBMatchCase.Checked;
    Params.bFindInSel := aCanFindInSel and CBFindInSelection.Enabled and CBFindInSelection.Checked;

    // Replace
    Params.bReplace := aReplace;
    Params.Replace := EditReplaceText.Text;
    AddComboBoxHistory(EditReplaceText);
    Params.bRepHex := CBReplaceHex.Checked;
    Params.bAskEachReplace := CBAskReplace.Checked;

    // Encoding
    if TargetEditorNoEx <> nil then
      Params.CodePage := TargetEditorNoEx.TextEncoding
    else
      Params.CodePage := TEncoding.Default.CodePage;

    // Range inside data
    if Params.bFindInSel then
    begin
      Params.Range.Start := TargetEditor.SelStart;
      Params.Range.AEnd := TargetEditor.SelStart+TargetEditor.SelLength;
    end
    else
      Params.Range := EntireFile;

    // Generate binary search pattern from given text
    if Params.bHex then
      Params.Needle := Hex2Data(Params.Text)
    else
    if Params.bUnicode then
      Params.Needle := String2Data(Params.Text, TEncoding.Unicode.CodePage)
    else
      Params.Needle := String2Data(Params.Text, Params.CodePage);

    if Length(Params.Needle) = 0 then
      raise EInvalidUserInput.Create('Specify search string');
  end;

  // Where to search - editor or files
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
      FileMasks := SplitPathList(EditFileNameMask.Text);
      AddComboBoxHistory(EditFileNameMask);
    end;
  end;
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
  FillParams(False, True);
  Result := 0;
  InFilesCount := 0;

  // Confirmation for "Replace in directories" - in contrast to "replace in open editor(s)", this is
  // saved immediately to files and can't be easily undone
  if (Action = saReplace) and (FindWhere.AType = fwSelectedDirectories) then
  begin
    s := 'Replace all occurances of "' + Searcher.Params.Text + '" to "' +
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
  if Result = 0 then
    s := 'Search string not found'
  else
  begin
    s := 'Search string ' + IfThen(Action = saReplace, 'replaced', 'found') + ' '+IntToStr(Result)+' time(s)';
    if (FindWhere.AType <> fwCurrentFile) then
      s := s + ' in ' + IntToStr(InFilesCount) + ' file(s)';
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
    Result := 'Found '+IntToStr(OldCount + NewCount)+' time(s)  (' + AData.DataSource.Path + ')';
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
            SelectAfterOperation := TFileRange.Create(Searcher.LastFound.Start, Searcher.LastFound.Start + NewSize);
            // Adjust search range
            Searcher.Range.AEnd := Searcher.Range.AEnd + (NewSize - Searcher.LastFound.Size);
          end;
        finally
          if Assigned(AEditor) then
            AEditor.UndoStack.EndAction();
        end;
      end;

      // Add found item to list. We always add items to list when replacing
      if Action in [saList, saReplace] then
      begin
        ResultsFrame.AddListItem(ResultsGroupNode, Searcher.Haystack, TFileRange.Create(Ptr, Ptr + NewSize));
      end;

      Inc(NewCount);
      Start := Ptr + NewSize;
      Progress.Show(Start - Searcher.Range.Start, Searcher.Range.Size, ProgressText());
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
    if NewCount = 0 then
      ResultsFrame.DeleteListGroup(ResultsGroupNode);
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
    Start := TargetEditor.SelStart - Searcher.NeedleSize();
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
begin
  Searcher := TDataSearcher.Create();
  CPReplace.Collapsed := True;
  CPFindInFiles.Collapsed := True;
  AutosizeForm();
end;

procedure TFindReplaceForm.FormDestroy(Sender: TObject);
begin
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
end;

procedure TFindReplaceForm.FormShow(Sender: TObject);
begin
  CheckEnabledControls();
end;

function TFindReplaceForm.GetTargetEditor: TEditorForm;
begin
  Result := MainForm.ActiveEditor;
end;

function TFindReplaceForm.GetTargetEditorNoEx: TEditorForm;
begin
  Result := MainForm.GetActiveEditorNoEx;
end;

procedure TFindReplaceForm.RBInCurrentEditorClick(Sender: TObject);
begin
  CheckEnabledControls();
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
        var
          i: Integer;
        begin
          Progress.Show(0, 'Scanning folders... (' + Path + ')');
          if (SearchRec.Attr and faDirectory) <> 0 then Exit(True);
          Result := False;
          if Length(FindWhere.FileMasks) = 0 then
            Result := True
          else
          for i:=0 to Length(FindWhere.FileMasks)-1 do
            if MatchesMask(SearchRec.Name, FindWhere.FileMasks[i]) then
              Result := True;
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
      DataSource.Open(fmOpenRead);
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
          TDataSaver.Save(Data, TFileDataSource, FileNames[i]);
          DataSource := Data.DataSource;
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
