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

  uHextorTypes, uMainForm, uEditorForm, uEditedData, uCallbackList,
  uDataSearcher, uSearchResultsTabFrame, uHextorDataSources;

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
    TFineWhereType = (fwCurrentFile, fwAllOpenFiles, fwSelectedDirectories);
    TFindWhere = record
      AType: TFineWhereType;
      Directories: TArray<string>;
      FileMask: string;
    end;
  private
    { Private declarations }
    FindWhere: TFindWhere;
    procedure FillParams(aReplace, aCanFindInSel: Boolean);
    function GetTargetEditor: TEditorForm;
    function GetTargetEditorNoEx: TEditorForm;
    procedure AutosizeForm();
    procedure CheckEnabledControls();
    procedure FindInData(AEditor: TEditorForm; AData: TEditedData; Action: TSearchAction; var Count: Integer; ResultsFrame: TSearchResultsTabFrame);
    function FindInDirectories(Action: TSearchAction; ResultsFrame: TSearchResultsTabFrame): Integer;
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

{ TFindReplaceForm }

procedure TFindReplaceForm.AutosizeForm();
var
  i, h: Integer;
begin
  h := 0;
  for i:=0 to CategoryPanelGroup1.Panels.Count-1 do
    Inc(h, TCustomCategoryPanel(CategoryPanelGroup1.Panels[i]).Height);
  //Constraints := TSizeConstraints.Create(
  h := h + (Height - ClientHeight) + 2;
  Constraints.MinHeight := h;
  Constraints.MaxHeight := h;
//  ClientHeight := h;
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
{var
  Count: Integer;
  Start, Ptr, ACaret: TFilePointer;
  Size, NewSize: Integer;
  Res: Integer;
  YesToAll, Cancelled: Boolean;
  ActionCode: Integer;
  LastReplaced: TFileRange;
  s: string;}
begin
  FindAll(saReplace);
{  FillParams(True, True);
  if FindWhere.AType <> fwCurrentFile then
    raise Exception.Create('Replace in files not implemented yet');
  Count := 0;
  Searcher.Haystack := TargetEditor.Data;
  Start := Searcher.Params.Range.Start;
  YesToAll := False;
  Cancelled := False;
  ActionCode := Random(1000000);
  LastReplaced := NoRange;

  TargetEditor.BeginUpdatePanes();
  try

    while Searcher.FindNext(Start, 1, Ptr, Size) do
    begin
      if (Searcher.Params.bAskEachReplace) and (not YesToAll) then
      begin
        // Show found occurrence in editor
        TargetEditor.BeginUpdatePanes();
        try
          ACaret := Ptr;
          TargetEditor.ScrollToShow(ACaret, -1, -1);
          TargetEditor.MoveCaret(ACaret, []);
          TargetEditor.SetSelection(Ptr, Ptr + Size);
        finally
          TargetEditor.EndUpdatePanes();
        end;
        LastReplaced := NoRange;  // To keep this selection if search cancelled

        TargetEditor.EndUpdatePanes();  // Redraw editor when showing confirmation
        try
          Res := MessageDlg('Replace this Occurrence?'+sLineBreak+'"'+Searcher.Params.Text+'"  ->  "'+Searcher.Params.Replace+'"',
                            mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel, TMsgDlgBtn.mbYesToAll], 0);
        finally
          TargetEditor.BeginUpdatePanes();
        end;

        case Res of
          mrYes: begin end;
          mrNo:
            begin
              Start := Ptr + Size;
              Continue;
            end;
          mrCancel:
            begin
              Cancelled := True;
              Break;
            end;
          mrYesToAll: YesToAll := True;
        end;
        ActionCode := Random(1000000);  // Confirmed changes will be separate "undo" steps
      end;

      TargetEditor.UndoStack.BeginAction('Replace_'+IntToStr(ActionCode), 'Replace');
      try
        if Searcher.ReplaceLastFound(NewSize) then
        begin
          LastReplaced := TFileRange.Create(Searcher.LastFound.Start, Searcher.LastFound.Start + NewSize);
          Searcher.Range.AEnd := Searcher.Range.AEnd + (NewSize - Searcher.LastFound.Size);
        end;
      finally
        TargetEditor.UndoStack.EndAction();
      end;

      Inc(Count);
      Start := Ptr+NewSize;

      MainForm.ShowProgress(Searcher, Start - Searcher.Range.Start, Searcher.Range.Size, 'Replaced '+IntToStr(Count)+' time(s)');
    end;

  finally
    MainForm.OperationDone(Searcher);
    // Move caret to last replaced
    if LastReplaced <> NoRange then
    begin
      TargetEditor.BeginUpdatePanes();
      try
        ACaret := LastReplaced.AEnd;
        TargetEditor.ScrollToShow(ACaret, -1, -1);
        TargetEditor.MoveCaret(ACaret, []);
      finally
        TargetEditor.EndUpdatePanes();
      end;
    end;
    TargetEditor.EndUpdatePanes();
  end;
  if not Cancelled then
  begin
    if Count > 0 then
      s := 'Search string replaced '+IntToStr(Count)+' times'
    else
      s := 'Search string not found';
    Application.MessageBox(PChar(s), 'Replace', MB_OK);
  end; }
end;

procedure TFindReplaceForm.BtnSelectDirectoryClick(Sender: TObject);
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

  EditInDirectories.Enabled := (RBInSelectedDirectories.Checked);
  BtnSelectDirectory.Enabled := EditInDirectories.Enabled;
  EditFileNameMask.Enabled := (RBInSelectedDirectories.Checked);
end;

function TFindReplaceForm.ConfirmReplace(AEditor: TEditorForm; Ptr,
  Size: TFilePointer; var YesToAll: Boolean): TModalResult;
var
  ACaret: TFilePointer;
begin
  // Show found occurrence in editor
  MainForm.ActiveEditor := AEditor;
  AEditor.BeginUpdatePanes();
  try
    ACaret := Ptr;
    AEditor.ScrollToShow(ACaret, -1, -1);
    AEditor.MoveCaret(ACaret, []);
    AEditor.SetSelection(Ptr, Ptr + Size);
  finally
    AEditor.EndUpdatePanes();
  end;

  AEditor.EndUpdatePanes();  // Redraw editor when showing confirmation
  try
    Result := MessageDlg('Replace this Occurrence?'+sLineBreak+'"'+Searcher.Params.Text+'"  ->  "'+Searcher.Params.Replace+'"',
                      mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel, TMsgDlgBtn.mbYesToAll], 0);
  finally
    AEditor.BeginUpdatePanes();
  end;

  case Result of
    mrCancel:
      begin
        Abort();
      end;
    mrYesToAll: YesToAll := True;
  end;

end;

procedure TFindReplaceForm.CPFindExpand(Sender: TObject);
begin
  AutosizeForm();
end;

procedure TFindReplaceForm.FillParams(aReplace, aCanFindInSel: Boolean);
var
  sl: TStringList;
begin
  with Searcher do
  begin
    Params.Text := EditFindText.Text;
    Params.bHex := CBFindHex.Checked;
    Params.bWildcards := CBWildcards.Checked;
    Params.bUnicode := CBUnicode.Checked;
    Params.bMatchCase := CBMatchCase.Checked;
    Params.bFindInSel := aCanFindInSel and CBFindInSelection.Enabled and CBFindInSelection.Checked;

    Params.bReplace := aReplace;
    Params.Replace := EditReplaceText.Text;
    Params.bRepHex := CBReplaceHex.Checked;
    Params.bAskEachReplace := CBAskReplace.Checked;

    if TargetEditorNoEx <> nil then
      Params.CodePage := TargetEditorNoEx.TextEncoding
    else
      Params.CodePage := TEncoding.Default.CodePage;

    if Params.bFindInSel then
    begin
      Params.Range.Start := TargetEditor.SelStart;
      Params.Range.AEnd := TargetEditor.SelStart+TargetEditor.SelLength;
    end
    else
      Params.Range := EntireFile;

    if Params.bHex then
      Params.Needle := Hex2Data(Params.Text)
    else
    if Params.bUnicode then
      Params.Needle := String2Data(Params.Text, TEncoding.Unicode.CodePage)
    else
      Params.Needle := String2Data(Params.Text, Params.CodePage);

    if Length(Params.Needle) = 0 then
      raise EInvalidUserInput.Create('Specify search string');

    if EditFindText.Items.IndexOf(Params.Text) < 0 then
      EditFindText.Items.Insert(0, Params.Text);
    while EditFindText.Items.Count > 20 do
      EditFindText.Items.Delete(EditFindText.Items.Count-1);
  end;

  with FindWhere do
  begin
    if RBInCurrentEditor.Checked then  AType := fwCurrentFile
    else
    if RBInAllOpenFiles.Checked then  AType := fwAllOpenFiles
    else
    if RBInSelectedDirectories.Checked then  AType := fwSelectedDirectories;

    if AType = fwSelectedDirectories then
    begin
      sl := TStringList.Create();
      try
        sl.Delimiter := PathSep;
        sl.QuoteChar := '"';
        sl.DelimitedText := EditInDirectories.Text;
        Directories := sl.ToStringArray();
      finally
        sl.Free;
      end;
//      Directories := string(EditInDirectories.Text).Split([PathSep], '"');
    end;

    if AType <> fwCurrentFile then
    begin
      FileMask := EditFileNameMask.Text;
      if Trim(FileMask) = '' then
        FileMask := '*';
    end;
  end;
end;

function TFindReplaceForm.FindAll(Action: TSearchAction): Integer;
var
  ResultsFrame: TSearchResultsTabFrame;
  ResultBoundToEditor: TEditorForm;
  i: Integer;
begin
  ResultsFrame := nil;
  FillParams(False, True);
  Result := 0;

  if (Action = saReplace) and (FindWhere.AType = fwSelectedDirectories) then
    raise Exception.Create('Replace in directory not implemented yet');

  if Action in [saList, saReplace] then
  begin
    if FindWhere.AType = fwCurrentFile then
      ResultBoundToEditor := TargetEditor
    else
      ResultBoundToEditor := nil;
    ResultsFrame := MainForm.SearchResultsFrame.StartNewList(ResultBoundToEditor, Searcher.Params.Text);
  end;

  try

    case FindWhere.AType of
      fwCurrentFile:
        begin
          FindInData(TargetEditor, TargetEditor.Data, Action, Result, ResultsFrame);
        end;
      fwAllOpenFiles:
        begin
          for i:=0 to MainForm.EditorCount-1 do
          begin
            FindInData(MainForm.Editors[i], MainForm.Editors[i].Data, Action, Result, ResultsFrame);
          end;
        end;
      fwSelectedDirectories:
        begin
          Result := FindInDirectories(Action, ResultsFrame);
        end;

    end;

  finally
    if Action in [saList, saReplace] then
      ResultsFrame.EndUpdateList();
  end;

  if Action in [saList, saReplace] then
  begin
    MainForm.ShowToolFrame(MainForm.SearchResultsFrame);
  end;
  Application.MessageBox(PChar('Search string ' + IfThen(Action = saReplace, 'replaced', 'found') + ' '+IntToStr(Result)+' times'), 'Search', MB_OK);
end;

procedure TFindReplaceForm.FindInData(AEditor: TEditorForm; AData: TEditedData;
  Action: TSearchAction; var Count: Integer; ResultsFrame: TSearchResultsTabFrame);
var
  ResultsGroupNode: Pointer;
  Start, Ptr, ACaret: TFilePointer;
  NewCount, Size, NewSize: Integer;
  YesToAll{, Cancelled}: Boolean;
  ActionCode: Integer;
  //LastReplaced: TFileRange;
  SelectAfterOperation: TFileRange;
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
//  Cancelled := False;
  ActionCode := Random(1000000);
  SelectAfterOperation := NoRange;

  if Assigned(AEditor) then
    AEditor.BeginUpdatePanes();
  try
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
      MainForm.ShowProgress(Searcher, Start - Searcher.Range.Start, Searcher.Range.Size, 'Found '+IntToStr(Count + NewCount)+' time(s)');
    end;
  finally
    // Move caret to where operation ended
    if (FindWhere.AType = fwCurrentFile) and (Assigned(AEditor)) and
       (SelectAfterOperation <> NoRange) then
    begin
      AEditor.BeginUpdatePanes();
      try
        ACaret := SelectAfterOperation.AEnd;
        AEditor.ScrollToShow(ACaret, -1, -1);
        AEditor.MoveCaret(ACaret, []);
      finally
        AEditor.EndUpdatePanes();
      end;
    end;
    if Assigned(AEditor) then
      AEditor.EndUpdatePanes();
    if NewCount = 0 then
      ResultsFrame.DeleteListGroup(ResultsGroupNode);
    Inc(Count, NewCount);
    MainForm.OperationDone(Searcher);
  end;
end;

function TFindReplaceForm.FindNext(Direction: Integer): Boolean;
var
  Start, Ptr, ACaret: TFilePointer;
  Dir, Size: Integer;
begin
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

  try
    if Searcher.FindNext(Start, Dir, Ptr, Size) then
    begin
      TargetEditor.BeginUpdatePanes();
      try
        ACaret := Ptr + IfThen(Dir>0, Size-1, 0);
        TargetEditor.ScrollToShow(ACaret, -1, -1);
        TargetEditor.MoveCaret(ACaret, []);
        TargetEditor.SetSelection(Ptr, Ptr + Size, False);
      finally
        TargetEditor.EndUpdatePanes();
      end;
      Result := True;
    end
    else
    begin
      TargetEditor.SetSelection(TargetEditor.CaretPos, -1);
      Result := False;
    end;
  finally
    MainForm.OperationDone(Searcher);
  end;
end;

procedure TFindReplaceForm.FormCreate(Sender: TObject);
begin
  Searcher := TDataSearcher.Create();
  Searcher.OnProgress.Add(MainForm.ShowProgress);
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
      FindNext(-1);
    if Key = VK_RIGHT then
      FindNext(1);
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

function TFindReplaceForm.FindInDirectories(Action: TSearchAction;
  ResultsFrame: TSearchResultsTabFrame): Integer;
var
  FileNames, FilesInDir: TStringDynArray;
  i: Integer;
  Data: TEditedData;
  DataSource: THextorDataSource;
begin
  Result := 0;
  // Collect a list of files matching requested directory/mask
  FileNames := [];
  for i:=0 to Length(FindWhere.Directories)-1 do
  begin
    FilesInDir := TDirectory.GetFiles(FindWhere.Directories[i], FindWhere.FileMask, TSearchOption.soAllDirectories);
    FileNames := FileNames + FilesInDir;
  end;

  // Search in files
  for i:=0 to Length(FileNames)-1 do
  begin
    DataSource := TFileDataSource.Create(FileNames[i]);
    try
      DataSource.Open(fmOpenRead);
      Data := TEditedData.Create();
      try
        Data.DataSource := DataSource;

        FindInData(nil, Data, Action, Result, ResultsFrame);  // <--

      finally
        Data.Free;
      end;
    finally
      DataSource.Free;
    end;


  end;


end;

procedure TFindReplaceForm.Timer1Timer(Sender: TObject);
begin
  if not Visible then Exit;
  CheckEnabledControls();
end;

end.
