unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math, Vcl.ExtCtrls, Vcl.Samples.Gauges,
  System.UITypes,

  uHextorTypes, uMainForm, uEditorForm, uEditedData, uUtil, uCallbackList,
  uDataSearcher;

type
  TFindReplaceForm = class(TForm)
    GBFind: TGroupBox;
    GBReplace: TGroupBox;
    Label1: TLabel;
    EditFindText: TComboBox;
    CBFindHex: TCheckBox;
    CBWildcards: TCheckBox;
    CBMatchCase: TCheckBox;
    BtnFindNext: TButton;
    BtnFindPrev: TButton;
    Label2: TLabel;
    EditReplaceText: TComboBox;
    CBReplaceHex: TCheckBox;
    BtnReplaceAll: TButton;
    BtnFindCount: TButton;
    CBUnicode: TCheckBox;
    CBFindInSelection: TCheckBox;
    Timer1: TTimer;
    CBReplaceInSelection: TCheckBox;
    CBAskReplace: TCheckBox;
    BtnFindList: TButton;
    procedure BtnFindNextClick(Sender: TObject);
    procedure BtnFindCountClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnReplaceAllClick(Sender: TObject);
    procedure CBFindInSelectionClick(Sender: TObject);
    procedure CBReplaceInSelectionClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillParams(aReplace, aCanFindInSel: Boolean);
    function GetTargetEditor: TEditorForm;
  public
    { Public declarations }
    Searcher: TDataSearcher;
    function FindNext(Direction: Integer): Boolean;
    property TargetEditor: TEditorForm read GetTargetEditor;
  end;

var
  FindReplaceForm: TFindReplaceForm;

implementation

{$R *.dfm}

{ TFindReplaceForm }

procedure TFindReplaceForm.BtnFindCountClick(Sender: TObject);
var
  Count: Integer;
  Start, Ptr: TFilePointer;
  Size: Integer;
begin
  FillParams(False, True);
  Count := 0;
  Start := Searcher.Params.Range.Start;
  if Sender = BtnFindList then
    MainForm.SearchResultsFrame.BeginUpdateList(GetTargetEditor, Searcher.Params.Text);
  try
    while Searcher.Find(Start, 1, Ptr, Size) do
    begin
      Inc(Count);
      if Sender = BtnFindList then
      begin
        MainForm.SearchResultsFrame.AddListItem(TFileRange.Create(Ptr, Ptr + Size));
      end;

      Start := Ptr+Size;
      MainForm.ShowProgress(Searcher, Start - Searcher.Params.Range.Start, Searcher.Params.Range.Size, 'Found '+IntToStr(Count)+' time(s)');
    end;
  finally
    MainForm.OperationDone(Searcher);
    if Sender = BtnFindList then
      MainForm.SearchResultsFrame.EndUpdateList();
  end;
  if Sender = BtnFindList then
  begin
    MainForm.ShowToolFrame(MainForm.SearchResultsFrame);
  end
  else
    Application.MessageBox(PChar('Search string found '+IntToStr(Count)+' times'), 'Search', MB_OK);
end;

procedure TFindReplaceForm.BtnFindNextClick(Sender: TObject);
begin
  FindNext((Sender as TButton).Tag);
end;

procedure TFindReplaceForm.BtnReplaceAllClick(Sender: TObject);
var
  Count: Integer;
  Start, Ptr, ACaret: TFilePointer;
  Size, NewSize: Integer;
  Res: Integer;
  YesToAll, Cancelled: Boolean;
  ActionCode: Integer;
  LastReplaced: TFileRange;
  s: string;
begin
  FillParams(True, True);
  Count := 0;
  Start := Searcher.Params.Range.Start;
  YesToAll := False;
  Cancelled := False;
  ActionCode := Random(1000000);
  LastReplaced := NoRange;

  TargetEditor.BeginUpdatePanes();
  try

    while Searcher.Find(Start, 1, Ptr, Size) do
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
          Res := MessageDlg('Replace this Occurrence?'+#13#10+'"'+Searcher.Params.Text+'"  ->  "'+Searcher.Params.Replace+'"',
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
          Searcher.Params.Range.AEnd := Searcher.Params.Range.AEnd + (NewSize - Searcher.LastFound.Size);
        end;
      finally
        TargetEditor.UndoStack.EndAction();
      end;

      Inc(Count);
      Start := Ptr+NewSize;

      MainForm.ShowProgress(Searcher, Start - Searcher.Params.Range.Start, Searcher.Params.Range.Size, 'Replaced '+IntToStr(Count)+' time(s)');
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

procedure TFindReplaceForm.FillParams(aReplace, aCanFindInSel: Boolean);
begin
  with Searcher do
  begin
    Haystack := GetTargetEditor.Data;

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

    if Params.bFindInSel then
    begin
      Params.Range.Start := TargetEditor.SelStart;
      Params.Range.AEnd := TargetEditor.SelStart+TargetEditor.SelLength;
    end
    else
      Params.Range := EntireFile;

    if Params.bHex then
      Params.Needle := Str2Bytes(Hex2Data(Params.Text, True))
    else
    if Params.bUnicode then
      Params.Needle := Str2Bytes(Params.Text)
    else
      Params.Needle := Str2Bytes(AnsiString(Params.Text));

    if Length(Params.Needle) = 0 then
      raise EInvalidUserInput.Create('Specify search string');

    if EditFindText.Items.IndexOf(Params.Text) < 0 then
      EditFindText.Items.Insert(0, Params.Text);
    while EditFindText.Items.Count > 20 do
      EditFindText.Items.Delete(EditFindText.Items.Count-1);
  end;
end;

function TFindReplaceForm.FindNext(Direction: Integer): Boolean;
var
  Start, Ptr, ACaret: TFilePointer;
  Dir, Size: Integer;
begin
  FillParams(False, False);
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
    if Searcher.Find(Start, Dir, Ptr, Size) then
    begin
      TargetEditor.BeginUpdatePanes();
      try
        ACaret := Ptr + IfThen(Dir>0, Size-1, 0);
        TargetEditor.ScrollToShow(ACaret, -1, -1);
        TargetEditor.MoveCaret(ACaret, []);
        TargetEditor.SetSelection(Ptr, Ptr + Size);
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

function TFindReplaceForm.GetTargetEditor: TEditorForm;
begin
  Result := MainForm.ActiveEditor;
end;

procedure TFindReplaceForm.Timer1Timer(Sender: TObject);
begin
  if not Visible then Exit;
  if MainForm.EditorCount = 0 then
  begin
    Close();
    Exit;
  end;
  CBFindInSelection.Enabled := (TargetEditor.SelLength > 0);
  CBReplaceInSelection.Enabled := CBFindInSelection.Enabled;
end;

end.
