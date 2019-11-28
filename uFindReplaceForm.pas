unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math, Vcl.ExtCtrls, Vcl.Samples.Gauges,

  uDWHexTypes, uMainForm, uEditorForm, uEditedData, uUtil, uCallbackList,
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
    BtnReplaceNext: TButton;
    BtnReplaceAll: TButton;
    BtnFindCount: TButton;
    CBUnicode: TCheckBox;
    CBFindInSelection: TCheckBox;
    Timer1: TTimer;
    ProgressPanel: TPanel;
    BtnAbort: TButton;
    Gauge1: TGauge;
    LblProgress: TLabel;
    procedure BtnFindNextClick(Sender: TObject);
    procedure BtnFindCountClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FAborted: Boolean;
    procedure FillParams(aReplace: Boolean);
    procedure ShowProgress(Sender: TDataSearcher; Pos, Total: TFilePointer);
    procedure OperationDone(Sender: TDataSearcher);
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

procedure TFindReplaceForm.BtnAbortClick(Sender: TObject);
begin
  FAborted := True;
end;

procedure TFindReplaceForm.BtnFindCountClick(Sender: TObject);
var
  Count: Integer;
  Range: TFileRange;
  Start, Ptr: TFilePointer;
  Size: Integer;
begin
  FillParams(False);
  Count := 0;
  if Searcher.Params.bFindInSel then
  begin
    Range.Start := TargetEditor.SelStart;
    Range.AEnd := TargetEditor.SelStart+TargetEditor.SelLength;
  end
  else
    Range := EntireFile;
  Start := Range.Start;
  while Searcher.Find(Range, Start, 1, Ptr, Size) do
  begin
    Inc(Count);
    Start := Ptr+Size;
  end;
  Application.MessageBox(PChar('Search string found '+IntToStr(Count)+' times'), 'Search', MB_OK);
end;

procedure TFindReplaceForm.BtnFindNextClick(Sender: TObject);
begin
  FindNext((Sender as TButton).Tag);
end;

procedure TFindReplaceForm.FillParams(aReplace: Boolean);
begin
  with Searcher do
  begin
    Haystack := GetTargetEditor.EditedData;

    Params.Text := EditFindText.Text;
    Params.bHex := CBFindHex.Checked;
    Params.bWildcards := CBWildcards.Checked;
    Params.bUnicode := CBUnicode.Checked;
    Params.bMatchCase := CBMatchCase.Checked;
    Params.bFindInSel := CBFindInSelection.Enabled and CBFindInSelection.Checked;

    Params.bReplace := aReplace;
    Params.Replace := EditReplaceText.Text;
    Params.bRepHex := CBReplaceHex.Checked;

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
  Start, Ptr: TFilePointer;
  Dir, Size: Integer;
begin
  FillParams(False);
  Dir := Direction;
  if Dir > 0 then
    Start := TargetEditor.SelStart + TargetEditor.SelLength
  else
  begin
    Start := TargetEditor.SelStart - Searcher.NeedleSize();
    if TargetEditor.SelLength=0 then Inc(Start);
    if Start > TargetEditor.GetFileSize()-1 then Start := TargetEditor.GetFileSize()-1;
  end;

  if Searcher.Find(EntireFile, Start, Dir, Ptr, Size) then
  begin
    TargetEditor.BeginUpdatePanes();
    try
      TargetEditor.MoveCaret(Ptr + IfThen(Dir>0, Size-1, 0), []);
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
end;

procedure TFindReplaceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Searcher.FSearchInProgress then
    FAborted := True;
end;

procedure TFindReplaceForm.FormCreate(Sender: TObject);
begin
  Searcher := TDataSearcher.Create();
  Searcher.OnProgress.Add(ShowProgress);
  Searcher.OnSearchDone.Add(OperationDone);
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

procedure TFindReplaceForm.OperationDone(Sender: TDataSearcher);
// Hide progress
begin
  Gauge1.Progress := 0;
  LblProgress.Caption := '';
  BtnAbort.Enabled := False;
  GBFind.Enabled := True;
  GBReplace.Enabled := True;
end;

procedure TFindReplaceForm.ShowProgress(Sender: TDataSearcher; Pos, Total: TFilePointer);
begin
  Gauge1.Progress := Round(Pos/Total*100);
  LblProgress.Caption := IntToStr(Pos)+' / '+IntToStr(Total);
  BtnAbort.Enabled := True;
  GBFind.Enabled := False;
  GBReplace.Enabled := False;

  FAborted := False;
  Application.ProcessMessages();
  if FAborted then Abort();
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
end;

end.
