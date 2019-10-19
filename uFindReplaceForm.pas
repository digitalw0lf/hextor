unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math,

  uMainForm, uUtil, Vcl.ExtCtrls, Vcl.Samples.Gauges;

type
  TSearchParams = record
    Text: string;
    bHex, bWildcards, bUnicode, bMatchCase: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;

    Needle: TBytes;
  end;

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
  private
    { Private declarations }
    Params: TSearchParams;
    FAborted: Boolean;
    FSearchInProgress: Boolean;
    procedure FillParams(aReplace: Boolean);
    procedure ShowProgress(Pos, Total: TFilePointer);
    procedure OperationDone();
  public
    { Public declarations }
    function ParamsDefined(): Boolean;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean;
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: Integer; Direction: Integer; var Ptr, Size: Integer): Boolean;
    function Find(Range: TFileRange; Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
    function FindNext(Direction: Integer): Boolean;
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
  if Params.bFindInSel then
  begin
    Range.Start := MainForm.SelStart;
    Range.AEnd := MainForm.SelStart+MainForm.SelLength;
  end
  else
    Range := EntireFile;
  Start := Range.Start;
  while Find(Range, Start, 1, Ptr, Size) do
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

function TFindReplaceForm.Find(Range: TFileRange; Start: TFilePointer;
  Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
const
  BlockSize = 1*MByte;
  BlockOverlap = 100*KByte;
var
  Data: TBytes;
  IPtr: Integer;
  Block: TFileRange;
begin
  if FSearchInProgress then Exit;
  if Range.AEnd < 0 then Range.AEnd := MainForm.GetFileSize();

  Ptr := Start;
  // Dynamically load by 1 MB, overlapped by 100 KB if we go backwards
  try
    FSearchInProgress := True;
    repeat
      if Direction > 0 then
      begin
        Block.Start := Ptr;
        Block.AEnd := Ptr + BlockSize
      end
      else
      begin
        Block.Start := Ptr - BlockSize + BlockOverlap;
        Block.AEnd := Ptr + BlockOverlap;
      end;
      if Block.Start < Range.Start then Block.Start := Range.Start;
      if Block.AEnd > Range.AEnd then Block.AEnd := Range.AEnd;

      // Take next data portion
      Data := MainForm.GetEditedData(Block.Start, Block.Size(), False);
      // Search in it
      Result := FindInBlock(@Data[0], Length(Data), Ptr - Block.Start, Direction, IPtr, Size);
      Ptr := Block.Start + IPtr;

      // Found
      if Result then Break;

      // Reached end of range
      if (Direction > 0) and (Block.AEnd = Range.AEnd) then Break;
      if (Direction < 0) and (Block.Start = Range.Start) then Break;

      ShowProgress(Ptr-Range.Start, Range.Size);
    until False;
  finally
    OperationDone();
  end;
end;

function TFindReplaceForm.FindInBlock(const Data: PByte; DataSize: Integer;
  Start: Integer; Direction: Integer; var Ptr, Size: Integer): Boolean;
// Search in given loaded block.
// Ptr is shifted in given direction, even if nothing found,
// until we are sure we have no chance to find
begin
  Ptr := Start;
  while (Ptr>=0) and (Ptr<DataSize) do
  begin
    if Match(@Data[Ptr], DataSize-Ptr, Size) then
      Exit(True);
    Inc(Ptr, Direction);
    if Ptr+Length(Params.Needle) > DataSize then Break;
  end;
  Result := False;
end;

function TFindReplaceForm.FindNext(Direction: Integer): Boolean;
var
  Start, Ptr: TFilePointer;
  Dir, Size: Integer;
begin
  FillParams(False);
  Dir := Direction;
  if Dir > 0 then
    Start := MainForm.SelStart + MainForm.SelLength
  else
  begin
    Start := MainForm.SelStart - Length(Params.Needle);
    if MainForm.SelLength=0 then Inc(Start);
    if Start > MainForm.GetFileSize()-1 then Start := MainForm.GetFileSize()-1;
  end;

  if Find(EntireFile, Start, Dir, Ptr, Size) then
  begin
    MainForm.BeginUpdatePanes();
    try
      MainForm.MoveCaret(Ptr + IfThen(Dir>0, Size-1, 0), []);
      MainForm.SetSelection(Ptr, Ptr + Size - 1);
    finally
      MainForm.EndUpdatePanes();
    end;
    Result := True;
  end
  else
  begin
    MainForm.SetSelection(MainForm.CaretPos, -1);
    Result := False;
  end;
end;

procedure TFindReplaceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FSearchInProgress then
    FAborted := True;
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

function TFindReplaceForm.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
var
  i: Integer;
begin
  if DataSize < Length(Params.Needle) then Exit(False);
  for i:=0 to Length(Params.Needle)-1 do
    if Data[i] <> Params.Needle[i] then Exit(False);
  Result := True;
  Size := Length(Params.Needle);
end;

procedure TFindReplaceForm.OperationDone;
// Hide progress
begin
  Gauge1.Progress := 0;
  LblProgress.Caption := '';
  BtnAbort.Enabled := False;
  GBFind.Enabled := True;
  GBReplace.Enabled := True;
  FSearchInProgress := False;
end;

function TFindReplaceForm.ParamsDefined: Boolean;
begin
  Result := (Params.Needle <> nil);
end;

procedure TFindReplaceForm.ShowProgress(Pos, Total: TFilePointer);
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
  CBFindInSelection.Enabled := (MainForm.SelLength > 0);
end;

end.
