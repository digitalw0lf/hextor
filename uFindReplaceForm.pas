unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math,

  uMainForm, uUtil, Vcl.ExtCtrls;

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
    procedure BtnFindNextClick(Sender: TObject);
    procedure BtnFindCountClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    Params: TSearchParams;
    procedure FillParams(aReplace: Boolean);
  public
    { Public declarations }
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean;
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: TFilePointer; Direction: Integer; var Ptr, Size: Integer): Boolean;
    function Find(Range: TFileRange; Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
    function FindNext(Direction: Integer): Boolean;
  end;

var
  FindReplaceForm: TFindReplaceForm;

implementation

{$R *.dfm}

{ TFindReplaceForm }

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
var
  Data: TBytes;
  IPtr: Integer;
begin
  // TODO: Dynamically load by 1 MB
  if Range.AEnd < 0 then Range.AEnd := MainForm.GetFileSize();
  Data := MainForm.GetEditedData(Range.Start, Range.Size(), False);
  Result := FindInBlock(@Data[0], Length(Data), Start - Range.Start, Direction, IPtr, Size);
  Ptr := Range.Start + IPtr;
end;

function TFindReplaceForm.FindInBlock(const Data: PByte; DataSize: Integer;
  Start: TFilePointer; Direction: Integer; var Ptr, Size: Integer): Boolean;
begin
  Ptr := Start;
  while (Ptr>=0) and (Ptr<=DataSize) do
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

procedure TFindReplaceForm.Timer1Timer(Sender: TObject);
begin
  if not Visible then Exit;
  CBFindInSelection.Enabled := (MainForm.SelLength > 0);
end;

end.
