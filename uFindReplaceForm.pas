unit uFindReplaceForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Math,

  uMainForm, uUtil;

type
  TSearchParams = record
    Text: string;
    bHex, bRegEx, bUnicode, bMatchCase, bWholeWords: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;

    Needle: TBytes;
  end;

  TFindReplaceForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    EditFindText: TComboBox;
    CBFindHex: TCheckBox;
    CBFindRegEx: TCheckBox;
    CBMatchCase: TCheckBox;
    CBWholeWords: TCheckBox;
    BtnFindNext: TButton;
    BtnFindPrev: TButton;
    Label2: TLabel;
    EditReplaceText: TComboBox;
    CBReplaceHex: TCheckBox;
    BtnReplaceNext: TButton;
    BtnReplaceAll: TButton;
    BtnFindCount: TButton;
    CBUnicode: TCheckBox;
    procedure BtnFindNextClick(Sender: TObject);
  private
    { Private declarations }
    Params: TSearchParams;
    procedure FillParams(aReplace: Boolean);
  public
    { Public declarations }
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean;
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: TFilePointer; Direction: Integer; var Ptr, Size: Integer): Boolean;
    function Find(Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
  end;

var
  FindReplaceForm: TFindReplaceForm;

implementation

{$R *.dfm}

{ TFindReplaceForm }

procedure TFindReplaceForm.BtnFindNextClick(Sender: TObject);
var
  Start, Ptr: TFilePointer;
  Dir, Size: Integer;
begin
  FillParams(False);
  Dir := (Sender as TButton).Tag;
  if Dir > 0 then
    Start := MainForm.SelStart + MainForm.SelLength
  else
    Start := MainForm.SelStart - 1;

  if Find(Start, Dir, Ptr, Size) then
  begin
    MainForm.BeginUpdatePanes();
    try
      MainForm.CaretPos := Ptr + IfThen(Dir>0, Size-1, 0);
      MainForm.SetSelection(Ptr, Ptr + Size - 1);
    finally
      MainForm.EndUpdatePanes();
    end;
  end
  else
  begin
    MainForm.SetSelection(MainForm.CaretPos, -1);
  end;
end;

procedure TFindReplaceForm.FillParams(aReplace: Boolean);
begin
  Params.Text := EditFindText.Text;
  Params.bHex := CBFindHex.Checked;
  Params.bRegEx := CBFindRegEx.Checked;
  Params.bMatchCase := CBMatchCase.Checked;
  Params.bWholeWords := CBWholeWords.Checked;

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
end;

function TFindReplaceForm.Find(Start: TFilePointer;
  Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
var
  Data: TBytes;
  IPtr: Integer;
begin
  Data := MainForm.GetEditedData(0, MainForm.GetFileSize(), False);
  IPtr := Ptr;
  Result := FindInBlock(@Data[0], Length(Data), Start, Direction, IPtr, Size);
  Ptr := IPtr;
end;

function TFindReplaceForm.FindInBlock(const Data: PByte; DataSize: Integer;
  Start: TFilePointer; Direction: Integer; var Ptr, Size: Integer): Boolean;
begin
  Ptr := Start;
  while (Ptr>=0) and (Ptr+Size<=DataSize) do
  begin
    if Match(@Data[Ptr], DataSize-Ptr, Size) then
      Exit(True);
    Inc(Ptr, Direction);
  end;
  Result := False;
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

end.
