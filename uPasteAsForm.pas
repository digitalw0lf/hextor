unit uPasteAsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Clipbrd,
  Vcl.ExtCtrls, System.Math,

  uUtil, uValueInterpretors;

type
  TPasteAsForm = class(TForm)
    RBValueArray: TRadioButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    RBText: TRadioButton;
    RBHex: TRadioButton;
    Panel1: TPanel;
    Label1: TLabel;
    RBElemByte: TRadioButton;
    RBElemWord: TRadioButton;
    RBElemDWord: TRadioButton;
    RBElemFloat: TRadioButton;
    RBElemDouble: TRadioButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    LblInputPreview: TLabel;
    LblOutputPreview: TLabel;
    Label4: TLabel;
    CBElemType: TComboBox;
    procedure BtnOkClick(Sender: TObject);
    procedure RBTextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CachedClipbrd: string;
    function TextToData(const Text: string): TBytes;
    procedure UpdatePreview();
  public
    { Public declarations }
    ResultData: TBytes;
    function DataPreviewStr(const Data: PByteArray; DataSize: Integer; MaxLen: Integer): string;
  end;

var
  PasteAsForm: TPasteAsForm;

implementation

{$R *.dfm}

procedure TPasteAsForm.BtnOkClick(Sender: TObject);
begin
  ResultData := TextToData(CachedClipbrd);
  ModalResult := mrOk;
end;

function TPasteAsForm.DataPreviewStr(const Data: PByteArray; DataSize: Integer; MaxLen: Integer): string;
// Returns readable view of passed data (unprintable characters replaced with \xDD)
var
  i: Integer;
begin
  Result := '';
  for i:=0 to DataSize-1 do
  begin
    if Data[i] >= Ord(AnsiChar(' ')) then
      Result := Result + Char(AnsiChar(Data[i]))
    else
      Result := Result + '\x' + IntToHex(Data[i],2);
    if (Length(Result) >= MaxLen) and (i < DataSize-1) then
    begin
      Result := Result + '...';
      Break;
    end;
  end;
end;

procedure TPasteAsForm.FormShow(Sender: TObject);
var
  n, i: Integer;
begin
  // Array elements type selector
  n := CBElemType.ItemIndex;
  CBElemType.Items.Clear();
  for i:=0 to ValueInterpretors.Count-1 do
  begin
    CBElemType.Items.Add(ValueInterpretors[i].Name);
  end;
  if n < 0 then n := CBElemType.Items.IndexOf('uint8');
  CBElemType.ItemIndex := n;

  // Cache clipboard text
  CachedClipbrd := Clipboard.AsText;
  UpdatePreview();
end;

procedure TPasteAsForm.RBTextClick(Sender: TObject);
begin
  UpdatePreview();
end;

function TPasteAsForm.TextToData(const Text: string): TBytes;
// Convert text to data using current settings from window
var
  ms: TMemoryStream;
  P: PAnsiChar;
  AText, S: AnsiString;
  Delim: TCharSet;
  Interp: TValueInterpretor;
  Buf: TBytes;
begin
  Result := nil;

  if RBText.Checked then
  begin
    // TODO: Choose encodings
    Result := Str2Bytes(AnsiString(Text));
  end
  else

  if RBHex.Checked then
  begin
    Result := HexToData(AnsiString(Text));
  end
  else

  if RBValueArray.Checked then
  begin
    AText := AnsiString(Text);
    P := @AText[Low(AText)];
    Delim := [#0..#255] - ['0'..'9', 'a'..'z', 'A'..'Z', '$', '-', '.'];

//    if RBElemByte.Checked then Interp := ValueInterpretors.FindInterpretor('byte')
//    else
//    if RBElemWord.Checked then Interp := ValueInterpretors.FindInterpretor('word')
//    else
//    if RBElemDWord.Checked then Interp := ValueInterpretors.FindInterpretor('dword')
//    else
//    if RBElemFloat.Checked then Interp := ValueInterpretors.FindInterpretor('float')
//    else
//    if RBElemDouble.Checked then Interp := ValueInterpretors.FindInterpretor('double')
//    else
//      Exit;
    Interp := ValueInterpretors.FindInterpretor(CBElemType.Text);
    if Interp = nil then Exit;
    SetLength(Buf, Interp.MinSize);

    ms := TMemoryStream.Create();
    try
      while P^ <> #0 do
      begin
        S := GetNextWord(P, Delim);
        Interp.FromVariant(string(S), Buf[0], Length(Buf));
        ms.Write(Buf, Length(Buf));
      end;

      Result := MakeBytes(ms.Memory^, ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TPasteAsForm.UpdatePreview;
// Show input/output preview with current settings
var
  ASample: string;
  AIn: AnsiString;
  AOut: TBytes;
begin
  ASample := Copy(CachedClipbrd, Low(CachedClipbrd), 1000);

  try
    AIn := AnsiString(ASample);
    LblInputPreview.Caption := DataPreviewStr(@AIn[Low(AIn)], Length(AIn), 50);
  except
    on E: Exception do
      LblInputPreview.Caption := E.Message;
  end;

  try
    AOut := TextToData(ASample);
    LblOutputPreview.Caption := DataPreviewStr(@AOut[0], Length(AOut), 50);
  except
    on E: Exception do
      LblOutputPreview.Caption := E.Message;
  end;
end;

end.
