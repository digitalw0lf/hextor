{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uPasteAsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Clipbrd,
  Vcl.ExtCtrls, System.Math, System.NetEncoding,

  uHextorTypes, uValueInterpretors;

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
    RBBase64: TRadioButton;
    RBURLEncode: TRadioButton;
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
    begin
      Result := Result + string(AnsiChar(Data[i]));
    end
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

function GetNextWord(var P: PChar; Delim: TSysCharSet): string;
var
  i:integer;
begin
  if P = nil then Exit('');
  while CharInSet(P^, Delim) do Inc(P);
  Delim := Delim + [#0];
  i:=0;
  while not CharInSet(P[i], Delim) do inc(i);
  SetLength(Result,i);
  Move(P[0], Result[Low(Result)], i*SizeOf(Char));
  P:=@P[i];
  while (P^<>#0) and (CharInSet(P^, Delim)) do inc(P);
end;

function ParseArrayText(const Text: string; const ElemType: string): TBytes;
// Parse text with delimited values to binary buffer
// Values may be larger then one byte (determined by ElemType)
// Any char that cannot be part of a number is treated as delimiter
// Examples:
// "0 10 20 30", ElemType = "byte"
// "0x010a, 0x020b", ElemType = "word"
// "0.1;-2.33", ElemType = "float"
var
  ms: TMemoryStream;
  P: PChar;
  Delim: TSysCharSet;
  S: string;
  Interp: TValueInterpretor;
  Buf: TBytes;
begin
  Result := nil;
  P := @Text[Low(Text)];
  Delim := [#0..#255] - ['0'..'9', 'a'..'z', 'A'..'Z', '$', '-', '.'];

  Interp := ValueInterpretors.FindInterpretor(ElemType);
  if Interp = nil then Exit;
  SetLength(Buf, Interp.MinSize);

  ms := TMemoryStream.Create();
  try
    while P^ <> #0 do
    begin
      S := GetNextWord(P, Delim);
      Interp.FromVariant(S, Buf[0], Length(Buf));
      ms.Write(Buf, Length(Buf));
    end;

    Result := MakeBytes(ms.Memory^, ms.Size);
  finally
    ms.Free;
  end;
end;

function TPasteAsForm.TextToData(const Text: string): TBytes;
// Convert text to data using current settings from window
begin
  Result := nil;

  if RBText.Checked then
  begin
    // TODO: Choose encodings
    Result := String2Data(Text, TEncoding.ANSI.CodePage);
  end
  else

  if RBHex.Checked then
  begin
    Result := Hex2Data(Text);
  end
  else

  if RBBase64.Checked then
  begin
    Result := TNetEncoding.Base64.DecodeStringToBytes(Text);
  end
  else

  if RBURLEncode.Checked then
  begin
    Result := String2Data(TNetEncoding.URL.Decode(Text, []));
  end
  else

  if RBValueArray.Checked then
  begin
    Result := ParseArrayText(Text, CBElemType.Text);
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
