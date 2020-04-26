{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uValueInterpretors;

interface

uses
  System.Classes, System.Types, System.SysUtils, Generics.Collections,
  System.Variants,

  uHextorTypes;

const
  MAX_STR_VALUE_LENGTH = 256;
  INFINITE_SIZE = -1;
  SAME_AS_MIN_SIZE = -2;

type
  TDataToVariantFunc = reference to function(const Data; Size: Integer): Variant;
  TVariantToDataFunc = reference to procedure(const V: Variant; var Data; Size: Integer);  // Raises EConvertError if failed

  TValueInterpretor = class
  private
    function GetName: string;
  public
    Names: TStringList;
    MinSize, MaxSize: Integer;
//    Greedy: Boolean;
    ToVariant: TDataToVariantFunc;
    FromVariant: TVariantToDataFunc;
    constructor Create();
    destructor Destroy(); override;
    property Name: string read GetName;
    function AddNames(const ANames: array of string): TValueInterpretor;
  end;

  TValueInterpretors = class (TObjectList<TValueInterpretor>)
  protected
    procedure RegisterBuiltinInterpretors();
  public
    function RegisterInterpretor(const ANames: array of string; AToVariant: TDataToVariantFunc;
      AFromVariant: TVariantToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{; AGreedy: Boolean = False}): TValueInterpretor;
    function FindInterpretor(const AName: string): TValueInterpretor;
    constructor Create();
    destructor Destroy(); override;
  end;

function IntToBin(X: Int64; Digits: Integer): string;
function BinToInt(const S: string): Int64;
function IntToOct(X: UInt64): string;

var
  ValueInterpretors: TValueInterpretors = nil;

implementation

function IntToBin(X: Int64; Digits: Integer): string;
// int to '110110101'
var
  i: Integer;
begin
  SetLength(Result, Digits);
  for i:=High(Result) downto Low(Result) do
  begin
    if (X and 1) <> 0 then
      Result[i] := '1'
    else
      Result[i] := '0';
    X := X shr 1;
  end;
end;

function BinToInt(const S: string): Int64;
// '110110101' to int
var
  i: Integer;
begin
  Result := 0;
  for i:=Low(S) to High(S) do
  begin
    Result := Result shl 1;
    if S[i] <> '0' then
      Result := Result or 1;
  end;
end;

function IntToOct(X: UInt64): string;
// Int to octal (without prefixes)
var
  d: Integer;
begin
  if X = 0 then Exit('0');
  Result := '';
  while X > 0 do
  begin
    d := X mod 8;
    Result := Char(Ord('0')+d) + Result;
    X := X div 8;
  end;
end;

// bin

function Bin2Variant(const Data; Size: Integer): Variant;
var
  i: Integer;
begin
  Result := '';
  for i:=0 to Size-1 do
  begin
    Result := Result + IntToBin(TByteArray(Data)[i], 8);
    if i < Size-1 then
      Result := Result + ' ';
  end;
end;

procedure Variant2Bin(const V: Variant; var Data; Size: Integer);
var
  i: Integer;
  S{, s1}: string;
//  P: PChar;
  Arr: TArray<string>;
begin
  S := V;
  if S = '' then raise EConvertError.Create('Empty string');
//  P := @S[Low(S)];
//  i := 0;
//  while P^ <> #0 do
//  begin
//    if i >= Size then raise EConvertError.Create('Too many values');
//    s1 := GetNextWord(P);
//    TByteArray(Data)[i] := BinToInt(s1);
//    Inc(i);
//  end;
  Arr := S.Split([' '], ExcludeEmpty);
  if Length(Arr) <> Size then raise EConvertError.CreateFmt('Expected %d values', [Size]);
  for i:=0 to Length(Arr)-1 do
    TByteArray(Data)[i] := BinToInt(Arr[i]);
end;

// intX

function Int2Variant(const Data; Size: Integer): Variant;
var
  x: Int64;
begin
  if Size > 8 then Size := 8;
  if (PByteArray(@Data)^[Size-1] and $80)<>0 then
    x := -1  // Expand sign bit
  else
    x := 0;
  Move(Data, x, Size);
//  Result := x;
  case Size of
    1: Result := ShortInt(x);
    2: Result := SmallInt(x);
    3, 4: Result := Integer(x);
    else Result := Int64(x);
  end;
end;

procedure Variant2Int(const V: Variant; var Data; Size: Integer);
var
  x: Int64;
begin
  if Size > 8 then Size := 8;
  x := V;
  Move(x, Data, Size);
end;

// uintX

function UInt2Variant(const Data; Size: Integer): Variant;
var
  x: UInt64;
begin
  if Size > 8 then Size := 8;
  x := 0;
  Move(Data, x, Size);
//  Result := x;
  case Size of
    1: Result := Byte(x);
    2: Result := Word(x);
    3, 4: Result := Cardinal(x);
    else Result := Int64(x);  // Troubles with COM/TScriptControl and UInt64
  end;
end;

procedure Variant2UInt(const V: Variant; var Data; Size: Integer);
var
  x: UInt64;
begin
  if Size > 8 then Size := 8;
  x := V;
  Move(x, Data, Size);
end;

// float

function Float2Variant(const Data; Size: Integer): Variant;
begin
  Result := Single(Data);
end;

procedure Variant2Float(const V: Variant; var Data; Size: Integer);
begin
  if VarIsStr(V) then
    Single(Data) := S2R(V)  // Handle both . and , as decimal separator
  else
    Single(Data) := V;
end;

// double

function Double2Variant(const Data; Size: Integer): Variant;
begin
  Result := Double(Data);
end;

procedure Variant2Double(const V: Variant; var Data; Size: Integer);
begin
  if VarIsStr(V) then
    Double(Data) := S2R(V)  // Handle both . and , as decimal separator
  else
    Double(Data) := V;
end;

// Ansi

function Ansi2Variant(const Data; Size: Integer): Variant;
begin
  Result := Data2String(MakeBytes(Data, Size), TEncoding.ANSI.CodePage); //MakeStr(Data, Size);
end;

procedure Variant2Ansi(const V: Variant; var Data; Size: Integer);
var
  tmp: AnsiString;
begin
  if Length(AnsiString(V)) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  tmp := AnsiString(V);
  Move(tmp[Low(tmp)], Data, Size);
end;

// Unicode

function Unicode2Variant(const Data; Size: Integer): Variant;
var
  S: string;
begin
  if (Size mod SizeOf(Char))<>0 then
    raise EConvertError.Create('Data size must be multiple of 2');
  SetString(S, PChar(@Data), Size div SizeOf(Char));
  Result := S;
end;

procedure Variant2Unicode(const V: Variant; var Data; Size: Integer);
var
  tmp: string;
begin
  if Length(string(V))*SizeOf(Char) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  tmp := string(V);
  Move(tmp[Low(tmp)], Data, Size);
end;


{ TValueInterpretor }

function TValueInterpretor.AddNames(const ANames: array of string): TValueInterpretor;
var
  i: Integer;
begin
  for i:=0 to Length(ANames)-1 do
    Names.Add(ANames[i]);
  Result := Self;
end;

constructor TValueInterpretor.Create;
begin
  Names := TStringList.Create();
end;

destructor TValueInterpretor.Destroy;
begin
  Names.Free;
  inherited;
end;

function TValueInterpretor.GetName: string;
begin
  if Names.Count > 0 then
    Result := Names[0]
  else
    Result := '';
end;

{ TValueInterpretors }

function TValueInterpretors.RegisterInterpretor(const ANames: array of string; AToVariant: TDataToVariantFunc;
  AFromVariant: TVariantToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{;
  AGreedy: Boolean = False}): TValueInterpretor;
begin
  Result := TValueInterpretor.Create();
  Result.AddNames(ANames);
  Result.MinSize := AMinSize;
  if AMaxSize = SAME_AS_MIN_SIZE then
    Result.MaxSize := AMinSize
  else
    Result.MaxSize := AMaxSize;
//  Result.Greedy := AGreedy;
  Result.ToVariant := AToVariant;
  Result.FromVariant := AFromVariant;
  Add(Result);
end;

constructor TValueInterpretors.Create;
begin
  inherited Create(True);
  RegisterBuiltinInterpretors();
end;

destructor TValueInterpretors.Destroy;
begin

  inherited;
end;

function TValueInterpretors.FindInterpretor(const AName: string): TValueInterpretor;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if Items[i].Names.IndexOf(AName) >= 0 then Exit(Items[i]);
  Result := nil;
end;

procedure TValueInterpretors.RegisterBuiltinInterpretors;
var
  i: Integer;
begin
  RegisterInterpretor(['int8'], Int2Variant, Variant2Int, 1);
  RegisterInterpretor(['uint8', 'char', 'byte'], UInt2Variant, Variant2UInt, 1);
  RegisterInterpretor(['int16'], Int2Variant, Variant2Int, 2);
  RegisterInterpretor(['uint16', 'word'], UInt2Variant, Variant2UInt, 2);
  RegisterInterpretor(['int32', 'int'], Int2Variant, Variant2Int, 4);
  RegisterInterpretor(['uint32', 'uint', 'dword', 'cardinal'], UInt2Variant, Variant2UInt, 4);
  RegisterInterpretor(['int64'], Int2Variant, Variant2Int, 8);
  RegisterInterpretor(['uint64', 'qword'], UInt2Variant, Variant2UInt, 8);

  // int8_t etc.
  for i:=0 to Count-1 do
    Items[i].AddNames([Items[i].Name+'_t']);

  RegisterInterpretor(['bin'], Bin2Variant, Variant2Bin, 1, 8);

  RegisterInterpretor(['float', 'single'], Float2Variant, Variant2Float, 4);
  RegisterInterpretor(['double'], Double2Variant, Variant2Double, 8);

  RegisterInterpretor(['ansi'], Ansi2Variant, Variant2Ansi, 1, MAX_STR_VALUE_LENGTH{, True});
  RegisterInterpretor(['unicode'], Unicode2Variant, Variant2Unicode, 2, MAX_STR_VALUE_LENGTH{, True});
end;

initialization
  ValueInterpretors := TValueInterpretors.Create();
finalization
  FreeAndNil(ValueInterpretors);
end.
