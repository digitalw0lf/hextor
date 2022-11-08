{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
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
  TValueInterpretors = class;
  TDataToVariantFunc = reference to function(const Data; Size: Integer): Variant;
  TVariantToDataFunc = reference to procedure(const V: Variant; var Data; Size: Integer);  // Raises EConvertError if failed
  TVariantToDataAutosizeFunc = reference to function(const V: Variant): TBytes;

  TValueInterpretor = class
  private
    FOwner: TValueInterpretors;
    FToVariant: TDataToVariantFunc;
    FFromVariant: TVariantToDataFunc;
    FFromVariantAutosize: TVariantToDataAutosizeFunc;  // Allocates required amount of memory
    function GetName: string;
  public
    Names: TStringList;
    MinSize, MaxSize: Integer;
//    Greedy: Boolean;
    constructor Create(AOwner: TValueInterpretors);
    destructor Destroy(); override;
    property Name: string read GetName;
    function AddNames(const ANames: array of string): TValueInterpretor;
    function SetFromVariantAutosize(AFromVariantAutosize: TVariantToDataAutosizeFunc): TValueInterpretor;
    function ToVariant(const Data; Size: Integer): Variant;
    procedure FromVariant(const V: Variant; var Data; Size: Integer); overload;
    function FromVariant(const V: Variant): TBytes; overload;
  end;

  TValueInterpretors = class (TObjectList<TValueInterpretor>)
  protected
    ByName: TDictionary<string, TValueInterpretor>;
    TextInterpretorsCache: TObjectDictionary<Cardinal, TValueInterpretor>;
    procedure RegisterBuiltinInterpretors();
  public
    function RegisterInterpretor(const ANames: array of string; AToVariant: TDataToVariantFunc;
      AFromVariant: TVariantToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{; AGreedy: Boolean = False}): TValueInterpretor;
    function FindInterpretor(const AName: string): TValueInterpretor;
    function GetTextInterpretor(CodePage: Cardinal): TValueInterpretor;
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
  Arr := S.Split([' '], TStringSplitOptions.ExcludeEmpty);
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
  Result := VarAsType(Single(Data), varSingle);  // Without VarAsType(), Result becomes varDouble
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

function Variant2AnsiAutosize(const V: Variant): TBytes;
var
  tmp: AnsiString;
begin
  tmp := AnsiString(V);
  Result := MakeBytes(tmp[Low(tmp)], Length(tmp) * SizeOf(tmp[Low(tmp)]));
end;

// Ucs-2 (Unicode)

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

function Variant2UnicodeAutosize(const V: Variant): TBytes;
var
  tmp: string;
begin
  tmp := string(V);
  Result := MakeBytes(tmp[Low(tmp)], Length(tmp) * SizeOf(tmp[Low(tmp)]));
end;

// Utf-8

function Utf82Variant(const Data; Size: Integer): Variant;
begin
  Result := Data2String(MakeBytes(Data, Size), TEncoding.UTF8.CodePage);
end;

procedure Variant2Utf8(const V: Variant; var Data; Size: Integer);
var
  tmp: UTF8String;
begin
  tmp := UTF8String(V);
  if Length(tmp)*SizeOf(tmp[Low(tmp)]) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  Move(tmp[Low(tmp)], Data, Size);
end;

function Variant2Utf8Autosize(const V: Variant): TBytes;
var
  tmp: UTF8String;
begin
  tmp := UTF8String(V);
  Result := MakeBytes(tmp[Low(tmp)], Length(tmp));
end;

{ TValueInterpretor }

function TValueInterpretor.AddNames(const ANames: array of string): TValueInterpretor;
var
  i: Integer;
begin
  for i:=0 to Length(ANames)-1 do
    Names.Add(ANames[i]);

  if FOwner <> nil then
    for i:=0 to Length(ANames)-1 do
      FOwner.ByName.AddOrSetValue(UpperCase(ANames[i]), Self);

  Result := Self;
end;

constructor TValueInterpretor.Create(AOwner: TValueInterpretors);
begin
  inherited Create();
  FOwner := AOwner;
  Names := TStringList.Create();
  Names.CaseSensitive := False;
end;

destructor TValueInterpretor.Destroy;
begin
  Names.Free;
  inherited;
end;

procedure TValueInterpretor.FromVariant(const V: Variant; var Data;
  Size: Integer);
begin
  FFromVariant(V, Data, Size);
end;

function TValueInterpretor.FromVariant(const V: Variant): TBytes;
begin
  if Assigned(FFromVariantAutosize) then
    Result := FFromVariantAutosize(V)
  else
  begin
    SetLength(Result, MinSize);
    FFromVariant(V, Result[0], MinSize);
  end;
end;

function TValueInterpretor.GetName: string;
begin
  if Names.Count > 0 then
    Result := Names[0]
  else
    Result := '';
end;

function TValueInterpretor.SetFromVariantAutosize(
  AFromVariantAutosize: TVariantToDataAutosizeFunc): TValueInterpretor;
begin
  FFromVariantAutosize := AFromVariantAutosize;
  Result := Self;
end;

function TValueInterpretor.ToVariant(const Data; Size: Integer): Variant;
begin
  Result := FToVariant(Data, Size);
end;

{ TValueInterpretors }

function TValueInterpretors.RegisterInterpretor(const ANames: array of string; AToVariant: TDataToVariantFunc;
  AFromVariant: TVariantToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{;
  AGreedy: Boolean = False}): TValueInterpretor;
begin
  Result := TValueInterpretor.Create(Self);
  Result.AddNames(ANames);
  Result.MinSize := AMinSize;
  if AMaxSize = SAME_AS_MIN_SIZE then
    Result.MaxSize := AMinSize
  else
    Result.MaxSize := AMaxSize;
//  Result.Greedy := AGreedy;
  Result.FToVariant := AToVariant;
  Result.FFromVariant := AFromVariant;
  Add(Result);
end;

constructor TValueInterpretors.Create;
begin
  inherited Create(True);
  ByName := TDictionary<string, TValueInterpretor>.Create();
  RegisterBuiltinInterpretors();
  TextInterpretorsCache := TObjectDictionary<Cardinal, TValueInterpretor>.Create([doOwnsValues]);
end;

function TValueInterpretors.GetTextInterpretor(
  CodePage: Cardinal): TValueInterpretor;
begin
  if TextInterpretorsCache.TryGetValue(CodePage, Result) then Exit;

  Result := TValueInterpretor.Create(nil);
  Result.AddNames(['text_cp' + IntToStr(CodePage)]);
  Result.MinSize := 1;
  Result.MaxSize := MAX_STR_VALUE_LENGTH;
  Result.FToVariant := function(const Data; Size: Integer): Variant
    begin
      Result := Data2String(MakeBytes(Data, Size), CodePage);
    end;
  Result.FFromVariant := procedure(const V: Variant; var Data; Size: Integer)
    var
      Buf: TBytes;
    begin
      Buf := String2Data(V, CodePage);
      if Length(Buf) <> Size then
        raise EInvalidUserInput.Create('Cannot change string length, only content');
      System.Move(Buf[0], Data, Size);
    end;
  Result.FFromVariantAutosize := function(const V: Variant): TBytes
    begin
      Result := String2Data(V, CodePage);
    end;

  TextInterpretorsCache.AddOrSetValue(CodePage, Result);
end;

destructor TValueInterpretors.Destroy;
begin
  ByName.Free;
  TextInterpretorsCache.Free;
  inherited;
end;

function TValueInterpretors.FindInterpretor(const AName: string): TValueInterpretor;
begin
  if AName.StartsWith('text_cp') then
  begin
    Result := GetTextInterpretor(StrToInt(Copy(AName, 8, MaxInt)));
    Exit;
  end;
  if ByName.TryGetValue(UpperCase(AName), Result) then Exit;
  Result := nil;
end;

procedure TValueInterpretors.RegisterBuiltinInterpretors;
var
  i: Integer;
begin
  RegisterInterpretor(['int8', 'i8'], Int2Variant, Variant2Int, 1);
  RegisterInterpretor(['uint8', 'u8', 'char', 'uchar', 'byte'], UInt2Variant, Variant2UInt, 1);
  RegisterInterpretor(['int16', 'i16', 'short'], Int2Variant, Variant2Int, 2);
  RegisterInterpretor(['uint16', 'u16', 'word', 'ushort'], UInt2Variant, Variant2UInt, 2);
  RegisterInterpretor(['int32', 'i32', 'int', 'long'], Int2Variant, Variant2Int, 4);
  RegisterInterpretor(['uint32', 'u32', 'uint', 'dword', 'cardinal', 'ulong'], UInt2Variant, Variant2UInt, 4);
  RegisterInterpretor(['int64', 'i64'], Int2Variant, Variant2Int, 8);
  RegisterInterpretor(['uint64', 'u64', 'qword'], UInt2Variant, Variant2UInt, 8);

  // int8_t etc.
  for i:=0 to Count-1 do
    Items[i].AddNames([Items[i].Name+'_t']);

  RegisterInterpretor(['bin'], Bin2Variant, Variant2Bin, 1, 8);

  RegisterInterpretor(['float', 'single'], Float2Variant, Variant2Float, 4);
  RegisterInterpretor(['double'], Double2Variant, Variant2Double, 8);

  RegisterInterpretor(['ansi'], Ansi2Variant, Variant2Ansi, 1, MAX_STR_VALUE_LENGTH{, True})
    .SetFromVariantAutosize(Variant2AnsiAutosize);
  RegisterInterpretor(['unicode', 'ucs2'], Unicode2Variant, Variant2Unicode, 2, MAX_STR_VALUE_LENGTH{, True})
    .SetFromVariantAutosize(Variant2UnicodeAutosize);
  RegisterInterpretor(['utf8'], Utf82Variant, Variant2Utf8, 1, MAX_STR_VALUE_LENGTH{, True})
    .SetFromVariantAutosize(Variant2Utf8Autosize);
end;

initialization
  ValueInterpretors := TValueInterpretors.Create();
finalization
  FreeAndNil(ValueInterpretors);
end.
