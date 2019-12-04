unit uValueInterpretors;

interface

uses
  System.Classes, System.Types, System.SysUtils, Generics.Collections,

  uUtil;

const
  MAX_STR_VALUE_LENGTH = 256;
  INFINITE_SIZE = -1;
  SAME_AS_MIN_SIZE = -2;

type
  TDataToStrFunc = reference to function(const Data; Size: Integer): string;
  TStrToDataFunc = reference to procedure(const S: string; var Data; Size: Integer);  // Raises EConvertError if failed
  TDataToIntFunc = reference to function(const Data; Size: Integer): Int64;

  TValueInterpretor = class
  private
    function GetName: string;
  public
    //Name: string;
    Names: TStringList;
    MinSize, MaxSize: Integer;
//    Greedy: Boolean;
    ToString: TDataToStrFunc;
    FromString: TStrToDataFunc;
    ToInt: TDataToIntFunc;
    constructor Create();
    destructor Destroy(); override;
    property Name: string read GetName;
    function AddNames(const ANames: array of string): TValueInterpretor;
    function Set_ToInt(AToInt: TDataToIntFunc): TValueInterpretor;
  end;

  TValueInterpretors = class (TObjectList<TValueInterpretor>)
  protected
    procedure RegisterBuiltinInterpretors();
  public
    function RegisterInterpretor(const AName: string; AToString: TDataToStrFunc;
      AFromString: TStrToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{; AGreedy: Boolean = False}): TValueInterpretor;
    function FindInterpretor(const AName: string): TValueInterpretor;
    constructor Create();
    destructor Destroy(); override;
  end;

var
  ValueInterpretors: TValueInterpretors = nil;

implementation

function IntToBin(X: Integer; Digits: Integer): string;
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

function BinToInt(const S: string): Integer;
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

function Data2Int(const Data; Size: Integer): Int64;
// Default converter of data to integer value
begin
  if Size > 8 then Size := 8;
  if (PByteArray(@Data)^[Size-1] and $80)<>0 then
    Result := -1  // Expand sign bit
  else
    Result := 0;
  Move(Data, Result, Size);
end;

// bin

function Bin2Str(const Data; Size: Integer): string;
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

procedure Str2Bin(const S: string; var Data; Size: Integer);
var
  i: Integer;
  s1: string;
  P: PChar;
begin
  if S = '' then raise EConvertError.Create('Empty string');
  P := @S[Low(S)];
  i := 0;
  while P^ <> #0 do
  begin
    if i >= Size then raise EConvertError.Create('Too many values');
    s1 := GetNextWord(P);
    TByteArray(Data)[i] := BinToInt(s1);
    Inc(i);
  end;
end;

// intX

function Int2Str(const Data; Size: Integer): string;
var
  x: Int64;
begin
  if Size > 8 then Size := 8;
  if (PByteArray(@Data)^[Size-1] and $80)<>0 then
    x := -1  // Expand sign bit
  else
    x := 0;
  Move(Data, x, Size);
  Result := IntToStr(x);
end;

procedure Str2Int(const S: string; var Data; Size: Integer);
var
  x: Int64;
begin
  if Size > 8 then Size := 8;
  x := StrToInt64(S);
  Move(x, Data, Size);
end;

// uintX

function UInt2Str(const Data; Size: Integer): string;
var
  x: UInt64;
begin
  if Size > 8 then Size := 8;
  x := 0;
  Move(Data, x, Size);
  Result := UIntToStr(x);
end;

procedure Str2UInt(const S: string; var Data; Size: Integer);
var
  x: UInt64;
begin
  if Size > 8 then Size := 8;
  x := StrToUInt64(S);
  Move(x, Data, Size);
end;

// float

function Float2Str(const Data; Size: Integer): string;
begin
  Result := R2S(Single(Data));
end;

procedure Str2Float(const S: string; var Data; Size: Integer);
begin
  Single(Data) := S2R(S);
end;

function Float2Int(const Data; Size: Integer): Int64;
begin
  Result := Trunc(Single(Data));
end;

// double

function Double2Str(const Data; Size: Integer): string;
begin
  Result := R2S(Double(Data));
end;

procedure Str2Double(const S: string; var Data; Size: Integer);
begin
  Double(Data) := S2R(S);
end;

function Double2Int(const Data; Size: Integer): Int64;
begin
  Result := Trunc(Double(Data));
end;

// Ansi

function Ansi2Str(const Data; Size: Integer): string;
begin
  Result := string(MakeStr(Data, Size));
end;

procedure Str2Ansi(const S: string; var Data; Size: Integer);
var
  tmp: AnsiString;
begin
  if Length(S) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  tmp := AnsiString(S);
  Move(tmp[Low(tmp)], Data, Size);
end;

// Unicode

function Unicode2Str(const Data; Size: Integer): string;
begin
  if (Size mod SizeOf(Char))<>0 then
    raise EConvertError.Create('Data size must be multiple of 2');
  SetString(Result, PChar(@Data), Size div SizeOf(Char));
end;

procedure Str2Unicode(const S: string; var Data; Size: Integer);
begin
  if Length(S)*SizeOf(Char) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  Move(S[Low(S)], Data, Size);
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

function TValueInterpretor.Set_ToInt(AToInt: TDataToIntFunc): TValueInterpretor;
begin
  ToInt := AToInt;
  Result := Self;
end;

{ TValueInterpretors }

function TValueInterpretors.RegisterInterpretor(const AName: string; AToString: TDataToStrFunc;
  AFromString: TStrToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE{;
  AGreedy: Boolean = False}): TValueInterpretor;
//var
//  Intr:  TValueInterpretor;
begin
  Result := TValueInterpretor.Create();
  Result.Names.Add(AName);
  Result.MinSize := AMinSize;
  if AMaxSize = SAME_AS_MIN_SIZE then
    Result.MaxSize := AMinSize
  else
    Result.MaxSize := AMaxSize;
//  Result.Greedy := AGreedy;
  Result.ToString := AToString;
  Result.FromString := AFromString;
  Result.ToInt := Data2Int;
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
  RegisterInterpretor('int8', Int2Str, Str2Int, 1);
  RegisterInterpretor('uint8', UInt2Str, Str2UInt, 1).AddNames(['char']);
  RegisterInterpretor('int16', Int2Str, Str2Int, 2);
  RegisterInterpretor('uint16', UInt2Str, Str2UInt, 2);
  RegisterInterpretor('int32', Int2Str, Str2Int, 4).AddNames(['int']);
  RegisterInterpretor('uint32', UInt2Str, Str2UInt, 4);
  RegisterInterpretor('int64', Int2Str, Str2Int, 8);
  RegisterInterpretor('uint64', UInt2Str, Str2UInt, 8);

  // int8_t etc.
  for i:=0 to Count-1 do
    Items[i].AddNames([Items[i].Name+'_t']);

  RegisterInterpretor('bin', Bin2Str, Str2Bin, 1, 8);

  RegisterInterpretor('float', Float2Str, Str2Float, 4);
  RegisterInterpretor('double', Double2Str, Str2Double, 8);

  RegisterInterpretor('ansi', Ansi2Str, Str2Ansi, 1, MAX_STR_VALUE_LENGTH{, True});
  RegisterInterpretor('unicode', Unicode2Str, Str2Unicode, 2, MAX_STR_VALUE_LENGTH{, True});
end;

initialization
  ValueInterpretors := TValueInterpretors.Create();
finalization
  FreeAndNil(ValueInterpretors);
end.
