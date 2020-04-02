unit uHextorTypes;

interface

uses
  SysUtils, Winapi.Windows, Generics.Collections, Vcl.Graphics, System.Math,
  System.SysConst;

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

  CharsInvalidInFileName = '/\:*?"<>|';

type
  // Address inside file/data source
  TFilePointer = Int64;

  TFileRange = record
  private
    function GetSize(): TFilePointer;
    procedure SetSize(Value: TFilePointer);
  public
    Start, AEnd: TFilePointer;
    property Size: TFilePointer read GetSize write SetSize;
    function Intersects(const BRange: TFileRange): Boolean; overload;
    function Intersects(BStart, BEnd: TFilePointer): Boolean; overload;
    class operator Equal(const A, B: TFileRange): Boolean; inline;
    class operator NotEqual(const A, B: TFileRange): Boolean; inline;
    constructor Create(BStart, BEnd: TFilePointer);
  end;

  EInvalidUserInput = class (Exception);
  ENoActiveEditor = class (EAbort);

  TColorArray = array of TColor;

  IHextorToolFrame = interface
    ['{4AB18488-6B7D-4A9B-9892-EC91DDF81745}']
    procedure OnShown();
  end;

function Data2Hex(Data: PByteArray; Size: Integer; InsertSpaces: Boolean = False): string; overload;
function Data2Hex(const Data: TBytes; InsertSpaces: Boolean = False): string; overload;
function Hex2Data(const Text: string): TBytes;
//function Data2String(Data: PByteArray; Size: Integer; CodePage: Integer = 0): string; overload;
function Data2String(const Data: TBytes; CodePage: Integer = 0): string; overload;
function String2Data(const Text: string; CodePage: Integer = 0): TBytes; overload;

function MakeValidFileName(const S: string): string;
function RemUnprintable(const s:UnicodeString; NewChar: WideChar='.'):UnicodeString;
function DivRoundUp(A, B: Int64): Int64; inline;
function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
function DataEqual(const Data1, Data2: TBytes): Boolean;
function MakeBytes(const Buf; BufSize:integer):tBytes; overload;
function MakeZeroBytes(Size: NativeInt): TBytes;
procedure InvertByteOrder(var Buf; BufSize:Integer);
function FillRangeInColorArray(var Colors: TColorArray; BaseAddr: TFilePointer; RangeStart, RangeEnd: TFilePointer; Color: TColor): Boolean;

function StrToInt64Relative(S: string; OldValue: TFilePointer): TFilePointer;
function TryS2R(s:UnicodeString; var Value:Double):Boolean;
function S2R(s:UnicodeString): Double;
function S2RDef(s:UnicodeString; const Default: Double = 0): Double;
function R2S(X: Double; Digits:integer = 10): string;
function R2Sf(X: Double; Digits:integer): string;
function FileSize2Str(s:Int64; FracDigits:integer=1):UnicodeString;
function Str2FileSize(const s:UnicodeString):Int64;

function GetAppBuildTime(Instance:THandle = 0):TDateTime;

const
  EntireFile: TFileRange = (Start: 0; AEnd: -1);
  NoRange: TFileRange = (Start: -1; AEnd: -1);

implementation

function Data2Hex(Data: PByteArray; Size: Integer; InsertSpaces: Boolean = False): string; overload;
const
  Convert: array[0..15] of Char = '0123456789ABCDEF';
var
  i: Integer;
  P: PChar;
begin
  if InsertSpaces then
    SetLength(Result, Size * 3)
  else
    SetLength(Result, Size * 2);
  P := @Result[Low(Result)];

  for i:=0 to Size-1 do
  begin
    P^ := Convert[Data[i] shr 4];
    Inc(P);
    P^ := Convert[Data[i] and $F];
    Inc(P);
    if InsertSpaces then
    begin
      P^ := ' ';
      Inc(P);
    end;
  end;
end;

function Data2Hex(const Data: TBytes; InsertSpaces: Boolean = False): string; overload;
begin
  Result := Data2Hex(@Data[0], Length(Data), InsertSpaces);
end;

const
  H2BValidSet = ['0'..'9','A'..'F','a'..'f'];
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

function Hex2Data(const Text: string): TBytes;
var
  i, j, k: Integer;
begin
  j:=-1; k := -1;
  SetLength(Result, Length(Text) div 2);
  for i:=Low(Text) to High(Text) do
  begin
    if not CharInSet(Text[i], H2BValidSet) then Continue;  // Skip invalid chars
    Inc(j);  // Index among valid source chars
    k := j div 2;  // Index of result byte
    if k >= Length(Result) then Break;  // Odd char count
    if (j mod 2) = 0 then
      Result[k] := H2BConvert[Text[i]] shl 4
    else
      Result[k] := Result[k] + H2BConvert[Text[i]];
  end;
  if k+1 < Length(Result) then
    SetLength(Result, k+1);
end;

threadvar
  EncodingsCache: TObjectDictionary<Integer, TEncoding>;
  // TODO: Prevent reported "Memory leak" for threads other then main

function GetCachedEncoding(CodePage: Integer): TEncoding;
// Returns TEncoding object with specified CodePage.
// Creates only one instance for every CodePage value
begin
  if EncodingsCache = nil then
    EncodingsCache := TObjectDictionary<Integer, TEncoding>.Create([doOwnsValues]);
  if EncodingsCache.TryGetValue(CodePage, Result) then Exit;
  Result := TEncoding.GetEncoding(CodePage);
  EncodingsCache.AddOrSetValue(CodePage, Result);
end;

//function Data2String(Data: PByteArray; Size: Integer; CodePage: Integer = 0): string; overload;
//// Convert data in specified encoding to WideString
////var
////  Buf: RawByteString;
//begin
////  SetLength(Buf, Size);
////  Move(Data[0], Buf[Low(Buf)], Size);  // TODO: how to convert without extra move?
////  SetCodePage(Buf, CodePage, False);
////  Result := string(Buf);  // Conversion
//end;

function Data2String(const Data: TBytes; CodePage: Integer = 0): string; overload;
// Convert data in specified encoding to WideString
begin
//  Result := Data2String(@Data[0], Length(Data), CodePage);
  Result := GetCachedEncoding(CodePage).GetString(Data);
end;

function String2Data(const Text: string; CodePage: Integer = 0): TBytes; overload;
//var
//  Buf: RawByteString;
begin
//  // TODO: Does this works for multi-byte codepages?
//  Buf := RawByteString(Text);
//  SetCodePage(Buf, CodePage, True);
//  SetLength(Result, Length(Buf));
//  Move(Buf[Low(Buf)], Result[0], Length(Buf));
  Result := GetCachedEncoding(CodePage).GetBytes(Text);
end;

function ReplaceAllChars(const Text:string; const OldChars:string; NewChar:Char): string; overload;
// Replace all chars in Text from OldChars with NewChar
var
  i:integer;
begin
  Result := Text;
  for i := Low(Result) to High(Result) do
    if OldChars.IndexOf(Result[i]) >= 0 then Result[i]:=NewChar;
end;

function MakeValidFileName(const S: string): string;
begin
  Result := ReplaceAllChars(S, CharsInvalidInFileName, '_');
end;

function RemUnprintable(const s: string; NewChar: Char='.'): string;
// Replace unprintable characters with dots
var
  i: integer;
begin
  Result := s;
  for i:=Low(Result) to High(Result) do
    if Result[i] < ' ' then Result[i] := NewChar;
end;

function DivRoundUp(A, B: Int64): Int64; inline;
begin
  Result := (A-1) div B + 1;
end;

function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
// Ограничивает X в диапазон [MinX,MaxX]
var
  t: TFilePointer;
begin
  Result:=X;
  if MinX>MaxX then
  begin
    t:=MinX;
    MinX:=MaxX;
    MaxX:=t;
  end;
  if Result<MinX then Result:=MinX;
  if Result>MaxX then Result:=MaxX;
end;

function DataEqual(const Data1, Data2: TBytes): Boolean;
begin
  Result := (Length(Data1) = Length(Data2)) and
            (CompareMem(@Data1[0], @Data2[0], Length(Data1)));
end;

function MakeBytes(const Buf; BufSize:integer):tBytes; overload;
// Create TBytes from given buffer
begin
  SetLength(Result,BufSize);
  Move(Buf,Result[0],BufSize);
end;

function MakeZeroBytes(Size: NativeInt): TBytes;
begin
  SetLength(Result, Size);
  FillChar(Result[0], Size, 0);
end;

procedure InvertByteOrder(var Buf; BufSize:Integer);
// Invert order of bytes in buffer
var
  i:Integer;
  b:Byte;
begin
  for i:=0 to (BufSize div 2)-1 do
  begin
    b:=PByteArray(@Buf)^[i];
    PByteArray(@Buf)^[i]:=PByteArray(@Buf)^[BufSize-i-1];
    PByteArray(@Buf)^[BufSize-i-1]:=b;
  end;
end;

function FillRangeInColorArray(var Colors: TColorArray; BaseAddr: TFilePointer;
  RangeStart, RangeEnd: TFilePointer; Color: TColor): Boolean;
// Fill range in color array with given color (assuming address of Colors[0] is BaseAddr)
var
  i: Integer;
begin
  RangeStart := RangeStart - BaseAddr;
  RangeEnd := RangeEnd - BaseAddr;
  if (RangeStart >= Length(Colors)) or (RangeEnd <= 0) then Exit(False);

  for i:=Max(0, RangeStart) to Min(Length(Colors), RangeEnd)-1 do
    Colors[i] := Color;
  Result := True;
end;

function StrToInt64Relative(S: string; OldValue: TFilePointer): TFilePointer;
// if S includes '-' or '+', it is treated as relative to OldValue
begin
  S := Trim(S);
  Result := StrToInt64(S);
  if CharInSet(S[Low(S)], ['-','+']) then
    Result := OldValue + Result;
end;

function TryS2R(s:UnicodeString; var Value:Double):Boolean;
// Convert string to floating-point number
// Treats both '.' and ',' as decimal separator
var
  i:integer;
begin
  s:=Trim(UpperCase(s));
  if s='NAN' then
  begin
    Value:=NaN;
    Exit(True);
  end;
  if s='INF' then
  begin
    Value:=Infinity;
    Exit(True);
  end;
  if s='-INF' then
  begin
    Value:=NegInfinity;
    Exit(True);
  end;

  for i:=Low(S) to High(s) do
    if s[i]=',' then s[i]:='.';
  Val(s,Value,i);
  Result:=(i=0);
end;

function S2R(s:UnicodeString): Double;
// Convert string to floating-point number
// Treats both '.' and ',' as decimal separator
begin
  if not TryS2R(s, Result) then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

function S2RDef(s:UnicodeString; const Default: Double = 0): Double;
// Convert string to floating-point number
// Treats both '.' and ',' as decimal separator
begin
  if not TryS2R(s, Result) then
    Result := Default;
end;

function R2S(X: Double; Digits:integer = 10): string;
// Convert number to string with no more than "Digits" digits after dot
var
  i, j, LowStr: Integer;
  S: ShortString;
begin
  if Digits < 0 then
  begin
    X := RoundTo(X, -Digits);
    Digits := 0;
  end;
  Str(x:1:Digits, S);
  Result := string(S);
  LowStr:=Low(Result);
  i:=Pos('.', Result);
  if i>=LowStr then  // Trim unsignificant zero's at right
  begin
    j:=High(Result);
    while (j>=LowStr) and (Result[j]='0') do Dec(j);
    if (j>=LowStr) and (Result[j]='.') then Dec(j);
    if j<>High(Result) then SetLength(Result, j+(1-LowStr));
  end;
//  if (i>=Low(Result)) and (i<=High(Result)) and (R2SSystemSep) then
//    Result[i]:=FormatSettings.DecimalSeparator;
end;

function R2Sf(X: Double; Digits:integer): string;
// Convert number to string with exactly "Digits" digits after dot
var
  S: ShortString;
begin
  Str(x:1:Digits, S);
  Result := string(S);
end;

function FileSize2Str(s: TFilePointer; FracDigits:integer=1): string;
// String representation of data size "10 B", "1.2 KB", "23.45 MB"
begin
  if s<1024 then
    Result:=IntToStr(s)+' B'
  else if s<1024*1024 then
    Result:=R2S(s/1024,FracDigits)+' KB'
  else if s<1024*1024*1024 then
    Result:=R2S(s/(1024*1024),FracDigits)+' MB'
  else if s<Int64(1024)*1024*1024*1024 then
    Result:=R2S(s/(1024*1024*1024),FracDigits)+' GB'
  else
    Result:=R2S(s/(1.0*1024*1024*1024*1024),FracDigits)+' TB';
end;

function Str2FileSize(const s: string): TFilePointer;
// Convert a string like "10.5 MB", " 12 kbytes" etc. to number of bytes
var
  i,l:integer;
  Mult:Int64;
  s1:UnicodeString;
begin
  Mult:=1; l:=Length(s)+1;
  for i:=1 to Length(s) do
  begin
    if not CharInSet(s[i],['0'..'9','.',',',' ',#9]) then
    begin
      if CharInSet(s[i],['b','B','б','Б']) then Mult:=1
      else
      if CharInSet(s[i],['k','K','к','К']) then Mult:=1024
      else
      if CharInSet(s[i],['m','M','м','М']) then Mult:=1024*1024
      else
      if CharInSet(s[i],['g','G','г','Г']) then Mult:=1024*1024*1024
      else
      if CharInSet(s[i],['t','T','т','Т']) then Mult:=Int64(1024)*1024*1024*1024
      else raise EConvertError.CreateFmt('"%s" is not a valid size', [s]);
      l:=i;
      break;
    end;
  end;
  s1:=Trim(Copy(s,1,l-1));
  Result:=Round(S2R(s1)*Mult);
end;

function GetAppBuildTime(Instance:THandle = 0):TDateTime;
// Returns application build time from PE file header
var
  Offset: Cardinal;
  FD: LongRec;
  Date, Time: TDateTime;
begin
  Result:=0;
  if Instance=0 then Instance:=HInstance;
  Offset:=PImageNtHeaders(Instance+DWORD(PImageDosHeader(Instance)._lfanew))
           .OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
  if Offset<>0 then begin
    Integer(FD):=PInteger(Offset+Instance+4)^;
    if TryEncodeDate(FD.Hi shr 9+1980,FD.Hi shr 5 and 15,FD.Hi and 31,Date)
      and TryEncodeTime(FD.Lo shr 11,FD.Lo shr 5 and 63,FD.Lo and 31 shl 1,0,Time) then
      Result:=Date+Time;
  end;
end;

{ TFileRange }

constructor TFileRange.Create(BStart, BEnd: TFilePointer);
begin
  Start := BStart;
  AEnd := BEnd;
end;

class operator TFileRange.Equal(const A, B: TFileRange): Boolean;
begin
  Result := (A.Start = B.Start) and (A.AEnd = B.AEnd);
end;

function TFileRange.GetSize: TFilePointer;
begin
  Result := AEnd-Start;
end;

function TFileRange.Intersects(BStart, BEnd: TFilePointer): Boolean;
begin
  Result := (BEnd > Start) and (BStart < AEnd);
end;

class operator TFileRange.NotEqual(const A, B: TFileRange): Boolean;
begin
  Result := (A.Start <> B.Start) or (A.AEnd <> B.AEnd);
end;

function TFileRange.Intersects(const BRange: TFileRange): Boolean;
begin
  Result := (BRange.AEnd > Start) and (BRange.Start < AEnd);
end;

procedure TFileRange.SetSize(Value: TFilePointer);
begin
  AEnd := Start + Value;
end;

initialization

finalization
  FreeAndNil(EncodingsCache);
end.
