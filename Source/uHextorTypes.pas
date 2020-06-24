{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHextorTypes;

interface

uses
  Winapi.Windows, Winapi.ShLwApi, SysUtils, System.Classes, Generics.Collections,
  Vcl.Graphics, System.Math, System.SysConst, System.Variants, superobject,

  uCallbackList;

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

  CharsInvalidInFileName = '/\:*?"<>|';

  cDay = 1.0;
  cHour = cDay / 24;
  cMinute = cHour / 60;
  cSecond = cMinute / 60;
  cMillisecond = cSecond / 1000;

type
  // Address inside file/data source
  TFilePointer = Int64;

  TFileRange = record
  private
    function GetSize(): TFilePointer; inline;
    procedure SetSize(Value: TFilePointer); inline;
  public
    Start, AEnd: TFilePointer;
    property Size: TFilePointer read GetSize write SetSize;
    function Intersects(const BRange: TFileRange): Boolean; overload; inline;
    function Intersects(BStart, BEnd: TFilePointer): Boolean; overload; inline;
    function Intersects(Value: TFilePointer): Boolean; overload; inline;
    class operator Equal(const A, B: TFileRange): Boolean; inline;
    class operator NotEqual(const A, B: TFileRange): Boolean; inline;
    constructor Create(BStart, BEnd: TFilePointer);
  end;

  TVariantRange = record
    AStart, AEnd: Variant;
    constructor Create(AStart, AEnd: Variant);
  end;
  TVariantRanges = record
    Ranges: TArray<TVariantRange>;
    function Contains(const Value: Variant): Boolean;
  end;

  TValueDisplayNotation = (nnDefault, nnBin, nnOct, nnDec, nnHex, nnAChar, nnWChar);

  EInvalidUserInput = class (Exception);
  ENoActiveEditor = class (EAbort);

  // Bookmarks, Selected range, Structure field etc. marked on data by some tool
  TTaggedDataRegion = class
    Owner: TObject;
    Data: Pointer;
    Range: TFileRange;
//    ZOrder: Integer;
    TextColor, BgColor, FrameColor: TColor;
    //OnGetHint, OnPopup
    constructor Create(AOwner: TObject; ARange: TFileRange;
      ATextColor, ABgColor, AFrameColor: TColor);
    function Accepted(): Boolean;
  end;
  TTaggedDataRegionList = class (TObjectList<TTaggedDataRegion>)
  public
    AcceptRange: TFileRange;
    function AddRegion(Owner: TObject; RangeStart, RangeEnd: TFilePointer; TextColor, BgColor, FrameColor: TColor): TTaggedDataRegion;
    constructor Create(); overload;
    constructor Create(const AAcceptRange: TFileRange); overload;
  end;


  IHextorToolFrame = interface
    ['{4AB18488-6B7D-4A9B-9892-EC91DDF81745}']
    procedure OnShown();
  end;

  // Convert structures<==>json  (helper wrapper)
  tJsonRtti = class
    class function ObjectToStr<T>(const obj: T; const ANotWriteEmptyField: boolean = false): string;
    class function StrToObject<T>(const S: string): T;
  end;

  // Manages progress reported by tasks and sub-tasks and passes it to
  // main GUI for display.
  // Any worker can create sub-tasks and define their portions of parent task's work.
  // Sub-tasks will report their own progress. Progress tracker calculates
  // total progress at any time moment.
  TProgressTracker = class
  public type
    TTask = class
      Worker: TObject;
      Portion: Double;  // What portion of parent task this task occupies
      TotalWorkFrom, TotalWorkTo: Double; // What portion of overall work this task occupies
      Progress: Double; // Current (relative) progress of this task
    end;
  protected
    TaskStack: TObjectStack<TTask>;
    FTotalProgress: Double;
    FLastText: string;
    LastDisplay: Cardinal;
    FDisplayInterval: Cardinal;
    function GetCurrentTask(): TTask;
  public
    OnTaskStart: TCallbackListP2<{Sender: }TProgressTracker, {Task: }TTask>;  // Called with Task already in stack
    OnTaskEnd: TCallbackListP2<{Sender: }TProgressTracker, {Task: }TTask>;    // Called with Task still in stack
    OnDisplay: TCallbackListP3<{Sender: }TProgressTracker, {TotalProgress: }Double, {Text: }string>;
    constructor Create();
    destructor Destroy(); override;
    property DisplayInterval: Cardinal read FDisplayInterval write FDisplayInterval;
    procedure TaskStart(Worker: TObject; PortionOfParent: Double = 1.0);
    procedure TaskEnd();
    procedure Show({Worker: TObject;} Pos, Total: TFilePointer; Text: string = '-'); overload;
    procedure Show({Worker: TObject;} AProgress: Double; Text: string = '-'); overload;
    function CurrentTaskLevel(): Integer;
    property CurrentTask: TTask read GetCurrentTask;
    property TotalProgress: Double read FTotalProgress;
  end;

function Data2Hex(Data: PByteArray; Size: Integer; InsertSpaces: Boolean = False): string; overload;
function Data2Hex(const Data: TBytes; InsertSpaces: Boolean = False): string; overload;
function Hex2Data(const Text: string): TBytes;
//function Data2String(Data: PByteArray; Size: Integer; CodePage: Integer = 0): string; overload;
function Data2String(const Data: TBytes; CodePage: Integer = 0): string; overload;
function String2Data(const Text: string; CodePage: Integer = 0): TBytes; overload;

function MakeValidFileName(const S: string): string;
function CanonicalizePath(const Path: string): string;
function PathIsInside(const InnerPath, OuterPath: string): Boolean;
function RemUnprintable(const s:UnicodeString; NewChar: WideChar='.'):UnicodeString;
function DivRoundUp(A, B: Int64): Int64; inline;
function NextAlignBoundary(BufStart, BufPos, Align: TFilePointer): TFilePointer;
function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
function AdjustPositionInData(var Pos: TFilePointer; Addr, OldSize, NewSize: TFilePointer): Boolean; overload;
function AdjustPositionInData(var Range: TFileRange; Addr, OldSize, NewSize: TFilePointer): Boolean; overload;
function DataEqual(const Data1, Data2: TBytes): Boolean;
function MakeBytes(const Buf; BufSize:integer):tBytes; overload;
function MakeZeroBytes(Size: NativeInt): TBytes;
procedure InvertByteOrder(var Buf; BufSize:Integer);
function VariantRange(const AStart, AEnd: Variant): TVariantRange; overload;
function VariantRange(const AValue: Variant): TVariantRange; overload;

function StrToInt64Relative(S: string; OldValue: TFilePointer): TFilePointer;
function TryS2R(s:UnicodeString; var Value:Double):Boolean;
function S2R(s:UnicodeString): Double;
function S2RDef(s:UnicodeString; const Default: Double = 0): Double;
function R2S(X: Double; Digits:integer = 10): string;
function R2Sf(X: Double; Digits:integer): string;
function FileSize2Str(s:Int64; FracDigits:integer=1):UnicodeString;
function Str2FileSize(const s:UnicodeString):Int64;

function GetAppBuildTime(Instance:THandle = 0):TDateTime;
procedure WriteLog(const LogSrc, Text: string); overload;
procedure WriteLog(const Text: string); overload;

const
  EntireFile: TFileRange = (Start: 0; AEnd: -1);
  NoRange: TFileRange = (Start: -1; AEnd: -1);

var
  Progress: TProgressTracker = nil;  // Grobal progress tracker instance for all operations

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

function CanonicalizePath(const Path: string): string;
// Simplifies a path by removing navigation elements
// such as "." and ".." to produce a direct, well-formed path
var
  S: string;
begin
  SetLength(S, MAX_PATH);
  PathCanonicalize(PChar(S), PChar(Path));
  Result := PChar(S);
end;

function PathIsInside(const InnerPath, OuterPath: string): Boolean;
// Check if InnerPath describes a file/directory inside OuterPath
// (does not checks file/dir existance)
var
  Inner, Outer: string;
begin
  Inner := IncludeTrailingPathDelimiter(CanonicalizePath(InnerPath));
  Outer := IncludeTrailingPathDelimiter(CanonicalizePath(OuterPath));
  Result := SameFileName(Outer, Copy(Inner, Low(Inner), Length(Outer)));
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

function NextAlignBoundary(BufStart, BufPos, Align: TFilePointer): TFilePointer;
// Given buffer start address, current position and alignment block size,
// returns next alignment boundary after current position (may be equal to current position)
begin
  Result := BufStart + ((BufPos - BufStart - 1) div Align + 1) * Align;
end;

function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
// Bound X by range [MinX,MaxX]
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

function AdjustPositionInData(var Pos: TFilePointer; Addr, OldSize, NewSize: TFilePointer): Boolean; overload;
// Adjust position Pos according to operation that changed block of data at position Addr
// from size OldSize to NewSize.
var
  OpSize: TFilePointer;
begin
  if NewSize = OldSize then Exit(False);
  if Pos < Addr then Exit(False);
  OpSize := NewSize - OldSize;
  Pos := Max(Pos + OpSize, Addr);  // Works for both deletion and insertion
  Result := True;
end;

function AdjustPositionInData(var Range: TFileRange; Addr, OldSize, NewSize: TFilePointer): Boolean; overload;
// Adjust start and end of Range according to operation that changed block of data at position Addr
// from size OldSize to NewSize.
begin
  if NewSize = OldSize then Exit(False);
  Result := AdjustPositionInData(Range.Start, Addr, OldSize, NewSize);
  Result := AdjustPositionInData(Range.AEnd, Addr, OldSize, NewSize) or Result;
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

function VariantRange(const AStart, AEnd: Variant): TVariantRange; overload;
begin
  Result.AStart := AStart;
  Result.AEnd := AEnd;
end;

function VariantRange(const AValue: Variant): TVariantRange; overload;
begin
  Result.AStart := AValue;
  Result.AEnd := AValue;
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
      if CharInSet(s[i],['b','B','á','Á']) then Mult:=1
      else
      if CharInSet(s[i],['k','K','ê','Ê']) then Mult:=1024
      else
      if CharInSet(s[i],['m','M','ì','Ì']) then Mult:=1024*1024
      else
      if CharInSet(s[i],['g','G','ã','Ã']) then Mult:=1024*1024*1024
      else
      if CharInSet(s[i],['t','T','ò','Ò']) then Mult:=Int64(1024)*1024*1024*1024
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

procedure WriteLog(const LogSrc, Text: string); overload;
var
  fn, ws: string;
  S: AnsiString;
  Now_: TDateTime;
  fs: TFileStream;
begin
  Now_ := Now();

  DateTimeToString(fn, 'yymmdd', Now_);
  fn := ExtractFilePath(ParamStr(0)) + 'Log\' + fn + '\' + LogSrc + '.log';

  DateTimeToString(ws, 'hh:nn:ss.zzz', Now_);
  S := AnsiString(ws) + ' | ' + AnsiString(Text) + sLineBreak;

  ForceDirectories(ExtractFilePath(fn));
  if FileExists(fn) then
    fs := TFileStream.Create(fn, fmOpenReadWrite)
  else
    fs := TFileStream.Create(fn, fmCreate);
  try
    fs.Seek(0, soEnd);
    fs.WriteBuffer(S[Low(S)], Length(S) * SizeOf(S[Low(S)]));
  finally
    fs.Free;
  end;
end;

procedure WriteLog(const Text: string); overload;
begin
  WriteLog('Log', Text);
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

function TFileRange.Intersects(Value: TFilePointer): Boolean;
begin
  Result := (Value >= Start) and (Value < AEnd);
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

{ TVariantRange }

constructor TVariantRange.Create(AStart, AEnd: Variant);
begin
  Self.AStart := AStart;
  Self.AEnd := AEnd;
end;

{ TVariantRanges }

function TypesCompatible(const v1, v2: Variant): Boolean;
begin
  Result := (VarIsNumeric(v1) and VarIsNumeric(v2)) or
            (VarIsStr(v1) and VarIsStr(v2));
end;

function TVariantRanges.Contains(const Value: Variant): Boolean;
var
  i: Integer;
begin
  for i:=0 to Length(Ranges)-1 do
    if (TypesCompatible(Value, Ranges[i].AStart)) and
       (TypesCompatible(Value, Ranges[i].AEnd)) and
       (Value >= Ranges[i].AStart) and (Value <= Ranges[i].AEnd) then
      Exit(True);
  Result := False;
end;

{ tJsonRtti }

class function tJsonRtti.ObjectToStr<T>(const obj: T; const ANotWriteEmptyField: boolean = false): string;
var
  ctx: TSuperRttiContext;
begin
  ctx := TSuperRttiContext.Create;
  ctx.NotWriteEmptyField := ANotWriteEmptyField;
  Result := ctx.AsJson<T>(obj).AsJson(true, False);
  ctx.Free;
end;

class function tJsonRtti.StrToObject<T>(const S: string): T;
var
  ctx: TSuperRttiContext;
begin
  ctx := TSuperRttiContext.Create;
  Result := ctx.AsType<T>(SO(S));
  ctx.Free;
end;

{ TTaggedDataRegion }

function TTaggedDataRegion.Accepted: Boolean;
// Dirty hack to be able to write like this:
//
// with Regions.Add(...) do
// if Accepted() then
// begin
//   OnClick := TagClick;
// end;
begin
  Result := Assigned(Self);
end;

constructor TTaggedDataRegion.Create(AOwner: TObject; ARange: TFileRange; ATextColor, ABgColor,
  AFrameColor: TColor);
begin
  inherited Create();
  Owner := AOwner;
  Range := ARange;
  TextColor := ATextColor;
  BgColor := ABgColor;
  FrameColor := AFrameColor;
end;

{ TTaggedDataRegionList }

function TTaggedDataRegionList.AddRegion(Owner: TObject; RangeStart, RangeEnd: TFilePointer;
  TextColor, BgColor, FrameColor: TColor): TTaggedDataRegion;
begin
  if (AcceptRange = EntireFile) or (AcceptRange.Intersects(RangeStart, RangeEnd)) then
  begin
    Result := TTaggedDataRegion.Create(Owner, TFileRange.Create(RangeStart, RangeEnd), TextColor, BgColor, FrameColor);
    Add(Result);
  end
  else
    Result := nil;
end;

constructor TTaggedDataRegionList.Create(const AAcceptRange: TFileRange);
begin
  inherited Create(True);
  AcceptRange := AAcceptRange;
end;

constructor TTaggedDataRegionList.Create;
begin
  Create(EntireFile);
end;

{ TProgressTracker }

constructor TProgressTracker.Create;
begin
  inherited;
  TaskStack := TObjectStack<TTask>.Create();
  FDisplayInterval := 100;
end;

function TProgressTracker.GetCurrentTask: TTask;
begin
  if TaskStack.Count = 0 then
    Result := nil
  else
    Result := TaskStack.Peek;
end;

function TProgressTracker.CurrentTaskLevel: Integer;
begin
  Result := TaskStack.Count;
end;

destructor TProgressTracker.Destroy;
begin
  TaskStack.Free;
  inherited;
end;

procedure TProgressTracker.Show({Worker: TObject;} AProgress: Double; Text: string);
// Task calls this to report it's own progress.
// '-' => do not change text.
// Must be surrounded by TaskStart() / TaskEnd()
var
  ATotalProgress: Double;
  CurTask: TTask;
begin
  CurTask := CurrentTask;
  if CurTask = nil then Exit;

  if Text <> '-' then
    FLastText := Text;

  with CurTask do
  begin
    Progress := AProgress;
    ATotalProgress := TotalWorkFrom + (TotalWorkTo - TotalWorkFrom) * Progress;
  end;
  FTotalProgress := ATotalProgress;

  // Pass to GUI once in 100 ms.
  // If overall task takes less then 100 ms, progress window will not be displayed
  if LastDisplay = 0 then LastDisplay := GetTickCount();
  if (GetTickCount() - LastDisplay > DisplayInterval) then
  begin
    LastDisplay := GetTickCount();

    OnDisplay.Call(Self, FTotalProgress, FLastText);
  end;
end;

procedure TProgressTracker.Show({Worker: TObject; }Pos, Total: TFilePointer; Text: string = '-');
var
  AProgress: Double;
begin
  if Total > 0 then
    AProgress := Pos/Total
  else
    AProgress := 0;
  Show({Worker,} AProgress, Text);
end;

procedure TProgressTracker.TaskEnd;
var
  Task: TTask;
begin
  if CurrentTaskLevel = 0 then
  begin
    Assert(False, 'Unbalanced Progress.TaskStart/TaskEnd');
    Exit;
  end;
  Show({CurrentTask.Worker,} 1.0);
  Task := TaskStack.Peek();
  OnTaskEnd.Call(Self, Task);
  TaskStack.Pop();
end;

procedure TProgressTracker.TaskStart(Worker: TObject; PortionOfParent: Double = 1.0);
var
  Task, ParentTask: TTask;
begin
  if (PortionOfParent <> 1.0) and (CurrentTask = nil) then
    raise Exception.Create('Top-level task thould have a portion equal to 1.0');
  Task := TTask.Create();
  Task.Worker := Worker;
  Task.Portion := PortionOfParent;
  ParentTask := CurrentTask;
  if ParentTask = nil then
  begin
    Task.TotalWorkFrom := 0.0;
    Task.TotalWorkTo := 1.0;
    LastDisplay := 0;
    FLastText := '';
  end
  else
  begin
    Task.TotalWorkFrom := TotalProgress;
    Task.TotalWorkTo := Task.TotalWorkFrom + (ParentTask.TotalWorkTo - ParentTask.TotalWorkFrom) * PortionOfParent;
  end;
  TaskStack.Push(Task);
  OnTaskStart.Call(Self, Task);
  Show({Worker,} 0.0);
end;

initialization
  Progress := TProgressTracker.Create();
finalization
  FreeAndNil(Progress);
  FreeAndNil(EncodingsCache);
end.
