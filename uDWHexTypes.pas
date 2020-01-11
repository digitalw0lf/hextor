unit uDWHexTypes;

interface

uses
  SysUtils, Generics.Collections, Vcl.Graphics, System.Math,

  uUtil;

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

type
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

  ENoActiveEditor = class (EAbort);

  TColorArray = array of TColor;

  IDWHexToolFrame = interface
    ['{4AB18488-6B7D-4A9B-9892-EC91DDF81745}']
    procedure OnShown();
  end;

function MakeValidFileName(const S: string): string;
function DivRoundUp(A, B: Int64): Int64; inline;
function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;
function DataEqual(const Data1, Data2: TBytes): Boolean;
function MakeZeroBytes(Size: NativeInt): TBytes;
function FillRangeInColorArray(var Colors: TColorArray; BaseAddr: TFilePointer; RangeStart, RangeEnd: TFilePointer; Color: TColor): Boolean;

const
  EntireFile: TFileRange = (Start: 0; AEnd: -1);
  NoRange: TFileRange = (Start: -1; AEnd: -1);

implementation

function MakeValidFileName(const S: string): string;
begin
  Result := ReplaceAllChars(S, CharsInvalidInFileName, '_');
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

function MakeZeroBytes(Size: NativeInt): TBytes;
begin
  SetLength(Result, Size);
  FillChar(Result[0], Size, 0);
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

end.
