unit uDWHexTypes;

interface

uses
  SysUtils, Generics.Collections,

  uUtil;

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

type
  TFilePointer = Int64;

  TFileRange = record
    Start, AEnd: TFilePointer;
    function Size(): TFilePointer;
  end;

  TCachedRegion = class
    Addr: TFilePointer;
    Data: TBytes;
    function Size(): TFilePointer;
  end;

  TCachedRegionsList = TObjectList<TCachedRegion>;

  ENoActiveEditor = class (EAbort);

function MakeValidFileName(const S: string): string;
function DivRoundUp(A, B: Int64): Int64; inline;
function BoundValue(X, MinX, MaxX: TFilePointer): TFilePointer;

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

{ TFileRange }

function TFileRange.Size: TFilePointer;
begin
  Result := AEnd-Start;
end;

{ TCachedRegion }

function TCachedRegion.Size: TFilePointer;
begin
  Result := Length(Data);
end;

end.
