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
  private
    function GetSize(): TFilePointer;
    procedure SetSize(Value: TFilePointer);
  public
    Start, AEnd: TFilePointer;
    property Size: TFilePointer read GetSize write SetSize;
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
function DataEqual(const Data1, Data2: TBytes): Boolean;

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

{ TFileRange }

function TFileRange.GetSize: TFilePointer;
begin
  Result := AEnd-Start;
end;

procedure TFileRange.SetSize(Value: TFilePointer);
begin
  AEnd := Start + Value;
end;

{ TCachedRegion }

function TCachedRegion.Size: TFilePointer;
begin
  Result := Length(Data);
end;

end.
