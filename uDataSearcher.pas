unit uDataSearcher;

interface

uses
  System.SysUtils,

  uDWHexTypes, uEditedData, uCallbackList, uUtil;

type
  TSearchParams = record
    Text: string;
    bHex, bWildcards, bUnicode, bMatchCase: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;
    bAskEachReplace: Boolean;

    Range: TFileRange;

    Needle: TBytes;
  end;

  TDataSearcher = class
  // Class for searching specified data in TEditedData.
  // Current limitation: only fixed-size needle
  protected
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: Integer; Direction: Integer; var Ptr, Size: Integer): Boolean;
    function GetReplacement(): TBytes;
  public
    Haystack: TEditedData;
    Params: TSearchParams;
    LastFound: TFileRange;
    FSearchInProgress: Boolean;
    OnProgress: TCallbackListP4<{Sender:}TObject, {Pos:}TFilePointer, {Total:}TFilePointer, {Text:}string>;
//    OnSearchDone: TCallbackListP1<TDataSearcher>;
    function ParamsDefined(): Boolean;
    function NeedleSize(): Integer; virtual;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; virtual;
    function Find(Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
    function ReplaceLastFound(var NewSize: Integer): Boolean;
  end;

implementation

{ TDataSearcher }

function TDataSearcher.Find(Start: TFilePointer;
  Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
const
  MinBlockSize = 64*KByte;
  MaxBlockSize = 1*MByte;
  BlockOverlap = 16*KByte;
var
  BlockSize: Integer;
  Data: TBytes;
  IPtr: Integer;
  Block: TFileRange;
begin
  if FSearchInProgress then Exit(False);
  if Params.Range.AEnd < 0 then Params.Range.AEnd := Haystack.GetSize();
  LastFound := NoRange;

  if (Start < Params.Range.Start) or (Start >= Params.Range.AEnd) then Exit(False);

  Ptr := Start;
  BlockSize := MinBlockSize;
  // Dynamically load blocks, overlapped by 16 KB if we go backwards
  // Block size increased on each step
  try
    FSearchInProgress := True;
    repeat
      if Direction > 0 then
      begin
        Block.Start := Ptr;
        Block.AEnd := Ptr + BlockSize
      end
      else
      begin
        Block.Start := Ptr - BlockSize + BlockOverlap;
        Block.AEnd := Ptr + BlockOverlap;
      end;
      if Block.Start < Params.Range.Start then Block.Start := Params.Range.Start;
      if Block.AEnd > Params.Range.AEnd then Block.AEnd := Params.Range.AEnd;

      // Take next data portion
      Data := Haystack.Get(Block.Start, Block.Size, False);
      // Search in it
      Result := FindInBlock(@Data[0], Length(Data), Ptr - Block.Start, Direction, IPtr, Size);
      Ptr := Block.Start + IPtr;

      // Found
      if Result then
      begin
        LastFound.Start := Ptr;
        LastFound.Size := Size;
        Break;
      end;

      // Reached end of range
      if (Direction > 0) and (Block.AEnd = Params.Range.AEnd) then Break;
      if (Direction < 0) and (Block.Start = Params.Range.Start) then Break;
      if BlockSize < MaxBlockSize then
        BlockSize := BlockSize * 2;

      OnProgress.Call(Self, Ptr-Params.Range.Start, Params.Range.Size, '-');
    until False;
  finally
    FSearchInProgress := False;
//    OnSearchDone.Call(Self);
  end;
end;

function TDataSearcher.FindInBlock(const Data: PByte; DataSize: Integer;
  Start: Integer; Direction: Integer; var Ptr, Size: Integer): Boolean;
// Search in given loaded block.
// Ptr is shifted in given direction, even if nothing found,
// until we are sure we have no chance to find
begin
  Ptr := Start;
  while (Ptr>=0) and (Ptr<DataSize) do
  begin
    if Match(@Data[Ptr], DataSize-Ptr, Size) then
      Exit(True);
    Inc(Ptr, Direction);
    if Ptr+NeedleSize() > DataSize then Break;
  end;
  Result := False;
end;

function TDataSearcher.GetReplacement: TBytes;
// Returns data that should be used to replace last found needle
// (This may depend on last found data if using something like RegEx)
begin
  if Params.bRepHex then
    Result := Str2Bytes(Hex2Data(AnsiString(Params.Replace)))
  else
  if Params.bUnicode then
    Result := Str2Bytes(Params.Replace)
  else
    Result := Str2Bytes(AnsiString(Params.Replace));
end;

function TDataSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
var
  i: Integer;
begin
  if DataSize < Length(Params.Needle) then Exit(False);
  for i:=0 to Length(Params.Needle)-1 do
    if Data[i] <> Params.Needle[i] then Exit(False);
  Result := True;
  Size := Length(Params.Needle);
end;

function TDataSearcher.NeedleSize: Integer;
begin
  Result := Length(Params.Needle);
end;

function TDataSearcher.ParamsDefined: Boolean;
begin
  Result := (Params.Needle <> nil);
end;


function TDataSearcher.ReplaceLastFound(var NewSize: Integer): Boolean;
// Replace last found needle according to Params
var
  Replace: TBytes;
begin
  if LastFound.Start < 0 then Exit(False);

  Replace := GetReplacement();
  NewSize := Length(Replace);

  Haystack.Change(LastFound.Start, LastFound.Size, NewSize, @Replace[0]);

  Result := True;
end;

end.
