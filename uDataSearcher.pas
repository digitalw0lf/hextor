unit uDataSearcher;

interface

uses
  System.SysUtils,

  uDWHexTypes, uEditedData, uCallbackList;

type
  TSearchParams = record
    Text: string;
    bHex, bWildcards, bUnicode, bMatchCase: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;

    Needle: TBytes;
  end;

  TDataSearcher = class
  // Class for searching specified data in TEditedData.
  // Current limitation: only fixed-size needle
  protected
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: Integer; Direction: Integer; var Ptr, Size: Integer): Boolean;
  public
    Haystack: TEditedData;
    Params: TSearchParams;
    FSearchInProgress: Boolean;
    OnProgress: TCallbackListP3<TDataSearcher, TFilePointer, TFilePointer>;
    OnSearchDone: TCallbackListP1<TDataSearcher>;
    function ParamsDefined(): Boolean;
    function NeedleSize(): Integer; virtual;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; virtual;
    function Find(Range: TFileRange; Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
  end;

implementation

{ TDataSearcher }

function TDataSearcher.Find(Range: TFileRange; Start: TFilePointer;
  Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
const
  BlockSize = 1*MByte;
  BlockOverlap = 100*KByte;
var
  Data: TBytes;
  IPtr: Integer;
  Block: TFileRange;
begin
  if FSearchInProgress then Exit(False);
  if Range.AEnd < 0 then Range.AEnd := Haystack.GetSize(); //TargetEditor.GetFileSize();

  Ptr := Start;
  // Dynamically load by 1 MB, overlapped by 100 KB if we go backwards
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
      if Block.Start < Range.Start then Block.Start := Range.Start;
      if Block.AEnd > Range.AEnd then Block.AEnd := Range.AEnd;

      // Take next data portion
      //Data := TargetEditor.GetEditedData(Block.Start, Block.Size, False);
      Data := Haystack.Get(Block.Start, Block.Size, False);
      // Search in it
      Result := FindInBlock(@Data[0], Length(Data), Ptr - Block.Start, Direction, IPtr, Size);
      Ptr := Block.Start + IPtr;

      // Found
      if Result then Break;

      // Reached end of range
      if (Direction > 0) and (Block.AEnd = Range.AEnd) then Break;
      if (Direction < 0) and (Block.Start = Range.Start) then Break;

      //ShowProgress(Ptr-Range.Start, Range.Size);
      OnProgress.Call(Self, Ptr-Range.Start, Range.Size);
    until False;
  finally
    FSearchInProgress := False;
    //OperationDone();
    OnSearchDone.Call(Self);
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


end.
