{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDataSearcher;

interface

uses
  System.SysUtils,

  uHextorTypes, uEditedData, uCallbackList;

type
  TSearchParams = record
    Text: string;
    bHex, bWildcards, bUnicode, bMatchCase: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;
    bAskEachReplace: Boolean;
    CodePage: Integer;

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
    Range: TFileRange;  // Actual range; equals to file size if Params.Range = EntireFile
    LastFound: TFileRange;
    FSearchInProgress: Boolean;
    OnProgress: TCallbackListP4<{Sender:}TObject, {Pos:}TFilePointer, {Total:}TFilePointer, {Text:}string>;
//    OnSearchDone: TCallbackListP1<TDataSearcher>;
    function ParamsDefined(): Boolean;
    function NeedleSize(): Integer; virtual;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; virtual;
    function FindNext(Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
    function ReplaceLastFound(var NewSize: Integer): Boolean;
  end;

implementation

{ TDataSearcher }

function TDataSearcher.FindNext(Start: TFilePointer;
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
  Range := Params.Range;
  if Range.AEnd < 0 then Range.AEnd := Haystack.GetSize();
  LastFound := NoRange;

  if (Start < Range.Start) or (Start >= Range.AEnd) then Exit(False);

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
      if Block.Start < Range.Start then Block.Start := Range.Start;
      if Block.AEnd > Range.AEnd then Block.AEnd := Range.AEnd;

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
      if (Direction > 0) and (Block.AEnd = Range.AEnd) then Break;
      if (Direction < 0) and (Block.Start = Range.Start) then Break;
      if BlockSize < MaxBlockSize then
        BlockSize := BlockSize * 2;

      OnProgress.Call(Self, Ptr-Range.Start, Range.Size, '-');
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
    Result := Hex2Data(Params.Replace)
  else
  if Params.bUnicode then
    Result := String2Data(Params.Replace, TEncoding.Unicode.CodePage)
  else
    Result := String2Data(Params.Replace, Params.CodePage);
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
