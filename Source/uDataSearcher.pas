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
  System.SysUtils, System.Math,

  uHextorTypes, uEditedData, uCallbackList, uValueInterpretors;

type
  EMatchPatternException = class (Exception);

  TExtMatchPatternElementType = (peNone, peBytes, peStr, peAny, peRanges, peScript);
  TExtMatchPatternElement = record
    _type: TExtMatchPatternElementType;
    Data: TBytes;            // for peBytes
    Text: string;            // for peStr, peScript (temporary)
    SizeInBytes: Integer;    // for peStr - size of Text in bytes in specified CodePage
    DataType: TValueInterpretor;  // for peRanges, peScript
    Name: string;            // for peRanges, peScript
    Ranges: TVariantRanges;  // for peRanges
    Inverse: Boolean;        // for peRanges - match all except ranges
    MinCount, MaxCount: Integer;  // for peAny, peRanges, peScript
  end;

  TExtMatchPattern = class
  // "Compiled" extended syntax match pattern.
  // Supports wildcards "??", data elements "{i32:100}" etc.
  private const
    cAnyByte  = '?';
    cTagStart = '{';
    cTagEnd   = '}';
    cTagDelim = ':';
    cEscape   = '\';
    cNot      = '!';
  private
    bHex, bIgnoreCase, bExtSyntax: Boolean;
    CodePage: Integer;
    Elements: array of TExtMatchPatternElement;
    function GetNextElement(var P: PChar; var Element: TExtMatchPatternElement): Boolean;
    function SimplifyElement(var Element: TExtMatchPatternElement): Boolean;
    function CombineElements(var Elem1: TExtMatchPatternElement; const Elem2: TExtMatchPatternElement): Boolean;
    function ParseTag(const Text: string): TExtMatchPatternElement;
    procedure Compile(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer);
    function MatchElement(const Data: PByte; DataSize: Integer;
      const Elem: TExtMatchPatternElement; var Size: Integer): Boolean;
  public
    constructor Create(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer);
    destructor Destroy(); override;
    function IsEmpty(): Boolean;
    procedure CalcMinMaxMatchSize(var MinSize, MaxSize: Integer);
    function Match(Data: PByte; DataSize: Integer; var Size: Integer): Boolean;
  end;

  TSearchParams = record
    Text: string;
    bHex, bUnicode, bIgnoreCase, bExtSyntax: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;
    bAskEachReplace: Boolean;
    CodePage: Integer;

    Range: TFileRange;
  end;

  TCustomDataSearcher = class
  // Class for searching in TEditedData.
  // Actual match criteria are defined in derived classes
  protected
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: Integer; Direction: Integer; MoreDataFollows: Boolean; var Ptr, Size: Integer): Boolean;
    function GetReplacement(): TBytes;
  public
    Haystack: TEditedData;
    Params: TSearchParams;
    Range: TFileRange;  // Actual range; equals to file size if Params.Range = EntireFile
    MinMatchSize, MaxMatchSize: Integer;  // Possible match size for current needle
    LastFound: TFileRange;
    FSearchInProgress: Boolean;
    function ParamsDefined(): Boolean; virtual;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; virtual; abstract;
    function FindNext(Start: TFilePointer; Direction: Integer; var Ptr: TFilePointer; var Size: Integer): Boolean;
    function ReplaceLastFound(var NewSize: Integer): Boolean;
  end;

  TSimpleDataSearcher = class (TCustomDataSearcher)
  // Searches for data equal to Needle.
  // This is not used now
  public
    Needle: TBytes;
    function ParamsDefined(): Boolean; override;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; override;
  end;

  TExtPatternDataSearcher = class (TCustomDataSearcher)
  // Searches for data that matches Pattern
  public
    Pattern: TExtMatchPattern;
    constructor Create();
    destructor Destroy(); override;
    function ParamsDefined(): Boolean; override;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; override;
  end;

implementation

{ TDataSearcher }

function TCustomDataSearcher.FindNext(Start: TFilePointer;
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
  LastBlock: Boolean;
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

      // Is this a last block of data?
      LastBlock := False;
      if (Direction > 0) and (Block.AEnd = Range.AEnd) then LastBlock := True;
      if (Direction < 0) and (Block.Start = Range.Start) then LastBlock := True;

      // Take next data portion
      Data := Haystack.Get(Block.Start, Block.Size, False);
      // Search in it
      Result := FindInBlock(@Data[0], Length(Data), Ptr - Block.Start, Direction, not LastBlock, IPtr, Size);
      Ptr := Block.Start + IPtr;

      // Found
      if Result then
      begin
        LastFound.Start := Ptr;
        LastFound.Size := Size;
        Break;
      end;

      // Reached end of range
      if LastBlock then Break;
      if BlockSize < MaxBlockSize then
        BlockSize := BlockSize * 2;

      Progress.Show(Ptr-Range.Start, Range.Size);
    until False;
  finally
    FSearchInProgress := False;
//    OnSearchDone.Call(Self);
  end;
end;

function TCustomDataSearcher.FindInBlock(const Data: PByte; DataSize: Integer;
  Start: Integer; Direction: Integer; MoreDataFollows: Boolean; var Ptr, Size: Integer): Boolean;
// Search in given loaded block.
// Ptr is shifted in given direction, even if nothing found,
// until we are sure we have no chance to find.
var
  StopWhen: Integer;
begin
  Ptr := Start;
  // If more data will follow, stop searching as soon as MaxMatchSize bytes left
  // to not miss large items on block boundary
  if MoreDataFollows then StopWhen := MaxMatchSize
                     else StopWhen := MinMatchSize;
  while (Ptr>=0) and (Ptr<DataSize) do
  begin
    if Match(@Data[Ptr], DataSize-Ptr, Size) then
      Exit(True);
    Inc(Ptr, Direction);
    if Ptr + StopWhen > DataSize then Break;
  end;
  Result := False;
end;

function TCustomDataSearcher.GetReplacement: TBytes;
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

function TCustomDataSearcher.ParamsDefined: Boolean;
begin
  Result := True;
end;


function TCustomDataSearcher.ReplaceLastFound(var NewSize: Integer): Boolean;
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

{ TExtMatchPattern }

procedure TExtMatchPattern.CalcMinMaxMatchSize(var MinSize, MaxSize: Integer);
var
  i, Sz: Integer;
//  Tmp: TBytes;
begin
  MinSize := 0;
  MaxSize := 0;
  for i:=0 to Length(Elements)-1 do
  begin
    case Elements[i]._type of
      peBytes:
        begin
          Inc(MinSize, Length(Elements[i].Data));
          Inc(MaxSize, Length(Elements[i].Data));
        end;
      peStr:
        begin
//          Tmp := String2Data(Elements[i].Text, CodePage);
//          Inc(MinSize, Length(Tmp));
//          Inc(MaxSize, Length(Tmp));
          Inc(MinSize, Elements[i].SizeInBytes);
          Inc(MaxSize, Elements[i].SizeInBytes);
        end;
      peAny:
        begin
          Inc(MinSize, Elements[i].MinCount);
          Inc(MaxSize, Elements[i].MaxCount);
        end;
      peRanges, peScript:
        begin
          Sz := Elements[i].DataType.MinSize;
          Inc(MinSize, Elements[i].MinCount * Sz);
          Inc(MaxSize, Elements[i].MaxCount * Sz);
        end;
    end;
  end;
end;

function TExtMatchPattern.CombineElements(var Elem1: TExtMatchPatternElement;
  const Elem2: TExtMatchPatternElement): Boolean;
// Combine two elements of same type into one, if possible
begin
  Result := False;
  if Elem1._type <> Elem2._type then Exit;
  case Elem1._type of
    peBytes:
      begin
        Elem1.Data := Elem1.Data + Elem2.Data;
        Result := True;
      end;
    peStr:
      begin
        Elem1.Text := Elem1.Text + Elem2.Text;
        Elem1.SizeInBytes := Elem1.SizeInBytes + Elem2.SizeInBytes;
        Result := True;
      end;
    peAny:
      begin
        Elem1.MinCount := Elem1.MinCount + Elem2.MinCount;
        Elem1.MaxCount := Elem1.MaxCount + Elem2.MaxCount;
        Result := True;
      end;
  end;
end;

procedure TExtMatchPattern.Compile(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer);
// Parse specified pattern into list of elements
var
  P: PChar;
  Elem: TExtMatchPatternElement;
begin
  bHex := AHex;
  bIgnoreCase := AIgnoreCase;
  bExtSyntax := AExtSyntax;
  CodePage := ACodePage;

  Elements := nil;
  P := @Text[Low(Text)];
  while GetNextElement(P, Elem) do
  begin
    SimplifyElement(Elem);
    if (Length(Elements) > 0) and (CombineElements(Elements[High(Elements)], Elem)) then
      Continue;
    Elements := Elements + [Elem];
  end;
end;

constructor TExtMatchPattern.Create(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer);
begin
  inherited Create();
  Compile(Text, AHex, AIgnoreCase, AExtSyntax, ACodePage);
end;

destructor TExtMatchPattern.Destroy;
begin

  inherited;
end;

const
  H2BDelimiters = [' ', #9, #10, #13];
  H2BValidSet = ['0'..'9','A'..'F','a'..'f'];
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

function ReadHexByte(var P: PChar; var b: Byte): Boolean;
// Read next hex pair from buffer pointed by P, skipping delimiters.
// Returns False when #0 is reached, throws exception if invaid character found
var
  j: Integer;
begin
  j := -1;
  while P^ <> #0 do
  begin
    if CharInSet(P^, H2BDelimiters) then
    begin
      // Skip spaces
    end
    else
    if CharInSet(P^, H2BValidSet) then
    begin
      Inc(j);  // Index among valid source chars
      if j = 0 then
        b := H2BConvert[P^] shl 4
      else
      begin
        b := b + H2BConvert[P^];
        Inc(P);
        Exit(True);
      end;
    end
    else
      raise EMatchPatternException.Create('Invalid character in hex pattern: ' + P^);
    Inc(P);
  end;
  if j = 0 then
    raise EMatchPatternException.Create('Odd character count in hex pattern');
  Result := False;
end;


function TExtMatchPattern.GetNextElement(var P: PChar;
  var Element: TExtMatchPatternElement): Boolean;
var
  p1: PChar;
  s: string;
  b: Byte;
  TmpBuf: TBytes;
begin
  Result := False;
  Finalize(Element);
  FillChar(Element, SizeOf(Element), 0);
  if bHex then  // In hex mode, skip all spaces
  begin
    while CharInSet(P^, H2BDelimiters) do
      Inc(P);
  end;
  if P^ = #0 then Exit;

  // Process extended  syntax elements (wildcards etc.)
  if bExtSyntax then
  begin
    case P^ of
      cAnyByte:  // '?' - any byte
        begin
          Inc(P);
          if bHex then
          begin
            if P^ <> cAnyByte then
              raise EMatchPatternException.Create('In hex search mode, use two question marks (??) to specify unknown byte');
            Inc(P);
          end;
          Element._type := peAny;
          Element.MinCount := 1;
          Element.MaxCount := 1;
          Result := True;
        end;
      cTagStart:  // '{' - tag with typed data element
        begin
          Inc(P);
          p1 := P;
          while (P^ <> cTagEnd) and (P^ <> #0) do
            Inc(P);
          if P^ <> cTagEnd then
            raise EMatchPatternException.Create('Unmatched opening bracket');
          SetString(s, p1, P - p1);
          Inc(P);
          Element := ParseTag(s);
          Result := True;
        end;
      cEscape:  // '\' - escape sequence
        begin
          Inc(P);
          case P^ of
            'x':
              begin
                Inc(P);
                Result := ReadHexByte(P, b);
                if Result then
                begin
                  Element._type := peBytes;
                  Element.Data := [b];
                end;
              end;
            cTagStart, cAnyByte, cEscape:
              begin
                Element._type := peStr;
                Element.Text := P^;
                Inc(P);
                Result := True;
              end;
            else
              begin
                raise EMatchPatternException.Create('Invalid escape char: ' + P^);
              end;
          end;
        end;
    end;
  end;

  if not Result then
  // Raw data
  begin
    if bHex then  // Hex
    begin
      Result := ReadHexByte(P, b);
      if Result then
      begin
        Element._type := peBytes;
        Element.Data := [b];
      end;
    end
    else  // Text
    begin
      Element._type := peStr;
      Element.Text := P^;
      Inc(P);
      Result := True;
    end;
  end;

  // Calculate space (in bytes) required by Text element
  if (Result) and (Element._type = peStr) then
  begin
    TmpBuf := String2Data(Element.Text, CodePage);
    Element.SizeInBytes := Length(TmpBuf);
  end;
end;

function TExtMatchPattern.IsEmpty: Boolean;
begin
  Result := (Length(Elements) = 0);
end;

function TExtMatchPattern.Match(Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
var
  i, sz: Integer;
begin
  Size := 0;
  for i:=0 to Length(Elements)-1 do
  begin
    if not MatchElement(Data, DataSize, Elements[i], sz) then Exit(False);
    Inc(Data, sz);
    Dec(DataSize, sz);
    Inc(Size, sz);
  end;
  Result := True;
end;

function TExtMatchPattern.MatchElement(const Data: PByte; DataSize: Integer;
  const Elem: TExtMatchPatternElement; var Size: Integer): Boolean;
// Match specified Element with data buffer.
// Returns True if data matches this element.
// For variable-sized elements, match is always "greedy".
var
  i, ElemSize, MaxCount: Integer;
  v: Variant;
  s: string;
begin
  Result := False;
  case Elem._type of
    peBytes:  // Match exact bytes
      begin
        if (DataSize >= Length(Elem.Data)) and (CompareMem(Data, @Elem.Data[0], Length(Elem.Data))) then
        begin
          Result := True;
          Size := Length(Elem.Data);
        end;
      end;
    peStr:  // Match text, possible ignoring case
      begin
        if (DataSize >= Elem.SizeInBytes) then
        begin
          // To compare case-insensitive, we have to convert file data to string using selected CodePage
          s := Data2String(MakeBytes(Data[0], Elem.SizeInBytes), CodePage);
          if bIgnoreCase then
            Result := (AnsiCompareText(s, Elem.Text) = 0)
          else
            // Actually, if not IgnoreCase then element will be converted to peBytes and we won't get here
            Result := (CompareStr(s, Elem.Text) = 0);
          if Result then
            Size := Elem.SizeInBytes;
        end;
      end;
    peAny:  // Match any bytes
      begin
        if (DataSize >= Elem.MinCount) then
        begin
          Result := True;
          Size := Min(Elem.MaxCount, DataSize);
        end;
      end;
    peRanges:  // Match specified count of elements by ranges
      begin
        ElemSize := Elem.DataType.MinSize;
        if DataSize < ElemSize * Elem.MinCount then Exit(False);
        MaxCount := Min(DataSize div ElemSize, Elem.MaxCount);
        for i:=0 to MaxCount-1 do
        begin
          try
            v := Elem.DataType.ToVariant(Data[ElemSize * i], ElemSize);
          except
            Exit(False);
          end;
          if (not Elem.Ranges.Contains(v)) xor (Elem.Inverse) then  // Found item that do not falls in ranges
          begin
            Result := (i >= Elem.MinCount);
            if Result then Size := i * ElemSize;
            Exit;
          end;
        end;
        Result := True;
        Size := MaxCount * ElemSize;
      end;
    peScript:
      begin
        // TODO
      end;
  end;
end;

procedure ThirdElemToMinMaxCount(const a: TArray<string>; var MinCount, MaxCount: Integer);
// number or number1..number2
var
  Text: string;
  i: Integer;
begin
  if Length(a) < 3 then
  begin
    MinCount := 1;
    MaxCount := 1;
    Exit;
  end;
  Text := a[2];
  i := Pos('..', Text);
  if i > 0 then
  begin
    MinCount := StrToInt(Trim(Copy(Text, Low(Text), i - 1)));
    MaxCount := StrToInt(Trim(Copy(Text, Low(Text) + i + 1, MaxInt)));
  end
  else
  begin
    MinCount := StrToInt(Trim(Text));
    MaxCount := MinCount;
  end;
  if (MinCount > MaxCount) or (MinCount < 0) or (MaxCount > 65535) then
    raise EMatchPatternException.Create('Invalid min/max count');
end;

function TExtMatchPattern.ParseTag(const Text: string): TExtMatchPatternElement;
// Parse content of a tag between "{" and "}"
var
  a, a1: TArray<string>;
begin
  a := Text.Split([cTagDelim]);
  if Length(a) = 0 then
    raise EMatchPatternException.Create('Empty tag');
  // If tag does not contains ':' - this is a hex string
  if Length(a) = 1 then
  begin
    Result._type := peBytes;
    Result.Data := Hex2Data(a[0]);
    Exit;
  end;
  a1 := a[0].Split([' '], ExcludeEmpty);
  // How many words are in first part of tag?
  case Length(a1) of
    1:  // Type specified - this is a "ranges" condition
      begin
        Result._type := peRanges;
        Result.DataType := ValueInterpretors.FindInterpretor(a1[0]);
        if Result.DataType = nil then
          raise EMatchPatternException.Create('Invalid type specifier: a1[0]');
        if a[1].StartsWith(cNot) then
        begin
          Result.Inverse := True;
          Delete(a[1], Low(a[1]), 1);
        end
        else
          Result.Inverse := False;
        Result.Ranges := StrToVariantRanges(a[1]);
        ThirdElemToMinMaxCount(a, Result.MinCount, Result.MaxCount);
      end;
    2:  // Type and name specified - this is a scripted condition
      begin
        Result._type := peScript;
        Result.DataType := ValueInterpretors.FindInterpretor(a1[0]);
        if Result.DataType = nil then
          raise EMatchPatternException.Create('Invalid type specifier: a1[0]');
        Result.Name := a1[1];
        Result.Text := a[1];  // Script text
        ThirdElemToMinMaxCount(a, Result.MinCount, Result.MaxCount);
      end;
    else
      raise EMatchPatternException.Create('Error in tag');
  end;
end;

function TExtMatchPattern.SimplifyElement(
  var Element: TExtMatchPatternElement): Boolean;
// Convert element to raw bytes if possible
var
  b: TBytes;
  i: Integer;
begin
  Result := False;
  case Element._type of
    peStr:
      begin
        // String can be converted to constant bytes if "Ignore case" is not selected
        if not bIgnoreCase then
        begin
          Element._type := peBytes;
          Element.Data := String2Data(Element.Text, CodePage);
          Element.Text := '';
          Element.SizeInBytes := 0;
          Result := True;
        end;
      end;
    peRanges:
      begin
        // Ranged expression can be converted to raw bytes if it unambiguously evaluates to constant value.
        // E.g. {i16:5:2} evaluates to 05000500
        if (Element.MinCount = Element.MaxCount) and (Element.MaxCount <= 1024) and
           (Length(Element.Ranges.Ranges) = 1) and (Element.Ranges.Ranges[0].AStart = Element.Ranges.Ranges[0].AEnd) and
           (not Element.Inverse) then
        begin
          Element._type := peBytes;
          SetLength(b, Element.DataType.MinSize);
          Element.DataType.FromVariant(Element.Ranges.Ranges[0].AStart, b[0], Length(b));
          SetLength(Element.Data, Length(b) * Element.MinCount);
          for i:=0 to Element.MinCount-1 do
            Move(b[0], Element.Data[i*Length(b)], Length(b));
          Element.DataType := nil;
          Element.Ranges.Ranges := nil;
          Element.MinCount := 0;
          Element.MaxCount := 0;
          Result := True;
        end;
      end;
  end;
end;

{ TSimpleDataSearcher }

function TSimpleDataSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
//var
//  i: Integer;
begin
  if DataSize < Length(Needle) then Exit(False);
//  for i:=0 to Length(Needle)-1 do
//    if Data[i] <> Needle[i] then Exit(False);
  if not CompareMem(Data, @Needle[0], Length(Needle)) then Exit(False);
  Result := True;
  Size := Length(Needle);
end;

function TSimpleDataSearcher.ParamsDefined: Boolean;
begin
  Result := (Needle <> nil);
end;

{ TExtPatternDataSearcher }

constructor TExtPatternDataSearcher.Create;
begin
  inherited;
end;

destructor TExtPatternDataSearcher.Destroy;
begin
  Pattern.Free;
  inherited;
end;

function TExtPatternDataSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
begin
  if not ParamsDefined() then Exit(False);
  if DataSize < MinMatchSize then Exit(False);
  Result := Pattern.Match(Data, DataSize, Size);
end;

function TExtPatternDataSearcher.ParamsDefined: Boolean;
begin
  Result := (Pattern <> nil);
end;

end.
