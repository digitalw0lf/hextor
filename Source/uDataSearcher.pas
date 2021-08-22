{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDataSearcher;

interface

uses
  System.SysUtils, System.Math, Generics.Collections, Winapi.Windows,
  Winapi.ActiveX,

  uHextorTypes, uEditedData, uCallbackList, uValueInterpretors, uActiveScript;

type
  EMatchPatternException = class (Exception);

  // "Compiled" extended syntax match pattern.
  // Supports wildcards "??", data elements "{i32:100}" etc.
  TExtMatchPattern = class
  public type
    // Pattern element: exact byte sequence or a value constrained by rule
    TElementType = (peNone, peBytes, peStr, peAny, peRanges, peScript, peGroupStart, peGroupEnd);
    TElement = record
      _type: TElementType;
      Data: TBytes;            // for peBytes
      Text: string;            // for peStr, peScript (temporary)
      SizeInBytes: Integer;    // for peStr - size of Text in bytes in specified CodePage; for peRanges - size of single value (e.g. {u16::10..15} -> SizeInBytes=2)
      DataType: TValueInterpretor;  // for peRanges, peScript
      Name: string;            // for peRanges, peScript
      Ranges: TVariantRanges;  // for peRanges
      Inverse: Boolean;        // for peRanges - match all except ranges
      MinCount, MaxCount: Integer;  // for peAny, peRanges, peScript
      Greedy: Boolean;         // for peAny, peRanges, peScript
    end;

    TFoundElement = record
      ElemIndex: Integer;
      Range: TFileRange;  // Relative addresses
      Data: TBytes;
      constructor Create(AElemIndex: Integer; ARange: TFileRange{; AData: TBytes; AName: string; ADataType: string});
    end;
    TFoundElements = TArray<TFoundElement>;

    TSearchGroup = record
      Elem1, Elem2: Integer;  // Elements index range
      Name: string;
    end;
    TSearchGroups = TArray<TSearchGroup>;
  protected const
    cAnyByte    = '?';
    cTagStart   = '{';
    cTagEnd     = '}';
    cTagDelim   = ':';
    cEscape     = '\';
    cNot        = '!';
    cNonGreedy  = '?';
    cGroupStart = '(';
    cGroupEnd   = ')';
  protected type
    TPossibleElemMatches = record
      // Result of element matcher
      WorstSize, BestSize, SizeStep: Integer;
      constructor Create(WorstSize, BestSize, SizeStep: Integer);
    end;
  protected
    bHex, bIgnoreCase, bExtSyntax, bNeedSubexpressions: Boolean;
    CodePage: Integer;
    Elements: TArray<TElement>;
    SearchGroups: TSearchGroups;
    OptimizedSimpleMatch: Boolean;  // True if pattern is a constant without variable ranges etc.
    function GetNextElement(var P: PChar; var Element: TElement): Boolean;
    function SimplifyElement(var Element: TElement): Boolean;
    function CombineElements(var Elem1: TElement; const Elem2: TElement): Boolean;
    function GetElementSize(DataType: TValueInterpretor; const Ranges: TVariantRanges): Integer;
    function ParseTag(const Text: string): TElement;
    procedure CollectGroups();
    procedure Compile(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer; ANeedSubexpressions: Boolean);
    function MatchElementsFrom(const Data: PByte; DataSize: Integer; ElemIndex: Integer; var Size: Integer; var Items: TFoundElements): Boolean;
    function MatchElement(const Data: PByte; DataSize: Integer;
      const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean; inline;
    function ElemCompareStr(const Data: PByte; DataSize: Integer; const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean;
    function ElemCompareValues(const Data: PByte; DataSize: Integer; const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean;
    function AdjustItemRanges(Items: TFoundElements; Delta: TFilePointer): TFoundElements;
    procedure MakeGroupsData(Data: PByte; var FoundElements: TFoundElements; var GroupsData: TArray<TBytes>);
    function ElementByName(const Name: string): Integer;
  public
    constructor Create(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer; ANeedSubexpressions: Boolean);
    destructor Destroy(); override;
    function IsEmpty(): Boolean;
    procedure CalcMinMaxMatchSize(var MinSize, MaxSize: Integer);
    function Match(Data: PByte; DataSize: Integer; var Size: Integer; var Items: TFoundElements): Boolean; inline;
  end;

  TExtReplacePatternElementType = (rpeNone, rpeBytes, rpeIndex, rpeName, rpeScript);
  TExtReplacePatternElement = record
    _type: TExtReplacePatternElementType;
    Data: TBytes;            // for rpeBytes
    Index: Integer;          // for rpeIndex
    Text: string;            // for rpeName, peScript
  end;

  TExtReplacePattern = class
  protected const
    cPlaceholder   = '$';
  protected
    bHex, bExtSyntax: Boolean;
    CodePage: Integer;
    Elements: TArray<TExtReplacePatternElement>;
    FHasPlaceholders: Boolean;
    function GetNextElement(var P: PChar; var Element: TExtReplacePatternElement): Boolean;
    procedure Compile(const Text: string; AHex, AExtSyntax: Boolean; ACodePage: Integer);
  public
    property HasPlaceholders: Boolean read FHasPlaceholders;
    constructor Create(const Text: string; AHex, AExtSyntax: Boolean; ACodePage: Integer);
  end;

  TSearchParams = record
    Text: string;
    bHex, bIgnoreCase, bExtSyntax: Boolean;
    bFindInSel: Boolean;
    bReplace: Boolean;
    Replace: string;
    bRepHex: Boolean;
    bAskEachReplace: Boolean;
    FindCodePage, ReplaceCodePage: Integer;

    Range: TFileRange;
  end;

  TCustomDataSearcher = class
  // Class for searching in TEditedData.
  // Actual match criteria are defined in derived classes
  protected
    function FindInBlock(const Data: PByte; DataSize: Integer; Start: Integer; Direction: Integer; MoreDataFollows: Boolean; var Ptr, Size: Integer): Boolean;
    function GetReplacement(): TBytes; virtual;
  public
    Haystack: TEditedData;
    Params: TSearchParams;
    Range: TFileRange;  // Actual range; equals to file size if Params.Range = EntireFile
    MinMatchSize, MaxMatchSize: Integer;  // Possible match size for current needle
    LastFoundRange: TFileRange;
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
  protected type
    TScriptFields = class(TInterfacedObject, IDispatch)
    public
      Owner: TExtPatternDataSearcher;
      // Expose found elements to scripts in replace pattern
      function GetIDsOfNames(const IID: TGUID; Names: Pointer;
        NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT;
        virtual; stdcall;
      function GetTypeInfo(Index: Integer; LocaleID: Integer;
        out TypeInfo): HRESULT; stdcall;
      function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
      function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
        Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
        ArgErr: Pointer): HRESULT; virtual; stdcall;
    end;
  protected
    ScriptEngine: TActiveScript;  // Expression evaluator
    ScriptFields: TScriptFields;
    LastMentionedType: TValueInterpretor;  // In replace placeholder - determines result size
    function GetReplacement(): TBytes; override;
  public
    Pattern: TExtMatchPattern;
    ReplacePattern: TExtReplacePattern;
    FoundElements: TExtMatchPattern.TFoundElements;
    FoundGroupsData: TArray<TBytes>;
    constructor Create();
    destructor Destroy(); override;
    function ParamsDefined(): Boolean; override;
    function Match(const Data: PByte; DataSize: Integer; var Size: Integer): Boolean; override;

    procedure PrepareScriptEnv();
  end;

implementation

uses
  uHextorDataSources;

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
  Regions: TSourceRegionArray;
//  t: Int64;
begin
  if FSearchInProgress then Exit(False);
  Range := Params.Range;
  if Range.AEnd < 0 then Range.AEnd := Haystack.GetSize();
  LastFoundRange := NoRange;

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

      Regions := Haystack.DataSource.GetRegions(Block);
      try

        if (Length(Regions) = 1) and (not Regions[0].HasData) then
        begin
          // Skip totally empty regions (e.g. unallocated memory)
          Result := False;
          if Direction = 1 then Ptr := Max(Regions[0].Range.AEnd - MaxMatchSize, Ptr)
                           else Ptr := Regions[0].Range.Start;
//          WriteLogFmt('Skip to %x', [Ptr]);
        end
        else
        begin
          // Take next data portion
          Data := Haystack.Get(Block.Start, Block.Size{, False});
          // Search in it
//          t := GetNanosec();
          Result := FindInBlock(@Data[0], Length(Data), Ptr - Block.Start, Direction, not LastBlock, IPtr, Size);
//          t := GetNanosec() - t;
//          WriteLogFmt('FindInBlock: (%x-%x) sz: %d ptr: %x time: %d', [Block.Start, Block.AEnd, Block.Size, Ptr, t div 1000000]);
          Ptr := Block.Start + IPtr;
        end;

      finally
        Regions.Free;
      end;

      // Found
      if Result then
      begin
        LastFoundRange.Start := Ptr;
        LastFoundRange.Size := Size;
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
    Result := String2Data(Params.Replace, Params.ReplaceCodePage);
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
  if LastFoundRange.Start < 0 then Exit(False);

  Replace := GetReplacement();
  NewSize := Length(Replace);

  Haystack.Change(LastFoundRange.Start, LastFoundRange.Size, NewSize, @Replace[0]);

  Result := True;
end;

{ TExtMatchPattern }

function TExtMatchPattern.GetElementSize(DataType: TValueInterpretor;
  const Ranges: TVariantRanges): Integer;
// Get size of elements of given DataType that can fall into Ranges.
// In case of fixed-size types, this does not depends on Ranges.
// In case of variable-size characters, this function fails if values in Ranges
// have different sizes (e.g. {utf8:'z'..'ß'})
var
  i: Integer;
begin
  // Simple fixed-size types
  if (DataType.MinSize = DataType.MaxSize) then Exit(DataType.MinSize);

  if (Ranges.IsEmpty()) then
    raise EMatchPatternException.CreateFmt('Cannot use empty range for variable-sized type %s', [DataType.Name]);

  // Size of values specified in Ranges
  Result := Length(DataType.FromVariant(Ranges.Ranges[0].AStart));

  for i := 0 to Length(Ranges.Ranges) - 1 do
  begin
    if (Length(DataType.FromVariant(Ranges.Ranges[0].AStart)) <> Result) or
       (Length(DataType.FromVariant(Ranges.Ranges[0].AEnd)) <> Result) then
      raise EMatchPatternException.Create('Values in range have different size');
  end;
end;

function TExtMatchPattern.AdjustItemRanges(Items: TFoundElements;
  Delta: TFilePointer): TFoundElements;
var
  i: Integer;
begin
  Result := Copy(Items);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i].Range.Start := Result[i].Range.Start + Delta;
    Result[i].Range.AEnd := Result[i].Range.AEnd + Delta;
  end;
end;

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
          Sz := Elements[i].SizeInBytes;
          Inc(MinSize, Elements[i].MinCount * Sz);
          Inc(MaxSize, Elements[i].MaxCount * Sz);
        end;
    end;
  end;
end;

procedure TExtMatchPattern.CollectGroups;
// Find matching brackets and generate groups list
var
  i, j: Integer;
  StartPos: TStack<Integer>;
  Group: TSearchGroup;
begin
  // Group[0] - entire match
  Group.Elem1 := 0;
  Group.Elem2 := High(Elements);
  Group.Name := '';
  SearchGroups := [Group];

  StartPos := TStack<Integer>.Create();
  try
    for i := 0 to Length(Elements) - 1 do
    begin
      if Elements[i]._type = peGroupStart then
        StartPos.Push(i)
      else
      if Elements[i]._type = peGroupEnd then
      begin
        if StartPos.Count = 0 then
          raise EMatchPatternException.Create('Unbalanced brackets');
        j := StartPos.Pop();
        Group.Elem1 := j;
        Group.Elem2 := i;
        Group.Name := '';
        SearchGroups := SearchGroups + [Group];
      end;
    end;
    if StartPos.Count <> 0 then
      raise EMatchPatternException.Create('Unbalanced brackets');
  finally
    StartPos.Free;
  end;
end;

function TExtMatchPattern.CombineElements(var Elem1: TElement;
  const Elem2: TElement): Boolean;
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

procedure TExtMatchPattern.Compile(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer; ANeedSubexpressions: Boolean);
// Parse specified pattern into list of elements
var
  P: PChar;
  Elem: TElement;
begin
  bHex := AHex;
  bIgnoreCase := AIgnoreCase;
  bExtSyntax := AExtSyntax;
  CodePage := ACodePage;
  bNeedSubexpressions := ANeedSubexpressions;

  Elements := nil;
  P := @Text[Low(Text)];
  while GetNextElement(P, Elem) do
  begin
    SimplifyElement(Elem);
    if (Length(Elements) > 0) and (CombineElements(Elements[High(Elements)], Elem)) then
      Continue;
    Elements := Elements + [Elem];
  end;

  CollectGroups();

  if (Length(Elements) = 1) and (Elements[0]._type = peBytes) then
    OptimizedSimpleMatch := True
  else
    OptimizedSimpleMatch := False;
end;

constructor TExtMatchPattern.Create(const Text: string; AHex, AIgnoreCase, AExtSyntax: Boolean; ACodePage: Integer; ANeedSubexpressions: Boolean);
begin
  inherited Create();
  Compile(Text, AHex, AIgnoreCase, AExtSyntax, ACodePage, ANeedSubexpressions);
end;

destructor TExtMatchPattern.Destroy;
begin

  inherited;
end;

function TExtMatchPattern.ElemCompareStr(const Data: PByte; DataSize: Integer;
  const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean;
// Compare Data with Element's text using pattern's CodePage.
var
  s: string;
begin
  if (DataSize < Elem.SizeInBytes) then Exit(False);
  // To compare case-insensitive, we have to convert file data to string using selected CodePage
  try
    s := Data2String(MakeBytes(Data[0], Elem.SizeInBytes), CodePage);
  except
    Exit(False);
  end;
  if bIgnoreCase then
    Result := (AnsiCompareText(s, Elem.Text) = 0)
  else
    // Actually, if not IgnoreCase then element will be converted to peBytes and we won't get here
    Result := (CompareStr(s, Elem.Text) = 0);
  if Result then
    PossibleMatches := TPossibleElemMatches.Create(Elem.SizeInBytes, Elem.SizeInBytes, 0);
end;

function TExtMatchPattern.ElemCompareValues(const Data: PByte; DataSize: Integer;
  const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean;
// Compare Data with Element's value ranges.
var
  i, ElemSize, MaxCount: Integer;
  v: Variant;
  Match: Boolean;
begin
  ElemSize := Elem.SizeInBytes;
  if DataSize < ElemSize * Elem.MinCount then Exit(False);
  MaxCount := Min(DataSize div ElemSize, Elem.MaxCount);
  PossibleMatches.WorstSize := Elem.MinCount * ElemSize;
  PossibleMatches.SizeStep := -ElemSize;
  Result := True;
  PossibleMatches.BestSize := MaxCount * ElemSize;
  if Elem.Ranges.Ranges <> nil then  // Empty range treated as "match any"
  begin
    for i:=0 to MaxCount-1 do
    begin
      try
        v := Elem.DataType.ToVariant(Data[ElemSize * i], ElemSize);
        Match := Elem.Ranges.Contains(v) xor Elem.Inverse;
      except
        Match := False;
      end;
      if (not Match) then  // Found item that do not falls in ranges
      begin
        Result := (i >= Elem.MinCount);
        if Result then PossibleMatches.BestSize := i * ElemSize;
        Break;
      end;
    end;
  end;
  if (Result) and (not Elem.Greedy) then
  begin
    SwapValues(PossibleMatches.WorstSize, PossibleMatches.BestSize);
    PossibleMatches.SizeStep := -PossibleMatches.SizeStep;
  end;
end;

function TExtMatchPattern.ElementByName(const Name: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(Elements) - 1 do
    if Elements[i].Name = Name then Exit(i);
  Result := -1;
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

function ReadTag(var P: PChar): string;
// Read a tag (char sequence between "{" and "}").
// Tries to correctly handle internal closing brackets "}" inside of character constants
var
  p1: PChar;
  InString: Boolean;
  InBrackets: Integer;
begin
  Inc(P);
  p1 := P;
  InString := False;
  InBrackets := 0;
  while (P^ <> #0) do
  begin
    if (P^ = '''') or (P^ = '"') then InString := not InString;
    if (not InString) and (CharInSet(P^, ['(', '[', '{'])) then Inc(InBrackets);
    if (not InString) and (InBrackets > 0) and (CharInSet(P^, [')', ']', '}'])) then Dec(InBrackets);
    if (not InString) and (InBrackets = 0) and (P^ = TExtMatchPattern.cTagEnd) then Break;
    Inc(P);
  end;
  if P^ <> TExtMatchPattern.cTagEnd then
    raise EMatchPatternException.Create('Unmatched opening bracket');
  SetString(Result, p1, P - p1);
  Inc(P);
end;

function TExtMatchPattern.GetNextElement(var P: PChar;
  var Element: TElement): Boolean;
var
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
          s := ReadTag(P);
          Element := ParseTag(s);
          Result := True;
        end;
      cGroupStart:  // '(' - elements group
        begin
          Inc(P);
          Element._type := peGroupStart;
          Result := True;
        end;
      cGroupEnd:  // ')'
        begin
          Inc(P);
          Element._type := peGroupEnd;
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
            cTagStart, cAnyByte, cEscape, cGroupStart, cGroupEnd:
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

procedure TExtMatchPattern.MakeGroupsData(Data: PByte; var FoundElements: TFoundElements;
  var GroupsData: TArray<TBytes>);
// Extract bytes from source buffer according to SearchGroups
var
  i, j: Integer;
  Buf: TBytes;
begin
  for i := 0 to Length(FoundElements) - 1 do
    FoundElements[i].Data := MakeBytes(Data[FoundElements[i].Range.Start], FoundElements[i].Range.Size);

  SetLength(GroupsData, Length(SearchGroups));
  for i := 0 to Length(SearchGroups) - 1 do
  begin
    Buf := nil;
    for j := SearchGroups[i].Elem1 to SearchGroups[i].Elem2 do
      Buf := Buf + FoundElements[j].Data;
    GroupsData[i] := Buf;
  end;
end;

function TExtMatchPattern.Match(Data: PByte; DataSize: Integer;
  var Size: Integer; var Items: TFoundElements): Boolean;
var
  Elem: ^TElement;
  Len: Integer;
begin
  // If we have a trivial case - just a constant expression without variable ranges,
  // case-insensitive strings etc., we can skip complex logic
  if OptimizedSimpleMatch then
  begin
    Elem := @Elements[0];
    Len := Length(Elem.Data);
    if (DataSize >= Len) and (CompareMem(Data, @Elem.Data[0], Len)) then
    begin
      Size := Len;
      SetLength(Items, 1);
      Items[0].ElemIndex := 0;
      Items[0].Range.Start := 0;
      Items[0].Range.AEnd := Len;
      Exit(True);
    end;
  end;

  Result := MatchElementsFrom(Data, DataSize, 0, Size, Items);
end;

function TExtMatchPattern.MatchElement(const Data: PByte; DataSize: Integer;
  const Elem: TElement; var PossibleMatches: TPossibleElemMatches): Boolean;
// Match specified Element with data buffer.
// Returns True if data matches this element.
// For variable-sized elements, match is always "greedy".
var
  Len: Integer;
begin
  Result := False;
  case Elem._type of
    peBytes:  // Match exact bytes
      begin
        Len := Length(Elem.Data);
        if (DataSize >= Len) and (CompareMem(Data, @Elem.Data[0], Len)) then
        begin
          Result := True;
          PossibleMatches := TPossibleElemMatches.Create(Len, Len, 0);
        end;
      end;
    peStr:  // Match text, possible ignoring case
      begin
        Result := ElemCompareStr(Data, DataSize, Elem, PossibleMatches);
      end;
    peAny:  // Match any bytes
      begin
        if (DataSize >= Elem.MinCount) then
        begin
          Result := True;
          PossibleMatches := TPossibleElemMatches.Create(Elem.MinCount, Min(Elem.MaxCount, DataSize), 1);
        end;
      end;
    peRanges:  // Match specified count of elements by ranges
      begin
        Result :=  ElemCompareValues(Data, DataSize, Elem, PossibleMatches);
      end;
    peScript:
      begin
        // TODO
      end;
    peGroupStart, peGroupEnd:  // This helper elements match empty space
      begin
        Result := True;
        PossibleMatches := TPossibleElemMatches.Create(0, 0, 0);
      end;
  end;
end;

function TExtMatchPattern.MatchElementsFrom(const Data: PByte; DataSize,
  ElemIndex: Integer; var Size: Integer; var Items: TFoundElements): Boolean;
// Recursive function - find a best match for subset of elements from ElemIndex to end of pattern
var
  PossibleMatches: TPossibleElemMatches;
  CurSize, RestSize: Integer;
  RestItems: TFoundElements;
begin
  if ElemIndex >= Length(Elements) then
  begin
    Size := 0;
    Items := nil;
    Exit(True);
  end;

  // Find possible matches for current element
  if not MatchElement(Data, DataSize, Elements[ElemIndex], PossibleMatches) then Exit(False);

  // If current element can match variable number of bytes (e.g. "{u8:0:1..10}"),
  // try all possible variants and choose best one
  CurSize := PossibleMatches.BestSize;
  repeat
    if MatchElementsFrom(@Data[CurSize], DataSize - CurSize, ElemIndex + 1, RestSize, RestItems) then
    begin
      Size := CurSize + RestSize;
      Items :=
        // Item for current element
        [TFoundElement.Create(ElemIndex, TFileRange.Create(0, CurSize){, nil, Elements[ElemIndex].Name, Elements[ElemIndex].DataType.Name})] +
        // Items for following elements
        AdjustItemRanges(RestItems, CurSize);
      Exit(True);
    end;
    if CurSize = PossibleMatches.WorstSize then Exit(False);
    Inc(CurSize, PossibleMatches.SizeStep);
  until False;

  Result := False;
  if Result then ;  // Avoid strange hint about unused Result in D 10.4
end;

procedure ThirdElemToMinMaxCount(const a: TArray<string>; var MinCount, MaxCount: Integer; var Greedy: Boolean);
// number or number1..number2
// non-greedy if ends with "?"
var
  Text: string;
  i: Integer;
begin
  if Length(a) < 3 then
  begin
    MinCount := 1;
    MaxCount := 1;
    Greedy := True;
    Exit;
  end;
  Text := a[2];
  if Text.EndsWith(TExtMatchPattern.cNonGreedy) then
  begin
    Greedy := False;
    Delete(Text, High(Text), Length(TExtMatchPattern.cNonGreedy));
  end
  else
    Greedy := True;
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

function TExtMatchPattern.ParseTag(const Text: string): TElement;
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
  a1 := a[0].Split([' '], TStringSplitOptions.ExcludeEmpty);
  // How many words are in first part of tag?
  case Length(a1) of
    1, 2:  // Type [and name] specified - this is a "ranges" condition
      begin
        Result._type := peRanges;
        Result.DataType := ValueInterpretors.FindInterpretor(a1[0]);
        if Result.DataType = nil then
          raise EMatchPatternException.Create('Invalid type specifier: ' + a1[0]);
        if Length(a1) = 2 then
          Result.Name := a1[1];
        if a[1].StartsWith(cNot) then
        begin
          Result.Inverse := True;
          Delete(a[1], Low(a[1]), 1);
        end
        else
          Result.Inverse := False;
        Result.Ranges := StrToVariantRanges(a[1]);
        Result.SizeInBytes := GetElementSize(Result.DataType, Result.Ranges);
        ThirdElemToMinMaxCount(a, Result.MinCount, Result.MaxCount, Result.Greedy);
      end;
//    2:  // Type and name specified - this is a scripted condition
//      begin
//        Result._type := peScript;
//        Result.DataType := ValueInterpretors.FindInterpretor(a1[0]);
//        if Result.DataType = nil then
//          raise EMatchPatternException.Create('Invalid type specifier: ' + a1[0]);
//        Result.Name := a1[1];
//        Result.Text := a[1];  // Script text
//        ThirdElemToMinMaxCount(a, Result.MinCount, Result.MaxCount, Result.Greedy);
//      end;
    else
      raise EMatchPatternException.Create('Error in tag');
  end;
end;

function TExtMatchPattern.SimplifyElement(
  var Element: TElement): Boolean;
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
    peRanges:  // Cannot "flatten" elements if they are used as subexpressions in Replace pattern
      if not bNeedSubexpressions then
      begin
        // Ranged expression can be converted to raw bytes if it unambiguously evaluates to constant value.
        // E.g. {i16:5:2} evaluates to 05000500
        if (Element.MinCount = Element.MaxCount) and (Element.MaxCount <= 1024) and
           (Length(Element.Ranges.Ranges) = 1) and (Element.Ranges.Ranges[0].AStart = Element.Ranges.Ranges[0].AEnd) and
           (not ((Element.DataType.Name = 'float') or (Element.DataType.Name = 'double'))) and  // Float's require inexact comparison
           (not Element.Inverse) then
        begin
          Element._type := peBytes;
          b := Element.DataType.FromVariant(Element.Ranges.Ranges[0].AStart);
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
  ReplacePattern.Free;
  ScriptEngine.Free;
  ScriptFields._Release();
  inherited;
end;

function TExtPatternDataSearcher.GetReplacement: TBytes;
// Returns data that should be used to replace last found needle.
// Uses found subexpressions if replace pattern contains placeholders.
var
  i, j: Integer;
  Elem: ^TExtReplacePatternElement;
  V: Variant;
begin
  if Assigned(ScriptEngine) then
  begin
    ScriptEngine.Reset();
    ScriptEngine.AddObject('ExtPatternDataSearcher', ScriptFields, True);
  end;
  Result := nil;
  for i := 0 to Length(ReplacePattern.Elements) - 1 do
  begin
    Elem := @ReplacePattern.Elements[i];
    case Elem._type of
      rpeBytes:
        Result := Result + Elem.Data;
      rpeIndex:
        if Elem.Index <= High(FoundGroupsData) then
          Result := Result + FoundGroupsData[Elem.Index]
        else
          raise EMatchPatternException.CreateFmt('Invalid subexpression index: %d', [Elem.Index]);
      rpeName:
        begin
          j := Pattern.ElementByName(Elem.Text);
          if j < 0 then
            raise EMatchPatternException.CreateFmt('Invalid subexpression name: %s', [Elem.Text]);
          Result := Result + FoundElements[j].Data;
        end;
      rpeScript:
        begin
          PrepareScriptEnv();
          LastMentionedType := nil;
          V := ScriptEngine.Eval(Elem.Text);
          if LastMentionedType = nil then
            raise EMatchPatternException.CreateFmt('Cannot determine value size for expression "%s"', [Elem.Text]);
          Result := Result + LastMentionedType.FromVariant(V);
        end;
    end;
  end;
end;

function TExtPatternDataSearcher.Match(const Data: PByte; DataSize: Integer;
  var Size: Integer): Boolean;
begin
  FoundElements := nil;
  FoundGroupsData := nil;
  if not ParamsDefined() then Exit(False);
  if DataSize < MinMatchSize then Exit(False);

  Result := Pattern.Match(Data, DataSize, Size, FoundElements);

  if Result then
    Pattern.MakeGroupsData(Data, FoundElements, FoundGroupsData);
end;

function TExtPatternDataSearcher.ParamsDefined: Boolean;
begin
  Result := (Pattern <> nil);
end;

procedure TExtPatternDataSearcher.PrepareScriptEnv;
begin
  if Assigned(ScriptEngine) then Exit;

  ScriptFields := TScriptFields.Create();
  ScriptFields._AddRef();
  ScriptFields.Owner := Self;
  ScriptEngine := TActiveScript.Create(nil);
  ScriptEngine.AddObject('ExtPatternDataSearcher', ScriptFields, True);
end;

{ TExtMatchPattern.TPossibleElemMatches }

constructor TExtMatchPattern.TPossibleElemMatches.Create(WorstSize, BestSize,
  SizeStep: Integer);
begin
  Self.WorstSize := WorstSize;
  Self.BestSize := BestSize;
  Self.SizeStep := SizeStep;
end;

{ TExtMatchPattern.TExtMatchPatternFoundItem }

constructor TExtMatchPattern.TFoundElement.Create(AElemIndex: Integer; ARange: TFileRange);
begin
  ElemIndex := AElemIndex;
  Range := ARange;
end;

{ TExtReplacePattern }

procedure TExtReplacePattern.Compile(const Text: string; AHex,
  AExtSyntax: Boolean; ACodePage: Integer);
// Parse specified pattern into list of elements
var
  P: PChar;
  Elem: TExtReplacePatternElement;
begin
  bHex := AHex;
  bExtSyntax := AExtSyntax;
  CodePage := ACodePage;
  FHasPlaceholders := False;

  Elements := nil;
  P := @Text[Low(Text)];
  while GetNextElement(P, Elem) do
  begin
    if (Elem._type = rpeBytes) and (Length(Elements) > 0) and (Elements[High(Elements)]._type = rpeBytes) then
    begin
      Elements[High(Elements)].Data := Elements[High(Elements)].Data + Elem.Data;
      Continue;
    end;
    Elements := Elements + [Elem];
  end;
end;

constructor TExtReplacePattern.Create(const Text: string; AHex,
  AExtSyntax: Boolean; ACodePage: Integer);
begin
  inherited Create();
  Compile(Text, AHex, AExtSyntax, ACodePage);
end;

function TExtReplacePattern.GetNextElement(var P: PChar;
  var Element: TExtReplacePatternElement): Boolean;
var
  s: string;
  b: Byte;
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

  // Process subexpression placeholders "$..."
  if bExtSyntax then
  begin
    case P^ of
      cPlaceholder:  // '$' - subexpression placeholder
        begin
          Inc(P);
          if P^ = TExtMatchPattern.cTagStart then
          // Named subexpression
          begin
            s := ReadTag(P);
            if s = '' then
              raise EMatchPatternException.Create('Error in replacement pattern');
            if IsValidIdent(s, False) then
              Element._type := rpeName
            else
              Element._type := rpeScript;
            Element.Text := s;
          end
          else
          // Indexed subexpression
          if (P^ >= '0') and (P^ <= '9') then
          begin
            Element._type := rpeIndex;
            Element.Index := Ord(P^) - Ord('0');
            Inc(P);
          end
          else
            raise EMatchPatternException.Create('Error in replacement pattern');
          FHasPlaceholders := True;
          Result := True;
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
        Element._type := rpeBytes;
        Element.Data := [b];
      end;
    end
    else  // Text
    begin
      Element._type := rpeBytes;
      Element.Data := String2Data(P^, CodePage);
      Inc(P);
      Result := True;
    end;
  end;
end;

{ TExtPatternDataSearcher.TOleFields }

function TExtPatternDataSearcher.TScriptFields.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
type
  PNames = ^TNames;
  TNames = array[0..100] of POleStr;
  PDispIDs = ^TDispIDs;
  TDispIDs = array[0..100] of Integer;
var
  i: Integer;
  AName: string;
begin
  AName := PNames(Names)^[0];
  i := Owner.Pattern.ElementByName(AName);
  if i >= 0 then
  begin
    PInteger(DispIDs)^ := i;
    Exit(S_OK);
  end;
  Result := DISP_E_UNKNOWNNAME;
end;

function TExtPatternDataSearcher.TScriptFields.GetTypeInfo(Index,
  LocaleID: Integer; out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TExtPatternDataSearcher.TScriptFields.GetTypeInfoCount(
  out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TExtPatternDataSearcher.TScriptFields.Invoke(DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult,
  ExcepInfo, ArgErr: Pointer): HRESULT;
var
  Put: Boolean;
begin
  // Unknown DispID
  if (DispID < 0) then
    Exit(DISP_E_MEMBERNOTFOUND);
  Put := ((Flags and (DISPATCH_PROPERTYPUT or DISPATCH_PROPERTYPUTREF)) <> 0);
  if Put then
    Exit(DISP_E_BADVARTYPE);
  Result := S_OK;
  POleVariant(VarResult)^ := Owner.Pattern.Elements[DispID].DataType.ToVariant(
    Owner.FoundElements[DispID].Data[0], Length(Owner.FoundElements[DispID].Data));
  // Original value type determines output value size.
  // E.g. ${X+1} will generate a value of same size as X
  Owner.LastMentionedType := Owner.Pattern.Elements[DispID].DataType;
end;

end.
