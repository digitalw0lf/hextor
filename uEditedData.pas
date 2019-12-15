unit uEditedData;

interface

uses
  System.Types, System.SysUtils, Generics.Collections, Math, Vcl.Forms,
  Generics.Defaults,

  uDWHexTypes, uDWHexDataSources, uUtil, uCallbackList, uLogFile, uSkipList;

type
  // This class contains virtual "data" of edited file.
  // It consists of buffers with changed data regions and references to unchanged
  // data regions in original source.
  TEditedData = class
  public type
    TDataPartType = (
      ptUnknown,
      ptSource,  // Ref to original data from DataSource
      ptBuffer); // Loaded (changed) data in Data buffer
    TDataPart = class
      PartType: TDataPartType;
      Addr: TFilePointer;  // Address in new (edited) data space
      Size: TFilePointer;
      SourceAddr: TFilePointer;  // Corresponding address in Source (for unchanged parts)
      Data: TBytes;  // For changed parts
      procedure Assign(Source: TDataPart);
      constructor Create(APartType: TDataPartType; AAddr: TFilePointer; ASize: TFilePointer = 0);
    end;
    TDataPartSkipList = class (TSkipListSet<TDataPart>)
    end;
    TDataPartList = TObjectList<TDataPart>;
    TDataPartArray = TArray<TDataPart>;
  private type
    // Dummy "DataPart" to pass to comparators with real TDataPart
    TDummyPartRec = record
      AClass: Pointer;
      PartType: TDataPartType;
      Addr: TFilePointer;
      Size: TFilePointer;
    end;
  private
    FSize: TFilePointer;
    FHasMovements: Boolean;
    function SplitPart({Part: TDataPart; }Addr: TFilePointer): Boolean; //TDataPart;
    function CombineParts(Addr: TFilePointer): Boolean;
    procedure PreparePartsForOperation(Addr, Size: TFilePointer{; var FirstPart, LastPart: TDataPart});
    procedure RecalcAddressesAfter(OldAddr, NewAddr: TFilePointer);
    procedure BoundToRange(var Addr, Size: TFilePointer);
  public
    DataSource: TDWHexDataSource;
    Resizable: Boolean;
    Parts: TDataPartSkipList;  // Treat as private except for saving to file
    // Data changed event. "Value" may be nil if fired as a result on Undo
    OnDataChanged: TCallbackListP4<{Addr:}TFilePointer, {OldSize:}TFilePointer, {NewSize:}TFilePointer, {Value:}PByteArray>;
    // Event for Undo stack
    OnBeforePartsReplace: TCallbackListP3<{Addr:}TFilePointer, {OldSize:}TFilePointer, {NewSize:}TFilePointer>;
    constructor Create();
    destructor Destroy(); override;
    procedure ResetParts();
    procedure GetOverlappingParts(Addr, Size: TFilePointer; var FirstPart, LastPart: TDataPart; {out} AParts: TDataPartList = nil); overload;
    procedure GetOverlappingParts(Addr, Size: TFilePointer; {out} AParts: TDataPartList); overload;
    function HasChanges(): Boolean;
    function HasMovements(): Boolean;
    function GetDebugDescr(): string;

    function GetSize(): TFilePointer;
    function Get(Addr: TFilePointer; Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;

    procedure ReplaceParts(Addr, OldSize: TFilePointer; const NewParts: array of TDataPart {TDataPartList});

    procedure Change(Addr, OldSize, NewSize: TFilePointer; Value: PByteArray); overload;

    procedure Change(Addr: TFilePointer; Size: TFilePointer; Value: PByteArray); overload;
    procedure Insert(Addr: TFilePointer; Size: TFilePointer; Value: PByteArray);
    procedure Delete(Addr: TFilePointer; Size: TFilePointer);
  end;

implementation

uses
  uEditorForm;

function CompareDataParts(const Left, Right: TEditedData.TDataPart): Integer;
// Left may be TDummyPartRec created for search
begin
  if InRange(Left.Addr, Right.Addr, Right.Addr + Right.Size - 1) then
    Result := 0
  else
    Result := CompareValue(Left.Addr, Right.Addr);
end;

{ TEditedData }

procedure TEditedData.BoundToRange(var Addr, Size: TFilePointer);
begin
  Addr := EnsureRange(Addr, 0, GetSize());
  Size := EnsureRange(Addr + Size, 0, GetSize()) - Addr;
end;

procedure TEditedData.Change(Addr: TFilePointer; Size: TFilePointer;
  Value: PByteArray);
begin
  Change(Addr, Size, Size, Value);
end;

constructor TEditedData.Create();
begin
  inherited Create();
//  Parts := TObjectList<TDataPart>.Create(True);
  Parts := TDataPartSkipList.Create(10, 4, TComparer<TDataPart>.Construct(CompareDataParts));
end;

procedure TEditedData.Delete(Addr: TFilePointer; Size: TFilePointer);
begin
  Change(Addr, Size, 0, nil);
end;

destructor TEditedData.Destroy;
begin
  Parts.Free;
  inherited;
end;

procedure TEditedData.PreparePartsForOperation(Addr, Size: TFilePointer);
// Prepare parts for operation on range [Addr..Addr+Size):
// Split parts at that boundaries
begin
  if (Addr < 0) or (Size < 0) or (Addr + Size > GetSize()) then
    raise Exception.Create('Trying to change out of range');

  SplitPart(Addr);
  if Size > 0 then
    SplitPart(Addr + Size);
end;

function TEditedData.Get(Addr: TFilePointer; Size: TFilePointer;
  ZerosBeyondEoF: Boolean): TBytes;
var
  CurrSize: TFilePointer;
  ReturnSize: NativeInt;
  i: Integer;
  oPos, oSize: TFilePointer;
  AParts: TDataPartList;
begin
  CurrSize := GetSize();
  if (Addr<0) or (Addr>CurrSize) then Exit(nil);
  if (not ZerosBeyondEoF) and (Addr>=CurrSize) then Exit(nil);

  StartTimeMeasure();

  if ZerosBeyondEoF then
    ReturnSize := Size
  else
  begin
    ReturnSize := Min(Size, CurrSize-Addr);
  end;
  SetLength(Result, ReturnSize);

  // Collect data from corresponding parts
  AParts := TDataPartList.Create(False);
  try
    GetOverlappingParts(Addr, Size, AParts);
    for i:=0 to AParts.Count-1 do
    begin
      oPos := Max(Addr, AParts[i].Addr);
      oSize := Min(AParts[i].Addr+AParts[i].Size, Addr+Size) - oPos;
      case AParts[i].PartType of
        ptSource:  // Data from original source
          begin
            DataSource.GetData(AParts[i].SourceAddr + (oPos - AParts[i].Addr),
                               oSize, Result[oPos-Addr]);
          end;
        ptBuffer:  // Cached/changed data
          begin
            Move(AParts[i].Data[oPos-AParts[i].Addr], Result[oPos-Addr], oSize);
          end;
      end;
    end;
  finally
    AParts.Free;
  end;

  if (ZerosBeyondEoF) and (Addr+ReturnSize > CurrSize) then
  // Fill with zeros beyond end of file
  begin
    FillChar(Result[CurrSize-Addr], Addr+ReturnSize-CurrSize, 0);
  end;

  EndTimeMeasure('Get', True);
end;

function TEditedData.GetDebugDescr: string;
var
  Part: TEditedData.TDataPart;
begin
  Result := '';
  for Part in Parts do
    Result := Result + 'sb'[Ord(Part.PartType)] + ' ' + IntToStr(Part.Addr)+' '+IntToStr(Part.Size)+' '+RemUnprintable(string(MakeStr(Copy(Part.Data, 0, 50))))+#13#10;
end;

procedure TEditedData.GetOverlappingParts(Addr,
  Size: TFilePointer; {out} AParts: TDataPartList);
var
  AFirstPart, ALastPart: TDataPart;
begin
  GetOverlappingParts(Addr, Size, AFirstPart, ALastPart, AParts);
end;

procedure TEditedData.GetOverlappingParts(Addr,
  Size: TFilePointer; var FirstPart, LastPart: TDataPart; {out} AParts: TDataPartList);
var
  Dummy: TDummyPartRec;
  Part: TDataPart;
begin
  if (Addr >= GetSize()) then
  begin
    FirstPart := nil;
    LastPart := nil;
    Exit;
  end;
  BoundToRange(Addr, Size);

  Dummy.Addr := Addr;
  if not Parts.TryFetch(@Dummy, FirstPart) then
    FirstPart := nil;

  if (Size = 0) and ((FirstPart = nil) or (FirstPart.Addr = Addr)) then
  // Special case for zero-length region on a part boundary
  begin
    FirstPart := nil;
    LastPart := nil;
    Exit;
  end;

  Dummy.Addr := Addr + Size - 1;
  if not Parts.TryFetch(@Dummy, LastPart) then
    LastPart := nil;

  if Assigned(AParts) then
  begin
    if Assigned(LastPart) then
      for Part in Parts.EnumerateRange(FirstPart, LastPart) do
        AParts.Add(Part)
    else
      for Part in Parts.EnumerateFrom(FirstPart) do
        AParts.Add(Part);
  end;
end;

function TEditedData.GetSize: TFilePointer;
begin
  Result := FSize;
end;

function TEditedData.HasChanges: Boolean;
// False if we have only one part that is equal to entire source.
var
  Part: TDataPart;
begin
  Result := False;
  for Part in Parts do
    if (Part.PartType = ptSource) and (Part.Addr = 0) and (Part.Size = GetSize()) then
      Continue
    else
      Exit(True);
end;

function TEditedData.HasMovements: Boolean;
// If some parts were moved due to insertion/deletion of data
//var
//  Part: TDataPart;
begin
  if not Resizable then Exit(False);

//  for Part in Parts do
//    if (Part.PartType = ptSource) and (Part.SourceAddr <> Part.Addr) then
//      Exit(True);
//  Result := False;
  Result := HasChanges() and FHasMovements;
end;

procedure TEditedData.Insert(Addr: TFilePointer; Size: TFilePointer;
  Value: PByteArray);
begin
  Change(Addr, 0, Size, Value);
end;

procedure TEditedData.RecalcAddressesAfter(OldAddr, NewAddr: TFilePointer);
var
  Dummy: TDummyPartRec;
  Part: TDataPart;
  AAddr: TFilePointer;
begin
  // TODO: There should be some better data structure that can handle this without full traverse
  Dummy.Addr := OldAddr;
  AAddr := NewAddr;
  for Part in Parts.EnumerateFrom(@Dummy) do
  begin
    Part.Addr := AAddr;
    if (Part.PartType = ptSource) and (Part.Addr <> Part.SourceAddr) then
      FHasMovements := True;
    AAddr := AAddr + Part.Size;
  end;
  FSize := AAddr;
end;

procedure TEditedData.ReplaceParts(Addr, OldSize: TFilePointer;
  const NewParts: array of TDataPart {TDataPartList});
// Replace data parts in range [Addr..Addr+OldSize) with NewParts
// Split/combine parts if needed
var
  Part: TDataPart;
  i: Integer;
  NewSize: TFilePointer;
  AValue: PByteArray;
  OldParts: TDataPartList;
begin
  // TODO: Optimize for a case when we really don't need to split parts and combine them back

  // Split parts on boundaries
  PreparePartsForOperation(Addr, OldSize{, FirstPart, LastPart});

  // Pass old parts to Undo stack
  NewSize := 0;
  for i:=0 to Length(NewParts)-1 do
    NewSize := NewSize + NewParts[i].Size;
  OnBeforePartsReplace.Call(Addr, OldSize, NewSize);

  // Delete old parts
  OldParts := TDataPartList.Create(False);
  GetOverlappingParts(Addr, OldSize, OldParts);
  for Part in OldParts do
    Parts.Remove(Part);
  OldParts.Free;

  // Re-calculate addresses of parts after this operation
  if NewSize <> OldSize then
    RecalcAddressesAfter(Addr + OldSize, Addr + NewSize);

  // Insert new parts
  for Part in NewParts do
    Parts.AddOrSet(Part);

  // Combine parts on boundaries if possible
  CombineParts(Addr);
  if NewSize > 0 then
    CombineParts(Addr + NewSize);

  // Call event. If we changed one part with in-memory data, pass pointer to that data
  if (Length(NewParts) = 1) and (NewParts[0].PartType = ptBuffer) then
    AValue := @NewParts[0].Data[0]
  else
    AValue := nil;
  OnDataChanged.Call(Addr, OldSize, NewSize, AValue);
end;

procedure TEditedData.ResetParts;
var
  Part: TDataPart;
begin
  Parts.Clear();
  // Create initial part corresponding to entire DataSource
  Part := TDataPart.Create(ptSource, 0, DataSource.GetSize());
  Part.SourceAddr := 0;
  Parts.AddOrSet(Part);
  FSize := Part.Size;
  FHasMovements := False;
end;

function TEditedData.SplitPart(Addr: TFilePointer): Boolean;
// Split Part into two parts of same type by address Addr
// Returns second part
var
  Part1, Part2: TDataPart;
  Size1: TFilePointer;
  Dummy: TDummyPartRec;
begin
  Result := False;
  Dummy.Addr := Addr;

  if not Parts.TryFetch(@Dummy, Part1) then Exit;
  if (Addr = Part1.Addr) or (Addr = Part1.Addr + Part1.Size) then Exit;

  Size1 := Addr - Part1.Addr;
  Part2 := TDataPart.Create(Part1.PartType, Addr, Part1.Size - Size1);

  case Part1.PartType of
    ptSource:
      begin
        Part2.SourceAddr := Part1.SourceAddr + Size1;
      end;
    ptBuffer:
      begin
        Part2.Data := Copy(Part1.Data, Size1, Part2.Size);
        SetLength(Part1.Data, Size1);
      end;
  end;
  Part1.Size := Size1;

  Parts.AddOrSet(Part2);
  Result := True;
end;

procedure TEditedData.Change(Addr, OldSize, NewSize: TFilePointer;
  Value: PByteArray);
// General-case data modification: replace data [Addr..Addr+OldSize) with Value of size NewSize.
// Can be used as overwrition/insertion/deletion with different parameters
var
  Part: TDataPart;
  NewParts: array of TDataPart;
  ASize: TFilePointer;
begin
  // If changing partially after end of data
  ASize := GetSize();
  if Addr + OldSize > ASize then
    OldSize := ASize - Addr;

  if (not Resizable) and (OldSize <> NewSize) then
    raise Exception.Create('Cannot resize data');

  if NewSize > 0 then
  begin
    Part := TDataPart.Create(ptBuffer, Addr, NewSize);
    Part.Data := MakeBytes(Value^, NewSize);
    NewParts := [Part];
  end
  else
    NewParts := nil;

  ReplaceParts(Addr, OldSize, NewParts);  // <--
end;

function TEditedData.CombineParts(Addr: TFilePointer): Boolean;
// Combine parts which meet at address Addr
var
  NewSize: TFilePointer;
  Dummy: TDummyPartRec;
  Part1, Part2: TDataPart;
begin
  Result := False;
  if (Addr <= 0) or (Addr >= GetSize()) then Exit;

  Dummy.Addr := Addr - 1;
  if not Parts.TryFetch(@Dummy, Part1) then Exit;
  if Part1.Addr + Part1.Size <> Addr then Exit;
  Dummy.Addr := Addr;
  if not Parts.TryFetch(@Dummy, Part2) then Exit;
  // Check if it is possible to combine
  if Part2.PartType <> Part1.PartType then Exit;
  if (Part2.PartType = ptSource) and (Part2.SourceAddr <> Part1.SourceAddr + Part1.Size) then Exit;


  NewSize := Part1.Size + Part2.Size;

  case Part1.PartType of
    ptSource:
      begin
        // Nothing to do here
      end;
    ptBuffer:
      begin
        SetLength(Part1.Data, NewSize);
        Move(Part2.Data[0], Part1.Data[Part1.Size], Part2.Size);
      end;
  end;

  Parts.Remove(Part2);
  Part1.Size := NewSize;
  Result := True;
end;

{ TEditedData.TDataPart }

procedure TEditedData.TDataPart.Assign(Source: TDataPart);
begin
  PartType := Source.PartType;
  Addr := Source.Addr;
  Size := Source.Size;
  SourceAddr := Source.SourceAddr;
  Data := Copy(Source.Data);
end;

constructor TEditedData.TDataPart.Create(APartType: TDataPartType; AAddr,
  ASize: TFilePointer);
begin
  inherited Create();
  PartType := APartType;
  Addr := AAddr;
  Size := ASize;
end;

end.
