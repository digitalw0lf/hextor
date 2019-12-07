unit uEditedData;

interface

uses
  System.Types, System.SysUtils, Generics.Collections, Math, Vcl.Forms,

  uDWHexTypes, uDWHexDataSources, uUtil, uCallbackList;

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
    end;
    TDataPartList = TObjectList<TDataPart>;
  private
    function SplitPart(Index: Integer; Addr: TFilePointer): Boolean;
    function CombineParts(Index1, Index2: Integer): Boolean;
    procedure PreparePartsForOperation(Addr, Size: TFilePointer; var Index1, Index2: Integer);
    procedure RecalcAddressesAfter(Addr: TFilePointer);
  public
    DataSource: TDWHexDataSource;
    Resizable: Boolean;
    Parts: TDataPartList;  // Treat as private except for saving to file
    OnDataChanged: TCallbackListP4<{Addr:}TFilePointer, {OldSize:}TFilePointer, {NewSize:}TFilePointer, {Value:}PByteArray>;
    constructor Create();
    destructor Destroy(); override;
    procedure ResetParts();
    procedure GetOverlappingParts(Addr, Size: TFilePointer; var Index1, Index2: Integer; {out} AParts: TDataPartList = nil); overload;
    procedure GetOverlappingParts(Addr, Size: TFilePointer; {out} AParts: TDataPartList); overload;
    function HasMovements(): Boolean;

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

{ TEditedData }

procedure TEditedData.Change(Addr: TFilePointer; Size: TFilePointer;
  Value: PByteArray);
begin
  Change(Addr, Size, Size, Value);
end;

constructor TEditedData.Create();
begin
  inherited Create();
  Parts := TObjectList<TDataPart>.Create(True);
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

procedure TEditedData.PreparePartsForOperation(Addr, Size: TFilePointer; var Index1,
  Index2: Integer);
// Prepare parts for operation on range [Addr..Addr+Size):
// Split parts at boundaries and return parts that cover exactly specified range
begin
  if (Addr < 0) or (Size < 0) or (Addr + Size > GetSize()) then
    raise Exception.Create('Trying to change out of range');

  GetOverlappingParts(Addr, Size, Index1, Index2);

  // On a part boundary
  if Index2 < Index1 then
    Exit;

  // Split blocks at start and end of selected region
  if SplitPart(Index1, Addr) then
  begin
    Inc(Index1);
    if Size > 0 then
      Inc(Index2);
  end;
  SplitPart(Index2, Addr + Size);
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
end;

procedure TEditedData.GetOverlappingParts(Addr,
  Size: TFilePointer; {out} AParts: TDataPartList);
var
  i1, i2: Integer;
begin
  GetOverlappingParts(Addr, Size, i1, i2, AParts);
end;

procedure TEditedData.GetOverlappingParts(Addr,
  Size: TFilePointer; var Index1, Index2: Integer; {out} AParts: TDataPartList);
var
  i: Integer;
begin
  if (Addr >= GetSize()) then
  begin
    Index1 := Parts.Count;
    Index2 := Parts.Count-1;
    Exit;
  end;
  Index1 := Parts.Count;
  Index2 := -1;
  for i:=0 to Parts.Count-1 do
  begin
    if (Parts[i].Addr<Addr+Size) and (Parts[i].Addr+Parts[i].Size>Addr) then
    begin
      if Assigned(AParts) then AParts.Add(Parts[i]);
      if i < Index1 then Index1 := i;
      if i > Index2 then Index2 := i;
    end;
    // Special case for zero-length region on a part boundary
    if (Size = 0) and (Parts[i].Addr = Addr) then
    begin
      Index1 := i;
      Index2 := i-1;
      Break;
    end;
  end;
end;

function TEditedData.GetSize: TFilePointer;
begin
  if Parts.Count = 0 then Result := 0
  else Result := Parts.Last.Addr + Parts.Last.Size;
end;

function TEditedData.HasMovements: Boolean;
// If some parts were moved due to insertion/deletion of data
var
  i: Integer;
begin
  if not Resizable then Exit(False);

  for i:=0 to Parts.Count-1 do
    if (Parts[i].PartType = ptSource) and (Parts[i].SourceAddr <> Parts[i].Addr) then
      Exit(True);
  Result := False;
end;

procedure TEditedData.Insert(Addr: TFilePointer; Size: TFilePointer;
  Value: PByteArray);
begin
  Change(Addr, 0, Size, Value);
end;

procedure TEditedData.RecalcAddressesAfter(Addr: TFilePointer);
var
  i: Integer;
begin
  // There should be some better data structure that can handle this without full traverse
  if Parts.Count > 0 then
  begin
    Parts[0].Addr := 0;
    for i:=1 to Parts.Count-1 do
    begin
      Parts[i].Addr := Parts[i-1].Addr + Parts[i-1].Size;
    end;
  end;
end;

procedure TEditedData.ReplaceParts(Addr, OldSize: TFilePointer;
  const NewParts: array of TDataPart {TDataPartList});
// Replace data parts in range [Addr..Addr+OldSize) with NewParts
// Split/combine parts if needed
var
  i1, i2, i, OldCount, NewCount: Integer;
begin
  // TODO: Optimize for a case when we really don't need to split parts and combine them back

  // Split parts on boundaries
  PreparePartsForOperation(Addr, OldSize, i1, i2);

  OldCount := i2 - i1 + 1;
  NewCount := Length(NewParts);

  // Replace parts with new ones
  if OldCount > NewCount then
    Parts.DeleteRange(i1 + NewCount, OldCount - NewCount)
  else
  begin
    for i:=0 to NewCount-OldCount-1 do
      Parts.Insert(i1 + OldCount, nil);
  end;
  for i:=0 to NewCount-1 do
    Parts[i1+i] := NewParts[i];
  i2 := i1 + NewCount - 1;

  RecalcAddressesAfter(Addr);

  // Combine parts on boundaries if possible
  if CombineParts(i1-1, i1) then
    Dec(i2);
  CombineParts(i2, i2+1);
end;

procedure TEditedData.ResetParts;
var
  Part: TDataPart;
begin
  Parts.Clear();
  // Create initial part corresponding to entire DataSource
  Part := TDataPart.Create();
  Part.PartType := ptSource;
  Part.Addr := 0;
  Part.Size := DataSource.GetSize();
  Part.SourceAddr := 0;
  Parts.Add(Part);
end;

function TEditedData.SplitPart(Index: Integer; Addr: TFilePointer): Boolean;
// Split Part ¹ Index into two parts of same type by address Addr
var
  Part1, Part2: TDataPart;
  Size1: TFilePointer;
begin
  Part1 := Parts[Index];
  if (Addr <= Part1.Addr) or (Addr >= Part1.Addr + Part1.Size) then Exit(False);

  Size1 := Addr - Part1.Addr;
  Part2 := TDataPart.Create();
  Part2.PartType := Part1.PartType;
  Part2.Addr := Addr;
  Part2.Size := Part1.Size - Size1;

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

  Parts.Insert(Index+1, Part2);
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
    Part := TDataPart.Create();
    Part.PartType := ptBuffer;
    Part.Addr := Addr;
    Part.Size := NewSize;
    Part.Data := MakeBytes(Value^, NewSize);
    NewParts := [Part];
  end
  else
    NewParts := nil;

  ReplaceParts(Addr, OldSize, NewParts);

  OnDataChanged.Call(Addr, OldSize, NewSize, Value);
end;

function TEditedData.CombineParts(Index1, Index2: Integer): Boolean;
// Combine parts Index1 through Index2
var
  i: Integer;
  NewSize, Ptr: TFilePointer;
begin
  Result := False;
  // Check if it is possible to combine
  if (Index1 < 0) or (Index2 >= Parts.Count) or (Index2 <= Index1) then Exit;
  for i:=Index1+1 to Index2 do
  begin
    if Parts[i].PartType <> Parts[Index1].PartType then Exit;
    if (Parts[i].PartType = ptSource) and (Parts[i].SourceAddr <> Parts[i-1].SourceAddr + Parts[i-1].Size) then Exit;
  end;

  NewSize := 0;
  for i:=Index1 to Index2 do
    NewSize := NewSize + Parts[i].Size;

  case Parts[Index1].PartType of
    ptSource:
      begin
        // Nothing to do here
      end;
    ptBuffer:
      begin
        Ptr := Parts[Index1].Size;
        SetLength(Parts[Index1].Data, NewSize);
        for i:=Index1+1 to Index2 do
        begin
          Move(Parts[i].Data[0], Parts[Index1].Data[Ptr], Parts[i].Size);
          Inc(Ptr, Parts[i].Size);
        end;
      end;
  end;
  Parts[Index1].Size := NewSize;

  Parts.DeleteRange(Index1+1, Index2-Index1);
  Result := True;
end;

end.
