unit uEditedData;

interface

uses
  System.Types, System.SysUtils, Generics.Collections, Math, Vcl.Forms,

  uDWHexTypes, uDWHexDataSources, uUtil;

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
    procedure CombineParts(Index1, Index2: Integer);
    function StartPartChange(Addr, Size: TFilePointer; InitContents: Boolean): TDataPart;
//    function FindPart(Addr: TFilePointer; var Index: Integer): Boolean;
    procedure DataChanged(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure DataInserted(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure DataDeleted(Addr: TFilePointer; Size: TFilePointer);
    procedure RecalcAddressesAfter(Addr: TFilePointer);
  public
    DataSource: TDWHexDataSource;
    Resizable: Boolean;
    OwnerEditor: TForm;  // TEditorForm
    Parts: TDataPartList;
    constructor Create(AOwnerEditor: TForm);
    destructor Destroy(); override;
    procedure ResetParts();
    procedure GetOverlappingParts(Addr, Size: TFilePointer; var Index1, Index2: Integer; {out} AParts: TDataPartList = nil); overload;
    procedure GetOverlappingParts(Addr, Size: TFilePointer; {out} AParts: TDataPartList); overload;
    function Get(Addr: TFilePointer; Size: TFilePointer; ZerosBeyondEoF: Boolean = False): TBytes;
    procedure Change(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure Insert(Addr: TFilePointer; Size: TFilePointer; const Value: PByteArray);
    procedure Delete(Addr: TFilePointer; Size: TFilePointer);
    function GetSize(): TFilePointer;
    function HasMovements(): Boolean;
  end;

implementation

uses
  uEditorForm;

{ TEditedData }

procedure TEditedData.Change(Addr: TFilePointer; Size: TFilePointer;
  const Value: PByteArray);
var
  Part: TDataPart;
begin
  if (not Resizable) and (Addr + Size > GetSize()) then
    Size := GetSize() - Addr;

  Part := StartPartChange(Addr, Size, False);
  Move(Value[0], Part.Data[Addr-Part.Addr], Size);

  DataChanged(Addr, Size, Value);
end;

constructor TEditedData.Create(AOwnerEditor: TForm);
begin
  inherited Create();
  OwnerEditor := AOwnerEditor;
  Parts := TObjectList<TDataPart>.Create(True);
end;

procedure TEditedData.DataChanged(Addr, Size: TFilePointer;
  const Value: PByteArray);
begin
  (OwnerEditor as TEditorForm).DataChanged(Addr, Size, Value);
end;

procedure TEditedData.DataDeleted(Addr, Size: TFilePointer);
begin
  RecalcAddressesAfter(Addr);
  (OwnerEditor as TEditorForm).DataDeleted(Addr, Size);
end;

procedure TEditedData.DataInserted(Addr, Size: TFilePointer;
  const Value: PByteArray);
begin
  RecalcAddressesAfter(Addr);
  (OwnerEditor as TEditorForm).DataInserted(Addr, Size, Value);
end;

procedure TEditedData.Delete(Addr: TFilePointer; Size: TFilePointer);
var
  i1, i2: Integer;
  AParts: TDataPartList;
begin
  if not Resizable then Exit;

  if (Addr < 0) or (Addr + Size > GetSize()) then
    raise Exception.Create('Trying to delete out of range');
  AParts := TDataPartList.Create(False);
  try
    GetOverlappingParts(Addr, Size, i1, i2, AParts);
    // Split blocks at start and end of selected region
    if SplitPart(i1, Addr) then
    begin
      Inc(i1);
      Inc(i2);
    end;
    SplitPart(i2, Addr + Size);

    // Delete covered parts
    Parts.DeleteRange(i1, i2-i1+1);

    // Combine adjusent parts of same type
    Dec(i1);
    if (i1 >= 0) and (i1 < Parts.Count-1) and (Parts[i1].PartType = Parts[i1+1].PartType) then
    begin
      if (Parts[i1].PartType = ptBuffer) or
         ((Parts[i1].PartType = ptSource) and (Parts[i1].SourceAddr + Parts[i1].Size = Parts[i1+1].SourceAddr)) then
        CombineParts(i1, i1+1);
    end;
  finally
    AParts.Free;
  end;

  DataDeleted(Addr, Size);
end;

destructor TEditedData.Destroy;
begin
  Parts.Free;
  inherited;
end;

//function TEditedData.FindPart(Addr: TFilePointer; var Index: Integer): Boolean;
//var
//  i: Integer;
//begin
//  for i:=0 to Parts.Count-1 do
//  begin
//    if Parts[i].Addr>Addr then
//    begin
//      Index := i;
//      Exit(False);
//    end;
//    if Parts[i].Addr+Parts[i].Size>Addr then
//    begin
//      Index := i;
//      Exit(True);
//    end;
//  end;
//  Index := Parts.Count;
//  Result := False;
//end;

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
  const Value: PByteArray);
var
  Part: TDataPart;
begin
  if not Resizable then Exit;

  Part := StartPartChange(Addr, 0, False);
  System.Insert(MakeBytes(Value[0], Size), Part.Data, Addr - Part.Addr);
  Part.Size := Part.Size + Size;

  DataInserted(Addr, Size, Value);
end;

procedure TEditedData.RecalcAddressesAfter(Addr: TFilePointer);
var
  i: Integer;
begin
  // There should be some better data structure that can handle this without full traverse
  for i:=1 to Parts.Count-1 do
  begin
    Parts[i].Addr := Parts[i-1].Addr + Parts[i-1].Size;
  end;
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

function TEditedData.StartPartChange(Addr, Size: TFilePointer; InitContents: Boolean): TDataPart;
// Find or create cached Part covering given range.
// May combine several old Parts that are overlapped by range.
var
  i1, i2: Integer;
  AParts: TDataPartList;
  Data: TBytes;
begin
  if (Addr < 0) or (Addr > GetSize()) then
    raise Exception.Create('Trying to change out of range');
  AParts := TDataPartList.Create(False);
  try
    GetOverlappingParts(Addr, Size, i1, i2, AParts);
    if AParts.Count = 0 then
    // On a part boundary or after end
    begin
      Result := TDataPart.Create();
      Result.PartType := ptBuffer;
      Result.Addr := Addr;
      Result.Size := Size;
      if InitContents then
        Result.Data := Get(Addr, Size, True)
      else
        Result.Data := MakeZeroBytes(Size);
      Parts.Insert(i1, Result);
    end
    else
    if (AParts.Count = 1) and (AParts[0].PartType = ptBuffer) then
    // Inside single loaded part - just return it
    begin
      Result := AParts[0];
      if (Result.Size < Size) then  // Part includes end of file
      begin
        SetLength(Result.Data, Size);
        FillChar(Result.Data[Result.Size], Size - Result.Size, 0);
        Result.Size := Size;
      end;
    end
    else
    // General case
    begin
      // Remember original content
      if InitContents then
        Data := Get(Addr, Size, True)
      else
        Data := MakeZeroBytes(Size);

      // Split blocks at start and end of selected region
      if SplitPart(i1, Addr) then
      begin
        Inc(i1);
        Inc(i2);
      end;
      SplitPart(i2, Addr + Size);

      if (Size = 0) then
      begin
        // Create zero-length part
        Result := TDataPart.Create();
        Result.PartType := ptBuffer;
        Result.Addr := Addr;
        Result.Size := 0;
        Parts.Insert(i1, Result);
      end
      else
      begin
        // Replace covered parts with one cached part
        Result := Parts[i1];
        Result.PartType := ptBuffer;
        Result.Size := Size;
        Result.Data := Data;

        Parts.DeleteRange(i1+1, i2-i1);
      end;

    end;

    // Combine adjusent cached parts
    i2 := i1;
    if (i1 > 0) and (Parts[i1-1].PartType = ptBuffer) then Dec(i1);
    if (i2 < Parts.Count-1) and (Parts[i2+1].PartType = ptBuffer) then Inc(i2);

    if i1<>i2 then
    begin
      CombineParts(i1, i2);
      Result := Parts[i1];
    end;
  finally
    AParts.Free;
  end;
//  HasUnsavedChanges := True;
end;


procedure TEditedData.CombineParts(Index1, Index2: Integer);
// Combine parts Index1 through Index2
var
  i: Integer;
  NewSize, Ptr: TFilePointer;
begin
  for i:=Index1+1 to Index2 do
    if Parts[i].PartType <> Parts[Index1].PartType then
      raise Exception.CreateFmt('Cannot combine parts %d (%d) and %d (%d)', [Index1, Ord(Parts[Index1].PartType), i, Ord(Parts[i].PartType)]);

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
end;

end.
