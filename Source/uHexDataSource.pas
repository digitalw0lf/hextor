{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2023  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHexDataSource;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.SysUtils, System.Classes, System.Types, Generics.Collections,
  Generics.Defaults, System.IOUtils, System.Math,

  uHextorTypes, uHextorDataSources;

type
  THexDataSource = class (THextorDataSource)
  // Intel Hex, Motorola S-Record etc. hex files
  protected type
    THexFileFormat = (hffUnknown, hffIntel, hffMotorola);
    THexFileRecordType = (hfrAddr, hfrData, hfrExecAddr);
    THexFileRecord = class
      _type: THexFileRecordType;
      Addr: TFilePointer;
      Data: TBytes;
      function AEnd(): TFilePointer;
    end;
  protected
    SrcFormat: THexFileFormat;
    Recs: TObjectList<THexFileRecord>;
    Modified: Boolean;
    class function CalcIntelChecksum(const Buf: PByteArray; Size: Integer): Byte;
    procedure LoadIntel(const Lines: TStringDynArray);
    procedure LoadMotorola(const Lines: TStringDynArray);
    procedure SortRecs();
    procedure Save(const FileName: string);
    procedure SaveIntel(var Lines: TStringDynArray);
    procedure SaveMotorola(var Lines: TStringDynArray);
  public
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
    procedure Open(Mode: Word; Share: Word = 0); override;
    procedure Close(); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    function GetRegions(const ARange: TFileRange): TSourceRegionArray; override;
  end;

implementation

{ THexDataSource }

class function THexDataSource.CalcIntelChecksum(const Buf: PByteArray; Size: Integer): Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    Inc(Result, Buf^[i]);
  Result := (not Result) + 1;
end;

function THexDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
var
  ARange: TFileRange;
  i: Integer;
  Rec: THexFileRecord;
  Range: TFileRange;
begin
  ARange.Start := Addr;
  ARange.Size := Size;
  Result := 0;
  for i := 0 to Recs.Count - 1 do
  begin
    Rec := Recs[i];
    if (Rec._type = hfrData) then
    begin
      Range := ARange.Intersect(Rec.Addr, Rec.AEnd);
      if Range.Size > 0 then
      begin
        Move(PByteArray(@Data)^[Range.Start - Addr], Rec.Data[Range.Start - Rec.Addr], Range.Size);
        Inc(Result, Range.Size);
      end;
    end;
  end;
end;

procedure THexDataSource.Close;
begin
  inherited;
  if Recs <> nil then
  begin
    if Modified then
    begin
  //--//
      // Save entire file when closing
  //    Save(Path);
  //--//
      Modified := False;
    end;
    Recs.Clear();
  end;
end;

constructor THexDataSource.Create(const APath: string);
begin
  inherited;
  Recs := TObjectList<THexFileRecord>.Create(True);
end;

destructor THexDataSource.Destroy;
begin
  FreeAndNil(Recs);
  inherited;
end;

function THexDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
var
  ARange: TFileRange;
  i: Integer;
  Rec: THexFileRecord;
  Range: TFileRange;
begin
  ARange.Start := Addr;
  ARange.Size := Size;
  FillChar(Data, Size, 0);
  for i := 0 to Recs.Count - 1 do
  begin
    Rec := Recs[i];
    if (Rec._type = hfrData) then
    begin
      Range := ARange.Intersect(Rec.Addr, Rec.AEnd);
      if Range.Size > 0 then
      begin
        Move(Rec.Data[Range.Start - Rec.Addr], PByteArray(@Data)^[Range.Start - Addr], Range.Size);
      end;
    end;
  end;
  Result := Size;
end;

function THexDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [{dspWritable,} dspHasRegions];
end;

function THexDataSource.GetRegions(
  const ARange: TFileRange): TSourceRegionArray;

  procedure AddRegion(const Range: TFileRange; HasData: Boolean);
  var
    Region: TSourceRegion;
  begin
    if not Range.Intersects(ARange) then Exit;
    if (Result <> nil) then
    begin
      Region := Result[High(Result)];
      if (Region.HasData = HasData) and (Region.Range.AEnd = Range.Start) then
      begin
        Region.Range.AEnd := Range.AEnd;
        Exit;
      end;
    end;
    Region := TSourceRegion.Create();
    Region.Range := Range;
    Region.HasData := HasData;
    if not HasData then
      Region.Description := 'Undefined';
    Result := Result + [Region];
  end;

var
  i: Integer;
  R: TFileRange;
  APtr: TFilePointer;
begin
  Result := nil;
  APtr := 0;
  for i := 0 to Recs.Count - 1 do
  if Recs[i]._type = hfrData then
  begin
    R.Start := APtr;
    R.AEnd := Recs[i].Addr;
    if R.Size > 0 then
      AddRegion(R, False);
    R.Start := Recs[i].Addr;
    R.AEnd := Recs[i].AEnd;
    AddRegion(R, True);
    APtr := R.AEnd;
  end;
end;

function THexDataSource.GetSize: TFilePointer;
begin
  if Recs.Count > 0 then Result := Recs.Last.AEnd
                    else Result := 0;
end;

{$IF Defined(CPUX64)}

function FastSwap(Value: LongWord): LongWord; register; overload;
asm
  bswap ecx
  mov eax, ecx
end;

function FastSwap(Value: Word): Word; register; overload;
asm
  xchg cl, ch
  mov ax, cx
end;

{$ENDIF CPUX64}

procedure THexDataSource.LoadIntel(const Lines: TStringDynArray);
var
  i, j: Integer;
  s: string;
  Buf: TBytes;
  ByteCount: Byte;
  RelAddr: Word;
  RecType: Byte;
  Checksum: Byte;
  BaseAddr: Cardinal;
  Rec: THexFileRecord;
begin
  BaseAddr := 0;
  for i := 0 to Length(Lines) - 1 do
  begin
    j := Pos(':', Lines[i]);
    if j = 0 then Continue;
    s := Trim(Copy(Lines[i], j + 1, MaxInt));
    Buf := Hex2Data(s);
    if Length(Buf) < 5 then Continue;  // TODO: Show format warnings
    ByteCount := Buf[0];
    if ByteCount > Length(Buf) + 5 then Continue;
    RelAddr := (Buf[1] shl 8) + Buf[2];
    RecType := Buf[3];
    Checksum := Buf[ByteCount + 4];
    if Checksum <> CalcIntelChecksum(@Buf[0], ByteCount + 4) then
    begin
      // TODO: Show warning
    end;

    case RecType of
      0: if ByteCount > 0 then  // Data
         begin
           Rec := THexFileRecord.Create();
           Rec._type := hfrData;
           Rec.Addr := BaseAddr + RelAddr;
           Rec.Data := Copy(Buf, 4, ByteCount);
           Recs.Add(Rec);
         end;
      1: begin  // End Of File
         end;
      2: begin  // Extended Segment Address
           if ByteCount < 2 then Continue;
           BaseAddr := Cardinal(FastSwap(PWord(@Buf[4])^)) shl 4;

           Rec := THexFileRecord.Create();
           Rec._type := hfrAddr;
           Rec.Addr := BaseAddr;
           Recs.Add(Rec);
         end;
      3: begin  // Start Segment Address
           // TODO
         end;
      4: begin  // Extended Linear Address
           if ByteCount < 2 then Continue;
           BaseAddr := Cardinal(FastSwap(PWord(@Buf[4])^)) shl 16;

           Rec := THexFileRecord.Create();
           Rec._type := hfrAddr;
           Rec.Addr := BaseAddr;
           Recs.Add(Rec);
         end;
      5: begin  // Start Linear Address
           // TODO
         end;
    end;
  end;
  SortRecs();
end;

procedure THexDataSource.LoadMotorola(const Lines: TStringDynArray);
begin
  // TODO
end;

procedure THexDataSource.Open(Mode, Share: Word);
var
  //Buf: TBytes;
  Lines: TStringDynArray;
begin
  inherited;
  //Buf := TFile.ReadAllBytes(Path);
  Lines := TFile.ReadAllLines(Path, TEncoding.ANSI);
  if Lines = nil then Exit;
  if Lines[0].StartsWith(':') then
  begin
    SrcFormat := hffIntel;
    LoadIntel(Lines);
  end
  else
  if Lines[0].StartsWith('S') then
  begin
    SrcFormat := hffMotorola;
    LoadMotorola(Lines);
  end
  else
  begin
    SrcFormat := hffUnknown;
    raise Exception.Create('Unknown file format');
  end;
end;

procedure THexDataSource.Save(const FileName: string);
var
  Lines: TStringDynArray;
begin
  case SrcFormat of
    hffIntel: SaveIntel(Lines);
    hffMotorola: SaveMotorola(Lines);
    else Exit;
  end;
  TFile.WriteAllLines(FileName, Lines, TEncoding.ANSI);
end;

procedure THexDataSource.SaveIntel(var Lines: TStringDynArray);
var
  i: Integer;
  Rec: THexFileRecord;
  Buf: TBytes;
  Adr: Word;
  BaseAddr: Cardinal;
begin
  Lines := nil;
  BaseAddr := 0;
  for i := 0 to Recs.Count - 1 do
  begin
    Rec := Recs[i];
    case Rec._type of
      hfrAddr:
        begin
          Adr := FastSwap(Rec.Addr shr 16);
          Buf := [2, 0, 0, 4] + MakeBytes(Adr, 2);
          Buf := Buf + [CalcIntelChecksum(@Buf[0], Length(Buf))];
          Lines := Lines + [':' + Data2Hex(Buf, False)];
          BaseAddr := Rec.Addr;
        end;
      hfrData:
        begin
          Adr := FastSwap(Rec.Addr - BaseAddr);
          Buf := [Length(Rec.Data)] + MakeBytes(Adr, 2) + [0] + Rec.Data;
          Buf := Buf + [CalcIntelChecksum(@Buf[0], Length(Buf))];
          Lines := Lines + [':' + Data2Hex(Buf, False)];
        end;
    end;
  end;
end;

procedure THexDataSource.SaveMotorola(var Lines: TStringDynArray);
begin

end;

procedure THexDataSource.SortRecs;
begin
  Recs.Sort(TComparer<THexFileRecord>.Construct(function(const Left, Right: THexFileRecord): Integer begin
    Result := CompareValue(Left.Addr, Right.Addr);
    if Result = 0 then
      Result := CompareValue(Ord(Left._type), Ord(Right._type));
  end));
end;

{ THexDataSource.THexFileRecord }

function THexDataSource.THexFileRecord.AEnd: TFilePointer;
begin
  Result := Addr + Length(Data);
end;

end.
