{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHextorDataSources;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.SysUtils, System.Types, System.Classes, Winapi.Windows, System.Math,

  uHextorTypes{, uLogFile};

type
  THextorDataSource = class;
  TCachedDataSource = class;
  TDataSourceProperty = (dspWritable, dspResizable);
  TDataSourceProperties = set of TDataSourceProperty;

  TDataCache = class
  // Read cache for slow data sources (like files)
  private
    DataSource: TCachedDataSource;
    ReadTime: Cardinal;
    Addr: TFilePointer;
    Buffer: TBytes;
  public
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer;
    procedure Flush();
    constructor Create(ADataSource: TCachedDataSource);
    destructor Destroy(); override;
  end;

  TSourceRegion = class
  // Native region of data source (e.g. allocated or free region of virtual memory).
  // If DataSource defines Regions, they must cover entire DataSource size without gaps.
  public
    Range: TFileRange;
    HasData: Boolean;
    Parent: TSourceRegion;
    HasChilds: Boolean;
    Description: string;
  end;

  TSourceRegionArray = TArray<TSourceRegion>;
  TSourceRegionArrayHelper = record helper for TSourceRegionArray
  public
    procedure Free;
  end;

  THextorDataSource = class
  private
    FPath, FDisplayName: string;
    function GetDisplayName: string; virtual;
  public
    property Path: string read FPath;
    property DisplayName: string read GetDisplayName;

    constructor Create(const APath: string); virtual;
    destructor Destroy(); override;
    procedure Open(Mode: Word); virtual; abstract;
    function GetProperties(): TDataSourceProperties; virtual;
    function CanBeSaved(): Boolean; virtual;
    function GetSize(): TFilePointer; virtual; abstract;
    procedure SetSize(NewSize: TFilePointer); virtual;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; virtual; abstract;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; virtual; abstract;
    procedure CopyContentFrom(Source: THextorDataSource); virtual;
    function GetRegions(const ARange: TFileRange): TSourceRegionArray; virtual;
  end;

  THextorDataSourceType = class of THextorDataSource;

  TCachedDataSource = class (THextorDataSource)
  protected
    Cache: TDataCache;
  public
    procedure Open(Mode: Word); override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; virtual; abstract;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; virtual; abstract;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
  end;

  TFileDataSource = class (TCachedDataSource)
  protected
    FileStream: TFileStream;
  public
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function CanBeSaved(): Boolean; override;
    function GetSize(): TFilePointer; override;
    procedure SetSize(NewSize: TFilePointer); override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    procedure CopyContentFrom(Source: THextorDataSource); override;
  end;

  TDiskDataSource = class (TFileDataSource)
  protected
    const SectorAlign = 512;
  public
    constructor Create(const APath: string); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    class function DiskDisplayName(const PathName, VolumeLabel: string): string;
  end;

  TProcMemDataSource = class (THextorDataSource)
  protected
    const PageSize = 4096;
    FSize: TFilePointer;
  public
    hProcess: Cardinal;
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    function GetRegions(const ARange: TFileRange): TSourceRegionArray; override;
  end;


implementation

uses
  Winapi.PsAPI;

{ THextorDataSource }

function THextorDataSource.CanBeSaved: Boolean;
// Is it possible to execute "Save" action for currently assigned data source
begin
  Result := dspWritable in GetProperties();
end;

procedure THextorDataSource.CopyContentFrom(Source: THextorDataSource);
const
  BlockSize = 10*MByte;
var
  Buf: TBytes;
  SourceSize, Pos: TFilePointer;
  Size: Integer;
begin
  SetLength(Buf, BlockSize);
  SourceSize := Source.GetSize();
  Pos := 0;

  while Pos < SourceSize do
  begin
    Size := Min(BlockSize, SourceSize - Pos);
    Source.GetData(Pos, Size, Buf[0]);
    ChangeData(Pos, Size, Buf[0]);
    Pos := Pos + Size;
  end;
end;

constructor THextorDataSource.Create(const APath: string);
begin
  inherited Create();
  FPath := APath;
end;

destructor THextorDataSource.Destroy;
begin

  inherited;
end;

function THextorDataSource.GetDisplayName: string;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := FPath;
end;

function THextorDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [];
end;

function THextorDataSource.GetRegions(
  const ARange: TFileRange): TSourceRegionArray;
begin
  Result := nil;
end;

procedure THextorDataSource.SetSize(NewSize: TFilePointer);
begin
  //raise Exception.Create('Size change not supported');
end;

{ TFileDataSource }

function TFileDataSource.CanBeSaved: Boolean;
begin
  // False if it is just a new empty "file" and not assigned with actual file on disk
  Result := (inherited) and (ExtractFilePath(Path) <> '');
end;

procedure TFileDataSource.CopyContentFrom(Source: THextorDataSource);
// Called on closed dest
//var
//  Dir: string;
begin
//  if (Source is TFileDataSource) and (ExtractFilePath((Source as TFileDataSource).Path) <> '') then
//  begin
////    ForceDirectories(ExtractFilePath(Path));
////    CopyFile(PChar(Source.Path), PChar(Path), False);
//    if (Source as TFileDataSource).FileStream <> nil then
//    begin
//      FileStream.Position := 0;
//      FileStream.CopyFrom((Source as TFileDataSource).FileStream, -1);
//    end;
//  end
//  else
    inherited;
end;

constructor TFileDataSource.Create(const APath: string);
begin
  inherited;
end;

destructor TFileDataSource.Destroy;
begin
  FileStream.Free;
  inherited;
end;

function TFileDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable, dspResizable];
end;

function TFileDataSource.GetSize: TFilePointer;
begin
  if FileStream <> nil then
    Result := FileStream.Size
  else
    Result := 0;
end;

function TFileDataSource.InternalChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  FileStream.Position := Addr;
  //FileStream.WriteBuffer(Data, Size);
  if FileStream.Write(Data, Size) <> Size then
    RaiseLastOSError();
  Result := Size;
end;

function TFileDataSource.InternalGetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
begin
  if FileStream = nil then
    Exit(0);

//  StartTimeMeasure();

  FileStream.Position := Addr;
  //Result := FileStream.Read(Data, Size);
  FileStream.ReadBuffer(Data, Size);
  Result := Size;

//  EndTimeMeasure('FileRead:', True);
end;

procedure TFileDataSource.Open(Mode: Word);
begin
  if ExtractFilePath(Path) <> '' then
  begin
    if Mode = fmCreate then
      ForceDirectories(ExtractFilePath(Path));
    FreeAndNil(FileStream);
    if Mode = fmOpenRead then
      Mode := Mode or fmShareDenyNone
    else
      Mode := Mode or fmShareExclusive;
    FileStream := TFileStream.Create(Path, Mode);
  end;
end;

procedure TFileDataSource.SetSize(NewSize: TFilePointer);
begin
  if FileStream <> nil then
    FileStream.Size := NewSize;
end;

{ TDiskDataSource }

function TDiskDataSource.InternalChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
var
  Ptr1, Ptr2: TFilePointer;
  Buf: TBytes;
begin
  if FileStream = nil then
    Exit(0);

  // Drive read/write operations must be aligned by 512 bytes
  if (Addr mod SectorAlign = 0) and (Size mod SectorAlign = 0) then
  begin
    Result := inherited InternalChangeData(Addr, Size, Data);
    Exit;
  end;

  // If not aligned - read, partial modify, write
  Ptr1 := (Addr div SectorAlign) * SectorAlign;
  Ptr2 := ((Addr + Size - 1) div SectorAlign + 1) * SectorAlign;
  SetLength(Buf, Ptr2 - Ptr1);

  inherited GetData(Ptr1, Ptr2 - Ptr1, Buf[0]);
  Move(Data, Buf[Addr - Ptr1], Size);

  inherited InternalChangeData(Ptr1, Ptr2 - Ptr1, Buf[0]);

  Result := Size;
end;

constructor TDiskDataSource.Create(const APath: string);
var
  Buf: array[0..1023] of Char;
  Len, m, f: Cardinal;
  PathName, VolumeLabel: string;
begin
  inherited;
  if APath.StartsWith('\\?\Volume') then
  begin
    // Drive letter (we use first item from PathNames)
    if GetVolumePathNamesForVolumeName(PChar(IncludeTrailingPathDelimiter(APath)), @Buf, Length(Buf), Len) then
      PathName := ExcludeTrailingPathDelimiter(PChar(@Buf))
    else
      PathName := '';
    // Volume label
    if GetVolumeInformation(PChar(IncludeTrailingPathDelimiter(APath)), @Buf, Length(Buf), nil, m, f, nil, 0) then
      VolumeLabel := PChar(@Buf)
    else
      VolumeLabel := '';
    FDisplayName := DiskDisplayName(PathName, VolumeLabel);
  end;
end;

function TDiskDataSource.InternalGetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
var
  Ptr1, Ptr2: TFilePointer;
  Buf: TBytes;
begin
  if FileStream = nil then
    Exit(0);

  // Drive read/write operations must be aligned by 512 bytes
  Ptr1 := (Addr div SectorAlign) * SectorAlign;
  Ptr2 := ((Addr + Size - 1) div SectorAlign + 1) * SectorAlign;
  SetLength(Buf, Ptr2 - Ptr1);

  inherited InternalGetData(Ptr1, Ptr2 - Ptr1, Buf[0]);
  Move(Buf[Addr - Ptr1], Data, Size);

  Result := Size;
end;

class function TDiskDataSource.DiskDisplayName(const PathName,
  VolumeLabel: string): string;
// Combine drive letter and label like "Windows (C:)"
begin
  if VolumeLabel = '' then
    Result := PathName
  else
  begin
    Result := VolumeLabel;
    if PathName <> '' then
      Result := Result + ' (' + PathName + ')';
  end;
end;

function TDiskDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable];
end;

function TDiskDataSource.GetSize: TFilePointer;
var
  Ret: Cardinal;
begin
  if not DeviceIoControl(FileStream.Handle, IOCTL_DISK_GET_LENGTH_INFO,
                         nil, 0, @Result, SizeOf(Result), Ret, nil) then
    Result := 0;
end;

procedure TDiskDataSource.Open(Mode: Word);
var
  Ret: Cardinal;
begin
  FreeAndNil(FileStream);
  if Mode = fmCreate then
    Mode := fmOpenReadWrite;
  if Mode = fmOpenRead then
    Mode := Mode or fmShareDenyNone
  else
    Mode := Mode or fmShareExclusive;
  FileStream := TFileStream.Create(Path, Mode);

  // Allow access to last sectors of volume
  DeviceIoControl(FileStream.Handle, FSCTL_ALLOW_EXTENDED_DASD_IO,
                  nil, 0, nil, 0, Ret, nil);
end;

{ TProcMemDataSource }

function TProcMemDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  Win32Check( Bool( WriteProcessMemory(hProcess, Pointer(Addr), @Data, Size, NativeUInt(nil^)) ) );
  Result := Size;
end;

constructor TProcMemDataSource.Create(const APath: string);
begin
  inherited;
end;

destructor TProcMemDataSource.Destroy;
begin
  if hProcess <> 0 then
    CloseHandle(hProcess);
  inherited;
end;

function TProcMemDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
var
  Res: Boolean;
  PtrStart, PtrEnd: TFilePointer;
  Dest: Pointer;
begin
  // Read by pages, because partially available range fails
  PtrStart := Addr;
  PtrEnd := Addr;
  while PtrEnd < Addr + Size do
  begin
    PtrEnd := Min(NextAlignBoundary(PtrStart + 1, PageSize), Addr + Size);
    Dest := @(PByteArray(@Data)[PtrStart - Addr]);
    Res := Bool( ReadProcessMemory(hProcess, Pointer(PtrStart), Dest, PtrEnd - PtrStart, NativeUInt(nil^)) );
    if not Res then
      ZeroMemory(Dest, PtrEnd - PtrStart);
    PtrStart := PtrEnd;
  end;
  Result := Size;
end;

function TProcMemDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable];
end;

function TProcMemDataSource.GetRegions(
  const ARange: TFileRange): TSourceRegionArray;
// Returns a list of allocated and free regions of process virtual address space.
// Warning! If requested range starts not from 0, first returned region is uncomplete;
// it starts on nearest page boundary
var
  mbi: MEMORY_BASIC_INFORMATION;
//  si: SYSTEM_INFO;
  lpMem: Pointer;
  fn: string;
  len: Cardinal;
  Region, ParentRegion: TSourceRegion;
begin
//  StartTimeMeasure();

  Result := nil;
  try
    // Get maximum address range from system info
  //  GetSystemInfo(si);
    ParentRegion := nil;
    lpMem := Pointer(ARange.Start);
    while (ARange = EntireFile) or (UIntPtr(lpMem) < ARange.AEnd) do
    begin
      if VirtualQueryEx(hProcess, lpMem, mbi, SizeOf(mbi)) = 0 then
      begin
        Break;
  //      RaiseLastOSError();
      end;

      SetLength(fn, 1024);

      len := GetMappedFileName(hProcess, mbi.BaseAddress, @fn[Low(fn)], Length(fn));
      if len > 0 then
        fn := Copy(fn, Low(fn), len)
      else
        fn := '';

  //    WriteLogFmt('MemRegions', '%x / %x Sz: %d State: %x Type: %x fn: %s', [UIntPtr(mbi.BaseAddress), UIntPtr(mbi.AllocationBase), mbi.RegionSize, mbi.State, mbi.Type_9, fn]);
      Region := TSourceRegion.Create();
      Region.Range := TFileRange.Create(UIntPtr(mbi.BaseAddress), UIntPtr(mbi.BaseAddress) + mbi.RegionSize);
      Region.HasData := (mbi.State <> MEM_FREE);
      Region.HasChilds := False;
      case mbi.State of
        MEM_COMMIT:
          begin
            case mbi.Type_9 of
              MEM_IMAGE:   Region.Description := 'Image';
              MEM_MAPPED:  Region.Description := 'Mapped';
              MEM_PRIVATE: Region.Description := 'Private';
              else         Region.Description := 'Commit';
            end;
          end;
        MEM_RESERVE:       Region.Description := 'Reserved';
        MEM_FREE:          Region.Description := 'Free';
      end;
      if fn <> '' then
        Region.Description := Region.Description + ': ' + fn;

      if (mbi.State <> MEM_FREE) then
      begin
        // Create parent region for regions sharing same allocation base
        if (ParentRegion = nil) or (UIntPtr(mbi.AllocationBase) <> ParentRegion.Range.Start) then
        begin
          ParentRegion := TSourceRegion.Create();
          ParentRegion.Range := TFileRange.Create(UIntPtr(mbi.AllocationBase), UIntPtr(mbi.AllocationBase));
          ParentRegion.HasData := True;
          ParentRegion.HasChilds := True;
          ParentRegion.Description := Region.Description;

          Result := Result + [ParentRegion];
        end;
        Region.Parent := ParentRegion;
        ParentRegion.Range.AEnd := Max(ParentRegion.Range.AEnd, UIntPtr(mbi.BaseAddress) + mbi.RegionSize);
      end;

      Result := Result + [Region];

      // increment lpMem to next region of memory
      lpMem := Pointer(UIntPtr(mbi.BaseAddress) + UIntPtr(mbi.RegionSize));
    end;
  except
    Result.Free;
    raise;
  end;

//  EndTimeMeasure('Mem.GetRegions', True);
end;

function TProcMemDataSource.GetSize: TFilePointer;
begin
  Result := FSize;
end;

procedure TProcMemDataSource.Open(Mode: Word);
var
  ProcID, Access: Cardinal;
  Regions: TSourceRegionArray;
begin
  ProcID := StrToInt(Path);
  if hProcess <> 0 then  CloseHandle(hProcess);
  case Mode of
    fmCreate, fmOpenReadWrite: Access := PROCESS_ALL_ACCESS;
    else Access := PROCESS_VM_READ or PROCESS_QUERY_INFORMATION;
  end;
  hProcess := OpenProcess(Access, false, ProcID);
  Win32Check(Bool(hProcess));

  Regions := GetRegions(TFileRange.Create(0, High(TFilePointer)));
  if Length(Regions) > 0 then
    FSize := Regions[High(Regions)].Range.AEnd
  else
    FSize := 0;
  Regions.Free;
end;

{ TDataCache }

function TDataCache.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  Flush();
  Result := DataSource.InternalChangeData(Addr, Size, Data);
end;

constructor TDataCache.Create(ADataSource: TCachedDataSource);
begin
  inherited Create();
  DataSource := ADataSource;
end;

destructor TDataCache.Destroy;
begin
  Flush();
  inherited;
end;

procedure TDataCache.Flush;
begin
  Addr := -1;
  Buffer := nil;
end;

function TDataCache.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
const
  ReadAlign = 512;
  MaxReadAhead = 1*MByte;
  CachingTime = 1000;  // Milliseconds
var
  ReadStart, ReadSize, SuggestedReadSize, SourceSize: TFilePointer;
//  t: Cardinal;
begin
//  WriteLog('Cache', 'Get:  ' + IntToStr(Addr) + ' ' + IntToStr(Size));
  // Cache missed?
  if (Self.Addr < 0) or (GetTickCount() - ReadTime > CachingTime) or
     (Addr < Self.Addr) or (Addr + Size > Self.Addr + Length(Buffer)) then
  begin
    // Align read range
    ReadStart := (Addr div ReadAlign) * ReadAlign;
    ReadSize := NextAlignBoundary(Addr + Size, ReadAlign) - ReadStart;
    // If reading sequential blocks, increase read size with each read
    if (ReadStart <= Self.Addr + Length(Buffer)) and (ReadStart + ReadSize > Self.Addr + Length(Buffer)) then
    begin
      SuggestedReadSize := Min(NextAlignBoundary(Length(Buffer) * 2, ReadAlign), MaxReadAhead);
      if ReadSize < SuggestedReadSize  then
        ReadSize := SuggestedReadSize;
    end;
    SourceSize := DataSource.GetSize();
    if ReadStart + ReadSize > SourceSize then
      ReadSize := SourceSize - ReadStart;
    // Read from source
    SetLength(Buffer, ReadSize);
//    t := GetTickCount();
    DataSource.InternalGetData(ReadStart, ReadSize, Buffer[0]);
//    t := GetTickCount() - t;
    Self.Addr := ReadStart;
    ReadTime := GetTickCount();
//    WriteLogFmt('Cache', 'Read: %d %d (%d ms)', [ReadStart, ReadSize, t]);
  end;

  // Return data from cache
  Move(Buffer[Addr - Self.Addr], Data, Size);
  Result := Size;
end;

{ TCachedDataSource }

function TCachedDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  Result := Cache.ChangeData(Addr, Size, Data);
end;

constructor TCachedDataSource.Create(const APath: string);
begin
  inherited;
  Cache := TDataCache.Create(Self);
end;

destructor TCachedDataSource.Destroy;
begin
  Cache.Free;
  inherited;
end;

function TCachedDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
begin
  Result := Cache.GetData(Addr, Size, Data);
end;

procedure TCachedDataSource.Open(Mode: Word);
begin
  Cache.Flush();
  inherited;
end;

{ TSourceRegionArrayHelper }

procedure TSourceRegionArrayHelper.Free;
var
  i: Integer;
begin
  for i := 0 to Length(Self)-1 do
    Self[i].Free;
end;

end.
