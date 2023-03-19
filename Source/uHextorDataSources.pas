{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2023  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
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
  TDataSourceProperty = (dspWritable, dspResizable, dspHasRegions);
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
  // Base class for all data sources
  private
    FPath, FDisplayName: string;
    function GetDisplayName: string; virtual;
  public
    property Path: string read FPath;
    property DisplayName: string read GetDisplayName;

    constructor Create(const APath: string); virtual;
    destructor Destroy(); override;
    procedure Open(Mode: Word; Share: Word = 0); virtual;
    procedure Close(); virtual; abstract;
    function GetProperties(): TDataSourceProperties; virtual;
    function CanBeSaved(): Boolean; virtual;
    function GetSize(): TFilePointer; virtual; abstract;
    procedure SetSize(NewSize: TFilePointer); virtual;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; virtual; abstract;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; virtual; abstract;
    function GetRegions(const ARange: TFileRange): TSourceRegionArray; virtual;
  end;

  THextorDataSourceType = class of THextorDataSource;

  TCachedDataSource = class (THextorDataSource)
  // Base class for slow datasources which need caching
  protected
    Cache: TDataCache;
  public
    procedure Close(); override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; virtual; abstract;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; virtual; abstract;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
  end;

  TFileDataSource = class (TCachedDataSource)
  // Handle-based data sources
  protected
    FileStream: TFileStream;
  public
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
    procedure Open(Mode: Word; Share: Word = 0); override;
    procedure Close(); override;
    function GetProperties(): TDataSourceProperties; override;
    function CanBeSaved(): Boolean; override;
    function GetSize(): TFilePointer; override;
    procedure SetSize(NewSize: TFilePointer); override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
  end;

  TDiskDataSource = class (TFileDataSource)
  protected
    const SectorAlign = 512;
  public
    constructor Create(const APath: string); override;
    procedure Open(Mode: Word; Share: Word = 0); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function InternalGetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function InternalChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    class function DiskDisplayName(const PathName, VolumeLabel: string): string;
  end;

  TProcMemDataSource = class (THextorDataSource)
  // Process memory
  protected
    const PageSize = 4096;
  protected
    FSize: TFilePointer;
  public
    hProcess: Cardinal;
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

  TRandGenDataSource = class (THextorDataSource)
  // Emulate large data source with random generated data
  protected
    const PageSize = 32*1024;
  protected
    FTextual: Boolean;
    FSize: TFilePointer;
    CachedPageAddr: TFilePointer;
    CachedPage: TBytes;
    procedure GenPage(Addr: TFilePointer);
  public
    constructor Create(const APath: string); override;
    destructor Destroy(); override;
    procedure Open(Mode: Word; Share: Word = 0); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    function GetRegions(const ARange: TFileRange): TSourceRegionArray; override;
  end;

function SameDataSource(DataSource1: THextorDataSource; DataSourceType2: THextorDataSourceType; const Path2: string): Boolean;

implementation

uses
  Winapi.PsAPI;

function SameDataSource(DataSource1: THextorDataSource; DataSourceType2: THextorDataSourceType; const Path2: string): Boolean;
begin
  Result := (THextorDataSourceType(DataSource1.ClassType) = DataSourceType2) and
            (SameFileName(DataSource1.Path, Path2));
end;

{ THextorDataSource }

function THextorDataSource.CanBeSaved: Boolean;
// Is it possible to execute "Save" action for currently assigned data source
begin
  Result := dspWritable in GetProperties();
end;


constructor THextorDataSource.Create(const APath: string);
begin
  inherited Create();
  FPath := APath;
end;

destructor THextorDataSource.Destroy;
begin
  Close();
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

procedure THextorDataSource.Open(Mode, Share: Word);
begin
  // Close current source (if any) before opening new one.
  // Descendants should call "inherited" in the beginning of Open() method.
  Close();
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

procedure TFileDataSource.Close;
begin
  inherited;
  FreeAndNil(FileStream);
end;

constructor TFileDataSource.Create(const APath: string);
begin
  inherited;
end;

destructor TFileDataSource.Destroy;
begin
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
  if Size <= 0 then
    Exit(0);

//  StartTimeMeasure();

  FileStream.Position := Addr;
  Result := FileStream.Read(Data, Size);
  if Result = 0 then
    RaiseLastOSError();
  if Result < Size then
    FillChar(PByteArray(@Data)[Result], Size - Result, 0);
//  FileStream.ReadBuffer(Data, Size);
//  Result := Size;

//  EndTimeMeasure('FileRead:', True);
end;

procedure TFileDataSource.Open(Mode: Word; Share: Word = 0);
begin
  inherited;
  if ExtractFilePath(Path) <> '' then
  begin
    if Mode = fmCreate then
      ForceDirectories(ExtractFilePath(Path));
    FileStream := TFileStream.Create(Path, Mode or Share);
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

procedure TDiskDataSource.Open(Mode: Word; Share: Word = 0);
var
  Ret: Cardinal;
begin
  // We don't call inherited Open() here, but explicitly call Close().
  Close();

  if Mode = fmCreate then
    Mode := fmOpenReadWrite;
  FileStream := TFileStream.Create(Path, Mode or Share);

  // Allow access to last sectors of volume
  DeviceIoControl(FileStream.Handle, FSCTL_ALLOW_EXTENDED_DASD_IO,
                  nil, 0, nil, 0, Ret, nil);
end;

{ TProcMemDataSource }

function TProcMemDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
var
  oldprotect, oldprotect2: DWORD;
begin
  VirtualProtectEx(hProcess, Pointer(Addr), Size, PAGE_EXECUTE_READWRITE, oldprotect);
  try
    Win32Check( Bool( WriteProcessMemory(hProcess, Pointer(Addr), @Data, Size, NativeUInt(nil^)) ) );
  finally
    VirtualProtectEx(hProcess, Pointer(Addr), Size, oldprotect, oldprotect2);
  end;
  Result := Size;
end;

procedure TProcMemDataSource.Close;
begin
  if hProcess <> 0 then
  begin
    CloseHandle(hProcess);
    hProcess := 0;
  end;
end;

constructor TProcMemDataSource.Create(const APath: string);
begin
  inherited;
end;

destructor TProcMemDataSource.Destroy;
begin
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
  Result := [dspWritable, dspHasRegions];
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
        ParentRegion.Range.AEnd := Max(ParentRegion.Range.AEnd, IntPtr(mbi.BaseAddress) + Int64(mbi.RegionSize));
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

procedure TProcMemDataSource.Open(Mode: Word; Share: Word = 0);
var
  ProcID, Access: Cardinal;
  Regions: TSourceRegionArray;
  i: Integer;
begin
  inherited;
  i := Pos(' ', Path);
  if i = 0 then i := Length(Path) + 1;
  ProcID := StrToInt(Copy(Path, Low(Path), i - 1));
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
  CachingTime = 950;  // Milliseconds
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
    SetLength(Buffer, 0);  // Discard and zero old content
    SetLength(Buffer, ReadSize);
//    t := GetTickCount();
    DataSource.InternalGetData(ReadStart, ReadSize, Buffer[0]);
//    t := GetTickCount() - t;
    Self.Addr := ReadStart;
    ReadTime := GetTickCount();
//    WriteLogFmt('Cache', 'Read: %d %d (%d ms)', [ReadStart, ReadSize, t]);
  end;

  if Addr + Size > Self.Addr + Length(Buffer) then
  begin
    // This may be in case when file size decreased, but caller does not knows this yet
    FillChar(Data, Size, 0);
    Size := Max(Self.Addr + Length(Buffer) - Addr, 0);
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

procedure TCachedDataSource.Close;
begin
  inherited;
  if Cache <> nil then
    Cache.Flush();
end;

constructor TCachedDataSource.Create(const APath: string);
begin
  inherited;
  Cache := TDataCache.Create(Self);
end;

destructor TCachedDataSource.Destroy;
begin
  FreeAndNil(Cache);
  inherited;
end;

function TCachedDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
begin
  Result := Cache.GetData(Addr, Size, Data);
end;

{ TSourceRegionArrayHelper }

procedure TSourceRegionArrayHelper.Free;
var
  i: Integer;
begin
  for i := 0 to Length(Self)-1 do
    Self[i].Free;
end;

{ TRandGenDataSource }

function TRandGenDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  // Not implemented
  Result := 0;
end;

constructor TRandGenDataSource.Create(const APath: string);
var
  s: TArray<string>;
begin
  inherited;
  s := APath.Split(['_']);
  FTextual := (s[0] = 't');
  FSize := Str2FileSize(s[1]);
  if (FSize < 0) then
    raise Exception.Create('Invalid file size');
  CachedPageAddr := -PageSize;
end;

destructor TRandGenDataSource.Destroy;
begin

  inherited;
end;

procedure TRandGenDataSource.GenPage(Addr: TFilePointer);
const
  Words: array[0..107] of AnsiString = (
    'the ', 'be ', 'to ', 'of ', 'and ', 'a ', 'in ', 'that ', 'have ', 'I ', 'it ', 'for ', 'not ', 'on ', 'with ', 'he ', 'as ', 'you ', 'do ', 'at ', 'this ', 'but ', 'his ', 'by ', 'from ', 'they ', 'we ', 'say ',
    'her ', 'she ', 'or ', 'an ', 'will ', 'my ', 'one ', 'all ', 'would ', 'there ', 'their ', 'what ', 'so ', 'up ', 'out ', 'if ', 'about ', 'who ', 'get ', 'which ', 'go ', 'me ', 'when ', 'make ', 'can ', 'like ',
    'time ', 'no ', 'just ', 'him ', 'know ', 'take ', 'people ', 'into ', 'year ', 'your ', 'good ', 'some ', 'could ', 'them ', 'see ', 'other ', 'than ', 'then ', 'now ', 'look ', 'only ', 'come ', 'its ', 'over ',
    'think ', 'also ', 'back ', 'after ', 'use ', 'two ', 'how ', 'our ', 'work ', 'first ', 'well ', 'way ', 'even ', 'new ', 'want ', 'because ', 'any ', 'these ', 'give ', 'day ', 'most ', 'us ',
    ', ', '. ', '! ', #13#10,
    ', ', '. ', '! ', #13#10
  );
var
  i, n: Integer;
  s1: AnsiString;
begin
  if Addr = CachedPageAddr then Exit;
  if Length(CachedPage) <> PageSize then
    SetLength(CachedPage, PageSize);
  if FTextual then
  begin
    RandSeed := Addr div PageSize;
    i := 0;
    while i < PageSize do
    begin
      n := Random(Length(Words));
      Move(Words[n][Low(AnsiString)], CachedPage[i], Min(Length(Words[n]), PageSize - i));
      Inc(i, Length(Words[n]));
    end;
  end
  else
  begin
    for i := 0 to PageSize div 4 - 1 do
    begin
      PIntegerArray(@CachedPage[0])[i] := i * (i mod 256) * (Addr div 1024 + 1) + Addr;
    end;
  end;
  s1 := AnsiString('-------- ADDR  ' + IntToHex(Addr, 16) + ' --------');
  Move(s1[Low(s1)], CachedPage[0], Length(s1));
  CachedPageAddr := Addr;
end;

function TRandGenDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
var
  i: Integer;
  a: Int64;
begin
  if Addr + Size > FSize then
    Size := FSize - Addr;
  for i := 0 to Size - 1 do
  begin
    a := Addr + i;
    if (a < CachedPageAddr) or (a >= CachedPageAddr + PageSize) then
      GenPage(a div PageSize * PageSize);
    PByteArray(@Data)[i] := CachedPage[a mod PageSize];
  end;
  Result := Size;
end;

function TRandGenDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable, dspResizable];
end;

function TRandGenDataSource.GetRegions(
  const ARange: TFileRange): TSourceRegionArray;
begin
  Result := inherited;
end;

function TRandGenDataSource.GetSize: TFilePointer;
begin
  Result := FSize;
end;

procedure TRandGenDataSource.Open(Mode: Word; Share: Word = 0);
begin

end;

end.
