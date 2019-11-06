unit uDWHexDataSources;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.SysUtils, System.Types, System.Classes, Winapi.Windows, System.Math,

  uDWHexTypes;

type
  TDataSourceProperty = (dspWritable, dspResizable);
  TDataSourceProperties = set of TDataSourceProperty;

  TDWHexDataSource = class
  private
    FPath: string;
  public
    property Path: string read FPath;

    constructor Create(const APath: string);
    destructor Destroy(); override;
    procedure Open(Mode: Word); virtual; abstract;
    function GetProperties(): TDataSourceProperties; virtual;
    function CanBeSaved(): Boolean; virtual;
    function GetSize(): TFilePointer; virtual; abstract;
    procedure SetSize(NewSize: TFilePointer); virtual;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; virtual; abstract;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; virtual; abstract;
    procedure CopyContentFrom(Source: TDWHexDataSource); virtual;
  end;

  TDWHexDataSourceType = class of TDWHexDataSource;

  TFileDataSource = class (TDWHexDataSource)
  protected
    FileStream: TFileStream;
  public
    constructor Create(const APath: string);
    destructor Destroy(); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function CanBeSaved(): Boolean; override;
    function GetSize(): TFilePointer; override;
    procedure SetSize(NewSize: TFilePointer); override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
    procedure CopyContentFrom(Source: TDWHexDataSource); override;
  end;

  TDiskDataSource = class (TFileDataSource)
  protected
    const SectorAlign = 512;
  public
    constructor Create(const APath: string);
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
  end;

  TProcMemDataSource = class (TDWHexDataSource)
  protected
    hProcess: Cardinal;
  public
    constructor Create(const APath: string);
    destructor Destroy(); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties; override;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data): Integer; override;
    function ChangeData(Addr: TFilePointer; Size: Integer; const Data): Integer; override;
  end;


implementation

{ TDWHexDataSource }

function TDWHexDataSource.CanBeSaved: Boolean;
// Is it possible to execute "Save" action for currently assigned data source
begin
  Result := dspWritable in GetProperties();
end;

procedure TDWHexDataSource.CopyContentFrom(Source: TDWHexDataSource);
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

constructor TDWHexDataSource.Create(const APath: string);
begin
  inherited Create();
  FPath := APath;
end;

destructor TDWHexDataSource.Destroy;
begin

  inherited;
end;

function TDWHexDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [];
end;

procedure TDWHexDataSource.SetSize(NewSize: TFilePointer);
begin
  //raise Exception.Create('Size change not supported');
end;

{ TFileDataSource }

function TFileDataSource.CanBeSaved: Boolean;
begin
  // False if it is just a new empty "file" and not assigned with actual file on disk
  Result := (inherited) and (ExtractFilePath(Path) <> '');
end;

function TFileDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
begin
  FileStream.Position := Addr;
  //FileStream.WriteBuffer(Data, Size);
  if FileStream.Write(Data, Size) <> Size then
    RaiseLastOSError();
  Result := Size;
end;

procedure TFileDataSource.CopyContentFrom(Source: TDWHexDataSource);
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

function TFileDataSource.GetData(Addr: TFilePointer; Size: Integer;
  var Data): Integer;
begin
  if FileStream = nil then
    Exit(0);

  FileStream.Position := Addr;
  //Result := FileStream.Read(Data, Size);
  FileStream.ReadBuffer(Data, Size);
  Result := Size;
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

procedure TFileDataSource.Open(Mode: Word);
begin
  if ExtractFilePath(Path) <> '' then
  begin
    if Mode = fmCreate then
      ForceDirectories(ExtractFilePath(Path));
    FreeAndNil(FileStream);
    FileStream := TFileStream.Create(Path, Mode or fmShareDenyWrite);
  end;
end;

procedure TFileDataSource.SetSize(NewSize: TFilePointer);
begin
  if FileStream <> nil then
    FileStream.Size := NewSize;
end;

{ TDiskDataSource }

function TDiskDataSource.ChangeData(Addr: TFilePointer; Size: Integer;
  const Data): Integer;
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

  inherited GetData(Ptr1, Ptr2 - Ptr1, Buf[0]);
//  Move(Buf[Addr - Ptr1], Data[0], Size);
  Move(Data, Buf[Addr - Ptr1], Size);

  inherited ChangeData(Ptr1, Ptr2 - Ptr1, Buf[0]);

  Result := Size;
end;

constructor TDiskDataSource.Create(const APath: string);
begin
  inherited;
end;

function TDiskDataSource.GetData(Addr: TFilePointer; Size: Integer;
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

  inherited GetData(Ptr1, Ptr2 - Ptr1, Buf[0]);
  Move(Buf[Addr - Ptr1], Data, Size);

  Result := Size;
end;

function TDiskDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable];
end;

function TDiskDataSource.GetSize: TFilePointer;
begin
  if Length(Path) < 2 then Exit(0);
  Result := DiskSize(Ord(Path[Low(Path)]) - Ord('A') + 1);
end;

procedure TDiskDataSource.Open(Mode: Word);
begin
  if Length(Path) <> 2 then Exit;
  FreeAndNil(FileStream);
  if Mode = fmCreate then
    Mode := fmOpenReadWrite;
  if Mode = fmOpenRead then
    Mode := Mode or fmShareDenyNone
  else
    Mode := Mode or fmShareExclusive;
  FileStream := TFileStream.Create('\\.\'+Path, Mode);
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
begin
  Res := Bool( ReadProcessMemory(hProcess, Pointer(Addr), @Data, Size, NativeUInt(nil^)) );
  if not Res then
    ZeroMemory(@Data, Size);
  Result := Size;
end;

function TProcMemDataSource.GetProperties: TDataSourceProperties;
begin
  Result := [dspWritable];
end;

function TProcMemDataSource.GetSize: TFilePointer;
begin
  Result := $100000000;
end;

procedure TProcMemDataSource.Open(Mode: Word);
var
  ProcID, Access: Cardinal;
begin
  ProcID := StrToInt(Path);
  if hProcess <> 0 then  CloseHandle(hProcess);
  case Mode of
    fmCreate, fmOpenReadWrite: Access := PROCESS_ALL_ACCESS;
    else Access := PROCESS_VM_READ;
  end;
  hProcess := OpenProcess(Access, false, ProcID);
  Win32Check(Bool(hProcess));
end;

end.
