unit uDWHexDataSources;

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
    function GetProperties(): TDataSourceProperties;
    function GetSize(): TFilePointer; virtual; abstract;
    function GetData(Addr: TFilePointer; Size: Integer; var Data: TBytes): Integer; virtual; abstract;
    function ChangeData(Addr: TFilePointer; const Data; DataSize: Integer): Integer; virtual; abstract;
    procedure CopyContentFrom(Source: TDWHexDataSource); virtual;
  end;

  TFileDataSource = class (TDWHexDataSource)
  protected
    FileStream: TFileStream;
  public
    constructor Create(const APath: string);
    destructor Destroy(); override;
    procedure Open(Mode: Word); override;
    function GetProperties(): TDataSourceProperties;
    function GetSize(): TFilePointer; override;
    function GetData(Addr: TFilePointer; Size: Integer; var Data: TBytes): Integer; override;
    function ChangeData(Addr: TFilePointer; const Data; DataSize: Integer): Integer; override;
    procedure CopyContentFrom(Source: TDWHexDataSource); override;
  end;

implementation

{ TDWHexDataSource }

procedure TDWHexDataSource.CopyContentFrom(Source: TDWHexDataSource);
const
  BlockSize = 1*MByte;
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
    Source.GetData(Pos, Size, Buf);
    ChangeData(Pos, Buf[0], Size);
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

{ TFileDataSource }

function TFileDataSource.ChangeData(Addr: TFilePointer;
  const Data; DataSize: Integer): Integer;
begin
  FileStream.Position := Addr;
  FileStream.WriteBuffer(Data, DataSize);
  Result := DataSize;
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
  var Data: TBytes): Integer;
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

end.
