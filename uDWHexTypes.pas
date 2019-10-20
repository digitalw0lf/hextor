unit uDWHexTypes;

interface

uses
  SysUtils,

  uUtil;

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

type
  TFilePointer = Int64;

  TFileRange = record
    Start, AEnd: TFilePointer;
    function Size(): TFilePointer;
  end;

function MakeValidFileName(const S: string): string;

implementation

function MakeValidFileName(const S: string): string;
begin
  Result := ReplaceAllChars(S, CharsInvalidInFileName, '_');
end;

{ TFileRange }

function TFileRange.Size: TFilePointer;
begin
  Result := AEnd-Start;
end;

end.
