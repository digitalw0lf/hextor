unit CryptoUtils;
{(C) Coban (alex@ritlabs.com)}

{$WARN COMBINING_SIGNED_UNSIGNED64 OFF}

interface

type
  PInt64Array = ^TInt64Array;
  TInt64Array = array[0..0] of Int64;

  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..0] of LongWord;

  PByteArray = ^TByteArray;
  TByteArray = array[0..0] of Byte;

  PByte = ^Byte;
  PLongWord = ^LongWord;

  function IntToHex(Int: Int64; IntSize: Byte): String;
  function ror(x: LongWord; y: Byte): LongWord;
  function rol(x: LongWord; y: Byte): LongWord;
  function ror64(x: Int64; y: Byte): Int64;
  function Endian(X: LongWord): LongWord;
  function Endian64(X: Int64): Int64;

implementation

const
  HexChars: array[0..15] of Char = ('0', '1', '2', '3', '4', '5',
                                    '6', '7', '8', '9', 'a', 'b',
                                    'c', 'd', 'e', 'f');

function IntToHex(Int: Int64; IntSize: Byte): String;
var
  n: Byte;
begin
  Result := '';
  for n := 0 to IntSize - 1 do
  begin
    Result := HexChars[Int and $F] + Result;
    Int := Int shr $4;
  end;
end;

function ror(x: LongWord; y: Byte): LongWord; assembler;
asm
  mov   cl,dl
  ror   eax,cl
end;

function rol(x: LongWord; y: Byte): LongWord; assembler;
asm
  mov   cl,dl
  rol   eax,cl
end;

function ror64(x: Int64; y: Byte): Int64;
begin
  Result := (x shr y) or (x shl (64 - y));
end;

function Endian(X: LongWord): LongWord; assembler;
asm
  bswap eax
end;

function Endian64(X: Int64): Int64;
begin
  Result :=          (X and $00000000000000FF) shl 56;
  Result := Result + (X and $000000000000FF00) shl 40;
  Result := Result + (X and $0000000000FF0000) shl 24;
  Result := Result + (X and $00000000FF000000) shl 8;
  Result := Result + (X and $000000FF00000000) shr 8;
  Result := Result + (X and $0000FF0000000000) shr 24;
  Result := Result + (X and $00FF000000000000) shr 40;
  Result := Result + (X and $FF00000000000000) shr 56;
end;

end.

