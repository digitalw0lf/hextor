unit Adler;
{(C) Coban(alex@ritlabs.com)}
{$DEFINE USE_ASM} //Remove this line to use pascal instead of assembler

interface
uses
  CryptoUtils;

  procedure Adler32Init(var adler: LongWord);
  procedure Adler32Update(var adler: LongWord; const buf: Pointer; len: LongWord);
  function Adler32Final(var adler: LongWord): String;

implementation

{$IFDEF USE_ASM}
procedure Adler32Init(var adler: LongWord); assembler;
asm
  mov   dword ptr[eax],1
end;
{$ELSE}
procedure Adler32Init(var adler: LongWord);
begin
  adler := 1;
end;
{$ENDIF}

{$IFDEF USE_ASM}
procedure Adler32Update(var adler: LongWord; const buf: Pointer; len: LongWord); assembler;
asm
  push  ebx
  push  esi
  push  eax

  xor   ebx,ebx
  mov   bx,word ptr[eax + 2] //s2

  and   dword ptr[eax],$FFFF           //s1
  mov   eax,[eax]

  mov   esi,edx
  or    ecx,ecx
  jz    @@end
@@loop:
  movzx edx,byte ptr[esi]
  add   eax,edx
  cmp   eax,0FFF1h
  jl    @@jump1
  sub   eax,0FFF1h
@@jump1:
  add   ebx,eax
  cmp   ebx,0FFF1h
  jl    @@jump2
  sub   ebx,0FFF1h
@@jump2:
  inc   esi
  dec   ecx
  jnz   @@loop
@@end:
  shl   ebx,16
  add   ebx,eax
  pop   eax
  mov   [eax],ebx

  pop   esi
  pop   ebx
end;
{$ELSE}
procedure Adler32Update(var adler: LongWord; const buf: Pointer; len: LongWord);
var
  s1, s2, n: LongWord;
begin
  s1 := adler and $0000FFFF;
  s2 := (adler shr 16) and $0000FFFF;
  if len > 0 then
    for n := 0 to len - 1 do
    begin
      s1 := (s1 + PByteArray(buf)^[n]);
      if (s1 >= 65521) then
        Dec(s1, 65521);
      Inc(s2, s1);
      if (s2 >= 65521) then
        Dec(s2, 65521);
    end;
  adler := (s2 shl 16) + s1;
end;
{$ENDIF}

function Adler32Final(var adler: LongWord): String;
begin
  Result := IntToHex(adler, 8);
end;


end.
