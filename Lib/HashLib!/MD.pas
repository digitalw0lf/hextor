unit MD;
{(C) Coban (alex@ritlabs.com)}

interface
uses
  CryptoUtils;

type
  PMD2CTX = ^TMD2CTX;
  TMD2CTX = record
    D: array[0..47] of Byte;   // buffer for forming digest in
    C: array[0..15] of Byte;   // checksum register
    i: Byte;                   // number of bytes handled mod 16
    L: Byte;                   // last checksum byte saved
  end;

  PMD4Ctx = ^TMD4Ctx;
  TMD4Ctx = record
    state: array[0..3] of LongWord;
    count: array[0..1] of LongWord;
    buffer: array[0..63] of Byte;
  end;

  TMDTransform = procedure(var state: array of LongWord; block: Pointer);

  procedure MD2Init(md: PMD2CTX);
  procedure MD2Update(md: PMD2CTX; SrcBuf: Pointer; BufLen: LongWord);
  function MD2Final(md: PMD2CTX): String;

  procedure MDInit(context: PMD4Ctx);
  procedure MDUpdate(context: PMD4Ctx; input: Pointer; inputLen: LongWord; tr_func: TMDTransform);
  procedure MD4Transform (var state: array of LongWord; block: Pointer);
  procedure MD5Transform (var state: array of LongWord; block: Pointer);
  function MDFinal(context: PMD4Ctx; tr_func: TMDTransform): String;  

implementation
const
  PI_SUBST: array[0..255] of Byte = (
    41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
    19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
    76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
    138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
    245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
    148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
    39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
    181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
    150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
    112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
    96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
    85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
    234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
    129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
    8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
    203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
    166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
    31, 26, 219, 153, 141, 51, 159, 17, 131, 20
  );

procedure MD2Init(md: PMD2CTX);
begin
  FillChar(md^.D, 16, 0);
  FillChar(md^.C, 16, 0);
  md^.i := 0; md^.L := 0;
end;

procedure MD2UpdateB(md: PMD2CTX; c: Byte);
var
  i, j, t: Byte;
  p: PByte;
begin
  i := md^.i;
  md^.D[16 + i] := c;
  md^.D[32 + i] := c xor md^.D[i];
  md^.C[i] := md^.C[i] xor PI_SUBST[$ff and (c xor md^.L)];
  md^.L := md^.C[i];
  md^.i := (i + 1) and 15;
  i := md^.i;
  if (i = 0) then
  begin
    t := 0;
    for j := 0 to 17 do
    begin
      p := @md.D;
      for i := 0 to 7 do
      begin
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
        p^ := p^ xor PI_SUBST[t]; t := p^; Inc(p);
      end;
      t := t + j;
    end;
  end;
end;

procedure MD2Update(md: PMD2CTX; SrcBuf: Pointer; BufLen: LongWord);
var
  i: LongWord;
begin
  if BufLen > 0 then
    for i := 0 to BufLen - 1 do
      MD2UpdateB(md, PByte(LongWord(SrcBuf) + i)^);
end;

function MD2Final(md: PMD2CTX): String;
var
  i, padlen: LongWord;
begin
  padlen  := 16 - md^.i;
  for i := 0 to padlen - 1 do
    MD2UpdateB(md, padlen);
  for i := 0 to 15 do
    MD2UpdateB(md, md^.C[i]);
  for i := 0 to 15 do
    Result := Result + IntToHex(md.D[i], 2);
end;

const
  MD_PADDING: array[0..63] of Byte = (
    $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  );

  S11 = 3;
  S12 = 7;
  S13 = 11;
  S14 = 19;
  S21 = 3;
  S22 = 5;
  S23 = 9;
  S24 = 13;
  S31 = 3;
  S32 = 9;
  S33 = 11;
  S34 = 15;

function F(x, y, z: LongWord): LongWord; assembler;
{begin
  Result := ((x and y) or ((not x) and (z)))
end;}
asm
  and   edx,eax
  not   eax
  and   eax,ecx
  or    eax,edx
end;

function G(x, y, z: LongWord): LongWord; assembler;
{begin
  Result := (x and y) or (x and z) or (y and z);
end;}
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  or    eax,ecx
  pop   ecx
  and   edx,ecx
  or    eax,edx
end;

function H(x, y, z: LongWord): LongWord; assembler;
{begin
  Result := x xor y xor z;
end;}
asm
  xor eax,edx
  xor eax,ecx
end;

procedure FF(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (F(b, c, d) + x);
  a := rol(a, s);
end;

procedure GG(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + G(b, c, d) + x + $5a827999;
  a := rol(a, s);
end;

procedure HH(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + H(b, c, d) + x + $6ed9eba1;
  a := rol(a, s);
end;

procedure MDInit(context: PMD4Ctx);
begin
  context^.count[0] := 0;
  context^.count[1] := 0;
  context^.state[0] := $67452301;
  context^.state[1] := $efcdab89;
  context^.state[2] := $98badcfe;
  context^.state[3] := $10325476;
end;

procedure MDEncode(output, input: Pointer; len: LongWord);
var
  i, j: LongWord;
begin
  i := 0; j := 0;
  while j < len do
  begin
    PByteArray(output)^[j] := (PDWordArray(input)^[i] and $ff);
    PByteArray(output)^[j + 1] := ((PDWordArray(input)^[i] shr 8) and $ff);
    PByteArray(output)^[j + 2] := ((PDWordArray(input)^[i] shr 16) and $ff);
    PByteArray(output)^[j + 3] := ((PDWordArray(input)^[i] shr 24) and $ff);
    Inc(i); Inc(j, 4);
  end;
end;

procedure MDDecode(output, input: Pointer; len: LongWord);
var
  i, j: LongWord;
begin
  i := 0; j := 0;
  while j < len do
  begin
    PDWordArray(output)^[i] := PByteArray(input)^[j] or (PByteArray(input)^[j + 1] shl 8) or (PByteArray(input)^[j + 2] shl 16) or (PByteArray(input)^[j + 3] shl 24);
    Inc(i); Inc(j, 4);
  end;
end;

procedure MD4Transform (var state: array of LongWord; block: Pointer);
var
  a, b, c, d: LongWord;
  x: array[0..15] of LongWord;
begin
  a := state[0]; b := state[1]; c := state[2]; d := state[3];
  MDDecode(@x, block, 64);

  FF (a, b, c, d, x[ 0], S11); //* 1 */
  FF (d, a, b, c, x[ 1], S12); //* 2 */
  FF (c, d, a, b, x[ 2], S13); //* 3 */
  FF (b, c, d, a, x[ 3], S14); //* 4 */
  FF (a, b, c, d, x[ 4], S11); //* 5 */
  FF (d, a, b, c, x[ 5], S12); //* 6 */
  FF (c, d, a, b, x[ 6], S13); //* 7 */
  FF (b, c, d, a, x[ 7], S14); //* 8 */
  FF (a, b, c, d, x[ 8], S11); //* 9 */
  FF (d, a, b, c, x[ 9], S12); //* 10 */
  FF (c, d, a, b, x[10], S13); //* 11 */
  FF (b, c, d, a, x[11], S14); //* 12 */
  FF (a, b, c, d, x[12], S11); //* 13 */
  FF (d, a, b, c, x[13], S12); //* 14 */
  FF (c, d, a, b, x[14], S13); //* 15 */
  FF (b, c, d, a, x[15], S14); //* 16 */

  //* Round 2 */
  GG (a, b, c, d, x[ 0], S21); //* 17 */
  GG (d, a, b, c, x[ 4], S22); //* 18 */
  GG (c, d, a, b, x[ 8], S23); //* 19 */
  GG (b, c, d, a, x[12], S24); //* 20 */
  GG (a, b, c, d, x[ 1], S21); //* 21 */
  GG (d, a, b, c, x[ 5], S22); //* 22 */
  GG (c, d, a, b, x[ 9], S23); //* 23 */
  GG (b, c, d, a, x[13], S24); //* 24 */
  GG (a, b, c, d, x[ 2], S21); //* 25 */
  GG (d, a, b, c, x[ 6], S22); //* 26 */
  GG (c, d, a, b, x[10], S23); //* 27 */
  GG (b, c, d, a, x[14], S24); //* 28 */
  GG (a, b, c, d, x[ 3], S21); //* 29 */
  GG (d, a, b, c, x[ 7], S22); //* 30 */
  GG (c, d, a, b, x[11], S23); //* 31 */
  GG (b, c, d, a, x[15], S24); //* 32 */

  //* Round 3 */
  HH (a, b, c, d, x[ 0], S31); //* 33 */
  HH (d, a, b, c, x[ 8], S32); //* 34 */
  HH (c, d, a, b, x[ 4], S33); //* 35 */
  HH (b, c, d, a, x[12], S34); //* 36 */
  HH (a, b, c, d, x[ 2], S31); //* 37 */
  HH (d, a, b, c, x[10], S32); //* 38 */
  HH (c, d, a, b, x[ 6], S33); //* 39 */
  HH (b, c, d, a, x[14], S34); //* 40 */
  HH (a, b, c, d, x[ 1], S31); //* 41 */
  HH (d, a, b, c, x[ 9], S32); //* 42 */
  HH (c, d, a, b, x[ 5], S33); //* 43 */
  HH (b, c, d, a, x[13], S34); //* 44 */
  HH (a, b, c, d, x[ 3], S31); //* 45 */
  HH (d, a, b, c, x[11], S32); //* 46 */
  HH (c, d, a, b, x[ 7], S33); //* 47 */
  HH (b, c, d, a, x[15], S34); //* 48 */

  //State updates
  state[0] := state[0] + a;
  state[1] := state[1] + b;
  state[2] := state[2] + c;
  state[3] := state[3] + d;
end;

procedure MDUpdate(context: PMD4Ctx; input: Pointer; inputLen: LongWord; tr_func: TMDTransform);
var
  i, index, partLen: LongWord;
begin
  //* Compute number of bytes mod 64 */
  index := (context^.count[0] shr 3) and $3F;

  //* Update number of bits */
  context^.count[0] := context^.count[0] + inputLen shl 3;
  if (context^.count[0] < (inputLen shl 3)) then
    Inc(context^.count[1]);

  context^.count[1] := context^.count[1] + inputLen shr 29;
  partLen := 64 - index;
  //* Transform as many times as possible.*/
  if (inputLen >= partLen) then
  begin
    Move(input^, context^.buffer[index], partLen);
    {output, input, len}
    tr_func(context^.state, @context^.buffer);
    i := partLen;
    while i + 63 < inputLen do
    begin
      tr_func(context^.state, Addr(PByteArray(input)^[i]));
      Inc(i, 64);
    end;
    index := 0;
  end
  else
    i := 0;
  //* Buffer remaining input */
  Move(PByteArray(input)^[i], context^.buffer[index], inputLen - i);
end;

function MDFinal(context: PMD4Ctx; tr_func: TMDTransform): String;
var
  digest: array[0..15] of AnsiChar;
  bits: array[0..7] of AnsiChar;
  index, padLen: LongWord;
begin
  //* Save number of bits */
  MDEncode(@bits, @context^.count, 8);

  //* Pad out to 56 mod 64. //*/
  index := (context^.count[0] shr 3) and $3f;
  if (index < 56) then
    padLen := 56 - index
  else
    padLen := 120 - index;

  MDUpdate(context, @MD_PADDING, padLen, tr_func);

  //* Append length (before padding) */
  MDUpdate(context, @bits, 8, tr_func);
  //* Store state in digest */
  MDEncode(@digest, @context^.state, 16);

  //* Zeroize sensitive information.*/
  FillChar(context^, 0, SizeOf(TMD4Ctx));

  Result := '';
  for index := 0 to 15 do
    Result := Result + IntToHex(Ord(digest[index]), 2);
end;

{-- MD5 --------------------------------------------------------}
const
  MD5_S11 = 7;
  MD5_S12 = 12;
  MD5_S13 = 17;
  MD5_S14 = 22;
  MD5_S21 = 5;
  MD5_S22 = 9;
  MD5_S23 = 14;
  MD5_S24 = 20;
  MD5_S31 = 4;
  MD5_S32 = 11;
  MD5_S33 = 16;
  MD5_S34 = 23;
  MD5_S41 = 6;
  MD5_S42 = 10;
  MD5_S43 = 15;
  MD5_S44 = 21;

function MD5_G(x, y, z: LongWord): LongWord; assembler;
{begin
  Result := (x and y) or (x and z) or (y and not z);
end;}
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  or    eax,ecx
  pop   ecx
  not   ecx
  and   edx,ecx
  or    eax,edx
end;

function I(x, y, z: LongWord): LongWord; assembler;
{begin
  Result := y xor (x or (not z));
end;}
asm
  not   ecx
  or    eax,ecx
  xor   eax,edx
end;

procedure MD5_FF(var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
  a := a + F(b, c, d) + x + ac;
  a := rol(a, s);
  a := a + b;
end;

procedure MD5_GG(var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
  a := a + MD5_G(b, c, d) + x + ac;
  a := rol(a, s);
  a := a + b;
end;

procedure MD5_HH(var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
 a := a + H(b, c, d) + x + ac;
 a := rol(a, s);
 a := a + b;
end;

procedure MD5_II(var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
 a := a + I(b, c, d) + x + ac;
 a := rol(a, s);
 a := a + b;
end;

procedure MD5Transform (var state: array of LongWord; block: Pointer);
var
  a, b, c, d: LongWord;
  x: array[0..15] of LongWord;
begin
  a := state[0]; b := state[1]; c := state[2]; d := state[3];
  MDDecode(@x, block, 64);

  //* Round 1 */
  MD5_FF(a, b, c, d, x[ 0], MD5_S11, $d76aa478); //* 1 */
  MD5_FF(d, a, b, c, x[ 1], MD5_S12, $e8c7b756); //* 2 */
  MD5_FF(c, d, a, b, x[ 2], MD5_S13, $242070db); //* 3 */
  MD5_FF(b, c, d, a, x[ 3], MD5_S14, $c1bdceee); //* 4 */
  MD5_FF(a, b, c, d, x[ 4], MD5_S11, $f57c0faf); //* 5 */
  MD5_FF(d, a, b, c, x[ 5], MD5_S12, $4787c62a); //* 6 */
  MD5_FF(c, d, a, b, x[ 6], MD5_S13, $a8304613); //* 7 */
  MD5_FF(b, c, d, a, x[ 7], MD5_S14, $fd469501); //* 8 */
  MD5_FF(a, b, c, d, x[ 8], MD5_S11, $698098d8); //* 9 */
  MD5_FF(d, a, b, c, x[ 9], MD5_S12, $8b44f7af); //* 10 */
  MD5_FF(c, d, a, b, x[10], MD5_S13, $ffff5bb1); //* 11 */
  MD5_FF(b, c, d, a, x[11], MD5_S14, $895cd7be); //* 12 */
  MD5_FF(a, b, c, d, x[12], MD5_S11, $6b901122); //* 13 */
  MD5_FF(d, a, b, c, x[13], MD5_S12, $fd987193); //* 14 */
  MD5_FF(c, d, a, b, x[14], MD5_S13, $a679438e); //* 15 */
  MD5_FF(b, c, d, a, x[15], MD5_S14, $49b40821); //* 16 */

  //* Round 2 */
  MD5_GG(a, b, c, d, x[ 1], MD5_S21, $f61e2562); //* 17 */
  MD5_GG(d, a, b, c, x[ 6], MD5_S22, $c040b340); //* 18 */
  MD5_GG(c, d, a, b, x[11], MD5_S23, $265e5a51); //* 19 */
  MD5_GG(b, c, d, a, x[ 0], MD5_S24, $e9b6c7aa); //* 20 */
  MD5_GG(a, b, c, d, x[ 5], MD5_S21, $d62f105d); //* 21 */
  MD5_GG(d, a, b, c, x[10], MD5_S22, $02441453); //* 22 */
  MD5_GG(c, d, a, b, x[15], MD5_S23, $d8a1e681); //* 23 */
  MD5_GG(b, c, d, a, x[ 4], MD5_S24, $e7d3fbc8); //* 24 */
  MD5_GG(a, b, c, d, x[ 9], MD5_S21, $21e1cde6); //* 25 */
  MD5_GG(d, a, b, c, x[14], MD5_S22, $c33707d6); //* 26 */
  MD5_GG(c, d, a, b, x[ 3], MD5_S23, $f4d50d87); //* 27 */
  MD5_GG(b, c, d, a, x[ 8], MD5_S24, $455a14ed); //* 28 */
  MD5_GG(a, b, c, d, x[13], MD5_S21, $a9e3e905); //* 29 */
  MD5_GG(d, a, b, c, x[ 2], MD5_S22, $fcefa3f8); //* 30 */
  MD5_GG(c, d, a, b, x[ 7], MD5_S23, $676f02d9); //* 31 */
  MD5_GG(b, c, d, a, x[12], MD5_S24, $8d2a4c8a); //* 32 */

  //* Round 3 */
  MD5_HH(a, b, c, d, x[ 5], MD5_S31, $fffa3942); //* 33 */
  MD5_HH(d, a, b, c, x[ 8], MD5_S32, $8771f681); //* 34 */
  MD5_HH(c, d, a, b, x[11], MD5_S33, $6d9d6122); //* 35 */
  MD5_HH(b, c, d, a, x[14], MD5_S34, $fde5380c); //* 36 */
  MD5_HH(a, b, c, d, x[ 1], MD5_S31, $a4beea44); //* 37 */
  MD5_HH(d, a, b, c, x[ 4], MD5_S32, $4bdecfa9); //* 38 */
  MD5_HH(c, d, a, b, x[ 7], MD5_S33, $f6bb4b60); //* 39 */
  MD5_HH(b, c, d, a, x[10], MD5_S34, $bebfbc70); //* 40 */
  MD5_HH(a, b, c, d, x[13], MD5_S31, $289b7ec6); //* 41 */
  MD5_HH(d, a, b, c, x[ 0], MD5_S32, $eaa127fa); //* 42 */
  MD5_HH(c, d, a, b, x[ 3], MD5_S33, $d4ef3085); //* 43 */
  MD5_HH(b, c, d, a, x[ 6], MD5_S34, $04881d05); //* 44 */
  MD5_HH(a, b, c, d, x[ 9], MD5_S31, $d9d4d039); //* 45 */
  MD5_HH(d, a, b, c, x[12], MD5_S32, $e6db99e5); //* 46 */
  MD5_HH(c, d, a, b, x[15], MD5_S33, $1fa27cf8); //* 47 */
  MD5_HH(b, c, d, a, x[ 2], MD5_S34, $c4ac5665); //* 48 */

  //* Round 4 */
  MD5_II(a, b, c, d, x[ 0], MD5_S41, $f4292244); //* 49 */
  MD5_II(d, a, b, c, x[ 7], MD5_S42, $432aff97); //* 50 */
  MD5_II(c, d, a, b, x[14], MD5_S43, $ab9423a7); //* 51 */
  MD5_II(b, c, d, a, x[ 5], MD5_S44, $fc93a039); //* 52 */
  MD5_II(a, b, c, d, x[12], MD5_S41, $655b59c3); //* 53 */
  MD5_II(d, a, b, c, x[ 3], MD5_S42, $8f0ccc92); //* 54 */
  MD5_II(c, d, a, b, x[10], MD5_S43, $ffeff47d); //* 55 */
  MD5_II(b, c, d, a, x[ 1], MD5_S44, $85845dd1); //* 56 */
  MD5_II(a, b, c, d, x[ 8], MD5_S41, $6fa87e4f); //* 57 */
  MD5_II(d, a, b, c, x[15], MD5_S42, $fe2ce6e0); //* 58 */
  MD5_II(c, d, a, b, x[ 6], MD5_S43, $a3014314); //* 59 */
  MD5_II(b, c, d, a, x[13], MD5_S44, $4e0811a1); //* 60 */
  MD5_II(a, b, c, d, x[ 4], MD5_S41, $f7537e82); //* 61 */
  MD5_II(d, a, b, c, x[11], MD5_S42, $bd3af235); //* 62 */
  MD5_II(c, d, a, b, x[ 2], MD5_S43, $2ad7d2bb); //* 63 */
  MD5_II(b, c, d, a, x[ 9], MD5_S44, $eb86d391); //* 64 */

  state[0] := state[0] + a;
  state[1] := state[1] + b;
  state[2] := state[2] + c;
  state[3] := state[3] + d;
end;

end.
