unit RIPEMD;
{(C) Coban (alex@ritlabs.com)}

interface
uses
  CryptoUtils;

type
  PRMDCtx = ^TRMDCtx;
  TRMDCtx = record
    MDBuf: array[0..4] of LongWord;
    length, curlen: LongWord;
    buf: array[0..63] of Byte;
  end;

  procedure RMD128Init(var md: TRMDCtx);
  procedure RMD160Init(var md: TRMDCtx);
  procedure RMDUpdate(var md: TRMDCtx; buf: Pointer; len: LongWord; sz: Word);
  function RMDFinal(var md: TRMDCtx; sz: LongWord): String;

implementation
function F(x, y, z: LongWord): LongWord;
begin
  Result := x xor y xor z;
end;

function G(x, y, z: LongWord): LongWord;
begin
  Result := (x and y) or ((not x) and z);
end;

function H(x, y, z: LongWord): LongWord;
begin
  Result := (x or (not y)) xor z;
end;

function I(x, y, z: LongWord): LongWord;
begin
 Result := (x and z) or (y and (not z));
end;

function J(x, y, z: LongWord): LongWord;
begin
  Result := x xor (y or (not z));
end;

procedure FF(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (F(b, c, d) + x);
  a := ROL(a, s);
end;

procedure GG(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (G(b, c, d) + x + $5a827999);
  a := ROL(a, s);
end;

procedure HH(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (H(b, c, d) + x + $6ed9eba1);
  a := ROL(a, s);
end;

procedure II(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (I(b, c, d) + x + $8f1bbcdc);
  a := ROL(a, s);
end;

procedure FFF(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (F(b, c, d) + x);
  a := ROL(a, s);
end;

procedure GGG(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (G(b, c, d) + x + $6d703ef3);
  a := ROL(a, s);
end;

procedure HHH(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (H(b, c, d) + x + $5c4dd124);
  a := ROL(a, s);
end;

procedure III(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (I(b, c, d) + x + $50a28be6);
  a := ROL(a, s);
end;

procedure FF160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + (F(b, c, d) + x);
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure GG160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + (G(b, c, d) + x + $5a827999);
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure HH160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a +(H(b, c, d) + x + $6ed9eba1);
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure II160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + (I((b), (c), (d)) + (x) + $8f1bbcdc);
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure JJ160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a :=  a + (J(b, c, d) + x + $a953fd4e);
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure FFF160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + F(b, c, d) + x;
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure GGG160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + G(b, c, d) + x + $7a6d76e9;
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure HHH160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + H(b, c, d) + x + $6d703ef3;
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure III160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + I(b, c, d) + x + $5c4dd124;
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure JJJ160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
  a := a + J(b, c, d) + x + $50a28be6;
  a := ROL(a, s) + e;
  c := ROL(c, 10);
end;

procedure MDInit(var MDBuf: array of LongWord);
begin
  MDbuf[0] := $67452301;
  MDbuf[1] := $efcdab89;
  MDbuf[2] := $98badcfe;
  MDbuf[3] := $10325476;
end;

procedure MDInit160(var MDbuf: array of LongWord);
begin
  MDInit(MDBuf);
  MDbuf[4] := $c3d2e1f0;
end;

procedure RMD128Init(var md: TRMDCtx);
begin
  md.length := 0; md.curlen := 0;
  MDInit(md.MDBuf);
end;

procedure RMD160Init(var md: TRMDCtx);
begin
  md.length := 0; md.curlen := 0;
  MDInit160(md.MDBuf);
end;

procedure compress(var MDbuf: array of LongWord; X: array of LongWord);
var
  aa, bb, cc, dd: LongWord;
  aaa, bbb, ccc, ddd: LongWord;
begin
   aa := MDbuf[0]; bb := MDbuf[1]; cc := MDbuf[2]; dd := MDbuf[3];
   aaa := MDbuf[0]; bbb := MDbuf[1]; ccc := MDbuf[2]; ddd := MDbuf[3];

   // round 1
   FF(aa, bb, cc, dd, X[ 0], 11);
   FF(dd, aa, bb, cc, X[ 1], 14);
   FF(cc, dd, aa, bb, X[ 2], 15);
   FF(bb, cc, dd, aa, X[ 3], 12);
   FF(aa, bb, cc, dd, X[ 4],  5);
   FF(dd, aa, bb, cc, X[ 5],  8);
   FF(cc, dd, aa, bb, X[ 6],  7);
   FF(bb, cc, dd, aa, X[ 7],  9);
   FF(aa, bb, cc, dd, X[ 8], 11);
   FF(dd, aa, bb, cc, X[ 9], 13);
   FF(cc, dd, aa, bb, X[10], 14);
   FF(bb, cc, dd, aa, X[11], 15);
   FF(aa, bb, cc, dd, X[12],  6);
   FF(dd, aa, bb, cc, X[13],  7);
   FF(cc, dd, aa, bb, X[14],  9);
   FF(bb, cc, dd, aa, X[15],  8);

   // round 2 
   GG(aa, bb, cc, dd, X[ 7],  7);
   GG(dd, aa, bb, cc, X[ 4],  6);
   GG(cc, dd, aa, bb, X[13],  8);
   GG(bb, cc, dd, aa, X[ 1], 13);
   GG(aa, bb, cc, dd, X[10], 11);
   GG(dd, aa, bb, cc, X[ 6],  9);
   GG(cc, dd, aa, bb, X[15],  7);
   GG(bb, cc, dd, aa, X[ 3], 15);
   GG(aa, bb, cc, dd, X[12],  7);
   GG(dd, aa, bb, cc, X[ 0], 12);
   GG(cc, dd, aa, bb, X[ 9], 15);
   GG(bb, cc, dd, aa, X[ 5],  9);
   GG(aa, bb, cc, dd, X[ 2], 11);
   GG(dd, aa, bb, cc, X[14],  7);
   GG(cc, dd, aa, bb, X[11], 13);
   GG(bb, cc, dd, aa, X[ 8], 12);

   // round 3 
   HH(aa, bb, cc, dd, X[ 3], 11);
   HH(dd, aa, bb, cc, X[10], 13);
   HH(cc, dd, aa, bb, X[14],  6);
   HH(bb, cc, dd, aa, X[ 4],  7);
   HH(aa, bb, cc, dd, X[ 9], 14);
   HH(dd, aa, bb, cc, X[15],  9);
   HH(cc, dd, aa, bb, X[ 8], 13);
   HH(bb, cc, dd, aa, X[ 1], 15);
   HH(aa, bb, cc, dd, X[ 2], 14);
   HH(dd, aa, bb, cc, X[ 7],  8);
   HH(cc, dd, aa, bb, X[ 0], 13);
   HH(bb, cc, dd, aa, X[ 6],  6);
   HH(aa, bb, cc, dd, X[13],  5);
   HH(dd, aa, bb, cc, X[11], 12);
   HH(cc, dd, aa, bb, X[ 5],  7);
   HH(bb, cc, dd, aa, X[12],  5);

   // round 4 
   II(aa, bb, cc, dd, X[ 1], 11);
   II(dd, aa, bb, cc, X[ 9], 12);
   II(cc, dd, aa, bb, X[11], 14);
   II(bb, cc, dd, aa, X[10], 15);
   II(aa, bb, cc, dd, X[ 0], 14);
   II(dd, aa, bb, cc, X[ 8], 15);
   II(cc, dd, aa, bb, X[12],  9);
   II(bb, cc, dd, aa, X[ 4],  8);
   II(aa, bb, cc, dd, X[13],  9);
   II(dd, aa, bb, cc, X[ 3], 14);
   II(cc, dd, aa, bb, X[ 7],  5);
   II(bb, cc, dd, aa, X[15],  6);
   II(aa, bb, cc, dd, X[14],  8);
   II(dd, aa, bb, cc, X[ 5],  6);
   II(cc, dd, aa, bb, X[ 6],  5);
   II(bb, cc, dd, aa, X[ 2], 12);

   // parallel round 1 
   III(aaa, bbb, ccc, ddd, X[ 5],  8); 
   III(ddd, aaa, bbb, ccc, X[14],  9);
   III(ccc, ddd, aaa, bbb, X[ 7],  9);
   III(bbb, ccc, ddd, aaa, X[ 0], 11);
   III(aaa, bbb, ccc, ddd, X[ 9], 13);
   III(ddd, aaa, bbb, ccc, X[ 2], 15);
   III(ccc, ddd, aaa, bbb, X[11], 15);
   III(bbb, ccc, ddd, aaa, X[ 4],  5);
   III(aaa, bbb, ccc, ddd, X[13],  7);
   III(ddd, aaa, bbb, ccc, X[ 6],  7);
   III(ccc, ddd, aaa, bbb, X[15],  8);
   III(bbb, ccc, ddd, aaa, X[ 8], 11);
   III(aaa, bbb, ccc, ddd, X[ 1], 14);
   III(ddd, aaa, bbb, ccc, X[10], 14);
   III(ccc, ddd, aaa, bbb, X[ 3], 12);
   III(bbb, ccc, ddd, aaa, X[12],  6);

   // parallel round 2 
   HHH(aaa, bbb, ccc, ddd, X[ 6],  9);
   HHH(ddd, aaa, bbb, ccc, X[11], 13);
   HHH(ccc, ddd, aaa, bbb, X[ 3], 15);
   HHH(bbb, ccc, ddd, aaa, X[ 7],  7);
   HHH(aaa, bbb, ccc, ddd, X[ 0], 12);
   HHH(ddd, aaa, bbb, ccc, X[13],  8);
   HHH(ccc, ddd, aaa, bbb, X[ 5],  9);
   HHH(bbb, ccc, ddd, aaa, X[10], 11);
   HHH(aaa, bbb, ccc, ddd, X[14],  7);
   HHH(ddd, aaa, bbb, ccc, X[15],  7);
   HHH(ccc, ddd, aaa, bbb, X[ 8], 12);
   HHH(bbb, ccc, ddd, aaa, X[12],  7);
   HHH(aaa, bbb, ccc, ddd, X[ 4],  6);
   HHH(ddd, aaa, bbb, ccc, X[ 9], 15);
   HHH(ccc, ddd, aaa, bbb, X[ 1], 13);
   HHH(bbb, ccc, ddd, aaa, X[ 2], 11);

   // parallel round 3 
   GGG(aaa, bbb, ccc, ddd, X[15],  9);
   GGG(ddd, aaa, bbb, ccc, X[ 5],  7);
   GGG(ccc, ddd, aaa, bbb, X[ 1], 15);
   GGG(bbb, ccc, ddd, aaa, X[ 3], 11);
   GGG(aaa, bbb, ccc, ddd, X[ 7],  8);
   GGG(ddd, aaa, bbb, ccc, X[14],  6);
   GGG(ccc, ddd, aaa, bbb, X[ 6],  6);
   GGG(bbb, ccc, ddd, aaa, X[ 9], 14);
   GGG(aaa, bbb, ccc, ddd, X[11], 12);
   GGG(ddd, aaa, bbb, ccc, X[ 8], 13);
   GGG(ccc, ddd, aaa, bbb, X[12],  5);
   GGG(bbb, ccc, ddd, aaa, X[ 2], 14);
   GGG(aaa, bbb, ccc, ddd, X[10], 13);
   GGG(ddd, aaa, bbb, ccc, X[ 0], 13);
   GGG(ccc, ddd, aaa, bbb, X[ 4],  7);
   GGG(bbb, ccc, ddd, aaa, X[13],  5);

   // parallel round 4
   FFF(aaa, bbb, ccc, ddd, X[ 8], 15);
   FFF(ddd, aaa, bbb, ccc, X[ 6],  5);
   FFF(ccc, ddd, aaa, bbb, X[ 4],  8);
   FFF(bbb, ccc, ddd, aaa, X[ 1], 11);
   FFF(aaa, bbb, ccc, ddd, X[ 3], 14);
   FFF(ddd, aaa, bbb, ccc, X[11], 14);
   FFF(ccc, ddd, aaa, bbb, X[15],  6);
   FFF(bbb, ccc, ddd, aaa, X[ 0], 14);
   FFF(aaa, bbb, ccc, ddd, X[ 5],  6);
   FFF(ddd, aaa, bbb, ccc, X[12],  9);
   FFF(ccc, ddd, aaa, bbb, X[ 2], 12);
   FFF(bbb, ccc, ddd, aaa, X[13],  9);
   FFF(aaa, bbb, ccc, ddd, X[ 9], 12);
   FFF(ddd, aaa, bbb, ccc, X[ 7],  5);
   FFF(ccc, ddd, aaa, bbb, X[10], 15);
   FFF(bbb, ccc, ddd, aaa, X[14],  8);

   // combine results
   ddd := ddd + cc + MDbuf[1];              // final result for MDbuf[0] 
   MDbuf[1] := MDbuf[2] + dd + aaa;
   MDbuf[2] := MDbuf[3] + aa + bbb;
   MDbuf[3] := MDbuf[0] + bb + ccc;
   MDbuf[0] := ddd;
end;

procedure compress160(var MDbuf: array of LongWord; X: array of LongWord);
var
  aa, bb, cc, dd, ee: LongWord;
  aaa, bbb, ccc, ddd, eee: LongWord;
begin
   aa := MDbuf[0]; bb := MDbuf[1]; cc := MDbuf[2]; dd := MDbuf[3]; ee := MDbuf[4];
   aaa := MDbuf[0]; bbb := MDbuf[1]; ccc := MDbuf[2]; ddd := MDbuf[3]; eee := MDbuf[4];

   // round 1
   FF160(aa, bb, cc, dd, ee, X[ 0], 11);
   FF160(ee, aa, bb, cc, dd, X[ 1], 14);
   FF160(dd, ee, aa, bb, cc, X[ 2], 15);
   FF160(cc, dd, ee, aa, bb, X[ 3], 12);
   FF160(bb, cc, dd, ee, aa, X[ 4],  5);
   FF160(aa, bb, cc, dd, ee, X[ 5],  8);
   FF160(ee, aa, bb, cc, dd, X[ 6],  7);
   FF160(dd, ee, aa, bb, cc, X[ 7],  9);
   FF160(cc, dd, ee, aa, bb, X[ 8], 11);
   FF160(bb, cc, dd, ee, aa, X[ 9], 13);
   FF160(aa, bb, cc, dd, ee, X[10], 14);
   FF160(ee, aa, bb, cc, dd, X[11], 15);
   FF160(dd, ee, aa, bb, cc, X[12],  6);
   FF160(cc, dd, ee, aa, bb, X[13],  7);
   FF160(bb, cc, dd, ee, aa, X[14],  9);
   FF160(aa, bb, cc, dd, ee, X[15],  8);

   // round 2 
   GG160(ee, aa, bb, cc, dd, X[ 7],  7);
   GG160(dd, ee, aa, bb, cc, X[ 4],  6);
   GG160(cc, dd, ee, aa, bb, X[13],  8);
   GG160(bb, cc, dd, ee, aa, X[ 1], 13);
   GG160(aa, bb, cc, dd, ee, X[10], 11);
   GG160(ee, aa, bb, cc, dd, X[ 6],  9);
   GG160(dd, ee, aa, bb, cc, X[15],  7);
   GG160(cc, dd, ee, aa, bb, X[ 3], 15);
   GG160(bb, cc, dd, ee, aa, X[12],  7);
   GG160(aa, bb, cc, dd, ee, X[ 0], 12);
   GG160(ee, aa, bb, cc, dd, X[ 9], 15);
   GG160(dd, ee, aa, bb, cc, X[ 5],  9);
   GG160(cc, dd, ee, aa, bb, X[ 2], 11);
   GG160(bb, cc, dd, ee, aa, X[14],  7);
   GG160(aa, bb, cc, dd, ee, X[11], 13);
   GG160(ee, aa, bb, cc, dd, X[ 8], 12);

   // round 3 
   HH160(dd, ee, aa, bb, cc, X[ 3], 11);
   HH160(cc, dd, ee, aa, bb, X[10], 13);
   HH160(bb, cc, dd, ee, aa, X[14],  6);
   HH160(aa, bb, cc, dd, ee, X[ 4],  7);
   HH160(ee, aa, bb, cc, dd, X[ 9], 14);
   HH160(dd, ee, aa, bb, cc, X[15],  9);
   HH160(cc, dd, ee, aa, bb, X[ 8], 13);
   HH160(bb, cc, dd, ee, aa, X[ 1], 15);
   HH160(aa, bb, cc, dd, ee, X[ 2], 14);
   HH160(ee, aa, bb, cc, dd, X[ 7],  8);
   HH160(dd, ee, aa, bb, cc, X[ 0], 13);
   HH160(cc, dd, ee, aa, bb, X[ 6],  6);
   HH160(bb, cc, dd, ee, aa, X[13],  5);
   HH160(aa, bb, cc, dd, ee, X[11], 12);
   HH160(ee, aa, bb, cc, dd, X[ 5],  7);
   HH160(dd, ee, aa, bb, cc, X[12],  5);

   // round 4 
   II160(cc, dd, ee, aa, bb, X[ 1], 11);
   II160(bb, cc, dd, ee, aa, X[ 9], 12);
   II160(aa, bb, cc, dd, ee, X[11], 14);
   II160(ee, aa, bb, cc, dd, X[10], 15);
   II160(dd, ee, aa, bb, cc, X[ 0], 14);
   II160(cc, dd, ee, aa, bb, X[ 8], 15);
   II160(bb, cc, dd, ee, aa, X[12],  9);
   II160(aa, bb, cc, dd, ee, X[ 4],  8);
   II160(ee, aa, bb, cc, dd, X[13],  9);
   II160(dd, ee, aa, bb, cc, X[ 3], 14);
   II160(cc, dd, ee, aa, bb, X[ 7],  5);
   II160(bb, cc, dd, ee, aa, X[15],  6);
   II160(aa, bb, cc, dd, ee, X[14],  8);
   II160(ee, aa, bb, cc, dd, X[ 5],  6);
   II160(dd, ee, aa, bb, cc, X[ 6],  5);
   II160(cc, dd, ee, aa, bb, X[ 2], 12);

   // round 5 
   JJ160(bb, cc, dd, ee, aa, X[ 4],  9);
   JJ160(aa, bb, cc, dd, ee, X[ 0], 15);
   JJ160(ee, aa, bb, cc, dd, X[ 5],  5);
   JJ160(dd, ee, aa, bb, cc, X[ 9], 11);
   JJ160(cc, dd, ee, aa, bb, X[ 7],  6);
   JJ160(bb, cc, dd, ee, aa, X[12],  8);
   JJ160(aa, bb, cc, dd, ee, X[ 2], 13);
   JJ160(ee, aa, bb, cc, dd, X[10], 12);
   JJ160(dd, ee, aa, bb, cc, X[14],  5);
   JJ160(cc, dd, ee, aa, bb, X[ 1], 12);
   JJ160(bb, cc, dd, ee, aa, X[ 3], 13);
   JJ160(aa, bb, cc, dd, ee, X[ 8], 14);
   JJ160(ee, aa, bb, cc, dd, X[11], 11);
   JJ160(dd, ee, aa, bb, cc, X[ 6],  8);
   JJ160(cc, dd, ee, aa, bb, X[15],  5);
   JJ160(bb, cc, dd, ee, aa, X[13],  6);

   // parallel round 1 
   JJJ160(aaa, bbb, ccc, ddd, eee, X[ 5],  8);
   JJJ160(eee, aaa, bbb, ccc, ddd, X[14],  9);
   JJJ160(ddd, eee, aaa, bbb, ccc, X[ 7],  9);
   JJJ160(ccc, ddd, eee, aaa, bbb, X[ 0], 11);
   JJJ160(bbb, ccc, ddd, eee, aaa, X[ 9], 13);
   JJJ160(aaa, bbb, ccc, ddd, eee, X[ 2], 15);
   JJJ160(eee, aaa, bbb, ccc, ddd, X[11], 15);
   JJJ160(ddd, eee, aaa, bbb, ccc, X[ 4],  5);
   JJJ160(ccc, ddd, eee, aaa, bbb, X[13],  7);
   JJJ160(bbb, ccc, ddd, eee, aaa, X[ 6],  7);
   JJJ160(aaa, bbb, ccc, ddd, eee, X[15],  8);
   JJJ160(eee, aaa, bbb, ccc, ddd, X[ 8], 11);
   JJJ160(ddd, eee, aaa, bbb, ccc, X[ 1], 14);
   JJJ160(ccc, ddd, eee, aaa, bbb, X[10], 14);
   JJJ160(bbb, ccc, ddd, eee, aaa, X[ 3], 12);
   JJJ160(aaa, bbb, ccc, ddd, eee, X[12],  6);

   // parallel round 2 
   III160(eee, aaa, bbb, ccc, ddd, X[ 6],  9); 
   III160(ddd, eee, aaa, bbb, ccc, X[11], 13);
   III160(ccc, ddd, eee, aaa, bbb, X[ 3], 15);
   III160(bbb, ccc, ddd, eee, aaa, X[ 7],  7);
   III160(aaa, bbb, ccc, ddd, eee, X[ 0], 12);
   III160(eee, aaa, bbb, ccc, ddd, X[13],  8);
   III160(ddd, eee, aaa, bbb, ccc, X[ 5],  9);
   III160(ccc, ddd, eee, aaa, bbb, X[10], 11);
   III160(bbb, ccc, ddd, eee, aaa, X[14],  7);
   III160(aaa, bbb, ccc, ddd, eee, X[15],  7);
   III160(eee, aaa, bbb, ccc, ddd, X[ 8], 12);
   III160(ddd, eee, aaa, bbb, ccc, X[12],  7);
   III160(ccc, ddd, eee, aaa, bbb, X[ 4],  6);
   III160(bbb, ccc, ddd, eee, aaa, X[ 9], 15);
   III160(aaa, bbb, ccc, ddd, eee, X[ 1], 13);
   III160(eee, aaa, bbb, ccc, ddd, X[ 2], 11);

   // parallel round 3
   HHH160(ddd, eee, aaa, bbb, ccc, X[15],  9);
   HHH160(ccc, ddd, eee, aaa, bbb, X[ 5],  7);
   HHH160(bbb, ccc, ddd, eee, aaa, X[ 1], 15);
   HHH160(aaa, bbb, ccc, ddd, eee, X[ 3], 11);
   HHH160(eee, aaa, bbb, ccc, ddd, X[ 7],  8);
   HHH160(ddd, eee, aaa, bbb, ccc, X[14],  6);
   HHH160(ccc, ddd, eee, aaa, bbb, X[ 6],  6);
   HHH160(bbb, ccc, ddd, eee, aaa, X[ 9], 14);
   HHH160(aaa, bbb, ccc, ddd, eee, X[11], 12);
   HHH160(eee, aaa, bbb, ccc, ddd, X[ 8], 13);
   HHH160(ddd, eee, aaa, bbb, ccc, X[12],  5);
   HHH160(ccc, ddd, eee, aaa, bbb, X[ 2], 14);
   HHH160(bbb, ccc, ddd, eee, aaa, X[10], 13);
   HHH160(aaa, bbb, ccc, ddd, eee, X[ 0], 13);
   HHH160(eee, aaa, bbb, ccc, ddd, X[ 4],  7);
   HHH160(ddd, eee, aaa, bbb, ccc, X[13],  5);

   // parallel round 4
   GGG160(ccc, ddd, eee, aaa, bbb, X[ 8], 15);
   GGG160(bbb, ccc, ddd, eee, aaa, X[ 6],  5);
   GGG160(aaa, bbb, ccc, ddd, eee, X[ 4],  8);
   GGG160(eee, aaa, bbb, ccc, ddd, X[ 1], 11);
   GGG160(ddd, eee, aaa, bbb, ccc, X[ 3], 14);
   GGG160(ccc, ddd, eee, aaa, bbb, X[11], 14);
   GGG160(bbb, ccc, ddd, eee, aaa, X[15],  6);
   GGG160(aaa, bbb, ccc, ddd, eee, X[ 0], 14);
   GGG160(eee, aaa, bbb, ccc, ddd, X[ 5],  6);
   GGG160(ddd, eee, aaa, bbb, ccc, X[12],  9);
   GGG160(ccc, ddd, eee, aaa, bbb, X[ 2], 12);
   GGG160(bbb, ccc, ddd, eee, aaa, X[13],  9);
   GGG160(aaa, bbb, ccc, ddd, eee, X[ 9], 12);
   GGG160(eee, aaa, bbb, ccc, ddd, X[ 7],  5);
   GGG160(ddd, eee, aaa, bbb, ccc, X[10], 15);
   GGG160(ccc, ddd, eee, aaa, bbb, X[14],  8);

   // parallel round 5
   FFF160(bbb, ccc, ddd, eee, aaa, X[12] ,  8);
   FFF160(aaa, bbb, ccc, ddd, eee, X[15] ,  5);
   FFF160(eee, aaa, bbb, ccc, ddd, X[10] , 12);
   FFF160(ddd, eee, aaa, bbb, ccc, X[ 4] ,  9);
   FFF160(ccc, ddd, eee, aaa, bbb, X[ 1] , 12);
   FFF160(bbb, ccc, ddd, eee, aaa, X[ 5] ,  5);
   FFF160(aaa, bbb, ccc, ddd, eee, X[ 8] , 14);
   FFF160(eee, aaa, bbb, ccc, ddd, X[ 7] ,  6);
   FFF160(ddd, eee, aaa, bbb, ccc, X[ 6] ,  8);
   FFF160(ccc, ddd, eee, aaa, bbb, X[ 2] , 13);
   FFF160(bbb, ccc, ddd, eee, aaa, X[13] ,  6);
   FFF160(aaa, bbb, ccc, ddd, eee, X[14] ,  5);
   FFF160(eee, aaa, bbb, ccc, ddd, X[ 0] , 15);
   FFF160(ddd, eee, aaa, bbb, ccc, X[ 3] , 13);
   FFF160(ccc, ddd, eee, aaa, bbb, X[ 9] , 11);
   FFF160(bbb, ccc, ddd, eee, aaa, X[11] , 11);

   ddd := ddd + cc + MDbuf[1];
   MDbuf[1] := MDbuf[2] + dd + eee;
   MDbuf[2] := MDbuf[3] + ee + aaa;
   MDbuf[3] := MDbuf[4] + aa + bbb;
   MDbuf[4] := MDbuf[0] + bb + ccc;
   MDbuf[0] := ddd;
end;

procedure RMDUpdate(var md: TRMDCtx; buf: Pointer; len: LongWord; sz: Word);
var
  X: array[0..15] of LongWord;
begin
  while (len > 0) do
  begin
    md.buf[md.curlen] := PByte(buf)^;
    Inc(md.curlen);
    buf := Ptr(LongWord(buf) + 1);
    if (md.curlen = 64) then
    begin
      Move((@md.buf)^, X, 64);
      if sz = 160 then
        compress160(md.MDBuf, X)
      else
        compress(md.MDBuf, X);
      Inc(md.length, 64);
      md.curlen := 0;
    end;
    Dec(len);
  end;
end;

procedure MDfinish(var MDbuf: array of LongWord; strptr: Pointer; lswlen, mswlen: LongWord; Cipher: Byte);
var
  X: array[0..15] of LongWord;
  i: LongWord;
begin
  FillChar(X, SizeOf(X), 0);
  i := 0;
  while i < (lswlen and 63) do
  begin
    X[i shr 2] := X[i shr 2] xor (PByte(strptr)^ shl (8 * (i and 3)));
    Inc(i);
    strptr := Ptr(LongWord(strptr) + 1);
  end;
  X[(lswlen shr 2) and 15] := X[(lswlen shr 2) and 15] xor (LongWord(1) shl (8 * (lswlen and 3) + 7));
  if ((lswlen and 63) > 55) then
  begin
    if Cipher = 160 then
      compress160(MDbuf, X)
    else
      compress(MDbuf, X);
    FillChar(X, SizeOf(X), 0);
  end;
  X[14] := lswlen shl 3;
  X[15] := (lswlen shr 29) or (mswlen shl 3);
  if Cipher = 160 then
    compress160(MDbuf, X)
  else
    compress(MDbuf, X);
end;

function RMDFinal(var md: TRMDCtx; sz: LongWord): String;
begin
  Inc(md.length, md.curlen);
  MDfinish(md.MDbuf, @md.buf, md.length, 0, sz);
  Result :=
    IntToHex(Endian(md.MDBuf[0]), 8) +
    IntToHex(Endian(md.MDBuf[1]), 8) +
    IntToHex(Endian(md.MDBuf[2]), 8) +
    IntToHex(Endian(md.MDBuf[3]), 8);
  if sz = 160 then
    Result := Result + IntToHex(Endian(md.MDBuf[4]), 8);
end;
end.
