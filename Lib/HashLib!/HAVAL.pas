unit HAVAL;
{(C) Coban (alex@ritlabs.com)}

interface
uses
  CryptoUtils;

type
  PHavalCtx = ^THavalCtx;
  THavalCtx = record
    count: array[0..1] of LongWord;                 // number of bits in a message
    fingerprint: array[0..7] of LongWord;           // current state of fingerprint
    block: array[0..31] of LongWord;                // buffer for a 32-word block
  end;


  procedure HavalInit(var state: THavalCtx);
  procedure HavalUpdate(var state: THavalCtx; str: Pointer; str_len, PASS: LongWord);
  function HavalFinal(var state: THavalCtx; PASS, HashLen: LongWord): String;


implementation
const
  VERSION = 1;                                      //Haval version, could be changed if necessary

  padding: array[0..127] of Byte = (
    $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  );

function f_1(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := ((x1) and ((x0) xor (x4)) xor (x2) and (x5) xor
    (x3) and (x6) xor (x0));
end;

function f_2(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := ((x2) and ((x1) and (not x3) xor (x4) and (x5) xor (x6) xor (x0)) xor
    (x4) and ((x1) xor (x5)) xor (x3) and (x5) xor (x0));
end;

function f_3(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := ((x3) and ((x1) and (x2) xor (x6) xor (x0)) xor
    (x1) and (x4) xor (x2) and (x5) xor (x0));
end;

function f_4(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := ((x4) and ((x5) and (not x2) xor (x3) and (not x6) xor (x1) xor (x6) xor (x0)) xor
    (x3) and ((x1) and (x2) xor (x5) xor (x6)) xor
    (x2) and (x6) xor (x0));
end;

function f_5(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := ((x0) and ((x1) and (x2) and (x3) xor (not x5)) xor
    (x1) and (x4) xor (x2) and (x5) xor (x3) and (x6));
end;

function Fphi_1p3(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_1(x1, x0, x3, x5, x6, x2, x4);
end;

function Fphi_1p4(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_1(x2, x6, x1, x4, x5, x3, x0);
end;

function Fphi_1neut(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_1(x3, x4, x1, x0, x5, x2, x6);
end;

function Fphi_1(x6, x5, x4, x3, x2, x1, x0, PASS: LongWord): LongWord;
begin
  if PASS = 3 then
    Result := Fphi_1p3(x6, x5, x4, x3, x2, x1, x0)
  else if PASS = 4 then
    Result := Fphi_1p4(x6, x5, x4, x3, x2, x1, x0)
  else
    Result := Fphi_1neut(x6, x5, x4, x3, x2, x1, x0);
end;

function Fphi_2p3(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_2(x4, x2, x1, x0, x5, x3, x6);
end;

function Fphi_2p4(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_2(x3, x5, x2, x0, x1, x6, x4)
end;

function Fphi_2neut(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_2(x6, x2, x1, x0, x3, x4, x5);
end;

function Fphi_2(x6, x5, x4, x3, x2, x1, x0, PASS: LongWord): LongWord;
begin
  if PASS = 3 then
    Result := Fphi_2p3(x6, x5, x4, x3, x2, x1, x0)
  else if PASS = 4 then
    Result := Fphi_2p4(x6, x5, x4, x3, x2, x1, x0)
  else
    Result := Fphi_2neut(x6, x5, x4, x3, x2, x1, x0);
end;

function Fphi_3p3(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_3(x6, x1, x2, x3, x4, x5, x0);
end;

function Fphi_3p4(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_3(x1, x4, x3, x6, x0, x2, x5);
end;

function Fphi_3neut(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_3(x2, x6, x0, x4, x3, x1, x5)
end;

function Fphi_3(x6, x5, x4, x3, x2, x1, x0, PASS: LongWord): LongWord;
begin
  if PASS = 3 then
    Result := Fphi_3p3(x6, x5, x4, x3, x2, x1, x0)
  else if PASS = 4 then
    Result := Fphi_3p4(x6, x5, x4, x3, x2, x1, x0)
  else
    Result := Fphi_3neut(x6, x5, x4, x3, x2, x1, x0);
end;

function Fphi_4p4(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_4(x6, x4, x0, x5, x2, x1, x3)
end;

function Fphi_4neut(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_4(x1, x5, x3, x2, x0, x4, x6);
end;

function Fphi_4(x6, x5, x4, x3, x2, x1, x0, PASS: LongWord): LongWord;
begin
  if PASS = 4 then
    Result := Fphi_4p4(x6, x5, x4, x3, x2, x1, x0)
  else
    Result := Fphi_4neut(x6, x5, x4, x3, x2, x1, x0);
end;

function Fphi_5(x6, x5, x4, x3, x2, x1, x0: LongWord): LongWord;
begin
  Result := f_5(x2, x5, x0, x6, x4, x3, x1);
end;

procedure FF_1(var x7: LongWord; x6, x5, x4, x3, x2, x1, x0, w, PASS: LongWord);
var
  temp: LongWord;
begin
  temp := Fphi_1(x6, x5, x4, x3, x2, x1, x0, PASS);
  x7 := ror(temp, 7) + ror(x7, 11) + w;
end;

procedure FF_2(var x7: LongWord; x6, x5, x4, x3, x2, x1, x0, w, c, PASS: LongWord);
var
  temp: LongWord;
begin
  temp := Fphi_2(x6, x5, x4, x3, x2, x1, x0, PASS);
  x7 := ror(temp, 7) + ror(x7, 11) + w + c;
end;

procedure FF_3(var x7: LongWord; x6, x5, x4, x3, x2, x1, x0, w, c, PASS: LongWord);
var
  temp: LongWord;
begin
  temp := Fphi_3(x6, x5, x4, x3, x2, x1, x0, PASS);
  x7 := ror(temp, 7) + ror(x7, 11) + w + c;
end;

procedure FF_4(var x7: LongWord; x6, x5, x4, x3, x2, x1, x0, w, c, PASS: LongWord);
var
  temp: LongWord;
begin
  temp := Fphi_4(x6, x5, x4, x3, x2, x1, x0, PASS);
  x7 := ror(temp, 7) + ror(x7, 11) + w + c;
end;

procedure FF_5(var x7: LongWord; x6, x5, x4, x3, x2, x1, x0, w, c: LongWord);
var
  temp: LongWord;
begin
  temp := Fphi_5(x6, x5, x4, x3, x2, x1, x0);
  x7 := ror(temp, 7) + ror(x7, 11) + w + c;
end;

procedure haval_hash_block(var state: THavalCtx; PASS: LongWord);
var
  t0, t1, t2, t3, t4, t5, t6, t7: LongWord;
  w: Pointer;
begin
  t0 := state.fingerprint[0];
  t1 := state.fingerprint[1];
  t2 := state.fingerprint[2];
  t3 := state.fingerprint[3];
  t4 := state.fingerprint[4];
  t5 := state.fingerprint[5];
  t6 := state.fingerprint[6];
  t7 := state.fingerprint[7];
  w := @state.block;

  // Pass 1
  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(w)^, PASS);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 1 * 4)^, PASS);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) + 2 * 4)^, PASS);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) + 3 * 4)^, PASS);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 4 * 4)^, PASS);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) + 5 * 4)^, PASS);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) + 6 * 4)^, PASS);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 7 * 4)^, PASS);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) + 8 * 4)^, PASS);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 9 * 4)^, PASS);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +10 * 4)^, PASS);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +11 * 4)^, PASS);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +12 * 4)^, PASS);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +13 * 4)^, PASS);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +14 * 4)^, PASS);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +15 * 4)^, PASS);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +16 * 4)^, PASS);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +17 * 4)^, PASS);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +18 * 4)^, PASS);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +19 * 4)^, PASS);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +20 * 4)^, PASS);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +21 * 4)^, PASS);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +22 * 4)^, PASS);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +23 * 4)^, PASS);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +24 * 4)^, PASS);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +25 * 4)^, PASS);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +26 * 4)^, PASS);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +27 * 4)^, PASS);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +28 * 4)^, PASS);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +29 * 4)^, PASS);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +30 * 4)^, PASS);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +31 * 4)^, PASS);

  // Pass 2
  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) + 5 * 4)^, $452821E6, PASS);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +14 * 4)^, $38D01377, PASS);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +26 * 4)^, $BE5466CF, PASS);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +18 * 4)^, $34E90C6C, PASS);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +11 * 4)^, $C0AC29B7, PASS);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +28 * 4)^, $C97C50DD, PASS);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) + 7 * 4)^, $3F84D5B5, PASS);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +16 * 4)^, $B5470917, PASS);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(w)^, $9216D5D9, PASS);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +23 * 4)^, $8979FB1B, PASS);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +20 * 4)^, $D1310BA6, PASS);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +22 * 4)^, $98DFB5AC, PASS);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 1 * 4)^, $2FFD72DB, PASS);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +10 * 4)^, $D01ADFB7, PASS);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) + 4 * 4)^, $B8E1AFED, PASS);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 8 * 4)^, $6A267E96, PASS);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +30 * 4)^, $BA7C9045, PASS);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 3 * 4)^, $F12C7F99, PASS);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +21 * 4)^, $24A19947, PASS);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) + 9 * 4)^, $B3916CF7, PASS);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +17 * 4)^, $0801F2E2, PASS);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +24 * 4)^, $858EFC16, PASS);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +29 * 4)^, $636920D8, PASS);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 6 * 4)^, $71574E69, PASS);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +19 * 4)^, $A458FEA3, PASS);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +12 * 4)^, $F4933D7E, PASS);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +15 * 4)^, $0D95748F, PASS);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +13 * 4)^, $728EB658, PASS);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 2 * 4)^, $718BCD58, PASS);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +25 * 4)^, $82154AEE, PASS);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +31 * 4)^, $7B54A41D, PASS);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +27 * 4)^, $C25A59B5, PASS);

  // Pass 3
  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +19 * 4)^, $9C30D539, PASS);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 9 * 4)^, $2AF26013, PASS);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) + 4 * 4)^, $C5D1B023, PASS);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +20 * 4)^, $286085F0, PASS);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +28 * 4)^, $CA417918, PASS);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +17 * 4)^, $B8DB38EF, PASS);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) + 8 * 4)^, $8E79DCB0, PASS);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +22 * 4)^, $603A180E, PASS);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +29 * 4)^, $6C9E0E8B, PASS);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +14 * 4)^, $B01E8A3E, PASS);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +25 * 4)^, $D71577C1, PASS);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +12 * 4)^, $BD314B27, PASS);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +24 * 4)^, $78AF2FDA, PASS);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +30 * 4)^, $55605C60, PASS);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +16 * 4)^, $E65525F3, PASS);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +26 * 4)^, $AA55AB94, PASS);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +31 * 4)^, $57489862, PASS);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +15 * 4)^, $63E81440, PASS);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) + 7 * 4)^, $55CA396A, PASS);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) + 3 * 4)^, $2AAB10B6, PASS);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 1 * 4)^, $B4CC5C34, PASS);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(w)^, $1141E8CE, PASS);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +18 * 4)^, $A15486AF, PASS);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +27 * 4)^, $7C72E993, PASS);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +13 * 4)^, $B3EE1411, PASS);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 6 * 4)^, $636FBC2A, PASS);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +21 * 4)^, $2BA9C55D, PASS);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +10 * 4)^, $741831F6, PASS);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +23 * 4)^, $CE5C3E16, PASS);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +11 * 4)^, $9B87931E, PASS);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) + 5 * 4)^, $AFD6BA33, PASS);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 2 * 4)^, $6C24CF5C, PASS);

  // Pass 4. executed only when PASS = 4 or 5
  if PASS > 3 then
  begin
    FF_4(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +24 * 4)^, $7A325381, PASS);
    FF_4(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 4 * 4)^, $28958677, PASS);
    FF_4(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(w)^, $3B8F4898, PASS);
    FF_4(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +14 * 4)^, $6B4BB9AF, PASS);
    FF_4(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 2 * 4)^, $C4BFE81B, PASS);
    FF_4(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) + 7 * 4)^, $66282193, PASS);
    FF_4(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +28 * 4)^, $61D809CC, PASS);
    FF_4(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +23 * 4)^, $FB21A991, PASS);

    FF_4(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +26 * 4)^, $487CAC60, PASS);
    FF_4(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 6 * 4)^, $5DEC8032, PASS);
    FF_4(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +30 * 4)^, $EF845D5D, PASS);
    FF_4(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +20 * 4)^, $E98575B1, PASS);
    FF_4(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +18 * 4)^, $DC262302, PASS);
    FF_4(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +25 * 4)^, $EB651B88, PASS);
    FF_4(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +19 * 4)^, $23893E81, PASS);
    FF_4(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 3 * 4)^, $D396ACC5, PASS);

    FF_4(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +22 * 4)^, $0F6D6FF3, PASS);
    FF_4(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +11 * 4)^, $83F44239, PASS);
    FF_4(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +31 * 4)^, $2E0B4482, PASS);
    FF_4(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +21 * 4)^, $A4842004, PASS);
    FF_4(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 8 * 4)^, $69C8F04A, PASS);
    FF_4(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +27 * 4)^, $9E1F9B5E, PASS);
    FF_4(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +12 * 4)^, $21C66842, PASS);
    FF_4(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) + 9 * 4)^, $F6E96C9A, PASS);

    FF_4(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) + 1 * 4)^, $670C9C61, PASS);
    FF_4(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +29 * 4)^, $ABD388F0, PASS);
    FF_4(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) + 5 * 4)^, $6A51A0D2, PASS);
    FF_4(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +15 * 4)^, $D8542F68, PASS);
    FF_4(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +17 * 4)^, $960FA728, PASS);
    FF_4(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +10 * 4)^, $AB5133A3, PASS);
    FF_4(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +16 * 4)^, $6EEF0B6C, PASS);
    FF_4(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +13 * 4)^, $137A3BE4, PASS);
  end;

  // Pass 5. executed only when PASS = 5
  if PASS = 5 then
  begin
    FF_5(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +27 * 4)^, $BA3BF050);
    FF_5(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 3 * 4)^, $7EFB2A98);
    FF_5(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +21 * 4)^, $A1F1651D);
    FF_5(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +26 * 4)^, $39AF0176);
    FF_5(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +17 * 4)^, $66CA593E);
    FF_5(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) +11 * 4)^, $82430E88);
    FF_5(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +20 * 4)^, $8CEE8619);
    FF_5(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +29 * 4)^, $456F9FB4);

    FF_5(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) +19 * 4)^, $7D84A5C3);
    FF_5(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(w)^, $3B8B5EBE);
    FF_5(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +12 * 4)^, $E06F75D8);
    FF_5(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) + 7 * 4)^, $85C12073);
    FF_5(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +13 * 4)^, $401A449F);
    FF_5(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) + 8 * 4)^, $56C16AA6);
    FF_5(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +31 * 4)^, $4ED3AA62);
    FF_5(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +10 * 4)^, $363F7706);

    FF_5(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) + 5 * 4)^, $1BFEDF72);
    FF_5(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) + 9 * 4)^, $429B023D);
    FF_5(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +14 * 4)^, $37D0D724);
    FF_5(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +30 * 4)^, $D00A1248);
    FF_5(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) +18 * 4)^, $DB0FEAD3);
    FF_5(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) + 6 * 4)^, $49F1C09B);
    FF_5(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +28 * 4)^, $075372C9);
    FF_5(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +24 * 4)^, $80991B7B);

    FF_5(t7, t6, t5, t4, t3, t2, t1, t0, PLongWord(LongWord(w) + 2 * 4)^, $25D479D8);
    FF_5(t6, t5, t4, t3, t2, t1, t0, t7, PLongWord(LongWord(w) +23 * 4)^, $F6E8DEF7);
    FF_5(t5, t4, t3, t2, t1, t0, t7, t6, PLongWord(LongWord(w) +16 * 4)^, $E3FE501A);
    FF_5(t4, t3, t2, t1, t0, t7, t6, t5, PLongWord(LongWord(w) +22 * 4)^, $B6794C3B);
    FF_5(t3, t2, t1, t0, t7, t6, t5, t4, PLongWord(LongWord(w) + 4 * 4)^, $976CE0BD);
    FF_5(t2, t1, t0, t7, t6, t5, t4, t3, PLongWord(LongWord(w) + 1 * 4)^, $04C006BA);
    FF_5(t1, t0, t7, t6, t5, t4, t3, t2, PLongWord(LongWord(w) +25 * 4)^, $C1A94FB6);
    FF_5(t0, t7, t6, t5, t4, t3, t2, t1, PLongWord(LongWord(w) +15 * 4)^, $409F60C4);
  end;

  Inc(state.fingerprint[0], t0);
  Inc(state.fingerprint[1], t1);
  Inc(state.fingerprint[2], t2);
  Inc(state.fingerprint[3], t3);
  Inc(state.fingerprint[4], t4);
  Inc(state.fingerprint[5], t5);
  Inc(state.fingerprint[6], t6);
  Inc(state.fingerprint[7], t7);
end;

procedure HavalInit(var state: THavalCtx);
begin
  state.count[0] := 0;
  state.count[1] := 0;
  state.fingerprint[0] := $243F6A88;
  state.fingerprint[1] := $85A308D3;
  state.fingerprint[2] := $13198A2E;
  state.fingerprint[3] := $03707344;
  state.fingerprint[4] := $A4093822;
  state.fingerprint[5] := $299F31D0;
  state.fingerprint[6] := $082EFA98;
  state.fingerprint[7] := $EC4E6C89;
end;


procedure HavalUpdate(var state: THavalCtx; str: Pointer; str_len, PASS: LongWord);
var
  i, rmd_len, fill_len: LongWord;
begin
  if PASS < 3 then PASS := 3;
  if PASS > 5 then PASS := 5;

  rmd_len  := (state.count[0] shr 3) and $7F;
  fill_len := 128 - rmd_len;

  state.count[0] := state.count[0] + (str_len shl 3);
  if state.count[0] < (str_len shl 3) then
    Inc(state.count[1]);

  state.count[1] := state.count[1] + (str_len shr 29);
  if (rmd_len + str_len >= 128) then
  begin
    Move(str^, Ptr(LongWord(@state.block) + rmd_len)^, fill_len);
    haval_hash_block (state, PASS);
    i := fill_len;
    while i + 127 < str_len do
    begin
      Move(Ptr(LongWord(str) + i)^, state.block, 128);
      haval_hash_block(state, PASS);
      Inc(i, 128);
    end;
    rmd_len := 0;
  end
  else
    i := 0;
  Move(Ptr(LongWord(str) + i)^, Ptr(LongWord(@state.block) + rmd_len)^, str_len - i);
end;

procedure haval_tailor(var state: THavalCtx; FPTLEN: LongWord);
var
  temp: LongWord;
begin
  if FPTLEN = 128 then
  begin
    temp := (state.fingerprint[7] and $000000FF) or
            (state.fingerprint[6] and $FF000000) or
            (state.fingerprint[5] and $00FF0000) or
            (state.fingerprint[4] and $0000FF00);
    state.fingerprint[0] := state.fingerprint[0] + ror(temp,  8);

    temp := (state.fingerprint[7] and $0000FF00) or
            (state.fingerprint[6] and $000000FF) or
            (state.fingerprint[5] and $FF000000) or
            (state.fingerprint[4] and $00FF0000);
    state.fingerprint[1] := state.fingerprint[1] + ror(temp, 16);

    temp := (state.fingerprint[7] and $00FF0000) or
            (state.fingerprint[6] and $0000FF00) or
            (state.fingerprint[5] and $000000FF) or
            (state.fingerprint[4] and $FF000000);
    state.fingerprint[2] := state.fingerprint[2] + ror(temp, 24);

    temp := (state.fingerprint[7] and $FF000000) or
            (state.fingerprint[6] and $00FF0000) or
            (state.fingerprint[5] and $0000FF00) or
            (state.fingerprint[4] and $000000FF);
    state.fingerprint[3] := state.fingerprint[3] + temp;
  end
  else if FPTLEN = 160 then
  begin
    temp := (state.fingerprint[7] and  $3F) or
            (state.fingerprint[6] and ($7F shl 25)) or
            (state.fingerprint[5] and ($3F shl 19));
    state.fingerprint[0] := state.fingerprint[0] + ror(temp, 19);
    temp := (state.fingerprint[7] and ($3F shl  6)) or
            (state.fingerprint[6] and  $3F) or
            (state.fingerprint[5] and ($7F shl 25));
    state.fingerprint[1] := state.fingerprint[1] + ror(temp, 25);
    temp := (state.fingerprint[7] and ($7F shl 12)) or
            (state.fingerprint[6] and ($3F shl  6)) or
            (state.fingerprint[5] and  $3F);
    state.fingerprint[2] := state.fingerprint[2] + temp;
    temp := (state.fingerprint[7] and ($3F shl 19)) or
            (state.fingerprint[6] and ($7F shl 12)) or
            (state.fingerprint[5] and ($3F shl  6));
    state.fingerprint[3] := state.fingerprint[3] + (temp shr 6);
    temp := (state.fingerprint[7] and ($7F shl 25)) or
            (state.fingerprint[6] and ($3F shl 19)) or
            (state.fingerprint[5] and ($7F shl 12));
    state.fingerprint[4] := state.fingerprint[4] + (temp shr 12);
  end
  else if FPTLEN = 192 then
  begin
    temp := (state.fingerprint[7] and  $1F) or
            (state.fingerprint[6] and ($3F shl 26));
    state.fingerprint[0] := state.fingerprint[0] + ror(temp, 26);
    temp := (state.fingerprint[7] and ($1F shl  5)) or
            (state.fingerprint[6] and  $1F);
    state.fingerprint[1] := state.fingerprint[1] + temp;
    temp := (state.fingerprint[7] and ($3F shl 10)) or
           (state.fingerprint[6] and ($1F shl  5));
    state.fingerprint[2] := state.fingerprint[2] + (temp shr 5);
    temp := (state.fingerprint[7] and ($1F shl 16)) or
            (state.fingerprint[6] and ($3F shl 10));
    state.fingerprint[3] := state.fingerprint[3] + (temp shr 10);
    temp := (state.fingerprint[7] and ($1F shl 21)) or
            (state.fingerprint[6] and ($1F shl 16));
    state.fingerprint[4] := state.fingerprint[4] + (temp shr 16);
    temp := (state.fingerprint[7] and ($3F shl 26)) or
            (state.fingerprint[6] and ($1F shl 21));
    state.fingerprint[5] := state.fingerprint[5] + (temp shr 21);
  end
  else if FPTLEN = 224 then
  begin
    Inc(state.fingerprint[0], ((state.fingerprint[7] shr 27) and $1F));
    Inc(state.fingerprint[1], ((state.fingerprint[7] shr 22) and $1F));
    Inc(state.fingerprint[2], ((state.fingerprint[7] shr 18) and $0F));
    Inc(state.fingerprint[3], ((state.fingerprint[7] shr 13) and $1F));
    Inc(state.fingerprint[4], ((state.fingerprint[7] shr  9) and $0F));
    Inc(state.fingerprint[5], ((state.fingerprint[7] shr  4) and $1F));
    Inc(state.fingerprint[6], (state.fingerprint[7] and $0F));
  end;
end;

procedure uint2ch(_word, _string: Pointer; wlen: LongWord);
var
  wp: PLongWord;
  sp: PByte;
begin
  wp := _word;
  sp := _string;
  while (LongWord(wp) < LongWord(_word) + wlen * 4 {!}) do
  begin
    sp^ := Byte((wp^)        and $FF); Inc(sp);
    sp^ := Byte((wp^ shr  8) and $FF); Inc(sp);
    sp^ := Byte((wp^ shr 16) and $FF); Inc(sp);
    sp^ := Byte((wp^ shr 24) and $FF); Inc(sp);
    Inc(wp);
  end;
end;

procedure haval_end(var state: THavalCtx; var final_fpt: array of Byte; PASS: LongWord; FPTLEN: LongWord);
var
  tail: array[0..9] of Byte;
  rmd_len, pad_len: LongWord;
begin
  tail[0] := Byte(((FPTLEN and $3) shl 6) or
    ((PASS and $7) shl 3) or
    (VERSION and $7));
  tail[1] := Byte((FPTLEN shr 2) and $FF);

  uint2ch(@state.count, @tail[2], 2);

  // pad out to 118 mod 128
  rmd_len := Byte((state.count[0] shr 3) and $7f);
  if rmd_len < 118 then
    pad_len := 118 - rmd_len
  else
    pad_len := 246 - rmd_len;
  HavalUpdate(state, @padding, pad_len, PASS);

  HavalUpdate(state, @tail, 10, PASS);

  haval_tailor(state, FPTLEN);

  //uint2ch(@state.fingerprint, @final_fpt, FPTLEN shr 5);
  Move(state.fingerprint, final_fpt, FPTLEN shr 3);

  FillChar(state, SizeOf(State), 0);
end;

function HavalFinal(var state: THavalCtx; PASS, HashLen: LongWord): String;
var
  fingerprint: array[0..256 shr 3 - 1] of Byte;
  i: Byte;
begin
  if PASS < 3 then PASS := 3;
  if PASS > 5 then PASS := 5;
  haval_end(state, fingerprint, PASS, HashLen);  
  Result := '';
  for i := 0 to HashLen shr 3 - 1 do
    Result := Result + IntToHex(fingerprint[i], 2);
end;

end.
