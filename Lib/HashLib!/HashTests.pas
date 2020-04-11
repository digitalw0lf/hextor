unit HashTests;
{(C) Coban (alex@ritlabs.com)}

interface
uses
  CryptoAPI;

  function HashTestHash(HashType: LongWord): LongWord;  //Test a single hash
  function HashTestLibrary: LongWord;                   //Test whole lib

implementation
function SingleTest(HashType: LongWord; Src, Confirm: String): Boolean;
var
  S: String;
begin
  Result := False;
  if HashStr(HashType, Src, S) = HASH_NOERROR then
    if S = Confirm then
      Result := True;
end;

function HashTestHash(HashType: LongWord): LongWord;
var
  ret: Boolean;
  ctx: THashContext;
  S: String;
  dret: LongWord;
  buf: array[0..100] of AnsiChar;
begin
  Result := HASH_NOERROR;
  ret := False;
  case HashType of
    HASH_CRC32: ret := SingleTest(HashType, 'checksum', '2eb0be7f');//'7fbeb02e');
    HASH_CRC32B: ret := SingleTest(HashType, 'checksum', 'de6fdf9a');//'9adf6fde');
    HASH_ADLER32: ret := SingleTest(HashType, 'resume', '09150292');
    HASH_MD2:
    begin
      ret := SingleTest(HashType, '', '8350e5a3e24c153df2275c9f80692773');
      ret := ret and SingleTest(HashType, 'a', '32ec01ec4a6dac72c0ab96fb34c0b5d1');
      ret := ret and SingleTest(HashType, 'abc', 'da853b0d3f88d99b30283a69e6ded6bb');
      ret := ret and SingleTest(HashType, 'message digest', 'ab4f496bfb2a530b219ff33031fe06b0');
      ret := ret and SingleTest(HashType, 'abcdefghijklmnopqrstuvwxyz', '4e8ddff3650292ab5a4108c3aa47940b');
      ret := ret and SingleTest(HashType, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 'da33def2a42df13975352846c30338cd');
      ret := ret and SingleTest(HashType, '12345678901234567890123456789012345678901234567890123456789012345678901234567890', 'd5976f79d83d3a0dc9806c3c66f3efd8');
    end;
    HASH_MD4:
    begin
      ret := SingleTest(HashType, '', '31d6cfe0d16ae931b73c59d7e0c089c0');
      ret := ret and SingleTest(HashType, 'a', 'bde52cb31de33e46245e05fbdbd6fb24');
      ret := ret and SingleTest(HashType, 'abc', 'a448017aaf21d8525fc10ae87aa6729d');
      ret := ret and SingleTest(HashType, 'message digest', 'd9130a8164549fe818874806e1c7014b');
      ret := ret and SingleTest(HashType, 'abcdefghijklmnopqrstuvwxyz', 'd79e1c308aa5bbcdeea8ed63df412da9');
      ret := ret and SingleTest(HashType, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', '043f8582f241db351ce627e153e7f0e4');
    end;
    HASH_MD5:
    begin
      ret := SingleTest(HashType, '', 'd41d8cd98f00b204e9800998ecf8427e');
      ret := ret and SingleTest(HashType, 'a', '0cc175b9c0f1b6a831c399e269772661');
      ret := ret and SingleTest(HashType, 'abc', '900150983cd24fb0d6963f7d28e17f72');
      ret := ret and SingleTest(HashType, 'message digest', 'f96b697d7cb7938d525a2f31aaf161d0');
      ret := ret and SingleTest(HashType, 'abcdefghijklmnopqrstuvwxyz', 'c3fcd3d76192e4007dfb496cca67e13b');
      ret := ret and SingleTest(HashType, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 'd174ab98d277d9f5a5611c2c9f419d9f');
      ret := ret and SingleTest(HashType, '12345678901234567890123456789012345678901234567890123456789012345678901234567890', '57edf4a22be3c955ac49da2e2107b67a');
    end;
    HASH_SHA1:
    begin
      ret := SingleTest(HashType, 'abc', 'a9993e364706816aba3e25717850c26c9cd0d89d');
      ret := ret and SingleTest(HashType, 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', '84983e441c3bd26ebaae4aa1f95129e5e54670f1');
    end;
    HASH_SHA256:
    begin
      ret := SingleTest(HashType, 'abc', 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
      ret := ret and SingleTest(HashType, 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1');
    end;
    HASH_SHA384:
    begin
      ret := SingleTest(HashType, 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu',
        '09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039');
    end;
    HASH_SHA512:
    begin
      ret := SingleTest(HashType, 'abc', 'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f');
      ret := ret and SingleTest(HashType, 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu',
        '8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909');
    end;
    HASH_TIGER128, HASH_TIGER160, HASH_TIGER192:
    begin
      ret := SingleTest(HASH_TIGER192, '', '24f0130c63ac933216166e76b1bb925ff373de2d49584e7a');
      ret := ret and SingleTest(HASH_TIGER192, 'abc', 'f258c1e88414ab2a527ab541ffc5b8bf935f7b951c132951');
      ret := ret and SingleTest(HASH_TIGER192, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ=abcdefghijklmnopqrstuvwxyz+0123456789',
        '467db80863ebce488df1cd1261655de957896565975f9197');
      ret := ret and SingleTest(HASH_TIGER192, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-',
        '00b83eb4e53440c576ac6aaee0a7485825fd15e70a59ffe4');
    end;
    HASH_GOST:
    begin
      ret := SingleTest(HashType, 'This is message, length=32 bytes',
        'b1c466d37519b82e8319819ff32595e047a28cb6f83eff1c6916a815a637fffa');
      ret := ret and SingleTest(HashType, 'Suppose the original message has length = 50 bytes',
        '471aba57a60a770d3a76130635c1fbea4ef14de51f78b4ae57dd893b62f55208');
    end;
    HASH_RIPEMD128:
    begin
      ret := SingleTest(HashType, '', 'cdf26213a150dc3ecb610f18f6b38b46');
      ret := ret and SingleTest(HashType, 'abc', 'c14a12199c66e4ba84636b0f69144c77');
      ret := ret and SingleTest(HashType, 'message digest', '9e327b3d6e523062afc1132d7df9d1b8');
      ret := ret and SingleTest(HashType, 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', 'e91806709fd9928ad92be015aeebc562');
      ret := ret and SingleTest(HashType, '12345678901234567890123456789012345678901234567890123456789012345678901234567890', '3f45ef194732c2dbb2c4a2c769795fa3');
    end;
    HASH_RIPEMD160:
    begin
      ret := SingleTest(HashType, '', '9c1185a5c5e9fc54612808977ee8f548b2258d31');
      ret := ret and SingleTest(HashType, 'abc', '8eb208f7e05d987a9b044a8e98c6b087f15a0bfc');
      ret := ret and SingleTest(HashType, 'message digest', '5d0689ef49d2fae572b881b123a85ffa21595f36');
      ret := ret and SingleTest(HashType, 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', '69a155bddf855b0973a0791d5b7a3326fb83e163');
      ret := ret and SingleTest(HashType, '12345678901234567890123456789012345678901234567890123456789012345678901234567890', '9b752e45573d4b39f4dbd3323cab82bf63326bfb');
    end;
    HASH_HAVAL128: ret := SingleTest(HashType, '', '1bdc556b29ad02ec09af8c66477f2a87');
    HASH_HAVAL160: ret := SingleTest(HashType, 'a', '5e1610fced1d3adb0bb18e92ac2b11f0bd99d8ed');
    HASH_HAVAL192:
    begin
      dret := HashInit(@ctx, HashType);
      if dret = HASH_NOERROR then
      begin
        S := '';
        buf := 'HAVAL'#0;
        ctx.lParam := 4;
        HashUpdate(@ctx, @buf, 5);
        dret := HashFinal(@ctx, S);
        if dret = HASH_NOERROR then
          if S = '74aa31182ff09bcce453a7f71b5a7c5e80872fa90cd93ae4' then
            ret := True;
      end;
    end;
    HASH_HAVAL224:
    begin
      dret := HashInit(@ctx, HashType);
      if dret = HASH_NOERROR then
      begin
        S := '';
        buf := '0123456789'#0;
        ctx.lParam := 4;
        HashUpdate(@ctx, @buf, 10);
        dret := HashFinal(@ctx, S);
        if dret = HASH_NOERROR then
          if S = '144cb2de11f05df7c356282a3b485796da653f6b702868c7dcf4ae76' then
            ret := True;
      end;
    end;
    HASH_HAVAL256:
    begin
      dret := HashInit(@ctx, HashType);
      if dret = HASH_NOERROR then
      begin
        S := '';
        buf := 'abcdefghijklmnopqrstuvwxyz'#0;
        ctx.lParam := 5;
        HashUpdate(@ctx, @buf, 26);
        dret := HashFinal(@ctx, S);
        if dret = HASH_NOERROR then
          if S = '1a1dc8099bdaa7f35b4da4e805f1a28fee909d8dee920198185cbcaed8a10a8d' then
            ret := True;
      end;
      dret := HashInit(@ctx, HashType);
      if dret = HASH_NOERROR then
      begin
        S := '';
        buf := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'#0;
        ctx.lParam := 5;
        HashUpdate(@ctx, @buf, 62);
        dret := HashFinal(@ctx, S);
        if dret = HASH_NOERROR then
        begin
          if S = 'c5647fc6c1877fff96742f27e9266b6874894f41a08f5913033d9d532aeddb39' then
            ret := ret and True
          else
            ret := False;
        end
        else
          ret := False;
      end;
    end;
    else
      Result := HASH_UNK_TYPE;
  end;
  if Result <> HASH_UNK_TYPE then
    if not ret then
      Result := HASH_TEST_FAILED;
end;

function HashTestLibrary: LongWord;
var
  i, HAv, Res: LongWord;
  EnumArray: array[0..HASH_MAX_TYPES - 1] of LongWord;  
begin
  Result := HASH_NOERROR;
  HAv := EnumHashTypes(@EnumArray, SizeOf(EnumArray));
  for i := 0 to HAv - 1 do
  begin
    Res := HashTestHash(EnumArray[i]);
    if Res <> HASH_NOERROR then
    begin
      Result := Res;
      Exit;
    end;
  end;
end;


end.
