{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHashFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.CheckLst, Vcl.ComCtrls, Generics.Collections, Vcl.Menus,
  Clipbrd, Math,

  {CryptoAPI,} HlpHashFactory, HlpIHash, HlpCRC, uHextorTypes, uEditorForm;

type
  EHashException = class(Exception);

  // Abstract hash function
  THashAlgorithm = class
  public
    Name: string;
    constructor Create(const AName: string);
    procedure Init(); virtual; abstract;
    procedure Update(Data: Pointer; Size: Integer); virtual; abstract;
    procedure Done(var Result: string); virtual; abstract;
    procedure Calculate(Data: Pointer; Size: Integer; var Res: string);
//    procedure Assign(Source: THashAlgorithm); virtual;
  end;

//  // Hash function from HashLib! library
//  THashLibHash = class(THashAlgorithm)
//  private
//    Context: THashContext;
//  public
//    HashType: LongWord;
//    procedure Init(); override;
//    procedure Update(Data: Pointer; Size: Integer); override;
//    procedure Done(var Result: string); override;
////    procedure Assign(Source: THashAlgorithm); override;
//  end;
//
//  // CRC16/CCITT
//  TCRC16Hash = class(THashAlgorithm)
//  private
//    Value: Word;
//  public
//    procedure Init(); override;
//    procedure Update(Data: Pointer; Size: Integer); override;
//    procedure Done(var Result: string); override;
//  end;

  // Hash function from HashLib4Pascal library
  THashLib4PascalHash = class(THashAlgorithm)
  private
    Hash: IHash;
  public
    procedure Init(); override;
    procedure Update(Data: Pointer; Size: Integer); override;
    procedure Done(var Result: string); override;
    constructor Create(AName: string; AHash: IHash);
  end;


  THashFrame = class(TFrame)
    Panel1: TPanel;
    Splitter1: TSplitter;
    RGDataRange: TRadioGroup;
    BtnCalculate: TButton;
    AlgorithmsListBox: TListBox;
    ResultPopupMenu: TPopupMenu;
    MICopy: TMenuItem;
    Panel2: TPanel;
    StaticText1: TStaticText;
    ResultListView: TListView;
    procedure BtnCalculateClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure AlgorithmsListBoxClick(Sender: TObject);
  private type
    //THashInitProc = reference to procedure (var Context: Pointer; );
  private
    { Private declarations }
//    FEditor: TEditorForm;
    procedure ShowAlgorithmsList();
    procedure RegisterBuiltInHashes();
  public
    { Public declarations }
    HashTypes: TObjectList<THashAlgorithm>;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function GetHashObject(const AlgorithmName: string): THashAlgorithm;
    procedure Calculate(FEditor: TEditorForm; Range: TFileRange; const Algorithms: array of string);
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

{ THashFrame }

procedure THashFrame.AlgorithmsListBoxClick(Sender: TObject);
begin
  BtnCalculate.Enabled := (AlgorithmsListBox.SelCount > 0);
end;

procedure THashFrame.BtnCalculateClick(Sender: TObject);
var
  i: Integer;
  Algorithms: array of string;
  Range: TFileRange;
  FEditor: TEditorForm;
begin
  FEditor := MainForm.ActiveEditor;
//  FEditor.OnClosed.Add(procedure(Sender: TEditorForm)
//    begin
//      FEditor := nil;
//    end);

  Algorithms := [];
  for i:=0 to AlgorithmsListBox.Items.Count-1 do
    if AlgorithmsListBox.Selected[i] then
      Algorithms := Algorithms + [AlgorithmsListBox.Items[i]];

  if RGDataRange.ItemIndex = 1 then
    Range := FEditor.SelectedRange
  else
    Range := TFileRange.Create(0, FEditor.Data.GetSize());

  Calculate(FEditor, Range, Algorithms);
end;

procedure THashFrame.Calculate(FEditor: TEditorForm; Range: TFileRange; const Algorithms: array of string);
const
  BlockSize = 1*MByte;
var
  a: Integer;
  HashObjs: array of THashAlgorithm;
  Hash: string;
  Data: TBytes;
  s: string;
  Ptr: TFilePointer;
  Size: Integer;
//  ctx: array of THashContext;
begin
  // Label above result list
  s := ExtractFileName(FEditor.DataSource.DisplayName);
  if (Range.Start <> 0) or (Range.AEnd <> FEditor.Data.GetSize()) then
    s := s + ' (Range ' + IntToStr(Range.Start) + '..' + IntToStr(Range.AEnd-1) + ')';
  StaticText1.Caption := s;

  SetLength(HashObjs, Length(Algorithms));
  for a:=0 to Length(Algorithms)-1 do
  begin
    HashObjs[a] := GetHashObject(Algorithms[a]);
    if HashObjs[a] <> nil then
      HashObjs[a].Init();
  end;

  ResultListView.Clear();

  Ptr := Range.Start;
  Progress.TaskStart(Self);
  try
    while Ptr < Range.AEnd do
    begin
      Size := Min(BlockSize, Range.AEnd - Ptr);

      Data := FEditor.Data.Get(Ptr, Size);

      for a:=0 to Length(Algorithms)-1 do
      begin
        if HashObjs[a] = nil then Continue;

        HashObjs[a].Update(@Data[0], Length(Data));

        Progress.Show((Ptr - Range.Start) + (Size * (a+1) div Length(Algorithms)), Range.Size);
      end;

      Ptr := Ptr + Size;

    end;

    for a:=0 to Length(Algorithms)-1 do
    begin
      if HashObjs[a] = nil then Continue;
      HashObjs[a].Done(Hash);

      with ResultListView.Items.Add() do
      begin
        Caption := Algorithms[a];
        SubItems.Add(Hash);
      end;
    end;

  finally
    Progress.TaskEnd();
  end;
end;

//const
//  HashesAvail: array[0..19] of String = (
//    'MD2', 'MD4', 'MD5', 'SHA1', 'SHA256', 'SHA384',
//    'SHA512', 'HAVAL128', 'HAVAL160', 'HAVAL192',
//    'HAVAl224', 'HAVAL256', {'GOST',} 'TIGER128',
//    'TIGER160', 'TIGER192', 'RIPE-MD128', 'RIPE-MD160',
//    'CRC32', 'CRC32b', 'Adler32'
//  );
//
//  HashesAvailID: array[0..19] of LongWord = (
//    HASH_MD2, HASH_MD4, HASH_MD5, HASH_SHA1, HASH_SHA256, HASH_SHA384,
//    HASH_SHA512, HASH_HAVAL128, HASH_HAVAL160, HASH_HAVAL192,
//    HASH_HAVAl224, HASH_HAVAL256, {HASH_GOST,} HASH_TIGER128,
//    HASH_TIGER160, HASH_TIGER192, HASH_RIPEMD128, HASH_RIPEMD160,
//    HASH_CRC32, HASH_CRC32B, HASH_ADLER32
//  );

constructor THashFrame.Create(AOwner: TComponent);
begin
  inherited;

  HashTypes := TObjectList<THashAlgorithm>.Create(True);
  RegisterBuiltInHashes();

  ShowAlgorithmsList();
end;

destructor THashFrame.Destroy;
begin
  HashTypes.Free;
  inherited;
end;

function THashFrame.GetHashObject(const AlgorithmName: string): THashAlgorithm;
var
  i: Integer;
begin
  for i:=0 to HashTypes.Count-1 do
    if SameText(HashTypes[i].Name, AlgorithmName) then
      Exit(HashTypes[i]);
  Result := nil;
end;

procedure THashFrame.MICopyClick(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  if ResultListView.SelCount = 1 then
    s := ResultListView.Selected.SubItems[0]
  else
  begin
    s := '';
    for i:=0 to ResultListView.Items.Count-1 do
      if ResultListView.Items[i].Selected then
        s := s + ResultListView.Items[i].Caption + #9 + ResultListView.Items[i].SubItems[0] + sLineBreak;
  end;
  Clipboard.AsText := StrToClipboard(s);
end;

procedure THashFrame.RegisterBuiltInHashes;
//var
//  i: Integer;
//  HLHash: THashLibHash;
begin
//  for i:=Low(HashesAvail) to High(HashesAvail) do
//  begin
//    HLHash := THashLibHash.Create('!'+HashesAvail[i]);
//    HLHash.HashType := HashesAvailID[i];
//    HashTypes.Add(HLHash);
//  end;
//
//  HashTypes.Add(TCRC16Hash.Create('?CRC16/CCITT'));

  HashTypes.Add(THashLib4PascalHash.Create('CRC16-CCITT-FALSE',  THashFactory.TChecksum.TCRC.CreateCRC(TCRCStandard.CRC16_CCITTFALSE) ));
  HashTypes.Add(THashLib4PascalHash.Create('CRC16-X25',    THashFactory.TChecksum.TCRC.CreateCRC(TCRCStandard.X25) ));
  HashTypes.Add(THashLib4PascalHash.Create('CRC16-XMODEM', THashFactory.TChecksum.TCRC.CreateCRC(TCRCStandard.XMODEM) ));
  HashTypes.Add(THashLib4PascalHash.Create('CRC32',        THashFactory.TChecksum.TCRC.CreateCRC32_PKZIP() ));
  HashTypes.Add(THashLib4PascalHash.Create('CRC32-C',      THashFactory.TChecksum.TCRC.CreateCRC32_CASTAGNOLI() ));
  HashTypes.Add(THashLib4PascalHash.Create('CRC64',        THashFactory.TChecksum.TCRC.CreateCRC64_ECMA_182() ));

  HashTypes.Add(THashLib4PascalHash.Create('MD2',          THashFactory.TCrypto.CreateMD2() ));
  HashTypes.Add(THashLib4PascalHash.Create('MD4',          THashFactory.TCrypto.CreateMD4() ));
  HashTypes.Add(THashLib4PascalHash.Create('MD5',          THashFactory.TCrypto.CreateMD5() ));

  HashTypes.Add(THashLib4PascalHash.Create('SHA-1',        THashFactory.TCrypto.CreateSHA1() ));
  HashTypes.Add(THashLib4PascalHash.Create('SHA-256',      THashFactory.TCrypto.CreateSHA2_256() ));
  HashTypes.Add(THashLib4PascalHash.Create('SHA-384',      THashFactory.TCrypto.CreateSHA2_384() ));
  HashTypes.Add(THashLib4PascalHash.Create('SHA-512',      THashFactory.TCrypto.CreateSHA2_512() ));
  HashTypes.Add(THashLib4PascalHash.Create('SHA-3-256',    THashFactory.TCrypto.CreateSHA3_256() ));

  HashTypes.Add(THashLib4PascalHash.Create('Adler32',      THashFactory.TChecksum.CreateAdler32() ));
  HashTypes.Add(THashLib4PascalHash.Create('RIPEMD-160',   THashFactory.TCrypto.CreateRIPEMD160() ));

end;

procedure THashFrame.ShowAlgorithmsList;
var
  i: Integer;
begin
  AlgorithmsListBox.Items.BeginUpdate();
  try
    AlgorithmsListBox.Clear();
    for i:=0 to HashTypes.Count-1 do
    begin
      AlgorithmsListBox.Items.Add(HashTypes[i].Name);
    end;
    AlgorithmsListBoxClick(nil);
  finally
    AlgorithmsListBox.Items.EndUpdate();
  end;
end;

{ THashAlgorithm }

//procedure THashAlgorithm.Assign(Source: THashAlgorithm);
//begin
//  Name := Source.Name;
//end;

procedure THashAlgorithm.Calculate(Data: Pointer; Size: Integer;
  var Res: string);
begin
  Init();
  Update(Data, Size);
  Done(Res);
end;

constructor THashAlgorithm.Create(const AName: string);
begin
  inherited Create();
  Name := AName;
end;

//{ THashLibHash }
//
////procedure THashLibHash.Assign(Source: THashAlgorithm);
////begin
////  inherited;
////  if Source is THashLibHash then
////  begin
////    HashType := (Source as THashLibHash).HashType;
////  end;
////end;
//
//procedure THashLibHash.Done(var Result: string);
//var
//  Res: Cardinal;
//begin
//  Res := HashFinal(@Context, Result);
//  if Res <> HASH_NOERROR then
//    raise EHashException.Create(HashErrorToStr(Res));
//end;
//
//procedure THashLibHash.Init;
//var
//  Res: Cardinal;
//begin
//  Res := HashInit(@Context, HashType);
//  if Res <> HASH_NOERROR then
//    raise EHashException.Create(HashErrorToStr(Res));
//end;
//
//procedure THashLibHash.Update(Data: Pointer; Size: Integer);
//var
//  Res: Cardinal;
//begin
//  Res := HashUpdate(@Context, Data, Size);
//  if Res <> HASH_NOERROR then
//    raise EHashException.Create(HashErrorToStr(Res));
//end;
//
//{ TCRC16Hash }
//
//procedure TCRC16Hash.Done(var Result: string);
//begin
//  Value := not Value;
//  Result := IntToHex(Value, 4);
//end;
//
//procedure TCRC16Hash.Init;
//begin
//  Value := $FFFF;
//end;
//
//procedure TCRC16Hash.Update(Data: Pointer; Size: Integer);
//var
//  i: Integer;
//  crc_hi,crc_lo: Byte;
//  t: Word;
//  v, X: Byte;
//begin
//  crc_hi:=Value shr 8; crc_lo:=Value and 255;
//  for i:=0 to Size-1 do
//  begin
//    X := PByteArray(Data)^[i];
//    t:=X xor crc_lo;
//    for v:=1 to 8 do
//    begin
//      if (t and 1)=1 then
//      begin
//        t:=t shr 1;
//        t:=t xor $8408;
//      end
//      else
//        t:=t shr 1;
//    end;
//    crc_lo:=t xor crc_hi;
//    crc_hi:=t shr 8;
//  end;
//  Value := crc_lo + (crc_hi shl 8);
//end;

{ THashLib4PascalHash }

constructor THashLib4PascalHash.Create(AName: string; AHash: IHash);
begin
  if AName = '' then
    AName := AHash.Name;
  inherited Create(AName);
  Hash := AHash;
end;

procedure THashLib4PascalHash.Done(var Result: string);
begin
  Result := Hash.TransformFinal.ToString();
end;

procedure THashLib4PascalHash.Init;
begin
  Hash.Initialize();
end;

procedure THashLib4PascalHash.Update(Data: Pointer; Size: Integer);
begin
  Hash.TransformUntyped(Data^, Size);
end;

end.
