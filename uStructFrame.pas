unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,

  uUtil, uDWHexTypes, uLogFile, Vcl.StdCtrls, Vcl.ComCtrls;

type
  EDSParserError = class (Exception);

  TDSFieldKind = (fkUnknown, fkSimple, fkArray, fkStruct);
  TDSSimpleDataType = string;  // e.g. 'uint16'

  // Base class for all elements
  TDSField = class
  public
    Kind: TDSFieldKind;
    Name: string;
    constructor Create(); virtual;
    procedure Assign(Source: TDSField); virtual;
    function Duplicate(): TDSField;
  end;
  TDSFieldClass = class of TDSField;

  // Simple data types - integers, floats
  TDSSimpleField = class (TDSField)
  public
    DataType: TDSSimpleDataType;
    Data: TBytes;
    constructor Create(); override;
    procedure Assign(Source: TDSField); override;
  end;

  // For arrays and structures
  TDSCompoundField = class (TDSField)
  public
    Fields: TObjectList<TDSField>;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); override;
  end;

  TDSArray = class (TDSCompoundField)
    constructor Create(); override;
  end;

  TDSStruct = class (TDSCompoundField)
    constructor Create(); override;
  end;

  // Class for parsing structure description to DS's
  TDSParser = class
  private
    BufStart, BufEnd, Ptr: PChar;
    function CharValidInName(C: Char): Boolean;
    procedure CheckValidName(const S: string);
    function ReadChar(): Char;
    procedure PutBack();
    function ReadLexem(): string;
    function PeekLexem(): string;
    function ReadType(): TDSField;
    function ReadStruct(): TDSStruct;
    function MakeArray(AType: TDSField; Count: Integer): TDSArray;
  public
    function ParseStruct(const Descr: string): TDSStruct;
  end;

  // Class for populating DataStructure from binary buffer
  TDSInterpretor = class
  private
    function GetFieldSize(const AType: string): Integer;
    procedure InterpretSimple(DS: TDSSimpleField; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretCompound(DS: TDSCompoundField; var Buf: Pointer; BufEnd: Pointer);
  public
    procedure Interpret(DS: TDSField; var Buf: Pointer; BufEnd: Pointer);
  end;

  TStructFrame = class(TFrame)
    Memo1: TMemo;
    Button1: TButton;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FParser: TDSParser;
    FInterpretor: TDSInterpretor;
    procedure ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Analyze(Addr: TFilePointer; const Data: TBytes; const Struct: string);
  end;

implementation

uses
  uValueFrame, uMainForm;

{$R *.dfm}

{ TStructFrame }

procedure TStructFrame.Analyze(Addr: TFilePointer; const Data: TBytes;
  const Struct: string);
var
  DS: TDSField;
  Buf, BufEnd: Pointer;
  i: Integer;
begin
  DS := FParser.ParseStruct(Memo1.Text);

  GetStartEndPointers(Data, Buf, BufEnd);

  FInterpretor.Interpret(DS, Buf, BufEnd);

  TreeView1.Items.Clear();

  with TDSCompoundField(DS) do
    for i:=0 to Fields.Count-1 do
      ShowStructTree(Fields[i], nil);

  TreeView1.Items[0].Expand(True);
end;

{ TDSParser }

function TDSParser.CharValidInName(C: Char): Boolean;
begin
  Result := (IsCharAlphaNumeric(C)) or (C='_');
end;

procedure TDSParser.CheckValidName(const S: string);
var
  i: Integer;
begin
  if S = '' then raise EDSParserError.Create('Empty name');
//  if not IsCharAlpha(S[Low(S)]) then raise EDSParserError.Create('Invalid name: ' + S);
  for i:=Low(S) to High(S) do
  begin
    if (not CharValidInName(S[i])) or
       ((i = Low(S)) and (CharInSet(S[i], ['0'..'9']))) then raise EDSParserError.Create('Invalid name: ' + S);
  end;
end;

function TDSParser.MakeArray(AType: TDSField; Count: Integer): TDSArray;
// Create Array of Count elements of type AType
var
  i: Integer;
  Element: TDSField;
begin
  Result := TDSArray.Create();
  for i:=0 to Count-1 do
  begin
    Element := AType.Duplicate();
    Element.Name := IntToStr(i);
    Result.Fields.Add(Element);
  end;
end;

function TDSParser.ParseStruct(const Descr: string): TDSStruct;
begin
  BufStart := @Descr[Low(Descr)];
  BufEnd := @Descr[High(Descr)+1];
  Ptr := BufStart;

  Result := ReadStruct();
end;

function TDSParser.PeekLexem: string;
// Returns next lexem without moving current pointer
var
  OldPtr: PChar;
begin
  OldPtr := Ptr;
  try
    Result := ReadLexem();
  finally
    Ptr := OldPtr;
  end;
end;

procedure TDSParser.PutBack;
begin
  if Ptr > BufStart then
    Dec(Ptr);
end;

function TDSParser.ReadChar: Char;
begin
  if Ptr = BufEnd then Exit(#0);
  Result := Ptr^;
  Inc(Ptr);
end;

function TDSParser.ReadLexem: string;
var
  First, C: Char;
begin
  // Skip spaces
  repeat
    First := ReadChar();
  until not CharInSet(First, [#9, #10, #13, ' ']);
  if First = #0 then Exit('');

  Result := First;
  // All punctuation is single-char
  if not CharValidInName(First) then Exit;

  // Read all digits and letters
  while True do
  begin
    C := ReadChar();
    if not CharValidInName(C) then Break;
    Result := Result + C;
  end;
  if C <> #0 then
    PutBack();
end;

function TDSParser.ReadStruct: TDSStruct;
// Read structure fields
var
  AType, AInstance: TDSField;
  AName, S, ACount: string;
  Count: Integer;
begin
  Result := TDSStruct.Create();

  while (Ptr < BufEnd) do
  begin
    if PeekLexem() = '}' then
    begin
      ReadLexem();
      Break;
    end;

    // Read type description
    AType := ReadType();

    if AType = nil then Break;

    try
      // Read field names
      repeat
        AName := ReadLexem();
        CheckValidName(AName);

        S := ReadLexem();
        if S = '[' then  // It is array
        begin
          // Read array size
          ACount := ReadLexem();
          Count := StrToInt(ACount);
          if ReadLexem() <> ']' then
            raise EDSParserError.Create('"]" expected');
          // Create array of Count elements of given type
          AInstance := MakeArray(AType, Count);

          S := ReadLexem();  // "," or ";"
        end
        else
        begin
          AInstance := AType.Duplicate();
        end;

        AInstance.Name := AName;
        Result.Fields.Add(AInstance);

        WriteLogF('Struct', AnsiString(AInstance.ClassName+' '+AInstance.Name));

        if S = ',' then Continue;  // Next name
        if S = ';' then Break;     // End of line
        if (S = '') or (S = '}') then Exit;       // End of description

        raise EDSParserError.Create('";" or "," expected');
      until False;
    finally
      AType.Free;
    end;

//    if S = '}' then Break;
  end;
end;

function TDSParser.ReadType: TDSField;
var
  S: string;
begin
  Result := nil;
  S := ReadLexem;
  if S = '' then Exit;
  if S = '{' then
  begin
    Result := ReadStruct();
  end
  else
  begin
    CheckValidName(S);
    Result := TDSSimpleField.Create();
    TDSSimpleField(Result).DataType := S;
  end;
end;

{ TDSSimpleField }

procedure TDSSimpleField.Assign(Source: TDSField);
begin
  inherited;
  if Source is TDSSimpleField then
  begin
    DataType := (Source as TDSSimpleField).DataType;
    Data := Copy((Source as TDSSimpleField).Data);
  end;
end;

constructor TDSSimpleField.Create;
begin
  inherited;
  Kind := fkSimple;
end;

{ TDSCompoundField }

procedure TDSCompoundField.Assign(Source: TDSField);
var
  i: Integer;
begin
  inherited;
  if Source is TDSCompoundField then
  begin
    Fields.Clear();
    for i:=0 to (Source as TDSCompoundField).Fields.Count-1 do
    begin
      Fields.Add( (Source as TDSCompoundField).Fields[i].Duplicate() );
    end;
  end;
end;

constructor TDSCompoundField.Create;
begin
  inherited;
  Fields := TObjectList<TDSField>.Create(True);
end;

destructor TDSCompoundField.Destroy;
begin
  Fields.Free;
  inherited;
end;

{ TDSArray }

constructor TDSArray.Create;
begin
  inherited;
  Kind := fkArray;
end;

{ TDSStruct }

constructor TDSStruct.Create;
begin
  inherited;
  Kind := fkStruct;
end;

{ TDSField }

procedure TDSField.Assign(Source: TDSField);
begin
  Kind := Source.Kind;
  Name := Source.Name;
end;

{ TDSField }

constructor TDSField.Create;
begin
  inherited;

end;

function TDSField.Duplicate(): TDSField;
begin
  Result := TDSFieldClass(Self.ClassType).Create();
  Result.Assign(Self);
end;

procedure TStructFrame.Button1Click(Sender: TObject);
var
  AData: TBytes;
  Addr: TFilePointer;
begin
  with MainForm.ActiveEditor do
    AData := GetSelectedOrAfterCaret(100*KByte, Addr, True);
  Analyze(MainForm.ActiveEditor.SelStart, AData, Memo1.Text);
end;

constructor TStructFrame.Create(AOwner: TComponent);
begin
  inherited;
  FParser := TDSParser.Create();
  FInterpretor := TDSInterpretor.Create();
end;

destructor TStructFrame.Destroy;
begin
  FParser.Free;
  FInterpretor.Free;
  inherited;
end;

procedure TStructFrame.ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
// Recursively show given DataStructure inside tree node
var
  Node: TTreeNode;
  S: string;
  Intr: TValueInterpretor;
  i: Integer;
begin
  // Field name
  S := DS.Name;

  // For simple field: show value
  if DS is TDSSimpleField then
  with TDSSimpleField(DS) do
  begin
    Intr := MainForm.ValueFrame.FindInterpretor(DataType);
    if Intr = nil then
      S := S + ': ' + Data2Hex(Data)
    else
      S := S + ': ' + Intr.ToString(Data[0], Length(Data));
  end;

  Node := TreeView1.Items.AddChild(ParentNode, S);

  // For compound field: recursively show fields
  if DS is TDSCompoundField then
  with TDSCompoundField(DS) do
  begin
    for i:=0 to Fields.Count-1 do
    begin
      ShowStructTree(Fields[i], Node);
    end;
  end;
end;

{ TDSInterpretor }

function TDSInterpretor.GetFieldSize(const AType: string): Integer;
var
  Intr: TValueInterpretor;
begin
  Intr := MainForm.ValueFrame.FindInterpretor(AType);
  if Intr = nil then
    raise EParserError.Create('Unknown type name: '+AType);
  if Intr.MinSize <> Intr.MaxSize then
    raise EParserError.Create('Type of unfixed size: '+AType);
  Result := Intr.MinSize;
end;

procedure TDSInterpretor.Interpret(DS: TDSField; var Buf: Pointer;
  BufEnd: Pointer);
begin
  if DS is TDSCompoundField then
    InterpretCompound(TDSCompoundField(DS), Buf, BufEnd)
  else
  if DS is TDSSimpleField then
    InterpretSimple(TDSSimpleField(DS), Buf, BufEnd)
  else
    raise EParserError.Create('Invalid class of field '+DS.Name);
end;

procedure TDSInterpretor.InterpretCompound(DS: TDSCompoundField;
  var Buf: Pointer; BufEnd: Pointer);
var
  i: Integer;
begin
  for i:=0 to DS.Fields.Count-1 do
    Interpret(DS.Fields[i], Buf, BufEnd);
end;

procedure TDSInterpretor.InterpretSimple(DS: TDSSimpleField; var Buf: Pointer;
  BufEnd: Pointer);
var
  Size: Integer;
begin
  Size := GetFieldSize(DS.DataType);
  SetLength(DS.Data, Size);
  GetFromBuf(Buf, DS.Data[0], Size, BufEnd);
end;

end.
