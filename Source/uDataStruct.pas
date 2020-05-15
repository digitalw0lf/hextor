{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDataStruct;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Generics.Collections,
  System.Math, Winapi.ActiveX, MSScriptControl_TLB, Variants, ComObj,

  uHextorTypes, uValueInterpretors, {uLogFile,} uCallbackList;

const
  // Synthetic "case" label for "default" branch
  DSDefaultCaseVariant: string = ':case_default:';

type
  EDSParserError = class (Exception);
  TDSSimpleDataType = string;  // e.g. 'uint16'
  TDSCompoundField = class;
  TDSComWrapper = class;

  // Base class for all elements
  TDSField = class
  public
    Name: string;
    Parent: TDSCompoundField;
    BufAddr: TFilePointer;  // Address and size in original data buffer
    BufSize: TFilePointer;
    DescrLineNum: Integer;  // Line number in structure description text
    ErrorText: string;      // Parsing error (e.g. "End of buffer" or "Value out of range")
    // When field is modified e.g. in SetFromVariant(), only it's internal
    // Data buffer if changed. Original file data should be updated in
    // OnChanged event
    OnChanged: TCallbackListP2<{DS:}TDSField, {Changer:}TObject>;
    constructor Create(); virtual;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); virtual;
    function Duplicate(): TDSField;
    function ToQuotedString(): string; virtual;
    function FullName(): string;
  private
    procedure DoChanged(Changer: TObject);
  end;
  TDSFieldClass = class of TDSField;
  TArrayOfDSField = array of TDSField;

  // Simple data types - integers, floats
  TDSSimpleField = class (TDSField)
  private
    FInterpretor: TValueInterpretor;
  public
    DataType: TDSSimpleDataType;
    BigEndian: Boolean;
    ValidationStr: string;
    Data: TBytes;
    constructor Create(); override;
    procedure Assign(Source: TDSField); override;
    function ValueAsVariant(): Variant;
    function ToString(): string; override;
    function ToQuotedString(): string; override;
    procedure SetFromVariant(const V: Variant; AChanger: TObject = nil); virtual;
    function GetInterpretor(RaiseException: Boolean = True): TValueInterpretor;
  end;

  // For arrays and structures
  TDSCompoundField = class (TDSField)
  private
    FComWrapper: TDSComWrapper;
    FCurParsedItem: Integer;  // During interpretation process - which item is currently populating
    function GetNamedFieldsCount: Integer;
    function GetNamedFields(Index: Integer): TDSField;
    function GetNamedFieldsInternal(var Index: Integer): TDSField;
  public
    Fields: TObjectList<TDSField>;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); override;
    function ToString(): string; override;
    function GetComWrapper(): TDSComWrapper;
    property NamedFieldsCount: Integer read GetNamedFieldsCount;  // See comment in GetNamedFields()
    property NamedFields[Index: Integer]: TDSField read GetNamedFields;
  end;

  TDSArray = class (TDSCompoundField)
  public
    ACount: string;  // Count as it is written in description. Interpreted during population of structures
    ElementType: TDSField;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); override;
    function ToString(): string; override;
    function ToQuotedString(): string; override;
  end;

  TDSStruct = class (TDSCompoundField)
  public
    constructor Create(); override;
  end;

  TDSConditional = class (TDSCompoundField)
  // Conditional node placeholder like "if", "switch" etc.
  // Evaluates to different branches according to value of ACondition
  public
    ACondition: string;
    Branches: TDictionary<Variant, TArrayOfDSField>;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); override;
  end;

  // DS wrapper for scripts
  TDSComWrapper = class(TInterfacedObject, IDispatch)
  private const
    DISPID_INDEX  = 1000001;  // "index" pseudo-field of arrays
    DISPID_LENGTH = 1000002;  // "length" pseudo-field of arrays
  private
    DSField: TDSField;
  public
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT;
      virtual; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; virtual; stdcall;
  end;

  // Class for parsing structure description to DS's
  TDSParser = class
  private
    BufStart, BufEnd, Ptr: PChar;
    CurLineNum: Integer;
    CurBigEndian: Boolean;  // Currently in big-endian mode
    LastStatementFields: TArrayOfDSField;  // e.g. for #valid
    function CharValidInName(C: Char): Boolean;
    function IsReservedWord(const S: string): Boolean;
    procedure CheckValidName(const S: string);
    function ReadChar(): Char;
    procedure PutBack();
    function ReadLexem(): string;
    function PeekLexem(): string;
    function ReadExpectedLexem(const Expected: array of string): string;
    function ReadExpressionStr(StopAtChars: TSysCharSet = [';']): string;
    function ReadLine(): string;
    function ReadType(): TDSField;
    function ReadStatement(): TArrayOfDSField;
    function ReadStruct(): TDSStruct;
    function ReadIfStatement(): TDSConditional;
    function ReadSwitchStatement(): TDSConditional;
    procedure ReadDirective();
    function MakeArray(AType: TDSField; const ACount: string): TDSArray;
    procedure EraseComments(var Buf: string);
  public
    function ParseStruct(const Descr: string): TDSStruct;
  end;

  TOnDSInterpretorGetData = reference to procedure (Addr, Size: TFilePointer; var Data: TBytes{; var AEndOfData: Boolean});

  // Class for populating DataStructure from binary buffer
  TDSInterpretor = class
  private
    FRootDS: TDSField;         // Root of DS that is parsed now
//    BufStart: Pointer;
    FStartAddr, FMaxSize: TFilePointer;  // Starting address of analysed struct in original file
    FCurAddr: TFilePointer;    // Current
    EndOfData: Boolean;        // End of input data reached
    FieldsProcessed: Integer;  // To show some progress
    ScriptControl: TScriptControl;  // Expression evaluator
//    DSStack: TStack<TDSField>; // Current stack of nested structures
    procedure ReadData(var Data: TBytes; Size: Integer);
    function GetFieldSize(DS: TDSSimpleField): Integer;
    procedure InterpretSimple(DS: TDSSimpleField);
    procedure InterpretStruct(DS: TDSStruct);
    procedure InterpretArray(DS: TDSArray);
    procedure InterpretConditional(DS: TDSConditional);
    procedure InternalInterpret(DS: TDSField);
    class function FindLastValueByName(DS: TDSField; const AName: string): TDSField;
    class function CalculateExpressionSimple(Expr: string; CurField: TDSField): Variant;
    function CalculateExpression(Expr: string; CurField: TDSField): Variant;
    procedure PrepareScriptEnv(CurField: TDSField);
    procedure ValidateField(DS: TDSSimpleField);
  public
    OnGetMoreData: TOnDSInterpretorGetData;
    OnFieldInterpreted: TCallbackListP2<{Sender:}TObject, {DS:}TDSField>;  // Called after each field populated with data from source
    procedure Interpret(DS: TDSField; Addr, MaxSize: TFilePointer);
    constructor Create();
    destructor Destroy(); override;

//    procedure CheckParent(DS: TDSField);
    class procedure DbgDescribe(DS: TDSField; Text: TStrings; Indent: string; AParent: TDSField);
    class function DbgDescr(DS: TDSField): string;
  end;

implementation

function SameName(const Name1, Name2: string): Boolean;
begin
  Result := (Trim(Name1) = Trim(Name2));
end;

function DuplicateDSFieldArray(const AFields: TArrayOfDSField): TArrayOfDSField;
var
  i: Integer;
begin
  SetLength(Result, Length(AFields));
  for i:=0 to Length(AFields)-1 do
    Result[i] := AFields[i].Duplicate();
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
  if S = '' then  raise EDSParserError.Create('Empty name');
  if IsReservedWord(S) then  raise EDSParserError.Create('Invalid name: ' + S);
//  if not IsCharAlpha(S[Low(S)]) then raise EDSParserError.Create('Invalid name: ' + S);
  for i:=Low(S) to High(S) do
  begin
    if (not CharValidInName(S[i])) or
       ((i = Low(S)) and (CharInSet(S[i], ['0'..'9']))) then  raise EDSParserError.Create('Invalid name: ' + S);
  end;
end;

procedure TDSParser.EraseComments(var Buf: string);
// Replace comments with spaces.
// Supports  //...  and  /*...*/
var
  i: Integer;
  InLineComment, InBlockComment: Boolean;
begin
  InLineComment := False;
  InBlockComment := False;
  for i:=Low(Buf) to High(Buf) do
  begin
    if (InLineComment or InBlockComment) then
    begin
      // Find comment end
      if (InLineComment) and (i <= High(Buf) - 2) and (Buf[i+1] = #13) and (Buf[i+2] = #10) then
        InLineComment := False;  // Up to, but not including, line break
      if (InBlockComment) and (i <= High(Buf) - 1) and (Buf[i] = '*') and (Buf[i+1] = '/') then
      begin  // Up to "*/" inclusive
        InBlockComment := False;
        Buf[i+1] := ' ';
      end;
      // Replace comments with spaces
      Buf[i] := ' ';
    end
    else
    begin
      // Find comment start
      if (i <= High(Buf) - 1) and (Buf[i] = '/') and (Buf[i+1] = '/') then
      begin
        InLineComment := True;
        Buf[i] := ' ';
      end;
      if (i <= High(Buf) - 1) and (Buf[i] = '/') and (Buf[i+1] = '*') then
      begin
        InBlockComment := True;
        Buf[i] := ' ';
      end;
    end;
  end;
end;

function TDSParser.IsReservedWord(const S: string): Boolean;
begin
  Result := SameName(S, 'if') or
            SameName(S, 'else') or
            SameName(S, 'switch') or
            SameName(S, 'case') or
            SameName(S, 'default');
end;

function TDSParser.MakeArray(AType: TDSField; const ACount: string): TDSArray;
// Create Array of Count elements of type AType
begin
  Result := TDSArray.Create();
  Result.ElementType := AType.Duplicate();
  Result.ElementType.Parent := Result;
  Result.ACount := ACount;
end;

function TDSParser.ParseStruct(const Descr: string): TDSStruct;
var
  Buffer: string;
begin
  if Descr = '' then
    raise EDSParserError.Create('Empty struct description');

  Buffer := Descr;
  EraseComments(Buffer);

  BufStart := @Buffer[Low(Buffer)];
  BufEnd := @Buffer[High(Buffer)+1];
  Ptr := BufStart;
  CurLineNum := 1;

  try
    Result := ReadStruct();
  except
    on E: Exception do
    begin
      E.Message := 'Line #' + IntToStr(CurLineNum) + ':' + sLineBreak + E.Message;
      raise;
    end;
  end;
end;

function TDSParser.PeekLexem: string;
// Returns next lexem without moving current pointer
var
  OldPtr: PChar;
  OldLnNum: Integer;
begin
  OldPtr := Ptr;
  OldLnNum := CurLineNum;
  try
    Result := ReadLexem();
  finally
    Ptr := OldPtr;
    CurLineNum := OldLnNum;
  end;
end;

procedure TDSParser.PutBack;
begin
  if Ptr > BufStart then
  begin
    Dec(Ptr);
    if Ptr^ = #10 then
      Dec(CurLineNum);
  end;
end;

function TDSParser.ReadChar: Char;
begin
  if Ptr = BufEnd then Exit(#0);
  Result := Ptr^;
  Inc(Ptr);
  if Result = #10 then Inc(CurLineNum);
end;

procedure TDSParser.ReadDirective;
// Read parser directive #... until end of line
var
  DirName, Value: string;
  i: Integer;
begin
  DirName := ReadLexem();

  if SameName(DirName, 'bigendian') then
  begin
    CurBigEndian := True;
    ReadLine();
    Exit;
  end;

  if SameName(DirName, 'littleendian') then
  begin
    CurBigEndian := False;
    ReadLine();
    Exit;
  end;

  if SameName(DirName, 'valid') then
  begin
    Value := Trim(ReadLine());
    if Value = '' then
      raise EDSParserError.Create('Validation statement expected');
    if LastStatementFields = nil then
      raise EDSParserError.Create('"#valid" directive should be just after validated fields');
    for i:=0 to Length(LastStatementFields)-1 do
      (LastStatementFields[i] as TDSSimpleField).ValidationStr := Value;
    Exit;
  end;

  raise EDSParserError.Create('Unknown parser directive: ' + DirName);
end;

function TDSParser.ReadExpectedLexem(const Expected: array of string): string;
// Read lexem and check if it is one from expected list
var
  i: Integer;
  s: string;
begin
  Result := ReadLexem();
  for i:=0 to High(Expected) do
    if Expected[i] = Result then Exit;
  if Length(Expected) = 0 then
  begin
  end
  else
  begin
    s := '';
    for i:=0 to Length(Expected)-2 do
    begin
      s := s + '"' + Expected[i] + '"';
      if i < Length(Expected)-2 then
        s := s + ', '
      else
        s := s + ' or ';
    end;
    s := s + '"' + Expected[High(Expected)] + '"';
  end;
  raise EDSParserError.Create(s + ' expected');
end;

function TDSParser.ReadExpressionStr(StopAtChars: TSysCharSet = [';']): string;
// Read expression text until closing bracket found
var
  Level: Integer;
  C: Char;
begin
  Result := '';
  Level := 0;

  repeat
    C := ReadChar();
    case C of
      '[', '(', '{': Inc(Level);
      ']', ')', '}': Dec(Level);
    end;
    if (Level < 0) or (C = #0) then Break;
    if (Level = 0) and (CharInSet(C, StopAtChars)) then Break;

    Result := Result + C;
  until False;

  if C <> #0 then PutBack();
end;

function TDSParser.ReadIfStatement: TDSConditional;
// if (Value) Statement;
var
  S: string;
  AFields, AElseFields: TArrayOfDSField;
  i: Integer;
begin
  Result := TDSConditional.Create();
  Result.DescrLineNum := CurLineNum;

  // Read Condition
  ReadExpectedLexem(['(']);
  S := ReadExpressionStr();
  ReadExpectedLexem([')']);
  Result.ACondition := S;

  // Read If Statement
  AFields := ReadStatement();
  for i:=0 to Length(AFields)-1 do
    AFields[i].Parent := Result;

  // Read Else Statement
  if SameName(PeekLexem(), 'else') then
  begin
    ReadLexem();
    AElseFields := ReadStatement();
    for i:=0 to Length(AElseFields)-1 do
      AElseFields[i].Parent := Result;
  end;

  // "if" branch
  Result.Branches.AddOrSetValue(True, AFields);
  // "else" branch
  if AElseFields <> nil then
    Result.Branches.AddOrSetValue(DSDefaultCaseVariant, AElseFields);
end;

function TDSParser.ReadLexem: string;
// Lexem: identifier, number or operator/punctuation
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

function TDSParser.ReadLine: string;
// Read until line break
var
  C: Char;
begin
  Result := '';
  repeat
    C := ReadChar();
    if C = #0 then Exit;
    if C = #13 then
    begin
      C := ReadChar();
      if C <> #10 then PutBack();
      Exit;
    end;
    Result := Result + C;
  until False;
end;

function TDSParser.ReadStatement: TArrayOfDSField;
// Statement is Type + list of names until ;
// It also may be conditional block like "if"
var
  AType, AInstance: TDSField;
  AName, S, ACount: string;
begin
  Result := nil;

  S := PeekLexem();

  if S = ';' then
  // Empty statement
  begin
    ReadLexem();
    Exit;
  end;

  while S = '#' do
  // Parser directive
  begin
    ReadLexem();
    ReadDirective();
    S := PeekLexem();
  end;

  if SameName(S, 'if') then
  // if (Value) Statement
  begin
    ReadLexem();
    Result := [ReadIfStatement()];
    Exit;
  end;

  if SameName(S, 'switch') then
  // switch (Expr) { case Value1: Statement1; ... }
  begin
    ReadLexem();
    Result := [ReadSwitchStatement()];
    Exit;
  end;

  if S = '}' then
  // End of strict
  begin
    Exit;
  end;

  // Read type description
  AType := ReadType();

  if AType = nil then Exit;

  try
    AType.DescrLineNum := CurLineNum;
    // Read field names
    repeat
      S := PeekLexem();
      if (S = ';') or (S = '') or (S = '}') or (SameName(S, 'else')) then
        AName := ''
      else
      begin
        AName := ReadLexem();
        CheckValidName(AName);
      end;

      S := PeekLexem();
      if S = '[' then  // It is array
      begin
        ReadLexem();
        // Read array size
        ACount := ReadExpressionStr();
        ReadExpectedLexem([']']);
        // Create array of Count elements of given type
        AInstance := MakeArray(AType, ACount);
        AInstance.DescrLineNum := CurLineNum;

        S := PeekLexem();  // "," or ";"
      end
      else
      begin
        AInstance := AType.Duplicate();
      end;

      AInstance.Name := AName;
      //AInstance.Parent := Result;
      //Result.Fields.Add(AInstance);
      Result := Result + [AInstance];

//      WriteLogF('Struct', AnsiString(AInstance.ClassName+' '+AInstance.Name));

      if S = ',' then  // Next name
      begin
        ReadLexem();
        Continue;
      end;
      if S = ';' then  // End of statement
      begin
        ReadLexem();
        Break;
      end;
      if SameName(S, 'else') then  // End of statement
      begin
        Break;
      end;
      if (S = '') or (S = '}') then Exit;       // End of description

      raise EDSParserError.Create('";" or "," expected');
    until False;
  finally
    AType.Free;
    LastStatementFields := Result;
  end;
end;

function TDSParser.ReadStruct: TDSStruct;
// Read structure fields
var
  i: Integer;
  Fields: TArrayOfDSField;
begin
  Result := TDSStruct.Create();
  try
    while (Ptr < BufEnd) do
    begin
      if PeekLexem() = '}' then Break;
      Fields := ReadStatement();
  //    if Fields = nil then Break;
      for i:=0 to Length(Fields)-1 do
      begin
        Fields[i].Parent := Result;
        Result.Fields.Add(Fields[i]);
      end;
    end;
    LastStatementFields := nil;  // "#valid" works only inside same struct
  except
    Result.Free;
    raise;
  end;
end;

function TDSParser.ReadSwitchStatement: TDSConditional;
//switch (expression) {
//  case cond1: statement1;
//  case cond2: statement2;
//  default: statementD;
//}
var
  S, ACond, ACase: string;
  AFields: TArrayOfDSField;
  i: Integer;
begin
  // Read Condition
  ReadExpectedLexem(['(']);
  ACond := ReadExpressionStr();
  ReadExpectedLexem([')']);

  ReadExpectedLexem(['{']);

  Result := TDSConditional.Create();
  Result.DescrLineNum := CurLineNum;
  Result.ACondition := ACond;
  try

    while True do
    begin
      S := ReadLexem();
      if SameName(S, 'case') then
      begin
        ACase := ReadExpressionStr([';', ':']);
        ReadExpectedLexem([':']);
        // Read Statement
        AFields := ReadStatement();
        for i:=0 to Length(AFields)-1 do
          AFields[i].Parent := Result;
        Result.Branches.AddOrSetValue(TDSInterpretor.CalculateExpressionSimple(ACase, nil), AFields);  // TODO: not only ints
        Continue;
      end
      else
      if SameName(S, 'default') then
      begin
        ReadExpectedLexem([':']);
        // Read Statement
        AFields := ReadStatement();
        for i:=0 to Length(AFields)-1 do
          AFields[i].Parent := Result;
        Result.Branches.AddOrSetValue(DSDefaultCaseVariant, AFields);
        Continue;
      end
      else
      if S = '}' then Break;

      raise EDSParserError.Create('"case", "default" or "}" expected');
    end;

//    ReadExpectedLexem(['}']);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TDSParser.ReadType: TDSField;
var
  S: string;
begin
  Result := nil;
  S := ReadLexem;
  if (S = '') or (S = ';') then Exit;
  if S = '{' then
  begin
    Result := ReadStruct();
    ReadExpectedLexem(['}']);
  end
  else
  begin
    CheckValidName(S);
    Result := TDSSimpleField.Create();
    TDSSimpleField(Result).DataType := S;
    TDSSimpleField(Result).BigEndian := CurBigEndian;
  end;
end;

{ TDSSimpleField }

procedure TDSSimpleField.Assign(Source: TDSField);
begin
  inherited;
  if Source is TDSSimpleField then
  begin
    DataType := (Source as TDSSimpleField).DataType;
    BigEndian := (Source as TDSSimpleField).BigEndian;
    ValidationStr := (Source as TDSSimpleField).ValidationStr;
    Data := Copy((Source as TDSSimpleField).Data);
    ErrorText := (Source as TDSSimpleField).ErrorText;
  end;
end;

constructor TDSSimpleField.Create;
begin
  inherited;
end;

function TDSSimpleField.GetInterpretor(
  RaiseException: Boolean): TValueInterpretor;
begin
  if FInterpretor = nil then
    FInterpretor := ValueInterpretors.FindInterpretor(DataType);
  Result := FInterpretor;
  if (Result = nil) and (RaiseException) then
    raise EDSParserError.Create('Unknown type name: ' + DataType);
end;

procedure TDSSimpleField.SetFromVariant(const V: Variant; AChanger: TObject = nil);
var
  Intr: TValueInterpretor;
begin
  Intr := GetInterpretor();
  Intr.FromVariant(V, Data[0], Length(Data));
  DoChanged(AChanger);
end;

function TDSSimpleField.ToQuotedString: string;
begin
  Result := ToString();
  if (DataType = 'ansi') or (DataType = 'unicode') then
    Result := '''' + Result + '''';
end;

function TDSSimpleField.ToString(): string;
var
  Intr: TValueInterpretor;
begin
  if Data = nil then Exit('');
  Intr := GetInterpretor(False);
  if Intr = nil then
    Result := string(Data2Hex(Data))
  else
    Result := Intr.ToVariant(Data[0], Length(Data));
end;

function TDSSimpleField.ValueAsVariant: Variant;
begin
  Result := GetInterpretor(True).ToVariant(Data[0], Length(Data));
end;

{ TDSCompoundField }

procedure TDSCompoundField.Assign(Source: TDSField);
var
  i, n: Integer;
begin
  inherited;
  if Source is TDSCompoundField then
  begin
    Fields.Clear();
    for i:=0 to (Source as TDSCompoundField).Fields.Count-1 do
    begin
      n := Fields.Add( (Source as TDSCompoundField).Fields[i].Duplicate() );
      Fields[n].Parent := Self;
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
  if Assigned(FComWrapper) then
    FComWrapper._Release();
  Fields.Free;
  inherited;
end;

function TDSCompoundField.GetComWrapper: TDSComWrapper;
begin
  if FComWrapper = nil then
  begin
    FComWrapper := TDSComWrapper.Create();
    FComWrapper.DSField := Self;
    FComWrapper._AddRef();
  end;
  Result := FComWrapper;
end;

function TDSCompoundField.GetNamedFields(Index: Integer): TDSField;
// In NamedFields, content of unnamed compound fields (e.g. conditional) is "flattened" into single list.
// So, in scripts we can access fields inside unnamed conditional branch as if it was directly
// inside parent structure
begin
  Result := GetNamedFieldsInternal(Index);
end;

function TDSCompoundField.GetNamedFieldsCount: Integer;
var
  i: Integer;
begin
  // TODO: Optimize
  Result := 0;
  for i:=0 to Fields.Count-1 do
    if (Fields[i].Name = '') and (Fields[i] is TDSCompoundField) then
      Inc(Result, (Fields[i] as TDSCompoundField).NamedFieldsCount)
    else
      Inc(Result);
end;

function TDSCompoundField.GetNamedFieldsInternal(var Index: Integer): TDSField;
var
  i: Integer;
begin
  // TODO: Optimize
  for i:=0 to Fields.Count-1 do
    if (Fields[i].Name = '') and (Fields[i] is TDSCompoundField) then
    begin
      Result := (Fields[i] as TDSCompoundField).GetNamedFieldsInternal(Index);
      if Result <> nil then Exit;
    end
    else
    begin
      Dec(Index);
      if Index < 0 then Exit(Fields[i]);
    end;
  Result := nil;
end;

function TDSCompoundField.ToString: string;
const
  MaxDispLen = 100;
var
  i: Integer;
begin
  Result := '(';
  for i:=0 to Fields.Count-1 do
  begin
    Result := Result + Fields[i].ToQuotedString;
    if i < Fields.Count-1 then
    begin
      if Length(Result) > MaxDispLen then
      begin
        Result := Result + '...';
        Break;
      end
      else
        Result := Result + ', ';
    end;
  end;
  Result := Result + ')';
end;

{ TDSArray }

procedure TDSArray.Assign(Source: TDSField);
begin
  inherited;
  if Source is TDSArray then
  begin
    ElementType := (Source as TDSArray).ElementType.Duplicate();
    ElementType.Parent := Self;
    ACount := (Source as TDSArray).ACount;
  end;

end;

constructor TDSArray.Create;
begin
  inherited;

end;

destructor TDSArray.Destroy;
begin
  ElementType.Free;
  inherited;
end;

function TDSArray.ToQuotedString: string;
begin
  Result := ToString();
  if (ElementType is TDSSimpleField) and (
      ((ElementType as TDSSimpleField).DataType = 'ansi') or
      ((ElementType as TDSSimpleField).DataType = 'unicode')) then
  begin
    Result := '"' + Result + '"';
  end;
end;

function TDSArray.ToString: string;
const
  MaxDispLen = 100;
var
  i: Integer;
begin
  if (ElementType is TDSSimpleField) and (
      ((ElementType as TDSSimpleField).DataType = 'ansi') or
      ((ElementType as TDSSimpleField).DataType = 'unicode')) then
  begin
    Result := '';
    for i:=0 to Min(Fields.Count, MaxDispLen)-1 do
      Result := Result + Fields[i].ToString();
    if Fields.Count > MaxDispLen then
      Result := Result + '...';
  end
  else
    Result := inherited;
end;

{ TDSStruct }

constructor TDSStruct.Create;
begin
  inherited;

end;

{ TDSField }

procedure TDSField.Assign(Source: TDSField);
begin
  Name := Source.Name;
  Parent := Source.Parent;
  DescrLineNum := Source.DescrLineNum;
end;

constructor TDSField.Create;
begin
  inherited;

end;

destructor TDSField.Destroy;
begin

  inherited;
end;

procedure TDSField.DoChanged(Changer: TObject);
begin
  OnChanged.Call(Self, Changer);
  if Parent <> nil then
    Parent.DoChanged(Changer);
end;

function TDSField.Duplicate(): TDSField;
begin
  Result := TDSFieldClass(Self.ClassType).Create();
  Result.Assign(Self);
end;

function TDSField.FullName: string;
var
  ParentFullName: string;
begin
  Result := Name;
  if Parent <> nil then
    ParentFullName := Parent.FullName()
  else
    ParentFullName := '';
  if ParentFullName <> '' then
    Result := ParentFullName + '.' + Result;
end;

function TDSField.ToQuotedString: string;
begin
  Result := ToString();
end;

{ TDSConditional }

procedure TDSConditional.Assign(Source: TDSField);
var
  APair: TPair<Variant, TArrayOfDSField>;
  AFields: TArrayOfDSField;
  i: Integer;
begin
  inherited;
  if Source is TDSConditional then
  begin
    ACondition := (Source as TDSConditional).ACondition;
    Branches.Clear();
    // Clone all conditional branches
    for APair in (Source as TDSConditional).Branches do
    begin
      AFields := DuplicateDSFieldArray(APair.Value);
      for i:=0 to Length(AFields)-1 do
        AFields[i].Parent := Self;
      Branches.AddOrSetValue(APair.Key, AFields);
    end;
  end;
end;

constructor TDSConditional.Create;
begin
  inherited;
  Branches := TDictionary<Variant, TArrayOfDSField>.Create();
end;

destructor TDSConditional.Destroy;
var
  AFields: TArrayOfDSField;
  i: Integer;
begin
  for AFields in Branches.Values do
    for i:=0 to Length(AFields)-1 do
      AFields[i].Free;
  Branches.Free;
  inherited;
end;

{ TDSInterpretor }

function TDSInterpretor.CalculateExpression(Expr: string;
  CurField: TDSField): Variant;
//var
//  DS: TDSField;
//  Operands: TArray<string>;
//  Value1, Value2: Variant;
begin
//  WriteLogF('Struct_Intepret', 'Expression: ' + Expr);
  try
    Expr := Trim(Expr);

    Result := CalculateExpressionSimple(Expr, CurField);
    if not VarIsEmpty(Result) then Exit;

    PrepareScriptEnv(CurField);
    Result := ScriptControl.Eval(Expr);
//    Exit;
//
//    if CurField <> nil then
//    begin
//      // Equality comparison
//      if Pos('=', Expr) > 0 then
//      begin
//        Operands := Expr.Split(['='], ExcludeEmpty);
//        if Length(Operands) <> 2 then
//          raise EDSParserError.Create('Error in expression: "'+Expr+'"');
//        Value1 := CalculateExpression(Operands[0], CurField);
//        Value2 := CalculateExpression(Operands[1], CurField);
//        Exit(Value1 = Value2);
//      end;
//
//      // Field name
//      // TODO: start search from current element, not from last
//      DS := FindLastValueByName(CurField, Expr);
//      if (DS <> nil) and (DS is TDSSimpleField) then
//        with TDSSimpleField(DS) do
//          if (Length(Data) > 0) {and (Length(Data) <= 4)} then
//          begin
//    //        Result := 0;
//    //        Move(Data[0], Result, Length(Data));
//            Result := GetInterpretor().ToVariant(Data[0], Length(Data));
//            Exit;
//          end;
//    end;

    if VarIsEmpty(Result) then
      raise EDSParserError.Create('Cannot calculate expression: "'+Expr+'"');

  finally
//    WriteLogF('Struct_Intepret', 'Result: ' + string(Result));
  end;

end;

class function TDSInterpretor.CalculateExpressionSimple(Expr: string;
  CurField: TDSField): Variant;
// Calculate expression that does not requires ScriptControl
var
  N: Integer;
begin
  VarClear(Result);
  Expr := Trim(Expr);

  // Number
  if TryStrToInt(Expr, N) then Exit(N);

  // Ansi char
  if (Length(Expr) = 3) and (Expr[Low(Expr)] = '''') and
     (Expr[High(Expr)] = '''') then
  begin
    Result := AnsiChar(Expr[Low(Expr)+1]);
    Exit;
  end;
end;

//procedure TDSInterpretor.CheckParent(DS: TDSField);
//var
//  i: Integer;
//begin
//  if DS is TDSCompoundField then
//  begin
//    for i:=0 to (DS as TDSCompoundField).Fields.Count-1 do
//    begin
//      if (DS as TDSCompoundField).Fields[i].Parent <> DS then
//        WriteLogF('Check', DS.Name+' '+(DS as TDSCompoundField).Fields[i].Name+' '+(DS as TDSCompoundField).Fields[i].Parent.Name);
//      CheckParent((DS as TDSCompoundField).Fields[i]);
//    end;
//  end;
//end;

constructor TDSInterpretor.Create;
begin
  inherited;
  ScriptControl := TScriptControl.Create(nil);
  ScriptControl.Language := 'JScript';
//  DSStack := TStack<TDSField>.Create();
end;

class function TDSInterpretor.DbgDescr(DS: TDSField): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create();
  try
    DbgDescribe(DS, sl, '', DS.Parent);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

class procedure TDSInterpretor.DbgDescribe(DS: TDSField; Text: TStrings;
  Indent: string; AParent: TDSField);
var
  i: Integer;
  s: string;
begin
  s := Indent + IntToHex(UIntPtr(DS), SizeOf(UIntPtr)*2) + ') ' + DS.ClassName + ' ' + DS.Name;
  if DS is TDSSimpleField then
    s := s + ' ' + DS.ToString();
  s := s + ' ^' + IntToHex(UIntPtr(DS.Parent), SizeOf(UIntPtr)*2);
  if DS.Parent <> AParent then
    s := s + '!!!';
  Text.Add(s);
  if DS is TDSCompoundField then
  begin
    for i:=0 to (DS as TDSCompoundField).Fields.Count-1 do
      DbgDescribe((DS as TDSCompoundField).Fields[i], Text, Indent + '  ', DS);
  end;
end;

destructor TDSInterpretor.Destroy;
begin
  ScriptControl.Free;
//  DSStack.Free;
  inherited;
end;

class function TDSInterpretor.FindLastValueByName(DS: TDSField;
  const AName: string): TDSField;
var
  i: Integer;
begin
  Result := nil;
  if (DS is TDSSimpleField) and (not TryStrToInt(DS.Name, i)) and (SameName(DS.Name, AName)) then Exit(DS);

  if DS is TDSCompoundField then
  with TDSCompoundField(DS) do
  begin
    for i:=Fields.Count-1 downto 0 do
    begin
      Result := FindLastValueByName(Fields[i], AName);
      if Result <> nil then Exit;
    end;
  end;
end;

function TDSInterpretor.GetFieldSize(DS: TDSSimpleField): Integer;
var
  Intr: TValueInterpretor;
begin
  Intr := DS.GetInterpretor();
  Result := Intr.MinSize;
end;

procedure TDSInterpretor.InternalInterpret(DS: TDSField);
begin
  DS.BufAddr := FCurAddr;

  // Real structure size is unknown, so suppose buffer size
  Progress.Show(FCurAddr - FStartAddr, FMaxSize, IntToStr(FieldsProcessed) + ' fields processed');
  try
    try
      if DS is TDSSimpleField then
        InterpretSimple(TDSSimpleField(DS))
      else
      if DS is TDSStruct then
        InterpretStruct(TDSStruct(DS))
      else
      if DS is TDSArray then
        InterpretArray(TDSArray(DS))
      else
      if DS is TDSConditional then
        InterpretConditional(TDSConditional(DS))
      else
        raise EParserError.Create('Invalid class of field '+DS.Name);
  //    WriteLogF('Struct_Intepret', AnsiString(DS.ClassName+' '+DS.Name+' = '+DS.ToString));
    except
      on E: Exception do
      begin
        if not E.Message.StartsWith('Line #') then
          E.Message := 'Line #' + IntToStr(DS.DescrLineNum) + ', Addr ' + IntToStr(DS.BufAddr) + ':' + sLineBreak + E.Message;
        DS.ErrorText := E.Message;
        raise;
      end;
    end;
  finally
    DS.BufSize := FCurAddr - DS.BufAddr;
    Inc(FieldsProcessed);
  end;

  OnFieldInterpreted.Call(Self, DS);
end;

procedure TDSInterpretor.Interpret(DS: TDSField; Addr, MaxSize: TFilePointer);
begin
//  CheckParent(DS);

  FRootDS := DS;
  FStartAddr := Addr;
  FMaxSize := MaxSize;
  FCurAddr := Addr;
  EndOfData := (MaxSize = 0);
  FieldsProcessed := 0;
//  WriteLogF('Struct_Intepret', 'Start');
  Progress.TaskStart(Self);
  try
    InternalInterpret(DS);
  finally
    Progress.TaskEnd();
  end;
end;

procedure TDSInterpretor.InterpretArray(DS: TDSArray);
var
  Count: Integer;
  i: Integer;
  Element: TDSField;
begin
  // Calculate element count
  if Trim(DS.ACount) = '' then
    Count := -1
  else
    Count := CalculateExpression(DS.ACount, DS);

  i := 0;
  DS.Fields.Clear();
  if Count > 0 then
    DS.Fields.Capacity := Count;
  while True do
  begin
    if Count >= 0 then
    begin
      if (i >= Count) then Break;
    end
    else
    begin
      // If empty count specified "[]", parse this array until end of buffer
      if EndOfData then Break;
    end;
    Element := DS.ElementType.Duplicate();
    Element.Name := IntToStr(i);
//    Element.Parent := DS;
    DS.FCurParsedItem := DS.Fields.Add(Element);
    InternalInterpret(Element);
    Inc(i);
  end;
end;

procedure TDSInterpretor.InterpretConditional(DS: TDSConditional);
// Conditional statement like "if" or "switch"
var
  ExprValue: Variant;
  AFields: TArrayOfDSField;
  AField: TDSField;
  i: Integer;
begin
  // Calculate condition
  ExprValue := CalculateExpression(DS.ACondition, DS);

  AFields := nil;
  // Find fields corresponding to condition value
  if not DS.Branches.TryGetValue(ExprValue, AFields) then
    // If no branch for that value, find "default" branch
    DS.Branches.TryGetValue(DSDefaultCaseVariant, AFields);

  if AFields <> nil then
  begin
    for i:=0 to Length(AFields)-1 do
    begin
      AField := AFields[i].Duplicate();
      DS.FCurParsedItem := DS.Fields.Add(AField);
      InternalInterpret(AField);
    end;
  end;
end;

procedure TDSInterpretor.InterpretStruct(DS: TDSStruct);
var
  i: Integer;
begin
  for i:=0 to DS.Fields.Count-1 do
  begin
    DS.FCurParsedItem := i;
    InternalInterpret(DS.Fields[i]);
  end;
end;

procedure TDSInterpretor.PrepareScriptEnv(CurField: TDSField);
// Populate ScriptControl with already parsed fields to use them in expressions.
// Innermost have higher precedence.
var
  DS: TDSField;
  Ok: Boolean;
  TryCounter: Integer;
begin
  Ok := False;
  TryCounter := 0;
  repeat
    try
      ScriptControl.Reset();
      DS := CurField;
      while DS <> nil do
      begin
        if DS is TDSCompoundField then
          ScriptControl.AddObject({DS.Name}'f' + IntToStr(Random(1000000)), (DS as TDSCompoundField).GetComWrapper(), True);
        DS := DS.Parent;
      end;
      Ok := True;
    except
      on E: EOleException do
      begin
//        WriteLogF('Struct_Intepret', AnsiString('TDSInterpretor.PrepareScriptEnv:E: ' + E.Message));
        if TryCounter <= 3 then
          Sleep(50)
        else
          raise;
      end;
    end;
    Inc(TryCounter);
  until Ok;
end;

procedure TDSInterpretor.ReadData(var Data: TBytes; Size: Integer);
begin
  if FCurAddr + Size > FStartAddr + FMaxSize then
    raise EDSParserError.Create('End of buffer');
  OnGetMoreData(FCurAddr, Size, Data);
  FCurAddr := FCurAddr + Size;
  EndOfData := (FCurAddr = FStartAddr + FMaxSize);
end;

procedure TDSInterpretor.ValidateField(DS: TDSSimpleField);
// Check field value against "#valid" directive and set DS.ErrorText

  function StrToValue(const S: string): Variant;
  // 'c' or 123
  begin
    Result := 0;
    if S = '' then Exit(0);
    if (S.StartsWith('''')) and (Length(S) = 3) and (S.EndsWith('''')) then
    begin
      //DS.GetInterpretor().FromVariant(S[Low(S)+1], Result, DS.GetInterpretor().MinSize);
      Result := S[Low(S)+1];
    end
    else
    begin
      Result := StrToInt64(S);
    end;
  end;

var
  V: string;
  i: Integer;
  MinValue, MaxValue, Value: Variant;
begin
  V := DS.ValidationStr;
  if V = '' then Exit;
  // Valid range
  i := V.IndexOf('..');
  if i >= 0 then
  begin
    MinValue := StrToValue(V.Substring(0, i));
    MaxValue := StrToValue(V.Substring(i+2, MaxInt));
  end
  else
  begin
    MinValue := StrToValue(V);
    MaxValue := MinValue;
  end;

  // Field value as integer
  Value := DS.GetInterpretor().ToVariant(DS.Data[0], Length(DS.Data));

  if (Value < MinValue) or (Value > MaxValue) then
    DS.ErrorText := 'Value out of range';
end;

procedure TDSInterpretor.InterpretSimple(DS: TDSSimpleField);
var
  Size: Integer;
begin
  Size := GetFieldSize(DS);
  ReadData(DS.Data, Size);
  if DS.BigEndian then
    InvertByteOrder(DS.Data[0], Size);
  ValidateField(DS);
end;

{ TDSComWrapper }

function TDSComWrapper.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
type
  PNames = ^TNames;
  TNames = array[0..100] of POleStr;
  PDispIDs = ^TDispIDs;
  TDispIDs = array[0..100] of Cardinal;
var
  i: Integer;
  AName: string;
begin
  AName := PNames(Names)^[0];
  if DSField is TDSArray then
  begin
    if SameText('index', AName) then
    // ArrayName.index = current index in array
    begin
      PCardinal(DispIDs)^ := DISPID_INDEX;
      Exit(S_OK);
    end;
    if SameText('length', AName) then
    // ArrayName.length = length of array
    begin
      PCardinal(DispIDs)^ := DISPID_LENGTH;
      Exit(S_OK);
    end;
  end;
  if DSField is TDSCompoundField then
    with (DSField as TDSCompoundField) do
      for i:=0 to NamedFieldsCount-1 do
        if SameText(NamedFields[i].Name, AName) then
        begin
          PCardinal(DispIDs)^ := i;
          Exit(S_OK);
        end;
  Result := DISP_E_UNKNOWNNAME;
end;

function TDSComWrapper.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDSComWrapper.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDSComWrapper.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
var
  Field: TDSField;
  SimpleField: TDSSimpleField;
  LoggedName, LoggedValue: string;
  Put: Boolean;
  V: Variant;
begin
  Result := DISP_E_MEMBERNOTFOUND;
  // Must be a compound field
  if not (DSField is TDSCompoundField) then
    Exit(DISP_E_MEMBERNOTFOUND);
  // No methods here
  if ((Flags and DISPATCH_METHOD) <> 0) then
    Exit(DISP_E_MEMBERNOTFOUND);

  Put := ((Flags and (DISPATCH_PROPERTYPUT or DISPATCH_PROPERTYPUTREF)) <> 0);

  LoggedValue := '';
  try
    // "index" pseudo-field = current index in array
    if DispID = DISPID_INDEX then
    begin
      if not (DSField is TDSArray) then
        Exit(DISP_E_MEMBERNOTFOUND);
      if Put then
        Exit(DISP_E_BADVARTYPE);
      LoggedName := 'index';
      POleVariant(VarResult)^ := (DSField as TDSArray).FCurParsedItem;
      Exit(S_OK);
    end;
    // "length" pseudo-field = length of array
    if DispID = DISPID_LENGTH then
    begin
      if not (DSField is TDSArray) then
        Exit(DISP_E_MEMBERNOTFOUND);
      if Put then
        Exit(DISP_E_BADVARTYPE);
      LoggedName := 'length';
      POleVariant(VarResult)^ := (DSField as TDSArray).Fields.Count;
      Exit(S_OK);
    end;

    // Unknown DispID
    if (DispID < 0) or (DispID >=(DSField as TDSCompoundField).Fields.Count) then
      Exit(DISP_E_MEMBERNOTFOUND);

    Result := S_OK;
    Field := (DSField as TDSCompoundField).NamedFields[DispID];
    LoggedName := Field.FullName();

    if Field is TDSCompoundField then
    // For compound field - return wrapper object
    begin
      if Put then
        Exit(DISP_E_BADVARTYPE);
      POleVariant(VarResult)^ := (Field as TDSCompoundField).GetComWrapper() as IDispatch;
      LoggedValue := Field.ToString();
    end
    else
    if Field is TDSSimpleField then
    // For simple field - return or change value
    begin
      SimpleField := TDSSimpleField(Field);
      if Put then
      begin
        // Assign new value and update source data
        V := Variant(TDispParams(Params).rgvarg[0]);
        SimpleField.SetFromVariant(V);
      end
      else
        // Return current value
        POleVariant(VarResult)^ := (Field as TDSSimpleField).ValueAsVariant();
    end;
  finally
    if (Result = S_OK) and (not Put) then
    begin
      if LoggedValue = '' then
        LoggedValue := string(POleVariant(VarResult)^);
//      WriteLogF('Struct_Intepret', LoggedName + ' = ' + LoggedValue);
    end;
  end;
end;

end.
