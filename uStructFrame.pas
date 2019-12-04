unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Types,

  uUtil, uDWHexTypes, uLogFile, ColoredPanel, uEditorForm, uValueInterpretors;

const
  // Synthetic "case" label for "default" branch
  DSDefaultCaseVariant: string = ':case_default:';

type
  EDSParserError = class (Exception);
  TDSSimpleDataType = string;  // e.g. 'uint16'
  TDSCompoundField = class;

  // Base class for all elements
  TDSField = class
  public
//    Kind: TDSFieldKind;
    Name: string;
    Parent: TDSCompoundField;
    BufAddr: TFilePointer;
    BufSize: Integer;
    DescrLineNum: Integer;  // Line number in structure description text
    constructor Create(); virtual;
    procedure Assign(Source: TDSField); virtual;
    function Duplicate(): TDSField;
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
    ErrorText: string;
    constructor Create(); override;
    procedure Assign(Source: TDSField); override;
    function ToString(): string; override;
    procedure SetFromString(const S: string); virtual;
    function GetInterpretor(RaiseException: Boolean = True): TValueInterpretor;
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
  public
    ACount: string;  // Count as it is written in description. Interpreted during population of structures
    ElementType: TDSField;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Assign(Source: TDSField); override;
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
    function ReadExpressionStr(): string;
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

  // Class for populating DataStructure from binary buffer
  TDSInterpretor = class
  private
    FRootDS: TDSField;    // Root of DS that is parsed now
    BufStart: Pointer;
    FAddr: TFilePointer;  // Starting address of analysed struct in original file
    function GetFieldSize(DS: TDSSimpleField): Integer;
    procedure InterpretSimple(DS: TDSSimpleField; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretStruct(DS: TDSStruct; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretArray(DS: TDSArray; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretConditional(DS: TDSConditional; var Buf: Pointer; BufEnd: Pointer);
    procedure InternalInterpret(DS: TDSField; var Buf: Pointer; BufEnd: Pointer);
    function FindLastValueByName(DS: TDSField; const AName: string): TDSField;
    function CalculateExpression(const Expr: string; Env: TDSField): Variant;
    procedure ValidateField(DS: TDSSimpleField);
  public
    procedure Interpret(DS: TDSField; Addr: TFilePointer; var Buf: Pointer; BufEnd: Pointer);
  end;

  TStructFrame = class(TFrame)
    DSDescrMemo: TMemo;
    DSTreeView: TTreeView;
    PnlButtonBar1: TPanel;
    BtnLoadDescr: TSpeedButton;
    BtnSaveDescr: TSpeedButton;
    LblStructName: TLabel;
    PnlButtonBar2: TPanel;
    BtnInterpret: TButton;
    SavedDescrsMenu: TPopupMenu;
    MIDummyDataStruct: TMenuItem;
    SaveDialog1: TSaveDialog;
    EditFieldValue: TEdit;
    procedure BtnInterpretClick(Sender: TObject);
    procedure BtnLoadDescrClick(Sender: TObject);
    procedure MIDummyDataStructClick(Sender: TObject);
    procedure BtnSaveDescrClick(Sender: TObject);
    procedure PnlButtonBar2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PnlButtonBar2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PnlButtonBar2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure DSTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure DSTreeViewEnter(Sender: TObject);
    procedure DSTreeViewExit(Sender: TObject);
    procedure DSTreeViewDblClick(Sender: TObject);
    procedure EditFieldValueExit(Sender: TObject);
    procedure EditFieldValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DSTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure DSTreeViewHint(Sender: TObject; const Node: TTreeNode;
      var Hint: string);
    procedure DSTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FParser: TDSParser;
    FInterpretor: TDSInterpretor;
    MPos: TPoint;
    ShownDS: TDSField;
    FEditor: TEditorForm;
    EditedNode: TTreeNode;
    EditedDS: TDSSimpleField;
    procedure ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
    procedure ExpandToNode(Node: TTreeNode);
    function DSSaveFolder(): string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Analyze(Addr: TFilePointer; const Data: TBytes; const Struct: string);
    function GetDataColors(Editor: TEditorForm; Addr: TFilePointer;
      Size: Integer; Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
  end;

implementation

uses
  uValueFrame, uMainForm;

{$R *.dfm}

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
      E.Message := 'Line #' + IntToStr(CurLineNum) + ':' + #13#10 + E.Message;
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

function TDSParser.ReadExpressionStr: string;
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
    if (Level = 0) and ((C = ';') or (C = ':')) then Break;

    Result := Result + C;
  until False;

  if C <> #0 then PutBack();
end;

function TDSParser.ReadIfStatement: TDSConditional;
// if (Value) Statement;
var
  S: string;
  AFields, AElseFields: TArrayOfDSField;
begin
  // Read Condition
  ReadExpectedLexem(['(']);
  S := ReadExpressionStr();
  ReadExpectedLexem([')']);

  // Read Statement
  AFields := ReadStatement();

  if SameName(PeekLexem(), 'else') then
  begin
    ReadLexem();
    AElseFields := ReadStatement();
  end;

  Result := TDSConditional.Create();
  Result.DescrLineNum := CurLineNum;
  Result.ACondition := S;
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

      WriteLogF('Struct', AnsiString(AInstance.ClassName+' '+AInstance.Name));

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
        Nop;
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

  while (Ptr < BufEnd) do
  begin
    Fields := ReadStatement();
    if Fields = nil then Break;
    for i:=0 to Length(Fields)-1 do
    begin
      Fields[i].Parent := Result;
      Result.Fields.Add(Fields[i]);
    end;
    if PeekLexem() = '}' then Break;
  end;
  LastStatementFields := nil;  // "#valid" works only inside same struct
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
        ACase := ReadExpressionStr();
        ReadExpectedLexem([':']);
        // Read Statement
        AFields := ReadStatement();
        Result.Branches.AddOrSetValue(StrToInt(Trim(ACase)), AFields);  // TODO: not only ints
        Continue;
      end
      else
      if SameName(S, 'default') then
      begin
        ReadExpectedLexem([':']);
        // Read Statement
        AFields := ReadStatement();
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
  if S = '' then Exit;
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

procedure TDSSimpleField.SetFromString(const S: string);
var
  Intr: TValueInterpretor;
begin
//  Intr := ValueInterpretors.FindInterpretor(DataType);
//  if Intr = nil then
//    raise EDSParserError.Create('Cannot convert string to ' + DataType)
//  else
//    Intr.FromString(S, Data[0], Length(Data));
  Intr := GetInterpretor();
  Intr.FromString(S, Data[0], Length(Data));
end;

function TDSSimpleField.ToString: string;
var
  Intr: TValueInterpretor;
begin
  Intr := GetInterpretor(False);
  if Intr = nil then
    Result := string(Data2Hex(Data))
  else
    Result := Intr.ToString(Data[0], Length(Data));
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
  Fields.Free;
  inherited;
end;

{ TDSArray }

procedure TDSArray.Assign(Source: TDSField);
begin
  inherited;
  if Source is TDSArray then
  begin
    ElementType := (Source as TDSArray).ElementType.Duplicate();
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

{ TDSStruct }

constructor TDSStruct.Create;
begin
  inherited;

end;

{ TDSField }

procedure TDSField.Assign(Source: TDSField);
begin
//  Kind := Source.Kind;
  Name := Source.Name;
  Parent := Source.Parent;
  DescrLineNum := Source.DescrLineNum;
end;

constructor TDSField.Create;
begin
  inherited;

end;

function TDSField.Duplicate(): TDSField;
begin
  Result := TDSFieldClass(Self.ClassType).Create();
  Result.Assign(Self);
end;

{ TDSConditional }

procedure TDSConditional.Assign(Source: TDSField);
var
  APair: TPair<Variant, TArrayOfDSField>;
begin
  inherited;
  if Source is TDSConditional then
  begin
    ACondition := (Source as TDSConditional).ACondition;
    Branches.Clear();
    // Clone all conditional branches
    for APair in (Source as TDSConditional).Branches do
    begin
      Branches.AddOrSetValue(APair.Key, DuplicateDSFieldArray(APair.Value));
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

{ TStructFrame }

procedure TStructFrame.Analyze(Addr: TFilePointer; const Data: TBytes;
  const Struct: string);
var
  Buf, BufEnd: Pointer;
  i: Integer;
  DS: TDSField;
begin
  DSTreeView.Items.Clear();
  FreeAndNil(ShownDS);

  // Parse structure description
  ShownDS := FParser.ParseStruct(DSDescrMemo.Text);

  GetStartEndPointers(Data, Buf, BufEnd);
  // Populate structure fields
  FInterpretor.Interpret(ShownDS, Addr, Buf, BufEnd);

  // Show tree
  // TODO: replace with faster tree view (VirtualTreeView?)
  DSTreeView.Items.BeginUpdate();
  try
    with TDSCompoundField(ShownDS) do
      for i:=0 to Fields.Count-1 do
        ShowStructTree(Fields[i], nil);

    for i:=0 to DSTreeView.Items.Count-1 do
    begin
      // Expand top-level nodes
      if (DSTreeView.Items[i].Parent = nil) and (DSTreeView.Items[i].Count < 30) then
        DSTreeView.Items[i].Expand(False);
      // Expand nodes with errors
      DS := DSTreeView.Items[i].Data;
      if (DS <> nil) and (DS is TDSSimpleField) and ((DS as TDSSimpleField).ErrorText <> '') then
        ExpandToNode(DSTreeView.Items[i]);
      MainForm.ShowProgress(Self, i+1, DSTreeView.Items.Count, 'Expand');
    end;
  finally
    DSTreeView.Items.EndUpdate();
    MainForm.OperationDone(Self);
  end;
end;

function TStructFrame.GetDataColors(Editor: TEditorForm; Addr: TFilePointer; Size: Integer;
  Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
var
  Node: TTreeNode;
  DS: TDSField;
begin
  Result := False;
  if Screen.ActiveControl <> DSTreeView then Exit;
  if Editor <> FEditor then Exit;

  Node := DSTreeView.Selected;
  if Node = nil then Exit;
  DS := Node.Data;

  Result := FillRangeInColorArray(BgColors, Addr,
    DS.BufAddr, DS.BufAddr + DS.BufSize, Color_ValueHighlightBg);
end;

procedure TStructFrame.BtnLoadDescrClick(Sender: TObject);
var
  fl: TStringList;
  i: Integer;
  mi: TMenuItem;
begin
  SavedDescrsMenu.Items.Clear();

  fl := EnumFiles(DSSaveFolder(), faAnyFile, '*.ds', []);
  try
    for i:=0 to fl.Count-1 do
    begin
      mi := TMenuItem.Create(Application);
      mi.Caption := ChangeFileExt(fl[i], '');
      mi.OnClick := MIDummyDataStructClick;
      SavedDescrsMenu.Items.Add(mi);
    end;
  finally
    fl.Free;
  end;

  PopupFromControl(SavedDescrsMenu, BtnLoadDescr);
end;

procedure TStructFrame.BtnInterpretClick(Sender: TObject);
const
  MaxSize = 100*KByte;
var
  AData: TBytes;
  Addr: TFilePointer;
begin
  FEditor := MainForm.ActiveEditor;
  with FEditor do
  begin
    if SelStart = GetFileSize() then
    begin
      Addr := 0;
      AData := GetEditedData(0, MaxSize);
    end
    else
      AData := GetSelectedOrAfterCaret(100*KByte, 100*MByte, Addr, True);
  end;
  Analyze(Addr, AData, DSDescrMemo.Text);
end;

constructor TStructFrame.Create(AOwner: TComponent);
begin
  inherited;
  FParser := TDSParser.Create();
  FInterpretor := TDSInterpretor.Create();
end;

destructor TStructFrame.Destroy;
begin
  ShownDS.Free;
  FParser.Free;
  FInterpretor.Free;
  inherited;
end;

function TStructFrame.DSSaveFolder: string;
begin
  Result := MainForm.SettingsFolder + 'DataStruct\';
end;

procedure TStructFrame.DSTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  DS: TDSField;
begin
  DS := Node.Data;
  if Stage = cdPrePaint then
  begin
    // Red background for fields with invalid values
    if (DS is TDSSimpleField) and
       ((DS as TDSSimpleField).ErrorText <> '') then
      Sender.Canvas.Brush.Color := clRed;
  end;
end;

procedure TStructFrame.DSTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if FEditor = nil then Exit;
  FEditor.BeginUpdatePanes();
  try
    if EditFieldValue.Visible then
      EditFieldValueExit(Sender);
    if (Node.Data <> nil) then
      FEditor.ScrollToShow(TDSField(Node.Data).BufAddr, -1, -1);
    FEditor.UpdatePanes();
  finally
    FEditor.EndUpdatePanes();
  end;
end;

procedure TStructFrame.DSTreeViewDblClick(Sender: TObject);
// Edit value
var
  Node: TTreeNode;
  DS: TDSField;
  R: TRect;
  w: Integer;
begin
  Node := DSTreeView.Selected;
  if Node = nil then Exit;
  DS := Node.Data;
  if (DS = nil) or (not (DS is TDSSimpleField)) then Exit;

  EditedNode := Node;
  EditedDS := TDSSimpleField(DS);
  EditFieldValue.Text := DS.ToString();
  EditFieldValue.Modified := False;

  R := Node.DisplayRect(True);
  w := DSTreeView.Canvas.TextWidth(DS.Name + ': ');
  EditFieldValue.Parent := DSTreeView;
  EditFieldValue.SetBounds(R.Left + w, R.Top - 2, R.Width - w + 30, EditFieldValue.Height);

  EditFieldValue.Show;
  EditFieldValue.SetFocus();
end;

procedure TStructFrame.DSTreeViewEnter(Sender: TObject);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewExit(Sender: TObject);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewHint(Sender: TObject; const Node: TTreeNode;
  var Hint: string);
var
  DS: TDSField;
begin
  DS := Node.Data;
  if (DS <> nil) and (DS is TDSSimpleField) then
     Hint := (DS as TDSSimpleField).ErrorText
  else
    Hint := '';
end;

procedure TStructFrame.DSTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (not (ssDouble in Shift)) and (EditFieldValue.Visible) then
    EditFieldValueExit(Sender);
end;

procedure TStructFrame.EditFieldValueExit(Sender: TObject);
// Apply changed field value
var
  DS: TDSSimpleField;
begin
  if FEditor = nil then Exit;
  if EditFieldValue.Modified then
  begin
    DS := EditedDS;
    if DS <> nil then
    begin
      DS.SetFromString(EditFieldValue.Text);
      FEditor.EditedData.Change(DS.BufAddr, DS.BufSize, @DS.Data[0]);
      EditedNode.Text := DS.Name + ': ' + DS.ToString();
    end;
  end;
  // Hide editor
  EditFieldValue.Hide();
  if EditFieldValue.Focused then
    DSTreeView.SetFocus();
end;

procedure TStructFrame.EditFieldValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditFieldValueExit(Sender);

  if Key = VK_ESCAPE then
  begin
    EditFieldValue.Modified := False;
    EditFieldValueExit(Sender);
  end;
end;

procedure TStructFrame.ExpandToNode(Node: TTreeNode);
begin
  while Node <> nil do
  begin
    Node.Expand(False);
    Node := Node.Parent;
  end;
end;

procedure TStructFrame.FrameResize(Sender: TObject);
begin
  DSDescrMemo.Constraints.MaxHeight := DSDescrMemo.Height + DSTreeView.Height - 20;
end;

procedure TStructFrame.MIDummyDataStructClick(Sender: TObject);
var
  fn: string;
begin
  fn := (Sender as TMenuItem).Caption;
  DSDescrMemo.Lines.LoadFromFile(DSSaveFolder + fn + '.ds');
  LblStructName.Caption := fn;
end;

procedure TStructFrame.PnlButtonBar2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MPos := Point(X, Y);
end;

procedure TStructFrame.PnlButtonBar2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (MPos.X >= 0) then
  begin
    DSDescrMemo.Height := DSDescrMemo.Height + (Y - MPos.Y);
  end;
end;

procedure TStructFrame.PnlButtonBar2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MPos := Point(-1, -1);
end;

procedure TStructFrame.ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
// Recursively show given DataStructure inside tree node
var
  Node: TTreeNode;
  S: string;
  i: Integer;
begin
  MainForm.ShowProgress(Self, DS.BufAddr - ShownDS.BufAddr, ShownDS.BufSize, 'Showing tree');

  // Field name
  S := DS.Name;

  // For simple field: show value
  if DS is TDSSimpleField then
    S := S + ': ' + TDSSimpleField(DS).ToString();

  // For array: show length
  if DS is TDSArray then
  with TDSArray(DS) do
    S := S + '[' + IntToStr(Fields.Count) + ']';

  if (S <> ''){ or (ParentNode = nil)} then
  begin
    Node := DSTreeView.Items.AddChild(ParentNode, S);
    Node.Data := DS;
  end
  else  // Don't create separate nodes for nameless fields - e.g. conditional statements
    Node := ParentNode;

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

procedure TStructFrame.BtnSaveDescrClick(Sender: TObject);
var
  fn: string;
begin
  ForceDirectories(DSSaveFolder());
  fn := LblStructName.Caption;
  SaveDialog1.InitialDir := DSSaveFolder();
  SaveDialog1.FileName := fn + '.ds';

  if not SaveDialog1.Execute() then Exit;
  fn := SaveDialog1.FileName;

  ForceDirectories(ExtractFilePath(fn));
  DSDescrMemo.Lines.SaveToFile(fn);
  LblStructName.Caption := ChangeFileExt(ExtractFileName(fn), '');
end;

{ TDSInterpretor }

function TDSInterpretor.CalculateExpression(const Expr: string;
  Env: TDSField): Variant;
var
  DS: TDSField;
  N: Integer;
  Operands: TArray<string>;
  Value1, Value2: Variant;
begin
  if TryStrToInt(Trim(Expr), N) then Exit(N);

  // Equality comparison
  if Pos('=', Expr) > 0 then
  begin
    Operands := Expr.Split(['='], ExcludeEmpty);
    if Length(Operands) <> 2 then
      raise EDSParserError.Create('Error in expression: "'+Expr+'"');
    Value1 := CalculateExpression(Operands[0], Env);
    Value2 := CalculateExpression(Operands[1], Env);
    Exit(Value1 = Value2);
  end;

  // Field name
  // TODO: start search from current element, not from last
  DS := FindLastValueByName(Env, Trim(Expr));
  if (DS <> nil) and (DS is TDSSimpleField) then
    with TDSSimpleField(DS) do
      if (Length(Data) > 0) and (Length(Data) <= 4) then
      begin
//        Result := 0;
//        Move(Data[0], Result, Length(Data));
        Result := GetInterpretor().ToInt(Data[0], Length(Data));
        Exit;
      end;

  raise EDSParserError.Create('Cannot calculate expression: "'+Expr+'"');
end;

function TDSInterpretor.FindLastValueByName(DS: TDSField;
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

procedure TDSInterpretor.InternalInterpret(DS: TDSField; var Buf: Pointer;
  BufEnd: Pointer);
var
  AStart: Pointer;
begin
  AStart := Buf;
  DS.BufAddr := FAddr + UIntPtr(Buf) - UIntPtr(BufStart);

  // Real structure size is unknown, so suppose buffer size
  MainForm.ShowProgress(Self, UIntPtr(Buf) - UIntPtr(BufStart), UIntPtr(BufEnd) - UIntPtr(BufStart), 'Interpreting data');

  try
    if DS is TDSStruct then
      InterpretStruct(TDSStruct(DS), Buf, BufEnd)
    else
    if DS is TDSArray then
      InterpretArray(TDSArray(DS), Buf, BufEnd)
    else
    if DS is TDSSimpleField then
      InterpretSimple(TDSSimpleField(DS), Buf, BufEnd)
    else
    if DS is TDSConditional then
      InterpretConditional(TDSConditional(DS), Buf, BufEnd)
    else
      raise EParserError.Create('Invalid class of field '+DS.Name);
  except
    on E: Exception do
    begin
      if not E.Message.StartsWith('Line #') then
        E.Message := 'Line #' + IntToStr(DS.DescrLineNum) + ':' + #13#10 + E.Message;
      raise;
    end;
  end;

  DS.BufSize := UIntPtr(Buf) - UIntPtr(AStart)
end;

procedure TDSInterpretor.Interpret(DS: TDSField; Addr: TFilePointer;
  var Buf: Pointer; BufEnd: Pointer);
begin
  FRootDS := DS;
  FAddr := Addr;
  BufStart := Buf;
  try
    InternalInterpret(DS, Buf, BufEnd);
  finally
    MainForm.OperationDone(Self);
  end;
end;

procedure TDSInterpretor.InterpretArray(DS: TDSArray; var Buf: Pointer;
  BufEnd: Pointer);
var
  Count: Integer;
  i: Integer;
  Element: TDSField;
begin
  // Calculate element count
  if DS.ACount = '' then
    Count := -1
  else
    Count := CalculateExpression(DS.ACount, FRootDS);

  i := 0;
  DS.Fields.Clear();
  while True do
  begin
    if Count >= 0 then
    begin
      if (i >= Count) then Break;
    end
    else
    begin
      // If empty count specified "[]", parse this array until end of buffer
      if UIntPtr(Buf) >= UIntPtr(BufEnd) then Break;
    end;
    Element := DS.ElementType.Duplicate();
    Element.Name := IntToStr(i);
    Element.Parent := DS;
    DS.Fields.Add(Element);
    InternalInterpret(Element, Buf, BufEnd);
    Inc(i);
  end;
end;

procedure TDSInterpretor.InterpretConditional(DS: TDSConditional;
  var Buf: Pointer; BufEnd: Pointer);
// Conditional statement like "if" or "switch"
var
  ExprValue: Variant;
  AFields: TArrayOfDSField;
  AField: TDSField;
  i: Integer;
begin
  // Calculate condition
  ExprValue := CalculateExpression(DS.ACondition, FRootDS);

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
      DS.Fields.Add(AField);
      InternalInterpret(AField, Buf, BufEnd);
    end;
  end;
end;

procedure TDSInterpretor.InterpretStruct(DS: TDSStruct;
  var Buf: Pointer; BufEnd: Pointer);
var
  i: Integer;
begin
  for i:=0 to DS.Fields.Count-1 do
    InternalInterpret(DS.Fields[i], Buf, BufEnd);
end;

procedure TDSInterpretor.ValidateField(DS: TDSSimpleField);
// Check field value against "#valid" directive and set DS.ErrorText

  function StrToValue(const S: string): Int64;
  // 'c' or 123
  begin
    Result := 0;
    if S = '' then Exit(0);
    if (S.StartsWith('''')) and (Length(S) = 3) and (S.EndsWith('''')) then
    begin
      DS.GetInterpretor().FromString(S[Low(S)+1], Result, DS.GetInterpretor().MinSize);
    end
    else
    begin
      Result := StrToInt64(S);
    end;
  end;

var
  V: string;
  i: Integer;
  MinValue, MaxValue, Value: Int64;
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
  Value := DS.GetInterpretor().ToInt(DS.Data[0], Length(DS.Data));

  if (Value < MinValue) or (Value > MaxValue) then
    DS.ErrorText := 'Value out of range';
end;

procedure TDSInterpretor.InterpretSimple(DS: TDSSimpleField; var Buf: Pointer;
  BufEnd: Pointer);
var
  Size: Integer;
begin
  Size := GetFieldSize(DS);
  SetLength(DS.Data, Size);
  GetFromBuf(Buf, DS.Data[0], Size, BufEnd);
  if DS.BigEndian then
    InvertByteOrder(DS.Data[0], Size);
  ValidateField(DS);
end;

end.
