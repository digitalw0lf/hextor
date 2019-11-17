unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Types,

  uUtil, uDWHexTypes, uLogFile, ColoredPanel, uEditorForm;

type
  EDSParserError = class (Exception);

//  TDSFieldKind = (fkUnknown, fkSimple, fkArray, fkStruct);
  TDSSimpleDataType = string;  // e.g. 'uint16'

  // Base class for all elements
  TDSField = class
  public
//    Kind: TDSFieldKind;
    Name: string;
    BufAddr: TFilePointer;
    BufSize: Integer;
    DescrLineNum: Integer;  // Line number in structure description text
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
    function ToString(): string; override;
    procedure SetFromString(const S: string); virtual;
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
    ElementType: TDSField;
    ACount: string;  // Count as it is written in description. Interpreted during population of structures
    constructor Create(); override;
    procedure Assign(Source: TDSField); override;
  end;

  TDSStruct = class (TDSCompoundField)
  public
    constructor Create(); override;
  end;

  // Class for parsing structure description to DS's
  TDSParser = class
  private
    BufStart, BufEnd, Ptr: PChar;
    CurLineNum: Integer;
    function CharValidInName(C: Char): Boolean;
    procedure CheckValidName(const S: string);
    function ReadChar(): Char;
    procedure PutBack();
    function ReadLexem(): string;
    function PeekLexem(): string;
    function ReadExpressionStr(): string;
    function ReadType(): TDSField;
    function ReadStruct(): TDSStruct;
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
    function GetFieldSize(const AType: string): Integer;
    procedure InterpretSimple(DS: TDSSimpleField; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretStruct(DS: TDSStruct; var Buf: Pointer; BufEnd: Pointer);
    procedure InterpretArray(DS: TDSArray; var Buf: Pointer; BufEnd: Pointer);
    procedure InternalInterpret(DS: TDSField; var Buf: Pointer; BufEnd: Pointer);
    function FindLastValueByName(DS: TDSField; const AName: string): TDSField;
    function CalculateExpression(const Expr: string; Env: TDSField): Integer;
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
    Button1: TButton;
    SavedDescrsMenu: TPopupMenu;
    MIDummyDataStruct: TMenuItem;
    SaveDialog1: TSaveDialog;
    EditFieldValue: TEdit;
    procedure Button1Click(Sender: TObject);
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

{ TStructFrame }

procedure TStructFrame.Analyze(Addr: TFilePointer; const Data: TBytes;
  const Struct: string);
var
  Buf, BufEnd: Pointer;
  i: Integer;
begin
  ShownDS.Free;
  ShownDS := FParser.ParseStruct(DSDescrMemo.Text);

  GetStartEndPointers(Data, Buf, BufEnd);

  FInterpretor.Interpret(ShownDS, Addr, Buf, BufEnd);

  DSTreeView.Items.Clear();

  with TDSCompoundField(ShownDS) do
    for i:=0 to Fields.Count-1 do
      ShowStructTree(Fields[i], nil);

  DSTreeView.Items[0].Expand(True);
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

function TDSParser.MakeArray(AType: TDSField; const ACount: string): TDSArray;
// Create Array of Count elements of type AType
begin
  Result := TDSArray.Create();
  Result.ElementType := AType.Duplicate();
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
    if (Level = 0) and (C = ';') then Break;

    Result := Result + C;
  until False;

  if C <> #0 then PutBack();
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
//  Count: Integer;
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
      AType.DescrLineNum := CurLineNum;
      // Read field names
      repeat
        AName := ReadLexem();
        CheckValidName(AName);

        S := ReadLexem();
        if S = '[' then  // It is array
        begin
          // Read array size
//          ACount := ReadLexem();
//          Count := StrToInt(ACount);
          ACount := ReadExpressionStr();
          if ReadLexem() <> ']' then
            raise EDSParserError.Create('"]" expected');
          // Create array of Count elements of given type
          AInstance := MakeArray(AType, ACount);
          AInstance.DescrLineNum := CurLineNum;

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
//  Kind := fkSimple;
end;

procedure TDSSimpleField.SetFromString(const S: string);
var
  Intr: TValueInterpretor;
begin
  Intr := MainForm.ValueFrame.FindInterpretor(DataType);
  if Intr = nil then
    raise EDSParserError.Create('Cannot convert string to ' + DataType)
  else
    Intr.FromString(S, Data[0], Length(Data));
end;

function TDSSimpleField.ToString: string;
var
  Intr: TValueInterpretor;
begin
  Intr := MainForm.ValueFrame.FindInterpretor(DataType);
  if Intr = nil then
    Result := string(Data2Hex(Data))
  else
    Result := Intr.ToString(Data[0], Length(Data));
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
//  Kind := fkArray;
end;

{ TDSStruct }

constructor TDSStruct.Create;
begin
  inherited;
//  Kind := fkStruct;
end;

{ TDSField }

procedure TDSField.Assign(Source: TDSField);
begin
//  Kind := Source.Kind;
  Name := Source.Name;
  DescrLineNum := Source.DescrLineNum;
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

procedure TStructFrame.BtnLoadDescrClick(Sender: TObject);
var
  fl: TStringList;
  i: Integer;
  mi: TMenuItem;
begin
  SavedDescrsMenu.Items.Clear();

  fl := EnumFiles(DSSaveFolder(), faAnyFile, '*.ds', []);

  for i:=0 to fl.Count-1 do
  begin
    mi := TMenuItem.Create(Application);
    mi.Caption := ChangeFileExt(fl[i], '');
    mi.OnClick := MIDummyDataStructClick;
    SavedDescrsMenu.Items.Add(mi);
  end;

  PopupFromControl(SavedDescrsMenu, BtnLoadDescr);
end;

procedure TStructFrame.Button1Click(Sender: TObject);
var
  AData: TBytes;
  Addr: TFilePointer;
begin
  FEditor := MainForm.ActiveEditor;
  with FEditor do
    AData := GetSelectedOrAfterCaret(100*KByte, 100*KByte, Addr, True);
  Analyze(MainForm.ActiveEditor.SelStart, AData, DSDescrMemo.Text);
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

function TStructFrame.DSSaveFolder: string;
begin
  Result := MainForm.SettingsFolder + 'DataStruct\';
end;

procedure TStructFrame.DSTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if EditFieldValue.Visible then
    EditFieldValueExit(Sender);
  FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewDblClick(Sender: TObject);
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
  FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewExit(Sender: TObject);
begin
  FEditor.UpdatePanes();
end;

procedure TStructFrame.EditFieldValueExit(Sender: TObject);
// Apply changed field value
var
  DS: TDSSimpleField;
begin
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
  // Field name
  S := DS.Name;

  // For simple field: show value
  if DS is TDSSimpleField then
    S := S + ': ' + TDSSimpleField(DS).ToString();

  // For array: show length
  if DS is TDSArray then
  with TDSArray(DS) do
    S := S + '[' + IntToStr(Fields.Count) + ']';

  Node := DSTreeView.Items.AddChild(ParentNode, S);
  Node.Data := DS;

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
  Env: TDSField): Integer;
var
  DS: TDSField;
begin
  if TryStrToInt(Expr, Result) then Exit;

  DS := FindLastValueByName(Env, Expr);
  if (DS <> nil) and (DS is TDSSimpleField) then
    with TDSSimpleField(DS) do
      if (Length(Data) > 0) and (Length(Data) <= 4) then
      begin
        Result := 0;
        Move(Data[0], Result, Length(Data));
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

function TDSInterpretor.GetFieldSize(const AType: string): Integer;
var
  Intr: TValueInterpretor;
begin
  Intr := MainForm.ValueFrame.FindInterpretor(AType);
  if Intr = nil then
    raise EParserError.Create('Unknown type name: '+AType);
//  if Intr.MinSize <> Intr.MaxSize then
//    raise EParserError.Create('Type of unfixed size: '+AType);
  Result := Intr.MinSize;
end;

procedure TDSInterpretor.InternalInterpret(DS: TDSField; var Buf: Pointer;
  BufEnd: Pointer);
var
  AStart: Pointer;
begin
  AStart := Buf;
  DS.BufAddr := FAddr + UIntPtr(Buf) - UIntPtr(BufStart);

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
  InternalInterpret(DS, Buf, BufEnd);
end;

procedure TDSInterpretor.InterpretArray(DS: TDSArray; var Buf: Pointer;
  BufEnd: Pointer);
var
  Count: Integer;
  i: Integer;
  Element: TDSField;
begin
  Count := CalculateExpression(DS.ACount, FRootDS);

  for i:=0 to Count-1 do
  begin
    Element := DS.ElementType.Duplicate();
    Element.Name := IntToStr(i);
    InternalInterpret(Element, Buf, BufEnd);
    DS.Fields.Add(Element);
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
