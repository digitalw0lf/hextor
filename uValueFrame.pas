unit uValueFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KControls, KGrids, Math,
  Generics.Collections, Vcl.StdCtrls, Clipbrd, System.Types,

  uDWHexTypes, uUtil, uEditorForm, Vcl.Menus;

const
  SUndefinedValue = 'N/A';

  MAX_STR_VALUE_LENGTH = 256;
  INFINITE_SIZE = -1;
  SAME_AS_MIN_SIZE = -2;

type
  TDataToStrFunc = reference to function(const Data; Size: Integer): string;
  TStrToDataFunc = reference to procedure(const S: string; var Data; Size: Integer);  // Raises EConvertError if failed

  TValueInterpretor = class
    Name: string;
    MinSize, MaxSize: Integer;
    ToString: TDataToStrFunc;
    FromString: TStrToDataFunc;
  end;

  TValueFrame = class(TFrame)
    ValuesGrid: TKGrid;
    ValuePopupMenu: TPopupMenu;
    MICopyValue: TMenuItem;
    procedure ValuesGridEditorDataToGrid(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean);
    procedure ValuesGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ValuesGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MICopyValueClick(Sender: TObject);
    procedure ValuesGridEditorSelect(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean);
  private
    type
      TValueGridRow = class (TKGridRow)
      public
        OrigDataSize: Integer;
        Hint: string;
      end;
  private
    { Private declarations }
    FInterpretors: TObjectList<TValueInterpretor>;
    FEditor: TEditorForm;
    FShownRange: TFileRange;
    property Interpretors: TObjectList<TValueInterpretor> read FInterpretors;
    procedure RegisterBuiltinInterpretors();
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure UpdateInfo();
    procedure RegisterInterpretor(const AName: string; AToString: TDataToStrFunc; AFromString: TStrToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE);
    function FindInterpretor(const AName: string): TValueInterpretor;
  end;

implementation

{$R *.dfm}

uses uMainForm;

// intX

function Int2Str(const Data; Size: Integer): string;
var
  x: Int64;
begin
  if (PByteArray(@Data)^[Size-1] and $80)<>0 then
    x := -1  // Expand sign bit
  else
    x := 0;
  Move(Data, x, Size);
  Result := IntToStr(x);
end;

procedure Str2Int(const S: string; var Data; Size: Integer);
var
  x: Int64;
begin
  x := StrToInt64(S);
  Move(x, Data, Size);
end;

// uintX

function UInt2Str(const Data; Size: Integer): string;
var
  x: UInt64;
begin
  x := 0;
  Move(Data, x, Size);
  Result := UIntToStr(x);
end;

procedure Str2UInt(const S: string; var Data; Size: Integer);
var
  x: UInt64;
begin
  x := StrToUInt64(S);
  Move(x, Data, Size);
end;

// float

function Float2Str(const Data; Size: Integer): string;
begin
  Result := R2S(Single(Data));
end;

procedure Str2Float(const S: string; var Data; Size: Integer);
begin
  Single(Data) := S2R(S);
end;

// double

function Double2Str(const Data; Size: Integer): string;
begin
  Result := R2S(Double(Data));
end;

procedure Str2Double(const S: string; var Data; Size: Integer);
begin
  Double(Data) := S2R(S);
end;

// Ansi

function Ansi2Str(const Data; Size: Integer): string;
begin
  Result := string(MakeStr(Data, Size));
end;

procedure Str2Ansi(const S: string; var Data; Size: Integer);
var
  tmp: AnsiString;
begin
  if Length(S) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  tmp := AnsiString(S);
  Move(tmp[Low(tmp)], Data, Size);
end;

// Unicode

function Unicode2Str(const Data; Size: Integer): string;
begin
  if (Size mod SizeOf(Char))<>0 then
    raise EConvertError.Create('Data size must be multiple of 2');
  SetString(Result, PChar(@Data), Size div SizeOf(Char));
end;

procedure Str2Unicode(const S: string; var Data; Size: Integer);
begin
  if Length(S)*SizeOf(Char) <> Size then
    raise EInvalidUserInput.Create('Cannot change string length, only content');
  Move(S[Low(S)], Data, Size);
end;

{ TValueFrame }

constructor TValueFrame.Create(AOwner: TComponent);
begin
  inherited;
  ValuesGrid.RowClass := TValueGridRow;
  ValuesGrid.RealizeRowClass;
  FInterpretors := TObjectList<TValueInterpretor>.Create(True);
  RegisterBuiltinInterpretors();
end;

destructor TValueFrame.Destroy;
begin
  FInterpretors.Free;
  inherited;
end;

function TValueFrame.FindInterpretor(const AName: string): TValueInterpretor;
var
  i: Integer;
begin
  for i:=0 to FInterpretors.Count-1 do
    if FInterpretors[i].Name = AName then Exit(FInterpretors[i]);
  Result := nil;
end;

procedure TValueFrame.MICopyValueClick(Sender: TObject);
begin
  Clipboard.AsText := ValuesGrid.Cells[ValuesGrid.Col, ValuesGrid.Row];
end;

procedure TValueFrame.RegisterBuiltinInterpretors;
begin
  RegisterInterpretor('int8', Int2Str, Str2Int, 1);
  RegisterInterpretor('uint8', UInt2Str, Str2UInt, 1);
  RegisterInterpretor('int16', Int2Str, Str2Int, 2);
  RegisterInterpretor('uint16', UInt2Str, Str2UInt, 2);
  RegisterInterpretor('int32', Int2Str, Str2Int, 4);
  RegisterInterpretor('uint32', UInt2Str, Str2UInt, 4);
  RegisterInterpretor('int64', Int2Str, Str2Int, 8);
  RegisterInterpretor('uint64', UInt2Str, Str2UInt, 8);

  RegisterInterpretor('float', Float2Str, Str2Float, 4);
  RegisterInterpretor('double', Double2Str, Str2Double, 8);

  RegisterInterpretor('ansi', Ansi2Str, Str2Ansi, 1, MAX_STR_VALUE_LENGTH);
  RegisterInterpretor('unicode', Unicode2Str, Str2Unicode, 2, MAX_STR_VALUE_LENGTH);
end;

procedure TValueFrame.RegisterInterpretor(const AName: string; AToString: TDataToStrFunc;
  AFromString: TStrToDataFunc; AMinSize: Integer; AMaxSize: Integer = SAME_AS_MIN_SIZE);
var
  Intr:  TValueInterpretor;
begin
  Intr := TValueInterpretor.Create();
  Intr.Name := AName;
  Intr.MinSize := AMinSize;
  if AMaxSize = SAME_AS_MIN_SIZE then
    Intr.MaxSize := AMinSize
  else
    Intr.MaxSize := AMaxSize;
  Intr.ToString := AToString;
  Intr.FromString := AFromString;
  Interpretors.Add(Intr);
end;

procedure TValueFrame.UpdateInfo;
// Show selection/data under cursor as values
var
  Data: TBytes;
  Greedy: Boolean;
  i, Size: Integer;
  S: string;
begin
  ValuesGrid.EditorMode := False;
  try
    FEditor := MainForm.ActiveEditor;
  except
    on E: ENoActiveEditor do
    begin
      SetKGridRowCount(ValuesGrid, 1);
      Exit;
    end;
  end;

  with FEditor do
  begin
//    if SelLength > 0 then
//    begin
//      FShownRange.Start := SelStart;
//      FShownRange.Size := Min(SelLength, MAX_STR_VALUE_LENGTH);
//      Greedy := True;
//    end
//    else
//    begin
//      FShownRange.Start := CaretPos;
//      FShownRange.Size := MAX_STR_VALUE_LENGTH;
//      Greedy := False;
//    end;
//
//    Data := GetEditedData(FShownRange.Start, FShownRange.Size);

    Data := GetSelectedOrAfterCaret(MAX_STR_VALUE_LENGTH, FShownRange.Start, True);
    Greedy := (SelLength > 0);

    SetKGridRowCount(ValuesGrid, Interpretors.Count + 1);
    for i:=0 to Interpretors.Count-1 do
    begin
      if Length(Data) < Interpretors[i].MinSize then
        S := SUndefinedValue
      else
      try
        if Greedy then Size := Min(Interpretors[i].MaxSize, Length(Data))
                  else Size := Interpretors[i].MinSize;
        (ValuesGrid.Rows[i+1] as TValueGridRow).OrigDataSize := Size;

        S := Interpretors[i].ToString(Data[0], Size);  // <--
      except
        S := SUndefinedValue;
      end;
      ValuesGrid.Cells[0, i+1] := Interpretors[i].Name;
      ValuesGrid.Cells[1, i+1] := S;
    end;
  end;
end;

procedure TValueFrame.ValuesGridEditorDataToGrid(Sender: TObject;
  AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean);
// Convert text to data and change bytes in editor
var
  n: Integer;
  s: string;
  Data: TBytes;
begin
  AssignText := False;
  if not (AEditor is TEdit) then Exit;
  n := ARow - 1;
  if n >= Interpretors.Count then Exit;
  s := (AEditor as TEdit).Text;
  if s = ValuesGrid.Cells[ACol, ARow] then Exit;  // Text not changed
  // Buffer of same size as original data
  SetLength(Data, (ValuesGrid.Rows[ARow] as TValueGridRow).OrigDataSize);

  try
    Interpretors[n].FromString(s, Data[0], Length(Data));

    if not DataEqual(Data, FEditor.GetEditedData(FShownRange.Start, Length(Data))) then
    begin
      FEditor.ChangeBytes(FShownRange.Start, Data);
    end;
  except
    on E: Exception do
      Application.MessageBox(PChar(E.Message), PChar(E.ClassName), MB_OK or MB_ICONERROR);
  end;
end;

procedure TValueFrame.ValuesGridEditorSelect(Sender: TObject;
  AEditor: TWinControl; ACol, ARow: Integer; SelectAll, CaretToLeft,
  SelectedByMouse: Boolean);
begin
  (AEditor as TEdit).SelectAll;
end;

procedure TValueFrame.ValuesGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  if (Button=mbRight) and (ValuesGrid.MouseToCell(X, Y, ACol, ARow)) and (ACol = 1) then
    ValuesGrid.FocusCell(ACol, ARow);
end;

procedure TValueFrame.ValuesGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  p: TPoint;
begin
  if (Button=mbRight) and (ValuesGrid.MouseToCell(X, Y, ACol, ARow)) and (ACol = 1) then
  begin
    p := ValuesGrid.ClientToScreen(Point(X, Y));
    ValuePopupMenu.Popup(p.X, p.Y);
  end;
end;

end.
