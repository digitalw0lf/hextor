unit uValueFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KControls, KGrids, Math,
  Generics.Collections, Vcl.StdCtrls, Clipbrd, System.Types, Vcl.Menus,

  uHextorTypes, uEditorForm, uValueInterpretors;

const
  SUndefinedValue = 'N/A';

type
  TValueFrame = class(TFrame, IHextorToolFrame)
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
    procedure ValuesGridClick(Sender: TObject);
    procedure ValuesGridExit(Sender: TObject);
  private
    type
      TValueGridRow = class (TKGridRow)
      public
        OrigDataSize: Integer;
        Defined: Boolean;
        Hint: string;
      end;
  private
    { Private declarations }
    FEditor: TEditorForm;
    FShownRange: TFileRange;
    procedure EditorSelectionChanged(Sender: TEditorForm);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure UpdateInfo();
    function GetDataColors(Editor: TEditorForm; Addr: TFilePointer; Size: Integer; Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
    procedure OnShown();
  end;

implementation

{$R *.dfm}

uses uMainForm;

{ TValueFrame }

constructor TValueFrame.Create(AOwner: TComponent);
begin
  inherited;
  ValuesGrid.RowClass := TValueGridRow;
  ValuesGrid.RealizeRowClass;
  MainForm.OnSelectionChanged.Add(EditorSelectionChanged);
end;

destructor TValueFrame.Destroy;
begin
  inherited;
end;

procedure TValueFrame.EditorSelectionChanged(Sender: TEditorForm);
begin
  if not Parent.Visible then Exit;
  UpdateInfo();
end;

function TValueFrame.GetDataColors(Editor: TEditorForm; Addr: TFilePointer; Size: Integer;
  Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
var
  VRow: TValueGridRow;
begin
  Result := False;
  if Screen.ActiveControl <> ValuesGrid then Exit;
  if Editor <> FEditor then Exit;

  VRow := ValuesGrid.Rows[ValuesGrid.Row] as TValueGridRow;
  if not VRow.Defined then Exit;

  Result := FillRangeInColorArray(BgColors, Addr,
    FShownRange.Start, FShownRange.Start+VRow.OrigDataSize, Color_ValueHighlightBg);
end;

procedure TValueFrame.MICopyValueClick(Sender: TObject);
begin
  Clipboard.AsText := ValuesGrid.Cells[ValuesGrid.Col, ValuesGrid.Row];
end;

procedure TValueFrame.OnShown;
begin
  UpdateInfo();
end;

procedure TValueFrame.UpdateInfo;
// Show selection/data under cursor as values
var
  AData: TBytes;
  Greedy: Boolean;
  i, Size: Integer;
  S: string;
  VRow: TValueGridRow;
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
    AData := GetSelectedOrAfterCaret(MAX_STR_VALUE_LENGTH, MAX_STR_VALUE_LENGTH, FShownRange.Start, True);
    Greedy := (SelLength > 0);

    SetKGridRowCount(ValuesGrid, ValueInterpretors.Count + 1);
    for i:=0 to ValueInterpretors.Count-1 do
    begin
      VRow := ValuesGrid.Rows[i+1] as TValueGridRow;
      VRow.Defined := False;
      VRow.Hint := '';
      if Length(AData) < ValueInterpretors[i].MinSize then
      begin
        S := SUndefinedValue;
        VRow.Hint := 'Not enough data';
      end
      else
      try
        if (Greedy) {and (ValueInterpretors[i].Greedy)} then
          Size := Min(ValueInterpretors[i].MaxSize, Length(AData))
        else
          Size := ValueInterpretors[i].MinSize;
        VRow.OrigDataSize := Size;

        S := ValueInterpretors[i].ToVariant(AData[0], Size);  // <--

        VRow.Defined := True;
      except
        on E:Exception do
        begin
          S := SUndefinedValue;
          VRow.Hint := E.Message;
        end;
      end;
      ValuesGrid.Cells[0, i+1] := ValueInterpretors[i].Name;
      ValuesGrid.Cells[1, i+1] := S;
    end;
  end;
end;

procedure TValueFrame.ValuesGridClick(Sender: TObject);
begin
  FEditor.UpdatePanes();
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
  if n >= ValueInterpretors.Count then Exit;
  s := (AEditor as TEdit).Text;
  if s = ValuesGrid.Cells[ACol, ARow] then Exit;  // Text not changed
  // Buffer of same size as original data
  SetLength(Data, (ValuesGrid.Rows[ARow] as TValueGridRow).OrigDataSize);

  try
    ValueInterpretors[n].FromVariant(s, Data[0], Length(Data));

    if not DataEqual(Data, FEditor.GetEditedData(FShownRange.Start, Length(Data))) then
    begin
      FEditor.ChangeBytes(FShownRange.Start, Data);
    end;
  except
    // Catch exception here so ValuesGrid can proprtly destroy editor etc.
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

procedure TValueFrame.ValuesGridExit(Sender: TObject);
begin
  FEditor.UpdatePanes();
end;

procedure TValueFrame.ValuesGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  ValuesGrid.SetFocus();
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
