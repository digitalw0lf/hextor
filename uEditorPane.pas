unit uEditorPane;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Graphics, System.Types, Winapi.Messages, Math,

  uLogFile, uUtil;

type
  TEditorPane = class(TPanel)
  private
    { Private declarations }
    FCaretPos: TPoint;
    FShowCaret: Boolean;
    FHorzScrollPos: Integer;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetCaretPos(const Value: TPoint);
    procedure SetShowCaret(const Value: Boolean);
    procedure SetHorzScrollPos(const Value: Integer);
  protected
    { Protected declarations }
    FLines: TStringList;
    ScrBmp: TBitmap;
    SelStart, SelLength: Integer;
    FUpdating: Integer;
    procedure Paint; override;
    procedure InternalPaint();
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WMGetDlgCode(var Msg: TWMNoParams); message WM_GETDLGCODE;
  public
    { Public declarations }
    CharSize: TSize;
    InsertModeCaret: Boolean;
    BgColors, TxColors: array of TColor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CharHeight(): Integer;
    function CharWidth(): Integer;
    function GetCharAt(x, y: Integer; var Pos: TPoint): Boolean;
    procedure SetSelection(AStart, ALength: Integer);
    procedure BeginUpdate();
    procedure EndUpdate();
  published
    { Published declarations }
    property Lines: TStringList read FLines;
    property Text: string read GetText write SetText;
    property CaretPos: TPoint read FCaretPos write SetCaretPos;
    property ShowCaret: Boolean read FShowCaret write SetShowCaret;
    property HorzScrollPos: Integer read FHorzScrollPos write SetHorzScrollPos;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DWF', [TEditorPane]);
end;

{ TEditorPane }

procedure TEditorPane.BeginUpdate;
begin
  Inc(FUpdating);
end;

constructor TEditorPane.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TStringList.Create();
  ScrBmp := TBitmap.Create();
  Color := clWindow;
  BevelOuter := bvNone;
  Font.Name := 'Consolas';//'Fixedsys';//'Courier New';
  CharSize.cx := Font.Height div 2;
  CharSize.cy := Font.Height;
end;

destructor TEditorPane.Destroy;
begin
  FLines.Free;
  ScrBmp.Free;
  inherited;
end;

procedure TEditorPane.DoEnter;
begin
  inherited;
  Paint();
end;

procedure TEditorPane.DoExit;
begin
  inherited;
  Paint();
end;

procedure TEditorPane.EndUpdate;
begin
  if FUpdating=0 then Exit;
  Dec(FUpdating);
  if FUpdating=0 then
    Paint();
end;

function TEditorPane.GetCharAt(x, y: Integer; var Pos: TPoint): Boolean;
begin
  Result := True;
  if (Lines.Count = 0) then
  begin
    Pos.X := 0;
    Pos.Y := 0;
    Exit;
  end;
  Pos.X := x div CharSize.cx + HorzScrollPos;
  Pos.Y := y div CharSize.cy;
//  Result := (Pos.Y>=0) and (Pos.Y<Lines.Count) and (Pos.X>=0) and (Pos.X<Length(Lines[Pos.Y]));
//  // After last char
//  if (Pos.Y=Lines.Count-1) and (Pos.X=Length(Lines[Pos.Y])) then Result := True;
  Pos.Y := BoundValue(Pos.Y, 0, Lines.Count-1);
  Pos.X := BoundValue(Pos.X, 0, Length(Lines[Pos.Y]));
end;

function TEditorPane.GetText: string;
begin
  Result := FLines.Text;
end;

procedure TEditorPane.InternalPaint;
var
  i, j, j1, ColorIndex: Integer;
  CurBgColor, PrevBgColor, CurTxColor, PrevTxColor: TColor;
  FirstVis, LastVis: Integer;  // First and last visible chars of line, 0-based
  R: TRect;
  s, s1: string;
begin
  ScrBmp.Canvas.Brush.Color := Self.Color;
  ScrBmp.Canvas.Font := Self.Font;
  ScrBmp.Canvas.FillRect(ClientRect);
  CharSize := ScrBmp.Canvas.TextExtent('O');
  ColorIndex := 0;
  // Draw text
  for i:=0 to Lines.Count-1 do
  begin
    s := Lines[i];
    FirstVis := HorzScrollPos;
    LastVis := Min(Length(s)-1, ClientWidth div CharSize.cx + HorzScrollPos);
    PrevBgColor := clNone;
    PrevTxColor := clNone;
    j1 := FirstVis;
    for j:=FirstVis to LastVis+1 do
    begin
      if j<=LastVis then
      begin
        if ColorIndex+j<Length(BgColors) then
          CurBgColor := BgColors[ColorIndex+j]
        else
          CurBgColor := Self.Color;
        if ColorIndex+j<Length(TxColors) then
          CurTxColor := TxColors[ColorIndex+j]
        else
          CurTxColor := Self.Font.Color;
      end
      else
      begin
        CurBgColor := clNone;
        CurTxColor := clNone;
      end;
      // When color changes, draw accumulated chars
      if (CurBgColor<>PrevBgColor) or (CurTxColor<>PrevTxColor) then
      begin
        if j>j1 then
        begin
          s1 := Copy(s, j1+1, j-j1);
          ScrBmp.Canvas.Brush.Color := PrevBgColor;
          ScrBmp.Canvas.Font.Color := PrevTxColor;
          ScrBmp.Canvas.TextOut((j1-HorzScrollPos) * CharSize.cx + 1, i * CharSize.cy, s1);
        end;
        PrevBgColor := CurBgColor;
        PrevTxColor := CurTxColor;
        j1 := j;
      end;
      if j > LastVis then Break;
    end;
    Inc(ColorIndex, Length(s));
  end;

  // Draw caret
  if (FShowCaret) and (CaretPos.Y >= 0) and (CaretPos.Y < Lines.Count) then
  with ScrBmp.Canvas do
  begin
    R := Rect(CharSize.cx * (CaretPos.X-HorzScrollPos) + 1, CharSize.cy * CaretPos.Y,
              CharSize.cx * (CaretPos.X-HorzScrollPos+1) + 1, CharSize.cy * (CaretPos.Y+1));
    if Focused then
    begin
      Pen.Color := clBlack;
      Brush.Color := clBlack;
    end
    else
    begin
      Pen.Color := clGray;
      Brush.Color := clGray;
    end;
    if InsertModeCaret then
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      MoveTo(R.Left-1, R.Top);
      LineTo(R.Left-1, R.Bottom);
    end
    else
    begin
      Pen.Mode := pmMergePenNot;
      Rectangle(R);
    end;
    Pen.Mode := pmCopy;
  end;

  {
  ScrBmp.Canvas.Pen.Color := clGray;
  ScrBmp.Canvas.Brush.Color := clGray;
  for i:=0 to Ord(High(TPenMode)) do
  begin
    R := Rect(i*CharSize.cx, 0, (i+1)*CharSize.cx, CharSize.cy);
    ScrBmp.Canvas.Pen.Mode := TPenMode(i);
    ScrBmp.Canvas.Rectangle(R);
  end;
  ScrBmp.Canvas.Pen.Color := clBlack;
  ScrBmp.Canvas.Brush.Color := clBlack;
  for i:=0 to Ord(High(TPenMode)) do
  begin
    R := Rect(i*CharSize.cx, CharSize.cy, (i+1)*CharSize.cx, 2*CharSize.cy);
    ScrBmp.Canvas.Pen.Mode := TPenMode(i);
    ScrBmp.Canvas.Rectangle(R);
  end;
  {}
end;

procedure TEditorPane.Paint;
begin
//  inherited Paint;
  if FUpdating>0 then Exit;
  if (ScrBmp.Width<>Width) or (ScrBmp.Height<>Height) then
    ScrBmp.SetSize(Width, Height);
  InternalPaint();
  Canvas.Draw(0, 0, ScrBmp);
  PaintControls(Canvas.Handle, nil);
end;

procedure TEditorPane.SetCaretPos(const Value: TPoint);
begin
  FCaretPos := Value;
  Paint();
end;

procedure TEditorPane.SetHorzScrollPos(const Value: Integer);
begin
  if FHorzScrollPos <> Value then
  begin
    FHorzScrollPos := Value;
    Paint();
  end;
end;

procedure TEditorPane.SetSelection(AStart, ALength: Integer);
begin
  SelStart := AStart;
  SelLength := ALength;
  Paint();
end;

procedure TEditorPane.SetShowCaret(const Value: Boolean);
begin
  if Value <> FShowCaret then
  begin
    FShowCaret := Value;
    Paint();
  end;
end;

procedure TEditorPane.SetText(const Value: string);
begin
  FLines.Text := Value;
  Paint();
end;

function TEditorPane.CharHeight: Integer;
begin
  if CharSize.cy>0 then
    Result := CharSize.cy
  else
    Result := ScrBmp.Canvas.TextHeight('O');
end;

function TEditorPane.CharWidth: Integer;
begin
  if CharSize.cx>0 then
    Result := CharSize.cx
  else
    Result := ScrBmp.Canvas.TextWidth('O');
end;

procedure TEditorPane.WMGetDlgCode(var Msg: TWMNoParams);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTTAB;
end;

end.
