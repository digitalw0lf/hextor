unit uEditorPane;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Graphics, System.Types, Winapi.Messages,

  uLogFile, uUtil;

type
  TEditorPane = class(TPanel)
  private
    FCaretPos: TPoint;
    FShowCaret: Boolean;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetCaretPos(const Value: TPoint);
    procedure SetShowCaret(const Value: Boolean);
    { Private declarations }
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
    CaretInsertMode: Boolean;
    BgColors: array of TColor;
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
  Pos.X := x div CharSize.cx;
  Pos.Y := y div CharSize.cy;
  Result := (Pos.Y>=0) and (Pos.Y<Lines.Count) and (Pos.X>=0) and (Pos.X<Length(Lines[Pos.Y]));
  // After last char
  if (Pos.Y=Lines.Count-1) and (Pos.X=Length(Lines[Pos.Y])) then Result := True;
end;

function TEditorPane.GetText: string;
begin
  Result := FLines.Text;
end;

procedure TEditorPane.InternalPaint;
var
  i, j, j1, ColorIndex: Integer;
  CurBgColor, PrevBgColor: TColor;
  R: TRect;
  s, s1: string;
begin
  ScrBmp.Canvas.Brush.Color := Self.Color;
  ScrBmp.Canvas.Font := Self.Font;
  ScrBmp.Canvas.FillRect(ClientRect);
  CharSize := ScrBmp.Canvas.TextExtent('O');
  ColorIndex := 0;
  for i:=0 to Lines.Count-1 do
  begin
    s := Lines[i];
    PrevBgColor := clNone;
    j1 := 0;
    for j:=0 to Length(s) do
    begin
      if j<Length(s) then
      begin
        if ColorIndex<Length(BgColors) then
          CurBgColor := BgColors[ColorIndex]
        else
          CurBgColor := Self.Color;
      end
      else
        CurBgColor := clNone;
      if CurBgColor<>PrevBgColor then
      begin
        if j>j1 then
        begin
          s1 := Copy(s, j1+1, j-j1);
          ScrBmp.Canvas.Brush.Color := PrevBgColor;
          ScrBmp.Canvas.TextOut(j1*CharSize.cx, i * CharSize.cy, s1);
        end;
        PrevBgColor := CurBgColor;
        j1 := j;
      end;
      if j=Length(s) then Break;
      Inc(ColorIndex);
    end;
  end;

  if (FShowCaret) and (CaretPos.Y >= 0) and (CaretPos.Y < Lines.Count) then
  with ScrBmp.Canvas do
  begin
    R := Rect(CharSize.cx * CaretPos.X, CharSize.cy * CaretPos.Y,
              CharSize.cx * (CaretPos.X+1), CharSize.cy * (CaretPos.Y+1));
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
    if CaretInsertMode then
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      MoveTo(R.Left+1, R.Top);
      LineTo(R.Left+1, R.Bottom);
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
end;

procedure TEditorPane.SetCaretPos(const Value: TPoint);
begin
  FCaretPos := Value;
  Paint();
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