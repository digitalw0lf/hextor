{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uBitsEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types;

type
  TBitsEditorForm = class(TForm)
    PaintBox1: TPaintBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    EditDec: TEdit;
    EditHex: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    const
      cw = 25;
      ch = 25;
  private
    { Private declarations }
    ScrBmp: TBitmap;
    FValue: Int64;
    HoveredBit: Integer;
    procedure InternalPaint();
    function BitRect(n: Integer): TRect;
    procedure SetValue(const Value: Int64);
    function BitAtScrPos(p: TPoint): Integer;
  public
    { Public declarations }
    ValueSize: Integer;
    OkEnabled: Boolean;
    property Value: Int64 read FValue write SetValue;
  end;

var
  BitsEditorForm: TBitsEditorForm;

implementation

{$R *.dfm}

function TBitsEditorForm.BitAtScrPos(p: TPoint): Integer;
var
  i: Integer;
begin
  for i:=0 to ValueSize*8-1 do
    if PtInRect(BitRect(i), p) then
      Exit(i);
  Result := -1;
end;

function TBitsEditorForm.BitRect(n: Integer): TRect;
begin
  Result.Left := ScrBmp.Width - cw - cw*n - (n div 8)*cw div 2;
  Result.Right := Result.Left + cw;
  Result.Top := 25;
  Result.Bottom := Result.Top + ch;
end;

procedure TBitsEditorForm.FormCreate(Sender: TObject);
begin
  ScrBmp := TBitmap.Create();
  FValue := Low(FValue);
end;

procedure TBitsEditorForm.FormDestroy(Sender: TObject);
begin
  ScrBmp.Free;
end;

procedure TBitsEditorForm.FormShow(Sender: TObject);
begin
  ClientWidth := PaintBox1.Left*2 + 25*ValueSize*8 + 12*ValueSize;
  if OkEnabled then
  begin
    BtnOk.Enabled := True;
    ActiveControl := BtnOk;
  end
  else
    BtnOk.Enabled := False;
end;

procedure TBitsEditorForm.InternalPaint;
var
  i: Integer;
  R, R1: TRect;
  s: string;
begin
  if (ScrBmp.Width <> PaintBox1.Width) or (ScrBmp.Height <> PaintBox1.Height) then
    ScrBmp.SetSize(PaintBox1.Width, PaintBox1.Height);

  with ScrBmp.Canvas do
  begin
    Brush.Color := PaintBox1.Color;
    FillRect(Rect(0, 0, ScrBmp.Width, ScrBmp.Height));

    Pen.Color := clBlack;
    Font.Name := PaintBox1.Font.Name;
    for i:=0 to ValueSize*8-1 do
    begin
      Font.Orientation := 0;
      s := IntToStr((Value shr i) and 1);
      if ((Value shr i) and 1)<>0 then
      begin
        Font.Color := clBlack;
      end
      else
      begin
        Font.Color := clLtGray;
      end;

      R := BitRect(i);

      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      Rectangle(R);
      Font.Size := 14;
      Brush.Style := bsClear;
      R1 := R;
      TextRect(R1, s, [tfCenter, tfVerticalCenter]);

      Font.Color := clBlack;
      Font.Size := 8;
      // bit number
      R1 := Rect(R.Left, R.Top-15, R.Right, R.Top);
      s := IntToStr(i);
      TextRect(R1, s, [tfCenter, tfVerticalCenter]);
      // bit value
      R1 := Rect(R.Left, R.Bottom, R.Right, R.Bottom+15);
      s := IntToStr(Int64(1) shl i);
      if (i <= 7) or (i = HoveredBit) then
      begin
        if Length(s)<=3 then
        begin
          TextRect(R1, s, [tfCenter, tfVerticalCenter]);
        end
        else
          TextOut(R1.Left + 3, R1.Top + 3, s);
      end;
    end;
  end;
end;

procedure TBitsEditorForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Button = mbLeft then
  begin
    i := BitAtScrPos(Point(x, y));
    if i >= 0 then
      Value := Value xor (1 shl i);
  end;
end;

procedure TBitsEditorForm.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HoveredBit := BitAtScrPos(Point(X, Y));
  if HoveredBit >= 0 then
    PaintBox1.Cursor := crHandPoint
  else
    PaintBox1.Cursor := crDefault;
  PaintBox1Paint(nil);
end;

procedure TBitsEditorForm.PaintBox1Paint(Sender: TObject);
begin
  InternalPaint();
  PaintBox1.Canvas.Draw(0, 0, ScrBmp);
end;

procedure TBitsEditorForm.SetValue(const Value: Int64);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    EditDec.Text := 'dec: ' + IntToStr(FValue);
    EditHex.Text := 'hex: ' + IntToHex(FValue, ValueSize*2);
    PaintBox1Paint(nil);
  end;
end;

end.
