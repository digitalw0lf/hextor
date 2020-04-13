{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uBitmapFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Math, System.Types,

  uHextorTypes, uEditorForm, Vcl.Buttons;

type
  TBitmapFrame = class(TFrame, IHextorToolFrame)
    MainPaintBox: TPaintBox;
    LeftPanel: TPanel;
    TopPanel: TPanel;
    Label1: TLabel;
    EditWidth: TSpinEdit;
    TrackBarWidth: TTrackBar;
    Label2: TLabel;
    EditHScroll: TSpinEdit;
    TrackBarHScroll: TTrackBar;
    EditScale: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    EditBPP: TComboBox;
    Label5: TLabel;
    EditPalette: TComboBox;
    VertScrollBar: TScrollBar;
    BtnFlipVert: TSpeedButton;
    BtnFlipHorz: TSpeedButton;
    Label6: TLabel;
    EditByteShift: TSpinEdit;
    procedure TrackBarWidthChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure TrackBarHScrollChange(Sender: TObject);
    procedure EditHScrollChange(Sender: TObject);
    procedure EditScaleChange(Sender: TObject);
    procedure EditBPPChange(Sender: TObject);
    procedure EditPaletteChange(Sender: TObject);
    procedure TopPanelResize(Sender: TObject);
    procedure MainPaintBoxPaint(Sender: TObject);
    procedure VertScrollBarChange(Sender: TObject);
    procedure MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnFlipVertClick(Sender: TObject);
    procedure BtnFlipHorzClick(Sender: TObject);
    procedure EditByteShiftChange(Sender: TObject);
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private type
    TCardinalArray = array[0..0] of Cardinal;
    PCardinalArray = ^TCardinalArray;
  private const
    ScalesList: array [0 .. 4] of Double = (0.25, 0.5, 1, 2, 4);
    bppList: array [0 .. 6] of Integer = (1, 2, 4, 8, 16, 24, 32);
    st2: array [0 .. 7] of Byte = (1, 2, 4, 8, 16, 32, 64, 128);
    st4: array [0 .. 3] of Byte = (1, 4, 16, 64);
    st16: array [0 .. 1] of Byte = (1, 16);
    st256: array [0 .. 3] of Integer = (1, 256, 256 * 256, 256 * 256 * 256);
    Palette: array [0 .. 2, 0 .. 15] of Integer = ((clblack, clwhite, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), (clblack, clBlue, clRed, clwhite, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0), (0, 11010048, 43008, 11053056, 168,
      11010216, 21672, 11053224, 5788760, 16733272, 5831768, 16776280, 5788927,
      16733439, 5831935, 16776447));
  private
    { Private declarations }
    FEditor: TEditorForm;
    ScrBmp: TBitmap;
    BmpData: PCardinalArray;
    PrevFileSize, FBitsPerScrollBarTick: TFilePointer;
    BaseVisibleBit, FirstVisibleBit: TFilePointer;
    Data: TBytes;
    BitsPerPixel: Integer;
    SelectedPalette: Integer;
    DisplayScale: Double;
    FlipVert, FlipHorz: Boolean;
    AWidth, HScroll: Integer;
    ByteShift: Integer;  // 0..3 bytes shift of first pixel in case of multy-byte pixels
    CLockControls: Integer;
    procedure CreateBitmap(aSX, aSY: Integer);
    function GetValue(Index: Integer): Cardinal;
    function ValueToColor(Value: Cardinal): Cardinal;
    procedure GenerateScrBmp();
    procedure DrawScrToPaintBox();
    procedure EditorVisibleRangeChanged(Sender: TEditorForm);
    function GetMaxVisibleBytesCount(): Integer;
    procedure UpdateScrollBars();
    function AddrToScreen(BitAddr: TFilePointer; var x, y: Integer; ScreenScale: Boolean): Boolean;
    function ScreenToAddr(x, y: Integer; var BitAddr: TFilePointer; ScreenScale: Boolean): Boolean;
  public
    { Public declarations }
    procedure Redraw();
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure OnShown();
  end;

procedure DrawEditorViewFrame(Canvas: TCanvas; R: TRect);

implementation

uses
  uMainForm;

{$R *.dfm}

procedure DrawEditorViewFrame(Canvas: TCanvas; R: TRect);
begin
  // Frame of data range shown in editor on some graphical bar
  with Canvas do
  begin
    Polyline([Point(R.Left + 3, R.Top),
              Point(R.Left, R.Top),
              Point(R.Left, R.Bottom),
              Point(R.Left + 4, R.Bottom)]);
    Polyline([Point(R.Right - 3, R.Top),
              Point(R.Right, R.Top),
              Point(R.Right, R.Bottom),
              Point(R.Right - 4, R.Bottom)]);
  end;
end;

{ TBitmapFrame }

procedure TBitmapFrame.CreateBitmap(aSX, aSY: Integer);
var
  BInfo: tagBITMAPINFO;
begin
  ScrBmp.Free;
  BInfo.bmiHeader.biSize := SizeOf(tagBITMAPINFOHEADER);
  BInfo.bmiHeader.biWidth := aSX;
  BInfo.bmiHeader.biHeight := -aSY;
  BInfo.bmiHeader.biPlanes := 1;
  BInfo.bmiHeader.biBitCount := 32;
  BInfo.bmiHeader.biCompression := BI_RGB;
  ScrBmp := TBitmap.Create();
  ScrBmp.Handle := CreateDIBSection(MainPaintBox.Canvas.Handle, BInfo,
    DIB_RGB_COLORS, Pointer(BmpData), 0, 0);
end;

function TBitmapFrame.AddrToScreen(BitAddr: TFilePointer; var x, y: Integer; ScreenScale: Boolean): Boolean;
var
  p: TFilePointer;
begin
  p := (BitAddr - FirstVisibleBit) div BitsPerPixel + HScroll;
  x := p mod AWidth;
  y := p div AWidth;
  Result := (x >= 0) and (x < ScrBmp.Width) and
            (y >= 0) and (y < ScrBmp.Height);
  if ScreenScale then
  begin
    x := Floor(x * DisplayScale);
    y := Floor(y * DisplayScale);
  end;
end;

procedure TBitmapFrame.BtnFlipHorzClick(Sender: TObject);
begin
  FlipHorz := BtnFlipHorz.Down;
  UpdateScrollBars();
  Redraw();
end;

procedure TBitmapFrame.BtnFlipVertClick(Sender: TObject);
begin
  FlipVert := BtnFlipVert.Down;
  UpdateScrollBars();
  Redraw();
end;

constructor TBitmapFrame.Create(AOwner: TComponent);
begin
  inherited;
  DisplayScale := 1;
  EditWidthChange(nil);
  EditHScrollChange(nil);
  EditBPPChange(nil);
  EditPaletteChange(nil);
  EditScaleChange(nil);
  MainForm.OnVisibleRangeChanged.Add(EditorVisibleRangeChanged);
end;

destructor TBitmapFrame.Destroy;
begin
  ScrBmp.Free;
  inherited;
end;

procedure TBitmapFrame.DrawScrToPaintBox();
var
  R: TRect;
  x: Integer;
  pt: TPoint;
begin
  if ScrBmp <> nil then
  begin
    R := Rect(0, 0, Round(ScrBmp.Width * DisplayScale), Round(ScrBmp.Height * DisplayScale));
    if FlipVert then
    begin
      x := R.Top;
      R.Top := R.Bottom;
      R.Bottom := x;
    end;
    if FlipHorz then
    begin
      x := R.Left;
      R.Left := R.Right;
      R.Right := x;
    end;

    with MainPaintBox.Canvas do
    begin
      GetBrushOrgEx(Handle, pt);
      if DisplayScale < 1 then
        SetStretchBltMode(Handle, HALFTONE)
      else
        SetStretchBltMode(Handle, COLORONCOLOR);
      SetBrushOrgEx(Handle, pt.x, pt.y, @pt);

//      StretchDraw(R, ScrBmp);
      StretchBlt(Handle, R.Left, R.Top, R.Width, R.Height,
        ScrBmp.Canvas.Handle, 0, 0, ScrBmp.Width,
        ScrBmp.Height, SRCCOPY);
    end;
  end;
end;

procedure TBitmapFrame.EditBPPChange(Sender: TObject);
begin
  BitsPerPixel := bppList[EditBPP.ItemIndex];
  UpdateScrollBars();
  Redraw();
end;

procedure TBitmapFrame.EditByteShiftChange(Sender: TObject);
begin
  ByteShift := EditByteShift.Value;
  Redraw();
end;

procedure TBitmapFrame.EditHScrollChange(Sender: TObject);
var
  x: Integer;
begin
  x := EditHScroll.Value;
//  if (x <= TrackBarHScroll.Max) and (x <> TrackBarHScroll.Position) then
  Inc(CLockControls);
  try
    TrackBarHScroll.Position := x;
  finally
    Dec(CLockControls);
  end;
  HScroll := EditHScroll.Value;
  Redraw();
end;

procedure TBitmapFrame.EditorVisibleRangeChanged(Sender: TEditorForm);
begin
  FEditor := MainForm.GetActiveEditorNoEx;
  if not Parent.Visible then Exit;

  if (Sender <> nil) and (Sender.Data.GetSize() <> PrevFileSize) then
    UpdateScrollBars();
  Redraw();
end;

procedure TBitmapFrame.EditPaletteChange(Sender: TObject);
begin
  SelectedPalette := EditPalette.ItemIndex;
  Redraw();
end;

procedure TBitmapFrame.EditScaleChange(Sender: TObject);
begin
  DisplayScale := ScalesList[EditScale.ItemIndex];
  UpdateScrollBars();
  Redraw();
end;

procedure TBitmapFrame.EditWidthChange(Sender: TObject);
var
  w: Integer;
begin
  w := EditWidth.Value;
//  if (w <= TrackBarWidth.Max) and (w <> TrackBarWidth.Position) then
  Inc(CLockControls);
  try
    TrackBarWidth.Position := w;
  finally
    Dec(CLockControls);
  end;
  AWidth := EditWidth.Value;
  UpdateScrollBars();
  Redraw();
end;

procedure TBitmapFrame.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ControlAtPos(ScreenToClient(MousePos), False, True, True) = MainPaintBox then
  begin
    VertScrollBar.Position := VertScrollBar.Position - WheelDelta;
  end;
end;

procedure TBitmapFrame.GenerateScrBmp();
var
  SX, SY: Integer;
  BitsPerLine: Integer;
  PixelCount, p: Integer;
  x, y: Integer;
  Value, Clr: Cardinal;
  R: TRect;
begin
  SX := Round(MainPaintBox.Width / DisplayScale);
  SY := Round(MainPaintBox.Height / DisplayScale);
  if (ScrBmp = nil) or (ScrBmp.Width <> SX) or (ScrBmp.Height <> SY) then
    CreateBitmap(SX, SY);
  ZeroMemory(BmpData, SX * SY * 4);

  if FEditor = nil then Exit;

  BaseVisibleBit := TFilePointer(VertScrollBar.Position) * FBitsPerScrollBarTick;

  BitsPerLine := BitsPerPixel * AWidth;

  FirstVisibleBit := BaseVisibleBit div BitsPerLine * BitsPerLine + ByteShift * 8;
  Data := FEditor.Data.Get(FirstVisibleBit div 8, SY * (BitsPerLine div 8));

  PixelCount := Length(Data) * 8 div BitsPerPixel;
  for p:=0 to PixelCount-1 do
  begin
    x := (p + HScroll) mod AWidth;
    y := (p + HScroll) div AWidth;
    if (x < 0) or (x >= SX) or (y < 0) or (y >= SY) then Continue;
    Value := GetValue(p + FirstVisibleBit mod 8);
    Clr := ValueToColor(Value);

    BmpData[y*SX+x] := Clr;
  end;

  // Frame of range visible in editor panes
  if AWidth > 4 then
  begin
    R.Left := 0;
    R.Right := AWidth-1;
    AddrToScreen(FEditor.FirstVisibleAddr()*8, x, y, False);
    R.Top := y;
    AddrToScreen((FEditor.FirstVisibleAddr() + FEditor.VisibleBytesCount())*8, x, y, False);
    R.Bottom := y - 1;
    ScrBmp.Canvas.Pen.Color := clYellow;
    DrawEditorViewFrame(ScrBmp.Canvas, R);
  end;
end;

function TBitmapFrame.GetMaxVisibleBytesCount: Integer;
// How many bytes will fit in screen with current settings
begin
  Result := EditWidth.Value * Round(MainPaintBox.Height / DisplayScale) * BitsPerPixel div 8;
end;

function TBitmapFrame.GetValue(Index: Integer): Cardinal;
begin
  if (Index < 0) or ((Index+1) * BitsPerPixel > Length(Data) * 8) then
    exit(0);

  case BitsPerPixel of
    1:
      Result := (Data[Index div 8] shr (Index mod 8)) and $01;
    2:
      Result := (Data[Index div 4] shr (Index mod 4 * 2)) and $03;
    4:
      Result := (Data[Index div 2] shr (Index mod 2 * 4)) and $0F;
    8:
      Result := Data[Index];
    16:
      Result := Data[Index * 2] + Data[Index * 2 + 1] shl 8;
    24:
      Result := Data[Index * 3] + Data[Index * 3 + 1] shl 8 +
        Data[Index * 3 + 2] shl 16;
    32:
      Result := Data[Index * 4] + Data[Index * 4 + 1] shl 8 +
        Data[Index * 4 + 2] shl 16 + Data[Index * 4 + 3] shl 24;
  else
    Result := 0;
  end;
end;

procedure TBitmapFrame.MainPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Addr: TFilePointer;
begin
  if FEditor = nil then Exit;
  ScreenToAddr(X, Y, Addr, True);
  Addr := Addr div 8;
  FEditor.MoveCaret(Addr, KeyboardStateToShiftState());
end;

procedure TBitmapFrame.MainPaintBoxPaint(Sender: TObject);
begin
  DrawScrToPaintBox();
end;

procedure TBitmapFrame.OnShown;
begin
  FEditor := MainForm.GetActiveEditorNoEx;
  Redraw();
end;

procedure TBitmapFrame.TopPanelResize(Sender: TObject);
begin
  UpdateScrollBars();
end;

procedure TBitmapFrame.Redraw;
begin
  GenerateScrBmp();
  DrawScrToPaintBox();
end;

function TBitmapFrame.ScreenToAddr(x, y: Integer; var BitAddr: TFilePointer; ScreenScale: Boolean): Boolean;
begin
  if ScreenScale then
  begin
    x := Floor(x / DisplayScale);
    y := Floor(y / DisplayScale);
  end;
  BitAddr := ((y * AWidth + x) - HScroll + FirstVisibleBit div BitsPerPixel) * BitsPerPixel;

  Result := (BitAddr >= 0) and (BitAddr < FEditor.Data.GetSize() * 8);
end;

procedure TBitmapFrame.TrackBarHScrollChange(Sender: TObject);
begin
  if CLockControls > 0 then Exit;
  EditHScroll.Value := TrackBarHScroll.Position;
end;

procedure TBitmapFrame.TrackBarWidthChange(Sender: TObject);
begin
  if CLockControls > 0 then Exit;
  EditWidth.Value := TrackBarWidth.Position;
end;

procedure TBitmapFrame.UpdateScrollBars;
begin
  if FEditor = nil then
  begin
    ConfigureScrollbar(VertScrollBar, 0, 0);
    Exit;
  end;

  // "Byte shift" field range
  EditByteShift.EditorEnabled := (BitsPerPixel > 8);
  EditByteShift.MaxValue := (BitsPerPixel-1) div 8;

  // Adjust Horizontal trackbars
  Inc(CLockControls);
  try
    TrackBarWidth.Width := TopPanel.Width - TrackBarWidth.Left;
    TrackBarWidth.Max := Round(MainPaintBox.Width / DisplayScale);
    TrackBarHScroll.Width := TopPanel.Width - TrackBarHScroll.Left;
    TrackBarHScroll.Max := Round(MainPaintBox.Width / DisplayScale);
  finally
    Dec(CLockControls);
  end;

  // Adjust vertical scrollbar
  FBitsPerScrollBarTick := Max(FEditor.Data.GetSize() * 8 div 10000{00000}, 1);
  PrevFileSize := FEditor.Data.GetSize();
  if PrevFileSize = 0 then
    ConfigureScrollbar(VertScrollBar, 0, 0)
  else
    ConfigureScrollbar(VertScrollBar, (PrevFileSize * 8 div FBitsPerScrollBarTick) - 1,
                       (GetMaxVisibleBytesCount() * 8 div FBitsPerScrollBarTick));
  VertScrollBar.LargeChange := VertScrollBar.PageSize;

end;

function TBitmapFrame.ValueToColor(Value: Cardinal): Cardinal;
begin
  case SelectedPalette of
    0:
      Result := Palette[0, Value mod 2];
    1:
      Result := Palette[1, Value mod 4];
    2:
      Result := Palette[2, Value mod 16];
    3:
      Result := (Value and $FF) * 256 * 256;
    4:
      Result := (Value and $FF) * 256;
    5:
      Result := Value and $FF;
    6:
      Result := Value;
    7:
      Result := RGB(GetBValue(Value), GetGValue(Value), GetRValue(Value));
  else
    Result := 0;
  end;

end;

procedure TBitmapFrame.VertScrollBarChange(Sender: TObject);
begin
  Redraw();
end;

end.
