{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHextorGUI;

interface

uses
  System.Classes, Vcl.Controls, Vcl.Menus, Vcl.Forms, System.Types,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, WinApi.Messages, Vcl.Graphics,
  Winapi.Windows, System.Math, Winapi.ShellAPI,

  uFormattedTextDraw;

type
  // Всплывающая подсказка, отображающая форматированный текст при помощи DrawFmtText()
  // В программе надо использовать глобальный FmtHint и его FmtHint.ShowHint()
  tFmtHintWindow = class(THintWindow)
  protected
    FmtText:string;
    HintWndMP:tPoint;
    AMouseMoveRect:TRect;
    HideTimer,ShowTimer:tTimer;
    ShowRect:TRect;
    procedure Paint; override;
    procedure OnHideTimer(Sender:tObject);
    procedure OnShowTimer(Sender:tObject);
  public
    const
      ChildControls = '%ChildCotrols%';
  public
    ForceReshowHintWindow:boolean;
    MaxWidth:integer;
    Delay:Cardinal;
    MouseMoveRect:TRect;
    procedure ShowHint(const Text:string; x:integer=-1; y:integer=-1; BgColor:tColor=$80FFFF; TextColor:tColor=clWindowText);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

  // Displays picture from given TImage (this allows having many instances of
  // some pictire in application windows without duplicating it's data)
  THintedImageProxy = class (TPaintBox)
  private
    FImageList: TImageList;
    FImageIndex: Integer;
    procedure SetImageList(const Value: TImageList);
    procedure SetImageIndex(const Value: Integer);
  published
  protected
    FImage: tImage;
    FHintFmt: string;
    procedure SetImage(const Value: tImage);
    procedure SetHintFmt(const Value: string);
    procedure UpdateSize();
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Image:tImage read FImage write SetImage;
    property ImageList:TImageList read FImageList write SetImageList;
    property ImageIndex:Integer read FImageIndex write SetImageIndex;
    property HintFmt:string read FHintFmt write SetHintFmt;
  end;

  TDropFileCatcher = class(TObject)
  private
    fDropHandle: HDROP;
    function GetFile(Idx: Integer): UnicodeString;
    function GetFileCount: Integer;
    function GetPoint: TPoint;
  public
    constructor Create(DropHandle: HDROP);
    destructor Destroy; override;
    property FileCount: Integer read GetFileCount;
    property Files[Idx: Integer]: UnicodeString read GetFile;
    property DropPoint: TPoint read GetPoint;
  end;

procedure PopupFromControl(Menu:tPopupMenu; Control:tControl);
function CreateFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
function MakeFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
function FmtHint(): TFmtHintWindow;
procedure AddComboBoxHistory(CB: TComboBox; Text: string; MaxCount: Integer = 20);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DWF', [THintedImageProxy]);
end;

procedure PopupFromControl(Menu:tPopupMenu; Control:tControl);
var
  p:tPoint;
begin
  if Menu=nil then
  begin
    if Control is tButton then
      Menu:=(Control as tButton).PopupMenu;
    if Control is TSpeedButton then
      Menu:=(Control as TSpeedButton).PopupMenu;
  end;
  p:=Point(0,Control.Height);
  p:=Control.ClientToScreen(p);
  Menu.Popup(p.X,p.Y);
end;

function CreateFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
// Создает окно и перемещает в него указанный компонент.
// Используется обычно чтобы создать кастомное диалоговое окно без
// необходимости добавлять в проект отдельный файл для формы
begin
  Application.CreateForm(TForm,Result);
  Result.Name:=Content.Name+'_Form';
  Result.BorderStyle:=BorderStyle;
  Content.Parent:=Result;
  Content.Left:=0;
  Content.Top:=0;
  Result.ClientWidth:=Content.Width;
  Result.ClientHeight:=Content.Height;
  Content.Align:=alClient;
  Result.Position:=poMainFormCenter;
  Content.Visible:=True;
  Result.Caption:=Caption;
end;

function MakeFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
// Создаёт окно с указанным содержимым, если оно ещё не создано
begin
  if (Content.Parent<>nil) and
     (Content.Parent is TForm) and
     (Content.Parent.Name=Content.Name+'_Form') then
  begin
    Result:=(Content.Parent as TForm);
    Result.Caption := Caption;
  end
  else
    Result:=CreateFormWithContent(Content,BorderStyle,Caption);
end;

var
  DefaultFmtHintWindow: TFmtHintWindow = nil;

function FmtHint(): TFmtHintWindow;
begin
  if DefaultFmtHintWindow = nil then
  begin
    DefaultFmtHintWindow := tFmtHintWindow.Create(Application);
  end;
  Result := DefaultFmtHintWindow;
end;

procedure AddComboBoxHistory(CB: TComboBox; Text: string; MaxCount: Integer = 20);
var
  n: Integer;
begin
  n := CB.Items.IndexOf(Text);
  if n >= 0 then
    CB.Items.Move(n, 0)
  else
    CB.Items.Insert(0, Text);
  while CB.Items.Count > MaxCount do
    CB.Items.Delete(CB.Items.Count-1);
end;

{ tFmtHintWindow }

constructor tFmtHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  HideTimer:=TTimer.Create(self);
  HideTimer.Interval:=230;
  HideTimer.OnTimer:=OnHideTimer;
  HideTimer.Enabled:=True;
  ShowTimer:=TTimer.Create(self);
  ShowTimer.Interval:=500;
  ShowTimer.OnTimer:=OnShowTimer;
  ShowTimer.Enabled:=False;
  MaxWidth:=400;
end;

destructor tFmtHintWindow.Destroy;
begin
  inherited;
end;

procedure tFmtHintWindow.OnHideTimer(Sender: tObject);
var
  mp:tPoint;
begin
  // Прячем хинт если курсор переместился
  if ((Visible) and (HandleAllocated)) or (ShowTimer.Enabled) then
  begin
    GetCursorPos(mp);
    //if (Abs(mp.X-HintWndMP.X)>5)or(Abs(mp.Y-HintWndMP.Y)>5) then
    if not PtInRect(AMouseMoveRect,mp) then
    begin
      if HandleAllocated then
        ReleaseHandle();
//      else
        ShowTimer.Enabled:=False;
    end;
  end;
end;

procedure tFmtHintWindow.OnShowTimer(Sender: tObject);
begin
  ShowTimer.Enabled:=False;
  ActivateHint(ShowRect,'');
end;

procedure tFmtHintWindow.Paint;
begin
  inherited;
  if FmtText<>ChildControls then
  begin
    Canvas.Font.Color:=Font.Color;
    DrawFmtText(Canvas, Rect(2, 2, ClientRect.Right, ClientRect.Bottom), FmtText);
  end;
end;

procedure tFmtHintWindow.ShowHint(const Text: string; x, y: integer; BgColor,
  TextColor: tColor);
// Всплывающая подсказка
var
  ts:TSize;
  r:tRect;
begin
  if self=nil then exit;

  if Text='' then
  begin
    ShowTimer.Enabled:=False;
    ReleaseHandle();
    exit;
  end;

  GetCursorPos(HintWndMP);
  if (x<0)or(y<0) then
  begin
    x:=HintWndMP.X;
    y:=HintWndMP.Y+20;
  end;
  if MouseMoveRect.IsEmpty then
    AMouseMoveRect:=Rect(HintWndMP.X-5,HintWndMP.Y-5,HintWndMP.X+5,HintWndMP.Y+5)
  else
    AMouseMoveRect:=MouseMoveRect;
  MouseMoveRect:=Rect(-1,-1,-1,-1);

  if (not ForceReshowHintWindow) and (Color=BgColor) and (Font.Color=TextColor) and
     (FmtText=Text) and (x=Left) and (y=Top) and
     (WindowHandle<>0) then exit;
  ForceReshowHintWindow:=false;

  Color:=BgColor;
  FmtText:=Text;
  if Text=ChildControls then
  begin
    ShowRect:=Rect(x,y,x+Width,y+Height);
  end
  else
  begin
    Font.Color:=TextColor;
    FillChar(r,SizeOf(r),0);
  //  DrawText(HintWindow.Canvas.Handle,PChar(Text),Length(Text),r,DT_CALCRECT or DT_NOPREFIX);
    DrawFmtText(Canvas,Rect(0,0,Min(Screen.Width,MaxWidth),1000),Text,@r,[ftfNoDraw]);

    ts.cx:=r.Right;
    ts.cy:=r.Bottom;
    inc(ts.cx,8);
    inc(ts.cy,4);
  {  if HintWindow.Visible then
    begin
      HintWindow.Caption:=Text;
      SetWindowPos(HintWindow.Handle, HWND_TOPMOST, x, y, ts.cx, ts.cy, SWP_NOACTIVATE);
    end
    else}
    ShowRect:=Rect(x,y,x+ts.cx,y+ts.cy);
  end;
  ShowTimer.Enabled:=False;

  if Delay=0 then
  begin
    ActivateHint(ShowRect,'');
  end
  else
  begin
    ShowTimer.Interval:=Delay;
    ShowTimer.Enabled:=True;
    Delay:=0;
  end;
end;

{ THintedImageProxy }

procedure THintedImageProxy.UpdateSize();
begin
  if Image<>nil then
    SetBounds(Left,Top,Image.Width,Image.Height);
  if ImageList<>nil then
    SetBounds(Left,Top,ImageList.Width,ImageList.Height);
end;

procedure THintedImageProxy.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FmtHint().ShowHint('');
end;

procedure THintedImageProxy.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FmtHint().ShowHint(HintFmt);
end;

procedure THintedImageProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and (AComponent=Image) then
    Image:=nil;
  if (Operation=opRemove) and (AComponent=ImageList) then
    ImageList:=nil;
end;

procedure THintedImageProxy.Paint;
begin
  inherited;
  if (Enabled) or (csDesigning in ComponentState) then
  begin
    if (Image<>nil) and (Image.Picture.Graphic<>nil) then
      Canvas.Draw(0,0,Image.Picture.Graphic);
    if (ImageList<>nil) and (ImageIndex>=0) and (ImageIndex<ImageList.Count) then
      ImageList.Draw(Canvas,0,0,ImageIndex);
  end
  else
  begin
    Canvas.Pen.Color:=clGrayText;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Width:=0;
    Canvas.Rectangle(ClientRect);
  end;
end;

procedure THintedImageProxy.SetHintFmt(const Value: string);
var
  p:TPoint;
  wc:TWinControl;
  c:TControl;
begin
  if Value<>FHintFmt then
  begin
    FHintFmt := Value;
    if (ComponentState*[csDesigning,csReading])=[] then
    // Обновляем хинт, если он сейчас показан
    begin
      if not GetCursorPos(p) then Exit;
      wc:=FindVCLWindow(p);
      if wc=nil then Exit;
      p:=wc.ScreenToClient(p);
      c:=wc.ControlAtPos(p,False,True,True);
      if c=Self then
        MouseMove([],0,0);
    end;
  end;
end;

procedure THintedImageProxy.SetImage(const Value: tImage);
begin
  if FImage<>Value then
  begin
    FImage:=Value;
    if Value<>nil then
    begin
      Value.FreeNotification(self);
      ImageList:=nil;
    end;
    UpdateSize();
    Refresh();
  end;
end;

procedure THintedImageProxy.SetImageIndex(const Value: Integer);
begin
  if FImageIndex<>Value then
  begin
    FImageIndex := Value;
    if ImageList<>nil then Refresh;
  end;
end;

procedure THintedImageProxy.SetImageList(const Value: TImageList);
begin
  if FImageList<>Value then
  begin
    FImageList:=Value;
    if Value<>nil then
    begin
      Value.FreeNotification(self);
      Image:=nil;
    end;
    UpdateSize();
    Refresh();
  end;
end;

{ TDropFileCatcher }

constructor TDropFileCatcher.Create(DropHandle: HDROP);
begin
  inherited Create;
  fDropHandle := DropHandle;
end;

destructor TDropFileCatcher.Destroy;
begin
  DragFinish(fDropHandle);
  inherited;
end;

function TDropFileCatcher.GetFile(Idx: Integer): UnicodeString;
var
  FileNameLength: Integer;
begin
  FileNameLength := DragQueryFile(fDropHandle, Idx, nil, 0);
  SetLength(Result, FileNameLength);
  DragQueryFile(fDropHandle, Idx, PWideChar(Result), FileNameLength + 1);
end;

function TDropFileCatcher.GetFileCount: Integer;
begin
  Result := DragQueryFile(fDropHandle, $FFFFFFFF, nil, 0);
end;

function TDropFileCatcher.GetPoint: TPoint;
begin
  DragQueryPoint(fDropHandle, {$IFDEF FPC}@{$ENDIF}Result);
end;

end.
