{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uHextorGUI;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, Vcl.Controls, Vcl.Menus,
  Vcl.Forms, System.Types, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  WinApi.Messages, Vcl.Graphics, Winapi.Windows, System.Math, Winapi.ShellAPI,
  Generics.Collections, Vcl.Themes, Vcl.AppEvnts,

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

  TDropFileCatcher = class;
  TDropFilesEvent = procedure (Sender: TDropFileCatcher; Control: TWinControl; Files: TStrings; DropPoint: TPoint) of Object;

  TDropFileCatcher = class(TComponent)
  private
    fDropHandle: HDROP;
    FControl: TWinControl;
    FAppEvents: TApplicationEvents;
    FOnDropFiles: TDropFilesEvent;
    FFiles: TStringList;
    FEnabled: Boolean;
    function GetFile(Idx: Integer): UnicodeString;
    function GetFileCount: Integer;
    function GetPoint: TPoint;
    procedure SetControl(const Value: TWinControl);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure SetOnDropFiles(const Value: TDropFilesEvent);
    procedure Internal_Disable();
    procedure Internal_Enable();
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Control: TWinControl read FControl write SetControl;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write SetOnDropFiles;
  end;

  TScrollEvent64 = procedure(Sender: TObject; ScrollCode: TScrollCode;
    var ScrollPos: Int64) of object;

  TScrollBar64 = class(TWinControl)
  private
    FKind: TScrollBarKind;
    FPosition: Int64;
    FMin: Int64;
    FMax: Int64;
    FPageSize: Int64;
    FRTLFactor: Integer;
    FSmallChange: Int64;
    FLargeChange: Int64;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent64;
    class constructor Create;
    class destructor Destroy;
    procedure DoScroll(var Message: TWMScroll);
    function NotRightToLeft: Boolean;
    procedure SetKind(Value: TScrollBarKind);
    procedure SetMax(Value: Int64);
    procedure SetMin(Value: Int64);
    procedure SetPosition(Value: Int64);
    procedure SetPageSize(Value: Int64);
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure CNCtlColorScrollBar(var Message: TMessage); message CN_CTLCOLORSCROLLBAR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    InternalRange: Int64;  // MAX value for underlying win32 SCROLLBAR (fits in 32 bits)
    procedure ChooseInternalRange();
    function Param64to32(Value: Int64): Integer;
    function Param32to64(Value: Integer): Int64;
    function CanObserve(const ID: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; dynamic;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Int64); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(APosition, AMin, AMax: Int64);
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property LargeChange: Int64 read FLargeChange write FLargeChange default 1;
    property Max: Int64 read FMax write SetMax default 100;
    property Min: Int64 read FMin write SetMin default 0;
    property PageSize: Int64 read FPageSize write SetPageSize;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property Position: Int64 read FPosition write SetPosition default 0;
    property ShowHint;
    property SmallChange: Int64 read FSmallChange write FSmallChange default 1;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property StyleElements;
    property OnContextPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnScroll: TScrollEvent64 read FOnScroll write FOnScroll;
    property OnStartDock;
    property OnStartDrag;
  end;

const
  sTextFromControl = '%%TextFromControl%%';  // Used as default parameter value

procedure PopupFromControl(Menu:tPopupMenu; Control:tControl);
function CreateFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
function MakeFormWithContent(Content:TWinControl; BorderStyle:TFormBorderStyle; const Caption:string=''):TForm;
function FmtHint(): TFmtHintWindow;
procedure AddComboBoxHistory(CB: TComboBox; Text: string = sTextFromControl; MaxCount: Integer = 20);
function PopulateMenuWithFileList(Menu: TMenuItem; AfterItem, BeforeItem: TMenuItem;
  Template: TMenuItem; FolderImageIndex: Integer; const Path, Mask: string;
  FilesForMenuItems: TDictionary<Integer, string>): Integer;
function FitTextInWidth(const Text: string; Canvas: TCanvas; MaxWidth: Integer; DotsPosition: Double = 0.66): string;

procedure Register;

implementation

uses
  Vcl.Consts;

procedure Register;
begin
  RegisterComponents('DWF', [THintedImageProxy, TScrollBar64, TDropFileCatcher]);
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

procedure AddComboBoxHistory(CB: TComboBox; Text: string = sTextFromControl; MaxCount: Integer = 20);
// Add given text to dropdown list of ComboBox, limiting total dropdown count
var
  ControlText: string;
  n: Integer;
begin
  ControlText := CB.Text;
  if Text = sTextFromControl then
    Text := ControlText;
  n := CB.Items.IndexOf(Text);
  if n >= 0 then
    CB.Items.Move(n, 0)
  else
    CB.Items.Insert(0, Text);
  while CB.Items.Count > MaxCount do
    CB.Items.Delete(CB.Items.Count-1);
  // Workaround: when moving item equal to current input box text,
  // TComboBox clears it's contents
  CB.Text := ControlText;
end;

function PopulateMenuWithFileList(Menu: TMenuItem; AfterItem, BeforeItem: TMenuItem;
  Template: TMenuItem; FolderImageIndex: Integer; const Path, Mask: string;
  FilesForMenuItems: TDictionary<Integer, string>): Integer;
// Populate menu with list of files and folders in specified folder
var
  i1, i2, i, n: Integer;
  fl: TStringDynArray;
  mi: TMenuItem;
begin
  Result := 0;
  // Delete old items
  if AfterItem  <> nil then i1 := AfterItem.MenuIndex + 1
                       else i1 := 0;
  if BeforeItem <> nil then i2 := BeforeItem.MenuIndex - 1
                       else i2 := Menu.Count - 1;
  for i:=i2 downto i1 do
    Menu.Items[i].Free;

  if not System.SysUtils.DirectoryExists(Path) then Exit;
  n := i1;

  // Process subdirectories
  fl := TDirectory.GetDirectories(Path);
  for i:=0 to Length(fl)-1 do
  begin
    mi := TMenuItem.Create(Application);
    mi.Caption := ExtractFileName(fl[i]);
    mi.ImageIndex := FolderImageIndex;
    Menu.Insert(n, mi);
    Inc(n);
    Inc(Result, PopulateMenuWithFileList(mi, nil, nil, Template, FolderImageIndex, fl[i], Mask, FilesForMenuItems));
  end;

  // Create items for files
  fl := TDirectory.GetFiles(Path, Mask);
  for i:=0 to Length(fl)-1 do
  begin
    mi := TMenuItem.Create(Application);
    mi.Caption := ChangeFileExt(ExtractFileName(fl[i]), '');
    mi.ImageIndex := Template.ImageIndex;
    mi.OnClick := Template.OnClick;
    mi.Tag := FilesForMenuItems.Count;
    FilesForMenuItems.AddOrSetValue(mi.Tag, fl[i]);
    Menu.Insert(n, mi);
    Inc(n);
    Inc(Result);
  end;
end;

function FitTextInWidth(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer; DotsPosition: Double = 0.66): string;
// Approximately fit text in specified width (in pixels) by replacing part of text with "...".
// DotsPosition defines a position of "..." in resulting text (0.5 -> in the middle)
var
  w, chars: Integer;
begin
  w := Canvas.TextWidth(Text);
  if w <= MaxWidth then Exit(Text);
  // Approx. count of chars that fit in MaxWidth
  chars := Round(MaxWidth / (w / Length(Text)));
  Result := Copy(Text, Low(Text), Trunc(chars * DotsPosition)) +
            '...' +
            Copy(Text, Length(Text) - Trunc(chars * (1 - DotsPosition)) + 5, MaxInt);
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

procedure TDropFileCatcher.AfterConstruction;
begin
  inherited;
  Internal_Enable();
end;

procedure TDropFileCatcher.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
var
  i: Integer;
  APoint: TPoint;
begin
  if (Msg.message = WM_DROPFILES) and (FControl <> nil) and (Msg.hwnd = FControl.Handle) then
  begin
    if Assigned(FOnDropFiles) then
    begin
      fDropHandle := Msg.wParam;

      FFiles.Clear();
      for i := 0 to GetFileCount()-1 do
        FFiles.Add(GetFile(i));
      APoint := GetPoint();

      FOnDropFiles(Self, FControl, FFiles, APoint);
    end;
  end;
end;

constructor TDropFileCatcher.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  if AOwner is TWinControl then
    FControl := TWinControl(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    FFiles := TStringList.Create();
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnMessage := ApplicationEventsMessage;
  end;
end;

destructor TDropFileCatcher.Destroy;
begin
  Internal_Disable();
  FFiles.Free;
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

procedure TDropFileCatcher.Internal_Disable;
begin
  if (not (csDesigning in ComponentState)) and
     //(not (csLoading in ComponentState)) and
     (FControl <> nil) and
     (FControl.HandleAllocated) and
     (FEnabled) then
  begin
    DragFinish(fDropHandle);
    DragAcceptFiles(FControl.Handle, False);
  end;
end;

procedure TDropFileCatcher.Internal_Enable;
begin
  if (not (csDesigning in ComponentState)) and
     //(not (csLoading in ComponentState)) and
     (FControl <> nil) and
     (FControl.HandleAllocated) and
     (FEnabled) then
  begin
    DragAcceptFiles(FControl.Handle, True);
  end;
end;

procedure TDropFileCatcher.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FControl) and (Operation = opRemove) then
  begin
    FControl := nil;
  end;

end;

procedure TDropFileCatcher.SetControl(const Value: TWinControl);
begin
  if FControl <> Value then
  begin
    Internal_Disable();
    if Assigned(FControl) then FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if Assigned(FControl) then FControl.FreeNotification(Self);
    Internal_Enable();
  end;
end;

procedure TDropFileCatcher.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    Internal_Disable();
    FEnabled := Value;
    Internal_Enable();
  end;
end;

procedure TDropFileCatcher.SetOnDropFiles(const Value: TDropFilesEvent);
begin
  FOnDropFiles := Value;
end;

{ TScrollBar64 }

constructor TScrollBar64.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Height := GetSystemMetrics(SM_CYHSCROLL);
  TabStop := True;
  ControlStyle := [csFramed, csDoubleClicks, csOpaque];
  FKind := sbHorizontal;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  InternalRange := FMax;
  FSmallChange := 1;
  FLargeChange := 1;
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    ImeMode := imDisable;
end;

class constructor TScrollBar64.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TScrollBar64, TScrollBarStyleHook);
end;

class destructor TScrollBar64.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TScrollBar64, TScrollBarStyleHook);
end;

procedure TScrollBar64.CreateParams(var Params: TCreateParams);
const
  Kinds: array[TScrollBarKind] of DWORD = (SBS_HORZ, SBS_VERT);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'SCROLLBAR');
  Params.Style := Params.Style or Kinds[FKind];
  if FKind = sbVertical then
    if not UseRightToLeftAlignment then
      Params.Style := Params.Style or SBS_RIGHTALIGN
    else
      Params.Style := Params.Style or SBS_LEFTALIGN;
  if NotRightToLeft then
    FRTLFactor := 1
  else
    FRTLFactor := -1;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TScrollBar64.CreateWnd;
var
  ScrollInfo: TScrollInfo;
  LBounds: TRect;
begin
  // Windows' does not always create the window size we ask for, so we have
  //  insist sometimes.  Setting BoundsRect will only adjust the size if needed.
  LBounds := BoundsRect;
  inherited CreateWnd;
  BoundsRect := LBounds;

  SetScrollRange(Handle, SB_CTL, Param64to32(FMin), Param64to32(FMax), False);
{$IF DEFINED(CLR)}
  ScrollInfo.cbSize := Marshal.SizeOf(TypeOf(ScrollInfo));
{$ELSE}
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
{$ENDIF}
  ScrollInfo.nPage := Param64to32(FPageSize);
  ScrollInfo.fMask := SIF_PAGE;
  SetScrollInfo(Handle, SB_CTL, ScrollInfo, False);
  if NotRightToLeft then
    SetScrollPos(Handle, SB_CTL, Param64to32(FPosition), True)
  else
    SetScrollPos(Handle, SB_CTL, Param64to32(FMax - FPosition), True);
end;

function TScrollBar64.NotRightToLeft: Boolean;
begin
  Result := (not IsRightToLeft) or (FKind = sbVertical);
end;

function TScrollBar64.Param32to64(Value: Integer): Int64;
var
  d: Double;
begin
  d := Value * (FMax / InternalRange);
  if d > High(Int64) - 1 then Result := High(Int64) //Int64(High(Int64)) + 1  // Prevent invalid FP operation
                         else Result := Round(d);
end;

function TScrollBar64.Param64to32(Value: Int64): Integer;
begin
  Result := Round(Value * (InternalRange / FMax));
end;

procedure TScrollBar64.SetKind(Value: TScrollBarKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
    RecreateWnd;
  end;
end;

procedure TScrollBar64.SetParams(APosition, AMin, AMax: Int64);
var
  PrevValue: Int64;
  UpdatePageSize: Boolean;
begin
  if (AMax < AMin) or (AMax < FPageSize) then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax - FPageSize + 1 then APosition := AMax - FPageSize + 1;
  UpdatePageSize := False;
  if (FMin <> AMin) or (FMax <> AMax) then
  begin
    FMin := AMin;
    FMax := AMax;
    PrevValue := InternalRange;
    ChooseInternalRange();
    if InternalRange <> PrevValue then
      UpdatePageSize := True;
    if HandleAllocated then
      SetScrollRange(Handle, SB_CTL, Param64to32(AMin), Param64to32(AMax), FPosition = APosition);
  end;
  if FPosition <> APosition then
  begin
    FPosition := APosition;
    if HandleAllocated then
    begin
      if sfHandleMessages in StyleServices.Flags then
      begin
        if NotRightToLeft then
          SetScrollPos(Handle, SB_CTL, Param64to32(FPosition), False)
        else
          SetScrollPos(Handle, SB_CTL, Param64to32(FMax - FPosition), False);
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end
      else
        begin
          if NotRightToLeft then
            SetScrollPos(Handle, SB_CTL, Param64to32(FPosition), True)
          else
            SetScrollPos(Handle, SB_CTL, Param64to32(FMax - FPosition), True);
        end;
    end;
    Enabled := True;
    Change;
  end;
  if UpdatePageSize then
  begin
    PrevValue := FPageSize;
    FPageSize := 0;
    PageSize := PrevValue;
  end;
end;

procedure TScrollBar64.SetPosition(Value: Int64);
begin
  SetParams(Value, FMin, FMax);
end;

procedure TScrollBar64.SetPageSize(Value: Int64);
var
  ScrollInfo: TScrollInfo;
begin
  if (FPageSize = Value) or (Value > FMax) then exit;
  FPageSize := Value;
{$IF DEFINED(CLR)}
  ScrollInfo.cbSize := Marshal.SizeOf(TypeOf(ScrollInfo));
{$ELSE}
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
{$ENDIF}
  if Value = 0 then
   ScrollInfo.nPage := 0
  else
    ScrollInfo.nPage := System.Math.Max(Param64to32(Value), 1);
  ScrollInfo.fMask := SIF_PAGE;
  if HandleAllocated then
   if sfHandleMessages in StyleServices.Flags then
   begin
     SetScrollInfo(Handle, SB_CTL, ScrollInfo, False);
     RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
   end
    else
      SetScrollInfo(Handle, SB_CTL, ScrollInfo, True);
end;

procedure TScrollBar64.SetMin(Value: Int64);
begin
  SetParams(FPosition, Value, FMax);
end;

procedure TScrollBar64.SetMax(Value: Int64);
begin
  SetParams(FPosition, FMin, Value);
end;

function TScrollBar64.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.PositionLinkID then
    Result := True
  else if ID = TObserverMapping.ControlValueID then
    Result := True;
end;

procedure TScrollBar64.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TScrollBar64.ChooseInternalRange;
begin
  InternalRange := System.Math.Min(FMax, 32767);
end;

procedure TScrollBar64.Scroll(ScrollCode: TScrollCode; var ScrollPos: Int64);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TScrollBar64.DoScroll(var Message: TWMScroll);
var
  ScrollPos: Int64;
  NewPos: Int64;
  ScrollInfo: TScrollInfo;
begin
  with Message do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, Int64(FSmallChange) * FRTLFactor);
      scLineDown:
        Inc(NewPos, Int64(FSmallChange) * FRTLFactor);
      scPageUp:
        Dec(NewPos, Int64(FLargeChange) * FRTLFactor);
      scPageDown:
        Inc(NewPos, Int64(FLargeChange) * FRTLFactor);
      scPosition, scTrack:
        with ScrollInfo do
        begin
{$IF DEFINED(CLR)}
          cbSize := Marshal.SizeOf(TypeOf(ScrollInfo));
{$ELSE}
          cbSize := SizeOf(ScrollInfo);
{$ENDIF}
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SB_CTL, ScrollInfo);
          NewPos := Param32to64(nTrackPos);
          { We need to reverse the positioning because SetPosition below
            calls SetParams that reverses the position. This acts as a
            double negative. }
          if not NotRightToLeft then NewPos := FMax - NewPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax - FPageSize + 1;
    end;
    if NewPos < FMin then NewPos := FMin;
    if NewPos > FMax - FPageSize + 1 then NewPos := FMax - FPageSize + 1;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
    SetPosition(ScrollPos);
  end;
end;

procedure TScrollBar64.CNHScroll(var Message: TWMHScroll);
begin
  DoScroll(Message);
  if Observers.IsObserving(TObserverMapping.PositionLinkID) then
    TLinkObservers.PositionLinkPosChanged(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
  begin
    TLinkObservers.ControlValueModified(Observers);
    TLinkObservers.ControlValueUpdate(Observers);
  end;
end;

procedure TScrollBar64.CNVScroll(var Message: TWMVScroll);
begin
  DoScroll(Message);
  if Observers.IsObserving(TObserverMapping.PositionLinkID) then
    TLinkObservers.PositionLinkPosChanged(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
  begin
    TLinkObservers.ControlValueModified(Observers);
    TLinkObservers.ControlValueUpdate(Observers);
  end;
end;

procedure TScrollBar64.CNCtlColorScrollBar(var Message: TMessage);
begin
  with Message do
    CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam);
end;

procedure TScrollBar64.WMPaint(var Message: TWMPaint);
begin
  if DoubleBuffered then
    DefaultHandler(Message)
  else
    inherited;
end;

procedure TScrollBar64.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  DefaultHandler(Message);
end;

end.
