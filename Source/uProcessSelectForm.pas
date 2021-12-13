{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uProcessSelectForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, TlHelp32, Generics.Collections,
  StrUtils, Vcl.Buttons

  {,uDebugUtils} ;

type
  TSelectionFrameForm = class(TForm)
  // Window that works as semi-transparent frame over given area of screen
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    Shape: TShape;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    destructor Destroy(); override;
  end;

  TProcessInfoRec = record
    PID: Cardinal;
    Name, CmdLine: string;
    Is32bit: Boolean;
  end;
  TProcessInfoList = TList<TProcessInfoRec>;

  TProcessSelectForm = class(TForm)
    ListView1: TListView;
    BtnOpen: TButton;
    BtnCancel: TButton;
    Label1: TLabel;
    EditFilter: TComboBox;
    Panel1: TPanel;
    Image1: TImage;
    LblDragPointer: TLabel;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    BtnClearFilter: TSpeedButton;
    BtnRefreshList: TSpeedButton;
    CBOpenRegionsFrame: TCheckBox;
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditFilterChange(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure BtnClearFilterClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure BtnRefreshListClick(Sender: TObject);
  private
    { Private declarations }
    FrameForm: TSelectionFrameForm;
    FDraggingPointer: Boolean;
    ProcList: TProcessInfoList;
    FilterText: string;
    procedure SetDraggingPointer(const Value: Boolean);
    property DraggingPointer: Boolean read FDraggingPointer write SetDraggingPointer;
    function GetProcessInfo(pid: Cardinal; var CommandLine: string; var Is32bit: Boolean): Boolean;
    procedure GetProcessList();
  public
    { Public declarations }
    procedure ShowProcessList();
    procedure SelectProcessInList(pid: Cardinal);
    function SelectedPID(): string;
    function SelectedName(): string;
  end;

var
  ProcessSelectForm: TProcessSelectForm;

implementation

{$R *.dfm}

uses uMainForm;

procedure TProcessSelectForm.BtnClearFilterClick(Sender: TObject);
begin
  EditFilter.Text := '';
  EditFilterChange(Sender);
end;

procedure TProcessSelectForm.BtnRefreshListClick(Sender: TObject);
begin
  GetProcessList();
  ShowProcessList();
end;

procedure TProcessSelectForm.EditFilterChange(Sender: TObject);
begin
  FilterText := Trim(EditFilter.Text);
  BtnClearFilter.Visible := (EditFilter.Text <> '');
  ShowProcessList();
  if ListView1.Items.Count = 1 then
    ListView1.ItemIndex := 0;
end;

procedure TProcessSelectForm.FormCreate(Sender: TObject);
begin
  FrameForm := TSelectionFrameForm.CreateNew(Self);
  ProcList := TProcessInfoList.Create();
end;

procedure TProcessSelectForm.FormDestroy(Sender: TObject);
begin
  ProcList.Free;
end;

procedure TProcessSelectForm.FormShow(Sender: TObject);
begin
  GetProcessList();
  ShowProcessList();
end;

type
  PPROCESS_BASIC_INFORMATION = ^_PROCESS_BASIC_INFORMATION;
  _PROCESS_BASIC_INFORMATION = packed record
    Reserved1: PVOID;
    PebBaseAddress: pointer;
    Reserved2: array[0..1] of PVOID;
    UniqueProcessId: ULONG_PTR;
    Reserved3: PVOID;
  end;

  // Records are 8-byte aligned

  _UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWideChar;
  end;

  PRTL_USER_PROCESS_PARAMETERS = ^_RTL_USER_PROCESS_PARAMETERS;
  _RTL_USER_PROCESS_PARAMETERS = record
    Reserved1: array[0..15] of BYTE;
    Reserved2: array[0..9] of PVOID;
    ImagePathName: _UNICODE_STRING;
    CommandLine: _UNICODE_STRING;
  end;

  _PEB = record
    Reserved1: array[0..1] of BYTE;
    BeingDebugged: BYTE;
    Reserved2: BYTE;
    Reserved3: array[0..1] of PVOID;
    Ldr: Pointer; //PPEB_LDR_DATA;
    ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
    Reserved4: array[0..2] of PVOID;
    AtlThunkSListPtr: PVOID;
    Reserved5: PVOID;
    Reserved6: ULONG;
    Reserved7: PVOID;
    Reserved8: ULONG;
    AtlThunkSListPtr32: ULONG;
    Reserved9: array[0..44] of PVOID;
    Reserved10: array[0..95] of BYTE;
    PostProcessInitRoutine: Pointer; //PPS_POST_PROCESS_INIT_ROUTINE;
    Reserved11: array[0..127] of BYTE;
    Reserved12: PVOID;
    SessionId: ULONG;
  end;

function NtQueryInformationProcess(ProcessHandle:THANDLE;
  ProcessInformationClass:DWORD;
  ProcessInformation:pointer;
  ProcessInformationLength:ULONG;
  ReturnLength:PULONG): NTStatus; stdcall; external 'ntdll.dll';

function TProcessSelectForm.GetProcessInfo(pid: Cardinal; var CommandLine: string; var Is32bit: Boolean): Boolean;
var
  hProcess: THandle;
  pbi: PPROCESS_BASIC_INFORMATION;
  peb: _PEB;
  ProcParam: _RTL_USER_PROCESS_PARAMETERS;
  status: NTStatus;
  BytesRead: NativeUInt;
  Ptr: Pointer;
begin
  CommandLine := '';
  Is32bit := False;
  pbi := nil;

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ{ or PROCESS_TERMINATE}, False, pid);
  if hProcess = 0 then
    Exit(False);

  try
    // Is process 32 bit?
    status := NtQueryInformationProcess(hProcess, {ProcessWow64Information}26, @Ptr, sizeof(Ptr), @BytesRead);
    if status = 0 then
      Is32bit := (Ptr <> nil);

    // Get command line
    New(pbi);  // Must be aligned
    status := NtQueryInformationProcess(hProcess, {ProcessBasicInformation}0, pbi, sizeof(pbi^), @BytesRead);
    if status <> 0 then
      raise Exception.Create('NtQueryInformationProcess returned ' + IntToStr(status));

    Win32Check(
      ReadProcessMemory(hProcess, pbi.PebBaseAddress, @peb, SizeOf(peb), BytesRead));

    Win32Check(
      ReadProcessMemory(hProcess, peb.ProcessParameters, @ProcParam, sizeof(ProcParam), BytesRead));

    SetLength(CommandLine, ProcParam.CommandLine.Length div SizeOf(Char));
    Win32Check(
      ReadProcessMemory(hProcess, ProcParam.CommandLine.Buffer, @CommandLine[1], ProcParam.CommandLine.Length, BytesRead));

  finally
    if pbi <> nil then Dispose(pbi);

    CloseHandle(hProcess);
  end;
  Result := True;
end;

procedure TProcessSelectForm.GetProcessList;
var
  vSnapshot: THandle;              // to store the tool help snapshot handle
  vProcessEntry: TProcessEntry32;  // temporarily store process information
  Rec: TProcessInfoRec;
begin
  // Erase all stored TProcess instances
  ProcList.Clear;

  // Get the snapshot of the current state
  vSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    // setting up the structure size information
    vProcessEntry.dwSize := SizeOf(TProcessEntry32);

    // make call to get first process information
    if not Process32First(vSnapshot, vProcessEntry) then
      RaiseLastOSError();

    // Loop to read and store the process information
    repeat
      Rec.PID := vProcessEntry.th32ProcessID;
      Rec.Name := vProcessEntry.szExeFile;
      try
        GetProcessInfo(Rec.PID, Rec.CmdLine, Rec.Is32bit);
      except
      end;

      // add new instance of TProcess storing the supplied process information
      ProcList.Add(Rec);

      // try to retrieve next process information. When this fails, the loop
      // will be stopped
    until not Process32Next(vSnapshot, vProcessEntry);

  finally
    // clean up the snapshot
    CloseHandle(vSnapshot);
  end;
end;

procedure TProcessSelectForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  BtnOpen.Enabled := (ListView1.Selected <> nil);
end;

procedure TProcessSelectForm.ListView1DblClick(Sender: TObject);
begin
  if BtnOpen.Enabled then
    BtnOpen.Click();
end;

procedure TProcessSelectForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    DraggingPointer := True;
end;

procedure TProcessSelectForm.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    DraggingPointer := False;
end;

function TProcessSelectForm.SelectedName: string;
begin
  if ListView1.Selected <> nil then
    Result := ListView1.Selected.Caption
  else
    Result := '';
end;

function TProcessSelectForm.SelectedPID: string;
begin
  if ListView1.Selected <> nil then
    Result := ListView1.Selected.SubItems[0]
  else
    Result := '';
end;

procedure TProcessSelectForm.SelectProcessInList(pid: Cardinal);
var
  i: Integer;
begin
  for i:=0 to ListView1.Items.Count-1 do
    if ListView1.Items[i].SubItems[0].ToInt64() = pid then
    begin
      ListView1.Selected := ListView1.Items[i];
      ListView1.Selected.MakeVisible(False);
      Exit;
    end;
  ListView1.Selected := nil;
end;

procedure TProcessSelectForm.SetDraggingPointer(const Value: Boolean);
begin
  if FDraggingPointer <> Value then
  begin
    FDraggingPointer := Value;
    if FDraggingPointer then
    begin
      EditFilter.Text := '';
      EditFilterChange(nil);
      LblDragPointer.Caption := 'Point to a window...';
      MainForm.Hide();
      Timer1.Enabled := True;
    end
    else
    begin
      LblDragPointer.Caption := 'Drag pointer to select process by window';
      FrameForm.Hide();
      MainForm.Show();
      Timer1.Enabled := False;
    end;
  end;
end;

procedure TProcessSelectForm.ShowProcessList;
var
  i: Integer;
  li: TListItem;
begin
  ListView1.Items.BeginUpdate();
  try
    ListView1.Clear();
    for i:=0 to ProcList.Count-1 do
    begin
      if (FilterText<>'') and (not ContainsText(ProcList[i].Name, FilterText)) then Continue;
      li := ListView1.Items.Add();
      li.Caption := ProcList[i].Name + IfThen(ProcList[i].Is32bit, '  (32bit)', '');
      li.SubItems.Add(IntToStr(ProcList[i].PID));
      li.SubItems.Add(ProcList[i].CmdLine);
    end;
  finally
    ListView1.Items.EndUpdate();
  end;
end;

procedure TProcessSelectForm.Timer1Timer(Sender: TObject);
var
  p: TPoint;
  h, h2: THandle;
  R: TRect;
  pid, pid2: Cardinal;
begin
  if not Visible then
  begin
    DraggingPointer := False;
    Exit;
  end;

  if DraggingPointer then
  begin
    GetCursorPos(p);
    h := WindowFromPoint(p);
    GetWindowThreadProcessId(h, pid);

    // Select given process in our list
    SelectProcessInList(pid);

    // Find top-level parent of selected window
    repeat
      h2 := GetParent(h);
      if h2 = 0 then Break;
      GetWindowThreadProcessId(h2, pid2);
      if pid2 <> pid then Break;  // I'm not sure is it possible
      h := h2;
    until False;

    // Hightlight window
    GetWindowRect(h, R);
    FrameForm.SetBounds(R.Left, R.Top, R.Width, R.Height);
    FrameForm.Visible := True;

//    Caption := Screen.FocusedForm.ClassName;
  end;
end;

{ TSelectionFrameForm }

constructor TSelectionFrameForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
begin
  inherited;
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  AlphaBlend := True;
  AlphaBlendValue := 200;
  TransparentColor := True;
  TransparentColorValue := clWhite;

  Shape := TShape.Create(Self);
  Shape.Align := alClient;
  Shape.Pen.Width := 16;
  Shape.Pen.Color := clFuchsia;
  Shape.Brush.Color := TransparentColorValue;
  Shape.Parent := Self;
end;

destructor TSelectionFrameForm.Destroy;
begin

  inherited;
end;

procedure TSelectionFrameForm.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_NCHITTEST) then
    Message.Result := IntPtr(HTTRANSPARENT)
  else
    inherited;
end;

end.
