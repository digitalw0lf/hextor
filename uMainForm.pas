unit uMainForm;

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
//{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, ColoredPanel, Vcl.Menus,
  System.Math, Generics.Collections, Clipbrd, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.Types, System.ImageList,
  Vcl.ImgList,

  uUtil, uLargeStr, uEditorPane, uLogFile{, uPathCompressTest};

const
  KByte = 1024;
  MByte = 1024*1024;
  GByte = 1024*1024*1024;

  HexCharsSet: TSysCharSet = ['0'..'9', 'A'..'F', 'a'..'f'];

type
  TFilePointer = Int64;

  TCachedRegion = class
    Addr: TFilePointer;
    Data: TBytes;
    function Size(): TFilePointer;
  end;

  TCachedRegionsList = TObjectList<TCachedRegion>;

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    ToolBar1: TToolBar;
    OpenDialog1: TOpenDialog;
    MainPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    VertScrollBar: TScrollBar;
    PaneLnNum: TEditorPane;
    PaneHex: TEditorPane;
    PaneText: TEditorPane;
    est1: TMenuItem;
    Copyas6Nwords1: TMenuItem;
    N1: TMenuItem;
    Decompress1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ActionList1: TActionList;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    ActionSetColumnsCount: TAction;
    View1: TMenuItem;
    Columnscount1: TMenuItem;
    N81: TMenuItem;
    N161: TMenuItem;
    N321: TMenuItem;
    Bywindowwidth1: TMenuItem;
    Regions1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ActionCut: TAction;
    ActionCopy: TAction;
    ActionPaste: TAction;
    ActionCopyAs: TAction;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Copyas1: TMenuItem;
    Paste1: TMenuItem;
    ActionSelectAll: TAction;
    Selectall1: TMenuItem;
    ActionGoToStart: TAction;
    ActionGoToEnd: TAction;
    ImageList16: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure VertScrollBarChange(Sender: TObject);
    procedure Copyas6Nwords1Click(Sender: TObject);
    procedure Decompress1Click(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure Regions1Click(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ActionCopyExecute(Sender: TObject);
    procedure PaneHexEnter(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionGoToStartExecute(Sender: TObject);
    procedure ActionGoToEndExecute(Sender: TObject);
    procedure PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaneHexKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaneHexKeyPress(Sender: TObject; var Key: Char);
    procedure PaneTextKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FCaretPos: TFilePointer;
    ByteColumnsOption: Integer;  // -1 - auto
    SelDragStart, SelDragEnd: TFilePointer;
    WasLeftMouseDown: Boolean;
    FCaretInByte: Integer;
    procedure SetCaretPos(const Value: TFilePointer);
    procedure UpdatePanesCarets();
    procedure PaneMouseMove(Sender: TObject; IsMouseDown: Boolean; Shift: TShiftState; X, Y: Integer);
    procedure CheckEnabledActions();
    procedure NewFileOpened();
    procedure SetCaretInByte(const Value: Integer);
  public
    { Public declarations }
    ByteColumns: Integer;
    TopVisibleRow: TFilePointer;
    FileName: string;
    FileStream: TFileStream;
    FileData: TBytes;
    FileDataLoaded: Boolean;
    CachedRegions: TCachedRegionsList;
    SelStart, SelLength: TFilePointer;
    procedure OpenFile(const AFileName: string);
    procedure SaveFile(const AFileName: string);
    function GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean): TBytes;
    function GetOrigFileSize(): TFilePointer;
    function GetFileSize(): TFilePointer;
    procedure UpdatePanes();
    procedure UpdateScrollBar();
    function GetVisibleRowsCount(): Integer;
    function FirstVisibleAddr(): TFilePointer;
    procedure ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
    function GetOverlappingRegions(Addr, Size: TFilePointer; var Index1, Index2: Integer): TCachedRegionsList; overload;
    function GetOverlappingRegions(Addr, Size: TFilePointer): TCachedRegionsList; overload;
    function FindCachedRegion(Addr: TFilePointer; var Index: Integer): Boolean;
    function CreateCachedRegion(Addr, Size: TFilePointer): TCachedRegion;
    function StartChanges(Addr, Size: TFilePointer): TCachedRegion;
    property CaretPos: TFilePointer read FCaretPos write SetCaretPos;
    property CaretInByte: Integer read FCaretInByte write SetCaretInByte;
    procedure SetSelection(AStart, AEnd: TFilePointer);
    procedure BeginUpdatePanes();
    procedure EndUpdatePanes();
  end;

function DivRoundUp(A, B: Int64): Int64; inline;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function DivRoundUp(A, B: Int64): Int64; inline;
begin
  Result := (A-1) div B + 1;
end;

{ TMainForm }

procedure TMainForm.ActionCopyExecute(Sender: TObject);
var
  Buf: TBytes;
  s: string;
begin
  if SelLength > 100*MByte then
    if Application.MessageBox(PChar('Try to copy '+IntToStr(SelLength)+' bytes to system clipboard?'), PChar('Copy'), MB_YESNO) <> IDYES then Exit;
  Buf := GetEditedData(SelStart, SelLength, False);
  if ActiveControl=PaneHex then
    s := Data2Hex(Buf, True)
  else
    s := MakeStr(Buf);
  Clipboard.AsText := s;
end;

procedure TMainForm.ActionGoToEndExecute(Sender: TObject);
begin
  BeginUpdatePanes();
  try
    SetSelection(0, -1);
    CaretPos := GetFileSize();
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.ActionGoToStartExecute(Sender: TObject);
begin
  BeginUpdatePanes();
  try
    SetSelection(0, -1);
    CaretPos := 0;
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
  FileName := '';
  FreeAndNil(FileStream);
  FileDataLoaded := False;

  NewFileOpened();
end;

procedure TMainForm.ActionOpenExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute() then Exit;
  OpenFile(OpenDialog1.FileName);
end;

procedure TMainForm.ActionPasteExecute(Sender: TObject);
var
  s: string;
  Buf: TBytes;
begin
  s := Clipboard.AsText;
  if ActiveControl=PaneHex then
    Buf := HexToData(s)
  else
    Buf := Str2Bytes(AnsiString(s));
  if Length(Buf)=0 then Exit;

  ChangeBytes(CaretPos, Buf);
  CaretPos := CaretPos + Length(Buf);
  UpdatePanes();
end;

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.FileName := FileName;
  if not SaveDialog1.Execute() then Exit;
  SaveFile(SaveDialog1.FileName);
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  SaveFile(FileName);
end;

procedure TMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  SetSelection(0, GetFileSize()-1);
  UpdatePanes();
end;

procedure TMainForm.BeginUpdatePanes;
begin
  PaneLnNum.BeginUpdate();
  PaneHex.BeginUpdate();
  PaneText.BeginUpdate();
end;

procedure TMainForm.ChangeBytes(Addr: TFilePointer; const Value: array of Byte);
var
  Region: TCachedRegion;
begin
  Region := StartChanges(Addr, Length(Value));
  Move(Value[0], Region.Data[Addr-Region.Addr], Length(Value));
end;

procedure TMainForm.CheckEnabledActions;
var
  FocusInEditor: Boolean;
begin
  FocusInEditor := (ActiveControl=PaneHex) or (ActiveControl=PaneText);

  ActionSave.Enabled := (FileName<>'');

  ActionCopy.Enabled := (FocusInEditor) and (SelLength > 0);
  ActionCut.Enabled := ActionCopy.Enabled;

  ActionSelectAll.Enabled := FocusInEditor;
end;

procedure TMainForm.Copyas6Nwords1Click(Sender: TObject);
var
  i: Integer;
  s: TStringBuilder;
begin
  s := TStringBuilder.Create();
  for i:=0 to Length(FileData) div 2-1 do
  begin
    s.Append(IntToStr(pSmallInt(@FileData[i*2])^)+#9);
    if (i+1) mod 6=0 then
      s.Append(#13#10);
  end;
  Clipboard.AsText := s.ToString;
  s.Free;

end;

function TMainForm.CreateCachedRegion(Addr, Size: TFilePointer): TCachedRegion;
var
  n: Integer;
begin
  Result := TCachedRegion.Create();
  Result.Addr := Addr;
  Result.Data := GetEditedData(Addr, Size, True);
  if FindCachedRegion(Addr, n) then
    raise Exception.Create('Trying to create overlapping region');
  CachedRegions.Insert(n, Result);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  bWriteLogFile := True;

  ByteColumns := 16;
  CachedRegions := TObjectList<TCachedRegion>.Create(True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CachedRegions.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  UpdateScrollBar();
  UpdatePanes();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  OpenFile('d:\DWF\Delphi\Tools\DWHex\Test\Unit1.pas');
end;

function TMainForm.GetEditedData(Addr, Size: TFilePointer; ZerosBeyondEoF: Boolean): TBytes;
var
  OrigSize, CurrSize: TFilePointer;
  ReadSize, ReturnSize: Integer;
  i: Integer;
  oPos, oSize: TFilePointer;
  Regions: TCachedRegionsList;
begin
  OrigSize := GetOrigFileSize();
  CurrSize := GetFileSize();
  if (Addr<0) or (Addr>CurrSize) then Exit(nil);
  if (not ZerosBeyondEoF) and (Addr>=CurrSize) then Exit(nil);

  if ZerosBeyondEoF then
    ReturnSize := Size
  else
  begin
    ReturnSize := Min(Size, CurrSize-Addr);
  end;
  SetLength(Result, ReturnSize);


  // Original data
  ReadSize := Size;
  if Addr + ReadSize > OrigSize then
    ReadSize := OrigSize - Addr;
  if ReadSize > 0 then
  begin
    if FileDataLoaded then
      //Result := Copy(FileData, Addr, ReadSize)
      Move(FileData[Addr], Result[0], ReadSize)
    else
    if FileStream<>nil then
    begin
      FileStream.Position := Addr;
      FileStream.ReadBuffer(Result, ReadSize);
    end;
  end;

  // Edited data
  Regions := GetOverlappingRegions(Addr, Size);
  for i:=0 to Regions.Count-1 do
  begin
    oPos := Max(Addr, Regions[i].Addr);
    oSize := Min(Regions[i].Addr+Regions[i].Size, Addr+Size) - oPos;
    Move(Regions[i].Data[oPos-Regions[i].Addr], Result[oPos-Addr], oSize);
  end;

  if (ZerosBeyondEoF) and (Addr+ReturnSize > CurrSize) then
  // Fill with zeros beyond end of file
  begin
    ZeroMemory(@Result[CurrSize-Addr], Addr+ReturnSize-CurrSize);
  end;
end;

function TMainForm.GetFileSize: TFilePointer;
// Edited file size (including appended region)
var
  p: TFilePointer;
begin
  Result := GetOrigFileSize();
  if (CachedRegions.Count>0) then
  with CachedRegions.Last do
  begin
    p := Addr + Size;
    if p > Result then
      Result := p;
  end;
end;

function TMainForm.GetOrigFileSize: TFilePointer;
// Original file size
begin
  if FileDataLoaded then
    Result := Length(FileData)
  else
  if FileStream<>nil then
    Result := FileStream.Size
  else
    Result := 0;
end;

function TMainForm.GetOverlappingRegions(Addr,
  Size: TFilePointer): TCachedRegionsList;
var
  Index1, Index2: Integer;
begin
  Result := GetOverlappingRegions(Addr, Size, Index1, Index2);
end;

function TMainForm.FindCachedRegion(Addr: TFilePointer; var Index: Integer): Boolean;
var
  i: Integer;
begin
  for i:=0 to CachedRegions.Count-1 do
  begin
    if CachedRegions[i].Addr>Addr then
    begin
      Index := i;
      Exit(False);
    end;
    if CachedRegions[i].Addr+CachedRegions[i].Size>Addr then
    begin
      Index := i;
      Exit(True);
    end;
  end;
  Index := CachedRegions.Count;
  Result := False;
end;

function TMainForm.FirstVisibleAddr: TFilePointer;
begin
  Result := TopVisibleRow * ByteColumns;
end;

function TMainForm.GetOverlappingRegions(Addr,
  Size: TFilePointer; var Index1, Index2: Integer): TCachedRegionsList;
var
  i: Integer;
begin
  Result := TCachedRegionsList.Create(False);
  Index1 := MaxInt;
  Index2 := -1;
  for i:=0 to CachedRegions.Count-1 do
    if (CachedRegions[i].Addr<Addr+Size) and (CachedRegions[i].Addr+CachedRegions[i].Size>Addr) then
    begin
      Result.Add(CachedRegions[i]);
      if i < Index1 then Index1 := i;
      if i > Index2 then Index2 := i;
    end;
end;

function TMainForm.GetVisibleRowsCount: Integer;
begin
  Result := PaneHex.Height div PaneHex.TextHeight();
end;

procedure TMainForm.N1Click(Sender: TObject);
begin
//  TestCompress();
end;

procedure TMainForm.NewFileOpened;
var
  s: string;
begin
  s := 'DWHex - ';
  if FileName <> '' then
    s := s + FileName
  else
    s := s + '(unnamed)';
  Self.Caption := s;

  CachedRegions.Clear();

  UpdateScrollBar();
  UpdatePanes();

  CheckEnabledActions();
end;

procedure TMainForm.OpenFile(const AFileName: string);
begin
  FileName := AFileName;

  //LoadEntireFile(OpenDialog1.FileName, FileData);
  FreeAndNil(FileStream);
  FileDataLoaded := False;
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

  NewFileOpened();
end;

procedure TMainForm.PaneHexEnter(Sender: TObject);
begin
  CheckEnabledActions();
end;

procedure TMainForm.PaneHexKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  Caption := Caption + ' ' + IntToStr(Key);
  case Key of
    VK_END:
      if ssCtrl in Shift then
        ActionGoToEnd.Execute()
      else
      begin

      end;

    VK_HOME:
      if ssCtrl in Shift then
        ActionGoToStart.Execute()
      else
      begin

      end;


  end;


end;

procedure TMainForm.PaneHexKeyPress(Sender: TObject; var Key: Char);
var
  APos: TFilePointer;
  Buf: TBytes;
  x: Byte;
  Digit: Integer;
begin
  if CharInSet(Key, HexCharsSet) then
  begin
    BeginUpdatePanes();
    try
      APos := CaretPos;
      Buf := GetEditedData(APos, 1, True);
      if Length(Buf)<>1 then Exit;
      x := Buf[0];
      Digit := StrToInt('$'+Key);
      if CaretInByte=0 then
        x := (x and $0F) or (Digit shl 4)
      else
        x := (x and $F0) or (Digit);
      ChangeBytes(CaretPos, [x]);

      if CaretInByte=0 then
        CaretInByte := 1
      else
      begin
        CaretPos := CaretPos + 1;
        CaretInByte := 0;
      end;
      UpdatePanes();
    finally
      EndUpdatePanes();
    end;
  end;
end;

procedure TMainForm.PaneHexMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ActiveControl := (Sender as TWinControl);
  if (Sender <> PaneLnNum) and (Button = mbLeft) then
  begin
    WasLeftMouseDown := True;
    PaneMouseMove(Sender, True, Shift+[ssLeft], X, Y);
  end;
end;

procedure TMainForm.PaneHexMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Sender <> PaneLnNum) and (ssLeft in Shift) and (WasLeftMouseDown) then
    PaneMouseMove(Sender, False, Shift, X, Y);
end;

procedure TMainForm.PaneHexMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    WasLeftMouseDown := False;
end;

procedure TMainForm.PaneMouseMove(Sender: TObject; IsMouseDown: Boolean;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  ACaretPos: TFilePointer;
  ACaretInByte: Integer;
begin
  BeginUpdatePanes();
  try
    if (Sender as TEditorPane).GetCharAt(X, Y, p) then
    begin
      if Sender = PaneHex then
      begin
        ACaretInByte := BoundValue(p.X mod 3, 0, 1);
        p.X := p.X div 3;
      end
      else
        ACaretInByte := 0;
      ACaretPos := FirstVisibleAddr() + (p.Y*ByteColumns + p.X);
      if (IsMouseDown) and (not (ssShift in Shift)) then
      begin
        SelDragStart := ACaretPos;
        SelDragEnd := -1;
      end
      else
        SelDragEnd := ACaretPos;
      SetSelection(SelDragStart, SelDragEnd);
      CaretPos := ACaretPos;
      CaretInByte := ACaretInByte;
    end;
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.PaneTextKeyPress(Sender: TObject; var Key: Char);
begin
  BeginUpdatePanes();
  try
    Caption := Caption + ' ' + IntToStr(Ord(Key));
    ChangeBytes(CaretPos, [Byte(Key)]);
    CaretPos := CaretPos + 1;
    UpdatePanes();
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.Regions1Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '';
  for i:=0 to CachedRegions.Count-1 do
    s := s + IntToStr(CachedRegions[i].Addr)+' '+IntToStr(CachedRegions[i].Size)+' '+RemUnprintable(MakeStr(CachedRegions[i].Data))+#13#10;
  Application.MessageBox(PChar(s),'');
end;

procedure TMainForm.SaveFile(const AFileName: string);
var
  i: Integer;
  FS: TFileStream;
begin
  if (AFileName='') then Exit;

  FreeAndNil(FileStream);

  ForceDirectories(ExtractFilePath(AFileName));

  // If saving to another file, copy original contents first
  if not SameFileName(AFileName, FileName) then
  begin
    if FileDataLoaded then
    begin
      FS := TFileStream.Create(FileName, fmCreate);
      try
        FS.WriteBuffer(FileData, Length(FileData));
      finally
        FreeAndNil(FS);
      end;
    end
    else
    begin
      if (FileName<>'') then
        CopyFile(PChar(FileName), PChar(AFileName), False);
    end;
  end;

  if FileExists(AFileName) then
    FS := TFileStream.Create(AFileName, fmOpenReadWrite)
  else
    FS := TFileStream.Create(AFileName, fmCreate);
  try
    for i:=0 to CachedRegions.Count-1 do
    begin
      FS.Position := CachedRegions[i].Addr;
      FS.WriteBuffer(CachedRegions[i].Data, CachedRegions[i].Size());
    end;
  finally
    FreeAndNil(FS);
  end;
  CachedRegions.Clear();

  OpenFile(AFileName);
end;

procedure TMainForm.SetCaretInByte(const Value: Integer);
begin
  if Value<>FCaretInByte then
  begin
    Assert((Value>=0) and (Value<=1), 'SetCaretInByte: '+IntToStr(Value));
    FCaretInByte := Value;
    UpdatePanesCarets();
  end;
end;

procedure TMainForm.SetCaretPos(const Value: TFilePointer);
begin
  if Value <> FCaretPos then
  begin
    FCaretPos := Value;
    UpdatePanesCarets();
  end;
end;

procedure TMainForm.SetSelection(AStart, AEnd: TFilePointer);
begin
  if AEnd=-1 then
  begin
    SelStart := AStart;
    SelLength := 0;
  end
  else
  begin
    if AStart>AEnd then
      Swap8Bytes(AStart, AEnd);
    SelStart := AStart;
    SelLength := AEnd-AStart+1;
  end;
  UpdatePanes();
  CheckEnabledActions();
end;

function TMainForm.StartChanges(Addr, Size: TFilePointer): TCachedRegion;
// Find or create CachedRegion covering given range.
// May combine several old regions that are overlapped by range.
var
  i1, i2: Integer;
  Regions: TCachedRegionsList;
  p1, p2: TFilePointer;
  Data: TBytes;
begin
  Regions := GetOverlappingRegions(Addr-1, Size+2, i1, i2);
  try
    if Regions.Count = 0 then
    begin
      Result := CreateCachedRegion(Addr, Size);
    end
    else
    begin
      p1 := Min(Addr, Regions.First.Addr);
      p2 := Max(Addr+Size, Regions.Last.Addr+Regions.Last.Size);
      Data := GetEditedData(p1, p2-p1, True);
      Result := Regions[0];
      Result.Data := Data;
      Result.Addr := p1;
      if i2>i1 then
        CachedRegions.DeleteRange(i1+1, i2-i1);
    end;
  finally
    Regions.Free;
  end;
end;

procedure TMainForm.Decompress1Click(Sender: TObject);
begin
//  TestDecompress();
end;

procedure TMainForm.EndUpdatePanes;
begin
  PaneLnNum.EndUpdate();
  PaneHex.EndUpdate();
  PaneText.EndUpdate();
end;

procedure TMainForm.UpdatePanes;
var
  Data: TBytes;
  i: Integer;
  Rows: Integer;
  sb: TStringBuilder;
  Lines: TStringList;
  s: AnsiString;
  c: AnsiChar;
  FirstVisibleAddress: Int64;
  IncludesFileEnd: Boolean;
begin
  BeginUpdatePanes();
  try
    Rows := GetVisibleRowsCount();
    FirstVisibleAddress := FirstVisibleAddr();
    Data := GetEditedData(FirstVisibleAddress, Rows * ByteColumns, False);
    IncludesFileEnd := (Length(Data) < Rows * ByteColumns);
    sb := TStringBuilder.Create();
    Lines := TStringList.Create();

    // Line numbers
    for i:=0 to DivRoundUp(Length(Data), ByteColumns)-1 do
    begin
      //sb.Append(IntToHex(FirstVisibleAddress + i*ByteColumns, 8)+#13#10);
      Lines.Add(IntToHex(FirstVisibleAddress + i*ByteColumns, 8));
    end;
    //PaneLnNum.Text := sb.ToString;
    PaneLnNum.Lines.Assign(Lines);

    // Hex
  //  sb.Clear();
    Lines.Clear();
    for i:=0 to Length(Data)-1 do
    begin
      sb.Append(IntToHex(Data[i], 2)+' ');
      if ((i+1) mod ByteColumns)=0 then
      begin
        Lines.Add(sb.ToString());
        sb.Clear();
      end;
    end;
    if (sb.Length>0) or (IncludesFileEnd) then
      Lines.Add(sb.ToString());
    PaneHex.Lines.Assign(Lines);

    // Text
    //sb.Clear();
    s := '';
    Lines.Clear();
    for i:=0 to Length(Data)-1 do
    begin
      if (Data[i] < Ord(' ')) or (Data[i] = $98) then
        c := '.'
      else
        c := AnsiChar(Data[i]);
      //sb.Append(c);
      s := s + c;
      if ((i+1) mod ByteColumns)=0 then
        //sb.Append(#13#10);
      begin
        //Lines.Add(sb.ToString());
        //sb.Clear();
        Lines.Add(s);
        s := '';
      end;
    end;
  //  if (sb.Length>0) or (IncludesFileEnd) then
  //    Lines.Add(sb.ToString());
    if (s<>'') or (IncludesFileEnd) then
      Lines.Add(s);
    PaneText.Lines.Assign(Lines);

    sb.Free;
    Lines.Free;

    UpdatePanesCarets();
  finally
    EndUpdatePanes();
  end;
end;

procedure TMainForm.UpdatePanesCarets;
var
  cp: TPoint;
  p: TFilePointer;

  procedure Update(Pane: TEditorPane; CharsPerByte: Integer);
  var
    L, i: Integer;
    N0: TFilePointer;
  begin
    // Selection background
    N0 := FirstVisibleAddr();
    L := GetVisibleRowsCount()*ByteColumns*CharsPerByte;
    if Length(Pane.BgColors)<>L then
      SetLength(Pane.BgColors, L);
    for i:=0 to L-1 do
    begin
      if (N0+(i div CharsPerByte)>=SelStart) and (N0+(i div CharsPerByte)<SelStart+SelLength) then
        Pane.BgColors[i] := clHighlight
      else
        Pane.BgColors[i] := Pane.Color;
    end;

    // Caret position
    Pane.CaretPos := Point(cp.X*CharsPerByte + IfThen(CharsPerByte>1, CaretInByte, 0), cp.Y);
  end;

begin
  p := FCaretPos - FirstVisibleAddr();
  cp := Point(p mod ByteColumns, p div ByteColumns);
  Update(PaneHex, 3);
  Update(PaneText, 1);
end;

procedure TMainForm.UpdateScrollBar;
var
  FileRows: Int64;
  ScreenRows: Integer;
begin
  FileRows := DivRoundUp(GetFileSize()+1, ByteColumns);
  ScreenRows := GetVisibleRowsCount();
  if (FileRows <= ScreenRows) then
  begin
    VertScrollBar.PageSize := 0;
    VertScrollBar.Max := 0;
  end
  else
  begin
    VertScrollBar.Max := FileRows - 1;
    VertScrollBar.PageSize := ScreenRows;
  end;
end;

procedure TMainForm.VertScrollBarChange(Sender: TObject);
begin
  TopVisibleRow := VertScrollBar.Position;
  UpdatePanes();
end;

{ TCachedRegion }

function TCachedRegion.Size: TFilePointer;
begin
  Result := Length(Data);
end;

end.
