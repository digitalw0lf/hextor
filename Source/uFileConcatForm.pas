unit uFileConcatForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  System.Math, System.RegularExpressions, System.Types, Vcl.ExtCtrls, Vcl.Menus,

  uHextorTypes, uHextorGUI, HlpHashFactory, HlpIHash;

type
  TFileConcatForm = class(TForm)
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    Label3: TLabel;
    SpeedButton3: TSpeedButton;
    EditResultFileName: TComboBox;
    EditCRCFileName: TComboBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    OpenDialog1: TOpenDialog;
    DropFileCatcher1: TDropFileCatcher;
    BtnClearList: TSpeedButton;
    ImageProxy1: THintedImageProxy;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    Remove1: TMenuItem;
    Movetotop1: TMenuItem;
    Movetobottom1: TMenuItem;
    FilesList: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
      Control: TWinControl; Files: TStrings; DropPoint: TPoint);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnClearListClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure FilesListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Movetotop1Click(Sender: TObject);
    procedure Movetobottom1Click(Sender: TObject);
    procedure FilesListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    procedure AddFilesToList(Files: TStrings);
    procedure ClearFileList();
    procedure ConcatFiles(Files: TStrings; CRCFileName: string; ResultFileName: string);
    function SuggestOriginalFile(FileName: string): string;
  public
    { Public declarations }
  end;

var
  FileConcatForm: TFileConcatForm;

implementation

{$R *.dfm}

uses
  uMainForm, uFileSplitForm;

procedure TFileConcatForm.AddFilesToList(Files: TStrings);
var
  Temp: TStringList;
  s: string;
begin
  if Files.Count = 0 then Exit;

  Temp := TStringList.Create();
  Temp.CaseSensitive := False;
  Temp.AddStrings(Files);
  Temp.Sort();

  s := SuggestOriginalFile(Temp[0]);

  if s <> '' then
  begin
    EditResultFileName.Items.Text := s;
    EditResultFileName.ItemIndex := 0;

    s := ChangeFileExt(s, '.crc');
    if FileExists(s) then
    begin
      EditCRCFileName.Items.Text := s;
      EditCRCFileName.ItemIndex := 0;
    end;
  end;

  FilesList.Items.AddStrings(Temp);

  Temp.Free;
end;

procedure TFileConcatForm.BtnOkClick(Sender: TObject);
begin
  if FilesList.Items.Count = 0 then
    raise EInvalidUserInput.Create('File list is empty');
  if Trim(EditResultFileName.Text) = '' then
    raise EInvalidUserInput.Create('Specify target file name');

  ConcatFiles(FilesList.Items, Trim(EditCRCFileName.Text), Trim(EditResultFileName.Text));

  ClearFileList();
  ModalResult := mrOk;
end;

procedure TFileConcatForm.ClearFileList;
begin
  FilesList.Items.Clear();
  EditCRCFileName.Items.Clear();
  EditCRCFileName.Text := '';
  EditResultFileName.Items.Clear();
  EditResultFileName.Text := '';
end;

procedure TFileConcatForm.ConcatFiles(Files: TStrings; CRCFileName,
  ResultFileName: string);
const
  BlockSize = 10 * MByte;
var
  i, PortionSize: Integer;
  SourceStream, DestStream: TFileStream;
  TotalSize, PartSize, Ptr, PartPtr: Int64;
  Buf: TBytes;
  CRCText: TStringList;
  Saved_Size: Int64;
  Saved_CRC: string;
  Hash: IHash;
  s: string;
begin
  FileSplitForm.AskOverwriteIfExists(ResultFileName);
  SetLength(Buf, BlockSize);

  Progress.TaskStart(Self);  // Overall progress
  try
    Progress.TaskStart(Self, IfThen(CRCFileName <> '', 0.7, 1.0));  // Progress for concatenation
    try
      TotalSize := 0;
      for i := 0 to Files.Count-1 do
        TotalSize := TotalSize + GetFileSizeNoOpen(Files[i]);

      ForceDirectories(ExtractFilePath(ResultFileName));
      DestStream := TFileStream.Create(ResultFileName, fmCreate);
      try
        Ptr := 0;

        for i := 0 to Files.Count-1 do  // Files
        begin
          SourceStream := TFileStream.Create(Files[i], fmOpenRead or fmShareDenyWrite);
          try
            PartSize := SourceStream.Size;
            PartPtr := 0;
            while PartPtr < PartSize do  // Portions inside file
            begin
              PortionSize := Min(BlockSize, PartSize - PartPtr);
              SourceStream.ReadBuffer(Buf[0], PortionSize);
              DestStream.WriteBuffer(Buf[0], PortionSize);
              Inc(PartPtr, PortionSize);
              Inc(Ptr, PortionSize);
              Progress.Show(Ptr, TotalSize);
            end;
          finally
            SourceStream.Free;
          end;
        end;
      finally
        DestStream.Free;
      end;
    finally
      Progress.TaskEnd();
    end;

    // Check CRC
    if CRCFileName <> '' then
    begin
      if GetFileSizeNoOpen(CRCFileName) > 10000 then
        raise Exception.Create('Invalid CRC file format');
      CRCText := TStringList.Create();
      try
        CRCText.LoadFromFile(CRCFileName);
        Saved_Size := StrToInt64Def(CRCText.Values['size'], -1);
        Saved_CRC := CRCText.Values['crc32'];
      finally
        CRCText.Free;
      end;
      if Length(Saved_CRC) <> 8 then
        raise Exception.Create('Invalid CRC file format');

      Hash := THashFactory.TChecksum.TCRC.CreateCRC32_PKZIP();
      Hash.Initialize();

      Progress.TaskStart(Self, 0.3);  // Progress for CRC
      try
        SourceStream := TFileStream.Create(ResultFileName, fmOpenRead or fmShareDenyWrite);
        try
          TotalSize := SourceStream.Size;
          Ptr := 0;
          while Ptr < TotalSize do
          begin
            PortionSize := Min(BlockSize, TotalSize - Ptr);
            SourceStream.ReadBuffer(Buf[0], PortionSize);
            Hash.TransformBytes(Buf, 0, PortionSize);
            Inc(Ptr, PortionSize);
            Progress.Show(Ptr, TotalSize, 'Verifying CRC');
          end;
        finally
          SourceStream.Free;
        end;
      finally
        Progress.TaskEnd();
      end;

      s := '';
      if (Saved_Size >= 0) and (Saved_Size <> TotalSize) then
        s := s + 'File size does not match the one stored in CRC file!' + sLineBreak + sLineBreak;
      if Saved_CRC <> Hash.TransformFinal().ToString() then
        s := s + 'CRC check FAILED!' + sLineBreak + sLineBreak;
      if s = '' then
        Application.MessageBox('CRC check passed successfully', 'Done', MB_OK)
      else
        Application.MessageBox(PChar(s), 'Done', MB_OK or MB_ICONWARNING);
    end;
  finally
    Progress.TaskEnd();
  end;
end;

procedure TFileConcatForm.DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
  Control: TWinControl; Files: TStrings; DropPoint: TPoint);
begin
  AddFilesToList(Files);
end;

procedure TFileConcatForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TFileConcatForm.Movetobottom1Click(Sender: TObject);
begin
  if FilesList.ItemIndex >= 0 then
    FilesList.Items.Move(FilesList.ItemIndex, FilesList.Items.Count-1);
end;

procedure TFileConcatForm.Movetotop1Click(Sender: TObject);
begin
  if FilesList.ItemIndex >= 0 then
    FilesList.Items.Move(FilesList.ItemIndex, 0);
end;

procedure TFileConcatForm.FilesListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  SrcIndex, DestIndex: Integer;
begin
  Accept := (Sender = Source);
  SrcIndex := FilesList.ItemIndex;
  DestIndex := FilesList.ItemAtPos(Point(X, Y), False);

  if (SrcIndex >= 0) and (SrcIndex < FilesList.Items.Count) and
     (DestIndex >= 0) and (DestIndex < FilesList.Items.Count) then
  begin
    FilesList.Items.Move(SrcIndex, DestIndex);
    FilesList.ItemIndex := DestIndex;
  end;
end;

procedure TFileConcatForm.FilesListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Button = mbRight then
  begin
    i := FilesList.ItemAtPos(Point(X, Y), True);
    if i>=0 then
      FilesList.ItemIndex := i;
  end;
end;

procedure TFileConcatForm.Remove1Click(Sender: TObject);
begin
  FilesList.DeleteSelected();
end;

procedure TFileConcatForm.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  AddFilesToList(OpenDialog1.Files);
end;

procedure TFileConcatForm.SpeedButton2Click(Sender: TObject);
begin
  OpenDialog2.FileName := EditCRCFileName.Text;
  if not OpenDialog2.Execute then Exit;
  EditCRCFileName.Text := OpenDialog2.FileName;
end;

procedure TFileConcatForm.SpeedButton3Click(Sender: TObject);
begin
  SaveDialog1.FileName := EditResultFileName.Text;
  if not SaveDialog1.Execute then Exit;
  EditResultFileName.Text := SaveDialog1.FileName;
end;

procedure TFileConcatForm.BtnClearListClick(Sender: TObject);
begin
  ClearFileList();
end;

function TFileConcatForm.SuggestOriginalFile(FileName: string): string;
begin
  // filename.ext.001
  with TRegEx.Match(FileName, '(.*)\.\d{1,6}$') do
    if Success then
      Exit(Groups[1].Value);
  // filename_001.ext
  with TRegEx.Match(FileName, '(.*)_\d{1,6}(\.[^\.]{1,10})') do
    if Success then
      Exit(Groups[1].Value + Groups[2].Value);
  // Unknown pattern
  Result := FileName + '.combined';
end;

end.
