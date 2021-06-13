unit uFileSplitForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.AppEvnts,
  Winapi.ShellAPI, System.IOUtils, System.Math, Vcl.ExtCtrls,

  uHextorTypes, uHextorGUI, uHextorDataSources, HlpHashFactory, HlpIHash;

type
  TFileSplitForm = class(TForm)
    Label1: TLabel;
    EditSourceFile: TEdit;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    EditTargetFolder: TComboBox;
    Label3: TLabel;
    EditTargetName: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    EditSplitBy: TComboBox;
    LblFilesCount: TLabel;
    CBCreateCRCFile: TCheckBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    DropFileCatcher1: TDropFileCatcher;
    SelectFolderDialog: TFileOpenDialog;
    ImageProxy1: THintedImageProxy;
    procedure DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
      Control: TWinControl; Files: TStrings; DropPoint: TPoint);
    procedure EditSourceFileChange(Sender: TObject);
    procedure EditSplitByChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CalcFilesCount(Silent: Boolean): Integer;
    function GeneratePartName(const Template: string; Index: Integer): string;
  public
    { Public declarations }
    procedure AskOverwriteIfExists(const FileName: string);
    procedure SplitFile(const SourceFile, TargetFolder, TargetNamesTemplate: string;
      PartSize: Int64; CreateCRCFile: Boolean);
  end;

var
  FileSplitForm: TFileSplitForm;

implementation

{$R *.dfm}

uses
  uMainForm;

procedure TFileSplitForm.BtnOkClick(Sender: TObject);
var
  SplitBy: Int64;
begin
  if not string(EditTargetName.Text).Contains('?') then
    raise EInvalidUserInput.Create('Target names template must contain "?" character');

  SplitBy := Str2FileSize(EditSplitBy.Text);
  if SplitBy <= 0 then
    raise EInvalidUserInput.Create('Invalid "Split by" size');

  SplitFile(Trim(EditSourceFile.Text), Trim(EditTargetFolder.Text), Trim(EditTargetName.Text), SplitBy, CBCreateCRCFile.Checked);

  ModalResult := mrOk;
end;

function TFileSplitForm.CalcFilesCount(Silent: Boolean): Integer;
var
  fn: string;
  sz1, sz2: Int64;
begin
  Result := -1;
  fn := Trim(EditSourceFile.Text);
  try
    sz1 := GetFileSizeNoOpen(fn);
    sz2 := Str2FileSize(EditSplitBy.Text);
    Result := DivRoundUp(sz1, sz2);
    LblFilesCount.Caption := IntToStr(Result) + ' files total';
  except
    LblFilesCount.Caption := '';
    if not Silent then raise;
  end;
end;

procedure TFileSplitForm.DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
  Control: TWinControl; Files: TStrings; DropPoint: TPoint);
begin
  if Files.Count >= 1 then
    EditSourceFile.Text := Files[0];
end;

procedure TFileSplitForm.EditSourceFileChange(Sender: TObject);
var
  fn, s: string;
begin
  fn := Trim(EditSourceFile.Text);
  // Target folder
  s := ExtractFilePath(fn);
  EditTargetFolder.Items.Clear();
  EditTargetFolder.Items.Add(s);
  EditTargetFolder.Items.Add(TPath.Combine(s, ExtractFileName(fn) + '.Parts'));
  EditTargetFolder.ItemIndex := 0;
  // Target file names
  s := ExtractFileName(fn);
  EditTargetName.Items.Clear();
  EditTargetName.Items.Add(s + '.???');
  EditTargetName.Items.Add(ChangeFileExt(s, '_???' + ExtractFileExt(s)));
  EditTargetName.ItemIndex := 0;
  // File count
  CalcFilesCount(True);
end;

procedure TFileSplitForm.EditSplitByChange(Sender: TObject);
begin
  CalcFilesCount(True);
end;

procedure TFileSplitForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
end;

procedure TFileSplitForm.FormShow(Sender: TObject);
begin
  with MainForm.ActiveEditor do
    if (DataSource is TFileDataSource) and (ExtractFilePath(DataSource.Path) <> '') then
    begin
      EditSourceFile.Text := DataSource.Path;
    end;
end;

function TFileSplitForm.GeneratePartName(const Template: string;
  Index: Integer): string;
var
  i, n: Integer;
begin
  i := Pos('?', Template);
  if i = 0 then Exit(Template);
  n := 1;
  while Template[i + n] = '?' do
    Inc(n);
  Result := Copy(Template, 1, i - 1) +
            Format('%.*d', [n, Index]) +
            Copy(Template, i + n, MaxInt);
end;

procedure TFileSplitForm.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.FileName := EditSourceFile.Text;
  if not OpenDialog1.Execute() then Exit;
  EditSourceFile.Text := OpenDialog1.FileName;
end;

procedure TFileSplitForm.SpeedButton2Click(Sender: TObject);
begin
  SelectFolderDialog.FileName := EditTargetFolder.Text;
  if not SelectFolderDialog.Execute then Exit;
  EditTargetFolder.Text := SelectFolderDialog.FileName;
end;

procedure TFileSplitForm.AskOverwriteIfExists(const FileName: string);
var
  s: string;
begin
  if FileExists(FileName) then
  begin
    s := 'Target file' + sLineBreak +
         FileName + sLineBreak +
         'already exists. Overwrite?';
    if Application.MessageBox(PChar(s), 'Overwrite', MB_OKCANCEL) <> IDOK then
      //raise Exception.Create('Operation cancelled');
      Abort();
  end;
end;

procedure TFileSplitForm.SplitFile(const SourceFile, TargetFolder,
  TargetNamesTemplate: string; PartSize: Int64; CreateCRCFile: Boolean);
const
  BlockSize = 10 * MByte;
var
  Index, Portion: Integer;
  SourceSize, ThisPartSize, Ptr, PartPtr: Int64;
  SourceStream, DestStream: TFileStream;
  DestName, s: string;
  Buf: TBytes;
  Hash: IHash;
begin
  SourceSize := GetFileSizeNoOpen(SourceFile);
  SetLength(Buf, Min(BlockSize, PartSize));

  Ptr := 0;
  Index := 0;
  Progress.TaskStart(Self);
  try
    if CreateCRCFile then
    begin
      Hash := THashFactory.TChecksum.TCRC.CreateCRC32_PKZIP();
      Hash.Initialize();
    end;
    SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
    try
      while Ptr < SourceSize do  // Files
      begin
        Inc(Index);
        DestName := TPath.Combine(TargetFolder, GeneratePartName(TargetNamesTemplate, Index));
        AskOverwriteIfExists(DestName);

        ForceDirectories(ExtractFilePath(DestName));
        DestStream := TFileStream.Create(DestName, fmCreate);
        try
          PartPtr := 0;
          ThisPartSize := Min(PartSize, SourceSize - Ptr);
          while PartPtr < ThisPartSize do  // Portions inside file
          begin
            Portion := Min(BlockSize, ThisPartSize - PartPtr);
            SourceStream.ReadBuffer(Buf[0], Portion);
            DestStream.WriteBuffer(Buf[0], Portion);
            Inc(PartPtr, Portion);
            Inc(Ptr, Portion);
            if CreateCRCFile then
              Hash.TransformBytes(Buf, 0, Portion);
            Progress.Show(Ptr, SourceSize);
          end;
        finally
          DestStream.Free;
        end;
      end;
    finally
      SourceStream.Free;
    end;

    if CreateCRCFile then
    begin
      DestName := TPath.Combine(TargetFolder, ChangeFileExt(ExtractFileName(SourceFile), '.crc'));
      AskOverwriteIfExists(DestName);

      s := 'filename=' + ExtractFileName(SourceFile) + sLineBreak +
           'size=' + IntToStr(SourceSize) + sLineBreak +
           'crc32=' + Hash.TransformFinal().ToString() + sLineBreak;

      TFile.WriteAllText(DestName, s, TEncoding.ANSI);
    end;
  finally
    Progress.TaskEnd();
  end;
end;

end.
