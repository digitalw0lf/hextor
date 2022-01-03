unit uFindAltStreamsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.IOUtils,
  System.Math,

  uHextorTypes, uHextorGUI, uHextorDataSources, Vcl.ExtCtrls;

type
  TFindAltStreamsForm = class(TForm)
    Label1: TLabel;
    EditFolders: TComboBox;
    BtnFind: TButton;
    BtnCancel: TButton;
    BtnSelectFolder: TSpeedButton;
    FileOpenDialog1: TFileOpenDialog;
    Label2: TLabel;
    EditStreamNames: TComboBox;
    ImageProxy1: THintedImageProxy;
    procedure BtnSelectFolderClick(Sender: TObject);
    procedure BtnFindClick(Sender: TObject);
  private
    { Private declarations }
  public type
    TNTFSAltStreamInfo = record
      Name: string;
      Size: Int64;
    end;
  public
    { Public declarations }
    function GetNTFSFileStreams(const FileName: string): TArray<TNTFSAltStreamInfo>;
  end;

var
  FindAltStreamsForm: TFindAltStreamsForm;

implementation

uses
  uMainForm, uSearchResultsTabFrame;

{$R *.dfm}

// WinApi headers for NTFS alternate streams
type
  WIN32_FIND_STREAM_DATA = packed record
    StreamSize: LARGE_INTEGER;
    cStreamName: array [0..MAX_PATH + 35] of WCHAR;
  end;
  PWIN32_FIND_STREAM_DATA = ^WIN32_FIND_STREAM_DATA;
  STREAM_INFO_LEVELS = (FindStreamInfoStandard, FindStreamInfoMaxInfoLevel);

function FindFirstStreamW(lpFileName:  LPCWSTR; InfoLevel:  STREAM_INFO_LEVELS;
  lpFindStreamData: PWIN32_FIND_STREAM_DATA; dwFlags: DWORD): THandle; stdcall; external kernel32;
function FindNextStreamW(hFindStream: THandle;
  lpFindStreamData: PWIN32_FIND_STREAM_DATA): BOOL; stdcall; external kernel32;


procedure TFindAltStreamsForm.BtnFindClick(Sender: TObject);
var
  Directories: TArray<string>;
  Filter: TNameFilter;
  Dir: string;
  ResultsFrame: TSearchResultsTabFrame;
  FoundCount: Integer;
begin
  Directories := SplitPathList(EditFolders.Text);
  if Length(Directories) = 0 then
    raise EInvalidUserInput.Create('Specify search directories');
  AddComboBoxHistory(EditFolders);

  Filter := TNameFilter.FromString(EditStreamNames.Text);
  AddComboBoxHistory(EditStreamNames);

  ModalResult := mrOk;

  FoundCount := 0;
  ResultsFrame := MainForm.SearchResultsFrame.StartNewList(nil, 'Alt. streams');
  Progress.TaskStart(Self);
  try
    for Dir in Directories do
    begin
      TDirectory.GetFileSystemEntries(Dir, TSearchOption.soAllDirectories,
        function(const Path: string; const SearchRec: TSearchRec): Boolean
        var
          MainFN, AltFN: string;
          i: Integer;
          St: TArray<TNTFSAltStreamInfo>;
//          ResultsGroupNode: Pointer;  // PResultTreeNode type is private to TSearchResultsTabFrame
        begin
          MainFN := TPath.Combine(Path, SearchRec.Name);
          St := GetNTFSFileStreams(MainFN);
//          if Length(St) > IfThen(SearchRec.Attr and faDirectory <> 0, 0, 1) then
//          begin
//            if St[0].Name = '' then Delete(St, 0, 1);
            // Add found streams to SearchResults
            for i := 0 to Length(St) - 1 do
            begin
              if St[i].Name = '' then Continue;  // Default unnamed stream
              AltFN := MainFN + ':' + St[i].Name;
              if not Filter.Matches(St[i].Name, AltFN) then Continue;
              ResultsFrame.AddListGroup(nil, AltFN + ' (' + FileSize2Str(St[i].Size) + ')', TFileDataSource, AltFN);
              //ResultsFrame.AddListItem(ResultsGroupNode, nil, NoRange, TEncoding.Default.CodePage);
              Inc(FoundCount);
            end;
//          end;

          Progress.Show(-1, Format('Found %d ADS''s (%s)', [FoundCount, Path]));
          Result := False;
        end
      );
    end;
  finally
    ResultsFrame.EndUpdateList();
    MainForm.ShowToolFrame(MainForm.SearchResultsFrame);
    Progress.TaskEnd();
  end;
end;

procedure TFindAltStreamsForm.BtnSelectFolderClick(Sender: TObject);
// Directory select dialog
var
  sl: TStringList;
begin
  if not FileOpenDialog1.Execute then Exit;
  sl := TStringList.Create();
  try
    sl.Assign(FileOpenDialog1.Files);
    sl.Delimiter := PathSep;
    sl.QuoteChar := '"';
    EditFolders.Text := sl.DelimitedText;
  finally
    sl.Free;
  end;
end;

function TFindAltStreamsForm.GetNTFSFileStreams(
  const FileName: string): TArray<TNTFSAltStreamInfo>;
// Get a list of Alternate NTFS streams of specified file (including default stream)
var
  Drive: string;
  CompLen, Flags: Cardinal;
  FindData: WIN32_FIND_STREAM_DATA;
  H: THandle;
  Info: TNTFSAltStreamInfo;
begin
  Result := [];
  Drive := ExtractFileDrive(FileName);
  if Drive = '' then Exit;
  Drive := IncludeTrailingPathDelimiter(Drive);
  if not GetVolumeInformation(PChar(Drive), nil, 0, nil, CompLen, Flags, nil, 0) then Exit;
  if (Flags and FILE_NAMED_STREAMS) = 0 then Exit;

  H := FindFirstStreamW(PChar(FileName), FindStreamInfoStandard, @FindData, 0);
  try
    if H <> INVALID_HANDLE_VALUE then
    begin
      repeat
        Info.Name := PChar(@FindData.cStreamName[0]);
        if (Info.Name.StartsWith(':')) and (Info.Name.EndsWith(':$DATA')) then
          Info.Name := Copy(Info.Name, Low(Info.Name) + 1, Length(Info.Name) - 7);
        Info.Size := FindData.StreamSize.QuadPart;
        Result := Result + [Info];
      until not FindNextStreamW(H, @FindData);
    end;
  finally
    Winapi.Windows.FindClose(H);
  end;
end;

end.
