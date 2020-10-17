{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uUpdaterForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.Samples.Gauges, Vcl.StdCtrls, System.UITypes, System.RegularExpressions,
  Winapi.WinInet, Winapi.UrlMon, System.IOUtils, Winapi.ShellAPI,
  Winapi.ActiveX, Vcl.ComCtrls, Vcl.OleCtrls, SHDocVw,

  uHextorTypes, uModuleSettings;

type
  TUpdaterSettings = class (TModuleSettings)
  public
    CheckInterval: Integer;
    LastCheck: TDateTime;
    procedure InitDefault(); override;
  end;

  TUpdaterForm = class(TForm, IBindStatusCallback)
    PageControl1: TPageControl;
    PgProgress: TTabSheet;
    PgInfo: TTabSheet;
    Panel1: TPanel;
    Gauge1: TGauge;
    BtnCancel: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    WebBrowser1: TWebBrowser;
    Panel2: TPanel;
    BtnCancel2: TButton;
    BtnUpdate: TButton;
    LblDownloadSize: TLabel;
    Panel3: TPanel;
    LblNewVersion: TLabel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private type
    // Information about available update
    TUpdateInfo = record
      Available: Boolean;     // Info html file downloaded successfully
      DownloadURL: string;
      DownloadSize: Int64;
      ClientSideName: string;
      RunParameters: string;
      Version: string;
    end;
  private
    { Private declarations }
    FCheckNow: Boolean;
    UpdateInfo: TUpdateInfo;
    InfoTempFileName, UpdateTempFileName: string;
    FAborted, FBusy: Boolean;
    procedure CheckUpdateAvailable();
    procedure ShowUpdateAvailable(NewAvailable: Boolean; ErrorMessage: string);
    procedure ShowStatusText(const Text: string; InProgress: Boolean = False);
    function DownloadFile(const URL, SaveAs: string; ReturnBytes: Boolean): TBytes;
    procedure DownloadUpdate(const Info: TUpdateInfo);

    // IBindStatusCallback
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out nPriority): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
      szStatusText: LPCWSTR): HResult; stdcall;
    function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
    function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
      stgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;

  public
    { Public declarations }
    Settings: TUpdaterSettings;
  end;

var
  UpdaterForm: TUpdaterForm;

implementation

uses
  uMainForm;

{$R *.dfm}

const
  UpdateInfoURL = 'https://digitalw0lf.github.io/hextor-pages/download/webupdate.htm';

function NormalyzeVersionStr(const S: string): string;
// v0.1-alpha3 => 0.1.3
var
  i: Integer;
begin
  Result := '';
  for i:=Low(S) to High(S) do
  begin
    if S[i] = '+' then Break
    else
    if CharInSet(S[i], ['0'..'9']) then
      Result := Result + S[i]
    else
      if (Result <> '') and (Result[High(Result)] <> '.') then
        Result := Result + '.';
  end;
  if (Result <> '') and (Result[High(Result)] = '.') then
    Delete(Result, High(Result), 1);
end;


function CompareVersionStr(const Left, Right: string): Integer;
// Compare two version strings like "v0.1-alpha3"
var
  ALeft, ARight: string;
  Position_Left, Position_Right: Integer;
  Left_Version, Right_Version: Integer;
begin
  ALeft := NormalyzeVersionStr(Left);
  ARight := NormalyzeVersionStr(Right);

  Repeat
    Position_Left := Pos( '.', ALeft );
    Position_Right := Pos( '.', ARight );

    If Position_Left > 0 Then
      Left_Version := StrToIntDef( Copy( ALeft, 1, Position_Left - 1 ), 0 ) Else
      Left_Version := StrToIntDef( ALeft, 0 );

    If Position_Right > 0 Then
      Right_Version := StrToIntDef( Copy( ARight, 1, Position_Right - 1 ), 0 ) Else
      Right_Version := StrToIntDef( ARight, 0 );

    If Right_Version > Left_Version Then
    Begin
      Result := -1;
      Exit;
    End;

    If Right_Version < Left_Version Then
    Begin
      Result := 1;
      Exit;
    End;

    System.Delete( ALeft, 1, Position_Left );
    System.Delete( ARight, 1, Position_Right );

  Until Position_Left = 0;

  Result := 0;
end;

procedure TUpdaterForm.BtnCancelClick(Sender: TObject);
begin
  if FBusy then
    FAborted := True;
end;

procedure TUpdaterForm.BtnUpdateClick(Sender: TObject);
begin
  ShowStatusText('Downloading...', True);
  Gauge1.Progress := 0;

  TThread.CreateAnonymousThread(procedure ()
    begin
      DownloadUpdate(UpdateInfo);
    end).Start;
end;

procedure TUpdaterForm.CheckUpdateAvailable;
// To be called in background thread.
// Downloads version info file and notifies MainForm if new update is available
var
  Buf: TBytes;
  InfoText: string;
  Info: TUpdateInfo;
  ErrorMessage: string;

  function GetInfo(const Code: string): string;
  begin
    with TRegEx.Match(InfoText, '<META NAME="' + Code + '" CONTENT="([^"]*)">') do
      if Success then
        Exit(Groups[1].Value)
      else
        Exit('');
  end;

begin
  ErrorMessage := '';
  FBusy := True;
  try
    // Download and parse update information file
    try
      Buf := DownloadFile(UpdateInfoURL, InfoTempFileName, True);
      if Buf = nil then
        raise Exception.Create('Failed to download update information');
      InfoText := Data2String(Buf);

      Info.Available := True;
      Info.DownloadURL := GetInfo('mxDownload');
      Info.DownloadSize := StrToIntDef(GetInfo('mxDownloadSize'), 0);
      Info.ClientSideName := GetInfo('mxClientSideName');
      Info.RunParameters := GetInfo('mxRunParameters');
      Info.Version := GetInfo('mxVersion');
    except
      on E: Exception do
      begin
        Info.Available := False;
        ErrorMessage := E.Message;
      end;
    end;

  finally
    FBusy := False;
    // Show result
    TThread.Queue(nil, procedure
      var
        Cmp: Integer;
        NewAvailable: Boolean;
      begin
        UpdateInfo := Info;
        NewAvailable := False;
        if Info.Available then
        begin
          UpdateTempFileName := TPath.Combine(MainForm.TempPath, Info.ClientSideName);

          if Info.Version <> '' then
          begin
            Cmp := CompareVersionStr(AppVersion, Info.Version);
            if Cmp < 0 then
              NewAvailable := True;
          end;

          if NewAvailable then
            MainForm.ShowUpdateAvailable(Info.Version);
        end;
        if Visible then
          ShowUpdateAvailable(NewAvailable, ErrorMessage);
      end);
  end;
end;

function TUpdaterForm.DownloadFile(const URL, SaveAs: string; ReturnBytes: Boolean): TBytes;
// Download file from URL.
// If SaveAs specified, save result to file.
// if ReturnBytes speified, return downloaded data
var
  fn: string;
  res: HRESULT;
begin
  if SaveAs <> '' then
    fn := SaveAs
  else
    fn := TPath.Combine(MainForm.TempPath, 'download.tmp');
  ForceDirectories(ExtractFilePath(fn));

  res := URLDownloadToFile(nil, PChar(URL), PChar(fn), 0, Self as IBindStatusCallback);
  if res <> S_OK then
    raise Exception.Create('Download failed');

  if ReturnBytes then
    Result := TFile.ReadAllBytes(fn)
  else
    Result := nil;
end;

procedure TUpdaterForm.DownloadUpdate(const Info: TUpdateInfo);
// To be called in background thread.
// Downloads and launches update.
var
  ErrorMessage: string;
  fn: string;
  LInfo: TUpdateInfo;
begin
  ErrorMessage := '';
  FBusy := True;
  try
    fn := TPath.Combine(MainForm.TempPath, Info.ClientSideName);
    try
      DeleteFile(fn);

      DownloadFile(Info.DownloadURL, fn, False);

      if not FileExists(fn) then
        raise Exception.Create('Failed to download update');
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
      end;
    end;

  finally
    FBusy := False;
    // Process result
    LInfo := Info;  // Capture parameter
    TThread.Queue(nil, procedure
      begin
        if ErrorMessage <> '' then
        begin
          ShowStatusText(ErrorMessage);
        end
        else
        begin
          Gauge1.Progress := 100;
          ShowStatusText('Installing...', True);
          If ShellExecute( Application.MainForm.Handle, PChar( 'open' ), PChar( fn ), PChar( LInfo.RunParameters ),
                           PChar( '' ), SW_SHOWNORMAL ) <= 32 Then
          begin
            ShowStatusText('Failed to start installation');
            Exit;
          end;
          Application.MainForm.Close();
        end;
      end);
  end;
end;

procedure TUpdaterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FBusy then
    FAborted := True;
end;

procedure TUpdaterForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to PageControl1.PageCount-1 do
    PageControl1.Pages[i].TabVisible := False;
  PageControl1.ActivePage := PgProgress;

  Settings := TUpdaterSettings.Create();
end;

procedure TUpdaterForm.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TUpdaterForm.FormShow(Sender: TObject);
begin
  if InfoTempFileName = '' then
    InfoTempFileName := TPath.Combine(MainForm.TempPath, 'UpdateInfo.htm');
  Gauge1.Progress := 0;
  // Start update check
  Timer1.Interval := 10;
  FCheckNow := True;
end;

function TUpdaterForm.GetBindInfo(out grfBINDF: DWORD;
  var bindinfo: TBindInfo): HResult;
begin
//  WriteLog('Updater', 'GetBindInfo');
  grfBINDF := BINDF_GETNEWESTVERSION or BINDF_PRAGMA_NO_CACHE;
  Result := S_OK;
end;

function TUpdaterForm.GetPriority(out nPriority): HResult;
begin
//  WriteLog('Updater', 'GetPriority');
  Result := S_OK;
end;

function TUpdaterForm.OnDataAvailable(grfBSCF, dwSize: DWORD;
  formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
begin
//  WriteLog('Updater', 'OnDataAvailable');
  Result := S_OK;
end;

function TUpdaterForm.OnLowResource(reserved: DWORD): HResult;
begin
//  WriteLog('Updater', 'OnLowResource');
  Result := S_OK;
end;

function TUpdaterForm.OnObjectAvailable(const iid: TGUID;
  punk: IInterface): HResult;
begin
//  WriteLog('Updater', 'OnObjectAvailable');
  Result := S_OK;
end;

function TUpdaterForm.OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
  szStatusText: LPCWSTR): HResult;
begin
//  WriteLogFmt('Updater', 'OnProgress %d / %d (%d) %s', [ulProgress, ulProgressMax, ulStatusCode, szStatusText]);
  TThread.Queue(nil, procedure
    begin
      Gauge1.Progress := Round(ulProgress / ulProgressMax * 100);
    end);
  if FAborted then
    Result := E_ABORT
  else
    Result := S_OK;
end;

function TUpdaterForm.OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult;
begin
//  WriteLog('Updater', 'OnStartBinding');
  Result := S_OK;
end;

function TUpdaterForm.OnStopBinding(hresult: HResult;
  szError: LPCWSTR): HResult;
begin
//  WriteLog('Updater', 'OnStopBinding');
  Result := S_OK;
end;

procedure TUpdaterForm.ShowStatusText(const Text: string; InProgress: Boolean = False);
begin
  Memo1.Text := Text;
  if InProgress then
    BtnCancel.Caption := 'Cancel'
  else
  begin
    Gauge1.Progress := 100;
    BtnCancel.Caption := 'Close';
  end;
  PageControl1.ActivePage := PgProgress;
end;

procedure TUpdaterForm.ShowUpdateAvailable(NewAvailable: Boolean;
  ErrorMessage: string);
begin
  if NewAvailable then
  begin
    LblNewVersion.Caption := Format('New version is available:  %s' + sLineBreak +
                                    sLineBreak +
                                    'You are using:  %s' + sLineBreak +
                                    sLineBreak +
                                    'Additional information:',
                                    [UpdateInfo.Version, AppVersion]);
    LblDownloadSize.Caption := 'Download size: ' + FileSize2Str(UpdateInfo.DownloadSize);
    PageControl1.ActivePage := PgInfo;
    WebBrowser1.Navigate(InfoTempFileName);
  end
  else
  begin
    if ErrorMessage <> '' then ShowStatusText(ErrorMessage)
                          else ShowStatusText('Your version is up-to-date');
  end;
end;

procedure TUpdaterForm.Timer1Timer(Sender: TObject);
begin
  if FBusy then Exit;  // Previous operation in progress

  Timer1.Enabled := False;
  Timer1.Interval := 30000;
  try
    if (FCheckNow) or
       ((not Visible) and
        (Settings.CheckInterval > 0) and
        (Now() - Settings.LastCheck >= Settings.CheckInterval * cDay)) then
    begin
      Settings.LastCheck := Now();
      Settings.Changed(False);
      FCheckNow := False;

      if Visible then
      begin
        Gauge1.Progress := 0;
        ShowStatusText('Checking for available update...', True);
      end;
      FAborted := False;

      TThread.CreateAnonymousThread(CheckUpdateAvailable).Start;
    end;
  finally
    Timer1.Enabled := True;
  end;
end;

{ TUpdaterSettings }

procedure TUpdaterSettings.InitDefault;
begin
  inherited;
  CheckInterval := 7;
end;

end.
