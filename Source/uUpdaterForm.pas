unit uUpdaterForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, mxWebUpdate, Vcl.ExtCtrls,
  Vcl.Samples.Gauges, Vcl.StdCtrls, System.UITypes, System.RegularExpressions,
  Winapi.WinInet,

  uHextorTypes, uModuleSettings;

type
  TUpdaterSettings = class (TModuleSettings)
  public
    CheckInterval: Integer;
    LastCheck: TDateTime;
    procedure InitDefault(); override;
  end;

  TUpdaterForm = class(TForm)
    mxWebUpdate1: TmxWebUpdate;
    Timer1: TTimer;
    Panel1: TPanel;
    Gauge1: TGauge;
    BtnCancel: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mxWebUpdate1DownloadError(Sender: TObject);
    procedure mxWebUpdate1Download(Sender: TObject; Total, Downloaded: Integer);
    procedure mxWebUpdate1NoUpdateFound(Sender: TObject);
    procedure mxWebUpdate1UpdateAvailable(Sender: TObject; ActualVersion,
      NewVersion: string; var CanUpdate: Boolean);
    procedure mxWebUpdate1AfterDownload(Sender: TObject; FileName: string);
    procedure mxWebUpdate1BeforeDownload(Sender: TObject; FileName: string);
    procedure Timer1Timer(Sender: TObject);
    procedure mxWebUpdate1BeforeGetInfo(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCheckNow: Boolean;
    FShouldCloseForm: Boolean;
    procedure CheckUpdateAvailable();
  public
    { Public declarations }
    UpdaterSettings: TUpdaterSettings;
  end;

var
  UpdaterForm: TUpdaterForm;

implementation

uses
  uMainForm;

{$R *.dfm}

const
  UpdateInfoURL = 'https://digitalw0lf.github.io/hextor-pages/download/webupdate.htm';

function DownloadFile( FURL: String; FUserName: String; FPassword: String; FBinary: Boolean; FSaveToFile: Boolean; FFileName: String ): string;
Var
{$WARNINGS OFF}
  hSession, hConnect, hRequest: hInternet;
  HostName, FileName: String;
  //mitec begin
  AcceptType: {$IFDEF UNICODE}LPWSTR{$ELSE}LPStr{$ENDIF};
  //mitec end
  Buffer: Pointer;
  BufferLength, Index: DWord;
  //mitec begin
  Data: Array[ 0..1024 ] Of AnsiChar;
  //mitec end
  Agent: String;
  InternetFlag: DWord;
  RequestMethod: PChar;
  FReferer: String;
  TempStr: String;
  FDownloadResult: Boolean;
  FStringResult: String;
  FFileSize: Integer;
  FDownloadedSize: DWord;
  FDownloadSize: DWord;
  F: File;

  Procedure CloseHandles;
  Begin
    InternetCloseHandle( hRequest );
    InternetCloseHandle( hConnect );
    InternetCloseHandle( hSession );
  End;
{$WARNINGS ON}

Begin
  Result := '';
  FFileSize := 0;
  Try
//    FAborting := False;
    TmxWebUpdate.ParseURL( FURL, HostName, FileName );
    Agent := 'Hextor';
{$WARNINGS OFF}
    hSession := InternetOpen( PChar( Agent ), INTERNET_OPEN_TYPE_PRECONFIG, Nil, Nil, 0 );
//mitec begin
    hConnect := InternetConnect( hSession, PChar( HostName ), 80, PChar( FUserName ), PChar( FPassword ), INTERNET_SERVICE_HTTP, 0, 0 );
//mitec end
    RequestMethod := 'GET';
    InternetFlag := INTERNET_FLAG_RELOAD;
    AcceptType := PChar( 'Accept: ' + '*/*' );

    hRequest := HttpOpenRequest( hConnect, RequestMethod, PChar( FileName ), 'HTTP/1.0', PChar( FReferer ), @AcceptType, InternetFlag, 0 );

    HttpSendRequest( hRequest, Nil, 0, Nil, 0 );
{$WARNINGS ON}

//    If FAborting Then
//    Begin
//      CloseHandles;
//      FDownloadResult := False;
//      Exit;
//    End;

    Index := 0;
    BufferLength := 1024;
{$WARNINGS OFF}
    GetMem( Buffer, BufferLength );
    FDownloadResult := HttpQueryInfo( hRequest, HTTP_QUERY_CONTENT_LENGTH, Buffer, BufferLength, Index );
{$WARNINGS ON}

//    If FAborting Then
//    Begin
//{$WARNINGS OFF}
//      FreeMem( Buffer );
//{$WARNINGS ON}
//      CloseHandles;
//      FDownloadResult := False;
//      Exit;
//    End;

    If FDownloadResult Or Not FBinary Then
    Begin
{$WARNINGS OFF}
//mitec begin
      If FDownloadResult Then FFileSize := StrToInt( String( Buffer ) );
//mitec end
{$WARNINGS ON}

      FDownloadedSize := 0;

      If FSaveToFile Then
      Begin
        AssignFile( F, FFileName );
        ReWrite( F, 1 );
      End
      Else FStringResult := '';

      While True Do
      Begin
//        If FAborting Then
//        Begin
//          If FSaveToFile Then CloseFile( F );
//{$WARNINGS OFF}
//          FreeMem( Buffer );
//{$WARNINGS ON}
//          CloseHandles;
//          FDownloadResult := False;
//          Exit;
//        End;

{$WARNINGS OFF}
        If Not InternetReadFile( hRequest, @Data, SizeOf( Data ), FDownloadSize ) Then Break
{$WARNINGS ON}
        Else
        Begin
          If FDownloadSize = 0 Then Break Else
          Begin
{$WARNINGS OFF}
            If FSaveToFile Then BlockWrite( f, Data, FDownloadSize ) Else
{$WARNINGS ON}
            Begin
              TempStr := string(Data);
              SetLength( TempStr, FDownloadSize );
              FStringResult := FStringResult + TempStr;
            End;

            Inc( FDownloadedSize, FDownloadSize );

//            DoDownload;
            Application.ProcessMessages;
          End;
        End;
      End;

      If FSaveToFile Then
      Begin
        FDownloadResult := FFileSize = Integer( FDownloadedSize )
      End
      Else
      Begin
        SetLength( FStringResult, FDownloadedSize );
        FDownloadResult := FDownloadedSize <> 0;
      End;

      If FSaveToFile Then CloseFile( f );
    End;

{$WARNINGS OFF}
    FreeMem( Buffer );
{$WARNINGS ON}
    CloseHandles;

    If FDownloadResult Then
    Begin
      If Not FSaveToFile Then
      Begin
        Result := FStringResult;
      End;
    End
//    Else DoDownloadError;

  Except
//    FDownloadResult := False;
    Result := '';
  End;
End;

procedure TUpdaterForm.CheckUpdateAvailable;
// To be called in background thread.
// Downloads version info file and notifies MainForm if new update is available
var
  InfoText, NewVersion: string;
  Cmp: Integer;
begin
  try
    NewVersion := '';

    InfoText := DownloadFile(UpdateInfoURL, '', '', False, False, '');

    with TRegEx.Match(InfoText, '<META NAME="mxVersion" CONTENT="([^"]*)">') do
      if Success then
      begin
        NewVersion := Groups[1].Value;
      end;
    if NewVersion <> '' then
    begin
      Cmp := TmxWebUpdate.CompareVersionStr(AppVersion, NewVersion);
      if Cmp < 0 then
        TThread.Queue(nil, procedure
          begin
            MainForm.ShowUpdateAvailable(NewVersion);
          end);
    end;
  except
    // Let eat bee
  end;
end;

procedure TUpdaterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if mxWebUpdate1.Active then
    mxWebUpdate1.Abort();
end;

procedure TUpdaterForm.FormCreate(Sender: TObject);
begin
  mxWebUpdate1.ProductInfo.Version := AppVersion;
  mxWebUpdate1.ProductInfo.URL := UpdateInfoURL;

  UpdaterSettings := TUpdaterSettings.Create();
end;

procedure TUpdaterForm.FormDestroy(Sender: TObject);
begin
  UpdaterSettings.Free;
end;

procedure TUpdaterForm.FormShow(Sender: TObject);
begin
  Gauge1.Progress := 0;
  // Start update check
  Timer1.Interval := 10;
  FCheckNow := True;
end;

procedure TUpdaterForm.mxWebUpdate1AfterDownload(Sender: TObject;
  FileName: string);
begin
  Memo1.Text := 'Installing...';
end;

procedure TUpdaterForm.mxWebUpdate1BeforeDownload(Sender: TObject;
  FileName: string);
begin
  Memo1.Text := 'Downloading...';
end;

procedure TUpdaterForm.mxWebUpdate1BeforeGetInfo(Sender: TObject);
begin
  Memo1.Text := 'Checking for available update...';
end;

procedure TUpdaterForm.mxWebUpdate1Download(Sender: TObject; Total,
  Downloaded: Integer);
begin
  if Total > 0 then
    Gauge1.Progress := Round(Downloaded / Total * 100)
  else
    Gauge1.Progress := 0;
end;

procedure TUpdaterForm.mxWebUpdate1DownloadError(Sender: TObject);
begin
  if Visible then
  begin
    MessageDlg( 'Error while downloading update', mtError, [ mbOK ], 0 );
  end;
end;

procedure TUpdaterForm.mxWebUpdate1NoUpdateFound(Sender: TObject);
begin
  Gauge1.Progress := 100;
  Memo1.Text := 'Your version is up-to-date';
  FShouldCloseForm := False;
end;

procedure TUpdaterForm.mxWebUpdate1UpdateAvailable(Sender: TObject;
  ActualVersion, NewVersion: string; var CanUpdate: Boolean);
begin
  if Visible then
  begin
    CanUpdate := MessageDlg( Format( 'New version is available: %s' + sLineBreak +
      sLineBreak +
      'You are using: %s' + sLineBreak +
      sLineBreak +
      'Do you want to update now?', [ NewVersion, ActualVersion ] ), mtInformation, [ mbYes, mbNo ], 0 ) = mrYes;
  end
  else
  begin
    MainForm.ShowUpdateAvailable(NewVersion);
  end;
end;

procedure TUpdaterForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Timer1.Interval := 30000;
  try
    if (FCheckNow) or
       ((UpdaterSettings.CheckInterval > 0) and
        (Now() - UpdaterSettings.LastCheck >= UpdaterSettings.CheckInterval * cDay)) then
    begin
      UpdaterSettings.LastCheck := Now();
      UpdaterSettings.Changed(False);
      FCheckNow := False;
      FShouldCloseForm := True;
      if Visible then
      begin
        if mxWebUpdate1.CheckForAnUpdate() then
          // Returns True if update was downloaded and launched
          MainForm.Close()
        else
          if FShouldCloseForm then
            Close();
      end
      else
        // If window not visible, check in background and notify main form
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
