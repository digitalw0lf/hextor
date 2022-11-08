{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uFileInfoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IOUtils,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.StrUtils,

  uEditorForm, uHextorTypes, uHextorDataSources, uFindAltStreamsForm;

type
  TFileInfoForm = class(TForm)
    Label1: TLabel;
    EditFullName: TEdit;
    Label2: TLabel;
    EditOrigSize: TEdit;
    Label4: TLabel;
    EditEditedSize: TEdit;
    BtnClose: TButton;
    Label3: TLabel;
    EditCreationTime: TEdit;
    Label5: TLabel;
    EditModificationTime: TEdit;
    GBStreams: TGroupBox;
    StreamsPanel: TPanel;
    StreamsListView: TListView;
    BtnStreamAdd: TButton;
    BtnStreamEdit: TButton;
    BtnStreamDelete: TButton;
    StaticText1: TStaticText;
    procedure BtnStreamEditClick(Sender: TObject);
    procedure BtnStreamAddClick(Sender: TObject);
    procedure BtnStreamDeleteClick(Sender: TObject);
  private
    { Private declarations }
    FEditor: TEditorForm;
    function StreamNameToCaption(const Name: string): string;
    function StreamCaptionToName(const Caption: string): string;
    function FullStreamName(const Name: string): string;
    procedure StartEditStream(const Name: string);
  private const
    sDefaultStreamCapt = '<Default stream>';
  public
    { Public declarations }
    procedure ShowInfo(AEditor: TEditorForm);
  end;

var
  FileInfoForm: TFileInfoForm;

implementation

{$R *.dfm}

uses uMainForm;

procedure TFileInfoForm.BtnStreamAddClick(Sender: TObject);
var
  Name, FullName: string;
  fs: TFileStream;
begin
  Name := '';
  if not InputQuery('Create stream', 'Create alternate NTFS stream with name:', Name) then Exit;
  FullName := FullStreamName(Name);
  if FileExists(FullName) then
    raise EInvalidUserInput.Create('Stream with such name already exists');
  fs := TFileStream.Create(FullName, fmCreate);
  fs.Free;
  StartEditStream(Name);
end;

procedure TFileInfoForm.BtnStreamDeleteClick(Sender: TObject);
var
  Name, FullName: string;
begin
  if StreamsListView.Selected = nil then Exit;
  Name := StreamCaptionToName(StreamsListView.Selected.Caption);
  if Name = '' then
    raise EInvalidUserInput.Create('Cannot delete default unnamed stream');
  if Application.MessageBox(PChar(Format('Delete stream "%s" in file "%s" ?', [Name, FEditor.DataSource.Path])),
    'Delete stream', MB_OKCANCEL) <> IDOK then Exit;

  FullName := FullStreamName(Name);
  if not DeleteFile(FullName) then
    RaiseLastOSError();

  ShowInfo(FEditor);
end;

procedure TFileInfoForm.BtnStreamEditClick(Sender: TObject);
var
  Name: string;
begin
  if StreamsListView.Selected = nil then Exit;
  Name := StreamCaptionToName(StreamsListView.Selected.Caption);
  StartEditStream(Name);
end;

function TFileInfoForm.FullStreamName(const Name: string): string;
var
  n1, n2: Integer;
begin
  Result := FEditor.DataSource.Path;
  // Remove stream name if it is specified in Path
  n1 := Result.LastDelimiter(PathDelim);
  n2 := Result.LastDelimiter(':');
  if (n1 >= 0) and (n2 > n1) then
    Delete(Result, n2 + 1, MaxInt);

  if Name <> '' then
    Result := Result + ':' + Name;
end;

{ TFileInfoForm }

procedure TFileInfoForm.ShowInfo(AEditor: TEditorForm);
var
  Size: Int64;
  DateTime: TDateTimeInfoRec;
  St: TArray<TFindAltStreamsForm.TNTFSAltStreamInfo>;
  i: Integer;
begin
  FEditor := AEditor;

  EditFullName.Text := AEditor.DataSource.Path;

  Size := AEditor.DataSource.GetSize();
  EditOrigSize.Text := IntToStr(Size) + ' bytes ( ' + FileSize2Str(Size) + ' )';
  Size := AEditor.Data.GetSize();
  EditEditedSize.Text := IntToStr(Size) + ' bytes ( ' + FileSize2Str(Size) + ' )';

  if (AEditor.DataSource is TFileDataSource) and (FileGetDateTimeInfo(AEditor.DataSource.Path, DateTime, True)) then
  begin
    EditCreationTime.Text := DateTimeToStr(DateTime.CreationTime);
    EditModificationTime.Text := DateTimeToStr(DateTime.TimeStamp);
  end
  else
  begin
    EditCreationTime.Text := '?';
    EditModificationTime.Text := '?';
  end;

  // Show NTFS streams (for files)
  StreamsPanel.Visible := False;
  StreamsListView.Items.Clear();
  if (AEditor.DataSource is TFileDataSource) then
  begin
    St := FindAltStreamsForm.GetNTFSFileStreams(AEditor.DataSource.Path);
    if St <> nil then
    begin
      StreamsPanel.Visible := True;
      StreamsListView.Items.BeginUpdate();
      try
        for i := 0 to Length(St) - 1 do
        begin
          with StreamsListView.Items.Add do
          begin
            Caption := StreamNameToCaption(St[i].Name);
            SubItems.Add(FileSize2Str(St[i].Size));
          end;
        end;
      finally
        StreamsListView.Items.EndUpdate();
      end;
      StreamsListView.ItemIndex := StreamsListView.Items.Count - 1;
    end;
  end;
end;

procedure TFileInfoForm.StartEditStream(const Name: string);
var
  AEditor: TEditorForm;
  FullName: string;
begin
  FullName := FullStreamName(Name);
  AEditor := MainForm.FindEditorWithSource(TFileDataSource, FullName);
  if AEditor <> nil then
    MainForm.ActiveEditor := AEditor
  else
    MainForm.OpenFile(FullName);
  Close();
end;

function TFileInfoForm.StreamCaptionToName(const Caption: string): string;
begin
  if Caption = sDefaultStreamCapt then Result := ''
                                  else Result := Caption;
end;

function TFileInfoForm.StreamNameToCaption(const Name: string): string;
begin
  if Name = '' then Result := sDefaultStreamCapt
               else Result := Name;
end;

end.
