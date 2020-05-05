unit uFileInfoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  uEditorForm, uHextorTypes, uHextorDataSources;

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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowInfo(AEditor: TEditorForm);
  end;

var
  FileInfoForm: TFileInfoForm;

implementation

{$R *.dfm}

{ TFileInfoForm }

procedure TFileInfoForm.ShowInfo(AEditor: TEditorForm);
var
  Size: Int64;
  DateTime: TDateTimeInfoRec;
begin
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
end;

end.
