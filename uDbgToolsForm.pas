unit uDbgToolsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  {$IFDEF USE_DWF_UTILS}, uProfilerFrame{$ENDIF}
  ;

type
  TDbgToolsForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF USE_DWF_UTILS}
    ProfilerFrame1: TProfilerFrame;
    {$ENDIF}
  end;

var
  DbgToolsForm: TDbgToolsForm;

implementation

{$R *.dfm}

procedure TDbgToolsForm.FormCreate(Sender: TObject);
begin
  {$IFDEF USE_DWF_UTILS}
  ProfilerFrame1 := TProfilerFrame.Create(Self);
  ProfilerFrame1.Parent := Self;
  {$ENDIF}
end;

end.
