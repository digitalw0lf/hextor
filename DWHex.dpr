program DWHex;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uEditorPane in 'uEditorPane.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
