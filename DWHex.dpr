program DWHex;

uses
  Vcl.Forms,
  uEditorPane in 'uEditorPane.pas',
  uDWHexTypes in 'uDWHexTypes.pas',
  uDWHexDataSources in 'uDWHexDataSources.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  uFindReplaceForm in 'uFindReplaceForm.pas' {FindReplaceForm},
  uDiskSelectForm in 'uDiskSelectForm.pas' {DiskSelectForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindReplaceForm, FindReplaceForm);
  Application.CreateForm(TDiskSelectForm, DiskSelectForm);
  Application.Run;
end.