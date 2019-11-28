program DWHex;

uses
  Vcl.Forms,
  uEditorPane in 'uEditorPane.pas',
  uDWHexTypes in 'uDWHexTypes.pas',
  uDWHexDataSources in 'uDWHexDataSources.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  uFindReplaceForm in 'uFindReplaceForm.pas' {FindReplaceForm},
  uDiskSelectForm in 'uDiskSelectForm.pas' {DiskSelectForm},
  uProcessSelectForm in 'uProcessSelectForm.pas' {ProcessSelectForm},
  uBitsEditorForm in 'uBitsEditorForm.pas' {BitsEditorForm},
  uEditorForm in 'uEditorForm.pas' {EditorForm},
  uValueFrame in 'uValueFrame.pas' {ValueFrame: TFrame},
  uStructFrame in 'uStructFrame.pas' {StructFrame: TFrame},
  uEditedData in 'uEditedData.pas',
  uCompareFrame in 'uCompareFrame.pas' {CompareFrame: TFrame},
  uCallbackList in 'uCallbackList.pas',
  uScriptFrame in 'uScriptFrame.pas' {ScriptFrame: TFrame},
  DWHex_TLB in 'DWHex_TLB.pas',
  uCoDWHex in 'uCoDWHex.pas' {CoDWHex: CoClass},
  uDbgToolsForm in 'uDbgToolsForm.pas' {DbgToolsForm},
  uProfilerFrame in 'D:\Work\Trunk\Units\Components\uProfilerFrame.pas' {ProfilerFrame: TFrame},
  uDataSearcher in 'uDataSearcher.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindReplaceForm, FindReplaceForm);
  Application.CreateForm(TDiskSelectForm, DiskSelectForm);
  Application.CreateForm(TProcessSelectForm, ProcessSelectForm);
  Application.CreateForm(TBitsEditorForm, BitsEditorForm);
  Application.CreateForm(TDbgToolsForm, DbgToolsForm);
  Application.Run;
end.
