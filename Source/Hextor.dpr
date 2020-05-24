program Hextor;

uses
  FastMM4,
  Vcl.Forms,
  uEditorPane in 'uEditorPane.pas',
  uHextorTypes in 'uHextorTypes.pas',
  uHextorDataSources in 'uHextorDataSources.pas',
  uOleAutoAPIWrapper in 'uOleAutoAPIWrapper.pas',
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
  uDbgToolsForm in 'uDbgToolsForm.pas' {DbgToolsForm},
  uDataSearcher in 'uDataSearcher.pas',
  uValueInterpretors in 'uValueInterpretors.pas',
  uUndoStack in 'uUndoStack.pas',
  uBitmapFrame in 'uBitmapFrame.pas' {BitmapFrame: TFrame},
  uProgressForm in 'uProgressForm.pas' {ProgressForm},
  uDataStruct in 'uDataStruct.pas',
  uSetFileSizeForm in 'uSetFileSizeForm.pas' {SetFileSizeForm},
  uFillBytesForm in 'uFillBytesForm.pas' {FillBytesForm},
  Vcl.Themes,
  Vcl.Styles,
  uSearchResultsFrame in 'uSearchResultsFrame.pas' {SearchResultsFrame: TFrame},
  uPasteAsForm in 'uPasteAsForm.pas' {PasteAsForm},
  uHextorGUI in 'uHextorGUI.pas',
  uAboutForm in 'uAboutForm.pas' {AboutForm},
  uHashFrame in 'uHashFrame.pas' {HashFrame: TFrame},
  uModifyWithExpressionForm in 'uModifyWithExpressionForm.pas' {ModifyWithExpressionForm},
  uCopyAsForm in 'uCopyAsForm.pas' {CopyAsForm},
  uSearchResultsTabFrame in 'uSearchResultsTabFrame.pas' {SearchResultsTabFrame: TFrame},
  uFileInfoForm in 'uFileInfoForm.pas' {FileInfoForm},
  uDataSaver in 'uDataSaver.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.HelpFile := 'Hextor.chm';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindReplaceForm, FindReplaceForm);
  Application.CreateForm(TDiskSelectForm, DiskSelectForm);
  Application.CreateForm(TProcessSelectForm, ProcessSelectForm);
  Application.CreateForm(TBitsEditorForm, BitsEditorForm);
  Application.CreateForm(TDbgToolsForm, DbgToolsForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TSetFileSizeForm, SetFileSizeForm);
  Application.CreateForm(TFillBytesForm, FillBytesForm);
  Application.CreateForm(TPasteAsForm, PasteAsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TModifyWithExpressionForm, ModifyWithExpressionForm);
  Application.CreateForm(TCopyAsForm, CopyAsForm);
  Application.CreateForm(TFileInfoForm, FileInfoForm);
  Application.Run;
end.
