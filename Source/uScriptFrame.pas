{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uScriptFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.OleCtrls, Vcl.ComCtrls,
  System.Diagnostics, SynEdit, SynEditHighlighter, SynEditCodeFolding,
  SynHighlighterJScript, Generics.Collections, Vcl.ToolWin, System.IOUtils,
  Vcl.Menus, Winapi.ShellAPI,

  uHextorTypes, uHextorGUI, uModuleSettings, uActiveScript;

const
  ImageIndex_Folder = 23;  // Index in  MainForm.ImageList16

type
  TScriptSettings = class (TModuleSettings)
    Text: string;
  end;

  TScriptFrame = class(TFrame, IHextorToolFrame)
    Timer1: TTimer;
    Splitter1: TSplitter;
    OutputPanel: TPanel;
    MemoOutput: TRichEdit;
    OutputToolPanel: TPanel;
    BtnClearOutput: TSpeedButton;
    ScriptEdit: TSynEdit;
    SynJScriptSyn1: TSynJScriptSyn;
    ToolBar1: TToolBar;
    BtnNew: TToolButton;
    BtnLoad: TToolButton;
    BtnSave: TToolButton;
    LblScriptName: TLabel;
    BtnRun: TToolButton;
    SavedScriptsMenu: TPopupMenu;
    MIBuiltinItemsMenu: TMenuItem;
    MIDummyScript: TMenuItem;
    MIAfterFileItems: TMenuItem;
    MIOrganizeFiles: TMenuItem;
    SaveAsMenu: TPopupMenu;
    MISaveAs: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure BtnRunClick(Sender: TObject);
    procedure BtnClearOutputClick(Sender: TObject);
    procedure BtnNewClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIOrganizeFilesClick(Sender: TObject);
    procedure MIDummyScriptClick(Sender: TObject);
  private
    { Private declarations }
    ScriptEngine: TActiveScript;
    CurScriptFileName: string;
    FilesForMenuItems: TDictionary<Integer, string>;  // MenuItem.Tag -> File name
    procedure PrepareScriptEnv();
    function UserScriptsFolder(): string;
    function BuiltInScriptsFolder(): string;
  public
    { Public declarations }
    Settings: TScriptSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure OnShown();
    procedure Init();
    procedure Uninit();
    function Eval(const Text: string): Variant;
    procedure AddLog(const Text: string);
  end;

implementation

uses
  uMainForm, uDataStruct, uEditorForm;

{$R *.dfm}

procedure TScriptFrame.BtnLoadClick(Sender: TObject);
begin
  FilesForMenuItems.Clear();
  // Built-in DSs
  PopulateMenuWithFileList(MIBuiltinItemsMenu, nil, nil,
    MIDummyScript, ImageIndex_Folder, BuiltInScriptsFolder(), '*.js', FilesForMenuItems);
  // User DSs
  PopulateMenuWithFileList(SavedScriptsMenu.Items, MIBuiltinItemsMenu, MIAfterFileItems,
    MIDummyScript, ImageIndex_Folder, UserScriptsFolder(), '*.js', FilesForMenuItems);

  PopupFromControl(SavedScriptsMenu, Sender as TControl);
end;

procedure TScriptFrame.BtnNewClick(Sender: TObject);
begin
  ScriptEdit.Clear();
  LblScriptName.Caption := '    ' + 'Unnamed';
  CurScriptFileName := '';
end;

procedure TScriptFrame.BtnRunClick(Sender: TObject);
var
  AText: string;
  Res: OleVariant;
//  t: Int64;
  Timer: TStopwatch;
  i: Integer;
  AEditor: TEditorForm;
  AEditors: TList<TEditorForm>;
begin
  AText := ScriptEdit.Text;

  Settings.Text := AText;
  Settings.Changed(True);

  //t := GetNanosec();
  Timer := TStopwatch.StartNew();

  AEditors := TList<TEditorForm>.Create();
  for i:=0 to MainForm.EditorCount-1 do
  begin
    AEditor := MainForm.Editors[i];
    AEditor.UndoStack.BeginAction('Script_'+IntToStr(Random(1000000)), 'Scripted change');
    AEditor.BeginUpdate();
    AEditors.Add(AEditor);
  end;
  Progress.TaskStart(Self);
  try

    Res := Eval(AText);  // <--

  finally
    Progress.TaskEnd();
    // Check with AEditors list because MainForm.Editors could have changed during script execution
    for i:=0 to MainForm.EditorCount-1 do
    begin
      AEditor := MainForm.Editors[i];
      if AEditors.IndexOf(AEditor) >= 0 then
      begin
        AEditor.EndUpdate();
        AEditor.UndoStack.EndAction();
      end;
    end;
    AEditors.Free;
  end;

  //t := GetNanosec() - t;
  Timer.Stop();
  AddLog('Result: ' + string(Res) + ', duration: ' + R2S(Timer.Elapsed.TotalMilliseconds*1000, 0) + ' mks');
end;

function TScriptFrame.BuiltInScriptsFolder: string;
begin
  Result := IncludeTrailingPathDelimiter( TPath.Combine(Settings.BuiltInSettingsFolder, 'Scripts') );
end;

constructor TScriptFrame.Create(AOwner: TComponent);
begin
  inherited;
  Settings := TScriptSettings.Create();
  FilesForMenuItems := TDictionary<Integer, string>.Create();

  ScriptEngine := TActiveScript.Create(Self);
end;

destructor TScriptFrame.Destroy;
begin
  FilesForMenuItems.Free;
  Settings.Free;
  inherited;
end;

function TScriptFrame.Eval(const Text: string): Variant;
// Evaluate given script text
begin
  PrepareScriptEnv();

  Result := ScriptEngine.Eval(Text);
end;

procedure TScriptFrame.Init;
begin
  ScriptEdit.Text := Settings.Text;
end;

procedure TScriptFrame.MIDummyScriptClick(Sender: TObject);
var
  n: Integer;
  fn, name: string;
begin
  n := (Sender as TMenuItem).Tag;
  if not FilesForMenuItems.TryGetValue(n, fn) then Exit;
  name := (Sender as TMenuItem).Caption;
  ScriptEdit.Lines.LoadFromFile(fn);
  CurScriptFileName := fn;
  LblScriptName.Caption := '    ' + name;
end;

procedure TScriptFrame.MIOrganizeFilesClick(Sender: TObject);
begin
  ForceDirectories(UserScriptsFolder());
  ShellExecute(0, '', PChar(UserScriptsFolder()), '', '', SW_SHOWNORMAL);
end;

procedure TScriptFrame.MISaveAsClick(Sender: TObject);
// Save script
var
  fn: string;
begin
  ForceDirectories(UserScriptsFolder());
  fn := CurScriptFileName;

  // If "Save as" pressed or file name still not specified - show "Save as" dialog.
  if (Sender = MISaveAs) or (fn = '') or (PathIsInside(fn, BuiltInScriptsFolder())) then
  begin
    if (fn = '') then
      fn := 'Script1'
    else
    if PathIsInside(fn, BuiltInScriptsFolder()) then
      fn := ChangeFileExt(ExtractFileName(fn), '');
    SaveDialog1.InitialDir := UserScriptsFolder();
    SaveDialog1.FileName := fn;

    if not SaveDialog1.Execute() then Exit;
    fn := SaveDialog1.FileName;
    if not PathIsInside(fn, UserScriptsFolder()) then
      if Application.MessageBox(PChar('If you save this script outside of default user settings folder, it will not be available in menu. Continue?'), PChar('Save warning'), MB_OKCANCEL) <> IDOK then Exit;
  end
  else
    fn := CurScriptFileName;

  ForceDirectories(ExtractFilePath(fn));
  ScriptEdit.Lines.SaveToFile(fn);
  ScriptEdit.MarkModifiedLinesAsSaved();
  CurScriptFileName := fn;
  // '    ' is added so vertical line, added by toolbar, does not overlaps caption
  LblScriptName.Caption := '    ' + ChangeFileExt(ExtractFileName(CurScriptFileName), '');
end;

procedure TScriptFrame.OnShown;
begin

end;

procedure TScriptFrame.PrepareScriptEnv;
begin
  ScriptEngine.Reset();
  // Main application object
  ScriptEngine.AddObject('app', MainForm.APIEnv.GetAPIWrapper(MainForm), True);
  // Utility functions
  ScriptEngine.AddObject('utils', MainForm.APIEnv.GetAPIWrapper(MainForm.Utils), True);
  // Parsed structure from StructFrame
  ScriptEngine.AddObject('_StructFrame', MainForm.APIEnv.GetAPIWrapper(MainForm.StructFrame.DSScriptEnv), True);
end;

procedure TScriptFrame.AddLog(const Text: string);
begin
  MemoOutput.Lines.Add(Text);
  SendMessage(MemoOutput.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TScriptFrame.BtnClearOutputClick(Sender: TObject);
begin
  MemoOutput.Lines.Clear();
end;

procedure TScriptFrame.Uninit;
begin
  if ScriptEdit.Text <> Settings.Text then
  begin
    Settings.Text := ScriptEdit.Text;
    Settings.Changed();
  end;
end;

function TScriptFrame.UserScriptsFolder: string;
begin
  Result := IncludeTrailingPathDelimiter( TPath.Combine(Settings.SettingsFolder, 'Scripts') );
end;

end.
