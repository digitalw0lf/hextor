{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uScriptFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.OleCtrls, MSScriptControl_TLB, Vcl.ComCtrls,
  System.Diagnostics, SynEdit,

  uHextorTypes, uModuleSettings;

type
  TScriptSettings = class (TModuleSettings)
    Text: string;
  end;

  TScriptFrame = class(TFrame)
    ToolPanel: TPanel;
    BtnRun: TSpeedButton;
//    ScriptControl1: TScriptControl;
    Timer1: TTimer;
    Splitter1: TSplitter;
    OutputPanel: TPanel;
    MemoOutput: TRichEdit;
    OutputToolPanel: TPanel;
    BtnClearOutput: TSpeedButton;
    ScriptEdit: TSynEdit;
    procedure BtnRunClick(Sender: TObject);
    procedure BtnClearOutputClick(Sender: TObject);
  private
    { Private declarations }
    ScriptControl1: TScriptControl;
    procedure PrepareScriptEnv();
  public
    { Public declarations }
    Settings: TScriptSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Init();
    procedure Uninit();
    function Eval(const Text: string): Variant;
  end;

implementation

uses
  uMainForm, uDataStruct;

{$R *.dfm}

procedure TScriptFrame.BtnRunClick(Sender: TObject);
var
  AText: string;
  Res: OleVariant;
//  t: Int64;
  Timer: TStopwatch;
  i: Integer;
begin
  AText := ScriptEdit.Text;

  Settings.Text := AText;
  Settings.Changed(True);

  //t := GetNanosec();
  Timer := TStopwatch.StartNew();

  for i:=0 to MainForm.EditorCount-1 do
  begin
    MainForm.Editors[i].UndoStack.BeginAction('Script_'+IntToStr(Random(1000000)), 'Scripted change');
    MainForm.Editors[i].BeginUpdatePanes();
  end;
  try

    Res := Eval(AText);  // <--

  finally
    for i:=0 to MainForm.EditorCount-1 do
    begin
      MainForm.Editors[i].EndUpdatePanes();
      MainForm.Editors[i].UndoStack.EndAction();
    end;
  end;

  //t := GetNanosec() - t;
  Timer.Stop();

  MemoOutput.Lines.Add('Result: ' + string(Res) + ', duration: ' + R2S(Timer.Elapsed.TotalMilliseconds*1000, 0) + ' mks');
  //ShowMemoCaret(MemoOutput, True);
//  MemoOutput.SelStart := MemoOutput.GetTextLen;
//  MemoOutput.Perform(EM_SCROLLCARET, 0, 0);
  SendMessage(MemoOutput.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

constructor TScriptFrame.Create(AOwner: TComponent);
begin
  inherited;
  Settings := TScriptSettings.Create();
  ScriptControl1 := TScriptControl.Create(Self);
  ScriptControl1.Language := 'JScript';
end;

destructor TScriptFrame.Destroy;
begin
  Settings.Free;
  inherited;
end;

function TScriptFrame.Eval(const Text: string): Variant;
// Evaluate given script text
begin
  PrepareScriptEnv();

  Result := ScriptControl1.Eval(Text);
end;

procedure TScriptFrame.Init;
begin
  ScriptEdit.Text := Settings.Text;
end;

procedure TScriptFrame.PrepareScriptEnv;
begin
  ScriptControl1.Reset();
  // Main application object
  ScriptControl1.AddObject('app', MainForm.APIEnv.GetAPIWrapper(MainForm), True);
  // Utility functions
  ScriptControl1.AddObject('utils', MainForm.APIEnv.GetAPIWrapper(MainForm.Utils), True);
  // Parsed structure from StructFrame
  if (MainForm.StructFrame.ShownDS <> nil) and
     (MainForm.StructFrame.ShownDS is TDSCompoundField) then
    ScriptControl1.AddObject('ds', (MainForm.StructFrame.ShownDS as TDSCompoundField).GetComWrapper(), False);
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

end.
