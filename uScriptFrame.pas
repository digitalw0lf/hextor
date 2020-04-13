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

  uHextorTypes;

type
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Init();
    procedure Uninit();
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
begin
  AText := ScriptEdit.Text;

  AppSettings.Script.Text := AText;
  MainForm.SaveSettings();

  //t := GetNanosec();
  Timer := TStopwatch.StartNew();

  PrepareScriptEnv();

  Res := ScriptControl1.Eval(AText);  // <--

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
  ScriptControl1 := TScriptControl.Create(Self);
  ScriptControl1.Language := 'JScript';
end;

destructor TScriptFrame.Destroy;
begin
  inherited;
end;

procedure TScriptFrame.Init;
begin
  ScriptEdit.Text := AppSettings.Script.Text;
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
  AppSettings.Script.Text := ScriptEdit.Text;
end;

end.
