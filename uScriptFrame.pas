unit uScriptFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.OleCtrls, MSScriptControl_TLB, Vcl.ComCtrls,

  uUtil;

type
  TScriptFrame = class(TFrame)
    ToolPanel: TPanel;
    BtnRun: TSpeedButton;
    ScriptControl1: TScriptControl;
    Timer1: TTimer;
    MemoScript: TRichEdit;
    Splitter1: TSplitter;
    OutputPanel: TPanel;
    MemoOutput: TRichEdit;
    OutputToolPanel: TPanel;
    BtnClearOutput: TSpeedButton;
    procedure BtnRunClick(Sender: TObject);
    procedure BtnClearOutputClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Init();
    procedure Uninit();
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TScriptFrame.BtnRunClick(Sender: TObject);
var
  AText: string;
  Res: OleVariant;
begin
  AText := MemoScript.Text;

  AppSettings.Script.Text := AText;
  MainForm.SaveSettings();

  Res := ScriptControl1.Eval(AText);

  MemoOutput.Lines.Add(Res);
  ShowMemoCaret(MemoOutput, True);
end;

constructor TScriptFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TScriptFrame.Destroy;
begin
  inherited;
end;

procedure TScriptFrame.Init;
begin
  MemoScript.Text := AppSettings.Script.Text;

  ScriptControl1.AddObject('DWHex', MainForm.DWHexOle, False);

end;

procedure TScriptFrame.BtnClearOutputClick(Sender: TObject);
begin
  MemoOutput.Lines.Clear();
end;

procedure TScriptFrame.Uninit;
begin
  AppSettings.Script.Text := MemoScript.Text;
end;

end.
