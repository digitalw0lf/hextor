{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Gauges, Vcl.StdCtrls,
  System.Types,

  uHextorTypes, uHextorGUI;

type
  TProgressForm = class(TForm)
    ProgressGauge: TGauge;
    ProgressTextLabel: TLabel;
    BtnAbort: TButton;
    procedure BtnAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    // To save window state before locking screen with progress window
    ActiveWindow: HWnd;
    FocusState: TFocusState;
    WindowList: TTaskWindowList;
  public
    { Public declarations }
    FOperationAborted: Boolean;
    procedure ShowLikeModal();
    procedure ProgressDisplay(Sender: TProgressTracker; TotalProgress: Double; Text: string);
    procedure ProgressTaskStart(Sender: TProgressTracker; Task: TProgressTracker.TTask);
    procedure ProgressTaskEnd(Sender: TProgressTracker; Task: TProgressTracker.TTask);
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.BtnAbortClick(Sender: TObject);
begin
//  FOperationAborted := True;
  Close();
end;

procedure TProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOperationAborted := True;

  if Visible then
  begin
    EnableTaskWindows(WindowList);
    if Screen.SaveFocusedList.Count > 0 then
    begin
      Screen.FocusedForm := TCustomForm(Screen.SaveFocusedList.First);
      Screen.SaveFocusedList.Remove(Screen.FocusedForm);
    end else Screen.FocusedForm := nil;
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  //  Exclude(FFormState, fsModal);
  //  Application.ModalFinished;
  end;
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := BtnAbort.Enabled;
  if CanClose then
    Progress.OnAborting.Call(Progress, @CanClose);
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TProgressForm.ProgressDisplay(Sender: TProgressTracker;
  TotalProgress: Double; Text: string);
// Show progress reported by ProgressTracker
var
  s: string;
begin
  ProgressGauge.Progress := Round(TotalProgress * 100);
  s := FitTextInWidth(Text, ProgressTextLabel.Canvas, ProgressTextLabel.Width);
  ProgressTextLabel.Caption := s;

  if not Visible then
    ShowLikeModal();

  FOperationAborted := False;
  Application.ProcessMessages();
  if FOperationAborted then Abort();
end;

procedure TProgressForm.ProgressTaskEnd(Sender: TProgressTracker;
  Task: TProgressTracker.TTask);
begin
  if Sender.CurrentTaskLevel() = 1 then
  begin
    // Hide progress window when top-level task finishes
    BtnAbort.Enabled := True;
    Sender.OnAborting.Clear();
    Close();
  end;
end;

procedure TProgressForm.ProgressTaskStart(Sender: TProgressTracker;
  Task: TProgressTracker.TTask);
begin
  BtnAbort.Enabled := Task.Abortable;
end;

procedure TProgressForm.ShowLikeModal;
// Disable all other windows, show this window and exit
begin
  if Visible then Exit;

  ActiveWindow := Application.ActiveFormHandle;

//  Include(FFormState, fsModal);

  FocusState := SaveFocusState;
  Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
  Screen.FocusedForm := Self;
  WindowList := DisableTaskWindows({ActiveWindow}0);
//  Application.ModalStarted;

  Show();
end;

end.
