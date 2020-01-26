unit uProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Gauges, Vcl.StdCtrls,
  System.Types;

type
  TProgressForm = class(TForm)
    ProgressGauge: TGauge;
    ProgressTextLabel: TLabel;
    BtnAbort: TButton;
    procedure BtnAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.BtnAbortClick(Sender: TObject);
begin
  FOperationAborted := True;
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

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
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
