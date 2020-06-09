unit uSettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Winapi.ShellAPI,

  uModuleSettings;

type
  TSettingsForm = class(TForm)
    Label1: TLabel;
    CBUpdateCheckInterval: TComboBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    LblOpenSettingsFolder: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure LblOpenSettingsFolderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowSettings();
    procedure ApplySettings();
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  uMainForm, uUpdaterForm;

{$R *.dfm}

{ TSettingsForm }

procedure TSettingsForm.ApplySettings;
begin
  case CBUpdateCheckInterval.ItemIndex of
    0: UpdaterForm.UpdaterSettings.CheckInterval := 7;
    1: UpdaterForm.UpdaterSettings.CheckInterval := 30;
    2: UpdaterForm.UpdaterSettings.CheckInterval := 0;
  end;
  UpdaterForm.UpdaterSettings.Changed(True);
end;

procedure TSettingsForm.BtnOKClick(Sender: TObject);
begin
  ApplySettings();
//  MainForm.SaveSettings();
  ModalResult := mrOk;
end;

procedure TSettingsForm.LblOpenSettingsFolderClick(Sender: TObject);
begin
  ShellExecute(0, '', PChar(TModuleSettings.SettingsFolder), '', '', SW_SHOWNORMAL);
end;

procedure TSettingsForm.ShowSettings;
begin
  if UpdaterForm.UpdaterSettings.CheckInterval = 0 then
    CBUpdateCheckInterval.ItemIndex := 2
  else
  if UpdaterForm.UpdaterSettings.CheckInterval <= 7 then
    CBUpdateCheckInterval.ItemIndex := 0
  else
    CBUpdateCheckInterval.ItemIndex := 1;
end;

end.
