{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

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
    0: UpdaterForm.Settings.CheckInterval := 7;
    1: UpdaterForm.Settings.CheckInterval := 30;
    2: UpdaterForm.Settings.CheckInterval := 0;
  end;
  UpdaterForm.Settings.Changed(True);
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
  if UpdaterForm.Settings.CheckInterval = 0 then
    CBUpdateCheckInterval.ItemIndex := 2
  else
  if UpdaterForm.Settings.CheckInterval <= 7 then
    CBUpdateCheckInterval.ItemIndex := 0
  else
    CBUpdateCheckInterval.ItemIndex := 1;
end;

end.
