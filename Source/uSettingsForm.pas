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

  uModuleSettings, Vcl.ExtCtrls, uHextorGUI;

type
  TSettingsForm = class(TForm)
    Label1: TLabel;
    CBUpdateCheckInterval: TComboBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    LblOpenSettingsFolder: TLabel;
    Label2: TLabel;
    CBCodePages: TComboBox;
    ImageProxy1: THintedImageProxy;
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
  uMainForm, uUpdaterForm, uHextorTypes;

{$R *.dfm}

{ TSettingsForm }

procedure TSettingsForm.ApplySettings;
var
  a: TArray<string>;
  cp: TArray<Integer>;
  i: Integer;
begin
  a := string(CBCodePages.Text).Split([' ', ',', ';'], TStringSplitOptions.ExcludeEmpty);
  SetLength(cp, Length(a));
  for i := 0 to Length(a) - 1 do
    cp[i] := StrToInt(a[i]);
  MainForm.Settings.AdditionalCodePages := cp;
  MainForm.Settings.Changed(True);
  UsedEncodings := MainForm.StandardCodePages + MainForm.Settings.AdditionalCodePages;

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
var
  s: string;
  i: Integer;
begin
  if UpdaterForm.Settings.CheckInterval = 0 then
    CBUpdateCheckInterval.ItemIndex := 2
  else
  if UpdaterForm.Settings.CheckInterval <= 7 then
    CBUpdateCheckInterval.ItemIndex := 0
  else
    CBUpdateCheckInterval.ItemIndex := 1;

  s := '';
  for i := 0 to Length(MainForm.Settings.AdditionalCodePages) - 1 do
    s := s + IntToStr(MainForm.Settings.AdditionalCodePages[i]) + ' ';
  CBCodePages.Text := s;
end;

end.
