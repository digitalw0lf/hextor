{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uAboutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls, Winapi.ShellAPI,

  uHextorTypes, uHextorGUI;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    LblBuildDate: TLabel;
    LblUrl: TLabel;
    Label4: TLabel;
    LicenseMemo: TMemo;
    LblLicense: TLabel;
    LblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LblUrlClick(Sender: TObject);
    procedure LblLicenseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TAboutForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  S := AppVersion;
  LblVersion.Caption := 'Version: ' + S;
  DateTimeToString(S, 'yyyy-mm-dd', GetAppBuildTime());
  LblBuildDate.Caption := 'Build: ' + S;
end;

procedure TAboutForm.LblLicenseClick(Sender: TObject);
begin
  with MakeFormWithContent(LicenseMemo, bsDialog, 'Hextor License Information') do
    ShowModal();
end;

procedure TAboutForm.LblUrlClick(Sender: TObject);
begin
  ShellExecute(0, '', PChar((Sender as TLabel).Hint), '', '', SW_SHOWNORMAL);
end;

end.
