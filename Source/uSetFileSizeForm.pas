{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uSetFileSizeForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  uHextorGUI, uHextorTypes;

type
  TSetFileSizeForm = class(TForm)
    Label1: TLabel;
    EditOldSize: TEdit;
    Label2: TLabel;
    EditNewSize: TEdit;
    LblFillValue: TLabel;
    EditFillValue: TComboBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    ImageProxy1: THintedImageProxy;
    procedure EditNewSizeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SetFileSizeForm: TSetFileSizeForm;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TSetFileSizeForm.EditNewSizeChange(Sender: TObject);
var
  OldSize, NewSize: TFilePointer;
begin
  try
    OldSize := StrToInt64(EditOldSize.Text);
    NewSize := MainForm.ParseFilePointer(EditNewSize.Text, OldSize);

    LblFillValue.Enabled := (NewSize > OldSize);
    EditFillValue.Enabled := (NewSize > OldSize);
  except
  end;
end;

end.
