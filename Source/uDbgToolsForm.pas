{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDbgToolsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls
  {$IFDEF USE_DWF_UTILS}, uProfilerFrame{$ENDIF}
  ;

type
  TDbgToolsForm = class(TForm)
    LblUpdateTime: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF USE_DWF_UTILS}
    ProfilerFrame1: TProfilerFrame;
    {$ENDIF}
  end;

var
  DbgToolsForm: TDbgToolsForm;

implementation

{$R *.dfm}

procedure TDbgToolsForm.FormCreate(Sender: TObject);
begin
  {$IFDEF USE_DWF_UTILS}
  ProfilerFrame1 := TProfilerFrame.Create(Self);
  ProfilerFrame1.Parent := Self;
  {$ENDIF}
end;

end.
