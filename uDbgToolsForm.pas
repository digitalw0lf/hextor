unit uDbgToolsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uProfilerFrame;

type
  TDbgToolsForm = class(TForm)
    ProfilerFrame1: TProfilerFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DbgToolsForm: TDbgToolsForm;

implementation

{$R *.dfm}

end.
