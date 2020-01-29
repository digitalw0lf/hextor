unit uSetFileSizeForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  ColoredPanel, uDWHexTypes;

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
    ImageProxy1: TImageProxy;
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
    NewSize := StrToInt64Relative(EditNewSize.Text, OldSize);

    LblFillValue.Enabled := (NewSize > OldSize);
    EditFillValue.Enabled := (NewSize > OldSize);
  except
  end;
end;

end.
