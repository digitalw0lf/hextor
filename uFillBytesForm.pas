unit uFillBytesForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, Vcl.ComCtrls, uHextorGUI;

type
  TFillBytesForm = class(TForm)
    TabControl1: TTabControl;
    EditPattern: TComboBox;
    RBPattern: TRadioButton;
    RBRandomBytes: TRadioButton;
    EditRandomMin: TSpinEdit;
    EditRandomMax: TSpinEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    LblCount: TLabel;
    EditCount: TEdit;
    RBExpression: TRadioButton;
    EditExpression: TComboBox;
    ImageProxy1: THintedImageProxy;
    procedure TabControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    FillEnabled: Boolean;
  end;

var
  FillBytesForm: TFillBytesForm;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TFillBytesForm.FormShow(Sender: TObject);
begin
  if FillEnabled then
  begin
    TabControl1.TabIndex := 1;
  end
  else
  begin
    TabControl1.TabIndex := 0;
  end;
  TabControl1Change(Sender);
end;

procedure TFillBytesForm.TabControl1Change(Sender: TObject);
begin
  LblCount.Visible := (TabControl1.TabIndex = 0);
  EditCount.Visible := (TabControl1.TabIndex = 0);
end;

procedure TFillBytesForm.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  if not FillEnabled then AllowChange := False;
end;

end.
