unit uFileSplitForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.AppEvnts,
  Winapi.ShellAPI, uHextorGUI;

type
  TFileSplitForm = class(TForm)
    Label1: TLabel;
    EditSourceFile: TEdit;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    EditTargetFolder: TComboBox;
    Label3: TLabel;
    EditTargetName: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    DropFileCatcher1: TDropFileCatcher;
    procedure DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
      Control: TWinControl; Files: TStrings; DropPoint: TPoint);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileSplitForm: TFileSplitForm;

implementation

{$R *.dfm}

procedure TFileSplitForm.DropFileCatcher1DropFiles(Sender: TDropFileCatcher;
  Control: TWinControl; Files: TStrings; DropPoint: TPoint);
begin
  if Files.Count >= 1 then
    EditSourceFile.Text := Files[0];
end;

end.
