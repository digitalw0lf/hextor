unit uAboutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls, Winapi.ShellAPI,

  uHextorTypes;

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
    procedure FormCreate(Sender: TObject);
    procedure LblUrlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  DateTimeToString(S, 'yyyy-mm-dd', GetAppBuildTime());
  LblBuildDate.Caption := 'Build: ' + S;
end;

procedure TAboutForm.LblUrlClick(Sender: TObject);
begin
  ShellExecute(0, '', PChar((Sender as TLabel).Hint), '', '', SW_SHOWNORMAL);
end;

end.
