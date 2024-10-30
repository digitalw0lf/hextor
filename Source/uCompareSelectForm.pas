unit uCompareSelectForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uHextorGUI;

type
  TCompareSelectForm = class(TForm)
    Label3: TLabel;
    Label4: TLabel;
    ImageProxy1: THintedImageProxy;
    HintedImageProxy1: THintedImageProxy;
    BtnCompare: TButton;
    BtnCancel: TButton;
    CBSyncBlockSize: TComboBox;
    GBFile1: TGroupBox;
    LblRange1Start: TLabel;
    LblRange1End: TLabel;
    CBCmpEditor1: TComboBox;
    CBRange1: TCheckBox;
    EditRange1Start: TEdit;
    EditRange1End: TEdit;
    CBDetectInsertions: TCheckBox;
    GBFile2: TGroupBox;
    LblRange2Start: TLabel;
    LblRange2End: TLabel;
    CBCmpEditor2: TComboBox;
    CBRange2: TCheckBox;
    EditRange2Start: TEdit;
    EditRange2End: TEdit;
    procedure CBCmpEditor1Change(Sender: TObject);
    procedure CBRange1Click(Sender: TObject);
    procedure CBRange2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CompareSelectForm: TCompareSelectForm;

implementation

uses
  uHextorTypes, uMainForm, uEditorForm;

{$R *.dfm}

procedure TCompareSelectForm.CBCmpEditor1Change(Sender: TObject);
var
  Editor: TEditorForm;
  Range: TFileRange;
begin
  BtnCompare.Enabled := (CBCmpEditor1.ItemIndex <> CBCmpEditor2.ItemIndex);

  if (Sender as TComboBox).ItemIndex >= 0 then
  begin
    Editor := MainForm.Editors[(Sender as TComboBox).ItemIndex];
    Range := Editor.SelectedRange;
    if Sender = CBCmpEditor1 then
    begin
      EditRange1Start.Text := IntToStr(Range.Start);
      EditRange1End.Text := IntToStr(Range.AEnd);
    end
    else
    begin
      EditRange2Start.Text := IntToStr(Range.Start);
      EditRange2End.Text := IntToStr(Range.AEnd);
    end;
  end;
end;

procedure TCompareSelectForm.CBRange1Click(Sender: TObject);
begin
  LblRange1Start.Enabled := CBRange1.Checked;
  EditRange1Start.Enabled := CBRange1.Checked;
  LblRange1End.Enabled := CBRange1.Checked;
  EditRange1End.Enabled := CBRange1.Checked;
end;

procedure TCompareSelectForm.CBRange2Click(Sender: TObject);
begin
  LblRange2Start.Enabled := CBRange2.Checked;
  EditRange2Start.Enabled := CBRange2.Checked;
  LblRange2End.Enabled := CBRange2.Checked;
  EditRange2End.Enabled := CBRange2.Checked;
end;

end.
