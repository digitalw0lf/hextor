{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uFillBytesForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, Vcl.ComCtrls, StrUtils,

  uHextorGUI, uHextorTypes, uEditorForm;

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
    procedure TabControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FillEnabled: Boolean;
    FEditor: TEditorForm;
    FInsertPos: TFilePointer;
    Range: TFileRange;
  end;

var
  FillBytesForm: TFillBytesForm;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TFillBytesForm.BtnOKClick(Sender: TObject);
var
  Insert: Boolean;
  Addr: TFilePointer;
  Size: Integer;
  AData, Pattern: TBytes;
  Rnd1, Rnd2: Integer;
  i: Integer;
begin
  Progress.TaskStart(Self);
  try
    Insert := (TabControl1.TabIndex = 0);
    if Insert then
    begin
      Addr := FInsertPos;
      Size := StrToInt(EditCount.Text);
    end
    else
    begin
      Addr := Range.Start;
      Size := Range.Size;
    end;

  //    StartTimeMeasure();
    if RBPattern.Checked then
    // Pattern
    begin
      Pattern := Hex2Data(EditPattern.Text);
      SetLength(AData, Size);
      if Length(Pattern) = 0 then
        raise EInvalidUserInput.Create('Specify hex pattern');
      if Length(Pattern) = 1 then
        FillChar(AData[0], Size, Pattern[0])
      else
        for i:=0 to Size-1 do
          AData[i] := Pattern[i mod Length(Pattern)];
    end
    else
    if RBRandomBytes.Checked then
    // Random bytes
    begin
      SetLength(AData, Size);
      Rnd1 := EditRandomMin.Value;
      Rnd2 := EditRandomMax.Value;
      for i:=0 to Size-1 do
        AData[i] := Rnd1 + Random(Rnd2 - Rnd1 + 1);
    end
    else Exit;
  //    EndTimeMeasure('Fill', True);

    with FEditor do
    begin
      UndoStack.BeginAction('', IfThen(Insert, 'Insert bytes', 'Fill selection'));
      try

        if Insert then
          Data.Insert(Addr, Size, @AData[0])
        else
          Data.Change(Addr, Size, @AData[0]);

      finally
        UndoStack.EndAction();
      end;
    end;
  finally
    Progress.TaskEnd();
  end;

  ModalResult := mrOk;
end;

procedure TFillBytesForm.FormShow(Sender: TObject);
begin
  FillEnabled := (Range.Size > 0);

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
