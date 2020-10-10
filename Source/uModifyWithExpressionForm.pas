{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uModifyWithExpressionForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  uEditedData, uHextorTypes, uHextorGUI, uEditorForm, uOleAutoAPIWrapper,
  uActiveScript;

type
  TModifyWithExpressionForm = class(TForm)
    Label1: TLabel;
    CBElementType: TComboBox;
    Label2: TLabel;
    EditExpression: TComboBox;
    Label3: TLabel;
    EditPattern: TComboBox;
    BtnOk: TButton;
    BtnCancel: TButton;
    Memo1: TMemo;
    LblSizeWarning: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure CBElementTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FEditor: TEditorForm;
    Range: TFileRange;
    procedure ApplyExpr(var AData: TBytes; const Expression: string;
      const Pattern: TBytes; ElementSize: Integer; SrcAddr: TFilePointer);
  end;

var
  ModifyWithExpressionForm: TModifyWithExpressionForm;

implementation

uses
  uMainForm;

{$R *.dfm}

{ TModifyWithExpressionForm }

type
  [API]
  TFillExpressionVars = class
  private
    fx: Cardinal;
    fp: Cardinal;
    fi: Integer;
    fa: Int64;
  public
    property x: Cardinal read fx;
    property p: Cardinal read fp;
    property i: Integer read fi;
    property a: Int64 read fa;
  end;

function GetElement(const Buf: TBytes; ElemSize, ElemIndex: Integer): UInt64; inline;
begin
  Result := 0;
  Move(Buf[ElemIndex * ElemSize], Result, ElemSize);
end;

procedure PutElement(var Buf: TBytes; ElemSize, ElemIndex: Integer; Value: UInt64); inline;
begin
  Move(Value, Buf[ElemIndex * ElemSize], ElemSize);
end;

procedure TModifyWithExpressionForm.ApplyExpr(var AData: TBytes;
  const Expression: string; const Pattern: TBytes; ElementSize: Integer; SrcAddr: TFilePointer);
// Apply Expression to values in AData
var
  ScriptEngine: TActiveScript;  // Expression evaluator
  ScriptVars: TFillExpressionVars;
  ElemCount, PatternElemCount, i: Integer;
  x: Int64;
begin
  ElemCount := Length(AData) div ElementSize;
  PatternElemCount := Length(Pattern) div ElementSize;

  ScriptEngine := TActiveScript.Create(nil);
  ScriptVars := TFillExpressionVars.Create();
  Progress.TaskStart(Self);
  try
    // Container for built-in variables x, p, i, a
    ScriptEngine.AddObject('ScriptVars', MainForm.APIEnv.GetAPIWrapper(ScriptVars), True);

    for i:=0 to ElemCount-1 do
    begin
      // Original element value
      ScriptVars.fx := GetElement(AData, ElementSize, i);
      // Corresponding value from pattern
      if PatternElemCount > 0 then
        ScriptVars.fp := GetElement(Pattern, ElementSize, i mod PatternElemCount)
      else
        ScriptVars.fp := 0;
      ScriptVars.fi := i;
      ScriptVars.fa := SrcAddr + i * ElementSize;

      // Evaluate
      x := ScriptEngine.Eval(Expression);
      PutElement(AData, ElementSize, i, x);

      if i mod 10000 = 0 then
        Progress.Show(i+1, ElemCount);
    end;

  finally
    Progress.TaskEnd();
    FreeAndNil(ScriptEngine);
    MainForm.APIEnv.ObjectDestroyed(ScriptVars);
    ScriptVars.Free;
  end;

end;

procedure TModifyWithExpressionForm.BtnOkClick(Sender: TObject);
var
  AData: TBytes;
  Expression: string;
  Pattern: TBytes;
  ElemSize: Integer;
begin
  Expression := EditExpression.Text;
  AddComboBoxHistory(EditExpression);
  Pattern := Hex2Data(EditPattern.Text);
  AddComboBoxHistory(EditPattern);
  ElemSize := 1 shl CBElementType.ItemIndex;

  // Get original data
  AData := FEditor.Data.Get(Range.Start, Range.Size);

  // Apply expression
  ApplyExpr(AData, Expression, Pattern, ElemSize, Range.Start);

  // Put data back
  with FEditor do
  begin
    UndoStack.BeginAction('', 'x = ' + Expression);
    try
      Data.Change(Range.Start, Range.Size, Range.Size, @AData[0]);
    finally
      UndoStack.EndAction();
    end;
  end;

  ModalResult := mrOk;
end;

procedure TModifyWithExpressionForm.CBElementTypeChange(Sender: TObject);
var
  Expression: string;
  Pattern: TBytes;
  ElemSize: Integer;
  W: string;
begin
  // Size warning
  Expression := EditExpression.Text;
  Pattern := Hex2Data(EditPattern.Text);
  ElemSize := 1 shl CBElementType.ItemIndex;

  W := '';
  if (Expression.Contains('p')) and (Length(Pattern) = 0) then
    W := 'Your expression requires non-empty pattern'
  else
  if (Range.Size mod ElemSize <> 0) or (Length(Pattern) mod ElemSize <> 0) then
    W := 'Length of selection or pattern is not multiple of element size';

  LblSizeWarning.Caption := W;
end;

procedure TModifyWithExpressionForm.FormShow(Sender: TObject);
begin
  CBElementTypeChange(Sender);
end;

end.
