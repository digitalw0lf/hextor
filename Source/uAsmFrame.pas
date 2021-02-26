{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uAsmFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter,
  SynHighlighterAsm, SynEdit, Vcl.ExtCtrls, Vcl.Buttons, Generics.Collections,
  Vcl.StdCtrls,

  Zydis, Zydis.Exception, Zydis.Decoder, Zydis.Formatter,

  uEditorForm, uHextorTypes, uEditedData, Vcl.Menus;

const
  Color_InstructionBg = $F0F8FC;
  Color_InstructionFr = $C0D0E0;
  Color_SelInstructionBg = $D0E8F0;
  Color_SelInstructionFr = $90A0B0;

type
  TAsmFrame = class(TFrame, IHextorToolFrame)
    ToolPanel: TPanel;
    SynEdit1: TSynEdit;
    SynAsmSyn1: TSynAsmSyn;
    BtnLockDisasm: TSpeedButton;
    CBArchitecture: TComboBox;
    DisasmPopupMenu: TPopupMenu;
    MIGoToAddr: TMenuItem;
    procedure CBArchitectureChange(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure BtnLockDisasmClick(Sender: TObject);
    procedure MIGoToAddrClick(Sender: TObject);
    procedure DisasmPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    FEditor: TEditorForm;
    FShownRange: TFileRange;
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorSelectionChanged(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure DataChanged(Sender: TEditedData; Addr: TFilePointer; OldSize, NewSize: TFilePointer; Value: PByteArray);
    function InstructionAtAddress(Addr: TFilePointer): Integer;
  public type
    TDisasmArchitecture = (daI386, daAMD64);
    TInstructionInFile = class
      Range: TFileRange;
      Decoded: Boolean;
      Instruction: TZydisDecodedInstruction;
    end;
  public
    { Public declarations }
    Instructions: TObjectList<TInstructionInFile>;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure UpdateInfo();
    procedure OnShown();
    function DisasmText(const Data: TBytes; StartAddr: TFilePointer; Architecture: TDisasmArchitecture): string;
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

{ TAsmFrame }

procedure TAsmFrame.BtnLockDisasmClick(Sender: TObject);
begin
  if not BtnLockDisasm.Down then
    UpdateInfo();
end;

procedure TAsmFrame.CBArchitectureChange(Sender: TObject);
begin
  UpdateInfo();
end;

constructor TAsmFrame.Create(AOwner: TComponent);
begin
  inherited;
  Instructions := TObjectList<TInstructionInFile>.Create(True);
  MainForm.OnSelectionChanged.Add(EditorSelectionChanged);
end;

procedure TAsmFrame.DataChanged(Sender: TEditedData; Addr, OldSize,
  NewSize: TFilePointer; Value: PByteArray);
begin
  if BtnLockDisasm.Down then
  begin
    AdjustPositionInData(FShownRange, Addr, OldSize, NewSize);
  end;
end;

destructor TAsmFrame.Destroy;
begin
  Instructions.Free;
  inherited;
end;

const
  ZydisErrorDescr: array[ZYDIS_STATUS_SUCCESS..ZYDIS_STATUS_IMPOSSIBLE_INSTRUCTION] of string = (
    'SUCCESS',
    'INVALID_PARAMETER',
    'INVALID_OPERATION',
    'INSUFFICIENT_BUFFER_SIZE',
    'NO_MORE_DATA',
    'DECODING_ERROR',
    'INSTRUCTION_TOO_LONG',
    'BAD_REGISTER',
    'ILLEGAL_LOCK',
    'ILLEGAL_LEGACY_PREFIX',
    'ILLEGAL_REX',
    'INVALID_MAP',
    'MALFORMED_EVEX',
    'MALFORMED_MVEX',
    'INVALID_MASK',
    'SKIP_OPERAND',
    'IMPOSSIBLE_INSTRUCTION');

function TAsmFrame.DisasmText(const Data: TBytes; StartAddr: TFilePointer; Architecture: TDisasmArchitecture): string;
var
  Formatter: Zydis.Formatter.TZydisFormatter;
  Decoder: Zydis.Decoder.TZydisDecoder;
  InstructionPointer: ZydisU64;
  Offset: Integer;
  Instruction: TInstructionInFile;//TZydisDecodedInstruction;
begin
  Result := '';
  Instructions.Clear();

  if (ZydisGetVersion <> ZYDIS_VERSION) then
  begin
    raise Exception.Create('Invalid Zydis version');
  end;

  Formatter := Zydis.Formatter.TZydisFormatter.Create(ZYDIS_FORMATTER_STYLE_INTEL);
  try
    Formatter.ForceMemorySegments := true;
    Formatter.ForceMemorySize := true;
    case Architecture of
      daI386:
        begin
          Decoder := Zydis.Decoder.TZydisDecoder.Create(ZYDIS_MACHINE_MODE_LONG_COMPAT_32, ZYDIS_ADDRESS_WIDTH_32);
          InstructionPointer := StartAddr; //$00400000;
        end;
      daAMD64:
        begin
          Decoder := Zydis.Decoder.TZydisDecoder.Create(ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_ADDRESS_WIDTH_64);
          InstructionPointer := StartAddr; //$007FFFFFFF400000;
        end
      else Exit('');
    end;
    try
      Offset := 0;
      // Decode instructions in Data
      while Offset < Length(Data) do
      begin
        Instruction := TInstructionInFile.Create();
        try
          Decoder.DecodeBuffer(@Data[Offset], Length(Data) - Offset, InstructionPointer,
            Instruction.Instruction);
        except
          on E: TZydisException do
          begin
            Result := Result + '; db ' + IntToHex(Data[Offset], 2) + 'h ; ' + ZydisErrorDescr[E.Status] + sLineBreak;
            Instruction.Range := TFileRange.Create(StartAddr + Offset, StartAddr + Offset + 1);
            Instruction.Decoded := False;
            Instruction.Instruction := Default(TZydisDecodedInstruction);
            Instructions.Add(Instruction);
            Inc(InstructionPointer, 1);
            Inc(Offset, 1);
            Continue;
          end;
        end;

        Result := Result + Formatter.FormatInstruction(Instruction.Instruction) + sLineBreak;
        Instruction.Range := TFileRange.Create(StartAddr + Offset, StartAddr + Offset + Instruction.Instruction.Length);
        Instruction.Decoded := True;
        Instructions.Add(Instruction);
        Inc(InstructionPointer, Instruction.Instruction.Length);
        Inc(Offset, Instruction.Instruction.Length);
      end;

    finally
      Decoder.Free;
    end;
  finally
    Formatter.Free;
  end;

end;

procedure TAsmFrame.EditorClosed(Sender: TEditorForm);
begin
  FEditor := nil;
  SynEdit1.Lines.Clear();
end;

procedure TAsmFrame.EditorGetTaggedRegions(Editor: TEditorForm; Start,
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  i, HLInstr: Integer;
  Bg, Fr: TColor;
begin
  if not Parent.Visible then Exit;

  if Screen.ActiveControl = SynEdit1 then
    HLInstr := SynEdit1.CaretY - 1
  else
    HLInstr := -1;

  for i:=0 to Instructions.Count-1 do
    if (Instructions[i].Decoded) or (i = HLInstr) then
    begin
      if i = HLInstr then
      begin
        Bg := Color_SelInstructionBg;
        Fr := Color_SelInstructionFr;
      end
      else
      begin
        Bg := Color_InstructionBg;
        Fr := Color_InstructionFr;
      end;
      Regions.AddRegion(Self, Instructions[i].Range.Start, Instructions[i].Range.AEnd, clNone, Bg, Fr);
    end;
end;

procedure TAsmFrame.EditorSelectionChanged(Sender: TEditorForm);
begin
  if not Parent.Visible then Exit;
  UpdateInfo();
end;

function TAsmFrame.InstructionAtAddress(Addr: TFilePointer): Integer;
var
  i: Integer;
begin
  for i:=0 to Instructions.Count-1 do
    if Instructions[i].Range.Intersects(Addr) then
      Exit(i);
  Result := -1;
end;

procedure TAsmFrame.MIGoToAddrClick(Sender: TObject);
begin
  FEditor.MoveCaret(MIGoToAddr.Tag, []);
end;

procedure TAsmFrame.OnShown;
begin
  UpdateInfo();
end;

procedure TAsmFrame.DisasmPopupMenuPopup(Sender: TObject);
var
  s: string;
  Addr: Int64;
begin
  s := SynEdit1.WordAtCursor;
  if (TryStrToInt64(s, Addr)) and (Addr >= 0) and (Addr <= FEditor.Data.GetSize()) then
  begin
    MIGoToAddr.Tag := Addr;
    MIGoToAddr.Caption := 'Go to ' + s;
    MIGoToAddr.Visible := True;
  end
  else
  begin
    MIGoToAddr.Visible := False;
  end;

end;

procedure TAsmFrame.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TAsmFrame.UpdateInfo;
// Show selection/data under cursor as values
var
  AData: TBytes;
  AActiveEditor: TEditorForm;
  UnderCaret: Integer;
begin
  // Link to active editor
  AActiveEditor := MainForm.GetActiveEditorNoEx();
  if FEditor <> AActiveEditor then
  begin
    if Assigned(FEditor) then
    begin
      FEditor.OnGetTaggedRegions.Remove(Self);
      FEditor.OnClosed.Remove(Self);
      FEditor.Data.OnDataChanged.Remove(Self);
    end;

    FEditor := AActiveEditor;
    if FEditor <> nil then
    begin
      FEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
      FEditor.OnClosed.Add(EditorClosed, Self);
      FEditor.Data.OnDataChanged.Add(DataChanged, Self);
    end
    else
    begin
      EditorClosed(nil);
      Exit;
    end;
  end;
  if FEditor = nil then Exit;

  with FEditor do
  begin
    if BtnLockDisasm.Down then
    begin
      AData := GetEditedData(FShownRange.Start, FShownRange.Size);
    end
    else
    begin
      AData := GetSelectedOrAfterCaret(64, 65536, FShownRange.Start, True);
      FShownRange.Size := Length(AData);
    end;

    try
      SynEdit1.Text := DisasmText(AData, FShownRange.Start, TDisasmArchitecture(CBArchitecture.ItemIndex));
    except
      on E: Exception do
        SynEdit1.Text := E.ClassName + ': ' + sLineBreak + E.Message;
    end;

    if BtnLockDisasm.Down then
    // If disasm range locked, highlight instruction in disasm when moving caret in editor
    begin
      UnderCaret := InstructionAtAddress(CaretPos);
      if UnderCaret >= 0 then
        SynEdit1.CaretY := UnderCaret + 1;
    end;
  end;
end;

end.
