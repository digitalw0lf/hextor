unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Types, Math, SynEdit, SynEditHighlighter,
  SynHighlighterCpp,

  uUtil, uDWHexTypes, uLogFile, ColoredPanel, uEditorForm, uValueInterpretors,
  uDataStruct;

type
  TStructFrame = class(TFrame)
    DSTreeView: TTreeView;
    PnlButtonBar1: TPanel;
    BtnLoadDescr: TSpeedButton;
    BtnSaveDescr: TSpeedButton;
    LblStructName: TLabel;
    PnlButtonBar2: TPanel;
    BtnInterpret: TButton;
    SavedDescrsMenu: TPopupMenu;
    MIDummyDataStruct: TMenuItem;
    SaveDialog1: TSaveDialog;
    EditFieldValue: TEdit;
    DSDescrEdit: TSynEdit;
    SynCppSyn1: TSynCppSyn;
    procedure BtnInterpretClick(Sender: TObject);
    procedure BtnLoadDescrClick(Sender: TObject);
    procedure MIDummyDataStructClick(Sender: TObject);
    procedure BtnSaveDescrClick(Sender: TObject);
    procedure PnlButtonBar2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PnlButtonBar2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PnlButtonBar2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure DSTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure DSTreeViewEnter(Sender: TObject);
    procedure DSTreeViewExit(Sender: TObject);
    procedure DSTreeViewDblClick(Sender: TObject);
    procedure EditFieldValueExit(Sender: TObject);
    procedure EditFieldValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DSTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure DSTreeViewHint(Sender: TObject; const Node: TTreeNode;
      var Hint: string);
    procedure DSTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FParser: TDSParser;
    FInterpretor: TDSInterpretor;
    MPos: TPoint;
    ShownDS: TDSField;
    FEditor: TEditorForm;
    EditedNode: TTreeNode;
    EditedDS: TDSSimpleField;
    procedure ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
    procedure ExpandToNode(Node: TTreeNode);
    function DSSaveFolder(): string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Analyze(Addr, Size: TFilePointer; const Struct: string);
    function GetDataColors(Editor: TEditorForm; Addr: TFilePointer;
      Size: Integer; Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
  end;

implementation

uses
  uValueFrame, uMainForm;

{$R *.dfm}

{ TStructFrame }

procedure TStructFrame.Analyze(Addr, Size: TFilePointer; {const Data: TBytes;}
  const Struct: string);
var
  i: Integer;
  DS: TDSField;
begin
  DSTreeView.Items.Clear();
  FreeAndNil(ShownDS);

  // Parse structure description
  ShownDS := FParser.ParseStruct(Struct);

  // Populate structure fields
  FInterpretor.OnGetMoreData := procedure (AAddr, ASize: TFilePointer; var Data: TBytes{; var AEndOfData: Boolean})
    begin
      Data := FEditor.EditedData.Get(AAddr, ASize);
    end;
  FInterpretor.Interpret(ShownDS, Addr, Size);

  // Show tree
  // TODO: replace with faster tree view (VirtualTreeView?)
  DSTreeView.Items.BeginUpdate();
  try
    with TDSCompoundField(ShownDS) do
      for i:=0 to Fields.Count-1 do
        ShowStructTree(Fields[i], nil);

    for i:=0 to DSTreeView.Items.Count-1 do
    begin
      // Expand top-level nodes
      if (DSTreeView.Items[i].Parent = nil) and (DSTreeView.Items[i].Count < 30) then
        DSTreeView.Items[i].Expand(False);
      // Expand nodes with errors
      DS := DSTreeView.Items[i].Data;
      if (DS <> nil) and (DS is TDSSimpleField) and ((DS as TDSSimpleField).ErrorText <> '') then
        ExpandToNode(DSTreeView.Items[i]);
      MainForm.ShowProgress(Self, i+1, DSTreeView.Items.Count, 'Expand');
    end;
  finally
    DSTreeView.Items.EndUpdate();
    MainForm.OperationDone(Self);
  end;
end;

function TStructFrame.GetDataColors(Editor: TEditorForm; Addr: TFilePointer; Size: Integer;
  Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
var
  Node: TTreeNode;
  DS: TDSField;
begin
  Result := False;
  if Screen.ActiveControl <> DSTreeView then Exit;
  if Editor <> FEditor then Exit;

  Node := DSTreeView.Selected;
  if Node = nil then Exit;
  DS := Node.Data;

  Result := FillRangeInColorArray(BgColors, Addr,
    DS.BufAddr, DS.BufAddr + DS.BufSize, Color_ValueHighlightBg);
end;

procedure TStructFrame.BtnLoadDescrClick(Sender: TObject);
var
  fl: TStringList;
  i: Integer;
  mi: TMenuItem;
begin
  SavedDescrsMenu.Items.Clear();

  fl := EnumFiles(DSSaveFolder(), faAnyFile, '*.ds', []);
  try
    for i:=0 to fl.Count-1 do
    begin
      mi := TMenuItem.Create(Application);
      mi.Caption := ChangeFileExt(fl[i], '');
      mi.OnClick := MIDummyDataStructClick;
      SavedDescrsMenu.Items.Add(mi);
    end;
  finally
    fl.Free;
  end;

  PopupFromControl(SavedDescrsMenu, BtnLoadDescr);
end;

procedure TStructFrame.BtnInterpretClick(Sender: TObject);
var
  Addr, Size: TFilePointer;
begin
  FEditor := MainForm.ActiveEditor;
  with FEditor do
  begin
    if SelStart = GetFileSize() then
    // Cursor at end of file -> parse entire file
    begin
      Addr := 0;
      Size := EditedData.GetSize();
    end
    else
    if SelLength > 0 then
    // Non-empty selection -> parse it
    begin
      Addr := SelStart;
      Size := SelLength;
    end
    else
    // Parce from cursor until end of file
    begin
      Addr := SelStart;
      Size := EditedData.GetSize() - Addr;
    end;
  end;

  Analyze(Addr, Size, DSDescrEdit.Text);
end;

constructor TStructFrame.Create(AOwner: TComponent);
begin
  inherited;
  FParser := TDSParser.Create();
  FInterpretor := TDSInterpretor.Create();
  FInterpretor.OnProgress.Add(MainForm.ShowProgress);
  FInterpretor.OnOperationDone.Add(MainForm.OperationDone);
end;

destructor TStructFrame.Destroy;
begin
  ShownDS.Free;
  FParser.Free;
  FInterpretor.Free;
  inherited;
end;

function TStructFrame.DSSaveFolder: string;
begin
  Result := MainForm.SettingsFolder + 'DataStruct\';
end;

procedure TStructFrame.DSTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  DS: TDSField;
begin
  DS := Node.Data;
  if Stage = cdPrePaint then
  begin
    // Red background for fields with invalid values
    if (DS is TDSSimpleField) and
       ((DS as TDSSimpleField).ErrorText <> '') then
      Sender.Canvas.Brush.Color := clRed;
  end;
end;

procedure TStructFrame.DSTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if FEditor = nil then Exit;
  FEditor.BeginUpdatePanes();
  try
    if EditFieldValue.Visible then
      EditFieldValueExit(Sender);
    if (Node.Data <> nil) then
      FEditor.ScrollToShow(TDSField(Node.Data).BufAddr, -1, -1);
    FEditor.UpdatePanes();
  finally
    FEditor.EndUpdatePanes();
  end;
end;

procedure TStructFrame.DSTreeViewDblClick(Sender: TObject);
// Edit value
var
  Node: TTreeNode;
  DS: TDSField;
  R: TRect;
  w: Integer;
begin
  Node := DSTreeView.Selected;
  if Node = nil then Exit;
  DS := Node.Data;
  if (DS = nil) or (not (DS is TDSSimpleField)) then Exit;

  EditedNode := Node;
  EditedDS := TDSSimpleField(DS);
  EditFieldValue.Text := DS.ToString();
  EditFieldValue.Modified := False;

  R := Node.DisplayRect(True);
  w := DSTreeView.Canvas.TextWidth(DS.Name + ': ');
  EditFieldValue.Parent := DSTreeView;
  EditFieldValue.SetBounds(R.Left + w, R.Top - 2, R.Width - w + 30, EditFieldValue.Height);

  EditFieldValue.Show;
  EditFieldValue.SetFocus();
end;

procedure TStructFrame.DSTreeViewEnter(Sender: TObject);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewExit(Sender: TObject);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TStructFrame.DSTreeViewHint(Sender: TObject; const Node: TTreeNode;
  var Hint: string);
var
  DS: TDSField;
begin
  DS := Node.Data;
  if (DS <> nil) and (DS is TDSSimpleField) then
     Hint := (DS as TDSSimpleField).ErrorText
  else
    Hint := '';
end;

procedure TStructFrame.DSTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (not (ssDouble in Shift)) and (EditFieldValue.Visible) then
    EditFieldValueExit(Sender);
end;

procedure TStructFrame.EditFieldValueExit(Sender: TObject);
// Apply changed field value
var
  DS: TDSSimpleField;
begin
  if FEditor = nil then Exit;
  if EditFieldValue.Modified then
  begin
    DS := EditedDS;
    if DS <> nil then
    begin
      DS.SetFromString(EditFieldValue.Text);
      FEditor.EditedData.Change(DS.BufAddr, DS.BufSize, @DS.Data[0]);
      EditedNode.Text := DS.Name + ': ' + DS.ToString();
    end;
  end;
  // Hide editor
  EditFieldValue.Hide();
  if EditFieldValue.Focused then
    DSTreeView.SetFocus();
end;

procedure TStructFrame.EditFieldValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditFieldValueExit(Sender);

  if Key = VK_ESCAPE then
  begin
    EditFieldValue.Modified := False;
    EditFieldValueExit(Sender);
  end;
end;

procedure TStructFrame.ExpandToNode(Node: TTreeNode);
begin
  while Node <> nil do
  begin
    Node.Expand(False);
    Node := Node.Parent;
  end;
end;

procedure TStructFrame.FrameResize(Sender: TObject);
begin
  DSDescrEdit.Constraints.MaxHeight := DSDescrEdit.Height + DSTreeView.Height - 20;
end;

procedure TStructFrame.MIDummyDataStructClick(Sender: TObject);
var
  fn: string;
begin
  fn := (Sender as TMenuItem).Caption;
  DSDescrEdit.Lines.LoadFromFile(DSSaveFolder + fn + '.ds');
  LblStructName.Caption := fn;
end;

procedure TStructFrame.PnlButtonBar2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MPos := Point(X, Y);
end;

procedure TStructFrame.PnlButtonBar2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (MPos.X >= 0) then
  begin
    DSDescrEdit.Height := DSDescrEdit.Height + (Y - MPos.Y);
  end;
end;

procedure TStructFrame.PnlButtonBar2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MPos := Point(-1, -1);
end;

procedure TStructFrame.ShowStructTree(DS: TDSField; ParentNode: TTreeNode);
// Recursively show given DataStructure inside tree node
var
  Node: TTreeNode;
  S: string;
  i: Integer;
begin
  MainForm.ShowProgress(Self, DS.BufAddr - ShownDS.BufAddr, ShownDS.BufSize, 'Showing tree');

  // Field name
  S := DS.Name;

  // For array: show length
  if DS is TDSArray then
  with TDSArray(DS) do
    S := S + '[' + IntToStr(Fields.Count) + ']';

  // Show value
  S := S + ': ' + DS.ToString();

  if (S <> ''){ or (ParentNode = nil)} then
  begin
    Node := DSTreeView.Items.AddChild(ParentNode, S);
    Node.Data := DS;
  end
  else  // Don't create separate nodes for nameless fields - e.g. conditional statements
    Node := ParentNode;

  // For compound field: recursively show fields
  if DS is TDSCompoundField then
  with TDSCompoundField(DS) do
  begin
    for i:=0 to Fields.Count-1 do
    begin
      ShowStructTree(Fields[i], Node);
    end;
  end;
end;

procedure TStructFrame.BtnSaveDescrClick(Sender: TObject);
var
  fn: string;
begin
  ForceDirectories(DSSaveFolder());
  fn := LblStructName.Caption;
  SaveDialog1.InitialDir := DSSaveFolder();
  SaveDialog1.FileName := fn + '.ds';

  if not SaveDialog1.Execute() then Exit;
  fn := SaveDialog1.FileName;

  ForceDirectories(ExtractFilePath(fn));
  DSDescrEdit.Lines.SaveToFile(fn);
  DSDescrEdit.MarkModifiedLinesAsSaved();
  LblStructName.Caption := ChangeFileExt(ExtractFileName(fn), '');
end;

end.
