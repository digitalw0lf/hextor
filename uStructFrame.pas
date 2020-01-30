unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Types, Math, SynEdit, SynEditHighlighter,
  SynHighlighterCpp, superobject, Clipbrd,

  uUtil, uDWHexTypes, uLogFile, ColoredPanel, uEditorForm, uValueInterpretors,
  uDataStruct, VirtualTrees;

type
  TDSTreeNode = record
    Caption: string;
    DSField: TDSField;
  end;
  PDSTreeNode = ^TDSTreeNode;

  TStructFrame = class(TFrame)
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
    DSTreeView: TVirtualStringTree;
    BtnCopyValue: TButton;
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
    procedure EditFieldValueExit(Sender: TObject);
    procedure EditFieldValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DSTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DSTreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DSTreeViewBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure DSTreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DSTreeViewNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure DSTreeViewEnter(Sender: TObject);
    procedure DSTreeViewExit(Sender: TObject);
    procedure DSTreeViewGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure BtnCopyValueClick(Sender: TObject);
  private
    { Private declarations }
    FParser: TDSParser;
    FInterpretor: TDSInterpretor;
    MPos: TPoint;
    ShownDS: TDSField;
    FEditor: TEditorForm;
    EditedNode: PVirtualNode;
    EditedDS: TDSSimpleField;
    procedure ShowStructTree(DS: TDSField; ParentNode: PVirtualNode);
    procedure ExpandToNode(Node: PVirtualNode);
    function DSSaveFolder(): string;
    function GetNodeDS(Node: PVirtualNode): TDSField;
    procedure EditorClosed(Sender: TEditorForm);
    function DSValueAsJsonObject(DS: TDSField): ISuperObject;
    function DSValueAsJson(DS: TDSField): string;
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
  DSTreeView.Clear();
  FreeAndNil(ShownDS);
  BtnCopyValue.Enabled := False;

  // Parse structure description
  ShownDS := FParser.ParseStruct(Struct);

  // Populate structure fields
  FInterpretor.OnGetMoreData := procedure (AAddr, ASize: TFilePointer; var Data: TBytes{; var AEndOfData: Boolean})
    begin
      Data := FEditor.EditedData.Get(AAddr, ASize);
    end;
  FInterpretor.Interpret(ShownDS, Addr, Size);

  // Show tree
  DSTreeView.BeginUpdate();
  try
    with TDSCompoundField(ShownDS) do
      for i:=0 to Fields.Count-1 do
        ShowStructTree(Fields[i], nil);

    DSTreeView.IterateSubtree(nil,
      procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
      begin
        // Expand top-level nodes
        if (Node.Parent = DSTreeView.RootNode) and (DSTreeView.ChildCount[Node] < 30) then
          DSTreeView.Expanded[Node] := True;
        // Expand nodes with errors
        DS := GetNodeDS(Node);
        if (DS <> nil) and (DS is TDSSimpleField) and ((DS as TDSSimpleField).ErrorText <> '') then
          ExpandToNode(Node);

      end,
      nil);

  finally
    DSTreeView.EndUpdate();
    MainForm.OperationDone(Self);
  end;

  BtnCopyValue.Enabled := True;
end;

function TStructFrame.GetDataColors(Editor: TEditorForm; Addr: TFilePointer; Size: Integer;
  Data: PByteArray; var TxColors, BgColors: TColorArray): Boolean;
var
//  Node: TTreeNode;
  Node: PVirtualNode;
  DS: TDSField;
begin
  Result := False;
  if Screen.ActiveControl <> DSTreeView then Exit;
  if Editor <> FEditor then Exit;

  Node := DSTreeView.FocusedNode;
//  Node := DSTreeView.Selected;
  if Node = nil then Exit;
  DS := PDSTreeNode(DSTreeView.GetNodeData(Node)).DSField;

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

procedure TStructFrame.BtnCopyValueClick(Sender: TObject);
begin
  if ShownDS <> nil then
    Clipboard.AsText := DSValueAsJson(ShownDS);
end;

procedure TStructFrame.BtnInterpretClick(Sender: TObject);
var
  Addr, Size: TFilePointer;
begin
  if Assigned(FEditor) then FEditor.OnClosed.Remove(EditorClosed);
  FEditor := MainForm.ActiveEditor;
  FEditor.OnClosed.Add(EditorClosed);

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

  DSTreeView.NodeDataSize := SizeOf(TDSTreeNode);
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

procedure TStructFrame.DSTreeViewBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  DS: TDSField;
begin
  DS := GetNodeDS(Node);
  // Red background for fields with invalid values
  if (DS is TDSSimpleField) and
     ((DS as TDSSimpleField).ErrorText <> '') then
    ItemColor := clRed;
end;

procedure TStructFrame.DSTreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if FEditor = nil then Exit;
  FEditor.BeginUpdatePanes();
  try
    if EditFieldValue.Visible then
      EditFieldValueExit(Sender);
    if (GetNodeDS(Node) <> nil) then
      FEditor.ScrollToShow(GetNodeDS(Node).BufAddr, -1, -1);
    FEditor.UpdatePanes();
  finally
    FEditor.EndUpdatePanes();
  end;
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

procedure TStructFrame.DSTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PDSTreeNode(Sender.GetNodeData(Node))^);
end;

procedure TStructFrame.DSTreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  DS: TDSField;
begin
  DS := GetNodeDS(Node);
  if (DS <> nil) and (DS is TDSSimpleField) then
     HintText := (DS as TDSSimpleField).ErrorText
  else
    HintText := '';
end;

procedure TStructFrame.DSTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PDSTreeNode;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data.Caption;
end;

procedure TStructFrame.DSTreeViewNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
// Edit value
var
  Node: PVirtualNode;
  DS: TDSField;
  R: TRect;
  w: Integer;
begin
  DS := GetNodeDS(HitInfo.HitNode); //DSTreeView.FocusedNode);
  if (DS = nil) or (not (DS is TDSSimpleField)) then Exit;

  EditedNode := HitInfo.HitNode; //DSTreeView.FocusedNode;
  EditedDS := TDSSimpleField(DS);
  EditFieldValue.Text := DS.ToString();
  EditFieldValue.Modified := False;

  Node := HitInfo.HitNode; //DSTreeView.FocusedNode;
  R := DSTreeView.GetDisplayRect(Node, -1, True);
//  R := Node.DisplayRect(True);
  w := DSTreeView.Canvas.TextWidth(DS.Name + ': ');
  EditFieldValue.Parent := DSTreeView;
  EditFieldValue.SetBounds(R.Left + w, R.Top - 2, R.Width - w + 30, EditFieldValue.Height);

  EditFieldValue.Show;
  EditFieldValue.SetFocus();

end;

function TStructFrame.DSValueAsJson(DS: TDSField): string;
var
  json: ISuperObject;
begin
  json := DSValueAsJsonObject(DS);
  Result := json.AsJSon(True, False);
end;

function TStructFrame.DSValueAsJsonObject(DS: TDSField): ISuperObject;
var
  i: Integer;
  Intr: TValueInterpretor;
  x: Variant;
begin
  Result := nil;
  if DS is TDSArray then
  begin
    Result := SA([]);
    for i:=0 to (DS as TDSArray).Fields.Count-1 do
      Result.AsArray.Add(DSValueAsJsonObject((DS as TDSArray).Fields[i]));
  end
  else
  if DS is TDSCompoundField then
  begin
    Result := SO();
    for i:=0 to (DS as TDSCompoundField).Fields.Count-1 do
      (Result as TSuperObject).O[(DS as TDSCompoundField).Fields[i].Name] :=
        DSValueAsJsonObject((DS as TDSCompoundField).Fields[i]);
  end
  else
  if DS is TDSSimpleField then
  begin
    Intr := (DS as TDSSimpleField).GetInterpretor(False);
    if Intr <> nil then
    begin
      x := Intr.ToVariant((DS as TDSSimpleField).Data[0], Length((DS as TDSSimpleField).Data));
      if VarIsOrdinal(x) then Result := TSuperObject.Create(Int64(x))
      else
      if VarIsFloat(x) then Result := TSuperObject.Create(Double(x))
      else
      if VarIsStr(x) then Result := TSuperObject.Create(string(x));
    end;
  end;
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
      //EditedNode.Text := DS.Name + ': ' + DS.ToString();
      PDSTreeNode(DSTreeView.GetNodeData(EditedNode)).Caption := DS.Name + ': ' + DS.ToString();
      DSTreeView.InvalidateNode(EditedNode);
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

procedure TStructFrame.EditorClosed(Sender: TEditorForm);
begin
  DSTreeView.Clear();
  FreeAndNil(ShownDS);
  FEditor := nil;
  BtnCopyValue.Enabled := False;
end;

procedure TStructFrame.ExpandToNode(Node: PVirtualNode);
begin
  while Node <> DSTreeView.RootNode do
  begin
    DSTreeView.Expanded[Node] := True;
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

function TStructFrame.GetNodeDS(Node: PVirtualNode): TDSField;
//var
//  Node: PVirtualNode;
begin
//  Node := DSTreeView.FocusedNode;
  if Node = nil then Exit(nil);
  Result := PDSTreeNode(DSTreeView.GetNodeData(Node)).DSField;
end;

procedure TStructFrame.ShowStructTree(DS: TDSField; ParentNode: PVirtualNode);
// Recursively show given DataStructure inside tree node
var
//  Node: TTreeNode;
  Node: PVirtualNode;
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
    Node := DSTreeView.AddChild(ParentNode);
    PDSTreeNode(Node.GetData).Caption := S;
    PDSTreeNode(Node.GetData).DSField := DS;

//    Node := DSTreeView.Items.AddChild(ParentNode, S);
//    Node.Data := DS;
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
