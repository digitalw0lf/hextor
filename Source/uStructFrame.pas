{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uStructFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Types, Math, SynEdit, SynEditHighlighter,
  SynHighlighterCpp, superobject, Clipbrd, VirtualTrees, System.IOUtils,
  Vcl.ToolWin, Winapi.ShellAPI,

  uHextorTypes, uHextorGUI, {uLogFile,} uEditorForm, uValueInterpretors,
  uDataStruct, uEditedData, uCallbackList, uModuleSettings;

const
  Color_DSFieldBg = $FFF8F8;
  Color_DSFieldFr = $F8E0E0;
  Color_ErrDSFieldBg = $B090FF;
  Color_SelDSFieldBg = $FFD0D0;
  Color_SelDSFieldFr = $E0A0A0;

  ImageIndex_Folder = 23;  // Index in  MainForm.ImageList16

type
  TStructInterpretRange = (irFile, irSelection);

  TStructSettings = class (TModuleSettings)
    Range: TStructInterpretRange;
  end;

  TDSTreeNode = record
    Caption: string;
    DSField: TDSField;
  end;
  PDSTreeNode = ^TDSTreeNode;

  TStructFrame = class(TFrame)
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
    ToolBar1: TToolBar;
    BtnNewDescr: TToolButton;
    BtnLoadDescr: TToolButton;
    BtnSaveDescr: TToolButton;
    SaveAsMenu: TPopupMenu;
    MISaveAs: TMenuItem;
    LblStructName: TLabel;
    InterpretRangeMenu: TPopupMenu;
    MIRangeEntireFile: TMenuItem;
    MIRangeSelection: TMenuItem;
    DSFieldPopupMenu: TPopupMenu;
    MISelectInEditor: TMenuItem;
    Panel1: TPanel;
    BtnHelp: TButton;
    MIGotoAddr: TMenuItem;
    MICopyFieldName: TMenuItem;
    MICopyFieldFullName: TMenuItem;
    MICopyFieldValue: TMenuItem;
    MIOrganizeFiles: TMenuItem;
    procedure BtnInterpretClick(Sender: TObject);
    procedure MIDummyDataStructClick(Sender: TObject);
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
    procedure MISaveAsClick(Sender: TObject);
    procedure BtnLoadDescrClick(Sender: TObject);
    procedure BtnNewDescrClick(Sender: TObject);
    procedure InterpretRangeMenuPopup(Sender: TObject);
    procedure MIRangeEntireFileClick(Sender: TObject);
    procedure MISelectInEditorClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure DSFieldPopupMenuPopup(Sender: TObject);
    procedure MIGotoAddrClick(Sender: TObject);
    procedure MICopyFieldNameClick(Sender: TObject);
    procedure MICopyFieldFullNameClick(Sender: TObject);
    procedure MICopyFieldValueClick(Sender: TObject);
    procedure DSTreeViewInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure DSTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MIOrganizeFilesClick(Sender: TObject);
  private const
    Unnamed_Struct = 'Unnamed';
  private
    { Private declarations }
    FParser: TDSParser;
    FInterpretor: TDSInterpretor;
    MPos: TPoint;
    FShownDS: TDSField;
    FEditor: TEditorForm;
    EditedNode: PVirtualNode;
    EditedDS: TDSSimpleField;
    MIGotoAddr_DestAddr: TFilePointer;
    NodesForDSs: TDictionary<TDSField, PVirtualNode>;
    FilesForMenuItems: TDictionary<Integer, string>;  // MenuItem.Tag -> File name
    CurDSFileName: string;
    function DSNodeText(DS: TDSField): string;
    procedure ShowStructTree(DS: TDSField; ParentNode: PVirtualNode);
//    procedure ExpandToNode(Node: PVirtualNode);
    function UserDSFolder(): string;
    function BuiltInDSFolder(): string;
    function GetNodeDS(Node: PVirtualNode): TDSField;
    function GetDSNode(DS: TDSField): PVirtualNode;
    procedure EditorClosed(Sender: TEditorForm);
    procedure AddRegionsForFields(DS{, HighlightDS}: TDSField; Start,
      AEnd: TFilePointer; Regions: TTaggedDataRegionList);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    function DSValueAsJsonObject(DS: TDSField): ISuperObject;
    function DSValueAsJson(DS: TDSField): string;
    procedure SetInterpretRange(const Value: TStructInterpretRange);
    function GetInterpretRange: TStructInterpretRange;
    procedure FieldInterpreted(Sender: TObject; DS: TDSField);
    procedure LoadDescrListToMenu(const Path: string; ParentMenu: TMenuItem);
  public
    { Public declarations }
    Settings: TStructSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Analyze(Addr, Size: TFilePointer; const Struct: string);
    property ShownDS: TDSField read FShownDS;
    property InterpretRange: TStructInterpretRange read GetInterpretRange write SetInterpretRange;
  end;

implementation

uses
  uValueFrame, uMainForm;

{$R *.dfm}

{ TStructFrame }

procedure TStructFrame.AddRegionsForFields(DS{, HighlightDS}: TDSField;
  Start, AEnd: TFilePointer; Regions: TTaggedDataRegionList);
// Add DS and it's childs as visible regions to Regions
var
  i{, c}: Integer;
  Bg: TColor;
begin
  // Check within requested address range
  if (DS.BufAddr >= AEnd) or (DS.BufAddr + DS.BufSize <= Start) then Exit;
  // Do not show directives
  if DS is TDSDirective then Exit;
  // Do not add separate regions for 1-byte elements of arrays
  if (DS is TDSSimpleField) and (DS.BufSize = 1) and
     (DS.Parent <> nil) and (DS.Parent is TDSArray) then
    Exit;

  // Background color
//  if DS = HighlightDS then
//    Bg := Color_ValueHighlightBg
//  else
//  begin
//    c := 255 - DS.Name.GetHashCode() and $1F;
//    Bg := RGB(c, c, 255);
//  end;
  if DS.ErrorText <> '' then
    Bg := Color_ErrDSFieldBg
  else
    Bg := Color_DSFieldBg;
  // Add this DS
  Regions.AddRegion(Self, DS.BufAddr, DS.BufAddr + DS.BufSize, clNone, Bg, Color_DSFieldFr);
  // Add childs
  if DS is TDSCompoundField then
    for i:=0 to TDSCompoundField(DS).Fields.Count-1 do
      AddRegionsForFields(TDSCompoundField(DS).Fields[i], {HighlightDS,} Start, AEnd, Regions);
end;

procedure TStructFrame.Analyze(Addr, Size: TFilePointer; {const Data: TBytes;}
  const Struct: string);
var
  Cnt: Cardinal;
//  DS: TDSField;
  AFromData: TEditedData;
  ASavedRootDS: TDSField;
  Node: PVirtualNode;
begin
  Progress.TaskStart(Self);
  try

    NodesForDSs.Clear();
    DSTreeView.Clear();
    FreeAndNil(FShownDS);
    BtnCopyValue.Enabled := False;

    // Parse structure description
    FShownDS := FParser.ParseStruct(Struct);

    // Populate structure fields
    FInterpretor.OnGetMoreData := procedure (AAddr, ASize: TFilePointer; var Data: TBytes{; var AEndOfData: Boolean})
      begin
        Data := FEditor.Data.Get(AAddr, ASize);
      end;

    Progress.TaskStart(Self, 0.9);
    try
      try
        FInterpretor.Interpret(ShownDS, Addr, Size);
      except
        // Show message and a partially parsed DS too
        on E: Exception do
          Application.ShowException(E);
      end;
    finally
      Progress.TaskEnd();
    end;

    AFromData := FEditor.Data;  // Capture for closure
    ShownDS.OnChanged.Add(procedure(DS: TDSField; Changer: TObject)
      begin
        // Update bytes in edited file when DS changes
        if DS is TDSSimpleField then
          AFromData.Change(DS.BufAddr, DS.BufSize, @TDSSimpleField(DS).Data[0]);
      end);

    // Redraw editor to show structure
    FEditor.UpdatePanes();

    // Show tree
    DSTreeView.BeginUpdate();
    Progress.TaskStart(Self, 0.1);
    try
      // Show first level child fields
      DSTreeViewInitChildren(DSTreeView, nil, Cnt);

      // Update node text when field changes
      ASavedRootDS := ShownDS;  // Capture for closure
      ShownDS.OnChanged.Add(procedure (DS: TDSField; Changer: TObject)
        var
          Node: PVirtualNode;
        begin
          if ShownDS <> ASavedRootDS then
            Exit;  // Do not try to update text if another DS is already shown in tree
          Node := GetDSNode(DS);
          if Node <> nil then
          begin
            PDSTreeNode(Node.GetData).Caption := DSNodeText(DS);
            DSTreeView.InvalidateNode(Node);
          end;
        end);

//      DSTreeView.IterateSubtree(nil,
//        procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
//        begin
//          // Expand top-level nodes
//          if (Node.Parent = DSTreeView.RootNode) and (DSTreeView.ChildCount[Node] < 30) then
//            DSTreeView.Expanded[Node] := True;
//          // Expand nodes with errors
//          DS := GetNodeDS(Node);
//          if (DS <> nil) and (DS is TDSSimpleField) and ((DS as TDSSimpleField).ErrorText <> '') then
//            ExpandToNode(Node);
//
//        end,
//        nil);
      if (DSTreeView.RootNodeCount = 1) then
      begin
        Node := DSTreeView.GetFirstChild(nil);
        Cnt := DSTreeView.ChildCount[Node];
        if (Cnt > 0) and (Cnt <= 10000) then
          DSTreeView.Expanded[Node] := True;
      end;

    finally
      DSTreeView.EndUpdate();
      Progress.TaskEnd();
    end;

    BtnCopyValue.Enabled := True;
  finally
    Progress.TaskEnd();
  end;
end;


function TStructFrame.GetDSNode(DS: TDSField): PVirtualNode;
// Returns tree node associated with given DSField
begin
  if not NodesForDSs.TryGetValue(DS, Result) then
    Result := nil;
end;

function TStructFrame.GetInterpretRange: TStructInterpretRange;
begin
  Result := Settings.Range;
end;

procedure TStructFrame.BtnCopyValueClick(Sender: TObject);
begin
  if ShownDS <> nil then
    Clipboard.AsText := DSValueAsJson(ShownDS);
end;

procedure TStructFrame.BtnHelpClick(Sender: TObject);
begin
  Application.HelpKeyword((Sender as TControl).HelpKeyword);
end;

procedure TStructFrame.BtnInterpretClick(Sender: TObject);
var
  Addr, Size: TFilePointer;
begin
  if Assigned(FEditor) then
  begin
    FEditor.OnClosed.Remove(Self);
    FEditor.OnGetTaggedRegions.Remove(Self);
  end;
  FEditor := MainForm.ActiveEditor;
  FEditor.OnClosed.Add(EditorClosed, Self);
  FEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);

  Addr := 0; Size := 0;
  with FEditor do
  begin
    case InterpretRange of
      irFile:
        begin
          // Parse entire file
          Addr := 0;
          Size := Data.GetSize();
        end;
      irSelection:
        begin
          Addr := SelStart;
          if SelLength > 0 then
            // Non-empty selection -> parse it
            Size := SelLength
          else
            // Parce from cursor until end of file
            Size := Data.GetSize() - Addr;
        end;
    end;
  end;

  Analyze(Addr, Size, DSDescrEdit.Text);
end;

procedure TStructFrame.BtnLoadDescrClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  SavedDescrsMenu.Items.Clear();
  FilesForMenuItems.Clear();

  // Submenu for Built-in DSs
  mi := TMenuItem.Create(Application);
  mi.Caption := 'Built-in';
  mi.ImageIndex := ImageIndex_Folder;
  SavedDescrsMenu.Items.Add(mi);
  LoadDescrListToMenu(BuiltInDSFolder(), mi);

  // User DSs
  LoadDescrListToMenu(UserDSFolder(), SavedDescrsMenu.Items);

  // "Organize descriptions"
  mi := TMenuItem.Create(Application);
  mi.Caption := '-';
  SavedDescrsMenu.Items.Add(mi);

  mi := TMenuItem.Create(Application);
  mi.Caption := 'Organize descriptions';
  mi.OnClick := MIOrganizeFilesClick;
  SavedDescrsMenu.Items.Add(mi);

  PopupFromControl(SavedDescrsMenu, BtnLoadDescr);
end;

procedure TStructFrame.BtnNewDescrClick(Sender: TObject);
begin
  DSDescrEdit.Clear();
  LblStructName.Caption := '    ' + Unnamed_Struct;
  CurDSFileName := '';
end;

function TStructFrame.BuiltInDSFolder: string;
begin
  Result := IncludeTrailingPathDelimiter( TPath.Combine(Settings.BuiltInSettingsFolder, 'DataStruct') );
end;

constructor TStructFrame.Create(AOwner: TComponent);
begin
  inherited;
  Settings := TStructSettings.Create();
  FilesForMenuItems := TDictionary<Integer, string>.Create();

  FParser := TDSParser.Create();
  FInterpretor := TDSInterpretor.Create();
  FInterpretor.OnFieldInterpreted.Add(FieldInterpreted);

  DSTreeView.NodeDataSize := SizeOf(TDSTreeNode);
  NodesForDSs := TDictionary<TDSField, PVirtualNode>.Create();
end;

destructor TStructFrame.Destroy;
begin
  NodesForDSs.Free;
  ShownDS.Free;
  FParser.Free;
  FInterpretor.Free;
  FilesForMenuItems.Free;
  Settings.Free;
  inherited;
end;

procedure TStructFrame.DSFieldPopupMenuPopup(Sender: TObject);
var
  DS: TDSField;
  Vis: Boolean;
  AValue: Variant;
begin
  // Show "Go to <addr>" menu item if this field looks like address in file
  Vis := False;
  DS := GetNodeDS(DSTreeView.FocusedNode);
  if (DS <> nil) and (DS is TDSSimpleField) then
  begin
    AValue := (DS as TDSSimpleField).ToVariant();
    if (VarIsOrdinal(AValue)) and (AValue >= 0) and (AValue < FEditor.GetFileSize()) then
    begin
      Vis := True;
      MIGotoAddr_DestAddr := AValue;
      MIGotoAddr.Caption := 'Go to ' + DS.ToString();
    end;
  end;
  MIGotoAddr.Visible := Vis;
end;

function TStructFrame.DSNodeText(DS: TDSField): string;
// Text for treeview node of this DS field
begin
  // Don't show directives in tree
  if DS is TDSDirective then
    Exit('');

  // Don't create separate nodes for nameless fields - e.g. conditional statements
  if DS.Name = '' then
    Exit('');

  // Field name
  Result := DS.Name;

  // For array: show length
  if DS is TDSArray then
  with TDSArray(DS) do
    Result := Result + '[' + IntToStr(Fields.Count) + ']';

  // Show value
  Result := Result + ': ' + DS.ToString();
end;

function TStructFrame.UserDSFolder: string;
begin
  Result := IncludeTrailingPathDelimiter( TPath.Combine(Settings.SettingsFolder, 'DataStruct') );
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

procedure TStructFrame.DSTreeViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  DS, ChildDS: TDSField;
  ChildNode: PVirtualNode;
begin
  DS := GetNodeDS(Node);
  if (DS <> nil) and (DS is TDSCompoundField) then
  begin
    Sender.BeginUpdate();
    try
      for ChildDS in (DS as TDSCompoundField).NamedFields do
      begin
        ChildNode := Sender.AddChild(Node);
        with PDSTreeNode(ChildNode.GetData())^ do
        begin
          DSField := ChildDS;
          NodesForDSs.AddOrSetValue(ChildDS, ChildNode);
        end;
      end;

      ChildCount := Sender.ChildCount[Node];
    finally
      Sender.EndUpdate();
    end;
  end
  else
    ChildCount := 0;
end;

procedure TStructFrame.DSTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  with PDSTreeNode(Sender.GetNodeData(Node))^ do
  begin
    Progress.Show(DSField.BufAddr - ShownDS.BufAddr, ShownDS.BufSize, 'Showing tree');

    Caption := DSNodeText(DSField);
    if (DSField is TDSCompoundField) and ((DSField as TDSCompoundField).Fields.Count > 0) then
      InitialStates := InitialStates + [ivsHasChildren];
  end;
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

  function IsChar(AField: TDSField): Boolean;
  begin
    Result := (AField is TDSSimpleField) and
              ((TDSSimpleField(AField).DataType = 'ansi') or (TDSSimpleField(AField).DataType = 'unicode'));
  end;

var
  i: Integer;
  Intr: TValueInterpretor;
  x: Variant;
  s: string;
  Field: TDSField;
begin
  Result := nil;
  if DS is TDSArray then
  begin
    if IsChar((DS as TDSArray).ElementType) then
    // Special case - show array of chars as string
    begin
      s := '';
      for i:=0 to (DS as TDSArray).Fields.Count-1 do
        s := s + (DS as TDSArray).Fields[i].ToString();
      Result := TSuperObject.Create(s);
    end
    else
    begin
      Result := SA([]);
      for i:=0 to (DS as TDSArray).Fields.Count-1 do
        Result.AsArray.Add(DSValueAsJsonObject((DS as TDSArray).Fields[i]));
    end;
  end
  else
  if DS is TDSCompoundField then
  begin
    Result := SO();
    for Field in (DS as TDSCompoundField).NamedFields do
    begin
      (Result as TSuperObject).O[Field.Name] := DSValueAsJsonObject(Field);
    end;
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
      FEditor.UndoStack.BeginAction('', 'Change '+DS.FullName());
      try

        DS.SetFromVariant(EditFieldValue.Text, Self);  // <--

      finally
        FEditor.UndoStack.EndAction();
      end;
    end;
    EditFieldValue.Modified := False;
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
  NodesForDSs.Clear();
  DSTreeView.Clear();
  FreeAndNil(FShownDS);
  FEditor := nil;
  BtnCopyValue.Enabled := False;
end;

procedure TStructFrame.EditorGetTaggedRegions(Editor: TEditorForm; Start,
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  SelDS: TDSField;
begin
  if ShownDS = nil then Exit;

  AddRegionsForFields(ShownDS, {SelDS,} Start, AEnd, Regions);

  if (Screen.ActiveControl = DSTreeView) and (DSTreeView.FocusedNode <> nil) then
  begin
    SelDS := GetNodeDS(DSTreeView.FocusedNode);
    if SelDS <> nil then
      Regions.AddRegion(Self, SelDS.BufAddr, SelDS.BufAddr + SelDS.BufSize, clNone, Color_SelDSFieldBg, Color_SelDSFieldFr);
  end;

end;

//procedure TStructFrame.ExpandToNode(Node: PVirtualNode);
//begin
//  while Node <> DSTreeView.RootNode do
//  begin
//    DSTreeView.Expanded[Node] := True;
//    Node := Node.Parent;
//  end;
//end;

procedure TStructFrame.FieldInterpreted(Sender: TObject; DS: TDSField);
// Called on new DS field initialization
begin
  // TODO: create and axpand nodes for fields with errors
//  if DS.ErrorText <> '' then
//    ExpandFields.Add(DS);
end;

procedure TStructFrame.FrameResize(Sender: TObject);
begin
  DSDescrEdit.Constraints.MaxHeight := DSDescrEdit.Height + DSTreeView.Height - 20;
end;

procedure TStructFrame.MICopyFieldFullNameClick(Sender: TObject);
var
  DS: TDSField;
begin
  DS := GetNodeDS(DSTreeView.FocusedNode);
  if DS <> nil then
    Clipboard.AsText := DS.FullName;
end;

procedure TStructFrame.MICopyFieldNameClick(Sender: TObject);
var
  DS: TDSField;
begin
  DS := GetNodeDS(DSTreeView.FocusedNode);
  if DS <> nil then
    Clipboard.AsText := DS.Name;
end;

procedure TStructFrame.MICopyFieldValueClick(Sender: TObject);
var
  DS: TDSField;
begin
  DS := GetNodeDS(DSTreeView.FocusedNode);
  if DS <> nil then
    Clipboard.AsText := RemUnprintable(DS.ToString());
end;

procedure TStructFrame.MIDummyDataStructClick(Sender: TObject);
var
  n: Integer;
  fn, name: string;
begin
  n := (Sender as TMenuItem).Tag;
  if not FilesForMenuItems.TryGetValue(n, fn) then Exit;
  name := (Sender as TMenuItem).Caption;
  DSDescrEdit.Lines.LoadFromFile(fn);
  LblStructName.Caption := '    ' + name;
  CurDSFileName := fn;
end;

procedure TStructFrame.MIGotoAddrClick(Sender: TObject);
begin
  if (MIGotoAddr_DestAddr < 0) or (MIGotoAddr_DestAddr >= FEditor.GetFileSize()) then Exit;

  FEditor.SelectAndShow(MIGotoAddr_DestAddr, MIGotoAddr_DestAddr);
end;

procedure TStructFrame.MIOrganizeFilesClick(Sender: TObject);
begin
  ForceDirectories(UserDSFolder());
  ShellExecute(0, '', PChar(UserDSFolder()), '', '', SW_SHOWNORMAL);
end;

procedure TStructFrame.MIRangeEntireFileClick(Sender: TObject);
begin
  InterpretRange := TStructInterpretRange((Sender as TMenuItem).Tag);
end;

procedure TStructFrame.MISaveAsClick(Sender: TObject);
// Save structure description
var
  fn: string;
begin
  ForceDirectories(UserDSFolder());
  fn := CurDSFileName;

  // If "Save as" pressed or file name still not specified - show "Save as" dialog.
  // Files can be saved only to special folder
  if (Sender = MISaveAs) or (fn = '') or (not PathIsInside(fn, UserDSFolder())) then
  begin
    if (fn = '') then
      fn := 'Struct1'
    else
    if not PathIsInside(fn, UserDSFolder()) then
      fn := ChangeFileExt(ExtractFileName(fn), '');
    SaveDialog1.InitialDir := UserDSFolder();
    SaveDialog1.FileName := fn{ + '.ds'};

    if not SaveDialog1.Execute() then Exit;
    fn := SaveDialog1.FileName;
    if not PathIsInside(fn, UserDSFolder()) then
      raise EInvalidUserInput.Create('Cannot save structure description outside of default user settings folder');
  end
  else
    fn := CurDSFileName;

  ForceDirectories(ExtractFilePath(fn));
  DSDescrEdit.Lines.SaveToFile(fn);
  DSDescrEdit.MarkModifiedLinesAsSaved();
  // '    ' is added so vertical line, added by toolbar, does not overlaps caption
  LblStructName.Caption := '    ' + ChangeFileExt(ExtractFileName(fn), '');
  CurDSFileName := fn;
end;

procedure TStructFrame.MISelectInEditorClick(Sender: TObject);
var
  DS: TDSField;
begin
  DS := GetNodeDS(DSTreeView.FocusedNode);
  if DS <> nil then
    FEditor.SelectAndShow(DS.BufAddr, DS.BufAddr + DS.BufSize);
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
// Returns DSField associated with given tree node
begin
  if Node = nil then Exit(ShownDS);
  Result := PDSTreeNode(DSTreeView.GetNodeData(Node)).DSField;
end;

procedure TStructFrame.InterpretRangeMenuPopup(Sender: TObject);
begin
  MIRangeEntireFile.Checked := (InterpretRange = irFile);
  MIRangeSelection.Checked := (InterpretRange = irSelection);
end;

procedure TStructFrame.LoadDescrListToMenu(const Path: string;
  ParentMenu: TMenuItem);
// Populate menu with list of files and folders in specified folder  
var
  fl: TStringDynArray;
  i: Integer;
  mi: TMenuItem;
begin
  if not System.SysUtils.DirectoryExists(Path) then Exit;

  // Subdirectories
  fl := TDirectory.GetDirectories(Path);
  for i:=0 to Length(fl)-1 do
  begin
    mi := TMenuItem.Create(Application);
    mi.Caption := ExtractFileName(fl[i]);
    mi.ImageIndex := ImageIndex_Folder;
    ParentMenu.Add(mi);
    LoadDescrListToMenu(fl[i], mi);
  end;

  // DS description files
  fl := TDirectory.GetFiles(Path, '*.ds');
  for i:=0 to Length(fl)-1 do
  begin
    mi := TMenuItem.Create(Application);
    mi.Caption := ChangeFileExt(ExtractFileName(fl[i]), '');
    mi.OnClick := MIDummyDataStructClick;
    mi.Tag := FilesForMenuItems.Count;
    FilesForMenuItems.AddOrSetValue(mi.Tag, fl[i]);
    ParentMenu.Add(mi);
  end;
end;

procedure TStructFrame.SetInterpretRange(const Value: TStructInterpretRange);
begin
  if Settings.Range <> Value then
  begin
    Settings.Range := Value;
    Settings.Changed();
  end;
end;

procedure TStructFrame.ShowStructTree(DS: TDSField; ParentNode: PVirtualNode);
// Recursively show given DataStructure inside tree node
var
//  Node: TTreeNode;
  Node: PVirtualNode;
  S: string;
  i: Integer;
begin
  Progress.Show(DS.BufAddr - ShownDS.BufAddr, ShownDS.BufSize, 'Showing tree');

  S := DSNodeText(DS);

  if (S <> ''){ or (ParentNode = nil)} then
  begin
    Node := DSTreeView.AddChild(ParentNode);
    PDSTreeNode(Node.GetData).Caption := S;
    PDSTreeNode(Node.GetData).DSField := DS;
  end
  else  // Don't create separate nodes for nameless fields - e.g. conditional statements
    Node := ParentNode;
  NodesForDSs.AddOrSetValue(DS, Node);

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

end.
