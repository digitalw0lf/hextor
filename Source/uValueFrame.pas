{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uValueFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Math,
  Generics.Collections, Vcl.StdCtrls, Clipbrd, System.Types, Vcl.Menus,

  uHextorTypes, uEditorForm, uValueInterpretors, VirtualTrees;

const
  SUndefinedValue = 'N/A';

type
  TValueFrame = class(TFrame, IHextorToolFrame)
    ValuePopupMenu: TPopupMenu;
    MICopyValue: TMenuItem;
    ValuesTreeView: TVirtualStringTree;
    procedure MICopyValueClick(Sender: TObject);
    procedure ValuesTreeViewFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ValuesTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure ValuesTreeViewNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure ValuesTreeViewEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ValuesTreeViewFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure ValuesTreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ValuesTreeViewExit(Sender: TObject);
    procedure ValuesTreeViewGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure ValuesTreeViewGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
  private
    type
      TValueNodeData = record
      public
        TypeName, Value: string;
        OrigDataSize: Integer;
        Defined: Boolean;
        Hint: string;
      end;
      PValueNodeData = ^TValueNodeData;
  private
    { Private declarations }
    FEditor: TEditorForm;
    FShownRange: TFileRange;
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorSelectionChanged(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure ShowTypesList();
    procedure ClearInfo();
    function ValueNode(Node: PVirtualNode; RequireDefined: Boolean = False; AbortIfNone: Boolean = False): PValueNodeData;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure UpdateInfo();
    procedure OnShown();
  end;

implementation

{$R *.dfm}

uses uMainForm;

{ TValueFrame }

procedure TValueFrame.ClearInfo;
var
  Node: PVirtualNode;
  NodeData: PValueNodeData;
begin
  ValuesTreeView.BeginUpdate();
  try
    for Node in ValuesTreeView.ChildNodes(nil) do
    begin
      NodeData := Node.GetData();
      NodeData.Value := '';
      NodeData.Defined := False;
      NodeData.Hint := '';
    end;
  finally
    ValuesTreeView.EndUpdate();
  end;
end;

constructor TValueFrame.Create(AOwner: TComponent);
begin
  inherited;
  ValuesTreeView.NodeDataSize := SizeOf(TValueNodeData);
  ShowTypesList();
  MainForm.OnSelectionChanged.Add(EditorSelectionChanged);
end;

destructor TValueFrame.Destroy;
begin
  inherited;
end;

procedure TValueFrame.EditorSelectionChanged(Sender: TEditorForm);
begin
  if not MainForm.ToolFrameVisible(Self) then Exit;
  UpdateInfo();
end;

procedure TValueFrame.EditorClosed(Sender: TEditorForm);
begin
  FEditor := nil;
  ClearInfo();
end;

procedure TValueFrame.EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  NodeData: PValueNodeData;
begin
  if Screen.ActiveControl <> ValuesTreeView then Exit;

  NodeData := ValueNode(ValuesTreeView.FocusedNode, True);
  if NodeData = nil then Exit;

  Regions.AddRegion(Self, FShownRange.Start, FShownRange.Start+NodeData.OrigDataSize, clNone, Color_ValueHighlightBg, clNone);
end;

procedure TValueFrame.MICopyValueClick(Sender: TObject);
var
  NodeData: PValueNodeData;
begin
  NodeData := ValueNode(ValuesTreeView.FocusedNode, True, True);

  Clipboard.AsText := StrToClipboard(NodeData.Value);
end;

procedure TValueFrame.OnShown;
begin
  UpdateInfo();
end;

procedure TValueFrame.ShowTypesList;
// List all available data types
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PValueNodeData;
begin
  ValuesTreeView.BeginUpdate();
  try
    ValuesTreeView.Clear();
    for i:=0 to ValueInterpretors.Count-1 do
    begin
      Node := ValuesTreeView.AddChild(nil);
      NodeData := Node.GetData;
      NodeData.TypeName := ValueInterpretors[i].Name;
      NodeData.Value := '';
      NodeData.OrigDataSize := 0;
      NodeData.Defined := False;
      NodeData.Hint := '';
    end;
  finally
    ValuesTreeView.EndUpdate();
  end;
end;

procedure TValueFrame.UpdateInfo;
// Show selection/data under cursor as values
var
  AEditor: TEditorForm;
  AData: TBytes;
  Greedy: Boolean;
  Size: Integer;
  S: string;
  AName: string;
  AOrigDataSize: Integer;
  ADefined: Boolean;
  AHint: string;
  NodeData: PValueNodeData;
  Node: PVirtualNode;
  Interp: TValueInterpretor;
begin
  ValuesTreeView.CancelEditNode();

  AEditor := MainForm.GetActiveEditorNoEx();

  if AEditor <> FEditor then
  begin
    if Assigned(FEditor) then
      FEditor.RemoveEventListener(Self);

    FEditor := AEditor;

    if Assigned(FEditor) then
    begin
      FEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
      FEditor.OnClosed.Add(EditorClosed, Self);
    end;
  end;

  if not Assigned(FEditor) then
  begin
    ClearInfo();
    Exit;
  end;

  with FEditor do
  begin
    AData := GetSelectedOrAfterCaret(MAX_STR_VALUE_LENGTH, MAX_STR_VALUE_LENGTH, FShownRange.Start, True);
    Greedy := (SelLength > 0);

    ValuesTreeView.BeginUpdate();
    try
      for Node in ValuesTreeView.ChildNodes(nil) do
      begin
        // Generate value description
        NodeData := Node.GetData();
        AName := NodeData.TypeName;
        Interp := ValueInterpretors.FindInterpretor(AName);

        S := '';
        ADefined := False;
        AHint := '';
        AOrigDataSize := 0;
        if Length(AData) < Interp.MinSize then
        begin
          S := SUndefinedValue;
          AHint := 'Not enough data';
        end
        else
        try
          if (Greedy) {and (ValueInterpretors[i].Greedy)} then
            Size := Min(Interp.MaxSize, Length(AData))
          else
            Size := Interp.MinSize;
          AOrigDataSize := Size;

          S := Interp.ToVariant(AData[0], Size);  // <--

          ADefined := True;
        except
          on E:Exception do
          begin
            S := SUndefinedValue;
            AHint := E.Message;
          end;
        end;

        // Show in list
        NodeData.Value := S;
        NodeData.OrigDataSize := AOrigDataSize;
        NodeData.Defined := ADefined;
        NodeData.Hint := AHint;
      end;
    finally
      ValuesTreeView.EndUpdate();
    end;
  end;
end;

function TValueFrame.ValueNode(Node: PVirtualNode; RequireDefined: Boolean = False; AbortIfNone: Boolean = False): PValueNodeData;
// Returns PValueNodeData for given node [only if node Value is defined], or raises Abort exception if
// this Node has no corresponding Value record
begin
  if Node = nil then
  begin
    if AbortIfNone then Abort()
                   else Exit(nil);
  end;
  Result := Node.GetData();
  if Result = nil then
  begin
    if AbortIfNone then Abort()
                   else Exit(nil);
  end;
  if (RequireDefined) and (not Result.Defined) then
  begin
    if AbortIfNone then Abort()
                   else Exit(nil);
  end;
end;

procedure TValueFrame.ValuesTreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column = 1) and (ValueNode(Node, True) <> nil);
end;

procedure TValueFrame.ValuesTreeViewExit(Sender: TObject);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TValueFrame.ValuesTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FEditor <> nil then
    FEditor.UpdatePanes();
end;

procedure TValueFrame.ValuesTreeViewFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := (NewColumn = 1);
end;

procedure TValueFrame.ValuesTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PValueNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Data <> nil then
    Finalize(Data^);
end;

procedure TValueFrame.ValuesTreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  NodeData: PValueNodeData;
begin
  NodeData := ValueNode(Node, False);
  if NodeData <> nil then
    HintText := NodeData.Hint;
end;

procedure TValueFrame.ValuesTreeViewGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: Boolean; var PopupMenu: TPopupMenu);
begin
  if (Column = 1) and (ValueNode(Node, True) <> nil) then
    PopupMenu := ValuePopupMenu;
end;

procedure TValueFrame.ValuesTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PValueNodeData;
begin
  Data := ValueNode(Node);
  if Assigned(Data) then
  begin
    case Column of
      0: CellText := Data.TypeName;
      1: CellText := Data.Value;
    end;
  end
end;

procedure TValueFrame.ValuesTreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
// Convert text to data and change bytes in editor
var
  NodeData: PValueNodeData;
  Interp: TValueInterpretor;
  s: string;
  Data: TBytes;
begin
  NodeData := ValueNode(Node);
  Interp := ValueInterpretors.FindInterpretor(NodeData.TypeName);
  if Interp = nil then Exit;
  s := NewText;
  if s = NodeData.Value then Exit;  // Text not changed
  // Buffer of same size as original data
  SetLength(Data, NodeData.OrigDataSize);

  try
    Interp.FromVariant(s, Data[0], Length(Data));

    if not DataEqual(Data, FEditor.GetEditedData(FShownRange.Start, Length(Data))) then
    begin
      FEditor.ChangeBytes(FShownRange.Start, Data);
    end;
  except
    // Catch exception here so ValuesTreeView can proprtly destroy editor etc.
    on E: Exception do
      Application.MessageBox(PChar(E.Message), PChar(E.ClassName), MB_OK or MB_ICONERROR);
  end;
end;

end.
