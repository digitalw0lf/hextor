{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uRegionsFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls,
  Vcl.StdCtrls, Generics.Collections, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,

  uHextorTypes, uEditorForm, uHextorDataSources;

type
  TRegionsFrame = class(TFrame, IHextorToolFrame)
    ToolPanel: TPanel;
    RegionsTreeView: TVirtualStringTree;
    procedure RegionsTreeViewFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure RegionsTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure RegionsTreeViewNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
  private type
    TRegionTreeNode = record
      Region: TSourceRegion;
    end;
    PRegionTreeNode = ^TRegionTreeNode;
  private
    { Private declarations }
    FEditor: TEditorForm;
    PointerSize: Integer;  // Size of pointer in shown data source - 4 or 8 bytes
    procedure ActiveEditorChanged(Sender: TEditorForm);
    procedure EditorClosed(Sender: TEditorForm);
    procedure ShowRegions(const Regions: TSourceRegionArray);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure UpdateInfo();
    procedure OnShown();
    procedure Init();
    procedure Uninit();
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

{ TRegionsFrame }

procedure TRegionsFrame.ActiveEditorChanged(Sender: TEditorForm);
begin
  if not MainForm.ToolFrameVisible(Self) then Exit;
  UpdateInfo();
end;

constructor TRegionsFrame.Create(AOwner: TComponent);
begin
  inherited;
  RegionsTreeView.NodeDataSize := SizeOf(TRegionTreeNode);
  MainForm.OnActiveEditorChanged.Add(ActiveEditorChanged);

end;

destructor TRegionsFrame.Destroy;
begin

  inherited;
end;

procedure TRegionsFrame.EditorClosed(Sender: TEditorForm);
begin
  FEditor := nil;
  RegionsTreeView.Clear();
end;

procedure TRegionsFrame.Init;
begin

end;

procedure TRegionsFrame.OnShown;
begin
  UpdateInfo();
end;

procedure TRegionsFrame.RegionsTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
//  FreeAndNil((PRegionTreeNode(Sender.GetNodeData(Node))^).Region);
  (PRegionTreeNode(Sender.GetNodeData(Node))^).Region.Free;
//  (PRegionTreeNode(Sender.GetNodeData(Node))^).Region := nil;
end;

procedure TRegionsFrame.RegionsTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PRegionTreeNode;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData = nil then  Exit;

  case Column of
    0: CellText := IntToHex(NodeData.Region.Range.Start, PointerSize * 2);
    1: CellText := IntToStr(NodeData.Region.Range.Size div 1024) + ' KB';
    2: CellText := NodeData.Region.Description;
  end;
end;

procedure TRegionsFrame.RegionsTreeViewNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
// On doubleclick: Switch to corresponding editor and select region
var
  NodeData: PRegionTreeNode;
  AEditor: TEditorForm;
begin
  if HitInfo.HitNode = nil then Exit;
  NodeData := PRegionTreeNode(RegionsTreeView.GetNodeData(HitInfo.HitNode));
  if (NodeData = nil) then Exit;

  AEditor := FEditor;

  MainForm.ActiveEditor := AEditor;
  AEditor.SelectAndShow(NodeData.Region.Range.Start, NodeData.Region.Range.AEnd);
end;

procedure TRegionsFrame.ShowRegions(const Regions: TSourceRegionArray);
var
  i: Integer;
  NodesByRegions: TDictionary<TSourceRegion, PVirtualNode>;
  Node, ParentNode: PVirtualNode;
  NodeData: PRegionTreeNode;
begin
  NodesByRegions := TDictionary<TSourceRegion, PVirtualNode>.Create();
  RegionsTreeView.BeginUpdate();
  try
    RegionsTreeView.Clear();
    for i := 0 to Length(Regions)-1 do
    begin
      if not NodesByRegions.TryGetValue(Regions[i].Parent, ParentNode) then
        ParentNode := nil;
      Node := RegionsTreeView.AddChild(ParentNode);
      NodeData := RegionsTreeView.GetNodeData(Node);
      NodeData.Region := Regions[i];
      NodesByRegions.AddOrSetValue(Regions[i], Node);
    end;
  finally
    RegionsTreeView.EndUpdate();
    NodesByRegions.Free;
  end;
end;

procedure TRegionsFrame.Uninit;
begin

end;

procedure TRegionsFrame.UpdateInfo;
var
  AActiveEditor: TEditorForm;
  Regions: TSourceRegionArray;
begin
  // Link to active editor
  AActiveEditor := MainForm.GetActiveEditorNoEx();
  if FEditor <> AActiveEditor then
  begin
    if Assigned(FEditor) then
      FEditor.RemoveEventListener(Self);

    FEditor := AActiveEditor;
    if FEditor <> nil then
    begin
//      FEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
      FEditor.OnClosed.Add(EditorClosed, Self);
//      FEditor.Data.OnDataChanged.Add(DataChanged, Self);
    end
    else
    begin
      EditorClosed(nil);
      Exit;
    end;
  end;
  if FEditor = nil then Exit;

  if FEditor.DataSource.GetSize() > $100000000 then
    PointerSize := 8
  else
    PointerSize := 4;
  // TODO: Regions may move when data changed?
  Regions := FEditor.DataSource.GetRegions(EntireFile);
  try
    ShowRegions(Regions);
  finally
    //Regions.Free;
  end;
end;

end.
