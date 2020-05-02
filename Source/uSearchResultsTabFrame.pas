unit uSearchResultsTabFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ComCtrls,
  System.Math, System.UITypes,

  uEditorForm, uHextorTypes, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TSearchResultsTabFrame = class(TFrame)
    ResultsList: TVirtualStringTree;
    Panel1: TPanel;
    LblFoundCount: TLabel;
    CBHighlightResults: TCheckBox;
    procedure ResultsListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ResultsListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ResultsListNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure ResultsListDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure CBHighlightResultsClick(Sender: TObject);
  private type
    TDisplayedNeedle = array[0..2] of string;  // Text[1] is highlighted needle, Text[0] and Text[2] is short context before and after
    TResultTreeNode = record
      Range: TFileRange;
      DisplayHex, DisplayText: TDisplayedNeedle;
    end;
    PResultTreeNode = ^TResultTreeNode;
  private
    { Private declarations }
    FEditor: TEditorForm;
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure UnsubscribeFromEditor();
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure StartList(AEditor: TEditorForm; const ASearchText: string);
    procedure AddListItem(const ARange: TFileRange);
    procedure EndUpdateList();
    property Editor: TEditorForm read FEditor;
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

procedure TSearchResultsTabFrame.AddListItem(const ARange: TFileRange);
// Add ARange to list of search results
var
  Node: PVirtualNode;
  RNode: PResultTreeNode;
  DispRange: TFileRange;
  AData: TBytes;
begin
  Node := ResultsList.AddChild(nil);
  RNode := Node.GetData;
  RNode.Range := ARange;
//  PResultTreeNode(Node.GetData).DisplayText := ADisplayText;
  DispRange.Start := Max(ARange.Start - 5, 0);
  DispRange.AEnd := Min(ARange.AEnd + 5, FEditor.Data.GetSize());
  AData := FEditor.Data.Get(DispRange.Start, Min(DispRange.Size, 100));
  RNode.DisplayHex[0] := Data2Hex(Copy(AData, 0, ARange.Start-DispRange.Start));
  RNode.DisplayHex[1] := Data2Hex(Copy(AData, ARange.Start-DispRange.Start, ARange.Size));
  RNode.DisplayHex[2] := Data2Hex(Copy(AData, ARange.AEnd-DispRange.Start, MaxInt));
  RNode.DisplayText[0] := RemUnprintable(Data2String(Copy(AData, 0, ARange.Start-DispRange.Start), FEditor.TextEncoding));
  RNode.DisplayText[1] := RemUnprintable(Data2String(Copy(AData, ARange.Start-DispRange.Start, ARange.Size), FEditor.TextEncoding));
  RNode.DisplayText[2] := RemUnprintable(Data2String(Copy(AData, ARange.AEnd-DispRange.Start, MaxInt), FEditor.TextEncoding));
end;

procedure TSearchResultsTabFrame.CBHighlightResultsClick(Sender: TObject);
begin
  if Assigned(FEditor) then
    FEditor.UpdatePanes();
end;

constructor TSearchResultsTabFrame.Create(AOwner: TComponent);
begin
  inherited;
  ResultsList.NodeDataSize := SizeOf(TResultTreeNode);

end;

destructor TSearchResultsTabFrame.Destroy;
begin
  UnsubscribeFromEditor();
  inherited;
end;

procedure TSearchResultsTabFrame.EditorClosed(Sender: TEditorForm);
begin
  UnsubscribeFromEditor();
//  ResultsList.Clear();
  // DoAfterEvent?
  Parent.Free;
end;

procedure TSearchResultsTabFrame.EditorGetTaggedRegions(Editor: TEditorForm;
  Start, AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  Node: PVirtualNode;
  RNode: PResultTreeNode;
begin
//  if not CBHighlightResults.Checked then Exit;
  if (not Parent.Visible) then Exit;

  for Node in ResultsList.Nodes() do
  begin
    RNode := Node.GetData;

    Regions.AddRegion(Self, RNode.Range.Start, RNode.Range.AEnd, clNone, $79EBFF, $00D7FD);
  end;
end;

procedure TSearchResultsTabFrame.EndUpdateList;
begin
  ResultsList.EndUpdate();
  LblFoundCount.Caption := IntToStr(ResultsList.ChildCount[nil]) + ' items';
  FEditor.UpdatePanes();
end;

procedure TSearchResultsTabFrame.ResultsListDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  S: ^TDisplayedNeedle;
  x: Integer;
begin
  case Column of
    1: S := @PResultTreeNode(Sender.GetNodeData(Node))^.DisplayHex;
    2: S := @PResultTreeNode(Sender.GetNodeData(Node))^.DisplayText;
    else Exit;
  end;
  x := CellRect.Left;
  TargetCanvas.Brush.Style := bsClear;
  TargetCanvas.TextRect(CellRect, x, 0, S[0]);
  x := x + TargetCanvas.TextWidth(S[0]);
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  TargetCanvas.TextRect(CellRect, x, 0, S[1]);
  x := x + TargetCanvas.TextWidth(S[1]);
  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
  TargetCanvas.TextRect(CellRect, x, 0, S[2]);
  DefaultDraw := False;
end;

procedure TSearchResultsTabFrame.ResultsListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PResultTreeNode(Sender.GetNodeData(Node))^);
end;

procedure TSearchResultsTabFrame.ResultsListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PResultTreeNode;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    case Column of
      0: CellText := IntToHex(Data.Range.Start, 8);
      1: CellText := Data.DisplayHex[0] + Data.DisplayHex[1] + Data.DisplayHex[2];
      2: CellText := Data.DisplayText[0] + Data.DisplayText[1] + Data.DisplayText[2];
    end;
end;

procedure TSearchResultsTabFrame.ResultsListNodeDblClick(
  Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  Data: PResultTreeNode;
begin
  if FEditor = nil then Exit;
  if HitInfo.HitNode = nil then Exit;
  Data := PResultTreeNode(ResultsList.GetNodeData(HitInfo.HitNode));
  if Data = nil then Exit;

  MainForm.ActiveEditor := FEditor;
  FEditor.SetSelection(Data.Range.Start, Data.Range.AEnd, True);
  FEditor.ScrollToShow(Data.Range.Start, -1, -1);
end;

procedure TSearchResultsTabFrame.StartList(AEditor: TEditorForm;
  const ASearchText: string);
var
  S: string;
begin
  FEditor := AEditor;
  FEditor.OnClosed.Add(EditorClosed, Self);
  FEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);

  S := ASearchText;
  if Length(S) > 20 then
    S := Copy(S, Low(S), 20) + '...';
  if Parent is TTabSheet then
    TTabSheet(Parent).Caption := S;
  ResultsList.BeginUpdate();
  ResultsList.Clear();
end;

procedure TSearchResultsTabFrame.UnsubscribeFromEditor();
begin
  if FEditor = nil then Exit;
  FEditor.OnClosed.Remove(Self);
  FEditor.OnGetTaggedRegions.Remove(Self);
  FEditor.UpdatePanes();
  FEditor := nil;
end;

end.
