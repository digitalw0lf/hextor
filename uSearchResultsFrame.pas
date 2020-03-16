unit uSearchResultsFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VirtualTrees, Math,

  uHextorTypes, uEditorForm, uUtil;

type
  TSearchResultsFrame = class(TFrame)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    ResultsList: TVirtualStringTree;
    StatusBar1: TStatusBar;
    procedure ResultsListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ResultsListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ResultsListNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure ResultsListDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
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
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BeginUpdateList(AEditor: TEditorForm; const ASearchText: string);
    procedure EndUpdateList();
    procedure AddListItem(const ARange: TFileRange);
  end;

implementation

{$R *.dfm}

procedure TSearchResultsFrame.AddListItem(const ARange: TFileRange);
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
  RNode.DisplayHex[0] := string(Data2Hex(Copy(AData, 0, ARange.Start-DispRange.Start)));
  RNode.DisplayHex[1] := string(Data2Hex(Copy(AData, ARange.Start-DispRange.Start, ARange.Size)));
  RNode.DisplayHex[2] := string(Data2Hex(Copy(AData, ARange.AEnd-DispRange.Start, MaxInt)));
  RNode.DisplayText[0] := RemUnprintable(string(MakeStr(Copy(AData, 0, ARange.Start-DispRange.Start))));
  RNode.DisplayText[1] := RemUnprintable(string(MakeStr(Copy(AData, ARange.Start-DispRange.Start, ARange.Size))));
  RNode.DisplayText[2] := RemUnprintable(string(MakeStr(Copy(AData, ARange.AEnd-DispRange.Start, MaxInt))));
end;

procedure TSearchResultsFrame.BeginUpdateList(AEditor: TEditorForm; const ASearchText: string);
var
  S: string;
begin
  FEditor := AEditor;
  FEditor.OnClosed.Add(EditorClosed);
  S := ASearchText;
  if Length(S) > 20 then
    S := Copy(S, Low(S), 20) + '...';
  TabSheet1.Caption := S;
  ResultsList.BeginUpdate();
  ResultsList.Clear();
end;

constructor TSearchResultsFrame.Create(AOwner: TComponent);
begin
  inherited;
  ResultsList.NodeDataSize := SizeOf(TResultTreeNode);
end;

procedure TSearchResultsFrame.EditorClosed(Sender: TEditorForm);
begin
  FEditor := nil;
  ResultsList.Clear();
end;

procedure TSearchResultsFrame.EndUpdateList;
begin
  ResultsList.EndUpdate();
  StatusBar1.Panels[0].Text := IntToStr(ResultsList.ChildCount[nil]) + ' items';
end;

procedure TSearchResultsFrame.ResultsListDrawText(Sender: TBaseVirtualTree;
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

procedure TSearchResultsFrame.ResultsListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PResultTreeNode(Sender.GetNodeData(Node))^);
end;

procedure TSearchResultsFrame.ResultsListGetText(Sender: TBaseVirtualTree;
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

procedure TSearchResultsFrame.ResultsListNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Data: PResultTreeNode;
begin
  if HitInfo.HitNode = nil then Exit;
  Data := PResultTreeNode(ResultsList.GetNodeData(HitInfo.HitNode));
  if Data = nil then Exit;

  //FEditor.ScrollToShow(Data.Range.Start, -1, -1);
  FEditor.SetSelection(Data.Range.Start, Data.Range.AEnd);
  FEditor.ScrollToShow(Data.Range.Start, -1, -1);
end;

end.
