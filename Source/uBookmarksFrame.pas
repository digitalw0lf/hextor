unit uBookmarksFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ToolWin,
  Vcl.ComCtrls, System.Generics.Collections, System.Math, Vcl.AppEvnts,
  System.Types,

  uHextorTypes, uEditorForm, uEditedData, uHextorGUI, uEditorPane, Vcl.Menus;

const
//  Color_BookmarkBg = $E7BFC8;
//  Color_BookmarkFr = $A449A3;
  Color_BookmarkBg = $C0F8EA;
  Color_BookmarkFr = $1DE6B5;

type
  TBookmarksList = class;

  TBookmark = class
  private
    Owner: TBookmarksList;
    FShortcut: Integer;
    procedure SetShortcut(const Value: Integer);
  public
    Editor: TEditorForm;
    Range: TFileRange;
    Caption: string;
    property Shortcut: Integer read FShortcut write SetShortcut;
    constructor Create();
    destructor Destroy(); override;
  end;

  TBookmarksList = class(TObjectList<TBookmark>)
  private
    FDestroying: Boolean;
  public
    destructor Destroy(); override;
  end;

  // Data for VirtualTreeView item
  TBMTreeNode = record
    Bookmark: TBookmark;
  end;
  PBMTreeNode = ^TBMTreeNode;

  TBookmarksFrame = class(TFrame)
    ToolBar1: TToolBar;
    BookmarksTreeView: TVirtualStringTree;
    BtnAddFolder: TToolButton;
    BtnAddBookmark: TToolButton;
    BtnDelete: TToolButton;
    ApplicationEvents1: TApplicationEvents;
    BMListPopupMenu: TPopupMenu;
    MIDelete: TMenuItem;
    MISelectDataBetween: TMenuItem;
    procedure BtnAddBookmarkClick(Sender: TObject);
    procedure BookmarksTreeViewFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure BookmarksTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure BookmarksTreeViewNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure BtnDeleteClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure BookmarksTreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure BookmarksTreeViewNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure MISelectDataBetweenClick(Sender: TObject);
    procedure BMListPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    procedure EditorClosed(Sender: TEditorForm);
    procedure EditorGetTaggedRegions(Editor: TEditorForm; Start: TFilePointer;
      AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
    procedure DataChanged(Sender: TEditedData; Addr: TFilePointer;
      OldSize, NewSize: TFilePointer; Value: PByteArray);
    procedure EditorAfterDrawPane(Editor: TEditorForm; Pane: TEditorPane; Canvas: TCanvas);
    procedure UpdateList();
    function FindBookmarkByCaption(const ACaption: string): TBookmark;
    function FindBookmarkByShortcut(AShortcut: Integer): TBookmark;
    function GetNewBookmarkCaption(): string;
    function GetUnusedShortcut(): Integer;
  public
    { Public declarations }
    Bookmarks: TBookmarksList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function AddBookmark(AEditor: TEditorForm; ARange: TFileRange; ACaption: string): TBookmark;
    procedure GoToBookmark(ABookmark: TBookmark);
  end;

implementation

uses
  uMainForm;

{$R *.dfm}

function RangeToStr(const ARange: TFileRange): string;
begin
  Result := IntToStr(ARange.Start);
  if ARange.Size > 0 then
    Result := Result + ' (+' + IntToStr(ARange.Size) + ')';
end;

function AAIdentifier(N: Integer): string;
// Sequence: A, B, ..., Z, AA, AB, ..., AZ, BA, ...
var
  Letters, M, i: Integer;
begin
  // How much letters we need
  Letters := 1;
  M := N;
  while M >= Round(Power(26, Letters)) do
  begin
    Dec(M, Round(Power(26, Letters)));
    Inc(Letters);
  end;
  // Generate letter sequence
  SetLength(Result, Letters);
  for i:=High(Result) downto Low(Result) do
  begin
    Result[i] := Char(Ord('A') + (M mod 26));
    M := M div 26;
  end;
end;

{ TBookmarksFrame }

function TBookmarksFrame.AddBookmark(AEditor: TEditorForm; ARange: TFileRange;
  ACaption: string): TBookmark;
begin
  Result := TBookmark.Create();
  Result.Owner := Bookmarks;
  Result.Editor := AEditor;
  Result.Range := ARange;
  Result.Caption := ACaption;
  Bookmarks.Add(Result);
  if not AEditor.HasEventListener(Self) then
  begin
    AEditor.OnClosed.Add(EditorClosed, Self);
    AEditor.OnGetTaggedRegions.Add(EditorGetTaggedRegions, Self);
    AEditor.OnAfterDrawPane.Add(EditorAfterDrawPane, Self);
    AEditor.Data.OnDataChanged.Add(DataChanged, Self);
  end;
end;

procedure TBookmarksFrame.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var
  isShift, isCtrl, isAlt: Boolean;
  N: Integer;
  BM: TBookmark;
  AEditor: TEditorForm;
  ARange: TFileRange;
  ACaption: string;
begin
  // Handle keyboard shortcuts in editor
  if Msg.message = WM_KEYDOWN then
  begin
    WriteLogFmt('KeyDown: %d', [Msg.wParam]);
    if (Msg.wParam >= Ord(AnsiChar('0'))) and (Msg.wParam <= Ord(AnsiChar('9'))) and
       (Screen.ActiveControl is TEditorPane) then
    begin
      isShift := (GetKeyState(VK_SHIFT) and $8000) <> 0;
      isCtrl := (GetKeyState(VK_CONTROL) and $8000) <> 0;
      isAlt := (GetKeyState(VK_MENU) and $8000) <> 0;
      if isCtrl and not isAlt then
      begin
        N := Msg.wParam - Ord(AnsiChar('0'));
        BM := FindBookmarkByShortcut(N);
        if isShift then
        // Ctrl+Shift+Digit - toggle bookmark
        begin
          AEditor := MainForm.ActiveEditor;
          ARange := AEditor.SelectedRange;
          ACaption := GetNewBookmarkCaption();
          // There is a bookmark with same shortcut?
          if BM <> nil then
          begin
            // If bookmark with same shortcut is already exactly here, remove it
            if (BM.Editor = AEditor) and (BM.Range = ARange) then
              BM.Free
            else
            // Move old bookmark to this position
            begin
              BM.Editor := AEditor;
              BM.Range := ARange;
            end;
          end
          else
          // Add new shortcut
          with AddBookmark(AEditor, AEditor.SelectedRange, GetNewBookmarkCaption()) do
          begin
            Shortcut := N;
          end;
          UpdateList();
        end
        else
        // Ctrl+Digit - go to bookmark
        begin
          if BM <> nil then
            GoToBookmark(BM);
        end;
      end;
    end;
  end;
end;

procedure TBookmarksFrame.BMListPopupMenuPopup(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PBMTreeNode;
  i: Integer;
  AEditor: TEditorForm;
begin
  // If user selected two bookmarks, located in one editor, show "Select data between"
  MISelectDataBetween.Visible := False;
  if BookmarksTreeView.SelectedCount = 2 then
  begin
    i := 0; AEditor := nil;
    for Node in BookmarksTreeView.SelectedNodes() do
    begin
      Data := BookmarksTreeView.GetNodeData(Node);
      if Data <> nil then
      begin
        case i of
          0: AEditor := Data.Bookmark.Editor;
          1: MISelectDataBetween.Visible := (AEditor = Data.Bookmark.Editor);
        end;
        Inc(i);
        if i >= 2 then Break;
      end;
    end;
  end;

  // "Delete" menu item
  MIDelete.Visible := (BookmarksTreeView.SelectedCount > 0);
end;

procedure TBookmarksFrame.BookmarksTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
//var
//  Data: PBMTreeNode;
begin
//  Data := Sender.GetNodeData(Node);
//  if Data <> nil then
//    Finalize(Data^);
end;

procedure TBookmarksFrame.BookmarksTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PBMTreeNode;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if TextType = ttNormal then
    begin
      case Column of
        // Caption
        0: CellText := Data.Bookmark.Caption;
        // Address and size
        1: CellText := RangeToStr(Data.Bookmark.Range);
        // File name
        2: CellText := FitTextInWidth(Data.Bookmark.Editor.DataSource.DisplayName,
                                      BookmarksTreeView.Canvas, BookmarksTreeView.Header.Columns[2].Width - 8, 0.2);
      end;
    end
    else
    begin
      // Subcaption - bookmark shortcut
      if Column = 0 then
      begin
        if Data.Bookmark.Shortcut >= 0 then
          CellText := '(Ctrl+' + IntToStr(Data.Bookmark.Shortcut) + ')';
      end;
    end;
  end;
end;

procedure TBookmarksFrame.BookmarksTreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data: PBMTreeNode;
begin
  // Edit bookmark caption
  Data := Sender.GetNodeData(Node);
  if (Data <> nil) and (Data.Bookmark <> nil) and (Column = 0) then
    Data.Bookmark.Caption := NewText;
end;

procedure TBookmarksFrame.BookmarksTreeViewNodeDblClick(
  Sender: TBaseVirtualTree; const HitInfo: THitInfo);
// On doubleclick: Switch to corresponding editor and bookmark position
var
  Data: PBMTreeNode;
begin
  if HitInfo.HitNode = nil then Exit;
  Data := PBMTreeNode(BookmarksTreeView.GetNodeData(HitInfo.HitNode));
  if (Data = nil) or (Data.Bookmark = nil) then Exit;

  GoToBookmark(Data.Bookmark);
end;

procedure TBookmarksFrame.BookmarksTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if TextType = ttStatic then
    TargetCanvas.Font.Color := clDkGray;
end;

procedure TBookmarksFrame.BtnAddBookmarkClick(Sender: TObject);
// Add bookmark at current selection
var
  AEditor: TEditorForm;
  ARange: TFileRange;
  ACaption: string;
begin
  AEditor := MainForm.ActiveEditor;
  ARange := AEditor.SelectedRange;
  ACaption := GetNewBookmarkCaption();
  with AddBookmark(AEditor, ARange, ACaption) do
  begin
    Shortcut := GetUnusedShortcut();
  end;
  UpdateList();
end;

procedure TBookmarksFrame.BtnDeleteClick(Sender: TObject);
// Delete selected bookmarks
var
  Node: PVirtualNode;
  Data: PBMTreeNode;
begin
  if BookmarksTreeView.SelectedCount = 0 then Exit;
  for Node in BookmarksTreeView.SelectedNodes() do
  begin
    Data := BookmarksTreeView.GetNodeData(Node);
    if Data <> nil then
      Data.Bookmark.Free;
  end;
  UpdateList();
end;

constructor TBookmarksFrame.Create(AOwner: TComponent);
begin
  inherited;
  Bookmarks := TBookmarksList.Create();
  BookmarksTreeView.NodeDataSize := SizeOf(TBMTreeNode);
end;

procedure TBookmarksFrame.DataChanged(Sender: TEditedData; Addr, OldSize,
  NewSize: TFilePointer; Value: PByteArray);
var
  i: Integer;
  Bm: TBookmark;
begin
  // Adjust position of bookmarks in editor that contains this EditedData
  if NewSize <> OldSize then
  begin
    for i:=0 to Bookmarks.Count-1 do
    begin
      Bm := Bookmarks[i];
      if Bm.Editor.Data = Sender then
      begin
        AdjustPositionInData(Bm.Range.Start, Addr, OldSize, NewSize);
        AdjustPositionInData(Bm.Range.AEnd, Addr, OldSize, NewSize);
      end;
    end;
  end;
end;

destructor TBookmarksFrame.Destroy;
begin
  Bookmarks.Free;
  inherited;
end;

procedure TBookmarksFrame.EditorAfterDrawPane(Editor: TEditorForm;
  Pane: TEditorPane; Canvas: TCanvas);
var
  i: Integer;
  BM: TBookmark;
  Rc, Rb: TRect;
  rw, rh: Integer;  // Bookmark indicator size
begin
  //  Draw bookmark indicators in "Addr" pane
  if Pane = Editor.PaneAddr then
  begin
    rh := Pane.CharHeight * 3 div 4;
    rw := rh * 3 div 4;
    Canvas.Pen.Color := Color_BookmarkFr;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Brush.Color := Color_BookmarkBg;
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Color := Pane.Font.Color;
    Canvas.Font.Height := Pane.Font.Height * 3 div 4;

    for i:=0 to Bookmarks.Count-1 do
    begin
      BM := Bookmarks[i];
      if (BM.Editor = Editor) and (Editor.GetByteScreenRect(Pane, BM.Range.Start, Rc)) then
      begin
        Rb.Left := Pane.Width - rw - 2;
        Rb.Top := (Rc.Top + Rc.Bottom) div 2 - rh div 2;
        Rb.Width := rw;
        Rb.Height := rh;
        Canvas.Rectangle(Rb);
        if BM.Shortcut >= 0 then
        begin
          Rb.Inflate(-1, -1);
          Canvas.TextRect(Rb, Rb.Left, Rb.Top, IntToStr(BM.Shortcut));
        end;
      end;
    end;
  end;
end;

procedure TBookmarksFrame.EditorClosed(Sender: TEditorForm);
var
  i: Integer;
  Changed: Boolean;
begin
  // Remove bookmarks of this editor from list
  Changed := False;
  for i:=Bookmarks.Count-1 downto 0 do
    if Bookmarks[i].Editor = Sender then
    begin
      Bookmarks[i].Free;
      Changed := True;
    end;
  if Changed then
    UpdateList()
end;

procedure TBookmarksFrame.EditorGetTaggedRegions(Editor: TEditorForm; Start,
  AEnd: TFilePointer; AData: PByteArray; Regions: TTaggedDataRegionList);
var
  i: Integer;
  Bm: TBookmark;
begin
  // Show bookmarks as tagged regions in Editor
  for i:=0 to Bookmarks.Count-1 do
  begin
    Bm := Bookmarks[i];
    if Bm.Editor = Editor then
      Regions.AddRegion(Bm, Bm.Range.Start, Bm.Range.AEnd, clNone, Color_BookmarkBg, Color_BookmarkFr);
  end;
end;

function TBookmarksFrame.FindBookmarkByCaption(
  const ACaption: string): TBookmark;
var
  i: Integer;
begin
  for i:=0 to Bookmarks.Count-1 do
    if Bookmarks[i].Caption = ACaption then
      Exit(Bookmarks[i]);
  Result := nil;
end;

function TBookmarksFrame.FindBookmarkByShortcut(AShortcut: Integer): TBookmark;
var
  i: Integer;
begin
  for i:=0 to Bookmarks.Count-1 do
    if Bookmarks[i].Shortcut = AShortcut then
      Exit(Bookmarks[i]);
  Result := nil;
end;

function TBookmarksFrame.GetNewBookmarkCaption: string;
// Get next unused caption of form 'Bookmark A', 'Bookmark B', ..., 'Bookmark AA', ...
var
  n: Integer;
begin
  n := -1;
  repeat
    Inc(n);
    Result := 'Bookmark ' + AAIdentifier(n);
  until FindBookmarkByCaption(Result) = nil;
end;

function TBookmarksFrame.GetUnusedShortcut: Integer;
// Get first unused shortcut in the order 1..9, 0
var
  i: Integer;
begin
  for i:=1 to 10 do
    if FindBookmarkByShortcut(i mod 10) = nil then
      Exit(i mod 10);
  Result := -1;
end;

procedure TBookmarksFrame.GoToBookmark(ABookmark: TBookmark);
// Jump to editor and position of ABookmark
var
  AEditor: TEditorForm;
begin
  AEditor := ABookmark.Editor;
  MainForm.ActiveEditor := AEditor;
  AEditor.SelectAndShow(ABookmark.Range.Start, ABookmark.Range.AEnd);
end;

procedure TBookmarksFrame.MISelectDataBetweenClick(Sender: TObject);
// Select data between two selected bookmarks
var
  Node: PVirtualNode;
  Data: PBMTreeNode;
  i: Integer;
  AEditor: TEditorForm;
  Range1, Range2, Range: TFileRange;
begin
  if BookmarksTreeView.SelectedCount < 2 then Exit;
  i := 0; AEditor := nil;
  for Node in BookmarksTreeView.SelectedNodes() do
  begin
    Data := BookmarksTreeView.GetNodeData(Node);
    if Data <> nil then
    begin
      case i of
        0: Range1 := Data.Bookmark.Range;
        1: Range2 := Data.Bookmark.Range;
      end;
      AEditor := Data.Bookmark.Editor;
      Inc(i);
      if i >= 2 then Break;
    end;
  end;

  if i < 2 then Exit;
  if Range2.Start >= Range1.AEnd then
    Range := TFileRange.Create(Range1.AEnd, Range2.Start)
  else
  if Range1.Start >= Range2.AEnd then
    Range := TFileRange.Create(Range2.AEnd, Range1.Start)
  else
    Exit;

  AEditor.SelectAndShow(Range.Start, Range.AEnd);
end;

procedure TBookmarksFrame.UpdateList;
// Show Bookmarks in tree view and update editors.
// Should be called immediately after every change of Bookmarks list to avoid
// invalid outdated pointers in tree view nodes
var
  i: Integer;
  Node: PVirtualNode;
//  Editors: TObjectList<TEditorForm>;
begin
//  Editors := TObjectList<TEditorForm>.Create(False);
  BookmarksTreeView.BeginUpdate();
  try
    BookmarksTreeView.RootNodeCount := Bookmarks.Count;
    i := 0;
    for Node in BookmarksTreeView.ChildNodes(nil) do
    begin
      PBMTreeNode(BookmarksTreeView.GetNodeData(Node)).Bookmark := Bookmarks[i];
//      if not Editors.Contains(Bookmarks[i].Editor) then
//        Editors.Add(Bookmarks[i].Editor);
      Inc(i);
    end;
  finally
    BookmarksTreeView.EndUpdate();
//    Editors.Free;
  end;

  // Update all visible editors - in case bookmarks were moved/deleted
  try
    with MainForm.ActiveEditor do
      if WindowState = wsMaximized then
        UpdatePanes()
      else
        for i:=0 to MainForm.EditorCount-1 do
          MainForm.Editors[i].UpdatePanes();
  except
    on E: EAbort do ;
  end;
end;

{ TBookmark }

constructor TBookmark.Create;
begin
  inherited;
  Shortcut := -1;
end;

destructor TBookmark.Destroy;
// UpdateList() should be called after freeing bookmarks (except on app termination)
begin
  if not Owner.FDestroying then
    Owner.Extract(Self);
//  Editor.OnClosed.Remove(Self);
  inherited;
end;

procedure TBookmark.SetShortcut(const Value: Integer);
begin
  if FShortcut <> Value then
  begin
    FShortcut := Value;
  end;
end;

{ TBookmarksList }

destructor TBookmarksList.Destroy;
begin
  FDestroying := True;
  inherited;
end;

end.
