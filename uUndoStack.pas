unit uUndoStack;

interface

uses
  Generics.Collections,

  uDWHexTypes, uEditedData, uCallbackList;

type
  // Undo stack contains Actions, each Action consists of Changes

  TUndoStackActionChange = record
    Addr, NewSize: TFilePointer;
    FirstPartIndex: Integer;
    DataParts: TEditedData.TDataPartArray;
  end;

  TUndoStackAction = class
  public
    Caption: string;  // Shown in "Undo ..." menu item
    Code: string;     // Used to combine related changes
    Changes: TList<TUndoStackActionChange>;  // One "Undo" can revert several changes
    SelStart, SelLength: TFilePointer;  // Selection before this action
    constructor Create();
    destructor Destroy(); override;
  end;

  TUndoStack = class
  public type
    TUndoDirection = (udUndo = -1, udNone = 0, udRedo = +1);  // udNode - normal editing
  private
    EditedData: TEditedData;
    UndoingNow: TUndoDirection;
    CurActionCode, CurActionCaption: string;
    procedure BeforePartsReplace(PartIndex1, PartIndex2: Integer; Addr, NewSize: TFilePointer);
    procedure Step(Direction: TUndoDirection);
  public
    Actions: TObjectList<TUndoStackAction>;
    CurPointer: Integer;
    OnActionCreating: TCallbackListP1<{Action:}TUndoStackAction>;
    OnActionReverted: TCallbackListP2<{Action:}TUndoStackAction, {Direction:}TUndoDirection>;
    constructor Create(AEditedData: TEditedData);
    destructor Destroy(); override;
    procedure Undo();
    procedure Redo();
    function CanUndo(var Caption: string): Boolean;
    function CanRedo(var Caption: string): Boolean;
    procedure BeginAction(const Code, Caption: string);
    procedure EndAction();
  end;

implementation

{ TUndoStack }

procedure TUndoStack.BeforePartsReplace(PartIndex1, PartIndex2: Integer;
  Addr, NewSize: TFilePointer);
// Remember data state before operation
var
  i, Count: Integer;
  Action: TUndoStackAction;
  Change: TUndoStackActionChange;
begin
  // Convert this edit to TUndoStackActionChange
  Change.FirstPartIndex := PartIndex1;
  Change.Addr := Addr;
  Change.NewSize := NewSize;
  Count := PartIndex2 - PartIndex1 + 1;
  SetLength(Change.DataParts, Count);
  for i:=0 to Count-1 do
  begin
    Change.DataParts[i] := TEditedData.TDataPart.Create();
    Change.DataParts[i].Assign(EditedData.Parts[PartIndex1+i]);
  end;

  Action := nil;
  case UndoingNow of
    udNone: // It is a new editing operation
      begin
        // Cannot "Redo" after fresh edit
        Actions.DeleteRange(CurPointer, Actions.Count - CurPointer);

        // Combine this change into last action if same code
        if (CurActionCode <> '') and (Actions.Count > 0) and (Actions.Last.Code = CurActionCode) then
          Action := Actions.Last
        else
        begin
          Action := TUndoStackAction.Create();
          Action.Code := CurActionCode;
          CurPointer := Actions.Add(Action) + 1;
          OnActionCreating.Call(Action);
        end;
        Action.Caption := CurActionCaption;
      end;
    udUndo:
      begin
        Action := Actions[CurPointer];
      end;
    udRedo:
      begin
        Action := Actions[CurPointer-1];
      end;
  end;
  Action.Changes.Add(Change);
end;

procedure TUndoStack.BeginAction(const Code, Caption: string);
// Several actions will be combined if same code
begin
  CurActionCode := Code;
  CurActionCaption := Caption;
end;

function TUndoStack.CanRedo(var Caption: string): Boolean;
begin
  Result := (CurPointer < Actions.Count);
  if Result then
    Caption := Actions[CurPointer].Caption
  else
    Caption := '';
end;

function TUndoStack.CanUndo(var Caption: string): Boolean;
begin
  Result := (CurPointer > 0);
  if Result then
    Caption := Actions[CurPointer - 1].Caption
  else
    Caption := '';
end;

constructor TUndoStack.Create(AEditedData: TEditedData);
begin
  inherited Create();
  EditedData := AEditedData;
  EditedData.OnBeforePartsReplace.Add(BeforePartsReplace);
  Actions := TObjectList<TUndoStackAction>.Create(True);
end;

destructor TUndoStack.Destroy;
begin
  EditedData.OnBeforePartsReplace.Remove(BeforePartsReplace);
  Actions.Free;
  inherited;
end;

procedure TUndoStack.EndAction;
begin
  CurActionCode := '';
  CurActionCaption := '';
end;

procedure TUndoStack.Redo;
begin
  Step(udRedo);
end;

procedure TUndoStack.Step(Direction: TUndoDirection);
// Move within action stack (-1 undo, +1 redo)
var
  Action: TUndoStackAction;
  TmpChanges: TList<TUndoStackActionChange>;
  i: Integer;
begin
  if ((Direction = udUndo) and (CurPointer = 0)) or
     ((Direction = udRedo) and (CurPointer = Actions.Count)) then Exit;
  if Direction = udUndo then
    Action := Actions[CurPointer - 1]
  else
    Action := Actions[CurPointer];
  // Move changes to temporary list so we can collect alternative (redo) changes to Action.Changes
  TmpChanges := TList<TUndoStackActionChange>.Create();
  TmpChanges.AddRange(Action.Changes);
  Action.Changes.Clear();

  // Apply changes from this Action in reverse order.
  // This will trigger BeforePartsReplace callbacks which will populate
  // our Action with inversed changes for subsequent redo/undo operation
  Inc(CurPointer, Ord(Direction));
  UndoingNow := Direction;
  try
    for i:=TmpChanges.Count-1 downto 0 do
    begin
      EditedData.ReplaceParts(TmpChanges[i].Addr, TmpChanges[i].NewSize, TmpChanges[i].DataParts);
    end;
  finally
    UndoingNow := udNone;
    TmpChanges.Free;
  end;
  OnActionReverted.Call(Action, Direction);
end;

procedure TUndoStack.Undo;
begin
  Step(udUndo);
end;

{ TUndoStackItem }

constructor TUndoStackAction.Create;
begin
  inherited;
  Changes := TList<TUndoStackActionChange>.Create();
end;

destructor TUndoStackAction.Destroy;
var
  i, j: Integer;
begin
  for i:=0 to Changes.Count-1 do
    for j:=0 to Length(Changes[i].DataParts)-1 do
      Changes[i].DataParts[j].Free;
  Changes.Free;
  inherited;
end;

end.
