{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uUndoStack;

interface

uses
  Generics.Collections,

  uHextorTypes, uEditedData, uCallbackList;

type
  // Undo stack contains Actions, each Action consists of Changes

  TUndoStackActionChange = record
    Addr, NewSize: TFilePointer;
    DataParts: TEditedData.TDataPartArray;
  end;

  // Saved caret/selection state
  TSelectionState = record
    CaretPos, SelStart, SelLength: TFilePointer;
  end;

  TUndoStackAction = class
  public
    Caption: string;  // Shown in "Undo ..." menu item
    Code: string;     // Used to combine related changes
    Changes: TList<TUndoStackActionChange>;  // One "Undo" can revert several changes
    //SelStart, SelLength: TFilePointer;  // Selection before this action
    SelBefore, SelAfter: TSelectionState;  // Selection before/after this action
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
    NestedActionsDepth: Integer;
    procedure BeforePartsReplace(Addr, OldSize, NewSize: TFilePointer);
    procedure Step(Direction: TUndoDirection);
  public
    Actions: TObjectList<TUndoStackAction>;
    CurPointer: Integer;
    OnActionCreating: TCallbackListP1<{Action:}TUndoStackAction>;
    OnActionCreated: TCallbackListP1<{Action:}TUndoStackAction>;
    OnActionReverted: TCallbackListP2<{Action:}TUndoStackAction, {Direction:}TUndoDirection>;
    constructor Create(AEditedData: TEditedData);
    destructor Destroy(); override;
    procedure Undo();
    procedure Redo();
    function CanUndo(var Caption: string): Boolean;
    function CanRedo(var Caption: string): Boolean;
    procedure BeginAction(const Code, Caption: string);
    procedure EndAction();
    procedure Clear();
  end;

implementation

{ TUndoStack }

procedure TUndoStack.BeforePartsReplace(Addr, OldSize, NewSize: TFilePointer);
// Remember data state before operation
var
  Action: TUndoStackAction;
  Change: TUndoStackActionChange;
  AParts: TEditedData.TDataPartList;
  i: Integer;
begin
  // Convert this edit to TUndoStackActionChange
  Change.Addr := Addr;
  Change.NewSize := NewSize;

  AParts := TEditedData.TDataPartList.Create(False);
  EditedData.GetOverlappingParts(Addr, OldSize, AParts);
  //Change.DataParts := AParts.ToArray();
  // Clone parts data
  SetLength(Change.DataParts, AParts.Count);
  for i:=0 to AParts.Count-1 do
  begin
    Change.DataParts[i] := TEditedData.TDataPart.Create(ptUnknown, 0);
    Change.DataParts[i].Assign(AParts[i]);
  end;
  AParts.Free;

  // Find an Action in the stack in which we will add this change
  Action := nil;
  case UndoingNow of  // What's going on now: new operation or undo/redo
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
// Several actions will be combined if same code.
// BeginAction() calls can be nested; only topmost call defines action grouping
begin
  if NestedActionsDepth = 0 then
  begin
    CurActionCode := Code;
    CurActionCaption := Caption;
  end;
  Inc(NestedActionsDepth);
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

procedure TUndoStack.Clear;
begin
  Actions.Clear();
  CurPointer := 0;
  CurActionCode := '';
  CurActionCaption := '';
end;

constructor TUndoStack.Create(AEditedData: TEditedData);
begin
  inherited Create();
  EditedData := AEditedData;
  EditedData.OnBeforePartsReplace.Add(BeforePartsReplace, Self);
  Actions := TObjectList<TUndoStackAction>.Create(True);
end;

destructor TUndoStack.Destroy;
begin
  EditedData.OnBeforePartsReplace.Remove(Self);
  Actions.Free;
  inherited;
end;

procedure TUndoStack.EndAction;
begin
  Assert(NestedActionsDepth > 0, 'Unmatched UndoStack.BeginAction/EndAction');
  Dec(NestedActionsDepth);
  if NestedActionsDepth = 0 then
  begin
    CurActionCode := '';
    CurActionCaption := '';
  end;
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
  Progress.TaskStart(Self, 1, False);
  try
    for i:=TmpChanges.Count-1 downto 0 do
    begin
      EditedData.ReplaceParts(TmpChanges[i].Addr, TmpChanges[i].NewSize, TmpChanges[i].DataParts);
      Progress.Show(TmpChanges.Count-i, TmpChanges.Count);
    end;
  finally
    UndoingNow := udNone;
    TmpChanges.Free;
    Progress.TaskEnd();
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
