unit uSkipList;

interface

uses
  Types, SysUtils, Classes, RTLConsts, Generics.Collections, Generics.Defaults,
  {Graphics,} Math;

type
  TSkipListSet<T> = class(TEnumerable<T>)
  private type
    PT = ^T;
    PItem = ^TItem;
    TItem = record
      Next, Down, Up: PItem;
      ChildCount: Integer;
      Value: PT;
    end;
    TItemPath = array of PItem;
  public type
    TElement = T;
    TEnumerator = class(TEnumerator<T>)
    private
      FOwner: TSkipListSet<T>;
      FItem: PItem;
      FLastItem: PItem;  // If assigned, enumeration stops after this item
      FInited: Boolean;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(Owner: TSkipListSet<T>);
    end;
    // For auto-refcounting
    IEnumerableRange = interface(IInterface)
      function GetEnumerator: TEnumerator;
    end;
    TEnumerableRange = class(TInterfacedObject, IEnumerableRange)
    protected
      FOwner: TSkipListSet<T>;
      FFirstItem, FLastItem: PItem;
    public
      constructor Create(Owner: TSkipListSet<T>);
      function GetEnumerator: TEnumerator;
    end;
  private
    FComparer: IComparer<T>;
    FLevels, FProb: Byte;
    FHeads: array of PItem;
    FOnNotify: TCollectionNotifyEvent<T>;
    FOwnsObjects: Boolean;
    function FindItemLEQ(const Key: T): TItemPath;
    function FindItemLE(const Key: T): TItemPath;
    function Insert(Path: TItemPath; Value: T): PItem;
    function Promote(After: PItem; Item: PItem): PItem;
    function DoExtract(const Key: T; Action: TCollectionNotification): T;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create(ALevels: Byte = 4; AProb: Byte = 4; AComparer: IComparer<T> = nil);
    destructor Destroy; override;
    procedure Clear;
    procedure AddOrSet(Value: T);
    function TryFetch(const Key: T; var Value: T): Boolean;
    // Находит элемент строго меньше указанного
    function FindLE(const Key: T; var Value: T): Boolean;
    // Находит элемент меньший или равный указанному
    function FindLEQ(const Key: T; var Value: T): Boolean;
    // Находит элемент строго больше указанного
    function FindGE(const Key: T; var Value: T): Boolean;
    // Находит элемент больший или равный указанному
    function FindGEQ(const Key: T; var Value: T): Boolean;
    function HasKey(const Key: T): Boolean;
    // Возвращает значение и удаляет его из списка
    function Extract(const Key: T): T;
    procedure Remove(const Key: T);
    function Get(const Key: T): T;
    function First: T;
    // Возвращает Enumerator для элемента меньшего или равного указанному
    function GetEnumeratorLEQ(const Key: T): TEnumerator;
    // Возвращает Enumerator для элемента строго меньше указанного
    function GetEnumeratorLE(const Key: T): TEnumerator;
    // Возвращает Enumerator для элемента большего или равного указанному
    function GetEnumeratorGEQ(const Key: T): TEnumerator;
    // Возвращает Enumerator для элемента строго больше указанного
    function GetEnumeratorGE(const Key: T): TEnumerator;
//    function GetEnumeratorForRange(const FirstKey, LastKey: T): TEnumerator;
    function EnumerateRange(const FirstKey, LastKey: T): IEnumerableRange;
    function EnumerateFrom(const FirstKey: T): IEnumerableRange;
    function Count(): Integer;
    function IsEmpty(): Boolean;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    //    procedure DebugDraw(Canvas: TCanvas; R: TRect);
  end;

  TSkipListMap<TKey, TValue> = class(TSkipListSet<TPair<TKey, TValue>>)
  public
    constructor Create(ALevels: Byte = 4; AProb: Byte = 4; AComparer: IComparer<TKey> = nil);
    procedure AddOrSet(const Key: TKey; Value: TValue);
    function TryFetch(const Key: TKey; var Value: TValue): Boolean;
    function HasKey(const Key: TKey): Boolean;
    procedure Remove(const Key: TKey);
    function Extract(const Key: TKey): TValue;
    function Get(const Key: TKey): TValue;
  end;

  TInterval<T> = record
    Start,
    Stop: T;
    constructor Create(const AStart, AStop: T);
  end;

  TIntRangeList<TValue> = class
  public type
    TInterval = TInterval<Integer>;
  private type
    TIntComparer = class(TComparer<TInterval>, IComparer<TInterval>)
    public
      function Compare(const Left, Right: TInterval): Integer; override;
    end;
  private
    FMap: TSkipListMap<TInterval,TValue>;
    FIntervalComparer: IComparer<TInterval>;
    FOnNotify: TCollectionNotifyEvent<TPair<TInterval, TValue>>;
    function FindPair(const Key: Integer; var Pair: TPair<TInterval,TValue>): Boolean;
    procedure OnMapNotify(Sender: TObject; const Item: TPair<TInterval,TValue>;
      Action: TCollectionNotification);
  public
    constructor Create(ALevels: Byte = 4; AProb: Byte = 4);
    destructor Destroy; override;
    procedure Clear;
    procedure Insert(const AStart, AStop: Integer; const Value: TValue); overload;
    procedure Insert(const Interval: TInterval;  const Value: TValue); overload;
    function TryFetchInterval(const Key: Integer; var Interval: TInterval): Boolean;
    function TryFetch(const Key: Integer; var Value: TValue): Boolean;
    procedure Remove(const Interval: TInterval);
    function Extract(const Interval: TInterval): TValue;
    function Get(const Interval: TInterval): TValue;
    property Map: TSkipListMap<TInterval,TValue> read FMap;
    property OnNotify: TCollectionNotifyEvent<TPair<TInterval,TValue>> read FOnNotify write FOnNotify;
  end;

implementation

{ TSkipListSet<T> }

procedure TSkipListSet<T>.AddOrSet(Value: T);
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Value);
  if Assigned(Path[0]) and (FComparer.Compare(Value, Path[0].Value^) = 0) then
  begin
    Notify(Path[0].Value^, cnRemoved);
    Path[0].Value^ := Value;
    Notify(Path[0].Value^, cnAdded);
  end
  else
    Insert(Path, Value);
end;

procedure TSkipListSet<T>.Clear;
var
  i: Integer;
  Item, Tmp: PItem;
begin
  for i := 0 to FLevels - 1 do
  begin
    Item := FHeads[i];
    while Assigned(Item) do
    begin
      if i = 0 then
      begin
        Notify(Item.Value^, cnRemoved);
        Dispose(Item.Value);
      end;
      Tmp := Item.Next;
      Dispose(Item);
      Item := Tmp;
    end;
    FHeads[i] := nil;
  end;
end;

function TSkipListSet<T>.Count: Integer;
var
  Item: PItem;
begin
  Result := 0;
  Item := FHeads[0];
  while Assigned(Item) do
  begin
    Inc(Result);
    Item := Item.Next;
  end;
end;

constructor TSkipListSet<T>.Create(ALevels, AProb: Byte;
  AComparer: IComparer<T>);
begin
  FLevels := ALevels;
  FProb := AProb;
  if Assigned(AComparer) then
    FComparer := AComparer
  else
    FComparer := TComparer<T>.Default;
  SetLength(FHeads, FLevels);
end;

{procedure TSkipListSet<T>.DebugDraw(Canvas: TCanvas; R: TRect);
var
  Item, UpItem: PItem;
  x, y, W, H: Integer;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);
  Canvas.Brush.Color := clBlack;
  Item := FHeads[0];
  H := (R.Bottom-R.Top) div FLevels;
  W := 1;
  x := 0;
  while Assigned(Item) do
  begin
    y := 1;
    UpItem := Item.Up;
    while Assigned(UpItem) do
    begin
      UpItem := UpItem.Up;
      Inc(y);
    end;
    Canvas.FillRect(Rect(X*w,H*y, (X+1)*w,0));
    Item := Item.Next;
    Inc(x);
  end;
end;{}

destructor TSkipListSet<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TSkipListSet<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSkipListSet<T>.EnumerateFrom(const FirstKey: T): IEnumerableRange;
var
  Path: TItemPath;
  Enum: TEnumerableRange;
begin
  Enum := TEnumerableRange.Create(Self);

  Path := FindItemLEQ(FirstKey);
  if Assigned(Path[0]) then
  begin
    Enum.FFirstItem := Path[0];
    if FComparer.Compare(FirstKey, Enum.FFirstItem.Value^) > 0 then
      Enum.FFirstItem := Enum.FFirstItem.Next;  // FindItem возвращает предыдущий элемент, если не найдено совпадение
  end;
  Result := Enum;
end;

function TSkipListSet<T>.EnumerateRange(const FirstKey,
  LastKey: T): IEnumerableRange;
var
  Path: TItemPath;
  Enum: TEnumerableRange;
begin
  Enum := TEnumerableRange.Create(Self);
  // Первый элемент
  Path := FindItemLEQ(FirstKey);
  if Assigned(Path[0]) then
  begin
    Enum.FFirstItem := Path[0];
    if FComparer.Compare(FirstKey, Enum.FFirstItem.Value^) > 0 then
      Enum.FFirstItem := Enum.FFirstItem.Next;  // FindItem возвращает предыдущий элемент, если не найдено совпадение
  end;
  // Обрабатываем ситуацию когда в диапазон не попало ни одного элемента
  if (Enum.FFirstItem <> nil) and
     (FComparer.Compare(Enum.FFirstItem.Value^, LastKey) > 0) then
  begin
    Enum.FFirstItem := nil;
    Exit(Enum);
  end;
  // Последний элемент
  Path := FindItemLEQ(LastKey);
  if Assigned(Path[0]) then
    Enum.FLastItem := Path[0];
  Result := Enum;
end;

function TSkipListSet<T>.Extract(const Key: T): T;
begin
  Result := DoExtract(Key, cnExtracted);
end;

function TSkipListSet<T>.DoExtract(const Key: T; Action: TCollectionNotification): T;
var
  Path: TItemPath;
  i: Integer;
  Tmp, NextUp: PItem;
begin
  SetLength(Path, FLevels);
  Path[0] := nil;
  if (FHeads[0] = nil) or (FComparer.Compare(Key, FHeads[0].Value^) < 0) then
    Exit(Default(T));
  // Удаляется первый элемент - это особый случай
  if (FComparer.Compare(Key, FHeads[0].Value^) = 0) then
  begin
    Result := FHeads[0].Value^;
    Notify(Result, Action);
    Dispose(FHeads[0].Value);
    for i := 0 to FLevels - 1 do
    begin
      // Кладём второй элемент на место первого, а первый удаляем
      Tmp := FHeads[i];
      if i = 0 then
        FHeads[i] := Tmp.Next
      else if Assigned(FHeads[i - 1]) then
        FHeads[i] := FHeads[i - 1].Up
      else
        FHeads[i] := nil;
      Dispose(Tmp);
      // Если удалённый элемент был последним в списке, то нужно создать новый элемент
      if (i > 0) and Assigned(FHeads[0]) and not Assigned(FHeads[i]) then
      begin
        New(FHeads[i]);
        FHeads[i].Next := nil;
        FHeads[i].Up := nil;
        FHeads[i].Down := FHeads[i-1];
        FHeads[i-1].Up := FHeads[i];
        FHeads[i].Value := FHeads[i-1].Value;
      end;
    end;
    Exit;
  end;
  i := FLevels - 1;
  Path[i] := FHeads[i];
  // Ищем элемент
  while True do
  begin
    if Assigned(Path[i].Next) and (FComparer.Compare(Key, Path[i].Next.Value^) > 0) then
      Path[i] := Path[i].Next
    else if Assigned(Path[i].Down) then
    begin
      Assert(i > 0);
      Path[i-1] := Path[i].Down;
      Dec(i);
    end
    else
      Break;
  end;
  if not Assigned(Path[0]) or not Assigned(Path[0].Next) then
    Exit(Default(T));
  Result := Path[0].Next.Value^;
  Notify(Result, Action);
  Dispose(Path[0].Next.Value);
  Tmp := Path[0].Next;
  // Из каждого уровня удаляем элемент, сохраняя связность списков
  for i := 0 to FLevels - 1 do
  begin
    NextUp := Tmp.Up;
    Path[i].Next := Tmp.Next;
    Dispose(Tmp);
    if not Assigned(NextUp) then
      Exit;
    Tmp := NextUp;
  end;
end;

function TSkipListSet<T>.FindItemLEQ(const Key: T): TItemPath;
var
  i: Integer;
begin
  SetLength(Result, FLevels);
  Result[0] := nil;
  if (FHeads[0] = nil) then
    Exit;
  if FComparer.Compare(Key, FHeads[0].Value^) < 0 then
    Exit;
  i := FLevels - 1;
  Result[i] := FHeads[i];
  while True do
  begin
    if Assigned(Result[i].Next) and (FComparer.Compare(Key, Result[i].Next.Value^) >= 0) then
      Result[i] := Result[i].Next
    else if Assigned(Result[i].Down) then
    begin
//      Assert(i > 0);
      Result[i-1] := Result[i].Down;
      Dec(i);
    end
    else
      Exit;
  end;
end;

function TSkipListSet<T>.FindLE(const Key: T; var Value: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLE(Key);
  Result := True;
  if Assigned(Path[0]) then
    Value := Path[0].Value^
  else
    Result := False;
end;

function TSkipListSet<T>.FindLEQ(const Key: T; var Value: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Key);
  Result := True;
  if Assigned(Path[0]) then
    Value := Path[0].Value^
  else
    Result := False;
end;

function TSkipListSet<T>.FindGE(const Key: T; var Value: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Key);
  Result := True;
  if Assigned(Path[0]) and Assigned(Path[0].Next) then
    Value := Path[0].Next.Value^
  else
    Result := False;
end;

function TSkipListSet<T>.FindGEQ(const Key: T; var Value: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLE(Key);
  Result := True;
  if Assigned(Path[0]) and Assigned(Path[0].Next) then
    Value := Path[0].Next.Value^
  else
    Result := False;
end;

function TSkipListSet<T>.FindItemLE(const Key: T): TItemPath;
var
  i: Integer;
begin
  SetLength(Result, FLevels);
  Result[0] := nil;
  if (FHeads[0] = nil) then
    Exit;
  if FComparer.Compare(Key, FHeads[0].Value^) < 0 then
    Exit;
  i := FLevels - 1;
  Result[i] := FHeads[i];
  while True do
  begin
    if Assigned(Result[i].Next) and (FComparer.Compare(Key, Result[i].Next.Value^) > 0) then
      Result[i] := Result[i].Next
    else if Assigned(Result[i].Down) then
    begin
//      Assert(i > 0);
      Result[i-1] := Result[i].Down;
      Dec(i);
    end
    else
      Exit;
  end;
end;

function TSkipListSet<T>.First: T;
begin
  if not Assigned(FHeads[0]) then
    Result := Default(T)
  else
    Result := FHeads[0].Value^;
end;

function TSkipListSet<T>.Get(const Key: T): T;
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Key);
  if not Assigned(Path[0]) or (FComparer.Compare(Key, Path[0].Value^) <> 0) then
    raise EListError.CreateRes(@SGenericItemNotFound)
  else
    Result := Path[0].Value^;
end;

function TSkipListSet<T>.GetEnumeratorGE(const Key: T): TEnumerator;
var
  Path: TItemPath;
begin
  Result := TEnumerator(DoGetEnumerator());
  Path := FindItemLEQ(Key);
  if Assigned(Path[0]) then
  begin
    if Assigned(Path[0].Next) then
      Result.FItem := Path[0].Next
    else
      Result.FItem := nil;
  end;
end;

function TSkipListSet<T>.GetEnumeratorGEQ(const Key: T): TEnumerator;
var
  Path: TItemPath;
begin
  Result := TEnumerator(DoGetEnumerator());
  Path := FindItemLE(Key);
  if Assigned(Path[0]) then
  begin
    if Assigned(Path[0].Next) then
      Result.FItem := Path[0].Next
    else
      Result.FItem := nil;
  end;
end;

function TSkipListSet<T>.GetEnumeratorLE(const Key: T): TEnumerator;
var
  Path: TItemPath;
begin
  Result := TEnumerator(DoGetEnumerator());
  Path := FindItemLE(Key);
  if Assigned(Path[0]) then
    Result.FItem := Path[0];
end;

function TSkipListSet<T>.GetEnumeratorLEQ(const Key: T): TEnumerator;
var
  Path: TItemPath;
begin
  Result := TEnumerator(DoGetEnumerator());
  Path := FindItemLEQ(Key);
  if Assigned(Path[0]) then
    Result.FItem := Path[0];
end;

//function TSkipListSet<T>.GetEnumeratorForRange(const FirstKey,
//  LastKey: T): TEnumerator;
//var
//  Path: TItemPath;
//begin
//  Result := TEnumerator(DoGetEnumerator());
//  Path := FindItem(FirstKey);
//  if Assigned(Path[0]) then
//    Result.FItem := Path[0];
//  Path := FindItem(LastKey);
//  if Assigned(Path[0]) then
//    Result.FLastItem := Path[0];
//end;

function TSkipListSet<T>.HasKey(const Key: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Key);
  if Assigned(Path[0]) and (FComparer.Compare(Key, Path[0].Value^) = 0) then
    Result := True
  else
    Result := False;
end;

function TSkipListSet<T>.Insert(Path: TItemPath; Value: T): PItem;
var
  i: Integer;
  Old, UpItem: PItem;
begin
  if not Assigned(Path[0]) then
  begin
    for i := 0 to FLevels - 1 do
    begin
      Old := FHeads[i];
      New(FHeads[i]);
      if i = 0 then
      begin
        if Assigned(Old) then
          Old.Up := nil;
        FHeads[i].Next := Old;
        FHeads[i].Down := nil;
        FHeads[i].Up := nil;
        New(FHeads[i].Value);
        FHeads[i].Value^ := Value;
      end
      else
      begin
        if Assigned(Old) {and ((FHeads[i-1].Next <> Old.Down) or (Random > 1/FProb))} then
        begin
          FHeads[i].Next := Old.Next;
          Dispose(Old);
//          if Assigned(FHeads[i-1].Next) then
//            FHeads[i-1].Next.Up := nil;
        end
        else
        begin
          FHeads[i].Next := Old;
        end;
        FHeads[i].Up := nil;
        FHeads[i].Down := FHeads[i-1];
        FHeads[i-1].Up := FHeads[i];
        FHeads[i].Value := FHeads[i-1].Value;
      end;
    end;
    Result := FHeads[0];
    UpItem := Result.Next;
    if Assigned(UpItem) then
      for i := 1 to FLevels - 1 do
        if Random < 1/FProb then
          UpItem := Promote(FHeads[i], UpItem)
        else
          Break;
    Notify(Result.Value^, cnAdded);
    Exit;
  end;
  Old := Path[0].Next;
  New(Result);
  Result.Up := nil;
  Result.Down := nil;
  Result.Next := Old;
  Path[0].Next := Result;
  New(Result.Value);
  Result.Value^ := Value;
  Notify(Result.Value^, cnAdded);
  UpItem := Result;
  for i := 1 to FLevels - 1 do
    if Random < 1/FProb then
      UpItem := Promote(Path[i], UpItem)
    else
      Exit;
end;

function TSkipListSet<T>.IsEmpty: Boolean;
begin
  Result := not Assigned(FHeads[0]);
end;

procedure TSkipListSet<T>.Notify(const Item: T;
  Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
  if OwnsObjects and (Action = cnRemoved) then
    PObject(@Item)^.DisposeOf;
end;

function TSkipListSet<T>.Promote(After: PItem; Item: PItem): PItem;
var
  Old: PItem;
begin
  Old := After.Next;
  New(Result);
  Result.Up := nil;
  Result.Next := Old;
  After.Next := Result;
  Result.Value := Item.Value;
  Result.Down := Item;
  Item.Up := Result;
end;

procedure TSkipListSet<T>.Remove(const Key: T);
var
  V: T;
begin
  V := DoExtract(Key, cnRemoved);
end;

function TSkipListSet<T>.TryFetch(const Key: T; var Value: T): Boolean;
var
  Path: TItemPath;
begin
  Path := FindItemLEQ(Key);
  Result := True;
  if Assigned(Path[0]) and (FComparer.Compare(Key, Path[0].Value^) = 0) then
    Value := Path[0].Value^
  else
    Result := False;
end;

{ TSkipListSet<T>.TEnumerator }

constructor TSkipListSet<T>.TEnumerator.Create(Owner: TSkipListSet<T>);
begin
  FOwner := Owner;
  FItem := FOwner.FHeads[0];
  FInited := False;
end;

function TSkipListSet<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := FItem.Value^;
end;

function TSkipListSet<T>.TEnumerator.DoMoveNext: Boolean;
begin
  if not FInited then
  begin
    Result := Assigned(FItem);
    FInited := True;
    Exit;
  end;
  Result := Assigned(FItem) and (FItem <> FLastItem) and (Assigned(FItem.Next));
  if Result then
    FItem := FItem.Next;
end;

{ TSkipListMap<TKey, TValue> }

procedure TSkipListMap<TKey, TValue>.AddOrSet(const Key: TKey; Value: TValue);
begin
  inherited AddOrSet(TPair<TKey, TValue>.Create(Key, Value));
end;

constructor TSkipListMap<TKey, TValue>.Create(ALevels, AProb: Byte;
  AComparer: IComparer<TKey>);
var
  KCmp: IComparer<TKey>;
  Cmp: IComparer<TPair<TKey, TValue>>;
begin
  if Assigned(AComparer) then
    KCmp := AComparer
  else
    KCmp := TComparer<TKey>.Default;
  Cmp := TComparer<TPair<TKey, TValue>>.Construct(
    function(const Left, Right: TPair<TKey, TValue>): Integer
    begin
      Result := KCmp.Compare(left.Key, Right.Key);
    end
  );
  inherited Create(ALevels, AProb, Cmp);
end;

function TSkipListMap<TKey, TValue>.Extract(const Key: TKey): TValue;
var
  Pair: TPair<TKey, TValue>;
begin
  Pair := inherited Extract(TPair<TKey, TValue>.Create(Key, Default(TValue)));
  Result := Pair.Value;
end;

function TSkipListMap<TKey, TValue>.Get(const Key: TKey): TValue;
var
  Pair: TPair<TKey, TValue>;
begin
  Pair := inherited Get(TPair<TKey, TValue>.Create(Key, Default(TValue)));
  Result := Pair.Value;
end;

function TSkipListMap<TKey, TValue>.HasKey(const Key: TKey): Boolean;
begin
  Result := inherited HasKey(TPair<TKey, TValue>.Create(Key, Default(TValue)));
end;

procedure TSkipListMap<TKey, TValue>.Remove(const Key: TKey);
begin
  inherited Remove(TPair<TKey, TValue>.Create(Key, Default(TValue)));
end;

function TSkipListMap<TKey, TValue>.TryFetch(const Key: TKey;
  var Value: TValue): Boolean;
var
  Pair: TPair<TKey, TValue>;
begin
  Result := inherited TryFetch(TPair<TKey, TValue>.Create(Key, Default(TValue)), Pair);
  if Result then
    Value := Pair.Value;
end;

{ TIntRangeList<TValue> }

procedure TIntRangeList<TValue>.Clear;
begin
  FMap.Clear;
end;

constructor TIntRangeList<TValue>.Create(ALevels, AProb: Byte);
begin
  FIntervalComparer := TIntComparer.Create;
  FMap := TSkipListMap<TInterval,TValue>.Create(ALevels, AProb, FIntervalComparer);
  FMap.OnNotify := OnMapNotify;
end;

destructor TIntRangeList<TValue>.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TIntRangeList<TValue>.Extract(const Interval: TInterval): TValue;
begin
  Result := FMap.Extract(Interval);
end;

function TIntRangeList<TValue>.FindPair(const Key: Integer;
  var Pair: TPair<TInterval, TValue>): Boolean;
var
  TmpKey, TmpVal: TPair<TInterval,TValue>;
begin
  TmpKey.Key.Start := Key;
  Result := TSkipListSet<TPair<TInterval, TValue>>(FMap).TryFetch(TmpKey, TmpVal);
  if not Result then
    Result := FMap.FindLE(TmpKey, TmpVal);
  if Result then
    Pair := TmpVal;
end;

function TIntRangeList<TValue>.Get(const Interval: TInterval): TValue;
begin
  Result := FMap.Get(Interval);
end;

procedure TIntRangeList<TValue>.Insert(const Interval: TInterval;
  const Value: TValue);
var
  Pair: TPair<TInterval,TValue>;
  ToRemove: array of TInterval;
  Enum: TEnumerator<TPair<TInterval,TValue>>;
  I: TInterval;
  ChangeLast: Boolean;
  V: TValue;
  n: Integer;
begin
  Enum := nil;
  try
    if FindPair(Interval.Start, Pair) then
    begin
      Enum := FMap.GetEnumeratorLEQ(Pair);
      if Pair.Key.Stop >= Interval.Start then
      begin
        if (Pair.Key.Start = Interval.Start) and (Pair.Key.Stop = Interval.Stop) then
        begin
          FMap.AddOrSet(Interval, Value);
          Exit;
        end;
        if (Pair.Key.Stop > Interval.Stop) {and (Pair.Key.Stop <> Integer.MaxValue){} then
        begin
          I.Start := Interval.Stop + 1;
          I.Stop := Pair.Key.Stop;
          FMap.AddOrSet(I, Pair.Value);
        end;
        Pair.Key.Stop := Pred(Interval.Start);
        FMap.AddOrSet(Pair.Key, Pair.Value);
      end;
    end
    else
      Enum := FMap.GetEnumerator;
    ChangeLast := False;
    while Enum.MoveNext do
    begin
      if Enum.Current.Key.Stop <= Interval.Stop then
      begin
        ToRemove := ToRemove + [Enum.Current.Key]
      end
      else if Enum.Current.Key.Start <= Interval.Stop then
      begin
        ChangeLast := True;
        Break;
      end
      else
        Break;
    end;
    if ChangeLast then
    begin
      I := Enum.Current.Key;
      V := FMap.Extract(I);
      I.Start := Succ(Interval.Stop);
      FMap.AddOrSet(I, V);
    end;
    for n := 0 to High(ToRemove) do
      FMap.Remove(ToRemove[n]);
    FMap.AddOrSet(Interval, Value);
  finally
    Enum.Free;
  end;
end;

procedure TIntRangeList<TValue>.OnMapNotify(Sender: TObject;
  const Item: TPair<TInterval, TValue>; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

procedure TIntRangeList<TValue>.Insert(const AStart, AStop: Integer;
  const Value: TValue);
var
  I: TInterval;
begin
  I.Start := AStart;
  I.Stop := AStop;
  Insert(I, Value);
end;

procedure TIntRangeList<TValue>.Remove(const Interval: TInterval);
begin
  FMap.Remove(Interval);
end;

function TIntRangeList<TValue>.TryFetch(const Key: Integer;
  var Value: TValue): Boolean;
var
  TmpVal: TPair<TInterval,TValue>;
begin
  Result := FindPair(Key, TmpVal);
  if Result and (TmpVal.Key.Stop < Key) then
    Result := False;
  if Result then
    Value := TmpVal.Value;
end;

function TIntRangeList<TValue>.TryFetchInterval(const Key: Integer;
  var Interval: TInterval): Boolean;
var
  TmpVal: TPair<TInterval,TValue>;
begin
  Result := FindPair(Key, TmpVal);
  if Result and (TmpVal.Key.Stop < Key) then
    Result := False;
  if Result then
    Interval := TmpVal.Key;
end;

{ TIntRangeList<TValue>.TIntComparer }

function TIntRangeList<TValue>.TIntComparer.Compare(const Left,
  Right: TInterval): Integer;
begin
  Result := CompareValue(Left.Start, Right.Start);
end;

{ TInterval<T> }

constructor TInterval<T>.Create(const AStart, AStop: T);
begin
  Start := AStart;
  Stop := AStop;
end;

{ TSkipListSet<T>.TEnumerableRange }

constructor TSkipListSet<T>.TEnumerableRange.Create(Owner: TSkipListSet<T>{;
  AFirstItem, ALastItem: PItem});
begin
  inherited Create();
  FOwner := Owner;
//  FFirstItem := AFirstItem;
//  FLastItem := ALastItem;
end;

function TSkipListSet<T>.TEnumerableRange.GetEnumerator: TEnumerator;
var
  AEnumerator: TEnumerator;
begin
  AEnumerator := TEnumerator.Create(FOwner);
  AEnumerator.FItem := FFirstItem;
  AEnumerator.FLastItem := FLastItem;
  Result := AEnumerator;
end;

end.
