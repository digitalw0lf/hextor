unit uCallbackList;

interface

type
  // Multiple-subscriber callback mechanism
  // (inspired by signal-slot in Qt)

  TCallbackListP1<T1> = record
  public type
    TCallback = reference to procedure(Param1: T1);
  private
    FList: array of TCallback;
  public
    procedure Add(Method: TCallback);
    procedure Remove(Method: TCallback);
    procedure Call(Param1: T1);
  end;

  TCallbackListP2<T1, T2> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2);
  private
    FList: array of TCallback;
  public
    procedure Add(Method: TCallback);
    procedure Remove(Method: TCallback);
    procedure Call(Param1: T1; Param2: T2);
  end;

  TCallbackListP3<T1, T2, T3> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2; Param3: T3);
  private
    FList: array of TCallback;
  public
    procedure Add(Method: TCallback);
    procedure Remove(Method: TCallback);
    procedure Call(Param1: T1; Param2: T2; Param3: T3);
  end;


implementation

{ TCallbackListP1<T1> }

procedure TCallbackListP1<T1>.Add(Method: TCallback);
begin
  FList := FList + [TCallback(Method)];
end;

procedure TCallbackListP1<T1>.Call(Param1: T1);
var
  i: Integer;
begin
  for i:=0 to Length(FList)-1 do
    FList[i](Param1);
end;

procedure TCallbackListP1<T1>.Remove(Method: TCallback);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if TCallback(FList[i]) = TCallback(Method) then
      Delete(FList, i, 1);
end;

{ TCallbackListP2<T1, T2> }

procedure TCallbackListP2<T1, T2>.Add(Method: TCallback);
begin
  FList := FList + [TCallback(Method)];
end;

procedure TCallbackListP2<T1, T2>.Call(Param1: T1; Param2: T2);
var
  i: Integer;
begin
  for i:=0 to Length(FList)-1 do
    FList[i](Param1, Param2);
end;

procedure TCallbackListP2<T1, T2>.Remove(Method: TCallback);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if TCallback(FList[i]) = TCallback(Method) then
      Delete(FList, i, 1);
end;

{ TCallbackListP3<T1, T2, T3> }

procedure TCallbackListP3<T1, T2, T3>.Add(Method: TCallback);
begin
  FList := FList + [TCallback(Method)];
end;

procedure TCallbackListP3<T1, T2, T3>.Call(Param1: T1; Param2: T2; Param3: T3);
var
  i: Integer;
begin
  for i:=0 to Length(FList)-1 do
    FList[i](Param1, Param2, Param3);
end;

procedure TCallbackListP3<T1, T2, T3>.Remove(Method: TCallback);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if TCallback(FList[i]) = TCallback(Method) then
      Delete(FList, i, 1);
end;

end.
