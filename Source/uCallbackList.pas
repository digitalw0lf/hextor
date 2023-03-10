{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2023  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uCallbackList;

interface

uses
  Generics.Collections;

type
  // Multiple-subscriber callback mechanism
  // (inspired by signal-slot in Qt)
  // Implemented as record so no need to explicitly call constructor
  // Source code is maintained using automatic generator CallbackListUnitGen

  TCallbackListP1<T1> = record
  public type
    TCallback = reference to procedure(Param1: T1);
    TCallbackPairArray = array of TPair<Pointer, TCallback>;
  private
    FList: TCallbackPairArray;
  public
    procedure Add(Method: TCallback; Id: Pointer = nil);
    procedure Remove(Id: Pointer);
    procedure Clear();
    function HasListener(Id: Pointer): Boolean;
    procedure Call(Param1: T1);
  end;

  TCallbackListP2<T1, T2> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2);
    TCallbackPairArray = array of TPair<Pointer, TCallback>;
  private
    FList: TCallbackPairArray;
  public
    procedure Add(Method: TCallback; Id: Pointer = nil);
    procedure Remove(Id: Pointer);
    procedure Clear();
    function HasListener(Id: Pointer): Boolean;
    procedure Call(Param1: T1; Param2: T2);
  end;

  TCallbackListP3<T1, T2, T3> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2; Param3: T3);
    TCallbackPairArray = array of TPair<Pointer, TCallback>;
  private
    FList: TCallbackPairArray;
  public
    procedure Add(Method: TCallback; Id: Pointer = nil);
    procedure Remove(Id: Pointer);
    procedure Clear();
    function HasListener(Id: Pointer): Boolean;
    procedure Call(Param1: T1; Param2: T2; Param3: T3);
  end;

  TCallbackListP4<T1, T2, T3, T4> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2; Param3: T3; Param4: T4);
    TCallbackPairArray = array of TPair<Pointer, TCallback>;
  private
    FList: TCallbackPairArray;
  public
    procedure Add(Method: TCallback; Id: Pointer = nil);
    procedure Remove(Id: Pointer);
    procedure Clear();
    function HasListener(Id: Pointer): Boolean;
    procedure Call(Param1: T1; Param2: T2; Param3: T3; Param4: T4);
  end;

  TCallbackListP5<T1, T2, T3, T4, T5> = record
  public type
    TCallback = reference to procedure(Param1: T1; Param2: T2; Param3: T3; Param4: T4; Param5: T5);
    TCallbackPairArray = array of TPair<Pointer, TCallback>;
  private
    FList: TCallbackPairArray;
  public
    procedure Add(Method: TCallback; Id: Pointer = nil);
    procedure Remove(Id: Pointer);
    procedure Clear();
    function HasListener(Id: Pointer): Boolean;
    procedure Call(Param1: T1; Param2: T2; Param3: T3; Param4: T4; Param5: T5);
  end;

implementation

{ TCallbackListP1<T1> }

procedure TCallbackListP1<T1>.Add(Method: TCallback; Id: Pointer = nil);
begin
  FList := FList + [TPair<Pointer, TCallback>.Create(Id, TCallback(Method))];
end;

procedure TCallbackListP1<T1>.Call(Param1: T1);
var
  i: Integer;
  TempList: TCallbackPairArray;
begin
  // Copy list so Adding/Removing new callbacks inside callbacks won't break this loop.
  TempList := Copy(FList);
  for i:=0 to Length(TempList)-1 do
    TempList[i].Value(Param1);
end;

procedure TCallbackListP1<T1>.Remove(Id: Pointer);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Delete(FList, i, 1);
end;

procedure TCallbackListP1<T1>.Clear();
begin
  FList := nil;
end;

function TCallbackListP1<T1>.HasListener(Id: Pointer): Boolean;
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Exit(True);
  Result := False;
end;

{ TCallbackListP2<T1, T2> }

procedure TCallbackListP2<T1, T2>.Add(Method: TCallback; Id: Pointer = nil);
begin
  FList := FList + [TPair<Pointer, TCallback>.Create(Id, TCallback(Method))];
end;

procedure TCallbackListP2<T1, T2>.Call(Param1: T1; Param2: T2);
var
  i: Integer;
  TempList: TCallbackPairArray;
begin
  // Copy list so Adding/Removing new callbacks inside callbacks won't break this loop.
  TempList := Copy(FList);
  for i:=0 to Length(TempList)-1 do
    TempList[i].Value(Param1, Param2);
end;

procedure TCallbackListP2<T1, T2>.Remove(Id: Pointer);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Delete(FList, i, 1);
end;

procedure TCallbackListP2<T1, T2>.Clear();
begin
  FList := nil;
end;

function TCallbackListP2<T1, T2>.HasListener(Id: Pointer): Boolean;
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Exit(True);
  Result := False;
end;

{ TCallbackListP3<T1, T2, T3> }

procedure TCallbackListP3<T1, T2, T3>.Add(Method: TCallback; Id: Pointer = nil);
begin
  FList := FList + [TPair<Pointer, TCallback>.Create(Id, TCallback(Method))];
end;

procedure TCallbackListP3<T1, T2, T3>.Call(Param1: T1; Param2: T2; Param3: T3);
var
  i: Integer;
  TempList: TCallbackPairArray;
begin
  // Copy list so Adding/Removing new callbacks inside callbacks won't break this loop.
  TempList := Copy(FList);
  for i:=0 to Length(TempList)-1 do
    TempList[i].Value(Param1, Param2, Param3);
end;

procedure TCallbackListP3<T1, T2, T3>.Remove(Id: Pointer);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Delete(FList, i, 1);
end;

procedure TCallbackListP3<T1, T2, T3>.Clear();
begin
  FList := nil;
end;

function TCallbackListP3<T1, T2, T3>.HasListener(Id: Pointer): Boolean;
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Exit(True);
  Result := False;
end;

{ TCallbackListP4<T1, T2, T3, T4> }

procedure TCallbackListP4<T1, T2, T3, T4>.Add(Method: TCallback; Id: Pointer = nil);
begin
  FList := FList + [TPair<Pointer, TCallback>.Create(Id, TCallback(Method))];
end;

procedure TCallbackListP4<T1, T2, T3, T4>.Call(Param1: T1; Param2: T2; Param3: T3; Param4: T4);
var
  i: Integer;
  TempList: TCallbackPairArray;
begin
  // Copy list so Adding/Removing new callbacks inside callbacks won't break this loop.
  TempList := Copy(FList);
  for i:=0 to Length(TempList)-1 do
    TempList[i].Value(Param1, Param2, Param3, Param4);
end;

procedure TCallbackListP4<T1, T2, T3, T4>.Remove(Id: Pointer);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Delete(FList, i, 1);
end;

procedure TCallbackListP4<T1, T2, T3, T4>.Clear();
begin
  FList := nil;
end;

function TCallbackListP4<T1, T2, T3, T4>.HasListener(Id: Pointer): Boolean;
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Exit(True);
  Result := False;
end;

{ TCallbackListP5<T1, T2, T3, T4, T5> }

procedure TCallbackListP5<T1, T2, T3, T4, T5>.Add(Method: TCallback; Id: Pointer = nil);
begin
  FList := FList + [TPair<Pointer, TCallback>.Create(Id, TCallback(Method))];
end;

procedure TCallbackListP5<T1, T2, T3, T4, T5>.Call(Param1: T1; Param2: T2; Param3: T3; Param4: T4; Param5: T5);
var
  i: Integer;
  TempList: TCallbackPairArray;
begin
  // Copy list so Adding/Removing new callbacks inside callbacks won't break this loop.
  TempList := Copy(FList);
  for i:=0 to Length(TempList)-1 do
    TempList[i].Value(Param1, Param2, Param3, Param4, Param5);
end;

procedure TCallbackListP5<T1, T2, T3, T4, T5>.Remove(Id: Pointer);
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Delete(FList, i, 1);
end;

procedure TCallbackListP5<T1, T2, T3, T4, T5>.Clear();
begin
  FList := nil;
end;

function TCallbackListP5<T1, T2, T3, T4, T5>.HasListener(Id: Pointer): Boolean;
var
  i: Integer;
begin
  for i:=Length(FList)-1 downto 0 do
    if FList[i].Key = Id then
      Exit(True);
  Result := False;
end;

end.
