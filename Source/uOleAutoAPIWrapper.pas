unit uOleAutoAPIWrapper;

interface

uses
  Winapi.Windows, System.SysUtils, System.Rtti, Generics.Collections,
  System.TypInfo, Winapi.ActiveX, System.Variants{,

  uUtil, uDebugUtils, uLogFile};

type
  APIAttribute = class(TCustomAttribute)

  end;

  TValuesArray = array of TValue;
  TAPIEnvironment = class;
  TAPITypeWrapper = class;

  IAPIWrapper = interface
    ['{1D3B54F1-D455-4F20-B961-39675EEF0F32}']
    function GetWrappedObject(): TObject;
  end;

  // Custom functions to convert to/from ole automation variants
  TValueToVariantProc = procedure(const AIn: TValue; var AOut: OleVariant; var Handled: Boolean) of Object;
  TVariantToValueProc = procedure(const AIn: Variant; var AOut: TValue; var Handled: Boolean) of Object;

  // Dispatch wrapper for object
  TAPIWrapper = class (TInterfacedObject, IAPIWrapper, IDispatch)
  private
    Environment: TAPIEnvironment;
    TypeWrapper: TAPITypeWrapper;
    function ValueToVariant(const X: TValue): OleVariant;
    function VariantToValue(const X: Variant): TValue;
    procedure ArgsToValues(var Params: TDispParams; var Values: TValuesArray);
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    WrappedObject: TObject;
    function GetWrappedObject(): TObject;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT;
      virtual; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; virtual; stdcall;
    constructor Create(AObject: TObject; AEnv: TAPIEnvironment);
    destructor Destroy(); override;
  end;

  // Type information for wrapper
  TAPITypeWrapper = class
    // List of class members. DispID = index in this list
    AMembers: TList<TRttiMember>;
    constructor Create();
    destructor Destroy(); override;
  end;

  TAPIEnvironment = class
  private
    RttiContext: TRttiContext;
    TypeWrappers: TObjectDictionary<TClass, TAPITypeWrapper>;
    Wrappers: TObjectDictionary<{TObject}Pointer, TAPIWrapper>;
    function GetTypeWrapper(AInstance: TObject): TAPITypeWrapper;
  public
    MemberVisibility: (amvAttributed, amvPublished, amvPublic);
    // Custom functions to convert to/from ole automation variants
    ValueToVariantProc: TValueToVariantProc;
    VariantToValueProc: TVariantToValueProc;
    function GetAPIWrapper(AObject: TObject): IDispatch;
    procedure ObjectDestroyed(AObject: TObject);
    constructor Create();
    destructor Destroy(); override;
  end;


implementation

function GetAPIAttribute(AEntity: TRttiObject): APIAttribute;
var
  LAttr: TCustomAttribute;
begin
  for LAttr in AEntity.GetAttributes() do
    if LAttr is APIAttribute then
      Exit(APIAttribute(LAttr));
  Result := nil;
end;

{ TAPIWrapper }

procedure TAPIWrapper.ArgsToValues(var Params: TDispParams; var Values: TValuesArray);
var
  i: Integer;
begin
  SetLength(Values, Params.cArgs);
  for i:=0 to Params.cArgs-1 do
    Values[Params.cArgs-1-i] := VariantToValue(Variant(Params.rgvarg[i]));
end;

constructor TAPIWrapper.Create(AObject: TObject; AEnv: TAPIEnvironment);
begin
  inherited Create({Intf});
  WrappedObject := AObject;
  Environment := AEnv;
  TypeWrapper := Environment.GetTypeWrapper(AObject);

  Environment.Wrappers.AddOrSetValue(WrappedObject, Self);
  _AddRef();
end;

destructor TAPIWrapper.Destroy;
begin
//  WriteLogF('Destroy: ' + ThreadStackStr());
  if Environment <> nil then
    Environment.Wrappers.Remove(WrappedObject);
  inherited;
end;

function TAPIWrapper.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HRESULT;
type
  PNames = ^TNames;
  TNames = array[0..100] of POleStr;
  PDispIDs = ^TDispIDs;
  TDispIDs = array[0..100] of Cardinal;
var
  i: Integer;
  AName: string;
begin
  AName := PNames(Names)^[0];
  for i:=0 to TypeWrapper.AMembers.Count-1 do
    if SameText(TypeWrapper.AMembers[i].Name, AName) then
    begin
      PCardinal(DispIDs)^ := i;
      Result := S_OK;
      Exit;
    end;
  Result := DISP_E_UNKNOWNNAME;
end;

function TAPIWrapper.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAPIWrapper.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAPIWrapper.GetWrappedObject: TObject;
begin
  Result := WrappedObject;
end;

function TAPIWrapper.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
var
  LMember: TRttiMember;
  Values: TValuesArray;
  AResult: TValue;
  Put: Boolean;
  WS: WideString;
begin
  Result := S_OK;
  try
    LMember := TypeWrapper.AMembers[DispID];

    // Convert arguments
    ArgsToValues(TDispParams(Params), Values);
    Put := ((Flags and (DISPATCH_PROPERTYPUT or DISPATCH_PROPERTYPUTREF)) <> 0);
    AResult := TValue.Empty;

    // Call method
    if LMember is TRttiMethod then
    begin
      AResult := (LMember as TRttiMethod).Invoke(WrappedObject, Values);
    end
    else
    // Access field
    if LMember is TRttiField then
    begin
      if Put then
        (LMember as TRttiField).SetValue(WrappedObject, Values[0])
      else
        AResult := (LMember as TRttiField).GetValue(WrappedObject);
    end
    else
    // Access property
    if LMember is TRttiProperty then
    begin
      if Put then
        (LMember as TRttiProperty).SetValue(WrappedObject, Values[0])
      else
        AResult := (LMember as TRttiProperty).GetValue(WrappedObject);
    end
    else
    // Access indexed property
    if LMember is TRttiIndexedProperty then
    begin
      if Put then
        (LMember as TRttiIndexedProperty).SetValue(WrappedObject, Copy(Values, 0, Length(Values)-1), Values[High(Values)])
      else
        AResult := (LMember as TRttiIndexedProperty).GetValue(WrappedObject, Values);
    end;

    // Convert result
    if VarResult <> nil then
    begin
      POleVariant(VarResult)^ := ValueToVariant(AResult);
    end;
  except
//    on E: EInvalidParamCount do Result := DISP_E_BADPARAMCOUNT;
//    on E: EInvalidParamType do  Result := DISP_E_BADVARTYPE;
    on E: Exception do
    begin
      if Assigned(ExcepInfo) then
      begin
        FillChar(ExcepInfo^, SizeOf(TExcepInfo), 0);
        TExcepInfo(ExcepInfo^).wCode := 1001;
        TExcepInfo(ExcepInfo^).BStrSource := SysAllocString('DWHex');
        WS := E.Message;
        TExcepInfo(ExcepInfo^).bstrDescription := SysAllocString(PWideChar(WS));
      end;
      Result := DISP_E_EXCEPTION;
    end;
  end;
end;

function TAPIWrapper.ValueToVariant(const X: TValue): OleVariant;
//var
//  Len, i: Integer;
//  arr: TBytes;
//  ti: PTypeInfo;
var
  Handled: Boolean;
begin
  if Assigned(Environment.ValueToVariantProc) then
  begin
    Handled := False;
    Environment.ValueToVariantProc(X, Result, Handled);
    if Handled then Exit;
  end;

  if (not X.IsEmpty) and (X.IsObject) then
  begin
    // If object is returned, convert it to wrapper
    Result := Environment.GetAPIWrapper(X.AsObject) as IDispatch;
  end
//  else
//  if X.IsArray then
//  begin
//    ti := X.TypeInfo;
//    DynArrayToVariant(Result, X.GetReferenceToRawArrayElement(0), ti);
//  end
  else
    Result := X.AsVariant;
end;

function TAPIWrapper.VariantToValue(const X: Variant): TValue;
var
  AWrapper: IAPIWrapper;
  Handled: Boolean;
begin
  if Assigned(Environment.VariantToValueProc) then
  begin
    Handled := False;
    Environment.VariantToValueProc(X, Result, Handled);
    if Handled then Exit;
  end;

  Result := TValue.FromVariant(X);

  // Convert wrappers to objects
  if (Result.Kind = tkInterface) then
    if (Supports(Result.AsInterface, IAPIWrapper, AWrapper)) then
    begin
      Result := AWrapper.GetWrappedObject();
    end;
end;

function TAPIWrapper._AddRef: Integer;
begin
//  WriteLogF('_AddRef: ' + IntToStr(FRefCount + 1) + ' ' + ThreadStackStr());
  Result := inherited _AddRef();
end;

function TAPIWrapper._Release: Integer;
begin
//  WriteLogF('_Release: ' + IntToStr(FRefCount - 1) + ' ' + ThreadStackStr());
  Result := inherited _Release();
end;

{ TAPIEnvironment }

constructor TAPIEnvironment.Create;
begin
  inherited;
  RttiContext := TRttiContext.Create;
  TypeWrappers := TObjectDictionary<TClass, TAPITypeWrapper>.Create([doOwnsValues]);
  Wrappers := TObjectDictionary<{TObject}Pointer, TAPIWrapper>.Create([{doOwnsValues}]);
end;

destructor TAPIEnvironment.Destroy;
var
  Wrapper: TAPIWrapper;
begin
  // We do not destroy wrappers here, because they are ref-counted by COM.
  // But we inform them that their Environment is gone
  for Wrapper in Wrappers.Values do
    Wrapper.Environment := nil;
  Wrappers.Free;
  TypeWrappers.Free;
  RttiContext.Free;
  inherited;
end;

function TAPIEnvironment.GetAPIWrapper(AObject: TObject): IDispatch;
var
  Wrapper: TAPIWrapper;
begin
  if AObject = nil then Exit(nil);
  if Wrappers.TryGetValue(AObject, Wrapper) then Exit(Wrapper);
  Result := TAPIWrapper.Create(AObject, Self);
end;

function TAPIEnvironment.GetTypeWrapper(AInstance: TObject): TAPITypeWrapper;
var
  LType: TRttiType;
  ClassAPIAttr: APIAttribute;
  LMember: TRttiMember;

  procedure ProcessMember(AMember: TRttiMember);
  var
    APIAttr: APIAttribute;
    Ok: Boolean;
  begin
    APIAttr := GetAPIAttribute(AMember);
    case MemberVisibility of
      amvAttributed:
        Ok := (APIAttr <> nil) or ((ClassAPIAttr <> nil) and (AMember.Visibility >= mvPublic));
      amvPublished:
        Ok := (AMember.Visibility >= mvPublished);
      amvPublic:
        Ok := (AMember.Visibility >= mvPublic);
      else
        Exit;
    end;
    if Ok then
    begin
      Result.AMembers.Add(AMember);
    end;
  end;

begin
  if AInstance = nil then Exit(nil);
  if TypeWrappers.TryGetValue(AInstance.ClassType, Result) then Exit;

  Result := TAPITypeWrapper.Create();
  TypeWrappers.AddOrSetValue(AInstance.ClassType, Result);

  { Extract type information for TSomeType type }
  LType := RttiContext.GetType(AInstance.ClassType);

  // Entire object annotated with [API]
  ClassAPIAttr := GetAPIAttribute(LType);

  for LMember in LType.GetMethods do ProcessMember(LMember);
  for LMember in LType.GetFields do ProcessMember(LMember);
  for LMember in LType.GetProperties do ProcessMember(LMember);
  for LMember in LType.GetIndexedProperties do ProcessMember(LMember);
end;

procedure TAPIEnvironment.ObjectDestroyed(AObject: TObject);
var
  Wrapper: TAPIWrapper;
begin
  if Wrappers.TryGetValue(AObject, Wrapper) then
    Wrapper._Release;
//  Wrappers.Remove(AObject);
end;

{ TAPITypeWrapper }

constructor TAPITypeWrapper.Create;
begin
  inherited;
  AMembers := TList<TRttiMember>.Create();
end;

destructor TAPITypeWrapper.Destroy;
begin
  AMembers.Free;
  inherited;
end;

end.
