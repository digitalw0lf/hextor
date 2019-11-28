unit uCoDWHex;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AxCtrls, Classes,
DWHex_TLB, StdVcl;

type
  TCoEditorForm = class(TAutoObject, IEditorForm)
  public
    function GetEditedData(Addr: Int64; Size: Int64; ZerosBeyondEoF: WordBool): OleVariant; stdcall;
    function GetFileSize: Int64; stdcall;
  end;

  TCoDWHex = class(TAutoObject, IConnectionPointContainer, ICoDWHex)
  private
    { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FEvents: ICoDWHexEvents;
    { note: FEvents maintains a *single* event sink. For access to more
      than one event sink, use FConnectionPoint.SinkList, and iterate
      through the list of sinks. }
    AActiveEditor: TCoEditorForm;
  public
    procedure Initialize; override;
  protected
    function Get_ActiveEditor: OleVariant; safecall;
    function GetEditorCount: SYSINT; stdcall;
    function Get_EditorCount: SYSINT; safecall;
    { Protected declarations }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; override; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; override; stdcall;
  end;

implementation

uses
  ComServ, uMainForm;

procedure TCoDWHex.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ICoDWHexEvents;
end;

procedure TCoDWHex.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;

  AActiveEditor := TCoEditorForm.Create();
end;


function TCoDWHex.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := inherited;
end;

function TCoDWHex.Get_ActiveEditor: OleVariant;
begin
  Result := (MainForm.ActiveEditor as IEditorForm);
//  Result := AActiveEditor as IEditorForm;
end;

function TCoDWHex.GetEditorCount: SYSINT;
begin
  Result := MainForm.EditorCount;
end;

function TCoDWHex.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := inherited;
end;

function TCoDWHex.Get_EditorCount: SYSINT;
begin
  Result := MainForm.EditorCount;
end;

{ TCoEditorForm }

function TCoEditorForm.GetEditedData(Addr, Size: Int64;
  ZerosBeyondEoF: WordBool): OleVariant;
begin
  Result := 0;
end;

function TCoEditorForm.GetFileSize: Int64;
begin
  Result := 1235;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCoDWHex, Class_CoDWHex,
    ciMultiInstance, tmSingle);
  TAutoObjectFactory.Create(ComServer, TCoEditorForm, CLASS_CoEditorForm,
    ciMultiInstance, tmSingle);
end.
