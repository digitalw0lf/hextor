library PluginTest;

uses
  FastMM4,
  System.SysUtils,
  System.Classes,
  System.Win.ComObj;

type
  {.$DEFINE HeadersForCodeInsight}

  {$IFDEF HeadersForCodeInsight}

  IDWHexApp = {dispinterface //}interface(IDispatch)
//  ['{1D3B54F1-D455-4F20-B961-39675EEF0F32}']
//    procedure AA(x: Integer);
    procedure DoTest(x: Integer); dispid 2;
  end;

  {$ELSE}
  IDWHexApp = OleVariant;
  {$ENDIF}

{$R *.res}

procedure init(App: {OleVariant} IDWHexApp); stdcall;
begin
  //App.ActiveEditor.Data.Insert(0, 103);
  App.DoTest(345);
end;

//type
//  TInsertProc = procedure(Addr: Int64; Value: Byte) of Object;
//
//procedure init(InsProc: TInsertProc); stdcall;
//begin
//  InsProc(0, 124);
//end;


exports
  init;

begin
end.
