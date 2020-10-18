{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uModuleSettings;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Rtti, superobject,

  uHextorTypes;

type
  // Settings for single module. Stored in separate json file.
  TModuleSettings = class
  protected
    FChanged: Boolean;
    procedure Load();
    procedure Save();
    function GetFileName(): string;
  public
    class var SettingsFolder: string;         // Folder for user settings, templates etc.
    class var BuiltInSettingsFolder: string;  // Folder with built-in templates etc.
    constructor Create();
    destructor Destroy(); override;
    procedure Changed(SaveNow: Boolean = False);
    procedure AfterConstruction(); override;
    procedure InitDefault(); virtual;
  end;

implementation

{ TModuleSettings }

procedure TModuleSettings.AfterConstruction;
begin
  inherited;
  Load();
end;

procedure TModuleSettings.Changed(SaveNow: Boolean);
begin
  FChanged := True;
  if SaveNow then
    Save();
end;

constructor TModuleSettings.Create;
begin
  inherited;
  InitDefault();
end;

destructor TModuleSettings.Destroy;
begin
  if FChanged then
    Save();
  inherited;
end;

function TModuleSettings.GetFileName: string;
// <Settings_Folder>\<Module_Name>.json
var
  Name: string;
begin
  Name := Self.ClassName;
  if Name.StartsWith('T') then
    Delete(Name, Low(Name), 1);
  if Name.EndsWith('Settings') then
    Delete(Name, High(Name) - Length('Settings') + 1, MaxInt);
  Result := TPath.Combine(SettingsFolder, Name + '.json');
end;

procedure TModuleSettings.InitDefault;
begin
  // Descendants should initialize their fields to default values in this method
end;

procedure TModuleSettings.Load;
var
  FN: string;
  ctx: TSuperRttiContext;
  json: ISuperObject;
  Value: TValue;
begin
  FN := GetFileName();
  if not FileExists(FN) then Exit;
  json := TSuperObject.ParseFile(FN, False);

  ctx := TSuperRttiContext.Create;
  try
    Value := TValue.From<TModuleSettings>(Self);
    ctx.FromJson(Self.ClassInfo, json, Value);
  finally
    ctx.Free;
  end;

  FChanged := False;
end;

procedure TModuleSettings.Save;
const
  BOM: array[0..1] of Byte = ($FF, $FE);
var
  ctx: TSuperRttiContext;
  fs: TFileStream;
begin
  System.SysUtils.ForceDirectories(SettingsFolder);
  fs := TFileStream.Create(GetFileName(), fmCreate);
  ctx := TSuperRttiContext.Create;
  try
    fs.WriteBuffer(BOM, SizeOf(BOM));

    ctx.AsJson<TModuleSettings>(Self).SaveTo(fs, True, False);
  finally
    fs.Free;
    ctx.Free;
  end;
  FChanged := False;
end;

end.
