{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2021  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uCopyAsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Clipbrd,
  Generics.Collections, System.IOUtils, System.Types, Winapi.ShellAPI,
  System.StrUtils, Vcl.Buttons, Vcl.Menus,

  uHextorTypes, superobject, uModuleSettings, uHextorGUI;

type
  TCopyAsForm = class(TForm)
    Label1: TLabel;
    CBElemType: TComboBox;
    Label2: TLabel;
    CBLayout: TComboBox;
    BtnCopy: TButton;
    BtnCancel: TButton;
    Label3: TLabel;
    CBNotation: TComboBox;
    Label4: TLabel;
    CBValuesPerLine: TComboBox;
    GroupBox1: TGroupBox;
    LblPreview: TLabel;
    BtnEditLayouts: TSpeedButton;
    OpenLayoutFolderMenu: TPopupMenu;
    MIBuiltInLayoutsFolder: TMenuItem;
    MIUserLayoutsFolder: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnEditLayoutsClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure CBElemTypeChange(Sender: TObject);
    procedure MIUserLayoutsFolderClick(Sender: TObject);
  public type
    TNotation = TValueDisplayNotation;
    TTwoStrings = array[0..1] of string;
    // Description of export layout (like "C array" or "Comma-separated")
    TExportLayout = class
      Name: string;
      Header,
      Separator,
      LineSeparator,
      Footer: string;
      TypeNames: array of TTwoStrings;  // Type name mapping in form ['int8', 'this_format_type_name']
      NotationPrefix, NotationSuffix: array[nnBin..nnWChar] of string;  // Number system prefix/suffix for (bin, oct, dec, hex)
      function ValueToStr(const V: Variant; Notation: TNotation): string; virtual;
      function TranslateType(const TypeName: string): string;
    end;
    // Built-in string escape languages
    TEscapeLanguage = (elNone, elCpp, elDelphi);
  private
    { Private declarations }
    BuiltinLayoutsFolder, UserLayoutsFolder: string;
    Layouts: TObjectList<TExportLayout>;
    procedure LoadLayoutsFoder(const Path: string);
    procedure LoadLayouts();
    function GetLayout(Name: string; var Layout: TExportLayout): Boolean;
    procedure ShowPreview();
  public
    { Public declarations }
    Data: TBytes;
    function DataToText(const AData: TBytes; ElemType: string; Notation: TNotation; LayoutName: string; ValuesPerLine: Integer = -1; MaxElements: Integer = -1): string;

    class function DataToEsapedString(const AData: TBytes; Language: TEscapeLanguage; CodePage: Integer): string;
  end;

var
  CopyAsForm: TCopyAsForm;

implementation

uses
  uMainForm, uValueInterpretors;

{$R *.dfm}

procedure TCopyAsForm.BtnCopyClick(Sender: TObject);
var
  Text: string;
begin
  Text := DataToText(Data, CBElemType.Text, TNotation(CBNotation.ItemIndex+1), CBLayout.Text, StrToIntDef(CBValuesPerLine.Text, -1));
  Clipboard.AsText := StrToClipboard(Text);
  ModalResult := mrOk;
end;

procedure TCopyAsForm.BtnEditLayoutsClick(Sender: TObject);
begin
  PopupFromControl(nil, Sender as TControl);
end;

procedure TCopyAsForm.CBElemTypeChange(Sender: TObject);
begin
  ShowPreview();
end;

class function TCopyAsForm.DataToEsapedString(const AData: TBytes;
  Language: TEscapeLanguage; CodePage: Integer): string;
const
  StrQuotes: array[TEscapeLanguage] of Char = (#0, '"', '''');
var
  sb: TStringBuilder;
  i: Integer;
  b: Byte;
  s: string;
begin
  sb := TStringBuilder.Create(Length(AData));
  try
    sb.Append(StrQuotes[Language]);
    for i := 0 to Length(AData)-1 do
    begin
      b := AData[i];

      case Language of
        elCpp:
          begin
            case b of
              9: s := '\t';
              10: s := '\n';
              13: s := '\r';
              Ord(''''): s := '\''';
              Ord('"'): s := '\"';
              Ord('?'): s := '\?';
              Ord('\'): s := '\\';
              else
                begin
                  if b < 32 then
                    s := '\x' + IntToHex(b, 2)
                  else
                    s := Data2String([b], CodePage);
                end;
            end;
          end;
        elDelphi:
          begin
            if b = Ord('''') then
              s := ''''''  // Two single qwuotes
            else
            if b < 32 then
              s := '''#$' + IntToHex(b, 2) + ''''
            else
              s := Data2String([b], CodePage);
          end;
      end;

      sb.Append(s);
    end;
    sb.Append(StrQuotes[Language]);

    Result := sb.ToString();
  finally
    sb.Free;
  end;
end;

function TCopyAsForm.DataToText(const AData: TBytes; ElemType: string;
  Notation: TNotation; LayoutName: string; ValuesPerLine,
  MaxElements: Integer): string;
// Convert data to text using configured layout
var
  sb: TStringBuilder;
  i, Line, ElemSize, Count: Integer;
  {SCount,} SType{, SLine}: string;
  Layout: TExportLayout;
  Interp: TValueInterpretor;
  V: Variant;

  function ExpandMacroses(const Text: string): string;
  // Substitute %count%, %type%
  begin
    Result := Text;
    if not Result.Contains('%') then Exit;
    Result := Result.Replace('%count%', IntToStr(Count));
    Result := Result.Replace('%count-1%', IntToStr(Count-1));
    Result := Result.Replace('%type%', SType);
    Result := Result.Replace('%line%', IntToStr(Line+1));
  end;

begin
  if not GetLayout(LayoutName, Layout) then
    raise Exception.Create('Layout not found');
  if Layout = nil then
    raise Exception.Create('Invalid layout description');
  Interp := ValueInterpretors.FindInterpretor(ElemType);
  if Interp = nil then
    raise Exception.Create('Invalid type name: ' + ElemType);

  sb := TStringBuilder.Create();
  Progress.TaskStart(Self);
  try
    ElemSize := Interp.MinSize;
    Count := Length(AData) div ElemSize;
    Line := 0;
    // For macros substitution
//    SCount := IntToStr(Count);
    SType := Layout.TranslateType(ElemType);
//    SLine := IntToStr(Line+1);

    // Header
    sb.Append(ExpandMacroses(Layout.Header));

    for i:=0 to Count-1 do
    begin
      // Element
      V := Interp.ToVariant(AData[i * ElemSize], ElemSize);
      sb.Append(Layout.ValueToStr(V, Notation));

      // Limit elements count for preview
      if (MaxElements > 0) and (i+1 >= MaxElements) then Break;

      if i < Count-1 then
      begin
        if (ValuesPerLine > 0) and ((i+1) mod ValuesPerLine = 0) then
        // New line
        begin
          Inc(Line);
//          SLine := IntToStr(Line+1);
          sb.Append(ExpandMacroses(Layout.LineSeparator));
        end
        else
          sb.Append(ExpandMacroses(Layout.Separator));
      end;

      if (i mod 10000) = 0 then
        Progress.Show(i+1, Count);
    end;
    // Footer
    sb.Append(ExpandMacroses(Layout.Footer));

    Result := sb.ToString();
  finally
    sb.Free;
    Progress.TaskEnd();
  end;
end;

procedure TCopyAsForm.FormCreate(Sender: TObject);
begin
  BuiltInLayoutsFolder := TPath.Combine(TModuleSettings.BuiltInSettingsFolder, 'ExportLayouts');
  UserLayoutsFolder := TPath.Combine(TModuleSettings.SettingsFolder, 'ExportLayouts');
  Layouts := TObjectList<TExportLayout>.Create(True);
end;

procedure TCopyAsForm.FormDestroy(Sender: TObject);
begin
  Layouts.Free();
end;

procedure TCopyAsForm.FormShow(Sender: TObject);
begin
  LoadLayouts();
  ShowPreview();
end;

function TCopyAsForm.GetLayout(Name: string;
  var Layout: TExportLayout): Boolean;
var
  i: Integer;
begin
  for i:=0 to Layouts.Count-1 do
    if (Layouts[i] <> nil) and (Layouts[i].Name = Name) then
    begin
      Layout := Layouts[i];
      Exit(True);
    end;
  Result := False;
end;

procedure TCopyAsForm.LoadLayouts();
var
  i: Integer;
  sel, name: string;
begin
  Layouts.Clear();
  LoadLayoutsFoder(UserLayoutsFolder);
  LoadLayoutsFoder(BuiltinLayoutsFolder);

  // Show layouts list
  sel := CBLayout.Text;
  CBLayout.Items.Clear();
  for i:=0 to Layouts.Count-1 do
  begin
    if Layouts[i] <> nil then
      name := Layouts[i].Name
    else
      name := '<error in description>';  // Loading Layout from json failed
    CBLayout.Items.Add(name);
  end;
  CBLayout.ItemIndex := CBLayout.Items.IndexOf(sel);
  if (CBLayout.ItemIndex < 0) and (CBLayout.Items.Count > 0) then
    CBLayout.ItemIndex := 0;
end;

procedure TCopyAsForm.LoadLayoutsFoder(const Path: string);
var
  fl: TStringDynArray;
  i: Integer;
  Layout: TExportLayout;
begin
  // Load layout descriptions from settings folder
  if not System.SysUtils.DirectoryExists(Path) then Exit;
  fl := TDirectory.GetFiles(Path, '*.json');
  for i:=0 to Length(fl)-1 do
  begin
    Layout := tJsonRtti.StrToObject<TExportLayout>(TFile.ReadAllText(fl[i]));
    Layouts.Add(Layout);
  end;
end;

procedure TCopyAsForm.MIUserLayoutsFolderClick(Sender: TObject);
var
  Path: string;
begin
  if Sender = MIUserLayoutsFolder then
    Path := UserLayoutsFolder
  else
    Path := BuiltInLayoutsFolder;
  ShellExecute(0, '', PChar(Path), '', '', SW_SHOWNORMAL);
end;

procedure TCopyAsForm.ShowPreview;
var
  Text: string;
begin
  try
    Text := DataToText(Data, CBElemType.Text, TNotation(CBNotation.ItemIndex+1), CBLayout.Text, StrToIntDef(CBValuesPerLine.Text, -1), 100);
    LblPreview.Caption := Text;
  except
    on E: Exception do
      LblPreview.Caption := E.Message;
  end;
end;

{ TCopyAsForm.TExportLayout }

function TCopyAsForm.TExportLayout.TranslateType(
  const TypeName: string): string;
// Translate standard type name to layout-specific type name
// (e.g. "uint8" -> "char");
var
  i: Integer;
begin
  for i:=0 to Length(TypeNames)-1 do
    if TypeNames[i][0] = TypeName then
      Exit(TypeNames[i][1]);
  Result := TypeName;
end;

function TCopyAsForm.TExportLayout.ValueToStr(const V: Variant; Notation: TNotation): string;
// Single value to string (including prefix) according to this layout
var
  x: Int64;
  Size: Integer;
begin
  Result := '';
  if VarIsOrdinal(V) then
  begin
    case VarType(V) of
      varShortInt, varByte:  Size := 1;
      varSmallint, varWord:  Size := 2;
      varInteger, varUInt32: Size := 4;
      varInt64, varUInt64:   Size := 8;
      else Exit('');
    end;
    x := V;
    if (Notation <> nnDec) and (Size < 8) then
      x := x and ((Int64(1) shl (Size*8)) - 1); // Negative bin/oct/hex not supported
    case Notation of
      nnBin: Result := IntToBin(x, Size * 8);
      nnOct: Result := IntToOct(x);
      nnDec: Result := IntToStr(x);
      nnHex: Result := IntToHex(x, Size * 2);
      nnAChar: Result := Char(AnsiChar(x));
      nnWChar: Result := Char(x);
    end;
  end
  else
    Result := V;
  Result := NotationPrefix[Notation] + Result + NotationSuffix[Notation];
end;

end.
