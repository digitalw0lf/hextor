unit uMediaFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.MPlayer, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Imaging.GIFImg,
  Vcl.StdCtrls, Vcl.Menus, System.IOUtils, Vcl.Clipbrd,

  uHextorTypes, uEditorForm;

type
  TMediaFrame = class(TFrame, IHextorToolFrame)
    ToolPanel: TPanel;
    ContentPanel: TPanel;
    Image1: TImage;
    MediaPlayerPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    MediaPlayer1: TMediaPlayer;
    BtnShow: TSpeedButton;
    ErrorMemo: TMemo;
    LblDetectedType: TLabel;
    PopupMenu1: TPopupMenu;
    MICopyImage: TMenuItem;
    MIDumpData: TMenuItem;
    FileSaveDialog1: TFileSaveDialog;
    procedure BtnShowClick(Sender: TObject);
    procedure MICopyImageClick(Sender: TObject);
    procedure MIDumpDataClick(Sender: TObject);
  private
    { Private declarations }
    Buf: TBytes;
    procedure EditorSelectionChanged(Sender: TEditorForm);
    procedure ShowContentControl(AControl: TControl; const DetectedType: string);
    procedure GetFormatsList(List: TStrings);
  public
    { Public declarations }
    procedure OnShown();
    procedure Init();
    procedure Uninit();
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure ShowMedia(const ABuf: TBytes);
  end;

implementation

{$R *.dfm}

uses uMainForm;

{ TFrame1 }

procedure TMediaFrame.BtnShowClick(Sender: TObject);
var
  AEditor: TEditorForm;
  Buf: TBytes;
begin
  AEditor := MainForm.ActiveEditor;
  if AEditor.SelLength = 0 then
    Buf := AEditor.GetEditedData(0, AEditor.GetFileSize())
  else
    Buf := AEditor.GetEditedData(AEditor.SelStart, AEditor.SelLength);
  ShowMedia(Buf);
end;

procedure TMediaFrame.MICopyImageClick(Sender: TObject);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create();
  try
    Bmp.Assign(Image1.Picture.Graphic);
    Clipboard.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TMediaFrame.MIDumpDataClick(Sender: TObject);
begin
  if Buf = nil then Exit;
  if not FileSaveDialog1.Execute then Exit;
  tfile.WriteAllBytes(FileSaveDialog1.FileName, Buf);
end;

constructor TMediaFrame.Create(AOwner: TComponent);
var
  sl: TStringList;
begin
  inherited;
  MainForm.OnSelectionChanged.Add(EditorSelectionChanged);
  ErrorMemo.Text := 'Select data in editor and press "Show" to display it as image/multimedia based on signature.' + sLineBreak + sLineBreak +
                    'Currently supported formats:' + sLineBreak + sLineBreak;
  sl := TStringList.Create();
  try
    GetFormatsList(sl);
    ErrorMemo.Lines.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

destructor TMediaFrame.Destroy;
begin

  inherited;
end;

procedure TMediaFrame.EditorSelectionChanged(Sender: TEditorForm);
begin

end;

procedure TMediaFrame.GetFormatsList(List: TStrings);
var
  i: Integer;
begin
  List.Delimiter := ';';
  List.StrictDelimiter := True;
  List.DelimitedText := GraphicFileMask(TGraphic);
  for i := 0 to List.Count - 1 do
    List[i] := List[i].Replace('*.', '');
end;

procedure TMediaFrame.Init;
begin

end;

procedure TMediaFrame.OnShown;
begin
  if true then ;
end;

procedure TMediaFrame.ShowContentControl(AControl: TControl; const DetectedType: string);
var
  i: Integer;
begin
  for i := 0 to ContentPanel.ControlCount - 1 do
    ContentPanel.Controls[i].Visible := (ContentPanel.Controls[i] = AControl);
  LblDetectedType.Caption := 'Detected type: ' + DetectedType;
end;

procedure TMediaFrame.ShowMedia(const ABuf: TBytes);
var
  st: TBytesStream;
begin
  Buf := ABuf;
  try
    if Buf = nil then
      raise Exception.Create('Empty selection');
    st := TBytesStream.Create(Buf);
    try
      st.Position := 0;
      Image1.Picture.LoadFromStream(st);
      ShowContentControl(Image1, Image1.Picture.Graphic.ClassName);
    finally
      st.Free
    end;
  except
    on E: Exception do
    begin
      ShowContentControl(ErrorMemo, '?');
      ErrorMemo.Text := E.Message;
    end;
  end;
end;

procedure TMediaFrame.Uninit;
begin

end;

end.
