unit uDiskSelectForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,

  uHextorTypes;

type
  TDiskSelectForm = class(TForm)
    ListView1: TListView;
    BtnOpen: TButton;
    BtnCancel: TButton;
    ImageList1: TImageList;
    procedure FormShow(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowDrivesList();
    function SelectedDrive(): string;
  end;

var
  DiskSelectForm: TDiskSelectForm;

implementation

{$R *.dfm}

{ TDiskSelectForm }

procedure TDiskSelectForm.FormShow(Sender: TObject);
begin
  ShowDrivesList();
end;

procedure TDiskSelectForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  BtnOpen.Enabled := (ListView1.Selected <> nil);
end;

procedure TDiskSelectForm.ListView1DblClick(Sender: TObject);
begin
  if BtnOpen.Enabled then
    BtnOpen.Click;
end;

function TDiskSelectForm.SelectedDrive: string;
begin
  if ListView1.Selected = nil then
    Result := ''
  else
    Result := ListView1.Selected.Caption;
end;

procedure TDiskSelectForm.ShowDrivesList;
var
  s, s1, lbl: string;
  Letters: TArray<string>;
  n, i: integer;
  FreeAvailable, TotalSpace, TotalFree: Int64;
  m, f, err: Cardinal;
  li: TListItem;

  Name: Char;
  _type: Integer;
  Size: Int64;
  ALabel: string;
begin
  ListView1.Clear();

//  When a user attempts to get information about a floppy drive that does not
//  have a floppy disk, or a CD-ROM drive that does not have a compact disc, the
//  system displays a message box for the user to insert a floppy disk or a compact
//  disc, respectively. To prevent the system from displaying this message box,
//  call the SetErrorMode function with SEM_FAILCRITICALERRORS.
  err:=SetErrorMode(SEM_FAILCRITICALERRORS);

  try
    SetLength(s,1000);
    i:=GetLogicalDriveStrings(Length(s),@s[1]);
    SetLength(s,i);
    Letters := s.Split([#0]);
    for n:=0 to Length(Letters)-1 do
    begin
      s1:=Letters[n];

      Name := s1[1];

      if CharInSet(s1[1], ['A','B']) then  // Prevent lag
      begin
        _type:=DRIVE_UNKNOWN;
        Size:=0;
        ALabel:='';
      end
      else
      begin
        i:=GetDriveType(PChar(s1));
        _type:=i;

        if GetDiskFreeSpaceEx(PChar(s1),FreeAvailable,TotalSpace,@TotalFree) then
          Size:=TotalSpace
        else
          Size:=-1;

        SetLength(lbl,100);
        FillChar(lbl[1],Length(lbl),0);
        GetVolumeInformation(PChar(s1),@lbl[1],Length(lbl),nil,m,f,nil,0);
        ALabel:=PChar(@lbl[1]);
      end;

      li := ListView1.Items.Add;

      li.Caption:=Name+':';
      li.ImageIndex:=_type;
      li.SubItems.Add(ALabel);
      if Size>=0 then
        li.SubItems.Add(FileSize2Str(Size))
      else
        li.SubItems.Add('?');
    end;

  finally
    SetErrorMode(err);
  end;
end;

end.
