{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDiskSelectForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,
  System.Win.Bluetooth, Vcl.ExtCtrls,

  uHextorTypes, uHextorDataSources;

type
  TDiskSelectForm = class(TForm)
    BtnOpen: TButton;
    BtnCancel: TButton;
    ImageList1: TImageList;
    DiskListView: TListView;
    RefreshTimer: TTimer;
    BusyPanel: TPanel;
    procedure FormShow(Sender: TObject);
    procedure DiskListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DiskListViewDblClick(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure ListPhysicalDrives(const diskClassGUID: TGUID; ListItemGroup: Integer);
//    procedure ListDriveLetters();
    procedure FindVolumes();
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

type
  DISK_EXTENT = record
    DiskNumber: DWORD;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;

  VOLUME_DISK_EXTENTS = record
    NumberOfDiskExtents: DWORD;
    Extents: array[0..31] of DISK_EXTENT;
  end;

procedure TDiskSelectForm.FindVolumes;
// List of logical volumes
var
  FindHandle: THandle;
//  VolumeNameBuf: array[0..MAX_PATH-1] of Char;
//  VolumePathNamesBuf: array[0..1023] of Char;
  Buf: array[0..1023] of Char;
  VolumeName, VolumePathNames, VolumeLabel: string;
  FreeAvailable, TotalSpace, TotalFree: Int64;
  VolumeSize: Int64;
  Res: Cardinal;
  s: string;
  m, f: Cardinal;
  _type: Integer;
  DiskHandle: THandle;
  diskExtents: VOLUME_DISK_EXTENTS;
  li: TListItem;
  i: Integer;
begin
  FindHandle := FindFirstVolume(@Buf, Length(Buf));
  if (FindHandle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError();

  try
    while True do
    begin
      Application.ProcessMessages();

      // "\\?\Volume{GIUD}"
      VolumeName := PChar(@Buf);

      if not VolumeName.StartsWith('\\?\') then
        raise Exception.CreateFmt('FindFirstVolume/FindNextVolume returned a bad path: %s', [VolumeName]);

      // Drive letter (we use first item from PathNames)
      if GetVolumePathNamesForVolumeName(PChar(VolumeName), @Buf, Length(Buf), Res) then
        VolumePathNames := ExcludeTrailingPathDelimiter(PChar(@Buf))
      else
        VolumePathNames := '';

      // Label
      GetVolumeInformation(PChar(VolumeName), @Buf, Length(Buf), nil, m, f, nil, 0);
      VolumeLabel:=PChar(@Buf);

      // Size
      if GetDiskFreeSpaceEx(PChar(VolumeName), FreeAvailable, TotalSpace, @TotalFree) then
        VolumeSize := TotalSpace
      else
        VolumeSize := -1;

      // Fixed/removable/cdrom etc.
      _type := GetDriveType(PChar(VolumeName));

      // On which phisical disk(s) it resides
      FillChar(diskExtents, SizeOf(diskExtents), 0);
      try
        DiskHandle := CreateFile( PChar(ExcludeTrailingPathDelimiter(VolumeName)),
                           0, //GENERIC_READ,
                           FILE_SHARE_READ or FILE_SHARE_WRITE,
                           nil,
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           0 );
        if DiskHandle = INVALID_HANDLE_VALUE then
          RaiseLastOSError();

        try
          if not DeviceIoControl(DiskHandle,
                                 IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
                                 nil, 0,
                                 @diskExtents, SizeOf(diskExtents),
                                 Res, nil) then
          begin
            diskExtents.NumberOfDiskExtents := 0;
          end;

        finally
          CloseHandle(DiskHandle);
        end;
      except

      end;

      // Add to list
      li := DiskListView.Items.Add;
      li.GroupID := 0;

      li.Caption := TDiskDataSource.DiskDisplayName(VolumePathNames, VolumeLabel);
      li.ImageIndex:=_type;
      if VolumeSize>=0 then
        li.SubItems.Add(FileSize2Str(VolumeSize))
      else
        li.SubItems.Add('?');
      li.SubItems.Add(ExcludeTrailingPathDelimiter(VolumeName));
      s := '';
      for i:=0 to diskExtents.NumberOfDiskExtents-1 do
      begin
        s := s + IntToStr(diskExtents.Extents[i].DiskNumber);
        if i < Integer(diskExtents.NumberOfDiskExtents-1) then
          s := s + ', ';
      end;
      li.SubItems.Add(s);

      //
      //  Move on to the next volume.
      if not FindNextVolumeW(FindHandle, @Buf, Length(Buf)) then
      begin
          if (GetLastError() <> ERROR_NO_MORE_FILES) then
            RaiseLastOSError();

          //
          //  Finished iterating
          //  through all the volumes.
          Break;
      end;
    end;

  finally
    FindVolumeClose(FindHandle);
  end;
end;

procedure TDiskSelectForm.FormShow(Sender: TObject);
begin
  RefreshTimer.Enabled := True;
end;

const
  SetupApiModuleName = 'SetupApi.dll';
  GUID_DEVINTERFACE_DISK:   TGUID = '{53F56307-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVINTERFACE_VOLUME: TGUID = '{53F5630D-B6BF-11D0-94F2-00A0C91EFB8B}';

  DIGCF_DEFAULT         = $00000001; // only valid with DIGCF_DEVICEINTERFACE
  {$EXTERNALSYM DIGCF_DEFAULT}
  DIGCF_PRESENT         = $00000002;
  {$EXTERNALSYM DIGCF_PRESENT}
  DIGCF_ALLCLASSES      = $00000004;
  {$EXTERNALSYM DIGCF_ALLCLASSES}
  DIGCF_PROFILE         = $00000008;
  {$EXTERNALSYM DIGCF_PROFILE}
  DIGCF_DEVICEINTERFACE = $00000010;
  {$EXTERNALSYM DIGCF_DEVICEINTERFACE}

  SPDRP_DEVICEDESC       = $00000000;
  SPDRP_FRIENDLYNAME     = $0000000C;
  SPDRP_CAPABILITIES     = $0000000F;
  SPDRP_CHARACTERISTICS  = $0000001B;
  SPDRP_REMOVAL_POLICY   = $0000001F;

  CM_DEVCAP_REMOVABLE = $00000004;

  CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL       = 1;
  CM_REMOVAL_POLICY_EXPECT_ORDERLY_REMOVAL  = 2;
  CM_REMOVAL_POLICY_EXPECT_SURPRISE_REMOVAL = 3;

type
  {$A-}
  STORAGE_DEVICE_NUMBER = packed record
    DeviceType: DWORD;
    DeviceNumber: DWORD;
    PartitionNumber: DWORD;
  end;

  HDEVINFO = Pointer;

  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: Cardinal;
    InterfaceClassGuid: TGUID;
    Flags: Cardinal;
    Reserved: ULONG_PTR;
  end;

  SP_DEVICE_INTERFACE_DETAIL_DATA = packed record
      cbSize: Cardinal;
      DevicePath: array [0..0] of Char;
  end;
  _SP_DEVICE_INTERFACE_DETAIL_DATA = SP_DEVICE_INTERFACE_DETAIL_DATA;
  PSP_DEVICE_INTERFACE_DETAIL_DATA = ^SP_DEVICE_INTERFACE_DETAIL_DATA;

  SP_DEVINFO_DATA = packed record
    cbSize: Cardinal;
    ClassGuid: TGUID;
    DevInst: Cardinal;
    Reserved: ULONG_PTR;
  end;
  {$EXTERNALSYM SP_DEVINFO_DATA}
  PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;
  {$EXTERNALSYM PSP_DEVINFO_DATA}

  DISK_GEOMETRY = record
    Cylinders: Int64;
    MediaType: DWORD;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;

  DISK_GEOMETRY_EX = record
    Geometry: DISK_GEOMETRY;
    DiskSize: Int64;
    Data: array[1..1000] of Byte;
  end;
  {$A+}

{$WARN SYMBOL_PLATFORM OFF}
function SetupDiGetClassDevs(ClassGuid: PGUID; const Enumerator: PWideChar;
  hwndParent: HWND; Flags: Cardinal): HDEVINFO; stdcall; external SetupApiModuleName name 'SetupDiGetClassDevsW' delayed;
{$EXTERNALSYM SetupDiGetClassDevs}
function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO; DeviceInfoData: Pointer; const InterfaceClassGuid: PGUID;
  MemberIndex: Cardinal;
  var DeviceInterfaceData: SP_DEVICE_INTERFACE_DATA): BOOL; stdcall; external SetupApiModuleName name 'SetupDiEnumDeviceInterfaces' delayed;
{$EXTERNALSYM SetupDiEnumDeviceInterfaces}
function SetupDiGetDeviceInterfaceDetail(DeviceInfoSet: HDEVINFO;
  var DeviceInterfaceData: SP_DEVICE_INTERFACE_DATA;
  DeviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA;
  DeviceInterfaceDetailDataSize: Cardinal; RequiredSize: PCardinal;
  Device: PSP_DEVINFO_DATA): BOOL; stdcall; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailW' delayed;
{$EXTERNALSYM SetupDiGetDeviceInterfaceDetail}
function SetupDiDestroyDeviceInfoList(
  DeviceInfoSet: HDEVINFO): BOOL; stdcall; external SetupApiModuleName name 'SetupDiDestroyDeviceInfoList' delayed;
{$EXTERNALSYM SetupDiDestroyDeviceInfoList}
function SetupDiGetDeviceRegistryProperty(
  DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSP_DEVINFO_DATA;
  AProperty: DWORD;
  PropertyRegDataType: PDWORD;
  PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD;
  RequiredSize: PDWORD
): BOOL; stdcall; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyW' delayed;

procedure TDiskSelectForm.ListPhysicalDrives(const diskClassGUID: TGUID; ListItemGroup: Integer);
var
  diskClassDevices: HDEVINFO;
//  diskClassDeviceInterfaceGuid: TGUID;
  device: SP_DEVINFO_DATA;
  deviceInterfaceData: SP_DEVICE_INTERFACE_DATA;
  deviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA;
  requiredSize: DWORD;
  deviceIndex: DWORD;
  disk: THandle;
  diskNumber: STORAGE_DEVICE_NUMBER;
  bytesReturned: DWORD;
  diskSize: Int64;
  diskDescr: string;
  diskCaps: Cardinal;
  PropBuf: array[0..1024] of Byte;
  DiskGeom: DISK_GEOMETRY_EX;

  li: TListItem;
begin
  diskClassDevices := Pointer(INVALID_HANDLE_VALUE);
  disk := INVALID_HANDLE_VALUE;

  //diskClassDeviceInterfaceGuid := GUID_DEVINTERFACE_VOLUME;//GUID_DEVINTERFACE_DISK;

  try
    //
    // Get the handle to the device information set for installed
    // disk class devices. Returns only devices that are currently
    // present in the system and have an enabled disk device
    // interface.
    //
    diskClassDevices := SetupDiGetClassDevs( @diskClassGUID, //diskClassDeviceInterfaceGuid,
                                             nil,
                                             0,
                                             DIGCF_PRESENT or
                                             DIGCF_DEVICEINTERFACE );
    if diskClassDevices = Pointer(INVALID_HANDLE_VALUE) then
      RaiseLastOSError();

    ZeroMemory( @deviceInterfaceData, SizeOf( SP_DEVICE_INTERFACE_DATA ) );
    deviceInterfaceData.cbSize := SizeOf( SP_DEVICE_INTERFACE_DATA );
    deviceIndex := 0;

    while ( SetupDiEnumDeviceInterfaces( diskClassDevices,
                                         nil,
                                         @diskClassGUID, //diskClassDeviceInterfaceGuid,
                                         deviceIndex,
                                         deviceInterfaceData ) ) do
    begin
      Application.ProcessMessages();


      Inc(deviceIndex);
      ZeroMemory(@device, SizeOf(device));
      device.cbSize := SizeOf(device);

      // Get "device" handle and size of disk name into requiredSize
      SetupDiGetDeviceInterfaceDetail( diskClassDevices,
                                       deviceInterfaceData,
                                       nil,
                                       0,
                                       @requiredSize,
                                       @device );
      if GetLastError( ) <> ERROR_INSUFFICIENT_BUFFER then
        RaiseLastOSError();

      deviceInterfaceDetailData :=  PSP_DEVICE_INTERFACE_DETAIL_DATA ( GetMemory( requiredSize ));

      ZeroMemory( deviceInterfaceDetailData, requiredSize );
      deviceInterfaceDetailData.cbSize := 8; //SizeOf( SP_DEVICE_INTERFACE_DETAIL_DATA );
      // Get disk name into deviceInterfaceDetailData.DevicePath
      if not SetupDiGetDeviceInterfaceDetail( diskClassDevices,
                                            deviceInterfaceData,
                                            deviceInterfaceDetailData,
                                            requiredSize,
                                            nil,
                                            nil ) then
        RaiseLastOSError();

      // Get device friendly name
      ZeroMemory(@PropBuf, SizeOf(PropBuf));
      if SetupDiGetDeviceRegistryProperty(diskClassDevices, @device,
                                          SPDRP_FRIENDLYNAME, nil, @PropBuf, SizeOf(PropBuf), nil) then
        diskDescr := PChar(@PropBuf)
      else
        diskDescr := '';

//      // Get device capabilities (removable?)
      diskCaps := 0;
      SetupDiGetDeviceRegistryProperty(diskClassDevices, @device,
                                       SPDRP_REMOVAL_POLICY, nil, @diskCaps, SizeOf(diskCaps), nil);


      diskNumber.DeviceNumber := 0;
      diskSize := 0;
      try
        disk := CreateFile( deviceInterfaceDetailData.DevicePath,
                           0, //GENERIC_READ,
                           FILE_SHARE_READ or FILE_SHARE_WRITE,
                           nil,
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           0 );
        if disk = INVALID_HANDLE_VALUE then
          RaiseLastOSError();

        // Get disk number (0, 1, 2, etc.)
        if not DeviceIoControl( disk,
                              IOCTL_STORAGE_GET_DEVICE_NUMBER,
                              nil,
                              0,
                              @diskNumber,
                              SizeOf( STORAGE_DEVICE_NUMBER ),
                              bytesReturned,
                              nil ) then
          RaiseLastOSError();

        // Get disk size
        if not DeviceIoControl(disk, IOCTL_DISK_GET_LENGTH_INFO,
                               nil, 0, @diskSize, SizeOf(diskSize),
                               bytesReturned, nil) then
          diskSize := 0;

        if DeviceIoControl(disk, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
                               nil, 0, @DiskGeom, SizeOf(DiskGeom),
                               bytesReturned, nil) then
          diskSize := DiskGeom.DiskSize;

        CloseHandle( disk );
      except
      end;
      disk := INVALID_HANDLE_VALUE;

      // Add to list view
      li := DiskListView.Items.Add;
      li.GroupID := ListItemGroup;
      li.Caption := diskDescr;
      if diskCaps = CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL then
        li.ImageIndex := DRIVE_FIXED
      else
        li.ImageIndex := DRIVE_REMOVABLE;
      if diskSize>=0 then
        li.SubItems.Add(FileSize2Str(diskSize))
      else
        li.SubItems.Add('?');
      li.SubItems.Add('\\?\PhysicalDrive' + IntToStr(diskNumber.DeviceNumber));

      FreeMemory(deviceInterfaceDetailData);
    end;
    if GetLastError( ) <> ERROR_NO_MORE_ITEMS then
      RaiseLastOSError();


  finally
    if ( diskClassDevices <> Pointer(INVALID_HANDLE_VALUE)) then
    begin
      SetupDiDestroyDeviceInfoList( diskClassDevices );
    end;

    if ( disk <> INVALID_HANDLE_VALUE ) then
    begin
      CloseHandle( disk );
    end;
  end;
end;

procedure TDiskSelectForm.DiskListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  BtnOpen.Enabled := (DiskListView.Selected <> nil);
end;

procedure TDiskSelectForm.DiskListViewDblClick(Sender: TObject);
begin
  if BtnOpen.Enabled then
    BtnOpen.Click;
end;

function TDiskSelectForm.SelectedDrive: string;
begin
  if DiskListView.Selected = nil then
    Result := ''
  else
    Result := DiskListView.Selected.SubItems[1];
end;

procedure TDiskSelectForm.ShowDrivesList;
var
  err: Cardinal;
begin
  DiskListView.Clear();
  BtnOpen.Enabled := False;
  DiskListView.Enabled := False;
  BusyPanel.Visible := True;
  Application.ProcessMessages();

//  When a user attempts to get information about a floppy drive that does not
//  have a floppy disk, or a CD-ROM drive that does not have a compact disc, the
//  system displays a message box for the user to insert a floppy disk or a compact
//  disc, respectively. To prevent the system from displaying this message box,
//  call the SetErrorMode function with SEM_FAILCRITICALERRORS.
  err:=SetErrorMode(SEM_FAILCRITICALERRORS);

  try
    try
      ListPhysicalDrives(GUID_DEVINTERFACE_DISK, 1);
    except
      on E: Exception do
        Application.ShowException(E);
    end;

    try
      FindVolumes();
    except
      on E: Exception do
        Application.ShowException(E);
    end;

  finally
    SetErrorMode(err);
    BtnOpen.Enabled := True;
    DiskListView.Enabled := True;
    BusyPanel.Visible := False;
  end;
end;

procedure TDiskSelectForm.RefreshTimerTimer(Sender: TObject);
begin
  RefreshTimer.Enabled := False;
  ShowDrivesList();
end;

end.
