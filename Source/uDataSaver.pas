{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDataSaver;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,

  uHextorTypes, uEditedData, uHextorDataSources;

type
  // Used to save TEditedData to TDataSource
  TDataSaver = class
  public
    class function ChooseSaveMethod(Data: TEditedData;
      DataSourceType: THextorDataSourceType; const APath: string;
      var InplaceSaving, UseTempFile: Boolean): Boolean;
    class procedure CopyDataRegion(Source, Dest: THextorDataSource;
      SourceAddr, DestAddr, Size: TFilePointer; ShowProgress: Boolean = False);
    class procedure Save(Data: TEditedData;
      NewDataSourceType: THextorDataSourceType; NewPath: string);
  end;

implementation

uses
  uMainForm;

{ TDataSaver }

class function TDataSaver.ChooseSaveMethod(Data: TEditedData;
  DataSourceType: THextorDataSourceType; const APath: string;
  var InplaceSaving, UseTempFile: Boolean): Boolean;
// How can we save our modified data to given target:
// Can we only write changed parts, or maybe we'll have to use temp file?
var
  SameSource: Boolean;
begin
  SameSource := (DataSourceType = Data.DataSource.ClassType) and
    (SameFileName(APath, Data.DataSource.Path));

  InplaceSaving := SameSource and (not Data.HasMovements());
  UseTempFile := (SameSource) and (not InplaceSaving);
  Result := True;
end;

class procedure TDataSaver.CopyDataRegion(Source, Dest: THextorDataSource;
  SourceAddr, DestAddr, Size: TFilePointer; ShowProgress: Boolean = False);
const
  BlockSize = 10 * MByte;
var
  Buf: TBytes;
  Pos: TFilePointer;
  PortionSize: Integer;
begin
  SetLength(Buf, BlockSize);
  Pos := 0;

  while Pos < Size do
  begin
    PortionSize := Min(BlockSize, Size - Pos);
    Source.GetData(SourceAddr + Pos, PortionSize, Buf[0]);
    Dest.ChangeData(DestAddr + Pos, PortionSize, Buf[0]);
    Pos := Pos + PortionSize;
    if (ShowProgress) and (Pos < Size) then
      Progress.Show(Pos, Size);
  end;
end;

class procedure TDataSaver.Save(Data: TEditedData;
  NewDataSourceType: THextorDataSourceType; NewPath: string);
// Save Data to selected path. If path as same as original data source,
// can write only changed parts.
// This method may replace Data.DataSource with new one, if saving required
// a temp file.
var
  Dest: THextorDataSource;
  InplaceSaving, UseTempFile: Boolean;
  TempFileName: string;
  APart: TEditedData.TDataPart;
begin
  if (NewDataSourceType = nil) or (NewPath = '') then
    Exit;

  if not ChooseSaveMethod(Data, NewDataSourceType, NewPath, InplaceSaving,
    UseTempFile) then
    raise Exception.Create('Cannot save this data to this target');

  if InplaceSaving then
  // If saving to same file, re-open it for writing
  begin
    try
      Data.DataSource.Open(fmOpenReadWrite);
    except
      // If failed to open for write (e.g. used by other app) - re-open for reading
      Data.DataSource.Open(fmOpenRead);
      raise;
    end;
    Dest := Data.DataSource;
  end
  else
  // If saving to another file, create another DataSource
  begin
    if UseTempFile then
    // Overwrite using temporary file
    begin
      if NewDataSourceType = TFileDataSource then
        TempFileName := NewPath + '_temp' + IntToStr(Random(10000))
      else
        TempFileName := MainForm.TempPath + 'save' + IntToStr(Random(10000));
      Dest := TFileDataSource.Create(TempFileName);
    end
    else
      // Save to new file
      Dest := NewDataSourceType.Create(NewPath);
    Dest.Open(fmCreate);
  end;

  // Write regions
  Progress.TaskStart(Data);
  try
    for APart in Data.Parts do
    begin
      if (InplaceSaving) and (APart.PartType = ptSource) then
        Continue;
      case APart.PartType of
        ptSource:
          begin
            Progress.TaskStart(Data, APart.Size / Data.GetSize);
            try
              CopyDataRegion(Data.DataSource, Dest, APart.SourceAddr, APart.Addr,
                APart.Size, True);
            finally
              Progress.TaskEnd();
            end;
          end;
        ptBuffer:
          Dest.ChangeData(APart.Addr, APart.Size, APart.Data[0]);
      end;

      Progress.Show(APart.Addr + APart.Size, Data.GetSize);
    end;
  finally
    Progress.TaskEnd();
  end;
  // TODO: Handle write errors

  // If saving in-place, we may have to truncate file
  if (InplaceSaving) and (dspResizable in Dest.GetProperties()) and
    (Data.GetSize() <> Dest.GetSize) then
    Dest.SetSize(Data.GetSize());

  if UseTempFile then
  // Saving throught temporary file
  begin
    if NewDataSourceType = TFileDataSource then
    // Destination is file - rename TempFile to target filename
    begin
      Data.DataSource.Free;
      Dest.Free;
      DeleteFile(NewPath);
      RenameFile(TempFileName, NewPath);
      Data.DataSource := TFileDataSource.Create(NewPath);
    end
    else
    // Not a file - copy tempfile content to destination
    begin
      Data.DataSource.Free;
      Data.DataSource := NewDataSourceType.Create(NewPath);
      Data.DataSource.Open(fmCreate);
      Data.DataSource.CopyContentFrom(Dest);
      Dest.Free;
      DeleteFile(TempFileName);
    end;
  end
  else
  begin
    if not InplaceSaving then
    // Switch to new file
    begin
      Data.DataSource.Free;
      Data.DataSource := Dest;
    end;
  end;

  // Open new saved file for reading
  Data.DataSource.Open(fmOpenRead);

end;

end.
