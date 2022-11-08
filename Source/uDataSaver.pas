{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uDataSaver;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Forms,

  uHextorTypes, uEditedData, uHextorDataSources;

type
  // Used to save TEditedData to TDataSource
  TDataSaver = class
  public
    class procedure CopyDataRegion(Source, Dest: THextorDataSource;
      SourceAddr, DestAddr, Size: TFilePointer; ShowProgress: Boolean = False);
    class procedure Save(Data: TEditedData;
      NewDataSourceType: THextorDataSourceType; NewPath: string); overload;
    class procedure Save(Data: TEditedData; Dest: THextorDataSource); overload;
  end;

implementation

uses
  uMainForm;

{ TDataSaver }

class procedure TDataSaver.CopyDataRegion(Source, Dest: THextorDataSource;
  SourceAddr, DestAddr, Size: TFilePointer; ShowProgress: Boolean = False);
// Copy range of data between data sources.
// If range is large, copy by blocks and report progress.
const
  BlockSize = 10 * MByte;
var
  Buf: TBytes;
  Pos, PortionStart: TFilePointer;
  PortionSize: Integer;
  BytesDone: TFilePointer;
  Direction: Integer;
begin
  if Size = 0 then Exit;
  if (Source = Dest) and (SourceAddr = DestAddr) then Exit;

  SetLength(Buf, Min(BlockSize, Size));
  BytesDone := 0;
  // If shifting data in same file towards higher addresses, we may have to process blocks
  // in revese order to not overwrite data before it have been copied
  if (Source = Dest) and (DestAddr > SourceAddr) and (DestAddr < SourceAddr + Size) then
    Direction := -1
  else
    Direction := 1;
  if Direction = -1 then Pos := Size //Max(Size - BlockSize, 0)
                    else Pos := 0;

  while BytesDone < Size do
  begin
    PortionSize := Min(BlockSize, Size - BytesDone);
    if Direction = 1 then PortionStart := Pos
                     else PortionStart := Pos - PortionSize;
    Source.GetData(SourceAddr + PortionStart, PortionSize, Buf[0]);
    Dest.ChangeData(DestAddr + PortionStart, PortionSize, Buf[0]);
    Pos := Pos + PortionSize * Direction;
    BytesDone := BytesDone + PortionSize;
    if (ShowProgress) and (BytesDone < Size) then
      Progress.Show(BytesDone, Size);
  end;
end;

class procedure TDataSaver.Save(Data: TEditedData; Dest: THextorDataSource);
var
  InplaceSaving: Boolean;
  APart: TEditedData.TDataPart;
  WriteByTypes: Boolean;
  // Blocks grouped by type. See comments (*) below.
  Parts1, Parts2, Parts3: TEditedData.TDataPartList;
  TotalSize, BytesToWrite, BytesDone: TFilePointer;
  i: Integer;

  procedure WritePart(Part: TEditedData.TDataPart);
  begin
    case Part.PartType of
      ptSource:
        begin
          Progress.TaskStart(Data, Part.Size / BytesToWrite);
          try
            CopyDataRegion(Data.DataSource, Dest, Part.SourceAddr, Part.Addr,
              Part.Size, True);
          finally
            Progress.TaskEnd();
          end;
        end;
      ptBuffer:
        Dest.ChangeData(Part.Addr, Part.Size, Part.Data[0]);
    end;

    BytesDone := BytesDone + Part.Size;
    Progress.Show(BytesDone, BytesToWrite);
  end;

begin
  InplaceSaving := (Dest = Data.DataSource);
  if InplaceSaving then
  // If saving to same file, re-open it for writing
  begin
    try
      Data.DataSource.Open(fmOpenReadWrite, fmShareExclusive);
    except
      // If failed to open for write (e.g. used by other app) - re-open for reading
      Data.DataSource.Open(fmOpenRead, fmShareDenyNone);
      raise;
    end;
  end
  else
  // Overwrite target
  begin
    Dest.Open(fmCreate);
  end;

  TotalSize := Data.GetSize();

  // If saving in-place, we may have to enlarge file
  if (InplaceSaving) and (dspResizable in Dest.GetProperties()) and
    (TotalSize > Dest.GetSize) then
    Dest.SetSize(TotalSize);

  // (*) If some blocks should be moved inside same file toward higher addresses,
  // they can overwrite other blocks before they are processed.
  // To prevent this, we group blocks by type and process them in this order:
  Parts1 := TEditedData.TDataPartList.Create(False);  // Moved to lower addresses
  Parts2 := TEditedData.TDataPartList.Create(False);  // Moved to higher addresses (will be written in reverse order)
  Parts3 := TEditedData.TDataPartList.Create(False);  // Blocks from memory (changed blocks)
  // If this is not the case, we can write all blocks sequentially to improve speed.
  WriteByTypes := False;
  BytesToWrite := 0;

  Progress.TaskStart(Data);
  try

    if InplaceSaving then
      Progress.OnAborting.Add(procedure (Sender: TProgressTracker; CanAbort: PBoolean)
        begin
          if Application.MessageBox(PChar('Aborting this operation will most likely corrupt your file and in-memory buffers. Are you sure you want to abort?'), PChar('Abort saving'), MB_ICONWARNING or MB_YESNO) <> IDYES then
            CanAbort^ := False;
        end);

    // Prepare block lists for saving
    if (InplaceSaving) then
    begin
      for APart in Data.Parts do
      begin
        if (APart.PartType = ptSource) then
        begin
          // Don't write blocks that have not been moved
          if APart.Addr = APart.SourceAddr then
          begin
          end
          else
          begin
            // Block moved to lower or higher addresses
            if APart.Addr < APart.SourceAddr then
              Parts1.Add(APart)
            else
            begin
              Parts2.Add(APart);
              // If we have such blocks, we will write blocks ordered by type
              WriteByTypes := True;
            end;
            BytesToWrite := BytesToWrite + APart.Size;
          end;
        end;
        if (APart.PartType = ptBuffer) then
        begin
          Parts3.Add(APart);
          BytesToWrite := BytesToWrite + APart.Size;
        end;
        Progress.Show(0);
      end;
    end
    else
      BytesToWrite := Data.GetSize();

    // Write blocks to file
    BytesDone := 0;
    if WriteByTypes then
    begin
      // Write blocks moved to lower addresses
      for i := 0 to Parts1.Count-1 do
        WritePart(Parts1[i]);
      // Write blocks that have been shifted towards higher addressed, in reverse order
      for i := Parts2.Count-1 downto 0 do
        WritePart(Parts2[i]);
      // Write blocks from memory
      for i := 0 to Parts3.Count-1 do
        WritePart(Parts3[i]);
    end
    else
    begin
      // Write all blocks sequentially
      for APart in Data.Parts do
      begin
        WritePart(APart);
      end;
    end;

  finally
    Progress.TaskEnd();
    Parts1.Free;
    Parts2.Free;
    Parts3.Free;
  end;

  // TODO: Handle write errors

  // If saving in-place, we may have to truncate file
  if (InplaceSaving) and (dspResizable in Dest.GetProperties()) and
    (TotalSize < Dest.GetSize) then
    Dest.SetSize(TotalSize);

  // Open new saved file for reading again
  if InplaceSaving then
    Data.DataSource.Open(fmOpenRead, fmShareDenyNone);
end;

class procedure TDataSaver.Save(Data: TEditedData;
  NewDataSourceType: THextorDataSourceType; NewPath: string);
var
  Dest: THextorDataSource;
begin
  if (NewDataSourceType = nil) or (NewPath = '') then
    Exit;

  Dest := NewDataSourceType.Create(NewPath);
  try
    Save(Data, Dest);
  finally
    Dest.Free;
  end;
end;

end.
