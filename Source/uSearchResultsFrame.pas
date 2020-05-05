{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2020  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uSearchResultsFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VirtualTrees, Math, System.UITypes, System.Types, Vcl.Menus,

  uHextorTypes, uEditorForm, uSearchResultsTabFrame;

type
  TSearchResultsFrame = class(TFrame)
    PageControl: TPageControl;
    ResultsTabMenu: TPopupMenu;
    MIClose: TMenuItem;
    procedure PageControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MICloseClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
    LastUsedNameIndex: Integer;
    ResultsTabMenuTabIndex: Integer;
    PrevPageIndex: Integer;
    procedure RefreshResultsEditor(PageIndex: Integer);
  public
    { Public declarations }
    function StartNewList(AEditor: TEditorForm; const ASearchText: string): TSearchResultsTabFrame;
  end;

implementation

{$R *.dfm}

procedure TSearchResultsFrame.MICloseClick(Sender: TObject);
begin
  if (ResultsTabMenuTabIndex >= 0) and (ResultsTabMenuTabIndex < PageControl.PageCount) then
    PageControl.Pages[ResultsTabMenuTabIndex].Free;
end;

procedure TSearchResultsFrame.PageControlChange(Sender: TObject);
begin
  RefreshResultsEditor(PrevPageIndex);
  RefreshResultsEditor(PageControl.ActivePageIndex);
end;

procedure TSearchResultsFrame.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  PrevPageIndex := PageControl.ActivePageIndex;
end;

procedure TSearchResultsFrame.PageControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  N: Integer;
  P: TPoint;
begin
  N := PageControl.IndexOfTabAt(X, Y);
  if N >= 0 then
  begin
    if Button = mbMiddle then
      PageControl.Pages[N].Free;
    if Button = mbRight then
    begin
      ResultsTabMenuTabIndex := N;
      P := (Sender as TControl).ClientToScreen(Point(X,Y));
      ResultsTabMenu.Popup(P.X, P.Y);
    end;
  end;
end;

procedure TSearchResultsFrame.RefreshResultsEditor(PageIndex: Integer);
var
  AEditor: TEditorForm;
begin
  // Refresh specified result page's editors to highlight found items in data
  // when switching between result pages

  if (PageIndex < 0) or (PageIndex >= PageControl.PageCount) then Exit;
  with PageControl.Pages[PageIndex] do
    if (ControlCount > 0) and (Controls[0] is TSearchResultsTabFrame) then
      for AEditor in (Controls[0] as TSearchResultsTabFrame).GetLinkedEditors() do
        AEditor.UpdatePanes();
end;

function TSearchResultsFrame.StartNewList(AEditor: TEditorForm; const ASearchText: string): TSearchResultsTabFrame;
// Add new results tab
var
  Sheet: TTabSheet;
begin
  Sheet := TTabSheet.Create(PageControl);
  Sheet.PageControl := PageControl;
  Result := TSearchResultsTabFrame.Create(Self);
  Inc(LastUsedNameIndex);
  Result.Name := 'SearchResultsTabFrame' + IntToStr(LastUsedNameIndex);
  Result.Parent := Sheet;
  Result.Align := alClient;
  Result.StartList(AEditor, ASearchText);
  PageControl.ActivePage := Sheet;
end;

end.
