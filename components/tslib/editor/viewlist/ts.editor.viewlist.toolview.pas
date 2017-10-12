{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts.Editor.viewlist.ToolView;

{$MODE DELPHI}

interface

uses
  SysUtils, ActnList, Menus, Contnrs, ExtCtrls, StdCtrls,

  VirtualTrees,

  ts.Core.TreeViewPresenter,

  ts.Editor.Interfaces, ts.Editor.ToolView.Base;

type
  TfrmViewList = class(TCustomEditorToolView, IEditorToolView)
    aclMain               : TActionList;
    actClose              : TAction;
    actCloseSelectedViews : TAction;
    btnClose              : TButton;
    mniClose              : TMenuItem;
    pnlBottom             : TPanel;
    pnlVST                : TPanel;
    ppmMain               : TPopupMenu;

    procedure actCloseExecute(Sender: TObject);
    procedure actCloseSelectedViewsExecute(Sender: TObject);

  private
    FVST      : TVirtualStringTree;
    FItemList : TObjectList;
    FTVP      : TTreeViewPresenter;

    procedure FTVPSelectionChanged(Sender: TObject);

  protected
    procedure UpdateView; override;
    procedure UpdateActions; override;
    procedure Refresh;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

uses
  Controls,

  ts.Core.ColumnDefinitions, ts.Core.Helpers,

  ts.Editor.ViewList.Data;

resourcestring
  SFileName    = 'Filename';
  SHighlighter = 'Highlighter';
  SModified    = 'Modified';
  SPath        = 'Path';

{$REGION 'construction and destruction'}
procedure TfrmViewList.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := True;
  with FTVP.ColumnDefinitions.AddColumn('FileName', SFileName, dtString, 200, 30, 1000) do
  begin

  end;
  FTVP.ColumnDefinitions.AddColumn('Highlighter', SHighlighter, dtString, 80);
  with FTVP.ColumnDefinitions.AddColumn('Modified', SModified, dtString, 80) do
  begin
    ColumnType := TColumnType.ctCheckBox;
  end;
  with FTVP.ColumnDefinitions.AddColumn('Path', SPath, dtString, 100, 30, 1000) do
  begin
  end;
  FItemList := TObjectList.Create;
  Refresh;
  FTVP.ItemsSource        := FItemList;
  FTVP.PopupMenu          := ppmMain;
  FTVP.TreeView           := FVST;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
end;

procedure TfrmViewList.BeforeDestruction;
begin
  FreeAndNil(FItemList);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmViewList.actCloseExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TfrmViewList.actCloseSelectedViewsExecute(Sender: TObject);
var
  V : IEditorView;
  I : Integer;
begin
  for I := 0 to FTVP.SelectedItems.Count - 1 do
  begin
    V := TEditorViewInfo(FTVP.SelectedItems[I]).View;
    Views.Delete(V);
  end;
  Refresh;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmViewList.FTVPSelectionChanged(Sender: TObject);
var
  V: IEditorView;
begin
  if Assigned(FTVP.SelectedItem) then
  begin
    V := (FTVP.SelectedItem as TEditorViewInfo).View;
    Manager.ActiveView := V;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmViewList.UpdateView;
begin
  FVST.Invalidate;
end;

procedure TfrmViewList.UpdateActions;
begin
  if FItemList.Count <> Views.Count then
  begin
    Refresh;
  end;
  inherited UpdateActions;
end;

procedure TfrmViewList.Refresh;
var
  I: Integer;
begin
  FItemList.Clear;
  for I := 0 to Views.Count - 1 do
  begin
    FItemList.Add(TEditorViewInfo.Create(Views[I]));
  end;
  FTVP.Refresh;
end;
{$ENDREGION}

end.

