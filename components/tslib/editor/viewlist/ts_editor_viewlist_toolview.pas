{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_ViewList_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, ActnList, Menus, Contnrs, ExtCtrls, StdCtrls,

  VirtualTrees,

  ts.Core.TreeViewPresenter,

  ts.Editor.Interfaces, ts_Editor_ToolView_Base;

type

  { TfrmViewList }

  TfrmViewList = class(TCustomEditorToolView, IEditorToolView)
    aclMain    : TActionList;
    actClose   : TAction;
    Button1: TButton;
    MenuItem2  : TMenuItem;
    mniClose   : TMenuItem;
    pnlBottom: TPanel;
    pnlVST     : TPanel;
    ppmMain    : TPopupMenu;
    ppmHL      : TPopupMenu;

    procedure actCloseExecute(Sender: TObject);

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
  ts.Core.ColumnDefinitions, ts.Core.Utils, ts.Core.Helpers,

  ts.Editor.ViewList.Data;

{$region 'construction and destruction' /fold}
procedure TfrmViewList.AfterConstruction;
var
  I: Integer;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := True;
  FTVP.ColumnDefinitions.AddColumn('FileName', dtString, 150);
  with FTVP.ColumnDefinitions.AddColumn('Highlighter', dtString, 80) do
  begin

  end;
  with FTVP.ColumnDefinitions.AddColumn('Modified', dtString, 80) do
  begin
    ColumnType := TColumnType.ctCheckBox;
    AllowEdit := True;
  end;
  FTVP.ColumnDefinitions.AddColumn('Path', dtString, 400);
  FItemList := TObjectList.Create;
  Refresh;
  FTVP.ItemsSource        := FItemList;
  FTVP.PopupMenu          := ppmMain;
  FTVP.TreeView           := FVST;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;

  ppmHL.Items.Clear;
  for I := 0 to Manager.Menus.HighlighterPopupMenu.Items.Count - 1 do
    ppmHL.Items.Add(CloneMenuItem(Manager.Menus.HighlighterPopupMenu.Items[I]));

  //ppmHL.Items.Assign(Manager.Menus.HighlighterPopupMenu.Items);
//  ppmMain.Items.Insert(1, ppmHL.Items);
  //ppmEditor.Items.Insert(15, ppmHighLighters.Items);
end;

procedure TfrmViewList.BeforeDestruction;
begin
  FreeAndNil(FItemList);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmViewList.actCloseExecute(Sender: TObject);
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
{$endregion}

{$region 'event handlers' /fold}
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
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmViewList.UpdateView;
begin
  FVST.Invalidate;
end;

procedure TfrmViewList.UpdateActions;
begin
  if FItemList.Count <> Views.Count then
    Refresh;
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
{$endregion}

end.

