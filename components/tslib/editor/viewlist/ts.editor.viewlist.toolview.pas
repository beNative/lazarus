{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Editor.ViewList.ToolView;

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

