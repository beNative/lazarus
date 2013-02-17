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

unit ts_Editor_ViewListForm;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, ActnList, Menus, Contnrs, ExtCtrls,

  ButtonPanel,

  VirtualTrees,

  ts_Core_TreeViewPresenter,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmViewList = class(TForm, IEditorToolView)
    aclMain    : TActionList;
    actClose   : TAction;
    MenuItem1  : TMenuItem;
    MenuItem2  : TMenuItem;
    mniClose   : TMenuItem;
    pnlButtons : TButtonPanel;
    pnlVST     : TPanel;
    ppmMain    : TPopupMenu;
    ppmHL      : TPopupMenu;

    procedure actCloseExecute(Sender: TObject);

  private
    FVST      : TVirtualStringTree;
    FItemList : TObjectList;
    FTVP      : TTreeViewPresenter;

    procedure FTVPSelectionChanged(Sender: TObject);

    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;
    function GetViews: IEditorViews;

  protected
    procedure UpdateView;
    procedure UpdateActions; override;
    procedure Refresh;

    property Views: IEditorViews
      read GetViews;

    property Manager: IEditorManager
      read GetManager;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean); override;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_ColumnDefinitions, ts_Core_Utils, ts_Core_Helpers;

{ TODO -oTS : Use Column Data templates }

{$region 'TEditorViewInfo' /fold}
type
  TEditorViewInfo = class
  private
    FView: TComponent; // TSI: no interface reference here!

    function GetFileName: string;
    function GetHighlighter: string;
    function GetModified: Boolean;
    function GetPath: string;
    function GetView: IEditorView;

  public
    constructor Create(AView: IEditorView);
    procedure BeforeDestruction; override;

    property View: IEditorView
      read GetView;

  published
    property FileName: string
      read GetFileName;

    property Path: string
      read GetPath;

    property Highlighter: string
      read GetHighlighter;

    property Modified: Boolean
      read GetModified;
  end;

constructor TEditorViewInfo.Create(AView: IEditorView);
begin
  FView := AView.Form;
end;

procedure TEditorViewInfo.BeforeDestruction;
begin
  FView := nil;
  inherited BeforeDestruction;
end;

function TEditorViewInfo.GetFileName: string;
begin
  Result := View.Form.Caption;
end;

function TEditorViewInfo.GetHighlighter: string;
begin
  Result := View.HighlighterItem.Name;
end;

function TEditorViewInfo.GetModified: Boolean;
begin
  Result := View.Modified;
end;

function TEditorViewInfo.GetPath: string;
begin
  Result := View.FileName;
end;

function TEditorViewInfo.GetView: IEditorView;
begin
  Result := FView as IEditorView;
end;
{$endregion}

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmViewList.AfterConstruction;
var
  I: Integer;
begin
  inherited AfterConstruction;
  FVST := CreateVST(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := True;
  FTVP.ColumnDefinitions.AddColumn('FileName', dtString, 150);
  FTVP.ColumnDefinitions.AddColumn('Path', dtString, 400);
  with FTVP.ColumnDefinitions.AddColumn('Highlighter', dtString, 80) do
  begin

  end;
  with FTVP.ColumnDefinitions.AddColumn('Modified', dtString, 80) do
  begin
    ColumnType := TColumnType.ctCheckBox;
  end;
  FItemList := TObjectList.Create;
  Refresh;
  FTVP.ItemsSource := FItemList;
  FTVP.PopupMenu := ppmMain;
  FTVP.TreeView := FVST;
  FTVP.OnSelectionChanged  := FTVPSelectionChanged;

  for I := 0 to Manager.Menus.HighlighterPopupMenu.Items.Count - 1 do
    ppmHL.Items.Add(CloneMenuItem(Manager.Menus.HighlighterPopupMenu.Items[I]));

  //ppmHL.Items.Assign(Manager.Menus.HighlighterPopupMenu.Items);
  ppmMain.Items.Insert(1, ppmHL.Items);
  //ppmEditor.Items.Insert(15, ppmHighLighters.Items);
end;

procedure TfrmViewList.BeforeDestruction;
begin
  FreeAndNil(FItemList);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmViewList.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmViewList.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

function TfrmViewList.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmViewList.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmViewList.GetViews: IEditorViews;
begin
  Result := Owner as IEditorViews;
end;

function TfrmViewList.GetForm: TForm;
begin
  Result := Self;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'action handlers' /fold}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

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

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

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

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

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

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

end.

