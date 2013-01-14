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

unit ts_Editor_ActionListViewForm;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  ComCtrls, Contnrs,

  FileUtil, LResources, ButtonPanel,

  VirtualTrees,

  ts_Core_TreeViewPresenter,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmActionListView = class(TForm, IEditorToolView)
    pgcMain         : TPageControl;
    pnlButtons      : TButtonPanel;
    pnlEditorList   : TPanel;
    tsMouseActions  : TTabSheet;
    tsCommands      : TTabSheet;
    tsActions       : TTabSheet;
    vstActions      : TVirtualStringTree;
    vstCommands     : TVirtualStringTree;
    vstMouseActions : TVirtualStringTree;

    procedure FormShow(Sender: TObject);

  private
    FTVPActions      : TTreeViewPresenter;
    FTVPCommands     : TTreeViewPresenter;
    FTVPMouseActions : TTreeViewPresenter;
    FActionItems     : TObjectList;
    FKeyStrokeItems  : TObjectList;
    FMouseItems      : TObjectList;

    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;

  protected
    procedure UpdateLists;
    property Manager: IEditorManager
      read GetManager;

    { IEditorToolView }
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean); override;
    procedure UpdateView;

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
  TypInfo, StrUtils, Variants,

  SynEditKeyCmds, SynEditMouseCmds,

  LCLProc,

  ts_Core_ColumnDefinitions, ts_Core_ColumnDefinitionsDataTemplate;

type
  TActionListTemplate = class(TColumnDefinitionsDataTemplate)
    function GetImageIndex(
      const Item        : TObject;
      const ColumnIndex : Integer
    ): Integer; override;
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): Variant; override;
  end;

  TKeyStrokeTemplate = class(TColumnDefinitionsDataTemplate)
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): Variant; override;
  end;

  TMouseActionTemplate = class(TColumnDefinitionsDataTemplate)
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): Variant; override;
  end;

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

function TActionListTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
var
  CD: TColumnDefinition;
begin
  Result := -1;
  if Assigned(Item) and Assigned(ColumnDefinitions)
    and (ColumnIndex < ColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
    if CD.Caption = '' then
    begin
      if IsPublishedProp(Item, 'ImageIndex') then
        Result := GetPropValue(Item, 'ImageIndex')
      else
        Result := -1;
      end
    end;
end;

function TActionListTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): Variant;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Shortcut') then
  begin
    Result := ShortCutToText(Result);
  end;
end;

function TKeyStrokeTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): Variant;
var
  C: TSynEditorCommand;
  S : string;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Command') then
  begin
    C := TSynEditorCommand(Integer(Result));
    S := EditorCommandToCodeString(C);
    S[1] := LowerCase(S[1]);
    Result := S;
  end
  else if SameText(APropertyName, 'Hint') then
  begin
    C := TSynEditorCommand(Integer((Item as TSynEditKeyStroke).Command));
    Result := EditorCommandToDescrString(C);
  end
  else if AnsiMatchText(APropertyName, ['Shortcut', 'Shortcut2']) then
  begin
    Result := ShortCutToText(Result);
  end;
end;

function TMouseActionTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): Variant;
var
  C: TSynEditorMouseCommand;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Command') then
  begin
    C := TSynEditorMouseCommand(Integer(Result));
    Result := MouseCommandName(C);
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmActionListView.AfterConstruction;
begin
  inherited AfterConstruction;
  FTVPActions := TTreeViewPresenter.Create(Self);
  FTVPActions.ListMode := True;
  FTVPActions.ImageList := Manager.Actions.ActionList.Images as TImageList;
  FTVPActions.ItemTemplate := TActionListTemplate.Create(FTVPActions.ColumnDefinitions);
  FTVPActions.ColumnDefinitions.AddColumn('Name', dtString, 150, 150, 200);
  with FTVPActions.ColumnDefinitions.AddColumn('', dtString, 24) do
  begin
    Fixed := True;
  end;
  FTVPActions.ColumnDefinitions.AddColumn('Category', dtString, 80);
  FTVPActions.ColumnDefinitions.AddColumn('Caption', dtString, 120, 100, 200);
  FTVPActions.ColumnDefinitions.AddColumn('Shortcut', dtString, 100);
  FTVPActions.ColumnDefinitions.AddColumn('Hint', dtString, 200, 200, 400);
  with FTVPActions.ColumnDefinitions.AddColumn('Visible', dtString, 50) do
  begin
    Fixed := True;
    CheckBox := True;
  end;
  with FTVPActions.ColumnDefinitions.AddColumn('Enabled', dtString, 50) do
  begin
    Fixed := True;
    CheckBox := True;
  end;

  FTVPCommands := TTreeViewPresenter.Create(Self);
  FTVPCommands.ListMode := True;
  FTVPCommands.ItemTemplate := TKeyStrokeTemplate.Create(FTVPCommands.ColumnDefinitions);
  FTVPCommands.ColumnDefinitions.AddColumn('Command', dtString, 200, 100, 400);
  FTVPCommands.ColumnDefinitions.AddColumn('Shortcut', dtString, 120);
  FTVPCommands.ColumnDefinitions.AddColumn('Shortcut2', dtString, 120);
  FTVPCommands.ColumnDefinitions.AddColumn('Hint', dtString, 200, 100, 600);

  FTVPMouseActions := TTreeViewPresenter.Create(Self);
  FTVPMouseActions.ListMode := True;
  FTVPMouseActions.ItemTemplate := TMouseActionTemplate.Create(FTVPMouseActions.ColumnDefinitions);
  FTVPMouseActions.ColumnDefinitions.AddColumn('Command', dtString, 200, 100, 400);
  FTVPMouseActions.ColumnDefinitions.AddColumn('Button', dtString, 120);
  FTVPMouseActions.ColumnDefinitions.AddColumn('Shift', dtString, 120);
  FTVPMouseActions.ColumnDefinitions.AddColumn('ShiftMask', dtString, 120);
  FTVPMouseActions.ColumnDefinitions.AddColumn('ClickCount', dtNumeric, 100);
  FTVPMouseActions.ColumnDefinitions.AddColumn('ClickDir', dtString, 100);
  with FTVPMouseActions.ColumnDefinitions.AddColumn('MoveCaret', dtString, 100) do
  begin
    Fixed := True;
    CheckBox := True;
  end;

  FActionItems := TObjectList.Create(False);
  FKeyStrokeItems := TObjectList.Create(False);
  FMouseItems := TObjectList.Create(False);

  FTVPActions.ItemsSource := FActionItems;
  FTVPActions.TreeView := vstActions;

  FTVPMouseActions.ItemsSource := FMouseItems;
  FTVPMouseActions.TreeView    := vstMouseActions;

  FTVPCommands.ItemsSource := FKeyStrokeItems;
  FTVPCommands.TreeView    := vstCommands;
end;

procedure TfrmActionListView.BeforeDestruction;
begin
  FreeAndNil(FActionItems);
  FreeAndNil(FKeyStrokeItems);
  FreeAndNil(FMouseItems);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmActionListView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmActionListView.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmActionListView.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmActionListView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmActionListView.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

procedure TfrmActionListView.UpdateView;
begin
  //
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmActionListView.FormShow(Sender: TObject);
begin
  UpdateLists;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmActionListView.UpdateLists;
var
  K: TCollectionItem;
  A: TContainedAction;
begin
  FActionItems.Clear;
  FKeyStrokeItems.Clear;
  FMouseItems.Clear;
  for K in Manager.ActiveView.Editor.MouseActions do
    FMouseItems.Add(K);
  for K in Manager.ActiveView.Editor.Keystrokes do
    FKeyStrokeItems.Add(K);
  for A in Manager.Actions.ActionList do
    FActionItems.Add(A);
  FTVPActions.Refresh;
  FTVPCommands.Refresh;
  FTVPMouseActions.Refresh;
  vstActions.Header.AutoFitColumns;
  vstMouseActions.Header.AutoFitColumns;
  vstCommands.Header.AutoFitColumns;
  vstActions.Header.MainColumn := 0;
  vstActions.Header.Options := vstActions.Header.Options + [hoAutoSpring, hoAutoResize];
  vstMouseActions.Header.MainColumn := 0;
  vstMouseActions.Header.Options := vstMouseActions.Header.Options + [hoAutoSpring, hoAutoResize];
  vstCommands.Header.MainColumn := 0;
  vstCommands.Header.Options := vstCommands.Header.Options + [hoAutoSpring, hoAutoResize];

end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

