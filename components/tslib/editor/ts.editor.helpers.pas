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

unit ts.Editor.Helpers;

{$MODE Delphi}

{ Some helper routines to build a simple editor. }

{ TODO: make factories to create instances. }

interface

uses
  Classes, SysUtils, ComCtrls, Menus, Controls, ActnList,

  ts.Editor.Interfaces, ts_Editor_Resources;

procedure AddActionButton(
  AParent : TToolBar;
  AAction : TBasicAction = nil
);

procedure AddEditorButton(
        AManager    : IEditorManager;
        AParent     : TToolBar;
  const AActionName : string = ''
);

procedure AddStandardEditorToolbarButtons(
  AManager : IEditorManager;
  AToolBar : TToolbar
);

procedure AddExperimentalEditorToolbarButtons(
  AManager : IEditorManager;
  AToolBar : TToolbar
);

procedure AddSelectionToolbarButtons(
  AManager : IEditorManager;
  AToolBar : TToolbar
);

implementation

uses
  Forms,

  ts_Editor_Manager;

procedure AddActionButton(AParent: TToolBar; AAction: TBasicAction);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  if not Assigned(AAction) then
    TB.Style := tbsDivider
  else
    TB.Action := AAction;
end;

procedure AddDropDownButton(AParent: TToolBar; AAction: TBasicAction;
  APopupMenu: TPopupMenu);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  TB.Style := tbsDropDown;
  TB.DropdownMenu := APopupMenu;
  TB.Action := AAction;
end;

procedure AddEditorButton(AManager: IEditorManager; AParent: TToolBar;
  const AActionName: string);
begin
  if AActionName = '' then
    AddActionButton(AParent)
  else
    AddActionButton(AParent, AManager.Actions[AActionName]);
end;

procedure AddEditorDropDownButton(AManager: IEditorManager; AParent: TToolBar;
  const AActionName: string; APopupMenu: TPopupMenu);
begin
  AddDropDownButton(AParent, AManager.Actions[AActionName], APopupMenu);
end;

procedure AddStandardEditorToolbarButtons(AManager: IEditorManager;
  AToolBar: TToolbar);
var
  Menus: IEditorMenus;

  procedure AddButton(const AActionName: string);
  begin
    AddEditorButton(AManager, AToolBar, AActionName);
  end;

  procedure AddDropDownButton(const AActionName: string; APopupMenu: TPopupMenu);
  begin
    AddEditorDropDownButton(AManager, AToolBar, AActionName, APopupMenu);
  end;

begin
  Menus := AManager.Menus;
  AToolBar.DoubleBuffered := True;
  AToolBar.Images := AManager.Actions.ActionList.Images;
  AddButton('actNew');
  AddButton('actOpen');
  AddButton('actSave');
  AddButton('actSaveAs');
  AddButton('');
  AddButton('actReload');
  AddButton('');
  AddButton('actCut');
  AddButton('actCopy');
  AddButton('actPaste');
  AddButton('');
  AddButton('actUndo');
  AddButton('actRedo');
  AddButton('');
  AddButton('actSearch');
  AddButton('actSearchReplace');
  //AddButton('');
  //AddButton('actAlignSelection');
  //AddButton('actSortSelection');
  AddButton('');
  AddDropDownButton('actToggleFoldLevel', Menus.FoldPopupMenu);
  AddButton('');
  //AddButton('actFormat');
  //AddButton('actSyncEdit');
  //AddButton('');
  //AddButton('actQuoteLines');
  //AddButton('actDeQuoteLines');
  //AddButton('actQuoteSelection');
  //AddButton('actDeQuoteSelection');
  //AddButton('');
  AddDropDownButton('actToggleHighlighter', Menus.HighlighterPopupMenu);
  AddButton('');
  AddButton('actShowCodeShaper');
  AddButton('actShowCodeFilter');
  AddButton('actShowCharacterMap');
  AddButton('actShowPreview');
  AddButton('');
  AddButton('actAutoGuessHighlighter');
  AddButton('actSmartSelect');
  AddButton('actInspect');
  AddButton('actSettings');
  AddButton('');
  AddButton('actShowSpecialCharacters');
  AddButton('actMonitorChanges');
  AddButton('actShowViews');
  AddButton('actStayOnTop');
  AddButton('actSingleInstance');
  AddButton('');
  AddButton('actCreateDesktopLink');
  AddButton('actNewSharedView');
  AddButton('actAbout');
end;

procedure AddExperimentalEditorToolbarButtons(AManager: IEditorManager;
  AToolBar: TToolbar);
var
  Menus: IEditorMenus;

  procedure AddButton(const AActionName: string);
  begin
    AddEditorButton(AManager, AToolBar, AActionName);
  end;

  procedure AddDropDownButton(const AActionName: string; APopupMenu: TPopupMenu);
  begin
    AddEditorDropDownButton(AManager, AToolBar, AActionName, APopupMenu);
  end;

begin
  Menus := AManager.Menus;
  AToolBar.DoubleBuffered := True;
  AToolBar.Images := AManager.Actions.ActionList.Images;
  AddButton('actSortSelection');
  AddButton('actShowPreview');
  AddButton('actInspect');
  AddButton('actMonitorChanges');
  AddButton('actShowViews');
  AddButton('actSingleInstance');
  AddButton('actCreateDesktopLink');
  AddButton('actNewSharedView');
end;

procedure AddSelectionToolbarButtons(AManager: IEditorManager;
  AToolBar: TToolbar);
var
  Menus: IEditorMenus;

  procedure AddButton(const AActionName: string);
  begin
    AddEditorButton(AManager, AToolBar, AActionName);
  end;

  procedure AddDropDownButton(const AActionName: string; APopupMenu: TPopupMenu);
  begin
    AddEditorDropDownButton(AManager, AToolBar, AActionName, APopupMenu);
  end;

begin
  Menus := AManager.Menus;
  AToolBar.DoubleBuffered := True;
  AToolBar.Images := AManager.Actions.ActionList.Images;
  AddButton('actAlignSelection');
  AddButton('actSortSelection');
  AddButton('actFormat');
  AddButton('actSyncEdit');
  AddButton('actQuoteLines');
  AddButton('actDeQuoteLines');
  AddButton('actQuoteSelection');
  AddButton('actQuoteLinesAndDelimit');
  AddButton('actDeQuoteSelection');
  AddButton('actLowerCaseSelection');
  AddButton('actUpperCaseSelection');
  AddButton('actConvertTabsToSpacesInSelection');
  AddButton('actStripFirstChar');
  AddButton('actStripLastChar');
  AddButton('actStripComments');
  AddButton('actStripMarkup');
  AddButton('actMergeBlankLines');
  AddButton('actToggleBlockCommentSelection');
end;

end.

