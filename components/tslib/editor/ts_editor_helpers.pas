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

unit ts_Editor_Helpers;

{$mode delphi}

{ handy functions to build a simple editor. }

//*****************************************************************************

interface

uses
  Classes, SysUtils, ComCtrls, Menus, Controls, ActnList,

  ts_Editor_Interfaces, ts_Editor_Resources;

procedure AddActionButton(AParent: TToolBar; AAction: TBasicAction = nil);

procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction = nil);

procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string = '');

procedure AddEditorButton(AParent: TToolBar; const AActionName: string = '');

procedure AddStandardEditorToolbarButtons(AToolBar: TToolbar);

procedure AddStandardEditorMenus(AMainMenu : TMainMenu);

{ TODO: set highlightertype }
{ TODO: add list with available highlighterttypes }

function CreateEditorView(
         AParent       : TWinControl;
   const AName         : string = '';
   const AFileName     : string = '';
   const AHighlighter  : string = 'TXT'
): IEditorView; overload;

function CreateEditorView(
         AParent       : THandle;
   const AName         : string = '';
   const AFileName     : string = '';
   const AHighlighter  : string = 'TXT'
): IEditorView; overload;

function EditorViewByName(const AName: string): IEditorView;

procedure AddEditorFileMenu(AMainMenu: TMainMenu);
procedure AddEditorEditMenu(AMainMenu: TMainMenu);
procedure AddEditorSelectionMenu(AMainMenu: TMainMenu);
procedure AddEditorSearchMenu(AMainMenu: TMainMenu);
procedure AddEditorViewMenu(AMainMenu : TMainMenu);
procedure AddEditorToolsMenu(AMainMenu: TMainMenu);
procedure AddEditorSettingsMenu(AMainMenu : TMainMenu);
procedure AddEditorHighlightersMenu(AMainMenu: TMainMenu);
procedure AddEditorHelpMenu(AMainMenu : TMainMenu);
procedure AddEditorDebugMenu(AMainMenu : TMainMenu);

//*****************************************************************************

implementation

uses
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

procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction);
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
    AParent.AddSeparator
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
      MI.RadioItem := True;
    AParent.Add(MI);
  end;
end;

procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string);
begin
  if AActionName = '' then
    AddActionMenuItem(AParent, nil)
  else
    AddActionMenuItem(AParent, EditorManager.Actions[AActionName]);
end;

procedure AddEditorButton(AParent: TToolBar; const AActionName: string);
begin
  if AActionName = '' then
    AddActionButton(AParent)
  else
    AddActionButton(AParent, EditorManager.Actions[AActionName]);
end;

procedure AddEditorDropDownButton(AParent: TToolBar; const AActionName: string;
  APopupMenu: TPopupMenu);
begin
  AddDropDownButton(AParent, EditorManager.Actions[AActionName], APopupMenu);
end;

procedure AddStandardEditorToolbarButtons(AToolBar: TToolbar);
var
  Menus: IEditorMenus;

  procedure AddButton(const AActionName: string);
  begin
    AddEditorButton(AToolBar, AActionName);
  end;

  procedure AddDropDownButton(const AActionName: string; APopupMenu: TPopupMenu);
  begin
    AddEditorDropDownButton(AToolBar, AActionName, APopupMenu);
  end;

begin
  Menus := EditorManager as IEditorMenus;
  AToolBar.DoubleBuffered := True;
  AToolBar.Images := EditorManager.Actions.ActionList.Images;
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
  AddButton('');
  AddButton('actAlignSelection');
  AddButton('actSortSelection');
  AddButton('actAlignAndSortSelection');
  AddButton('actOpenSelectionInNewEditor');
  AddButton('');
  AddButton('actIncFontSize');
  AddButton('actDecFontSize');
  AddButton('');
  AddDropDownButton('actToggleFoldLevel', Menus.FoldPopupMenu);
  AddButton('');
  AddButton('actFormat');
  AddButton('actSyncEdit');
  AddButton('');
  AddButton('actToggleBlockCommentSelection');
  AddButton('actQuoteLines');
  AddButton('actDeQuoteLines');
  AddButton('actQuoteSelection');
  AddButton('actDeQuoteSelection');
  AddButton('');
  AddDropDownButton('actToggleHighlighter', Menus.HighlighterPopupMenu);
  AddButton('');
  AddButton('actShapeCode');
  AddButton('actFilterCode');
  AddButton('');
  AddButton('actShowPreview');
  AddButton('actAutoGuessHighlighter');
  AddButton('actSmartSelect');
  AddButton('actInspect');
  AddButton('actSettings');
  AddButton('');
  AddButton('actAbout');
  AddButton('actMonitorChanges');
  AddButton('actShowViews');
  AddButton('');
  AddButton('actCreateDesktopLink');
end;

procedure AddEditorFileMenu(AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SFileMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actNew');
  AddEditorMenuItem(MI, 'actOpen');
  AddEditorMenuItem(MI, 'actSave');
  AddEditorMenuItem(MI, 'actSaveAs');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actReload');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actClose');
  AddEditorMenuItem(MI, 'actCloseOthers');
  AddEditorMenuItem(MI, 'actExit');
end;

procedure AddEditorEditMenu(AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Edit';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actUndo');
  AddEditorMenuItem(MI, 'actRedo');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actCut');
  AddEditorMenuItem(MI, 'actCopy');
  AddEditorMenuItem(MI, 'actPaste');
  AddEditorMenuItem(MI, 'actSelectAll');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actCopyFileName');
  AddEditorMenuItem(MI, 'actCopyFilePath');
  AddEditorMenuItem(MI, 'actCopyFullPath');
end;

procedure AddEditorSelectionMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSeLectionMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actAlignSelection');
  AddEditorMenuItem(MI, 'actSortSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actLowerCaseSelection');
  AddEditorMenuItem(MI, 'actUpperCaseSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actOpenSelectionInNewEditor');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actQuoteSelection');
  AddEditorMenuItem(MI, 'actDeQuoteSelection');
  AddEditorMenuItem(MI, 'actQuoteLines');
  AddEditorMenuItem(MI, 'actQuoteLinesAndDelimit');
  AddEditorMenuItem(MI, 'actDeQuoteLines');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actToggleBlockCommentSelection');
  AddEditorMenuItem(MI, 'actPascalStringOfSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actStripFirstChar');
  AddEditorMenuItem(MI, 'actStripLastChar');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actStripMarkup');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actEncodeBase64');
  AddEditorMenuItem(MI, 'actDecodeBase64');
end;

procedure AddEditorSearchMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSearchMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actSearch');
  AddEditorMenuItem(MI, 'actSearchReplace');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actFindNext');
  AddEditorMenuItem(MI, 'actFindPrevious');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actFindNextWord');
  AddEditorMenuItem(MI, 'actFindPrevWord');
end;

procedure AddEditorViewMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SViewMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actShowViews');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actIncFontSize');
  AddEditorMenuItem(MI, 'actDecFontSize');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actShowControlCharacters');
  AddEditorMenuItem(MI, 'actShowPreview');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actAutoGuessHighlighter');
end;

procedure AddEditorToolsMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SToolsMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actInsertCharacterFromMap');
  AddEditorMenuItem(MI, 'actInsertColorValue');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actShapeCode');
  AddEditorMenuItem(MI, 'actFilterCode');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actSmartSelect');
  AddEditorMenuItem(MI, 'actFormat');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actMonitorChanges');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actCreateDesktopLink');
end;

procedure AddEditorSettingsMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSettingsMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actSettings');
  AddEditorMenuItem(MI, 'actStayOnTop');
  AddEditorMenuItem(MI, 'actToggleMaximized');
  AddEditorMenuItem(MI, 'actSingleInstance');
end;

procedure AddEditorHighlightersMenu(AMainMenu: TMainMenu);
var
  MI : TMenuItem;
  M  : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SHighlightersMenuCaption;
  AMainMenu.Items.Add(MI);
  for M in EditorManager.Menus.HighlighterPopupMenu.Items do
  begin
    AddEditorMenuItem(MI, M.Action.Name);
  end;
end;

procedure AddEditorHelpMenu(AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SHelpMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actAbout');
end;

procedure AddStandardEditorMenus(AMainMenu : TMainMenu);
begin
  AMainMenu.Images := EditorManager.Actions.ActionList.Images;
  AddEditorFileMenu(AMainMenu);
  AddEditorEditMenu(AMainMenu);
  AddEditorSelectionMenu(AMainMenu);
  AddEditorSearchMenu(AMainMenu);
  AddEditorViewMenu(AMainMenu);
  AddEditorToolsMenu(AMainMenu);
  AddEditorSettingsMenu(AMainMenu);
  AddEditorHighlightersMenu(AMainMenu);
  AddEditorHelpMenu(AMainMenu);
end;

procedure AddEditorDebugMenu(AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SDebugMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actInspect');
  AddEditorMenuItem(MI, 'actShowActions');
end;

function CreateEditorView(AParent: TWinControl; const AName: string;
  const AFileName: string; const AHighlighter: string): IEditorView;
var
  V: IEditorView;
begin
  V := (EditorManager as IEditorViews).Add(AName, AFileName, AHighlighter);
  V.Form.DisableAutoSizing;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  V.PopupMenu := (EditorManager as IEditorMenus).EditorPopupMenu;
  V.Form.Visible := True;
  V.Form.EnableAutoSizing;
  Result := V;
end;

function CreateEditorView(AParent: THandle; const AName: string; const AFileName: string; const AHighlighter: string): IEditorView;
var
  V: IEditorView;
begin
  V := (EditorManager as IEditorViews).Add(AName, AFileName, AHighlighter);
  V.Form.DisableAutoSizing;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.ParentWindow := AParent;
  V.PopupMenu := (EditorManager as IEditorMenus).EditorPopupMenu;
  V.Form.Visible := True;
  V.Form.EnableAutoSizing;
  Result := V;
end;

function EditorViewByName(const AName: string): IEditorView;
begin
  Result := (EditorManager as IEditorViews).ViewByName[AName];
end;

end.

