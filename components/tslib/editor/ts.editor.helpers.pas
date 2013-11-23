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

  ts.Editor.Interfaces, ts.Editor.Settings, ts_Editor_Resources;

procedure AddActionButton(
  AParent : TToolBar;
  AAction : TBasicAction = nil
);

function AddActionMenuItem(
  AParent : TMenuItem;
  AAction:  TBasicAction = nil
): TMenuItem;

function AddEditorMenuItem(
        AManager    : IEditorManager;
        AParent     : TMenuItem;
  const AActionName : string = ''
): TMenuItem;

procedure AddEditorButton(
        AManager    : IEditorManager;
        AParent     : TToolBar;
  const AActionName : string = ''
);

procedure AddStandardEditorToolbarButtons(
  AManager : IEditorManager;
  AToolBar : TToolbar
);

procedure AddStandardEditorMenus(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);

function CreateEditorSettings(
  AOwner : TComponent = nil
): IEditorSettings;

function CreateEditorManager(
        AOwner            : TComponent = nil;
        APersistSettings  : Boolean = False;
  const ASettingsFileName : string = ''
): IEditorManager; overload;


{ TS: new version }
function CreateEditorManager(
  AOwner    : TComponent;
  ASettings : IEditorSettings
): IEditorManager; overload;

{ TODO: set highlightertype }
{ TODO: add list with available highlighterttypes }

function CreateEditorView(
         AParent       : TWinControl;
         AManager      : IEditorManager;
   const AName         : string = '';
   const AFileName     : string = '';
   const AHighlighter  : string = 'TXT'
): IEditorView; overload;

procedure AddEditorFileMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);

procedure AddEditorEditMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorSelectionMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorInsertMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorSearchMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorViewMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorToolsMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorSettingsMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorHighlightersMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorHelpMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
);
procedure AddEditorDebugMenu(
  AManager  : IEditorManager;
  AMainMenu : TMainMenu
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

function AddActionMenuItem(AParent: TMenuItem;
  AAction: TBasicAction): TMenuItem;
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Caption := cLineCaption;
    AParent.Add(MI);
  end
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
    begin
      // On Windows 7 or above with themes enabled, the menu item will not
      // be drawn as checked when it has a glyph assigned to it.
      // Qt does not have this problem.
      MI.GlyphShowMode := gsmNever;
      MI.RadioItem := True;
      {$IFDEF LCLGTK2}
      MI.RadioItem  := False;
      {$ENDIF}
    end;
    if (AAction is TAction) and (TAction(AAction).AutoCheck) then
    begin
      // Onn Windows 7 or above with themes enabled, the menu item will not
      // be drawn as checked when it has a glyph assigned to it.
      // Qt does not have this problem.
      MI.GlyphShowMode := gsmNever;
      MI.ShowAlwaysCheckable := True;
    end;
    AParent.Add(MI);
  end;
  Result := MI;
end;

function AddEditorMenuItem(AManager: IEditorManager; AParent: TMenuItem;
  const AActionName: string): TMenuItem;
begin
  if AActionName = '' then
    Result := AddActionMenuItem(AParent, nil)
  else
    Result := AddActionMenuItem(AParent, AManager.Actions[AActionName]);
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
  AddButton('');
  AddButton('actAlignSelection');
  AddButton('actSortSelection');
  AddButton('');
  AddDropDownButton('actToggleFoldLevel', Menus.FoldPopupMenu);
  AddButton('');
  AddButton('actFormat');
  AddButton('actSyncEdit');
  AddButton('');
  AddButton('actQuoteLines');
  AddButton('actDeQuoteLines');
  AddButton('actQuoteSelection');
  AddButton('actDeQuoteSelection');
  AddButton('');
  AddDropDownButton('actToggleHighlighter', Menus.HighlighterPopupMenu);
  AddButton('');
  AddButton('actShapeCode');
  AddButton('actFilterCode');
  AddButton('actInsertCharacterFromMap');
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

procedure AddEditorFileMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SFileMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actNew');
  AddEditorMenuItem(AManager, MI, 'actOpen');
  AddEditorMenuItem(AManager, MI, 'actSave');
  AddEditorMenuItem(AManager, MI, 'actSaveAs');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actReload');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actClose');
  AddEditorMenuItem(AManager, MI, 'actCloseOthers');
  AddEditorMenuItem(AManager, MI, 'actExit');
end;

procedure AddEditorEditMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Edit';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actUndo');
  AddEditorMenuItem(AManager, MI, 'actRedo');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actCut');
  AddEditorMenuItem(AManager, MI, 'actCopy');
  AddEditorMenuItem(AManager, MI, 'actPaste');
  AddEditorMenuItem(AManager, MI, 'actSelectAll');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actCopyFileName');
  AddEditorMenuItem(AManager, MI, 'actCopyFilePath');
  AddEditorMenuItem(AManager, MI, 'actCopyFullPath');
end;

procedure AddEditorSelectionMenu(AManager: IEditorManager; AMainMenu: TMainMenu
  );
var
  MI  : TMenuItem;
  SMI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSeLectionMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actAlignSelection');
  AddEditorMenuItem(AManager, MI, 'actSortSelection');
  AddEditorMenuItem(AManager, MI, 'actSyncEdit');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actLowerCaseSelection');
  AddEditorMenuItem(AManager, MI, 'actUpperCaseSelection');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actConvertTabsToSpacesInSelection');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actQuoteSelection');
  AddEditorMenuItem(AManager, MI, 'actDeQuoteSelection');
  AddEditorMenuItem(AManager, MI, 'actQuoteLines');
  AddEditorMenuItem(AManager, MI, 'actQuoteLinesAndDelimit');
  AddEditorMenuItem(AManager, MI, 'actDeQuoteLines');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actToggleBlockCommentSelection');
  AddEditorMenuItem(AManager, MI, 'actPascalStringOfSelection');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actStripFirstChar');
  AddEditorMenuItem(AManager, MI, 'actStripLastChar');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actStripMarkup');
  AddEditorMenuItem(AManager, MI);
  SMI := AddEditorMenuItem(AManager, MI, 'actSelectionEncodeMenu');
  AddEditorMenuItem(AManager, SMI, 'actEncodeBase64');
  AddEditorMenuItem(AManager, SMI, 'actEncodeURL');
  SMI := AddEditorMenuItem(AManager, MI, 'actSelectionDecodeMenu');
  AddEditorMenuItem(AManager, SMI, 'actDecodeBase64');
  AddEditorMenuItem(AManager, SMI, 'actDecodeURL');
end;

procedure AddEditorInsertMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SInsertMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actInsertCharacterFromMap');
  AddEditorMenuItem(AManager, MI, 'actInsertColorValue');
  AddEditorMenuItem(AManager, MI, 'actInsertGUID');
end;

procedure AddEditorSearchMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSearchMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actSearch');
  AddEditorMenuItem(AManager, MI, 'actFindAllOccurences');
  AddEditorMenuItem(AManager, MI, 'actSearchReplace');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actFindNext');
  AddEditorMenuItem(AManager, MI, 'actFindPrevious');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actFindNextWord');
  AddEditorMenuItem(AManager, MI, 'actFindPrevWord');
end;

procedure AddEditorViewMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SViewMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actShowViews');
  AddEditorMenuItem(AManager, MI, 'actShowActions');
  AddEditorMenuItem(AManager, MI, 'actShowPreview');
  AddEditorMenuItem(AManager, MI, 'actShowMiniMap');
  AddEditorMenuItem(AManager, MI, 'actShowHTMLViewer');
  AddEditorMenuItem(AManager, MI, 'actShowStructureViewer');
  AddEditorMenuItem(AManager, MI, 'actShowHexEditor');
  AddEditorMenuItem(AManager, MI, 'actShowScriptEditor');
end;

procedure AddEditorToolsMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SToolsMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actShapeCode');
  AddEditorMenuItem(AManager, MI, 'actFilterCode');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actSmartSelect');
  AddEditorMenuItem(AManager, MI, 'actFormat');
  AddEditorMenuItem(AManager, MI, 'actAutoGuessHighlighter');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actCreateDesktopLink');
  AddEditorMenuItem(AManager, MI, 'actMonitorChanges');
end;

procedure AddEditorSettingsMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SSettingsMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actSettings');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actShowSpecialCharacters');
  AddEditorMenuItem(AManager, MI, 'actIncFontSize');
  AddEditorMenuItem(AManager, MI, 'actDecFontSize');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actStayOnTop');
  AddEditorMenuItem(AManager, MI, 'actToggleMaximized');
  AddEditorMenuItem(AManager, MI);
  AddEditorMenuItem(AManager, MI, 'actSingleInstance');
end;

procedure AddEditorHighlightersMenu(AManager: IEditorManager;
  AMainMenu: TMainMenu);
var
  MI : TMenuItem;
  M  : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SHighlightersMenuCaption;
  AMainMenu.Items.Add(MI);
  for M in AManager.Menus.HighlighterPopupMenu.Items do
  begin
    AddEditorMenuItem(AManager, MI, M.Action.Name);
  end;
end;

procedure AddEditorHelpMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SHelpMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actAbout');
end;

procedure AddStandardEditorMenus(AManager: IEditorManager; AMainMenu: TMainMenu
  );
begin
  AMainMenu.Images := AManager.Actions.ActionList.Images;
  AddEditorFileMenu(AManager, AMainMenu);
  AddEditorEditMenu(AManager, AMainMenu);
  AddEditorSelectionMenu(AManager, AMainMenu);
  AddEditorSearchMenu(AManager, AMainMenu);
  AddEditorViewMenu(AManager, AMainMenu);
  AddEditorToolsMenu(AManager, AMainMenu);
  AddEditorSettingsMenu(AManager, AMainMenu);
  AddEditorHighlightersMenu(AManager, AMainMenu);
  AddEditorHelpMenu(AManager, AMainMenu);
end;

function CreateEditorSettings(AOwner : TComponent): IEditorSettings;
begin
  Result := TEditorSettings.Create(AOwner);
end;

function CreateEditorManager(AOwner: TComponent; APersistSettings: Boolean;
  const ASettingsFileName: string): IEditorManager;
var
  O : TComponent;
  S : string;
begin
  if not Assigned(AOwner) then
    O := Application
  else
    O := AOwner;
  Result := TdmEditorManager.Create(O, nil);
  Result.PersistSettings := APersistSettings;
  if APersistSettings then
  begin
    if ASettingsFileName = '' then
      S := ApplicationName + '.xml'
    else
      S := ASettingsFileName;
    Result.Settings.FileName := S;
  end;
end;

function CreateEditorManager(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
begin
  Result := TdmEditorManager.Create(AOwner, ASettings);
end;

procedure AddEditorDebugMenu(AManager: IEditorManager; AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := SDebugMenuCaption;
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(AManager, MI, 'actInspect');
end;

function CreateEditorView(AParent: TWinControl; AManager: IEditorManager;
  const AName: string; const AFileName: string; const AHighlighter: string
  ): IEditorView;
var
  V: IEditorView;
begin
  V := AManager.Views.Add(AName, AFileName, AHighlighter);
  V.Form.DisableAutoSizing;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  V.PopupMenu := AManager.Menus.EditorPopupMenu;
  V.Form.Visible := True;
  V.Form.EnableAutoSizing;
  Result := V;
end;

end.

