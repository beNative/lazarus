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

unit ts.Editor.Factories.Toolbars;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Menus, Controls, ActnList, ComCtrls,

  ts.Editor.Interfaces, ts_Editor_Resources;

type

  { TEditorToolbarsFactory }

  TEditorToolbarsFactory = class(TInterfacedObject, IEditorToolbarsFactory)
  strict private
    FActions : IEditorActions;
    FMenus   : IEditorMenus;

    function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    function CreateToolButton(
            AParent     : TToolBar;
      const AActionName : string = '';
            APopupMenu  : TPopupMenu = nil
    ): TToolButton; overload;

  public
    constructor Create(
      AActions  : IEditorActions;
      AMenus    : IEditorMenus
    );

    function CreateMainToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;

    function CreateSelectionToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;
  end;

implementation



{ TEditorToolbarsFactory }

function TEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  //TB := TToolButton.Create(AParent.Owner);
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
  if not Assigned(AAction) then
    TB.Style := tbsDivider
  else
  begin
    if Assigned(APopupMenu) then
    begin
      TB.Style        := tbsDropDown;
      TB.DropdownMenu := APopupMenu;
    end;
    TB.Action := AAction;
  end;
  Result := TB;
end;

function TEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  const AActionName: string; APopupMenu: TPopupMenu): TToolButton;
begin
  if AActionName = '' then
    Result := CreateToolButton(AParent, nil)
  else
    Result := CreateToolButton(AParent, FActions[AActionName], APopupMenu);
end;

constructor TEditorToolbarsFactory.Create(AActions: IEditorActions;
  AMenus: IEditorMenus);
begin
  inherited Create;
  FActions := AActions;
  FMenus   := AMenus;
end;

function TEditorToolbarsFactory.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  TB.Images := FActions.ActionList.Images;

  CreateToolButton(TB, 'actNew');
  CreateToolButton(TB, 'actOpen');
  CreateToolButton(TB, 'actSave');
  CreateToolButton(TB, 'actSaveAs');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actReload');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCut');
  CreateToolButton(TB, 'actCopy');
  CreateToolButton(TB, 'actPaste');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actUndo');
  CreateToolButton(TB, 'actRedo');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSearch');
  CreateToolButton(TB, 'actSearchReplace');
  //CreateToolButton('');
  //CreateToolButton('actAlignSelection');
  //CreateToolButton('actSortSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleFoldLevel', FMenus.FoldPopupMenu);
  CreateToolButton(TB);
  //CreateToolButton('actFormat');
  //CreateToolButton('actSyncEdit');
  //CreateToolButton('');
  //CreateToolButton('actQuoteLines');
  //CreateToolButton('actDeQuoteLines');
  //CreateToolButton('actQuoteSelection');
  //CreateToolButton('actDeQuoteSelection');
  //CreateToolButton('');
  CreateToolButton(TB, 'actToggleHighlighter', FMenus.HighlighterPopupMenu);
  CreateToolButton(TB);
  CreateToolButton(TB, 'actShowCodeShaper');
  CreateToolButton(TB, 'actShowCodeFilter');
  CreateToolButton(TB, 'actShowCharacterMap');
  CreateToolButton(TB, 'actShowPreview');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actAutoGuessHighlighter');
  CreateToolButton(TB, 'actSmartSelect');
  CreateToolButton(TB, 'actInspect');
  CreateToolButton(TB, 'actSettings');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actShowSpecialCharacters');
  CreateToolButton(TB, 'actMonitorChanges');
  CreateToolButton(TB, 'actShowViews');
  CreateToolButton(TB, 'actStayOnTop');
  CreateToolButton(TB, 'actSingleInstance');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCreateDesktopLink');
  CreateToolButton(TB, 'actNewSharedView');
  CreateToolButton(TB, 'actAbout');
  Result := TB;

  //AddButton('actSortSelection');
  //AddButton('actShowPreview');
  //AddButton('actInspect');
  //AddButton('actMonitorChanges');
  //AddButton('actShowViews');
  //AddButton('actSingleInstance');
  //AddButton('actCreateDesktopLink');
  //AddButton('actNewSharedView');


end;

function TEditorToolbarsFactory.CreateSelectionToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  TB.Images := FActions.ActionList.Images;

  CreateToolButton(TB, 'actAlignSelection');
  CreateToolButton(TB, 'actSortSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actIndent');
  CreateToolButton(TB, 'actUnindent');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actMergeBlankLines');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actFormat');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSyncEdit');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actQuoteLines');
  CreateToolButton(TB, 'actDeQuoteLines');
  CreateToolButton(TB, 'actQuoteSelection');
  //CreateToolButton(TB, 'actQuoteLinesAndDelimit');
  CreateToolButton(TB, 'actDeQuoteSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actLowerCaseSelection');
  CreateToolButton(TB, 'actUpperCaseSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actConvertTabsToSpacesInSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actStripFirstChar');
  CreateToolButton(TB, 'actStripLastChar');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actStripComments');
  CreateToolButton(TB, 'actStripMarkup');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleBlockCommentSelection');
  Result := TB;
end;

end.

