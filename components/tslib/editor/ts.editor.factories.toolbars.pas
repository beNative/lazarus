{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Factories.Toolbars;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Menus, Controls, ComCtrls, Toolwin,

  ts.Editor.Interfaces, ts.Editor.Resources;

const
  DEFAULT_EDGE_BORDERS = [ebLeft, ebTop, ebRight, ebBottom];
  DEFAULT_EDGE_INNER   = esNone;
  DEFAULT_EDGE_OUTER   = esNone;
  DEFAULT_TRANSPARANT  = True;

type

  { TEditorToolbarsFactory }

  TEditorToolbarsFactory = class(TInterfacedObject, IEditorToolbarsFactory)
  private
    FActions : IEditorActions;
    FMenus   : IEditorMenus;

    FEdgeBorders : TEdgeBorders;
    FEdgeInner   : TEdgeStyle;
    FEdgeOuter   : TEdgeStyle;
    FTransparant : Boolean;

    procedure ApplyDefaultProperties(
      AToolbar : TToolbar
    );

    function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    function CreateToolButton(
      AParent           : TToolBar;
      const AActionName : string = '';
      APopupMenu        : TPopupMenu = nil
    ): TToolButton; overload;

  public
    procedure AfterConstruction; override;

    constructor Create(
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    );

    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateSelectionToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateRightToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    property EdgeBorders: TEdgeBorders
      read FEdgeBorders write FEdgeBorders default DEFAULT_EDGE_BORDERS;

    property EdgeInner: TEdgeStyle
      read FEdgeInner write FEdgeInner default DEFAULT_EDGE_INNER;

    property EdgeOuter: TEdgeStyle
      read FEdgeOuter write FEdgeOuter default DEFAULT_EDGE_OUTER;

    property Transparant: Boolean
      read FTransparant write FTransparant default DEFAULT_TRANSPARANT;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TEditorToolbarsFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  FEdgeBorders := DEFAULT_EDGE_BORDERS;
  FEdgeInner   := DEFAULT_EDGE_INNER;
  FEdgeOuter   := DEFAULT_EDGE_OUTER;
  FTransparant := DEFAULT_TRANSPARANT;
end;

constructor TEditorToolbarsFactory.Create(AActions: IEditorActions;
  AMenus: IEditorMenus);
begin
  inherited Create;
  FActions := AActions;
  FMenus   := AMenus;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TEditorToolbarsFactory.ApplyDefaultProperties(AToolbar: TToolbar);
begin
  AToolbar.EdgeBorders  := EdgeBorders;
  AToolbar.EdgeInner    := EdgeInner;
  AToolbar.EdgeOuter    := EdgeOuter;
  AToolbar.Transparent  := Transparant;
  AToolbar.List         := True;
  AToolbar.ShowCaptions := False;
end;

function TEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB : TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.ShowCaption := False;
  TB.Parent      := AParent;
  if not Assigned(AAction) then
  begin
    TB.Style := tbsDivider;
  end
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
{$ENDREGION}

{$REGION 'public methods'}
function TEditorToolbarsFactory.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.Parent      := AParent;
  ApplyDefaultProperties(TB);
  TB.Images      := FActions.ActionList.Images;

  CreateToolButton(TB, 'actNew');
  CreateToolButton(TB, 'actOpen');
  CreateToolButton(TB, 'actSave');
  CreateToolButton(TB, 'actSaveAs');
  CreateToolButton(TB, 'actSaveAll');
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
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleFoldLevel', FMenus.FoldPopupMenu);
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleHighlighter', FMenus.HighlighterPopupMenu);
  CreateToolButton(TB, 'actAutoGuessHighlighter');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actShowCodeShaper');
  CreateToolButton(TB, 'actShowCodeFilter');
  CreateToolButton(TB, 'actShowCharacterMap');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSmartSelect');
  CreateToolButton(TB, 'actFormat');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSettings');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actShowSpecialCharacters');
  CreateToolButton(TB, 'actNewSharedView');
  CreateToolButton(TB, 'actMonitorChanges');
  CreateToolButton(TB, 'actSortSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCreateDesktopLink');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actAbout');
  Result := TB;
end;

function TEditorToolbarsFactory.CreateRightToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  ApplyDefaultProperties(TB);
  TB.Images := FActions.ActionList.Images;

  CreateToolButton(TB, 'actShowViews');
  CreateToolButton(TB, 'actSingleInstance');
  CreateToolButton(TB, 'actStayOnTop');
  CreateToolButton(TB, 'actClose');

  Result := TB;
end;

function TEditorToolbarsFactory.CreateSelectionToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  ApplyDefaultProperties(TB);
  TB.Images := FActions.ActionList.Images;

  CreateToolButton(TB, 'actAlignSelection');
  CreateToolButton(TB, 'actSortSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actIndent');
  CreateToolButton(TB, 'actUnindent');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actMergeBlankLines');
  CreateToolButton(TB, 'actCompressSpace');
  CreateToolButton(TB, 'actCompressWhitespace');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actFormat');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSyncEdit');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actQuoteLines');
  CreateToolButton(TB, 'actDeQuoteLines');
  CreateToolButton(TB, 'actQuoteSelection');
  CreateToolButton(TB, 'actDeQuoteSelection');
  CreateToolButton(TB, 'actQuoteLinesAndDelimit');
  CreateToolButton(TB, 'actPascalStringOfSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actLowerCaseSelection');
  CreateToolButton(TB, 'actUpperCaseSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actConvertTabsToSpaces');
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
{$ENDREGION}

end.
