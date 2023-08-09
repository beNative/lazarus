{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Factories.Toolbars;

{$MODE DELPHI}

interface

uses
  Classes, Controls, ComCtrls, Menus,

  ts.RichEditor.Interfaces;

type
  TRichEditorToolbarsFactory = class(TInterfacedObject,
    IRichEditorToolbarsFactory
  )
  private
    FActions : IRichEditorActions;

  protected
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
    constructor Create(
      AActions : IRichEditorActions
    ); reintroduce; virtual;
    destructor Destroy; override;

    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

implementation

uses
  Toolwin;

{$REGION 'construction and destruction'}
constructor TRichEditorToolbarsFactory.Create(AActions: IRichEditorActions);
begin
  inherited Create;
  FActions := AActions;
end;

destructor TRichEditorToolbarsFactory.Destroy;
begin
  FActions := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TRichEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent      := AParent;
  TB.ShowCaption := False;
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

function TRichEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  const AActionName: string; APopupMenu: TPopupMenu): TToolButton;
begin
  if AActionName = '' then
    Result := CreateToolButton(AParent, nil)
  else
    Result := CreateToolButton(AParent, FActions[AActionName], APopupMenu);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TRichEditorToolbarsFactory.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  TB.EdgeBorders  := [ebLeft, ebTop, ebRight, ebBottom];
  TB.EdgeInner    := esNone;
  TB.EdgeOuter    := esNone;
  TB.Transparent  := True;
  TB.List         := True;
  TB.ButtonWidth  := 24;
  TB.ShowCaptions := True;
  TB.ImagesWidth  := 16;
  TB.Wrapable     := False;
  TB.Parent       := AParent;
  TB.Images       := FActions.ActionList.Images;
  TB.Align        := alClient;

  CreateToolButton(TB, 'actSave');
  CreateToolButton(TB, 'actSaveAs');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCut');
  CreateToolButton(TB, 'actCopy');
  CreateToolButton(TB, 'actPaste');
  //CreateToolButton(TB);
  //CreateToolButton(TB, 'actUndo');
  //CreateToolButton(TB, 'actRedo');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actBold');
  CreateToolButton(TB, 'actItalic');
  CreateToolButton(TB, 'actUnderline');
  CreateToolButton(TB, 'actStrikeThrough');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSetFont');
  CreateToolButton(TB, 'actSetFontColor');
  CreateToolButton(TB, 'actSetBackgroundColor');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actAlignLeft');
  CreateToolButton(TB, 'actAlignCenter');
  CreateToolButton(TB, 'actAlignRight');
  //CreateToolButton(TB, 'actAlignJustify'); // not working yet
  //CreateToolButton(TB);
  //CreateToolButton(TB, 'actIncFontSize');
  //CreateToolButton(TB, 'actDecFontSize');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actIncIndent');
  CreateToolButton(TB, 'actDecIndent');
  CreateToolButton(TB, 'actBulletList');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleWordWrap');
  CreateToolButton(TB, 'actShowSpecialCharacters');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actEditParagraphStyle');
  CreateToolButton(TB, 'actEditTextStyle');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actInsertHyperlink');
  CreateToolButton(TB, 'actInsertImage');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actClear');
  CreateToolButton(TB, 'actShowPreview');
  CreateToolButton(TB, 'actShowStructureViewer');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSelectTable');
  CreateToolButton(TB, 'actInsertColumnBefore');
  CreateToolButton(TB, 'actInsertColumnAfter');
  CreateToolButton(TB, 'actInsertRowBefore');
  CreateToolButton(TB, 'actInsertRowAfter');
  CreateToolButton(TB, 'actDeleteRow');
  CreateToolButton(TB, 'actDeleteColumn');
  Result := TB;
end;
{$ENDREGION}

end.

