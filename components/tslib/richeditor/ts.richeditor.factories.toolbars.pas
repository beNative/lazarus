{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Factories.Toolbars;

{$MODE DELPHI}

interface

uses
  Classes, Controls, ComCtrls, Menus,

  ts.RichEditor.Interfaces;

type

  { TRichEditorToolbarsFactory }

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
    procedure BeforeDestruction; override;

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

procedure TRichEditorToolbarsFactory.BeforeDestruction;
begin
  FActions := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TRichEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
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
  TB.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  TB.EdgeInner   := esNone;
  TB.EdgeOuter   := esNone;
  TB.Transparent := True;
  TB.Parent      := AParent;
  TB.Images      := FActions.Actions.Images;
  TB.ButtonWidth := 10;

  CreateToolButton(TB, 'actSave');
  CreateToolButton(TB, 'actSaveAs');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCut');
  CreateToolButton(TB, 'actCopy');
  CreateToolButton(TB, 'actPaste');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actUndo');
  CreateToolButton(TB, 'actRedo');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actBold');
  CreateToolButton(TB, 'actItalic');
  CreateToolButton(TB, 'actUnderline');
  CreateToolButton(TB, 'actStrikeThrough');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actAlignLeft');
  CreateToolButton(TB, 'actAlignCenter');
  CreateToolButton(TB, 'actAlignRight');
  CreateToolButton(TB, 'actAlignJustify');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actFont');
  CreateToolButton(TB, 'actColor');
  CreateToolButton(TB, 'actBkColor');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actIncFontSize');
  CreateToolButton(TB, 'actDecFontSize');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleWordWrap');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actInsertHyperlink');
  CreateToolButton(TB, 'actInsertImage');
  Result := TB;
end;
{$ENDREGION}

end.

