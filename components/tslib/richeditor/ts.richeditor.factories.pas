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

unit ts.RichEditor.Factories;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, Menus, ComCtrls,

  ts.RichEditor.Interfaces;

type

  { TRichEditorFactories }

  TRichEditorFactories = class
  public
    class function CreateManager(
      AOwner    : TComponent
   //  ASettings : IRichEditorSettings = nil
     ): IRichEditorManager;

    class function CreateView(
      AParent            : TWinControl;
      AManager           : IRichEditorManager = nil;
      const AName        : string = '';
      const AFileName    : string = ''
    ): IRichEditorView;

    class function CreateMainMenu(
      AOwner   : TComponent;
      AActions : IRichEditorActions
      //AMenus   : IRichEditorMenus
    ): TMainMenu;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IRichEditorActions
//      AMenus   : IRichEditorMenus
    ): TToolbar;

  end;

implementation

uses
  ts.RichEditor.Manager,

  ts.RichEditor.Factories.Toolbars, ts.RichEditor.Factories.Menus;

{$REGION 'public methods'}
class function TRichEditorFactories.CreateManager(AOwner: TComponent
  ): IRichEditorManager;
begin
  Result := TdmRichEditorManager.Create(AOwner);
end;

class function TRichEditorFactories.CreateView(AParent: TWinControl;
  AManager: IRichEditorManager; const AName: string; const AFileName: string
  ): IRichEditorView;
var
  V: IRichEditorView;
begin
  V := AManager.AddView(AName);
  V.BeginUpdate;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  V.PopupMenu := AManager.EditorPopupMenu;
  V.Form.Visible := True;
  V.EndUpdate;
  if AFileName <> '' then
    V.LoadFromFile(AFileName);
  Result := V;
end;

class function TRichEditorFactories.CreateMainMenu(AOwner: TComponent;
  AActions: IRichEditorActions): TMainMenu;
var
  REMF: IRichEditorMenusFactory;
begin
  REMF := TRichEditorMenusFactory.Create(AActions);
  Result := REMF.CreateMainMenu(AOwner);
end;

class function TRichEditorFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IRichEditorActions): TToolbar;
var
  RETF : IRichEditorToolbarsFactory;
begin
  RETF := TRichEditorToolbarsFactory.Create(AActions);
  Result := RETF.CreateMainToolbar(AOwner, AParent);
end;
{$ENDREGION}

end.

