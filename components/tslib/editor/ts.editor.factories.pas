{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

{
  TODO:
    factory for statusbar with customizable sections
    factory for custom toolbar, popup menus, etc.
}

unit ts.Editor.Factories;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, Menus, ComCtrls,

  ts.Editor.Interfaces;

type
  TEditorFactories = class
  public
    class function CreateSettings(
      AOwner          : TComponent;
      const AFileName : string = ''
    ): IEditorSettings;

    class function CreateManager(
      AOwner    : TComponent;
      ASettings : IEditorSettings = nil
     ): IEditorManager;

    class function CreateView(
      AParent            : TWinControl;
      AManager           : IEditorManager = nil;
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = 'TXT'
    ): IEditorView;

    class function CreateMainMenu(
      AOwner   : TComponent;
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    ): TMainMenu;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    ): TToolbar;

    class function CreateSelectionToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    ): TToolbar;

    class function CreateTopRightToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    ): TToolbar;
  end;

implementation

uses
  Forms,

  ts.Editor.Factories.Settings, ts.Editor.Factories.Manager,
  ts.Editor.Factories.Views, ts.Editor.Factories.Menus,
  ts.Editor.Factories.Toolbars;

{$REGION 'public methods'}
class function TEditorFactories.CreateManager(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
var
  EMF : IEditorManagerFactory;
begin
  EMF := TEditorManagerFactory.Create;
  if not Assigned(ASettings) then
    ASettings := CreateSettings(AOwner);
  Result := EMF.CreateInstance(AOwner, ASettings);
end;

class function TEditorFactories.CreateSettings(AOwner: TComponent;
  const AFileName: string): IEditorSettings;
var
  ESF : IEditorSettingsFactory;
begin
  ESF := TEditorSettingsFactory.Create;
  Result := ESF.CreateInstance(AOwner, AFileName);
end;

class function TEditorFactories.CreateView(AParent: TWinControl;
  AManager: IEditorManager; const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  EVF : IEditorViewFactory;
begin
  EVF := TEditorViewFactory.Create;
  if not Assigned(AManager) then
    AManager := CreateManager(Application);
  Result := EVF.CreateInstance(AParent, AManager, AName, AFileName, AHighlighter);
end;

class function TEditorFactories.CreateMainMenu(AOwner: TComponent;
  AActions: IEditorActions; AMenus: IEditorMenus): TMainMenu;
var
  EMF : IEditorMenusFactory;
begin
  EMF := TEditorMenusFactory.Create(AActions, AMenus);
  Result := EMF.CreateMainMenu(AOwner);
end;

class function TEditorFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IEditorActions;
  AMenus: IEditorMenus): TToolbar;
var
  TBF : IEditorToolbarsFactory;
begin
  TBF := TEditorToolbarsFactory.Create(AActions, AMenus);
  Result := TBF.CreateMainToolbar(AOwner, AParent);
end;

class function TEditorFactories.CreateSelectionToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IEditorActions;
  AMenus: IEditorMenus): TToolbar;
var
  TBF : IEditorToolbarsFactory;
begin
  TBF := TEditorToolbarsFactory.Create(AActions, AMenus);
  Result := TBF.CreateSelectionToolbar(AOwner, AParent);
end;

class function TEditorFactories.CreateTopRightToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IEditorActions;
  AMenus: IEditorMenus): TToolbar;
var
  TBF : IEditorToolbarsFactory;
begin
  TBF := TEditorToolbarsFactory.Create(AActions, AMenus);
  Result := TBF.CreateRightToolbar(AOwner, AParent);
end;
{$ENDREGION}

end.

