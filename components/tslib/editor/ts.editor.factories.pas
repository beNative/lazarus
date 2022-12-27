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
  AParent: TWinControl; AActions: IEditorActions; AMenus: IEditorMenus)
  : TToolbar;
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

