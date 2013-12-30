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

unit ts.Editor.Factories;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Controls, Menus,

  ts.Editor.Interfaces;

type

  { TEditorFactories }

  TEditorFactories = class
    class function CreateSettings(
      AOwner : TComponent
    ): IEditorSettings;

    class function CreateManager(
      AOwner    : TComponent;
      ASettings : IEditorSettings
     ): IEditorManager;

    class function CreateView(
            AParent       : TWinControl;
            AManager      : IEditorManager;
      const AName         : string = '';
      const AFileName     : string = '';
      const AHighlighter  : string = 'TXT'
    ): IEditorView;

    class function CreateMainMenu(
      AOwner   : TComponent;
      AActions : IEditorActions;
      AMenus   : IEditorMenus
    ): TMainMenu;
  end;

implementation

uses
  ts.Editor.Factories.Settings, ts.Editor.Factories.Manager,
  ts.Editor.Factories.Views, ts.Editor.Factories.Menus;

{ TEditorFactories }

class function TEditorFactories.CreateManager(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
var
  EMF : IEditorManagerFactory;
begin
  EMF := TEditorManagerFactory.Create;
  Result := EMF.CreateInstance(AOwner, ASettings);
end;

class function TEditorFactories.CreateSettings(
  AOwner: TComponent): IEditorSettings;
var
  ESF : IEditorSettingsFactory;
begin
  ESF := TEditorSettingsFactory.Create;
  Result := ESF.CreateInstance(AOwner);
end;

class function TEditorFactories.CreateView(AParent: TWinControl;
  AManager: IEditorManager; const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  EVF : IEditorViewFactory;
begin
  EVF := TEditorViewFactory.Create;
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

end.

