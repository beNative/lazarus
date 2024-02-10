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

unit ts.HtmlEditor.Factories;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, Menus, ComCtrls,

  ts.HtmlEditor.Interfaces;

type
  THtmlEditorFactories = class
  public
    class function CreateManager(
      AOwner : TComponent
     ): IHtmlEditorManager;

    class function CreateView(
      AParent         : TWinControl;
      AManager        : IHtmlEditorManager = nil;
      const AName     : string = '';
      const AFileName : string = ''
    ): IHtmlEditorView;

    class function CreateMainMenu(
      AOwner   : TComponent;
      AActions : IHtmlEditorActions
    ): TMainMenu;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IHtmlEditorActions
    ): TToolbar;

    class function CreateBrowserToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IHtmlEditorActions
    ): TToolbar;

  end;

implementation

uses
  ts.HtmlEditor.Manager, ts.HtmlEditor.Factories.Toolbars,
  ts.HtmlEditor.Factories.Menus;

class function THtmlEditorFactories.CreateManager(AOwner: TComponent
  ): IHtmlEditorManager;
begin
  Result := TdmHtmlEditorManager.Create(AOwner);
end;

class function THtmlEditorFactories.CreateView(AParent: TWinControl;
  AManager: IHtmlEditorManager; const AName: string; const AFileName: string
  ): IHtmlEditorView;
var
  V : IHtmlEditorView;
begin
  V := AManager.AddView(AName);
  V.BeginUpdate;
  V.Form.BorderStyle := bsNone;
  V.Form.Align       := alClient;
  V.Form.Parent      := AParent;
  V.PopupMenu        := AManager.EditorPopupMenu;
  V.Form.Visible     := True;
  V.EndUpdate;
  if AFileName <> '' then
    V.LoadFromFile(AFileName);
  Result := V;
end;

class function THtmlEditorFactories.CreateMainMenu(AOwner: TComponent;
  AActions: IHtmlEditorActions): TMainMenu;
var
  HEMF : IHtmlEditorMenusFactory;
begin
  HEMF   := THtmlEditorMenusFactory.Create(AActions);
  Result := HEMF.CreateMainMenu(AOwner);
end;

class function THtmlEditorFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IHtmlEditorActions): TToolbar;
var
  HETF : IHtmlEditorToolbarsFactory;
begin
  HETF   := THtmlEditorToolbarsFactory.Create(AActions);
  Result := HETF.CreateMainToolbar(AOwner, AParent);
end;

class function THtmlEditorFactories.CreateBrowserToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IHtmlEditorActions): TToolbar;
var
  HETF : IHtmlEditorToolbarsFactory;
begin
  HETF   := THtmlEditorToolbarsFactory.Create(AActions);
  Result := HETF.CreateBrowserToolbar(AOwner, AParent);
end;

end.

