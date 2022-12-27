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
     ): IRichEditorManager;

    class function CreateView(
      AParent         : TWinControl;
      AManager        : IRichEditorManager = nil;
      const AName     : string = '';
      const AFileName : string = ''
    ): IRichEditorView;

    class function CreateMainMenu(
      AOwner   : TComponent;
      AActions : IRichEditorActions
    ): TMainMenu;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : IRichEditorActions
    ): TToolbar;

  end;

implementation

uses
  ts.RichEditor.Manager, ts.RichEditor.Factories.Toolbars,
  ts.RichEditor.Factories.Menus;

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
  V : IRichEditorView;
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

class function TRichEditorFactories.CreateMainMenu(AOwner: TComponent;
  AActions: IRichEditorActions): TMainMenu;
var
  REMF : IRichEditorMenusFactory;
begin
  REMF   := TRichEditorMenusFactory.Create(AActions);
  Result := REMF.CreateMainMenu(AOwner);
end;

class function TRichEditorFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: IRichEditorActions): TToolbar;
var
  RETF : IRichEditorToolbarsFactory;
begin
  RETF   := TRichEditorToolbarsFactory.Create(AActions);
  Result := RETF.CreateMainToolbar(AOwner, AParent);
end;
{$ENDREGION}

end.

