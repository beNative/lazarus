{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.HtmlEditor.Factories.Menus;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Menus,

  ts.HtmlEditor.Interfaces;

type
  THtmlEditorMenusFactory = class(TInterfacedObject, IHtmlEditorMenusFactory)
  private
    FActions : IHtmlEditorActions;

  protected
    function CreateMainMenu(
      AOwner : TComponent
    ): TMainMenu;

  public
    constructor Create(
      AActions : IHtmlEditorActions
    ); reintroduce; virtual;
    destructor Destroy; override;

  end;

implementation

{$REGION 'construction and destruction'}
constructor THtmlEditorMenusFactory.Create(AActions: IHtmlEditorActions);
begin
  inherited Create;
  FActions := AActions;
end;

destructor THtmlEditorMenusFactory.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function THtmlEditorMenusFactory.CreateMainMenu(AOwner: TComponent): TMainMenu;
var
  MM : TMainMenu;
begin
  MM := TMainMenu.Create(AOwner);
  MM.Images := FActions.ActionList.Images;
  Result := MM;
end;
{$ENDREGION}

end.

