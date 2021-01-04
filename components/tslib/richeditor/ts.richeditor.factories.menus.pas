{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Factories.Menus;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Menus,

  ts.RichEditor.Interfaces;

type

  { TRichEditorMenusFactory }

  TRichEditorMenusFactory = class(TInterfacedObject, IRichEditorMenusFactory)
  private
    FActions : IRichEditorActions;

  protected
    function CreateMainMenu(
      AOwner : TComponent
    ): TMainMenu;

  public
    constructor Create(
      AActions : IRichEditorActions
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TRichEditorMenusFactory.Create(AActions: IRichEditorActions);
begin
  inherited Create;
  FActions := AActions;
end;

procedure TRichEditorMenusFactory.BeforeDestruction;
begin
  FActions := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TRichEditorMenusFactory.CreateMainMenu(AOwner: TComponent): TMainMenu;
var
  MM : TMainMenu;
begin
  MM := TMainMenu.Create(AOwner);
  MM.Images := FActions.Actions.Images;
  Result := MM;
end;
{$ENDREGION}

end.

