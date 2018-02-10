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
      AOwner   : TComponent
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

