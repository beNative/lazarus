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
  public
    constructor Create(
      AActions : IRichEditorActions
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    function CreateMainMenu(
      AOwner   : TComponent
    ): TMainMenu;

//
//  procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction = nil);
//
//procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string = '');


  end;

implementation

//procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction);
//var
//  MI: TMenuItem;
//begin
//  if not Assigned(AAction) then
//    AParent.AddSeparator
//  else
//  begin
//    MI := TMenuItem.Create(AParent.Owner);
//    MI.Action := AAction;
//    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
//      MI.RadioItem := True;
//    AParent.Add(MI);
//  end;
//end;
//
//procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string);
//begin
//  if AActionName = '' then
//    AddActionMenuItem(AParent, nil)
//  else
//    AddActionMenuItem(AParent, RichEditorActions[AActionName]);
//end;
//

{ TRichEditorMenusFactory }

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

