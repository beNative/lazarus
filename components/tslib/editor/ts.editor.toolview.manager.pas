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

unit ts.Editor.Toolview.Manager;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Contnrs,

  ts.Editor.Interfaces;

type

  { TToolView }

  TToolView = class
  strict private
    FName      : string;
    FFormClass : TComponentClass;
    FInstance  : IEditorToolView;
    FManager   : IEditorManager;

    function GetInstance: IEditorToolView;
    function GetVisible: Boolean;

  public
    constructor Create(AManager: IEditorManager);
    procedure BeforeDestruction; override;

    property Name: string
      read FName write FName;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property Instance: IEditorToolView
      read GetInstance;

    property Visible: Boolean
      read GetVisible;
  end;

  { TToolViewManager }

  TToolViewManager = class
  strict private
    FItems   : TObjectList;
    FManager : IEditorManager;
    function GetCount: Integer;

  public
    function Register(
            AFormClass : TComponentClass;
      const AName      : string = ''
    ): Boolean;

    constructor Create(AEditorManager: IEditorManager);

    procedure BeforeDestruction; override;

    function FindByName(const AName: string): IEditorToolView;

    property Items: TObjectList
      read FItems;

    property Count: Integer
      read GetCount;
  end;

implementation

uses
  StrUtils, Forms;

{ TToolView }

{$region 'construction and destruction' /fold}
constructor TToolView.Create(AManager: IEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TToolView.BeforeDestruction;
begin
  FInstance := nil;
  FManager := nil;
  inherited BeforeDestruction;
end;
{$endregion}

function TToolView.GetInstance: IEditorToolView;
begin
  if not Assigned(FInstance) then
    FInstance := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as IEditorToolView;
  Result := FInstance;
end;

function TToolView.GetVisible: Boolean;
begin
  Result := Assigned(FInstance) and FInstance.Visible;
end;

{ TToolViewManager }

{$region 'construction and destruction' /fold}
constructor TToolViewManager.Create(AEditorManager: IEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems    := TObjectList.Create(True);
end;

procedure TToolViewManager.BeforeDestruction;
begin
  FManager := nil;
  FItems.Free;
  inherited BeforeDestruction;
end;
{$endregion}

function TToolViewManager.FindByName(const AName: string): IEditorToolView;
var
  TV : TToolView;
  I  : Integer;
begin
  I := 0;
  while (I < FItems.Count) and not Assigned(Result) do
  begin
    TV := TToolView(FItems[I]);
    if TV.Name = AName then
      Result := TV.Instance;
    Inc(I);
  end;
end;

function TToolViewManager.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TToolViewManager.Register(AFormClass: TComponentClass;
  const AName: string): Boolean;
var
  S : string;
  C : TToolView;
begin
  S := IfThen(AName = '', AFormClass.ClassName, AName);
  C := TToolView.Create(FManager);
  C.Name := S;
  C.FormClass := AFormClass;
  FItems.Add(C);
  Result := True;
end;

end.

