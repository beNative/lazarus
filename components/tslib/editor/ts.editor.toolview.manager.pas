{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.ToolView.Manager;

{$MODE Delphi}

interface

{ TToolViewManager allows for lazy instantiation of registered toolviews. }

uses
  Classes, SysUtils, Contnrs, Forms,

  ts.Editor.Interfaces, ts.Editor.Tools.Settings,

  ts_Editor_ToolView_Base;

type

  { TToolView }

  TToolView = class(TInterfacedObject, IEditorToolView)
  strict private
    FName          : string;
    FFormClass     : TComponentClass;
    FForm          : TForm;
    FManager       : IEditorManager;
    FSettingsClass : TComponentClass;
    FToolView      : IEditorToolView;

  strict protected
    function GetForm: TForm;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    procedure SetFocus;
    function Focused: Boolean;
    function GetName: string;

  public
    constructor Create(
            AManager       : IEditorManager;
            AFormClass     : TComponentClass;
            ASettingsClass : TComponentClass;
      const AName          : string
    );
    procedure BeforeDestruction; override;

    property Name: string
      read GetName;

    { Lets the view respond to changes. }
    procedure UpdateView;

    property Form: TForm
      read GetForm;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property SettingsClass: TComponentClass
      read FSettingsClass write FSettingsClass;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

  { TToolViews }

  TToolViews = class(TInterfacedObject, IEditorToolViews)
  strict private
    FItems   : TInterfaceList;
    FManager : IEditorManager;

  strict protected
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
            AFormClass     : TComponentClass;
            ASettingsClass : TComponentClass;
      const AName          : string = ''
    ): Boolean;

    procedure Hide;

    property Views[AIndex: Integer]: IEditorToolView
      read GetView;

    property ViewByName[AName: string]: IEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;

  public
    constructor Create(AEditorManager: IEditorManager);
    procedure BeforeDestruction; override;

  end;

implementation

uses
  StrUtils,

  ts.Core.SharedLogger;

{ TToolView }

{$region 'construction and destruction' /fold}
constructor TToolView.Create(AManager: IEditorManager;
  AFormClass: TComponentClass; ASettingsClass: TComponentClass;
  const AName: string);
begin
  inherited Create;
  FManager       := AManager;
  FFormClass     := AFormClass;
  FSettingsClass := ASettingsClass;
  FName          := AName;

  if Assigned(ASettingsClass) then
    FManager.Settings.ToolSettings.RegisterSettings(
      ASettingsClass,
      ASettingsClass.ClassName
    );
end;

procedure TToolView.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TToolView.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as TForm;
    Logger.Send('Created ' + FForm.Name);
  end;
  Result := FForm;
end;

function TToolView.GetVisible: Boolean;
begin
  Result := Assigned(FForm) and FForm.Visible;
end;

procedure TToolView.SetVisible(AValue: Boolean);
begin
  if AValue <> Visible then
  begin
    if not AValue and Assigned(FForm) then
    begin
      FForm.Visible := False
    end
    else
      Form.Visible := AValue;
  end;
end;

procedure TToolView.SetFocus;
begin
  if Assigned(FForm) and FForm.CanFocus then
    FForm.SetFocus;
end;

function TToolView.GetName: string;
begin
  Result := FName;
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TToolView.UpdateView;
begin
  if Assigned(FToolView) then
    FToolView.UpdateView;
end;

function TToolView.Focused: Boolean;
begin
  Result := Assigned(FForm) and FForm.Focused;
end;
{$endregion}

{ TToolViews }

{$region 'construction and destruction' /fold}
constructor TToolViews.Create(AEditorManager: IEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems   := TInterfaceList.Create;
end;

procedure TToolViews.BeforeDestruction;
begin
  FManager := nil;
  FItems.Free;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TToolViews.GetView(AIndex: Integer): IEditorToolView;
begin
  Result := FItems[AIndex] as IEditorToolView;
end;

function TToolViews.GetViewByName(AName: string): IEditorToolView;
var
  TV : IEditorToolView;
  I  : Integer;
begin
  I := 0;
  Result := nil;
  while (I < FItems.Count) and not Assigned(Result) do
  begin
    TV := Views[I];
    if TV.Name = AName then
      Result := TV;
    Inc(I);
  end;
  if not Assigned(Result) then
    raise Exception.CreateFmt('ToolView (%s) not found!', [AName]);
end;

function TToolViews.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TToolViews.GetEnumerator: TEditorToolViewListEnumerator;
begin
  Result := TEditorToolViewListEnumerator.Create(Self);
end;
{$endregion}

{$region 'protected methods' /fold}
function TToolViews.Register(AFormClass: TComponentClass;
  ASettingsClass: TComponentClass; const AName: string): Boolean;
var
  S  : string;
  TV : IEditorToolView;
begin
  S  := IfThen(AName = '', AFormClass.ClassName, AName);
  TV := TToolView.Create(FManager, AFormClass, ASettingsClass, S);
  FItems.Add(TV);
  Result := True;
end;

procedure TToolViews.Hide;
var
  TV: IEditorToolView;
begin
  for TV in (Self as IEditorToolViews) do
  begin
    TV.Visible := False;
    FManager.Events.DoHideToolView(TV);
  end;
end;
{$endregion}

end.

