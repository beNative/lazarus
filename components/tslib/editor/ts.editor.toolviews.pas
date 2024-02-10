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

unit ts.Editor.ToolViews;

{$MODE DELPHI}

interface

{ TEditorToolViews allows for lazy instantiation of registered toolviews. }

uses
  Classes, SysUtils, Forms,

  ts.Editor.Interfaces;

type
  TEditorToolView = class(TInterfacedObject, IEditorToolView)
  private
    FName          : string;
    FFormClass     : TComponentClass;
    FForm          : TForm;
    FManager       : IEditorManager;
    FSettingsClass : TPersistentClass;
    FToolView      : IEditorToolView;

  protected
    {$REGION 'property access methods'}
    function GetForm: TForm;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    {$ENDREGION}

    procedure SetFocus;
    function Focused: Boolean;
    function GetName: string;

  public
    constructor Create(
      AManager       : IEditorManager;
      AFormClass     : TComponentClass;
      ASettingsClass : TPersistentClass;
      const AName    : string
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

    property SettingsClass: TPersistentClass
      read FSettingsClass write FSettingsClass;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

  TEditorToolViews = class(TInterfacedObject, IEditorToolViews)
  private
    FItems   : TInterfaceList;
    FManager : IEditorManager;

  protected
    {$REGION 'property access methods'}
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;
    {$ENDREGION}

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
      AFormClass     : TComponentClass;
      ASettingsClass : TPersistentClass;
      const AName    : string = ''
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
    destructor Destroy; override;

  end;

implementation

uses
  StrUtils,

  ts.Core.Logger;

{$REGION 'TToolView'}
{$REGION 'construction and destruction'}
constructor TEditorToolView.Create(AManager: IEditorManager;
  AFormClass: TComponentClass; ASettingsClass: TPersistentClass;
  const AName: string);
begin
  inherited Create;
  FManager       := AManager;
  FFormClass     := AFormClass;
  FSettingsClass := ASettingsClass;
  FName          := AName;

  //if Assigned(ASettingsClass) then
  //  FManager.Settings.ToolSettings.RegisterSettings(
  //    ASettingsClass,
  //    ASettingsClass.ClassName
  //  );
end;

procedure TEditorToolView.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorToolView.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as TForm;
  end;
  Result := FForm;
end;

function TEditorToolView.GetVisible: Boolean;
begin
  Result := Assigned(FForm) and FForm.Visible;
end;

procedure TEditorToolView.SetVisible(AValue: Boolean);
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

procedure TEditorToolView.SetFocus;
begin
  if Assigned(FForm) and FForm.CanFocus then
    FForm.SetFocus;
end;

function TEditorToolView.GetName: string;
begin
  Result := FName;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TEditorToolView.UpdateView;
begin
  if Assigned(FToolView) then
    FToolView.UpdateView;
end;

function TEditorToolView.Focused: Boolean;
begin
  Result := Assigned(FForm) and FForm.Focused;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TToolViews'}
{$REGION 'construction and destruction'}
constructor TEditorToolViews.Create(AEditorManager: IEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems   := TInterfaceList.Create;
end;

destructor TEditorToolViews.Destroy;
begin
  FManager := nil;
  FItems.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorToolViews.GetView(AIndex: Integer): IEditorToolView;
begin
  Result := FItems[AIndex] as IEditorToolView;
end;

function TEditorToolViews.GetViewByName(AName: string): IEditorToolView;
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

function TEditorToolViews.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TEditorToolViews.GetEnumerator: TEditorToolViewListEnumerator;
begin
  Result := TEditorToolViewListEnumerator.Create(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TEditorToolViews.Register(AFormClass: TComponentClass;
  ASettingsClass: TPersistentClass; const AName: string): Boolean;
var
  S  : string;
  TV : IEditorToolView;
begin
  S  := IfThen(AName = '', AFormClass.ClassName, AName);
  TV := TEditorToolView.Create(FManager, AFormClass, ASettingsClass, S);
  FItems.Add(TV);
  Result := True;
end;

procedure TEditorToolViews.Hide;
var
  TV : IEditorToolView;
begin
  for TV in (Self as IEditorToolViews) do
  begin
    TV.Visible := False;
    FManager.Events.DoHideToolView(TV);
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

