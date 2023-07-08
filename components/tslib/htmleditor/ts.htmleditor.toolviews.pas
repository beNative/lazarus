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

unit ts.HtmlEditor.ToolViews;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms,

  ts.HtmlEditor.Interfaces;

type
  THtmlEditorToolView = class(TInterfacedObject, IHtmlEditorToolView)
  private
    FName          : string;
    FFormClass     : TComponentClass;
    FForm          : TForm;
    FManager       : IHtmlEditorManager;
    FSettingsClass : TComponentClass;
    FToolView      : IHtmlEditorToolView;

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
      AManager       : IHtmlEditorManager;
      AFormClass     : TComponentClass;
      ASettingsClass : TComponentClass;
      const AName    : string
    );
    destructor Destroy; override;

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

  THtmlEditorToolViews = class(TInterfacedObject, IHtmlEditorToolViews)
  private
    FItems   : TInterfaceList;
    FManager : IHtmlEditorManager;

  protected
    {$REGION 'property access methods'}
    function GetView(AIndex: Integer): IHtmlEditorToolView;
    function GetViewByName(AName: string): IHtmlEditorToolView;
    function GetCount: Integer;
    {$ENDREGION}

    function GetEnumerator: THtmlEditorToolViewListEnumerator;

    function Register(
      AFormClass     : TComponentClass;
      ASettingsClass : TComponentClass;
      const AName    : string = ''
    ): Boolean;

    procedure Hide;

    property Views[AIndex: Integer]: IHtmlEditorToolView
      read GetView;

    property ViewByName[AName: string]: IHtmlEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;

  public
    constructor Create(AEditorManager: IHtmlEditorManager);
    destructor Destroy; override;

  end;

implementation

uses
  StrUtils;

function THtmlEditorToolView.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as TForm;
  end;
  Result := FForm;
end;

function THtmlEditorToolView.GetVisible: Boolean;
begin
  Result := Assigned(FForm) and FForm.Visible;
end;

procedure THtmlEditorToolView.SetVisible(AValue: Boolean);
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

procedure THtmlEditorToolView.SetFocus;
begin
  if Assigned(FForm) and FForm.CanFocus then
    FForm.SetFocus;
end;

function THtmlEditorToolView.Focused: Boolean;
begin
  Result := Assigned(FForm) and FForm.Focused;
end;

function THtmlEditorToolView.GetName: string;
begin
  Result := FName;
end;

constructor THtmlEditorToolView.Create(AManager: IHtmlEditorManager;
  AFormClass: TComponentClass; ASettingsClass: TComponentClass;
  const AName: string);
begin
  inherited Create;
  FManager       := AManager;
  FFormClass     := AFormClass;
  FSettingsClass := ASettingsClass;
  FName          := AName;
end;

destructor THtmlEditorToolView.Destroy;
begin
  FManager := nil;
  inherited Destroy;
end;

procedure THtmlEditorToolView.UpdateView;
begin
  if Assigned(FToolView) then
    FToolView.UpdateView;
end;

function THtmlEditorToolViews.GetView(AIndex: Integer): IHtmlEditorToolView;
begin
  Result := FItems[AIndex] as IHtmlEditorToolView;
end;

function THtmlEditorToolViews.GetViewByName(AName: string): IHtmlEditorToolView;
var
  TV : IHtmlEditorToolView;
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

function THtmlEditorToolViews.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function THtmlEditorToolViews.GetEnumerator: THtmlEditorToolViewListEnumerator;
begin
  Result := THtmlEditorToolViewListEnumerator.Create(Self);
end;

function THtmlEditorToolViews.Register(AFormClass: TComponentClass;
  ASettingsClass: TComponentClass; const AName: string): Boolean;
var
  S  : string;
  TV : IHtmlEditorToolView;
begin
  S  := IfThen(AName = '', AFormClass.ClassName, AName);
  TV := THtmlEditorToolView.Create(FManager, AFormClass, ASettingsClass, S);
  FItems.Add(TV);
  Result := True;
end;

procedure THtmlEditorToolViews.Hide;
var
  TV : IHtmlEditorToolView;
begin
  for TV in (Self as IHtmlEditorToolViews) do
  begin
    TV.Visible := False;
    FManager.Events.DoHideToolView(TV);
  end;
end;

constructor THtmlEditorToolViews.Create(AEditorManager: IHtmlEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems   := TInterfaceList.Create;
end;

destructor THtmlEditorToolViews.Destroy;
begin
  FManager := nil;
  FItems.Free;
  inherited Destroy;
end;

end.

