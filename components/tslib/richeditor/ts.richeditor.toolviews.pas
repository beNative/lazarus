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

unit ts.RichEditor.ToolViews;

{$MODE DELPHI}

interface

{ TRichEditorToolViews allows for lazy instantiation of registered toolviews. }

uses
  Classes, SysUtils, Forms,

  ts.RichEditor.Interfaces{, ts.RichEditor.Tools.Settings};

type
  TRichEditorToolView = class(TInterfacedObject, IRichEditorToolView)
  private
    FName          : string;
    FFormClass     : TComponentClass;
    FForm          : TForm;
    FManager       : IRichEditorManager;
    FSettingsClass : TComponentClass;
    FToolView      : IRichEditorToolView;

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
      AManager       : IRichEditorManager;
      AFormClass     : TComponentClass;
      ASettingsClass : TComponentClass;
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

    property SettingsClass: TComponentClass
      read FSettingsClass write FSettingsClass;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

  TRichEditorToolViews = class(TInterfacedObject, IRichEditorToolViews)
  private
    FItems   : TInterfaceList;
    FManager : IRichEditorManager;

  protected
    {$REGION 'property access methods'}
    function GetView(AIndex: Integer): IRichEditorToolView;
    function GetViewByName(AName: string): IRichEditorToolView;
    function GetCount: Integer;
    {$ENDREGION}

    function GetEnumerator: TRichEditorToolViewListEnumerator;

    function Register(
      AFormClass     : TComponentClass;
      ASettingsClass : TComponentClass;
      const AName    : string = ''
    ): Boolean;

    procedure Hide;

    property Views[AIndex: Integer]: IRichEditorToolView
      read GetView;

    property ViewByName[AName: string]: IRichEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;

  public
    constructor Create(AEditorManager: IRichEditorManager);
    destructor Destroy; override;

  end;

implementation

uses
  StrUtils,

  ts.Core.Logger;

{$REGION 'TToolView'}
{$REGION 'construction and destruction'}
constructor TRichEditorToolView.Create(AManager: IRichEditorManager;
  AFormClass: TComponentClass; ASettingsClass: TComponentClass;
  const AName: string);
begin
  inherited Create;
  FManager       := AManager;
  FFormClass     := AFormClass;
  FSettingsClass := ASettingsClass;
  FName          := AName;

  //if Assigned(ASettingsClass) then
  //  FManager.
  //  Settings.ToolSettings.RegisterSettings(
  //    ASettingsClass,
  //    ASettingsClass.ClassName
  //  );
end;

procedure TRichEditorToolView.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TRichEditorToolView.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as TForm;
  end;
  Result := FForm;
end;

function TRichEditorToolView.GetVisible: Boolean;
begin
  Result := Assigned(FForm) and FForm.Visible;
end;

procedure TRichEditorToolView.SetVisible(AValue: Boolean);
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

procedure TRichEditorToolView.SetFocus;
begin
  if Assigned(FForm) and FForm.CanFocus then
    FForm.SetFocus;
end;

function TRichEditorToolView.GetName: string;
begin
  Result := FName;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TRichEditorToolView.UpdateView;
begin
  if Assigned(FToolView) then
    FToolView.UpdateView;
end;

function TRichEditorToolView.Focused: Boolean;
begin
  Result := Assigned(FForm) and FForm.Focused;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TToolViews'}
{$REGION 'construction and destruction'}
constructor TRichEditorToolViews.Create(AEditorManager: IRichEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems   := TInterfaceList.Create;
end;

destructor TRichEditorToolViews.Destroy;
begin
  FManager := nil;
  FItems.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TRichEditorToolViews.GetView(AIndex: Integer): IRichEditorToolView;
begin
  Result := FItems[AIndex] as IRichEditorToolView;
end;

function TRichEditorToolViews.GetViewByName(AName: string): IRichEditorToolView;
var
  TV : IRichEditorToolView;
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

function TRichEditorToolViews.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRichEditorToolViews.GetEnumerator: TRichEditorToolViewListEnumerator;
begin
  Result := TRichEditorToolViewListEnumerator.Create(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TRichEditorToolViews.Register(AFormClass: TComponentClass;
  ASettingsClass: TComponentClass; const AName: string): Boolean;
var
  S  : string;
  TV : IRichEditorToolView;
begin
  S  := IfThen(AName = '', AFormClass.ClassName, AName);
  TV := TRichEditorToolView.Create(FManager, AFormClass, ASettingsClass, S);
  FItems.Add(TV);
  Result := True;
end;

procedure TRichEditorToolViews.Hide;
var
  TV: IRichEditorToolView;
begin
  for TV in (Self as IRichEditorToolViews) do
  begin
    TV.Visible := False;
    FManager.Events.DoHideToolView(TV);
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

