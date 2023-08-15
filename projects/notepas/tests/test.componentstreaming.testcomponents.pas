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

unit Test.ComponentStreaming.TestComponents;

interface

uses
  Classes, Contnrs;

type
  TParentComponent = class;

  TChildComponent = class(TComponent)
  private
    FParent     : TParentComponent;
    FTestString : string;

    procedure SetParent(const Value: TParentComponent);

  protected
    procedure SetParentComponent(AParent: TComponent); override;

  public
    destructor Destroy; override;

    function GetParentComponent: TComponent; override;

    function HasParent: Boolean; override;

    property Parent: TParentComponent
      read FParent write SetParent;

  published
    property TestString: string
      read FTestString write FTestString;
  end;

  TParentComponent = class(TComponent)
  private
    FChildren : TObjectList;

  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Children: TObjectList
      read FChildren write FChildren;
  end;

implementation

{$REGION 'TChildComponent'}
destructor TChildComponent.Destroy;
begin
  Parent := nil;
  inherited Destroy;
end;

function TChildComponent.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

function TChildComponent.HasParent: Boolean;
begin
  Result := Assigned(FParent);
end;

procedure TChildComponent.SetParent(const Value: TParentComponent);
begin
  if FParent <> Value then
  begin
    if Assigned(FParent) then
      FParent.FChildren.Remove(Self);
    FParent := Value;
    if Assigned(FParent) then
      FParent.FChildren.Add(Self);
  end;
end;

procedure TChildComponent.SetParentComponent(AParent: TComponent);
begin
  if AParent is TParentComponent then
    SetParent(AParent as TParentComponent);
end;
{$ENDREGION}

{$REGION 'TParentComponent'}
constructor TParentComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChildren := TObjectList.Create;
end;

destructor TParentComponent.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

procedure TParentComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I : Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    Proc(TComponent(FChildren[I]));
end;
{$ENDREGION}

initialization
  RegisterClasses([TChildComponent, TParentComponent]);

end.
