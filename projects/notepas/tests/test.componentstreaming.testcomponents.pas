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
  inherited;
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
  inherited;
  FChildren := TObjectList.Create;
end;

destructor TParentComponent.Destroy;
begin
  FChildren.Free;
  inherited;
end;

procedure TParentComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    Proc(TComponent(FChildren[i]));
end;
{$ENDREGION}

initialization
  RegisterClasses([TChildComponent, TParentComponent]);

end.
