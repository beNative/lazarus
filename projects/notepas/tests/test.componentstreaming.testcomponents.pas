unit Test.ComponentStreaming.TestComponents;

interface

uses
  Classes, Contnrs;

type
  TParentComponent = class;

  { TChildComponent }

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
    property Parent: TParentComponent read FParent write SetParent;

  published
    property TestString: string
      read FTestString write FTestString;
  end;

  TParentComponent = class(TComponent)
  private
    FChildren: TObjectList;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Children: TObjectList
      read FChildren;
  end;

implementation

{ TChildComponent }

destructor TChildComponent.Destroy;
begin
  Parent := nil;
  inherited;
end;

function TChildComponent.GetParentComponent: TComponent;
begin
  Result := FParent;
  //Result := inherited GetParentComponent;
end;

function TChildComponent.HasParent: Boolean;
begin
  //Result := inherited HasParent;
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

{ TParentComponent }

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

initialization
  RegisterClasses([TChildComponent, TParentComponent]);

end.
