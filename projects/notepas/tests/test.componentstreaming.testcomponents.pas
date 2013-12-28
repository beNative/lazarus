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

{
unit TestComponentsReg;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  TestComponents;

type
  TParentComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  ColnEdit;

type
  TChildComponentCollectionItem = class(TCollectionItem)
  private
    FChildComponent: TChildComponent;
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    property ChildComponent: TChildComponent read FChildComponent write FChildComponent;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read GetName write SetName;
  end;

  TChildComponentCollection = class(TOwnedCollection)
  private
    FDesigner: IDesigner;
  public
    property Designer: IDesigner read FDesigner write FDesigner;
  end;

procedure Register;
begin
  RegisterClass(TChildComponent);
  RegisterNoIcon([TChildComponent]);
  RegisterComponents('Test', [TParentComponent]);
  RegisterComponentEditor(TParentComponent, TParentComponentEditor);
end;

{ TParentComponentEditor }

procedure TParentComponentEditor.ExecuteVerb(Index: Integer);
var
  LCollection: TChildComponentCollection;
  i: Integer;
begin
  LCollection := TChildComponentCollection.Create(Component, TChildComponentCollectionItem);
  LCollection.Designer := Designer;
  for i := 0 to TParentComponent(Component).Childs.Count - 1 do
    with TChildComponentCollectionItem.Create(nil) do
    begin
      ChildComponent := TChildComponent(TParentComponent(Component).Childs[i]);
      Collection := LCollection;
    end;
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component, LCollection, 'Childs');
end;

function TParentComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Childs...';
end;

function TParentComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TChildComponentCollectionItem }

constructor TChildComponentCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
  begin
    FChildComponent := TChildComponent.Create(TComponent(TOwnedCollection(Collection).Owner).Owner);
    FChildComponent.Name := TChildComponentCollection(Collection).Designer.UniqueName(TChildComponent.ClassName);
    FChildComponent.Parent := TParentComponent(TComponent(TOwnedCollection(Collection).Owner));
  end;
end;

destructor TChildComponentCollectionItem.Destroy;
begin
  FChildComponent.Free;
  inherited;
end;

function TChildComponentCollectionItem.GetDisplayName: string;
begin
  Result := FChildComponent.Name;
end;

function TChildComponentCollectionItem.GetName: string;
begin
  Result := FChildComponent.Name;
end;

procedure TChildComponentCollectionItem.SetName(const Value: string);
begin
  FChildComponent.Name := Value;
end;

end.
}
