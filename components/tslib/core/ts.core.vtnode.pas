unit ts.Core.VTNode;

{$MODE DELPHI}

interface

uses
  VirtualTrees;

{
  Documentation
    TVTNode is a type designed to be used as the data structure where each
    treenode in a treeview is pointing to.

    For any treenode (of type PVirtualNode) this can be obtained by the following
    method defined in TBaseVirtualStringTree:
          function GetNodeData<T>(pNode: PVirtualNode): T;

    type
      TMyData = class
        ...
      end;
}
type
  { TVTNode }

  TVTNode<T> = class
  private
    FCheckState : TCheckState;
    FCheckType  : TCheckType;
    FData       : T;
    FHint       : string;
    FImageIndex : Integer;
    FOwnsObject : Boolean;
    FText       : string;
    FTree       : TCustomVirtualStringTree;
    FVNode      : PVirtualNode;

  private
    function SearchTree(ANode: TVTNode<T>; const AData: T): TVTNode<T>;

  protected
    {$REGION 'property access methods'}
    function GetCheckState: TCheckState;
    function GetCheckType: TCheckType;
    function GetChildCount: UInt32;
    function GetData: T;
    function GetHint: string;
    function GetImageIndex: Integer;
    function GetIndex: Integer;
    function GetItem(AIndex: UInt32): TVTNode<T>;
    function GetLevel: Integer;
    function GetOwnsObject: Boolean;
    function GetText: string; virtual;
    function GetTree: TCustomVirtualStringTree;
    function GetVisible: Boolean;
    function GetVNode: PVirtualNode;
    procedure SetCheckState(const Value: TCheckState);
    procedure SetCheckType(const Value: TCheckType);
    procedure SetData(const Value: T);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetOwnsObject(AValue: Boolean);
    procedure SetText(const Value: string); virtual;
    procedure SetVisible(const Value: Boolean);
    procedure SetVNode(const Value: PVirtualNode);
    {$ENDREGION}

  public
    constructor Create(
      ATree        : TCustomVirtualStringTree;
      const AData  : T;
      AOwnsObject  : Boolean = True;
      AParentVNode : PVirtualNode = nil;
      const AText  : string = ''
    ); overload; virtual;
    constructor Create(
      ATree        : TCustomVirtualStringTree;
      AOwnsObject  : Boolean = True;
      AParentVNode : PVirtualNode = nil;
      const AText  : string = ''
    ); overload; virtual;
    procedure BeforeDestruction; override;

    function Add(const AData: T; AOwnsObject: Boolean = True): TVTNode<T>;
    function Find(const AData: T): TVTNode<T>;

    { Points to the corresponding node of the virtual treeview. }
    property VNode: PVirtualNode
      read GetVNode write SetVNode;

    property ChildCount: UInt32
      read GetChildCount;

    { User defined data that is associated with the current node. }
    property Data: T
      read GetData write SetData;

    property CheckState: TCheckState
      read GetCheckState write SetCheckState;

    property CheckType: TCheckType
      read GetCheckType write SetCheckType;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Items[AIndex: UInt32]: TVTNode<T>
      read GetItem; default;

    property Index: Integer
      read GetIndex;

    property Level: Integer
      read GetLevel;

    { If Data is of a class type, this determines if Data is freed when TVTNode
    instance is freed. }
    property OwnsObject: Boolean
      read GetOwnsObject write SetOwnsObject;

    property Text: string
      read GetText write SetText;

    property Tree: TCustomVirtualStringTree
      read GetTree;

    property Hint: string
      read GetHint write SetHint;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

implementation

uses
  SysUtils,

  ts.Core.Logger;

{$REGION 'construction and destruction'}
constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree; const AData: T;
  AOwnsObject: Boolean; AParentVNode: PVirtualNode; const AText: string);
begin
  FTree       := ATree;
  FData       := AData;
  FOwnsObject := AOwnsObject;
  FText       := AText;
  if not Assigned(AParentVNode) then // create rootnode
    FVNode := FTree.AddChild(nil, Self);
end;

constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree;
  AOwnsObject: Boolean; AParentVNode: PVirtualNode; const AText: string);
begin
  FTree       := ATree;
  FData       := Default(T);
  FOwnsObject := AOwnsObject;
  FText       := AText;
  if not Assigned(AParentVNode) then // create rootnode
    FVNode := FTree.AddChild(nil, Self);
end;

procedure TVTNode<T>.BeforeDestruction;
begin
  if (GetTypekind(T) = tkClass) and OwnsObject then
    FreeAndNil(FData);
  FTree  := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TVTNode<T>.GetCheckState: TCheckState;
begin
  if Assigned(VNode) then
    FCheckState := VNode.CheckState;
  Result := FCheckState;
end;

procedure TVTNode<T>.SetCheckState(const Value: TCheckState);
begin
  FCheckState := Value;
  if Assigned(VNode) then
    VNode.CheckState := Value;
end;

function TVTNode<T>.GetCheckType: TCheckType;
begin
  if Assigned(VNode) then
    FCheckType := VNode.CheckType;
  Result := FCheckType;
end;

procedure TVTNode<T>.SetCheckType(const Value: TCheckType);
begin
  FCheckType := Value;
  if Assigned(VNode) then
    VNode.CheckType := Value;
end;

function TVTNode<T>.GetChildCount: UInt32;
begin
  if Assigned(VNode) then
    Result := VNode.ChildCount
  else
    Result := 0;
end;

function TVTNode<T>.GetData: T;
begin
  Result := FData;
end;

procedure TVTNode<T>.SetData(const Value: T);
begin
  FData := Value;
end;

function TVTNode<T>.GetHint: string;
begin
  Result := FHint;
end;

procedure TVTNode<T>.SetHint(const Value: string);
begin
  FHint := Value;
end;

function TVTNode<T>.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TVTNode<T>.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

function TVTNode<T>.GetIndex: Integer;
begin
  if Assigned(VNode) then
    Result := VNode.Index
  else
    Result := 0;
end;

function TVTNode<T>.GetOwnsObject: Boolean;
begin
  Result := FOwnsObject;
end;

procedure TVTNode<T>.SetOwnsObject(AValue: Boolean);
begin
  FOwnsObject := aValue;
end;

function TVTNode<T>.GetTree: TCustomVirtualStringTree;
begin
  Result := FTree;
end;

function TVTNode<T>.GetItem(AIndex: UInt32): TVTNode<T>;
var
  I	 : UInt32;
	VN : PVirtualNode;
begin
	VN := VNode.FirstChild;
  if AIndex > 0 then
  begin
    for I := 0 to AIndex - 1 do
    begin
      VN := VN.NextSibling;
    end;
  end;
  Result := TVTNode<T>(FTree.GetNodeData(VN)^);
end;

function TVTNode<T>.GetLevel: Integer;
begin
  if Assigned(FTree) and Assigned(FVNode) then
  begin
    Result := FTree.GetNodeLevel(VNode);
  end
  else
    Result := 0;
end;

function TVTNode<T>.GetText: string;
begin
  Result := FText;
end;

procedure TVTNode<T>.SetText(const Value: string);
begin
  FText := Value;
end;

function TVTNode<T>.GetVisible: Boolean;
begin
  if Assigned(FTree) and Assigned(FVNode) then
  begin
    Result := FTree.IsVisible[VNode];
  end
  else
    Result := False;
end;

procedure TVTNode<T>.SetVisible(const Value: Boolean);
begin
  if Assigned(FTree) and Assigned(FVNode) then
  begin
    FTree.IsVisible[VNode] := Value;
  end;
end;

function TVTNode<T>.GetVNode: PVirtualNode;
begin
  Result := FVNode;
end;

procedure TVTNode<T>.SetVNode(const Value: PVirtualNode);
begin
  if Value <> VNode then
  begin
    FVNode := Value;
    if Assigned(FVNode) then
    begin
      FVNode.CheckState := FCheckState;
      FVNode.CheckType  := FCheckType;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Search with recursion. }

function TVTNode<T>.SearchTree(ANode: TVTNode<T>;const AData: T): TVTNode<T>;
var
  I      : Integer;
  LFound : Boolean;
  LItem  : TVTNode<T>;
begin
  I := 0;
  LFound := False;
  while (I < ANode.ChildCount) and not LFound do
  begin
    LItem := ANode.Items[I];
    if LItem.Data = AData then
    begin
      Result := LItem;
      LFound := True;
    end
    else
    begin
      LItem  := SearchTree(LItem, AData);
      LFound := Assigned(LItem);
    end;
    Inc(I);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TVTNode<T>.Add(const AData: T; AOwnsObject: Boolean): TVTNode<T>;
var
  LVTNode : TVTNode<T>;
  LVNode  : PVirtualNode;
begin
  if Assigned(FTree) then
  begin
    Logger.Send('Assigned AData', Assigned(AData));
    Logger.Send('Assigned VNode', Assigned(VNode));
    if not Assigned(VNode) then // create root node if it does not exist
    begin
      VNode := FTree.AddChild(nil, Self);
    end;
    LVTNode := TVTNode<T>.Create(FTree, AData, AOwnsObject, VNode);
    LVNode := FTree.AddChild(VNode, LVTNode);
    LVTNode.VNode := LVNode;
    Result := LVTNode;
  end
  else
    Result := nil;
end;

function TVTNode<T>.Find(const AData: T): TVTNode<T>;
begin
  Result := SearchTree(Self, AData);
end;
{$ENDREGION}

end.

