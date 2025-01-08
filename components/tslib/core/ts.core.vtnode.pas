{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.VTNode;

{$MODE DELPHI}

interface

uses
  VirtualTrees;

{$REGION 'documentation'}
{
    TVTNode is a type designed to be used as the data structure where each
    treenode in a treeview is pointing to.

    For any treenode (of type PVirtualNode) this can be obtained by the following
    method defined in TBaseVirtualStringTree:
          function GetNodeData<T>(pNode: PVirtualNode): T;

    type
      TMyData = class
        ...
      end;

    TODO: non generic version where visible node content is stored in Text and
          Hint properties.
}
{$ENDREGION}

type
  TVTNode<T> = class
  //public type
    //TVTNodeEnumerator<K: T> = record
    //strict private
    //  FCurrent : TVTNode<K>;
    //  FFirst   : Boolean;
    //public
    //  constructor Create(AVTNode: TVTNode<K>);
    //
    //  function GetCurrent: TVTNode<K>;
    //
    //  function MoveNext: Boolean;
    //
    //  property Current: TVTNode<K>
    //    read GetCurrent;
    //end;

  private
    FTree       : TCustomVirtualStringTree;
    FVNode      : PVirtualNode;
    FData       : T;
    FText       : string;
    FHint       : string;
    FImageIndex : Integer;
    FCheckState : TCheckState;
    FCheckType  : TCheckType;
    FOwnsObject : Boolean;

  private
    function SearchTree(ANode: TVTNode<T>; const AData: T): TVTNode<T>;
    function VTNodeFromVNode(const AVNode: PVirtualNode): TVTNode<T>;

  protected
    {$REGION 'property access methods'}
    function GetAlign: Byte;
    function GetCheckState: TCheckState;
    function GetCheckType: TCheckType;
    function GetChildCount: UInt32;
    function GetData: T;
    function GetExpanded: Boolean;
    function GetFirstChildData: T;
    function GetFirstChildNode: TVTNode<T>;
    function GetFocused: Boolean;
    function GetHint: string;
    function GetImageIndex: Integer;
    function GetIndex: Integer;
    function GetItem(AIndex: UInt32): T;
    function GetLastChildData: T;
    function GetLastChildNode: TVTNode<T>;
    function GetLevel: Integer;
    function GetNextSiblingData: T;
    function GetNextSiblingNode: TVTNode<T>;
    function GetNode(AIndex: UInt32): TVTNode<T>;
    function GetNodeHeight: Word;
    function GetOwnsObject: Boolean;
    function GetParentData: T;
    function GetParentNode: TVTNode<T>;
    function GetPrevSiblingData: T;
    function GetPrevSiblingNode: TVTNode<T>;
    function GetSelected: Boolean;
    function GetStates: TVirtualNodeStates;
    function GetText: string; virtual;
    function GetTotalCount: Cardinal;
    function GetTotalHeight: Cardinal;
    function GetTree: TCustomVirtualStringTree;
    function GetVisible: Boolean;
    function GetVNode: PVirtualNode;
    procedure SetAlign(const Value: Byte);
    procedure SetCheckState(const Value: TCheckState);
    procedure SetCheckType(const Value: TCheckType);
    procedure SetData(const Value: T);
    procedure SetExpanded(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetNodeHeight(const Value: Word);
    procedure SetOwnsObject(AValue: Boolean);
    procedure SetSelected(const Value: Boolean);
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
    destructor Destroy; override;

    //function GetEnumerator: TVTNodeEnumerator<T>;
    function DataEquals(const AData1: T; const AData2: T): Boolean;

    function Add(const AData: T; AOwnsObject: Boolean = True): TVTNode<T>;
    function Find(const AData: T): TVTNode<T>;
    procedure Select;
    procedure SetFocus;
    procedure Expand;
    procedure Collapse;
    procedure FullExpand;
    procedure FullCollapse;

    function HasChildren: Boolean;
    function HasParent: Boolean;

    { line/button alignment }
    property Align: Byte
      read GetAlign write SetAlign;

    property ChildCount: UInt32
      read GetChildCount;

    { User defined data that is associated with the current node. }
    property Data: T
      read GetData write SetData;

    { Indicates the current check state. }
    property CheckState: TCheckState
      read GetCheckState write SetCheckState;

    { Indicates which check type shall be used for this node. }
    property CheckType: TCheckType
      read GetCheckType write SetCheckType;

    property Expanded: Boolean
      read GetExpanded write SetExpanded;

    property Focused: Boolean
      read GetFocused write SetFocused;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    { Index of  the node with regard to its parent. }
    property Index: Integer
      read GetIndex;

    { Indexed access to the data of child nodes. }
    property Items[AIndex: UInt32]: T
      read GetItem; default;

    property Level: Integer
      read GetLevel;

    { Child nodes. }
    property Nodes[AIndex: UInt32]: TVTNode<T>
      read GetNode;

    property NodeHeight: Word
      read GetNodeHeight write SetNodeHeight;

    { If Data is of a class type, this determines if Data is freed when TVTNode
    instance is freed. }
    property OwnsObject: Boolean
      read GetOwnsObject write SetOwnsObject;

    property Selected: Boolean
      read GetSelected write SetSelected;

    { states describing various properties of the node (expanded, initialized etc.) }
    property States: TVirtualNodeStates
      read GetStates;

    property Text: string
      read GetText write SetText;

    { Total children including recursive child nodes. }
    property TotalCount: Cardinal
      read GetTotalCount;

    property TotalHeight: Cardinal
      read GetTotalHeight;

    property Tree: TCustomVirtualStringTree
      read GetTree;

    property Hint: string
      read GetHint write SetHint;

    property Visible: Boolean
      read GetVisible write SetVisible;

    { Points to the corresponding node record of the virtual treeview. }
    property VNode: PVirtualNode
      read GetVNode write SetVNode;

    property ParentNode: TVTNode<T>
      read GetParentNode;

    property PrevSiblingNode: TVTNode<T>
      read GetPrevSiblingNode;

    property NextSiblingNode: TVTNode<T>
      read GetNextSiblingNode;

    property FirstChildNode: TVTNode<T>
      read GetFirstChildNode;

    property LastChildNode: TVTNode<T>
      read GetLastChildNode;

    property ParentData: T
      read GetParentData;

    property PrevSiblingData: T
      read GetPrevSiblingData;

    property NextSiblingData: T
      read GetNextSiblingData;

    property FirstChildData: T
      read GetFirstChildData;

    property LastChildData: T
      read GetLastChildData;

  end;

implementation

uses
  SysUtils,

  ts.Core.Logger;

(*
{$REGION 'TVTNode<T>.TVTNodeEnumerator<K>'}
constructor TVTNode<T>.TVTNodeEnumerator<K>.Create(AVTNode: TVTNode<K>);
begin
  FCurrent := AVTNode;
  FFirst   := True;
end;

function TVTNode<T>.TVTNodeEnumerator<K>.GetCurrent: TVTNode<K>;
begin
  Result := FCurrent;
end;

function TVTNode<T>.TVTNodeEnumerator<K>.MoveNext: Boolean;
var
  LTree : TCustomVirtualStringTree;
begin
  if Assigned(FCurrent) then
  begin
    if FFirst then
    begin
      FFirst := False;
    end
    else
    begin
      LTree := FCurrent.FTree;
      FCurrent := LTree.GetNodeData<TVTNode<K>>(FCurrent.VNode.NextSibling);
    end;
  end;
  Result := Assigned(FCurrent);
end;
{$ENDREGION}
*)

{$REGION 'construction and destruction'}
constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree; const AData: T;
  AOwnsObject: Boolean; AParentVNode: PVirtualNode; const AText: string);
begin
  FTree       := ATree;
  FData       := AData;
  FOwnsObject := AOwnsObject;
  FText       := AText;
  FImageIndex := -1;
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

destructor TVTNode<T>.Destroy;
begin
  if (GetTypekind(T) = tkClass) and OwnsObject then
    TObject(Pointer(@FData)^).Free;
  FTree  := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TVTNode<T>.GetAlign: Byte;
begin
  if Assigned(VNode) then
    Result := VNode.Align
  else
    Result := 0;
end;

procedure TVTNode<T>.SetAlign(const Value: Byte);
begin
  if Assigned(VNode) then
    VNode.Align := Value;
end;

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

function TVTNode<T>.GetExpanded: Boolean;
begin
  Result := Assigned(FTree) and (FTree.Expanded[VNode] = True);
end;

procedure TVTNode<T>.SetExpanded(const Value: Boolean);
begin
  if Value <> Expanded then
  begin
    if Assigned(FTree) then
      FTree.Expanded[VNode] := Value;
  end;
end;

function TVTNode<T>.GetFirstChildData: T;
begin
  if Assigned(FirstChildNode) then
    Result := FirstChildNode.Data
  else
    Result := Default(T);
end;

function TVTNode<T>.GetFirstChildNode: TVTNode<T>;
begin
  if Assigned(VNode) then
    Result := VTNodeFromVNode(VNode.FirstChild)
  else
    Result := nil;
end;

function TVTNode<T>.GetFocused: Boolean;
begin
  Result := Assigned(FTree) and (FTree.FocusedNode = VNode);
end;

procedure TVTNode<T>.SetFocused(const Value: Boolean);
begin
  if Value <> Focused then
  begin
    if Assigned(FTree) then
    begin
      FTree.FocusedNode := VNode;
      FTree.InvalidateNode(VNode);
    end;
  end;
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

function TVTNode<T>.GetItem(AIndex: UInt32): T;
begin
	Result := Nodes[AIndex].Data;
end;

function TVTNode<T>.GetLastChildData: T;
begin
  if Assigned(LastChildNode) then
    Result := LastChildNode.Data
  else
    Result := Default(T);
end;

function TVTNode<T>.GetLastChildNode: TVTNode<T>;
begin
  if Assigned(VNode) then
    Result := VTNodeFromVNode(VNode.LastChild)
  else
    Result := nil;
end;

function TVTNode<T>.GetLevel: Integer;
begin
  if Assigned(FTree) and Assigned(VNode) then
    Result := FTree.GetNodeLevel(VNode)
  else
    Result := 0;
end;

function TVTNode<T>.GetNextSiblingData: T;
begin
  if Assigned(NextSiblingNode) then
    Result := NextSiblingNode.Data
  else
    Result := Default(T);
end;

function TVTNode<T>.GetNextSiblingNode: TVTNode<T>;
begin
  if Assigned(VNode) then
    Result := VTNodeFromVNode(VNode.NextSibling)
  else
    Result := nil;
end;

function TVTNode<T>.GetNode(AIndex: UInt32): TVTNode<T>;
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

function TVTNode<T>.GetNodeHeight: Word;
begin
  Result := VNode.NodeHeight;
end;

procedure TVTNode<T>.SetNodeHeight(const Value: Word);
begin
  VNode.NodeHeight := Value;
end;

function TVTNode<T>.GetText: string;
begin
  Result := FText;
end;

function TVTNode<T>.GetOwnsObject: Boolean;
begin
  Result := FOwnsObject;
end;

function TVTNode<T>.GetParentData: T;
begin
  Result := ParentNode.Data;
end;

function TVTNode<T>.GetParentNode: TVTNode<T>;
begin
  if Assigned(VNode) then
    Result := VTNodeFromVNode(VNode.Parent)
  else
    Result := nil;
end;

function TVTNode<T>.GetPrevSiblingData: T;
begin
  Result := PrevSiblingNode.Data;
end;

function TVTNode<T>.GetPrevSiblingNode: TVTNode<T>;
begin
  if Assigned(VNode) then
    Result := VTNodeFromVNode(VNode.PrevSibling)
  else
    Result := nil;
end;

procedure TVTNode<T>.SetOwnsObject(AValue: Boolean);
begin
  FOwnsObject := AValue;
end;

function TVTNode<T>.GetSelected: Boolean;
begin
  Result := Assigned(FTree) and FTree.Selected[VNode];
end;

procedure TVTNode<T>.SetSelected(const Value: Boolean);
begin
  if Value <> Selected then
  begin
    if Assigned(FTree) then
      FTree.Selected[VNode] := True;
  end;
end;

function TVTNode<T>.GetStates: TVirtualNodeStates;
begin
  Result := VNode.States;
end;

procedure TVTNode<T>.SetText(const Value: string);
begin
  FText := Value;
end;

function TVTNode<T>.GetTotalCount: Cardinal;
begin
  if Assigned(VNode) then
    Result := VNode.TotalCount
  else
    Result := 0;
end;

function TVTNode<T>.GetTotalHeight: Cardinal;
begin
  if Assigned(VNode) then
    Result := VNode.TotalHeight
  else
    Result := 0;
end;

function TVTNode<T>.GetTree: TCustomVirtualStringTree;
begin
  Result := FTree;
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
{ Works for both class and interface types. }

function TVTNode<T>.DataEquals(const AData1, AData2: T): Boolean;
begin
  Result := TObject(Pointer(@AData1)^) = TObject(Pointer(@AData2)^);
end;

{ Search with recursion. }

function TVTNode<T>.SearchTree(ANode: TVTNode<T>; const AData: T): TVTNode<T>;
var
  I      : UInt32;
  LFound : Boolean;
  LNode  : TVTNode<T>;
begin
  I      := 0;
  LFound := False;
  Result := nil;
  while (I < ANode.ChildCount) and not LFound do
  begin
    LNode := ANode.Nodes[I];
    if DataEquals(LNode.Data, AData) then
    begin
      Result := LNode;
      LFound := True;
    end
    else
    begin
      LNode := SearchTree(LNode, AData);
      if Assigned(LNode) and DataEquals(LNode.Data, AData) then
      begin
        Result := LNode;
        LFound := True;
      end
    end;
    Inc(I);
  end;
end;

function TVTNode<T>.VTNodeFromVNode(const AVNode: PVirtualNode): TVTNode<T>;
begin
  Result := TVTNode<T>(FTree.GetNodeData(AVNode)^);
end;
{$ENDREGION}

{$REGION 'protected methods'}
//function TVTNode<T>.GetEnumerator: TVTNodeEnumerator<T>;
//begin
//  Result := TVTNodeEnumerator<T>.Create(FirstChildNode);
//end;
{$ENDREGION}

{$REGION 'public methods'}
function TVTNode<T>.Add(const AData: T; AOwnsObject: Boolean): TVTNode<T>;
var
  LVTNode : TVTNode<T>;
  LVNode  : PVirtualNode;
begin
  if not Assigned(VNode) then // create root node if it does not exist
  begin
    VNode := FTree.AddChild(nil, Self);
  end;
  LVTNode := TVTNode<T>.Create(FTree, AData, AOwnsObject, VNode);
  LVNode := FTree.AddChild(VNode, LVTNode);
  LVTNode.VNode := LVNode;
  Result := LVTNode;
end;

procedure TVTNode<T>.Collapse;
begin
  Expanded := False;
end;

procedure TVTNode<T>.Expand;
begin
  Expanded := True;
end;

function TVTNode<T>.Find(const AData: T): TVTNode<T>;
begin
  Result := SearchTree(Self, AData);
end;

{ Collapse node including children. }

procedure TVTNode<T>.FullCollapse;
begin
  if Assigned(FTree) then
    FTree.FullCollapse(VNode);
end;

{ Expand node including children. }

procedure TVTNode<T>.FullExpand;
begin
  if Assigned(FTree) then
    FTree.FullExpand(VNode);
end;

function TVTNode<T>.HasChildren: Boolean;
begin
  Result := ChildCount > 0;
end;

function TVTNode<T>.HasParent: Boolean;
begin
  Result := Assigned(ParentNode);
end;

procedure TVTNode<T>.SetFocus;
begin
  Focused := True;
end;

procedure TVTNode<T>.Select;
begin
  Selected := True;
end;
{$ENDREGION}

end.
