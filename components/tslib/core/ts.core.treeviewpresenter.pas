(*
  Copyright (c) 2011-2013, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit ts.Core.TreeViewPresenter;

{
  Changes by Tim Sinaeve:

    - Ported to Lazarus
    - ImageList: TCustomImageList instead of TImageList
    - Header options added in InitColumns
    - Added DoMeasureItem for row height calculation
    - Fixed ShowHeader property
    - Support for column Margin property
    - Fixed passing Selected parameter in custom draw events
    - Added TStringlist support
}

{$MODE Delphi}

interface

uses
  Classes, Controls, Menus, SysUtils, Types, Contnrs, ComCtrls,
{$IFDEF Windows}
  ActiveX,
{$ENDIF}
  ts.Core.ColumnDefinitions, ts.Core.DataTemplates,

  VirtualTrees;

type
  TCompareEvent = procedure(Sender: TObject; Item1, Item2: TObject;
    ColumnIndex: Integer; var Result: Integer) of object;
  TDragBeginEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;
  TDragOverEvent = procedure(Sender: TObject; TargetItem: TObject;
    var AllowDrop: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; TargetItem: TObject;
    DragOperation: TDragOperation) of object;
  TUpdateTrigger = (utPropertyChanged, utLostFocus, utExplicit);
  TCollectionChangedAction = (caAdd, caRemove, caReplace, caMove, caReset);
  TCollectionChangedEvent = procedure(Sender: TObject; Item: TObject;
    Action: TCollectionChangedAction) of object;

const
  UpdateTriggerDefault = utPropertyChanged;

type
  TPropertyChangedEvent = procedure(
    ASender        : TObject;
    APropertyName  : string;
    AUpdateTrigger : TUpdateTrigger = utPropertyChanged
  ) of object;

  TFilterEvent = procedure(
        Item     : TObject;
    var Accepted : Boolean
  ) of object;

  TSelectionChangingEvent = procedure(Sender: TObject; TargetItem: TObject;
    var AllowChange: Boolean) of object;

  INotifyPropertyChanged = interface
    ['{6627279B-8112-4A92-BBD3-795185A41966}']
    function GetOnPropertyChanged: TPropertyChangedEvent;
    property OnPropertyChanged: TPropertyChangedEvent
      read GetOnPropertyChanged;
  end;

  INotifyCollectionChanged = interface
    ['{FE0D3160-6BCE-46B6-B01D-1B3C23EA76F3}']
    function GetOnCollectionChanged: TCollectionChangedEvent;
    property OnCollectionChanged: TCollectionChangedEvent
      read GetOnCollectionChanged;
  end;

  ICollectionView = interface(INotifyCollectionChanged)
    ['{A13215DC-49AC-46AF-A85A-EEC3CC0D709C}']
    function GetCurrentItem: TObject;
    procedure SetCurrentItem(const Value: TObject);
    function GetItemsSource: TObjectList;
    procedure SetItemsSource(const Value: TObjectList);
    function GetItemTemplate: IDataTemplate;
    procedure SetItemTemplate(const Value: IDataTemplate);

    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property ItemsSource: TObjectList read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
  end;

  TCheckSupport = (
    csNone,
    csSimple,
    csTriState,
    csRadio
  );

  TSelectionMode = (
    smSingle,
    smLevel,
    smMulti,
    smNone
  );

  { TTreeViewPresenter }

  TTreeViewPresenter = class(TComponent, ICollectionView, INotifyPropertyChanged)
  private
    FAction: TBasicAction;
    FUpdateCount: Integer;
    FAllowClearSelection: Boolean;
    FAllowMove: Boolean;
    FCheckedItems: TObjectList;
    FChecking: Boolean;
    FCheckSupport: TCheckSupport;
    FCollectionChanging: Integer;
    FCurrentNode: PVirtualNode;
    FColumnDefinitions: TColumnDefinitions;
    FExpandedItems: TObjectList;
    FHitInfo: THitInfo;
    FImageList: TCustomImageList;
    FItemsSource: TObjectList;
    FItemTemplate: IDataTemplate;
    FListMode: Boolean;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOnCollectionChanged: TCollectionChangedEvent;
    FOnCompare: TCompareEvent;
    FOnDoubleClick: TNotifyEvent;
    FOnDragBegin: TDragBeginEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnKeyAction: TKeyEvent;
    FOnFilter: TFilterEvent;
    FOnPropertyChanged: TPropertyChangedEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSelectionChanging: TSelectionChangingEvent;
    FProgressBar: TProgressBar;
    FPopupMenu: TPopupMenu;
    FSelectedItems: TObjectList;
    FShowHeader: Boolean;
    FSelectionMode: TSelectionMode;
    FSorting: Boolean;
    FSyncing: Boolean;
    FSyncMode: Boolean;
    FTreeView: TVirtualStringTree;

    procedure DoMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; var NodeHeight: Integer);
    procedure DoAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
    procedure DoBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure DoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoCurrentItemPropertyChanged(Sender: TObject;
      PropertyName: string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    {$IFDEF Windows}
    procedure DoDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    {$ENDIF}
    procedure DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure DoEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFilterNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoFocusChanging(Sender: TBaseVirtualTree;
      OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);	  
   

    procedure DoGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure DoGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DoHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
    procedure DoInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
    procedure DoNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure DrawCheckBox(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Boolean);
    procedure DrawProgressBar(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);
    procedure DrawImage(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);

    procedure ExpandNode(Node: PVirtualNode);
    function GetCheckedItem: TObject;
    function GetCheckedItems: TObjectList;
    function GetCanMoveCurrentToNext: Boolean;
    function GetCanMoveCurrentToPrevious: Boolean;
    function GetCurrentItem: TObject;
    function GetExpandedItems: TObjectList;
    procedure GetItemNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetItemsSource: TObjectList;
    function GetItemTemplate: IDataTemplate; overload;
    function GetItemTemplate(const Item: TObject): IDataTemplate; overload;
    procedure GetItemsNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode): TObjectList;
    function GetNodeItemsAsObject(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetParentItem(const Level: Integer): TObject;
    function GetSelectedItem: TObject;
    function GetSelectedItems: TObjectList;
    function CalcCheckBoxRect(const Rect: TRect): TRect;
    function CalcImageRect(const Rect: TRect): TRect;
    function IsMouseInCheckBox(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
    function ToggleCheckBox(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    procedure ToggleIcon(Node: PVirtualNode; Column: TColumnIndex);

    function GetOnCollectionChanged: TCollectionChangedEvent;
    function GetOnPropertyChanged: TPropertyChangedEvent;

    procedure InitColumns;
    procedure InitControl;
    procedure InitEvents;
    procedure InitProperties;
    procedure MoveCurrentToFirst;
    procedure MoveCurrentToLast;
    procedure MoveCurrentToNext;
    procedure MoveCurrentToPrevious;

    procedure ReadMultiSelect(Reader: TReader);
    procedure ResetRootNodeCount;

    procedure SetCheckedItem(const Value: TObject);
    procedure SetCheckedItems(const Value: TObjectList);
    procedure SetCheckSupport(const Value: TCheckSupport);
    procedure SetColumnDefinitions(const Value: TColumnDefinitions);
    procedure SetCurrentItem(const Value: TObject);
    procedure SetExpandedItems(const Value: TObjectList);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetItemsSource(const Value: TObjectList);
    procedure SetItemTemplate(const Value: IDataTemplate);
    procedure SetListMode(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode; Item: TObject);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode; Items: TObjectList);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: TObjectList);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetSorting(const Value: Boolean);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoCheckedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoDblClick(Sender: TObject);
    procedure DoExpandedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
   procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoSelectedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
   procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DeleteItems(Items: TObjectList);
    procedure SelectAll;
    procedure FullCollapse;
    procedure FullExpand;

    procedure Refresh;

    property CheckedItem: TObject read GetCheckedItem write SetCheckedItem;
    property CheckedItems: TObjectList read GetCheckedItems write SetCheckedItems;
    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property ExpandedItems: TObjectList read GetExpandedItems write SetExpandedItems;
    property ParentItem[const Level: Integer]: TObject read GetParentItem;
    property ItemsSource: TObjectList read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property SelectedItems: TObjectList read GetSelectedItems write SetSelectedItems;
  published
    property Action: TBasicAction
      read FAction write FAction;
    property AllowClearSelection: Boolean
      read FAllowClearSelection write FAllowClearSelection default True;
    property AllowMove: Boolean read FAllowMove write FAllowMove default True;
    property CheckSupport: TCheckSupport read FCheckSupport write SetCheckSupport default csNone;
    property ColumnDefinitions: TColumnDefinitions
      read FColumnDefinitions write SetColumnDefinitions;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ListMode: Boolean read FListMode write SetListMode default True;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property MultiLine: Boolean read FMultiLine write FMultiLine default True;
    property OnCollectionChanged: TCollectionChangedEvent read GetOnCollectionChanged;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnDragBegin: TDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnKeyAction: TKeyEvent read FOnKeyAction write FOnKeyAction;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property OnSelectionChanging: TSelectionChangingEvent
      read FOnSelectionChanging write FOnSelectionChanging;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode default smSingle;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property Sorting: Boolean read FSorting write SetSorting default True;
    property SyncMode: Boolean read FSyncMode write FSyncMode default False;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property TreeView: TVirtualStringTree read FTreeView write SetTreeView;
    property OnFilter: TFilterEvent read FOnFilter write FOnFilter;
  end;

implementation

uses
  Themes, Math, Forms, Graphics,

{$IFDEF Windows}
  Windows,
{$ENDIF}

  LCLType,

  sharedlogger,

  ts.Core.ColumnDefinitionsDataTemplate;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Item: TObject;
    Items: TObjectList;
    ItemsAsObject: TObject;
  end;

var
  CheckBoxSize: Byte;

procedure Synchronize(Target, Source: TObjectList);
var
  i: Integer;
begin
  i := 0;
  while i < Target.Count do
  begin
    if Source.IndexOf(Target[i]) <> -1 then
    begin
      Inc(i);
    end
    else
    begin
      Target.Delete(i);
    end;
  end;
  for i := 0 to Pred(Source.Count) do
  begin
    if Target.IndexOf(Source[i]) = -1 then
    begin
      Target.Add(Source[i]);
    end;
  end;
end;
{ TTreeViewPresenter }

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FAllowClearSelection := True;
  FAllowMove := True;
  FShowHeader := True;
  FSorting := True;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Smooth := True;
  FProgressBar.Visible := False;
  FAllowMove := True;
  FListMode := True;
  FMultiLine := True;
  FCheckedItems := TObjectList.Create(False);
  FExpandedItems := TObjectList.Create(False);
  FSelectedItems := TObjectList.Create(False);
  FOnCollectionChanged := DoSourceCollectionChanged;
  FColumnDefinitions := TColumnDefinitions.Create(Self, TColumnDefinition);
  FItemTemplate := TColumnDefinitionsDataTemplate.Create(FColumnDefinitions);
end;

destructor TTreeViewPresenter.Destroy;
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    FColumnDefinitions.Free;
  end;
  FCheckedItems.Free;
  FExpandedItems.Free;
  FSelectedItems.Free;
  inherited;
end;

procedure TTreeViewPresenter.ApplyFilter;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
  BeginUpdate;
  try
    //Refresh;
    LNode := FTreeView.GetFirst;
    while Assigned(LNode) do
    begin
      DoFilterNode(FTreeView, LNode);
      LNode := FTreeView.GetNext(LNode);
    end;
  finally
    EndUpdate;
  end;
  end;
end;

procedure TTreeViewPresenter.BeginUpdate;
begin
  FTreeView.BeginUpdate;
end;

function TTreeViewPresenter.CalcCheckBoxRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - CheckBoxSize) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - CheckBoxSize) div 2;
  Result.Right := Result.Left + CheckBoxSize;
  Result.Bottom := Result.Top + CheckBoxSize;
end;

function TTreeViewPresenter.CalcImageRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - ImageList.Width) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - ImageList.Height) div 2;
  Result.Right := Result.Left + ImageList.Width;
  Result.Bottom := Result.Top + ImageList.Height;
end;

procedure TTreeViewPresenter.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MultiSelect', ReadMultiSelect, nil, False);
end;

procedure TTreeViewPresenter.DeleteItems(Items: TObjectList);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Items) then
  begin
    LNode := FTreeView.GetFirst;
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and (Items.IndexOf(LItem) > -1) then
      begin
        FTreeView.DeleteNode(LNode);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.SelectAll;
begin
  TreeView.SelectAll(True);
  UpdateSelectedItems;
end;

procedure TTreeViewPresenter.DoMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  VST: TVirtualStringTree;
  I  : Integer;
  H  : Integer;
begin
  if MultiLine and Sender.MultiLine[Node] then
  begin
    VST := Sender as TVirtualStringTree;
    TargetCanvas.Font := Sender.Font;
    H := 0;
    NodeHeight := VST.DefaultNodeHeight;
    for I := 0 to VST.Header.Columns.Count - 1 do
    begin
      H := VST.ComputeNodeHeight(TargetCanvas, Node, I);
      if H > NodeHeight then
        NodeHeight := H;
    end;
    if NodeHeight > Integer(VST.DefaultNodeHeight) then
      NodeHeight := NodeHeight + 4; // needed to avoid multiline text drawing issues
  end;
end;

procedure TTreeViewPresenter.DoAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  B: Boolean;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    if ColumnDefinitions[Column].ColumnType = TColumnType.ctCheckBox then
    begin
      B := LItemTemplate.GetText(LItem, Column) = '1';
      DrawCheckBox(TargetCanvas, Node, Column, CellRect, B);
    end
    else
    begin
      LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, FImageList,
        dmAfterCellPaint, vsSelected in Node.States);
    end;
  end;
end;

procedure TTreeViewPresenter.DoBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, FImageList,
      dmBeforeCellPaint, vsSelected in Node.States);
  end;
end;

procedure TTreeViewPresenter.DoChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if FSelectionMode <> smNone then
  begin
    UpdateSelectedItems;
    if Assigned(FOnSelectionChanged) then
    begin
      FOnSelectionChanged(Self);
    end;
  end;
end;

procedure TTreeViewPresenter.DoChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateCheckedItems;
end;

procedure TTreeViewPresenter.DoCheckedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
    if FCollectionChanging = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdd: FTreeView.CheckState[LNode] := csCheckedNormal;
            caRemove: FTreeView.CheckState[LNode] := csUncheckedNormal;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetSelectedItems(FCheckedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  LItem1, LItem2: TObject;
  LItemTemplate1, LItemTemplate2: IDataTemplate;
  S1, S2: string;
  D1, D2: Double;
begin
  LItem1 := GetNodeItem(Sender, Node1);
  LItem2 := GetNodeItem(Sender, Node2);
  LItemTemplate1 := GetItemTemplate(LItem1);
  LItemTemplate2 := GetItemTemplate(LItem2);

  if Assigned(FOnCompare) then
  begin
    FOnCompare(Self, LItem1, LItem2, Column, Result);
  end
  else
  begin
    // Using item template to sort
    if Assigned(LItemTemplate1) and Assigned(LItemTemplate2)
      and (Column < ColumnDefinitions.Count) and (Column >= 0) then
    begin
      S1 := LItemTemplate1.GetText(LItem1, Column);
      S2 := LItemTemplate2.GetText(LItem2, Column);
      case ColumnDefinitions[Column].DataType of
       dtString:
          Result := CompareText(S1, S2);
       dtNumeric:
       begin
         D1 := StrToFloatDef(S1, 0);
         D2 := StrToFloatDef(S2, 0);
         Result := CompareValue(D1, D2);
       end;
       dtDateTime:
       begin
         D1 := Double(StrToDateTimeDef(S1, 0));
         D2 := Double(StrToDateTimeDef(S2, 0));
         Result := CompareValue(D1, D2);
       end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and (FTreeView.SelectedCount > 0) then
  begin
    FTreeView.BeginUpdate;
    for LNode in FTreeView.GetSortedSelection(True) do
    begin
      FTreeView.InvalidateNode(LNode);
      FTreeView.SortTree(FTreeView.Header.SortColumn, FTreeView.Header.SortDirection, False);
      DoFilterNode(FTreeView, LNode);
      LItem := GetNodeItem(FTreeView, LNode);
      DoSourceCollectionChanged(Sender, LItem, caReplace);
    end;
    FTreeView.EndUpdate;
  end;
end;

procedure TTreeViewPresenter.DoDblClick(Sender: TObject);
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
begin
  LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
  FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, True, LHitInfo);

  if not IsMouseInToggleIcon(LHitInfo) then
  begin
    if FListMode and Assigned(LHitInfo.HitNode)
      and (LHitInfo.HitColumn < 1)
      and ((hiOnNormalIcon in LHitInfo.HitPositions)
      or (not Assigned(OnDoubleClick) and not Assigned(Action))) then
    begin
      FTreeView.ToggleNode(LHitInfo.HitNode);
    end
    else
    begin
      if ([hiOnItemButton..hiOnItemCheckbox] * LHitInfo.HitPositions = [])
        and Assigned(LHitInfo.HitNode)
        and not IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        if FTreeView.FocusedNode = LHitInfo.HitNode then
        begin
          if Assigned(FOnDoubleClick) and Assigned(FAction) then
          begin
            FOnDoubleClick(Self);
          end
          else if not (csDesigning in ComponentState) and Assigned(FAction) then
          begin
            FAction.Execute();
          end
          else if Assigned(FOnDoubleClick) then
          begin
            FOnDoubleClick(Self);
          end;
        end;
      end;
    end;
  end
  else
  begin
    if Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count)
      and (ColumnDefinitions[LHitInfo.HitColumn].ToggleMode = tmDoubleClick) then
    begin
      ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := FAllowMove;

  if Assigned(FOnDragBegin) then
  begin
    FOnDragBegin(Self, Allowed);
  end;
end;

{$IFDEF Windows}
procedure TTreeViewPresenter.DoDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  i: Integer;
  LItem: TObject;
  LNode: PVirtualNode;
  LSelectedNodes: TNodeArray;
begin
  LNode := Sender.DropTargetNode;
  LItem := GetNodeItem(Sender, LNode);
  if Assigned(LItem) then
  begin
    LSelectedNodes := Sender.GetSortedSelection(False);
    if ssCtrl in Shift then
    begin
      if Assigned(FOnDragDrop) then
      begin
        FOnDragDrop(Sender, LItem, doCopy);
      end;
      Sender.ReinitNode(LNode, True);
    end
    else
    begin
      if Assigned(FOnDragDrop) then
      begin
        FOnDragDrop(Sender, LItem, doMove);
      end;
      if Sender = Source then
      begin
        Inc(FUpdateCount);
        try
          for i := Low(LSelectedNodes) to High(LSelectedNodes) do
          begin
            FCurrentNode := LSelectedNodes[i].Parent;
            case Mode of
              dmNowhere: FTreeView.MoveTo(LSelectedNodes[i], nil, amAddChildLast, False);
              dmAbove: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertBefore, False);
              dmOnNode:
              begin
                if FCurrentNode <> LNode then
                begin
                  FTreeView.MoveTo(LSelectedNodes[i], LNode, amAddChildLast, False);
                  ExpandNode(LNode);
                end;
              end;
              dmBelow: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertAfter, False);
            end;
          end;
        finally
          Dec(FUpdateCount);
        end;
      end;
    end;
  end;
end;
{$ENDIF}

procedure TTreeViewPresenter.DoDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Pt.Y > -1 then
  begin
    LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
    LItem := GetNodeItem(Sender, LNode);
    case Mode of
      dmAbove, dmBelow: Accept := FAllowMove;
    end;
    if Assigned(FOnDragOver) then
    begin
      FOnDragOver(Sender, LItem, Accept);
    end
    else
    begin
      if Sender = Source then
      begin
        if not Assigned(LNode) then
        begin
          LNode := Sender.RootNode;
        end;
        Accept := Assigned(GetNodeItemsAsObject(Sender, LNode));
        if Accept then
        begin
          //for LItemNode in Sender.SelectedNodes do
          //begin
          //  if (LItemNode = LNode) or (LItemNode.Parent = LNode) or
          //    Sender.HasAsParent(LNode, LItemNode) then
          //  begin
          //    Accept := False;
          //    Break;
          //  end;
          //end;
        end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FTreeView.Header.SortColumn = Column then
  begin
    FTreeView.Sort(Node.Parent, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
  end;
end;

procedure TTreeViewPresenter.DoEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Assigned(ColumnDefinitions) and (Column > -1)
    and (Column < ColumnDefinitions.Count)
    and (ColumnDefinitions[Column].ColumnType in [ctText, TColumnType.ctCheckBox])
    and ColumnDefinitions[Column].AllowEdit;
end;

procedure TTreeViewPresenter.DoExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoExpandedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FCollectionChanging = 0) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      if GetNodeItem(FTreeView, LNode) = Item then
      begin
        case Action of
          caAdd: ExpandNode(LNode);
          caRemove: FTreeView.Expanded[LNode] := False;
        end;
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoFilterNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LAccepted: Boolean;
begin
  LItem := GetNodeItem(Sender, Node);
  LAccepted := True;

  if Assigned(FOnFilter) then
    FOnFilter(LItem, LAccepted);

  Sender.IsVisible[Node] := LAccepted;

  //if Assigned(ColumnDefinitions) then
  //begin
  //  for i := 0 to Pred(ColumnDefinitions.Count) do
  //  begin
  //    if Assigned(ColumnDefinitions[i].Filter) then
  //    begin
  //      Sender.IsVisible[Node] := Sender.IsVisible[Node]
  //        or not ColumnDefinitions[i].Filter(LItem);
  //    end;
  //  end;
  //end;

  if Sender.IsVisible[Node] and Sender.Selected[Node] then
  begin
    Sender.Selected[Node] := False;
  end;
end;

procedure TTreeViewPresenter.DoFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LItem: TObject;
begin
  LItem := GetNodeItem(Sender, Node);
  if Assigned(LItem) then
  begin
    // nothing to do here yet
  end;
end;

procedure TTreeViewPresenter.DoFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  LItem: TObject;
begin
  if OldNode <> NewNode then
  begin
    LItem := GetNodeItem(Sender, NewNode);
    if Assigned(FOnSelectionChanging) then
    begin
      FOnSelectionChanging(Sender, LItem, Allowed);
      if not Allowed and Assigned(OldNode) then
      begin
        FTreeView.Selected[OldNode] := True;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SetNodeItems(Sender, Node, nil);
  if FHitInfo.HitNode = Node then
  begin
    FHitInfo.HitNode := nil;
  end;
end;

procedure TTreeViewPresenter.DoGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    HintText := LItemTemplate.GetHint(LItem, Column);
  end;
end;

procedure TTreeViewPresenter.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    LItem := GetNodeItem(Sender, Node);
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      ImageIndex := LItemTemplate.GetImageIndex(LItem, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    if Assigned(ColumnDefinitions) and (Column > -1)
      and (Column < ColumnDefinitions.Count)
      and (ColumnDefinitions[Column].ColumnType <> TColumnType.ctText) then
    begin
      CellText := '';
    end
    else
    begin
      CellText := LItemTemplate.GetText(LItem, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  LCursor: TCursor;
begin
  if FSorting and (HitInfo.Button = mbLeft) and (HitInfo.Column > -1)
    and (HitInfo.Column < ColumnDefinitions.Count)
    and (coSortable in ColumnDefinitions[HitInfo.Column].ColumnOptions) then
  begin
    LCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if Sender.SortColumn <> HitInfo.Column then
      begin
        Sender.SortColumn := HitInfo.Column;
      end
      else
      begin
        if Sender.SortDirection = sdAscending then
        begin
          Sender.SortDirection := sdDescending;
        end
        else
        begin
          Sender.SortDirection := sdAscending;
        end;
      end;

      if Sender.SortColumn = -1 then
      begin
        Refresh();
      end;
    finally
      Screen.Cursor := LCursor;
    end;
  end;
end;

procedure TTreeViewPresenter.DoHeaderDblClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  LCursor: TCursor;
begin
  if FSorting and (HitInfo.Button = mbLeft) and (HitInfo.Column > -1)
    and (HitInfo.Column < ColumnDefinitions.Count)
    and (coSortable in ColumnDefinitions[HitInfo.Column].ColumnOptions) then
  begin
    LCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if Sender.SortColumn = HitInfo.Column then
      begin
        Sender.SortColumn := -1;
        Refresh();
      end;
    finally
      Screen.Cursor := LCursor;
    end;
  end;
end;

procedure TTreeViewPresenter.DoIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);
var
  LCellText: string;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LCellText := LItemTemplate.GetText(LItem, ColumnDefinitions.MainColumnIndex);
  end
  else
  begin
    LCellText := '';
  end;

  Result := StrLIComp(PChar(SearchText), PChar(LCellText),
    Min(Length(SearchText), Length(LCellText)));
end;

procedure TTreeViewPresenter.DoInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LParentItem: TObject;
begin
  Include(InitialStates, ivsMultiline);
  DoPropertyChanged('ParentItem');
  case FCheckSupport of
    csTriState: Node.CheckType := ctTriStateCheckBox;
    csRadio: Node.CheckType := ctRadioButton;
  else
    Node.CheckType := ctCheckBox;
  end;

  if Assigned(ParentNode) then
  begin
    LParentItem := GetNodeItem(Sender, ParentNode);
    LItemTemplate := GetItemTemplate(LParentItem);
    LItem := LItemTemplate.GetItem(LParentItem, Node.Index);
  end
  else
  begin
    LItem := FItemsSource[Node.Index];
  end;

  SetNodeItem(Sender, Node, LItem);

  //LItemTemplate := GetItemTemplate(LItem);
  //if Assigned(Sender) then
  //begin
  //  if Assigned(LItemTemplate) then
  //  begin
  //    Sender.ChildCount[Node] := LItemTemplate.GetItemCount(LItem);
  //  end
  //  else
  //  begin
  //    Sender.ChildCount[Node] := 0;
  //  end;
  //end;
end;

procedure TTreeViewPresenter.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  LAllowed: Boolean;
  LNodes: TNodeArray;
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN:
      begin
        if Assigned(Action) and (FTreeView.SelectedCount > 0) then
        begin
          Action.Execute();
        end;
      end;
      VK_SPACE:
      begin
        if ToggleCheckBox(FTreeView.FocusedNode, FTreeView.FocusedColumn) then
          Key := 0;
      end;
    end;
  end;

  // moving elements with Ctrl+Up and Ctrl+Down only when sorting is off
  if FAllowMove and (ssCtrl in Shift) and (FTreeView.Header.SortColumn = -1) then
  begin
    case Key of
      VK_UP:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := Low(LNodes) to High(LNodes) do
        begin
          if LNodes[i].PrevSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := Low(LNodes) to High(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].PrevSibling, amInsertBefore, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
      VK_DOWN:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := High(LNodes) downto Low(LNodes) do
        begin
          if LNodes[i].NextSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := High(LNodes) downto Low(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].NextSibling, amInsertAfter, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
    end;
  end;

  if Assigned(FOnKeyAction) then
  begin
    FOnKeyAction(Self, Key, Shift);
  end;
end;

procedure TTreeViewPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
  FChecking := False;
  if not (ssDouble in Shift)
    and not (tsVCLDragPending in FTreeView.TreeStates) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);
    if not Assigned(LHitInfo.HitNode) then
    begin
      if FAllowClearSelection then
      begin
        FTreeView.FocusedNode := nil;
        if FTreeView.FocusedNode = nil then
          FTreeView.ClearSelection()
        else
          Abort;
      end
      else
      begin
        Abort;
      end;
    end
    else
    begin
      if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        FChecking := True;
        FTreeView.FocusedColumn := LHitInfo.HitColumn;
        FTreeView.RepaintNode(LHitInfo.HitNode);
        Abort;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
{$IFDEF Windows}
  if GetAsyncKeyState(VK_LBUTTON) = 0 then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);

    if Assigned(FHitInfo.HitNode) and (FHitInfo.HitNode <> LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(FHitInfo.HitNode);
    end;

    if Assigned(LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;

    FHitInfo := LHitInfo;

    if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      FHitInfo.HitPositions := [hiOnItem, hiOnItemCheckbox];

    if IsMouseInToggleIcon(LHitInfo) then
      FHitInfo.HitPositions := [hiOnItem, hiOnNormalIcon];
  end;
{$ENDIF}
end;

procedure TTreeViewPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(FHitInfo.HitNode)
    and not (tsVCLDragPending in FTreeView.TreeStates) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);
    if (FHitInfo.HitNode = LHitInfo.HitNode)
      and (FHitInfo.HitColumn = LHitInfo.HitColumn)
      and Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count) then
    begin
      LItem := GetNodeItem(FTreeView, LHitInfo.HitNode);
      LItemTemplate := GetItemTemplate(LItem);
      LColumnDefinition := ColumnDefinitions[LHitInfo.HitColumn];
      Logger.Send('Value', LItemTemplate.GetValue(LItem, LHitInfo.HitColumn).AsString);

      if Assigned(LItemTemplate) then
      begin
        if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn)
          and FChecking and LColumnDefinition.AllowEdit then
        begin
          LItemTemplate.SetValue(LItem, LHitInfo.HitColumn,
            not LItemTemplate.GetValue(LItem, LHitInfo.HitColumn).AsBoolean);
        end;

        if (hiOnNormalIcon in FHitInfo.HitPositions)
          and IsMouseInToggleIcon(LHitInfo)
          and (LColumnDefinition.ToggleMode = tmClick) then
        begin
          ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
        end;
      end;
    end;

    FTreeView.RepaintNode(FHitInfo.HitNode);
    if LHitInfo.HitNode <> FHitInfo.HitNode then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.SetText(LItem, Column, NewText);
  end;
end;

procedure TTreeViewPresenter.DoNodeMoved(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LItems: TObjectList;
  LItemTemplate: IDataTemplate;
begin
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    LItem := GetNodeItem(Sender, Node);
    FItemsSource.Move(FItemsSource.IndexOf(LItem), Node.Index);
  end
  else
  begin
    LItem := GetNodeItem(Sender, Node.Parent);
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      LItems := LItemTemplate.GetItems(LItem);
      if Assigned(LItems) then
      begin
        LItem := GetNodeItem(Sender, Node);
        LItems.Move(LItems.IndexOf(LItem), Node.Index);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    // TEMP
    TargetCanvas.Font.Color := TreeView.Font.Color;
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CDefaultCellRect,
      FImageList, dmPaintText, False);
  end;
end;

procedure TTreeViewPresenter.DoSelectedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FSelectionMode <> smNone) then
  begin
    if FCollectionChanging = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdd: FTreeView.Selected[LNode] := True;
            caRemove: FTreeView.Selected[LNode] := False;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetCheckedItems(FSelectedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  if Assigned(FOnPropertyChanged) then
  begin
    FOnPropertyChanged(Self, APropertyName, AUpdateTrigger);
  end;
end;

procedure TTreeViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
  LSelectedNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FUpdateCount = 0) then
  begin
    case Action of
      caAdd:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        if Assigned(LNode) then
          FTreeView.ReinitNode(LNode, False)
        else
          Refresh;
      end;

      caRemove:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        LNode := FTreeView.IterateSubtree(LNode, GetItemNode, Pointer(Item));

        // find node to select after deleting current node
        if FTreeView.Selected[LNode] and (FSelectionMode = smSingle) then
        begin
          LSelectedNode := FTreeView.GetNextVisibleSibling(LNode);

          if not Assigned(LSelectedNode) then
            LSelectedNode := FTreeView.GetPreviousVisibleSibling(LNode);
          if not Assigned(LSelectedNode) then
            LSelectedNode := LNode.Parent;
          if Assigned(LSelectedNode) then
          begin
            FTreeView.Selected[LSelectedNode] := True;
            FTreeView.FocusedNode := LSelectedNode;
          end;
        end;

        FTreeView.DeleteNode(LNode);
      end;

      caReplace:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        FTreeView.Sort(LNode, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
      end;

      caMove:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        if Assigned(LNode) then
        begin
          FTreeView.ReinitChildren(LNode, False);
          FTreeView.InvalidateChildren(LNode, True);
        end
        else
          Refresh;
      end;

      caReset:
      begin
        ResetRootNodeCount;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DrawCheckBox(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Boolean);
var
  LThemedButton: TThemedButton;
  LCheckBoxRect: TRect;
//  LDetails: TThemedElementDetails;
  LState: Cardinal;
begin
  LCheckBoxRect := CalcCheckBoxRect(CellRect);

  if (Column > -1) and (Column < ColumnDefinitions.Count)
    and ColumnDefinitions[Column].AllowEdit then
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedNormal;
    end
    else
    begin
       LThemedButton := tbCheckBoxUncheckedNormal;
    end;

    if IsMouseInCheckBox(Node, Column) then
    begin
      Inc(LThemedButton);
    end;
  end
  else
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedDisabled;
    end
    else
    begin
      LThemedButton := tbCheckBoxUncheckedDisabled;
    end;
  end;

  if (FHitInfo.HitNode = Node) and (FHitInfo.HitColumn = Column)
    and (hiOnItemCheckbox in FHitInfo.HitPositions)
{$IFDEF Windows}
    and (GetAsyncKeyState(VK_LBUTTON) <> 0)
{$ENDIF}
    and ColumnDefinitions[FHitInfo.HitColumn].AllowEdit then
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedPressed;
    end
    else
    begin
      LThemedButton := tbCheckBoxUncheckedPressed;
    end;
  end;

  //if StyleServices.Enabled and
  //  (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  //begin
  //  LDetails := StyleServices.GetElementDetails(LThemedButton);
  //  StyleServices.DrawElement(TargetCanvas.Handle, LDetails, LCheckBoxRect);
  //end
  //else
  begin
    LState := DFCS_BUTTONCHECK;
    if LThemedButton in [tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot] then
    begin
      LState := LState or DFCS_CHECKED;
    end;
{$IFDEF Windows}
    DrawFrameControl(TargetCanvas.Handle, LCheckBoxRect, DFC_BUTTON, LState);
{$ENDIF}
  end;
end;

procedure TTreeViewPresenter.DrawImage(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
var
  LRect: TRect;
begin
  if Assigned(ImageList) then
  begin
    LRect := CalcImageRect(CellRect);
    ImageList.Draw(TargetCanvas, LRect.Left, LRect.Top, Value);
  end;
end;

procedure TTreeViewPresenter.DrawProgressBar(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
//var
//  LDetails: TThemedElementDetails;
begin
  //if StyleServices.Enabled and
  //  (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  //begin
  //  InflateRect(CellRect, -1, -1);
  //  LDetails := StyleServices.GetElementDetails(tpBar);
  //  StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
  //  InflateRect(CellRect, -2, -2);
  //  CellRect.Right := CellRect.Left + Trunc(RectWidth(CellRect) * Value / 100);
  //  LDetails := StyleServices.GetElementDetails(tpChunk);
  //  StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
  //end
  //else
  begin
    InflateRect(CellRect, -1, -1);
    FProgressBar.Position := Value;
    FProgressBar.Height := RectHeight(CellRect);
    FProgressBar.Width := RectWidth(CellRect);
    FProgressBar.PaintTo(TargetCanvas, CellRect.Left, 1);
  end;
end;

procedure TTreeViewPresenter.EndUpdate;
begin
  inherited;
  FTreeView.EndUpdate();
end;

procedure TTreeViewPresenter.ExpandNode(Node: PVirtualNode);
begin
  if Assigned(FTreeView) then
  begin
    while Assigned(Node) do
    begin
      if [vsChecking..vsExpanded] * Node.States = [] then
      begin
        FTreeView.Expanded[Node] := True;
      end;
      Node := FTreeView.NodeParent[Node];
    end;
  end;
end;

procedure TTreeViewPresenter.FullCollapse;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FullCollapse();
  end;
end;

procedure TTreeViewPresenter.FullExpand;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.FullExpand();
  end;
end;

function TTreeViewPresenter.GetCanMoveCurrentToNext: Boolean;
var
  LNode: PVirtualNode;
begin
  Result := False;

  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetNextVisible(LNode);
    Result := Assigned(LNode);
  end;
end;

function TTreeViewPresenter.GetCanMoveCurrentToPrevious: Boolean;
var
  LNode: PVirtualNode;
begin
  Result := False;

  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetPreviousVisible(LNode);
    Result := Assigned(LNode);
  end;
end;

function TTreeViewPresenter.GetCheckedItem: TObject;
begin
  if FCheckedItems.Count > 0 then
  begin
    Result := FCheckedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetCheckedItems: TObjectList;
begin
  UpdateCheckedItems;
  Result := FCheckedItems;
end;

function TTreeViewPresenter.GetCurrentItem: TObject;
begin
  Result := GetSelectedItem;
end;

function TTreeViewPresenter.GetExpandedItems: TObjectList;
begin
  UpdateExpandedItems;
  Result := FExpandedItems;
end;

procedure TTreeViewPresenter.GetItemNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := GetNodeItem(Sender, Node) = TObject(Data);
end;

function TTreeViewPresenter.GetItemsSource: TObjectList;
begin
  Result := FItemsSource;
end;

function TTreeViewPresenter.GetItemTemplate: IDataTemplate;
begin
  Result := FItemTemplate;
end;

function TTreeViewPresenter.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  Result := nil;
  if Assigned(FItemTemplate) then
  begin
    Result := FItemTemplate.GetItemTemplate(Item);
  end;
end;

procedure TTreeViewPresenter.GetItemsNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Sender.GetNodeData(Node));
  Abort := Assigned(LNodeData) and (LNodeData.ItemsAsObject = TObject(Data));
end;

function TTreeViewPresenter.GetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Item;
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetNodeItems(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObjectList;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Items;
  end
  else
  begin
    if Node = Tree.RootNode then
    begin
      Result := {View.}ItemsSource;
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TTreeViewPresenter.GetOnCollectionChanged: TCollectionChangedEvent;
begin
  Result := FOnCollectionChanged;
end;

function TTreeViewPresenter.GetOnPropertyChanged: TPropertyChangedEvent;
begin
  Result := FOnPropertyChanged;
end;

function TTreeViewPresenter.GetNodeItemsAsObject(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.ItemsAsObject;
  end
  else
  begin
    if Node = Tree.RootNode then
    begin
      Result := {View.}ItemsSource;
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TTreeViewPresenter.GetParentItem(const Level: Integer): TObject;
var
  LLevel: Integer;
  LNode: PVirtualNode;
begin
  Result := GetNodeItem(FTreeView, FCurrentNode);

  LLevel := 0;
  LNode := FCurrentNode;
  while Assigned(LNode) and (LLevel < Level) do
  begin
    LNode := LNode.Parent;
    Result := GetNodeItem(FTreeView, LNode);
    Inc(LLevel);
  end;
end;

function TTreeViewPresenter.GetSelectedItem: TObject;
begin
  if (FSelectedItems.Count > 0) and (FSelectionMode <> smNone) then
  begin
    Result := FSelectedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetSelectedItems: TObjectList;
begin
  if (FSelectedItems.Count > 0) and (FSelectionMode = smNone) then
  begin
    FSelectedItems.Clear;
  end;
  Result := FSelectedItems;
end;

procedure TTreeViewPresenter.InitColumns;
var
  i: Integer;
  CD: TColumnDefinition;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    FTreeView.Header.Columns.Clear;
    if Assigned(FColumnDefinitions) then
    begin
      for i := 0 to Pred(FColumnDefinitions.Count) do
      begin
        CD := TColumnDefinition(FColumnDefinitions[i]);
        with FTreeView.Header.Columns.Add do
        begin
          Text      := CD.Caption;
          Width     := CD.Width;
          MinWidth  := CD.MinWidth;
          MaxWidth  := CD.MaxWidth;
          CheckBox  := CD.ColumnType = TColumnType.ctCheckBox;
          Alignment := CD.Alignment;
          Spacing   := CD.Spacing;
          Margin    := CD.Margin;
          Options := Options + [
            coWrapCaption,  // Caption could be wrapped across several header lines to fit columns width.
            coSmartResize,  // Column is resized to its largest entry which is in view (instead of its largest visible entry).
            coResizable,    // Column can be resized.
            coAutoSpring
          ];
          if CD.Fixed then
          begin
            Options := Options + [coFixed];
            Options := Options - [coSmartResize, coAutoSpring];
          end;
          if CheckBox then
          begin
            CheckState := csUncheckedNormal;
            CheckType  := ctCheckBox;
            Style      := vsOwnerDraw;
          end;
        end;
      end;
    end;
    FTreeView.Header.Options := FTreeView.Header.Options + [
      hoVisible,         // Header is visible.
      hoColumnResize,    // Resizing columns with the mouse is allowed.
      hoDblClickResize,  // Allows a column to resize itself to its largest entry.
      hoShowImages,      // Show header images.
      hoShowSortGlyphs   // Allow visible sort glyphs.
    ];
  end;
end;

procedure TTreeViewPresenter.InitControl;
begin
  if Assigned(FTreeView) and ([csDesigning, csDestroying] * ComponentState = []) then
  begin
    FTreeView.Images := ImageList;
    FTreeView.NodeDataSize := SizeOf(TNodeData);
    FTreeView.PopupMenu := PopupMenu;

    InitColumns;
    InitProperties;
    InitEvents;
    ResetRootNodeCount;
  end;
end;

procedure TTreeViewPresenter.InitEvents;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    FTreeView.OnAfterCellPaint := DoAfterCellPaint;
    FTreeView.OnMeasureItem  := DoMeasureItem;
    FTreeView.OnBeforeCellPaint := DoBeforeCellPaint;
    FTreeView.OnChange := DoChange;
    FTreeView.OnChecked := DoChecked;
    FTreeView.OnCollapsed := DoCollapsed;
    FTreeView.OnCompareNodes := DoCompareNodes;
    FTreeView.OnDblClick := DoDblClick;
    FTreeView.OnDragAllowed := DoDragAllowed;
{$IFDEF Windows}
    FTreeView.OnDragDrop := DoDragDrop;
{$ENDIF}
    FTreeView.OnDragOver := DoDragOver;
    FTreeView.OnEdited := DoEdited;
    FTreeView.OnEditing := DoEditing;
    FTreeView.OnExpanded := DoExpanded;
    FTreeView.OnFocusChanged := DoFocusChanged;
    FTreeView.OnFocusChanging := DoFocusChanging;
    FTreeView.OnFreeNode := DoFreeNode;
    FTreeView.OnGetHint := DoGetHint;
    FTreeView.OnGetImageIndex := DoGetImageIndex;
    FTreeView.OnGetText := DoGetText;
    FTreeView.OnHeaderClick := DoHeaderClick;
    FTreeView.OnHeaderDblClick := DoHeaderDblClick;
    FTreeView.OnIncrementalSearch := DoIncrementalSearch;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnKeyDown := DoKeyDown;
    FTreeView.OnMouseDown := DoMouseDown;
    FTreeView.OnMouseMove := DoMouseMove;
    FTreeView.OnMouseUp := DoMouseUp;
    FTreeView.OnNewText := DoNewText;
    FTreeView.OnNodeMoved := DoNodeMoved;
    FTreeView.OnPaintText := DoPaintText;
  end;
end;

procedure TTreeViewPresenter.InitProperties;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    if FShowHeader then
    begin
      FTreeView.Header.Options := FTreeView.Header.Options + [hoVisible];
    end
    else
    begin
      FTreeView.Header.Options := FTreeView.Header.Options - [hoVisible];
    end;

    if FAllowMove then
    begin
      FTreeView.DragMode := dmAutomatic;
    end
    else
    begin
      FTreeView.DragMode := dmManual;
    end;

    if FCheckSupport <> csNone then
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions + [toCheckSupport];
    end
    else
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions - [toCheckSupport];
    end;

    if FMultiSelect then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toMultiSelect];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toMultiSelect];
    end;

    if FListMode then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toShowRoot, toShowTreeLines];
    end;
    // TSI
    //FTreeView.Hint := ' '; // otherwise no tooltips are shown!

    FTreeView.TreeOptions.AutoOptions :=
      FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] + [toAutoSort];
    FTreeView.TreeOptions.MiscOptions :=
      FTreeView.TreeOptions.MiscOptions - [toToggleOnDblClick];
    FTreeView.TreeOptions.PaintOptions :=
      FTreeView.TreeOptions.PaintOptions + [toHideFocusRect];
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toFullRowSelect, toExtendedFocus];
  end;
end;

function TTreeViewPresenter.IsMouseInCheckBox(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
  LRect: TRect;
begin
  if Assigned(Node) and Assigned(ColumnDefinitions) and (Column > -1)
    and (Column < ColumnDefinitions.Count)
    and (ColumnDefinitions[Column].ColumnType = TColumnType.ctCheckBox) then
  begin
    LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
    FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, True, LHitInfo);
    LRect := FTreeView.GetDisplayRect(Node, Column, False);
    LRect := CalcCheckBoxRect(LRect);
    Result := PtInRect(LRect, LCursorPos);
  end
  else
  begin
    Result := False;
  end;
end;

function TTreeViewPresenter.IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
var
  LCursorPos: TPoint;
  LRect: TRect;
begin
  if Assigned(ColumnDefinitions) and (HitInfo.HitColumn > -1)
    and (HitInfo.HitColumn < ColumnDefinitions.Count) then
  begin
    if ColumnDefinitions[HitInfo.HitColumn].ColumnType = ctImage then
    begin
      LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
      LRect := FTreeView.GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
      LRect := CalcImageRect(LRect);
      if PtInRect(LRect, LCursorPos) then
      begin
        Include(HitInfo.HitPositions, hiOnNormalIcon);
      end;
    end;

    Result := (hiOnNormalIcon in HitInfo.HitPositions)
      and (ColumnDefinitions[HitInfo.HitColumn].ToggleMode <> tmNone);
  end
  else
  begin
    Result := False;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToFirst;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstVisible();
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToLast;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetLastVisible();
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToNext;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetNextVisibleSibling(LNode);
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.MoveCurrentToPrevious;
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.GetFirstSelected();
    LNode := FTreeView.GetPreviousVisibleSibling(LNode);
    if Assigned(LNode) then
    begin
      FTreeView.Selected[LNode] := True;
      if FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.ReadMultiSelect(Reader: TReader);
begin
  if Reader.ReadBoolean then
  begin
    FSelectionMode := smMulti;
  end
  else
  begin
    FSelectionMode := smSingle;
  end;
end;

procedure TTreeViewPresenter.Refresh;
//var
//  LCheckedItems: TObjectList;
//  LExpandedItems: TObjectList;
//  LSelectedItems: TObjectList;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    //LCheckedItems := TObjectList.Create(False);
    //LCheckedItems.AddList(CheckedItems);
    //LExpandedItems := TObjectList.Create(False);
    //LExpandedItems.AddList(ExpandedItems);
    //LSelectedItems := TObjectList.Create(False);
    //LSelectedItems.AddList(SelectedItems);
    ResetRootNodeCount;
    //CheckedItems := LCheckedItems;
    //ExpandedItems := LExpandedItems;
    //SelectedItems := LSelectedItems;
  end;
end;

procedure TTreeViewPresenter.ResetRootNodeCount;
begin
  if Assigned(FTreeView) then
  begin
    if Assigned({View.}ItemsSource) and Assigned({View.}ItemTemplate) then
    begin
      FTreeView.Clear;
      FTreeView.RootNodeCount := {View.}ItemTemplate.GetItemCount({View.}ItemsSource{.AsObject});
    end
    else
    begin
      FTreeView.RootNodeCount := 0;
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckedItem(const Value: TObject);
begin
  FCheckedItems.Clear();
  if Assigned(Value) then
  begin
    FCheckedItems.Add(Value);
  end;
  SetCheckedItems(FCheckedItems);
end;

procedure TTreeViewPresenter.SetCheckedItems(const Value: TObjectList);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirstInitialized();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and (Value.IndexOf(LItem) <> -1) then
      begin
        FTreeView.CheckState[LNode] := csCheckedNormal;
      end
      else
      begin
        FTreeView.CheckState[LNode] := csUncheckedNormal;
      end;
      LNode := FTreeView.GetNextInitialized(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckSupport(const Value: TCheckSupport);
begin
  FCheckSupport := Value;
  InitProperties;
end;

procedure TTreeViewPresenter.SetColumnDefinitions(
  const Value: TColumnDefinitions);
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    FColumnDefinitions.Free;
  end;
  FColumnDefinitions := Value;
  InitColumns;
end;

procedure TTreeViewPresenter.SetCurrentItem(const Value: TObject);
begin
  SetSelectedItem(Value);
end;

procedure TTreeViewPresenter.SetExpandedItems(const Value: TObjectList);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
  FTreeView.FullCollapse;
  if Assigned(Value) and (Value.Count > 0) then
  begin
    LNode := FTreeView.GetFirst;
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and (Value.IndexOf(LItem) > -1) then
      begin
        ExpandNode(LNode);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
  end;
end;

procedure TTreeViewPresenter.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
  SetTreeView(FTreeView);
end;

procedure TTreeViewPresenter.SetItemsSource(const Value: TObjectList);
begin
  if FItemsSource <> Value then
  begin
    FItemsSource := Value;
    ResetRootNodeCount;
    DoPropertyChanged('ItemsSource');
  end;
end;

procedure TTreeViewPresenter.SetItemTemplate(const Value: IDataTemplate);
begin
  FItemTemplate := Value;
  if Assigned(FTreeView) then
  begin
    InitColumns;
    ResetRootNodeCount;
  end;
end;

procedure TTreeViewPresenter.SetListMode(const Value: Boolean);
begin
  FListMode := Value;
  InitProperties;
end;

procedure TTreeViewPresenter.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
  InitProperties;
end;

procedure TTreeViewPresenter.SetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Item: TObject);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    LNodeData.Item := Item;
  end;
end;


procedure TTreeViewPresenter.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  SetTreeView(FTreeView);
end;

procedure TTreeViewPresenter.SetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode; Items: TObjectList);
var
  LNodeData: PNodeData;
  //LCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    if Assigned(LNodeData.Items) then
    begin
      //LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnCollectionChanged);
      //LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;
    LNodeData.Items := Items;
    if Assigned(LNodeData.Items) then
    begin
      LNodeData.ItemsAsObject := Items;
      //LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnCollectionChanged);
      //LCollectionChanged.Add(DoSourceCollectionChanged);
    end;
  end;
end;

procedure TTreeViewPresenter.SetSelectedItem(const Value: TObject);
begin
  if ((Value <> SelectedItem) or (SelectedItems.Count > 1))
    and (FSelectionMode <> smNone) then
  begin
    FSelectedItems.Clear();
    if Assigned(Value) then
    begin
      FSelectedItems.Add(Value);
    end;
    SetSelectedItems(FSelectedItems);
  end;
end;

procedure TTreeViewPresenter.SetSelectedItems(const Value: TObjectList);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in FTreeView.ComponentState)
    and (FSelectionMode <> smNone) then
  begin
    FTreeView.BeginUpdate();
    FTreeView.ClearSelection();
    if Assigned(Value) and (Value.Count > 0) then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and (Value.IndexOf(LItem) <> -1) then
        begin
          FTreeView.Selected[LNode] := True;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
      LNode := FTreeView.GetFirstSelected();
      FTreeView.FocusedNode := LNode;
      if Assigned(LNode) and (FCollectionChanging = 0)
        and FTreeView.HandleAllocated then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
    FTreeView.EndUpdate();
  end;
end;

procedure TTreeViewPresenter.SetSelectionMode(const Value: TSelectionMode);
begin
  FSelectionMode := Value;
  InitProperties;
end;

procedure TTreeViewPresenter.SetShowHeader(const Value: Boolean);
begin
  FShowHeader := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetSorting(const Value: Boolean);
begin
  FSorting := Value;
  if Assigned(FTreeView) and not FSorting then
  begin
    FTreeView.Header.SortColumn := -1;
    Refresh();
  end;
  InitProperties();
end;

procedure TTreeViewPresenter.SetTreeView(const Value: TVirtualStringTree);
begin
  if FTreeView <> Value then
  begin
    if Assigned(FTreeView) then
    begin
      FTreeView.RemoveFreeNotification(Self);
    end;

    FTreeView := Value;

    if Assigned(FTreeView) then
    begin
      FTreeView.FreeNotification(Self);
    end;

    if not (csDesigning in ComponentState) then
    begin
      FProgressBar.Parent := FTreeView;
    end;
    InitControl;
  end;
end;

function TTreeViewPresenter.ToggleCheckBox(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
begin
  Result := False;
  if Assigned(ColumnDefinitions) and (FTreeView.FocusedColumn > -1)
    and (FTreeView.FocusedColumn < ColumnDefinitions.Count) then
  begin
    LItem := GetNodeItem(FTreeView, FTreeView.FocusedNode);
    LItemTemplate := GetItemTemplate(LItem);
    LColumnDefinition := ColumnDefinitions[FTreeView.FocusedColumn];

    if Assigned(LItemTemplate) and LColumnDefinition.AllowEdit
      and (LColumnDefinition.ColumnType = TColumnType.ctCheckBox) then
    begin
      LItemTemplate.SetValue(LItem, FTreeView.FocusedColumn,
        not LItemTemplate.GetValue(LItem, FTreeView.FocusedColumn).AsBoolean);
      Result := True;
    end;
  end;
end;

procedure TTreeViewPresenter.ToggleIcon(Node: PVirtualNode;
  Column: TColumnIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
  //LValue: TValue;

  //procedure ToggleValue;
  //begin
  //  if LValue.AsOrdinal < LValue.TypeData.MaxValue then
  //  begin
  //    TValue.Make(LValue.AsOrdinal + 1, LValue.TypeInfo, LValue);
  //  end
  //  else
  //  begin
  //    TValue.Make(LValue.TypeData.MinValue, LValue.TypeInfo, LValue);
  //  end;
  //end;

begin
  LItem := GetNodeItem(FTreeView, Node);
  LColumnDefinition := ColumnDefinitions[Column];
  if LColumnDefinition.ColumnType = ctImage then
  begin
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      //LValue := LItemTemplate.GetValue(LItem, Column);
      //if LValue.IsOrdinal then
      //begin
      //  ToggleValue;
      //  LItemTemplate.SetValue(LItem, Column, LValue);
      //end;
    end;
  end
  else
  begin
    //LColumnDefinition.ImageIndexPropertyExpression.Instance := LItem;
    //LValue := LColumnDefinition.ImageIndexPropertyExpression.Value;
    //if LValue.IsOrdinal then
    //begin
    //  ToggleValue;
    //  LColumnDefinition.ImageIndexPropertyExpression.Value := LValue;
    //end;
  end;
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  FCheckedItems.Clear;
  LNode := FTreeView.GetFirstChecked;
  while Assigned(LNode) do
  begin
    LItem := GetNodeItem(FTreeView, LNode);
    if Assigned(LItem) then
    begin
      FCheckedItems.Add(LItem);
    end;
    LNode := FTreeView.GetNextChecked(LNode);
  end;
end;

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  FExpandedItems.Clear;
  LNode := FTreeView.GetFirst;
  while Assigned(LNode) do
  begin
    LItem := GetNodeItem(FTreeView, LNode);
    if Assigned(LItem) and FTreeView.Expanded[LNode] then
    begin
      FExpandedItems.Add(LItem);
    end;
    LNode := FTreeView.GetNext(LNode);
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LItem: TObject;
  LSelectedNodes: TNodeArray;
begin
  FSelectedItems.Clear;
  LSelectedNodes := FTreeView.GetSortedSelection(False);
  for i := Low(LSelectedNodes) to High(LSelectedNodes) do
  begin
    LItem := GetNodeItem(FTreeView, LSelectedNodes[i]);
    if Assigned(LItem) then
    begin
      FSelectedItems.Add(LItem);
    end;
  end;
  DoPropertyChanged('CurrentItem');
  DoPropertyChanged('SelectedItem');
  DoPropertyChanged('SelectedItems');
end;

initialization
{$IFDEF Windows}
  CheckBoxSize := GetSystemMetrics(SM_CYMENUCHECK);
{$ENDIF}

end.
