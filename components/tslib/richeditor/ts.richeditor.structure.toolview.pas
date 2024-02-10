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

unit ts.RichEditor.Structure.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActiveX,

  VirtualTrees, KControls, KMemo,

  ts.Core.VTNode,
  ts.RichEditor.ToolView.Base, ts.RichEditor.Interfaces;

type
  TBlockNode = TVTNode<TKMemoBlock>;

  TStructureToolView = class(TCustomRichEditorToolView)
    imlMain : TImageList;

  private
    FTree        : TVirtualStringTree;
    FRootNode    : TVTNode<TKMemoBlock>;
    FRootData    : TKMemoBlock;
    FDraggedNode : TBlockNode;

    {$REGION 'property access methods'}
    function GetFocusedNode: TBlockNode;
    function GetKMemo: TKMemo;
    function GetKMemoNotifier: IKMemoNotifier;
    function GetSelectedBlock: TKMemoBlock;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure EditorChange(Sender: TObject);
    procedure EditorSelectBlock(Sender: TObject);

    procedure FTreeDragAllowed(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      Column      : TColumnIndex;
      var Allowed : Boolean
    );
    procedure FTreeDragDrop(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      DataObject : IDataObject;
      Formats    : TFormatArray;
      Shift      : TShiftState;
      const Pt   : TPoint;
      var Effect : LongWord;
      Mode       : TDropMode
    );
    procedure FTreeDragOver(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      Shift      : TShiftState;
      State      : TDragState;
      const Pt   : TPoint;
      Mode       : TDropMode;
      var Effect : LongWord;
      var Accept : Boolean
    );
    procedure FTreeGetHint(
      Sender             : TBaseVirtualTree;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      var LineBreakStyle : TVTTooltipLineBreakStyle;
      var HintText       : string
    );
    procedure FTreeGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : Integer
    );
    procedure FTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    {$ENDREGION}

  protected
    procedure CreateTreeView;

    function ActiveBlocks: TKMemoBlocks;
    function AddNodes(AKMemoBlock: TKMemoContainer): Boolean;
    function AddBlockToTree(
      ABlock  : TKMemoBlock;
      AParent : TKMemoBlock = nil
    ): TBlockNode;

    procedure BuildTreeView;
    procedure ClearTree;

    procedure UpdateActions; override;
    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property SelectedBlock: TKMemoBlock
      read GetSelectedBlock;

    property FocusedNode: TBlockNode
      read GetFocusedNode;

    property KMemo: TKMemo
      read GetKMemo;

    property KMemoNotifier: IKMemoNotifier
      read GetKMemoNotifier;
  end;

implementation

{$R *.lfm}

uses
  KGraphics,

  ts.RichEditor.Utils,
  ts.Core.Logger;

{$REGION 'construction and destruction'}
procedure TStructureToolView.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateTreeView;
  Manager.Events.AddOnChangeHandler(EditorChange);
  Manager.Events.AddOnSelectBlockHandler(EditorSelectBlock);
  BuildTreeView;
end;

destructor TStructureToolView.Destroy;
begin
  if Assigned(Manager) and Assigned(Manager.Events) then
  begin
    Manager.Events.RemoveOnChangeHandler(EditorChange);
    Manager.Events.RemoveOnSelectBlockHandler(EditorSelectBlock);
  end;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TStructureToolView.GetKMemo: TKMemo;
begin
  Result := View.Editor;
end;

function TStructureToolView.GetFocusedNode: TBlockNode;
begin
  Result := TVTNode<TKMemoBlock>(FTree.GetNodeData(FTree.FocusedNode)^);
end;

function TStructureToolView.GetKMemoNotifier: IKMemoNotifier;
begin
  Result := IKMemoNotifier(KMemo);
end;

function TStructureToolView.GetSelectedBlock: TKMemoBlock;
begin
  Result := KMemo.SelectedBlock;
end;
{$ENDREGION}

{$REGION 'event handlers'}

//var
//  N : TVTNode<TKMemoBlock>;
//begin
//  N := FRootNode.Find(ABlock);
//  if Assigned(N) and Assigned(N.VNode) then
//    N.Select;

procedure TStructureToolView.FTreeDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  FDraggedNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node));
  Allowed := True;
end;

procedure TStructureToolView.FTreeDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
//var
//  LAttachMode : TVTNodeAttachMode;
//  LNode       : TBlockNode;
begin
  //LNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Sender.GetNodeAt(Pt.x, Pt.y))^);
  //Sender.FocusedNode := LNode.VNode;
  //LNode.Data.ParentBlocks.AddAt(FDraggedNode.Data);
  //if Mode = dmOnNode then
  //  LAttachMode := amInsertBefore
  //else if Mode = dmAbove then
  //  LAttachMode := amInsertBefore
  //else if Mode = dmBelow then
  //  LAttachMode := amInsertAfter
  //else
  //  LAttachMode := amAddChildLast;
end;

procedure TStructureToolView.FTreeDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TStructureToolView.FTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  LNode : TBlockNode;
begin
  LNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
  if Assigned(LNode) and Assigned(LNode.Data) then
    HintText := LNode.Hint;
end;

procedure TStructureToolView.FTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LNode : TBlockNode;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    LNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
    if Assigned(LNode) and Assigned(LNode.Data) then
      ImageIndex := LNode.ImageIndex;
  end;
end;

procedure TStructureToolView.FTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  LVTNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
  LVTNode.Free;
end;

procedure TStructureToolView.FTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  LVTNode  := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
  if LVTNode.Text.IsEmpty then
  begin
    CellText := LVTNode.Data.ClassName;
  end
  else
  begin
    CellText := LVTNode.Text;
  end;
end;

procedure TStructureToolView.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LVTNode : TVTNode<TKMemoBlock>;
begin
  if Assigned(Node) then
  begin
    LVTNode := TVTNode<TKMemoBlock>(Sender.GetNodeData(Node)^);
    if Assigned(LVTNode) and (LVTNode <> FRootNode) and Assigned(LVTNode.Data) then
    begin
      KMemoNotifier.SelectBlock(LVTNode.Data, sgpNone);
    end;
  end;
end;

procedure TStructureToolView.EditorChange(Sender: TObject);
begin
  Logger.Enter(Self, 'EditorChange');
  Modified;
  Logger.Leave(Self, 'EditorChange');
end;

procedure TStructureToolView.EditorSelectBlock(Sender: TObject);
begin
  Logger.Enter(Self, 'EditorSelectBlock');
  Modified;
  Logger.Leave(Self, 'EditorSelectBlock');
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TStructureToolView.CreateTreeView;
begin
  FTree := TVirtualStringTree.Create(Self);
  FTree.Parent                       := Self;
  FTree.Align                        := alClient;
  FTree.Images                       := imlMain;
  FTree.HintMode                     := hmHintAndDefault;
  FTree.BorderStyle                  := bsNone;
  FTree.ShowHint                     := True;
  FTree.PopupMenu                    := View.PopupMenu;
  FTree.DoubleBuffered               := True;
  FTree.AnimationDuration            := 100;
  FTree.AutoExpandDelay              := 500;
  FTree.BorderSpacing.Left           := 0;
  FTree.BorderSpacing.Right          := 0;
  FTree.BorderSpacing.Top            := 0;
  FTree.BorderSpacing.Bottom         := 0;
  FTree.ButtonFillMode               := fmTransparent;
  FTree.Color                        := clWhite;
  FTree.Indent                       := 20;
  FTree.LineMode                     := lmBands;
  FTree.Colors.FocusedSelectionColor := clGray;
  FTree.Colors.HotColor              := clBlue;
  FTree.Header.AutoSizeIndex         := 0;
  FTree.Header.DefaultHeight         := 17;
  FTree.Header.Options               := [hoAutoResize, hoColumnResize, hoDrag];
  FTree.TreeOptions.AnimationOptions := [
    toAnimatedToggle,
    toAdvancedAnimatedToggle
  ];
  FTree.TreeOptions.AutoOptions := [
    toAutoDropExpand,
    toAutoScroll,
    toAutoScrollOnExpand,
    toAutoTristateTracking,
    toAutoChangeScale,
    toDisableAutoscrollOnEdit
  ];
  FTree.TreeOptions.MiscOptions := [
    toAcceptOLEDrop,
    toEditable,
    toFullRepaintOnResize,
    toInitOnSave,
    toReportMode,
    toToggleOnDblClick,
    toWheelPanning
  ];
  FTree.TreeOptions.PaintOptions := [
    toHotTrack,
    toPopupMode,
    toShowBackground,
    toShowButtons,
    toShowDropmark,
    toShowHorzGridLines,
    toShowRoot,
    toShowTreeLines,
    toShowVertGridLines,
    toThemeAware,
    toUseBlendedImages,
    toStaticBackground
  ];
  FTree.TreeOptions.SelectionOptions := [
    toMultiSelect,
    toCenterScrollIntoView,
    toAlwaysSelectNode
  ];
  FTree.OnFreeNode      := FTreeFreeNode;
  FTree.OnGetText       := FTreeGetText;
  FTree.OnFocusChanged  := FTreeFocusChanged;
  FTree.OnGetImageIndex := FTreeGetImageIndex;
  FTree.OnGetHint       := FTreeGetHint;
  FTree.OnDragAllowed   := FTreeDragAllowed;
  FTree.OnDragDrop      := FTreeDragDrop;
  FTree.OnDragOver      := FTreeDragOver;
  FRootData      := TKMemoBlock.Create;
  FRootNode      := TVTNode<TKMemoBlock>.Create(FTree, FRootData, False);
  FRootNode.Text := 'Root';
end;

function TStructureToolView.ActiveBlocks: TKMemoBlocks;
begin
  if Assigned(SelectedBlock) then
  begin
    if SelectedBlock is TKMemoContainer then
    begin
      Result := (SelectedBlock as TKMemoContainer).Blocks;
    end
    else
    begin
      Result := SelectedBlock.ParentBlocks;
    end;
  end
  else
  begin
    Result := KMemo.ActiveBlocks;
  end;
end;

function TStructureToolView.AddNodes(AKMemoBlock: TKMemoContainer): Boolean;
var
  I      : Integer;
  LBlock : TKMemoBlock;
  LTable : TKMemoTable;
  LRow   : TKMemoTableRow;
  LCell  : TKMemoTableCell;
begin
  Result := True;
  if AKMemoBlock is TKMemoTable then
  begin
    LTable := AKMemoBlock as TKMemoTable;
    for I := 0 to LTable.RowCount - 1 do
    begin
      LRow := LTable.Rows[I];
      AddBlockToTree(LRow, LTable);
      AddNodes(LRow);
    end;
  end
  else if AKMemoBlock is TKMemoTableRow then
  begin
    LRow := AKMemoBlock as TKMemoTableRow;
    for I := 0 to LRow.CellCount - 1 do
    begin
      LCell := LRow.Cells[I];
      AddBlockToTree(LCell, LRow);
      AddNodes(LCell);
    end;
  end
  else if AKMemoBlock is TKMemoTableCell then
  begin
    LCell := AKMemoBlock as TKMemoTableCell;
    for I := 0 to LCell.Blocks.Count - 1 do
    begin
      LBlock := LCell.Blocks[I];
      AddBlockToTree(LBlock, LCell);
      if LBlock is TKMemoContainer then
        AddNodes(LBlock as TKMemoContainer);
    end;
  end
  else
  begin
    for I := 0 to AKMemoBlock.Blocks.Count - 1 do
    begin
      LBlock := AKMemoBlock.Blocks.Items[I];
      AddBlockToTree(LBlock, AKMemoBlock);
      if LBlock is TKMemoContainer then
        AddNodes(LBlock as TKMemoContainer);
    end;
  end;
end;

function TStructureToolView.AddBlockToTree(ABlock: TKMemoBlock;
  AParent: TKMemoBlock): TBlockNode;
var
  LNode : TBlockNode;
  LNew  : TBlockNode;
  LHint : string;
begin
  Result := nil;
  LNode  := nil;
  if Assigned(ABlock) then
  begin
    if Assigned(AParent) then
    begin
      LNode := FRootNode.Find(AParent);
    end;
    if not Assigned(LNode) then
    begin
      LNode := FRootNode;
    end;
    LNew := LNode.Add(ABlock, False);
    LHint := Format('%s'#13#10, [ABlock.ClassName]) + BlockInfoString(ABlock);

    if ABlock is TKMemoParagraph then
    begin
      LNew.ImageIndex := 0;
      LNew.Text := ABlock.Text;
      if Assigned((ABlock as TKMemoParagraph).NumberBlock) then
      begin
        LNew.Text := (ABlock as TKMemoParagraph).NumberBlock.Text + LNew.Text;
        LHint := LHint + LNew.Text;
      end;
    end
    else if ABlock is TKMemoHyperlink then
    begin
      LNew.ImageIndex := 3;
      LNew.Text := (ABlock as TKMemoHyperlink).URL;
    end
    else if ABlock is TKMemoTextBlock then
    begin
      LNew.ImageIndex := 1;
      LNew.Text := ABlock.Text;
    end
    else if ABlock is TKMemoImageBlock then
    begin
      LNew.ImageIndex := 2;
      LNew.Text := Format('%d x %d', [ABlock.Width, ABlock.Height]);
      LHint := LHint + ImageInfoString(ABlock as TKMemoImageBlock);
    end
    else if ABlock is TKMemoTable then
    begin
      LNew.ImageIndex := 5;
      LNew.Text := TableInfoString(ABlock as TKMemoTable);
    end
    else if ABlock is TKMemoTableRow then
    begin
      LNew.ImageIndex := 6;
      LNew.Text := TableRowInfoString(ABlock as TKMemoTableRow);
    end
    else if ABlock is TKMemoTableCell then
    begin
      LNew.ImageIndex := 7;
      LNew.Text := TableCellInfoString(ABlock as TKMemoTableCell);
    end
    else if ABlock is TKMemoContainer then
    begin
      LNew.ImageIndex := 4;
    end;
    LNew.Hint := LHint;
    Result := LNew;
  end;
  Modified;
end;

{ Rebuilds the treeview and selects the corresponding node for the selected
  block in the editor. }

procedure TStructureToolView.BuildTreeView;
var
  I      : Integer;
  LBlock : TKMemoBlock;
  N      : TBlockNode;
begin
  FTree.BeginUpdate;
  try
    KMemo.LockUpdate;
    try
      ClearTree;
      if KMemo.Blocks.Count > 0 then
      begin
        for I := 0 to KMemo.Blocks.Count - 1 do
        begin
          LBlock := KMemo.Blocks.Items[I];
          AddBlockToTree(LBlock);
          if LBlock is TKMemoContainer then
            AddNodes(LBlock as TKMemoContainer);
        end;
        LBlock := SelectedBlock;
        if not Assigned(LBlock) then
          LBlock := KMemo.ActiveBlock;
        N := FRootNode.Find(LBlock);
      end;
      FRootNode.Expand;
    finally
      KMemo.UnlockUpdate;
    end;
  finally
    FTree.EndUpdate;
    if Assigned(N) then
      N.Select;
  end;
end;

procedure TStructureToolView.ClearTree;
begin
  FTree.Clear;
  FRootNode := TVTNode<TKMemoBlock>.Create(FTree, FRootData, False);
end;

procedure TStructureToolView.UpdateActions;
begin
  if Update then
  begin
    UpdateView;
    Update := False;
  end;
  inherited UpdateActions;
end;

procedure TStructureToolView.UpdateView;
begin
  BuildTreeView;
end;
{$ENDREGION}

end.
