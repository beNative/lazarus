{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.Helpers;

{$MODE DELPHI}

{ Collection of factory types to create components with some common settings. }

interface

uses
  Classes, SysUtils, Controls, Contnrs, ExtCtrls, Forms,

  RTTIGrids, RTTICtrls,

  PropEdits,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.DataTemplates;

type

  { VST }

  { factory singleton to create TVirtualStringTree instances with common
    settings }

  VST = record
  strict private class var
    FHeaderOptions    : TVTHeaderOptions;
    FAutoOptions      : TVTAutoOptions;
    FStringOptions    : TVTStringOptions;
    FMiscOptions      : TVTMiscOptions;
    FPaintOptions     : TVTPaintOptions;
    FSelectionOptions : TVTSelectionOptions;
    FAnimationOptions : TVTAnimationOptions;

 strict private const
   {$REGION 'default VST options'}
    DEFAULT_VST_HEADEROPTIONS = [
    { Adjust a column so that the header never exceeds the client width of the
      owner control. }
    hoAutoResize,
    { Resizing columns with the mouse is allowed. }
    hoColumnResize,
    { Allows a column to resize itself to its largest entry. }
    hoDblClickResize,
    { Dragging columns is allowed. }
//    hoDrag,
    { Header captions are highlighted when mouse is over a particular column. }
//    hoHotTrack,
    { Header items with the owner draw style can be drawn by the application
      via event. }
//    hoOwnerDraw,
    { Header can only be dragged horizontally. }
//    hoRestrictDrag,
    { Show application defined header hint. }
    hoShowHint,
    { Show header images. }
    hoShowImages,
    { Allow visible sort glyphs. }
//    hoShowSortGlyphs,
    { Distribute size changes of the header to all columns, which are sizable
      and have the coAutoSpring option enabled. hoAutoResize must be enabled
      too. }
//    hoAutoSpring,
    { Fully invalidate the header (instead of subsequent columns only) when a
      column is resized. }
    hoFullRepaintOnResize,
    { Disable animated resize for all columns. }
    hoDisableAnimatedResize,
    { Allow resizing header height via mouse. }
//    hoHeightResize,
    { Allow the header to resize itself to its default height. }
//    hoHeightDblClickResize
    { Header is visible. }
    hoVisible
    { Clicks on the header will make the clicked column the SortColumn or toggle
      sort direction if it already was the sort column }
    // hoHeaderClickAutoSort
  ];
    DEFAULT_VST_SELECTIONOPTIONS = [
      { Prevent user from selecting with the selection rectangle in multiselect
        mode. }
  //    toDisableDrawSelection,
      {  Entries other than in the main column can be selected, edited etc. }
      toExtendedFocus
      { Hit test as well as selection highlight are not constrained to the text
        of a node. }
  //    toFullRowSelect,
      { Constrain selection to the same level as the selection anchor. }
  //    toLevelSelectConstraint,
      { Allow selection, dragging etc. with the middle mouse button. This and
        toWheelPanning are mutual exclusive. }
  //    toMiddleClickSelect,
      { Allow more than one node to be selected. }
  //    toMultiSelect,
      {  Allow selection, dragging etc. with the right mouse button. }
  //    toRightClickSelect,
      { Constrain selection to nodes with same parent. }
  //    toSiblingSelectConstraint,
      { Center nodes vertically in the client area when scrolling into view. }
  //    toCenterScrollIntoView
      { Simplifies draw selection, so a node's caption does not need to intersect
        with the selection rectangle. }
  //    toSimpleDrawSelection
    ];
    DEFAULT_VST_MISCOPTIONS = [
      { Register tree as OLE accepting drop target }
  //    toAcceptOLEDrop,
      { Show checkboxes/radio buttons. }
  //    toCheckSupport,
      { Node captions can be edited. }
  //    toEditable,
      { Fully invalidate the tree when its window is resized
        (CS_HREDRAW/CS_VREDRAW). }
  //    toFullRepaintOnResize,
      { Use some special enhancements to simulate and support grid behaviour. }
      toGridExtensions,
      { Initialize nodes when saving a tree to a stream. }
      toInitOnSave,
      { Tree behaves like TListView in report mode. }
  //    toReportMode,
      { Toggle node expansion state when it is double clicked. }
      toToggleOnDblClick,
      { Support for mouse panning (wheel mice only). This option and
        toMiddleClickSelect are mutal exclusive, where panning has precedence. }
      toWheelPanning,
      { The tree does not allow to be modified in any way. No action is executed
        and node editing is not possible. }
      //toReadOnly,
      { When set then GetNodeHeight will trigger OnMeasureItem to allow variable
        node heights. }
      toVariableNodeHeight,
      { Start node dragging by clicking anywhere in it instead only on the
        caption or image. Must be used together with toDisableDrawSelection. }
  //    toFullRowDrag,
      { Allows changing a node's height via mouse. }
  //    toNodeHeightResize,
      { Allows to reset a node's height to FDefaultNodeHeight via a double click. }
  //    toNodeHeightDblClickResize,
      { Editing mode can be entered with a single click }
  //    toEditOnClick,
      { Editing mode can be entered with a double click }
      toEditOnDblClick
    ];
    DEFAULT_VST_PAINTOPTIONS = [
      { Avoid drawing the dotted rectangle around the currently focused node. }
      toHideFocusRect,
      { Paint tree as would it always have the focus }
      toPopupMode,
      { Display collapse/expand buttons left to a node. }
      toShowButtons,
      { Show the dropmark during drag'n drop operations. }
      toShowDropmark,
      { Display horizontal lines to simulate a grid. }
  //    toShowHorzGridLines,
      { Use the background image if there's one. }
      toShowBackground,
      { Show static background instead of a tiled one. }
      toStaticBackground,
      { Show lines also at top level (does not show the hidden/internal root
        node). }
      toShowRoot,
      { Display tree lines to show hierarchy of nodes. }
      toShowTreeLines,
      { Display vertical lines (depending on columns) to simulate a grid. }
  //    toShowVertGridLines,
      { Draw UI elements (header, tree buttons etc.) according to the current
        theme if enabled (Windows XP+ only, application must be themed). }
      toThemeAware,
      { Enable alpha blending for ghosted nodes or those which are being
        cut/copied. }
      toUseBlendedImages,
      { Enable alpha blending for node selections. }
      toUseBlendedSelection
    ];
    DEFAULT_VST_STRINGOPTIONS = [
      { If set then the caption is automatically saved with the tree node,
        regardless of what is saved in the user data. }
  //    toSaveCaptions,
      { Show static text in a caption which can be differently formatted than the
        caption but cannot be edited. }
  //    toShowStaticText,
      { Automatically accept changes during edit if the user finishes editing
        other then VK_RETURN or ESC. If not set then changes are cancelled. }
      toAutoAcceptEditChange
    ];
    DEFAULT_VST_ANIMATIONOPTIONS = [
      { Expanding and collapsing a node is animated (quick window scroll). }
      toAnimatedToggle,
      { Do some advanced animation effects when toggling a node. }
      toAdvancedAnimatedToggle
    ];
    DEFAULT_VST_AUTOOPTIONS = [
      { Expand node if it is the drop target for more than a certain time. }
      toAutoDropExpand,
      { Nodes are expanded (collapsed) when getting (losing) the focus. }
  //    toAutoExpand,
      { Scroll if mouse is near the border while dragging or selecting. }
      toAutoScroll,
      { Scroll as many child nodes in view as possible after expanding a node. }
      toAutoScrollOnExpand,
      { Sort tree when Header.SortColumn or Header.SortDirection change or sort
        node if child nodes are added. }
      toAutoSort,
      { Large entries continue into next column(s) if there's no text in them
        (no clipping). }
  //    toAutoSpanColumns,
      { Checkstates are automatically propagated for tri state check boxes. }
      toAutoTristateTracking,
      { Node buttons are hidden when there are child nodes, but all are invisible.}
  //    toAutoHideButtons,
      { Delete nodes which where moved in a drag operation (if not directed
        otherwise). }
      toAutoDeleteMovedNodes,
      { Disable scrolling a node or column into view if it gets focused. }
  //    toDisableAutoscrollOnFocus,
      { Change default node height automatically if the system's font scale is
        set to big fonts. }
      toAutoChangeScale,
      { Frees any child node after a node has been collapsed (HasChildren flag
        stays there). }
  //    toAutoFreeOnCollapse,
      { Do not center a node horizontally when it is edited. }
      toDisableAutoscrollOnEdit,
      { When set then columns (if any exist) will be reordered from lowest index
        to highest index and vice versa when the tree's bidi mode is changed. }
      toAutoBidiColumnOrdering
    ];
    {$ENDREGION}

  public
    class property HeaderOptions: TVTHeaderOptions
      read FHeaderOptions write FHeaderOptions;

    class property SelectionOptions: TVTSelectionOptions
      read FSelectionOptions write FSelectionOptions;

    class property PaintOptions: TVTPaintOptions
      read FPaintOptions write FPaintOptions;

    class property MiscOptions: TVTMiscOptions
      read FMiscOptions write FMiscOptions;

    class property AutoOptions: TVTAutoOptions
      read FAutoOptions write FAutoOptions;

    class property StringOptions: TVTStringOptions
      read FStringOptions write FStringOptions;

    class constructor Create;

    class function Create(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TVirtualStringTree; static;
  end;

  { PI }

  PI = record
  public
    class function Create(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TTIPropertyGrid; static;
  end;

{ Cannot be used yet because the assignment sequence seems to matter. }

function CreateTVP(
  AOwner        : TComponent;
  AVST          : TVirtualStringTree;
  AItemsSource  : TObjectList = nil;
  AItemTemplate : IDataTemplate = nil
): TTreeViewPresenter;

{ Create a TTIPropertyGrid instance to be used as a property inspector. }

function CreatePI(
  AOwner  : TComponent;
  AParent : TWinControl
): TTIPropertyGrid;

{ Make the given form a child control of a given other visual control (in most
  cases this will be a TPanel). }

procedure AssignFormParent(
  AForm   : TCustomForm;
  AParent : TCustomControl
);

implementation

uses
  Graphics, TypInfo,

  ObjectInspector;

{$REGION 'TLocalClass'}
type
  TLocalClass = class
  strict private
    class var FInspector: TTIPropertyGrid;

  private
    class procedure OnSetSelection(const ASelection: TPersistentSelectionList);

    class property Inspector: TTIPropertyGrid
      read FInspector write FInspector;
  end;

{ PI }

class function PI.Create(AOwner: TComponent;
  AParent: TWinControl): TTIPropertyGrid;
var
  PI: TTIPropertyGrid;
begin
  PI := TTIPropertyGrid.Create(AOwner);
  PI.DoubleBuffered     := True;
  PI.BackgroundColor    := clWindow;
  PI.Color              := clForm;
  PI.DrawHorzGridLines  := False;
  PI.HighlightColor     := clGray;
  PI.GutterColor        := clForm;
  PI.GutterEdgeColor    := clSilver;
  PI.Parent             := AParent;
  PI.Align              := alClient;
  PI.DefaultItemHeight  := 23;
  PI.PreferredSplitterX := 200;
  PI.SplitterX          := 200;
  PI.Filter             := [
    tkInteger,
    tkChar,
    tkEnumeration,
    tkFloat,
    tkSet,
    tkSString,
    tkLString,
    tkAString,
    tkWString,
    tkVariant,
    tkArray,
    tkWChar,
    tkBool,
    tkInt64,
    tkQWord,
    tkDynArray,
    tkUString,
    tkUChar
  ];
  GlobalDesignHook      := PI.PropertyEditorHook;
  TLocalClass.Inspector := PI;
  PI.PropertyEditorHook.AddHandlerSetSelection(TLocalClass.OnSetSelection);
  PI.Layout := oilHorizontal;
  Result := PI;
end;

class procedure TLocalClass.OnSetSelection(const ASelection: TPersistentSelectionList);
begin
  if ASelection.Count > 0 then
  begin
    Inspector.TIObject := ASelection.Items[0];
  end;
end;
{$ENDREGION}

{ VST }

{$REGION 'VST'}
{ Class constructor sets default values for our factories' properties. }

class constructor VST.Create;
begin
  FHeaderOptions    := DEFAULT_VST_HEADEROPTIONS;
  FSelectionOptions := DEFAULT_VST_SELECTIONOPTIONS;
  FMiscOptions      := DEFAULT_VST_MISCOPTIONS;
  FStringOptions    := DEFAULT_VST_STRINGOPTIONS;
  FAutoOptions      := DEFAULT_VST_AUTOOPTIONS;
  FPaintOptions     := DEFAULT_VST_PAINTOPTIONS;
  FAnimationOptions := DEFAULT_VST_ANIMATIONOPTIONS;
end;

{ Factory method to create a TVirtualStringTree instance. }

class function VST.Create(AOwner: TComponent;
  AParent: TWinControl): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  VST          := TVirtualStringTree.Create(AOwner);
  VST.Parent   := AParent;
  VST.HintMode := hmTooltip;
  VST.Align    := alClient;
  VST.DrawSelectionMode := smBlendedRectangle;
  VST.Header.Height := 20;
  VST.Header.Font.Color := clWindowText;
  VST.SelectionBlendFactor := 128;
  VST.Font.Color := clWindowText; // for consistent looks with different themes
  VST.Colors.FocusedSelectionColor         := clSilver;
  VST.Colors.FocusedSelectionBorderColor   := clGray;
  VST.Colors.SelectionRectangleBorderColor := clGray;
  VST.Colors.SelectionRectangleBlendColor  := clSilver;
  VST.Colors.GridLineColor                 := clWindowFrame;
  VST.Header.Options               := FHeaderOptions;
  VST.TreeOptions.SelectionOptions := FSelectionOptions;
  VST.TreeOptions.MiscOptions      := FMiscOptions;
  VST.TreeOptions.PaintOptions     := FPaintOptions;
  VST.TreeOptions.StringOptions    := FStringOptions;
  VST.TreeOptions.AnimationOptions := FAnimationOptions;
  VST.TreeOptions.AutoOptions      := FAutoOptions;
  VST.ButtonStyle := bsTriangle;
  VST.ButtonFillMode := fmTransparent;
  Result := VST;
end;
{$ENDREGION}
function CreateTVP(AOwner: TComponent; AVST: TVirtualStringTree;
  AItemsSource: TObjectList; AItemTemplate: IDataTemplate): TTreeViewPresenter;
var
  TVP : TTreeViewPresenter;
begin
  TVP              := TTreeViewPresenter.Create(AOwner);
  TVP.ListMode     := True;
  TVP.MultiSelect  := False;
  TVP.AllowMove    := False;
  // if not assigned it will be autocreated and map the values to the published
  // properties of the objects in the ItemsSource list.
  if Assigned(AItemTemplate) then
    TVP.ItemTemplate := AItemTemplate;
  TVP.ItemsSource  := AItemsSource;
  Result := TVP;
end;

function CreatePI(AOwner: TComponent; AParent: TWinControl): TTIPropertyGrid;
var
  PI: TTIPropertyGrid;
begin
  PI := TTIPropertyGrid.Create(AOwner);
  PI.DoubleBuffered     := True;
  PI.BackgroundColor    := clWindow;
  PI.Color              := clForm;
  PI.DrawHorzGridLines  := False;
  PI.HighlightColor     := clGray;
  PI.GutterColor        := clForm;
  PI.GutterEdgeColor    := clSilver;
  PI.Parent             := AParent;
  PI.Align              := alClient;
  PI.DefaultItemHeight  := 23;
  PI.PreferredSplitterX := 200;
  PI.SplitterX          := 200;
  PI.Filter             := [
    tkInteger,
    tkChar,
    tkEnumeration,
    tkFloat,
    tkSet,
    tkSString,
    tkLString,
    tkAString,
    tkWString,
    tkVariant,
    tkArray,
    tkWChar,
    tkBool,
    tkInt64,
    tkQWord,
    tkDynArray,
    tkUString,
    tkUChar
  ];
  GlobalDesignHook      := PI.PropertyEditorHook;
  TLocalClass.Inspector := PI;
  PI.PropertyEditorHook.AddHandlerSetSelection(TLocalClass.OnSetSelection);
  PI.Layout := oilHorizontal;
  Result := PI;
end;

procedure AssignFormParent(AForm: TCustomForm; AParent: TCustomControl);
begin
  if AForm.Parent <> AParent then
  begin;
    AParent.BeginUpdateBounds;
    AForm.BeginUpdateBounds;
    AForm.Parent := AParent;
    AForm.BorderStyle := bsNone;
    AForm.Align := alClient;
    AForm.EndUpdateBounds;
    AParent.EndUpdateBounds;
  end;
end;

end.

