unit ts_Core_Helpers;

{$mode delphi}

{ Some helpers to create controls with commonly used settings. }

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, Contnrs, ExtCtrls, Forms,

  RTTIGrids, RTTICtrls,

  PropEdits,

  VirtualTrees, XMLTree,

  ts_Core_TreeViewPresenter, ts_Core_DataTemplates;

function CreateVST(
  AOwner  : TComponent;
  AParent : TWinControl
): TVirtualStringTree;

function CreateXMLTree(
  AOwner  : TComponent;
  AParent : TWinControl
): TXMLTree;

{ Cannot be used yet because the assignment sequence seems to matter. }

function CreateTVP(
  AOwner        : TComponent;
  AVST          : TVirtualStringTree;
  AItemsSource  : TObjectList = nil;
  AItemTemplate : IDataTemplate = nil
): TTreeViewPresenter;

function CreatePI(
  AOwner  : TComponent;
  AParent : TWinControl
): TTIPropertyGrid;

procedure AssignFormParent(
  AForm   : TCustomForm;
  AParent : TCustomControl
);

//*****************************************************************************

implementation

uses
  TypInfo, ObjectInspector;

type
  TLocalClass = class
  strict private
    class var FInspector: TTIPropertyGrid;

  private
    class procedure OnSetSelection(const ASelection: TPersistentSelectionList);

    class property Inspector: TTIPropertyGrid
      read FInspector write FInspector;
  end;

class procedure TLocalClass.OnSetSelection(const ASelection: TPersistentSelectionList);
begin
  if ASelection.Count > 0 then
  begin
    Inspector.TIObject := ASelection.Items[0];
  end;
end;

function CreateVST(AOwner: TComponent; AParent: TWinControl): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  VST          := TVirtualStringTree.Create(AOwner);
  VST.Parent   := AParent;
  VST.HintMode := hmTooltip;
  VST.Align    := alClient;
  VST.Header.Height := 18;
  VST.Header.Options := [
//    hoAutoResize,
    hoAutoSpring,
    hoColumnResize,
    hoDblClickResize,
    hoDisableAnimatedResize,
//    hoDrag,
//    hoFullRepaintOnResize,
    hoShowSortGlyphs,
    hoVisible
  ];
  VST.TreeOptions.SelectionOptions := [
    toExtendedFocus
  ];
  VST.TreeOptions.MiscOptions := [
//    toAcceptOLEDrop,
//    toEditable,
//    toEditOnClick,
//    toFullRepaintOnResize,
//    toEditOnDblClick,
    toInitOnSave,
    toToggleOnDblClick,
    toVariableNodeHeight,
    toWheelPanning
  ];
  VST.TreeOptions.PaintOptions := [
    toHideFocusRect,
    toPopupMode,
    toShowButtons,
    toShowDropmark,
    toShowHorzGridLines,
    toShowBackground,
    toStaticBackground,
    toShowRoot,
    toShowTreeLines,
    toShowVertGridLines,
    toThemeAware,
    toUseBlendedImages,
    toUseBlendedSelection
  ];
  VST.DragType := dtVCL; // dtOLE does not work yet
  Result := VST;
end;

function CreateXMLTree(AOwner: TComponent; AParent: TWinControl): TXMLTree;
var
  XT: TXMLTree;
begin
  XT := TXMLTree.Create(AOwner);
  XT.Parent := AParent;
  XT.Align := alClient;
  Result := XT;
end;

function CreateTVP(AOwner: TComponent; AVST: TVirtualStringTree;
  AItemsSource: TObjectList; AItemTemplate: IDataTemplate): TTreeViewPresenter;
var
  TVP : TTreeViewPresenter;
begin
  TVP              := TTreeViewPresenter.Create(AOwner);
  TVP.ListMode     := True;
  TVP.MultiSelect  := False;
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
  PI.Parent             := AParent;
  PI.Align              := alClient;
  PI.DefaultItemHeight  := 17;
  PI.PreferredSplitterX := 160;
  PI.SplitterX          := 160;
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
    AForm.Visible := True;
    AForm.EndUpdateBounds;
    AParent.EndUpdateBounds;
  end;
end;

end.

