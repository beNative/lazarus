unit ts_Core_Helpers;

{$mode delphi}

{ Some helpers to create controls with commonly used settings. }

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, Contnrs,

  RTTIGrids, RTTICtrls,

  PropEdits,

  VirtualTrees,

  ts_Core_TreeViewPresenter, ts_Core_DataTemplates;

function CreateVST(
  AOwner  : TComponent;
  AParent : TWinControl
): TVirtualStringTree;

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

//*****************************************************************************

implementation

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
  VST.Header.Options := VST.Header.Options + [hoAutoSpring, hoAutoResize];
  Result := VST;
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
  GlobalDesignHook      := PI.PropertyEditorHook;
  TLocalClass.Inspector := PI;
  PI.PropertyEditorHook.AddHandlerSetSelection(TLocalClass.OnSetSelection);
  Result := PI;
end;

end.

