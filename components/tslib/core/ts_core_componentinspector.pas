{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Core_ComponentInspector;

{$MODE Delphi}

{ Author: Tim Sinaeve
  Lazarus version with native RTTI grid
}

// TODO :
//  - store every component inspector form that is created in a component list.

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Contnrs, TypInfo,

  LCLIntf, LMessages,

  RTTIGrids, PropEdits;

type
  TComponentInspectorForm = class(TForm)
    cbxInspector : TComboBox;
    pnlMain      : TPanel;

    procedure cbxInspectorChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);

  private
    FInspector: TTIPropertyGrid;
    // message handlers
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;

    // property access methods
    function GetActiveItem: TPersistent;
    function GetPropertyEditorHook: TPropertyEditorHook;
    procedure SetActiveItem(const AValue: TPersistent);

    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnModified(Sender: TObject);
    procedure OnAddDependency(
      const AClass: TClass;
      const AnUnitName: shortstring
    );

    function OnBeforeAddPersistent(
      Sender           : TObject;
      APersistentClass : TPersistentClass;
      Parent           : TPersistent
    ): Boolean;
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnPersistentAdded(
      APersistent : TPersistent;
      Select      : Boolean
    );
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnDeletePersistent(var APersistent: TPersistent);

  protected
    procedure UpdateActions; override;
    
  public
    procedure AfterConstruction; override;

    procedure AddComponentToInspector(AComponent: TPersistent); virtual;
    procedure FocusComponentInInspector(AComponent: TPersistent); virtual;

    property ActiveItem: TPersistent
      read GetActiveItem write SetActiveItem;

    property PropertyEditorHook: TPropertyEditorHook
      read GetPropertyEditorHook;
  end;

procedure InspectComponent(AComponent : TComponent);

procedure InspectComponents(AComponent : TComponent); overload;

procedure InspectApplicationComponents;

procedure InspectComponents(AComponents : array of TComponent); overload;

procedure InspectComponents(AComponents : TComponentList); overload;

implementation

{$R *.lfm}

uses
  LCLType,

  ts.Core.Utils;

const
  COMBOBOX_FORMAT = '%s %s';


procedure InspectComponent(AComponent : TComponent);
var
  F : TComponentInspectorForm;
begin
  if Assigned(AComponent) then
  begin
    F := TComponentInspectorForm.Create(Application);
    F.AddComponentToInspector(AComponent);
    F.Show;
  end
  else
    raise Exception.Create('No component Assigned');
end;

procedure InspectComponents(AComponents : array of TComponent);
var
  F : TComponentInspectorForm;
  C : TComponent;
begin
  if Length(AComponents) > 0 then
  begin
    F := TComponentInspectorForm.Create(Application);
    for C in AComponents do
      F.AddComponentToInspector(C);
    F.Show;
    F.FocusComponentInInspector(AComponents[0]);
  end
  else
    raise Exception.Create('Component array is empty');
end;

procedure InspectComponents(AComponents : TComponentList);
var
  F : TComponentInspectorForm;
  C : TComponent;
begin
  if Assigned(AComponents) then
  begin
    if AComponents.Count > 0 then
    begin
      F := TComponentInspectorForm.Create(Application);
      for C in AComponents do
        F.AddComponentToInspector(C);
    F.Show;
    F.FocusComponentInInspector(AComponents[0]);
  end
  end
  else
    raise Exception.Create('Componentlist not assigned');
end;

procedure InspectComponents(AComponent : TComponent);
var
  CL : TComponentList;
  I  : Integer;
begin
  CL := TComponentList.Create(False);
  try
    for I := 0 to AComponent.ComponentCount - 1 do
      CL.Add(AComponent.Components[I]);
    InspectComponents(CL);
  finally
    CL.Free;
  end;
end;

procedure InspectApplicationComponents;
var
  CL : TComponentList;
  I  : Integer;
  J  : Integer;
begin
  CL := TComponentList.Create(False);
  try
    for I := 0 to Screen.FormCount - 1 do
      for J :=  0 to Screen.Forms[I].ComponentCount - 1 do
        CL.Add(Screen.Forms[I].Components[J]);

    for I := 0 to Screen.DataModuleCount - 1 do
      for J :=  0 to Screen.DataModules[I].ComponentCount - 1 do
        CL.Add(Screen.DataModules[I].Components[J]);

    InspectComponents(CL);
  finally
    CL.Free;
  end;
end;

procedure TComponentInspectorForm.AfterConstruction;
var
  M : TMonitor;
begin
  inherited AfterConstruction;
  FInspector := TTIPropertyGrid.Create(Self);
  FInspector.Parent := pnlMain;
  FInspector.Align := alClient;
  FInspector.DefaultItemHeight := 17;
  FInspector.PreferredSplitterX := 150;
  FInspector.SplitterX := 150;
  FInspector.DoubleBuffered := True;
  FInspector.Filter := [
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
    tkInterface,
    tkClass,
    tkObject,
    tkWChar,
    tkBool,
    tkInt64,
    tkQWord,
    tkDynArray,
    tkUString,
    tkUChar,
    tkHelper
  ];
  GlobalDesignHook := PropertyEditorHook;

  PropertyEditorHook.AddHandlerSetSelection(OnSetSelection);
  PropertyEditorHook.AddHandlerModified(OnModified);
  PropertyEditorHook.AddHandlerBeforeAddPersistent(OnBeforeAddPersistent);
  PropertyEditorHook.AddHandlerPersistentAdded(OnPersistentAdded);
  PropertyEditorHook.AddHandlerDeletePersistent(OnDeletePersistent);
  PropertyEditorHook.AddHandlerAddDependency(OnAddDependency);
  PropertyEditorHook.AddHandlerComponentRenamed(OnComponentRenamed);
  PropertyEditorHook.AddHandlerPersistentDeleting(OnPersistentDeleting);

  DoubleBuffered := True;

  M      := Screen.MonitorFromWindow(Application.MainFormHandle);
  Top    := 0;
  Left   := M.Left;
{$IFDEF windows}
  Height := Screen.WorkAreaHeight - TaskbarHeight;
{$endif}
end;



function TComponentInspectorForm.GetActiveItem: TPersistent;
begin
  Result := FInspector.TIObject;
end;

procedure TComponentInspectorForm.SetActiveItem(const AValue: TPersistent);
begin
  if AValue <> ActiveItem then
  begin
    FocusComponentInInspector(AValue);
  end;
end;

function TComponentInspectorForm.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result := FInspector.PropertyEditorHook;
end;



procedure TComponentInspectorForm.CMDialogKey(var Msg: TCMDialogKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else
    inherited;
end;



procedure TComponentInspectorForm.OnSetSelection(const ASelection: TPersistentSelectionList);
begin
  if ASelection.Count > 0 then
  begin
    AddComponentToInspector(ASelection.Items[0]);
    FocusComponentInInspector(ASelection.Items[0]);
  end;
end;

procedure TComponentInspectorForm.OnModified(Sender: TObject);
begin

end;

procedure TComponentInspectorForm.OnAddDependency(const AClass: TClass; const AnUnitName: shortstring);
begin

end;

function TComponentInspectorForm.OnBeforeAddPersistent(Sender: TObject; APersistentClass: TPersistentClass; Parent: TPersistent): boolean;
begin
  Result := False;
end;

procedure TComponentInspectorForm.OnComponentRenamed(AComponent: TComponent);
begin

end;

procedure TComponentInspectorForm.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
begin

end;

procedure TComponentInspectorForm.OnPersistentDeleting(APersistent: TPersistent);
begin

end;

procedure TComponentInspectorForm.OnDeletePersistent(var APersistent: TPersistent);
begin

end;

procedure TComponentInspectorForm.cbxInspectorChange(Sender: TObject);
begin
  if Assigned(cbxInspector.Items.Objects[cbxInspector.ItemIndex]) then
    FInspector.TIObject := cbxInspector.Items.Objects[cbxInspector.ItemIndex] as
      TPersistent;
end;

procedure TComponentInspectorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TComponentInspectorForm.FormResize(Sender: TObject);
begin
  FInspector.SplitterX := FInspector.ClientWidth div 2;
end;

procedure TComponentInspectorForm.FormShow(Sender: TObject);
begin
  if cbxInspector.Items.Count > 0 then
  begin
    cbxInspector.ItemIndex := 0;
    FocusComponentInInspector(cbxInspector.Items.Objects[0] as TPersistent);
  end;
end;



procedure TComponentInspectorForm.UpdateActions;
begin
  if not Focused and not Active then
  begin
    FInspector.RefreshPropertyValues;
  end;
  inherited UpdateActions;
end;



procedure TComponentInspectorForm.AddComponentToInspector(
  AComponent: TPersistent);
var
  S     : string;
  sName : string;
begin
  if AComponent is TComponent then
  begin
    sName := TComponent(AComponent).Name;
    if sName = '' then
      sName := 'unnamed';
    S := Format(COMBOBOX_FORMAT, [sName, AComponent.ClassName])
  end
  else
    S := Format('%s[%d]', [AComponent.ClassName,
      (AComponent as TCollectionItem).Index]);

  if cbxInspector.Items.IndexOfObject(AComponent) = -1 then
    cbxInspector.Items.AddObject(S, AComponent);
end;

procedure TComponentInspectorForm.FocusComponentInInspector(
  AComponent: TPersistent);
var
  PSL: TPersistentSelectionList;
begin
  FInspector.TIObject := AComponent;
  PropertyEditorHook.LookupRoot := AComponent;
  PSL := TPersistentSelectionList.Create;
  try
    PSL.Add(AComponent);
    FInspector.Selection := PSL;
  finally
    FreeAndNil(PSL);
  end;
end;


//*****************************************************************************

end.
