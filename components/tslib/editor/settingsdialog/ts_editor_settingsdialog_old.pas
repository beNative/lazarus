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

unit ts_Editor_SettingsDialog_Old;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ExtCtrls, ComCtrls, StdCtrls,
  Contnrs,

  PropEdits,

  RTTIGrids, RTTICtrls, ButtonPanel,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions,

  ts.Components.XMLTree,

  ts.Editor.Interfaces;

{
  TODO:
    - Associate file extensions
    - this can be implemented as a toolform with IEditorToolView support
}

type

  { TfrmEditorSettings }

  TfrmEditorSettings = class(TForm)
    {$region 'designer controls' /fold}
    aclMain                     : TActionList;
    actApplySettings: TAction;
    actAssociate: TAction;
    actReloadSettings: TAction;
    actOpenSettingsFile: TAction;
    btnApply: TButton;
    btnOpenSettingsFile: TButton;
    btnReloadSettings: TButton;
    btnAssociate: TButton;
    btnClose: TButton;
    btnOK: TButton;
    Label1: TLabel;
    lblAttributeAliases: TLabel;
    pnlHARightTop: TPanel;
    pnlHARightBottom: TPanel;
    pnlToolSettings: TPanel;
    pnlTSLeft: TPanel;
    pnlTSRight: TPanel;
    pnlTSRightBottom: TPanel;
    pnlHLRightTop: TPanel;
    imlMain                     : TImageList;
    pcMain                      : TPageControl;
    pnlBottom                   : TPanel;
    pnlHALeft                   : TPanel;
    pnlHLLeft: TPanel;
    pnlHARight                  : TPanel;
    pnlHLRight: TPanel;
    pnlHighlighterAttributes    : TPanel;
    pnlHighlighters: TPanel;
    pnlHLRightBottom: TPanel;
    pnlTSRightTop: TPanel;
    pnlPI                       : TPanel;
    pnlTop                      : TPanel;
    pnlXML: TPanel;
    splHAVertical               : TSplitter;
    splHLVertical: TSplitter;
    mmoAliasNames: TTIMemo;
    splHLVertical1: TSplitter;
    tsToolSettings: TTabSheet;
    tsHighlighters: TTabSheet;
    tsDebug: TTabSheet;
    tsXML: TTabSheet;
    tsHighlighterAttributes              : TTabSheet;
    tsSettings                  : TTabSheet;
    {$endregion}

    procedure actAssociateExecute(Sender: TObject);
    procedure actOpenSettingsFileExecute(Sender: TObject);
    procedure actReloadSettingsExecute(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FHATVPSelectionChanged(Sender: TObject);
    procedure FHLPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure FHLTVPSelectionChanged(Sender: TObject);
    procedure FPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure FTSTVPSelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure plObjectInspector1AddAvailPersistent(APersistent: TPersistent;
      var Allowed: boolean);
  private
    FHATVP   : TTreeViewPresenter; // HighlighterAttributes presenter
    FHLTVP   : TTreeViewPresenter; // Highlighters presenter
    FTSTVP   : TTreeViewPresenter; // ToolSettings presenter
    FHAList  : TObjectList;
    FHLList  : TObjectList;
    FTSList  : TObjectList;
    FPI      : TTIPropertyGrid;
    FHAPI    : TTIPropertyGrid;
    FHLPI    : TTIPropertyGrid;
    FSHLPI   : TTIPropertyGrid;
    FTSPI    : TTIPropertyGrid;
    FHAVST   : TVirtualStringTree;
    FHLVST   : TVirtualStringTree;
    FTSVST   : TVirtualStringTree;
    FXMLTree : TXMLTree;

    procedure SettingsChangedHandler(ASender: TObject);

    function GetSettings: IEditorSettings;
    function GetManager: IEditorManager;

  protected
    procedure UpdateControls;
    procedure UpdateData;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Execute: Boolean;
    procedure Apply;

    property Manager: IEditorManager
      read GetManager;

    property Settings: IEditorSettings
      read GetSettings;
  end;

procedure ExecuteSettingsDialog(
  AOwner: TComponent
);

implementation

{$R *.lfm}

uses
  StrUtils,

  ts.Core.DataTemplates, ts.Core.Helpers,

  ts.Components.FileAssociation,

  ts.Editor.HighlighterAttributes, ts.Editor.Highlighters,
  ts.Editor.Tools.Settings,

  ts.Core.SharedLogger;

resourcestring
  SAttributeName = 'Attribute name';
  SToolName      = 'Toolname';

const
  NAME_PROPERTY           = 'Name';
  TAG_PROPERTY            = 'Tag';
  SYNHIGHLIGHTER_PROPERTY = 'SynHighlighter';
  ALIASNAMES_PROPERTY     = 'AliasNames';
  ATTRIBUTES_PROPERTY     = 'Attributes';
  STYLE_PROPERTY          = 'Style';
  STYLEMASK_PROPERTY      = 'StyleMask';

var
  FForm: TfrmEditorSettings;

{$region 'TSynAttributesDataTemplate' /fold}
type
  TSynAttributesDataTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
  end;

function TSynAttributesDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := (Item as THighlighterAttributesItem).Name;
end;
{$endregion}

{$region 'THighlightersDataTemplate' /fold}
type
  THighlightersDataTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
  end;

function THighlightersDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := (Item as THighlighterItem).Highlighter;
end;
{$endregion}

{$region 'TToolSettingsDataTemplate' /fold}
type
  TToolSettingsDataTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
  end;

function TToolSettingsDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := (Item as TComponent).ClassName;
end;
{$endregion}

{$region 'interfaced methods' /fold}

procedure ExecuteSettingsDialog(AOwner: TComponent);
begin
  if not Assigned(FForm) then
    FForm := TfrmEditorSettings.Create(AOwner);
  FForm.Execute;
end;

{$endregion}

{$region 'construction and destruction' /fold}
procedure TfrmEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FHAList             := TObjectList.Create(False);
  FHATVP              := TTreeViewPresenter.Create(Self);
  FHATVP.ItemTemplate := TSynAttributesDataTemplate.Create;
  FHLList             := TObjectList.Create(False);
  FHLTVP              := TTreeViewPresenter.Create(Self);
  FHLTVP.ItemTemplate := THighlightersDataTemplate.Create;
  FTSList             := TObjectList.Create(False);
  FTSTVP              := TTreeViewPresenter.Create(Self);
  FTSTVP.ItemTemplate := TToolSettingsDataTemplate.Create;
  FPI                 := CreatePI(Self, pnlPI);
  FHAPI               := CreatePI(Self, pnlHARightBottom);
  FHLPI               := CreatePI(Self, pnlHLRightTop);
  FSHLPI              := CreatePI(Self, pnlHLRightBottom);
  FTSPI               := CreatePI(Self, pnlTSRightBottom);
  FHAVST              := VST.Create(Self, pnlHALeft);
  FHLVST              := VST.Create(Self, pnlHLLeft);
  FTSVST              := VST.Create(Self, pnlTSLeft);
  FXMLTree            := CreateXMLTree(Self, pnlXML);
  UpdateData;

  Settings.AddEditorSettingsChangedHandler(SettingsChangedHandler);
  UpdateControls;

  FHLPI.OnEditorFilter  := FHLPIEditorFilter;
  FPI.OnEditorFilter    := FPIEditorFilter;
  FHAPI.OnEditorFilter  := FPIEditorFilter;
  FSHLPI.OnEditorFilter := FPIEditorFilter;
  FTSPI.OnEditorFilter  := FPIEditorFilter;
  pcMain.ActivePageIndex := 0;
end;

procedure TfrmEditorSettings.BeforeDestruction;
begin
  FreeAndNil(FHAList);
  FreeAndNil(FHLList);
  FreeAndNil(FTSList);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmEditorSettings.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TfrmEditorSettings.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmEditorSettings.FHATVPSelectionChanged(Sender: TObject);
begin
  mmoAliasNames.Link.TIObject := FHATVP.CurrentItem as TPersistent;
  mmoAliasNames.Link.TIPropertyName := ALIASNAMES_PROPERTY;
  FHAPI.ExpandedProperties.Add(ATTRIBUTES_PROPERTY);
  FHAPI.ExpandedProperties.Add(ATTRIBUTES_PROPERTY + '.' + STYLE_PROPERTY);
  FHAPI.ExpandedProperties.Add(ATTRIBUTES_PROPERTY + '.' + STYLEMASK_PROPERTY);
  FHAPI.TIObject := FHATVP.CurrentItem as TPersistent;
end;

procedure TfrmEditorSettings.FTSTVPSelectionChanged(Sender: TObject);
begin
  if Assigned(FTSTVP.CurrentItem) then
  begin
    Logger.Send(FTSTVP.CurrentItem.ClassName, (FTSTVP.CurrentItem as TComponent).Name);
    FTSPI.TIObject := (FTSTVP.CurrentItem as TComponent);
  end;
end;

procedure TfrmEditorSettings.FHLPIEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  if AnsiMatchText(
    aEditor.GetName,
    [TAG_PROPERTY, NAME_PROPERTY, SYNHIGHLIGHTER_PROPERTY]
  ) then
    aShow := False;
end;

procedure TfrmEditorSettings.FHLTVPSelectionChanged(Sender: TObject);
begin
  FHLPI.TIObject := FHLTVP.CurrentItem as TPersistent;
  if Assigned(FHLTVP.CurrentItem) then
  begin
    if FHLTVP.CurrentItem is THighlighterItem then
      FSHLPI.TIObject := (FHLTVP.CurrentItem as THighlighterItem).SynHighlighter
  end;
end;

procedure TfrmEditorSettings.FPIEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  if AnsiMatchText(aEditor.GetName, [TAG_PROPERTY, NAME_PROPERTY]) then
    aShow := False;
end;

procedure TfrmEditorSettings.actOpenSettingsFileExecute(Sender: TObject);
var
  S: String;
begin
  S := ExtractFilePath(Application.ExeName) + Settings.FileName;
  Manager.OpenFile(S);
end;

{ TEMP: just a test! }
{ TODO -oTS : make dedicated association settings form }

procedure TfrmEditorSettings.actAssociateExecute(Sender: TObject);
var
  AR : TAppReg;
begin
  AR.AppName         := 'Notepas';
  AR.AppDescription  := 'Notepas text editor' ;
  AR.AppNameNoSpaces := 'Notepas';
  AR.ExtData         := '.pas .inc';
  AR.ExtIcon         := Application.ExeName + ',0';
  AR.ExtName         := 'Pascal source file';
  AR.ExtNameNoSpaces := 'PascalSourceFile';
  AR.CmdData         := Format('"%s"', [Application.ExeName]) + '"%1"';
  AR.CmdIcon         := Format('%s', [ExtractFilePath(Application.ExeName) + 'Notepas.ico']);
  AR.CmdName         := 'Edit with Notepas';
  AR.CmdNameNoSpaces :=  'Open';

  CreateFileAssociation(
    AR,
    True,
    True,
    True,
    True
  );
  ClearIconCache;
end;

procedure TfrmEditorSettings.actReloadSettingsExecute(Sender: TObject);
begin
  Settings.Load;
  Apply;
end;

procedure TfrmEditorSettings.btnApplyClick(Sender: TObject);
begin
  Apply;
end;

procedure TfrmEditorSettings.OKButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TfrmEditorSettings.plObjectInspector1AddAvailPersistent(
  APersistent: TPersistent; var Allowed: boolean);
begin
  Allowed := True;
end;

procedure TfrmEditorSettings.SettingsChangedHandler(ASender: TObject);
begin
  UpdateControls;
end;

{$endregion}

{$region 'protected methods' /fold}
procedure TfrmEditorSettings.UpdateControls;
begin
  tsDebug.TabVisible        := Settings.DebugMode;
  tsXML.TabVisible          := Settings.DebugMode;
  tsHighlighters.TabVisible := Settings.DebugMode;
  if Settings.DebugMode then
    FXMLTree.XML := Settings.XML;
end;

procedure TfrmEditorSettings.UpdateData;
var
  I: Integer;
begin
  FPI.TIObject := (Settings as IInterfaceComponentReference).GetComponent;
  FHATVP.MultiSelect := True;
  FHATVP.ColumnDefinitions.AddColumn(NAME, SAttributeName, dtString, 150);
  FHAList.Clear;
  for I := 0 to Settings.HighlighterAttributes.Count - 1 do
  begin
    FHAList.Add(Settings.HighlighterAttributes[I]);
  end;
  FHATVP.ItemsSource := FHAList;
  FHATVP.TreeView := FHAVST;
  FHATVP.OnSelectionChanged   := FHATVPSelectionChanged;

  FHLTVP.MultiSelect := True;
  FHLTVP.ColumnDefinitions.AddColumn(NAME, SAttributeName, dtString, 150);
  FHLList.Clear;
  for I := 0 to Settings.Highlighters.Count - 1 do
  begin
    FHLList.Add(Settings.Highlighters[I]);
  end;
  FHLTVP.ItemsSource := FHLList;
  FHLTVP.TreeView := FHLVST;
  FHLTVP.OnSelectionChanged := FHLTVPSelectionChanged;

  FTSTVP.MultiSelect := True;
  FTSTVP.ColumnDefinitions.AddColumn(NAME, SToolName, dtString, 150);
  FTSList.Clear;
  for I := 0 to Settings.ToolSettings.Count - 1 do
  begin
    Logger.Send(Settings.ToolSettings.Components[I].ClassName, TComponent(Settings.ToolSettings.Components[I]).Name);
    FTSList.Add(Settings.ToolSettings.Components[I]);
  end;
  FTSTVP.ItemsSource := FTSList;
  FTSTVP.TreeView := FTSVST;
  FTSTVP.OnSelectionChanged := FTSTVPSelectionChanged;
end;
{$endregion}

{$region 'public methods' /fold}
function TfrmEditorSettings.Execute: Boolean;
begin
  ShowModal;
  Result := True;
end;

procedure TfrmEditorSettings.Apply;
begin
  Settings.Apply;
end;
{$endregion}

end.

