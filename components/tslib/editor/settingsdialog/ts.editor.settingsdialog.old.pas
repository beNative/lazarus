{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.SettingsDialog.Old;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ExtCtrls, ComCtrls, StdCtrls,
  Contnrs,

  PropEdits,

  RTTIGrids, RTTICtrls,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions,

  ts.Editor.Interfaces;

{
  TODO:
    - Associate file extensions
    - this can be implemented as a toolform with IEditorToolView support
}

type

  { TfrmEditorSettings }

  TfrmEditorSettings = class(TForm)
    {$REGION 'designer controls'}
    aclMain                  : TActionList;
    actApplySettings         : TAction;
    actAssociate             : TAction;
    actReloadSettings        : TAction;
    actOpenSettingsFile      : TAction;
    btnApply                 : TButton;
    btnOpenSettingsFile      : TButton;
    btnReloadSettings        : TButton;
    btnAssociate             : TButton;
    btnClose                 : TButton;
    btnOK                    : TButton;
    Label1                   : TLabel;
    lblAttributeAliases      : TLabel;
    pnlHARightTop            : TPanel;
    pnlHARightBottom         : TPanel;
    pnlHLRightTop            : TPanel;
    imlMain                  : TImageList;
    pcMain                   : TPageControl;
    pnlBottom                : TPanel;
    pnlHALeft                : TPanel;
    pnlHLLeft                : TPanel;
    pnlHARight               : TPanel;
    pnlHLRight               : TPanel;
    pnlHighlighterAttributes : TPanel;
    pnlHighlighters          : TPanel;
    pnlHLRightBottom         : TPanel;
    pnlPI                    : TPanel;
    pnlTop                   : TPanel;
    splHAVertical            : TSplitter;
    splHLVertical            : TSplitter;
    mmoAliasNames            : TTIMemo;
    tsHighlighters           : TTabSheet;
    tsDebug                  : TTabSheet;
    tsHighlighterAttributes  : TTabSheet;
    tsSettings               : TTabSheet;
    {$ENDREGION}

    procedure actApplySettingsExecute(Sender: TObject);
    procedure actAssociateExecute(Sender: TObject);
    procedure actOpenSettingsFileExecute(Sender: TObject);
    procedure actReloadSettingsExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FHATVPSelectionChanged(Sender: TObject);
    procedure FHLPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure FHLTVPSelectionChanged(Sender: TObject);
    procedure FPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure OKButtonClick(Sender: TObject);
    procedure plObjectInspector1AddAvailPersistent(APersistent: TPersistent;
      var Allowed: boolean);
  private
    FHATVP   : TTreeViewPresenter; // HighlighterAttributes presenter
    FHLTVP   : TTreeViewPresenter; // Highlighters presenter
    FHAList  : TObjectList;
    FHLList  : TObjectList;
    FPI      : TTIPropertyGrid;
    FHAPI    : TTIPropertyGrid;
    FHLPI    : TTIPropertyGrid;
    FSHLPI   : TTIPropertyGrid;
    FHAVST   : TVirtualStringTree;
    FHLVST   : TVirtualStringTree;

    procedure SettingsChangedHandler(ASender: TObject);

    function GetSettings: IEditorSettings;
    function GetManager: IEditorManager;

  protected
    procedure UpdateControls;
    procedure UpdateData;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

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
  ts.Core.Utils,

  ts.Core.Logger;

resourcestring
  SAttributeName = 'Attribute name';
  SHighlighter   = 'Highlighter';
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

{$REGION 'TSynAttributesDataTemplate'}
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
{$ENDREGION}

{$REGION 'THighlightersDataTemplate'}
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
{$ENDREGION}

{$REGION 'TToolSettingsDataTemplate'}
type
  TToolSettingsDataTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
  end;

function TToolSettingsDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := (Item as TComponent).Name;
end;
{$ENDREGION}

{$REGION 'interfaced methods'}
procedure ExecuteSettingsDialog(AOwner: TComponent);
begin
  if not Assigned(FForm) then
    FForm := TfrmEditorSettings.Create(AOwner);
  FForm.Execute;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FHAList             := TObjectList.Create(False);
  FHATVP              := TTreeViewPresenter.Create(Self);
  FHATVP.ItemTemplate := TSynAttributesDataTemplate.Create;
  FHLList             := TObjectList.Create(False);
  FHLTVP              := TTreeViewPresenter.Create(Self);
  FHLTVP.ItemTemplate := THighlightersDataTemplate.Create;
  FPI                 := CreatePI(Self, pnlPI);
  FHAPI               := CreatePI(Self, pnlHARightBottom);
  FHLPI               := CreatePI(Self, pnlHLRightTop);
  FSHLPI              := CreatePI(Self, pnlHLRightBottom);
  FHAVST              := VST.Create(Self, pnlHALeft);
  FHLVST              := VST.Create(Self, pnlHLLeft);

  Settings.AddEditorSettingsChangedHandler(SettingsChangedHandler);
  UpdateControls;

  FHLPI.OnEditorFilter  := FHLPIEditorFilter;
  FPI.OnEditorFilter    := FPIEditorFilter;
  FHAPI.OnEditorFilter  := FPIEditorFilter;
  FSHLPI.OnEditorFilter := FPIEditorFilter;
  UpdateData;
  pcMain.ActivePageIndex := 0;
end;

destructor TfrmEditorSettings.Destroy;
begin
  if Assigned(Manager) and Assigned(Settings) then
    Settings.RemoveEditorSettingsChangedHandler(SettingsChangedHandler);
  FreeAndNil(FHAList);
  FreeAndNil(FHLList);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmEditorSettings.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TfrmEditorSettings.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmEditorSettings.FHATVPSelectionChanged(Sender: TObject);
begin
  mmoAliasNames.Link.TIObject := FHATVP.CurrentItem as TPersistent;
  mmoAliasNames.Link.TIPropertyName := ALIASNAMES_PROPERTY;
  FHAPI.ExpandedProperties.Add(ATTRIBUTES_PROPERTY);
  FHAPI.TIObject := FHATVP.CurrentItem as TPersistent;
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

procedure TfrmEditorSettings.btnOKClick(Sender: TObject);
begin
    Settings.Save;
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
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmEditorSettings.actOpenSettingsFileExecute(Sender: TObject);
var
  S : string;
begin
  S := GetApplicationConfigPath + Settings.FileName;
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
  AR.CmdNameNoSpaces := 'Open';

  CreateFileAssociation(
    AR,
    True,
    True,
    True,
    True
  );
  ClearIconCache;
end;

procedure TfrmEditorSettings.actApplySettingsExecute(Sender: TObject);
begin
  Apply;
end;

procedure TfrmEditorSettings.actReloadSettingsExecute(Sender: TObject);
begin
  Settings.Load;
  Apply;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditorSettings.UpdateControls;
begin
  tsDebug.TabVisible        := Settings.DebugMode;
  tsHighlighters.TabVisible := Settings.DebugMode;
end;

procedure TfrmEditorSettings.UpdateData;
var
  I : Integer;
begin
  FPI.TIObject := (Settings as IInterfaceComponentReference).GetComponent;
  FPI.Update;
  FHATVP.MultiSelect := True;
  FHATVP.ColumnDefinitions.AddColumn(NAME, SAttributeName, dtString, 150);
  FHAList.Clear;
  for I := 0 to Settings.HighlighterAttributes.Count - 1 do
  begin
    FHAList.Add(Settings.HighlighterAttributes[I]);
  end;
  FHATVP.ItemsSource := FHAList;
  FHATVP.TreeView := FHAVST;
  FHATVP.OnSelectionChanged := FHATVPSelectionChanged;
  FHLTVP.MultiSelect := True;
  FHLTVP.ColumnDefinitions.AddColumn(NAME, SHighlighter, dtString, 150);
  FHLList.Clear;
  for I := 0 to Settings.Highlighters.Count - 1 do
  begin
    FHLList.Add(Settings.Highlighters[I]);
  end;
  FHLTVP.ItemsSource := FHLList;
  FHLTVP.TreeView := FHLVST;
  FHLTVP.OnSelectionChanged := FHLTVPSelectionChanged;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TfrmEditorSettings.Execute: Boolean;
begin
  ShowModal;
  Result := True;
end;

procedure TfrmEditorSettings.Apply;
begin
  Settings.Apply;
end;
{$ENDREGION}

end.

