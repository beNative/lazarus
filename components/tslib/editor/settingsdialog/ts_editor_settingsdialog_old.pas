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

  RTTIGrids, ButtonPanel,

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
    btnOpenSettingsFile: TButton;
    btnReloadSettings: TButton;
    btnAssociate: TButton;
    pnlHLRightTop: TPanel;
    pnlButtons                  : TButtonPanel;
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
    pnlPI                       : TPanel;
    pnlTop                      : TPanel;
    pnlXML: TPanel;
    splHAVertical               : TSplitter;
    splHLVertical: TSplitter;
    tsHighlighters: TTabSheet;
    tsDebug: TTabSheet;
    tsXML: TTabSheet;
    tsHighlighterAttributes              : TTabSheet;
    tsSettings                  : TTabSheet;
    {$endregion}

    procedure actAssociateExecute(Sender: TObject);
    procedure actOpenSettingsFileExecute(Sender: TObject);
    procedure actReloadSettingsExecute(Sender: TObject);
    procedure FHAPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure FHATVPSelectionChanged(Sender: TObject);
    procedure FHLPIEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure FHLTVPSelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure plObjectInspector1AddAvailPersistent(APersistent: TPersistent;
      var Allowed: boolean);
  private
    FHATVP   : TTreeViewPresenter;
    FHLTVP   : TTreeViewPresenter;
    FHAList  : TObjectList;
    FHLList  : TObjectList;
    FPI      : TTIPropertyGrid;
    FHAPI    : TTIPropertyGrid;
    FHLPI    : TTIPropertyGrid;
    FSHLPI   : TTIPropertyGrid;
    FHAVST   : TVirtualStringTree;
    FHLVST   : TVirtualStringTree;
    FXMLTree : TXMLTree;

    function GetSettings: IEditorSettings;
    function GetManager: IEditorManager;

  protected
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
  ts.Core.DataTemplates, ts.Core.Helpers,

  ts.Components.FileAssociation,

  ts.Editor.HighlighterAttributes, ts.Editor.Highlighters,

  sharedlogger;

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
  Result := (Item as THighlighterItem).Name;
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
  FPI                 := CreatePI(Self, pnlPI);
  FHAPI               := CreatePI(Self, pnlHARight);
  FHLPI               := CreatePI(Self, pnlHLRightTop);
  FSHLPI              := CreatePI(Self, pnlHLRightBottom);
  FHAVST              := CreateVST(Self, pnlHALeft);
  FHLVST              := CreateVST(Self, pnlHLLeft);
  FXMLTree            := CreateXMLTree(Self, pnlXML);
  UpdateData;
  tsDebug.TabVisible        := Settings.DebugMode;
  tsXML.TabVisible          := Settings.DebugMode;
  tsHighlighters.TabVisible := Settings.DebugMode;
  if Settings.DebugMode then
    FXMLTree.XML := Settings.XML;
  FHLPI.OnEditorFilter := FHLPIEditorFilter;
end;

procedure TfrmEditorSettings.BeforeDestruction;
begin
  FreeAndNil(FHAList);
  FreeAndNil(FHLList);
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
  FHAPI.ExpandedProperties.Add('Attributes');
  FHAPI.ExpandedProperties.Add('Attributes.Style');
  FHAPI.ExpandedProperties.Add('Attributes.StyleMask');
  FHAPI.TIObject := FHATVP.CurrentItem as TPersistent;
end;

procedure TfrmEditorSettings.FHLPIEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  Logger.Send(aEditor.GetName);
  if aEditor.GetName = 'SynHighlighter' then
    aShow := False;
end;

procedure TfrmEditorSettings.FHLTVPSelectionChanged(Sender: TObject);
begin
  FHLPI.TIObject := FHLTVP.CurrentItem as TPersistent;
  if Assigned(FHLTVP.CurrentItem) then
  begin
    Logger.Send ('CurrentItem:', FHLTVP.CurrentItem.ClassName);
    if FHLTVP.CurrentItem is THighlighterItem then
      FSHLPI.TIObject := (FHLTVP.CurrentItem as THighlighterItem).SynHighlighter
    else
      Logger.SendError('WTF');

  end;
end;

procedure TfrmEditorSettings.actOpenSettingsFileExecute(Sender: TObject);
var
  S: String;
begin
  S := ExtractFilePath(Application.ExeName) + Settings.FileName;
  Manager.OpenFile(S);
end;

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

procedure TfrmEditorSettings.FHAPIEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin

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
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmEditorSettings.UpdateData;
var
  I: Integer;
begin
  FPI.TIObject := (Settings as IInterfaceComponentReference).GetComponent;
  FHATVP.MultiSelect := True;
  FHATVP.ColumnDefinitions.AddColumn('Name', dtString, 100);
  FHAList.Clear;
  for I := 0 to Settings.HighlighterAttributes.Count - 1 do
  begin
    FHAList.Add(Settings.HighlighterAttributes[I]);
  end;
  FHATVP.ItemsSource := FHAList;
  FHATVP.TreeView := FHAVST;
  FHATVP.OnSelectionChanged   := FHATVPSelectionChanged;

  FHLTVP.MultiSelect := True;
  FHLTVP.ColumnDefinitions.AddColumn('Name', dtString, 100);
  FHLList.Clear;
  for I := 0 to Settings.Highlighters.Count - 1 do
  begin
    FHLList.Add(Settings.Highlighters[I]);
  end;
  FHLTVP.ItemsSource := FHLList;
  FHLTVP.TreeView := FHLVST;
  FHLTVP.OnSelectionChanged := FHLTVPSelectionChanged;
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

