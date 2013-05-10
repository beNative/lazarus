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

unit ts_Editor_SettingsDialog;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ExtCtrls, ComCtrls, StdCtrls,
  Contnrs,

  RTTIGrids, ButtonPanel,

  VirtualTrees,

  ts_Core_TreeViewPresenter, ts_Core_ColumnDefinitions,

  ts_Components_XMLTree,

  ts_Editor_Interfaces;

//=============================================================================

{
  TODO:
    - Associate file extensions
    - this can be implemented as a toolform with IEditorToolView support
}

type
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
    pnlButtons                  : TButtonPanel;
    imlMain                     : TImageList;
    pcMain                      : TPageControl;
    pnlBottom                   : TPanel;
    pnlHALeft                   : TPanel;
    pnlHARight                  : TPanel;
    pnlHighlighterAttributes    : TPanel;
    pnlPI                       : TPanel;
    pnlTop                      : TPanel;
    pnlXML: TPanel;
    splHAVertical               : TSplitter;
    tsDebug: TTabSheet;
    tsXML: TTabSheet;
    tsHighlighters              : TTabSheet;
    tsSettings                  : TTabSheet;
    {$endregion}

    procedure actAssociateExecute(Sender: TObject);
    procedure actOpenSettingsFileExecute(Sender: TObject);
    procedure actReloadSettingsExecute(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tsXMLEnter(Sender: TObject);

  private
    FTVP     : TTreeViewPresenter;
    FList    : TObjectList;
    FPI      : TTIPropertyGrid;
    FHAPI    : TTIPropertyGrid;
    FHAVST   : TVirtualStringTree;
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

//=============================================================================

procedure ExecuteSettingsDialog(
  AOwner: TComponent
);

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_DataTemplates, ts_Core_Helpers,

  ts_Components_FileAssociation,

  ts_Editor_HighlighterAttributes;

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

{$region 'interfaced methods' /fold}
//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

procedure ExecuteSettingsDialog(AOwner: TComponent);
begin
  if not Assigned(FForm) then
    FForm := TfrmEditorSettings.Create(AOwner);
  FForm.Execute;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************
{$endregion}

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FList             := TObjectList.Create(False);
  FTVP              := TTreeViewPresenter.Create(Self);
  FTVP.ItemTemplate := TSynAttributesDataTemplate.Create;
  FPI               := CreatePI(Self, pnlPI);
  FHAPI             := CreatePI(Self, pnlHARight);
  FHAVST            := CreateVST(Self, pnlHALeft);
  FXMLTree          := CreateXMLTree(Self, pnlXML);

  UpdateData;
  tsDebug.TabVisible := Settings.DebugMode;
  tsXML.TabVisible   := Settings.DebugMode;
end;

procedure TfrmEditorSettings.BeforeDestruction;
begin
  FreeAndNil(FList);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmEditorSettings.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TfrmEditorSettings.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmEditorSettings.FTVPSelectionChanged(Sender: TObject);
begin
  FHAPI.ExpandedProperties.Add('Attributes');
  FHAPI.ExpandedProperties.Add('Attributes.Style');
  FHAPI.ExpandedProperties.Add('Attributes.StyleMask');
  FHAPI.TIObject := FTVP.CurrentItem as TPersistent;
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

procedure TfrmEditorSettings.OKButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TfrmEditorSettings.tsXMLEnter(Sender: TObject);
begin
  FXMLTree.XML := Settings.XML;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmEditorSettings.UpdateData;
var
  I: Integer;
begin
  FPI.TIObject := (Settings as IInterfaceComponentReference).GetComponent;
  FTVP.MultiSelect := True;
  FTVP.ColumnDefinitions.AddColumn('Name', dtString, 100);
  FList.Clear;
  for I := 0 to Settings.HighlighterAttributes.Count - 1 do
  begin
    FList.Add(Settings.HighlighterAttributes[I]);
  end;
  FTVP.ItemsSource := FList;
  FTVP.TreeView := FHAVST;
  FTVP.OnSelectionChanged   := FTVPSelectionChanged;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

{$region 'public methods' /fold}
//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

function TfrmEditorSettings.Execute: Boolean;
begin
  ShowModal;
  Result := True;
end;

procedure TfrmEditorSettings.Apply;
begin
  Settings.Apply;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

end.

