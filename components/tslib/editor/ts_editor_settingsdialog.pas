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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, ComCtrls, ButtonPanel, Contnrs,

  RTTIGrids,

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
    tsXML: TTabSheet;
    tsHighlighters              : TTabSheet;
    tsSettings                  : TTabSheet;
    {$endregion}

    procedure FTVPSelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tsXMLEnter(Sender: TObject);

  private
    FEditorSettings : IEditorSettings;
    FTVP            : TTreeViewPresenter;
    FList           : TObjectList;
    FPI             : TTIPropertyGrid;
    FHAPI           : TTIPropertyGrid;
    FHAVST          : TVirtualStringTree;
    FXMLTree        : TXMLTree;

    function GetEditorSettings: IEditorSettings;
    procedure SetEditorSettings(const AValue: IEditorSettings);

  protected
    procedure UpdateData;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Execute: Boolean;
    procedure Apply;

    property EditorSettings: IEditorSettings
      read GetEditorSettings write SetEditorSettings;
  end;

//=============================================================================

procedure ExecuteSettingsDialog(
  AEditorSettings : IEditorSettings
);

//*****************************************************************************

implementation

{$R *.lfm}

uses
  SynEditHighlighter, PropEdits,

  ts_Core_DataTemplates, ts_Core_Helpers,

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

procedure ExecuteSettingsDialog(AEditorSettings: IEditorSettings);
begin
  if not Assigned(FForm) then
    FForm := TfrmEditorSettings.Create(Application);
  FForm.EditorSettings := AEditorSettings;
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
  FList := TObjectList.Create(False);
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.ItemTemplate := TSynAttributesDataTemplate.Create;
  FPI := CreatePI(Self, pnlPI);
  FHAPI := CreatePI(Self, pnlHARight);
  FHAVST := CreateVST(Self, pnlHALeft);

  //FXMLTree := CreateXMLTree(Self, pnlXML);
end;

procedure TfrmEditorSettings.BeforeDestruction;
begin
  FEditorSettings := nil;
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

function TfrmEditorSettings.GetEditorSettings: IEditorSettings;
begin
  Result := FEditorSettings;
end;

procedure TfrmEditorSettings.SetEditorSettings(const AValue: IEditorSettings);
begin
  if AValue <> EditorSettings then
  begin
    FEditorSettings  := AValue;
    FPI.TIObject := (FEditorSettings as IInterfaceComponentReference).GetComponent;
    UpdateData;
  end;
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

procedure TfrmEditorSettings.OKButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TfrmEditorSettings.tsXMLEnter(Sender: TObject);
begin
  FXMLTree.XML := EditorSettings.XML;
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
  FTVP.MultiSelect := True;
  FTVP.ColumnDefinitions.AddColumn('Name', dtString, 100);
  FList.Clear;
  for I := 0 to EditorSettings.HighlighterAttributes.Count - 1 do
  begin
    FList.Add(EditorSettings.HighlighterAttributes[I]);
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
  FEditorSettings.Apply;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

end.

