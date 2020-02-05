{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.VirtualDBTree;

{ Author: Tim Sinaeve
  Coded a long time ago for applications like BATBase, RecordPlayer and
  SnippetSource and brought back to life to support Lazarus.

  (29/04/2010)

  REMARK:

  ClipboardFormats.Text needs to be assigned if we want to support Drag
  operations. After a long investigation (13/06/2010) this turned out to be
  the reason why drag and drop did not work if we dynamically create the
  TCheckVirtualDBTreeEx instance.
  At designtime this property is assigned by default.
}

interface

{$MODE DELPHI}

uses
  ActnList, Classes, ComCtrls, Controls, DB, DBCtrls, Dialogs, ExtCtrls, Forms,
  Graphics, Menus, Variants, Windows, ImgList, ActiveX,

  VirtualTrees,

  ts.Components.VirtualDBTreeEx;

{ Form holding data-aware treeview for flat database files.
  To support this treeview, the table must include following fields for storing
  the node data:

  ID    : the primary key of the table
  Parent: reference to the parent node ID (0 means that record is the rootnode)
  Type  : node-type      0 : node item
                         1 : folder node
  Name  : name to be shown as the node identifier in the treeview }

{ TODO: support DB-update for a selection of multiple nodes like for a delete
        operation. }

const
  DEFAULT_KEYFIELDNAME      = 'Id';
  DEFAULT_LEVELFIELDNAME    = 'Id';
  DEFAULT_PARENTFIELDNAME   = 'ParentId';
  DEFAULT_PATHFIELDNAME     = '';
  DEFAULT_IMGIDXFIELDNAME   = 'ImageIndex';
  DEFAULT_NODETYPEFIELDNAME = 'NodeTypeId';
  DEFAULT_VIEWFIELDNAME     = 'NodeName';

type
  TNewFolderNodeEvent = procedure(Sender: TObject) of object;
  TNewItemNodeEvent = procedure(Sender: TObject) of object;
  TDropFilesEvent = procedure(
    Sender      : TBaseVirtualTree;
    AFiles      : TStrings;
    AAttachMode : TVTNodeAttachMode
  ) of object;

type

  { TfrmVirtualDBTree }

  TfrmVirtualDBTree = class(TForm)
    {$REGION 'designer controls'}
    actCollapseAllNodes    : TAction;
    actDeleteSelectedNodes : TAction;
    actExpandAllNodes      : TAction;
    actNewFolderNode       : TAction;
    actNewItemNode         : TAction;
    actNewRootFolderNode   : TAction;
    alsMain                : TActionList;
    btnCollapseAllNodes    : TToolButton;
    btnDivider             : TToolButton;
    btnExpandAllNodes      : TToolButton;
    btnNewFolder           : TToolButton;
    btnNewItem             : TToolButton;
    btnNewRoot             : TToolButton;
    dscMain                : TDataSource;
    imlMain                : TImageList;
    mniDelete              : TMenuItem;
    mniNewChild            : TMenuItem;
    mniNewFolder           : TMenuItem;
    navTreeView            : TDBNavigator;
    pnlTopLeft             : TPanel;
    pnlTopRight            : TPanel;
    pnlTree                : TPanel;
    ppmTreeView            : TPopupMenu;
    N1                     : TMenuItem;
    mniNewRoot             : TMenuItem;
    pnlMain                : TPanel;
    pnlTop                 : TPanel;
    pnlBottom              : TPanel;
    tlbTop                 : TToolBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actNewRootFolderNodeExecute(Sender: TObject);
    procedure actNewFolderNodeExecute(Sender: TObject);
    procedure actNewItemNodeExecute(Sender: TObject);
    procedure actDeleteSelectedNodesExecute(Sender: TObject);
    procedure actExpandAllNodesExecute(Sender: TObject);
    procedure actCollapseAllNodesExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure dscMainDataChange(Sender: TObject; Field: TField);

    procedure FTreeViewCreateEditor(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      out EditLink : IVTEditLink
    );
    procedure FTreeViewDragAllowed(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      Column      : TColumnIndex;
      var Allowed : Boolean
    );
    procedure FTreeViewDragDrop(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      DataObject : IDataObject;
      Formats    : TFormatArray;
      Shift      : TShiftState;
      const Pt   : TPoint;
      var Effect : DWORD;
      Mode       : TDropMode
    );
    procedure FTreeViewDragOver(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      Shift      : TShiftState;
      State      : TDragState;
      const Pt   : TPoint;
      Mode       : TDropMode;
      var Effect : DWORD;
      var Accept : Boolean
    );
    procedure FTreeViewEdited(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    {$ENDREGION}

  private
    FTreeView              : TCheckVirtualDBTreeEx;
    FOnDropFiles           : TDropFilesEvent;
    FDataSet               : TDataSet;
    FOnNewFolderNode       : TNotifyEvent;
    FOnNewItemNode         : TNotifyEvent;
    FOnDeleteSelectedNodes : TNotifyEvent;
    FNodeTypeFieldName     : string;

    {$REGION 'property access methods'}
    function GetImageList: TCustomImageList;
    function GetImgIdxField: TField;
    function GetImgIdxFieldName: string;
    function GetKeyField: TField;
    function GetKeyFieldName: string;
    function GetLevelField: TField;
    function GetLevelFieldName: string;
    function GetNodeTypeField: TField;
    function GetNodeTypeFieldName: string;
    function GetParentField: TField;
    function GetParentFieldName: string;
    function GetPathField: TField;
    function GetPathFieldName: string;
    function GetViewField: TField;
    function GetViewFieldName: string;
    procedure SetImageList(AValue: TCustomImageList);
    procedure SetImgIdxFieldName(const AValue: string);
    procedure SetKeyFieldName(const AValue: string);
    procedure SetLevelFieldName(const AValue: string);
    procedure SetNodeTypeFieldName(AValue: string);
    procedure SetParentFieldName(const AValue: string);
    procedure SetPathFieldName(const AValue: string);
    procedure SetViewFieldName(const AValue: string);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetMultiSelect(const Value: Boolean);
    function GetMultiSelect: Boolean;
    procedure SetToolbarBottomVisible(const Value: Boolean);
    procedure SetToolbarTopVisible(const Value: Boolean);
    function GetToolbarBottomVisible: Boolean;
    function GetToolbarTopVisible: Boolean;
  {$ENDREGION}

    procedure GetFileListFromObj(
      const DataObj : IDataObject;
      AFileList     : TStrings
    );

  protected
    procedure PostTreeData(
      AParentId   : Integer;
      ANodeType   : Integer;
      const AName : string
    ); virtual;

    procedure InitializeTreeView;

    // event dispatch methods
    procedure DoNewFolderNode; dynamic;
    procedure DoNewItemNode; dynamic;
    procedure DoDropFiles(
      AFiles            : TStrings;
      const AAttachMode : TVTNodeAttachMode
    ); dynamic;
    procedure DoDeleteSelectedNodes; dynamic;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure NewFolderNode;
    procedure NewItemNode;
    procedure NewSubItemNode;
    procedure NewRootFolderNode;
    procedure DeleteSelectedNodes;

    // public properties
    property KeyField: TField
      read GetKeyField;

    property ParentField: TField
      read GetParentField;

    property LevelField: TField
      read GetLevelField;

    property ImgIdxField: TField
      read GetImgIdxField;

    property NodeTypeField: TField
      read GetNodeTypeField;

    property PathField: TField
      read GetPathField;

    property ViewField: TField
      read GetViewField;

    property DataSet: TDataSet
      read FDataSet write SetDataSet;

    property TreeView: TCheckVirtualDBTreeEx
      read FTreeView;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect default True;

    property ToolbarTopVisible: Boolean
      read GetToolbarTopVisible write SetToolbarTopVisible default True;

    property ToolbarBottomVisible: Boolean
      read GetToolbarBottomVisible write SetToolbarBottomVisible default True;

    property KeyFieldName: string
      read GetKeyFieldName write SetKeyFieldName;

    property ParentFieldName: string
      read GetParentFieldName write SetParentFieldName;

    property LevelFieldName: string
      read GetLevelFieldName write SetLevelFieldName;

    property PathFieldName: string
      read GetPathFieldName write SetPathFieldName;

    property NodeTypeFieldName: string
      read GetNodeTypeFieldName write SetNodeTypeFieldName;

    property ImgIdxFieldName: string
      read GetImgIdxFieldName write SetImgIdxFieldName;

    property ViewFieldName: string
      read GetViewFieldName write SetViewFieldName;

    property ImageList: TCustomImageList
      read GetImageList write SetImageList;

    // events
    property OnNewFolderNode: TNotifyEvent
      read FOnNewFolderNode write FOnNewFolderNode;

    property OnNewItemNode: TNotifyEvent
      read FOnNewItemNode write FOnNewItemNode;

    property OnDropFiles: TDropFilesEvent
      read FOnDropFiles write FOnDropFiles;

    property OnDeleteSelectedNodes : TNotifyEvent
      read FOnDeleteSelectedNodes write FOnDeleteSelectedNodes;
  end;

implementation

{$R *.lfm}

uses
  SysUtils, ShellApi,

  SnippetSource.VirtualTree.Editors;

resourcestring
  SDeleteSelectedItems = 'Are you sure you want to delete te selected item(s)?';
  SNewFolder           = 'New folder';
  SNew                 = 'New';

{$REGION 'construction and destruction'}
procedure TfrmVirtualDBTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FTreeView := TCheckVirtualDBTreeEx.Create(Self);
  InitializeTreeView;
  MultiSelect          := True;
  ToolbarTopVisible    := True;
  ToolbarBottomVisible := True;

  KeyFieldName      := DEFAULT_KEYFIELDNAME;
  LevelFieldName    := DEFAULT_LEVELFIELDNAME;
  ParentFieldName   := DEFAULT_PARENTFIELDNAME;
  PathFieldName     := DEFAULT_PATHFIELDNAME;
  ImgIdxFieldName   := DEFAULT_IMGIDXFIELDNAME;
  ViewFieldName     := DEFAULT_VIEWFIELDNAME;
  NodeTypeFieldName := DEFAULT_NODETYPEFIELDNAME;
end;

procedure TfrmVirtualDBTree.BeforeDestruction;
begin
  dscMain.DataSet := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TfrmVirtualDBTree.SetDataSet(const Value: TDataSet);
begin
  if Value <> DataSet then
  begin
    dscMain.DataSet := Value;
    FDataSet := Value;
  end;
end;

function TfrmVirtualDBTree.GetToolbarBottomVisible: Boolean;
begin
  Result := pnlBottom.Visible;
end;

procedure TfrmVirtualDBTree.SetToolbarBottomVisible(const Value: Boolean);
begin
  pnlBottom.Visible := Value;
end;

function TfrmVirtualDBTree.GetToolbarTopVisible: Boolean;
begin
  Result := pnlTop.Visible;
end;

procedure TfrmVirtualDBTree.SetToolbarTopVisible(const Value: Boolean);
begin
  pnlTop.Visible := Value;
end;

function TfrmVirtualDBTree.GetMultiSelect: Boolean;
begin
  Result := toMultiSelect in FTreeView.TreeOptions.SelectionOptions;
end;

procedure TfrmVirtualDBTree.SetMultiSelect(const Value: Boolean);
begin
  if Value then
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toMultiSelect]
  else
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions - [toMultiSelect];
end;

function TfrmVirtualDBTree.GetKeyFieldName: string;
begin
  Result := FTreeView.KeyFieldName;
end;

function TfrmVirtualDBTree.GetLevelField: TField;
begin
  Result := FTreeView.LevelField;
end;

function TfrmVirtualDBTree.GetImgIdxFieldName: string;
begin
  Result := FTreeView.ImgIdxFieldName;
end;

function TfrmVirtualDBTree.GetImgIdxField: TField;
begin
  Result := FTreeView.ImgIdxField;
end;

function TfrmVirtualDBTree.GetImageList: TCustomImageList;
begin
  Result := FTreeView.Images;
end;

function TfrmVirtualDBTree.GetKeyField: TField;
begin
  Result := FTreeView.KeyField;
end;

procedure TfrmVirtualDBTree.SetImgIdxFieldName(const AValue: string);
begin
  FTreeView.ImgIdxFieldName := AValue;
end;

procedure TfrmVirtualDBTree.SetKeyFieldName(const AValue: string);
begin
  FTreeView.KeyFieldName := AValue;
end;

function TfrmVirtualDBTree.GetLevelFieldName: string;
begin
  Result := FTreeView.LevelFieldName;
end;

function TfrmVirtualDBTree.GetNodeTypeField: TField;
begin
  Result := DataSet.FieldByName(NodeTypeFieldName);
end;

function TfrmVirtualDBTree.GetNodeTypeFieldName: string;
begin
  Result := FNodeTypeFieldName;
end;

procedure TfrmVirtualDBTree.SetNodeTypeFieldName(AValue: string);
begin
  FNodeTypeFieldName := AValue;
end;

function TfrmVirtualDBTree.GetParentField: TField;
begin
  Result := FTreeView.ParentField;
end;

procedure TfrmVirtualDBTree.SetLevelFieldName(const AValue: string);
begin
  FTreeView.LevelFieldName := AValue;
end;

function TfrmVirtualDBTree.GetParentFieldName: string;
begin
  Result := FTreeView.ParentFieldName;
end;

function TfrmVirtualDBTree.GetPathField: TField;
begin
  Result := FTreeView.PathField;
end;

procedure TfrmVirtualDBTree.SetParentFieldName(const AValue: string);
begin
  FTreeView.ParentFieldName := AValue;
end;

function TfrmVirtualDBTree.GetPathFieldName: string;
begin
  Result := FTreeView.PathFieldName;
end;

procedure TfrmVirtualDBTree.SetPathFieldName(const AValue: string);
begin
  FTreeView.PathFieldName := AValue;
end;

function TfrmVirtualDBTree.GetViewField: TField;
begin
  Result := FTreeView.ViewField;
end;

function TfrmVirtualDBTree.GetViewFieldName: string;
begin
  Result := FTreeView.ViewFieldName;
end;

procedure TfrmVirtualDBTree.SetImageList(AValue: TCustomImageList);
begin
  if ImageList <> AValue then
  begin
    FTreeView.Images := AValue;
  end;
end;

procedure TfrmVirtualDBTree.SetViewFieldName(const AValue: string);
begin
  FTreeView.ViewFieldName := AValue;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TfrmVirtualDBTree.DoNewFolderNode;
begin
  if Assigned(FOnNewFolderNode) then
    FOnNewFolderNode(Self);
end;

procedure TfrmVirtualDBTree.DoNewItemNode;
begin
  if Assigned(FOnNewItemNode) then
    FOnNewItemNode(Self);
end;

procedure TfrmVirtualDBTree.DoDropFiles(AFiles: TStrings;
  const AAttachMode : TVTNodeAttachMode);
begin
  if Assigned(AFiles) and Assigned(FOnDropFiles) then
    FOnDropFiles(FTreeView, AFiles, AAttachMode);
end;

procedure TfrmVirtualDBTree.DoDeleteSelectedNodes;
begin
  if Assigned(FOnDeleteSelectedNodes) then
    FOnDeleteSelectedNodes(Self);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmVirtualDBTree.actNewRootFolderNodeExecute(Sender: TObject);
begin
  NewRootFolderNode;
end;

procedure TfrmVirtualDBTree.actNewFolderNodeExecute(Sender: TObject);
begin
  NewFolderNode;
end;

procedure TfrmVirtualDBTree.actNewItemNodeExecute(Sender: TObject);
begin
  NewItemNode;
end;

procedure TfrmVirtualDBTree.actDeleteSelectedNodesExecute(
  Sender: TObject);
begin
  if MessageDlg(SDeleteSelectedItems, mtWarning, [mbYes, mbNo], 0) in [mrYes] then
  begin
    DeleteSelectedNodes;
  end;
end;

procedure TfrmVirtualDBTree.actExpandAllNodesExecute(Sender: TObject);
begin
  FTreeView.ExpandAll;
end;

procedure TfrmVirtualDBTree.actCollapseAllNodesExecute(Sender: TObject);
begin
  FTreeView.CollapseAll;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVirtualDBTree.dscMainDataChange(Sender: TObject; Field: TField);
begin
  if Field = nil then
    FTreeView.ScrollIntoView(FTreeView.FocusedNode, True);
end;

procedure TfrmVirtualDBTree.FTreeViewCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditLink.Create;
end;

procedure TfrmVirtualDBTree.FTreeViewDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed  := True;
end;

procedure TfrmVirtualDBTree.FTreeViewDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
var
  I          : Integer;
  SL         : TStringList;
  AttachMode : TVTNodeAttachMode;
  Node       : PVirtualNode;
begin
  Node := Sender.GetNodeAt(Pt.x, Pt.y);
  Sender.FocusedNode := Node;
  if Mode = dmOnNode then
    AttachMode := amInsertBefore
  else if Mode = dmAbove then
    AttachMode := amInsertBefore
  else if Mode = dmBelow then
    AttachMode := amInsertAfter
  else
    AttachMode := amAddChildLast;

  SL := TStringList.Create;
  try
    for I := 0 to High(Formats) - 1 do
    begin
      if (Formats[I] = CF_HDROP) then
      begin
        GetFileListFromObj(DataObject, SL);
        DoDropFiles(SL, AttachMode);
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmVirtualDBTree.FTreeViewDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmVirtualDBTree.FTreeViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  ViewField.AsString := FTreeView.NodeText[Node];
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmVirtualDBTree.GetFileListFromObj(const DataObj: IDataObject;
  AFileList: TStrings);
var
  FmtEtc           : TFormatEtc;         // specifies required data format
  Medium           : TStgMedium;         // storage medium containing file list
  DroppedFileCount : Integer;            // number of dropped files
  I                : Integer;            // loops thru dropped files
  FileNameLength   : Integer;            // length of a dropped file name
  FileName         : WideString;         // name of a dropped file
begin
  // Get required storage medium from data object
  FmtEtc.cfFormat := CF_HDROP;
  FmtEtc.ptd      := nil;
  FmtEtc.dwAspect := DVASPECT_CONTENT;
  FmtEtc.lindex   := -1;
  FmtEtc.tymed    := TYMED_HGLOBAL;
  DataObj.GetData(FmtEtc, Medium);
  try
    try
      // Get count of files dropped
      DroppedFileCount := DragQueryFile(
        Medium.hGlobal, $FFFFFFFF, nil, 0
      );
      // Get name of each file dropped and process it
      for I := 0 to Pred(DroppedFileCount) do
      begin
        // get length of file name, then name itself
        FileNameLength := DragQueryFile(Medium.hGlobal, I, nil, 0);
        SetLength(FileName, FileNameLength);
        DragQueryFileW(
          Medium.hGlobal, I, PWideChar(FileName), FileNameLength + 1
        );
        // add file name to list
        AFileList.Append(string(FileName));
      end;
    finally
      // Tidy up - release the drop handle
      // don't use DropH again after this
      DragFinish(Medium.hGlobal);
    end;
  finally
    ReleaseStgMedium(Medium);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmVirtualDBTree.PostTreeData(AParentId: Integer;
  ANodeType: Integer; const AName: string);
begin
  DataSet.Append;
  ParentField.AsInteger   := AParentId;
  NodeTypeField.AsInteger := ANodeType;
  ViewField.AsString      := AName;
  if DataSet.State in dsEditModes then
    DataSet.Post;
end;

procedure TfrmVirtualDBTree.InitializeTreeView;
begin
  with FTreeView do
  begin
    Parent                        := pnlTree;
    Align                         := alClient;
    DoubleBuffered                := True;
    AnimationDuration             := 100;
    AutoExpandDelay               := 500;
    BorderStyle                   := bsNone;
    BorderSpacing.Left            := 0;
    BorderSpacing.Right           := 0;
    BorderSpacing.Top             := 0;
    BorderSpacing.Bottom          := 0;
    ButtonFillMode                := fmTransparent;
    Color                         := clWhite;
    DataSource                    := dscMain;
    DefaultPasteMode              := amInsertBefore;
    DefaultText                   := 'Node';
    DragMode                      := dmAutomatic;
    DragType                      := dtOLE;
    DragOperations                := [doMove];
    EditDelay                     := 200;
    HintMode                      := hmTooltip;
    IncrementalSearch             := isAll;
    IncrementalSearchStart        := ssAlwaysStartOver;
    Indent                        := 20;
    LineMode                      := lmBands;
    PopupMenu                     := ppmTreeView;

    Colors.FocusedSelectionColor  := clGray;
    Colors.HotColor               := clBlue;

    Header.AutoSizeIndex          := 0;
    Header.DefaultHeight          := 17;
    Header.Options                := [hoAutoResize, hoColumnResize, hoDrag];
    Header.PopupMenu              := ppmTreeView;
{
    dboAllowChecking,
    dboAllowStructureChange,
    dboAlwaysStructured,
    dboCheckChildren,
    dboCheckDBStructure,
    dboListView,
    dboParentStructure,
    dboPathStructure,
    dboReadOnly,
    dboShowChecks,
    dboTrackActive,
    dboTrackChanges,
    dboTrackCursor,
    dboViewAll,
    dboWriteLevel,
    dboWriteSecondary
 }
    DBOptions := [
      dboAllowStructureChange,
      dboAlwaysStructured,
      dboCheckChildren,
      dboCheckDBStructure,
      dboParentStructure,
      dboTrackActive,
      dboTrackChanges,
      dboTrackCursor,
      dboViewAll
    ];

    ClipboardFormats.Add('CSV');
    ClipboardFormats.Add('HTML Format');
    ClipboardFormats.Add('Plain text');
    ClipboardFormats.Add('Rich Text Format');
    ClipboardFormats.Add('Rich Text Format Without Objects');
    ClipboardFormats.Add('Unicode text');
    ClipboardFormats.Add('Virtual Tree Data');

    OnCreateEditor := FTreeViewCreateEditor;
    OnDragAllowed  := FTreeViewDragAllowed;
    OnDragOver     := FTreeViewDragOver;
    OnDragDrop     := FTreeViewDragDrop;
    OnEdited       := FTreeViewEdited;

    TreeOptions.AnimationOptions := [
      toAnimatedToggle,
      toAdvancedAnimatedToggle
    ];
    TreeOptions.AutoOptions := [
      toAutoDropExpand,
      toAutoScroll,
      toAutoScrollOnExpand,
      toAutoTristateTracking,
      toAutoChangeScale,
      toDisableAutoscrollOnEdit
    ];
    TreeOptions.MiscOptions := [
      toAcceptOLEDrop,
      toEditable,
      toFullRepaintOnResize,
      toInitOnSave,
      toReportMode,
      toToggleOnDblClick,
      toWheelPanning
    ];
    TreeOptions.PaintOptions := [
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
      toStaticBackground
    ];
    TreeOptions.SelectionOptions := [toMultiSelect, toCenterScrollIntoView];
    TreeOptions.StringOptions := [toAutoAcceptEditChange];
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmVirtualDBTree.NewFolderNode;
var
  Id : Integer;
begin
  if NodeTypeField.AsInteger = 1 then
    Id := KeyField.AsInteger
  else
    Id := ParentField.AsInteger;
  if DataSet.IsEmpty then // no parent, so we create a rootnode
    Id := 0;
  PostTreeData(Id, 1, SNewFolder);
  DoNewFolderNode;
end;

procedure TfrmVirtualDBTree.NewItemNode;
var
  Id : Integer;
begin
   if NodeTypeField.AsInteger = 1 then
    Id := KeyField.AsInteger
  else
    Id := ParentField.AsInteger;
  PostTreeData(Id, 2, SNew);
  DoNewItemNode;
end;

procedure TfrmVirtualDBTree.NewSubItemNode;
var
  Id : Integer;
begin
  Id := KeyField.AsInteger;
  PostTreeData(Id, 2, SNew);
  DoNewItemNode;
end;

procedure TfrmVirtualDBTree.NewRootFolderNode;
begin
  PostTreeData(0, 1, SNewFolder);
  DoNewFolderNode;
end;

procedure TfrmVirtualDBTree.DeleteSelectedNodes;
begin
  FTreeView.DeleteSelection;
  DoDeleteSelectedNodes;
end;
{$ENDREGION}
end.
