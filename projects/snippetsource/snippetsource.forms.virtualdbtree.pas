{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

  REMARK: ClipboardFormats.
  Text needs to be assigned if we want to support Drag operations. After a long
  investigation (13/06/2010) this turned out to be the reason why drag and drop
  did not work if we dynamically create the TCheckVirtualDBTreeEx instance.
  At designtime this property is assigned by default.
}

interface

{$MODE DELPHI}

uses
  ActnList, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  Menus, Variants, Windows, ImgList, ActiveX, Types,
  DB, DBCtrls,

  VirtualTrees,

  ts.Components.VirtualDBTreeEx;

{ Form holding data-aware treeview for flat database files.
  To support this treeview, the table must include following fields for storing
  the node data:

  Id    : the primary key of the table
  Parent: reference to the parent node Id (0 means that record is the rootnode)
  Type  : node-type      0 : node item
                         1 : folder node
  Name  : name to be shown as the node identifier in the treeview }

{ TODO: support DB-update for a selection of multiple nodes like for a delete
        operation. }

const
  DEFAULT_KEYFIELDNAME      = 'Id';
  DEFAULT_PARENTFIELDNAME   = 'ParentId';
  DEFAULT_IMGIDXFIELDNAME   = 'ImageIndex';
  DEFAULT_IMAGEFIELDNAME    = 'Image';
  DEFAULT_NODETYPEFIELDNAME = 'NodeTypeId';
  DEFAULT_NAMEFIELDNAME     = 'NodeName';

type
  TNewFolderNodeEvent = procedure(Sender: TObject) of object;
  TNewItemNodeEvent   = procedure(Sender: TObject) of object;
  TDropFilesEvent     = procedure(
    Sender      : TBaseVirtualTree;
    AFiles      : TStrings;
    AAttachMode : TVTNodeAttachMode
  ) of object;
  TDropTextEvent     = procedure(
    Sender      : TBaseVirtualTree;
    AText       : TStrings;
    AAttachMode : TVTNodeAttachMode
  ) of object;

type
  TfrmVirtualDBTree = class(TForm)
    {$REGION 'designer controls'}
    actCancel                 : TAction;
    actClearImage             : TAction;
    actCollapseAllNodes       : TAction;
    actDeleteSelectedNodes    : TAction;
    actDuplicateSelectedNodes : TAction;
    actExpandAllNodes         : TAction;
    actClearNodeData          : TAction;
    actMoveDown               : TAction;
    actMoveUp                 : TAction;
    actNewFolderNode          : TAction;
    actNewItemNode            : TAction;
    actNewRootFolderNode      : TAction;
    actPost                   : TAction;
    actRefresh                : TAction;
    alMain                    : TActionList;
    btnCollapseAllNodes       : TToolButton;
    btnDivider1               : TToolButton;
    btnDivider2               : TToolButton;
    btnDuplicateSelectedNodes : TToolButton;
    btnExpandAllNodes         : TToolButton;
    btnNewFolder              : TToolButton;
    btnNewItem                : TToolButton;
    btnNewRoot                : TToolButton;
    dscMain                   : TDataSource;
    imlMain                   : TImageList;
    mniClearNodeData          : TMenuItem;
    mniCancel                 : TMenuItem;
    mniClearImage             : TMenuItem;
    mniCollapseAllNodes       : TMenuItem;
    mniDelete                 : TMenuItem;
    mniDuplicateSelectedNodes : TMenuItem;
    mniExpandAllNodes         : TMenuItem;
    mniMoveDown               : TMenuItem;
    mniMoveUp                 : TMenuItem;
    mniNewChild               : TMenuItem;
    mniNewFolder              : TMenuItem;
    mniNewRoot                : TMenuItem;
    mniPost                   : TMenuItem;
    mniRefresh                : TMenuItem;
    N1                        : TMenuItem;
    N2                        : TMenuItem;
    N3                        : TMenuItem;
    N4                        : TMenuItem;
    navTreeView               : TDBNavigator;
    pnlMain                   : TPanel;
    pnlTop                    : TPanel;
    pnlTopLeft                : TPanel;
    pnlTopRight               : TPanel;
    pnlTree                   : TPanel;
    ppmTreeView               : TPopupMenu;
    shpLine                   : TShape;
    tlbTop                    : TToolBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actCancelExecute(Sender: TObject);
    procedure actClearImageExecute(Sender: TObject);
    procedure actClearNodeDataExecute(Sender: TObject);
    procedure actCopyNodeDataExecute(Sender: TObject);
    procedure actDuplicateSelectedNodesExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actNewRootFolderNodeExecute(Sender: TObject);
    procedure actNewFolderNodeExecute(Sender: TObject);
    procedure actNewItemNodeExecute(Sender: TObject);
    procedure actDeleteSelectedNodesExecute(Sender: TObject);
    procedure actExpandAllNodesExecute(Sender: TObject);
    procedure actCollapseAllNodesExecute(Sender: TObject);
    procedure actPostExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
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
    FTreeView                 : TCheckVirtualDBTreeEx;
    FOnDropFiles              : TDropFilesEvent;
    FOnDropText               : TDropTextEvent;
    FDataSet                  : TDataSet;
    FOnNewFolderNode          : TNotifyEvent;
    FOnNewItemNode            : TNotifyEvent;
    FOnDeleteSelectedNodes    : TNotifyEvent;
    FOnDuplicateSelectedNodes : TNotifyEvent;
    FOnMoveUpSelectedNodes    : TNotifyEvent;
    FOnMoveDownSelectedNodes  : TNotifyEvent;
    FOnCopyNodeData           : TNotifyEvent;
    FOnClearNodeData          : TNotifyEvent;
    FNodeTypeFieldName        : string;

    {$REGION 'property access methods'}
    function GetImageFieldName: string;
    function GetImageList: TCustomImageList;
    function GetImgIdxField: TField;
    function GetImgIdxFieldName: string;
    function GetKeyField: TField;
    function GetKeyFieldName: string;
    function GetMultiSelect: Boolean;
    function GetNodeTypeField: TField;
    function GetNodeTypeFieldName: string;
    function GetParentField: TField;
    function GetParentFieldName: string;
    function GetSelectionCount: Integer;
    function GetToolbarTopVisible: Boolean;
    function GetNameField: TField;
    function GetNameFieldName: string;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetImageFieldName(AValue: string);
    procedure SetImageList(AValue: TCustomImageList);
    procedure SetImgIdxFieldName(const AValue: string);
    procedure SetKeyFieldName(const AValue: string);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetNodeTypeFieldName(AValue: string);
    procedure SetParentFieldName(const AValue: string);
    procedure SetToolbarTopVisible(const Value: Boolean);
    procedure SetNameFieldName(const AValue: string);
    {$ENDREGION}

    procedure GetFileListFromObj(
      const ADataObj : IDataObject;
      AFileList      : TStrings
    );
    procedure GetTextFromObj(
      ADataObject : IDataObject;
      AStrings    : TStringList
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
    procedure DoCopyNodeData; dynamic;
    procedure DoDropFiles(
      AFiles            : TStrings;
      const AAttachMode : TVTNodeAttachMode
    ); dynamic;
    procedure DoDropText(
      AText             : TStrings;
      const AAttachMode : TVTNodeAttachMode
    ); dynamic;
    procedure DoDeleteSelectedNodes; dynamic;
    procedure DoDuplicateSelectedNodes; dynamic;
    procedure DoMoveUpSelectedNodes; dynamic;
    procedure DoMoveDownSelectedNodes; dynamic;
    procedure DoClearNodeData; dynamic;

    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure NewFolderNode;
    procedure NewItemNode;
    procedure NewSubItemNode;
    procedure NewRootFolderNode;
    procedure DeleteSelectedNodes;
    procedure RetrieveSelectedNodeIds(ANodeIds: TStrings);

    // public properties
    property KeyField: TField
      read GetKeyField;

    property ParentField: TField
      read GetParentField;

    property ImgIdxField: TField
      read GetImgIdxField;

    property NodeTypeField: TField
      read GetNodeTypeField;

    property NameField: TField
      read GetNameField;

    property DataSet: TDataSet
      read FDataSet write SetDataSet;

    property TreeView: TCheckVirtualDBTreeEx
      read FTreeView;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect default True;

    property ToolbarTopVisible: Boolean
      read GetToolbarTopVisible write SetToolbarTopVisible default True;

    property SelectionCount: Integer
      read GetSelectionCount;

    property KeyFieldName: string
      read GetKeyFieldName write SetKeyFieldName;

    property ParentFieldName: string
      read GetParentFieldName write SetParentFieldName;

    property NodeTypeFieldName: string
      read GetNodeTypeFieldName write SetNodeTypeFieldName;

    property ImageFieldName: string
      read GetImageFieldName write SetImageFieldName;

    property ImgIdxFieldName: string
      read GetImgIdxFieldName write SetImgIdxFieldName;

    property NameFieldName: string
      read GetNameFieldName write SetNameFieldName;

    property ImageList: TCustomImageList
      read GetImageList write SetImageList;

    // events
    property OnNewFolderNode: TNotifyEvent
      read FOnNewFolderNode write FOnNewFolderNode;

    property OnNewItemNode: TNotifyEvent
      read FOnNewItemNode write FOnNewItemNode;

    property OnDropFiles: TDropFilesEvent
      read FOnDropFiles write FOnDropFiles;

    property OnDropText: TDropTextEvent
      read FOnDropText write FOnDropText;

    property OnDeleteSelectedNodes : TNotifyEvent
      read FOnDeleteSelectedNodes write FOnDeleteSelectedNodes;

    property OnDuplicateSelectedNodes: TNotifyEvent
      read FOnDuplicateSelectedNodes write FOnDuplicateSelectedNodes;

    property OnMoveUpSelectedNodes: TNotifyEvent
      read FOnMoveUpSelectedNodes write FOnMoveUpSelectedNodes;

    property OnMoveDownSelectedNodes: TNotifyEvent
      read FOnMoveDownSelectedNodes write FOnMoveDownSelectedNodes;

    property OnCopyNodeData: TNotifyEvent
      read FOnCopyNodeData write FOnCopyNodeData;

    property OnClearNodeData: TNotifyEvent
      read FOnClearNodeData write FOnClearNodeData;
  end;

implementation

{$R *.lfm}

uses
  SysUtils, ShellApi, Rtti, ComObj, Clipbrd, TypInfo,

  ts.Core.Logger, ts.Core.Utils,

  SnippetSource.VirtualTree.Editors, SnippetSource.Resources;

{$REGION 'construction and destruction'}
procedure TfrmVirtualDBTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FTreeView := TCheckVirtualDBTreeEx.Create(Self);
  InitializeTreeView;
  MultiSelect       := True;
  ToolbarTopVisible := True;
  Logger.Send('FTreeView.DBOptions', TValue.From<TDBVTOptions>(FTreeView.DBOptions));

  KeyFieldName      := DEFAULT_KEYFIELDNAME;
  ParentFieldName   := DEFAULT_PARENTFIELDNAME;
  ImgIdxFieldName   := DEFAULT_IMGIDXFIELDNAME;
  NameFieldName     := DEFAULT_NAMEFIELDNAME;
  NodeTypeFieldName := DEFAULT_NODETYPEFIELDNAME;
  ImageFieldName    := DEFAULT_IMAGEFIELDNAME;
end;

destructor TfrmVirtualDBTree.Destroy;
begin
  dscMain.DataSet := nil;
  inherited Destroy;
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

procedure TfrmVirtualDBTree.SetKeyFieldName(const AValue: string);
begin
  FTreeView.KeyFieldName := AValue;
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

function TfrmVirtualDBTree.GetParentFieldName: string;
begin
  Result := FTreeView.ParentFieldName;
end;

procedure TfrmVirtualDBTree.SetParentFieldName(const AValue: string);
begin
  FTreeView.ParentFieldName := AValue;
end;

function TfrmVirtualDBTree.GetSelectionCount: Integer;
begin
  Result := FTreeView.SelectedCount;
end;

function TfrmVirtualDBTree.GetNameField: TField;
begin
  Result := FTreeView.NameField;
end;

function TfrmVirtualDBTree.GetNameFieldName: string;
begin
  Result := FTreeView.NameFieldName;
end;

function TfrmVirtualDBTree.GetImageFieldName: string;
begin
  Result := FTreeView.ImageFieldName;
end;

procedure TfrmVirtualDBTree.SetImageFieldName(AValue: string);
begin
  if ImageFieldName <> AValue then
  begin;
    FTreeView.ImageFieldName := AValue;
  end;
end;

procedure TfrmVirtualDBTree.SetImageList(AValue: TCustomImageList);
begin
  if ImageList <> AValue then
  begin
    FTreeView.Images := AValue;
  end;
end;

procedure TfrmVirtualDBTree.SetNameFieldName(const AValue: string);
begin
  FTreeView.NameFieldName := AValue;
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

procedure TfrmVirtualDBTree.DoCopyNodeData;
begin
  if Assigned(OnCopyNodeData) then
    OnCopyNodeData(Self);
end;

procedure TfrmVirtualDBTree.DoDropFiles(AFiles: TStrings;
  const AAttachMode : TVTNodeAttachMode);
begin
  if Assigned(AFiles) and Assigned(FOnDropFiles) then
    FOnDropFiles(FTreeView, AFiles, AAttachMode);
end;

procedure TfrmVirtualDBTree.DoDropText(AText: TStrings;
  const AAttachMode: TVTNodeAttachMode);
begin
  if Assigned(AText) and Assigned(FOnDropText) then
    FOnDropText(FTreeView, AText, AAttachMode);
end;

procedure TfrmVirtualDBTree.DoDeleteSelectedNodes;
begin
  if Assigned(FOnDeleteSelectedNodes) then
    FOnDeleteSelectedNodes(Self);
end;

procedure TfrmVirtualDBTree.DoDuplicateSelectedNodes;
begin
  if Assigned(FOnDuplicateSelectedNodes) then
    FOnDuplicateSelectedNodes(Self);
end;

procedure TfrmVirtualDBTree.DoMoveUpSelectedNodes;
begin
  if Assigned(FOnMoveUpSelectedNodes) then
    FOnMoveUpSelectedNodes(Self);
end;

procedure TfrmVirtualDBTree.DoMoveDownSelectedNodes;
begin
  if Assigned(FOnMoveDownSelectedNodes) then
    FOnMoveDownSelectedNodes(Self);
end;

procedure TfrmVirtualDBTree.DoClearNodeData;
begin
  if Assigned(FOnClearNodeData) then
    FOnClearNodeData(Self);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmVirtualDBTree.actNewRootFolderNodeExecute(Sender: TObject);
begin
  NewRootFolderNode;
end;

procedure TfrmVirtualDBTree.actCopyNodeDataExecute(Sender: TObject);
begin
  DoCopyNodeData;
end;

procedure TfrmVirtualDBTree.actCancelExecute(Sender: TObject);
begin
  DataSet.Cancel;
end;

procedure TfrmVirtualDBTree.actClearImageExecute(Sender: TObject);
begin
  if not (DataSet.State in dsEditModes) then
  begin
    DataSet.Edit;
  end;
  FTreeView.ImageField.Clear;
  DataSet.Post;
  DataSet.Refresh;
  //FTreeView.InvalidateNode(FTreeView.FocusedNode);
  FTreeView.Refresh;
end;

procedure TfrmVirtualDBTree.actClearNodeDataExecute(Sender: TObject);
begin
  DoClearNodeData;
end;

procedure TfrmVirtualDBTree.actDuplicateSelectedNodesExecute(Sender: TObject);
begin
  DoDuplicateSelectedNodes;
end;

procedure TfrmVirtualDBTree.actMoveDownExecute(Sender: TObject);
begin
  DoMoveDownSelectedNodes;
end;

procedure TfrmVirtualDBTree.actMoveUpExecute(Sender: TObject);
begin
  DoMoveUpSelectedNodes;
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

procedure TfrmVirtualDBTree.actPostExecute(Sender: TObject);
begin
  DataSet.Post;
end;

procedure TfrmVirtualDBTree.actRefreshExecute(Sender: TObject);
begin
  DataSet.Refresh;
  FTreeView.UpdateTree;
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
  Allowed := True;
end;

procedure TfrmVirtualDBTree.FTreeViewDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
var
  I           : Integer;
  SL          : TStringList;
  LAttachMode : TVTNodeAttachMode;
  LNode       : PVirtualNode;
begin
  LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Assigned(LNode) then
  begin
    Sender.ClearSelection;
    Sender.FocusedNode := LNode;
  end;

  case Mode of
    dmOnNode, dmAbove:
      LAttachMode := amInsertBefore;
    dmBelow:
      LAttachMode := amInsertAfter;
  else
    LAttachMode := amAddChildLast;
  end;
  Logger.Send('AttachMode', GetEnumName(TypeInfo(TVTNodeAttachMode), Ord(LAttachMode)));

  for I := 0 to High(Formats) - 1 do
  begin
    Logger.Send(I.ToString, GetClipboardFormatName(Formats[I]));
    if Formats[I] = CF_HDROP then
    begin
      SL := TStringList.Create;
      try
        GetFileListFromObj(DataObject, SL);
        DoDropFiles(SL, LAttachMode);
      finally
        FreeAndNil(SL);
      end;
    end
    else if Formats[I] in [CF_TEXT, CF_UNICODETEXT] then
    begin
      SL := TStringList.Create;
       try
         GetTextFromObj(DataObject, SL);
         DoDropText(SL, LAttachMode);
       finally
         FreeAndNil(SL);
       end;
    end;
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
  if not (DataSet.State in dsEditModes) then
  begin
    DataSet.Edit;
  end;
  NameField.AsString := FTreeView.NodeText[Node];
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmVirtualDBTree.GetFileListFromObj(const ADataObj: IDataObject;
  AFileList: TStrings);
var
  LFmtEtc           : TFormatEtc;      // specifies required data format
  LMedium           : TStgMedium;      // storage medium containing file list
  LDroppedFileCount : Integer;         // number of dropped files
  I                 : Integer;         // loops thru dropped files
  LFileNameLength   : Integer;         // length of a dropped file name
  LFileName         : WideString;      // name of a dropped file
begin
  // Get required storage LMedium from data object
  LFmtEtc.cfFormat := CF_HDROP;
  LFmtEtc.ptd      := nil;
  LFmtEtc.dwAspect := DVASPECT_CONTENT;
  LFmtEtc.lindex   := -1;
  LFmtEtc.tymed    := TYMED_HGLOBAL;
  ADataObj.GetData(LFmtEtc, LMedium);
  try
    try
      // Get count of files dropped
      LDroppedFileCount := DragQueryFile(
        LMedium.hGlobal, $FFFFFFFF, nil, 0
      );
      // Get name of each file dropped and process it
      for I := 0 to Pred(LDroppedFileCount) do
      begin
        // get length of file name, then name itself
        LFileNameLength := DragQueryFile(LMedium.hGlobal, I, nil, 0);
        SetLength(LFileName, LFileNameLength);
        DragQueryFileW(
          LMedium.hGlobal, I, PWideChar(LFileName), LFileNameLength + 1
        );
        // add file name to list
        AFileList.Append(string(LFileName));
      end;
    finally
      // Tidy up - release the drop handle
      // don't use DropH again after this
      DragFinish(LMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(LMedium);
  end;
end;

procedure TfrmVirtualDBTree.GetTextFromObj(ADataObject: IDataObject;
  AStrings: TStringList);
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  PText: Pointer;
  Text: string;
begin
  AStrings.Clear;

  // First, try to get the CF_UNICODETEXT format.
  FormatEtc.cfFormat := CF_UNICODETEXT;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  if ADataObject.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      if StgMedium.tymed = TYMED_HGLOBAL then
      begin
        PText := GlobalLock(StgMedium.hGlobal);
        if Assigned(PText) then
        begin
          try
            Text := WideString(PWideChar(PText));  // Convert PWideChar to WideString.
            AStrings.Text := Text;
          finally
            GlobalUnlock(StgMedium.hGlobal);
          end;
        end;
      end;
    finally
      ReleaseStgMedium(StgMedium);
    end;
    Exit;  // If CF_UNICODETEXT is successfully retrieved, exit the procedure.
  end;

  // If CF_UNICODETEXT is not available, try to get CF_TEXT.
  FormatEtc.cfFormat := CF_TEXT;
  if ADataObject.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      if StgMedium.tymed = TYMED_HGLOBAL then
      begin
        PText := GlobalLock(StgMedium.hGlobal);
        if Assigned(PText) then
        begin
          try
            Text := string(PAnsiChar(PText));  // Convert PAnsiChar to String.
            AStrings.Text := Text;
          finally
            GlobalUnlock(StgMedium.hGlobal);
          end;
        end;
      end;
    finally
      ReleaseStgMedium(StgMedium);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmVirtualDBTree.UpdateActions;
begin
  inherited UpdateActions;
  actCancel.Enabled := DataSet.State in dsEditModes;
  actPost.Enabled   := DataSet.State in dsEditModes;
end;

procedure TfrmVirtualDBTree.PostTreeData(AParentId: Integer;
  ANodeType: Integer; const AName: string);
begin
  DataSet.Append;
  ParentField.AsInteger   := AParentId;
  NodeTypeField.AsInteger := ANodeType;
  NameField.AsString      := AName;
  if DataSet.State in dsEditModes then
    DataSet.Post;
end;

procedure TfrmVirtualDBTree.InitializeTreeView;
begin
  with FTreeView do
  begin
    Parent                       := pnlTree;
    Align                        := alClient;
    DoubleBuffered               := True;
    AnimationDuration            := 100;
    AutoExpandDelay              := 500;
    BorderStyle                  := bsNone;
    BorderSpacing.Left           := 0;
    BorderSpacing.Right          := 0;
    BorderSpacing.Top            := 0;
    BorderSpacing.Bottom         := 0;
    ButtonFillMode               := fmTransparent;
    Color                        := clWhite;
    DataSource                   := dscMain;
    DefaultPasteMode             := amInsertBefore;
    DefaultText                  := 'Node';
    DragMode                     := dmAutomatic;
    DragType                     := dtOLE;
    DragOperations               := [doMove];
    EditDelay                    := 200;
    HintMode                     := hmTooltip;
    IncrementalSearch            := isAll;
    IncrementalSearchStart       := ssAlwaysStartOver;
    Indent                       := 20;
    LineMode                     := lmBands;
    PopupMenu                    := ppmTreeView;
    Font.Name                    := 'Segoe UI';
    Font.Size                    := 9;

    Colors.FocusedSelectionColor := clGray;
    Colors.HotColor              := clBlue;

    Header.AutoSizeIndex         := 0;
    Header.DefaultHeight         := 17;
    Header.Options               := [hoAutoResize, hoColumnResize, hoDrag];
    Header.PopupMenu             := ppmTreeView;
    DBOptions := [
      dboAllowStructureChange,
      dboAlwaysStructured,
      dboCheckChildren,
      dboCheckDBStructure,
      dboParentStructure,
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
    TreeOptions.StringOptions    := [toAutoAcceptEditChange];
  end;
  FTreeView.AutoAdjustLayout(lapAutoAdjustForDPI, 96, Self.PixelsPerInch, 0,0);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmVirtualDBTree.NewFolderNode;
var
  LId : Integer;
begin
  if NodeTypeField.AsInteger = 1 then
    LId := KeyField.AsInteger
  else
    LId := ParentField.AsInteger;
  if DataSet.IsEmpty then // no parent, so we create a rootnode
    LId := 0;
  PostTreeData(LId, 1, SNewFolder);
  DoNewFolderNode;
end;

procedure TfrmVirtualDBTree.NewItemNode;
var
  LId : Integer;
begin
  if NodeTypeField.AsInteger = 1 then
    LId := KeyField.AsInteger
  else
    LId := ParentField.AsInteger;
  PostTreeData(LId, 2, SNew);
  DoNewItemNode;
end;

procedure TfrmVirtualDBTree.NewSubItemNode;
var
  LId : Integer;
begin
  LId := KeyField.AsInteger;
  PostTreeData(LId, 2, SNew);
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

procedure TfrmVirtualDBTree.RetrieveSelectedNodeIds(ANodeIds: TStrings);
var
  LNode : PVirtualNode;
begin
  for LNode in FTreeView.SelectedNodes do
  begin
    FTreeView.GetDBNodeData(LNode);
    ANodeIds.Add('%d', [PDBVTData(FTreeView.GetNodeData(LNode)).Id]);
  end;
end;
{$ENDREGION}
end.
