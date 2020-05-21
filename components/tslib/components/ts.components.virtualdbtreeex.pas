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

{$REGION 'comments'}
{
  Based on the original version by Adem Baba, with modifications by
  Vadim Sedulin and C.S. Phua.

  Ported to Lazarus with many modifications by Tim Sinaeve (28/04/2010)
  - Added support for Image blob fields (21/05/2020)
}
{$ENDREGION}

unit ts.Components.VirtualDBTreeEx;

{$MODE DELPHI}

{ A TCustomVirtualStringTree descendant for displaying a DB table as a tree. }

interface

uses
  Classes, Controls, DB, ImgList, ActiveX, Graphics,

  VirtualTrees;

type
  TDBVTOption = (
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
  );
  TDBVTOptions = set of TDBVTOption;

  TDBVTStatus = (
    dbtsChanged,
    dbtsChecking,
    dbtsDataChanging,
    dbtsDataOpening,
    dbtsDragDrop,
    dbtsEditing,
    dbtsEmpty,
    dbtsInsert,
    dbtsStructured,
    dbtsToggleAll
  );
  TDBVTStatuses = set of TDBVTStatus;

  TDBVTChangeMode = (
    dbcmEdit,
    dbcmInsert,
    dbcmStructure
  );

  TDBVTGoToMode = (
    gtmFromFirst,
    gtmNext,
    gtmPrev
  );

  TDBVTNodeStatus = (
    dbnsDelete,
    dbnsEdit,
    dbnsInited,
    dbnsNew,
    dbnsNone,
    dbnsRefreshed
  );

  PDBVTData = ^TDBVTData;

  TDBVTData = record
    Id     : Double;
    Level  : Integer;
    Status : TDBVTNodeStatus;
    Parent : PVirtualNode;
  end;

  TBaseVirtualDBTreeEx = class;
  TVirtualDBTreeExDataLink = class;

  TVTDBOpenQueryEvent = procedure(
    Sender    : TBaseVirtualDBTreeEx;
    var Allow : Boolean
  ) of object;

  TVTDBWriteQueryEvent = procedure(
    Sender     : TBaseVirtualDBTreeEx;
    Node       : PVirtualNode;
    Column     : TColumnIndex;
    ChangeMode : TDBVTChangeMode;
    var Allow  : Boolean
  ) of object;

  TVTNodeDataChangedEvent = procedure(
    Sender         : TBaseVirtualDBTreeEx;
    Node           : PVirtualNode;
    Field          : TField;
    var UpdateNode : Boolean
  ) of object;

  TVTNodeFromDBEvent = procedure(
    Sender : TBaseVirtualDBTreeEx;
    Node   : PVirtualNode
  ) of object;

  TVTPathToDBEvent = procedure(
    Sender   : TBaseVirtualDBTreeEx;
    var Path : string
  ) of object;

  TVirtualDBTreeExDataLink = class(TDataLink)
  private
    FVirtualDBTreeEx: TBaseVirtualDBTreeEx;

  public
    constructor Create(ATree: TBaseVirtualDBTreeEx); virtual;
    procedure BeforeDestruction; override;

  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure RecordChanged(Field: TField); override;
  end;

  TBaseVirtualDBTreeEx = class(TCustomVirtualStringTree)
  private
    FCurId             : Double;
    FDataLink          : TVirtualDBTreeExDataLink;
    FDBDataSize        : Integer;
    FDBOptions         : TDBVTOptions;
    FDBStatus          : TDBVTStatuses;
    FImageField        : TField;
    FImageFieldName    : string;
    FImgIdxField       : TField;
    FImgIdxFieldName   : string;
    FKeyField          : TField;
    FKeyFieldName      : string;
    FLevelField        : TField;
    FLevelFieldName    : string;
    FMaxLevel          : Integer;
    FOnNodeDataChanged : TVTNodeDataChangedEvent;
    FOnOpeningDataSet  : TVTDBOpenQueryEvent;
    FOnReadNodeFromDB  : TVTNodeFromDBEvent;
    FOnReadPathFromDB  : TVTPathToDBEvent;
    FOnWritePathToDB   : TVTPathToDBEvent;
    FOnWritingDataSet  : TVTDBWriteQueryEvent;
    FParentField       : TField;
    FParentFieldName   : string;
    FPathField         : TField;
    FPathFieldName     : string;
    FViewField         : TField;
    FViewFieldName     : string;

    {$REGION 'property access methods'}
    function GetDBNodeDataSize: Integer;
    function GetDBOptions: TDBVTOptions;
    function GetDBStatus: TDBVTStatuses;
    function GetDataSource: TDataSource;
    procedure SetImageFieldName(AValue: string);
    procedure SetKeyFieldName(const Value: string);
    procedure SetLevelFieldName(const Value: string);
    procedure SetParentFieldName(const Value: string);
    procedure SetPathFieldName(const Value: string);
    procedure SetViewFieldName(const Value: string);
    procedure SetImgIdxFieldName(const Value: string);
    procedure SetDBNodeDataSize(Value: Integer);
    procedure SetDBOptions(Value: TDBVTOptions);
    procedure SetDataSource(Value: TDataSource);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    {$ENDREGION}

    procedure RefreshListNode;
    procedure RefreshNodeByParent;
    procedure RefreshNodeByPath;

  protected
    function GetOptionsClass: TTreeOptionsClass; override;

    function CanOpenDataSet: Boolean; virtual;
    function CanWriteToDataSet(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      ChangeMode : TDBVTChangeMode
    ): Boolean; virtual;

    function FindChild(Node: PVirtualNode; ID: Double): PVirtualNode;
    function FindNode(Start: PVirtualNode; ID: Double): PVirtualNode;
    function HasVisibleChildren(Node: PVirtualNode): Boolean;

    procedure DataLinkActiveChanged; virtual;
    procedure DataLinkChanged; virtual;
    procedure DataLinkEditingChanged; virtual;
    procedure DataLinkRecordChanged(Field: TField); virtual;
    procedure DataLinkScrolled; virtual;

    function DoChecking(
      Node              : PVirtualNode;
      var NewCheckState : TCheckState
    ): Boolean; override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoCollapsed(Node: PVirtualNode); override;
    procedure DoDragDrop(
      Source     : TObject;
      DataObject : IDataObject;
      Formats    : TFormatArray;
      Shift      : TShiftState;
      const Pt   : TPoint;
      var Effect : LongWord;
      Mode       : TDropMode
    ); override;
    procedure DoEdit; override;
    procedure DoFocusChange(
      Node   : PVirtualNode;
      Column : TColumnIndex
    ); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoInitNode(
      AParent, Node  : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;
    procedure DoNodeDataChanged(
      Node           : PVirtualNode;
      Field          : TField;
      var UpdateNode : Boolean
    ); virtual;
    procedure DoNodeMoved(Node: PVirtualNode); override;
    procedure DoOpeningDataSet(var Allow: Boolean); virtual;
    procedure DoAfterItemPaint(
      Canvas         : TCanvas;
      Node           : PVirtualNode;
      const ItemRect : TRect
    ); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); virtual;
    procedure DoReadPathFromDB(var APath: string); virtual;
    procedure DoWritePathToDB(var APath: string); virtual;
    procedure DoWritingDataSet(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      ChangeMode : TDBVTChangeMode;
      var Allow  : Boolean
    ); virtual;
    function DoGetImageIndex(
      Node        : PVirtualNode;
      Kind        : TVTImageKind;
      Column      : TColumnIndex;
      var Ghosted : Boolean;
      var Index   : Integer
    ): TCustomImageList; override;

    procedure InitFields; virtual;
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

    procedure ReadNodeFromDB(Node: PVirtualNode); virtual;
    procedure RefreshNode; virtual;
    procedure RefreshNodes;
    procedure ResetFields; virtual;
    procedure SetFocusToNode(Node: PVirtualNode);
    procedure ToggleListView;
    procedure ToggleViewMode;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanEdit(
      Node   : PVirtualNode;
      Column : TColumnIndex
    ): Boolean; override;
    function GetDBNodeData(Node: PVirtualNode): Pointer;
    function GoToRec(AId: Double): Boolean; overload;
    procedure GoToRec(AString: string; AMode: TDBVTGoToMode); overload;
    function DoCancelEdit: Boolean; override;
    function DoEndEdit: Boolean; override;
    procedure AddNode(AParent: PVirtualNode);
    procedure CheckAllChildren(Node: PVirtualNode);
    procedure CollapseAll;
    procedure DeleteSelection;
    procedure ExpandAll;

    procedure OnDragOverHandler(
      Sender     : TBaseVirtualTree;
      Source     : TObject;
      Shift      : TShiftState;
      State      : TDragState;
      const Pt   : TPoint;
      Mode       : TDropMode;
      var Effect : LongWord;
      var Accept : Boolean
    );

    procedure UnCheckAll(Node: PVirtualNode; OnlyChildren: Boolean);
    procedure UpdateTree;

    property DBNodeDataSize: Integer
      read GetDBNodeDataSize write SetDBNodeDataSize;

    property DBStatus: TDBVTStatuses
      read GetDBStatus;

    property KeyField: TField
      read FKeyField;

    property LevelField: TField
      read FLevelField;

    property OnNodeDataChanged: TVTNodeDataChangedEvent
      read FOnNodeDataChanged write FOnNodeDataChanged;

    property OnReadNodeFromDB: TVTNodeFromDBEvent
      read FOnReadNodeFromDB write FOnReadNodeFromDB;

    property ParentField: TField
      read FParentField;

    property PathField: TField
      read FPathField;

    property ViewField: TField
      read FViewField;

    property ImgIdxField: TField
      read FImgIdxField;

    property ImageField: TField
      read FImageField;

    property Canvas;

  published
    property DBOptions: TDBVTOptions
      read GetDBOptions write SetDBOptions;

    property DataSource: TDataSource
      read GetDataSource write SetDataSource;

    property KeyFieldName: string
      read FKeyFieldName write SetKeyFieldName;

    property LevelFieldName: string
      read FLevelFieldName write SetLevelFieldName;

    property OnOpeningDataSet: TVTDBOpenQueryEvent
      read FOnOpeningDataSet write FOnOpeningDataSet;

    property OnReadPathFromDB: TVTPathToDBEvent
      read FOnReadPathFromDB write FOnReadPathFromDB;

    property OnWritePathToDB: TVTPathToDBEvent
      read FOnWritePathToDB write FOnWritePathToDB;

    property OnWritingDataSet: TVTDBWriteQueryEvent
      read FOnWritingDataSet write FOnWritingDataSet;

    property ParentFieldName: string
      read FParentFieldName write SetParentFieldName;

    property PathFieldName: string
      read FPathFieldName write SetPathFieldName;

    property ImageFieldName: string
      read FImageFieldName write SetImageFieldName;

    property TreeOptions: TStringTreeOptions
      read GetOptions write SetOptions;

    property ViewFieldName: string
      read FViewFieldName write SetViewFieldName;

    property ImgIdxFieldName: string
      read FImgIdxFieldName write SetImgIdxFieldName;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ButtonFillMode;
    property ButtonStyle;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragImageKind;
    property DragKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetHelpContext;
    property OnGetHint;
    property OnGetImageIndex;
    property OnGetLineStyle;
    property OnGetPopupMenu;
    property OnGetText;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnPaintBackground;
    property OnPaintText;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
    property OnUpdating;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBarOptions;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property Visible;
    property WantTabs;
  end;

  TVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): string;
    procedure SetNodeText(Node: PVirtualNode; const Value: string);

  protected
    function DoCompare(
      Node1, Node2: PVirtualNode;
      Column: TColumnIndex
    ): Integer; override;
    procedure DoGetText(
      Node      : PVirtualNode;
      Column    : TColumnIndex;
      TextType  : TVSTTextType;
      var AText : string
    ); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;
    procedure DoNodeDataChanged(
      Node           : PVirtualNode;
      Field          : TField;
      var UpdateNode : Boolean
    ); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;
    procedure DoWritingDataSet(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      ChangeMode : TDBVTChangeMode;
      var Allow  : Boolean
    ); override;

  public
    constructor Create(AOwner: TComponent); override;

    property Canvas;

    property NodeText[Node: PVirtualNode]: string
	    read GetNodeText write SetNodeText;
  end;

  TCustomDBCheckVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    FCheckDataLink   : TVirtualDBTreeExDataLink;
    FResultField     : TField;
    FResultFieldName : string;

    function GetCheckDataSource: TDataSource;
    function GetCheckList: TStringList;
    procedure SetCheckDataSource(Value: TDataSource);
    procedure SetResultFieldName(const Value: string);

  protected
    function DoChecking(
      Node              : PVirtualNode;
      var NewCheckState : TCheckState
    ): Boolean; override;

    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoOpeningDataSet(var Allow: Boolean); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;

    procedure CheckDataLinkActiveChanged; virtual;
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CheckList : TStringList
      read GetCheckList;

    property ResultField : TField
      read FResultField;

    property CheckDataSource : TDataSource
      read GetCheckDataSource write SetCheckDataSource;

    property ResultFieldName : string
      read FResultFieldName write SetResultFieldName;
  end;

  TDBCheckVirtualDBTreeEx = class(TCustomDBCheckVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): string;
    procedure SetNodeText(Node: PVirtualNode; const Value: string);

  protected
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex):
      Integer; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
      TVSTTextType; var Text: string); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text:
      string); override;
    procedure DoNodeDataChanged(Node: PVirtualNode; Field: TField; var
      UpdateNode: Boolean); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;
    procedure DoWritingDataSet(Node: PVirtualNode; Column: TColumnIndex;
      ChangeMode: TDBVTChangeMode; var Allow: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    property Canvas;

    property NodeText[Node: PVirtualNode]: string
      read GetNodeText write SetNodeText;

  published
    property CheckDataSource;
    property ResultFieldName;
  end;

  TCustomCheckVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    FList : TStringList;

    function GetCheckList: TStringList;
    procedure SetCheckList(Value: TStringList);

  protected
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CheckList: TStringList
      read GetCheckList write SetCheckList;
  end;

  TCheckVirtualDBTreeEx = class(TCustomCheckVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): string;
    procedure SetNodeText(Node: PVirtualNode; const Value: string);

  protected
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex):
      Integer; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
      TVSTTextType; var Text: string); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const Text: string); override;
    procedure DoNodeDataChanged(Node: PVirtualNode; Field: TField; var
      UpdateNode: Boolean); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;
    procedure DoWritingDataSet(Node: PVirtualNode; Column: TColumnIndex;
      ChangeMode: TDBVTChangeMode; var Allow: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    property Canvas;

    property NodeText[Node: PVirtualNode]: string
      read GetNodeText write SetNodeText;
  end;

type
  TDBNodeData = record
    Text       : string;
    ImageIndex : Integer;
    Image      : TMemoryStream;
  end;
  PDBNodeData = ^TDBNodeData;

implementation

uses
  SysUtils, Math,

  ts.Core.Logger;

type
  THackedTreeOptions = class(TStringTreeOptions);

{$REGION 'TVirtualDBTreeExDataLink'}
{$REGION 'construction and destruction'}
constructor TVirtualDBTreeExDataLink.Create(ATree: TBaseVirtualDBTreeEx);
begin
  inherited Create;
  FVirtualDBTreeEx := ATree;
end;

procedure TVirtualDBTreeExDataLink.BeforeDestruction;
begin
  FVirtualDBTreeEx := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

procedure TVirtualDBTreeExDataLink.ActiveChanged;
begin
  FVirtualDBTreeEx.DataLinkActiveChanged;
end;

procedure TVirtualDBTreeExDataLink.DataSetScrolled(Distance: Integer);
begin
  FVirtualDBTreeEx.DataLinkScrolled;
end;

procedure TVirtualDBTreeExDataLink.DataSetChanged;
begin
  FVirtualDBTreeEx.DataLinkChanged;
end;

procedure TVirtualDBTreeExDataLink.RecordChanged(Field: TField);
begin
  FVirtualDBTreeEx.DataLinkRecordChanged(Field);
end;

procedure TVirtualDBTreeExDataLink.EditingChanged;
begin
  FVirtualDBTreeEx.DataLinkEditingChanged;
end;
{$ENDREGION}

{$REGION 'TBaseVirtualDBTreeEx'}
{$REGION 'construction and destruction'}
constructor TBaseVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink    := TVirtualDBTreeExDataLink.Create(Self);
  FDBDataSize  := SizeOf(TDBVTData);
  NodeDataSize := FDBDataSize;
  FDBOptions   := [dboCheckDBStructure, dboParentStructure, dboWriteLevel,
    dboWriteSecondary, dboTrackActive, dboTrackChanges, dboTrackCursor,
    dboAlwaysStructured, dboViewAll];
  OnDragOver := OnDragOverHandler;
  with TStringTreeOptions(inherited TreeOptions) do
  begin
    PaintOptions := PaintOptions + [toShowHorzGridLines, toShowTreeLines,
      toShowVertGridLines, toShowRoot];
    MiscOptions := MiscOptions + [toGridExtensions];
  end;
end;

destructor TBaseVirtualDBTreeEx.Destroy;
begin
  FDataLink.DataSource := nil;
  FDataLink.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TBaseVirtualDBTreeEx.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

function TBaseVirtualDBTreeEx.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TBaseVirtualDBTreeEx.SetImageFieldName(AValue: string);
begin
  if ImageFieldName <> AValue then
  begin
    FImageFieldName := AValue;
    if dboTrackActive in FDBOptions then
      DataLinkActiveChanged
    else
    begin
      FDBStatus := FDBStatus + [dbtsChanged];
      FImageField := nil;
      if FDataLink.Active and (FImageFieldName <> '') then
        FImageField := FDataLink.DataSet.FieldByName(FImageFieldName);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.SetKeyFieldName(const Value: string);
begin
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    if dboTrackActive in FDBOptions then
      DataLinkActiveChanged
    else
    begin
      FDBStatus := FDBStatus + [dbtsChanged];
      FKeyField := nil;
      if FDataLink.Active and (FKeyFieldName <> '') then
        FKeyField := FDataLink.DataSet.FieldByName(FPathFieldName);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.SetParentFieldName(const Value: string);
begin
  if FParentFieldName <> Value then
  begin
    FParentFieldName := Value;
    FParentField := nil;
    if (dboParentStructure in FDBOptions) and
      (dboTrackActive in FDBOptions) then
    begin
      if not (dboListView in FDBOptions) then
        DataLinkActiveChanged
      else if (dboAlwaysStructured in FDBOptions) then
        DataLinkActiveChanged
      else
      begin
        FDBStatus := FDBStatus + [dbtsStructured];
        if FDataLink.Active and (FParentFieldName <> '') then
          FParentField := FDataLink.DataSet.FieldByName(FParentFieldName);
      end;
    end
    else
    begin
      if not (dboTrackActive in FDBOptions) then
        FDBStatus := FDBStatus + [dbtsChanged];
      if FDataLink.Active and (FParentFieldName <> '') then
        FParentField := FDataLink.DataSet.FieldByName(FParentFieldName);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.SetPathFieldName(const Value: string);
begin
  if FPathFieldName <> Value then
  begin
    FPathFieldName := Value;
    FPathField := nil;
    if (dboPathStructure in FDBOptions) and (dboTrackActive in FDBOptions) then
    begin
      if not (dboListView in FDBOptions) then
        DataLinkActiveChanged
      else if (dboAlwaysStructured in FDBOptions) then
        DataLinkActiveChanged
      else
      begin
        FDBStatus := FDBStatus + [dbtsStructured];
        if FDataLink.Active and (FPathFieldName <> '') then
          FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
      end;
    end
    else
    begin
      if not (dboTrackActive in FDBOptions) then
        FDBStatus := FDBStatus + [dbtsChanged];
      if FDataLink.Active and (FPathFieldName <> '') then
        FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.SetLevelFieldName(const Value: string);
begin
  if FLevelFieldName <> Value then
  begin
    FLevelFieldName := Value;
    FLevelField := nil;
    if FDataLink.Active and (FLevelFieldName <> '') then
      FLevelField := FDataLink.DataSet.FieldByName(FLevelFieldName);
  end;
end;

procedure TBaseVirtualDBTreeEx.SetDBNodeDataSize(Value: Integer);
begin
  if (Value <> DBNodeDataSize) and (Value >= 0) then
  begin
    NodeDataSize := FDBDataSize + Value;
    UpdateTree;
  end;
end;

function TBaseVirtualDBTreeEx.GetDBNodeDataSize: Integer;
begin
  Result := NodeDataSize - FDBDataSize;
end;

function TBaseVirtualDBTreeEx.GetDBStatus: TDBVTStatuses;
begin
  Result := FDBStatus;
end;

function TBaseVirtualDBTreeEx.GetDBOptions: TDBVTOptions;
begin
  Result := FDBOptions;
end;

procedure TBaseVirtualDBTreeEx.SetDBOptions(Value: TDBVTOptions);
var
  LToBeSet     : TDBVTOptions;
  LToBeCleared : TDBVTOptions;
begin
  EndEditNode;
  LToBeSet     := Value - FDBOptions;
  LToBeCleared := FDBOptions - Value;
  FDBOptions   := Value;

  if (dboTrackCursor in LToBeSet) then
  begin
    if not (dboTrackActive in FDBOptions) or not
      (dboTrackChanges in FDBOptions)
    then
    begin
      FDBOptions := FDBOptions + [dboTrackActive, dboTrackChanges];
      FDBOptions := FDBOptions - [dboAllowChecking];
      if (dbtsChanged in FDBStatus) then
        DataLinkActiveChanged;
    end
    else
      DataLinkScrolled;
  end
  else if (dboTrackChanges in LToBeSet) then
  begin
    if not (dboTrackActive in FDBOptions) then
      FDBOptions := FDBOptions + [dboTrackActive];
    if dbtsChanged in FDBStatus then
      DataLinkActiveChanged;
  end
  else if dboTrackActive in LToBeSet then
  begin
    if dbtsChanged in FDBStatus then
      DataLinkActiveChanged;
  end
  else if dboTrackActive in LToBeCleared then
  begin
    FDBOptions := FDBOptions - [dboTrackCursor, dboTrackChanges];
    FDBOptions := FDBOptions + [dboReadOnly];
  end
  else if dboTrackChanges in LToBeCleared then
  begin
    FDBOptions := FDBOptions - [dboTrackCursor];
    FDBOptions := FDBOptions + [dboReadOnly];
  end
  else if dboTrackCursor in LToBeCleared then
    FDBOptions := FDBOptions + [dboReadOnly];

  if dboShowChecks in LToBeSet then
  begin
    if dboTrackCursor in FDBOptions then
    begin
      FDBOptions := FDBOptions - [dboShowChecks];
      FDBOptions := FDBOptions + [dboViewAll];
    end
    else
    begin
      BeginUpdate;
      THackedTreeOptions(TreeOptions).MiscOptions :=
        THackedTreeOptions(TreeOptions).MiscOptions + [toCheckSupport];
      if not (dboViewAll in FDBOptions) then
        ToggleViewMode;
      EndUpdate;
    end;
  end
  else if dboShowChecks in LToBeCleared then
  begin
    BeginUpdate;
    THackedTreeOptions(TreeOptions).MiscOptions :=
      THackedTreeOptions(TreeOptions).MiscOptions - [toCheckSupport];
    if not (dboViewAll in FDBOptions) then
    begin
      FDBOptions := FDBOptions + [dboViewAll];
      RefreshNodes;
    end;
    EndUpdate;
  end
  else if dboViewAll in LToBeSet then
  begin
    if dboShowChecks in FDBOptions then
      ToggleViewMode;
  end
  else if dboViewAll in LToBeCleared then
  begin
    if dboShowChecks in FDBOptions then
      ToggleViewMode
    else
      FDBOptions := FDBOptions + [dboViewAll];
  end;

  if dboPathStructure in LToBeSet then
  begin
    FDBOptions := FDBOptions - [dboParentStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboParentStructure in LToBeSet then
  begin
    FDBOptions := FDBOptions - [dboPathStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboPathStructure in LToBeCleared then
  begin
    FDBOptions := FDBOptions + [dboParentStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboParentStructure in LToBeCleared then
  begin
    FDBOptions := FDBOptions + [dboPathStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end;

  if dboAlwaysStructured in LToBeSet then
  begin
    if not (dbtsStructured in FDBStatus) then
      RefreshNodes;
  end
  else if dboAlwaysStructured in LToBeCleared then
  begin
    if dboShowChecks in FDBOptions then
      FDBOptions := FDBOptions + [dboAlwaysStructured];
  end;

  if dboListView in LToBeSet then
    ToggleListView
  else if dboListView in LToBeCleared then
  begin
    if dbtsStructured in FDBStatus then
      ToggleListView
    else
      RefreshNodes;
  end;
  if (dboReadOnly in LToBeCleared) and
    (not (dboTrackCursor in FDBOptions) or not
    (dboTrackChanges in FDBOptions) or not (dboTrackActive in FDBOptions)) then
    FDBOptions := FDBOptions + [dboReadOnly];
end;
{$ENDREGION}

{$REGION 'protected mehods'}
procedure TBaseVirtualDBTreeEx.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(FDataLink)
    and (AComponent = DataSource)
  then
    DataSource := nil;
end;

procedure TBaseVirtualDBTreeEx.DataLinkActiveChanged;
begin
  if not (csDesigning in ComponentState) then
  begin
    ResetFields;
    if dboTrackActive in FDBOptions then
    begin
      if FDataLink.Active then
        InitFields;
      UpdateTree;
    end
    else
      FDBStatus := FDBStatus + [dbtsChanged];
  end;
end;

procedure TBaseVirtualDBTreeEx.DataLinkScrolled;
var
  KeyID: Double;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FKeyField) and not (dbtsDataChanging in FDBStatus) and
      (dboTrackCursor in FDBOptions) then
    begin
      FDBStatus := FDBStatus + [dbtsDataChanging];
      KeyID := FKeyField.AsFloat;
      if (KeyID <> 0.0) and (KeyID <> FCurId) then
        SetFocusToNode(FindNode(nil, KeyID));
      FDBStatus := FDBStatus - [dbtsDataChanging];
      if not Assigned(FocusedNode) then
        SetFocusToNode(GetFirst);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.DataLinkChanged;
begin
  if not (csDesigning in ComponentState) then
  begin
    if not FDataLink.Editing and not (dbtsDataChanging in FDBStatus) then
    begin
      if dboTrackChanges in FDBOptions then
        RefreshNodes
      else
        FDBStatus := FDBStatus + [dbtsChanged];
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.DataLinkRecordChanged(Field: TField);
var
  UpdateField: Boolean;
  Node: PVirtualNode;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(Field) and (dboTrackChanges in FDBOptions) then
    begin
      UpdateField := False;
      if dboTrackCursor in FDBOptions then
        Node := FocusedNode
      else
        Node := FindNode(nil, FKeyField.AsFloat);
      if Assigned(Node) then
      begin
        DoNodeDataChanged(Node, Field, UpdateField);
        if UpdateField then
          InvalidateNode(Node);
      end;
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.DataLinkEditingChanged;
var
  LData : PDBVTData;
  LNode : PVirtualNode;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FDataLink.Editing and not (dbtsEditing in FDBStatus)
      and not (dbtsInsert in FDBStatus) then
    begin
      if dboTrackChanges in FDBOptions then
      begin
        if (dboTrackCursor in FDBOptions) and
          (FDataLink.DataSet.State = dsEdit) then
        begin
          if Assigned(FocusedNode) and (dboTrackCursor in FDBOptions) then
          begin
            LData := GetNodeData(FocusedNode);
            LData.Status := dbnsEdit;
          end;
        end
        else if FDataLink.DataSet.State = dsInsert then
        begin
          LNode := AddChild(nil);
          ValidateNode(LNode, False);
          LData := GetNodeData(LNode);
          LData.Id := 0.0;
          LData.Level := 0;
          LData.Parent := nil;
          LData.Status := dbnsInited;
          ReadNodeFromDB(LNode);
          LData.Status := dbnsNew;
          if (dboTrackCursor in FDBOptions) then
            SetFocusToNode(LNode);
        end;
      end
      else
        FDBStatus := FDBStatus + [dbtsChanged];
    end;
    if Assigned(FocusedNode) and (dboTrackChanges in FDBOptions) and
      (dboTrackCursor in FDBOptions) then
      InvalidateNode(FocusedNode);
  end;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TBaseVirtualDBTreeEx.SetViewFieldName(const Value: string);
begin
  if FViewFieldName <> Value then
  begin
    FViewField := nil;
    FViewFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TBaseVirtualDBTreeEx.SetImgIdxFieldName(const Value: string);
begin
  if FImgIdxFieldName <> Value then
  begin
    FImgIdxField := nil;
    FImgIdxFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

function TBaseVirtualDBTreeEx.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TBaseVirtualDBTreeEx.SetOptions(const Value: TStringTreeOptions);
begin
  inherited TreeOptions := Value;
end;

function TBaseVirtualDBTreeEx.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TBaseVirtualDBTreeEx.CanEdit(Node: PVirtualNode;
  Column: TColumnIndex):
Boolean;
begin
  if not (dbtsEditing in FDBStatus) and not (dbtsInsert in FDBStatus) then
    Result := CanWriteToDataSet(Node, Column, dbcmEdit)
  else
    Result := True;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TBaseVirtualDBTreeEx.DoFocusChange(Node: PVirtualNode; Column:
  TColumnIndex);
var
  LData : PDBVTData;
begin
  if Assigned(Node) then
  begin
    LData := GetNodeData(Node);
    if Assigned(LData) and (LData.Id <> FCurId) then
    begin
      FCurId := LData.Id;
      if (FCurId <> 0.0) and not (dbtsDataChanging in FDBStatus) and
        (dboTrackCursor in FDBOptions) then
      begin
        FDBStatus := FDBStatus + [dbtsDataChanging];

        // TODO: check TSI
        if FDataLink.DataSet.State in dsEditModes then
          FDataLink.DataSet.Post;

        FDataLink.DataSet.Locate(FKeyFieldName, LData.Id, []);
        FDBStatus := FDBStatus - [dbtsDataChanging];
      end;
    end;
  end;
  inherited;
end;

procedure TBaseVirtualDBTreeEx.DoEdit;
var
  LData : PDBVTData;
begin
  inherited DoEdit;
  if IsEditing then
  begin
    LData := GetNodeData(FocusedNode);
    if LData.Status = dbnsEdit then
      FDBStatus := FDBStatus + [dbtsEditing]
    else if LData.Status = dbnsNew then
      FDBStatus := FDBStatus + [dbtsInsert]
    else if not (dbtsInsert in FDBStatus) then
    begin
      FDBStatus := FDBStatus + [dbtsEditing];
      FDataLink.DataSet.Edit;
    end;
  end;
end;

function TBaseVirtualDBTreeEx.DoEndEdit: Boolean;
var
  LData : PDBVTData;
begin
  Result := inherited DoEndEdit;
  if Result then
  begin
    LData := GetNodeData(FocusedNode);
    LData.Status := dbnsRefreshed;
    if (dbtsEditing in FDBStatus) then
    begin
      FDBStatus := FDBStatus - [dbtsEditing] + [dbtsDataChanging];
      if FDataLink.Editing then
        FDataLink.DataSet.Post;
      FDBStatus := FDBStatus - [dbtsDataChanging];
    end
    else if (dbtsInsert in FDBStatus) then
    begin
      FDBStatus := FDBStatus - [dbtsInsert] + [dbtsDataChanging];
      if FDataLink.Editing then
        FDataLink.DataSet.Post;
      FDBStatus := FDBStatus - [dbtsDataChanging];
      LData.Id := FKeyField.AsFloat;
      FCurId := LData.Id;
    end;
  end;
end;

function TBaseVirtualDBTreeEx.DoCancelEdit: Boolean;
var
  LData : PDBVTData;
begin
  if dbtsInsert in FDBStatus then
  begin
    FDBStatus := FDBStatus - [dbtsInsert] + [dbtsDataChanging];
    if FDataLink.Editing then
      FDataLink.DataSet.Cancel;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    Result := inherited DoCancelEdit;
    DeleteNode(FocusedNode);
    DataLinkScrolled;
    Exit;
  end
  else if dbtsEditing in FDBStatus then
  begin
    FDBStatus := FDBStatus - [dbtsEditing] + [dbtsDataChanging];
    if FDataLink.Editing then
      FDataLink.DataSet.Cancel;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    LData := GetNodeData(FocusedNode);
    LData.Status := dbnsRefreshed;
  end;
  Result := inherited DoCancelEdit;
end;

procedure TBaseVirtualDBTreeEx.DoCollapsed(Node: PVirtualNode);
var
  LFocus : PVirtualNode;
begin
  if Assigned(Node) then
  begin
    if Assigned(FocusedNode) and HasAsParent(FocusedNode, Node) then
    begin
      LFocus := Node;
      if not Selected[LFocus] then
      begin
        LFocus := GetNextSibling(Node);
        while Assigned(LFocus) and not Selected[LFocus] do
          LFocus := GetNextVisible(LFocus);
        if not Assigned(LFocus) then
        begin
          LFocus := GetPreviousVisible(Node);
          while Assigned(LFocus) and not Selected[LFocus] do
            LFocus := GetPreviousVisible(LFocus);
          if not Assigned(LFocus) then
            LFocus := Node;
        end;
      end;
      FocusedNode := LFocus;
      Selected[LFocus] := True;
    end;
    LFocus := GetNextSibling(Node);
    if not Assigned(LFocus) then
    begin
      LFocus := GetLastChild(Node);
      if not Assigned(LFocus) then
        LFocus := Node;
      LFocus := GetNext(LFocus);
    end;
    Node := GetNext(Node);
    while Node <> LFocus do
    begin
      Selected[Node] := False;
      Node := GetNext(Node);
    end;
  end;
  inherited;
end;

procedure TBaseVirtualDBTreeEx.DoDragDrop(Source: TObject; DataObject:
  IDataObject; Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint;
  var Effect: LongWord; Mode: TDropMode);
var
  LCanProcess : Boolean;
  LFocus      : PVirtualNode;
begin
  Effect := DROPEFFECT_MOVE;
  if CanWriteToDataSet(DropTargetNode, 0, dbcmStructure) then
  begin
    LCanProcess := True;
    if LCanProcess then
    begin
      LFocus := FocusedNode;
      BeginUpdate;
      FDataLink.DataSet.DisableControls;
      FDBStatus := FDBStatus + [dbtsDataChanging, dbtsDragDrop];
      ProcessDrop(DataObject, DropTargetNode, Effect, amAddChildLast);
      Effect := DROPEFFECT_LINK;
      FocusedNode := nil;
      EndUpdate;
      FDataLink.DataSet.EnableControls;
      FDBStatus := FDBStatus - [dbtsDataChanging, dbtsDragDrop];
      FCurId := 0.0;
      FocusedNode := LFocus;
    end
    else
      Effect := DROPEFFECT_NONE;
  end
  else
    Effect := DROPEFFECT_NONE;
  inherited;
end;

procedure TBaseVirtualDBTreeEx.DoNodeMoved(Node: PVirtualNode);
var
  LData     : PDBVTData;
  LPath     : string;
  LParent   : PVirtualNode;
  LParentId : Double;
  LLevel    : Integer;
begin
  if (dbtsDragDrop in FDBStatus) then
  begin
    LParentId := 0.0;
    LLevel := 0;
    LParent := Node.Parent;
    if LParent <> RootNode then
    begin
      LData := GetNodeData(LParent);
      LLevel := LData.Level + 1;
      LParentId := LData.Id;
      if (dboPathStructure in FDBOptions)
        or (dboWriteSecondary in FDBOptions) then
      begin
        LPath := FloatToStr(LParentId);
        LParent := LParent.Parent;
        while LParent <> RootNode do
        begin
          LData := GetNodeData(LParent);
          LPath := Format('%d.%s', [LData.Id, LPath]);
          LParent := LParent.Parent;
        end;
      end;
    end;
    LData := GetNodeData(Node);
    LData.Level := LLevel;
    FDataLink.DataSet.Locate(FKeyFieldName, LData.Id, []);
    FDataLink.DataSet.Edit;
    if (dboPathStructure in FDBOptions) or
      (dboWriteSecondary in FDBOptions) then
    begin
      DoWritePathToDB(LPath);
      FPathField.AsString := LPath;
    end;
    if (dboParentStructure in FDBOptions)
      or (dboWriteSecondary in FDBOptions) then
    begin
      FParentField.AsFloat := LParentId;
    end;
    if dboWriteLevel in FDBOptions then
    begin
      FLevelField.AsInteger := LLevel;
    end;
    FDataLink.DataSet.Post;
    inherited;
    Node := GetFirstChild(Node);
    while Assigned(Node) do
    begin
      DoNodeMoved(Node);
      Node := GetNextSibling(Node);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.DoFreeNode(Node: PVirtualNode);
var
  LData   : PDBVTData;
  LDBData : PDBNodeData;
begin
  LData := GetNodeData(Node);
  if (LData.Status = dbnsDelete) then
  begin
    if Assigned(FDataLink)
      and FDataLink.DataSet.Locate(FKeyFieldName, LData.Id, []) then
      FDataLink.DataSet.Delete;
  end;
  LDBData := PDBNodeData(GetDBNodeData(Node));
  if Assigned(LDBData.Image) then
    FreeAndNil(LDBData.Image);
  inherited;
end;

procedure TBaseVirtualDBTreeEx.DoOpeningDataSet(var Allow: Boolean);
begin
  Allow := (FViewField <> nil);
  if Allow then
  begin
    if Assigned(FOnOpeningDataSet) then
      FOnOpeningDataSet(Self, Allow);
  end;
end;

procedure TBaseVirtualDBTreeEx.DoAfterItemPaint(Canvas: TCanvas;
  Node: PVirtualNode; const ItemRect: TRect);
var
  LPos    : Integer;
  LLevel  : Integer;
  P       : TPicture;
  LDBData : PDBNodeData;
begin
  inherited DoAfterItemPaint(Canvas, Node, ItemRect);
  LDBData := PDBNodeData(GetDBNodeData(Node));
  if Assigned(LDBData.Image) and (LDBData.Image.Size > 0) then
  begin
    P := TPicture.Create;
    try
      LLevel := GetNodeLevel(Node) + 1;
      LPos := 4 + LLevel * Indent;
      LDBData.Image.Position := 0;
      P.LoadFromStream(LDBData.Image);
      Canvas.FillRect(LPos, ItemRect.Top + 1, LPos + 16, ItemRect.Top + 1 + 16);
      Canvas.Draw(LPos , ItemRect.Top + 1, P.Graphic);
    finally
      FreeAndNil(P);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field:
  TField; var UpdateNode: Boolean);
begin
  if Assigned(FOnNodeDataChanged) then
    FOnNodeDataChanged(Self, Node, Field, UpdateNode);
end;

procedure TBaseVirtualDBTreeEx.DoReadPathFromDB(var APath: string);
begin
  if Assigned(FOnReadPathFromDB) then
    FOnReadPathFromDB(Self, APath);
end;

procedure TBaseVirtualDBTreeEx.DoWritePathToDB(var APath: string);
begin
  if Assigned(FOnWritePathToDB) then
    FOnWritePathToDB(Self, APath);
end;

function TBaseVirtualDBTreeEx.DoGetImageIndex(Node: PVirtualNode; Kind:
  TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer
): TCustomImageList;
begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);

  if Column = Header.MainColumn then
  begin
    if (Kind = ikNormal) or (Kind = ikSelected) then
    begin
      Index := PDBNodeData(GetDBNodeData(Node)).ImageIndex;
    end;
  end;

  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self, Node, Kind, Column, Ghosted, Index);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TBaseVirtualDBTreeEx.OnDragOverHandler(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode:
  TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := CanWriteToDataSet(DropTargetNode, 0, dbcmStructure);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TBaseVirtualDBTreeEx.SetFocusToNode(Node: PVirtualNode);
begin
  if Assigned(FocusedNode) then
    Selected[FocusedNode] := False;
  FocusedNode := Node;
  if Assigned(Node) then
  begin
    Selected[Node] := True;
    FullyVisible[Node] := True;
  end;
end;

function TBaseVirtualDBTreeEx.FindChild(Node: PVirtualNode; ID: Double):
PVirtualNode;
var
  LData : PDBVTData;
begin
  Result := GetFirstChild(Node);
  while Assigned(Result) do
  begin
    LData := GetNodeData(Result);
    if LData.Id = ID then
      Break;
    Result := GetNextSibling(Result);
  end;
end;

function TBaseVirtualDBTreeEx.FindNode(Start: PVirtualNode; ID: Double):
PVirtualNode;
var
  LData : PDBVTData;
begin
  if Assigned(Start) then
    Result := Start
  else
    Result := GetFirst;
  while Assigned(Result) do
  begin
    LData := GetNodeData(Result);
    if LData.Id = ID then
      break;
    Result := GetNext(Result);
  end;
end;

procedure TBaseVirtualDBTreeEx.GoToRec(AString: string; AMode: TDBVTGoToMode);
var
  LText   : string;
  LNode   : PVirtualNode;
  LColumn : TColumnIndex;
begin
  LText := '';
  EndEditNode;
  LColumn := Header.MainColumn;
  case AMode of
    gtmFromFirst:
    begin
      LNode := GetFirst;
      AMode := gtmNext;
    end;
    gtmNext:
      LNode := GetNext(FocusedNode);
    gtmPrev:
      LNode := GetPrevious(FocusedNode);
    else
      LNode := nil;
  end;
  while Assigned(LNode) do
  begin
    DoGetText(LNode, LColumn, ttNormal, LText);
    if Pos(AString, LText) = 1 then
      break;
    if AMode = gtmNext then
      LNode := GetNext(LNode)
    else
      LNode := GetPrevious(LNode);
  end;
  if Assigned(LNode) then
    SetFocusToNode(LNode);
end;

function TBaseVirtualDBTreeEx.GoToRec(AId: Double): Boolean;
var
  LNode : PVirtualNode;
begin
  LNode := FindNode(nil, AId);
  if Assigned(LNode) then
    SetFocusToNode(LNode);
  Result := LNode <> nil;
end;

procedure TBaseVirtualDBTreeEx.AddNode(AParent: PVirtualNode);
var
  LLevel    : Integer;
  LParentId : Double;
  LPath     : string;
  LNode     : PVirtualNode;
  LData     : PDBVTData;
begin
  EndEditNode;
  if CanWriteToDataSet(AParent, 0, dbcmInsert) then
  begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    LNode := AddChild(AParent);
    if (AParent = nil) or (AParent = RootNode) then
    begin
      LLevel := 0;
      LParentId := 0.0;
      LPath := '';
    end
    else
    begin
      LData := GetNodeData(AParent);
      LLevel := LData.Level + 1;
      LParentId := LData.Id;
      if (dboPathStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
      then
      begin
        LPath := FloatToStr(LParentId);
        AParent := AParent.Parent;
        while AParent <> RootNode do
        begin
          LData := GetNodeData(AParent);
          LPath := Format('%d.%s', [LData.Id, LPath]);
          AParent := AParent.Parent;
        end;
      end;
    end;
    LData := GetNodeData(LNode);
    LData.Id := 0.0;
    LData.Level := LLevel;
    LData.Parent := nil;
    FDBStatus := FDBStatus + [dbtsInsert];
    FDataLink.DataSet.Insert;
    if not (dboListView in FDBOptions) then
    begin
      if (dboPathStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
      then
      begin
        DoWritePathToDB(LPath);
        FPathField.AsString := LPath;
      end;
      if (dboParentStructure in FDBOptions) or
        (dboWriteSecondary in FDBOptions)
      then
        FParentField.AsFloat := LParentId;
      if (dboWriteLevel in FDBOptions) then
        FLevelField.AsInteger := LLevel;
    end;
    DoReadNodeFromDB(LNode);
    FCurId := 0.0;
    SetFocusToNode(LNode);
    FDBStatus := FDBStatus - [dbtsDataChanging];
    EditNode(LNode, Header.MainColumn);
  end;
end;

procedure TBaseVirtualDBTreeEx.DeleteSelection;
var
  LData  : PDBVTData;
  LNode  : PVirtualNode;
  LTemp  : PVirtualNode;
  LLast  : PVirtualNode;
  LFocus : PVirtualNode;
begin
  if not (dbtsDataChanging in FDBStatus) and Assigned(FocusedNode) and
    (SelectedCount > 0) and not (dboReadOnly in FDBOptions) and
    FDataLink.Active and Assigned(FKeyField) and not FDataLink.ReadOnly then
  begin
    LNode := GetFirst;
    LFocus := FocusedNode;
    while Selected[LFocus.Parent] do
      LFocus := LFocus.Parent;
    LTemp := LFocus;
    repeat
      LFocus := GetNextSibling(LFocus);
    until
      not Assigned(LFocus) or not Selected[LFocus];
    if not Assigned(LFocus) then
    begin
      LFocus := LTemp;
      repeat
        LFocus := GetPreviousSibling(LFocus);
      until
        not Assigned(LFocus) or not Selected[LFocus];
      if not Assigned(LFocus) then
        LFocus := LTemp.Parent;
      if LFocus = RootNode then
        LFocus := nil;
    end;
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    try
      FDataLink.DataSet.DisableControls;
      try
        while Assigned(LNode) do
        begin
          if Selected[LNode] then
          begin
            LTemp := LNode;
            LLast := GetNextSibling(LNode);
            repeat
              LData := GetNodeData(LTemp);
              LData.Status := dbnsDelete;
              LTemp := GetNext(LTemp);
            until LTemp = LLast;
            if not Assigned(LTemp) and (LNode.Parent <> RootNode) then
              LTemp := GetNextSibling(LNode.Parent);
            DeleteNode(LNode);
            LNode := LTemp;
          end
          else
            LNode := GetNextVisible(LNode);
        end;
      finally
        FDataLink.DataSet.EnableControls;
      end;
    finally
      EndUpdate;
    end;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    if Assigned(LFocus) and (LFocus <> RootNode) then
      SetFocusToNode(LFocus);
  end;
end;

function TBaseVirtualDBTreeEx.GetDBNodeData(Node: PVirtualNode): Pointer;
begin
  if not Assigned(Node) or (DBNodeDataSize = 0) then
    Result := nil
  else
    Result := PAnsiChar(GetNodeData(Node)) + FDBDataSize;
end;

procedure TBaseVirtualDBTreeEx.RefreshNodes;
var
  LData : PDBVTData;
  LNode : PVirtualNode;
  LTemp : PVirtualNode;
  I     : Integer;
begin
  if not (dbtsDataChanging in FDBStatus) and CanOpenDataSet then
  begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    FMaxLevel := 0;
    FCurId := 0.0;
    if dboAlwaysStructured in FDBOptions then
    begin
      if not (dbtsStructured in FDBStatus) then
        Clear
      else if dboListView in FDBOptions then
      begin
        FDBOptions := FDBOptions - [dboListView];
        ToggleListView;
        FDBOptions := FDBOptions + [dboListView];
      end;
      FDBStatus := FDBStatus + [dbtsStructured];
    end
    else
    begin
      if dboListView in FDBOptions then
        FDBStatus := FDBStatus - [dbtsStructured]
      else
      begin
        if not (dbtsStructured in FDBStatus) then
          Clear;
        FDBStatus := FDBStatus + [dbtsStructured];
      end;
    end;
    LTemp := GetFirst;
    if not Assigned(LTemp) then
      FDBStatus := FDBStatus + [dbtsEmpty];
    while Assigned(LTemp) do
    begin
      LData := GetNodeData(LTemp);
      if LData.Status = dbnsRefreshed then
        LData.Status := dbnsNone;
      LTemp := GetNext(LTemp);
    end;
    FDataLink.DataSet.DisableControls;
    if not FDataLink.DataSet.EOF or not FDataLink.DataSet.Bof then
      FCurId := FKeyField.AsFloat;
    I := 0;
    while not FDataLink.DataSet.EOF do
    begin
      RefreshNode;
      FDataLink.DataSet.Next;
      Inc(I);
    end;
    if (I > 0) then
      FDataLink.DataSet.MoveBy(-I);
    I := 0;
    while not (FDataLink.DataSet.Bof) do
    begin
      RefreshNode;
      FDataLink.DataSet.Prior;
      Inc(I);
    end;
    if (I > 0) then
      FDataLink.DataSet.MoveBy(I);
    if (dboTrackCursor in FDBOptions) and Assigned(FocusedNode) then
    begin
      Selected[FocusedNode] := False;
      FocusedNode := nil;
    end;
    LTemp := GetFirst;
    while Assigned(LTemp) do
    begin
      LData := GetNodeData(LTemp);
      if (LData.Status <> dbnsRefreshed) then
      begin
        LNode := GetNextSibling(LTemp);
        DeleteNode(LTemp);
      end
      else
      begin
        if (dbtsStructured in FDBStatus)
          and (dboParentStructure in FDBOptions) then
        begin
          LData.Level := GetNodeLevel(LTemp);
          FMaxLevel := Max(FMaxLevel, LData.Level);
        end;
        if (dboTrackCursor in FDBOptions) and not Assigned(FocusedNode) and
          (LData.Id = FCurId) then
        begin
          Selected[LTemp] := True;
          FocusedNode := LTemp;
          FullyVisible[LTemp] := True;
        end;
        LNode := GetNext(LTemp);
      end;
      LTemp := LNode;
    end;
    FDataLink.DataSet.EnableControls;
    FDBStatus := FDBStatus - [dbtsDataChanging, dbtsChanged, dbtsEmpty];
    if (dboAlwaysStructured in FDBOptions) and (dboListView in FDBOptions) then
      ToggleListView;
    if not (dboListView in FDBOptions) and not (dboViewAll in FDBOptions) and
      not (dbtsToggleAll in FDBStatus) then
      ToggleViewMode;
    EndUpdate;
  end;
end;

procedure TBaseVirtualDBTreeEx.RefreshNode;
begin
  if not (dbtsStructured in FDBStatus) then
    RefreshListNode
  else if (dboPathStructure in FDBOptions) then
    RefreshNodeByPath
  else
    RefreshNodeByParent;
end;

procedure TBaseVirtualDBTreeEx.RefreshListNode;
var
  LData : PDBVTData;
  LNode : PVirtualNode;
  LId   : Double;
begin
  LId := FKeyField.AsFloat;
  if dbtsEmpty in FDBStatus then
    LNode := nil
  else
    LNode := FindChild(nil, LId);
  if not Assigned(LNode) then
  begin
    LNode := AddChild(nil);
    ValidateNode(LNode, False);
    LData := GetNodeData(LNode);
    LData.Id := LId;
    LData.Parent := nil;
    LData.Status := dbnsInited;
  end;
  ReadNodeFromDB(LNode);
end;

procedure TBaseVirtualDBTreeEx.RefreshNodeByPath;
var
  LPos   : Integer;
  LId    : Double;
  LLevel : Integer;
  LNode  : PVirtualNode;
  LLast  : PVirtualNode;
  LData  : PDBVTData;
  LTemp  : string;
  LPath  : string;
begin
  LData := nil;
  LPath := FPathField.AsString;
  LLast := RootNode;
  DoReadPathFromDB(LPath);
  LTemp := FloatToStr(FKeyField.AsFloat);
  if LPath = '' then
    LPath := LTemp
  else
    LPath := Format('%s.%s', [LPath, LTemp]);

  repeat
    LNode := LLast;
    LPos := System.Pos('.', LPath);
    if (LPos = 0) then
    begin
      LTemp := LPath;
      LPos := Length(LPath);
    end
    else
      LTemp := Copy(LPath, 1, LPos - 1);

    LId := StrToFloat(LTemp);
    LLast := FindChild(LNode, LId);

    if Assigned(LLast) then
      Delete(LPath, 1, LPos);
  until not Assigned(LLast) or (LPath = '');

  if LPath = '' then
  begin
    LNode := LLast;
    LData := GetNodeData(LNode);
  end
  else
  begin
    if (LNode = RootNode) then
      LLevel := -1
    else
    begin
      LData := GetNodeData(LNode);
      LLevel := LData.Level;
    end;

    repeat
      LPos := System.Pos('.', LPath);
      if (LPos = 0) then
      begin
        LTemp := LPath;
        LPos := Length(LPath);
      end
      else
        LTemp := Copy(LPath, 1, LPos - 1);

      LId := StrToFloat(LTemp);
      LNode := AddChild(LNode);
      ValidateNode(LNode, False);
      LData := GetNodeData(LNode);
      Inc(LLevel);
      LData.Id := LId;
      LData.Level := LLevel;
      LData.Status := dbnsInited;
      LData.Parent := nil;
      Delete(LPath, 1, LPos);
    until LPath = '';
  end;
  if LData <> nil then
    FMaxLevel := Max(FMaxLevel, LData.Level);
  if LNode <> nil then
    ReadNodeFromDB(LNode);
end;

procedure TBaseVirtualDBTreeEx.RefreshNodeByParent;
var
  LId       : Double;
  LParentId : Double;
  LData     : PDBVTData;
  LThis     : PVirtualNode;
  LParent   : PVirtualNode;
  LTemp     : PVirtualNode;
  LCreated  : Boolean;
begin
  LId := FKeyField.AsFloat;
  LParentId := FParentField.AsFloat;
  if (LId = 0.0) then
  begin
    Exit;
  end;
  LThis := nil;
  LParent := nil;
  LTemp := GetFirst;
  if (LParentId = 0.0) or (LParentId = LId) then
  begin
    LParent := RootNode;
    LParentId := 0.0;
  end;
  while Assigned(LTemp) and (not Assigned(LThis) or not Assigned(LParent)) do
  begin
    LData := GetNodeData(LTemp);
    if (LData.Id = LId) then
    begin
      if (LData.Status = dbnsRefreshed) then
      begin
        Exit;
      end;
      LThis := LTemp;
    end;
    if (LData.Id = LParentId) and (LParentId <> 0.0) then
      LParent := LTemp;
    LTemp := GetNext(LTemp);
  end;
  if not Assigned(LParent) then
  begin
    LParent := AddChild(nil);
    ValidateNode(LParent, False);
    LData := GetNodeData(LParent);
    LData.Id := LParentId;
    LData.Status := dbnsInited;
    LData.Parent := nil;
  end;
  LCreated := True;
  if Assigned(LThis) then
  begin
    if (LThis.Parent <> LParent) then
    begin
      if HasAsParent(LParent, LThis) then
      begin
        LData := GetNodeData(LParent);
        if (LData.Status = dbnsRefreshed) then
        begin
          Exit;
        end;
        LTemp := LThis;
        LThis := LParent;
        LParent := LTemp;
        LData := GetNodeData(LParent);
        LData.Id := LParentId;
      end
      else
      begin
        MoveTo(LThis, LParent, amAddChildLast, False);
      end;
    end;
  end
  else
  begin
    LCreated := False;
    LThis := AddChild(LParent);
    ValidateNode(LThis, False);
  end;
  LData := GetNodeData(LThis);
  LData.Id := LId;
  if not LCreated then
    LData.Status := dbnsInited;
  ReadNodeFromDB(LThis);
end;

procedure TBaseVirtualDBTreeEx.UpdateTree;
begin
  Clear;
  RefreshNodes;
end;

procedure TBaseVirtualDBTreeEx.ToggleListView;
var
  LData : PDBVTData;
  LNode : PVirtualNode;
  LTemp : PVirtualNode;
begin
  if dbtsDragDrop in FDBStatus then
    Exit;
  BeginUpdate;
  if (dboListView in FDBOptions) then
  begin
    LNode := GetFirst;
    while Assigned(LNode) do
    begin
      LData := GetNodeData(LNode);
      if (LNode.Parent <> RootNode) then
      begin
        LData.Parent := LNode.Parent;
        LTemp := GetNextSibling(LNode);
        if not Assigned(LTemp) then
        begin
          LTemp := GetLastChild(LNode);
          if Assigned(LTemp) then
          begin
            LTemp := GetNext(LTemp);
            if not Assigned(LTemp) then
              LTemp := GetNext(LNode);
          end
          else
            LTemp := GetNext(LNode);
        end;
        MoveTo(LNode, RootNode, amAddChildLast, False);
        LNode := LTemp;
      end
      else
        LNode := GetNext(LNode);
    end;
    if not (dboViewAll in FDBOptions) then
      ToggleViewMode;
  end
  else
  begin
    LNode := GetFirst;
    while Assigned(LNode) do
    begin
      LData := GetNodeData(LNode);
      if LData.Parent <> nil then
      begin
        LTemp := GetNextSibling(LNode);
        MoveTo(LNode, LData.Parent, amAddChildLast, False);
        LData.Parent := nil;
        LNode := LTemp;
      end
      else
        LNode := GetNextSibling(LNode);
    end;
    if not (dboViewAll in FDBOptions) then
    begin
      FDBStatus := FDBStatus + [dbtsToggleAll];
      ToggleViewMode;
      FDBStatus := FDBStatus - [dbtsToggleAll];
    end;
  end;
  if Assigned(FocusedNode) then
    FullyVisible[FocusedNode] := True;
  EndUpdate;
end;

procedure TBaseVirtualDBTreeEx.ResetFields;
begin
  FKeyField := nil;
  FParentField := nil;
  FPathField := nil;
  FLevelField := nil;
  FViewField := nil;
  FImgIdxField := nil;
end;

procedure TBaseVirtualDBTreeEx.InitFields;
begin
  if (FKeyFieldName <> '') then
    FKeyField := FDataLink.DataSet.FieldByName(FKeyFieldName);
  if (FParentFieldName <> '') then
    FParentField := FDataLink.DataSet.FieldByName(FParentFieldName);
  if (FPathFieldName <> '') then
    FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
  if (FLevelFieldName <> '') then
    FLevelField := FDataLink.DataSet.FieldByName(FLevelFieldName);
  if FViewFieldName <> '' then
    FViewField := DataSource.DataSet.FieldByName(FViewFieldName);
  if FImgIdxFieldName <> '' then
    FImgIdxField := DataSource.DataSet.FieldByName(FImgIdxFieldName);
  if FImageFieldName <> '' then
    FImageField := DataSource.DataSet.FieldByName(FImageFieldName);
end;

function TBaseVirtualDBTreeEx.CanOpenDataSet: Boolean;
begin
  Result := (FKeyField <> nil);
  if Result and (not (dboListView in FDBOptions) or (dboAlwaysStructured in
    FDBOptions)) then
    Result := ((dboPathStructure in FDBOptions) and Assigned(FPathField)) or
      ((dboParentStructure in FDBOptions) and Assigned(FParentField));
  if Result then
    DoOpeningDataSet(Result);
end;

procedure TBaseVirtualDBTreeEx.ReadNodeFromDB(Node: PVirtualNode);
var
  LData : PDBVTData;
begin
  LData := GetNodeData(Node);
  if (LData.Status <> dbnsNone) and (LData.Status <> dbnsRefreshed) then
    DoReadNodeFromDB(Node);
  LData.Status := dbnsRefreshed;
end;

procedure TBaseVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
begin
  if Assigned(FOnReadNodeFromDB) then
    FOnReadNodeFromDB(Self, Node);
end;

function TBaseVirtualDBTreeEx.CanWriteToDataSet(Node: PVirtualNode; Column:
  TColumnIndex; ChangeMode: TDBVTChangeMode): Boolean;
begin
  Result := not (dboReadOnly in FDBOptions) and Assigned(FKeyField) and
    FDataLink.DataSet.CanModify;
  if Result then
  begin
    if dboListView in DBOptions then
    begin
      if (ChangeMode = dbcmStructure) or (ChangeMode = dbcmInsert) then
        Result := (Node = nil) or (Node = RootNode);
    end
    else if (ChangeMode = dbcmStructure) or (ChangeMode = dbcmInsert) then
    begin
      Result := (((dboPathStructure in FDBOptions) or (dboWriteSecondary in
        FDBOptions)) and Assigned(FPathField) and FPathField.CanModify) or
        (((dboParentStructure in FDBOptions) or
        (dboWriteSecondary in FDBOptions))
        and Assigned(FParentField) and FParentField.CanModify);
      if Result and (dboWriteLevel in FDBOptions) then
        Result := Assigned(FLevelField) and FLevelField.CanModify;
    end;
    if Result then
      DoWritingDataSet(Node, Column, ChangeMode, Result);
  end;
end;

procedure TBaseVirtualDBTreeEx.DoWritingDataSet(Node: PVirtualNode; Column:
  TColumnIndex; ChangeMode: TDBVTChangeMode; var Allow: Boolean);
begin
  if (ChangeMode = dbcmEdit) and (Column = Header.MainColumn) then
    Allow := FViewField.CanModify;
  if Allow then
  begin
    if Assigned(FOnWritingDataSet) then
      FOnWritingDataSet(Self, Node, Column, ChangeMode, Allow);
  end;
end;

function TBaseVirtualDBTreeEx.DoChecking(Node: PVirtualNode; var NewCheckState:
  TCheckState): Boolean;
begin
  if dbtsDataChanging in FDBStatus then
    Result := True
  else if (dboShowChecks in FDBOptions) and (dboAllowChecking in FDBOptions)
  then
    Result := inherited DoChecking(Node, NewCheckState)
  else
    Result := False;
end;

procedure TBaseVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
begin
  if not (dbtsDataChanging in FDBStatus) then
  begin
    BeginUpdate;
    if CheckState[Node] = csCheckedNormal then
    begin
      if dboCheckChildren in FDBOptions then
        CheckAllChildren(Node);
    end
    else if not (dboViewAll in FDBOptions) then
      ToggleViewMode;
    if not (dbtsChecking in FDBStatus) and (dboPathStructure in
      FDBOptions) then
      RefreshNodes;
    EndUpdate;
    inherited;
  end;
end;

procedure TBaseVirtualDBTreeEx.CheckAllChildren(Node: PVirtualNode);
begin
  if (dboShowChecks in FDBOptions) and (dboAllowChecking in FDBOptions) then
  begin
    FDBStatus := FDBStatus + [dbtsChecking];
    Node := GetFirstChild(Node);
    while Assigned(Node) do
    begin
      CheckState[Node] := csCheckedNormal;
      Node := GetNextSibling(Node);
    end;
    FDBStatus := FDBStatus - [dbtsChecking];
  end;
end;

procedure TBaseVirtualDBTreeEx.UnCheckAll(Node: PVirtualNode; OnlyChildren:
  Boolean);
var
  LChanged : Boolean;
  LLast    : PVirtualNode;
begin
  if (dboShowChecks in FDBOptions) and (dboAllowChecking in FDBOptions) then
  begin
    LChanged := False;
    LLast := GetNextSibling(Node);
    if not Assigned(LLast) then
    begin
      LLast := GetLastChild(Node);
      if not Assigned(LLast) then
        LLast := Node;
      LLast := GetNext(LLast);
    end;
    if OnlyChildren then
      Node := GetNext(Node);
    while Node <> LLast do
    begin
      if CheckState[Node] <> csUncheckedNormal then
      begin
        CheckState[Node] := csUncheckedNormal;
        if CheckState[Node] = csUncheckedNormal then
          LChanged := True;
      end;
    end;
    if LChanged then
      ToggleViewMode;
  end;
end;

procedure TBaseVirtualDBTreeEx.DoInitNode(AParent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  LData : PDBNodeData;
begin
  inherited DoInitNode(AParent, Node, InitStates);
  Node.CheckType   := ctCheckBox;
  Node.CheckState  := csUncheckedNormal;
  LData            := PDBNodeData(GetDBNodeData(Node));
  LData.Image      := TMemoryStream.Create;
  LData.ImageIndex := -1;
  LData.Text       := '';
end;

procedure TBaseVirtualDBTreeEx.ToggleViewMode;
var
  LNode : PVirtualNode;
begin
  if not (dbtsDataChanging in FDBStatus) and (dboShowChecks in FDBOptions) then
  begin
    BeginUpdate;
    if dboViewAll in FDBOptions then
      RefreshNodes()
    else
    begin
      if dbtsToggleAll in FDBStatus then
        RefreshNodes;
      LNode := GetLastChild(RootNode);
      while Assigned(LNode) and (LNode <> RootNode) do
      begin
        if (CheckState[LNode] <> csCheckedNormal) and not
          Assigned(GetFirstChild(LNode)) then
        begin
          DeleteNode(LNode);
          if dboListView in FDBOptions then
            FDBStatus := FDBStatus - [dbtsStructured];
        end;
        LNode := GetPrevious(LNode);
      end;
    end;
    EndUpdate;
  end;
end;

function TBaseVirtualDBTreeEx.HasVisibleChildren(Node: PVirtualNode): Boolean;
var
  LLast : PVirtualNode;
begin
  Result := False;
  if Assigned(Node) then
  begin
    LLast := GetNextSibling(Node);
    if not Assigned(LLast) then
    begin
      LLast := GetLastChild(Node);
      if not Assigned(LLast) then
        LLast := Node;
      LLast := GetNext(LLast);
    end;
    Node := GetNext(Node);
    while Node <> LLast do
    begin
      if IsVisible[Node] then
      begin
        Result := True;
        Break;
      end;
      Node := GetNext(Node);
    end;
  end;
end;

procedure TBaseVirtualDBTreeEx.ExpandAll;
var
  LNode : PVirtualNode;
begin
  LNode := GetFirst;
  BeginUpdate;
  while Assigned(LNode) do
  begin
    Expanded[LNode] := True;
    LNode := GetNext(LNode);
  end;
  EndUpdate;
end;

procedure TBaseVirtualDBTreeEx.CollapseAll;
var
  LNode : PVirtualNode;
begin
  LNode := GetFirst;
  BeginUpdate;
  while Assigned(LNode) do
  begin
    Expanded[LNode] := False;
    LNode := GetNext(LNode);
  end;
  EndUpdate;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TVirtualDBTreeEx'}
{$REGION 'construction and destruction'}
constructor TVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  DBNodeDataSize := SizeOf(TDBNodeData);
end;
{$ENDREGION}

procedure TVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  LData : PDBNodeData;
begin
  inherited DoReadNodeFromDB(Node);
  NodeText[Node] := ViewField.AsString;
  LData := PDBNodeData(GetDBNodeData(Node));
  if Assigned(ImgIdxField) then
  begin
    LData.ImageIndex := ImgIdxField.AsInteger;
  end
  else
    LData.ImageIndex := -1;
  if Assigned(ImageField) then
  begin
    (ImageField as TBlobField).SaveToStream(LData.Image);
  end;
end;

procedure TVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field: TField;
  var UpdateNode: Boolean);
var
  LData: PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if Field = ImgIdxField then
  begin
    LData := PDBNodeData(GetDBNodeData(Node));
    LData.ImageIndex := Field.AsInteger;
    UpdateNode := True;
  end
  else if Field = ImageField then
  begin
    LData := PDBNodeData(GetDBNodeData(Node));
    (ImageField as TBlobField).SaveToStream(LData.Image);
  end;
end;

procedure TVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var AText: string);
begin
  if Assigned(Node) and (Node <> RootNode) then
  begin
    if (Column = Header.MainColumn) and (TextType = ttNormal) then
      AText := NodeText[Node]
    else
      inherited;
  end;
end;

procedure TVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
begin
  if Column = Header.MainColumn then
    ViewField.AsString := Text;
end;

procedure TVirtualDBTreeEx.DoWritingDataSet(Node: PVirtualNode; Column:
  TColumnIndex; ChangeMode: TDBVTChangeMode; var Allow: Boolean);
begin
  if ChangeMode = dbcmEdit then
  begin
    if Column = Header.MainColumn then
      inherited
    else
      Allow := False;
  end;
end;

function TVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column:
  TColumnIndex): Integer;
begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
  begin
    if Column = Header.MainColumn then
    begin
      if NodeText[Node1] > NodeText[Node2] then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;

function TVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): string;
begin
  if Assigned(Node) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text;
end;

procedure TVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; const Value:
  string);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;
{$ENDREGION}

{$REGION 'TCustomDBCheckVirtualDBTreeEx'}
{$REGION 'construction and destruction'}
constructor TCustomDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckDataLink := TVirtualDBTreeExDataLink.Create(Self);
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking];
  FResultField := nil;
end;

destructor TCustomDBCheckVirtualDBTreeEx.Destroy;
begin
  FResultField := nil;
  FCheckDataLink.Free;
  inherited Destroy;
end;
{$ENDREGION}

procedure TCustomDBCheckVirtualDBTreeEx.CheckDataLinkActiveChanged;
begin
  if not (csDesigning in ComponentState) then
  begin
    FResultField := nil;
    if FCheckDataLink.Active then
    begin
      if FResultFieldName <> '' then
        FResultField := FCheckDataLink.DataSet.FieldByName(FResultFieldName);
    end;
    UpdateTree;
  end;
end;

procedure TCustomDBCheckVirtualDBTreeEx.DoOpeningDataSet(var Allow: Boolean);
begin
  if Assigned(FResultField) then
    inherited
  else
    Allow := False;
end;

procedure TCustomDBCheckVirtualDBTreeEx.SetCheckDataSource(Value: TDataSource);
begin
  FCheckDataLink.DataSource := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

function TCustomDBCheckVirtualDBTreeEx.GetCheckList: TStringList;
var
  LData : PDBVTData;
  LNode : PVirtualNode;
begin
  Result := TStringList.Create;
  LNode  := GetFirst;
  while Assigned(LNode) do
  begin
    LData := GetNodeData(LNode);
    if CheckState[LNode] = csCheckedNormal then
      Result.Add(FloatToStr(LData.Id));
    LNode := GetNext(LNode);
  end;
end;

function TCustomDBCheckVirtualDBTreeEx.GetCheckDataSource: TDataSource;
begin
  Result := FCheckDataLink.DataSource;
end;

procedure TCustomDBCheckVirtualDBTreeEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(FCheckDataLink) and (AComponent =
    CheckDataSource) then
    CheckDataSource := nil;
end;

procedure TCustomDBCheckVirtualDBTreeEx.SetResultFieldName(
  const Value: string);
begin
  if FResultFieldName <> Value then
  begin
    FResultFieldName := Value;
    CheckDataLinkActiveChanged;
  end;
end;

function TCustomDBCheckVirtualDBTreeEx.DoChecking(Node: PVirtualNode; var
  NewCheckState: TCheckState): Boolean;
begin
  if dbtsDataChanging in DBStatus then
    Result := inherited DoChecking(Node, NewCheckState)
  else if Assigned(FResultField) and FResultField.CanModify then
    Result := inherited DoChecking(Node, NewCheckState)
  else
    Result := False;
end;

procedure TCustomDBCheckVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
var
  LData : PDBVTData;
begin
  if not (dbtsDataChanging in DBStatus) then
  begin
    LData := GetNodeData(Node);
    if CheckState[Node] = csCheckedNormal then
    begin
      FCheckDataLink.DataSet.Insert;
      FResultField.AsFloat := LData.Id;
      FCheckDataLink.DataSet.Post;
    end
    else if FCheckDataLink.DataSet.Locate(FResultFieldName, LData.Id, []) then
      FCheckDataLink.DataSet.Delete;
  end;
  inherited;
end;

procedure TCustomDBCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  LData : PDBVTData;
begin
  inherited;
  LData := GetNodeData(Node);
  if FCheckDataLink.DataSet.Locate(FResultFieldName, LData.Id, []) then
    CheckState[Node] := csCheckedNormal
  else
    CheckState[Node] := csUncheckedNormal;
end;
{$ENDREGION}

{$REGION 'TDBCheckVirtualDBTreeEx'}
constructor TDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  DBNodeDataSize := SizeOf(TDBNodeData);
end;

procedure TDBCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  LData : PDBNodeData;
begin
  inherited;
  NodeText[Node] := ViewField.AsString;
  LData := PDBNodeData(GetDBNodeData(Node));
  if ImgIdxField <> nil then
  begin
    LData.ImageIndex := ImgIdxField.AsInteger;
  end
  else
    LData.ImageIndex := -1;
end;

procedure TDBCheckVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field:
  TField; var UpdateNode: Boolean);
var
  LData : PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if Field = ImgIdxField then
  begin
    LData := PDBNodeData(GetDBNodeData(Node));
    LData.ImageIndex := Field.AsInteger;
    UpdateNode := True;
  end;
end;

procedure TDBCheckVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column:
  TColumnIndex; TextType: TVSTTextType; var Text: string);
begin
  if Assigned(Node) and (Node <> RootNode) then
  begin
    if (Column = Header.MainColumn) and (TextType = ttNormal) then
      Text := NodeText[Node]
    else
      inherited;
  end;
end;

procedure TDBCheckVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column:
  TColumnIndex; const Text: string);
begin
  if Column = Header.MainColumn then
    ViewField.AsString := Text;
end;

procedure TDBCheckVirtualDBTreeEx.DoWritingDataSet(Node: PVirtualNode; Column:
  TColumnIndex; ChangeMode: TDBVTChangeMode; var Allow: Boolean);
begin
  if ChangeMode = dbcmEdit then
  begin
    if Column = Header.MainColumn then
      inherited
    else
      Allow := False;
  end;
end;

function TDBCheckVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column:
  TColumnIndex): Integer;
begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
  begin
    if Column = Header.MainColumn then
    begin
      if NodeText[Node1] > NodeText[Node2] then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;

procedure TDBCheckVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; const Value:
  string);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;

function TDBCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): string;
begin
  if Assigned(Node) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text
  else
    Result := '';
end;
{$ENDREGION}

{$REGION 'TCustomCheckVirtualDBTreeEx'}
{$REGION 'construction and destruction'}
constructor TCustomCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TStringList.Create;
  FList.Sorted := True;
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking];
end;

destructor TCustomCheckVirtualDBTreeEx.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TCustomCheckVirtualDBTreeEx.GetCheckList: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(FList);
end;

procedure TCustomCheckVirtualDBTreeEx.SetCheckList(Value: TStringList);
begin
  FList.Assign(Value);
  UpdateTree;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TCustomCheckVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
var
  LData  : PDBVTData;
  LIndex : Integer;
begin
  if not (dbtsDataChanging in DBStatus) then
  begin
    LData := GetNodeData(Node);
    if CheckState[Node] = csCheckedNormal then
      FList.Add(FloatToStr(LData.Id))
    else
    begin
      LIndex := FList.IndexOf(FloatToStr(LData.Id));
      if LIndex <> -1 then
        FList.Delete(LIndex);
    end;
  end;
  inherited;
end;

procedure TCustomCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  LData  : PDBVTData;
  LIndex : Integer;
begin
  inherited DoReadNodeFromDB(Node);
  LData := GetNodeData(Node);
  LIndex := 0;
  if FList.Find(FloatToStr(LData.Id), LIndex) then
    CheckState[Node] := csCheckedNormal
  else
    CheckState[Node] := csUncheckedNormal;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TCheckVirtualDBTreeEx'}
{$REGION 'construction and destruction'}
constructor TCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DBNodeDataSize := SizeOf(TDBNodeData);
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  LData : PDBNodeData;
begin
  inherited DoReadNodeFromDB(Node);
  NodeText[Node] := ViewField.AsString;
  LData := PDBNodeData(GetDBNodeData(Node));
  if Assigned(ImgIdxField) then
    LData.ImageIndex := ImgIdxField.AsInteger
  else
    LData.ImageIndex := -1;
  if Assigned(ImageField) then
  begin
    (ImageField as TBlobField).SaveToStream(LData.Image);
  end;
end;

procedure TCheckVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field:
  TField; var UpdateNode: Boolean);
var
  LData : PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if Field = ImgIdxField then
  begin
    LData := PDBNodeData(GetDBNodeData(Node));
    LData.ImageIndex := Field.AsInteger;
    UpdateNode := True;
  end
  else if Field = ImageField then
  begin
    LData := PDBNodeData(GetDBNodeData(Node));
    (ImageField as TBlobField).SaveToStream(LData.Image);
    UpdateNode := True;
  end;
end;

procedure TCheckVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column:
  TColumnIndex; TextType: TVSTTextType; var Text: string);
begin
  if Assigned(Node) and (Node <> RootNode) then
  begin
    if (Column = Header.MainColumn) and (TextType = ttNormal) then
      Text := NodeText[Node]
    else
      inherited DoGetText(Node, Column, TextType, Text);
  end;
end;

procedure TCheckVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column:
  TColumnIndex; const Text: string);
begin
  if Column = Header.MainColumn then
    ViewField.AsString := Text;
end;

procedure TCheckVirtualDBTreeEx.DoWritingDataSet(Node: PVirtualNode; Column:
  TColumnIndex; ChangeMode: TDBVTChangeMode; var Allow: Boolean);
begin
  if ChangeMode = dbcmEdit then
  begin
    if Column = Header.MainColumn then
      inherited
    else
      Allow := False;
  end;
end;

function TCheckVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column:
  TColumnIndex): Integer;
begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
  begin
    if Column = Header.MainColumn then
    begin
      if NodeText[Node1] > NodeText[Node2] then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TCheckVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; const Value:
  string);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;

function TCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): string;
begin
  Result := '';
  if Assigned(Node) and (Node <> RootNode) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text;
end;
{$ENDREGION}
{$ENDREGION}
end.

