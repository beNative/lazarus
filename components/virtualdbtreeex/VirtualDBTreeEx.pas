
{ Lazarus port by Tim Sinaeve (28/04/2010) }


{——————————————————————————————————————————————————————————————————————————————-
 02-JAN-2002 C.S. Phua

  - Modified from work of Vadim Sedulin and Adem Baba. Renamed all VirtualDBTree
    components to VirtualDBTreeEx to preserve the work previously done. Can't
    find a way to make it a descendant of VirtualDBTree though.

  - Changes made:
    * Changed integer type ID and Parent ID to type Double. This is done because
      integer type might not be enough to hold a hugh database's records. With
      Double type, one can store a number greater than zero (>0) up to 15 digits
      without the loss of accuracy in ID field. This gives more room to play with
      ID, for instance, I need to do this in ID:-
      xxxxxyyyyyyyyyy where xxxxx is site ID, yyyyyyyyyy is record ID of each site.
      Note: It still works with integer field as ID and ParentID even the codes
      expecting a Double field.

    * Added an ImgIdxFieldName property that can be assigned an integer field. When
      an ImageList is specified, it will take the ImageList's image as Icon of
      VirtualTree nodes by indexing it with the field specified in ImgIdxFieldName
      property. I need this feature to show different icons of treenodes, based on
      the type of treenodes, i.e. I have field ID, ParentID, NType in my database
      to form a tree.

    * Modified sorting options to enable OnCompareNodes event for custom sorting
      options. The original default sorting codes will be used if user does not
      supply OnCompareNodes event codes.

    * Changed the type name TSimpleData to TDBNodeData and move it to Interface
      section so that user who uses the method GetDBNodeData can type cast the
      pointer returned to TDBNodeData and access its member. Added an integer member
      named ImgIdx in the structure too.

    * Fixed the TreeOptions issue (finally). Now VirtualDBTreeEx user can play with
      settings in TreeOptions :)

    * Lastly, spent 10 minutes to produce new .dcr for VirtualDBTreeEx based on
      .dcr of VirtualDBTree.


 22-OCT-2001 Vadim Sedulin
  - TBaseVirtualDBTree::GoTo TBaseVirtualDBTree.GoToRec
  - TBaseVirtualDBTree::Update TBaseVirtualDBTree.UpdateTree

 23-OCT-2001 Adem Baba
  - I haven't done much other than to reorganize the code so that it is now one
    unit as oppsed to two units it originally was. I believe this is justifiable
    since this code is about 2,000 lines, there is, IMHO, no need to split them.
    Just look at VirtualTrees's line count --easily over 20,000 lines in a single
    unit.

  - I have removed all comments from the original code since they were Cyrillic,
      and they would have lost a lot in translation especially since I do not
      know Russian at all (I am only assuming they were Russian) :-)

  - I have renamed TSimpleVirtualDBTree to TVirtualDBTree. I believe it reflects
    its purpose better.

  - I have also merged the code in TCustomSimpleVirtualDBTree into TBaseVirtualDBTree;
    since everything else is derived from TBaseVirtualDBTree.

  - I got rid of TCheckDataLink, in favor of TVirtualDBTreeDataLink (which is
    renamed from TVTDataLink). There was no need for two descendants of TDataLink.

  - Finally, I have renamed the resultant file VirtualDBTree.

  Things to do:
    - Check to see if we really need these classes separately:
      TCustomCheckVirtualDBTree and TCheckVirtualDBTree. It looks as if they should
      be merged into a single class.

    - DCRs must be designed for
        - TVirtualDBTree,
        - TDBCheckVirtualDBTree,
        - TCheckVirtualDBTree

    - A demo. A demo is badly needed. I hope someone does come along and do it,
      as I am simply hopeless with those things.
 ——————————————————————————————————————————————————————————————————————————————}

unit VirtualDBTreeEx;

{$mode delphi}

interface

uses
  Classes, Controls, DB, ImgList, ActiveX,

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
    ID: Double;
    Level: Integer;
    Status: TDBVTNodeStatus;
    Parent: PVirtualNode;
  end;

  TBaseVirtualDBTreeEx = class;
  TVirtualDBTreeExDataLink = class;

  TVTDBOpenQueryEvent = procedure(Sender: TBaseVirtualDBTreeEx; var Allow:
    Boolean) of object;
  TVTDBWriteQueryEvent = procedure(Sender: TBaseVirtualDBTreeEx; Node:
    PVirtualNode; Column: TColumnIndex; ChangeMode: TDBVTChangeMode; var Allow:
    Boolean) of object;
  TVTNodeDataChangedEvent = procedure(Sender: TBaseVirtualDBTreeEx; Node:
    PVirtualNode; Field: TField; var UpdateNode: Boolean) of object;
  TVTNodeFromDBEvent = procedure(Sender: TBaseVirtualDBTreeEx; Node:
    PVirtualNode) of object;
  TVTPathToDBEvent = procedure(Sender: TBaseVirtualDBTreeEx; var Path: string)
    of object;

  TVirtualDBTreeExDataLink = class(TDataLink)
  private
    FVirtualDBTreeEx: TBaseVirtualDBTreeEx;
  public
    constructor Create(ATree: TBaseVirtualDBTreeEx); virtual;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure RecordChanged(Field: TField); override;
  end;

  TBaseVirtualDBTreeEx = class(TCustomVirtualStringTree)
  private
    FCurID: Double;
    FDBDataSize: Integer;
    FDBOptions: TDBVTOptions;
    FDBStatus: TDBVTStatuses;
    FDataLink: TVirtualDBTreeExDataLink;
    FKeyField: TField;
    FKeyFieldName: string;
    FLevelField: TField;
    FLevelFieldName: string;
    FMaxLevel: Integer;
    FOnNodeDataChanged: TVTNodeDataChangedEvent;
    FOnOpeningDataSet: TVTDBOpenQueryEvent;
    FOnReadNodeFromDB: TVTNodeFromDBEvent;
    FOnReadPathFromDB: TVTPathToDBEvent;
    FOnWritePathToDB: TVTPathToDBEvent;
    FOnWritingDataSet: TVTDBWriteQueryEvent;
    FParentField: TField;
    FParentFieldName: string;
    FPathField: TField;
    FPathFieldName: string;
    FViewField: TField;
    FViewFieldName: string;
    FImgIdxField: TField;
    FImgIdxFieldName: string;
    function GetDBNodeDataSize: Integer;
    function GetDBOptions: TDBVTOptions;
    function GetDBStatus: TDBVTStatuses;
    function GetDataSource: TDataSource;
    procedure RefreshListNode;
    procedure RefreshNodeByParent;
    procedure RefreshNodeByPath;
    procedure SetDBNodeDataSize(Value: Integer);
    procedure SetDBOptions(Value: TDBVTOptions);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetLevelFieldName(const Value: string);
    procedure SetParentFieldName(const Value: string);
    procedure SetPathFieldName(const Value: string);
    procedure SetViewFieldName(const Value: string);
    procedure SetImgIdxFieldName(const Value: string);

  protected
    function CanOpenDataSet: Boolean; virtual;
    function CanWriteToDataSet(Node: PVirtualNode; Column: TColumnIndex;
      ChangeMode: TDBVTChangeMode): Boolean; virtual;
    function DoChecking(Node: PVirtualNode; var NewCheckState: TCheckState):
      Boolean; override;
    function FindChild(Node: PVirtualNode; ID: Double): PVirtualNode;
    function FindNode(Start: PVirtualNode; ID: Double): PVirtualNode;
    function HasVisibleChildren(Node: PVirtualNode): Boolean;
    procedure DataLinkActiveChanged; virtual;
    procedure DataLinkChanged; virtual;
    procedure DataLinkEditingChanged; virtual;
    procedure DataLinkRecordChanged(Field: TField); virtual;
    procedure DataLinkScrolled; virtual;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoCollapsed(Node: PVirtualNode); override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint;
      var Effect: LongWord; Mode: TDropMode); override;
    procedure DoEdit; override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
      override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoInitNode(AParent, Node: PVirtualNode; var InitStates:
      TVirtualNodeInitStates); override;
    procedure DoNodeDataChanged(Node: PVirtualNode; Field: TField; var
      UpdateNode: Boolean); virtual;
    procedure DoNodeMoved(Node: PVirtualNode); override;
    procedure DoOpeningDataSet(var Allow: Boolean); virtual;
    procedure DoReadNodeFromDB(Node: PVirtualNode); virtual;
    procedure DoReadPathFromDB(var APath: string); virtual;
    procedure DoWritePathToDB(var APath: string); virtual;
    procedure DoWritingDataSet(Node: PVirtualNode; Column: TColumnIndex;
      ChangeMode: TDBVTChangeMode; var Allow: Boolean); virtual;
    procedure InitFields; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure ReadNodeFromDB(Node: PVirtualNode); virtual;
    procedure RefreshNode; virtual;
    procedure RefreshNodes;
    procedure ResetFields; virtual;
    procedure SetFocusToNode(Node: PVirtualNode);
    procedure ToggleListView;
    procedure ToggleViewMode;
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    function GetOptionsClass: TTreeOptionsClass; override;

    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
      TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean;
      override;
    function GetDBNodeData(Node: PVirtualNode): Pointer;
    function GoToRec(ID: Double): Boolean; overload;
    function DoCancelEdit: Boolean; override;
    function DoEndEdit: Boolean; override;
    procedure AddNode(AParent: PVirtualNode);
    procedure CheckAllChildren(Node: PVirtualNode);
    procedure CollapseAll;
    procedure DeleteSelection;
    procedure ExpandAll;
    procedure GoToRec(AString: string; Mode: TDBVTGoToMode); overload;
    procedure OnDragOverHandler(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
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
    property Canvas;

  published { Since all these properties are published for all descendants,
              we might as well publish them here and save whitespace}
{$IFDEF COMPILER_5p}
    property OnContextPopup;
{$ENDIF COMPILER_5p}
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
    property DBOptions: TDBVTOptions
      read GetDBOptions write SetDBOptions;
    property DataSource: TDataSource
      read GetDataSource write SetDataSource;
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
    property KeyFieldName: string
      read FKeyFieldName write SetKeyFieldName;
    property LevelFieldName: string
      read FLevelFieldName write SetLevelFieldName;
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
{$IFDEF COMPILER_5_UP}
    property OnContextPopup;
{$ENDIF COMPILER_5_UP}
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
    property OnOpeningDataSet: TVTDBOpenQueryEvent
      read FOnOpeningDataSet write FOnOpeningDataSet;
    property OnPaintBackground;
    property OnPaintText;
    property OnReadPathFromDB: TVTPathToDBEvent
      read FOnReadPathFromDB write FOnReadPathFromDB;
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
    property OnWritePathToDB: TVTPathToDBEvent
      read FOnWritePathToDB write FOnWritePathToDB;
    property OnWritingDataSet: TVTDBWriteQueryEvent
      read FOnWritingDataSet write FOnWritingDataSet;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFieldName: string
      read FParentFieldName write SetParentFieldName;
    property ParentFont;
    property ParentShowHint;
    property PathFieldName: string
      read FPathFieldName write SetPathFieldName;
    property PopupMenu;
    property ScrollBarOptions;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TStringTreeOptions
      read GetOptions write SetOptions;
    property ViewFieldName: string
      read FViewFieldName write SetViewFieldName;
    property ImgIdxFieldName: string
      read FImgIdxFieldName write SetImgIdxFieldName;
    property Visible;
    property WantTabs;
  end;

  TVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): UnicodeString;
    procedure SetNodeText(Node: PVirtualNode; const Value: UnicodeString);
  protected
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex):
      Integer; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
      TVSTTextType; var AText: string); override;
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
    property NodeText[Node: PVirtualNode]: UnicodeString
	    read GetNodeText write SetNodeText;
  end;

  TCustomDBCheckVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    FCheckDataLink: TVirtualDBTreeExDataLink;
    FResultField: TField;
    FResultFieldName: string;
    function GetCheckDataSource: TDataSource;
    function GetCheckList: TStringList;
    procedure SetCheckDataSource(Value: TDataSource);
    procedure SetResultFieldName(const Value: string);
  protected
    function DoChecking(Node: PVirtualNode; var NewCheckState: TCheckState):
      Boolean; override;
    procedure CheckDataLinkActiveChanged; virtual;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoOpeningDataSet(var Allow: Boolean); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CheckList: TStringList read GetCheckList;
    property ResultField: TField read FResultField;
    property CheckDataSource: TDataSource read GetCheckDataSource write
      SetCheckDataSource;
    property ResultFieldName: string read FResultFieldName write
      SetResultFieldName;
  end;

  TDBCheckVirtualDBTreeEx = class(TCustomDBCheckVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): UnicodeString;
    procedure SetNodeText(Node: PVirtualNode; const Value: UnicodeString);
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
    property NodeText[Node: PVirtualNode]: UnicodeString
      read GetNodeText write SetNodeText;
  published
    property CheckDataSource;
    property ResultFieldName;
  end;

  TCustomCheckVirtualDBTreeEx = class(TBaseVirtualDBTreeEx)
  private
    FList: TStringList;
    function GetCheckList: TStringList;
    procedure SetCheckList(Value: TStringList);
  protected
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoReadNodeFromDB(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CheckList: TStringList read GetCheckList write SetCheckList;
  end;

  TCheckVirtualDBTreeEx = class(TCustomCheckVirtualDBTreeEx)
  private
    function GetNodeText(Node: PVirtualNode): UnicodeString;
    procedure SetNodeText(Node: PVirtualNode; const Value: UnicodeString);
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
    property NodeText[Node: PVirtualNode]: UnicodeString read GetNodeText write
      SetNodeText;
  end;

procedure Register;

type
  TDBNodeData = record
    Text: UnicodeString;
    ImgIdx: Integer;
  end;
  PDBNodeData = ^TDBNodeData;

implementation

uses
  SysUtils, Math;

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TVirtualDBTreeEx,
    TDBCheckVirtualDBTreeEx, TCheckVirtualDBTreeEx]);
end;

type
  THackedTreeOptions = class(TStringTreeOptions);

{------------------------------------------------------------------------------}

constructor TVirtualDBTreeExDataLink.Create(ATree: TBaseVirtualDBTreeEx);
begin
  inherited Create;
  FVirtualDBTreeEx := ATree;
end;

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

{------------------------------------------------------------------------------}

constructor TBaseVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TVirtualDBTreeExDataLink.Create(Self);
  FDBDataSize := sizeof(TDBVTData);
  NodeDataSize := FDBDataSize;
  FDBOptions := [dboCheckDBStructure, dboParentStructure, dboWriteLevel,
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
  FDataLink.Free;
  inherited;
end;

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

procedure TBaseVirtualDBTreeEx.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(FDataLink) and
    (AComponent = DataSource)
  then
    DataSource := nil;
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
  if (FParentFieldName <> Value) then
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
  if (FPathFieldName <> Value) then
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
  if (FLevelFieldName <> Value) then
  begin
    FLevelFieldName := Value;
    FLevelField := nil;
    if FDataLink.Active and (FLevelFieldName <> '') then
      FLevelField := FDataLink.DataSet.FieldByName(FLevelFieldName);
  end;
end;

procedure TBaseVirtualDBTreeEx.DataLinkActiveChanged;
begin
  if not (csDesigning in ComponentState) then
  begin
    ResetFields;
    if (dboTrackActive in FDBOptions) then
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
      if (KeyID <> 0.0) and (KeyID <> FCurID) then
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
      if (dboTrackChanges in FDBOptions) then
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
  D: PDBVTData;
  Node: PVirtualNode;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FDataLink.Editing and not (dbtsEditing in FDBStatus) and not (dbtsInsert
      in FDBStatus) then
    begin
      if dboTrackChanges in FDBOptions then
      begin
        if (dboTrackCursor in FDBOptions) and
          (FDataLink.DataSet.State = dsEdit)
        then
        begin
          if Assigned(FocusedNode) and (dboTrackCursor in FDBOptions) then
          begin
            D := GetNodeData(FocusedNode);
            D.Status := dbnsEdit;
          end;
        end
        else if FDataLink.DataSet.State = dsInsert then
        begin
          Node := AddChild(nil);
          ValidateNode(Node, False);
          D := GetNodeData(Node);
          D.ID := 0.0;
          D.Level := 0;
          D.Parent := nil;
          D.Status := dbnsInited;
          ReadNodeFromDB(Node);
          D.Status := dbnsNew;
          if (dboTrackCursor in FDBOptions) then
            SetFocusToNode(Node);
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

procedure TBaseVirtualDBTreeEx.DoFocusChange(Node: PVirtualNode; Column:
  TColumnIndex);
var
  D: PDBVTData;
begin
  if Assigned(Node) then
  begin
    D := GetNodeData(Node);
    if D.ID <> FCurID then
    begin
      FCurID := D.ID;
      if (FCurID <> 0.0) and not (dbtsDataChanging in FDBStatus) and
        (dboTrackCursor in FDBOptions) then
      begin
        FDBStatus := FDBStatus + [dbtsDataChanging];
        FDataLink.DataSet.Locate(FKeyFieldName, D.ID, []);
        FDBStatus := FDBStatus - [dbtsDataChanging];
      end;
    end;
  end;
  inherited;
end;

function TBaseVirtualDBTreeEx.CanEdit(Node: PVirtualNode;
  Column: TColumnIndex):
Boolean;
begin
  if not (dbtsEditing in FDBStatus) and not (dbtsInsert in FDBStatus) then
    Result := CanWriteToDataSet(Node, Column, dbcmEdit)
  else
    Result := True;
end;

procedure TBaseVirtualDBTreeEx.DoEdit;
var
  Data: PDBVTData;
begin
  inherited;
  if IsEditing then
  begin
    Data := GetNodeData(FocusedNode);
    if Data.Status = dbnsEdit then
      FDBStatus := FDBStatus + [dbtsEditing]
    else if Data.Status = dbnsNew then
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
  Data: PDBVTData;
begin
  Result := inherited DoEndEdit;
  if Result then
  begin
    Data := GetNodeData(FocusedNode);
    Data.Status := dbnsRefreshed;
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
      Data.ID := FKeyField.AsFloat;
      FCurID := Data.ID;
    end;
  end;
end;

function TBaseVirtualDBTreeEx.DoCancelEdit: Boolean;
var
  Data: PDBVTData;
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
  else if (dbtsEditing in FDBStatus) then
  begin
    FDBStatus := FDBStatus - [dbtsEditing] + [dbtsDataChanging];
    if FDataLink.Editing then
      FDataLink.DataSet.Cancel;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    Data := GetNodeData(FocusedNode);
    Data.Status := dbnsRefreshed;
  end;
  Result := inherited DoCancelEdit;
end;

procedure TBaseVirtualDBTreeEx.DoCollapsed(Node: PVirtualNode);
var
  Focus: PVirtualNode;
begin
  if Assigned(Node) then
  begin
    if Assigned(FocusedNode) and HasAsParent(FocusedNode, Node) then
    begin
      Focus := Node;
      if not Selected[Focus] then
      begin
        Focus := GetNextSibling(Node);
        while Assigned(Focus) and not Selected[Focus] do
          Focus := GetNextVisible(Focus);
        if not Assigned(Focus) then
        begin
          Focus := GetPreviousVisible(Node);
          while Assigned(Focus) and not Selected[Focus] do
            Focus := GetPreviousVisible(Focus);
          if not Assigned(Focus) then
            Focus := Node;
        end;
      end;
      FocusedNode := Focus;
      Selected[Focus] := True;
    end;
    Focus := GetNextSibling(Node);
    if not Assigned(Focus) then
    begin
      Focus := GetLastChild(Node);
      if not Assigned(Focus) then
        Focus := Node;
      Focus := GetNext(Focus);
    end;
    Node := GetNext(Node);
    while Node <> Focus do
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
  CanProcess: Boolean;
  Focus: PVirtualNode;
begin
  Effect := DROPEFFECT_MOVE;
  if CanWriteToDataSet(DropTargetNode, 0, dbcmStructure) then
  begin
    CanProcess := True;
    if CanProcess then
    begin
      Focus := FocusedNode;
      BeginUpdate;
      FDataLink.DataSet.DisableControls;
      FDBStatus := FDBStatus + [dbtsDataChanging, dbtsDragDrop];
      ProcessDrop(DataObject, DropTargetNode, LongWord(Effect), amAddChildLast);
      Effect := DROPEFFECT_LINK;
      FocusedNode := nil;
      EndUpdate;
      FDataLink.DataSet.EnableControls;
      FDBStatus := FDBStatus - [dbtsDataChanging, dbtsDragDrop];
      FCurID := 0.0;
      FocusedNode := Focus;
    end
    else
      Effect := DROPEFFECT_NONE;
  end
  else
    Effect := DROPEFFECT_NONE;
  inherited;
end;

procedure TBaseVirtualDBTreeEx.OnDragOverHandler(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode:
  TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := CanWriteToDataSet(DropTargetNode, 0, dbcmStructure);
end;

procedure TBaseVirtualDBTreeEx.DoNodeMoved(Node: PVirtualNode);
var
  Data: PDBVTData;
  Path: string;
  Parent: PVirtualNode;
  ParentID: Double;
  Level: Integer;
begin
  if (dbtsDragDrop in FDBStatus) then
  begin
    ParentID := 0.0;
    Level := 0;
    Parent := Node.Parent;
    if Parent <> RootNode then
    begin
      Data := GetNodeData(Parent);
      Level := Data.Level + 1;
      ParentID := Data.ID;
      if (dboPathStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
      then
      begin
        Path := FloatToStr(ParentID);
        Parent := Parent.Parent;
        while Parent <> RootNode do
        begin
          Data := GetNodeData(Parent);
          Path := Format('%d.%s', [Data.ID, Path]);
          Parent := Parent.Parent;
        end;
      end;
    end;
    Data := GetNodeData(Node);
    Data.Level := Level;
    FDataLink.DataSet.Locate(FKeyFieldName, Data.ID, []);
    FDataLink.DataSet.Edit;
    if (dboPathStructure in FDBOptions) or
      (dboWriteSecondary in FDBOptions) then
    begin
      DoWritePathToDB(Path);
      FPathField.AsString := Path;
    end;
    if (dboParentStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
    then
    begin
      FParentField.AsFloat := ParentID;
    end;
    if (dboWriteLevel in FDBOptions) then
    begin
      FLevelField.AsInteger := Level;
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
  Data: PDBVTData;
begin
  Data := GetNodeData(Node);
  if (Data.Status = dbnsDelete) then
  begin
    if FDataLink.DataSet.Locate(FKeyFieldName, Data.ID, []) then
      FDataLink.DataSet.Delete;
  end;
  inherited;
end;

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
  Data: PDBVTData;
begin
  Result := GetFirstChild(Node);
  while Assigned(Result) do
  begin
    Data := GetNodeData(Result);
    if Data.ID = ID then
      break;
    Result := GetNextSibling(Result);
  end;
end;

function TBaseVirtualDBTreeEx.FindNode(Start: PVirtualNode; ID: Double):
PVirtualNode;
var
  Data: PDBVTData;
begin
  if Assigned(Start) then
    Result := Start
  else
    Result := GetFirst;
  while Assigned(Result) do
  begin
    Data := GetNodeData(Result);
    if Data.ID = ID then
      break;
    Result := GetNext(Result);
  end;
end;

procedure TBaseVirtualDBTreeEx.GoToRec(AString: string; Mode: TDBVTGoToMode);
var
  Text: string;
  Node: PVirtualNode;
  Column: TColumnIndex;
begin
  Text := '';
  EndEditNode;
  Column := Header.MainColumn;
  case Mode of
    gtmFromFirst:
    begin
      Node := GetFirst;
      Mode := gtmNext;
    end;
    gtmNext:
      Node := GetNext(FocusedNode);
    gtmPrev:
      Node := GetPrevious(FocusedNode);
    else
      Node := nil;
  end;
  while Assigned(Node) do
  begin
    DoGetText(Node, Column, ttNormal, Text);
    if Pos(AString, Text) = 1 then
      break;
    if Mode = gtmNext then
      Node := GetNext(Node)
    else
      Node := GetPrevious(Node);
  end;
  if Assigned(Node) then
    SetFocusToNode(Node);
end;

function TBaseVirtualDBTreeEx.GoToRec(ID: Double): Boolean;
var
  Node: PVirtualNode;
begin
  Node := FindNode(nil, ID);
  if Assigned(Node) then
    SetFocusToNode(Node);
  Result := Node <> nil;
end;

procedure TBaseVirtualDBTreeEx.AddNode(AParent: PVirtualNode);
var
  Level: Integer;
  ParentID: Double;
  Path: string;
  Node: PVirtualNode;
  Data: PDBVTData;
begin
  EndEditNode;
  if CanWriteToDataSet(AParent, 0, dbcmInsert) then
  begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    Node := AddChild(AParent);
    if (AParent = nil) or (AParent = RootNode) then
    begin
      Level := 0;
      ParentID := 0.0;
      Path := '';
    end
    else
    begin
      Data := GetNodeData(AParent);
      Level := Data.Level + 1;
      ParentID := Data.ID;
      if (dboPathStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
      then
      begin
        Path := FloatToStr(ParentID);
        AParent := AParent.Parent;
        while AParent <> RootNode do
        begin
          Data := GetNodeData(AParent);
          Path := Format('%d.%s', [Data.ID, Path]);
          AParent := AParent.Parent;
        end;
      end;
    end;
    Data := GetNodeData(Node);
    Data.ID := 0.0;
    Data.Level := Level;
    Data.Parent := nil;
    FDBStatus := FDBStatus + [dbtsInsert];
    FDataLink.DataSet.Insert;
    if not (dboListView in FDBOptions) then
    begin
      if (dboPathStructure in FDBOptions) or (dboWriteSecondary in FDBOptions)
      then
      begin
        DoWritePathToDB(Path);
        FPathField.AsString := Path;
      end;
      if (dboParentStructure in FDBOptions) or
        (dboWriteSecondary in FDBOptions)
      then
        FParentField.AsFloat := ParentID;
      if (dboWriteLevel in FDBOptions) then
        FLevelField.AsInteger := Level;
    end;
    DoReadNodeFromDB(Node);
    FCurID := 0.0;
    SetFocusToNode(Node);
    FDBStatus := FDBStatus - [dbtsDataChanging];
    EditNode(Node, Header.MainColumn);
  end;
end;

procedure TBaseVirtualDBTreeEx.DeleteSelection;
var
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
  Last: PVirtualNode;
  Focus: PVirtualNode;
begin
  if not (dbtsDataChanging in FDBStatus) and Assigned(FocusedNode) and
    (SelectedCount > 0) and not (dboReadOnly in FDBOptions) and
    FDataLink.Active
    and Assigned(FKeyField) and not FDataLink.ReadOnly then
  begin
    Node := GetFirst;
    Focus := FocusedNode;
    while Selected[Focus.Parent] do
      Focus := Focus.Parent;
    Temp := Focus;
    repeat
      Focus := GetNextSibling(Focus);
    until not Assigned(Focus) or not Selected[Focus];
    if not Assigned(Focus) then
    begin
      Focus := Temp;
      repeat
        Focus := GetPreviousSibling(Focus);
      until not Assigned(Focus) or not Selected[Focus];
      if not Assigned(Focus) then
        Focus := Temp.Parent;
      if Focus = RootNode then
        Focus := nil;
    end;
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    FDataLink.DataSet.DisableControls;
    while Assigned(Node) do
    begin
      if Selected[Node] then
      begin
        Temp := Node;
        Last := GetNextSibling(Node);
        repeat
          Data := GetNodeData(Temp);
          Data.Status := dbnsDelete;
          Temp := GetNext(Temp);
        until Temp = Last;
        if not Assigned(Temp) and (Node.Parent <> RootNode) then
          Temp := GetNextSibling(Node.Parent);
        DeleteNode(Node);
        Node := Temp;
      end
      else
        Node := GetNextVisible(Node);
    end;
    FDataLink.DataSet.EnableControls;
    EndUpdate;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    if Assigned(Focus) and (Focus <> RootNode) then
      SetFocusToNode(Focus);
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
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
  I: Integer;
begin
  if not (dbtsDataChanging in FDBStatus) and CanOpenDataSet then
  begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    FMaxLevel := 0;
    FCurID := 0.0;
    if (dboAlwaysStructured in FDBOptions) then
    begin
      if not (dbtsStructured in FDBStatus) then
        Clear
      else if (dboListView in FDBOptions) then
      begin
        FDBOptions := FDBOptions - [dboListView];
        ToggleListView;
        FDBOptions := FDBOptions + [dboListView];
      end;
      FDBStatus := FDBStatus + [dbtsStructured];
    end
    else
    begin
      if (dboListView in FDBOptions) then
        FDBStatus := FDBStatus - [dbtsStructured]
      else
      begin
        if not (dbtsStructured in FDBStatus) then
          Clear;
        FDBStatus := FDBStatus + [dbtsStructured];
      end;
    end;
    Temp := GetFirst;
    if not Assigned(Temp) then
      FDBStatus := FDBStatus + [dbtsEmpty];
    while Assigned(Temp) do
    begin
      Data := GetNodeData(Temp);
      if Data.Status = dbnsRefreshed then
        Data.Status := dbnsNone;
      Temp := GetNext(Temp);
    end;
    FDataLink.DataSet.DisableControls;
    if not FDataLink.DataSet.EOF or not FDataLink.DataSet.Bof then
      FCurID := FKeyField.AsFloat;
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
    Temp := GetFirst;
    while Assigned(Temp) do
    begin
      Data := GetNodeData(Temp);
      if (Data.Status <> dbnsRefreshed) then
      begin
        Node := GetNextSibling(Temp);
        DeleteNode(Temp);
      end
      else
      begin
        if (dbtsStructured in FDBStatus) and (dboParentStructure in FDBOptions)
        then
        begin
          Data.Level := GetNodeLevel(Temp);
          FMaxLevel := Max(FMaxLevel, Data.Level);
        end;
        if (dboTrackCursor in FDBOptions) and not Assigned(FocusedNode) and
          (Data.ID = FCurID) then
        begin
          Selected[Temp] := True;
          FocusedNode := Temp;
          FullyVisible[Temp] := True;
        end;
        Node := GetNext(Temp);
      end;
      Temp := Node;
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
  Data: PDBVTData;
  Node: PVirtualNode;
  ID: Double;
begin
  ID := FKeyField.AsFloat;
  if dbtsEmpty in FDBStatus then
    Node := nil
  else
    Node := FindChild(nil, ID);
  if not Assigned(Node) then
  begin
    Node := AddChild(nil);
    ValidateNode(Node, False);
    Data := GetNodeData(Node);
    Data.ID := ID;
    Data.Parent := nil;
    Data.Status := dbnsInited;
  end;
  ReadNodeFromDB(Node);
end;

procedure TBaseVirtualDBTreeEx.RefreshNodeByPath;
var
  Pos: Integer;
  ID: Double;
  Level: Integer;
  Node: PVirtualNode;
  Last: PVirtualNode;
  Data: PDBVTData;
  Temp: string;
  Path: string;
begin
  Data := nil;
  Path := FPathField.AsString;
  Last := RootNode;
  DoReadPathFromDB(Path);
  Temp := FloatToStr(FKeyField.AsFloat);
  if Path = '' then
    Path := Temp
  else
    Path := Format('%s.%s', [Path, Temp]);

  repeat
    Node := Last;
    Pos := System.Pos('.', Path);
    if (Pos = 0) then
    begin
      Temp := Path;
      Pos := Length(Path);
    end
    else
      Temp := Copy(Path, 1, Pos - 1);

    try
      ID := StrToFloat(Temp);
      Last := FindChild(Node, ID);
    except
      Exit;
    end;

    if Assigned(Last) then
      Delete(Path, 1, Pos);
  until not Assigned(Last) or (Path = '');

  if Path = '' then
  begin
    Node := Last;
    Data := GetNodeData(Node);
  end
  else
  begin
    if (Node = RootNode) then
      Level := -1
    else
    begin
      Data := GetNodeData(Node);
      Level := Data.Level;
    end;

    repeat
      Pos := System.Pos('.', Path);
      if (Pos = 0) then
      begin
        Temp := Path;
        Pos := Length(Path);
      end
      else
        Temp := Copy(Path, 1, Pos - 1);

      try
        ID := StrToFloat(Temp);
        Node := AddChild(Node);
        ValidateNode(Node, False);
        Data := GetNodeData(Node);
        Inc(Level);
        Data.ID := ID;
        Data.Level := Level;
        Data.Status := dbnsInited;
        Data.Parent := nil;
        Delete(Path, 1, Pos);
      except
        Exit;
      end;
    until Path = '';
  end;
  if Data <> nil then
    FMaxLevel := Max(FMaxLevel, Data.Level);
  if Node <> nil then
    ReadNodeFromDB(Node);
end;

procedure TBaseVirtualDBTreeEx.RefreshNodeByParent;
var
  ID: Double;
  ParentID: Double;
  Data: PDBVTData;
  This: PVirtualNode;
  Parent: PVirtualNode;
  Temp: PVirtualNode;
  Created: Boolean;
begin
  ID := FKeyField.AsFloat;
  ParentID := FParentField.AsFloat;
  if (ID = 0.0) then
  begin
    Exit;
  end;
  This := nil;
  Parent := nil;
  Temp := GetFirst;
  if (ParentID = 0.0) or (ParentID = ID) then
  begin
    Parent := RootNode;
    ParentID := 0.0;
  end;
  while Assigned(Temp) and (not Assigned(This) or not Assigned(Parent)) do
  begin
    Data := GetNodeData(Temp);
    if (Data.ID = ID) then
    begin
      if (Data.Status = dbnsRefreshed) then
      begin
        Exit;
      end;
      This := Temp;
    end;
    if (Data.ID = ParentID) and (ParentID <> 0.0) then
      Parent := Temp;
    Temp := GetNext(Temp);
  end;
  if not Assigned(Parent) then
  begin
    Parent := AddChild(nil);
    ValidateNode(Parent, False);
    Data := GetNodeData(Parent);
    Data.ID := ParentID;
    Data.Status := dbnsInited;
    Data.Parent := nil;
  end;
  Created := True;
  if Assigned(This) then
  begin
    if (This.Parent <> Parent) then
    begin
      if HasAsParent(Parent, This) then
      begin
        Data := GetNodeData(Parent);
        if (Data.Status = dbnsRefreshed) then
        begin
          Exit;
        end;
        Temp := This;
        This := Parent;
        Parent := Temp;
        Data := GetNodeData(Parent);
        Data.ID := ParentID;
      end
      else
      begin
        MoveTo(This, Parent, amAddChildLast, False);
      end;
    end;
  end
  else
  begin
    Created := False;
    This := AddChild(Parent);
    ValidateNode(This, False);
  end;
  Data := GetNodeData(This);
  Data.ID := ID;
  if not Created then
    Data.Status := dbnsInited;
  ReadNodeFromDB(This);
end;

procedure TBaseVirtualDBTreeEx.UpdateTree;
begin
  Clear;
  RefreshNodes;
end;

procedure TBaseVirtualDBTreeEx.ToggleListView;
var
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
begin
  if dbtsDragDrop in FDBStatus then
    Exit;
  BeginUpdate;
  if (dboListView in FDBOptions) then
  begin
    Node := GetFirst;
    while Assigned(Node) do
    begin
      Data := GetNodeData(Node);
      if (Node.Parent <> RootNode) then
      begin
        Data.Parent := Node.Parent;
        Temp := GetNextSibling(Node);
        if not Assigned(Temp) then
        begin
          Temp := GetLastChild(Node);
          if Assigned(Temp) then
          begin
            Temp := GetNext(Temp);
            if not Assigned(Temp) then
              Temp := GetNext(Node);
          end
          else
            Temp := GetNext(Node);
        end;
        MoveTo(Node, RootNode, amAddChildLast, False);
        Node := Temp;
      end
      else
        Node := GetNext(Node);
    end;
    if not (dboViewAll in FDBOptions) then
      ToggleViewMode;
  end
  else
  begin
    Node := GetFirst;
    while Assigned(Node) do
    begin
      Data := GetNodeData(Node);
      if Data.Parent <> nil then
      begin
        Temp := GetNextSibling(Node);
        MoveTo(Node, Data.Parent, amAddChildLast, False);
        Data.Parent := nil;
        Node := Temp;
      end
      else
        Node := GetNextSibling(Node);
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
  ToBeSet, ToBeCleared: TDBVTOptions;
begin
  EndEditNode;
  ToBeSet := Value - FDBOptions;
  ToBeCleared := FDBOptions - Value;
  FDBOptions := Value;

  if (dboTrackCursor in ToBeSet) then
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
  else if (dboTrackChanges in ToBeSet) then
  begin
    if not (dboTrackActive in FDBOptions) then
      FDBOptions := FDBOptions + [dboTrackActive];
    if dbtsChanged in FDBStatus then
      DataLinkActiveChanged;
  end
  else if dboTrackActive in ToBeSet then
  begin
    if dbtsChanged in FDBStatus then
      DataLinkActiveChanged;
  end
  else if dboTrackActive in ToBeCleared then
  begin
    FDBOptions := FDBOptions - [dboTrackCursor, dboTrackChanges];
    FDBOptions := FDBOptions + [dboReadOnly];
  end
  else if dboTrackChanges in ToBeCleared then
  begin
    FDBOptions := FDBOptions - [dboTrackCursor];
    FDBOptions := FDBOptions + [dboReadOnly];
  end
  else if dboTrackCursor in ToBeCleared then
    FDBOptions := FDBOptions + [dboReadOnly];

  if dboShowChecks in ToBeSet then
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
  else if dboShowChecks in ToBeCleared then
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
  else if dboViewAll in ToBeSet then
  begin
    if dboShowChecks in FDBOptions then
      ToggleViewMode;
  end
  else if dboViewAll in ToBeCleared then
  begin
    if dboShowChecks in FDBOptions then
      ToggleViewMode
    else
      FDBOptions := FDBOptions + [dboViewAll];
  end;

  if dboPathStructure in ToBeSet then
  begin
    FDBOptions := FDBOptions - [dboParentStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboParentStructure in ToBeSet then
  begin
    FDBOptions := FDBOptions - [dboPathStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboPathStructure in ToBeCleared then
  begin
    FDBOptions := FDBOptions + [dboParentStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end
  else if dboParentStructure in ToBeCleared then
  begin
    FDBOptions := FDBOptions + [dboPathStructure];
    if dboTrackActive in FDBOptions then
      UpdateTree;
  end;

  if dboAlwaysStructured in ToBeSet then
  begin
    if not (dbtsStructured in FDBStatus) then
      RefreshNodes;
  end
  else if dboAlwaysStructured in ToBeCleared then
  begin
    if dboShowChecks in FDBOptions then
      FDBOptions := FDBOptions + [dboAlwaysStructured];
  end;

  if dboListView in ToBeSet then
    ToggleListView
  else if dboListView in ToBeCleared then
  begin
    if dbtsStructured in FDBStatus then
      ToggleListView
    else
      RefreshNodes;
  end;
  if (dboReadOnly in ToBeCleared) and
    (not (dboTrackCursor in FDBOptions) or not
    (dboTrackChanges in FDBOptions) or not (dboTrackActive in FDBOptions)) then
    FDBOptions := FDBOptions + [dboReadOnly];
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

procedure TBaseVirtualDBTreeEx.DoOpeningDataSet(var Allow: Boolean);
begin
  Allow := (FViewField <> nil);
  if Allow then
  begin
    if Assigned(FOnOpeningDataSet) then
      FOnOpeningDataSet(Self, Allow);
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

procedure TBaseVirtualDBTreeEx.ReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBVTData;
begin
  Data := GetNodeData(Node);
  if (Data.Status <> dbnsNone) and (Data.Status <> dbnsRefreshed) then
    DoReadNodeFromDB(Node);
  Data.Status := dbnsRefreshed;
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
  Changed: Boolean;
  Last: PVirtualNode;
begin
  if (dboShowChecks in FDBOptions) and (dboAllowChecking in FDBOptions) then
  begin
    Changed := False;
    Last := GetNextSibling(Node);
    if not Assigned(Last) then
    begin
      Last := GetLastChild(Node);
      if not Assigned(Last) then
        Last := Node;
      Last := GetNext(Last);
    end;
    if OnlyChildren then
      Node := GetNext(Node);
    while Node <> Last do
    begin
      if CheckState[Node] <> csUncheckedNormal then
      begin
        CheckState[Node] := csUncheckedNormal;
        if CheckState[Node] = csUncheckedNormal then
          Changed := True;
      end;
    end;
    if Changed then
      ToggleViewMode;
  end;
end;

procedure TBaseVirtualDBTreeEx.DoInitNode(AParent: PVirtualNode; Node:
  PVirtualNode; var InitStates: TVirtualNodeInitStates);
begin
  inherited;
  Node.CheckType := ctCheckBox;
  Node.CheckState := csUncheckedNormal;
end;

procedure TBaseVirtualDBTreeEx.ToggleViewMode;
var
  Node: PVirtualNode;
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
      Node := GetLastChild(RootNode);
      while Assigned(Node) and (Node <> RootNode) do
      begin
        if (CheckState[Node] <> csCheckedNormal) and not
          Assigned(GetFirstChild(Node)) then
        begin
          DeleteNode(Node);
          if dboListView in FDBOptions then
            FDBStatus := FDBStatus - [dbtsStructured];
        end;
        Node := GetPrevious(Node);
      end;
    end;
    EndUpdate;
  end;
end;

function TBaseVirtualDBTreeEx.HasVisibleChildren(Node: PVirtualNode): Boolean;
var
  Last: PVirtualNode;
begin
  Result := False;
  if Assigned(Node) then
  begin
    Last := GetNextSibling(Node);
    if not Assigned(Last) then
    begin
      Last := GetLastChild(Node);
      if not Assigned(Last) then
        Last := Node;
      Last := GetNext(Last);
    end;
    Node := GetNext(Node);
    while Node <> Last do
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
  Node: PVirtualNode;
begin
  Node := GetFirst;
  BeginUpdate;
  while Assigned(Node) do
  begin
    Expanded[Node] := True;
    Node := GetNext(Node);
  end;
  EndUpdate;
end;

procedure TBaseVirtualDBTreeEx.CollapseAll;
var
  Node: PVirtualNode;
begin
  Node := GetFirst;
  BeginUpdate;
  while Assigned(Node) do
  begin
    Expanded[Node] := False;
    Node := GetNext(Node);
  end;
  EndUpdate;
end;

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

function TBaseVirtualDBTreeEx.DoGetImageIndex(Node: PVirtualNode; Kind:
  TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: Integer): TCustomImageList;

begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);

  if (Column = Header.MainColumn) then
  begin
    if (Kind = ikNormal) or (Kind = ikSelected) then
      Index := PDBNodeData(GetDBNodeData(Node)).ImgIdx;
  end;

  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self, Node, Kind, Column, Ghosted, Index);
end;

{------------------------------------------------------------------------------}

constructor TVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  DBNodeDataSize := SizeOf(TDBNodeData);
end;

procedure TVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBNodeData;
begin
  NodeText[Node] := ViewField.AsString;
  Data := PDBNodeData(GetDBNodeData(Node));
  if ImgIdxField <> nil then
    Data.ImgIdx := ImgIdxField.AsInteger
  else
    Data.ImgIdx := -1;

end;

procedure TVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field: TField;
  var UpdateNode: Boolean);
var
  Data: PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if (Field = ImgIdxField) then
  begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data.ImgIdx := Field.AsInteger;
    UpdateNode := True;
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

procedure TVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; const Value:
  UnicodeString);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;

function TVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): UnicodeString;
begin
  if Assigned(Node) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text;
end;

{------------------------------------------------------------------------------}

constructor TCustomDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  FCheckDataLink := TVirtualDBTreeExDataLink.Create(Self);
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking];
  FResultField := nil;
end;

destructor TCustomDBCheckVirtualDBTreeEx.Destroy;
begin
  FCheckDataLink.Free;
  inherited;
end;

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
  Data: PDBVTData;
begin
  if not (dbtsDataChanging in DBStatus) then
  begin
    Data := GetNodeData(Node);
    if CheckState[Node] = csCheckedNormal then
    begin
      FCheckDataLink.DataSet.Insert;
      FResultField.AsFloat := Data.ID;
      FCheckDataLink.DataSet.Post;
    end
    else if FCheckDataLink.DataSet.Locate(FResultFieldName, Data.ID, []) then
      FCheckDataLink.DataSet.Delete;
  end;
  inherited;
end;

procedure TCustomDBCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBVTData;
begin
  inherited;
  Data := GetNodeData(Node);
  if FCheckDataLink.DataSet.Locate(FResultFieldName, Data.ID, []) then
    CheckState[Node] := csCheckedNormal
  else
    CheckState[Node] := csUncheckedNormal;
end;

{------------------------------------------------------------------------------}

constructor TDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  DBNodeDataSize := SizeOf(TDBNodeData);
end;

procedure TDBCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBNodeData;
begin
  NodeText[Node] := ViewField.AsString;
  Data := PDBNodeData(GetDBNodeData(Node));
  if ImgIdxField <> nil then
    Data.ImgIdx := ImgIdxField.AsInteger
  else
    Data.ImgIdx := -1;

  inherited;
end;

procedure TDBCheckVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field:
  TField; var UpdateNode: Boolean);
var
  Data: PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if Field = ImgIdxField then
  begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data.ImgIdx := Field.AsInteger;
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
  //  If Column = Header.MainColumn Then If NodeText[Node1] > NodeText[Node2] Then Result := 1
  //    Else Result := -1
  //  Else Result := 0;
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
  UnicodeString);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;

function TDBCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): UnicodeString;
begin
  if Assigned(Node) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text;
end;

{------------------------------------------------------------------------------}

constructor TCustomCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  FList := TStringList.Create;
  FList.Sorted := True;
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking];
end;

destructor TCustomCheckVirtualDBTreeEx.Destroy;
begin
  FList.Free;
  inherited;
end;

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

procedure TCustomCheckVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
var
  Data: PDBVTData;
  Index: Integer;
begin
  if not (dbtsDataChanging in DBStatus) then
  begin
    Data := GetNodeData(Node);
    if CheckState[Node] = csCheckedNormal then
      FList.Add(FloatToStr(Data.ID))
    else
    begin
      Index := FList.IndexOf(FloatToStr(Data.ID));
      if Index <> -1 then
        FList.Delete(Index);
    end;
  end;
  inherited;
end;

procedure TCustomCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBVTData;
  Index: Integer;
begin
  Index := 0;
  inherited;
  Data := GetNodeData(Node);
  if FList.Find(FloatToStr(Data.ID), Index) then
    CheckState[Node] := csCheckedNormal
  else
    CheckState[Node] := csUncheckedNormal;
end;

{------------------------------------------------------------------------------}

constructor TCheckVirtualDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;
  DBNodeDataSize := sizeof(TDBNodeData);
end;

procedure TCheckVirtualDBTreeEx.DoReadNodeFromDB(Node: PVirtualNode);
var
  Data: PDBNodeData;
begin
  NodeText[Node] := ViewField.AsString;
  Data := PDBNodeData(GetDBNodeData(Node));
  if ImgIdxField <> nil then
    Data.ImgIdx := ImgIdxField.AsInteger
  else
    Data.ImgIdx := -1;


  inherited;
end;

procedure TCheckVirtualDBTreeEx.DoNodeDataChanged(Node: PVirtualNode; Field:
  TField; var UpdateNode: Boolean);
var
  Data: PDBNodeData;
begin
  if Field = ViewField then
  begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  end
  else if Field = ImgIdxField then
  begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data.ImgIdx := Field.AsInteger;
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
      inherited;
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
  //  If Column = Header.MainColumn Then If NodeText[Node1] > NodeText[Node2] Then Result := 1
  //    Else Result := -1
  //  Else Result := 0;
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

procedure TCheckVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; const Value:
  UnicodeString);
begin
  if Assigned(Node) then
    PDBNodeData(GetDBNodeData(Node)).Text := Value;
end;

function TCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): UnicodeString;
begin
  Result := '';
  if Assigned(Node) and (Node <> RootNode) then
    Result := PDBNodeData(GetDBNodeData(Node)).Text;
end;

function TCustomDBCheckVirtualDBTreeEx.GetCheckList: TStringList;
var
  Data: PDBVTData;
  Node: PVirtualNode;
begin
  Result := TStringList.Create;
  Node := GetFirst;
  while Assigned(Node) do
  begin
    Data := GetNodeData(Node);
    if CheckState[Node] = csCheckedNormal then
      Result.Add(FloatToStr(Data.ID));
    Node := GetNext(Node);
  end;
end;

end.

