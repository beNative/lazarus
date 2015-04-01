unit VirtualDBGrid;

// TVirtualDBGrid
// version: 1.03 beta
//
// This Unit contains the TVirtualDBGrid component
//
// TVirtualDBGrid is an descendant of TCustomVirtualStringTree
//   witch was created by Ing. Mike Lischke (public@lischke-online.de, www.lischke-online.de).
//
//  Version 1.03 - 1.02 was based on Version 4.0.16 of TCustomVirtualStringTree
//  Version 1.00 was based on Version 3.8.3 of TCustomVirtualStringTree
//
// The contents of this file "VirtualDBGrid.pas" is subject to
// the Mozilla Public License  Version 1.1 (the "License")
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under
// the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied.
// See the License for the specific language governing rights
// and limitations under the License.
//
// The original code is VirtualDBGrid.pas, released ? 2003/12.
// and is written by Peter Sulek
// (mailto:virtualdbgrid@virtualdbgrid.wz.cz)
//----------------------------------------------------------------------------------------------------------------------
// LCL port: Luiz Americo Pereira Camara



{$mode delphi}

{.$define VTV_VER_5}
{.$define DEBUG_VDBGRID}

interface

uses
  LCLType, types, delphicompat, LCLIntf, SysUtils, Classes, Controls, VirtualTrees, DB, Dialogs,
  Variants, ImgList, Forms, Graphics, ExtCtrls, Buttons, LMessages;

const
  clWhiteSmoke  : TColor = $00F5F5F5;
  clLightYellow : TColor = $00E0FFFF;

  DefaultIndicatorColor = clBtnFace;

  // FieldFlag constants
  ffUndeclared=   high(byte);
  ffDBField =     0;
  ffCalculated =  1;
  ffIndicator  =  2;

type
  TVTDBAdvOption = (
  aoEditable,               // allows edit nodes and update changes to database
                            // If aoFullRowSelect is true then this flag(aoEditable)
                            // is ignored (none of changes where updated to database)
  aoStrippedRows,           // grid lines will be stripped
  aoShowHorzLines,          // show horizontal grid lines
  aoShowVertLines,          // show vertical grid lines
  aoCenterScrollIntoView,   // enables toCenterScrollIntoView
  aoAutoInsertIndicator,    // If AddDefaultsFieldsToColumns is called and aoAutoInsertIndicator
                            // is set then will be insert indicator column automatically
  aoAllowSorting,           // if is set then click on header will begin sorting procedure
                            // depending of SortType in DBOptions
  aoHighlightSortColumn,    // highlight sort column with custom color
  aoHourGlassCursor,        // show hourglass cursor on sort action
                            // (usefull if there is too much records to sort)
  aoSortDBFieldColumns,     // sort columns with column type = ctDBField
                            // {only if aoAllowSorting is set}
  aoEditDBFieldColumns,     // if set, then editing colum with type ctDBField is allowed
  aoSortCalculatedColumns,  // sort columns with column type = ctCalculated
                            // {only if aoAllowSorting is set}
  aoEditCalculatedColumns,  // if set, then editing colum with type ctCalculated is allowed
  aoFullRowSelect,          // enable full row select, see aoEditable for details
  aoMultiSelect,            // enable multi select
  aoAutoToggleBoolean,      // toggle boolean fields when the cell is double clicked
  aoEditOnClick,            // Editing mode can be entered with a single click
  aoEditOnDblClick,         // Editing mode can be entered with a double click
  aoAddDefaultColumns       // Add default columns even if one or more column(s) are added at design time
  );

  TVTDBAdvOptions = set of TVTDBAdvOption;

  { --- Types --- }
  TColumnType =           (ctDBField, ctCalculated, ctIndicator);
  TIndicatorAlign =       (aiLeft, aiCenter, aiRight);
  TIndicatorVAlign =      (aiTop, aiMiddle, aiBottom);
  TNavigateFromPosition = (nfpBegin, nfpCurrent, nfpEnd);
  // type of sorting
  //  :stNone     - Dont start sorting on header click, but you can start sorting manually
  //                by calling SetSortColumn procedure
  //  :stBuildIn  - buildin sorting feature(slower on big database)
  //  :stCustom   - when there was a click on header then OnCustomSort event
  //                will be triggered to allow user sort database by their way
  //                (in some cases, this is much faster)
  TSortingType =          (stNone, stBuildIn, stCustom);

  // type of getting count of records in database
  //  :rcFromDataset - use Dataset.RecordCount, non-functional for most SQL dataset
  //  :rcCustom  - trigger event OnGetRecordCount for getting record count by user
  //               something like SELECT COUNT(*) FROM TABLEXXX  a pass a return value
  //               to RecordCount in OnGetRecordCount event
  TRecordCountType =      (rcFromDataset, rcCustom);

const
  DefaultAdvOptions = [aoEditable, aoStrippedRows, aoShowHorzLines, aoShowVertLines,
                       aoAllowSorting, aoHighlightSortColumn, aoAutoInsertIndicator,
                       aoHourGlassCursor, aoSortDBFieldColumns,
                       aoEditDBFieldColumns, aoSortCalculatedColumns];

type
  TRecordData = class;
  TCustomVirtualDBGrid = class;
  { --- Events --- }
  { TOnGetRecordCountEvent - Triggered when we need to know how much records is in the database.            }
  {                          If isn't assigned this event, than standard 'dataset.recordcount' will be used }
  TOnGetRecordCountEvent   = procedure(Sender: TCustomVirtualDBGrid; var RecordCount: longint) of object;

  { TOnCalculateValueEvent - Triggered when at least one column has column type = ctCalculated and }
  {                          we want to fillup value for this calculated column                    }
  TOnCalculateValueEvent   = procedure(Sender: TCustomVirtualDBGrid; const IDText: string;
                                       Column: TColumnIndex; RecordData: TRecordData;
                                       RowIndex: Cardinal; var CalculatedValue: String;
                                       var CalculatedValueType: TFieldType) of object;

  { TOnFormatFieldValueEvent - Triggered when loading data from dataset}
  TOnFormatFieldValueEvent = procedure(Sender: TCustomVirtualDBGrid; Column: TColumnIndex;
                                       RecordData: TRecordData;
                                       RowIndex: Cardinal; Field: TField;
                                       var FieldValue: Variant) of object;

  { TOnLoadRecordEvent - Triggered when record from database was loaded into VirtualDBGrid }
  {                      Assigning this event can reduce speed of VirtualDBGrid            }
  TOnLoadRecordEvent       = procedure(Sender: TCustomVirtualDBGrid; RecordData: TRecordData;
                                       RowIndex: Cardinal) of object;

  { TOnCustomSortEvent - Triggered when SortType in DBOptions is stCustom to sort database }
  {                      by user                                                           }
  {          :Column - Column index by which will be sorted                                }
  {      :ColumnType - type of column (ctDBField, ctCalculated)                            }
  {          :SortBy - If column is ctCalculated, then SortBy = Colum title(Header caption)}
  {                    of column. If column is ctDBField, then SortBy = FieldName property }
  {                    of column.                                                          }
  {   :SortDirection - Sorting direction, can be sdAscending or sdDescending               }
  {     :RefreshGrid - If True then after this event the grid will be refreshed            }
  {                    Default is TRUE.                                                    }
  TOnCustomSortEvent         = procedure(Sender: TCustomVirtualDBGrid; Column: TColumnIndex;
                                         ColumnType: TColumnType; const SortBy: string;
                                         SortDirection: TSortDirection;
                                         var RefreshGrid: boolean) of object;

  { TOnPostChanges - Triggered when grid is at the end of editing cell, you can or not post }
  {                  changes to the grid/database                                           }
  { :FieldNameOrIDText - if ColumnType = ctDBField then value of FieldNameOrIDText contains }
  {                      FieldName property of column                                       }
  {                      if ColumnType = ctCalculated then value of FieldNameOrIDText contains }
  {                      Text property of column                                            }
  {            :Column - Column index on which will be posted changes                       }
  {        :ColumnType - type of column (ctDBField, ctCalculated)                           }
  {        :RecordData - data of current record (TRecordData object)                        }
  {          :NewValue - new posted value                                                   }
  {       :PostChanges - set to True if you want to post changed, or False to not post      }
  TOnPostChanges             = procedure(Sender: TCustomVirtualDBGrid; const FieldNameOrIDText: string;
                                         Column: TcolumnIndex; ColumnType: TColumnType;
                                         RecordData: TRecordData; RowIndex: Cardinal;
                                         var NewValue: String; var PostChanges: boolean)
                                         of object;

  { TOnChangeSort  - Triggered when sorting in the grid was changed                          }
  {    :SortColumn - column index of sorted column                                           }
  { :SortDirection - sort direction                                                          }
  TOnChangeSort              = procedure(Sender: TCustomVirtualDBGrid; SortColumn: TColumnIndex;
                                         SortDirection: TSortDirection) of object;

  TOnCompareRecord = procedure(Sender: TCustomVirtualDBGrid; Record1, Record2: TRecordData; Column: TColumnIndex;
    var Result: Integer) of object;

  TOnRecordDblClick = procedure(Sender: TCustomVirtualDBGrid; Column: TColumnIndex; RecordData: TRecordData) of object;

  PDBFieldValueRec = ^TDBFieldValueRec;
  TDBFieldValueRec = record
    FieldName:  string;
    FieldValue: variant;
    FieldType:  TFieldType;
    FieldFlag:  byte;
  end;


  TRecordData = class
  private
    fList : TFpList;
    fRecNo: Longint;

    function GetCalculatedByIdx(Index: Integer): boolean;
    function GetCalculated(const IDText: string): boolean;
    function GetCalculatedValueByIdx(Index: Integer): Variant;
    procedure PutCalculatedValueByIdx(Index: Integer; Value: Variant);
    function GetCalculatedValue(const IDText: string): Variant;
    procedure PutCalculatedValue(const IDText: string; Value: Variant);

    function GetField(Index: Integer) : TDBFieldValueRec;
    procedure PutField(Index: Integer; Value: TDBFieldValueRec);
    function GetFieldName(Index: Integer) : string;
    procedure PutFieldName(Index: Integer; Value: string);
    function GetFieldValueByIdx(Index: Integer) : Variant;
    function GetFieldValue(const FieldName: String) : Variant;
    procedure PutFieldValueByIdx(Index: Integer; Value: Variant);
    procedure PutFieldValue(const FieldName: String; Value: Variant);
    function GetFieldTypeByIdx(Index: Integer) : TFieldType;
    function GetFieldType(const FieldName: String) : TFieldType;
    function GetFieldsCount: Integer; inline;
    function GetIndidicatorByIdx(Index: Integer): boolean;
    function GetFielFlag(Index: Integer): byte;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ClearItems;
    procedure Add(const AFieldName: string; AFieldValue: Variant; AFieldType: TFieldType;
                  AFieldFlag: byte);
    procedure Edit(const AFieldName: string; const AFieldFlag: byte; NewFieldValue: Variant); overload;
    procedure Edit(const AFieldName: string; const AFieldFlag: byte; NewFieldValue: Variant;
                   NewFieldType: TFieldType); overload;
    procedure Insert(Index: Integer; const AFieldName: string; AFieldValue: Variant;
                     AFieldType: TFieldType; AFieldFlag: byte);

    procedure Delete(Index: Integer);
    function IndexOf(const AFieldName: string) : Integer; overload;
    function IndexOf(const AFieldName: string; const AFieldFlag: byte) : Integer; overload;
    procedure Exchange(Index1, Index2: Integer);


    property IsIndicatorByIdx[Index: Integer]:     boolean          read GetIndidicatorByIdx;
    property IsCalculatedByIdx[Index: Integer]:    boolean          read GetCalculatedByIdx;
    property IsCalculated[IDText: string]:         boolean          read GetCalculated;
    property CalculatedValueByIdx[Index: Integer]: Variant          read GetCalculatedValueByIdx write PutCalculatedValueByIdx;
    property CalculatedValue[IDText: string]:      Variant          read GetCalculatedValue      write PutCalculatedValue;
    property FieldName[Index: Integer]:            string           read GetFieldName            write PutFieldName;
    property FieldValueByIdx[Index: Integer]:      Variant          read GetFieldValueByIdx      write PutFieldValueByIdx;
    property FieldValue[FieldName: String]:        Variant          read GetFieldValue           write PutFieldValue;
    property FieldTypeByIdx[Index: Integer]:       TFieldType       read GetFieldTypeByIdx;
    property FieldType[FieldName: String]:         TFieldType       read GetFieldType;
    property FieldFlag[Index: Integer]:            Byte             read GetFielFlag;
    property Fields[Index: Integer]:               TDBFieldValueRec read GetField                write PutField;
    property FieldsCount:                          Integer          read GetFieldsCount;

    property RecNo: longint read fRecNo write fRecNo;
  end;

  PNodeData = ^TNodeData;
  TNodeData= record
    RecordData: TRecordData;
    RecNo: Integer;
  end;

  TRecordDataClass = class of TRecordData;

  { TVirtualDBTreeDataLink }

  TVirtualDBTreeDataLink = class(TDataLink)
  private
    FVirtualDBTree: TCustomVirtualDBGrid;
  public
    constructor Create(ATree: TCustomVirtualDBGrid); virtual;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetScrolled(Distance: Integer); override;
  end;


  { TVirtualDBTreeColumn }

  TVirtualDBTreeColumn = class(TVirtualTreeColumn)
  private
    fFieldName:  String;
    fField:      TField;
    fColumnType: TColumnType;
    FIsDefault: Boolean;
    procedure CalculateWidth(Tree: TCustomVirtualDBGrid);
    procedure InternalSetFieldName(const AFieldName: String);
    procedure SetFieldName(const AFieldName: String);
    procedure SetColumnType(Value: TColumnType);
    function GetOwnerTree: TCustomVirtualDBGrid;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Equals(OtherColumn: TObject): Boolean; override;
    procedure LoadFromStream(const Stream: TStream; Version: Integer);
    procedure SaveToStream(const Stream: TStream);
  published
    property FieldName: String read FFieldName write SetFieldName;
    property ColumnType: TColumnType read fColumnType write SetColumnType;
  end;

  { TVirtualDBTreeColumns }

  TVirtualDBTreeColumns = class(TVirtualTreeColumns)
  private
    fLastCount: Integer;
    procedure CalculateDefaultColumnsWidth;
  protected
    {.$IFDEF COMPILER_6_UP}
    //procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {.$ENDIF}
    procedure Update(Item: TCollectionItem); override;
    function IndexOf(const FieldNameOrIDText: string; ColumnType: TColumnType): Integer;
  public
    property HeaderBitmap;
  end;

  TVTDBHeader = class(TVTHeader)
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
  end;



  TVTDBOptions = class(TPersistent)
  private
    fOwner:             TCustomVirtualDBGrid;
    fDataLink:          TVirtualDBTreeDataLink;
    fIndicatorImIndex:  TImageIndex;
    fIndicatorAlign:    TIndicatorAlign;
    fIndicatorVAlign:   TIndicatorVAlign;
    fOddRowColor:       TColor;
    fEvenRowColor:      TColor;
    fSortingType:       TSortingType;
    fRecordCountType:   TRecordCountType;
    fSortColumnBgColor: TColor;
    fAdvOptions:        TVTDBAdvOptions;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetIndicatorImIndex(Value: TImageIndex);
    procedure SetIndicatorAlign(Value: TIndicatorAlign);
    procedure SetIndicatorVAlign(Value: TIndicatorVAlign);
    procedure SetOddRowColor(value: tcolor);
    procedure SetEvenRowColor(value: tcolor);
    procedure SetAdvOptions(value: TVTDBAdvOptions);
    procedure SetSortColumnBgColor(Value: TColor);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomVirtualDBGrid); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Treeview: TCustomVirtualDBGrid read FOwner;
    property DataLink: TVirtualDBTreeDataLink read FDataLink;
  published
    property DataSource:          TDataSource      read GetDataSource      write SetDataSource;
    property IndicatorImageIndex: TImageIndex      read fIndicatorImIndex  write SetIndicatorImIndex default -1;
    property IndicatorAlign:      TIndicatorAlign  read fIndicatorAlign    write SetIndicatorAlign stored true default aiCenter;
    property IndicatorVAlign:     TIndicatorVAlign read fIndicatorVAlign   write SetIndicatorVAlign stored true default aiMiddle;
    property OddRowColor:         TColor           read fOddRowColor       write SetOddRowColor;
    property EvenRowColor:        TColor           read fEvenRowColor      write SetEvenRowColor;
    property SortingType:         TSortingType     read fSortingType       write fSortingType stored true default stBuildIn;
    property RecordCountType:     TRecordCountType read fRecordCountType   write fRecordCountType stored true default rcFromDataset;
    property SortColumnBgColor:   TColor           read fSortColumnBgColor write SetSortColumnBgColor;
    property AdvOptions:          TVTDBAdvOptions  read fAdvOptions        write SetAdvOptions default DefaultAdvOptions;
  end;

  { TCustomVirtualDBGrid }

  TCustomVirtualDBGrid = class(TCustomVirtualStringTree)
  private
    FInternalDataOffset:      longword;
    fLoadingDataFlag:         integer;
    fLastRecordCount:         longint;
    fRecordCount:             longint;

    fDBOptions:               TVTDBOptions;
    fOnGetRecordCount:        TOnGetRecordCountEvent;
    fOnCalculateValue:        TOnCalculateValueEvent;
    fOnFormatFieldValue:      TOnFormatFieldValueEvent;
    fOnLoadRecord:            TOnLoadRecordEvent;
    fOnCustomSort:            TOnCustomSortEvent;
    fOnPostChanges:           TOnPostChanges;
    fOnChangeSort:            TOnChangeSort;
    FOnCompareRecord: TOnCompareRecord;
    FOnRecordDblClick: TOnRecordDblClick;
    fIndicatorBMP:            TBitmap;

    function GetHeader: TVTDBHeader;
    procedure AddDefaultColumns;
    procedure IndicatorBitmapNeeded;
    procedure RemoveDefaultColumns;
    procedure SetHeader(Value: TVTDBHeader);
    procedure SetDBOptions(const Value: TVTDBOptions);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function InternalGetNodeData(Node: PVirtualNode): PNodeData;
    procedure InternalInitializeDBTree;
    procedure UpdateVisibleDBTree(AlwaysUpdate: Boolean; UpdateLoadedData: Boolean = False);
    procedure UpdateDBTree(StartNode: PVirtualNode; NodeCount: Cardinal;
      AlwaysUpdate: Boolean; UpdateLoadedData: Boolean);
    function IsDataCreated(ANode: PVirtualNode): boolean;
    // Return number of current record in database
    //   - if database is closed, returns 0
    function GetCurrentDBRecNo: LongInt;
    function AddColumn(AColumnType: TColumnType; const AFieldName, ACaption: string; AWidth: Integer=-1): TVirtualDBTreeColumn;
    procedure IncLoadingDataFlag;
    procedure DecLoadingDataFlag;
    function IsDataLoading: boolean;
    function GetIndicatorColumn: TVirtualDBTreeColumn;
    function GetSortingColumn: TVirtualDBTreeColumn;
    procedure SetFocusToActualRecNo;
    procedure UpdateCurrentRecord;
  protected
    procedure InitializeWnd; override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: String); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: String); override;
//    {$ifdef VTV_VER_5}
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo);
    //{$else}
    //procedure DoHeaderClick(Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    //{$endif}
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;
    function DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex): Boolean; override;
    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; var Color: TColor;
           var EraseAction: TItemEraseAction); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; out NextNonEmpty: TColumnIndex); override;
    // new
    procedure DoGetRecordCount(var RecordCount: LongInt); virtual;
    procedure DoCalculateValue(const IDText: string; Column: TColumnIndex;
        RecordData: TRecordData; RowIndex: Cardinal; var CalculatedValue: String;
        var CalculatedValueType: TFieldType);
    procedure DoFormatFieldValue(Column: TColumnIndex; RecordData: TRecordData;
        RowIndex: Cardinal; Field: TField; var FieldValue: Variant); virtual;
    procedure DoLoadRecord(RecordData: TRecordData; RowIndex: Cardinal); virtual;
    procedure DoCustomSort(Column: TColumnIndex; ColumnType: TColumnType;
        const SortBy: string; SortDirection: TSortDirection; var RefreshGrid: boolean); virtual;
    procedure DoPostChanges(const FieldNameOrIDText: string;
        Column: TcolumnIndex; ColumnType: TColumnType; RecordData: TRecordData;
        RowIndex: Cardinal; var NewValue: String; var PostChanges: boolean); virtual;
    procedure DoChangeSort(SortColumn: TColumnIndex;
        SortDirection: TSortDirection); virtual;
    procedure DoRecordDblClick(Column: TColumnIndex; RecordData: TRecordData);

    function GetRecordCount: longint;
    procedure LoadRecordData(ANode: PVirtualNode; CreateData: Boolean);

    function FindNodeByRecNo(ARecNo: LongInt): PVirtualNode;
    function SetFocusToNode(Node: PVirtualNode; Center: Boolean = True): Boolean;
    procedure GotoRecNo(NewRecNo: longint);
    function GetNodeByIndex(Index: Cardinal): PVirtualNode;

    function GetSelectedRecord(Index: Integer): TRecordData;
    function GetVisibleNodesCount: Cardinal;
    // Value -1 in SortDirection mean that to autodetect sortdirection of column to sort by
    procedure DoSortColumn(AColumn: TColumnIndex; ASortDirection: Integer= -1);
    //procedure RemoveColumnFromRecordData(ColumnItem: TVirtualDBTreeColumn);
    procedure RearrangeColumnsRecordData;
  protected
    function GetHeaderClass: TVTHeaderClass; override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure DoScroll(DeltaX, DeltaY: Integer); override;
    function GetRecordDataClass: TRecordDataClass; virtual;

    procedure DataLinkActiveChanged; virtual;
    procedure DataLinkChanged; virtual;
    procedure DataLinkRecordChanged(Field: TField); virtual;

    function GetDataSet: TDataSet; inline;

    property InternalRecordCount: longint   read fRecordCount;

    property DBOptions: TVTDBOptions read fDBOptions write SetDBOptions;
    property LinkedDataSet: TDataSet read GetDataSet;

    property OnGetRecordCount:        TOnGetRecordCountEvent   read fOnGetRecordCount        write fOnGetRecordCount;
    property OnCalculateValue:        TOnCalculateValueEvent   read fOnCalculateValue        write fOnCalculateValue;
    property OnFormatFieldValue:      TOnFormatFieldValueEvent read fOnFormatFieldValue      write fOnFormatFieldValue;
    property OnLoadRecord:            TOnLoadRecordEvent       read fOnLoadRecord            write fOnLoadRecord;
    property OnCustomSort:            TOnCustomSortEvent       read fOnCustomSort            write fOnCustomSort;
    property OnPostChanges:           TOnPostChanges           read fOnPostChanges           write fOnPostChanges;
    property OnChangeSort:            TOnChangeSort            read fOnChangeSort            write fOnChangeSort;
    property OnCompareRecord: TOnCompareRecord read FOnCompareRecord write FOnCompareRecord;
    property OnRecordDblClick: TOnRecordDblClick read FOnRecordDblClick write FOnRecordDblClick;

    // discarded VirtualTreeView properties that we doesn't allow to change by user
    property TreeOptions: TStringTreeOptions       read GetOptions          write SetOptions;
    property RootNodeCount stored false;
    property DefaultText stored false;
    property OnFreeNode;
    property OnGetText;
    property OnNewText;
    property DefaultNodeHeight;
    property OnCreateDataObject;
    property OnLoadNode;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnResetNode;
    property OnSaveNode;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddDBColumn(const AFieldName, ACaption: string; AWidth: Integer=-1);
    procedure AddCalcColumn(const IDText: string; AWidth: Integer);
    procedure AddIndicatorColumn(AWidth: Integer);
    function GetNodeRecordData(Node: PVirtualNode): TRecordData;
    procedure SetSortColumn(const ColumnTitle: string; Direction: TSortDirection);
    procedure UpdateAllRecords(UpdateLoadedData: Boolean = True);
    // navigate trought the treeview
    function Navigate(FromPosition: TNavigateFromPosition; Delta: Longint): boolean;
    procedure ReInitializeDBGrid;
    function IsDataOk(AData: PNodeData): Boolean;

    property SortingColumn:                  TVirtualDBTreeColumn read GetSortingColumn;
    property IndicatorColumn:                TVirtualDBTreeColumn read GetIndicatorColumn;
    property SelectedRecord[Index: Integer]: TRecordData          read GetSelectedRecord;
  published
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
    {
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    }
    property BorderStyle default bsSingle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultPasteMode;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header: TVTDBHeader read GetHeader write SetHeader;
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
    property NodeDataSize;
    {$ifdef COMPILER_7_UP}
      property ParentBackground;
    {$endif COMPILER_7_UP}
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
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
    {$ifdef COMPILER_5_UP}
      property OnContextPopup;
    {$endif COMPILER_5_UP}
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
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
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
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResize;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
    property OnUTF8KeyPress;
  end;


  TVirtualDBGrid = class(TCustomVirtualDBGrid)
  public
    property Canvas;
  published
    property BorderSpacing;
    // New events by TVirtualDBGrid
    property DBOptions; 
    property OnGetRecordCount;
    property OnCalculateValue;
    property OnFormatFieldValue;
    property OnLoadRecord;
    property OnCustomSort;
    property OnPostChanges;
    property OnChangeSort;
    property OnCompareRecord;
    property OnRecordDblClick;
  end;

  TControlClick = class(TControl)
  published
    property OnClick;
  end;

  function NullVar2Int(Value: Variant): Integer;
  function NullVar2Double(Value: Variant): Double;
  function NullVar2DateTime(Value: Variant): TDateTime;
  function NullVar2Guid(Value: Variant): String;
  function NullVar2Bool(Value: Variant): boolean;

  function CompareRecordData(Record1, Record2: TRecordData; Column: TColumnIndex;
    Reverse: Boolean = False): Integer;

implementation

uses
  Math, dbconst, fgl {$ifdef DEBUG_VDBGRID}, vtlogger{$endif};

type
  TIntegerList = TFPGList<Integer>;

function NullVar2Int(Value: Variant): Integer;
begin
  if VarIsNull(Value) then
    Result:= 0
  else
  begin
    try
      Result := VarAsType(Value, varInteger);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2Double(Value: Variant): Double;
begin
  if VarIsNull(Value) then
    Result := 0
  else
  begin
    try
      Result := VarAsType(Value, varDouble);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2DateTime(Value: Variant): TDateTime;
begin
  if VarIsNull(Value) then
    Result := 0
  else
  begin
    try
      Result := VarAsType(Value, varDate);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2Guid(Value: Variant): String;
const
  NullGuid: String = '{00000000-0000-0000-0000-000000000000}';
begin
  Result := VarToStrDef(Value, NullGuid);
end;

function NullVar2Bool(Value: Variant): Boolean;
begin
  if VarIsNull(Value) then
    Result := False
  else
  begin
    try
      Result := VarAsType(Value, varBoolean);
    except
      Result := False;
    end;
  end;
end;

function CompareRecordData(Record1, Record2: TRecordData; Column: TColumnIndex;
  Reverse: Boolean): Integer;
var
  Data1Value,
  Data2Value : Variant;
begin
  // Get Data values of Data1 & Data2
  Data1Value:= Record1.FieldValueByIdx[Column];
  Data2Value:= Record2.FieldValueByIdx[Column];

  case Record1.FieldTypeByIdx[Column] of
    // string types
    ftString,
    ftMemo,
    ftFixedChar: begin
         Result := AnsiCompareText( VarToStr(Data1Value),
                                    VarToStr(Data2Value));
    end;

    // integer types
    ftSmallint,
    ftInteger,
    ftLargeint,
    ftAutoInc,
    ftWord: begin
         Result:= CompareValue( NullVar2Int(Data1Value),
                                NullVar2Int(Data2Value));
    end;

    // float types
    ftFloat: begin
     Result:= CompareValue( NullVar2Double(Data1Value),
                            NullVar2Double(Data2Value));
    end;

    // date types
     ftDate,
     ftDateTime,
     ftTime: begin
         Result:= CompareValue( NullVar2DateTime(Data1Value),
                                NullVar2DateTime(Data2Value) );
    end;

    // mena typy (SK, CZ, ...)
    //todo see if using double here is ok
    ftCurrency    : begin
         Result:= CompareValue( NullVar2Double(Data1Value),
                                NullVar2Double(Data2Value));
    end;

    ftGuid:         begin
         Result:= CompareText( NullVar2Guid(Data1Value),
                               NullVar2Guid(Data2Value));
    end;

    ftBoolean : begin
         Result:= CompareValue( Integer(NullVar2Bool(Data1Value)),
                                Integer(NullVar2Bool(Data2Value)));
    end;

    else
      Result := 1;
  end;
  if Reverse then
    Result := -Result;
end;

{ ============================================================================ }
{ --- TRecordData ---------------------------------------------------------------- }

constructor TRecordData.Create;
begin
  inherited Create;
  fList := TFpList.Create;
  //fRecNo := 0;
end;

destructor TRecordData.Destroy;
begin
  ClearItems;
  fList.Destroy;
  inherited Destroy;
end;


procedure TRecordData.ClearItems;
var
  i   : integer;
  rec : PDBFieldValueRec;
begin
  for i := 0 to fList.Count - 1 do
  begin
    rec := PDBFieldValueRec(fList[i]);
    if rec <> nil then
      Dispose(rec);
  end;
  fList.Clear;
end;

procedure TRecordData.Edit(const AFieldName: string; const AFieldFlag: byte;
   NewFieldValue: Variant);
var
  i : integer;
begin
  if (AFieldName <> '') then
  begin
     for i := 0 to FieldsCount - 1 do
     begin
       if (UpperCase(Fields[i].FieldName) = UpperCase(AFieldName)) and
          (Fields[i].FieldFlag = AFieldFlag) then
          begin
            PDBFieldValueRec(fList[i])^.FieldValue:= NewFieldValue;
            Break;
          end;
     end;
  end;
end;


procedure TRecordData.Edit(const AFieldName: string; const AFieldFlag: byte;
    NewFieldValue: Variant; NewFieldType: TFieldType);
var
  i : integer;
begin
  if (AFieldName <> '') then
  begin
     for i := 0 to FieldsCount - 1 do
     begin
       if (UpperCase(Fields[i].FieldName) = UpperCase(AFieldName)) and
          (Fields[i].FieldFlag = AFieldFlag) then
          begin
            PDBFieldValueRec(fList[i])^.FieldValue := NewFieldValue;
            PDBFieldValueRec(fList[i])^.FieldType := NewFieldType;
            Break;
          end;
     end;
  end;
end;


procedure TRecordData.Add(const AFieldName: string; AFieldValue: Variant; AFieldType: TFieldType;
                          AFieldFlag: byte);
var
  rec: PDBFieldValueRec;
begin
  New(rec);
  rec^.FieldName := AFieldName;
  rec^.FieldValue := AFieldValue;
  rec^.FieldType := AFieldType;
  rec^.FieldFlag := AFieldFlag;
  fList.Add(rec);
end;

procedure TRecordData.Insert(Index: Integer; const AFieldName: string; AFieldValue: Variant;
                             AFieldType: TFieldType; AFieldFlag: byte);
var
  rec: PDBFieldValueRec;
begin
  new(rec);
  rec^.FieldName := AFieldName;
  rec^.FieldValue := AFieldValue;
  rec^.FieldType := AFieldType;
  rec^.FieldFlag := AFieldFlag;
  fList.Insert(Index, rec);
end;

procedure TRecordData.Delete(Index: Integer);
var
  rec: PDBFieldValueRec;
begin
  if (Index > -1) and (Index < FieldsCount) then
  begin
    rec := PDBFieldValueRec(fList[Index]);
    if (rec <> nil) then
       dispose(rec);
    fList.Delete(Index);
  end;
end;

function TRecordData.IndexOf(const AFieldName: string) : Integer;
var
  i : integer;
begin
   Result := -1;
   for i := 0 to FieldsCount - 1 do
   begin
     if (UpperCase(PDBFieldValueRec(fList[i])^.FieldName) = UpperCase(AFieldName)) then
     begin
        if (AFieldName <> '') or (PDBFieldValueRec(fList[i])^.FieldFlag = ffIndicator) then
          Result := i;
        Break;
     end;
   end;
end;

function TRecordData.IndexOf(const AFieldName: string; const AFieldFlag: byte) : Integer;
var
  i : integer;
begin
  Result := -1;
  if (AFieldName <> '') or (AFieldFlag = ffIndicator) then
  begin
     for i := 0 to FieldsCount - 1 do
     begin
       if (UpperCase(PDBFieldValueRec(fList[i])^.FieldName) = UpperCase(AFieldName)) and
          (PDBFieldValueRec(fList[i])^.FieldFlag = AFieldFlag) then
          begin
            Result:= i;
            Break;
          end;
     end;
  end;
end;


procedure TRecordData.Exchange(Index1, Index2: Integer);
var
  Item1, Item2: PDBFieldValueRec;
begin
  if (Index1 < 0) or (Index1 >= FieldsCount) then exit;
  if (Index2 < 0) or (Index2 >= FieldsCount) then exit;

  Item1 := PDBFieldValueRec(FList[Index1]);
  Item2 := PDBFieldValueRec(FList[Index2]);

  // Item 1
  PDBFieldValueRec(FList[Index1])^.FieldName:=  Item2^.FieldName;
  PDBFieldValueRec(FList[Index1])^.FieldValue:= Item2^.FieldValue;
  PDBFieldValueRec(FList[Index1])^.FieldType:=  Item2^.FieldType;
  PDBFieldValueRec(FList[Index1])^.FieldFlag:=  Item2^.FieldFlag;

  // Item 2
  PDBFieldValueRec(FList[Index2])^.FieldName:=  Item1^.FieldName;
  PDBFieldValueRec(FList[Index2])^.FieldValue:= Item1^.FieldValue;
  PDBFieldValueRec(FList[Index2])^.FieldType:=  Item1^.FieldType;
  PDBFieldValueRec(FList[Index2])^.FieldFlag:=  Item1^.FieldFlag;
end;


function TRecordData.GetFieldsCount: Integer;
begin
  Result := fList.Count;
end;

function TRecordData.GetIndidicatorByIdx(Index: Integer): boolean;
begin
  result := false;

  if (Index > -1) and (Index < FieldsCount) then
     result := (PDBFieldValueRec(fList[Index])^.FieldFlag = ffIndicator);
end;

function TRecordData.GetFielFlag(Index: Integer): byte;
begin
  result := ffUndeclared;

  if (Index > -1) and (Index < FieldsCount) then
     result := PDBFieldValueRec(fList[Index])^.FieldFlag;
end;

function TRecordData.GetCalculatedByIdx(Index: Integer): boolean;
begin
  Result := false;
  if (Index > -1) and (Index < FieldsCount) then
     Result := (PDBFieldValueRec(fList[Index])^.FieldFlag = ffCalculated);
end;

function TRecordData.GetCalculated(const IDText: string): boolean;
var i : integer;
begin
  Result := false;

  if (IDText <> '')
  then begin
     for i := 0 to FieldsCount-1 do
     begin
       if UpperCase(Fields[i].FieldName) = UpperCase(IDText)
          then begin
            Result:= (Fields[i].FieldFlag = ffCalculated);
            break;
          end;
     end;
  end;
end;

function TRecordData.GetCalculatedValueByIdx(Index: Integer): Variant;
begin
  if IsCalculatedByIdx[Index] then
    Result := FieldValueByIdx[Index]
  else
    Result := Null;
end;

procedure TRecordData.PutCalculatedValueByIdx(Index: Integer; Value: Variant);
begin
  if IsCalculatedByIdx[Index] then
     FieldValueByIdx[Index] := Value;
end;

function TRecordData.GetCalculatedValue(const IDText: string): Variant;
begin
  if IsCalculated[IDText] then
    Result := FieldValue[IDText]
  else
    Result := Null;
end;

procedure TRecordData.PutCalculatedValue(const IDText: string; Value: Variant);
begin
  if IsCalculated[IDText] then
     FieldValue[IDText] := Value;
end;

function TRecordData.GetField(Index: Integer) : TDBFieldValueRec;
begin
  if (Index > -1) and (Index < fList.Count) then
    Result:= PDBFieldValueRec(fList[Index])^;
end;

procedure TRecordData.PutField(Index: Integer; Value: TDBFieldValueRec);
begin
  if (Index > -1) and (Index < fList.Count) then
    PDBFieldValueRec(fList[Index])^:= Value;
end;

function TRecordData.GetFieldName(Index: Integer) : string;
begin
  if (Index > -1) and (Index < fList.Count) then
    Result := PDBFieldValueRec(fList[Index])^.FieldName;
end;

procedure TRecordData.PutFieldName(Index: Integer; Value: string);
begin
  if (Index > -1) and (Index < fList.Count) then
    PDBFieldValueRec(fList[Index])^.FieldName := Value;
end;

function TRecordData.GetFieldValueByIdx(Index: Integer) : Variant;
begin
  if (Index < 0) or (Index >= FieldsCount) then
    Result := null
  else
    Result := PDBFieldValueRec(fList[Index])^.FieldValue;
end;

function TRecordData.GetFieldValue(const FieldName: String) : Variant;
var
  i: integer;
begin
  Result := null;

  if (FieldName <> '') then
  begin
     for i := 0 to FieldsCount - 1 do
     begin
       if UpperCase(Fields[i].FieldName) = UpperCase(FieldName)
          then begin
            Result:= PDBFieldValueRec(fList[i])^.FieldValue;
            break;
          end;
     end;
  end;
end;


procedure TRecordData.PutFieldValueByIdx(Index: Integer; Value: Variant);
begin
  if (Index < 0) or (Index >= FieldsCount) then exit;

  PDBFieldValueRec(fList[Index])^.FieldValue:= Value;
end;


procedure TRecordData.PutFieldValue(const FieldName: String; Value: Variant);
var
  i : integer;
begin
  if (FieldName <> '') then
  begin
     for i := 0 to FieldsCount-1 do
     begin
       if UpperCase(Fields[i].FieldName) = UpperCase(FieldName)
          then begin
            PDBFieldValueRec(fList[i])^.FieldValue:= Value;
            break;
          end;
     end;
  end;
end;


function TRecordData.GetFieldTypeByIdx(Index: Integer) : TFieldType;
begin
  if (Index < 0) or (Index >= FieldsCount) then
    Result := ftUnknown
  else
    Result := PDBFieldValueRec(fList[Index])^.FieldType;
end;

function TRecordData.GetFieldType(const FieldName: string) : TFieldType;
var
  i : integer;
begin
  Result := ftUnknown;

  if (FieldName <> '') then
  begin
    for i := 0 to FieldsCount - 1 do
    begin
     if UpperCase(Fields[i].FieldName) = UpperCase(FieldName)
        then begin
          Result:= PDBFieldValueRec(fList[i])^.FieldType;
          break;
        end;
    end;
  end;
end;

{ --- TRecordData ---------------------------------------------------------------- }
{ ============================================================================ }



{ ============================================================================ }
{ --- TVirtualDBTreeDataLink ------------------------------------------------- }

constructor TVirtualDBTreeDataLink.Create(ATree: TCustomVirtualDBGrid);
begin
  inherited Create;
  FVirtualDBTree := ATree;
end;

procedure TVirtualDBTreeDataLink.ActiveChanged;
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'ActiveChanged');{$endif}
  //Logger.SendCallStack(lcAll, 'ActiveChanged');
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        DatabaseError(SUniDirectional);

  FVirtualDBTree.DataLinkActiveChanged;
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'ActiveChanged');{$endif}
end;

procedure TVirtualDBTreeDataLink.DataSetChanged;
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'DatasetChanged');{$endif}
  //Logger.SendCallStack(lcAll, 'DatasetChanged');
  FVirtualDBTree.DataLinkChanged;
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'DatasetChanged');{$endif}
end;

procedure TVirtualDBTreeDataLink.RecordChanged(Field: TField);
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'RecordChanged');{$endif}
  //Logger.SendCallStack(lcAll, 'RecordChanged');
  FVirtualDBTree.DataLinkRecordChanged(Field);
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'RecordChanged');{$endif}
end;


procedure TVirtualDBTreeDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'DataSetScrolled');{$endif}
  //Logger.SendCallStack(lcAll, 'DatasetScrolled');
  FVirtualDBTree.SetFocusToActualRecNo;
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'DataSetScrolled');{$endif}
end;


{ --- TVirtualDBTreeDataLink ------------------------------------------------- }
{ ============================================================================ }



{ ============================================================================ }
{ --- TVirtualDBTreeColumn --------------------------------------------------- }

procedure TVirtualDBTreeColumn.InternalSetFieldName(const AFieldName: String);
var
  Tree: TCustomVirtualDBGrid;
  Dataset: TDataSet;
begin
  Tree := TCustomVirtualDBGrid(GetOwnerTree);
  Dataset := Tree.LinkedDataSet;
  FFieldName := AFieldName;
  if Assigned(Dataset) then
  begin
    FField := Dataset.FindField(FFieldName);
    if FField <> nil then
      Text := FField.DisplayLabel;
    if Tree.HandleAllocated then
      CalculateWidth(Tree);
  end
  else
    FField := nil;
  //todo improve text setting when changing fieldname
  if Text = '' then
    Text := AFieldName;
end;

procedure TVirtualDBTreeColumn.CalculateWidth(Tree: TCustomVirtualDBGrid);
var
  CalcCanvas: TCanvas;
  TextSize: TSize;
  FieldWidth: Integer;
  CaptionWidth: Integer;
  TestStr: String;
begin
  if FField <> nil then
  begin
    // Calculate width
    CalcCanvas := TVirtualDBTreeColumns(Owner).HeaderBitmap.Canvas;
    CalcCanvas.Font := Owner.Header.Font;
    // width of column caption
    GetTextExtentPoint32(CalcCanvas.Handle, PChar(Text), Length(Text), TextSize);
    CaptionWidth := TextSize.cx + Spacing + Margin * 2;

    //todo: see how to handle DisplayWidth
    //FieldSize := FField.DisplayWidth;

    case FField.DataType of
     ftString:
       TestStr := StringOfChar('w', Min(FField.Size, 40));
     ftInteger, ftWord:
       TestStr := '999999';
     ftBoolean:
       TestStr := 'False';
     else
       TestStr := '99999999999999';
    end;

    CalcCanvas := Tree.Canvas;
    CalcCanvas.Font := Tree.Font;
    GetTextExtentPoint32(CalcCanvas.Handle, PChar(TestStr), Length(TestStr), TextSize);
    FieldWidth := TextSize.cx + Tree.Margin * 2;

    // which width FieldSize is greater then set it
    if CaptionWidth > FieldWidth then
    begin
      if CaptionWidth > - 1 then
        Width := CaptionWidth;
    end
    else begin
      if FieldWidth > - 1 then
        Width := FieldWidth;
    end;
  end;
end;

procedure TVirtualDBTreeColumn.SetFieldName(const AFieldName: String);
begin
  if ColumnType = ctDBField then
  begin
    if FFieldName <> AFieldName then
      InternalSetFieldName(AFieldName);
  end
  else
    fFieldName:= '';
end;


procedure TVirtualDBTreeColumn.SetColumnType(Value: TColumnType);
var
  OwnerTree: TCustomVirtualDBGrid;
begin
  if Value <> fColumnType then
  begin
    if Value = ctIndicator then
    begin
      OwnerTree := GetOwnerTree;
      OwnerTree.IndicatorBitmapNeeded;
      if OwnerTree.IndicatorColumn <> nil then
        raise Exception.Create('Duplicate Indicator Column');
      //force the first position
      //necessary to set Index to 0 also to work inside Begin/EndUpdate
      Index := 0;
      Position := 0;
    end;

    fColumnType := Value;

    if Value <> ctDBField then
    begin
      fFieldName := '';
      if Value = ctIndicator then
      begin
        Text := '';
        if not (csLoading in OwnerTree.ComponentState) then
        begin
          Options := Options - [coDraggable, coResizable, coShowDropMark];
          Color := DefaultIndicatorColor;
        end;
      end;
    end;
    InternalSetFieldName(fFieldName);
  end;
end;


function TVirtualDBTreeColumn.GetOwnerTree: TCustomVirtualDBGrid;
begin
  Result := TCustomVirtualDBGrid(Owner.Header.Treeview);
end;


function TVirtualDBTreeColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

constructor TVirtualDBTreeColumn.Create(Collection: TCollection);
begin
  //FField:= nil;
  //FieldName:= '';
  fColumnType:= ctDBField;
  //fSavedMainColumn:= 0;

  inherited Create(Collection);
end;



procedure TVirtualDBTreeColumn.Assign(Source: TPersistent);
begin
  if Source is TVirtualDBTreeColumn then
  begin
    FieldName := TVirtualDBTreeColumn(Source).FieldName;
    ColumnType:= TVirtualDBTreeColumn(Source).ColumnType;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;



function TVirtualDBTreeColumn.Equals(OtherColumn: TObject): Boolean;
begin
  Result := (FieldName =  TVirtualDBTreeColumn(OtherColumn).FieldName) and
            (ColumnType = TVirtualDBTreeColumn(OtherColumn).ColumnType);
end;


procedure TVirtualDBTreeColumn.LoadFromStream(const Stream: TStream; Version: Integer);
var
  Dummy: Integer;
  S: String;

begin
  with Stream do
  begin
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(S, Dummy);
    ReadBuffer(PChar(S)^, Dummy);
    FieldName := S;
  end;
end;


procedure TVirtualDBTreeColumn.SaveToStream(const Stream: TStream);
var
  Dummy: Integer;
begin
  with Stream do
  begin
    Dummy := Length(FFieldName);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PChar(FFieldName)^, Dummy);
  end;
end;


{ --- TVirtualDBTreeColumn --------------------------------------------------- }
{ ============================================================================ }



{ ============================================================================ }
{ --- TVirtualDBTreeColumns -------------------------------------------------- }


{.$IFDEF COMPILER_6_UP}
{procedure TVirtualDBTreeColumns.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var
   Header: TVTDBHeader;
   Grid:   TCustomVirtualDBGrid;
begin
  inherited;

  case Action of
    cnDeleting: begin
      Header:= TVTDBHeader(GetOwner);
      Grid:=   TCustomVirtualDBGrid(Header.GetOwner);
      if (Grid <> nil) then
          Grid.RemoveColumnFromRecordData(TVirtualDBTreeColumn(Item));
    end;
  end;
end;}
{.$ENDIF}


{
procedure TCustomVirtualDBGrid.RemoveColumnFromRecordData(ColumnItem: TVirtualDBTreeColumn);
var Run: PVirtualNode;
    Data: PNodeData;
    Index: Integer;
    RecordData: TRecordData;
    FieldName: string;
    FieldFlag: byte;
begin
  BeginUpdate;
  try
    Run:= GetFirst;
    while Assigned(Run) do
    begin
      Data:= InternalGetNodeData(Run);
      if IsDataOk(Data) then
      begin
         RecordData:= nil;
         RecordData:= Data.RecordData;

         if Assigned(RecordData) then
         begin
           case ColumnItem.ColumnType of
                ctDBField: begin
                   FieldName:= ColumnItem.FieldName;
                   FieldFlag:= ffDBField;
                end;

                ctCalculated: begin
                   FieldName:= ColumnItem.Text;
                   FieldFlag:= ffCalculated;
                end;

                ctIndicator: begin
                   FieldName:= '';
                   FieldFlag:= ffIndicator;
                end;
           end;
           Index:= RecordData.IndexOf(FieldName, FieldFlag);

           if (Index > -1) and (Index < RecordData.FieldsCount)
              then RecordData.Delete(Index);

         end;
      end;

      Run:= GetNextSibling(Run);
    end;
  finally
    EndUpdate;
  end;
end;
}

procedure TCustomVirtualDBGrid.RearrangeColumnsRecordData;
var Run: PVirtualNode;
    Data: PNodeData;
    Index, loop, maxloop: Integer;
    RecordData: TRecordData;
    FieldName: string;
    FieldFlag: byte;
    ColumnType: TColumnType;
begin
  BeginUpdate;
  try
    Run:= GetFirst;
    while Assigned(Run) do
    begin
      Data:= InternalGetNodeData(Run);
      if IsDataOk(Data) then
      begin
         RecordData:= Data.RecordData;

         if Assigned(RecordData) then
         begin

           loop:= 0;
           maxloop:= RecordData.FieldsCount;
           repeat
              FieldName:= RecordData.Fields[loop].FieldName;
              FieldFlag:= RecordData.FieldFlag[loop];
              case (FieldFlag) of
                   ffDBField: ColumnType:= ctDBField;
                   ffCalculated: ColumnType:= ctCalculated;
                   ffIndicator: ColumnType:= ctIndicator;
                   ffUndeclared: begin
                     inc(loop);
                     continue;
                   end;
              end;


              Index:= TVirtualDBTreeColumns(Header.Columns).IndexOf(FieldName, ColumnType);

              // If the column doesnt exist then remove from RecordData
              if (Index = -1) then
              begin
                 RecordData.Delete(loop);
                 dec(maxloop);
              end;

              inc(loop);

           until (loop >= maxloop);

         end;
      end;

      Run:= GetNextSibling(Run);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TVirtualDBTreeColumns.CalculateDefaultColumnsWidth;
var
  i: Integer;
  Column: TVirtualDBTreeColumn;
  Tree: TCustomVirtualDBGrid;
begin
  Tree := TCustomVirtualDBGrid(Header.Treeview);
  for i := 0 to Count - 1 do
  begin
    Column := TVirtualDBTreeColumn(Items[i]);
    if Column.IsDefault then
      Column.CalculateWidth(Tree);
  end;
end;

procedure TVirtualDBTreeColumns.Update(Item: TCollectionItem);
var
   Header: TVTDBHeader;
   Grid:   TCustomVirtualDBGrid;
begin
  inherited Update(Item);
  if (fLastCount <> Count) then
  begin
    if (Count < fLastCount) then
    begin
      Header:= TVTDBHeader(GetOwner);
      Grid:=   TCustomVirtualDBGrid(Header.GetOwner);
      if (Grid <> nil) then
         Grid.RearrangeColumnsRecordData;
    end;
    fLastCount:= Count;
  end;

  if (Item = nil) then exit;

  if (TVirtualDBTreeColumn(Item).ColumnType = ctIndicator)and
     (TVirtualDBTreeColumn(Item).Position <> 0)
  then begin
    MessageDlg('Column with column type ctIndicator must have position 0', mtError, [mbok], 0);
    TVirtualDBTreeColumn(Item).Position:= 0;
  end;
end;

function TVirtualDBTreeColumns.IndexOf(const FieldNameOrIDText: string; ColumnType: TColumnType): Integer;
var
   loop: integer;
   column: TVirtualDBTreeColumn;
begin
  result:= -1;

  for loop:=0 to count-1 do
  begin
     column:= TVirtualDBTreeColumn(Items[loop]);
     if (column.ColumnType = ColumnType) then
     begin
        case ColumnType of
             ctDBField: begin
                if (UpperCase(column.FieldName) = UpperCase(FieldNameOrIDText)) then
                begin
                   Result:= loop;
                   break;
                end;
             end;

             ctCalculated: begin
                if (UpperCase(column.Text) = UpperCase(FieldNameOrIDText)) then
                begin
                   Result:= loop;
                   break;
                end;
             end;

             ctIndicator: begin
                Result:= loop;
                break;
             end;
        end;
     end;
  end;
end;


{ --- TVirtualDBTreeColumns -------------------------------------------------- }
{ ============================================================================ }



{ ============================================================================ }
{ --- TVTDBHeader ------------------------------------------------------------ }

function TVTDBHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  Result:= TVirtualDBTreeColumns;
end;

{ --- TVTDBHeader ------------------------------------------------------------ }
{ ============================================================================ }



{ ============================================================================ }
{ --- TVTDBOptions ----------------------------------------------------------- }

function TVTDBOptions.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TVTDBOptions.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TVTDBOptions.SetIndicatorImIndex(Value: TImageIndex);
begin
  if Value <> fIndicatorImIndex then
  begin
    fIndicatorImIndex := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetIndicatorAlign(Value: TIndicatorAlign);
begin
  if Value <> fIndicatorAlign then
  begin
    fIndicatorAlign := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetIndicatorVAlign(Value: TIndicatorVAlign);
begin
  if Value <> fIndicatorVAlign then
  begin
    fIndicatorVAlign := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetOddRowColor(value: tcolor);
begin
  if fOddRowColor <> Value then
  begin
    fOddRowColor := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetEvenRowColor(value: tcolor);
begin
  if fEvenRowColor <> Value then
  begin
    fEvenRowColor := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetAdvOptions(value: TVTDBAdvOptions);
var
   WAutoOptions:      TVTAutoOptions;
   WMiscOptions:      TVTMiscOptions;
   WPaintOptions:     TVTPaintOptions;
   WSelectionOptions: TVTSelectionOptions;
begin
  WAutoOptions:= Treeview.TreeOptions.AutoOptions;
  WMiscOptions:= Treeview.TreeOptions.MiscOptions;
  WPaintOptions:= Treeview.TreeOptions.PaintOptions;
  WSelectionOptions:= Treeview.TreeOptions.SelectionOptions;

  fAdvOptions:= value;

  if (aoEditable in fAdvOptions) and (not (aoFullRowSelect in fAdvOptions)) 
     then Include(WMiscOptions, toEditable)
     else Exclude(WMiscOptions, toEditable);

  if (aoEditOnClick in fAdvOptions)
     then Include(WMiscOptions, toEditOnClick)
     else Exclude(WMiscOptions, toEditOnClick);

  if (aoEditOnDblClick in fAdvOptions) then
     begin
       Include(WMiscOptions, toEditOnDblClick);
       Exclude(WMiscOptions, toToggleOnDblClick);
     end
     else
     begin
       Exclude(WMiscOptions, toEditOnDblClick);
       Include(WMiscOptions, toToggleOnDblClick);
     end;

  if (aoShowHorzLines in fAdvOptions)
     then Include(WPaintOptions, toShowHorzGridLines)
     else Exclude(WPaintOptions, toShowHorzGridLines);

  if (aoShowVertLines in fAdvOptions)
     then Include(WPaintOptions, toShowVertGridLines)
     else Exclude(WPaintOptions, toShowVertGridLines);

  if (aoCenterScrollIntoView in fAdvOptions)
     then Include(WSelectionOptions, toCenterScrollIntoView)
     else Exclude(WSelectionOptions, toCenterScrollIntoView);

  if (aoFullRowSelect in fAdvOptions)
     then Include(WSelectionOptions, toFullRowSelect)
     else Exclude(WSelectionOptions, toFullRowSelect);

  if (aoMultiSelect in fAdvOptions)
     then Include(WSelectionOptions, toMultiSelect)
     else Exclude(WSelectionOptions, toMultiSelect);


  Treeview.TreeOptions.AutoOptions:= WAutoOptions;
  Treeview.TreeOptions.MiscOptions:= WMiscOptions;
  Treeview.TreeOptions.PaintOptions:= WPaintOptions;
  Treeview.TreeOptions.SelectionOptions:= WSelectionOptions;
  Treeview.Invalidate;
end;


procedure TVTDBOptions.SetSortColumnBgColor(Value: TColor);
begin
  if fSortColumnBgColor <> value then
  begin
    fSortColumnBgColor:= value;
    Treeview.Invalidate;
  end;
end;


function TVTDBOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


constructor TVTDBOptions.Create(AOwner: TCustomVirtualDBGrid);
begin
  inherited Create;
  FOwner:=             AOwner;
  FDataLink:=          TVirtualDBTreeDataLink.Create(AOwner);
  fIndicatorImIndex:=  -1;
  fIndicatorAlign:=    aiCenter;
  fIndicatorVAlign:=   aiMiddle;
  fEvenRowColor:=      clWindow;
  fOddRowColor:=       clWhiteSmoke;
  fSortingType:=       stBuildIn;
  fRecordCountType:=   rcFromDataset;
  fSortColumnBgColor:= clLightYellow;
  AdvOptions:=         DefaultAdvOptions;
end;



destructor TVTDBOptions.Destroy;
begin
  fDataLink.Destroy;
  inherited Destroy;
end;



procedure TVTDBOptions.Assign(Source: TPersistent);
begin
  if (Source is TVTDBOptions) then
  begin
    DataSource:=          TVTDBOptions(Source).DataSource;
    IndicatorImageIndex:= TVTDBOptions(Source).IndicatorImageIndex;
    IndicatorAlign:=      TVTDBOptions(Source).IndicatorAlign;
    IndicatorVAlign:=     TVTDBOptions(Source).IndicatorVAlign;
    OddRowColor:=         TVTDBOptions(Source).OddRowColor;
    EvenRowColor:=        TVTDBOptions(Source).EvenRowColor;
    SortingType:=         TVTDBOptions(Source).SortingType;
    AdvOptions:=          TVTDBOptions(Source).AdvOptions;
  end
  else inherited Assign(Source);

end;

{ --- TVTDBOptions ----------------------------------------------------------- }
{ ============================================================================ }



{ ============================================================================ }
{ --- TCustomVirtualDBGrid --------------------------------------------------- }

function TCustomVirtualDBGrid.GetHeader: TVTDBHeader;
begin
  Result := TVTDBHeader(inherited Header);
end;

procedure TCustomVirtualDBGrid.AddDefaultColumns;
var
  FieldList: TFields;
  Field: TField;
  Column: TVirtualDBTreeColumn;
  i: Integer;
begin
  FieldList := LinkedDataSet.Fields;
  if FieldList.Count = 0 then
    Exit;
  Header.Columns.BeginUpdate;
  try
    // If aoAutoInsertIndicator is set then automatically add indicator column
    if aoAutoInsertIndicator in DBOptions.AdvOptions then
    begin
      Column := AddColumn(ctIndicator, '', '', 15);
      Column.IsDefault := True;
    end;
    for i := 0 to FieldList.Count - 1 do
    begin
      Field := FieldList[i];
      Column := AddColumn(ctDBField, Field.FieldName, Field.DisplayName, - 1);
      Column.IsDefault := True;
    end;
  finally
    Header.Columns.EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.IndicatorBitmapNeeded;
begin
  if fIndicatorBMP = nil then
  begin
    fIndicatorBMP := TBitmap.Create;
    fIndicatorBMP.SetSize(10, 10);
    fIndicatorBMP.Canvas.Brush.Color := clFuchsia;
    fIndicatorBMP.Canvas.FillRect(0, 0, 12, 12);

    fIndicatorBMP.Canvas.Brush.Color := clBlack;
    fIndicatorBMP.Canvas.Pen.Color := clBlack;
    fIndicatorBMP.Canvas.Polygon([Point(0, 0), Point(4, 4), Point(4, 5), Point(0, 9)]);
    fIndicatorBMP.Canvas.Polygon([Point(5, 0), Point(9, 4), Point(9, 5), Point(5, 9)]);
    fIndicatorBMP.TransparentMode := tmFixed;
    fIndicatorBMP.TransparentColor := clFuchsia;
    fIndicatorBMP.Transparent := True;
  end;
end;

procedure TCustomVirtualDBGrid.RemoveDefaultColumns;
var
  i: Integer;
  Column: TVirtualDBTreeColumn;
  Columns: TVirtualDBTreeColumns;
begin
  Columns :=  TVirtualDBTreeColumns(Header.Columns);
  Columns.BeginUpdate;
  try
    i := 0;
    while i < Columns.Count do
    begin
      Column :=  TVirtualDBTreeColumn(Columns[i]);
      if Column.IsDefault then
        Column.Destroy
      else
        Inc(i);
    end;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.SetHeader(Value: TVTDBHeader);
begin
  inherited Header := Value;
end;

constructor TCustomVirtualDBGrid.Create(Owner: TComponent);
begin
  inherited;
  DefaultText := '';
  FInternalDataOffset := AllocateInternalDataArea(SizeOf(TNodeData));
  //fLoadingDataFlag := 0;
  //fLastRecordCount := 0;


  Header.Options := [hoColumnResize,hoDrag,hoHotTrack,hoShowHint,hoShowSortGlyphs,hoVisible];
  Header.SortColumn := -1;
  
  with TreeOptions do
  begin
    //AnimationOptions:= [];
    AutoOptions:=      [toAutoDropExpand, toAutoScroll, toAutoTristateTracking,
                       toAutoDeleteMovedNodes];
    MiscOptions:=      [toAcceptOLEDrop, toGridExtensions, toInitOnSave,
                       toToggleOnDblClick, toWheelPanning];
    PaintOptions:=     [toHideFocusRect, toShowDropmark, toThemeAware, toUseBlendedImages];
    SelectionOptions:= [toDisableDrawSelection, toExtendedFocus, toMiddleClickSelect,
                       toRightClickSelect];
    StringOptions:=    [toSaveCaptions,toAutoAcceptEditChange];
  end;
  fDBOptions := TVTDBOptions.Create(self);
end;

destructor TCustomVirtualDBGrid.Destroy;
begin
  fDBOptions.Free;
  fIndicatorBMP.Free;
  inherited Destroy;
end;

function TCustomVirtualDBGrid.GetIndicatorColumn: TVirtualDBTreeColumn;
var
  IndColumn: TColumnIndex;
begin
  Result := nil;
  // indicator column is always at position 0
  IndColumn := Header.Columns.ColumnFromPosition(0);
  if IndColumn <> NoColumn then
  begin
    Result := TVirtualDBTreeColumn(Header.Columns[IndColumn]);
    if Result.ColumnType <> ctIndicator then
       Result := nil;
  end;
end;

function TCustomVirtualDBGrid.GetSortingColumn: TVirtualDBTreeColumn;
var
   Index: Integer;
begin
  result:= nil;

  Index:= Header.SortColumn;
  if (Index > NoColumn) then
      Result:= TVirtualDBTreeColumn(Header.Columns[Index]);
end;

procedure TCustomVirtualDBGrid.InitializeWnd;
begin
  inherited InitializeWnd;
  //Set the selected node after the handle is created
  if FocusedNode <> nil then
    Selected[FocusedNode] := True;
  TVirtualDBTreeColumns(Header.Columns).CalculateDefaultColumnsWidth;
end;

function TCustomVirtualDBGrid.GetHeaderClass: TVTHeaderClass;
begin
  Result := TVTDBHeader;
end;

function TCustomVirtualDBGrid.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TVirtualDBTreeColumn;
end;

function TCustomVirtualDBGrid.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

function TCustomVirtualDBGrid.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TCustomVirtualDBGrid.SetOptions(const Value: TStringTreeOptions);
begin
  inherited TreeOptions := Value;
end;

procedure TCustomVirtualDBGrid.DataLinkActiveChanged;
var
  ColumnPosition: Integer;
begin
  if (not (csLoading in ComponentState)) and
     (not IsDataLoading) then
  begin
     IncLoadingDataFlag;
     BeginUpdate;
     try
       if Assigned(LinkedDataSet) and LinkedDataSet.Active then
       begin
         fRecordCount := GetRecordCount;
         fLastRecordCount := fRecordCount;
         if (Header.Columns.Count = 0) or (aoAddDefaultColumns in DBOptions.AdvOptions) then
           AddDefaultColumns;
         InternalInitializeDBTree;
         if Header.Columns.Count > 0 then
         begin
           ColumnPosition := IfThen(IndicatorColumn <> nil, 1, 0);
           FocusedColumn := Header.Columns.ColumnFromPosition(ColumnPosition);
         end;
       end
       else
       begin
         fRecordCount := 0;
         fLastRecordCount := 0;
         RemoveDefaultColumns;
         Clear;
       end;
     finally
       EndUpdate;
       DecLoadingDataFlag;
     end;

     if Assigned(LinkedDataSet) and LinkedDataSet.Active then
     begin
       //todo: refactor to call UpdateDBTree earlier
       if (DBOptions.SortingType = stBuildIn) and (Header.SortColumn <> NoColumn) then
       begin
         // The sort feature requires that all data be loaded
         UpdateAllRecords(False);
         SortTree(Header.SortColumn, Header.SortDirection);
       end
       else
         UpdateVisibleDBTree(True);

       if fRecordCount > 0 then
         SetFocusToNode(FindNodeByRecNo(1), False);
     end;
  end;
end;

procedure TCustomVirtualDBGrid.DataLinkChanged;
var
  DoSort: Boolean;
begin
  // we can reflect changes in database(like insert or delete record(s))
  // only if DBOptions.RecordCountType = rcFromDataset is set and we know
  // how many records is in the dataset

  if (not (csLoading in ComponentState)) and
     (DBOptions.RecordCountType = rcFromDataset) and
     (not IsDataLoading) then
  begin
    if Assigned(LinkedDataSet) and LinkedDataSet.Active then
    begin
       {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'DataLinkChanged');{$endif}
       DoSort := (DBOptions.SortingType = stBuildIn) and (Header.SortColumn <> NoColumn);
       //Skip GetRecordCount and retrieve RecordCount directly from dataset
       //since we already know that RecordCountType is rcFromDataset
       fRecordCount := LinkedDataSet.RecordCount;
       // If old record count(fLastRecordCount) <> to new record count
       // then, there was add or remove some record and we want to reflect this changes
       // Otherwise is necessary to sync to dataset RecNo to reflect a call to Last/First
       if (fRecordCount <> fLastRecordCount) then
       begin
          ReInitializeDBGrid;
          fLastRecordCount := fRecordCount;
          if DoSort then
          begin
            UpdateAllRecords;
            SortTree(Header.SortColumn, Header.SortDirection);
          end;
       end
       else
       begin
         //todo track the dataset state to avoid unnecessary calls to UpdateAllRecords
         if LinkedDataset.State <> dsInsert then
         begin
           UpdateAllRecords;
           if DoSort then
             SortTree(Header.SortColumn, Header.SortDirection);
           SetFocusToActualRecNo;
         end;
       end;
       {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'DataLinkChanged');{$endif}
    end;
  end;

end;

procedure TCustomVirtualDBGrid.DataLinkRecordChanged(Field: TField);
begin
  if not (csLoading in ComponentState) and not IsDataLoading and Assigned(LinkedDataSet) then
  begin
     IncLoadingDataFlag;
     try
       UpdateCurrentRecord;
       //disable the sort here because it'll be sorted later in response to post
       //check if the changed field is the sort field and do the sort if so
       {
       if (Field <> nil) and (DBOptions.SortingType = stBuildIn)
         and (Header.SortColumn <> NoColumn)
         and (TVirtualDBTreeColumns(Header.Columns).IndexOf(Field.FieldName, ctDBField) = Header.SortColumn) then
       begin
         UpdateAllRecords(False);
         SortTree(Header.SortColumn, Header.SortDirection);
       end;
       }
     finally
       DecLoadingDataFlag;
     end;
  end;
end;

function TCustomVirtualDBGrid.GetDataSet: TDataSet;
begin
  Result := fDBOptions.fDataLink.DataSet;
end;

procedure TCustomVirtualDBGrid.DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; var Color: TColor;
           var EraseAction: TItemEraseAction);
begin
  if (not (aoStrippedRows in DBOptions.AdvOptions)) then exit;

  with Canvas do
  begin
    if Odd(Node.Index)
       then Color := DBOptions.OddRowColor
       else Color := DBOptions.EvenRowColor;

    EraseAction := eaColor;
  end;

  inherited;
end;


procedure TCustomVirtualDBGrid.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PNodeData;
begin
  if (LinkedDataSet <> nil) and (LinkedDataSet.Active) and not IsDataLoading then
  begin
    Data := InternalGetNodeData(Node);
    if IsDataOk(Data) then
      GotoRecNo(Data.RecordData.RecNo);
  end;
  inherited DoFocusChange(Node, Column);
end;


procedure TCustomVirtualDBGrid.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (aoHighlightSortColumn in DBOptions.AdvOptions) and
     (Column > NoColumn) then
  begin
    Canvas.Brush.Color := Header.Columns[Column].Color;
    if ((Column <> FocusedColumn) or (Node <> FocusedNode)) and
       (Column = Header.SortColumn)
       then
       begin
         Canvas.Brush.Color := DBOptions.SortColumnBgColor;
         Canvas.FillRect(CellRect);
       end;
  end;

  inherited;
end;


procedure TCustomVirtualDBGrid.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellRect: TRect);
var
  X, Y, IconWidth, IconHeight, ImageIndex: Integer;
  R: TRect;
  GridColumn: TVirtualDBTreeColumn;
begin
  GridColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if GridColumn.ColumnType = ctIndicator then
  begin
    R := CellRect;
    // draw indicator arrow
    with Canvas do
    begin
      if toShowVertGridLines in TreeOptions.PaintOptions then
        Inc(R.Right);
      if toShowHorzGridLines in TreeOptions.PaintOptions then
        Inc(R.Bottom);

      Brush.Color := GridColumn.Color;
      FillRect(R);
      DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT {or BF_MIDDLE});

      if Node = FocusedNode then
      begin
        ImageIndex := DBOptions.IndicatorImageIndex;
        // Get Indicator bitmap width
        if (ImageIndex > -1) and (Images <> nil) and (ImageIndex < Images.Count) then
        begin
          IconWidth := Images.Width;
          IconHeight := Images.Height;
        end
        else begin
          IconWidth := fIndicatorBMP.Width;
          IconHeight := fIndicatorBMP.Height;
          ImageIndex := -1;
        end;

        // Calculate X coordinate
        case DBOptions.IndicatorAlign of
          aiLeft:
            X := 0;
          aiCenter:
            X := ((R.Right - R.Left) - IconWidth) div 2 + 1;
          aiRight:
            X := (R.Right - R.Left) - IconWidth;
        end;

        // Calculate Y coordinate
        case DBOptions.IndicatorVAlign of
          aiTop:
            Y := 0;
          aiMiddle:
            Y := ((R.Bottom - R.Top) - IconHeight) div 2 + 1;
          aiBottom:
            Y := (R.Bottom - R.Top) - IconHeight;
         end;

         if ImageIndex = -1 then
           Canvas.Draw(X, Y, fIndicatorBMP)
         else
           Images.Draw(Canvas, X, Y, ImageIndex);
      end;
    end;
  end
  else
    inherited DoAfterCellPaint(Canvas, Node, Column, CellRect);
end;

procedure TCustomVirtualDBGrid.DoFreeNode(Node: PVirtualNode);
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(Node);
  if IsDataOk(Data) then
    FreeAndNil(Data.RecordData);
  inherited DoFreeNode(Node);
end;

procedure TCustomVirtualDBGrid.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
var
  Data: PNodeData;
  DBColumn: TVirtualDBTreeColumn;
  FieldValue: variant;
begin
  if Column <= NoColumn  then
    Exit;
  DBColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if (DBColumn.ColumnType = ctIndicator) or (DBColumn.Style = vsOwnerDraw) then
    Exit;
  Data := InternalGetNodeData(Node);
  if IsDataOk(Data) then
  begin
    //todo: add a Column > FieldIndex map to avoid using fieldname for the data lookup
    if DBColumn.ColumnType = ctDBField then
      FieldValue := Data.RecordData.FieldValue[DBColumn.FieldName]
    else
      FieldValue := Data.RecordData.FieldValueByIdx[DBColumn.Index];
    Text := VarToStr(FieldValue);
  end;
end;

procedure TCustomVirtualDBGrid.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  NodeData: PNodeData;
begin
  //inherited DoInitNode(Parent, Node, InitStates);
  NodeData := InternalGetNodeData(Node);
  NodeData^.RecNo := Node^.Index + 1;
end;

procedure TCustomVirtualDBGrid.DoNewText(Node: PVirtualNode;
  Column: TColumnIndex; const Text: String);
var
  WField:  TField;
  DBColumn: TVirtualDBTreeColumn;
  Data: PNodeData;
  PostChanges: Boolean;
  PostText: String;
begin
  // if we dont want to post changes to database (toEditable is not set in the treeoptions->misc) then exit
  {if not (toEditable in TreeOptions.MiscOptions)
     then exit;}

  //todo: see if these Column checks are really necessary
  if Column <= NoColumn then
    Exit;

  // if column is ctIndicator then exit
  DBColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if DBColumn.ColumnType = ctIndicator then
    Exit;

  Data := InternalGetNodeData(Node);
  if not IsDataOk(Data) then
    Exit;

  PostText := Text;

  if DBColumn.ColumnType = ctCalculated then
  begin
    PostChanges := True;
    DoPostChanges(DBColumn.Text, Column, DBColumn.ColumnType, Data.RecordData, Node.Index,
      PostText, PostChanges);
    if PostChanges then
      Data.RecordData.CalculatedValue[DBColumn.Text] := PostText;
  end
  else begin
    PostChanges := False;
    DoPostChanges(DBColumn.FieldName, Column, DBColumn.ColumnType, Data.RecordData,
      Node.Index, PostText, PostChanges);
    if PostChanges and Assigned(LinkedDataSet) and LinkedDataSet.CanModify then
    begin
      WField := LinkedDataSet.FindField(DBColumn.FieldName);
      if WField <> nil then
      begin
        //todo: see if is necessary some kind of error handling here
        try
          LinkedDataSet.Edit;
          WField.AsString := PostText;
          LinkedDataSet.Post;
        except
        end;
      end;
    end;
  end;
  inherited DoNewText(Node, Column, Text);
end;

//{$ifdef VTV_VER_5}
procedure TCustomVirtualDBGrid.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  if (DBOptions.SortingType <> stNone) and (aoAllowSorting in DBOptions.AdvOptions) then
  begin
    DoSortColumn(HitInfo.Column);
    DoChangeSort(Header.SortColumn, Header.SortDirection);
  end;
  inherited DoHeaderClick(HitInfo);
end;
//{$else}
//procedure TCustomVirtualDBGrid.DoHeaderClick(Column: TColumnIndex;
//    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//begin
//  if (DBOptions.SortingType <> stNone) and (aoAllowSorting in DBOptions.AdvOptions) then
//  begin
//    DoSortColumn(Column);
//    DoChangeSort(Header.SortColumn, Header.SortDirection);
//  end;
//  inherited DoHeaderClick(Column, Button, Shift, X, Y);
//end;
//{$endif}


procedure TCustomVirtualDBGrid.DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition);
var
   loop: integer;
begin
  if (Column > NoColumn) then
    if (TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType <> ctIndicator)
       then
       inherited DoHeaderDragged(Column, OldPosition);

  with Header do
  begin
    for loop:= 0 to Columns.Count-1 do
    begin
      if (TVirtualDBTreeColumn(Columns[loop]).ColumnType = ctIndicator) then
        if (Columns[loop].Position <> 0) then
        begin
           Columns[loop].Position := 0;
           Invalidate(Columns[loop]);
           exit;
        end;
    end;
  end;
end;


function TCustomVirtualDBGrid.DoFocusChanging(OldNode, NewNode: PVirtualNode;
   OldColumn, NewColumn: TColumnIndex): Boolean;
begin
  if (NewColumn <= NoColumn) then
  begin
    result:= false;
    exit;
  end;

  Result := (TVirtualDBTreeColumn(Header.Columns[NewColumn]).ColumnType <> ctIndicator);

  if Result then
     Result := inherited DoFocusChanging(OldNode, NewNode, OldColumn, NewColumn);
end;


function TCustomVirtualDBGrid.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
var
  Data1,
  Data2: PNodeData;
  ColType: TColumnType;

begin
  Result := 0;
  try
    //todo: override Sort method and move all these checks to there

    // If Column is out of bounds then do nothing ...
    if (Column < 0) or (Column >= Header.Columns.Count) then exit;

    ColType:= TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType;

    // If column is ctIndicator then do nothing ...
    if (ColType = ctIndicator) then exit;

    // If column is DBField and we dont want to sort this type of column then do nothing ...
    if (ColType = ctDBField) and
       (not (aoSortDBFieldColumns in DBOptions.AdvOptions)) then exit;

    // If column is ctCalculated and we dont want to sort this type of column then do nothing ...
    if (ColType = ctCalculated) and
       (not (aoSortCalculatedColumns in DBOptions.AdvOptions)) then exit;


    Data1:= InternalGetNodeData(Node1);
    Data2:= InternalGetNodeData(Node2);

    if (not IsDataOk(Data1)) or (not IsDataOk(Data2))
       then exit;


    // If DataType(FieldType) of compared nodes are not identical the do nothing ..
    if (Data1^.RecordData.FieldTypeByIdx[Column] <> Data2^.RecordData.FieldTypeByIdx[Column])
       then exit;

    if Assigned(FOnCompareRecord) then
      FOnCompareRecord(Self, Data1.RecordData, Data2.RecordData, Column, Result)
    else
      Result := CompareRecordData(Data1.RecordData, Data2.RecordData, Column);

  except
    Result:= 1;
  end;
end;


procedure TCustomVirtualDBGrid.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
   ColumnType: TColumnType;
   Col: TVirtualDBTreeColumn;
begin
  inherited DoCanEdit(Node, Column, Allowed);
  if not Allowed then
    Exit;
  if (Column > NoColumn) then
  begin
    Col:= TVirtualDBTreeColumn(Header.Columns[Column]);
    ColumnType:= col.ColumnType; 

    case ColumnType of
         ctIndicator:
            Allowed:= false;

         ctDBField:
            Allowed:= Allowed and (aoEditDBFieldColumns in DBOptions.AdvOptions);

         ctCalculated:
            Allowed:= Allowed and (aoEditCalculatedColumns in DBOptions.AdvOptions);
    end;
  end
  else
  Allowed:= False;
end;

procedure TCustomVirtualDBGrid.HandleMouseDblClick(var Message: TLMMouse;
  const HitInfo: THitInfo);
var
  Column: TVirtualDBTreeColumn;
  Field: TField;
  Data: PNodeData;
begin
  inherited HandleMouseDblClick(Message, HitInfo);
  if HitInfo.HitNode <> nil then
  begin
    //toggle boolean fields
    if (aoAutoToggleBoolean in fDBOptions.AdvOptions) and
      (HitInfo.HitNode = FocusedNode) and
      (HitInfo.HitColumn > NoColumn) then
    begin
      Column := TVirtualDBTreeColumn(Header.Columns[HitInfo.HitColumn]);
      if (Column.ColumnType = ctDBField) and Assigned(LinkedDataset) then
      begin
        Field := LinkedDataSet.FindField(Column.FieldName);
        if (Field <> nil) and (Field.DataType = ftBoolean) then
        begin
          //todo: implement toggling through single click
          //todo: is necessary also to block edit through another ways (F2)
          if aoEditOnDblClick in fDBOptions.AdvOptions then
            DoCancelEdit;
          LinkedDataSet.Edit;
          Field.AsBoolean := not Field.AsBoolean;
          LinkedDataSet.Post;
        end;
      end;
    end;
    //fire extended dblclick event
    Data := InternalGetNodeData(HitInfo.HitNode);
    if IsDataOk(Data) then
      DoRecordDblClick(HitInfo.HitColumn, Data^.RecordData);
  end;
end;

procedure TCustomVirtualDBGrid.AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; out NextNonEmpty: TColumnIndex);
begin
  inherited;
  if (PaintInfo.Column <= NoColumn) then exit;

  with PaintInfo do
  begin
     if TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType = ctIndicator
     then begin
       Exclude(PaintOptions, poDrawSelection);
     end;
  end;
end;

procedure TCustomVirtualDBGrid.DoGetRecordCount(var RecordCount: LongInt);
begin
  if Assigned(fOnGetRecordCount) then
    fOnGetRecordCount(Self, RecordCount);
end;


procedure TCustomVirtualDBGrid.DoCalculateValue(const IDText: string;
            Column: TColumnIndex; RecordData: TRecordData; RowIndex: Cardinal;
            var CalculatedValue: String; var CalculatedValueType: TFieldType);
begin
  if Assigned(fOnCalculateValue) then
     fOnCalculateValue(Self, IDText, Column, RecordData, RowIndex,
                       CalculatedValue, CalculatedValueType);
end;



procedure TCustomVirtualDBGrid.DoFormatFieldValue(Column: TColumnIndex;
             RecordData: TRecordData; RowIndex: Cardinal; Field: TField;
             var FieldValue: Variant);
begin
  if Assigned(fOnFormatFieldValue) then
    fOnFormatFieldValue(Self, Column, RecordData, RowIndex, Field, FieldValue);
end;


procedure TCustomVirtualDBGrid.DoLoadRecord(RecordData: TRecordData;
     RowIndex: Cardinal);
begin
  if Assigned(fOnLoadRecord) then
     fOnLoadRecord(Self, RecordData, RowIndex);
end;


procedure TCustomVirtualDBGrid.DoCustomSort(Column: TColumnIndex;
         ColumnType: TColumnType; const SortBy: string; SortDirection: TSortDirection;
         var RefreshGrid: boolean);
begin
  if Assigned(fOnCustomSort) then
     fOnCustomSort(Self, Column, ColumnType, SortBy, SortDirection, RefreshGrid);
end;

procedure TCustomVirtualDBGrid.DoPostChanges(const FieldNameOrIDText: string;
     Column: TcolumnIndex; ColumnType: TColumnType; RecordData: TRecordData;
     RowIndex: Cardinal; var NewValue: String; var PostChanges: boolean);
begin
  if Assigned(fOnPostChanges)
     then fOnPostChanges(Self, FieldNameOrIDText, Column, ColumnType, RecordData,
                         RowIndex, NewValue, PostChanges)
     else PostChanges:= true;
end;

procedure TCustomVirtualDBGrid.DoScroll(DeltaX, DeltaY: Integer);
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'DoScroll');{$endif}
  //todo: elaborate an algorithm to update only the scrolled nodes ??
  if DeltaY <> 0 then
     UpdateVisibleDBTree(False);
  inherited DoScroll(DeltaX, DeltaY);
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'DoScroll');{$endif}
end;


procedure TCustomVirtualDBGrid.DoChangeSort(SortColumn: TColumnIndex;
        SortDirection: TSortDirection); 
begin
  if Assigned(fOnChangeSort) then
     fOnChangeSort(Self, SortColumn, SortDirection);
end;

procedure TCustomVirtualDBGrid.DoRecordDblClick(Column: TColumnIndex;
  RecordData: TRecordData);
begin
  if Assigned(FOnRecordDblClick) then
    FOnRecordDblClick(Self, Column, RecordData);
end;


function TCustomVirtualDBGrid.GetRecordCount: longint;
begin
  Result := 0;
  if DBOptions.RecordCountType = rcFromDataset then
  begin
    if Assigned(LinkedDataSet) and LinkedDataSet.Active then
      Result := LinkedDataSet.RecordCount;
  end
  else
    DoGetRecordCount(Result);
end;

function TCustomVirtualDBGrid.FindNodeByRecNo(ARecNo: LongInt): PVirtualNode;
var
  Node: PVirtualNode;
  Data: PNodeData;
begin
  Result := nil;

  Node := GetFirst;
  while Node <> nil do
  begin
    Data := InternalGetNodeData(Node);

    if Data <> nil then
    begin
      if Data^.RecNo = ARecNo then
      begin
        Result := Node;
        Break;
      end;
    end;
    Node := GetNext(Node);
  end;
end;

function TCustomVirtualDBGrid.SetFocusToNode(Node: PVirtualNode; Center: Boolean): Boolean;
begin
  Result := Assigned(Node);
  if not Result then
    Exit;

  if Assigned(FocusedNode) then
    Selected[FocusedNode] := False;

  //call DoFocusNode instead of FocusedNode to avoid calling DoFocusChange
  DoFocusNode(Node, False);
  //don't set Selected if Handle is not allocated to avoid premature handle creation
  if HandleAllocated then
    Selected[Node] := True;
  //currently calling fully visible is not necessary since there's no parent node
  //FullyVisible[Node] := True;
  //todo: change ScrollIntoView behavior?
  ScrollIntoView(Node, Center);
end;

procedure TCustomVirtualDBGrid.GotoRecNo(NewRecNo: longint);
begin
  //Assumes LinkedDataset is <> nil and active
  IncLoadingDataFlag;
  try
    //todo: use LinkedDataset.RecNo directly??
    //Maybe not since some datasets don't support setting RecNo
    LinkedDataSet.MoveBy(NewRecNo - LinkedDataSet.RecNo);
  finally
    DecLoadingDataFlag;
  end;
end;

function TCustomVirtualDBGrid.GetNodeByIndex(Index: Cardinal): PVirtualNode;
begin
  result:= GetFirst;
  while Assigned(result) and (result.Index <> Index) do
     result:= GetNextSibling(result);

  if (result.Index <> Index) then
     result:= nil; 
end;

function TCustomVirtualDBGrid.GetSelectedRecord(Index: Integer): TRecordData;
var
   Run, Node: PVirtualNode;
   Data: PNodeData;
   I:    Integer;
begin
  Result:= nil;

  if (Index > -1) and (Index < SelectedCount)
     then begin
        Node:= nil;
        Run:= GetFirstSelected;
        I:= 0;
        while Assigned(Run) and (I < SelectedCount) do
        begin
           if (I = Index) then
           begin
             Node:= Run;
             break;
           end;

           Run:= GetNextSelected(Run);
           Inc(I);
        end;

        if Assigned(Node) then
        begin
           Data := InternalGetNodeData(Node);
           if Data <> nil then
             Result := Data.RecordData;
        end;
     end;
end;

function TCustomVirtualDBGrid.AddColumn(AColumnType: TColumnType; const AFieldName, ACaption: string;
  AWidth: Integer=-1): TVirtualDBTreeColumn;
begin
  Result := TVirtualDBTreeColumn(Header.Columns.Add);
  Result.ColumnType := AColumnType;
  Result.Text := ACaption;

  if AColumnType = ctDBField then
     Result.FieldName := AFieldName;

  if AWidth <> -1 then
    Result.Width := AWidth;
end;

procedure TCustomVirtualDBGrid.IncLoadingDataFlag;
begin
   Inc(fLoadingDataFlag);
end;

procedure TCustomVirtualDBGrid.DecLoadingDataFlag;
begin
   Dec(fLoadingDataFlag);
end;

function TCustomVirtualDBGrid.IsDataLoading: boolean;
begin
  Result:= (fLoadingDataFlag <> 0);
end;


procedure TCustomVirtualDBGrid.AddDBColumn(const AFieldName, ACaption: string; AWidth: Integer=-1);
begin
  AddColumn(ctDBField, AFieldName, ACaption, AWidth);
end;

procedure TCustomVirtualDBGrid.AddCalcColumn(const IDText: string; AWidth: Integer);
begin
  AddColumn(ctCalculated, '', IDText, AWidth);
end;

procedure TCustomVirtualDBGrid.AddIndicatorColumn(AWidth: Integer);
begin
  AddColumn(ctIndicator, '', '', AWidth);
end;

function TCustomVirtualDBGrid.GetNodeRecordData(Node: PVirtualNode): TRecordData;
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(Node);
  if Data <> nil then
    Result := Data^.RecordData
  else
    Result := nil;
end;

procedure TCustomVirtualDBGrid.SetSortColumn(const ColumnTitle: string; Direction: TSortDirection);

var I  : integer;
begin
  for I:= 0 to Header.Columns.Count-1 do
  begin
     if (UpperCase(Header.Columns[I].Text) = UpperCase(ColumnTitle)) then
     begin
        DoSortColumn(I, Integer(Direction));
        break;
     end;
  end;
end;


procedure TCustomVirtualDBGrid.SetDBOptions(const Value: TVTDBOptions);
begin
  FDBOptions.Assign(Value);
end;



function TCustomVirtualDBGrid.Navigate(FromPosition: TNavigateFromPosition; Delta: Longint): boolean;
var
   Node: PVirtualNode;
   Count: Longint;
   Max: Longint;
begin
  result:= false;

  case FromPosition of
       nfpBegin:
          Node:= GetFirstVisible;

       nfpCurrent:
          Node:= FocusedNode;

       nfpEnd:
          Node:= GetLastVisible;
  end;

  if (Delta <> 0) then
  begin
    Count:= 0;
    Max:= Abs(Delta);
    while (Node <> nil)and(Count < Max) do
    begin
      Inc(Count);
      if (Delta > 0)
         then Node:= GetNextVisibleSibling(Node)
         else Node:= GetPreviousVisibleSibling(Node);
    end;
  end;

  Result := SetFocusToNode(Node, false);
end;


function TCustomVirtualDBGrid.InternalGetNodeData(Node: PVirtualNode): PNodeData;
begin
  if (Node <> nil) and (Node <> RootNode) then
    Result := PNodeData(PByte(Node) + FInternalDataOffset)
  else
    Result := nil;
end;

function TCustomVirtualDBGrid.IsDataOk(AData: PNodeData): Boolean;
begin
  Result := Assigned(AData) and Assigned(AData.RecordData);
end;

procedure TCustomVirtualDBGrid.InternalInitializeDBTree;
begin
  Clear;
  // Set Nodes count equals to database records count
  RootNodeCount := FRecordCount;
end;

procedure TCustomVirtualDBGrid.UpdateVisibleDBTree(AlwaysUpdate: Boolean;
  UpdateLoadedData: Boolean);
begin
  if not Assigned(LinkedDataSet) or not LinkedDataSet.Active or IsDataLoading then
    Exit;
  UpdateDBTree(TopNode, GetVisibleNodesCount, AlwaysUpdate, UpdateLoadedData);
end;

procedure TCustomVirtualDBGrid.ReInitializeDBGrid;
var
  OldOffsetXY: TPoint;
begin
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'ReInitializeDBGrid');{$endif}
  OldOffsetXY := OffsetXY;

  BeginUpdate;

  // Initialize database tree
  InternalInitializeDBTree;

  // Set back offset X & Y
  OffsetXY := OldOffsetXY;

  // Update database tree
  UpdateVisibleDBTree(True);

  EndUpdate;
  // Set focus
  //Focus must be set outside Begin/EndUpdate calls as a workaround to a VTV bug
  SetFocusToActualRecNo;
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'ReInitializeDBGrid');{$endif}
end;


procedure TCustomVirtualDBGrid.SetFocusToActualRecNo;
var
  WRecNo: LongInt;
begin
  if not (csLoading in ComponentState) and not IsDataLoading then
  begin
    WRecNo := GetCurrentDBRecNo;
    if WRecNo <> 0 then
      SetFocusToNode(FindNodeByRecNo(WRecNo), False);
  end;
end;

procedure TCustomVirtualDBGrid.UpdateCurrentRecord;
var
  Node: PVirtualNode;
begin
  // Assume LinkedDataset <> nil
  Node := FindNodeByRecNo(LinkedDataSet.RecNo);
  if Node <> nil then
  begin
    LoadRecordData(Node, True);
    InvalidateNode(Node);
  end;
end;

procedure TCustomVirtualDBGrid.UpdateAllRecords(UpdateLoadedData: Boolean = True);
begin
  if not Assigned(LinkedDataSet) or not LinkedDataSet.Active or IsDataLoading then
    Exit;
  BeginUpdate;
  try
    UpdateDBTree(GetFirst, VisibleCount, False, UpdateLoadedData);
  finally
    EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.UpdateDBTree(StartNode: PVirtualNode; NodeCount: Cardinal;
  AlwaysUpdate: Boolean; UpdateLoadedData: Boolean);
var
  Count: Cardinal;

  OldRecNo,
  DatasetRecNo,
  NodeRecNo,
  MoveCount: LongInt;

  Run: PVirtualNode;

  DoLoad, DoCreateData : Boolean;
begin
  //it's up to the caller check for LinkedDataset and IsDataLoading
  {$ifdef DEBUG_VDBGRID}Logger.EnterMethod(lcAll, 'UpdateDBTree');{$endif}
  {$ifdef DEBUG_VDBGRID}Logger.Send(lcAll, 'NodeCount: %d AlwaysUpdate: %s UpdateLoadedData: %s',
    [NodeCount, BoolToStr(AlwaysUpdate, True), BoolToStr(UpdateLoadedData, True)]);{$endif}
  Run := StartNode;
  if not Assigned(Run) then
    Exit;
    
  IncLoadingDataFlag;
  try
    LinkedDataSet.CheckBrowseMode;
    LinkedDataSet.DisableControls;
    DatasetRecNo := LinkedDataSet.RecNo;
    OldRecNo := DatasetRecNo;

    DoLoad := AlwaysUpdate;
    DoCreateData := AlwaysUpdate;
    Count := 0;
    while Assigned(Run) and (Count <= NodeCount) do
    begin
      // Initialize node to ensure RecNo is set
      if not (vsInitialized in Run^.States) then
        InitNode(Run);
      NodeRecNo := InternalGetNodeData(Run)^.RecNo;
      // If we dont want always update data, then we must test that
      // if node has data created, and if not than we can load data from database
      // to node's data
      if not AlwaysUpdate then
      begin
        DoCreateData := not IsDataCreated(Run);
        DoLoad := DoCreateData or UpdateLoadedData;
      end;

      if DoLoad then
      begin
        //Move to the RecNo pointed by the run node
        MoveCount := LinkedDataSet.MoveBy(NodeRecNo - DatasetRecNo);
        //Update the DatasetRecNo
        Inc(DatasetRecNo, MoveCount);
        {$ifdef DEBUG_VDBGRID}Logger.Send(lcAll, 'LoadingData', Run.Index);{$endif}
        LoadRecordData(Run, DoCreateData); // load data from database
      end;

      Inc(Count);
      Run := GetNextSibling(Run);
    end;
    if (LinkedDataSet.RecNo <> OldRecNo) then
      GotoRecNo(OldRecNo);
  finally
    LinkedDataSet.EnableControls;
    DecLoadingDataFlag;
  end;
  {$ifdef DEBUG_VDBGRID}Logger.ExitMethod(lcAll, 'UpdateDBTree');{$endif}
end;


function TCustomVirtualDBGrid.IsDataCreated(ANode: PVirtualNode): boolean;
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(ANode);
  Result := IsDataOk(Data);
end;

procedure TCustomVirtualDBGrid.LoadRecordData(ANode: PVirtualNode; CreateData: Boolean);
var
    I, Idx, ColIdx: Integer;
    WFieldName:     String;
    WIDText:        string;
    WField:         TField;
    WFieldValue:    Variant;
    WCalcValue:     String;
    WCalcType:      TFieldType;
    ColType:        TColumnType;
    Data:           PNodeData;
    RecordNo:       longint;

    CalculatedColumns: TIntegerList;
begin
  Data := InternalGetNodeData(ANode);
  if Data = nil then
    Exit;
  if Data^.RecordData = nil then
  begin
    // If AlwaysUpdate is false then we dont want to reload existing values from database
    if not CreateData then
      Exit;
    // If RecordData is nil then create it
    Data.RecordData := GetRecordDataClass.Create;
    // necessary to avoid memory leaks when scrolling to fast
    if not (vsInitialized in ANode^.States) then
      InitNode(ANode);
  end;

  // CalculatedColumns to archive calculated column indexes. Load on demand.
  CalculatedColumns := nil;
  try
    RecordNo := LinkedDataSet.RecNo;
    // If current record number is other than -1 then setup RecordData.RecNo
    if (RecordNo <> -1) then
      Data.RecordData.RecNo := RecordNo;

    // Cycle for columns and load data from database and store to Node data
    for I := 0 to Header.Columns.Count - 1 do
    begin
      ColType := TVirtualDBTreeColumn(Header.Columns[I]).ColumnType;

      case ColType of
        ctDBField:
        begin
          WFieldName := TVirtualDBTreeColumn(Header.Columns[I]).FieldName;
          if RecordNo <> -1 then
            WField := LinkedDataSet.FindField(WFieldName)
          else
            WField := nil;

          Idx := Data.RecordData.IndexOf(WFieldName, ffDBField);

          if WField <> nil then
          begin
            WFieldValue := WField.Value;
            DoFormatFieldValue(Header.Columns[i].Index, Data.RecordData,
              ANode.Index, WField, WFieldValue);

            if Idx = -1 then
              Data.RecordData.Add(WFieldName, WFieldValue, WField.DataType, ffDBField)
            else
            begin
               if Idx <> I then
                 Data.RecordData.Exchange(Idx, I);
               Data.RecordData.Edit(WFieldName, ffDBField, WFieldValue, WField.DataType);
            end;
          end
          else
          begin // if field doesnt exists than add empty values
            if Idx = -1 then
              Data.RecordData.Add(WFieldName, Null, ftUnknown, ffDBField)
            else
            begin
              if Idx <> I then
                Data.RecordData.Exchange(Idx, I);
              Data.RecordData.Edit(WFieldName, ffDBField, Null);
            end;
          end;
        end;

        ctCalculated:
        begin
          if CalculatedColumns = nil then
            CalculatedColumns := TIntegerList.Create;
          CalculatedColumns.Add(I);
        end;

        ctIndicator:
        begin
          Data.RecordData.Add('', '', ftUnknown, ffIndicator);
        end;
      end;
    end;

    // fillup calculated columns
    if CalculatedColumns <> nil then
    begin
      for I := 0 to CalculatedColumns.Count - 1 do
      begin
        ColIdx := CalculatedColumns[I];
        WCalcType := ftString;
        WIDText := TVirtualDBTreeColumn(Header.Columns[ColIdx]).Text;
        Idx := Data.RecordData.IndexOf(WIDText, ffCalculated);

        WCalcValue := '';
        DoCalculateValue(Header.Columns[ColIdx].Text, Header.Columns[ColIdx].Index,
          Data.RecordData, ANode.Index, WCalcValue, WCalcType);
        if (Idx = -1)
           then Data.RecordData.Insert(ColIdx, WIDText, WCalcValue, WCalcType, ffCalculated)
           else begin
             if (Idx <> ColIdx) then Data.RecordData.Exchange(Idx, ColIdx);
             Data.RecordData.Edit(WIDText, ffCalculated, WCalcValue);
           end;
      end;
    end;
    // Fireup OnLoadRecord event
    DoLoadRecord(Data.RecordData, ANode.Index);
  finally
    CalculatedColumns.Free;
  end;
end;

procedure TCustomVirtualDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateVisibleDBTree(False);
end;

function TCustomVirtualDBGrid.GetVisibleNodesCount: Cardinal;
var
  Node: PVirtualNode;
  AHeight, AControlHeight: Integer;
begin
  //todo: see why this is called twice at startup
  Result := 0;
  AControlHeight := ClientHeight;
  //TopNode
  Node := GetNodeAt(0, 0, True, AHeight);
  while Node <> nil do
  begin
    Inc(AHeight, Node.NodeHeight);
    Inc(Result);
    if AHeight >= AControlHeight then
      Break;
    Node := GetNextVisibleSibling(Node);
  end;
end;

procedure TCustomVirtualDBGrid.DoSortColumn(AColumn: TColumnIndex; ASortDirection: Integer= -1);
var
   sDirection,
   OldSortDirection: TSortDirection;
   OldCursor:        TCursor;
   OldSortColumn:    TColumnIndex;
   SortBy:           string;
   RefreshGrid:      boolean;
   ColumnType:       TColumnType;
begin
  if (AColumn > NoColumn) and (AColumn < Header.Columns.Count) then
  begin
    ColumnType:= TVirtualDBTreeColumn(Header.Columns[AColumn]).ColumnType;

    if (ColumnType <> ctIndicator) and
       (
        ((ColumnType = ctDBField) and (aoSortDBFieldColumns in DBOptions.AdvOptions))
        or
        ((ColumnType = ctCalculated) and (aoSortCalculatedColumns in DBOptions.AdvOptions))
       )
    then begin
      OldSortDirection:= Header.SortDirection;
      OldSortColumn:=    Header.SortColumn;

      //OldSortColumn:= Header.SortColumn;
      // if autodetect sortdirection than ...
      sDirection:= sdAscending;
      // if some sort direction was set than get true sortdirection type
      if (ASortDirection > -1) then
      begin
         if (ASortDirection >= Byte(sdAscending)) and
            (ASortDirection <= Byte(sdDescending))
            then sDirection:= TSortDirection(ASortDirection);
      end;

         if (AColumn = Header.SortColumn) then
         begin

            // Do only if autodetect sortdirection was enabled(setting value -1 to ASortDirection)
            if (ASortDirection = -1) then
              if (Header.SortDirection = sdAscending)
                 then sDirection:= sdDescending
                 else sDirection:= sdAscending;
         end
         else Header.SortColumn:= AColumn;

      // If current sort options are the same as the following one then exit
      if (OldSortDirection = sDirection) and
         (OldSortColumn = AColumn) then exit;


      try
         // Set HourGlass cursor if we want that
         if (aoHourGlassCursor in DBOptions.AdvOptions)
            then begin
              OldCursor:= Screen.Cursor;
              Screen.Cursor:= crHourGlass;
            end;

         // Sort the tree
         Header.SortDirection:= sDirection;


         if (DBOptions.SortingType = stBuildIn)or(ColumnType = ctCalculated) // Buildin sorting
         then begin
             // Loads all record if there wasn't sorted the tree yet
             // We must have all data to use sorting, that it can be very slow
             // to load all data from database. Than it is recommended to do not use
             // autosort feature. Instead of this, sort the database by your way, and
             // then tell VirtualDBGrid to reload data(func. ReInitializeDBGrid) to see changes...
             //if (OldSortColumn <= NoColumn) then
             UpdateAllRecords(False);

             SortTree(Header.SortColumn, Header.SortDirection, False);
         end
         else begin // User sorting
           RefreshGrid:= true;
           SortBy:= '';
           case ColumnType of
               ctDBField:
                  SortBy:= TVirtualDBTreeColumn(Header.Columns[AColumn]).FieldName;

               {ctCalculated:
                  SortBy:= TVirtualDBTreeColumn(Header.Columns[AColumn]).Text;}
           end;

           DoCustomSort(AColumn, ColumnType, SortBy, sDirection, RefreshGrid);

           if (RefreshGrid) then ReInitializeDBGrid;
         end;

      finally
         // Set back old cursor
         if (aoHourGlassCursor in DBOptions.AdvOptions)
            then Screen.Cursor:= OldCursor;
      end;

    end;
  end;
end;

function TCustomVirtualDBGrid.GetRecordDataClass: TRecordDataClass;
begin
  Result := TRecordData;
end;

function TCustomVirtualDBGrid.GetCurrentDBRecNo: LongInt;
begin
  if Assigned(LinkedDataSet) and LinkedDataSet.Active then
    Result := LinkedDataSet.RecNo
  else
    Result := 0;
end;

{ --- TCustomVirtualDBGrid --------------------------------------------------- }
{ ============================================================================ }

end.
