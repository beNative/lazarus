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

unit SnippetSource.Modules.Data;

{$MODE DELPHI}

interface

{$REGION 'documentation'}
{
  Documentation that lead to the final implementation:
    http://lists.lazarus-ide.org/pipermail/lazarus/2014-November/154238.html
    http://forum.lazarus-ide.org/index.php/topic,31362.0.html
    http://free-pascal-lazarus.989080.n3.nabble.com/Lazarus-TSQLQuery-Getting-autoincremented-ID-value-after-insert-MySQL-td4039238i20.html

  Some things to know in regard of SQLite (excerpt from SQLite documentation):

  - The AUTOINCREMENT keyword imposes extra CPU, memory, disk space, and disk
    I/O overhead and should be avoided if not strictly needed. It is usually
    not needed.
  - In SQLite, a column with type INTEGER PRIMARY KEY is an alias for the ROWID
    (except in WITHOUT ROWID tables) which is always a 64-bit signed integer.
  - On an INSERT, if the ROWID or INTEGER PRIMARY KEY column is not explicitly
    given a value, then it will be filled automatically with an unused integer,
    usually one more than the largest ROWID currently in use. This is true
    regardless of whether or not the AUTOINCREMENT keyword is used.
  - If the AUTOINCREMENT keyword appears after INTEGER PRIMARY KEY, that changes
    the automatic ROWID assignment algorithm to prevent the reuse of ROWIDs over
    the lifetime of the database. In other words, the purpose of AUTOINCREMENT
    is to prevent the reuse of ROWIDs from previously deleted rows.

    These settings are used on the qrySnippet component that is used for
    editing:
      sqoAutoApplyUpdates
      sqoAutoCommit

    => This means that after every post on the dataset the following statements
       are automatically executed in this order:
         qrySnippet.ApplyUpdates;
         trsMain.Commit;

       sqoKeepOpenOnCommit
         By default the dataset is closed after executing Commit. This flag
         prevents this behaviour and does not require the need to execute the
         query again to fetch all data after each change is posted.

    Settings on trsMain (TSQLTransaction)
       stoUseImplicit
         This means that the database opens a transaction long enough to
         complete your query and then then closes it automatically. By setting
         this the connection also doesn't start automatically a new transaction.

    For bulk inserts the sqoAutoApplyUpdates and sqoAutoCommit have a great
    impact on performance. To avoid these operations for each insert this needs
    to be disabled and ApplyUpdates and Commit need to be called manually when
    all records are inserted.

  ISSUES
   - RefreshSQL gets called after every insert or update regardless of the
     specified provider flags on the key field (pfRefreshOnInsert,
     pfRefreshOnUpdate) or whether sqoRefreshUsingSelect is specified in the
     query's options.

     You cannot use Blob fields as calculated or lookup fields.

   RTF text
     - RTF text is stored in Base64 encoding.
       To make the RTF text searchable a copy with only the flat text is stored.
}
{$ENDREGION}

uses
  Windows,
  Classes, SysUtils, Controls, Graphics,
  DB, SQLScript,

  fgl,

  SQLDB, SQLite3Conn,

  ts.Core.Logger,

  SnippetSource.Interfaces;

type
  TImageMap = TFPGMapObject<Integer, TBitmap>;

type
  TdmSnippetSource = class(TDataModule,
    ISQLite, IConnection, ISnippet, IDataSet, ISearch, IGlyphs, IHighlighters,
    IQuery
  )
    {$REGION 'designer controls'}
    conMain          : TSQLite3Connection;
    imlGlyphs        : TImageList;
    imlNodeTypes     : TImageList;
    qryGlyph         : TSQLQuery;
    qryHighlighter   : TSQLQuery;
    qrySearch        : TSQLQuery;
    qryNodeType      : TSQLQuery;
    qryQuery         : TSQLQuery;
    qrySnippet       : TSQLQuery;
    scrCreateIndexes : TSQLScript;
    scrCreateTables  : TSQLScript;
    scrInsertData    : TSQLScript;
    trsMain          : TSQLTransaction;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure qryGlyphBeforePost(DataSet: TDataSet);
    procedure qryGlyphNewRecord(DataSet: TDataSet);
    procedure qrySnippetAfterOpen(ADataSet: TDataSet);
    procedure qrySnippetAfterPost(ADataSet: TDataSet);
    procedure qrySnippetAfterRefresh(ADataSet: TDataSet);
    procedure qrySnippetBeforeOpen(ADataSet: TDataSet);
    procedure qrySnippetBeforePost(ADataSet: TDataSet);
    procedure qrySnippetBeforeRefresh(DataSet: TDataSet);
    procedure qrySnippetBeforeScroll(ADataSet: TDataSet);
    procedure qrySnippetNewRecord(ADataSet: TDataSet);

    procedure FSettingsChange(Sender: TObject);
    {$ENDREGION}

  private
    FSettings        : ISettings;
    FBulkInsertMode  : Boolean;
    FHLImages        : TImageMap;
    FFocusedId       : Int64;
    FLastActiveViews : string;
    FImage           : TBitmap;

    {$REGION 'property access mehods'}
    function GetActive: Boolean;
    function GetActiveViews: string;
    function GetAutoApplyUpdates: Boolean;
    function GetAutoCommit: Boolean;
    function GetHtmlData: string;
    function GetHtmlText: string;
    function GetImage: TBitmap;
    function GetLocked: Boolean;
    function GetRtfText: string;
    function GetRtfData: string;
    function GetDataSet: TSQLQuery;
    function GetDateCreated: TDateTime;
    function GetDateModified: TDateTime;
    function GetSequence: Integer;
    function GetVersion: string;
    function GetFileName: string;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetGlyphDataSet: TDataSet;
    function GetGlyphList: TImageList;
    function GetHighlighter: string;
    function GetHighlighterDataSet: TDataSet;
    function GetId: Integer;
    function GetImageIndex: Integer;
    function GetImageList: TImageList;
    function GetSearchDataSet: TSQLQuery;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeId: Integer;
    function GetParentId: Integer;
    function GetQuery: TSQLQuery;
    function GetRecordCount: Integer;
    function GetSize: Int64;
    function GetSource: string;
    function GetText: string;
    procedure SetActive(AValue: Boolean);
    procedure SetActiveViews(AValue: string);
    procedure SetAutoApplyUpdates(AValue: Boolean);
    procedure SetAutoCommit(AValue: Boolean);
    procedure SetHtmlData(AValue: string);
    procedure SetHtmlText(AValue: string);
    procedure SetImage(AValue: TBitmap);
    procedure SetLocked(AValue: Boolean);
    procedure SetRtfText(AValue: string);
    procedure SetRtfData(AValue: string);
    procedure SetDateCreated(AValue: TDateTime);
    procedure SetDateModified(AValue: TDateTime);
    procedure SetFileName(AValue: string);
    procedure SetFoldLevel(AValue: Integer);
    procedure SetFoldState(AValue: string);
    procedure SetHighlighter(AValue: string);
    procedure SetImageIndex(AValue: Integer);
    procedure SetNodeName(AValue: string);
    procedure SetNodePath(AValue: string);
    procedure SetNodeTypeId(AValue: Integer);
    procedure SetParentId(AValue: Integer);
    procedure SetSequence(AValue: Integer);
    procedure SetSource(AValue: string);
    procedure SetText(AValue: string);
    {$ENDREGION}

  protected
    procedure CreateLookupFields;

    procedure EnsureSqliteExists;
    procedure EnsureWebView2LoaderExists;

    procedure FillImageMapFromDataSet(
      AImageMap : TImageMap;
      ADataSet  : TDataSet
    );
    procedure FillNodeTypesImageList;
    procedure FillImageMaps;

    procedure InitField(AField: TField);
    procedure InitFields(ADataSet: TDataSet);

    {$REGION 'ISearch'}
    procedure Search(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );
    {$ENDREGION}

    {$REGION 'ISQLite'}
    function IntegrityCheck: Boolean;
    procedure ShrinkMemory;
    procedure Vacuum;
    {$ENDREGION}

    {$REGION 'IConnection'}
    procedure BeginBulkInserts;
    procedure EndBulkInserts;

    procedure ConnectToDatabase(const AFileName: string);
    function BackupDatabase: string;
    procedure CreateNewDatabase;
    procedure CreateDatabaseTables;
    procedure CreateDatabaseIndexes;

    procedure SetupConfigurationData;

    procedure ExecuteDirect(const ASQL: string);

    function ApplyUpdates: Boolean;
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
    procedure EndTransaction;
    {$ENDREGION}

    {$REGION 'IQuery'}
    procedure Execute(const ASQL: string);
    function LastId: Integer;
    function NewSequence: Integer;
    function QueryValue(const ASQL: string): Variant;

    property Query: TSQLQuery
      read GetQuery;
    {$ENDREGION}

    {$REGION 'IDataSet'}
    function Post: Boolean;
    function Edit: Boolean;
    function Append: Boolean;
    procedure DuplicateRecords(AValues: TStrings);
    procedure MoveDownRecords(AValues: TStrings);
    procedure MoveUpRecords(AValues: TStrings);
    procedure UpdateNodePaths;
    procedure UpdateSequences;

    procedure EnableControls;
    procedure DisableControls;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : ISettings
    ); reintroduce; virtual;
    destructor Destroy; override;

    {$REGION 'ISearch'}
    property SearchDataSet: TSQLQuery
      read GetSearchDataSet;
    {$ENDREGION}

    {$REGION 'ISQLite'}
    { Database size in bytes. }
    property Size: Int64
      read GetSize;

    { Version of the SQLite database. }
    property Version: string
      read GetVersion;
    {$ENDREGION}

    {$REGION 'IConnection'}
    property AutoApplyUpdates: Boolean
      read GetAutoApplyUpdates write SetAutoApplyUpdates;

    property AutoCommit: Boolean
      read GetAutoCommit write SetAutoCommit;

    { SQLite database file. }
    property FileName: string
      read GetFileName write SetFileName;
    {$ENDREGION}

    {$REGION 'ISnippet'}
    { Last active view(s) for the current record. }
    property ActiveViews: string
      read GetActiveViews write SetActiveViews;

    { When locked, the record cannot be edited. }
    property Locked: Boolean
      read GetLocked write SetLocked;

    { Textual representation of the RTF content. This is used for text
      searching. }
    property RtfText: string
      read GetRtfText write SetRtfText;

    { Field containing the RTF content of the node's comment. }
    property RtfData: string
      read GetRtfData write SetRtfData;

    property HtmlText: string
      read GetHtmlText write SetHtmlText;

    property HtmlData: string
      read GetHtmlData write SetHtmlData;

    property Source: string
      read GetSource write SetSource;

    { DateTime of creation of the current record. }
    property DateCreated: TDateTime
      read GetDateCreated write SetDateCreated;

    { DateTime of last modificaton to current record. }
    property DateModified: TDateTime
      read GetDateModified write SetDateModified;

    { Currently not used. }
    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    { Currently not used. }
    property FoldState: string
      read GetFoldState write SetFoldState;

    { Highlighter code corresponding to the highlighter to select for the
      editor view. }
    property Highlighter: string
      read GetHighlighter write SetHighlighter;

    { Primary key field. }
    property Id: Integer
      read GetId;

    property NodeName: string
      read GetNodeName write SetNodeName;

    { Currently not used. }
    property NodePath: string
      read GetNodePath write SetNodePath;

    { Reference to NodeType.Id }
    property NodeTypeId: Integer
      read GetNodeTypeId write SetNodeTypeId;

    { Reference to Snippet.Id }
    property ParentId: Integer
      read GetParentId write SetParentId;

    property Sequence: Integer
      read GetSequence write SetSequence;

    { Flat text shown in the editor view. }
    property Text: string
      read GetText write SetText;

    { Corresponds to the index in the stock image list and is not a foreign
      key. }
    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Image: TBitmap
      read GetImage write SetImage;
    {$ENDREGION}

    {$REGION 'IDataSet'}
    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TSQLQuery
      read GetDataSet;
    {$ENDREGION}

    {$REGION 'IGlyphs'}
    property GlyphList: TImageList
      read GetGlyphList;

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property ImageList: TImageList
      read GetImageList;
    {$ENDREGION}

    {$REGION 'IHighLighters'}
    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;
    {$ENDREGION}
  end;

implementation

{$R *.lfm}

uses
  Variants, TypInfo, Zipper, FileUtil, LazFileUtils, LResources,

  SnippetSource.Resources;

{$REGION 'non-interfaced routines'}
function QueryLookup(AConnection: TSQLConnection; const AQuery: string;
const AParams: array of const) : Variant; overload;
var
  DS : TSQLQuery;
  I  : Integer;
begin
  if not Assigned(AConnection) then
    raise Exception.CreateFmt(SParameterNotAssigned, ['AConnection']);
  DS := TSQLQuery.Create(nil);
  try
    DS.SQLConnection  := AConnection;
    if Length(AParams) > 0 then
      DS.SQL.Text := Format(AQuery, AParams)
    else
      DS.SQL.Text := AQuery;
    try
      DS.Active := True;
    except
      raise Exception.CreateFmt(SQueryLookupErrorRunningQuery, [DS.SQL.Text]);
    end;
    if DS.IsEmpty then
      Result := Unassigned
    else if DS.RecordCount = 1 then
// Cannot use this because fieldname can differ from the one given in the select
//        Result := DS.FieldValues[AResultField]
    begin
      if DS.Fields.Count = 1 then
        Result := DS.Fields[0].Value
      else
      begin
        Result := VarArrayCreate([0, DS.Fields.Count], varVariant);
        for I := 0 to DS.Fields.Count - 1 do
          Result[I] := DS.Fields[I].Value
      end;
    end
    else
      raise Exception.CreateFmt(SQueryLookupTooManyRecords, [DS.SQL.Text])
  finally
    DS.Free;
  end;
end;

function QueryLookup(AConnection: TSQLConnection; const AQuery: string)
  : Variant; overload;
begin
  Result := QueryLookup(AConnection, AQuery, []);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TdmSnippetSource.Create(AOwner: TComponent; ASettings: ISettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  FSettings.AddOnChangeHandler(FSettingsChange);
end;

procedure TdmSnippetSource.AfterConstruction;
begin
  Logger.Enter(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FImage := TBitmap.Create;
  EnsureSqliteExists;
//  EnsureWebView2LoaderExists;
  FHLImages := TImageMap.Create(True);
  qrySnippet.UsePrimaryKeyAsKey := True;
  ConnectToDatabase(FSettings.Database);

  Logger.Leave(Self, 'AfterConstruction');
end;

destructor TdmSnippetSource.Destroy;
begin
  ApplyUpdates;
  Commit;
  DataSet.Active := False;
  conMain.Connected := False;
  FSettings := nil;
  FImage.Free;
  FHLImages.Free;
  inherited Destroy;
  Logger.Info('DM Destroyed');
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmSnippetSource.FSettingsChange(Sender: TObject);
begin
  ConnectToDatabase(FSettings.Database);
end;

{$REGION 'qryGlyph'}
procedure TdmSnippetSource.qryGlyphBeforePost(DataSet: TDataSet);
begin
//  DataSet.FieldByName('DateModified').AsDateTime := Now;
end;

procedure TdmSnippetSource.qryGlyphNewRecord(DataSet: TDataSet);
begin
//  DataSet.FieldByName('DateCreated').AsDateTime := Now;
end;
{$ENDREGION}

{$REGION 'qrySnippet'}
{
  This is the place where we create persistent fields.

  see https://stackoverflow.com/questions/9064162/how-to-create-a-tdataset-lookup-field-at-runtime

  If you want to use lookup fields in your dataset you need to create persistent
  fields.
  Persistent fields are created based on the fielddefs collection and need to be
  created BEFORE you open the dataset.
}

procedure TdmSnippetSource.qrySnippetBeforeOpen(ADataSet: TDataSet);
var
  I  : Integer;
  FD : TFieldDef = nil;
  F  : TField    = nil;
  DS : TSQLQuery;
begin
  Logger.Enter(Self, 'qrySnippetBeforeOpen');
  DS := ADataSet as TSQLQuery;
  // These two steps are required to get the FieldDefs of the PK initialized
  // correctly.
  DS.Prepare;
  DS.ServerIndexDefs.Update; // Required to create Id field as ftAutoInc.
  DS.Fields.Clear;
  DS.FieldDefs.Clear;
  // Required to force Update when switching to another database
  DS.FieldDefs.Updated := False;
  DS.FieldDefs.Update;
  for I := 0 to DS.FieldDefs.Count - 1 do
  begin
    FD := DS.FieldDefs[I];
    F  := FD.CreateField(DS);
    // below is just intended for diagnostic reasons to inspect fields @runtime
    F.Name := Format('fld%s%s', [DS.Name, F.FieldName]);
  end;
  CreateLookupFields;
  Logger.Leave(Self, 'qrySnippetBeforeOpen');
end;

{ Assigns DateModified value just before posting the record. }

procedure TdmSnippetSource.qrySnippetBeforePost(ADataSet: TDataSet);
begin
  DateModified := Now;
  if ADataSet.FieldByName('Sequence').IsNull then
    ADataSet.FieldByName('Sequence').AsInteger := NewSequence;
  FLastActiveViews := ActiveViews;
end;

procedure TdmSnippetSource.qrySnippetBeforeRefresh(DataSet: TDataSet);
begin
  FFocusedId := Id;
end;

procedure TdmSnippetSource.qrySnippetBeforeScroll(ADataSet: TDataSet);
begin
  if ADataSet.State in dsEditModes then
    ADataSet.Post;
end;

procedure TdmSnippetSource.qrySnippetAfterOpen(ADataSet: TDataSet);
begin
  InitFields(ADataSet);
end;

procedure TdmSnippetSource.qrySnippetAfterPost(ADataSet: TDataSet);
var
  LUpdateStatus : TUpdateStatus;
  LLastId       : Integer;
begin
  Logger.Enter(Self, 'qrySnippetAfterPost');
  if not FBulkInsertMode then
  begin
    LUpdateStatus := ADataSet.UpdateStatus;
    ApplyUpdates;
    Commit;
    if LUpdateStatus = usInserted then
    begin
      LLastId := (qrySnippet.DataBase as TSQLite3Connection).GetInsertID;
      ADataSet.DisableControls;
      ADataSet.Refresh;
      ADataSet.Locate('Id', LLastId, []);
      ADataSet.EnableControls;
    end;
  end;
  Logger.Leave(Self, 'qrySnippetAfterPost');
end;

procedure TdmSnippetSource.qrySnippetAfterRefresh(ADataSet: TDataSet);
begin
  ADataSet.Locate('Id', FFocusedId, []);
end;

procedure TdmSnippetSource.qrySnippetNewRecord(ADataSet: TDataSet);
begin
  Logger.Enter(Self, 'qrySnippetNewRecord');
  // forces new value for AutoInc field
  ADataSet.FieldByName('Id').Value                := 0;
  ADataSet.FieldByName('DateCreated').AsDateTime  := Now;
  ADataSet.FieldByName('ActiveViews').AsString    := FLastActiveViews;
  ADataSet.FieldByName('HighlighterId').AsInteger := 1;
  if ADataSet.FieldByName('NodeTypeId').AsInteger = 0 then
  begin
    ADataSet.FieldByName('NodeTypeId').AsInteger := 1;
    ADataSet.FieldByName('ImageIndex').AsInteger := 0;
  end;
  Logger.Leave(Self, 'qrySnippetNewRecord');
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'property access mehods'}
{$REGION 'ISnippet'}
function TdmSnippetSource.GetActiveViews: string;
begin
  Result := DataSet.FieldByName('ActiveViews').AsString;
end;

procedure TdmSnippetSource.SetActiveViews(AValue: string);
begin
  if Edit then
    DataSet.FieldValues['ActiveViews'] := AValue
end;

function TdmSnippetSource.GetHtmlData: string;
begin
  Result := DataSet.FieldByName('HtmlData').AsString;
end;

procedure TdmSnippetSource.SetHtmlData(AValue: string);
begin
  DataSet.FieldValues['HtmlData'] := AValue;
end;

function TdmSnippetSource.GetHtmlText: string;
begin
  Result := DataSet.FieldByName('HtmlText').AsString;
end;

function TdmSnippetSource.GetImage: TBitmap;
var
  LStream : TStream;
begin
  LStream := TMemoryStream.Create;
  try
    (DataSet.FieldByName('Image') as TBlobField).SaveToStream(LStream);
    FImage.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
  Result := FImage;
end;

function TdmSnippetSource.GetLocked: Boolean;
begin
  Result := DataSet.FieldByName('Locked').AsBoolean;
end;

procedure TdmSnippetSource.SetHtmlText(AValue: string);
begin
  DataSet.FieldValues['HtmlText'] := AValue;
end;

procedure TdmSnippetSource.SetImage(AValue: TBitmap);
var
  LStream : TStream;
begin
  if not AValue.Empty then
  begin
    FImage.Assign(AValue);
    LStream := TMemoryStream.Create;
    try
      FImage.SaveToStream(LStream);
      if Edit then
      begin
        (DataSet.FieldByName('Image') as TBlobField).LoadFromStream(LStream);
        Post;
      end;
    finally
      LStream.Free;
    end;
  end;
end;

procedure TdmSnippetSource.SetLocked(AValue: Boolean);
begin
  if Edit then
  begin
    DataSet.FieldByName('Locked').AsBoolean := AValue;
    if not Post then
      Exception.Create('Database error!');
  end
  else
    Exception.Create('Database error!');
end;

function TdmSnippetSource.GetSource: string;
begin
  if DataSet.FieldValues['Source'] <> Null then
    Result := DataSet.FieldValues['Source']
  else
    Result := '';
end;

function TdmSnippetSource.GetImageIndex: Integer;
begin
  if DataSet.FieldValues['ImageIndex'] <> Null then
    Result := DataSet.FieldValues['ImageIndex']
  else
    Result := 0;
end;

procedure TdmSnippetSource.SetImageIndex(AValue: Integer);
begin
  DataSet.FieldValues['ImageIndex'] := AValue;
end;

function TdmSnippetSource.GetDateCreated: TDateTime;
begin
  if DataSet.FieldValues['DateCreated'] <> Null then
    Result := DataSet.FieldValues['DateCreated']
  else
    Result := 0;
end;

procedure TdmSnippetSource.SetDateCreated(AValue: TDateTime);
begin
  DataSet.FieldValues['DateCreated'] := AValue;
end;

function TdmSnippetSource.GetDateModified: TDateTime;
begin
  if DataSet.FieldValues['DateModified'] <> Null then
    Result := DataSet.FieldValues['DateModified']
  else
    Result := 0;
end;

function TdmSnippetSource.GetSequence: Integer;
begin
  Result := DataSet.FieldByName('Sequence').AsInteger;
end;

procedure TdmSnippetSource.SetSequence(AValue: Integer);
begin
  DataSet.FieldValues['Sequence'] := AValue;
end;

procedure TdmSnippetSource.SetDateModified(AValue: TDateTime);
begin
  DataSet.FieldValues['DateModified'] := AValue;
end;

function TdmSnippetSource.GetRtfText: string;
begin
  Result := DataSet.FieldByName('RtfText').AsString;
end;

procedure TdmSnippetSource.SetRtfText(AValue: string);
begin
  DataSet.FieldValues['RtfText'] := AValue;
end;

function TdmSnippetSource.GetRtfData: string;
begin
  Result := DataSet.FieldByName('RtfData').AsString;
end;

procedure TdmSnippetSource.SetRtfData(AValue: string);
begin
  DataSet.FieldValues['RtfData'] := AValue;
end;

function TdmSnippetSource.GetFoldLevel: Integer;
begin
  Result := DataSet.FieldValues['FoldLevel'];
end;

procedure TdmSnippetSource.SetFoldLevel(AValue: Integer);
begin
  DataSet.FieldValues['FoldLevel'] := AValue;
end;

function TdmSnippetSource.GetFoldState: string;
begin
  Result := DataSet.FieldByName('FoldState').AsString;
end;

procedure TdmSnippetSource.SetFoldState(AValue: string);
begin
  DataSet.FieldValues['FoldState'] := AValue;
end;

function TdmSnippetSource.GetHighlighter: string;
begin
  Result := DataSet.FieldByName('Highlighter').AsString;
end;

procedure TdmSnippetSource.SetHighlighter(AValue: string);
var
  LId     : Variant;
  //MS      : TMemoryStream;
  //LKey    : Integer;
  //LBitmap : TBitmap;
begin
  if AValue <> Highlighter then
  begin
   if not qryHighlighter.Active then
      qryHighlighter.Active := True;
    LId := qryHighlighter.Lookup('Code', VarArrayOf([AValue]), 'Id');
    if VarIsNull(LId) then
      LId := 1;
    DataSet.FieldValues['HighlighterId'] := LId;

    //LKey := Integer(LId);
    //if FHLImages.TryGetData(LKey, LBitmap) then
    //begin
    //  MS := TMemoryStream.Create;
    //  try
    //    LBitmap.SaveToStream(MS);
    //    MS.Position := 0;
    //    (qrySnippet.FieldByName('Image') as TBlobField).Clear;
    //    (qrySnippet.FieldByName('Image') as TBlobField).LoadFromStream(MS);
    //  finally
    //    MS.Free;
    //  end;
    //end;
  end;
end;

procedure TdmSnippetSource.SetSource(AValue: string);
begin
  DataSet.FieldValues['Source'] := AValue;
end;

function TdmSnippetSource.GetId: Integer;
begin
  if VarIsNull(DataSet.FieldValues['Id']) then
    Result := 0
  else
    Result := DataSet.FieldValues['Id'];
end;

function TdmSnippetSource.GetNodeName: string;
begin
  Result := DataSet.FieldByName('NodeName').AsString;
end;

procedure TdmSnippetSource.SetNodeName(AValue: string);
begin
  DataSet.FieldValues['NodeName'] := AValue;
end;

function TdmSnippetSource.GetNodePath: string;
begin
  Result := DataSet.FieldByName('NodePath').AsString;
end;

procedure TdmSnippetSource.SetNodePath(AValue: string);
begin
  DataSet.FieldValues['NodePath'] := AValue;
end;

function TdmSnippetSource.GetNodeTypeId: Integer;
begin
  Result := DataSet.FieldValues['NodeTypeId'];
end;

procedure TdmSnippetSource.SetNodeTypeId(AValue: Integer);
begin
  DataSet.FieldValues['NodeTypeId'] := AValue;
end;

function TdmSnippetSource.GetParentId: Integer;
begin
  Result := DataSet.FieldByName('ParentId').AsInteger;
end;

procedure TdmSnippetSource.SetParentId(AValue: Integer);
begin
  DataSet.FieldValues['ParentId'] := AValue;
end;

function TdmSnippetSource.GetText: string;
begin
  Result := DataSet.FieldByName('Text').AsString;
end;

procedure TdmSnippetSource.SetText(AValue: string);
begin
  DataSet.FieldValues['Text'] := AValue;
end;
{$ENDREGION}

{$REGION 'IConnection'}
function TdmSnippetSource.GetAutoApplyUpdates: Boolean;
begin
  Result := sqoAutoApplyUpdates in DataSet.Options;
end;

procedure TdmSnippetSource.SetAutoApplyUpdates(AValue: Boolean);
begin
  if AValue <> AutoApplyUpdates then
  begin
    if AValue then
      DataSet.Options := DataSet.Options + [sqoAutoApplyUpdates]
    else
      DataSet.Options := DataSet.Options - [sqoAutoApplyUpdates];
  end;
end;

function TdmSnippetSource.GetAutoCommit: Boolean;
begin
  Result := sqoAutoCommit in DataSet.Options;
end;

procedure TdmSnippetSource.SetAutoCommit(AValue: Boolean);
begin
  if AValue <> AutoCommit then
  begin
    if AValue then
      DataSet.Options := DataSet.Options + [sqoAutoCommit]
    else
      DataSet.Options := DataSet.Options - [sqoAutoCommit];
  end;
end;

function TdmSnippetSource.GetFileName: string;
begin
  Result := conMain.DatabaseName;
end;

procedure TdmSnippetSource.SetFileName(AValue: string);
begin
  if AValue <> FileName then
  begin
    conMain.Connected    := False;
    conMain.DatabaseName := AValue;
    conMain.Connected    := True;
    Active := True;
  end;
end;
{$ENDREGION}

{$REGION 'ISQLite'}
function TdmSnippetSource.GetVersion: string;
begin
  Result := QueryLookup(conMain, SQL_SQLITE_VERSION);
end;

function TdmSnippetSource.GetSize: Int64;
begin
  Result := FileSize(FileName);
end;
{$ENDREGION}

{$REGION 'ISearch'}
function TdmSnippetSource.GetSearchDataSet: TSQLQuery;
begin
  Result := qrySearch;
end;
{$ENDREGION}

{$REGION 'IDataSet'}
function TdmSnippetSource.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

procedure TdmSnippetSource.SetActive(AValue: Boolean);
begin
  DataSet.Active := AValue;
end;

function TdmSnippetSource.GetDataSet: TSQLQuery;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;
{$ENDREGION}

{$REGION 'IGlyphs'}
function TdmSnippetSource.GetImageList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSource.GetGlyphDataSet: TDataSet;
begin
  Result := qryGlyph;
end;

function TdmSnippetSource.GetGlyphList: TImageList;
begin
  Result := imlGlyphs;
end;
{$ENDREGION}

{$REGION 'IHighLighters'}
function TdmSnippetSource.GetHighlighterDataSet: TDataSet;
begin
  Result := qryHighlighter;
end;
{$ENDREGION}

{$REGION 'IQuery'}
function TdmSnippetSource.GetQuery: TSQLQuery;
begin
  Result := qryQuery;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmSnippetSource.CreateLookupFields;
var
  F : TField = nil;
begin
  if not Assigned(DataSet.FindField('Highlighter')) then
  begin
    F := TStringField.Create(DataSet);
    F.DataSet           := DataSet;
    F.LookupDataSet     := qryHighlighter;
    F.KeyFields         := 'HighlighterId';
    F.FieldName         := 'Highlighter';
    F.FieldKind         := fkLookup;
    F.LookupKeyFields   := 'Id';
    F.LookupResultField := 'Code';
    F.LookupCache       := True;
    F.ProviderFlags     := [];
  end;
  F := nil;
end;

procedure TdmSnippetSource.EnsureSqliteExists;
var
  LResourceHandle : THandle;
  LResourceData   : THandle;
  LResourceSize   : LongInt;
  LFileHandle     : THandle;
  LBytesWritten   : DWORD;
begin
  if not FileExistsUTF8(SQLITE3_DLL) then
  begin
    // Find the resource
    LResourceHandle := FindResource(HInstance, 'SQLITE3', RT_RCDATA);
    if LResourceHandle = 0 then
      raise Exception.Create('Resource not found');

    // Load the resource data into memory
    LResourceData := LoadResource(HInstance, LResourceHandle);
    if LResourceData = 0 then
      raise Exception.Create('Error loading resource');
    try
      LResourceSize := SizeOfResource(HInstance, LResourceHandle);

      // Create the file and write the resource data to it
      LFileHandle := CreateFile(SQLITE3_DLL,
        GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      if LFileHandle = INVALID_HANDLE_VALUE then
        raise Exception.Create('Error creating file');
      try
        if not WriteFile(LFileHandle, Pointer(LResourceData)^, LResourceSize, LBytesWritten, nil) then
          raise Exception.Create('Error writing to file');
      finally
        CloseHandle(LFileHandle);
      end;
    finally
      FreeResource(LResourceData);
    end;
  end;
end;

procedure TdmSnippetSource.EnsureWebView2LoaderExists;
var
  LResourceHandle : THandle;
  LResourceData   : THandle;
  LResourceSize   : LongInt;
  LFileHandle     : THandle;
  LBytesWritten   : DWORD;
begin
  if not FileExistsUTF8(WEBVIEW2LOADER_DLL) then
  begin
    // Find the resource
    LResourceHandle := FindResource(HInstance, 'WEBVIEW2LOADER', RT_RCDATA);
    if LResourceHandle = 0 then
      raise Exception.Create('Resource not found');

    // Load the resource data into memory
    LResourceData := LoadResource(HInstance, LResourceHandle);
    if LResourceData = 0 then
      raise Exception.Create('Error loading resource');
    try
      LResourceSize := SizeOfResource(HInstance, LResourceHandle);

      // Create the file and write the resource data to it
      LFileHandle := CreateFile(WEBVIEW2LOADER_DLL,
        GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      if LFileHandle = INVALID_HANDLE_VALUE then
        raise Exception.Create('Error creating file');
      try
        if not WriteFile(LFileHandle, Pointer(LResourceData)^, LResourceSize, LBytesWritten, nil) then
          raise Exception.Create('Error writing to file');
      finally
        CloseHandle(LFileHandle);
      end;
    finally
      FreeResource(LResourceData);
    end;
  end;
end;

procedure TdmSnippetSource.FillImageMapFromDataSet(AImageMap: TImageMap;
  ADataSet: TDataSet);
var
  F  : TBlobField;
  P  : TPicture      = nil;
  MS : TMemoryStream = nil;
  BM : TBitmap;
begin
  ADataSet.DisableControls;
  P := TPicture.Create;
  MS := TMemoryStream.Create;
  try
    ADataSet.First;
    F := TBlobField(ADataSet.FieldByName('Image'));
    while not ADataSet.EOF do
    begin
      if not F.IsNull then
      begin
        MS.Clear;
        F.SaveToStream(MS);
        MS.Position := 0;
        P.LoadFromStream(MS);
        BM := TBitmap.Create;
        BM.Assign(P.Bitmap);
        AImageMap.Add(qryHighlighter.FieldByName('Id').AsInteger, BM);
      end;
      ADataSet.Next;
    end;
  finally
    FreeAndNil(MS);
    FreeAndNil(P);
    ADataSet.EnableControls;
  end;
end;

procedure TdmSnippetSource.FillNodeTypesImageList;
var
  F  : TBlobField;
  P  : TPicture = nil;
  MS : TMemoryStream;
begin
  qryNodeType.First;
  F := TBlobField(qryNodeType.FieldByName('Image'));
  while not qryNodeType.EOF do
  begin
    if not F.IsNull then
    begin
      P := TPicture.Create;
      try
        MS := TMemoryStream.Create;
        try
          F.SaveToStream(MS);
          MS.Position := 0;
          P.LoadFromStream(MS);
          imlNodeTypes.Add(P.Bitmap, nil);
        finally
          FreeAndNil(MS);
        end;
      finally
        FreeAndNil(P);
      end;
    end;
    qryNodeType.Next;
  end;
end;

procedure TdmSnippetSource.FillImageMaps;
var
  F  : TBlobField;
  P  : TPicture = nil;
  MS : TMemoryStream;
  BM : TBitmap;
begin
  qryHighlighter.First;
  F := TBlobField(qryHighlighter.FieldByName('Image'));
  while not qryNodeType.EOF do
  begin
    if not F.IsNull then
    begin
      P := TPicture.Create;
      try
        MS := TMemoryStream.Create;
        try
          F.SaveToStream(MS);
          MS.Position := 0;
          P.LoadFromStream(MS);
          BM := TBitmap.Create;
          BM.Assign(P.Bitmap);
          FHLImages.Add(qryHighlighter.FieldByName('Id').AsInteger, BM);
            //imlNodeTypes.Add(P.Bitmap, nil);
          finally
          FreeAndNil(MS);
        end;
      finally
        FreeAndNil(P);
      end;
    end;
    qryNodeType.Next;
  end;
end;

{ The provider flag pfRefreshOnInsert is used to fetch the Id-value of the last
  inserted record.
  To make this work the query component's RefreshSQL statement is executed right
  after a new record is inserted.
  For SQLite it looks as follows: "select last_insert_rowid() as Id" }

procedure TdmSnippetSource.InitField(AField: TField);
begin
  if AField is TFloatField then
  begin
    TFloatField(AField).DisplayFormat := '#,##0.00';
  end
  else if AField is TDateTimeField then
  begin
    AField.Alignment := taCenter;
  end;
  if SameText(AField.FieldName, 'Id') then
  begin
    AField.Required :=  True;
    AField.ProviderFlags := [pfInKey];
  end
  // setup lookupfields
  else if SameText(AField.FieldName, 'HighLighter') then
  begin
    AField.KeyFields     := 'HighlighterId';
    AField.ProviderFlags := [];
  end
  else
  begin
    AField.ProviderFlags := [
      pfInUpdate // Changes to the field should be propagated to the database.
    ];
  end;
end;

procedure TdmSnippetSource.InitFields(ADataSet: TDataSet);
var
  LField : TField;
begin
  for LField in ADataSet.Fields do
    InitField(LField);
end;

{$REGION 'ISQLite'}
function TdmSnippetSource.IntegrityCheck: Boolean;
begin
  Result := QueryLookup(conMain, 'pragma integrity_check;') = 'ok';
end;

procedure TdmSnippetSource.ShrinkMemory;
begin
  conMain.ExecuteDirect('pragma shrink_memory;');
end;

procedure TdmSnippetSource.Vacuum;
begin
  conMain.ExecuteDirect('vacuum;');
end;
{$ENDREGION}

{$REGION 'IConnection'}
function TdmSnippetSource.ApplyUpdates: Boolean;
begin
  if DataSet.ChangeCount > 0 then
  begin
    DataSet.ApplyUpdates;
    Result := True;
  end
  else
    Result := False;
  if qryGlyph.ChangeCount > 0 then
  begin
    qryGlyph.ApplyUpdates;
    Result := True;
  end;
end;

procedure TdmSnippetSource.CreateNewDatabase;
begin
  CreateDatabaseTables;
  CreateDatabaseIndexes;
  SetupConfigurationData;
end;

{ Creates all tables. They are dropped first in case they would exist. }

procedure TdmSnippetSource.CreateDatabaseTables;
begin
  Logger.Info('Creating new tables...');
  scrCreateTables.ExecuteScript;
end;

{ Creates indexes for all tables. They are dropped first in case they would
  exist. }

procedure TdmSnippetSource.CreateDatabaseIndexes;
begin
  Logger.Info('Creating new indexes...');
  scrCreateIndexes.ExecuteScript;
end;

{ Inserts configuration data for highlighters, comment types and node types.
  Any data in these tables is cleared before insert. }

procedure TdmSnippetSource.SetupConfigurationData;
begin
  Logger.Info('Insert configuration data...');
  scrInsertData.ExecuteScript;
end;

procedure TdmSnippetSource.Commit;
begin
  Logger.Enter(Self, 'Commit');
  DataSet.SQLTransaction.Commit;
  Logger.Leave(Self, 'Commit');
end;

procedure TdmSnippetSource.ExecuteDirect(const ASQL: string);
begin
  conMain.ExecuteDirect(ASQL);
end;

procedure TdmSnippetSource.Rollback;
begin
  Logger.Enter(Self, 'Rollback');
  DataSet.SQLTransaction.Rollback;
  Logger.Leave(Self, 'Rollback');
end;

procedure TdmSnippetSource.StartTransaction;
begin
  Logger.Enter(Self, 'StartTransaction');
  DataSet.SQLTransaction.StartTransaction;
  Logger.Leave(Self, 'StartTransaction');
end;

procedure TdmSnippetSource.EndTransaction;
begin
  Logger.Enter(Self, 'EndTransaction');
  DataSet.SQLTransaction.EndTransaction;
  Logger.Leave(Self, 'EndTransaction');
end;
{$ENDREGION}

{$REGION 'IQuery'}
procedure TdmSnippetSource.Execute(const ASQL: string);
begin
  qryQuery.Active   := False;
  qryQuery.SQL.Text := ASQL;
  Logger.Info(ASQL);
  qryQuery.Active   := True;
end;

function TdmSnippetSource.LastId: Integer;
begin
  Result := QueryValue(SQL_LAST_ID);
end;

function TdmSnippetSource.NewSequence: Integer;
var
  S : string;
begin
  S := Format(SQL_NEW_SEQUENCE, [ParentId]);
  Result := QueryValue(S);
end;

function TdmSnippetSource.QueryValue(const ASQL: string): Variant;
begin
  Execute(ASQL);
  Result := qryQuery.Fields[0].Value;
  qryQuery.Active := False;
end;
{$ENDREGION}

{$REGION 'IDataSet'}
procedure TdmSnippetSource.DuplicateRecords(AValues: TStrings);
begin
  Logger.Enter(Self, 'DuplicateRecords');
  ExecuteDirect(Format(SQL_DUPLICATE_IDS, [AValues.CommaText]));
  DataSet.Refresh;
  Logger.Leave(Self, 'DuplicateRecords');
end;

procedure TdmSnippetSource.MoveDownRecords(AValues: TStrings);
var
  S : string;
  S2 : string;
begin
  Logger.Enter(Self, 'MoveDownRecords');
  //S1 := Format(SQL_MOVEDOWN_IDS1, [
  //  AValues.Count,
  //  IntToStr(ParentId),
  //  AValues[0]
  //]);
  //S2 := Format(SQL_MOVEDOWN_IDS2, [
  //  AValues.CommaText
  //]);
  //Logger.SendText(S1);
  //Logger.SendText(S2);
  //ExecuteDirect(S1);
  //ExecuteDirect(S2);

  S := Format(SQL_MOVEDOWN_IDS, [
    AValues.Count,
    IntToStr(ParentId),
    AValues[0],
    AValues.CommaText
  ]);
  Logger.SendStrings('TS',S);
  ExecuteDirect(S);
  DataSet.Refresh;
  Logger.Leave(Self, 'MoveDownRecords');
end;

procedure TdmSnippetSource.MoveUpRecords(AValues: TStrings);
var
  S1 : string;
  S2 : string;
begin
  Logger.Enter(Self, 'MoveUpRecords');
  S1 := Format(SQL_MOVEUP_IDS1, [
    AValues.Count,
    IntToStr(ParentId),
    AValues[0]
  ]);
  S2 := Format(SQL_MOVEUP_IDS2, [
    AValues.CommaText
  ]);
  Logger.SendText(S1);
  Logger.SendText(S2);
  ExecuteDirect(S1);
  ExecuteDirect(S2);
  DataSet.Refresh;
  Logger.Leave(Self, 'MoveUpRecords');
end;

procedure TdmSnippetSource.UpdateNodePaths;
begin
  ExecuteDirect(SQL_UPDATE_NODEPATH);
  DataSet.Refresh;
end;

procedure TdmSnippetSource.UpdateSequences;
begin
  ExecuteDirect(SQL_UPDATE_SEQUENCE);
  DataSet.Refresh;
end;

function TdmSnippetSource.Post: Boolean;
begin
  if DataSet.Active and (DataSet.State in dsEditModes) then
  begin
    DataSet.Post;
    Result := True;
  end
  else
    Result := False;
end;

function TdmSnippetSource.Append: Boolean;
begin
  if DataSet.Active and not (DataSet.State in dsEditModes) then
  begin
    DataSet.Append;
    Result := True;
  end
  else
    Result := False;
end;

function TdmSnippetSource.Edit: Boolean;
begin
  if DataSet.Active then
  begin
    if not (DataSet.State in dsEditModes) then
      DataSet.Edit;
    Result := True;
  end
  else
    Result := False;
end;

{ Starts buffering inserts to apply and commit them in one go by calling
  EndBulkInserts. }

procedure TdmSnippetSource.BeginBulkInserts;
begin
  FBulkInsertMode := True;
end;

procedure TdmSnippetSource.EndBulkInserts;
begin
  if FBulkInsertMode then
  begin;
    FBulkInsertMode := False;
    ApplyUpdates;
    Commit;
  end;
end;

{ Connects to the SQLite database file specified by AFileName. }

procedure TdmSnippetSource.ConnectToDatabase(const AFileName: string);
var
  LFileName : string;
begin
  Logger.Send('AFileName', AFileName);
  if not FileExists(FSettings.Database) then
  begin
    FSettings.Database := DEFAULT_DATABASE_NAME;
  end;
  if FilenameIsAbsolute(FSettings.Database) then
  begin
    LFileName := FSettings.Database;
  end
  else
  begin
    LFileName := CreateAbsolutePath(FSettings.Database, ProgramDirectory);
  end;
  if not SameFileName(LFileName, conMain.DatabaseName) then
  begin
    conMain.Connected := False;
    qryHighlighter.Active := False;
      //FillImageMapFromDataSet(FHLImages, qryHighlighter);
    qryGlyph.Active :=  False;
    qryNodeType.Active := False;
    //FillNodeTypesImageList;
    DataSet.Active := False;
    conMain.DatabaseName := LFileName;
    if (not FileExists(LFileName)) or (FileSize(LFileName) = 0) then
    begin
      CreateNewDatabase;
    end;
    Logger.Info('Connecting to SQLite DB: %s', [LFileName]);
    conMain.Connected := True;
    qryHighlighter.Active := True;
      //FillImageMapFromDataSet(FHLImages, qryHighlighter);
    qryGlyph.Active :=  True;
    qryNodeType.Active := True;
    //FillNodeTypesImageList;
    DataSet.Active := True;
  end;
end;

function TdmSnippetSource.BackupDatabase: string;
var
  LZipper   : TZipper;
  LFileName : string;
begin
  Logger.Enter(Self, 'BackupDatabase');
  LZipper := TZipper.Create;
  try
    DisableControls;
    LFileName := Format('%s\%s_%s.zip', [
      ExtractFilePath(conMain.DatabaseName),
      LazFileUtils.ExtractFileNameWithoutExt(ExtractFileName(conMain.DatabaseName)),
      FormatDateTime('YYYYmmdd_hhnn', Now)
      ]
    );
    if FileExists(LFileName) then
      DeleteFile(LFileName);
    LZipper.FileName  := LFileName;
    DataSet.Active    := False;
    conMain.Connected := False;
    LZipper.Entries.AddFileEntry(
      conMain.DatabaseName,
      ExtractFileName(conMain.DatabaseName) // just filename, no path
    );
    LZipper.ZipAllFiles;
    conMain.Connected := True;
    DataSet.Active    := True;
    EnableControls;
  finally
    LZipper.Free;
  end;
  Result := LFileName;
  Logger.Leave(Self, 'BackupDatabase');
end;

procedure TdmSnippetSource.DisableControls;
begin
  FFocusedId := Id;
  DataSet.DisableControls;
end;

procedure TdmSnippetSource.EnableControls;
begin
  DataSet.Locate('Id', FFocusedId, []);
  FLastActiveViews := ActiveViews;
  DataSet.EnableControls;
end;
{$ENDREGION}

{$REGION 'ISearch'}
procedure TdmSnippetSource.Search(const ASearchString: string;
  ASearchInText: Boolean; ASearchInName: Boolean; ASearchInComment: Boolean);
begin
  SearchDataSet.Active   := False;
  SearchDataSet.SQL.Text := Format(SQL_LOOKUP_QUERY, [ASearchString]);
  SearchDataSet.Active   := True;
end;
{$ENDREGION}
{$ENDREGION}

end.

