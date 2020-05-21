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

  TODO
    - bulk insert for faster adding lots of files. Performance is much increased
      by disabling autocommit and ony once committing after all records are
      inserted.
    - delete and update for a given list of node Id's

  ISSUES
   - RefreshSQL gets called after every insert or update regardless of the
     specified provider flags on the key field (pfRefreshOnInsert,
     pfRefreshOnUpdate) or whether sqoRefreshUsingSelect is specified in the
     query's options.
}
{$ENDREGION}

uses
  Classes, SysUtils, FileUtil, Controls,

  sqldb, sqlite3conn, db,

  ts.Core.Logger,

  SnippetSource.Interfaces, sqlscript;

type
  TdmSnippetSource = class(TDataModule,
    ISQLite, IConnection, ISnippet, IDataSet, ILookup, IGlyphs, IHighlighters
  )
    conMain           : TSQLite3Connection;
    imlGlyphs         : TImageList;
    qryGlyph          : TSQLQuery;
    qryHighlighter    : TSQLQuery;
    qryNodeType       : TSQLQuery;
    qrySnippet        : TSQLQuery;
    qryLookup         : TSQLQuery;
    scrCreateDatabase : TSQLScript;
    trsMain           : TSQLTransaction;

    {$REGION 'event handlers'}
    procedure conMainLog(
      Sender    : TSQLConnection;
      EventType : TDBEventType;
      const Msg : string
    );
    procedure qryGlyphBeforePost(DataSet: TDataSet);
    procedure qryGlyphNewRecord(DataSet: TDataSet);
    procedure qrySnippetAfterOpen(ADataSet: TDataSet);
    procedure qrySnippetAfterPost(ADataSet: TDataSet);
    procedure qrySnippetBeforeOpen(ADataSet: TDataSet);
    procedure qrySnippetBeforePost(ADataSet: TDataSet);
    procedure qrySnippetBeforeScroll(ADataSet: TDataSet);
    procedure qrySnippetNewRecord(ADataSet: TDataSet);
    {$ENDREGION}

  private
    FSettings       : ISettings;
    FBulkInsertMode : Boolean;
    FReadOnly       : Boolean;

    {$REGION 'property access mehods'}
    function GetActive: Boolean;
    function GetAutoApplyUpdates: Boolean;
    function GetAutoCommit: Boolean;
    function GetComment: string;
    function GetCommentRtf: string;
    function GetDataSet: TSQLQuery;
    function GetDateCreated: TDateTime;
    function GetDateModified: TDateTime;
    function GetDBVersion: string;
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
    function GetLookupDataSet: TDataSet;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeId: Integer;
    function GetParentId: Integer;
    function GetReadOnly: Boolean;
    function GetRecordCount: Integer;
    function GetSize: Int64;
    function GetText: string;
    procedure SetActive(AValue: Boolean);
    procedure SetAutoApplyUpdates(AValue: Boolean);
    procedure SetAutoCommit(AValue: Boolean);
    procedure SetComment(AValue: string);
    procedure SetCommentRtf(AValue: string);
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
    procedure SetReadOnly(AValue: Boolean);
    procedure SetText(AValue: string);
    {$ENDREGION}

  protected
    procedure CreateLookupFields;
    procedure InitField(AField : TField);
    procedure InitFields(ADataSet : TDataSet);
    procedure Lookup(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );

    {$REGION 'ISQLite'}
    function IntegrityCheck: Boolean;
    procedure ShrinkMemory;
    procedure Vacuum;
    {$ENDREGION}

    {$REGION 'IConnection'}
    procedure CreateNewDatabase;
    procedure Execute(const ASQL: string);
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
    procedure EndTransaction;
    {$ENDREGION}

    {$REGION 'IDataSet'}
    function Post: Boolean;
    function Edit: Boolean;
    function Append: Boolean;
    function ApplyUpdates: Boolean;

    procedure BeginBulkInserts;
    procedure EndBulkInserts;
    procedure EnableControls;
    procedure DisableControls;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : ISettings
    ); reintroduce; virtual;

    {$REGION 'ISQLite'}
    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property Size: Int64
      read GetSize;

    property DBVersion: string
      read GetDBVersion;
    {$ENDREGION}

    {$REGION 'IConnection'}
    property AutoApplyUpdates: Boolean
      read GetAutoApplyUpdates write SetAutoApplyUpdates;

    property AutoCommit: Boolean
      read GetAutoCommit write SetAutoCommit;
    {$ENDREGION}

    {$REGION 'ISnippet'}
    property Comment: string
      read GetComment write SetComment;

    property CommentRtf: string
      read GetCommentRtf write SetCommentRtf;

    property DateCreated: TDateTime
      read GetDateCreated write SetDateCreated;

    property DateModified: TDateTime
      read GetDateModified write SetDateModified;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property FoldState: string
      read GetFoldState write SetFoldState;

    property Highlighter: string
      read GetHighlighter write SetHighlighter;

    property Id: Integer
      read GetId;

    property NodeName: string
      read GetNodeName write SetNodeName;

    property NodePath: string
      read GetNodePath write SetNodePath;

    property NodeTypeId: Integer
      read GetNodeTypeId write SetNodeTypeId;

    property ParentId: Integer
      read GetParentId write SetParentId;

    property Text: string
      read GetText write SetText;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TSQLQuery
      read GetDataSet;

    property LookupDataSet: TDataSet
      read GetLookupDataSet;

    property FileName: string
      read GetFileName write SetFileName;

    property ImageList: TImageList
      read GetImageList;

    property GlyphList: TImageList
      read GetGlyphList;

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;
    {$ENDREGION}
  end;

implementation

{$R *.lfm}

uses
  Variants, TypInfo, LazFileUtils,

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
end;

procedure TdmSnippetSource.AfterConstruction;
var
  LFileName : string;
begin
  Logger.Enter(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FSettings.DataBase := DATABASE_NAME;
  if FilenameIsAbsolute(FSettings.DataBase) then
  begin
    LFileName := FSettings.DataBase;
  end
  else
  begin
    LFileName := CreateAbsolutePath(FSettings.DataBase, ProgramDirectory);
  end;
  Logger.Info('Connecting to SQLite DB: %s', [LFileName]);
  conMain.DatabaseName := LFileName;
  conMain.Connected := True;
  if (not FileExists(LFileName)) or (FileSize(LFileName) = 0) then
  begin
    CreateNewDatabase;
  end;
  qrySnippet.UsePrimaryKeyAsKey := True;
  qryHighlighter.Active := True;
  qryGlyph.Active :=  True;
  DataSet.Active := True;
  Logger.Leave(Self, 'AfterConstruction');
end;

destructor TdmSnippetSource.Destroy;
begin
  ApplyUpdates;
  Commit;
  DataSet.Active := False;
  conMain.Connected := False;
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmSnippetSource.conMainLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: string);
//var
//  S : string;
begin
  //S := GetEnumName(TypeInfo(TDBEventType), Ord(EventType));
//  Logger.SendText(Msg);
end;

procedure TdmSnippetSource.qryGlyphBeforePost(DataSet: TDataSet);
begin
  DataSet.FieldByName('DateModified').AsDateTime := Now;
end;

procedure TdmSnippetSource.qryGlyphNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('DateCreated').AsDateTime := Now;
end;

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
  DS := ADataSet as TSQLQuery;
  // These two steps are required to get the FieldDefs of the PK initialized
  // correctly.
  DS.Prepare;
  DS.ServerIndexDefs.Update; // Required to create Id field as ftAutoInc.
  DS.Fields.Clear;
  DS.FieldDefs.Clear;
  DS.FieldDefs.Update;
  for I := 0 to DS.FieldDefs.Count - 1 do
  begin
    FD := DS.FieldDefs[I];
    F := FD.CreateField(DS);
    // below is just intended for diagnostic reasons to inspect fields @runtime
    F.Name := Format('fld%s%s', [DS.Name, F.FieldName]);
  end;
  CreateLookupFields;
end;

procedure TdmSnippetSource.qrySnippetBeforePost(ADataSet: TDataSet);
begin
  Logger.Enter(Self, 'qrySnippetBeforePost');
  DateModified := Now;
  Logger.Leave(Self, 'qrySnippetBeforePost');
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
      ADataSet.Refresh;
      ADataSet.Locate('Id' , LLastID,[]);
    end;
  end;
  Logger.Leave(Self, 'qrySnippetAfterPost');
end;

procedure TdmSnippetSource.qrySnippetNewRecord(ADataSet: TDataSet);
begin
  Logger.Enter(Self, 'qrySnippetNewRecord');
  // forces new value for AutoInc field
  qrySnippet.FieldByName('Id').Value := 0;
  qrySnippet.FieldByName('DateCreated').AsDateTime := Now;
  qrySnippet.FieldByName('HighlighterId').AsInteger := 1;
  if qrySnippet.FieldByName('NodeTypeId').AsInteger = 0 then
  begin
    qrySnippet.FieldByName('NodeTypeId').AsInteger := 1;
    qrySnippet.FieldByName('ImageIndex').AsInteger := 0;
  end;
  Logger.Leave(Self, 'qrySnippetNewRecord');
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TdmSnippetSource.GetActive: Boolean;
begin
  Result := qrySnippet.Active;
end;

procedure TdmSnippetSource.SetActive(AValue: Boolean);
begin
  qrySnippet.Active := AValue;
end;

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

function TdmSnippetSource.GetDataSet: TSQLQuery;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetRecordCount: Integer;
begin
  Result := qrySnippet.RecordCount;
end;

function TdmSnippetSource.GetSize: Int64;
begin
  Result := FileSize(FileName);
end;

function TdmSnippetSource.GetLookupDataSet: TDataSet;
begin
  Result := qryLookup;
end;

function TdmSnippetSource.GetImageIndex: Integer;
begin
  Result := qrySnippet.FieldValues['ImageIndex'];
end;

procedure TdmSnippetSource.SetImageIndex(AValue: Integer);
begin
  qrySnippet.FieldValues['ImageIndex'] := AValue;
end;

function TdmSnippetSource.GetImageList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSource.GetDateCreated: TDateTime;
begin
  if qrySnippet.FieldValues['DateCreated'] <> Null then
    Result := qrySnippet.FieldValues['DateCreated']
  else
    Result := 0;
end;

procedure TdmSnippetSource.SetDateCreated(AValue: TDateTime);
begin
  qrySnippet.FieldValues['DateCreated'] := AValue;
end;

function TdmSnippetSource.GetDateModified: TDateTime;
begin
  if qrySnippet.FieldValues['DateModified'] <> Null then
    Result := qrySnippet.FieldValues['DateModified']
  else
    Result := 0;
end;

procedure TdmSnippetSource.SetDateModified(AValue: TDateTime);
begin
  qrySnippet.FieldValues['DateModified'] := AValue;
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
  end;
end;

function TdmSnippetSource.GetComment: string;
begin
  Result := qrySnippet.FieldByName('Comment').AsString;
end;

procedure TdmSnippetSource.SetComment(AValue: string);
begin
  qrySnippet.FieldValues['Comment'] := AValue;
end;

function TdmSnippetSource.GetCommentRtf: string;
begin
  Result := qrySnippet.FieldByName('CommentRtf').AsString;
end;

procedure TdmSnippetSource.SetCommentRtf(AValue: string);
begin
  qrySnippet.FieldValues['CommentRtf'] := AValue;
end;

function TdmSnippetSource.GetFoldLevel: Integer;
begin
  Result := qrySnippet.FieldValues['FoldLevel'];
end;

procedure TdmSnippetSource.SetFoldLevel(AValue: Integer);
begin
  qrySnippet.FieldValues['FoldLevel'] := AValue;
end;

function TdmSnippetSource.GetGlyphDataSet: TDataSet;
begin
  Result := qryGlyph;
end;

function TdmSnippetSource.GetGlyphList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSource.GetFoldState: string;
begin
  Result := qrySnippet.FieldByName('FoldState').AsString;
end;

procedure TdmSnippetSource.SetFoldState(AValue: string);
begin
  qrySnippet.FieldValues['FoldState'] := AValue;
end;

function TdmSnippetSource.GetHighlighter: string;
begin
  Result := qrySnippet.FieldByName('Highlighter').AsString;
end;

procedure TdmSnippetSource.SetHighlighter(AValue: string);
var
  LId : Variant;
begin
  if AValue <> Highlighter then
  begin
     if not qryHighlighter.Active then
        qryHighlighter.Active := True;
    LId := qryHighlighter.Lookup('Code', VarArrayOf([AValue]), 'Id');
    if VarIsNull(LId) then
      LId := 1;
    qrySnippet.FieldValues['HighlighterId'] := LId;
  end;
end;

function TdmSnippetSource.GetHighlighterDataSet: TDataSet;
begin
  Result := qryHighlighter;
end;

function TdmSnippetSource.GetId: Integer;
begin
  if VarIsNull(qrySnippet.FieldValues['Id']) then
    Result := 0
  else
    Result := qrySnippet.FieldValues['Id'];
end;

function TdmSnippetSource.GetNodeName: string;
begin
  Result := qrySnippet.FieldByName('NodeName').AsString;
end;

procedure TdmSnippetSource.SetNodeName(AValue: string);
begin
  qrySnippet.FieldValues['NodeName'] := AValue;
end;

function TdmSnippetSource.GetNodePath: string;
begin
  Result := qrySnippet.FieldByName('NodePath').AsString;
end;

procedure TdmSnippetSource.SetNodePath(AValue: string);
begin
  qrySnippet.FieldValues['NodePath'] := AValue;
end;

function TdmSnippetSource.GetNodeTypeId: Integer;
begin
  Result := qrySnippet.FieldValues['NodeTypeId'];
end;

procedure TdmSnippetSource.SetNodeTypeId(AValue: Integer);
begin
  qrySnippet.FieldValues['NodeTypeId'] := AValue;
end;

function TdmSnippetSource.GetParentId: Integer;
begin
  Result := qrySnippet.FieldValues['ParentId'];
end;

procedure TdmSnippetSource.SetParentId(AValue: Integer);
begin
  qrySnippet.FieldValues['ParentId'] := AValue;
end;

function TdmSnippetSource.GetText: string;
begin
  Result := qrySnippet.FieldByName('Text').AsString;
end;

procedure TdmSnippetSource.SetText(AValue: string);
begin
  qrySnippet.FieldValues['Text'] := AValue;
end;

function TdmSnippetSource.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TdmSnippetSource.SetReadOnly(AValue: Boolean);
begin
  FReadOnly := AValue;
end;

function TdmSnippetSource.GetDBVersion: string;
begin
  Result := QueryLookup(conMain, 'select sqlite_version();');
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmSnippetSource.CreateLookupFields;
var
  F : TField = nil;
begin
  if not Assigned(qrySnippet.FindField('Highlighter')) then
  begin
    F := TStringField.Create(qrySnippet);
    F.DataSet           := qrySnippet;
    F.LookupDataSet     := qryHighlighter;
    F.KeyFields         := 'HighlighterId';
    F.FieldName         := 'Highlighter';
    F.FieldKind         := fkLookup;
    F.LookupKeyFields   := 'Id';
    F.LookupResultField := 'Code';
    F.LookupCache       := True;
    F.ProviderFlags     := [];
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
  // setup lookupfield
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
  Field : TField;
begin
  for Field in ADataSet.Fields do
    InitField(Field);
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
procedure TdmSnippetSource.CreateNewDatabase;
begin
  Logger.Info('Creating new database...');
  scrCreateDatabase.ExecuteScript;
end;

procedure TdmSnippetSource.Commit;
begin
  Logger.Enter(Self, 'Commit');
  DataSet.SQLTransaction.Commit;
  Logger.Leave(Self, 'Commit');
end;

procedure TdmSnippetSource.Execute(const ASQL: string);
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

{$REGION 'IDataSet'}
function TdmSnippetSource.Post: Boolean;
begin
  if DataSet.Active and (DataSet.State in dsEditModes) then
  begin
    Logger.Info('Post');
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
    Logger.Info('Append');
    DataSet.Append;
    Result := True;
  end
  else
    Result := False;
end;

function TdmSnippetSource.ApplyUpdates: Boolean;
begin
  if DataSet.ChangeCount > 0 then
  begin
    Logger.Info('ApplyUpdates');
    Logger.Send('DataSet.ChangeCount', DataSet.ChangeCount);
    DataSet.ApplyUpdates;
    Result := True;
  end
  else
    Result := False;
end;

function TdmSnippetSource.Edit: Boolean;
begin
  if DataSet.Active and not (DataSet.State in dsEditModes) then
  begin
    Logger.Info('Edit');
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

procedure TdmSnippetSource.DisableControls;
begin
  DataSet.DisableControls;
end;

procedure TdmSnippetSource.EnableControls;
begin
  DataSet.EnableControls;
end;
{$ENDREGION}

procedure TdmSnippetSource.Lookup(const ASearchString: string;
  ASearchInText: Boolean; ASearchInName: Boolean; ASearchInComment: Boolean);
begin
  qryLookup.Active := False;
  qryLookup.SQL.Text := Format(
    'select * from Snippet where Text like ''%%%s%%''',
    [ASearchString]
  );
  qryLookup.Active := True;
end;
{$ENDREGION}

end.

