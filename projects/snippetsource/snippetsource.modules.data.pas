{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

--------------------------------------------------------------------------------
     interface
    procedure RefreshADatasetAfterInsert(pDataSet: TSQLQuery);overload;
    procedure RefreshADatasetAfterInsert(pDataSet: TSQLQuery; pKeyField: string);overload;

implementation

procedure RefreshADatasetAfterInsert(pDataSet: TSQLQuery; pKeyField: string);
//This procedure refreshes a dataset and positions cursor to last record
//To be used if Dataset is not guaranteed to be sorted by an autoincrement primary key
var
  vLastID: Integer;
  vUpdateStatus : TUpdateStatus;
begin
  vUpdateStatus := pDataset.UpdateStatus;
  //Get last inserted ID in the database
  pDataset.ApplyUpdates;
  vLastID:=(pDataSet.DataBase as TSQLite3Connection).GetInsertID;
  //Now come back to respective row
  if vUpdateStatus = usInserted then begin
    pDataset.Refresh;
    //Refresh and go back to respective row
    pDataset.Locate(pKeyField,vLastID,[]);
  end;
end;

procedure RefreshADatasetAfterInsert(pDataSet: TSQLQuery);
//This procedure refreshes a dataset and positions cursor to last record
//To be used only if DataSet is guaranteed to be sorted by an autoincrement primary key
var
  vLastID: Integer;
  vUpdateStatus : TUpdateStatus;
begin
  vUpdateStatus := pDataset.UpdateStatus;
  pDataset.ApplyUpdates;
  vLastID:=(pDataSet.DataBase as TSQLite3Connection).GetInsertID;
  if vUpdateStatus = usInserted then begin
    pDataset.Refresh;
    //Dangerous!
    pDataSet.Last;
  end;
end;

procedure TDataModule1.SQLQuery1AfterPost(DataSet: TDataSet);
begin
  RefreshADatasetAfterInsert(Dataset as TSQLQuery); //If your dataset is sorted by primary key
end;

procedure TDataModule1.SQLQuery2AfterPost(DataSet: TDataSet);
begin
  RefreshADatasetAfterInsert(Dataset as TSQLQuery, 'ID'); //if you are not sure that the dataset is always sorted by primary key
end;

--------------------------------------------------------------------------------

}
{$ENDREGION}

uses
  Classes, SysUtils, FileUtil, Controls,

  sqldb, sqlite3conn, db, BufDataset,

  SnippetSource.Interfaces, ts.Core.SharedLogger;

type

  { TdmSnippetSource }

  TdmSnippetSource = class(TDataModule,
    IConnection, ISnippet, IDataSet, ILookup, IGlyphs
  )
    conMain           : TSQLite3Connection;
    imlGlyphs         : TImageList;
    qryGlyph          : TSQLQuery;
    qryHighlighter    : TSQLQuery;
    qryNodeType       : TSQLQuery;
    qrySnippet        : TSQLQuery;
    scrCreateDatabase : TSQLScript;
    trsMain           : TSQLTransaction;

    {$REGION 'event handlers'}
    procedure conMainLog(
      Sender    : TSQLConnection;
      EventType : TDBEventType;
      const Msg : string
    );
    procedure qrySnippetAfterOpen(DataSet: TDataSet);
    procedure qrySnippetAfterPost(DataSet: TDataSet);
    procedure qrySnippetBeforeOpen(DataSet: TDataSet);
    procedure qrySnippetBeforePost(DataSet: TDataSet);
    procedure qrySnippetBeforeScroll(DataSet: TDataSet);
    procedure qrySnippetNewRecord(DataSet: TDataSet);
    {$ENDREGION}

  private
    {$REGION 'property access mehods'}
    function GetActive: Boolean;
    function GetAutoApplyUpdates: Boolean;
    function GetAutoCommit: Boolean;
    function GetComment: string;
    function GetCommentRtf: string;
    function GetDataSet: TSQLQuery;
    function GetDateCreated: TDateTime;
    function GetDateModified: TDateTime;
    function GetFileName: string;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetGlyphDataSet: TDataSet;
    function GetGlyphList: TImageList;
    function GetHighlighter: string;
    function GetId: Integer;
    function GetImageIndex: Integer;
    function GetImageList: TImageList;
    function GetLookupDataSet: TDataSet;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeId: Integer;
    function GetParentId: Integer;
    function GetRecordCount: Integer;
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
    procedure SetText(AValue: string);
    {$ENDREGION}

  protected
    procedure SendWatchValues;

    procedure CreateLookupFields;
    procedure InitField(AField : TField);
    procedure InitFields(ADataSet : TDataSet);

    procedure LoadGlyphs;

    { IConnection }
    procedure CreateNewDatabase;
    procedure Execute(const ASQL: string);
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
    procedure EndTransaction;

    function Post: Boolean;
    function Edit: Boolean;
    function Append: Boolean;

    procedure DisableControls;
    procedure EnableControls;

    procedure Lookup(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { IConnection }
    property AutoApplyUpdates: Boolean
      read GetAutoApplyUpdates write SetAutoApplyUpdates;

    property AutoCommit: Boolean
      read GetAutoCommit write SetAutoCommit;

    { ISnippet }
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

    property NodeTypeId: Integer read
      GetNodeTypeId write SetNodeTypeId;

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
  end;

implementation

{$R *.lfm}

uses
  Variants, TypInfo,

  ts.Core.Logger.Channel.IPC;

resourcestring
  SQueryLookupErrorRunningQuery = 'Error running query [%s]';
  SQueryLookupTooManyRecords    = 'The query [%s] returned too many records';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';

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
procedure TdmSnippetSource.AfterConstruction;
begin
  inherited AfterConstruction;
  qrySnippet.UsePrimaryKeyAsKey := True;
  qryHighlighter.Active := True;
  DataSet.Active := True;
end;

procedure TdmSnippetSource.BeforeDestruction;
begin
  qrySnippet.ApplyUpdates;
  Commit;
  DataSet.Active := False;
  conMain.Connected := False;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmSnippetSource.conMainLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: string);
var
  S : string;
begin
  S := GetEnumName(TypeInfo(TDBEventType), Ord(EventType));
  Logger.Send(S, Msg);
end;

{
  This is the place where we create persistent fields.

  see https://stackoverflow.com/questions/9064162/how-to-create-a-tdataset-lookup-field-at-runtime

  If you want to use lookup fields in your dataset you need to create persistent
  fields.
  Persistent fields are created based on the fielddefs collection and need to be
  created BEFORE you open the dataset.
}

procedure TdmSnippetSource.qrySnippetBeforeOpen(DataSet: TDataSet);
var
  I  : Integer;
  FD : TFieldDef = nil;
  F  : TField = nil;
  DS : TSQLQuery;
begin
  DS := DataSet as TSQLQuery;
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

procedure TdmSnippetSource.qrySnippetBeforePost(DataSet: TDataSet);
begin
  DateModified := Now;
end;

procedure TdmSnippetSource.qrySnippetBeforeScroll(DataSet: TDataSet);
begin
  if DataSet.State in dsEditModes then
    DataSet.Post;
end;

procedure TdmSnippetSource.qrySnippetAfterOpen(DataSet: TDataSet);
begin
  InitFields(DataSet);
end;

procedure TdmSnippetSource.qrySnippetAfterPost(DataSet: TDataSet);
var
  LId: Integer;
begin
  LId := DataSet.FieldByName('Id').AsInteger;
  Logger.Send('Id', DataSet.FieldByName('Id').AsInteger);
  if not DataSet.ControlsDisabled then
  begin
    DataSet.DisableControls;
    try
      qrySnippet.ApplyUpdates;
      Commit;
      qrySnippet.Refresh;
      qrySnippet.Locate('Id', VarArrayOf([LId]), []);

    finally
      DataSet.EnableControls;
    end;
  end;
end;

procedure TdmSnippetSource.qrySnippetNewRecord(DataSet: TDataSet);
begin
  // forces new value for AutoInc field
  qrySnippet.FieldByName('Id').Value := 0;
  qrySnippet.FieldByName('DateCreated').AsDateTime := Now;
  qrySnippet.FieldByName('HighlighterId').AsInteger := 1;
  if  qrySnippet.FieldByName('NodeTypeId').AsInteger = 0 then
  begin
    qrySnippet.FieldByName('NodeTypeId').AsInteger := 1;
    qrySnippet.FieldByName('ImageIndex').AsInteger := 1;

  end;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TdmSnippetSource.GetActive: Boolean;
begin
  Result := qrySnippet.Active;
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

procedure TdmSnippetSource.SetActive(AValue: Boolean);
begin
  qrySnippet.Active := AValue;
end;

function TdmSnippetSource.GetDataSet: TSQLQuery;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetRecordCount: Integer;
begin
  Result := qrySnippet.RecordCount;
end;

function TdmSnippetSource.GetLookupDataSet: TDataSet;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetImageIndex: Integer;
begin
  Result := qrySnippet.FieldValues['ImageIndex'];
end;

function TdmSnippetSource.GetImageList: TImageList;
begin
  Result := imlGlyphs;
end;

procedure TdmSnippetSource.SetImageIndex(AValue: Integer);
begin
  qrySnippet.FieldValues['ImageIndex'] := AValue;
end;

function TdmSnippetSource.GetDateCreated: TDateTime;
begin
  Result := qrySnippet.FieldValues['DateCreated'];
end;

procedure TdmSnippetSource.SetDateCreated(AValue: TDateTime);
begin
  qrySnippet.FieldValues['DateCreated'] := AValue;
end;

function TdmSnippetSource.GetDateModified: TDateTime;
begin
  Result := qrySnippet.FieldValues['DateModified'];
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
  LId: Variant;
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
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmSnippetSource.SendWatchValues;
//var
//  S : string;
begin
  //S := GetEnumName(TypeInfo(TDataSetState), Ord(DataSet.State));
  //S := System.Copy(S, 3, Length(S));
  //Logger.Watch('State', S);
  //S := GetEnumName(TypeInfo(TUpdateStatus), Ord(DataSet.UpdateStatus));
  //S := System.Copy(S, 3, Length(S));
  //Logger.Watch('UpdateStatus', S);
  //S := GetEnumName(TypeInfo(TUpdateMode), Ord(DataSet.UpdateMode));
  //S := System.Copy(S, 3, Length(S));
  //Logger.Watch('UpdateMode', S);
end;

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

{
  The provider flag pfRefreshOnInsert is used to fetch the Id-value of the last
  inserted record.
  To make this work the query component's RefreshSQL statement is executed right
  after a new record is inserted.
  For SQLite it looks as follows: "select last_insert_rowid() as Id"
}

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
    AField.ProviderFlags := [
       pfInKey
//       pfRefreshOnInsert // This field's value should be refreshed after insert.
    ];
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

procedure TdmSnippetSource.LoadGlyphs;
begin
  //
end;

procedure TdmSnippetSource.Execute(const ASQL: string);
begin
  conMain.ExecuteDirect(ASQL);
end;

procedure TdmSnippetSource.CreateNewDatabase;
begin
  scrCreateDatabase.ExecuteScript;
end;

procedure TdmSnippetSource.Commit;
begin
  DataSet.SQLTransaction.Commit;
end;

procedure TdmSnippetSource.Rollback;
begin
  DataSet.SQLTransaction.Rollback;
end;

procedure TdmSnippetSource.StartTransaction;
begin
  DataSet.SQLTransaction.StartTransaction;
end;

procedure TdmSnippetSource.EndTransaction;
begin
  DataSet.SQLTransaction.EndTransaction;
end;

{$REGION 'IDataSet'}
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
  if DataSet.Active and not (DataSet.State in dsEditModes) then
  begin
    DataSet.Edit;
    Result := True;
  end
  else
    Result := False;
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
var
  R : Variant;
begin
  R := QueryLookup(
  conMain,
  'select ID from Snippet where Text like ''%%%s%%'' limit 1',
    [ASearchString]
  );
  DataSet.Locate('Id', VarArrayOf([R]), []);
end;
{$ENDREGION}

initialization
{$IFDEF WINDOWS}
  Logger.Channels.Add(TIPCChannel.Create);
{$ENDIF}

end.

