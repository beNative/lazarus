{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit snippetsource.modules.data.old;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, FileUtil,

  sqlite3conn, sqldb, db,

  ts.Core.Sharedlogger,

  SnippetSource.Interfaces;

type
  TdmSnippetSourceOld = class(TDataModule,
    IConnection, ISnippet, IDataSet, ILookup, IGlyphs
  )
    {$REGION 'designer controls'}
    conMain                  : TSQLite3Connection;
    imlGlyphs                : TImageList;
    imlMain                  : TImageList;
    LongintField1: TLongintField;
    qryGlyphID               : TLongintField;
    qryGlyphImage            : TBlobField;
    qryGlyphImageIndex       : TLongintField;
    qryGlyphName             : TMemoField;
    qryHighlighter: TSQLQuery;
    qryHighlighterCode: TWideStringField;
    qryHighlighterDescription: TWideStringField;
    qryHighlighterId: TAutoIncField;
    qryHighlighterImageIndex: TLongintField;
    qryHighlighterName: TWideStringField;
    qryHighlighterSingleLineCommentTypeId: TLongintField;
    qryHighlighterStreamCommentTypeId: TLongintField;
    qryNodeTypeID            : TLongintField;
    qryNodeTypeImageIndex    : TLongintField;
    qryNodeTypeName          : TMemoField;
    qrySnippet               : TSQLQuery;
    qryGlyph                 : TSQLQuery;
    qryNodeType              : TSQLQuery;
    qrySnippetComment: TMemoField;
    qrySnippetCommentRtf: TMemoField;
    qrySnippetDateCreated: TDateTimeField;
    qrySnippetDateModified: TDateTimeField;
    qrySnippetFoldLevel: TLongintField;
    qrySnippetFoldState: TStringField;
    qrySnippetHighlighterId: TLongintField;
    qrySnippetId: TAutoIncField;
    qrySnippetImageIndex: TLongintField;
    qrySnippetNodeName: TStringField;
    qrySnippetNodePath: TMemoField;
    qrySnippetNodeTypeId: TLongintField;
    qrySnippetParentId: TLongintField;
    qrySnippetText: TMemoField;
    scrCreateDatabase        : TSQLScript;
    trsSnippet               : TSQLTransaction;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure conMainAfterConnect(Sender: TObject);
    procedure conMainLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
    procedure qrySnippetAfterInsert(DataSet: TDataSet);
    procedure qrySnippetAfterPost(DataSet: TDataSet);
    procedure qrySnippetAfterScroll(DataSet: TDataSet);
    procedure qrySnippetBeforeScroll(DataSet: TDataSet);
    procedure qrySnippetNewRecord(DataSet: TDataSet);
    procedure qrySnippetTextGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    {$ENDREGION}

  private
    FInserted    : Boolean;
    FHighlighter : string;

    {$REGION 'property access methods'}
    function GetGlyphDataSet: TDataSet;
    function GetGlyphList: TImageList;
    function GetImageList: TImageList;
    function GetImageIndex: Integer;
    procedure SetDateCreated(AValue: TDateTime);
    function GetDateCreated: TDateTime;
    procedure SetDateModified(AValue: TDateTime);
    function GetDateModified: TDateTime;
    function GetComment: string;
    function GetCommentRTF: string;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetHighlighter: string;
    function GetID: Integer;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeID: Integer;
    function GetParentID: Integer;
    function GetText: string;
    procedure SetComment(AValue: string);
    procedure SetCommentRTF(AValue: string);
    procedure SetFoldLevel(AValue: Integer);
    procedure SetFoldState(AValue: string);
    procedure SetHighlighter(AValue: string);
    procedure SetImageIndex(AValue: Integer);
    procedure SetNodeName(AValue: string);
    procedure SetNodePath(AValue: string);
    procedure SetNodeTypeID(AValue: Integer);
    procedure SetParentID(AValue: Integer);
    procedure SetText(AValue: string);
    function GetFileName: string;
    procedure SetFileName(AValue: string);
    function GetActive: Boolean;
    function GetDataSet: TDataSet;
    function GetRecordCount: Integer;
    procedure SetActive(AValue: Boolean);
    function GetLookupDataSet: TDataSet;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadGlyphs;
    procedure Execute(const ASQL: string);
    procedure CreateNewDatabase;
    function Post: Boolean;
    procedure Append;
    function Edit: Boolean;

    procedure LocateHighlighter;

    procedure DisableControls;
    procedure EnableControls;

    procedure Lookup(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );

    property ImageList: TImageList
      read GetImageList;

    property GlyphList: TImageList
      read GetGlyphList;

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property FileName: string
      read GetFileName write SetFileName;

    { ISnippet }
    property Comment: string
      read GetComment write SetComment;

    property CommentRTF: string
      read GetCommentRTF write SetCommentRTF;

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

    property ID: Integer
      read GetID;

    property NodeName: string
      read GetNodeName write SetNodeName;

    property NodePath: string
      read GetNodePath write SetNodePath;

    property NodeTypeID: Integer read
      GetNodeTypeID write SetNodeTypeID;

    property ParentID: Integer
      read GetParentID write SetParentID;

    property Text: string
      read GetText write SetText;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TDataSet
      read GetDataSet;

    property LookupDataSet: TDataSet
      read GetLookupDataSet;
  end;

implementation

{$R *.lfm}

uses
  Variants, Forms;

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
procedure TdmSnippetSourceOld.AfterConstruction;
begin
  inherited AfterConstruction;
  conMain.Directory    := ExtractFileDir(Application.ExeName);
  conMain.DatabaseName := 'snippets.db';
  conMain.Connected    := True;
//  trsSnippet.Active    := True;
  qryNodeType.Active    := True;
  qryHighlighter.Active := True;
  DataSet.Active       := True;


  //Execute('PRAGMA synchronous = 1;'); // speeds up inserts
  //
  ////ExecuteDirect('PRAGMA synchronous = 0;');
  ////ExecuteDirect('PRAGMA journal_mode = WAL;');
  //Execute('PRAGMA journal_mode = OFF;');
  //Execute('PRAGMA locking_mode = EXCLUSIVE;');
  //Execute('PRAGMA temp_store = MEMORY;');
  //Execute('PRAGMA count_changes = OFF;');
  //Execute('PRAGMA PAGE_SIZE = 4096;');
  //Execute('PRAGMA default_cache_size = 700000;');
  //Execute('PRAGMA cache_size = 700000;');
  //Execute('PRAGMA automatic_index = 1;');


//  LoadGlyphs;
//  BuildImageList(GlyphDataSet, imlGlyphs);
end;

procedure TdmSnippetSourceOld.BeforeDestruction;
begin
  qrySnippet.ApplyUpdates;
  trsSnippet.Commit;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TdmSnippetSourceOld.GetActive: Boolean;
begin
  Result := qrySnippet.Active;
end;

procedure TdmSnippetSourceOld.SetActive(AValue: Boolean);
begin
  qrySnippet.Active := AValue;
end;

function TdmSnippetSourceOld.GetLookupDataSet: TDataSet;
begin
  Result := qrySnippet;
end;

function TdmSnippetSourceOld.GetImageIndex: Integer;
begin
  Result := qrySnippetImageIndex.AsInteger;
end;

procedure TdmSnippetSourceOld.SetDateCreated(AValue: TDateTime);
begin
  qrySnippetDateCreated.AsDateTime := AValue;
end;

function TdmSnippetSourceOld.GetDateCreated: TDateTime;
begin
  Result := qrySnippetDateCreated.AsDateTime;
end;

procedure TdmSnippetSourceOld.SetDateModified(AValue: TDateTime);
begin
  qrySnippetDateModified.AsDateTime := AValue;
end;

function TdmSnippetSourceOld.GetDateModified: TDateTime;
begin
  Result := qrySnippetDateModified.AsDateTime;
end;

function TdmSnippetSourceOld.GetComment: string;
begin
  Result := qrySnippetComment.AsString;
end;

function TdmSnippetSourceOld.GetCommentRTF: string;
begin
  Result := qrySnippetCommentRTF.AsString;
end;

function TdmSnippetSourceOld.GetFoldLevel: Integer;
begin
  Result := qrySnippetFoldLevel.AsInteger;
end;

function TdmSnippetSourceOld.GetFoldState: string;
begin
  Result := qrySnippetFoldState.AsString;
end;

function TdmSnippetSourceOld.GetHighlighter: string;
begin
  Result := FHighlighter;
end;

function TdmSnippetSourceOld.GetID: Integer;
begin
  Result := qrySnippetId.AsInteger;
end;

function TdmSnippetSourceOld.GetNodeName: string;
begin
  Result := qrySnippetNodeName.AsString;
end;

function TdmSnippetSourceOld.GetNodePath: string;
begin
  Result := qrySnippetNodePath.AsString;
end;

function TdmSnippetSourceOld.GetNodeTypeID: Integer;
begin
  Result := qrySnippetNodeTypeId.AsInteger;
end;

function TdmSnippetSourceOld.GetParentID: Integer;
begin
  Result := qrySnippetParentId.AsInteger;
end;

function TdmSnippetSourceOld.GetText: string;
begin
  Result := qrySnippetText.AsString;
end;

procedure TdmSnippetSourceOld.SetComment(AValue: string);
begin
  qrySnippetComment.AsString := AValue;
end;

procedure TdmSnippetSourceOld.SetCommentRTF(AValue: string);
begin
  qrySnippetCommentRTF.AsString := AValue;
end;

procedure TdmSnippetSourceOld.SetFoldLevel(AValue: Integer);
begin
  qrySnippetFoldLevel.AsInteger := AValue;
end;

procedure TdmSnippetSourceOld.SetFoldState(AValue: string);
begin
  qrySnippetFoldState.AsString := AValue;
end;

procedure TdmSnippetSourceOld.SetHighlighter(AValue: string);
begin
  if AValue <> Highlighter then
  begin
    FHighlighter := AValue;
    if not qryHighlighter.Active then
      qryHighlighter.Active := True;
    qrySnippetHighlighterId.AsInteger :=
      qryHighlighter.Lookup('Code', VarArrayOf([AValue]), 'Id');
  end;
end;

procedure TdmSnippetSourceOld.SetImageIndex(AValue: Integer);
begin
  qrySnippetImageIndex.AsInteger := AValue;
end;

procedure TdmSnippetSourceOld.SetNodeName(AValue: string);
begin
  qrySnippetNodeName.AsString := AValue;
end;

procedure TdmSnippetSourceOld.SetNodePath(AValue: string);
begin
  qrySnippetNodePath.AsString := AValue;
end;

procedure TdmSnippetSourceOld.SetNodeTypeID(AValue: Integer);
begin
  qrySnippetNodeTypeId.AsInteger := AValue;
end;

procedure TdmSnippetSourceOld.SetParentID(AValue: Integer);
begin
  qrySnippetParentId.AsInteger := AValue;
end;

procedure TdmSnippetSourceOld.SetText(AValue: string);
begin
  qrySnippetText.AsString := AValue;
end;

function TdmSnippetSourceOld.GetDataSet: TDataSet;
begin
  Result := qrySnippet;
end;

function TdmSnippetSourceOld.GetRecordCount: Integer;
begin
  Result := qrySnippet.RecordCount;
end;

function TdmSnippetSourceOld.GetGlyphDataSet: TDataSet;
begin
  Result := qryGlyph;
end;

function TdmSnippetSourceOld.GetGlyphList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSourceOld.GetImageList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSourceOld.GetFileName: string;
begin
  Result := conMain.DatabaseName;
end;

procedure TdmSnippetSourceOld.SetFileName(AValue: string);
begin
  if AValue <> FileName then
  begin
    conMain.Connected    := False;
    conMain.DatabaseName := AValue;
    conMain.Connected    := True;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmSnippetSourceOld.conMainAfterConnect(Sender: TObject);
begin
  qryGlyph.Active := True;
end;

procedure TdmSnippetSourceOld.conMainLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  Logger.Send('SQL = %s', [Msg]);
end;

procedure TdmSnippetSourceOld.qrySnippetAfterInsert(DataSet: TDataSet);
begin
  FInserted := True;
end;

procedure TdmSnippetSourceOld.qrySnippetAfterPost(DataSet: TDataSet);
var
  LId : Integer;
begin
  qrySnippet.DisableControls;
  try
    LId := qrySnippetId.Value;
    qrySnippet.ApplyUpdates;
    trsSnippet.Commit; // will close the dataset and the transaction
    qrySnippet.Active := True; // will start a new transaction
    qrySnippet.Refresh;
  finally
    qrySnippet.EnableControls;
    if FInserted then
    begin
      qrySnippet.Last;
      FInserted := False;
    end
    else
    begin
      qrySnippet.Locate('Id', VarArrayOf([LId]), []);
    end;
  end;
end;

procedure TdmSnippetSourceOld.qrySnippetAfterScroll(DataSet: TDataSet);
begin
  if qryNodeType.Active then
  begin
    qryNodeType.Locate(
      'ID',
      VarArrayOf(
        [qrySnippetNodeTypeId.Value]
      ),
      []
    );
  end;
  LocateHighlighter;
  FHighlighter := QueryLookup(
    conMain,
    'select Code from Highlighter where ID = %d',
    [qrySnippetHighlighterId.AsInteger]
  );
  if not qryGlyph.Active then
    qryGlyph.Active := True;
  //qryGlyph.Locate(
  //  'ID',
  //  VarArrayOf(
  //    [qrySnippetGlyphID.Value]
  //  ),
  //  []
  //);
end;

procedure TdmSnippetSourceOld.qrySnippetBeforeScroll(DataSet: TDataSet);
begin
  if DataSet.State in dsEditModes then
    DataSet.Post;
end;

procedure TdmSnippetSourceOld.qrySnippetNewRecord(DataSet: TDataSet);
begin
  qrySnippetId.Value := 0;
end;

procedure TdmSnippetSourceOld.qrySnippetTextGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  aText := Sender.AsString;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TdmSnippetSourceOld.LoadGlyphs;
begin
//  raise Exception.Create('Not implemented!');
end;

procedure TdmSnippetSourceOld.Execute(const ASQL: string);
begin
  conMain.ExecuteDirect(ASQL);
end;

procedure TdmSnippetSourceOld.CreateNewDatabase;
begin
  scrCreateDatabase.ExecuteScript;
end;

function TdmSnippetSourceOld.Post: Boolean;
begin
  if DataSet.State in dsEditModes then
  begin
    DataSet.Post;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdmSnippetSourceOld.Append;
begin
  DataSet.Append;
end;

function TdmSnippetSourceOld.Edit: Boolean;
begin
  if not (DataSet.State in dsEditModes) then
  begin
    DataSet.Edit;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdmSnippetSourceOld.LocateHighlighter;
begin
  if DataSet.Active and qryHighlighter.Active then
  begin
    FHighlighter :=
      qryHighlighter.Lookup(
        'Id',
        VarArrayOf([qrySnippetHighlighterId.Value]),
        'Code'
      );
  end;
end;

procedure TdmSnippetSourceOld.DisableControls;
begin
  DataSet.DisableControls;
end;

procedure TdmSnippetSourceOld.EnableControls;
begin
  DataSet.EnableControls;
end;

procedure TdmSnippetSourceOld.Lookup(const ASearchString: string;
  ASearchInText: Boolean; ASearchInName: Boolean; ASearchInComment: Boolean);
var
  R : Variant;
begin
  R := QueryLookup(
  conMain,
  'select ID from Snippet where Text like ''%%%s%%'' limit 1',
    [ASearchString]
  );
  DataSet.Locate('ID', VarArrayOf([R]), []);
end;
{$ENDREGION}

end.

