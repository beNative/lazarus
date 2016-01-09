{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

{ This unit is intended as a replacement for SnippetSource_Modules_Data. It
  uses the standard Lazarus SQLDB components. }

unit SnippetSource_Modules_Data2;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, ImgList, Controls,

  sqlite3conn, sqldb, sqldblib, db, FileUtil,

  ts.Core.Sharedlogger,

  SnippetSource.Interfaces;

type

  { TdmSnippetSource }

  TdmSnippetSource = class(TDataModule,
    IConnection, ISnippet, IDataSet, ILookup, IGlyphs
  )
    {$region 'designer controls' /fold}
    conMain                  : TSQLite3Connection;
    imlGlyphs                : TImageList;
    imlMain                  : TImageList;
    qryGlyphID               : TLongintField;
    qryGlyphImage            : TBlobField;
    qryGlyphImageIndex       : TLongintField;
    qryGlyphName             : TMemoField;
    qryNodeTypeID            : TLongintField;
    qryNodeTypeImageIndex    : TLongintField;
    qryNodeTypeName          : TMemoField;
    qrySnippet               : TSQLQuery;
    qrySnippetComment        : TMemoField;
    qrySnippetCommentRTF     : TMemoField;
    qrySnippetDateCreated    : TDateTimeField;
    qrySnippetDateModified   : TDateTimeField;
    qrySnippetFoldLevel      : TLongintField;
    qrySnippetFoldState      : TStringField;
    qrySnippetHighlighterID  : TLongintField;
    qrySnippetID             : TLongintField;
    qrySnippetImageIndex     : TLongintField;
    qrySnippetNodeName       : TStringField;
    qrySnippetNodePath       : TWideStringField;
    qrySnippetNodeTypeID     : TLongintField;
    qrySnippetParentID       : TLongintField;
    qrySnippetText           : TMemoField;
    qryGlyph                 : TSQLQuery;
    qryNodeType              : TSQLQuery;
    scrCreateDatabase        : TSQLScript;
    trsSnippet               : TSQLTransaction;
    {$endregion}

    {$region 'event handlers' /fold}
    procedure conMainAfterConnect(Sender: TObject);
    procedure conMainLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
    procedure qrySnippetAfterEdit(DataSet: TDataSet);
    procedure qrySnippetAfterInsert(DataSet: TDataSet);
    procedure qrySnippetAfterPost(DataSet: TDataSet);
    procedure qrySnippetAfterScroll(DataSet: TDataSet);
    procedure qrySnippetBeforeScroll(DataSet: TDataSet);
    procedure qrySnippetNewRecord(DataSet: TDataSet);
    {$endregion}

  private
    FNeedsRefresh : Boolean;
    FInserted     : Boolean;

    {$region 'property access methods' /fold}
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
    {$endregion}

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure LoadGlyphs;
    function Execute(const ASQL: string): Boolean;
    procedure CreateNewDatabase;
    function Post: Boolean;
    function Append: Boolean;
    function Edit: Boolean;

    procedure DisableControls;
    procedure EnableControls;

    procedure Lookup(
      const ASearchString    : string;
            ASearchInText    : Boolean;
            ASearchInName    : Boolean;
            ASearchInComment : Boolean
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

{$region 'construction and destruction' /fold}
procedure TdmSnippetSource.AfterConstruction;
begin
  inherited AfterConstruction;
  conMain.Directory := ExtractFileDir(Application.ExeName);
  conMain.DatabaseName := 'snippets.db';
  conMain.Connected := True;
  trsSnippet.Active := True;
  DataSet.Active := True;


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

  qryNodeType.Active    := True;
//  LoadGlyphs;
//  BuildImageList(GlyphDataSet, imlGlyphs);
end;

procedure TdmSnippetSource.BeforeDestruction;
begin
  qrySnippet.ApplyUpdates;
  trsSnippet.Commit;
  inherited BeforeDestruction;
end;

{$endregion}

{$region 'property access mehods' /fold}
function TdmSnippetSource.GetActive: Boolean;
begin
  Result := qrySnippet.Active;
end;

procedure TdmSnippetSource.SetActive(AValue: Boolean);
begin
  qrySnippet.Active := AValue;
end;

function TdmSnippetSource.GetLookupDataSet: TDataSet;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetImageIndex: Integer;
begin
  Result := qrySnippetImageIndex.AsInteger;
end;

procedure TdmSnippetSource.SetDateCreated(AValue: TDateTime);
begin
  qrySnippetDateCreated.AsDateTime := AValue;
end;

function TdmSnippetSource.GetDateCreated: TDateTime;
begin
  Result := qrySnippetDateCreated.AsDateTime;
end;

procedure TdmSnippetSource.SetDateModified(AValue: TDateTime);
begin
  qrySnippetDateModified.AsDateTime := AValue;
end;

function TdmSnippetSource.GetDateModified: TDateTime;
begin
  Result := qrySnippetDateModified.AsDateTime;
end;

function TdmSnippetSource.GetComment: string;
begin
  Result := qrySnippetComment.AsString;
end;

function TdmSnippetSource.GetCommentRTF: string;
begin
  Result := qrySnippetCommentRTF.AsString;
end;

function TdmSnippetSource.GetFoldLevel: Integer;
begin
  Result := qrySnippetFoldLevel.AsInteger;
end;

function TdmSnippetSource.GetFoldState: string;
begin
  Result := qrySnippetFoldState.AsString;
end;

function TdmSnippetSource.GetHighlighter: string;
begin
  Result := QueryLookup(
    conMain,
    'select Code from Highlighter where ID = %d',
    [qrySnippetHighlighterID.AsInteger]
  );
  Logger.Send('Highlighter', Result);
end;

function TdmSnippetSource.GetID: Integer;
begin
  Result := qrySnippetID.AsInteger;
end;

function TdmSnippetSource.GetNodeName: string;
begin
  Result := qrySnippetNodeName.AsString;
end;

function TdmSnippetSource.GetNodePath: string;
begin
  Result := qrySnippetNodePath.AsString;
end;

function TdmSnippetSource.GetNodeTypeID: Integer;
begin
  Result := qrySnippetNodeTypeID.AsInteger;
end;

function TdmSnippetSource.GetParentID: Integer;
begin
  Result := qrySnippetParentID.AsInteger;
end;

function TdmSnippetSource.GetText: string;
begin
  Result := qrySnippetText.AsString;
end;

procedure TdmSnippetSource.SetComment(AValue: string);
begin
  qrySnippetComment.AsString := AValue;
end;

procedure TdmSnippetSource.SetCommentRTF(AValue: string);
begin
  qrySnippetCommentRTF.AsString := AValue;
end;

procedure TdmSnippetSource.SetFoldLevel(AValue: Integer);
begin
  qrySnippetFoldLevel.AsInteger := AValue;
end;

procedure TdmSnippetSource.SetFoldState(AValue: string);
begin
  qrySnippetFoldState.AsString := AValue;
end;

procedure TdmSnippetSource.SetHighlighter(AValue: string);
begin
  Logger.Send('SetHighlighter', AValue);
  qrySnippetHighlighterID.AsInteger :=
    QueryLookup(
      conMain,
      'select ID from Highlighter where Code = ''%s''',
      [AValue]
    );
end;

procedure TdmSnippetSource.SetImageIndex(AValue: Integer);
begin
  qrySnippetImageIndex.AsInteger := AValue;
end;

procedure TdmSnippetSource.SetNodeName(AValue: string);
begin
  qrySnippetNodeName.AsString := AValue;
end;

procedure TdmSnippetSource.SetNodePath(AValue: string);
begin
  qrySnippetNodePath.AsString := AValue;
end;

procedure TdmSnippetSource.SetNodeTypeID(AValue: Integer);
begin
  qrySnippetNodeTypeID.AsInteger := AValue;
end;

procedure TdmSnippetSource.SetParentID(AValue: Integer);
begin
  qrySnippetParentID.AsInteger := AValue;
end;

procedure TdmSnippetSource.SetText(AValue: string);
begin
  qrySnippetText.AsString := AValue;
end;

function TdmSnippetSource.GetDataSet: TDataSet;
begin
  Result := qrySnippet;
end;

function TdmSnippetSource.GetRecordCount: Integer;
begin
  Result := qrySnippet.RecordCount;
end;

function TdmSnippetSource.GetGlyphDataSet: TDataSet;
begin
  Result := qryGlyph;
end;

function TdmSnippetSource.GetGlyphList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmSnippetSource.GetImageList: TImageList;
begin
  Result := imlGlyphs;
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
{$endregion}

{$region 'event handlers' /fold}
procedure TdmSnippetSource.conMainAfterConnect(Sender: TObject);
begin
   qryGlyph.Active := True;
end;

procedure TdmSnippetSource.conMainLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  Logger.Send(Msg);
end;

procedure TdmSnippetSource.qrySnippetAfterEdit(DataSet: TDataSet);
begin
  //FNeedsRefresh := True;
end;

procedure TdmSnippetSource.qrySnippetAfterInsert(DataSet: TDataSet);
begin
  FInserted := True;
end;

procedure TdmSnippetSource.qrySnippetAfterPost(DataSet: TDataSet);
var
  N : Integer;
begin
  qrySnippet.DisableControls;
  try
    N := qrySnippet.RecNo;
    qrySnippet.ApplyUpdates;
    trsSnippet.Commit; // will close the dataset and the transaction
    qrySnippet.Active := True; // will start a new transaction
    qrySnippet.Refresh;
    if FNeedsRefresh then
    begin

      //qrySnippet.Refresh;
  //    qrySnippet.EnableControls;
      //FNeedsRefresh := False;
    end
    else
    begin
      //if N < qrySnippet.RecordCount then
      //  qrySnippet.RecNo := N;
    end;

    //else
  //    qrySnippet.Resync([rmExact]);
    //
    //trsSnippet.Commit;

  finally
    qrySnippet.EnableControls;
    if FInserted then
    begin
      qrySnippet.Last;
      FInserted := False;
    end
    else
    begin
      if N < qrySnippet.RecordCount then
        qrySnippet.RecNo := N;
    end;

  end;

end;

procedure TdmSnippetSource.qrySnippetAfterScroll(DataSet: TDataSet);
begin
  if not qryNodeType.Active then
    qryNodeType.Active := True;
  qryNodeType.Locate(
    'ID',
    VarArrayOf(
      [qrySnippetNodeTypeID.Value]
    ),
    []
  );
  //if not qryGlyph.Active then
  //  qryGlyph.Active := True;
  //qryGlyph.Locate(
  //  'ID',
  //  VarArrayOf(
  //    [qrySnippetGlyphID.Value]
  //  ),
  //  []
  //);
end;

procedure TdmSnippetSource.qrySnippetBeforeScroll(DataSet: TDataSet);
begin
  if DataSet.State in dsEditModes then
    DataSet.Post;
end;

procedure TdmSnippetSource.qrySnippetNewRecord(DataSet: TDataSet);
begin
  qrySnippetID.Value := 0;
  FNeedsRefresh := True;
end;
{$endregion}

{$region 'private methods' /fold}
{$endregion}

{$region 'protected methods' /fold}
{$endregion}

{$region 'public methods' /fold}
procedure TdmSnippetSource.LoadGlyphs;
begin
  raise Exception.Create('Not implemented!');
end;

function TdmSnippetSource.Execute(const ASQL: string): Boolean;
begin
  conMain.ExecuteDirect(ASQL);
end;

procedure TdmSnippetSource.CreateNewDatabase;
begin
  scrCreateDatabase.ExecuteScript;
end;

function TdmSnippetSource.Post: Boolean;
begin
  if DataSet.State in dsEditModes then
  begin
    DataSet.Post;
    Result := True;
  end
  else
    Result := False;
end;

function TdmSnippetSource.Append: Boolean;
begin
  DataSet.Append;
end;

function TdmSnippetSource.Edit: Boolean;
begin
  if not (DataSet.State in dsEditModes) then
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

procedure TdmSnippetSource.Lookup(const ASearchString: string;
  ASearchInText: Boolean; ASearchInName: Boolean; ASearchInComment: Boolean);
begin
  raise Exception.Create('Not implemented!');
end;
{$endregion}

end.

