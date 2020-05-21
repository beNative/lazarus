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

unit SnippetSource.Interfaces;

{$MODE DELPHI}

{ Main application interfaces. }

interface

uses
  Classes, SysUtils, Controls,

  db, sqldb;

type
  IConnection = interface
  ['{8F9C0BCC-16D3-49A3-984A-415520289A2F}']
    {$REGION 'property access mehods'}
    function GetAutoApplyUpdates: Boolean;
    function GetAutoCommit: Boolean;
    function GetFileName: string;
    procedure SetAutoApplyUpdates(AValue: Boolean);
    procedure SetAutoCommit(AValue: Boolean);
    procedure SetFileName(AValue: string);
    {$ENDREGION}

    procedure CreateNewDatabase;
    procedure Execute(const ASQL: string);
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
    procedure EndTransaction;

    property AutoApplyUpdates: Boolean
      read GetAutoApplyUpdates write SetAutoApplyUpdates;

    property AutoCommit: Boolean
      read GetAutoCommit write SetAutoCommit;

    property FileName: string
      read GetFileName write SetFileName;
  end;

  ISQLite = interface
  ['{334F8C6C-B0C9-4A40-BA70-DEBAFAAE9442}']
  function GetDBVersion: string;
    {$REGION 'property access mehods'}
    function GetReadOnly: Boolean;
    function GetSize: Int64;
    procedure SetReadOnly(AValue: Boolean);
    {$ENDREGION}

    function IntegrityCheck: Boolean;
    procedure ShrinkMemory;
    procedure Vacuum;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property Size: Int64
      read GetSize;

    property DBVersion: string
      read GetDBVersion;
  end;

  ISnippet = interface
  ['{72ECC77F-765D-417E-ABCE-D78355A53CB7}']
    {$REGION 'property access mehods'}
    function GetComment: string;
    function GetCommentRtf: string;
    function GetDateCreated: TDateTime;
    function GetDateModified: TDateTime;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetHighlighter: string;
    function GetId: Integer;
    function GetImageIndex: Integer;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeId: Integer;
    function GetParentId: Integer;
    function GetText: string;
    procedure SetComment(AValue: string);
    procedure SetCommentRtf(AValue: string);
    procedure SetDateCreated(AValue: TDateTime);
    procedure SetDateModified(AValue: TDateTime);
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
  end;

  IDataSet = interface
  ['{13211D24-9ECD-42BE-AB95-C4F833D123E6}']
    {$REGION 'property access mehods'}
    function GetActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function GetDataSet: TSQLQuery;
    function GetRecordCount: Integer;
    {$ENDREGION}

    function Post: Boolean;
    function Append: Boolean;
    function Edit: Boolean;
    function ApplyUpdates: Boolean;

    procedure EnableControls;
    procedure DisableControls;
    procedure BeginBulkInserts;
    procedure EndBulkInserts;

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TSQLQuery
      read GetDataSet;
  end;

  ILookup = interface
  ['{6B45FCDB-7F36-450D-9C2E-820C69204F4D}']
    {$REGION 'property access mehods'}
    function GetLookupDataSet: TDataSet;
    {$ENDREGION}

    procedure Lookup(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );

    property LookupDataSet: TDataSet
      read GetLookupDataSet;
  end;

  IGlyphs = interface
  ['{E3C86684-4FD7-4EB5-8097-06ED826061C8}']
    {$REGION 'property access mehods'}
    function GetGlyphDataSet: TDataSet;
    //function GetGlyphList: TImageList;
    function GetImageList: TImageList;
    {$ENDREGION}

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property ImageList: TImageList
      read GetImageList;
    //
    //property GlyphList: TImageList
    //  read GetGlyphList;
  end;

  IHighlighters = interface
  ['{AF44F562-9439-43EA-BDDA-F3918BDC0083}']
    function GetHighlighterDataSet: TDataSet;

    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;
  end;

  ISettings = interface
  ['{60E1B364-44E0-4A91-B12B-EF21059AC8C9}']
    function GetDataBase: string;
    procedure SetDataBase(const AValue: string);

    property DataBase: string
      read GetDataBase write SetDataBase;
  end;

implementation

end.

