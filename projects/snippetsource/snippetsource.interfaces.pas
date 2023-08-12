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

unit SnippetSource.Interfaces;

{$MODE DELPHI}

{ Main application interfaces. }

interface

uses
  Classes, SysUtils, Controls, Graphics,

  DB, SQLDB;

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

    procedure ConnectToDatabase(const AFileName: string);
    function BackupDatabase: string;
    procedure CreateDatabaseTables;
    procedure CreateDatabaseIndexes;
    procedure CreateNewDatabase;
    procedure SetupConfigurationData;
    procedure BeginBulkInserts;
    procedure EndBulkInserts;

    procedure ExecuteDirect(const ASQL: string);
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

  { Interface used to perform arbitrary queries on the Snippets database.  }

  IQuery = interface
  ['{4969FE0E-1003-4D39-B181-E2D46DFB9AE0}']
    {$REGION 'property access methods'}
    function GetQuery: TSQLQuery;
    {$ENDREGION}

    procedure Execute(const ASQL: string);
    function LastId: Integer;
    function QueryValue(const ASQL: string): Variant;

    property Query: TSQLQuery
      read GetQuery;
  end;

  { Interface to SQLite specific functions and data. }

  ISQLite = interface
  ['{334F8C6C-B0C9-4A40-BA70-DEBAFAAE9442}']
    {$REGION 'property access mehods'}
    function GetVersion: string;
    function GetSize: Int64;
    {$ENDREGION}

    function IntegrityCheck: Boolean;
    procedure ShrinkMemory;
    procedure Vacuum;

    property Size: Int64
      read GetSize;

    property Version: string
      read GetVersion;
  end;

  ISnippet = interface
  ['{72ECC77F-765D-417E-ABCE-D78355A53CB7}']
  function GetActiveViews: string;
    {$REGION 'property access mehods'}
    function GetHtmlText: string;
    function GetHtmlData: string;
    function GetImage: TBitmap;
    function GetLocked: Boolean;
    function GetRtfText: string;
    function GetRtfData: string;
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
    function GetSource: string;
    function GetText: string;
    procedure SetActiveViews(AValue: string);
    procedure SetHtmlData(AValue: string);
    procedure SetHtmlText(AValue: string);
    procedure SetImage(AValue: TBitmap);
    procedure SetLocked(AValue: Boolean);
    procedure SetRtfText(AValue: string);
    procedure SetRtfData(AValue: string);
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
    procedure SetSource(AValue: string);
    procedure SetText(AValue: string);
    {$ENDREGION}

    property ActiveViews: string
      read GetActiveViews write SetActiveViews;

    property Locked: Boolean
      read GetLocked write SetLocked;

    property RtfText: string
      read GetRtfText write SetRtfText;

    property RtfData: string
      read GetRtfData write SetRtfData;

    property HtmlText: string
      read GetHtmlText write SetHtmlText;

    property HtmlData: string
      read GetHtmlData write SetHtmlData;

    property Source: string
      read GetSource write SetSource;

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

    property Image: TBitmap
      read GetImage write SetImage;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;
  end;

  { Interfaces the main dataset which is used to display the snippets tree.  }

  IDataSet = interface
  ['{13211D24-9ECD-42BE-AB95-C4F833D123E6}']
    {$REGION 'property access mehods'}
    function GetActive: Boolean;
    function GetDataSet: TSQLQuery;
    function GetRecordCount: Integer;
    procedure SetActive(AValue: Boolean);
    {$ENDREGION}

    function Append: Boolean;
    function Edit: Boolean;
    function Post: Boolean;
    function ApplyUpdates: Boolean;
    procedure DuplicateRecords(AValues: TStrings);
    procedure MoveDownRecords(AValues: TStrings);
    procedure MoveUpRecords(AValues: TStrings);

    procedure EnableControls;
    procedure DisableControls;

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TSQLQuery
      read GetDataSet;
  end;

  ISearch = interface
  ['{6B45FCDB-7F36-450D-9C2E-820C69204F4D}']
    {$REGION 'property access mehods'}
    function GetSearchDataSet: TSQLQuery;
    {$ENDREGION}

    procedure Search(
      const ASearchString : string;
      ASearchInText       : Boolean;
      ASearchInName       : Boolean;
      ASearchInComment    : Boolean
    );

    property SearchDataSet: TSQLQuery
      read GetSearchDataSet;
  end;

  IGlyphs = interface
  ['{E3C86684-4FD7-4EB5-8097-06ED826061C8}']
    {$REGION 'property access mehods'}
    function GetGlyphDataSet: TDataSet;
    function GetImageList: TImageList;
    {$ENDREGION}

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property ImageList: TImageList
      read GetImageList;
  end;

  IHighlighters = interface
  ['{AF44F562-9439-43EA-BDDA-F3918BDC0083}']
    function GetHighlighterDataSet: TDataSet;

    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;
  end;

  { Holds persistable application settings. }

  { ISettings }

  ISettings = interface
  ['{60E1B364-44E0-4A91-B12B-EF21059AC8C9}']
    {$REGION 'property access methods'}
    function GetAutoHideEditorToolBar: Boolean;
    function GetAutoHideRichEditorToolBar: Boolean;
    function GetDatabase: string;
    function GetDebugMode: Boolean;
    function GetDefaultRichEditorFontName: string;
    function GetEmitLogMessages: Boolean;
    function GetHtmlEditMode: Boolean;
    function GetHtmlSourceVisible: Boolean;
    function GetLastFocusedId: Integer;
    function GetPythonVirtualEnvironmentName: string;
    procedure SetAutoHideEditorToolBar(AValue: Boolean);
    procedure SetAutoHideRichEditorToolBar(AValue: Boolean);
    procedure SetDatabase(const AValue: string);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDefaultRichEditorFontName(AValue: string);
    procedure SetEmitLogMessages(AValue: Boolean);
    procedure SetHtmlEditMode(AValue: Boolean);
    procedure SetHtmlSourceVisible(AValue: Boolean);
    procedure SetLastFocusedId(AValue: Integer);
    {$ENDREGION}

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure SetPythonVirtualEnvironmentName(AValue: string);

    property Database: string
      read GetDatabase write SetDatabase;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property HtmlEditMode: Boolean
      read GetHtmlEditMode write SetHtmlEditMode;

    property HtmlSourceVisible: Boolean
      read GetHtmlSourceVisible write SetHtmlSourceVisible;

    property LastFocusedId: Integer
      read GetLastFocusedId write SetLastFocusedId;

    property AutoHideEditorToolBar: Boolean
      read GetAutoHideEditorToolBar write SetAutoHideEditorToolBar;

    property AutoHideRichEditorToolBar: Boolean
      read GetAutoHideRichEditorToolBar write SetAutoHideRichEditorToolBar;

    property DefaultRichEditorFontName: string
      read GetDefaultRichEditorFontName write SetDefaultRichEditorFontName;

    property EmitLogMessages: Boolean
      read GetEmitLogMessages write SetEmitLogMessages;

    property PythonVirtualEnvironmentName: string
      read GetPythonVirtualEnvironmentName write SetPythonVirtualEnvironmentName;
  end;

implementation

end.

