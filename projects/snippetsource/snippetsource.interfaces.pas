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

unit SnippetSource.Interfaces;

{$MODE DELPHI}

{ Main application interfaces. }

interface

uses
  Classes, SysUtils, DB, Controls;

type
  IConnection = interface
  ['{8F9C0BCC-16D3-49A3-984A-415520289A2F}']
    function GetFileName: string;
    procedure SetFileName(AValue: string);

    function Execute(const ASQL: string): Boolean;
    procedure CreateNewDatabase;

    property FileName: string
      read GetFileName write SetFileName;
  end;

  ISQLiteSettings = interface
  ['{334F8C6C-B0C9-4A40-BA70-DEBAFAAE9442}']
    function GetAutomaticIndex: Boolean;
    function GetAutoVacuum: Boolean;
    procedure SetAutomaticIndex(AValue: Boolean);
    procedure SetAutoVacuum(AValue: Boolean);

    property AutoVacuum: Boolean
      read GetAutoVacuum write SetAutoVacuum;

    property AutomaticIndex: Boolean
      read GetAutomaticIndex write SetAutomaticIndex;
  end;

  ISnippet = interface
  ['{72ECC77F-765D-417E-ABCE-D78355A53CB7}']
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
  end;

  IDataSet = interface
  ['{13211D24-9ECD-42BE-AB95-C4F833D123E6}']
    function GetActive: Boolean;
    function GetDataSet: TDataSet;
    function GetRecordCount: Integer;
    function Post: Boolean;
    function Append: Boolean;
    function Edit: Boolean;

    procedure DisableControls;
    procedure EnableControls;
    procedure SetActive(AValue: Boolean);

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSet: TDataSet
      read GetDataSet;
  end;

  ILookup = interface
  ['{6B45FCDB-7F36-450D-9C2E-820C69204F4D}']
    function GetLookupDataSet: TDataSet;
    procedure Lookup(
      const ASearchString    : string;
            ASearchInText    : Boolean;
            ASearchInName    : Boolean;
            ASearchInComment : Boolean
    );
    property LookupDataSet: TDataSet
      read GetLookupDataSet;
  end;

  IGlyphs = interface
  ['{E3C86684-4FD7-4EB5-8097-06ED826061C8}']
    function GetGlyphDataSet: TDataSet;
    function GetGlyphList: TImageList;
    function GetImageList: TImageList;

    procedure LoadGlyphs;

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property ImageList: TImageList
      read GetImageList;

    property GlyphList: TImageList
      read GetGlyphList;
  end;

implementation

end.

