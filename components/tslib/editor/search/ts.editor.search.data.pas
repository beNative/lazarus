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

unit ts.Editor.Search.Data;

{ Data structures used to display the search results hierarchically. }

{ TSearchResultGroup -> TSearchResultLine -> TSearchResult }

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TSearchResult }

  TSearchResult = class(TPersistent)
  private
    FIndex      : Integer;
    FLine       : Integer;
    FColumn     : Integer;
    FBlockBegin : TPoint;
    FBlockEnd   : TPoint;
    FFileName   : string;
    FMatch      : string;
    FShowMatch  : Boolean;
    FStartPos   : Integer;
    FEndPos     : Integer;
    FViewName   : string;

    function GetText: string;

  public
    property BlockBegin : TPoint
      read FBlockBegin write FBlockBegin;

    property BlockEnd : TPoint
      read FBlockEnd write FBlockEnd;

    property ViewName: string
      read FViewName write FViewName;

    property FileName : string
      read FFileName write FFileName;

    property Index: Integer
      read FIndex write FIndex;

    property Line : Integer
      read FLine write FLine;

    property Column : Integer
      read FColumn write FColumn;

    property StartPos: Integer
      read FStartPos write FStartPos;

    property EndPos: Integer
      read FEndPos write FEndPos;

    property Match: string
      read FMatch write FMatch;

    property ShowMatch: Boolean
      read FShowMatch write FShowMatch;

  published
    property Text: string
      read GetText;

  end;

  { TSearchResultLine }

  TSearchResultLine = class(TPersistent)
  private
    FLine : Integer;
    FList : TObjectList;
    function GetText: string;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property List: TObjectList
      read FList;

    property Line : Integer
      read FLine write FLine;

  published
    property Text: string
      read GetText;

  end;

  { TSearchResultGroup }

  TSearchResultGroup = class(TPersistent)
  private
    FLines    : TObjectList;
    FFileName : string;
    FViewName : string;

    function GetText: string;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Lines: TObjectList
      read FLines;

    property ViewName: string
      read FViewName write FViewName;

    property FileName : string
      read FFileName write FFileName;

  published
    property Text: string
      read GetText;

  end;

implementation

resourcestring
  SPosition = 'Position: (%d, %d)';
  SMatch    = ': ''%s''';
  SLine     = 'Line: %d';
  SGroup    = '%s (%d matching lines)';

{$REGION 'TSearchResult'}
function TSearchResult.GetText: string;
begin
  Result := Format(SPosition, [Column, Line]);
  if ShowMatch then
  begin
    Result := Result + Format(SMatch, [Match]);
  end;
end;
{$ENDREGION}

{$REGION 'TSearchResultLine'}
procedure TSearchResultLine.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TObjectList.Create(True);
end;

procedure TSearchResultLine.BeforeDestruction;
begin
  FList.Free;
  inherited BeforeDestruction;
end;

function TSearchResultLine.GetText: string;
begin
  Result := Format(SLine, [Line]);
end;
{$ENDREGION}

{$REGION 'TSearchResultGroup'}
function TSearchResultGroup.GetText: string;
begin
  Result := Format(SGroup, [FFileName, FLines.Count]);
end;

procedure TSearchResultGroup.AfterConstruction;
begin
  inherited AfterConstruction;
  FLines := TObjectList.Create(True);
end;

procedure TSearchResultGroup.BeforeDestruction;
begin
  FLines.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
