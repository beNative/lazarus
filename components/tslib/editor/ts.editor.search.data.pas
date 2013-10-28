{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Search.Data;

{$MODE Delphi}

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
    property List: TObjectList
      read FList;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Line : Integer
      read FLine write FLine;

  published
    property Text: string
      read GetText;

  end;

  { TSearchResultGroup }

  TSearchResultGroup = class(TPersistent)
  private
    FLines     : TObjectList;
    FFileName : string;
    FViewName: string;
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

{ TSearchResult }

function TSearchResult.GetText: string;
begin
  Result := Format('(%d, %d)', [Column, Line]);
end;

{ TSearchResultLine }

function TSearchResultLine.GetText: string;
begin
  Result := IntToStr(Line);
end;

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

{ TSearchResultGroup }

function TSearchResultGroup.GetText: string;
begin
  Result := Format('%s (%d matching lines)', [FFileName, FLines.Count]);
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

end.

