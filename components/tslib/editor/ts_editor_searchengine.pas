{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_SearchEngine;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Contnrs,

  SynEditTypes, SynEditSearch,

  ts_Editor_Interfaces,

  sharedloggerlcl;

type
  TSearchResult = class
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

  public
    property BlockBegin : TPoint
      read FBlockBegin write FBlockBegin;

    property BlockEnd : TPoint
      read FBlockEnd write FBlockEnd;

    property ViewName: string
      read FViewName write FViewName;

  published
    property Index: Integer
      read FIndex write FIndex;

    property FileName : string
      read FFileName write FFileName;

    property Line : Integer
      read FLine write FLine;

    property Column : Integer
      read FColumn write FColumn;

    property StartPos: Integer
      read FStartPos write FStartPos;

    property EndPos: Integer
      read FEndPos write FEndPos;
  end;

  TSearchEngine = class(TComponent, IEditorSearchEngine)
  private
    FOptions        : TSynSearchOptions;
    FSearchAllViews : Boolean;
    FSearchText     : string;
    FReplaceText    : string;
    FItemList       : TObjectList;
    FCurrentIndex   : Integer;
    FSESearch       : TSynEditSearch;

    function GetCurrentIndex: Integer;
    function GetItemList: TObjectList;
    function GetManager: IEditorManager;
    function GetOptions: TSynSearchOptions;
    function GetReplaceText: string;
    function GetSearchAllViews: Boolean;
    function GetSearchText: string;
    function GetView: IEditorView;
    function GetViews: IEditorViews;
    procedure SetCurrentIndex(AValue: Integer);
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetReplaceText(AValue: string);
    procedure SetSearchAllViews(AValue: Boolean);
    procedure SetSearchText(AValue: string);

  protected
    procedure AddResultsForView(AView: IEditorView);
    function PosToLineCol(const AString: string;
      const AOffset: TPoint; APos: Integer): TPoint;

    { IEditorSearchEngine }
    procedure Execute;
    procedure FindNext;
    procedure FindPrevious;
    procedure Replace;
    procedure ReplaceAll;

    property Manager: IEditorManager
      read GetManager;

   property CurrentIndex: Integer
      read GetCurrentIndex write SetCurrentIndex;

    property Views: IEditorViews
      read GetViews;

    property View: IEditorView
      read GetView;

    property Options: TSynSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText: string
      read GetReplaceText write SetReplaceText;

    property SearchAllViews: Boolean
      read GetSearchAllViews write SetSearchAllViews;

    property ItemList: TObjectList
      read GetItemList;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

//*****************************************************************************

implementation

uses
  FileUtil,

  ts_Core_StringUtils,

  ts_Editor_Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TSearchEngine.AfterConstruction;
begin
  inherited AfterConstruction;
  FItemList := TObjectList.Create(True);
  FSESearch := TSynEditSearch.Create;
end;

procedure TSearchEngine.BeforeDestruction;
begin
  FItemList.Free;
  FSESearch.Free;
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TSearchEngine.GetItemList: TObjectList;
begin
  Result := FItemList;
end;

function TSearchEngine.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TSearchEngine.GetCurrentIndex: Integer;
begin
  Result := FCurrentIndex;
end;

function TSearchEngine.GetOptions: TSynSearchOptions;
begin
  Result := FOptions;
end;

function TSearchEngine.GetReplaceText: string;
begin
  Result := FReplaceText;
end;

procedure TSearchEngine.SetReplaceText(AValue: string);
begin
  if AValue <> ReplaceText then
  begin
    FReplaceText := AValue;
    FSESearch.Replacement := AValue;
  end;
end;

function TSearchEngine.GetSearchAllViews: Boolean;
begin
  Result := FSearchAllViews;
end;

function TSearchEngine.GetSearchText: string;
begin
  Result := FSearchText;
end;

procedure TSearchEngine.SetSearchText(AValue: string);
begin
  if AValue <> SearchText then
  begin
    FSearchText := AValue;
    FSESearch.Pattern := AValue;
  end;
end;

function TSearchEngine.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TSearchEngine.GetViews: IEditorViews;
begin
  Result := Owner as IEditorViews;
end;

procedure TSearchEngine.SetCurrentIndex(AValue: Integer);
begin
  FCurrentIndex := AValue;
end;

procedure TSearchEngine.SetOptions(AValue: TSynSearchOptions);
begin
  if AValue <> Options then
  begin
    FOptions := AValue;
    FSESearch.Sensitive          := ssoMatchCase in FOptions;
    FSESearch.Whole              := ssoWholeWord in FOptions;
    FSESearch.RegularExpressions := ssoRegExpr in FOptions;
    FSESearch.RegExprMultiLine   := ssoRegExprMultiLine in FOptions;
    FSESearch.Backwards          := ssoBackwards in FOptions;
  end;
end;

procedure TSearchEngine.SetSearchAllViews(AValue: Boolean);
begin
  FSearchAllViews := AValue;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TSearchEngine.AddResultsForView(AView: IEditorView);
var
  SR           : TSearchResult;
  ptStart      : TPoint;
  ptEnd        : TPoint;
  ptCurrent    : TPoint;
  ptFoundStart : TPoint;
  ptFoundEnd   : TPoint;
begin
  ptStart := Point(1, 1);
  ptEnd.Y := AView.Lines.Count;
  ptEnd.X := Length(AView.Lines[ptEnd.Y - 1]) + 1;
  ptCurrent := ptStart;
  while FSESearch.FindNextOne(
    AView.Lines,
    ptStart,
    ptEnd,
    ptFoundStart,
    ptFoundEnd,
    True           // Support unicode case
  ) do
  begin
    SR := TSearchResult.Create;
    SR.FileName   := ExtractFileName(AView.FileName);
    SR.ViewName   := AView.Name;
    SR.BlockBegin := ptFoundStart;
    SR.BlockEnd   := ptFoundEnd;
    SR.StartPos   := PointToPos(AView.Lines, ptFoundStart);
    SR.EndPos     := PointToPos(AView.Lines, ptFoundEnd);
    SR.Column     := ptFoundStart.X;
    SR.Line       := ptFoundStart.Y;
    SR.Index      := ItemList.Count + 1;
    ItemList.Add(SR);
    ptStart := ptFoundEnd;
  end;
end;

function TSearchEngine.PosToLineCol(const AString: string;
  const AOffset: TPoint; APos: Integer): TPoint;
var
  I: Integer;
begin
  Result := AOffset;
  I := 1;
  while I < APos do
  begin
    if AString[I] in [#10, #13] then
    begin
      Inc(Result.Y);
      Result.X := 1;
      Inc(I);
      if (I < APos) and (AString[I] in [#10, #13])
        and (AString[I] <> AString[I - 1]) then
        Inc(I);
    end
    else
    begin
      Inc(Result.X);
      Inc(I);
    end;
  end;
end;

{ Get matching position of FSearchText in the AText, starting from postion APos.
  When no match is found -1 is returned.

  TODO:
    ssoSelectedOnly
    FBlockSelection.ActiveSelectionMode = smLine/smColumn
}

procedure TSearchEngine.Execute;
var
  V: IEditorView;
  I: Integer;
begin
  FItemList.Clear;
  if SearchAllViews then
  begin
    for I := 0 to Views.Count - 1 do
    begin
      V := Views[I];
      AddResultsForView(V);
    end;
  end
  else
    AddResultsForView(View);
end;

procedure TSearchEngine.FindNext;
var
  SR: TSearchResult;
  V: IEditorView;
begin
  if CurrentIndex < ItemList.Count - 1 then
  begin
    Inc(FCurrentIndex);
    SR := ItemList[CurrentIndex] as TSearchResult;
    Manager.ActivateView(SR.ViewName);
    Manager.ActiveView.SelStart := SR.StartPos;
    Manager.ActiveView.SelEnd := SR.StartPos + Length(SearchText);
  end;
end;

procedure TSearchEngine.FindPrevious;
var
  SR: TSearchResult;
  V: IEditorView;
begin
  if CurrentIndex > 0 then
  begin
    Dec(FCurrentIndex);
    SR := ItemList[CurrentIndex] as TSearchResult;
    Manager.ActivateView(SR.ViewName);
    Manager.ActiveView.SelStart := SR.StartPos;
    Manager.ActiveView.SelEnd := SR.StartPos + Length(SearchText);
  end;
end;

procedure TSearchEngine.Replace;
var
  SR : TSearchResult;
  V  : IEditorView;
begin
  if CurrentIndex >= 0 then
  begin
    SR := ItemList[CurrentIndex] as TSearchResult;
    Logger.Send('SR.Index', SR.Index);
    Logger.Watch('SearchText', SearchText);
    Logger.Watch('ReplaceText', ReplaceText);
    Options := Options + [ssoReplace];
    Manager.ActiveView.Editor.SearchReplaceEx(SearchText, ReplaceText, Options, SR.BlockBegin);
    Options := Options - [ssoReplace];
  end;
end;

procedure TSearchEngine.ReplaceAll;
var
  SR : TSearchResult;
  V  : IEditorView;
  I  : Integer;
begin
  if CurrentIndex >= 0 then
  begin
    SR := ItemList[CurrentIndex] as TSearchResult;
    Logger.Send('SR.Index', SR.Index);
    Logger.Watch('SearchText', SearchText);
    Logger.Watch('ReplaceText', ReplaceText);
    Options := Options + [ssoReplaceAll];
    if SearchAllViews then
    begin
      for I := 0 to Manager.Views.Count - 1 do
      begin
        V := Manager.Views[I];
        V.BeginUpdate; // handle all replacements as one operation that we can undo
        V.Editor.SearchReplace(SearchText, ReplaceText, Options);
        V.EndUpdate;
      end;
    end
    else
    begin
      V := Manager.ActiveView;
      V.BeginUpdate; // handle all replacements as one operation that we can undo
      V.Editor.SearchReplaceEx(SearchText, ReplaceText, Options, SR.BlockBegin);
      V.EndUpdate;
    end;
    Options := Options - [ssoReplaceAll];
    //Logger.Send('Result', Manager.ActiveView.Editor.SearchReplace(SearchText, ReplaceText, Options));
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

