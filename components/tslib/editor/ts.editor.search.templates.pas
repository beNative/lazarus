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

unit ts.Editor.Search.Templates;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Contnrs,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions, ts.Core.DataTemplates,
  ts.Core.ColumnDefinitionsDataTemplate;

type
  { TSearchResultGroupTemplate }

  TSearchResultGroupTemplate = class(TColumnDefinitionsDataTemplate, IDataTemplate)
  private
    FTemplate: IDataTemplate;

  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): TObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
      override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TSearchResultLineTemplate }

  TSearchResultLineTemplate = class(TColumnDefinitionsDataTemplate, IDataTemplate)
  private
//    FTemplate: IDataTemplate;

  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): TObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
      override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TSearchResultTemplate }

  TSearchResultTemplate = class(TColumnDefinitionsDataTemplate, IDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

  end;


implementation

uses
  ts.Editor.Search.Data;

{ TSearchResultTemplate }

function TSearchResultTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TSearchResult then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResult;
end;

{ TSearchResultLineTemplate }

function TSearchResultLineTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TSearchResultLine then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultLineTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResultLine;
end;

function TSearchResultLineTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TSearchResultLineTemplate.GetItems(const Item: TObject): TObjectList;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List
  else
    Result := inherited GetItems(Item);
end;

function TSearchResultLineTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List[Index]
  else
    Result := inherited GetItem(Item, Index);
end;

procedure TSearchResultLineTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  //FTemplate := TSearchResultTemplate.Create(FColumnDefinitions);
  RegisterDataTemplate(TSearchResultTemplate.Create(FColumnDefinitions));
end;

procedure TSearchResultLineTemplate.BeforeDestruction;
begin
  //FTemplate := nil;
  inherited BeforeDestruction;
end;

{ TSearchResultGroupTemplate }

function TSearchResultGroupTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TSearchResultGroupTemplate.GetItems(const Item: TObject): TObjectList;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines
  else
    Result := inherited GetItems(Item);
end;

function TSearchResultGroupTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  Assert(Assigned(Item), 'Item not assigned!');
  if Item is TSearchResultGroup then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultGroupTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines[Index]
  else
    Result := inherited GetItem(Item, Index);
end;

function TSearchResultGroupTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResultGroup;
end;

procedure TSearchResultGroupTemplate.AfterConstruction;
begin
  //FTemplate := TSearchResultLineTemplate.Create(FColumnDefinitions);
  RegisterDataTemplate(TSearchResultLineTemplate.Create(FColumnDefinitions));
  inherited AfterConstruction;
end;

procedure TSearchResultGroupTemplate.BeforeDestruction;
begin
//  FTemplate := nil;
  inherited BeforeDestruction;
end;

end.

