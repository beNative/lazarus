unit ts.Editor.Templates;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs, ActnList,

  ts.Core.DataTemplates, ts.Core.ColumnDefinitionsDataTemplate;

type

  { TActionTemplate }

  TActionTemplate = class(TColumnDefinitionsDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string;
      override;
  end;

  { TActionCategoryTemplate }

  TActionCategoryTemplate = class(TColumnDefinitionsDataTemplate)
  private
    FActionTemplate : IDataTemplate;

    FActions : TObjectList;

  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): TObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
       override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string;
      override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TActionTemplate }

function TActionTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  if Item is TContainedAction then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TActionTemplate.GetTemplateDataClass: TClass;
begin
  Result := TContainedAction;
end;

function TActionTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Item is TContainedAction then
  begin
    if ColumnIndex = 1 then
      Result := (Item as TAction).Caption;
  end
  else
    Result := inherited GetText(Item, ColumnIndex);
end;

{ TActionCategoryTemplate }

function TActionCategoryTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TObjectList then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TActionCategoryTemplate.GetTemplateDataClass: TClass;
begin
  Result := TObjectList;
end;

function TActionCategoryTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TObjectList then
  begin
    Result := 1;
  end
  else
    Result := inherited GetItemCount(Item);
end;

function TActionCategoryTemplate.GetItems(const Item: TObject): TObjectList;
var
  AL : TActionList;
  A  : TContainedAction;
begin
  if Item is TObjectList then
  begin
    Result := Item as TObjectList;
  end
  else
    Result := inherited GetItems(Item);
end;

function TActionCategoryTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TObjectList then
  begin
    Result := Item;
  end
  else
    Result := inherited GetItem(Item, Index);
end;

function TActionCategoryTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Item is TContainedAction then
  begin
    if ColumnIndex = 0 then
      Result := 'Actions'
    else
      Result := '';
  end
  else
    Result := inherited GetText(Item, ColumnIndex);
end;

procedure TActionCategoryTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  FActionTemplate := TActionTemplate.Create(FColumnDefinitions);
  RegisterDataTemplate(FActionTemplate);
end;

procedure TActionCategoryTemplate.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

end.

