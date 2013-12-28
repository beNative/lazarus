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

unit ts.Editor.HighlighterAttributes;

{ Collection class which wraps TSynHighlighterAttributes. This collection is
  intended to support persistence to XML and enables us to embed extra
  settings. }

{$MODE Delphi}

interface

uses
  Classes, SysUtils,

  SynEditHighlighter;

type
  THighlighterAttributes = class;

  THighlighterAttributesItem = class(TCollectionItem)
  private
    FAttributes : TSynHighlighterAttributes;
    FName       : string;
    FAliasNames : TStrings;

    // private property access methods
    function GetAliasNames: TStrings;
    function GetAttributes: TSynHighlighterAttributes;
    procedure SetAliasNames(AValue: TStrings);
    procedure SetAttributes(AValue: TSynHighlighterAttributes);
    procedure SetCollection(const Value: THighlighterAttributes); reintroduce;
    function GetCollection: THighlighterAttributes;

  protected
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;

  public
    // constructors and destructors
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    // public methods
    procedure Assign(Source: TPersistent); override;

    // public properties
    { Collection that owns the instance of current THighlighterAttributesItem item. }
    property Collection: THighlighterAttributes
      read GetCollection write SetCollection;

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

  published
    // published properties
    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write SetDisplayName;

    property Attributes: TSynHighlighterAttributes
      read GetAttributes write SetAttributes;

    property AliasNames: TStrings
      read GetAliasNames write SetAliasNames;

  end;

  TSynHighlighterAttributesItemClass = class of THighlighterAttributesItem;

  { THighlighterAttributes inherits from TOwnedCollection to show
    the items in the Object Treeview at designtime. }

  THighlighterAttributes = class(TOwnedCollection)
  private
  type
    THighlighterAttributesEnumerator = class
    private
      FHighlighterAttributes : THighlighterAttributes;
      FPosition              : Integer;

      function GetCurrent: THighlighterAttributesItem;
    public
      constructor Create(AHighlighterAttributes: THighlighterAttributes);

      function MoveNext: Boolean;

      property Current: THighlighterAttributesItem
        read GetCurrent;
    end;

    // property access methods
    function GetItem(Index: Integer): THighlighterAttributesItem;
    function GetItemByName(const AName: string): THighlighterAttributesItem;
    procedure SetItem(Index: Integer; const Value: THighlighterAttributesItem);
    procedure SetItemByName(const AName: string; AValue: THighlighterAttributesItem);

  protected
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;

  public
    // constructors and destructors
    constructor Create(AOwner : TPersistent);

    function Add: THighlighterAttributesItem;
    function Insert(Index: Integer): THighlighterAttributesItem;
    function Owner: TComponent; reintroduce;

    function RegisterItem(
      const AName       : string;
            AAliasNames : array of string
    ): Boolean;

    function GetEnumerator: THighlighterAttributesEnumerator;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): THighlighterAttributesItem;
    function Find(const AName: string): THighlighterAttributesItem;

    // public properties
    { The TCollectionItem decendant class of the collection items. }
    property ItemClass;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: THighlighterAttributesItem
      read GetItem write SetItem; default;

    property ItemsByName[const AName: string]: THighlighterAttributesItem
      read GetItemByName write SetItemByName;
  end;

implementation

uses
  Graphics;

{$region 'THighlighterAttributes' /fold}

{$region 'THighlighterAttributes.THighlighterAttributesEnumerator' /fold}
function THighlighterAttributes.THighlighterAttributesEnumerator.GetCurrent: THighlighterAttributesItem;
begin
  Result := FHighlighterAttributes[FPosition];
end;

constructor THighlighterAttributes.THighlighterAttributesEnumerator.Create(
  AHighlighterAttributes: THighlighterAttributes);
begin
  FHighlighterAttributes := AHighlighterAttributes;
  FPosition := -1;
end;

function THighlighterAttributes.THighlighterAttributesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FHighlighterAttributes.Count;
end;
{$endregion}

{$region 'construction and destruction' /fold}

constructor THighlighterAttributes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, THighlighterAttributesItem);
end;

{$endregion}

{$region 'property access mehods' /fold}

function THighlighterAttributes.GetItem(Index: Integer): THighlighterAttributesItem;
begin
  Result := inherited Items[Index] as THighlighterAttributesItem;
end;

procedure THighlighterAttributes.SetItem(Index: Integer; const Value:
  THighlighterAttributesItem);
begin
  Items[Index].Assign(Value);
end;

function THighlighterAttributes.GetItemByName(const AName: string):
  THighlighterAttributesItem;
begin
  Result := Find(AName);
end;

procedure THighlighterAttributes.SetItemByName(const AName: string; AValue:
  THighlighterAttributesItem);
var
  Item: THighlighterAttributesItem;
begin
  Item := Find(AName);
  if Assigned(Item) then
    Item.Assign(AValue)
  else
  begin
    Item := Add;
    Item.Name := AName;
    Item.Attributes.StoredName := AName;
  end;
end;

{$endregion}

{$region 'protected methods' /fold}

{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
    affects more than one item in the collection }

procedure THighlighterAttributes.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change
// Update gets called from TCollection.Changed.
end;

{ Responds when items are added to or removed from the collection. }

procedure THighlighterAttributes.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
// The actions that can be used to respond to are:
//   - cnAdded      : an item was just added to the collection
//   - cnExtracting : an item is about to be removed from the collection (but
//                    not freed)
//   - cnDeleting   : an item is about to be removed from the collection and
//                    then freed
end;

{$endregion}

{$region 'public methods' /fold}

{ Adds a new THighlighterAttributesItem instance to the THighlighterAttributes
  collection. }

function THighlighterAttributes.Add: THighlighterAttributesItem;
begin
  Result := inherited Add as THighlighterAttributesItem
end;

{ Inserts a new THighlighterAttributesItem instance to the THighlighterAttributes
  collection before position specified with Index. }

function THighlighterAttributes.Insert(Index: Integer): THighlighterAttributesItem;
begin
  Result := inherited Insert(Index) as THighlighterAttributesItem;
end;

{ Constructs a unique itemname for a new collection item. }

procedure THighlighterAttributes.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  THighlighterAttributesItem(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

function THighlighterAttributes.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

function THighlighterAttributes.RegisterItem(const AName: string;
  AAliasNames: array of string): Boolean;
var
  Item: THighlighterAttributesItem;
  S   : string;
begin
  Item := Find(AName);
  if Assigned(Item) then
  begin
    for S in AAliasNames do
      Item.FAliasNames.Add(S);
    Item.Attributes.StoredName := AName;
    Result := False
  end
  else
  begin
    Item := Add;
    Item.Name := AName;
    for S in AAliasNames do
      Item.FAliasNames.Add(S);
    Item.Attributes.StoredName := AName;
    Result := True;
  end;
end;

function THighlighterAttributes.GetEnumerator: THighlighterAttributesEnumerator;
begin
  Result := THighlighterAttributesEnumerator.Create(Self);
end;

function THighlighterAttributes.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
    Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
    is passed to it as a parameter. If no item has the specified ID, FindItemID
    returns nil. }

function THighlighterAttributes.FindItemID(ID: Integer): THighlighterAttributesItem;
begin
  Result := inherited FindItemID(ID) as THighlighterAttributesItem;
end;

function THighlighterAttributes.Find(const AName: string): THighlighterAttributesItem;
var
  I : Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;
{$endregion}

{$region 'THighlighterAttributesItem' /fold}

{$region 'construction and destruction' /fold}
constructor THighlighterAttributesItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAttributes := TSynHighlighterAttributes.Create('', '');
  FAliasNames := TStringList.Create;
  TStringList(FAliasNames).Duplicates := dupIgnore;
  TStringList(FAliasNames).Sorted     := True;
end;

destructor THighlighterAttributesItem.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FAliasNames);
  inherited;
end;
{$endregion}

{$region 'property access mehods' /fold}
function THighlighterAttributesItem.GetCollection: THighlighterAttributes;
begin
  Result := inherited Collection as THighlighterAttributes;
end;

procedure THighlighterAttributesItem.SetCollection(const Value: THighlighterAttributes);
begin
  inherited Collection := Value;
end;

function THighlighterAttributesItem.GetAliasNames: TStrings;
begin
  Result := FAliasNames;
end;

function THighlighterAttributesItem.GetAttributes: TSynHighlighterAttributes;
begin
  Result := FAttributes;
end;

procedure THighlighterAttributesItem.SetAliasNames(AValue: TStrings);
begin
  FAliasNames.Assign(AValue);
end;

procedure THighlighterAttributesItem.SetAttributes(AValue: TSynHighlighterAttributes);
begin
  FAttributes.Assign(AValue);
end;

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

function THighlighterAttributesItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure THighlighterAttributesItem.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, Name) <> 0) and
    (Collection is THighlighterAttributes) and
    (THighlighterAttributes(Collection).IndexOf(Value) >= 0) then
    raise Exception.CreateFmt('Duplicate name [%s]!', [Value]);
  FName := Value;
  inherited;
end;
{$endregion}

{$region 'public methods' /fold}

procedure THighlighterAttributesItem.Assign(Source: TPersistent);
var
  AI : THighlighterAttributesItem;
begin
 if (Source <> Self) and (Source is THighlighterAttributesItem) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     AI := THighlighterAttributesItem(Source);
     Attributes.Assign(AI.Attributes);
     FAliasNames.Assign(AI.FAliasNames);
   finally
     if Assigned(Collection) then
       Collection.EndUpdate;
   end;
 end
 else
   inherited Assign(Source);
end;

{$endregion}

end.
