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

unit ts_Editor_SynHighlighterAttributesCollection;

{ Collection class which wraps TSynHighlighterAttributes. This collection is
  intended to support persistence to XML.

  In addition the comment tags can be stored.
}

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils,

  SynEditHighlighter;

//=============================================================================

type
  TSynHighlighterAttributesCollection = class;

  TSynHighlighterAttributesItem = class(TCollectionItem)
  private
    FAttributes : TSynHighlighterAttributes;
    FName       : string;
    // private property access methods
    function GetAttributes: TSynHighlighterAttributes;
    procedure SetAttributes(AValue: TSynHighlighterAttributes);
    procedure SetCollection(const Value: TSynHighlighterAttributesCollection); reintroduce;
    function GetCollection: TSynHighlighterAttributesCollection;
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
    { Collection that owns the instance of current TSynHighlighterAttributesItem item. }
    property Collection: TSynHighlighterAttributesCollection
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

  end;

  TSynHighlighterAttributesItemClass = class of TSynHighlighterAttributesItem;

//=============================================================================

  { TSynHighlighterAttributesCollection inherits from TOwnedCollection to show the items in
    the Object Treeview of the Delphi IDE at designtime. }

  TSynHighlighterAttributesCollection = class(TOwnedCollection)
  private
    // property access methods
    function GetItem(Index: Integer): TSynHighlighterAttributesItem;
    procedure SetItem(Index: Integer; const Value: TSynHighlighterAttributesItem);
  protected
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;
  public
    // constructors and destructors
    constructor Create(AOwner : TPersistent);

    function Add: TSynHighlighterAttributesItem;
    function Insert(Index: Integer): TSynHighlighterAttributesItem;
    function Owner: TComponent; reintroduce;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TSynHighlighterAttributesItem;
    function Find(const AName: string): TSynHighlighterAttributesItem;

    // public properties
    { The TCollectionItem decendant class of the collection items. }
    property ItemClass;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TSynHighlighterAttributesItem
      read GetItem write SetItem; default;
  end;

//*****************************************************************************

implementation

uses
  Graphics;

{
_______________________________________________________________________________
_______________________________________________________________________________

                     TSynHighlighterAttributesCollection
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TSynHighlighterAttributesCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSynHighlighterAttributesItem);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Items|-------------------------------------------------------------------

function TSynHighlighterAttributesCollection.GetItem(Index: Integer): TSynHighlighterAttributesItem;
begin
  Result := inherited Items[Index] as TSynHighlighterAttributesItem;
end;

procedure TSynHighlighterAttributesCollection.SetItem(Index: Integer; const Value: TSynHighlighterAttributesItem);
begin
  Items[Index].Assign(Value);
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection }

procedure TSynHighlighterAttributesCollection.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change
// Update gets called from TCollection.Changed.
end;

//-----------------------------------------------------------------------------

{ Responds when items are added to or removed from the collection. }

procedure TSynHighlighterAttributesCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
// The actions that can be used to respond to are:
//   - cnAdded      : an item was just added to the collection
//   - cnExtracting : an item is about to be removed from the collection (but
//                    not freed)
//   - cnDeleting   : an item is about to be removed from the collection and
//                    then freed
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Adds a new TSynHighlighterAttributesItem instance to the TSynHighlighterAttributesCollection
  collection. }

function TSynHighlighterAttributesCollection.Add: TSynHighlighterAttributesItem;
begin
  Result := inherited Add as TSynHighlighterAttributesItem
end;

//-----------------------------------------------------------------------------

{ Inserts a new TSynHighlighterAttributesItem instance to the TSynHighlighterAttributesCollection
  collection before position specified with Index. }

function TSynHighlighterAttributesCollection.Insert(Index: Integer): TSynHighlighterAttributesItem;
begin
  Result := inherited Insert(Index) as TSynHighlighterAttributesItem;
end;

//-----------------------------------------------------------------------------

{ Constructs a unique itemname for a new collection item. }

procedure TSynHighlighterAttributesCollection.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TSynHighlighterAttributesItem(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

//-----------------------------------------------------------------------------

function TSynHighlighterAttributesCollection.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

//-----------------------------------------------------------------------------

function TSynHighlighterAttributesCollection.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

//-----------------------------------------------------------------------------

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil. }

function TSynHighlighterAttributesCollection.FindItemID(ID: Integer): TSynHighlighterAttributesItem;
begin
  Result := inherited FindItemID(ID) as TSynHighlighterAttributesItem;
end;

//-----------------------------------------------------------------------------

function TSynHighlighterAttributesCollection.Find(const AName: string): TSynHighlighterAttributesItem;
var
  I : Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

{
_______________________________________________________________________________
_______________________________________________________________________________

                          TSynHighlighterAttributesItem
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TSynHighlighterAttributesItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAttributes := TSynHighlighterAttributes.Create('', '');
  // Add your property storage initializations here.
end;

//-----------------------------------------------------------------------------

destructor TSynHighlighterAttributesItem.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TSynHighlighterAttributesItem.GetCollection: TSynHighlighterAttributesCollection;
begin
  Result := inherited Collection as TSynHighlighterAttributesCollection;
end;

procedure TSynHighlighterAttributesItem.SetCollection(const Value: TSynHighlighterAttributesCollection);
begin
  inherited Collection := Value;
end;

function TSynHighlighterAttributesItem.GetAttributes: TSynHighlighterAttributes;
begin
  Result := FAttributes;
end;

procedure TSynHighlighterAttributesItem.SetAttributes(AValue: TSynHighlighterAttributes);
begin
  FAttributes := AValue;
end;

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

function TSynHighlighterAttributesItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TSynHighlighterAttributesItem.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, Name) <> 0) and
    (Collection is TSynHighlighterAttributesCollection) and
    (TSynHighlighterAttributesCollection(Collection).IndexOf(Value) >= 0) then
    raise Exception.CreateFmt('Duplicate name [%s]!', [Value]);
  //FAttributes.StoredName := Value;
  FName := Value;
  inherited;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TSynHighlighterAttributesItem.Assign(Source: TPersistent);
var
  AI : TSynHighlighterAttributesItem;
begin
 if (Source <> Self) and (Source is TSynHighlighterAttributesItem) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     AI := TSynHighlighterAttributesItem(Source);
     Attributes.Assign(AI.Attributes);
   finally
     if Assigned(Collection) then
       Collection.EndUpdate;
   end;
 end
 else
   inherited Assign(Source);
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

//*****************************************************************************

end.
