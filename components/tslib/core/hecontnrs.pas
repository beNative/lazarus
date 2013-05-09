unit heContnrs; { http://code.google.com/p/fprb/wiki/heContnrs }

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// Copyright 2010, bflm. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//------------------------------------------------------------------------------
// The list container is a FPC port/modification of source code from
// the Google Go project: http://code.google.com/p/go
//
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE-GO file.
//------------------------------------------------------------------------------

{$if (FPC_VERSION < 2) or ((FPC_VERSION = 2) and (FPC_RELEASE < 6))}
{$fatal 'Requires FPC >= 2.6.0'}
{$endif}

interface

uses
  SysUtils;

type

  EMapKeyNotFound = class(Exception);

  { TheEnumerator }

  generic TheEnumerator<TIterator, TValue> = object
  public type
    TGetCurrent = function(var Iterator: TIterator): TValue of object;
    TMoveNext = function(var Iterator: TIterator): Boolean of object;
  private
    FGetCurrent: TGetCurrent;
    FIterator: TIterator;
    FMoveNext: TMoveNext;
    function GetCurrent: TValue;
  public
    procedure Init(const InitialIterator: TIterator; const Mover: TMoveNext; const Getter: TGetCurrent);
    function MoveNext: Boolean;
    property Current: TValue read GetCurrent;
  end;

  { TheEnumeratorProvider }

  generic TheEnumeratorProvider<TProvidedEnumerator> = object
  public
    FEnumerator: TProvidedEnumerator;
    function GetEnumerator: TProvidedEnumerator;
  end;

  { TheObjectVector }

  generic TheObjectVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheObjectVector }
  public type
    PItem = ^TItem;
    TCompare = function(const A, B: TItem): Integer;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    FOwnObjects: Boolean;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetItems(const Index: Integer; const AValue: TItem);
    procedure Sort(Left, Right: Integer; const Compare: TCompare);
  protected
    property Data: PItem read FData;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    function SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Insert(const Index: Integer; const Item: TItem);
    procedure Kill;
    procedure Pack;
    procedure Sort(const Compare: TCompare);
    procedure Swap(const ItemAIndex, ItemBIndex: Integer);
    procedure Wipe;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems write SetItems; default;
    property Last: TItem read GetLast;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  { TheVector }

  generic TheVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheVector }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetItems(const Index: Integer; const AValue: TItem);
    procedure Sort(Left, Right: Integer);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    function SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Insert(const Index: Integer; const Item: TItem);
    procedure Pack;
    procedure Sort;
    procedure Swap(const ItemAIndex, ItemBIndex: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems write SetItems; default;
    property Last: TItem read GetLast;
  end;

  { TheCmpVector }

  generic TheCmpVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheCmpVector }
  public type
    PItem = ^TItem;
    TCompare = function(const A, B: TItem): Integer;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetItems(const Index: Integer; const AValue: TItem);
    procedure Sort(Left, Right: Integer);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Compare(const A, B: TItem): Integer; virtual; // abstract;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    function SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Insert(const Index: Integer; const Item: TItem);
    procedure Pack;
    procedure Sort;
    procedure Swap(const ItemAIndex, ItemBIndex: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems write SetItems; default;
    property Last: TItem read GetLast;
  end;

  { TheSortVector }

  generic TheSortVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheSortVector }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems; default;
    property Last: TItem read GetLast;
  end;

  { TheVectorSet }

  generic TheVectorSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheVectorSet }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function GetMembership(const Item: TItem): Boolean;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Exclude(const Item: TItem): Boolean; // true => was in set
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean; // true => was in set
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property Last: TItem read GetLast;
  end;

  { TheVectorMap }

  generic TheVectorMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheVectorMap }
  public type
    PItem = ^TItem;
    TItem = record
      Key: TKey;
      Value: TValue;
    end;
    TKeyEnumerator = specialize TheEnumerator<Integer, TKey>;
    TValueEnumerator = specialize TheEnumerator<Integer, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetKey(const Index: Integer): TKey;
    function GetCurrentKey(var Index: Integer): TKey;
    function GetLast: TItem;
    function GetMap(const AKey: TKey): TValue;
    function GetValue(const Index: Integer): TValue;
    function GetCurrentValue(var Index: Integer): TValue;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
    procedure SetCapacity(AValue: Integer);
    procedure SetMap(const AKey: TKey; const AValue: TValue);
  protected
    property Data: PItem read FData;
    function MissingKeyValue(const AKey: TKey): TValue; virtual;
  public
    destructor Destroy; override;
    function Extract(const Index: Integer): TItem;
    function Find(const AKey: TKey; out Index: Integer): Boolean;
    function Has(const AKey: TKey): Boolean;
    function IndexOf(const AKey: TKey): Integer;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Remove(const AKey: TKey): Integer;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Item[const Index: Integer]: TItem read GetItem;
    property Key[const Index: Integer]: TKey read GetKey;
    property Last: TItem read GetLast;
    property Map[const AKey: TKey]: TValue read GetMap write SetMap; default;
    property Value[const Index: Integer]: TValue read GetValue;
  end;

  { TheCmpVectorMap }

  generic TheCmpVectorMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheCmpVectorMap }
  public type
    PItem = ^TItem;
    TItem = record
      Key: TKey;
      Value: TValue;
    end;
    TKeyEnumerator = specialize TheEnumerator<Integer, TKey>;
    TValueEnumerator = specialize TheEnumerator<Integer, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetKey(const Index: Integer): TKey;
    function GetCurrentKey(var Index: Integer): TKey;
    function GetLast: TItem;
    function GetMap(const AKey: TKey): TValue;
    function GetValue(const Index: Integer): TValue;
    function GetCurrentValue(var Index: Integer): TValue;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
    procedure SetCapacity(AValue: Integer);
    procedure SetMap(const AKey: TKey; const AValue: TValue);
  protected
    property Data: PItem read FData;
    function MissingKeyValue(const AKey: TKey): TValue; virtual;
  public
    destructor Destroy; override;
    function Compare(const A, B: TKey): Integer; virtual; // abstract;
    function Extract(const Index: Integer): TItem;
    function Find(const AKey: TKey; out Index: Integer): Boolean;
    function Has(const AKey: TKey): Boolean;
    function IndexOf(const AKey: TKey): Integer;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Remove(const AKey: TKey): Integer;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Item[const Index: Integer]: TItem read GetItem;
    property Key[const Index: Integer]: TKey read GetKey;
    property Last: TItem read GetLast;
    property Map[const AKey: TKey]: TValue read GetMap write SetMap; default;
    property Value[const Index: Integer]: TValue read GetValue;
  end;

  { TheObjectVectorMap }

  generic TheObjectVectorMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheObjectVectorMap }
  public type
    PItem = ^TItem;
    TItem = record
      Key: TKey;
      Value: TValue;
    end;
    TKeyEnumerator = specialize TheEnumerator<Integer, TKey>;
    TValueEnumerator = specialize TheEnumerator<Integer, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    FOwnObjects: Boolean;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetKey(const Index: Integer): TKey;
    function GetCurrentKey(var Index: Integer): TKey;
    function GetLast: TItem;
    function GetMap(const AKey: TKey): TValue;
    function GetValue(const Index: Integer): TValue;
    function GetCurrentValue(var Index: Integer): TValue;
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
    procedure SetCapacity(AValue: Integer);
    procedure SetMap(const AKey: TKey; const AValue: TValue);
  protected
    property Data: PItem read FData;
    function MissingKeyValue(const AKey: TKey): TValue; virtual;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    destructor Destroy; override;
    function Compare(const A, B: TKey): Integer; virtual; // abstract;
    function Extract(const Index: Integer): TItem;
    function Find(const AKey: TKey; out Index: Integer): Boolean;
    function Has(const AKey: TKey): Boolean;
    function IndexOf(const AKey: TKey): Integer;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Remove(const AKey: TKey): Integer;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Item[const Index: Integer]: TItem read GetItem;
    property Key[const Index: Integer]: TKey read GetKey;
    property Last: TItem read GetLast;
    property Map[const AKey: TKey]: TValue read GetMap write SetMap; default;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
    property Value[const Index: Integer]: TValue read GetValue;
  end;

  { TheCmpVectorSet }

  generic TheCmpVectorSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheCmpVectorSet }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function GetMembership(const Item: TItem): Boolean;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Compare(const A, B: TItem): Integer; virtual; // abstract;
    function Exclude(const Item: TItem): Boolean; // true => was in set
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean; // true => was in set
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property Last: TItem read GetLast;
  end;

  { TheObjectVectorSet }

  generic TheObjectVectorSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheObjectVectorSet }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    FOwnObjects: Boolean;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetItem(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    function GetMembership(const Item: TItem): Boolean;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
  protected
    property Data: PItem read FData;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    destructor Destroy; override;
    function Compare(const A, B: TItem): Integer; virtual; // abstract;
    function Exclude(const Item: TItem): Boolean; // true => was in set
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean; // true => was in set
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Kill;
    procedure Pack;
    procedure Wipe;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property Last: TItem read GetLast;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  { TheCmpSortVector }

  generic TheCmpSortVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheCmpSortVector }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
  protected
    property Data: PItem read FData;
  public
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Compare(const A, B: TItem): Integer; virtual; // abstract;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems; default;
    property Last: TItem read GetLast;
  end;

  { TheObjectSortVector }

  generic TheObjectSortVector<TItem> = class { http://code.google.com/p/fprb/wiki/TheObjectSortVector }
  public type
    PItem = ^TItem;
    TEnumerator = specialize TheEnumerator<Integer, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: PItem;
    FOwnObjects: Boolean;
    function GetFirst: TItem;
    function GetItems(const Index: Integer): TItem;
    function GetCurrent(var Index: Integer): TItem;
    function GetLast: TItem;
    procedure Insert(const Index: Integer; const Item: TItem);
    function MoveNext(var Index: Integer): Boolean;
    function MovePrev(var Index: Integer): Boolean;
    procedure SetCapacity(AValue: Integer);
  protected
    property Data: PItem read FData;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    destructor Destroy; override;
    function Add(const Item: TItem): Integer;
    function Compare(const A, B: TItem): Integer; virtual; // abstract;
    function Extract(const Index: Integer): TItem;
    function Find(const Item: TItem; out Index: Integer): Boolean;
    function GetEnumerator: TEnumerator;
    function Has(const Item: TItem): Boolean;
    function IndexOf(const Item: TItem): Integer;
    function Push(const Item: TItem): TItem;
    function Remove(const Item: TItem): Integer;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure Kill;
    procedure Pack;
    procedure Wipe;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property First: TItem read GetFirst;
    property Items[const Index: Integer]: TItem read GetItems; default;
    property Last: TItem read GetLast;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  { TheList }

  generic TheList<TItem> = class { http://code.google.com/p/fprb/wiki/TheList }
  public type
    PNode = ^TNode;
    TNode = object
    private
      FNext: PNode;
      FPrev: PNode;
    public
      Item: TItem;
      property Next: PNode read FNext;
      property Prev: PNode read FPrev;
    end;
  private type
    TIterator = record
      List: TObject;
      Node: PNode;
    end;
  public type
    TEnumerator = specialize TheEnumerator<TIterator, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private type
    PNode_ = ^TNode_;
    TNode_ = object
    public
      FNext: PNode_;
      FPrev: PNode_;
      FItem: TItem;
    end;
  private
    FCount: Integer;
    FFirst: PNode;
    FLast: PNode;
    function CurrentItem(var Iterator: TIterator): TItem;
    function InsertAfter_(const After, Node: PNode): PNode;
    function InsertBack(const Node: PNode): PNode;
    function InsertBefore_(const Node, Before: PNode): PNode;
    function InsertFront(const Node: PNode): PNode;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function NewIterator: TIterator;
    function NewNode(const AItem: TItem): PNode;
  public
    destructor Destroy; override;
    function Extract(const Node: PNode): PNode;
    function GetEnumerator: TEnumerator;
    function InsertAfter(const After: PNode; const AItem: TItem): PNode;
    function InsertBefore(const AItem: TItem; const Before: PNode): PNode;
    function MoveAfter(const After, Node: PNode): PNode;
    function MoveBefore(const Node, Before: PNode): PNode;
    function MoveToBack(const Node: PNode): PNode;
    function MoveToFront(const Node: PNode): PNode;
    function PushBack(const AItem: TItem): PNode;
    function PushFront(const AItem: TItem): PNode;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Remove(Node: PNode);
    property Count: Integer read FCount;
    property First: PNode read FFirst;
    property Last: PNode read FLast;
  end;

  { TheObjectList }

  generic TheObjectList<TItem> = class { http://code.google.com/p/fprb/wiki/TheObjectList }
  public type
    PNode = ^TNode;
    TNode = object
    private
      FNext: PNode;
      FPrev: PNode;
    public
      Item: TItem;
      property Next: PNode read FNext;
      property Prev: PNode read FPrev;
    end;
  private type
    TIterator = record
      List: TObject;
      Node: PNode;
    end;
  public type
    TEnumerator = specialize TheEnumerator<TIterator, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private type
    PNode_ = ^TNode_;
    TNode_ = object
    public
      FNext: PNode_;
      FPrev: PNode_;
      FItem: TItem;
    end;
  private
    FCount: Integer;
    FFirst: PNode;
    FLast: PNode;
    FOwnObjects: Boolean;
    function CurrentItem(var Iterator: TIterator): TItem;
    function InsertAfter_(const After, Node: PNode): PNode;
    function InsertBack(const Node: PNode): PNode;
    function InsertBefore_(const Node, Before: PNode): PNode;
    function InsertFront(const Node: PNode): PNode;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function NewIterator: TIterator;
    function NewNode(const AItem: TItem): PNode;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    destructor Destroy; override;
    function Extract(const Node: PNode): PNode;
    function GetEnumerator: TEnumerator;
    function InsertAfter(const After: PNode; const AItem: TItem): PNode;
    function InsertBefore(const AItem: TItem; const Before: PNode): PNode;
    function MoveAfter(const After, Node: PNode): PNode;
    function MoveBefore(const Node, Before: PNode): PNode;
    function MoveToBack(const Node: PNode): PNode;
    function MoveToFront(const Node: PNode): PNode;
    function PushBack(const AItem: TItem): PNode;
    function PushFront(const AItem: TItem): PNode;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    procedure Remove(Node: PNode);
    property Count: Integer read FCount;
    property First: PNode read FFirst;
    property Last: PNode read FLast;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  { TheBTreeSet }

  generic TheBTreeSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheBTreeSet }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TItem;
    end;
  public type
    TEnumerator = specialize TheEnumerator<TIterator, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TItem;
    function Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetLast: TItem;
    function GetCurrent(var Iterator: TIterator): TItem;
    function GetMembership(const Item: TItem): Boolean;
    function GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  public
    constructor Create(const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Exclude(const Item: TItem): Boolean;
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TItem read GetFirst;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TItem read GetLast;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property Range[const RangeFrom, RangeTo: TItem]: TEnumeratorProvider read GetRange;
  end;

  { TheCmpBTreeSet }

  generic TheCmpBTreeSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheCmpBTreeSet }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TItem;
    end;
  public type
    TEnumerator = specialize TheEnumerator<TIterator, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TItem;
    function Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetLast: TItem;
    function GetCurrent(var Iterator: TIterator): TItem;
    function GetMembership(const Item: TItem): Boolean;
    function GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  public
    constructor Create(const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Compare(const A, B: TItem): Integer; virtual; // abstract
    function Exclude(const Item: TItem): Boolean;
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TItem read GetFirst;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TItem read GetLast;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property Range[const RangeFrom, RangeTo: TItem]: TEnumeratorProvider read GetRange;
  end;

  { TheObjectBTreeSet }

  generic TheObjectBTreeSet<TItem> = class { http://code.google.com/p/fprb/wiki/TheObjectBTreeSet }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TItem;
    end;
  public type
    TEnumerator = specialize TheEnumerator<TIterator, TItem>;
    TEnumeratorProvider = specialize TheEnumeratorProvider<TEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FOwnObjects: Boolean;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TItem;
    function Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
    function GetFirst: TItem;
    function GetLast: TItem;
    function GetCurrent(var Iterator: TIterator): TItem;
    function GetMembership(const Item: TItem): Boolean;
    function GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SetMembership(const Item: TItem; const AValue: Boolean);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  public
    constructor Create(const AOwnObjects: Boolean = True; const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Compare(const A, B: TItem): Integer; virtual; // abstract
    function Exclude(const Item: TItem): Boolean;
    function GetEnumerator: TEnumerator;
    function Include(const Item: TItem): Boolean;
    function Reversed: TEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TItem read GetFirst;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TItem read GetLast;
    property Membership[const Item: TItem]: Boolean read GetMembership write SetMembership; default;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
    property Range[const RangeFrom, RangeTo: TItem]: TEnumeratorProvider read GetRange;
  end;

  { TheBTreeMap }

  generic TheBTreeMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheBTreeMap }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TItem = packed record
        Key: TKey;
        Value: TValue;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem; // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TKey;
    end;
  public type
    TKeyEnumerator = specialize TheEnumerator<TIterator, TKey>;
    TValueEnumerator = specialize TheEnumerator<TIterator, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TValue;
    function Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
    function GetCurrent(var Iterator: TIterator): TValue;
    function GetCurrentKey(var Iterator: TIterator): TKey;
    function GetFirst: TValue;
    function GetFirstKey: TKey;
    function GetLast: TValue;
    function GetLastKey: TKey;
    function GetMap(const Key: TKey): TValue;
    function GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
    function GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SetMap(const Key: TKey; const Value: TValue);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  protected
    function MissingKeyValue(const Key: TKey): TValue; virtual;
  public
    constructor Create(const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Delete(const Key: TKey): Boolean;
    function Extract(const Key: TKey; out Value: TValue): Boolean;
    function Get(const Key: TKey; out Value: TValue): Boolean;
    function Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TValue read GetFirst;
    property FirstKey: TKey read GetFirstKey;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TValue read GetLast;
    property LastKey: TKey read GetLastKey;
    property Map[const Key: TKey]: TValue read GetMap write SetMap; default;
    property Range[const RangeFrom, RangeTo: TKey]: TValueEnumeratorProvider read GetRange;
    property RangeKeys[const RangeFrom, RangeTo: TKey]: TKeyEnumeratorProvider read GetRangeKeys;
  end;

  { TheCmpBTreeMap }

  generic TheCmpBTreeMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheCmpBTreeMap }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TItem = packed record
        Key: TKey;
        Value: TValue;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem; // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TKey;
    end;
  public type
    TKeyEnumerator = specialize TheEnumerator<TIterator, TKey>;
    TValueEnumerator = specialize TheEnumerator<TIterator, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TValue;
    function Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
    function GetCurrent(var Iterator: TIterator): TValue;
    function GetCurrentKey(var Iterator: TIterator): TKey;
    function GetFirst: TValue;
    function GetFirstKey: TKey;
    function GetLast: TValue;
    function GetLastKey: TKey;
    function GetMap(const Key: TKey): TValue;
    function GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
    function GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SetMap(const Key: TKey; const Value: TValue);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  protected
    function MissingKeyValue(const Key: TKey): TValue; virtual;
  public
    constructor Create(const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Compare(const A, B: TKey): Integer; virtual; // abstract
    function Delete(const Key: TKey): Boolean;
    function Extract(const Key: TKey; out Value: TValue): Boolean;
    function Get(const Key: TKey; out Value: TValue): Boolean;
    function Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TValue read GetFirst;
    property FirstKey: TKey read GetFirstKey;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TValue read GetLast;
    property LastKey: TKey read GetLastKey;
    property Map[const Key: TKey]: TValue read GetMap write SetMap; default;
    property Range[const RangeFrom, RangeTo: TKey]: TValueEnumeratorProvider read GetRange;
    property RangeKeys[const RangeFrom, RangeTo: TKey]: TKeyEnumeratorProvider read GetRangeKeys;
  end;

  { TheObjectBTreeMap }

  generic TheObjectBTreeMap<TKey, TValue> = class { http://code.google.com/p/fprb/wiki/TheObjectBTreeMap }
  private type
    PPage = ^TPage;
    PData = ^TDataPage;
    PIndex = ^TIndexPage;
    TPage = packed record // object
      Count: Integer;
      IsIndex: LongBool;
    end;
    TIndexPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Index: array[0..1] of record
        Child: PPage; // ^Index or data page, count in KIndex-1..2*KIndex+2 items except root
        DataPage: PData; // ^Data page, count in KIndex-1..2*KIndex+1 items except root
      end;
    end;
    TItem = packed record
        Key: TKey;
        Value: TValue;
    end;
    TDataPage = packed record // object(TPage)
      Hdr: TPage; // must be first
      Prev, Next: PData;
      Data: array[0..1] of TItem; // KData-1..2*KData items
    end;
    TIterator = record
      Page: PData;
      Index: Integer;
      UseSentinel: Boolean;
      Sentinel: TKey;
    end;
  public type
    TKeyEnumerator = specialize TheEnumerator<TIterator, TKey>;
    TValueEnumerator = specialize TheEnumerator<TIterator, TValue>;
    TKeyEnumeratorProvider = specialize TheEnumeratorProvider<TKeyEnumerator>;
    TValueEnumeratorProvider = specialize TheEnumeratorProvider<TValueEnumerator>;
  private
    FCount: Integer;
    FFirst: PPage;
    FKData: Integer;
    FKIndex: Integer;
    FLast: PPage;
    FOwnObjects: Boolean;
    FRoot: PPage;
  private
    function ExtractData(const P: PPage; const Index: Integer): TValue;
    function Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
    function GetCurrent(var Iterator: TIterator): TValue;
    function GetCurrentKey(var Iterator: TIterator): TKey;
    function GetFirst: TValue;
    function GetFirstKey: TKey;
    function GetLast: TValue;
    function GetLastKey: TKey;
    function GetMap(const Key: TKey): TValue;
    function GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
    function GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
    function Insert(const P: PPage; const Index: Integer): PPage;
    function Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
    function InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
    function MoveNext(var Iterator: TIterator): Boolean;
    function MovePrev(var Iterator: TIterator): Boolean;
    function Page(const IsIndex: Boolean; const LeftmostChild: PPage = nil): PPage;
    function Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
    procedure CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
    procedure Clear(const P: PPage);
    procedure Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
    procedure ExtractIndex(const P: PPage; const Index: Integer);
    procedure MoveLeft(const Left, P: PPage; const N: Integer = 1);
    procedure MoveRight(const P, Right: PPage; const N: Integer = 1);
    procedure Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SetMap(const Key: TKey; const Value: TValue);
    procedure SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
    procedure SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    procedure Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
    procedure Underflow(const Parent, P: PPage; const ParentIndex: Integer);
    procedure Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
    property Root: PPage Read FRoot;
  protected
    function MissingKeyValue(const Key: TKey): TValue; virtual;
  public
    constructor Create(const AOwnObjects: Boolean = True; const AKIndex: Integer = 64; const AKData: Integer = 32);
    destructor Destroy; override;
    function Compare(const A, B: TKey): Integer; virtual; // abstract
    function Delete(const Key: TKey): Boolean;
    function Extract(const Key: TKey; out Value: TValue): Boolean;
    function Get(const Key: TKey; out Value: TValue): Boolean;
    function Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean = True): Boolean;
    function Keys: TKeyEnumeratorProvider;
    function KeysReversed: TKeyEnumeratorProvider;
    function Values: TValueEnumeratorProvider;
    function ValuesReversed: TValueEnumeratorProvider;
    procedure Clear;
    property Count: Integer Read FCount;
    property First: TValue read GetFirst;
    property FirstKey: TKey read GetFirstKey;
    property KData: Integer Read FKData;
    property KIndex: Integer Read FKIndex;
    property Last: TValue read GetLast;
    property LastKey: TKey read GetLastKey;
    property Map[const Key: TKey]: TValue read GetMap write SetMap; default;
    property Range[const RangeFrom, RangeTo: TKey]: TValueEnumeratorProvider read GetRange;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
    property RangeKeys[const RangeFrom, RangeTo: TKey]: TKeyEnumeratorProvider read GetRangeKeys;
  end;

implementation

uses
  Math;

{ TheEnumerator }

function TheEnumerator.GetCurrent: TValue;
begin
  Result := FGetCurrent(FIterator);
end;

function TheEnumerator.MoveNext: Boolean;
begin
  Result := FMoveNext(FIterator);
end;

procedure TheEnumerator.Init(const InitialIterator: TIterator; const Mover: TMoveNext; const Getter: TGetCurrent);
begin
  Assert(Assigned(Mover));
  Assert(Assigned(Getter));
  FIterator := InitialIterator;
  FMoveNext := Mover;
  FGetCurrent := Getter;
end;

{ TheEnumeratorProvider }

function TheEnumeratorProvider.GetEnumerator: TProvidedEnumerator;
begin
  Result := FEnumerator;
end;

{ TheObjectVector }

function TheObjectVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheObjectVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result :=FData[Count - 1];
end;

function TheObjectVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheObjectVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheObjectVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheObjectVector.SetItems(const Index: Integer; const AValue: TItem);
begin
  Assert((Index >= 0) and (Index < Count));
  FData[Index] := AValue;
end;

procedure TheObjectVector.Sort(Left, Right: Integer; const Compare: TCompare);
var
  L, R: Integer;
  Pivot: TItem;
begin
  repeat
   L := Left;
   R := Right;
   Pivot := FData[(L + R) shr 1];
   repeat
     while Compare(Pivot, FData[L]) > 0 do
       L += 1;
     while Compare(Pivot, FData[R]) < 0 do
       R -= 1;
     if L <= R then begin
       Swap(L, R);
       L += 1;
       R -= 1;
     end;
   until L > R;
   if Left < R then
     Sort(Left, R, Compare);
   Left := L;
  until L >= Right;
end;

constructor TheObjectVector.Create(const AOwnObjects: Boolean);
begin
  inherited Create;
  OwnObjects := AOwnObjects;
end;

destructor TheObjectVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectVector.Add(const Item: TItem): Integer;
begin
  Result := Count;
  Inc(FCount);
  if Result >= Capacity then
    Capacity := 2 * Count;
  FData[Result] := Item;
end;

function TheObjectVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheObjectVector.Find(const Item: TItem; out Index: Integer): Boolean;
begin
  Index := IndexOf(Item);
  Result := Index >= 0;
end;

function TheObjectVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheObjectVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheObjectVector.IndexOf(const Item: TItem): Integer;
begin
  Assert(@Item = @Item); // hint off
  for Result := 0 to Count - 1 do
    if FData[Result] = Item then
      Exit;
  Result := -1;
end;

function TheObjectVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheObjectVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result);
end;

function TheObjectVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

function TheObjectVector.SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Result := FData[ItemAIndex];
  FData[ItemAIndex] := ItemB;
end;

procedure TheObjectVector.Clear;
var I: Integer;
begin
  if OwnObjects then
    for I := 0 to Count - 1 do
      FData[I].Free;
  FCount := 0;
  Capacity := 0;
end;

procedure TheObjectVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheObjectVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  FData[Index] := Item;
end;

procedure TheObjectVector.Kill;
begin
  OwnObjects := False;
  Free;
end;

procedure TheObjectVector.Pack;
begin
  Capacity := Count;
end;

procedure TheObjectVector.Sort(const Compare: TCompare);
begin
  if Count > 1 then
    Sort(0, Count - 1, Compare);
end;

procedure TheObjectVector.Swap(const ItemAIndex, ItemBIndex: Integer);
var Item: TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Assert((ItemBIndex >= 0) and (ItemBIndex < Count));
  Item := FData[ItemAIndex];
  FData[ItemAIndex] := FData[ItemBIndex];
  FData[ItemBIndex] := Item;
end;

procedure TheObjectVector.Wipe;
begin
  FCount := 0;
end;

{ TheVector }

procedure TheVector.Pack;
begin
  Capacity := Count;
end;

procedure TheVector.Sort;
begin
  if Count > 1 then
    Sort(0, Count - 1);
end;

procedure TheVector.Swap(const ItemAIndex, ItemBIndex: Integer);
var Item: TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Assert((ItemBIndex >= 0) and (ItemBIndex < Count));
  Item := FData[ItemAIndex];
  FData[ItemAIndex] := FData[ItemBIndex];
  FData[ItemBIndex] := Item;
end;

function TheVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheVector.SetItems(const Index: Integer; const AValue: TItem);
begin
  Assert((Index >= 0) and (Index < Count));
  FData[Index] := AValue;
end;

procedure TheVector.Sort(Left, Right: Integer);
var
  L, R: Integer;
  Pivot: TItem;
begin
  Assert(@Pivot = @Pivot); // hint off
  repeat
   L := Left;
   R := Right;
   Pivot := FData[(L + R) div 2];
   repeat
     while Pivot > FData[L] do
       L += 1;
     while Pivot < FData[R] do
       R -= 1;
     if L <= R then begin
       Swap(L, R);
       L += 1;
       R -= 1;
     end;
   until L > R;
   if Left < R then
     Sort(Left, R);
   Left := L;
  until L >= Right;
end;

destructor TheVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheVector.Add(const Item: TItem): Integer;
begin
  Result := Count;
  Inc(FCount);
  if Result >= Capacity then
    Capacity := 2 * Count;
  Initialize(FData[Result]);
  FData[Result] := Item;
end;

function TheVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheVector.Find(const Item: TItem; out Index: Integer): Boolean;
begin
  Index := IndexOf(Item);
  Result := Index >= 0;
end;

function TheVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheVector.IndexOf(const Item: TItem): Integer;
begin
  Assert(@Item = @Item); // hint off
  for Result := 0 to Count - 1 do
    if FData[Result] = Item then
      Exit;
  Result := -1;
end;

function TheVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result);
end;

function TheVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

function TheVector.SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Result := FData[ItemAIndex];
  FData[ItemAIndex] := ItemB;
end;

procedure TheVector.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

{ TheCmpVector }

procedure TheCmpVector.Pack;
begin
  Capacity := Count;
end;

procedure TheCmpVector.Sort;
begin
  if Count > 1 then
    Sort(0, Count - 1);
end;

procedure TheCmpVector.Swap(const ItemAIndex, ItemBIndex: Integer);
var Item: TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Assert((ItemBIndex >= 0) and (ItemBIndex < Count));
  if ItemAIndex = ItemBIndex then
    Exit;
  Item := FData[ItemAIndex];
  FData[ItemAIndex] := FData[ItemBIndex];
  FData[ItemBIndex] := Item;
end;

function TheCmpVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheCmpVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheCmpVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheCmpVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheCmpVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheCmpVector.SetItems(const Index: Integer; const AValue: TItem);
begin
  Assert((Index >= 0) and (Index < Count));
  FData[Index] := AValue;
end;

procedure TheCmpVector.Sort(Left, Right: Integer);
var
  L, R: Integer;
  Pivot: TItem;
begin
  repeat
   L := Left;
   R := Right;
   Pivot := FData[(L + R) div 2];
   repeat
     while Compare(Pivot, FData[L]) > 0 do
       L += 1;
     while Compare(Pivot, FData[R]) < 0 do
       R -= 1;
     if L <= R then begin
       Swap(L, R);
       L += 1;
       R -= 1;
     end;
   until L > R;
   if Left < R then
     Sort(Left, R);
   Left := L;
  until L >= Right;
end;

destructor TheCmpVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpVector.Add(const Item: TItem): Integer;
begin
  Result := Count;
  Inc(FCount);
  if Result >= Capacity then
    Capacity := 2 * Count;
  Initialize(FData[Result]);
  FData[Result] := Item;
end;

function TheCmpVector.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheCmpVector.Find(const Item: TItem; out Index: Integer): Boolean;
begin
  Index := IndexOf(Item);
  Result := Index >= 0;
end;

function TheCmpVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheCmpVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheCmpVector.IndexOf(const Item: TItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Compare(FData[Result], Item) = 0 then
      Exit;
  Result := -1;
end;

function TheCmpVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheCmpVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result);
end;

function TheCmpVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

function TheCmpVector.SwapWith(const ItemAIndex: Integer; const ItemB: TItem): TItem;
begin
  Assert((ItemAIndex >= 0) and (ItemAIndex < Count));
  Result := FData[ItemAIndex];
  FData[ItemAIndex] := ItemB;
end;

procedure TheCmpVector.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheCmpVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheCmpVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

{ TheSortVector }

procedure TheSortVector.Pack;
begin
  Capacity := Count;
end;

function TheSortVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheSortVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheSortVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheSortVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheSortVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheSortVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheSortVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

destructor TheSortVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheSortVector.Add(const Item: TItem): Integer;
begin
  Find(Item, Result);
  Insert(Result, Item);
end;

function TheSortVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheSortVector.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H: Integer;
begin
  Assert(@Item = @Item); // hint off
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    if Item < FData[Index] then
      H := Index - 1
    else if Item = FData[Index] then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheSortVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheSortVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheSortVector.IndexOf(const Item: TItem): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

function TheSortVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheSortVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheSortVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheSortVector.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheSortVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheSortVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

{ TheCmpSortVector }

procedure TheCmpSortVector.Pack;
begin
  Capacity := Count;
end;

function TheCmpSortVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpSortVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpSortVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheCmpSortVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheCmpSortVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheCmpSortVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheCmpSortVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

destructor TheCmpSortVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpSortVector.Add(const Item: TItem): Integer;
begin
  Find(Item, Result);
  Insert(Result, Item);
end;

function TheCmpSortVector.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpSortVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheCmpSortVector.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(Item, FData[Index]);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheCmpSortVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheCmpSortVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheCmpSortVector.IndexOf(const Item: TItem): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

function TheCmpSortVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheCmpSortVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheCmpSortVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheCmpSortVector.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheCmpSortVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheCmpSortVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

{ TheObjectSortVector }

procedure TheObjectSortVector.Pack;
begin
  Capacity := Count;
end;

procedure TheObjectSortVector.Wipe;
begin
  FCount := 0;
end;

function TheObjectSortVector.GetItems(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectSortVector.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectSortVector.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheObjectSortVector.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheObjectSortVector.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheObjectSortVector.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheObjectSortVector.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

constructor TheObjectSortVector.Create(const AOwnObjects: Boolean);
begin
  inherited Create;
  OwnObjects := AOwnObjects;
end;

destructor TheObjectSortVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectSortVector.Add(const Item: TItem): Integer;
begin
  Find(Item, Result);
  Insert(Result, Item);
end;

function TheObjectSortVector.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheObjectSortVector.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheObjectSortVector.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(Item, FData[Index]);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheObjectSortVector.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheObjectSortVector.Has(const Item: TItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TheObjectSortVector.IndexOf(const Item: TItem): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

function TheObjectSortVector.Push(const Item: TItem): TItem;
begin
  Add(Item);
  Result := Item;
end;

function TheObjectSortVector.Remove(const Item: TItem): Integer;
begin
  if Find(Item, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheObjectSortVector.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheObjectSortVector.Clear;
var I: Integer;
begin
  if OwnObjects then
    for I := 0 to Count - 1 do
      FData[I].Free;
  FCount := 0;
  Capacity := 0;
end;

procedure TheObjectSortVector.Delete(const Index: Integer);
begin
  Extract(Index);
end;

procedure TheObjectSortVector.Kill;
begin
  OwnObjects := False;
  Free;
end;

procedure TheObjectSortVector.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  FData[Index] := Item;
end;

{ TheVectorSet }

function TheVectorSet.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheVectorSet.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H: Integer;
begin
  Assert(@Item = @Item); // hint off
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    if Item < FData[Index] then
      H := Index - 1
    else if Item = FData[Index] then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheVectorSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheVectorSet.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheVectorSet.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheVectorSet.GetMembership(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
end;

function TheVectorSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

procedure TheVectorSet.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

function TheVectorSet.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheVectorSet.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheVectorSet.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheVectorSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

destructor TheVectorSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheVectorSet.Exclude(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
  if Result then
    Extract(Index);
end;

function TheVectorSet.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheVectorSet.Include(const Item: TItem): Boolean;
var Index: Integer;
begin
  if Find(Item, Index) then
    Exit(True);
  Insert(Index, Item);
  Result := False;
end;

function TheVectorSet.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheVectorSet.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheVectorSet.Pack;
begin
  Capacity := Count;
end;

{ TheCmpVectorSet }

function TheCmpVectorSet.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheCmpVectorSet.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(Item, FData[Index]);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheCmpVectorSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheCmpVectorSet.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpVectorSet.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpVectorSet.GetMembership(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
end;

function TheCmpVectorSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

procedure TheCmpVectorSet.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

function TheCmpVectorSet.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheCmpVectorSet.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheCmpVectorSet.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheCmpVectorSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

destructor TheCmpVectorSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpVectorSet.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpVectorSet.Exclude(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
  if Result then
    Extract(Index);
end;

function TheCmpVectorSet.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheCmpVectorSet.Include(const Item: TItem): Boolean;
var Index: Integer;
begin
  if Find(Item, Index) then
    Exit(True);
  Insert(Index, Item);
  Result := False;
end;

function TheCmpVectorSet.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheCmpVectorSet.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheCmpVectorSet.Pack;
begin
  Capacity := Count;
end;

{ TheObjectVectorSet }

function TheObjectVectorSet.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheObjectVectorSet.Find(const Item: TItem; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(Item, FData[Index]);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheObjectVectorSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheObjectVectorSet.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectVectorSet.GetCurrent(var Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectVectorSet.GetMembership(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
end;

function TheObjectVectorSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

procedure TheObjectVectorSet.Insert(const Index: Integer; const Item: TItem);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index] := Item;
end;

function TheObjectVectorSet.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheObjectVectorSet.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheObjectVectorSet.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheObjectVectorSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

constructor TheObjectVectorSet.Create(const AOwnObjects: Boolean);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
end;

destructor TheObjectVectorSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectVectorSet.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheObjectVectorSet.Exclude(const Item: TItem): Boolean;
var Index: Integer;
begin
  Result := Find(Item, Index);
  if Result then
    Extract(Index);
end;

function TheObjectVectorSet.GetEnumerator: TEnumerator;
begin
  Result.Init(-1, @MoveNext, @GetCurrent);
end;

function TheObjectVectorSet.Include(const Item: TItem): Boolean;
var Index: Integer;
begin
  if Find(Item, Index) then
    Exit(True);
  Insert(Index, Item);
  Result := False;
end;

function TheObjectVectorSet.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrent);
end;

procedure TheObjectVectorSet.Clear;
var I: Integer;
begin
  if OwnObjects then
    for I := 0 to Count - 1 do
      FData[I].Free;
  FCount := 0;
  Capacity := 0;
end;

procedure TheObjectVectorSet.Kill;
begin
  OwnObjects := False;
  Free;
end;

procedure TheObjectVectorSet.Pack;
begin
  Capacity := Count;
end;

procedure TheObjectVectorSet.Wipe;
begin
  FCount := 0;
end;

{ TheVectorMap }

function TheVectorMap.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheVectorMap.Find(const AKey: TKey; out Index: Integer): Boolean;
var L, H: Integer;
begin
  Assert(@AKey = @AKey); // hint off
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    if AKey < FData[Index].Key then
      H := Index - 1
    else if AKey = FData[Index].Key then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheVectorMap.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheVectorMap.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheVectorMap.GetMap(const AKey: TKey): TValue;
var Index: Integer;
begin
  if Find(AKey, Index) then
    Exit(FData[Index].Value);
  Result := MissingKeyValue(AKey);
end;

function TheVectorMap.GetKey(const Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheVectorMap.GetCurrentKey(var Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheVectorMap.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheVectorMap.GetValue(const Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

function TheVectorMap.GetCurrentValue(var Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

procedure TheVectorMap.Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index].Key := AKey;
  FData[Index].Value := AValue;
end;

function TheVectorMap.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheVectorMap.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheVectorMap.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheVectorMap.SetMap(const AKey: TKey; const AValue: TValue);
var Index: Integer;
begin
  if Find(AKey, Index) then
    FData[Index].Value := AValue
  else
    Insert(Index, AKey, AValue);
end;

function TheVectorMap.MissingKeyValue(const AKey: TKey): TValue;
begin
  Assert(@AKey = @AKey); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

destructor TheVectorMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheVectorMap.Has(const AKey: TKey): Boolean;
var Index: Integer;
begin
  Result := Find(AKey, Index);
end;

function TheVectorMap.IndexOf(const AKey: TKey): Integer;
begin
  if not Find(AKey, Result) then
    Result := -1;
end;

function TheVectorMap.Keys: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentKey);
end;

function TheVectorMap.KeysReversed: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentKey);
end;

function TheVectorMap.Remove(const AKey: TKey): Integer;
begin
  if Find(AKey, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheVectorMap.Values: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentValue);
end;

function TheVectorMap.ValuesReversed: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentValue);
end;

procedure TheVectorMap.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheVectorMap.Delete(const Index: Integer);
begin
  Assert((Index >= 0) and (Index < Count));
  Extract(Index);
end;

procedure TheVectorMap.Pack;
begin
  Capacity := Count;
end;

{ TheCmpVectorMap }

function TheCmpVectorMap.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index]);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheCmpVectorMap.Find(const AKey: TKey; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(AKey, FData[Index].Key);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheCmpVectorMap.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheCmpVectorMap.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheCmpVectorMap.GetMap(const AKey: TKey): TValue;
var Index: Integer;
begin
  if Find(AKey, Index) then
    Exit(FData[Index].Value);
  Result := MissingKeyValue(AKey);
end;

function TheCmpVectorMap.GetKey(const Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheCmpVectorMap.GetCurrentKey(var Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheCmpVectorMap.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheCmpVectorMap.GetValue(const Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

function TheCmpVectorMap.GetCurrentValue(var Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

procedure TheCmpVectorMap.Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index]);
  FData[Index].Key := AKey;
  FData[Index].Value := AValue;
end;

function TheCmpVectorMap.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheCmpVectorMap.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheCmpVectorMap.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheCmpVectorMap.SetMap(const AKey: TKey; const AValue: TValue);
var Index: Integer;
begin
  if Find(AKey, Index) then
    FData[Index].Value := AValue
  else
    Insert(Index, AKey, AValue);
end;

function TheCmpVectorMap.MissingKeyValue(const AKey: TKey): TValue;
begin
  Assert(@AKey = @AKey); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

destructor TheCmpVectorMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpVectorMap.Compare(const A, B: TKey): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpVectorMap.Has(const AKey: TKey): Boolean;
var Index: Integer;
begin
  Result := Find(AKey, Index);
end;

function TheCmpVectorMap.IndexOf(const AKey: TKey): Integer;
begin
  if not Find(AKey, Result) then
    Result := -1;
end;

function TheCmpVectorMap.Keys: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentKey);
end;

function TheCmpVectorMap.KeysReversed: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentKey);
end;

function TheCmpVectorMap.Remove(const AKey: TKey): Integer;
begin
  if Find(AKey, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheCmpVectorMap.Values: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentValue);
end;

function TheCmpVectorMap.ValuesReversed: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentValue);
end;

procedure TheCmpVectorMap.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Finalize(FData[I]);
  FCount := 0;
  Capacity := 0;
end;

procedure TheCmpVectorMap.Delete(const Index: Integer);
begin
  Assert((Index >= 0) and (Index < Count));
  Extract(Index);
end;

procedure TheCmpVectorMap.Pack;
begin
  Capacity := Count;
end;

{ TheObjectVectorMap }

function TheObjectVectorMap.Extract(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
  Dec(FCount);
  Finalize(FData[Index].Key);
  Move(FData[Index + 1], FData[Index], (Count - Index) * SizeOf(TItem));
end;

function TheObjectVectorMap.Find(const AKey: TKey; out Index: Integer): Boolean;
var L, H, Cmp: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do begin
    Index := (L + H) shr 1;
    Cmp := Compare(AKey, FData[Index].Key);
    if Cmp < 0 then
      H := Index - 1
    else if Cmp = 0 then
      Exit(True)
    else
      L := Index + 1;
  end;
  Index := L;
  Result := False;
end;

function TheObjectVectorMap.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := FData[0];
end;

function TheObjectVectorMap.GetItem(const Index: Integer): TItem;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index];
end;

function TheObjectVectorMap.GetMap(const AKey: TKey): TValue;
var Index: Integer;
begin
  if Find(AKey, Index) then
    Exit(FData[Index].Value);
  Result := MissingKeyValue(AKey);
end;

function TheObjectVectorMap.GetKey(const Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheObjectVectorMap.GetCurrentKey(var Index: Integer): TKey;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Key;
end;

function TheObjectVectorMap.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := FData[Count - 1];
end;

function TheObjectVectorMap.GetValue(const Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

function TheObjectVectorMap.GetCurrentValue(var Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := FData[Index].Value;
end;

procedure TheObjectVectorMap.Insert(const Index: Integer; const AKey: TKey; const AValue: TValue);
begin
  Assert((Index >= 0) and (Index <= Count));
  if Count = Capacity then
    Capacity := 2 * (Count + 1);
  Move(FData[Index], FData[Index + 1], (Count - Index) * SizeOf(TItem));
  Inc(FCount);
  Initialize(FData[Index].Key);
  FData[Index].Key := AKey;
  FData[Index].Value := AValue;
end;

function TheObjectVectorMap.MoveNext(var Index: Integer): Boolean;
begin
  Inc(Index);
  Result := Index < Count;
end;

function TheObjectVectorMap.MovePrev(var Index: Integer): Boolean;
begin
  Dec(Index);
  Result := Index >= 0;
end;

procedure TheObjectVectorMap.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);
  if AValue = Capacity then
    Exit;
  if AValue < Count then
    AValue := Count;
  ReAllocMem(FData, AValue * SizeOf(FData[0]));
  FCapacity := AValue;
end;

procedure TheObjectVectorMap.SetMap(const AKey: TKey; const AValue: TValue);
var Index: Integer;
begin
  if Find(AKey, Index) then
    FData[Index].Value := AValue
  else
    Insert(Index, AKey, AValue);
end;

function TheObjectVectorMap.MissingKeyValue(const AKey: TKey): TValue;
begin
  Assert(@AKey = @AKey); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

constructor TheObjectVectorMap.Create(const AOwnObjects: Boolean);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
end;

destructor TheObjectVectorMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectVectorMap.Compare(const A, B: TKey): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheObjectVectorMap.Has(const AKey: TKey): Boolean;
var Index: Integer;
begin
  Result := Find(AKey, Index);
end;

function TheObjectVectorMap.IndexOf(const AKey: TKey): Integer;
begin
  if not Find(AKey, Result) then
    Result := -1;
end;

function TheObjectVectorMap.Keys: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentKey);
end;

function TheObjectVectorMap.KeysReversed: TKeyEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentKey);
end;

function TheObjectVectorMap.Remove(const AKey: TKey): Integer;
begin
  if Find(AKey, Result) then
    Delete(Result)
  else
    Result := -1;
end;

function TheObjectVectorMap.Values: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(-1, @MoveNext, @GetCurrentValue);
end;

function TheObjectVectorMap.ValuesReversed: TValueEnumeratorProvider;
begin
  Result.FEnumerator.Init(Count, @MovePrev, @GetCurrentValue);
end;

procedure TheObjectVectorMap.Clear;
var I: Integer;
begin
  for I := 0 to Count - 1 do begin
    Finalize(FData[I].Key);
    if OwnObjects then
      FData[I].Value.Free;
  end;
  FCount := 0;
  Capacity := 0;
end;

procedure TheObjectVectorMap.Delete(const Index: Integer);
begin
  Assert((Index >= 0) and (Index < Count));
  Extract(Index);
end;

procedure TheObjectVectorMap.Pack;
begin
  Capacity := Count;
end;

{ TheList }

destructor TheList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheList.InsertAfter(const After: PNode; const AItem: TItem): PNode;
begin
  Result := InsertAfter_(After, NewNode(AItem));
end;

function TheList.InsertAfter_(const After, Node: PNode): PNode;
var
  Node_: PNode_ absolute Node;
  After_: PNode_ absolute After;
begin
  if After^.Next = nil then
    FLast := Node
  else
    After_^.FNext^.FPrev := Node_;
  Node_^.FNext := After_^.FNext;
  After_^.FNext := Node_;
  Node_^.FPrev := After_;
  Inc(FCount);
  Result := Node;
end;

function TheList.InsertBack(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if Last = nil then begin
    FFirst := Node;
    FLast := Node;
    Node_^.FPrev := nil;
    Node_^.FNext := nil;
    FCount := 1;
    Exit(Node);
  end;
  Result := InsertAfter_(Last, Node);
end;

function TheList.InsertBefore(const AItem: TItem; const Before: PNode): PNode;
begin
  Result := InsertBefore_(NewNode(AItem), Before);
end;

function TheList.InsertBefore_(const Node, Before: PNode): PNode;
var
  Node_: PNode_ absolute Node;
  Before_: PNode_ absolute Before;
begin
  if Before_^.FPrev = nil then
    FFirst := Node
  else
    Before_^.FPrev^.FNext := Node_;
  Node_^.FPrev := Before_^.FPrev;
  Before_^.FPrev := Node_;
  Node_^.FNext := Before_;
  Inc(FCount);
  Result := Node;
end;

function TheList.InsertFront(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if First = nil then begin
    FFirst := Node;
    FLast := Node;
    Node_^.FPrev := nil;
    Node_^.FNext := nil;
    FCount := 1;
    Exit(Node);
  end;
  Result := InsertBefore_(Node, First);
end;

function TheList.MoveToBack(const Node: PNode): PNode;
begin
  Result := InsertBack(Extract(Node));
end;

function TheList.MoveToFront(const Node: PNode): PNode;
begin
  Result := InsertFront(Extract(Node));
end;

function TheList.NewNode(const AItem: TItem): PNode;
var Node_: PNode_ absolute Result;
begin
  New(Result);
  Node_^.FPrev := nil;
  Node_^.FNext := nil;
  Result^.Item := AItem;
end;

function TheList.PushBack(const AItem: TItem): PNode;
begin
  Result := InsertBack(NewNode(AItem));
end;

function TheList.PushFront(const AItem: TItem): PNode;
begin
  Result := InsertFront(NewNode(AItem));
end;

function TheList.Extract(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if Node^.Prev = nil then
    FFirst := Node^.Next
  else
    Node_^.FPrev^.FNext := Node_^.FNext;
  if Node^.Next = nil then
    FLast := Node^.Prev
  else
    Node_^.FNext^.FPrev := Node_^.FPrev;
  Node_^.FPrev := nil;
  Node_^.FNext := nil;
  Dec(FCount);
  Result := Node;
end;

function TheList.NewIterator: TIterator;
begin
  Result.List := Self;
  Result.Node := nil;
end;

function TheList.GetEnumerator: TEnumerator;
begin
  Result.Init(NewIterator, @MoveNext, @CurrentItem);
end;

function TheList.CurrentItem(var Iterator: TIterator): TItem;
begin
  Result := Iterator.Node^.Item;
end;

function TheList.MoveNext(var Iterator: TIterator): Boolean;
begin
  if Iterator.Node <> nil then begin
    Iterator.Node := Iterator.Node^.Next;
    if Iterator.Node <> nil then
      Exit(True);
    Iterator.List := nil;
    Exit(False);
  end;
  if Iterator.List <> nil then
    Iterator.Node := PNode(TheList(Iterator.List).First);
  Result := Iterator.Node <> nil;
end;

function TheList.MovePrev(var Iterator: TIterator): Boolean;
begin
  if Iterator.Node <> nil then begin
    Iterator.Node := Iterator.Node^.Prev;
    if Iterator.Node <> nil then
      Exit(True);
    Iterator.List := nil;
    Exit(False);
  end;
  if Iterator.List <> nil then
    Iterator.Node := PNode(TheList(Iterator.List).Last);
  Result := Iterator.Node <> nil;
end;

function TheList.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(NewIterator, @MovePrev, @CurrentItem);
end;

function TheList.MoveAfter(const After, Node: PNode): PNode;
begin
  Assert(After <> Node);
  Result := InsertAfter_(After, Extract(Node));
end;

function TheList.MoveBefore(const Node, Before: PNode): PNode;
begin
  Assert(Node <> Before);
  Result := InsertBefore_(Extract(Node), Before);
end;

procedure TheList.Remove(Node: PNode);
begin
  Extract(Node);
  Dispose(Node);
end;

procedure TheList.Clear;
var Node, Next: PNode;
begin
  Node := First;
  while Node <> nil do begin
    Next := Node^.Next;
    Dispose(Node);
    Node := Next;
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

{ TheObjectList }

destructor TheObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectList.InsertAfter(const After: PNode; const AItem: TItem): PNode;
begin
  Result := InsertAfter_(After, NewNode(AItem));
end;

function TheObjectList.InsertAfter_(const After, Node: PNode): PNode;
var
  Node_: PNode_ absolute Node;
  After_: PNode_ absolute After;
begin
  if After^.Next = nil then
    FLast := Node
  else
    After_^.FNext^.FPrev := Node_;
  Node_^.FNext := After_^.FNext;
  After_^.FNext := Node_;
  Node_^.FPrev := After_;
  Inc(FCount);
  Result := Node;
end;

function TheObjectList.InsertBack(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if Last = nil then begin
    FFirst := Node;
    FLast := Node;
    Node_^.FPrev := nil;
    Node_^.FNext := nil;
    FCount := 1;
    Exit(Node);
  end;
  Result := InsertAfter_(Last, Node);
end;

function TheObjectList.InsertBefore(const AItem: TItem; const Before: PNode): PNode;
begin
  Result := InsertBefore_(NewNode(AItem), Before);
end;

function TheObjectList.InsertBefore_(const Node, Before: PNode): PNode;
var
  Node_: PNode_ absolute Node;
  Before_: PNode_ absolute Before;
begin
  if Before_^.FPrev = nil then
    FFirst := Node
  else
    Before_^.FPrev^.FNext := Node_;
  Node_^.FPrev := Before_^.FPrev;
  Before_^.FPrev := Node_;
  Node_^.FNext := Before_;
  Inc(FCount);
  Result := Node;
end;

function TheObjectList.InsertFront(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if First = nil then begin
    FFirst := Node;
    FLast := Node;
    Node_^.FPrev := nil;
    Node_^.FNext := nil;
    FCount := 1;
    Exit(Node);
  end;
  Result := InsertBefore_(Node, First);
end;

function TheObjectList.MoveToBack(const Node: PNode): PNode;
begin
  Result := InsertBack(Extract(Node));
end;

function TheObjectList.MoveToFront(const Node: PNode): PNode;
begin
  Result := InsertFront(Extract(Node));
end;

function TheObjectList.NewNode(const AItem: TItem): PNode;
var Node_: PNode_ absolute Result;
begin
  New(Result);
  Node_^.FPrev := nil;
  Node_^.FNext := nil;
  Result^.Item := AItem;
end;

function TheObjectList.PushBack(const AItem: TItem): PNode;
begin
  Result := InsertBack(NewNode(AItem));
end;

function TheObjectList.PushFront(const AItem: TItem): PNode;
begin
  Result := InsertFront(NewNode(AItem));
end;

function TheObjectList.Extract(const Node: PNode): PNode;
var Node_: PNode_ absolute Node;
begin
  if Node^.Prev = nil then
    FFirst := Node^.Next
  else
    Node_^.FPrev^.FNext := Node_^.FNext;
  if Node^.Next = nil then
    FLast := Node^.Prev
  else
    Node_^.FNext^.FPrev := Node_^.FPrev;
  Node_^.FPrev := nil;
  Node_^.FNext := nil;
  Dec(FCount);
  Result := Node;
end;

function TheObjectList.NewIterator: TIterator;
begin
  Result.List := Self;
  Result.Node := nil;
end;

function TheObjectList.GetEnumerator: TEnumerator;
begin
  Result.Init(NewIterator, @MoveNext, @CurrentItem);
end;

function TheObjectList.CurrentItem(var Iterator: TIterator): TItem;
begin
  Result := Iterator.Node^.Item;
end;

function TheObjectList.MoveNext(var Iterator: TIterator): Boolean;
begin
  if Iterator.Node <> nil then begin
    Iterator.Node := Iterator.Node^.Next;
    if Iterator.Node <> nil then
      Exit(True);
    Iterator.List := nil;
    Exit(False);
  end;
  if Iterator.List <> nil then
    Iterator.Node := PNode(TheObjectList(Iterator.List).First);
  Result := Iterator.Node <> nil;
end;

function TheObjectList.MovePrev(var Iterator: TIterator): Boolean;
begin
  if Iterator.Node <> nil then begin
    Iterator.Node := Iterator.Node^.Prev;
    if Iterator.Node <> nil then
      Exit(True);
    Iterator.List := nil;
    Exit(False);
  end;
  if Iterator.List <> nil then
    Iterator.Node := PNode(TheObjectList(Iterator.List).Last);
  Result := Iterator.Node <> nil;
end;

function TheObjectList.Reversed: TEnumeratorProvider;
begin
  Result.FEnumerator.Init(NewIterator, @MovePrev, @CurrentItem);
end;

function TheObjectList.MoveAfter(const After, Node: PNode): PNode;
begin
  Assert(After <> Node);
  Result := InsertAfter_(After, Extract(Node));
end;

function TheObjectList.MoveBefore(const Node, Before: PNode): PNode;
begin
  Assert(Node <> Before);
  Result := InsertBefore_(Extract(Node), Before);
end;

procedure TheObjectList.Remove(Node: PNode);
begin
  Extract(Node);
  if OwnObjects then
    Node^.Item.Free;
  Dispose(Node);
end;

constructor TheObjectList.Create(const AOwnObjects: Boolean);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
end;

procedure TheObjectList.Clear;
var Node, Next: PNode;
begin
  Node := First;
  while Node <> nil do begin
    if OwnObjects then
      Node^.Item.Free;
    Next := Node^.Next;
    Dispose(Node);
    Node := Next;
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

// BTrees section, keep last ===================================================

{$R- do not remove, required for BTrees implementation}

{ TheBTreeSet }

procedure TheBTreeSet.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheBTreeSet.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheBTreeSet.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else
    for I := 0 to P^.Count - 1 do
      Finalize(PData(P)^.Data[I]);
  FreeMem(P);
end;

procedure TheBTreeSet.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheBTreeSet.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheBTreeSet.Create(const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

destructor TheBTreeSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheBTreeSet.Exclude(const Item: TItem): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Item, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheBTreeSet.GetEnumerator: TEnumerator;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheBTreeSet.Include(const Item: TItem): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      if Find(P, Item, Index) then // Key found
        Exit(True);

      if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Item)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Item);
        Exit(False);
      end;
    until False
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Item);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheBTreeSet.Reversed: TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheBTreeSet.ExtractData(const P: PPage; const Index: Integer): TItem;
begin
  Result := PData(P)^.Data[Index];
  Finalize(PData(P)^.Data[Index]);
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheBTreeSet.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheBTreeSet.Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
type PItem = ^TItem;
var
  L, H: Integer;
  MidItem: PItem;
  Mid: Integer absolute Index;
begin
  Assert(@Item = @Item); // hint off
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidItem := @PIndex(P)^.Index[Mid].DataPage^.Data[0]
    else
      MidItem := @PData(P)^.Data[Mid];
    if Item > MidItem^ then
      L := Mid + 1
    else if Item = MidItem^ then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheBTreeSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0];
end;

function TheBTreeSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1];
end;

function TheBTreeSet.GetCurrent(var Iterator: TIterator): TItem;
begin
  Result := Iterator.Page^.Data[Iterator.Index];
end;

function TheBTreeSet.GetMembership(const Item: TItem): Boolean;
var
  P: PData;
  Index: Integer;
begin
  Result := Seek(Item, P, Index);
end;

function TheBTreeSet.GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if RangeFrom <= RangeTo then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheBTreeSet.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheBTreeSet.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheBTreeSet.InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
begin
  Result := Insert(P, Index);
  Initialize(PData(P)^.Data[Index]);
  PData(P)^.Data[Index] := Item;
  Inc(FCount);
end;

function TheBTreeSet.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Iterator.Page^.Data[Iterator.Index] <= Iterator.Sentinel));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheBTreeSet.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Iterator.Page^.Data[Iterator.Index] >= Iterator.Sentinel));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheBTreeSet.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

procedure TheBTreeSet.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheBTreeSet.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Item);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Item)
    end else
      InsertItem(Right, 0, Item);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Item);
end;

procedure TheBTreeSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

function TheBTreeSet.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheBTreeSet.Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Item, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

procedure TheBTreeSet.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Item)
  else
    InsertItem(P, Index, Item);
end;

procedure TheBTreeSet.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheBTreeSet.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheBTreeSet.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

{ TheBTreeMap }

procedure TheBTreeMap.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheBTreeMap.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheBTreeMap.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else
    for I := 0 to P^.Count - 1 do
      Finalize(PData(P)^.Data[I]);
  FreeMem(P);
end;

procedure TheBTreeMap.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheBTreeMap.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheBTreeMap.Create(const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

function TheBTreeMap.Delete(const Key: TKey): Boolean;
var Dummy: TValue;
begin
  Result := Extract(Key, Dummy);
end;

destructor TheBTreeMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheBTreeMap.ExtractData(const P: PPage; const Index: Integer): TValue;
begin
  Result := PData(P)^.Data[Index].Value;
  Finalize(PData(P)^.Data[Index]);
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheBTreeMap.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheBTreeMap.Extract(const Key: TKey; out Value: TValue): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            Value := ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          Value := ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheBTreeMap.Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
type PKey = ^TKey;
var
  L, H: Integer;
  MidKey: PKey;
  Mid: Integer absolute Index;
begin
  Assert(@Key = @Key); // hint offf
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidKey := @PIndex(P)^.Index[Mid].DataPage^.Data[0].Key
    else
      MidKey := @PData(P)^.Data[Mid].Key;
    if Key > MidKey^ then
      L := Mid + 1
    else if Key = MidKey^ then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheBTreeMap.GetCurrent(var Iterator: TIterator): TValue;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Value;
end;

function TheBTreeMap.GetCurrentKey(var Iterator: TIterator): TKey;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Key;
end;

function TheBTreeMap.GetFirst: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Value;
end;

function TheBTreeMap.GetFirstKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Key;
end;

function TheBTreeMap.GetLast: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Value;
end;

function TheBTreeMap.GetLastKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Key;
end;

function TheBTreeMap.GetMap(const Key: TKey): TValue;
begin
  if not Get(Key, Result) then
    Result := MissingKeyValue(Key);
end;

function TheBTreeMap.GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if RangeFrom <= RangeTo then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheBTreeMap.GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if RangeFrom <= RangeTo then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheBTreeMap.Get(const Key: TKey; out Value: TValue): Boolean;
var
  P: PPage;
  Index: Integer;
begin
  Result := False;
  P := Root;
  if P <> nil then // tree is non empty
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Value := PIndex(P)^.Index[Index].DataPage^.Data[0].Value
        else
          Value := PData(P)^.Data[Index].Value
      else if P^.IsIndex then
        P := PIndex(P)^.Index[Index].Child
      else
        break; // give up
    until Result;
end;

function TheBTreeMap.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheBTreeMap.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheBTreeMap.InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
begin
  Result := Insert(P, Index);
  Initialize(PData(P)^.Data[Index]);
  PData(P)^.Data[Index].Key := Key;
  PData(P)^.Data[Index].Value := Value;
  Inc(FCount);
end;

procedure TheBTreeMap.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

function TheBTreeMap.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Iterator.Page^.Data[Iterator.Index].Key <= Iterator.Sentinel));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheBTreeMap.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Iterator.Page^.Data[Iterator.Index].Key >= Iterator.Sentinel));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheBTreeMap.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheBTreeMap.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Key, Value);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Key, Value)
    end else
      InsertItem(Right, 0, Key, Value);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Key, Value);
end;

procedure TheBTreeMap.SetMap(const Key: TKey; const Value: TValue);
begin
  Put(Key, Value);
end;

function TheBTreeMap.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheBTreeMap.Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Key, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

function TheBTreeMap.Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean): Boolean;
var Dummy: TValue;
begin
  Result := Put(Key, Value, Dummy, CanOverwrite);
end;

function TheBTreeMap.Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Swap(PIndex(P)^.Index[Index].DataPage^.Data[0].Value, Value, Prev, CanOverwrite)
        else
          Swap(PData(P)^.Data[Index].Value, Value, Prev, CanOverwrite)
      else if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Key, Value)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Key, Value);
        break;
      end;
    until Result
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Key, Value);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheBTreeMap.Keys: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
end;

function TheBTreeMap.KeysReversed: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheBTreeMap.Values: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheBTreeMap.ValuesReversed: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

procedure TheBTreeMap.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Key, Value)
  else
    InsertItem(P, Index, Key, Value);
end;

procedure TheBTreeMap.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheBTreeMap.Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
begin
  Prev := Dest;
  if CanOverwrite then
    Dest := Value;
end;

procedure TheBTreeMap.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheBTreeMap.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

function TheBTreeMap.MissingKeyValue(const Key: TKey): TValue;
begin
  Assert(@Key = @Key); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

{ TheCmpBTreeSet }

procedure TheCmpBTreeSet.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheCmpBTreeSet.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheCmpBTreeSet.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else
    for I := 0 to P^.Count - 1 do
      Finalize(PData(P)^.Data[I]);
  FreeMem(P);
end;

procedure TheCmpBTreeSet.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheCmpBTreeSet.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheCmpBTreeSet.Create(const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

destructor TheCmpBTreeSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpBTreeSet.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpBTreeSet.Exclude(const Item: TItem): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Item, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheCmpBTreeSet.GetEnumerator: TEnumerator;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheCmpBTreeSet.Include(const Item: TItem): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      if Find(P, Item, Index) then // Key found
        Exit(True);

      if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Item)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Item);
        Exit(False);
      end;
    until False
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Item);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheCmpBTreeSet.Reversed: TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheCmpBTreeSet.ExtractData(const P: PPage; const Index: Integer): TItem;
begin
  Result := PData(P)^.Data[Index];
  Finalize(PData(P)^.Data[Index]);
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheCmpBTreeSet.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheCmpBTreeSet.Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
type PItem = ^TItem;
var
  L, H, Cmp: Integer;
  MidItem: PItem;
  Mid: Integer absolute Index;
begin
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidItem := @PIndex(P)^.Index[Mid].DataPage^.Data[0]
    else
      MidItem := @PData(P)^.Data[Mid];
    Cmp := Compare(Item, MidItem^);
    if Cmp > 0 then
      L := Mid + 1
    else if Cmp = 0 then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheCmpBTreeSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0];
end;

function TheCmpBTreeSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1];
end;

function TheCmpBTreeSet.GetCurrent(var Iterator: TIterator): TItem;
begin
  Result := Iterator.Page^.Data[Iterator.Index];
end;

function TheCmpBTreeSet.GetMembership(const Item: TItem): Boolean;
var
  P: PData;
  Index: Integer;
begin
  Result := Seek(Item, P, Index);
end;

function TheCmpBTreeSet.GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheCmpBTreeSet.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheCmpBTreeSet.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheCmpBTreeSet.InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
begin
  Result := Insert(P, Index);
  Initialize(PData(P)^.Data[Index]);
  PData(P)^.Data[Index] := Item;
  Inc(FCount);
end;

function TheCmpBTreeSet.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index], Iterator.Sentinel) <= 0));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheCmpBTreeSet.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index], Iterator.Sentinel) >= 0));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheCmpBTreeSet.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

procedure TheCmpBTreeSet.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheCmpBTreeSet.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Item);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Item)
    end else
      InsertItem(Right, 0, Item);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Item);
end;

procedure TheCmpBTreeSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

function TheCmpBTreeSet.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheCmpBTreeSet.Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Item, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

procedure TheCmpBTreeSet.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Item)
  else
    InsertItem(P, Index, Item);
end;

procedure TheCmpBTreeSet.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheCmpBTreeSet.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheCmpBTreeSet.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

{ TheObjectBTreeSet }

procedure TheObjectBTreeSet.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheObjectBTreeSet.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheObjectBTreeSet.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else if OwnObjects then
    for I := 0 to P^.Count - 1 do
      PData(P)^.Data[I].Free;
  FreeMem(P);
end;

procedure TheObjectBTreeSet.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheObjectBTreeSet.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheObjectBTreeSet.Create(const AOwnObjects: Boolean; const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

destructor TheObjectBTreeSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectBTreeSet.Compare(const A, B: TItem): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheObjectBTreeSet.Exclude(const Item: TItem): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Item, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheObjectBTreeSet.GetEnumerator: TEnumerator;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheObjectBTreeSet.Include(const Item: TItem): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      if Find(P, Item, Index) then // Key found
        Exit(True);

      if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Item)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Item);
        Exit(False);
      end;
    until False
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Item);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheObjectBTreeSet.Reversed: TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheObjectBTreeSet.ExtractData(const P: PPage; const Index: Integer): TItem;
begin
  Result := PData(P)^.Data[Index];
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheObjectBTreeSet.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheObjectBTreeSet.Find(const P: PPage; const Item: TItem; out Index: Integer): Boolean;
type PItem = ^TItem;
var
  L, H, Cmp: Integer;
  MidItem: PItem;
  Mid: Integer absolute Index;
begin
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidItem := @PIndex(P)^.Index[Mid].DataPage^.Data[0]
    else
      MidItem := @PData(P)^.Data[Mid];
    Cmp := Compare(Item, MidItem^);
    if Cmp > 0 then
      L := Mid + 1
    else if Cmp = 0 then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheObjectBTreeSet.GetFirst: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0];
end;

function TheObjectBTreeSet.GetLast: TItem;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1];
end;

function TheObjectBTreeSet.GetCurrent(var Iterator: TIterator): TItem;
begin
  Result := Iterator.Page^.Data[Iterator.Index];
end;

function TheObjectBTreeSet.GetMembership(const Item: TItem): Boolean;
var
  P: PData;
  Index: Integer;
begin
  Result := Seek(Item, P, Index);
end;

function TheObjectBTreeSet.GetRange(const RangeFrom, RangeTo: TItem): TEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheObjectBTreeSet.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheObjectBTreeSet.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheObjectBTreeSet.InsertItem(const P: PPage; const Index: Integer; const Item: TItem): PPage;
begin
  Result := Insert(P, Index);
  PData(P)^.Data[Index] := Item;
  Inc(FCount);
end;

function TheObjectBTreeSet.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index], Iterator.Sentinel) <= 0));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheObjectBTreeSet.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index], Iterator.Sentinel) >= 0));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheObjectBTreeSet.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

procedure TheObjectBTreeSet.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheObjectBTreeSet.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Item);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Item)
    end else
      InsertItem(Right, 0, Item);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Item);
end;

procedure TheObjectBTreeSet.SetMembership(const Item: TItem; const AValue: Boolean);
begin
  if AValue then
    Include(Item)
  else
    Exclude(Item);
end;

function TheObjectBTreeSet.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheObjectBTreeSet.Seek(const Item: TItem; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Item, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

procedure TheObjectBTreeSet.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Item: TItem);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Item)
  else
    InsertItem(P, Index, Item);
end;

procedure TheObjectBTreeSet.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheObjectBTreeSet.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheObjectBTreeSet.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

{ TheCmpBTreeMap }

procedure TheCmpBTreeMap.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheCmpBTreeMap.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheCmpBTreeMap.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else
    for I := 0 to P^.Count - 1 do
      Finalize(PData(P)^.Data[I]);
  FreeMem(P);
end;

procedure TheCmpBTreeMap.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheCmpBTreeMap.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheCmpBTreeMap.Create(const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

function TheCmpBTreeMap.Delete(const Key: TKey): Boolean;
var Dummy: TValue;
begin
  Result := Extract(Key, Dummy);
end;

destructor TheCmpBTreeMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheCmpBTreeMap.Compare(const A, B: TKey): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheCmpBTreeMap.ExtractData(const P: PPage; const Index: Integer): TValue;
begin
  Result := PData(P)^.Data[Index].Value;
  Finalize(PData(P)^.Data[Index]);
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheCmpBTreeMap.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheCmpBTreeMap.Extract(const Key: TKey; out Value: TValue): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            Value := ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          Value := ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheCmpBTreeMap.Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
type PKey = ^TKey;
var
  L, H, Cmp: Integer;
  MidKey: PKey;
  Mid: Integer absolute Index;
begin
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidKey := @PIndex(P)^.Index[Mid].DataPage^.Data[0].Key
    else
      MidKey := @PData(P)^.Data[Mid].Key;
    Cmp := Compare(Key, Midkey^);
    if Cmp > 0 then
      L := Mid + 1
    else if Cmp = 0 then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheCmpBTreeMap.GetCurrent(var Iterator: TIterator): TValue;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Value;
end;

function TheCmpBTreeMap.GetCurrentKey(var Iterator: TIterator): TKey;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Key;
end;

function TheCmpBTreeMap.GetFirst: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Value;
end;

function TheCmpBTreeMap.GetFirstKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Key;
end;

function TheCmpBTreeMap.GetLast: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Value;
end;

function TheCmpBTreeMap.GetLastKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Key;
end;

function TheCmpBTreeMap.GetMap(const Key: TKey): TValue;
begin
  if not Get(Key, Result) then
    Result := MissingKeyValue(Key);
end;

function TheCmpBTreeMap.GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheCmpBTreeMap.GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheCmpBTreeMap.Get(const Key: TKey; out Value: TValue): Boolean;
var
  P: PPage;
  Index: Integer;
begin
  Result := False;
  P := Root;
  if P <> nil then // tree is non empty
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Value := PIndex(P)^.Index[Index].DataPage^.Data[0].Value
        else
          Value := PData(P)^.Data[Index].Value
      else if P^.IsIndex then
        P := PIndex(P)^.Index[Index].Child
      else
        break; // give up
    until Result;
end;

function TheCmpBTreeMap.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheCmpBTreeMap.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheCmpBTreeMap.InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
begin
  Result := Insert(P, Index);
  Initialize(PData(P)^.Data[Index]);
  PData(P)^.Data[Index].Key := Key;
  PData(P)^.Data[Index].Value := Value;
  Inc(FCount);
end;

procedure TheCmpBTreeMap.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

function TheCmpBTreeMap.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index].Key, Iterator.Sentinel) <= 0));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheCmpBTreeMap.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index].Key, Iterator.Sentinel) >= 0));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheCmpBTreeMap.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheCmpBTreeMap.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Key, Value);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Key, Value)
    end else
      InsertItem(Right, 0, Key, Value);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Key, Value);
end;

procedure TheCmpBTreeMap.SetMap(const Key: TKey; const Value: TValue);
begin
  Put(Key, Value);
end;

function TheCmpBTreeMap.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheCmpBTreeMap.Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Key, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

function TheCmpBTreeMap.Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean): Boolean;
var Dummy: TValue;
begin
  Result := Put(Key, Value, Dummy, CanOverwrite);
end;

function TheCmpBTreeMap.Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Swap(PIndex(P)^.Index[Index].DataPage^.Data[0].Value, Value, Prev, CanOverwrite)
        else
          Swap(PData(P)^.Data[Index].Value, Value, Prev, CanOverwrite)
      else if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Key, Value)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Key, Value);
        break;
      end;
    until Result
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Key, Value);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheCmpBTreeMap.Keys: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
end;

function TheCmpBTreeMap.KeysReversed: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheCmpBTreeMap.Values: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheCmpBTreeMap.ValuesReversed: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

procedure TheCmpBTreeMap.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Key, Value)
  else
    InsertItem(P, Index, Key, Value);
end;

procedure TheCmpBTreeMap.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheCmpBTreeMap.Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
begin
  Prev := Dest;
  if CanOverwrite then
    Dest := Value;
end;

procedure TheCmpBTreeMap.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheCmpBTreeMap.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

function TheCmpBTreeMap.MissingKeyValue(const Key: TKey): TValue;
begin
  Assert(@Key = @Key); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

{ TheObjectBTreeMap }

procedure TheObjectBTreeMap.CheckSiblings(const Parent: PPage; const ParentIndex: Integer; out Left, Right: PPage);
begin
  Left := nil;
  Right := nil;
  if ParentIndex >= 0 then begin
    if ParentIndex > 0 then
      Left := PIndex(Parent)^.Index[ParentIndex - 1].Child;
    if ParentIndex < Parent^.Count then
      Right := PIndex(Parent)^.Index[ParentIndex + 1].Child;
  end;
end;

procedure TheObjectBTreeMap.Clear;
begin
  if Root <> nil then begin
    Clear(Root);
    FCount := 0;
    FFirst := nil;
    FLast := nil;
    FRoot  := nil;
  end;
end;

procedure TheObjectBTreeMap.Clear(const P: PPage);
var I: Integer;
begin
  if P^.IsIndex then
    for I := 0 to P^.Count do // Count=n Ch0 Med0 ... Chn-1 Medn-1 Chn
      Clear(PIndex(P)^.Index[I].Child)
  else begin
    for I := 0 to P^.Count - 1 do
      Finalize(PData(P)^.Data[I].Key);
    if OwnObjects then
      for I := 0 to P^.Count - 1 do
        PData(P)^.Data[I].Value.Free;
  end;
  FreeMem(P);
end;

procedure TheObjectBTreeMap.Concat(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  MoveLeft(P, Right, Right^.Count);
  if PData(Right)^.Next <> nil then
    PData(Right)^.Next^.Prev := PData(P)
  else
    FLast := P;
  PData(P)^.Next := PData(Right)^.Next;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    ExtractIndex(Parent, ParentIndex);
    PIndex(Parent)^.Index[ParentIndex].Child := P;
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

procedure TheObjectBTreeMap.ConcatIndex(const Parent, P, Right: PPage; const ParentIndex: Integer);
begin
  PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
  Move(PIndex(Right)^.Index[0], PIndex(P)^.Index[P^.Count + 1], (2 * Right^.Count + 1) * SizeOf(Pointer));
  P^.Count += Right^.Count + 1;
  FreeMem(Right);
  if Parent^.Count > 1 then begin
    Parent^.Count -= 1;
    if ParentIndex < Parent^.Count then
      Move(
        PIndex(Parent)^.Index[ParentIndex + 1].DataPage,
        PIndex(Parent)^.Index[ParentIndex].DataPage,
        2 * (Parent^.Count - ParentIndex) * SizeOf(Pointer)
      );
  end else begin
    FreeMem(Root);
    FRoot := P;
  end;
end;

constructor TheObjectBTreeMap.Create(const AOwnObjects: Boolean; const AKIndex: Integer; const AKData: Integer);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
  FKIndex := Max(AKIndex, 2);
  FKData := Max(AKData, 1);
end;

function TheObjectBTreeMap.Delete(const Key: TKey): Boolean;
var Dummy: TValue;
begin
  Result := Extract(Key, Dummy);
end;

destructor TheObjectBTreeMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TheObjectBTreeMap.Compare(const A, B: TKey): Integer;
begin
  Result := 0; // hint off
  Assert(@A = @A); // hint off
  Assert(@B = @B); // hint off
  raise EAbstractError.Create(Format('%s.Compare', [ClassName]));
end;

function TheObjectBTreeMap.ExtractData(const P: PPage; const Index: Integer): TValue;
begin
  Result := PData(P)^.Data[Index].Value;
  Finalize(PData(P)^.Data[Index].Key);
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PData(P)^.Data[Index + 1], PData(P)^.Data[Index], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  FCount -=1;
end;

procedure TheObjectBTreeMap.ExtractIndex(const P: PPage; const Index: Integer);
begin
  P^.Count -= 1;
  if Index < P^.Count then
    Move(PIndex(P)^.Index[Index + 1], PIndex(P)^.Index[Index], ((P^.Count - Index) * 2 + 1) * SizeOf(Pointer));
end;

function TheObjectBTreeMap.Extract(const Key: TKey; out Value: TValue): Boolean;
var
  Parent, P, DataPage: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then begin
          DataPage := PPage(PIndex(P)^.Index[Index].DataPage);
          if DataPage^.Count > KData then
            Value := ExtractData(DataPage, 0)
          else begin
            if (P^.Count < KIndex) and (P <> Root) then
              Underflow(Parent, P, ParentIndex, Index);
            ParentIndex := Index + 1;
            Parent := P;
            P := PIndex(P)^.Index[ParentIndex].Child;
            Result := False;
          end;
        end else begin
          Value := ExtractData(P, Index);
          if P^.Count < KData then
            if P <> Root then
              Underflow(Parent, P, ParentIndex)
            else if Count = 0 then
              Clear;
        end
      else if P^.IsIndex then begin
        if (P^.Count < KIndex) and (P <> Root) then
          Underflow(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child;
      end else
        break; // give up
    until Result
end;

function TheObjectBTreeMap.Find(const P: PPage; const Key: TKey; out Index: Integer): Boolean;
type PKey = ^TKey;
var
  L, H, Cmp: Integer;
  MidKey: PKey;
  Mid: Integer absolute Index;
begin
  L := 0;
  H := P^.Count - 1;
  while L <= H do begin
    Mid := (L + H) shr 1;
    if P^.IsIndex then
      MidKey := @PIndex(P)^.Index[Mid].DataPage^.Data[0].Key
    else
      MidKey := @PData(P)^.Data[Mid].Key;
    Cmp := Compare(Key, Midkey^);
    if Cmp > 0 then
      L := Mid + 1
    else if Cmp = 0 then
      Exit(True)
    else
      H := Mid - 1;
  end;
  Index  := L;
  Result := False;
end;

function TheObjectBTreeMap.GetCurrent(var Iterator: TIterator): TValue;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Value;
end;

function TheObjectBTreeMap.GetCurrentKey(var Iterator: TIterator): TKey;
begin
  Result := Iterator.Page^.Data[Iterator.Index].Key;
end;

function TheObjectBTreeMap.GetFirst: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Value;
end;

function TheObjectBTreeMap.GetFirstKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FFirst)^.Data[0].Key;
end;

function TheObjectBTreeMap.GetLast: TValue;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Value;
end;

function TheObjectBTreeMap.GetLastKey: TKey;
begin
  Assert(Count <> 0);
  Result := PData(FLast)^.Data[FLast^.Count - 1].Key;
end;

function TheObjectBTreeMap.GetMap(const Key: TKey): TValue;
begin
  if not Get(Key, Result) then
    Result := MissingKeyValue(Key);
end;

function TheObjectBTreeMap.GetRange(const RangeFrom, RangeTo: TKey): TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

function TheObjectBTreeMap.GetRangeKeys(const RangeFrom, RangeTo: TKey): TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.UseSentinel := True;
  Iterator.Sentinel := RangeTo;

  if Compare(RangeFrom, RangeTo) <= 0 then begin
    Seek(RangeFrom, Iterator.Page, Iterator.Index);
    Dec(Iterator.Index);
    Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
    Exit;
  end;

  // RangeFrom > RangeTo
  if Seek(RangeFrom, Iterator.Page, Iterator.Index) and (Iterator.Index < PPage(Iterator.Page)^.Count) then
    Inc(Iterator.Index);
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheObjectBTreeMap.Get(const Key: TKey; out Value: TValue): Boolean;
var
  P: PPage;
  Index: Integer;
begin
  Result := False;
  P := Root;
  if P <> nil then // tree is non empty
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Value := PIndex(P)^.Index[Index].DataPage^.Data[0].Value
        else
          Value := PData(P)^.Data[Index].Value
      else if P^.IsIndex then
        P := PIndex(P)^.Index[Index].Child
      else
        break; // give up
    until Result;
end;

function TheObjectBTreeMap.Insert(const P: PPage; const Index: Integer): PPage;
begin
  if Index < P^.Count then
    if P^.IsIndex then
      Move(PIndex(P)^.Index[Index].DataPage, PIndex(P)^.Index[Index + 1].DataPage, (P^.Count - Index) * 2 * SizeOf(Pointer))
    else
      Move(PData(P)^.Data[Index], PData(P)^.Data[Index + 1], (P^.Count - Index) * SizeOf(PData(P)^.Data[0]));
  Inc(P^.Count);
  Result := P;
end;

function TheObjectBTreeMap.Insert(const P: PPage; const Index: Integer; const DataPage, Child: PPage): PPage;
begin
  Result := Insert(P, Index);
  PIndex(Result)^.Index[Index].DataPage := PData(DataPage);
  PIndex(Result)^.Index[Index + 1].Child := Child;
end;

function TheObjectBTreeMap.InsertItem(const P: PPage; const Index: Integer; const Key: TKey; const Value: TValue): PPage;
begin
  Result := Insert(P, Index);
  Initialize(PData(P)^.Data[Index].Key);
  PData(P)^.Data[Index].Key := Key;
  PData(P)^.Data[Index].Value := Value;
  Inc(FCount);
end;

procedure TheObjectBTreeMap.MoveLeft(const Left, P: PPage; const N: Integer);
begin
  Move(PData(P)^.Data[0], PData(Left)^.Data[Left^.Count], N * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[N], PData(P)^.Data[0], (P^.Count - N) * SizeOf(PData(P)^.Data[0]));
  Left^.Count += N;
  P^.Count -= N;
end;

function TheObjectBTreeMap.MoveNext(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Inc(Iterator.Index);
    if Iterator.Index < PPage(Iterator.Page)^.Count then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index].Key, Iterator.Sentinel) <= 0));
    Iterator.Page := Iterator.Page^.Next;
    Iterator.Index := -1;
  end;
  Result := False;
end;

function TheObjectBTreeMap.MovePrev(var Iterator: TIterator): Boolean;
begin
  while Iterator.Page <> nil do begin
    Dec(Iterator.Index);
    if Iterator.Index >= 0 then
      Exit(not Iterator.UseSentinel or (Compare(Iterator.Page^.Data[Iterator.Index].Key, Iterator.Sentinel) >= 0));
    Iterator.Page := Iterator.Page^.Prev;
    if Iterator.Page <> nil then
      Iterator.Index := PPage(Iterator.Page)^.Count;
  end;
  Result := False;
end;

procedure TheObjectBTreeMap.MoveRight(const P, Right: PPage; const N: Integer);
begin
  Move(PData(Right)^.Data[0], PData(Right)^.Data[N], Right^.Count * SizeOf(PData(P)^.Data[0]));
  Move(PData(P)^.Data[P^.Count - N], PData(Right)^.Data[0], N * SizeOf(PData(P)^.Data[0]));
  Right^.Count += N;
  P^.Count -= N;
end;

procedure TheObjectBTreeMap.Overflow(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count < 2 * KData) then begin
    MoveLeft(Left, P);
    InsertItem(P, Index - 1, Key, Value);
    Exit;
  end;

  if (Right <> nil) and (Right^.Count < 2 * KData) then begin
    if Index < 2 * KData then begin
      MoveRight(P, Right);
      InsertItem(P, Index, Key, Value)
    end else
      InsertItem(Right, 0, Key, Value);
    Exit;
  end;

  SplitData(Parent, P, ParentIndex, Index, Key, Value);
end;

procedure TheObjectBTreeMap.SetMap(const Key: TKey; const Value: TValue);
begin
  Put(Key, Value);
end;

function TheObjectBTreeMap.Page(const IsIndex: Boolean; const LeftmostChild: PPage): PPage;
begin
  if IsIndex then begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + (4 * KIndex + 3) * SizeOf(Pointer));
    PIndex(Result)^.Index[0].Child := LeftmostChild;
  end else begin
    GetMem(Result, SizeOf(TPage.Count) + SizeOf(TPage.IsIndex) + 2 * SizeOf(Pointer) +  2 * KData * SizeOf(TDataPage.Data[0]));
    PData(Result)^.Prev := nil;
    PData(Result)^.Next := nil;
  end;
  Result^.IsIndex := IsIndex;
  Result^.Count := 0;
end;

function TheObjectBTreeMap.Seek(const Key: TKey; out P: PData; out Index: Integer): Boolean;
begin
  Result := False;
  P := PData(Root);
  if P <> nil then begin // tree is non empty
    repeat
      Result := Find(PPage(P), Key, Index);
      if Result then begin // Key found
        if PPage(P)^.IsIndex then begin
          P := PIndex(P)^.Index[Index].DataPage;
          Index := 0;
        end;
      end else if PPage(P)^.IsIndex then
        P := PData(PIndex(P)^.Index[Index].Child)
      else
        break;
    until Result;
  end;
end;

function TheObjectBTreeMap.Put(const Key: TKey; const Value: TValue; const CanOverwrite: Boolean): Boolean;
var Dummy: TValue;
begin
  Result := Put(Key, Value, Dummy, CanOverwrite);
end;

function TheObjectBTreeMap.Put(const Key: TKey; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean): Boolean;
var
  P, Parent: PPage;
  Index, ParentIndex: Integer;
begin
  Result := False;
  ParentIndex := -1;
  Parent := nil;
  P := Root;
  if P <> nil then
    repeat
      Result := Find(P, Key, Index);
      if Result then // Key found
        if P^.IsIndex then
          Swap(PIndex(P)^.Index[Index].DataPage^.Data[0].Value, Value, Prev, CanOverwrite)
        else
          Swap(PData(P)^.Data[Index].Value, Value, Prev, CanOverwrite)
      else if P^.IsIndex then begin
        if P^.Count > 2 * KIndex then
          SplitIndex(Parent, P, ParentIndex, Index);
        ParentIndex := Index;
        Parent := P;
        P := PIndex(P)^.Index[Index].Child
      end else begin
        if P^.Count < 2 * KData then // page is not full
          InsertItem(P, Index, Key, Value)
        else // page is full
          Overflow(Parent, P, ParentIndex, Index, Key, Value);
        break;
      end;
    until Result
  else begin // tree is empty
    FRoot := InsertItem(Page(False), 0, Key, Value);
    FFirst := Root;
    FLast := Root;
  end;
end;

function TheObjectBTreeMap.Keys: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrentKey);
end;

function TheObjectBTreeMap.KeysReversed: TKeyEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrentKey);
end;

function TheObjectBTreeMap.Values: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FFirst);
  Iterator.Index := -1;
  Iterator.UseSentinel := False;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MoveNext, @GetCurrent);
end;

function TheObjectBTreeMap.ValuesReversed: TValueEnumeratorProvider;
var Iterator: TIterator;
begin
  Iterator.Page := PData(FLast);
  Iterator.UseSentinel := False;
  if FLast <> nil then
    Iterator.Index := FLast^.Count;
  Assert(Iterator.Page = Iterator.Page); // hint off
  Result.FEnumerator.Init(Iterator, @MovePrev, @GetCurrent);
end;

procedure TheObjectBTreeMap.SplitData(const Parent, P: PPage; const ParentIndex, Index: Integer; const Key: TKey; const Value: TValue);
var Right: PPage;
begin
  Right := Page(False);
  if PData(P)^.Next <> nil then begin // P was not last
    PData(Right)^.Next := PData(P)^.Next;
    PData(Right)^.Next^.Prev := PData(Right);
  end else  // P was last
    FLast := Right;
  PData(P)^.Next := PData(Right);
  PData(Right)^.Prev := PData(P);
  Move(PData(P)^.Data[KData], PData(Right)^.Data[0], KData * SizeOf(PData(P)^.Data[0]));
  P^.Count := KData;
  Right^.Count := KData;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, Right, Right)
  else
    FRoot := Insert(Page(True, P), 0, Right, Right);
  if Index > KData then
    InsertItem(Right, Index - KData, Key, Value)
  else
    InsertItem(P, Index, Key, Value);
end;

procedure TheObjectBTreeMap.SplitIndex(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Right: PPage;
begin
  Right := Page(True);
  Move(PIndex(P)^.Index[KIndex + 1], PIndex(Right)^.Index[0], (2 * KIndex + 1) * SizeOf(Pointer));
  P^.Count := KIndex;
  Right^.Count := KIndex;
  if ParentIndex >= 0 then
    Insert(Parent, ParentIndex, PPage(PIndex(P)^.Index[KIndex].DataPage), Right)
  else
    FRoot := Insert(Page(True, P), 0, PPage(PIndex(P)^.Index[KIndex].DataPage), Right);
  if Index > KIndex then begin
    P := Right;
    Index -= KIndex + 1;
  end;
end;

procedure TheObjectBTreeMap.Swap(var Dest: TValue; const Value: TValue; out Prev: TValue; const CanOverwrite: Boolean);
begin
  Prev := Dest;
  if CanOverwrite then
    Dest := Value;
end;

procedure TheObjectBTreeMap.Underflow(const Parent, P: PPage; const ParentIndex: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);
  if (Left <> nil) and (Left^.Count + P^.Count >= 2 * KData) then
    MoveRight(Left, P)
  else if (Right <> nil) and (P^.Count + Right^.Count >= 2 * KData) then
    MoveLeft(P, Right)
  else if Left <> nil then
    Concat(Parent, Left, P, ParentIndex - 1)
  else
    Concat(Parent, P, Right, ParentIndex);
end;

procedure TheObjectBTreeMap.Underflow(const Parent: PPage; var P: PPage; const ParentIndex: Integer; var Index: Integer);
var Left, Right: PPage;
begin
  CheckSiblings(Parent, ParentIndex, Left, Right);

  if (Left <> nil) and (Left^.Count > KIndex) then begin
    Move(PIndex(P)^.Index[0], PIndex(P)^.Index[1], (2 * P^.Count + 1) * SizeOf(Pointer));
    PIndex(P)^.Index[0].Child := PIndex(Left)^.Index[Left^.Count].Child;
    PIndex(P)^.Index[0].DataPage := PIndex(Parent)^.Index[ParentIndex - 1].DataPage;
    P^.Count += 1;
    Index += 1;
    Left^.Count -= 1;
    PIndex(Parent)^.Index[ParentIndex - 1].DataPage := PIndex(Left)^.Index[Left^.Count].DataPage;
    Exit;
  end;

  if (Right <> nil) and (Right^.Count > KIndex) then begin
    PIndex(P)^.Index[P^.Count].DataPage := PIndex(Parent)^.Index[ParentIndex].DataPage;
    P^.Count += 1;
    PIndex(P)^.Index[P^.Count].Child := PIndex(Right)^.Index[0].Child;
    PIndex(Parent)^.Index[ParentIndex].DataPage := PIndex(Right)^.Index[0].DataPage;
    Move(PIndex(Right)^.Index[1], PIndex(Right)^.Index[0], (2 * Right^.Count + 1) * SizeOf(Pointer));
    Right^.Count -= 1;
    Exit;
  end;

  if Left <> nil then begin
    Index += Left^.Count + 1;
    ConcatIndex(Parent, Left, P, ParentIndex - 1);
    P := Left;
    Exit;
  end;

  ConcatIndex(Parent, P, Right, ParentIndex);
end;

function TheObjectBTreeMap.MissingKeyValue(const Key: TKey): TValue;
begin
  Assert(@Key = @Key); // hint off
  Initialize(Result); // hint off
  raise EMapKeyNotFound.Create(ClassName);
end;

end.

