{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

(*
  Copyright (c) 2011-2012, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit ts.Collections;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, TypInfo,

  ts.Core.Generics.Defaults, ts.Core.Generics.Collections,

  ts.Core.Value;

type
  TCollectionChangedAction = (
    caAdd,
    caRemove,
    caReplace,
    caMove,
    caExtract //internal use only
  );
  TCollectionChangedEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionChangedAction) of object;

  //TArray = record
  //public
  //  class function Copy<T>(Values: array of T): TArray<T>; static;
  //end;

  IList = interface;
  IList<T> = interface;

  //{$M+}
  IEnumerator = interface
    function GetCurrent: TValue;
    function MoveNext: Boolean;
    property Current: TValue read GetCurrent;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable = interface
    function AsObject: TObject;
    function GetCount: NativeInt;
    function GetItemType: PTypeInfo;
    function Contains(const Value: TValue): Boolean;
    function GetEnumerator: IEnumerator;
    function ToArray: TArray<TValue>;
    function ToList: IList;
    property Count: NativeInt read GetCount;
    property ItemType: PTypeInfo read GetItemType;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function Contains(const Value: T): Boolean;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
    function ToList: IList<T>;
  end;

  IList = interface(IEnumerable)
    ['{3FB348F0-95FE-4608-9E7E-98ED9B2A7940}']
    function GetCapacity: NativeInt;
    function GetCount: NativeInt;
    procedure SetCapacity(const Value: NativeInt);

    function Add(const Value: TValue): NativeInt;
    procedure AddRange(const Values: array of TValue); overload;
    procedure AddRange(Values: IEnumerable); overload;
    procedure Clear;
    procedure Delete(const Index: NativeInt);
    procedure DeleteRange(const Index, Count: NativeInt);
    function Extract(const Value: TValue): TValue;
    function First: TValue;
    function GetItem(const Index: NativeInt): TValue;
    //function GetOnCollectionChanged: IEvent;
    function IndexOf(const Value: TValue): NativeInt;
    procedure Insert(const Index: NativeInt; const Value: TValue);
    procedure InsertRange(const Index: NativeInt; const Values: array of TValue); overload;
    procedure InsertRange(const Index: NativeInt; Values: IEnumerable); overload;
    function Last: TValue;
    function LastIndexOf(const Value: TValue): NativeInt;
    procedure Move(const OldIndex, NewIndex: NativeInt);
    function Remove(const Value: TValue): NativeInt;
    procedure RemoveRange(const Values: array of TValue); overload;
    procedure RemoveRange(Values: IEnumerable); overload;
    procedure SetItem(const Index: NativeInt; const Value: TValue);
    procedure Sort; overload;
    procedure Sort(Comparer: IComparer<TValue>); overload;
    //procedure Sort(Comparison: TComparison<TValue>); overload;

    property Capacity: NativeInt read GetCapacity write SetCapacity;
    property Count: NativeInt read GetCount;
    property Items[const Index: NativeInt]: TValue read GetItem write SetItem; default;
    //property OnCollectionChanged: IEvent read GetOnCollectionChanged;
  end;

  IList<T> = interface(IEnumerable<T>)
    function GetCapacity: NativeInt;
    function GetCount: NativeInt;
    procedure SetCapacity(const Value: NativeInt);

    function Add(const Value: T): NativeInt;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(Values: IEnumerable<T>); overload;
    procedure Clear;
    procedure Delete(const Index: NativeInt);
    procedure DeleteRange(const Index, Count: NativeInt);
    function Extract(const Value: T): T;
    function First: T;
    function GetItem(const Index: NativeInt): T;
    //function GetOnCollectionChanged: IEvent<TCollectionChangedEvent<T>>;
    function IndexOf(const Value: T): NativeInt;
    procedure Insert(const Index: NativeInt; const Value: T);
    procedure InsertRange(const Index: NativeInt; const Values: array of T); overload;
    procedure InsertRange(const Index: NativeInt; Values: IEnumerable<T>); overload;
    function Last: T;
    function LastIndexOf(const Value: T): NativeInt;
    procedure Move(const OldIndex, NewIndex: NativeInt);
    function Remove(const Value: T): NativeInt;
    procedure RemoveRange(const Values: array of T); overload;
    procedure RemoveRange(Values: IEnumerable<T>); overload;
    procedure SetItem(const Index: NativeInt; const Value: T);
    procedure Sort(Comparer: IComparer<T>); overload;
    //procedure Sort(Comparison: TComparison<T>); overload;

    function AsList: IList;

    property Capacity: NativeInt read GetCapacity write SetCapacity;
    property Count: NativeInt read GetCount;
    property Items[const Index: NativeInt]: T read GetItem write SetItem; default;
    //property OnCollectionChanged: IEvent<TCollectionChangedEvent<T>>
    //  read GetOnCollectionChanged;
  end;

  IStack<T> = interface(IEnumerable<T>)
    function GetCapacity: NativeInt;
    function GetCount: NativeInt;
    procedure SetCapacity(const Value: NativeInt);

    procedure Clear;
    function Peek: T;
    procedure Push(const Value: T);
    function Pop: T;

    function ToArray: TArray<T>;

    property Capacity: NativeInt read GetCapacity write SetCapacity;
    property Count: NativeInt read GetCount;
  end;

  TEnumerator = class(TInterfacedObject, IEnumerator)
  protected
    function GetCurrentBase: TValue; virtual;
    function IEnumerator.GetCurrent = GetCurrentBase;
  public
    function MoveNext: Boolean; virtual;
    property Current: TValue read GetCurrentBase;
  end;

  TEnumerator<T> = class(TEnumerator, IEnumerator<T>)
  protected
    function GetCurrentBase: TValue; override;
    function GetCurrent: T; virtual;
  public
    property Current: T read GetCurrent;
  end;

  TEnumerable = class(TInterfacedObject, IEnumerable)
  protected
    function GetCount: NativeInt; virtual;
    function GetEnumeratorBase: IEnumerator; virtual;
    function IEnumerable.GetEnumerator = GetEnumeratorBase;
    function GetItemType: PTypeInfo; virtual;
    function ToArrayBase: TArray<TValue>;
    function ToListBase: IList; virtual; abstract;
    function IEnumerable.ToArray = ToArrayBase;
    function IEnumerable.ToList = ToListBase;
  public
    function AsObject: TObject;
    function Contains(const Value: TValue): Boolean; virtual;
    property Count: NativeInt read GetCount;
    property ItemType: PTypeInfo read GetItemType;
  end;

  TEnumerable<T> = class(TEnumerable, IEnumerable<T>, IEnumerable)
  protected
    function GetEnumeratorBase: IEnumerator; override;
    function GetItemType: PTypeInfo; override;
    function ToListBase: IList; override;
  public
    function Contains(const Value: TValue): Boolean; overload; override;
    function Contains(const Value: T): Boolean; reintroduce; overload; virtual;
    function GetEnumerator: IEnumerator<T>; reintroduce; virtual;
    function ToArray: TArray<T>; virtual;
    function ToList: IList<T>; virtual;
  end;

  TListBase<T> = class(TEnumerable<T>, IList<T>, IList)
  private
    FComparer: IComparer<T>;
    //FOnCollectionChanged: Event<TCollectionChangedEvent<T>>;
    function FirstBase: TValue;
    function GetItemBase(const Index: NativeInt): TValue;
    //function GetOnCollectionChanged: IEvent<TCollectionChangedEvent<T>>;
    //function GetOnCollectionChangedBase: IEvent;
    function LastBase: TValue;
    function IList.First = FirstBase;
    function IList.GetItem = GetItemBase;
    function IList.GetOnCollectionChanged = GetOnCollectionChangedBase;
    function IList.Last = LastBase;
  protected
    procedure Notify(const Value: T; const Action: TCollectionChangedAction); virtual;
    function GetCapacity: NativeInt; virtual;
    function GetItem(const Index: NativeInt): T; virtual;
    procedure SetCapacity(const Value: NativeInt); virtual;
    procedure SetItem(const Index: NativeInt; const Value: T); overload; virtual;
    procedure SetItem(const Index: NativeInt; const Value: TValue); overload;
    property Comparer: IComparer<T> read FComparer;
  public
    constructor Create; overload;
    constructor Create(Comparer: IComparer<T>); overload;
    constructor Create(Values: IEnumerable<T>); overload;
    constructor Create(const Values: array of T); overload;

    destructor Destroy; override;

    function Add(const Value: T): NativeInt; overload; virtual;
    function Add(const Value: TValue): NativeInt; overload;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Values: array of TValue); overload;
    procedure AddRange(Values: IEnumerable); overload;
    procedure AddRange(Values: IEnumerable<T>); overload;
    procedure Clear; virtual;
    function Contains(const Value: T): Boolean; override;
    procedure Delete(const Index: NativeInt); virtual;
    procedure DeleteRange(const Index, Count: NativeInt);
    function Extract(const Value: T): T; overload; virtual;
    function Extract(const Value: TValue): TValue; overload;
    function First: T; virtual;
    function IndexOf(const Value: T): NativeInt; overload; virtual;
    function IndexOf(const Value: TValue): NativeInt; overload;
    procedure Insert(const Index: NativeInt; const Value: T); overload; virtual;
    procedure Insert(const Index: NativeInt; const Value: TValue); overload;
    procedure InsertRange(const Index: NativeInt; const Values: array of T); overload;
    procedure InsertRange(const Index: NativeInt; const Values: array of TValue); overload;
    procedure InsertRange(const Index: NativeInt; Values: IEnumerable); overload;
    procedure InsertRange(const Index: NativeInt; Values: IEnumerable<T>); overload;
    function Last: T;
    function LastIndexOf(const Value: T): NativeInt; overload;
    function LastIndexOf(const Value: TValue): NativeInt; overload;
    procedure Move(const OldIndex, NewIndex: NativeInt); virtual;
    function Remove(const Value: T): NativeInt; overload; virtual;
    function Remove(const Value: TValue): NativeInt; overload;
    procedure RemoveRange(const Values: array of T); overload;
    procedure RemoveRange(const Values: array of TValue); overload;
    procedure RemoveRange(Values: IEnumerable); overload;
    procedure RemoveRange(Values: IEnumerable<T>); overload;
    procedure Sort; overload;
    procedure Sort(Comparer: IComparer<T>); overload; virtual;
    //procedure Sort(Comparer: IComparer<TValue>); overload; virtual;
    //procedure Sort(Comparison: TComparison<T>); overload;
    //procedure Sort(Comparison: TComparison<TValue>); overload;

    function AsList: IList;
    function GetEnumerator: IEnumerator<T>; override;
    function ToArray: TArray<T>; override;

    //type
    //  TEnumerator = class(TEnumerator<T>)
    //  private
    //    FList: TListBase<T>;
    //    FIndex: Integer;
    //  protected
    //    function GetCurrent: T; override;
    //  public
    //    constructor Create(AList: TListBase<T>);
    //    function MoveNext: Boolean; override;
    //    property Current: T read GetCurrent;
    //  end;

    property Items[const Index: NativeInt]: T read GetItem write SetItem; default;
    //property OnCollectionChanged: IEvent<TCollectionChangedEvent<T>>
    //  read GetOnCollectionChanged;
  end;

  TList<T> = class(TListBase<T>)
  private
    FItems: TArray<T>;
    FCount: NativeInt;
    procedure Grow;
    procedure InternalDelete(const Index: NativeInt;
      const Action: TCollectionChangedAction);
  protected
    function GetCapacity: NativeInt; override;
    function GetCount: NativeInt; override;
    function GetItem(const Index: NativeInt): T; override;
    procedure SetCapacity(const Value: NativeInt); override;
    procedure SetItem(const Index: NativeInt; const Value: T); override;
  public
    procedure Clear; override;
    procedure Delete(const Index: NativeInt); override;
    function Extract(const Value: T): T; override;
    procedure Insert(const Index: NativeInt; const Value: T); override;
    procedure Move(const OldIndex, NewIndex: NativeInt); override;
    procedure Sort(Comparer: IComparer<T>); override;
  end;

  TObjectList<T: class> = class(TList<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const Value: T; const Action: TCollectionChangedAction); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(Comparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(Values: IEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(const Values: array of T; AOwnsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TArray }

//class function TArray.Copy<T>(Values: array of T): TArray<T>;
//var
//  i: Integer;
//begin
//  SetLength(Result, Length(Values));
//  for i := Low(Values) to High(Values) do
//  begin
//    Result[i] := Values[i];
//  end;
//end;

{ TEnumerator }

function TEnumerator.GetCurrentBase: TValue;
begin
  //Result := TValue.Empty;
  Result := TValue.Null;
end;

function TEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

{ TEnumerator<T> }

function TEnumerator<T>.GetCurrent: T;
begin
  Result := Default(T);
end;

function TEnumerator<T>.GetCurrentBase: TValue;
begin
  //Result := TValue.From<T>(GetCurrent());
end;

{ TEnumerable }

function TEnumerable.AsObject: TObject;
begin
  Result := Self;
end;

function TEnumerable.Contains(const Value: TValue): Boolean;
begin
  Result := False;
end;

function TEnumerable.GetCount: NativeInt;
var
  LEnumerator: IEnumerator;
begin
  Result := 0;
  LEnumerator := GetEnumeratorBase();
  while LEnumerator.MoveNext do
  begin
    Inc(Result);
  end;
end;

function TEnumerable.GetEnumeratorBase: IEnumerator;
begin
  Result := TEnumerator.Create();
end;

function TEnumerable.GetItemType: PTypeInfo;
begin
  Result := nil;
end;

function TEnumerable.ToArrayBase: TArray<TValue>;
//{$IF CompilerVersion = 21}
//type
//  PValue = ^TValue;
//{$IFEND}
var
  i: Integer;
  LEnumerator: IEnumerator;
begin
  i := 0;
  SetLength(Result, 2);
  LEnumerator := GetEnumeratorBase();
  while LEnumerator.MoveNext do
  begin
    if Length(Result) <= i then
    begin
      SetLength(Result, Length(Result) * 2);
    end;
//{$IF CompilerVersion = 21}
//    PValue(@Result[i])^ := LEnumerator.Current;
//{$ELSE}
    Result[i] := LEnumerator.Current;
//{$IFEND}
    Inc(i);
  end;
  SetLength(Result, i);
end;

{ TEnumerable<T> }

function TEnumerable<T>.Contains(const Value: TValue): Boolean;
begin
  //Result := Contains(Value.AsType<T>);
end;

function TEnumerable<T>.Contains(const Value: T): Boolean;
begin
  Result := False;
end;

function TEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator<T>.Create();
end;

function TEnumerable<T>.GetEnumeratorBase: IEnumerator;
begin
  Result := GetEnumerator();
end;

function TEnumerable<T>.GetItemType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerable<T>.ToArray: TArray<T>;
var
  i: Integer;
  LEnumerator: IEnumerator<T>;
begin
  i := 0;
  SetLength(Result, 2);
  LEnumerator := GetEnumerator();
  while LEnumerator.MoveNext do
  begin
    if Length(Result) <= i then
    begin
      SetLength(Result, Length(Result) * 2);
    end;
    Result[i] := LEnumerator.Current;
    Inc(i);
  end;
  SetLength(Result, i);
end;

function TEnumerable<T>.ToList: IList<T>;
begin
  Result := TList<T>.Create(Self);
end;

function TEnumerable<T>.ToListBase: IList;
begin
  Result := ToList().AsList;
end;

{ TListBase<T> }

constructor TListBase<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TListBase<T>.Create(Comparer: IComparer<T>);
begin
  inherited Create();
  FComparer := Comparer;
  if not Assigned(FComparer) then
  begin
    FComparer := TComparer<T>.Default;
  end;
end;

constructor TListBase<T>.Create(Values: IEnumerable<T>);
begin
  Create();
  AddRange(Values);
end;

constructor TListBase<T>.Create(const Values: array of T);
begin
  Create();
  AddRange(Values);
end;

destructor TListBase<T>.Destroy;
begin
  Clear();
  inherited;
end;

function TListBase<T>.Add(const Value: T): NativeInt;
begin
  Result := Count;
  //Insert(Count, Value);
end;

function TListBase<T>.Add(const Value: TValue): NativeInt;
begin
  //Result := Add(Value.AsType<T>);
end;

procedure TListBase<T>.AddRange(const Values: array of T);
begin
  InsertRange(Count, Values);
end;

procedure TListBase<T>.AddRange(const Values: array of TValue);
begin
  InsertRange(Count, Values);
end;

procedure TListBase<T>.AddRange(Values: IEnumerable);
begin
  InsertRange(Count, Values);
end;

procedure TListBase<T>.AddRange(Values: IEnumerable<T>);
begin
  InsertRange(Count, Values);
end;

function TListBase<T>.AsList: IList;
begin
  Result := Self;
end;

procedure TListBase<T>.Clear;
begin
  // implemented in descendants
end;

function TListBase<T>.Contains(const Value: T): Boolean;
begin
  //Result := IndexOf(Value) >= 0;
end;

procedure TListBase<T>.Delete(const Index: NativeInt);
begin
  // implemented in descendants
end;

procedure TListBase<T>.DeleteRange(const Index, Count: NativeInt);
var
  i: NativeInt;
begin
  if (Index < 0) or (Count < 0) or (Index + Count < 0)
    or (Index + Count > Self.Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  for i := 0 to Count - 1 do
    Delete(Index);
end;

function TListBase<T>.Extract(const Value: T): T;
begin
  // implemented in descendants
end;

function TListBase<T>.Extract(const Value: TValue): TValue;
begin
  //Result := TValue.From<T>(Extract(Value.AsType<T>));
end;

function TListBase<T>.First: T;
begin
  Result := Items[0];
end;

function TListBase<T>.FirstBase: TValue;
begin
  //Result := TValue.From<T>(First);
end;

function TListBase<T>.GetItem(const Index: NativeInt): T;
begin
  raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TListBase<T>.GetCapacity: NativeInt;
begin
  // implemented in descendants
end;

function TListBase<T>.GetEnumerator: IEnumerator<T>;
begin
  //Result := TEnumerator.Create(Self);
end;

function TListBase<T>.GetItemBase(const Index: NativeInt): TValue;
begin
  //Result := TValue.From<T>(GetItem(Index));
end;

//function TListBase<T>.GetOnCollectionChanged: IEvent<TCollectionChangedEvent<T>>;
//begin
//  Result := FOnCollectionChanged;
//end;
//
//function TListBase<T>.GetOnCollectionChangedBase: IEvent;
//begin
//  Result := OnCollectionChanged;
//end;

function TListBase<T>.IndexOf(const Value: T): NativeInt;
var
  i: NativeInt;
begin
  //for i := 0 to Count - 1 do
  //  if FComparer.Compare(Items[i], Value) = 0 then
  //    Exit(i);
  Result := -1;
end;

function TListBase<T>.IndexOf(const Value: TValue): NativeInt;
var
  LItem: T;
begin
  //if Value.TryAsType<T>(LItem) then
  //begin
  //  Result := IndexOf(LItem);
  //end
  //else
  begin
    Result := -1;
  end;
end;

procedure TListBase<T>.Insert(const Index: NativeInt; const Value: T);
begin
  // implemented in descendants
end;

procedure TListBase<T>.Insert(const Index: NativeInt; const Value: TValue);
begin
  //Insert(Index, Value.AsType<T>);
end;

procedure TListBase<T>.InsertRange(const Index: NativeInt;
  const Values: array of T);
var
  i: NativeInt;
  LItem: T;
begin
  i := Index;
  for LItem in Values do
  begin
    //Insert(i, LItem);
    Inc(i);
  end;
end;

procedure TListBase<T>.InsertRange(const Index: NativeInt;
  const Values: array of TValue);
var
  i: NativeInt;
  LItem: TValue;
begin
  i := Index;
  for LItem in Values do
  begin
    //Insert(i, LItem);
    Inc(i);
  end;
end;

procedure TListBase<T>.InsertRange(const Index: NativeInt; Values: IEnumerable);
var
  i: NativeInt;
  LItem: TValue;
begin
  i := Index;
  for LItem in Values do
  begin
    //Insert(i, LItem);
    Inc(i);
  end;
end;

procedure TListBase<T>.InsertRange(const Index: NativeInt; Values: IEnumerable<T>);
var
  i: NativeInt;
  LItem: T;
begin
  i := Index;
  for LItem in Values do
  begin
    //Insert(i, LItem);
    Inc(i);
  end;
end;

function TListBase<T>.Last: T;
begin
  Result := Items[Count - 1];
end;

function TListBase<T>.LastBase: TValue;
begin
  //Result := TValue.From<T>(Last);
end;

function TListBase<T>.LastIndexOf(const Value: T): NativeInt;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    //if FComparer.Compare(Items[i], Value) = 0 then
//      Exit(i);
  Result := -1;
end;

function TListBase<T>.LastIndexOf(const Value: TValue): NativeInt;
begin
  //Result := LastIndexOf(Value.AsType<T>);
end;

procedure TListBase<T>.Move(const OldIndex, NewIndex: NativeInt);
begin
  // implemented in descendants
end;

procedure TListBase<T>.Notify(const Value: T; const Action: TCollectionChangedAction);
var
  LAction: TCollectionChangedAction;
begin
  case Action of
    caExtract: LAction := caRemove;
  else
    LAction := Action;
  end;
  //FOnCollectionChanged.Invoke(Self, Value, LAction);
end;

function TListBase<T>.Remove(const Value: T): NativeInt;
begin
  //Result := IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

function TListBase<T>.Remove(const Value: TValue): NativeInt;
begin
  //Result := Remove(Value.AsType<T>);
end;

procedure TListBase<T>.RemoveRange(const Values: array of T);
var
  LItem: T;
begin
  for LItem in Values do
  begin
    //Remove(LItem);
  end;
end;

procedure TListBase<T>.RemoveRange(const Values: array of TValue);
var
  LItem: TValue;
begin
  for LItem in Values do
  begin
    //Remove(LItem);
  end;
end;

procedure TListBase<T>.RemoveRange(Values: IEnumerable);
var
  LItem: TValue;
begin
  for LItem in Values do
  begin
    //Remove(LItem);
  end;
end;

procedure TListBase<T>.RemoveRange(Values: IEnumerable<T>);
var
  LItem: T;
begin
  for LItem in Values do
  begin
    //Remove(LItem);
  end;
end;

procedure TListBase<T>.SetCapacity(const Value: NativeInt);
begin
  // implemented in descendants
end;

procedure TListBase<T>.SetItem(const Index: NativeInt; const Value: T);
begin
  // implemented in descendants
end;

procedure TListBase<T>.SetItem(const Index: NativeInt; const Value: TValue);
begin
  //SetItem(Index, Value.AsType<T>);
end;

procedure TListBase<T>.Sort;
begin
  Sort(FComparer);
end;

procedure TListBase<T>.Sort(Comparer: IComparer<T>);
begin
  // implemented in descendants
end;

//procedure TListBase<T>.Sort(Comparer: IComparer<TValue>);
//begin
//  Sort(TComparer<T>.Construct(
//    function(const Left, Right: T): Integer
//    begin
//      Result := Comparer.Compare(TValue.From<T>(Left), TValue.From<T>(Right));
//    end));
//end;
//
//procedure TListBase<T>.Sort(Comparison: TComparison<T>);
//begin
//  Sort(TComparer<T>.Construct(Comparison));
//end;
//
//procedure TListBase<T>.Sort(Comparison: TComparison<TValue>);
//begin
//  Sort(TComparer<TValue>.Construct(Comparison));
//end;

function TListBase<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := Items[i];
  end;
end;

{ TListBase<T>.TEnumerator }

//constructor TListBase<T>.TEnumerator.Create(AList: TListBase<T>);
//begin
//  inherited Create();
//  FList := AList;
//  FIndex := -1;
//end;
//
//function TListBase<T>.TEnumerator.GetCurrent: T;
//begin
//  Result := FList[FIndex];
//end;
//
//function TListBase<T>.TEnumerator.MoveNext: Boolean;
//begin
//  if FIndex < FList.Count then
//  begin
//    Inc(FIndex);
//    Result := FIndex < FList.Count;
//  end
//  else
//    Result := False;
//end;

{ TList<T> }

procedure TList<T>.Clear;
var
  i: NativeInt;
begin
  while FCount > 0 do
    Delete(FCount - 1);
  SetLength(FItems, 0);
end;

procedure TList<T>.Delete(const Index: NativeInt);
begin
  InternalDelete(Index, caRemove);
end;

function TList<T>.Extract(const Value: T): T;
var
  i: NativeInt;
begin
  //i := IndexOf(Value);
  if i = -1 then
  begin
    Result := Default(T);
  end
  else
  begin
    Result := FItems[i];
    InternalDelete(i, caExtract);
  end;
end;

function TList<T>.GetCapacity: NativeInt;
begin
  Result := Length(FItems);
end;

function TList<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TList<T>.GetItem(const Index: NativeInt): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

procedure TList<T>.Grow;
begin
  if FCount = 0 then
    SetLength(FItems, 1)
  else
    SetLength(FItems, FCount * 2);
end;

procedure TList<T>.Insert(const Index: NativeInt; const Value: T);
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if FCount = Length(FItems) then
    Grow();

  if Index < FCount then
  begin
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(T));
    FillChar(FItems[Index], SizeOf(FItems[Index]), 0);
  end;

  FItems[Index] := Value;
  Inc(FCount);
  Notify(Value, caAdd);
end;

procedure TList<T>.InternalDelete(const Index: NativeInt;
  const Action: TCollectionChangedAction);
var
  LItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[Index];
  FItems[Index] := Default(T);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(T));
    FillChar(FItems[FCount], SizeOf(T), 0);
  end;

  Notify(LItem, Action);
end;

procedure TList<T>.Move(const OldIndex, NewIndex: NativeInt);
var
  LItem: T;
begin
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[OldIndex];
  FItems[OldIndex] := Default(T);
  if OldIndex < NewIndex then
    System.Move(FItems[OldIndex + 1], FItems[OldIndex], (NewIndex - OldIndex) * SizeOf(T))
  else
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (OldIndex - NewIndex) * SizeOf(T));

  FillChar(FItems[NewIndex], SizeOf(T), 0);
  FItems[NewIndex] := LItem;

  Notify(LItem, caMove);
end;

procedure TList<T>.SetCapacity(const Value: NativeInt);
begin
  if Value < Count then
    DeleteRange(Value, Count - Value);
  SetLength(FItems, Value);
end;

procedure TList<T>.SetItem(const Index: NativeInt; const Value: T);
var
  LItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[Index];
  FItems[Index] := Value;

  Notify(LItem, caRemove);
  Notify(Value, caAdd);
end;

procedure TList<T>.Sort(Comparer: IComparer<T>);
begin
  //TArray.Sort<T>(FItems, Comparer, 0, Count);
end;

{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create();
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(Comparer: IComparer<T>;
  AOwnsObjects: Boolean);
begin
  inherited Create(Comparer);
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(Values: IEnumerable<T>;
  AOwnsObjects: Boolean);
begin
  inherited Create(Values);
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const Values: array of T;
  AOwnsObjects: Boolean);
begin
  inherited Create(Values);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectList<T>.Notify(const Value: T; const Action: TCollectionChangedAction);
begin
  inherited Notify(Value, Action);
  if FOwnsObjects and (Action = caRemove) then
    Value.Free;
end;

end.

