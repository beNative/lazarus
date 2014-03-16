unit ODictionary;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  ODictionary.pas

  TODictionary
    - TDictionary<NativeInt, TObject> replacement for non-unicode Delphi and FPC
    - can hold sorted and/or unique NativeInt values
      -> sorted & unique by default
    - basically it is an integer list with associated objects
      -> something like TStringList for integers

}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes, System.Types,
  {$ELSE}
  SysUtils, Classes, Types,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Defaults, System.Generics.Collections,
    {$ELSE}
    Generics.Defaults, Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp;

type

  {$IFDEF FPC}
    TValueRelationship = -1..1;

  const
    LessThanValue = Low(TValueRelationship);
    EqualsValue = 0;
    GreaterThanValue = High(TValueRelationship);

  type
  {$ENDIF}

  {$IFDEF O_GENERICS}
  TODictionaryChange = procedure(Sender: TObject; aKey: ONativeInt; Action: TCollectionNotification) of object;
  {$ELSE}
  TODictionaryChange = procedure(Sender: TObject; aKey: ONativeInt; Action: TListNotification) of object;
  {$ENDIF}

  TODictionaryItem = record
    Key: ONativeInt;
    Value: TObject;
  end;
  PODictionaryItem = ^TODictionaryItem;
  {$IFDEF O_GENERICS}
  TODictionaryItemStorageItem = PODictionaryItem;
  {$ELSE}
  TODictionaryItemStorageItem = Pointer;
  {$ENDIF}

  TODictionarySorted = (soNone, soAscending, soDescending);


  TODictionary = class({$IFDEF O_GENERICS}TList<PODictionaryItem>{$ELSE}TList{$ENDIF})
  private
    fOnChange: TODictionaryChange;
    fLoading: Boolean;
    fDuplicates: TDuplicates;
    fSorted: TODictionarySorted;
    fOwnsObjects: Boolean;

    function CompareValue(A, B: ONativeInt): TValueRelationship;

    procedure InternalInsert(aIndex: Integer; aKey: ONativeInt);

    function GetItem(aIndex: Integer): ONativeInt;
    procedure SetItem(aIndex: Integer; aKey: ONativeInt);
    function GetObject(aIndex: Integer): TObject;
    procedure SetObject(aIndex: Integer; aObject: TObject);
    function GetObjectOfKey(aKey: ONativeInt): TObject;
    procedure SetObjectOfKey(aKey: ONativeInt; aObject: TObject);
    {$IFNDEF NEXTGEN}
    function GetPointer(aIndex: Integer): Pointer;
    procedure SetPointer(aIndex: Integer; aPointer: Pointer);
    function GetPointerOfKey(aKey: ONativeInt): Pointer;
    procedure SetPointerOfKey(aKey: ONativeInt; aPointer: Pointer);
    {$ENDIF}

    procedure SetSorted(aSorted: TODictionarySorted);
  protected
    {$IFDEF O_GENERICS}
    procedure Notify(const aItem: PODictionaryItem; aAction: TCollectionNotification); override;
    procedure DoChange(aKey: ONativeInt; aAction: TCollectionNotification);
    {$ELSE}
    procedure Notify(aPtr: Pointer; aAction: TListNotification); override;
    procedure DoChange(aKey: ONativeInt; aAction: TListNotification);
    {$ENDIF}
  public
    constructor Create(
      aDuplicates: TDuplicates = dupIgnore;
      aSorted: TODictionarySorted = soAscending;
      aOwnsObjects: Boolean = False);
    procedure Assign(Source: TODictionary);

    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    property Loading: Boolean read fLoading;

    function Add(aKey: ONativeInt): Integer;
    function AddObject(aKey: ONativeInt; aObject: TObject): Integer;
    {$IFNDEF NEXTGEN}
    function AddPointer(aKey: ONativeInt; aPointer: Pointer): Integer;
    {$ENDIF}
    function Extract(aKey: ONativeInt): ONativeInt;
    function First: ONativeInt;
    function Find(aKey: ONativeInt; var Index: Integer): Boolean;
    function IndexOf(aKey: ONativeInt): Integer;
    procedure Insert(aIndex: Integer; aKey: ONativeInt);
    function Last: ONativeInt;
    function Remove(aKey: ONativeInt): Integer;
    property Keys[aIndex: Integer]: ONativeInt read GetItem write SetItem; default;
    property Objects[aIndex: Integer]: TObject read GetObject write SetObject;
    property ObjectOfKey[aKey: ONativeInt]: TObject read GetObjectOfKey write SetObjectOfKey;
    {$IFNDEF NEXTGEN}
    property Pointers[aIndex: Integer]: Pointer read GetPointer write SetPointer;
    property PointerOfKey[aKey: ONativeInt]: Pointer read GetPointerOfKey write SetPointerOfKey;
    {$ENDIF}

    {$IFNDEF O_DELPHI_2009}//missing functionality in D2009
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Exchange(Index1, Index2: Integer);
    {$ENDIF}

    procedure DefaultSort(aAsc: Boolean = True);

    property OnChange: TODictionaryChange read fOnChange write fOnChange;
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    property Sorted: TODictionarySorted read fSorted write SetSorted;
  end;

implementation

resourcestring
  SDuplicateInteger = 'ODictionary does not allow duplicates';
  SCannotExchange = 'Sorted ODictionary does not allow exchanging items';
  SCannotMove = 'Sorted ODictionary does not allow moving items';

{ TODictionary }

function TODictionary.Add(aKey: ONativeInt): Integer;
begin
  if (Sorted = soNone) then
    Result := Count
  else
    if Find(aKey, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: raise EListError.Create(SDuplicateInteger);
      end;
  if Result < 0 then
    Result := 0;
  if Result > Count then
    Result := Count;
  InternalInsert(Result, aKey);
end;

function TODictionary.AddObject(aKey: ONativeInt; aObject: TObject): Integer;
begin
  Result := Add(aKey);
  Objects[Result] := aObject;
end;

{$IFNDEF NEXTGEN}
function TODictionary.AddPointer(aKey: ONativeInt; aPointer: Pointer): Integer;
begin
  Result := Add(aKey);
  Pointers[Result] := aPointer;
end;
{$ENDIF}

procedure TODictionary.Assign(Source: TODictionary);
var
  I: Integer;
begin
  Clear;
  Capacity := Source.Count;
  fOnChange := Source.fOnChange;
  fDuplicates := Source.fDuplicates;
  fSorted := Source.fSorted;

  for I := 0 to Source.Count - 1 do begin
    InternalInsert(I, Source[I]);
  end;
end;

function TODictionary.CompareValue(A, B: ONativeInt): TValueRelationship;
begin
  if A = B then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

constructor TODictionary.Create(aDuplicates: TDuplicates;
  aSorted: TODictionarySorted;
  aOwnsObjects: Boolean);
begin
  inherited Create;

  fOnChange := nil;
  fLoading := False;
  fDuplicates := aDuplicates;
  fSorted := aSorted;
  fOwnsObjects := aOwnsObjects;
end;

{$IFNDEF O_GENERICS}
function SortAsc(Item1, Item2: Pointer): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
begin
  if PODictionaryItem(Item1)^.Key = PODictionaryItem(Item2)^.Key then
    Result := EqualsValue
  else if PODictionaryItem(Item1)^.Key < PODictionaryItem(Item2)^.Key then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

function SortDesc(Item1, Item2: Pointer): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
begin
  if PODictionaryItem(Item1)^.Key = PODictionaryItem(Item2)^.Key then
    Result := EqualsValue
  else if PODictionaryItem(Item1)^.Key > PODictionaryItem(Item2)^.Key then
  Result := LessThanValue
else
  Result := GreaterThanValue;
end;
{$ENDIF}

procedure TODictionary.DefaultSort(aAsc: Boolean);
begin
  {$IFDEF O_GENERICS}
  if aAsc then
    Sort(TDelegatedComparer<PODictionaryItem>.Construct(
      function(const Item1, Item2: PODictionaryItem): Integer
      begin
        if Item1^.Key = Item2^.Key then
          Result := EqualsValue
        else if Item1^.Key < Item2^.Key then
          Result := LessThanValue
        else
          Result := GreaterThanValue;
      end
    ))
  else
    Sort(TDelegatedComparer<PODictionaryItem>.Construct(
      function(const Item1, Item2: PODictionaryItem): Integer
      begin
        if Item1^.Key = Item2^.Key then
          Result := EqualsValue
        else if Item1^.Key > Item2^.Key then
          Result := LessThanValue
        else
          Result := GreaterThanValue;
      end
    ));
  {$ELSE}
  if aAsc then
    Sort(SortAsc)
  else
    Sort(SortDesc);
  {$ENDIF}
end;

{$IFDEF O_GENERICS}
procedure TODictionary.DoChange(aKey: ONativeInt; aAction: TCollectionNotification);
{$ELSE}
procedure TODictionary.DoChange(aKey: ONativeInt; aAction: TListNotification);
{$ENDIF}
begin
  if Assigned(fOnChange) then
    fOnChange(Self, aKey, aAction);
end;

{$IFNDEF O_DELPHI_2009}
procedure TODictionary.Exchange(Index1, Index2: Integer);
begin
  if (Sorted = soNone) then
    inherited Exchange(Index1, Index2)
  else
    raise EListError.Create(SCannotExchange);
end;
{$ENDIF}

function TODictionary.Extract(aKey: ONativeInt): ONativeInt;
var
  I: Integer;
  Ptr: PODictionaryItem;
begin
  Result := 0;
  I := IndexOf(aKey);
  if I >= 0 then
  begin
    Result := aKey;
    Ptr := inherited Items[I];
    Delete(I);
    {$IFDEF O_GENERICS}
    Notify(Ptr, cnExtracted);
    {$ELSE}
    Notify(Ptr, lnExtracted);
    {$ENDIF}
  end;
end;

function TODictionary.Find(aKey: ONativeInt; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareValue(Keys[I], aKey);
    if Sorted = soDescending then
      C := -C;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TODictionary.First: ONativeInt;
begin
  Result := Keys[0];
end;

function TODictionary.GetItem(aIndex: Integer): ONativeInt;
begin
  Result := TODictionaryItem(inherited Items[aIndex]^).Key;
end;

function TODictionary.GetObject(aIndex: Integer): TObject;
begin
  Result := TODictionaryItem(inherited Items[aIndex]^).Value;
end;

function TODictionary.GetObjectOfKey(aKey: ONativeInt): TObject;
begin
  Result := Objects[IndexOf(aKey)];
end;

{$IFNDEF NEXTGEN}
function TODictionary.GetPointer(aIndex: Integer): Pointer;
begin
  Result := Pointer(TODictionaryItem(inherited Items[aIndex]^).Value);
end;

function TODictionary.GetPointerOfKey(aKey: ONativeInt): Pointer;
begin
  Result := Pointers[IndexOf(aKey)];
end;
{$ENDIF}

function TODictionary.IndexOf(aKey: ONativeInt): Integer;
begin
  if (Sorted = soNone) then begin
    Result := 0;
    while (Result < Count) and (Keys[Result] <> aKey) do
      Inc(Result);
    if Result = Count then
      Result := -1;

  end else if not Find(aKey, Result) then
    Result := -1;
end;

procedure TODictionary.Insert(aIndex: Integer; aKey: ONativeInt);
begin
  if (Sorted = soNone) then
    InternalInsert(aIndex, aKey)
  else
    Add(aKey);
end;

procedure TODictionary.InternalInsert(aIndex: Integer; aKey: ONativeInt);
var PD: PODictionaryItem;
begin
  GetMem(PD, SizeOf(TODictionaryItem));
  PD^.Key := aKey;
  PD^.Value := nil;
  inherited Insert(aIndex, PD);
end;

function TODictionary.Last: ONativeInt;
begin
  Result := Keys[Count-1];
end;

{$IFNDEF O_DELPHI_2009}//missing functionality in D2009
procedure TODictionary.Move(CurIndex, NewIndex: Integer);
begin
  if (Sorted = soNone) then
    inherited Move(CurIndex, NewIndex)
  else
    raise EListError.Create(SCannotMove);
end;
{$ENDIF}

{$IFDEF O_GENERICS}
procedure TODictionary.Notify(const aItem: PODictionaryItem; aAction: TCollectionNotification);
begin
  DoChange(TODictionaryItem(aItem^).Key, aAction);
  if aAction = cnRemoved then begin
    if fOwnsObjects then
      aItem.Value.Free;
    FreeMem(aItem, SizeOf(TODictionaryItem));
  end;
end;
{$ELSE}
procedure TODictionary.Notify(aPtr: Pointer; aAction: TListNotification);
begin
  DoChange(TODictionaryItem(aPtr^).Key, aAction);
  if aAction = lnDeleted then begin
    if fOwnsObjects then
      TODictionaryItem(aPtr^).Value.Free;
    FreeMem(aPtr, SizeOf(TODictionaryItem));
  end;
end;
{$ENDIF}

procedure TODictionary.ReadData(Reader: TReader);
begin
  fLoading := True;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      Add(Reader.ReadInt64);
    end;
    Reader.ReadListEnd;
  finally
    fLoading := False;
  end;
end;

function TODictionary.Remove(aKey: ONativeInt): Integer;
begin
  Result := IndexOf(aKey);
  if Result >= 0 then
    Delete(Result);
end;

procedure TODictionary.SetItem(aIndex: Integer; aKey: ONativeInt);
begin
  TODictionaryItem(inherited Items[aIndex]^).Key := aKey;
end;

procedure TODictionary.SetObject(aIndex: Integer; aObject: TObject);
begin
  TODictionaryItem(inherited Items[aIndex]^).Value := aObject;
end;

procedure TODictionary.SetObjectOfKey(aKey: ONativeInt; aObject: TObject);
begin
  Objects[IndexOf(aKey)] := aObject;
end;

{$IFNDEF NEXTGEN}
procedure TODictionary.SetPointer(aIndex: Integer; aPointer: Pointer);
begin
  TODictionaryItem(inherited Items[aIndex]^).Value := TObject(aPointer);
end;

procedure TODictionary.SetPointerOfKey(aKey: ONativeInt; aPointer: Pointer);
begin
  Pointers[IndexOf(aKey)] := aPointer;
end;
{$ENDIF}

procedure TODictionary.SetSorted(aSorted: TODictionarySorted);
begin
  if fSorted <> aSorted then
  begin
    if aSorted <> soNone then DefaultSort(aSorted = soAscending);
    fSorted := aSorted;
  end;
end;

procedure TODictionary.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger(Keys[I]);
  Writer.WriteListEnd;
end;

end.
