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

unit ts_Core_KeyValues;

{ Author: Tim Sinaeve }

{
  FEATURES:
    - When a given name does not exist in the collection, the value Unassigned
      is returned (see GetItemValue). => maybe this default behaviour should be
      made configurable.

  TODO :
    - include a TObject property to associate with a key value?
    - Add As<type> methods to get the Value in the appropriate type (like
      DB-fields from a TDataSet)
    - AddMethods AssignFromDataSet passing the fieldnames ?
    - Make it possible to receive keyvalues in an array of const structure (
      so we can use TDataSet.AppendRecord and TDataSet.InsertRecord and
      TDataSet.SetFields)
    - Description property
    - procedure to reset all values for all keys to Unassigned (or a given
      value?) => reset to Unassigned or remove all values from collection?
    - Write to / save from INI file or stream (streaming support needs to be
      investigated)
    - CheckRequiredValues(const AKeynames : string = ''), which checks if the
      given keynames (or all if they are not passed) have a value assigned. If
      this is not the case an exception will be raised.
    - Enumeration support (so we can use it in "for in" statements) in D2006+
}

{
  var
    KV : TtsKeyValues;
    V  : Variant;
  begin
    KV := TtsKeyValues.Create;
    try
      // Normal way to access the keyvalues
      KV['KeyName'] := 'Value';

      // Access the keyvalues like a record type using a custom invokable
      // Variant instance.
      V := KV.AsVarDataRecord;
      V.KeyName := 'Value';
    finally
      KV.Free;
    end;
  end;

  TODO => make it safe when the KeyValues instance is destroyed.
}

{$mode delphi}

//*****************************************************************************

interface

uses
  SysUtils, Classes;

//=============================================================================

type
  TtsKeyValues = class;

  TtsKeyValue = class(TCollectionItem)
  private
    FName        : string;
    FDisplayName : string;
    FValue       : Variant;

  protected
    procedure SetName(const Value: string);
    function GetCollection: TtsKeyValues;
    procedure SetCollection(const Value: TtsKeyValues); reintroduce;
    procedure SetValue(const Value: Variant);
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

  public
    // public methods
    procedure Assign(Source: TPersistent); override;
    function KeyValueString : string;

    // public properties
    { Collection that owns the instance of current TtsKeyValue item. }
    property Collection: TtsKeyValues
      read GetCollection write SetCollection;

  published
    { The item's key name. }
    property Name: string
      read FName write SetName;

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    { The item's key value. }
    property Value : Variant
      read FValue write SetValue;
  end;

  TtsKeyValueClass = class of TtsKeyValue;

//=============================================================================

  TtsKeyValues = class(TCollection)
  private
    FOnChanged : TNotifyEvent;

  protected
    FRefCount : Integer;

    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetItem(Index: Integer): TtsKeyValue;
    procedure SetItem(Index: Integer; const Value: TtsKeyValue);
    function GetItemValue(AName: string): Variant;
    procedure SetItemValue(AName: string; const Value: Variant);
    procedure SetItemName(Item: TCollectionItem); override;
    function GetItemObject(Name: string): TObject;
    procedure SetItemObject(Name: string; const Value: TObject);

    procedure Update(AItem: TCollectionItem); override;

  public
    // constructors and destructors
    constructor Create; overload; virtual;
    constructor Create(const AKeyNames : string); overload;

    function Add: TtsKeyValue;
    function Insert(Index: Integer): TtsKeyValue;
    function Owner: TComponent; reintroduce;

    function KeyValuesString : string;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TtsKeyValue;
    function Find(const AName: string): TtsKeyValue;

    function AsVarArray(const AKeyNames : string) : Variant;
    function AsCommaText(const AKeyNames : string) : string; overload;
    function AsCommaText : string; overload;
    function AsDelimitedText(const AKeyNames    : string;
                             const ADelimiter   : string;
                                   AQuoteValues : Boolean = False;
                                   AQuoteChar   : Char = '"'): string; overload;
    function AsDelimitedText(const ADelimiter   : string;
                                   AQuoteValues : Boolean = False;
                                   AQuoteChar   : Char = '"'): string; overload;

    function AsVarDataRecord : Variant;

    // public properties
    { The TCollectionItem decended class of the Items in the collection. }
    property ItemClass;

    { Returns the item values for a given key (Name). }
    property Values[Name : string]: Variant
      read GetItemValue write SetItemValue; default;

    { Returns the item values for a given key (Name), and stores/returns the
      values as object references. }
    property Objects[Name : string]: TObject
      read GetItemObject write SetItemObject;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TtsKeyValue
      read GetItem write SetItem;
  end; // TtsKeyValues

//*****************************************************************************

implementation

uses
  Variants, Windows,

  ts_Core_Utils;

//=============================================================================

type
  { A custom variant type that implements the mapping from the property names
    to the KeyValues instance. }
  TVarDataRecordType = class(TInvokeableVariantType)
  protected
   // function FixupIdent(const AText: string): string; override;
  public

    procedure Clear(var V: TVarData); override;
    procedure Copy(var   Dest     : TVarData;
                   const Source   : TVarData;
                   const Indirect : Boolean); override;
    function GetProperty(var   Dest : TVarData;
                         const V    : TVarData;
                         const Name : string): Boolean; override;
    function SetProperty(const V     : TVarData;
                         const Name  : string;
                         const Value : TVarData): Boolean; override;
  end;

//-----------------------------------------------------------------------------

type
  { Our customized layout of the variants record data. We only need a reference
    to the TtsKeyValues instance. }
  TVarDataRecordData = packed record
    VType                           : TVarType;
    Reserved1, Reserved2, Reserved3 : Word;
    KeyValues                       : TtsKeyValues;
    Reserved4                       : LongInt;
  end;

//-----------------------------------------------------------------------------

var
  { The global instance of the custom variant type. The data of the custom
    variant is stored in a TVarData record (which is common to all variants),
    but the methods and properties are implemented in this class instance. }
  VarDataRecordType : TVarDataRecordType = nil;

//*****************************************************************************
// non-interfaced routines                                               BEGIN
//*****************************************************************************

{ A global function the get our custom VarType value. This may vary and thus
  is determined at runtime. }

function VarDataRecord: TVarType;
begin
  Result := VarDataRecordType.VarType;
end;

//-----------------------------------------------------------------------------

{ A global function that fills the TVarData fields of the Variant with the
  correct values. }

function VarDataRecordCreate(AKeyValues: TtsKeyValues): Variant;
begin
  VarClear(Result);
  TVarDataRecordData(Result).VType     := VarDataRecord;
  TVarDataRecordData(Result).KeyValues := AKeyValues;
end;

//*****************************************************************************
// non-interfaced routines                                                 END
//*****************************************************************************

{
_______________________________________________________________________________
_______________________________________________________________________________

                             TVarDataRecordType
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TVarDataRecordType.Clear(var V: TVarData);
begin
  { No fancy things to do here, we are only holding a referece to a
    TtsKeyValues instance and we are not supposed to destroy it here. }
  SimplisticClear(V);
end;

//-----------------------------------------------------------------------------

procedure TVarDataRecordType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  { No fancy things to do here, we are only holding a referece to a
    TtsKeyValues instance that can simply be copied here. }
  SimplisticCopy(Dest, Source, Indirect);
end;

//-----------------------------------------------------------------------------

//function TVarDataRecordType.FixupIdent(const AText: string): string;
//begin
//  Result := AText;
//end;

//-----------------------------------------------------------------------------

function TVarDataRecordType.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := True;
  Variant(Dest) := TVarDataRecordData(V).KeyValues[Name];
end;

//-----------------------------------------------------------------------------

function TVarDataRecordType.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := True;
  TVarDataRecordData(V).KeyValues[Name] := Variant(Value);
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

{
_______________________________________________________________________________
_______________________________________________________________________________

                               TtsKeyValues
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TtsKeyValues.Create;
begin
  inherited Create(TtsKeyValue);
end;

//-----------------------------------------------------------------------------

{ Creates a list of keyvalues for the given comma seperated list of key names. }

constructor TtsKeyValues.Create(const AKeyNames: string);
var
  SL : TStringList;
  I  : Integer;
begin
  Create;
  SL := TStringList.Create;
  SL.CommaText := AKeyNames;
  try
    for I := 0 to SL.Count - 1 do
      Values[SL[I]] := Unassigned;
  finally
    SL.Free;
  end;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Items|-------------------------------------------------------------------

function TtsKeyValues.GetItem(Index: Integer): TtsKeyValue;
begin
  Result := inherited Items[Index] as TtsKeyValue;
end;

procedure TtsKeyValues.SetItem(Index: Integer; const Value: TtsKeyValue);
begin
  Items[Index].Assign(Value);
end;

//---|ItemsByName|-------------------------------------------------------------

function TtsKeyValues.GetItemValue(AName: string): Variant;
var
  P : TtsKeyValue;
begin
  P := Find(AName);
  if Assigned(P) then
    Result := P.Value
  else
    Result := Unassigned;
end;

procedure TtsKeyValues.SetItemValue(AName: string; const Value: Variant);
var
  P : TtsKeyValue;
begin
  P := Find(AName);
  if Assigned(P) then
    P.Value := Value
  else
  begin
    P := Add;
    P.Name  := AName;
    P.Value := Value;
  end;
end;

//---|ObjectsByName|-----------------------------------------------------------

function TtsKeyValues.GetItemObject(Name: string): TObject;
begin
  // TODO : check for a valid object reference here
  Result := TObject(TVarData(Values[Name]).vInteger);
end;

procedure TtsKeyValues.SetItemObject(Name: string; const Value: TObject);
begin
  Values[Name] := Integer(Value);
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
         affects more than one item in the collection. }

procedure TtsKeyValues.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change.
// Update gets called from TCollection.Changed.
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

//-----------------------------------------------------------------------------

{ Constructs a unique itemname for a new collection-item. }

procedure TtsKeyValues.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TtsKeyValue(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Adds a new TtsKeyValue instance to the TtsKeyValues collection. }

function TtsKeyValues.Add: TtsKeyValue;
begin
  Result := inherited Add as TtsKeyValue;
end;

//-----------------------------------------------------------------------------

{ Inserts a new TtsKeyValue instance to the TtsKeyValues collection before
  position specified with Index }

function TtsKeyValues.Insert(Index: Integer): TtsKeyValue;
begin
  Result := inherited Insert(Index) as TtsKeyValue;
end;

//-----------------------------------------------------------------------------

{ Represents the Keyvalues collection as a string which can be used for
  diagnostic purposes. }

function TtsKeyValues.KeyValuesString: string;
var
  I  : Integer;
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      SL.Add(Items[I].KeyValueString);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

//-----------------------------------------------------------------------------

function TtsKeyValues.Owner: TComponent;
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

{ IInterface support }

function TtsKeyValues._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

//-----------------------------------------------------------------------------

{ IInterface support }

function TtsKeyValues._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

//-----------------------------------------------------------------------------

{ IInterface support }

function TtsKeyValues.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

//-----------------------------------------------------------------------------

function TtsKeyValues.IndexOf(const AName: string): Integer;
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

function TtsKeyValues.FindItemID(ID: Integer): TtsKeyValue;
begin
  Result := inherited FindItemID(ID) as TtsKeyValue;
end;

//-----------------------------------------------------------------------------

function TtsKeyValues.Find(const AName: string): TtsKeyValue;
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

                                TtsKeyValue
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Collection|--------------------------------------------------------------

function TtsKeyValue.GetCollection: TtsKeyValues;
begin
  Result := inherited Collection as TtsKeyValues;
end;

procedure TtsKeyValue.SetCollection(const Value: TtsKeyValues);
begin
  inherited Collection := Value;
end;

//---|DisplayName|-------------------------------------------------------------

function TtsKeyValue.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TtsKeyValue.SetDisplayName(const Value: string);
begin
  if Value <> DisplayName then
  begin
    Changed(False);
    FDisplayName := Value;
  end;
end;

//---|Name|--------------------------------------------------------------------

procedure TtsKeyValue.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    Changed(False);
    FName := Value;
  end;
end;

//---|Value|-------------------------------------------------------------------

procedure TtsKeyValue.SetValue(const Value: Variant);
begin
  if Value <> Self.Value then
  begin
    Changed(False);
    FValue := Value;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TtsKeyValue.Assign(Source: TPersistent);
begin
 if (Source <> Self) and (Source is TtsKeyValue) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     { <PropertyName> := TtsKeyValue(Source).<PropertyName> }
     Value       := TtsKeyValue(Source).Value;
     Name        := TtsKeyValue(Source).Name;
     DisplayName := TtsKeyValue(Source).DisplayName;
   finally
     if Assigned(Collection) then
       Collection.EndUpdate;
   end;
 end
 else
   inherited Assign(Source);
end;

//-----------------------------------------------------------------------------

{ Used to represent the current item as text. }

function TtsKeyValue.KeyValueString: string;
begin
  if Value <> Null then
    Result := Format('%s = %s', [Name, Value])
  else
    Result := Format('%s = %s', [Name, '<Null>']);
end;

//-----------------------------------------------------------------------------

{ Returns a comma seperated string of values for the given comma seperated
  key names. }

function TtsKeyValues.AsCommaText(const AKeyNames: string): string;
var
  I  : Integer;
  N  : Integer;
  S  : string;
begin
  N := WordCount(AKeyNames, [',']);
  for I := 0 to N - 1 do
  begin
    S := Values[ExtractWord(I + 1, AKeyNames, [','])];
    if I > 0 then
      Result := Result + ',' + S
    else
      Result := S;
  end;
end;

//-----------------------------------------------------------------------------

{ Returns a comma seperated string of all values. }

function TtsKeyValues.AsCommaText: string;
var
  I  : Integer;
  S  : string;
begin
  for I := 0 to Count - 1 do
  begin
    S := VarToStrDef(Items[I].Value, '');
    if I > 0 then
      Result := Result + ',' + S
    else
      Result := S;
  end;
end;

//-----------------------------------------------------------------------------

function TtsKeyValues.AsDelimitedText(const AKeyNames, ADelimiter: string;
  AQuoteValues: Boolean; AQuoteChar: Char): string;
var
  I  : Integer;
  N  : Integer;
  S  : string;
begin
  N := WordCount(AKeyNames, [',']);
  for I := 0 to N - 1 do
  begin
    S := Values[ExtractWord(I + 1, AKeyNames, [','])];
    if AQuoteValues then
      S := AnsiQuotedStr(S, AQuoteChar);
    if I > 0 then
      Result := Result + ADelimiter + S
    else
      Result := S;
  end;
end;

//-----------------------------------------------------------------------------

function TtsKeyValues.AsDelimitedText(const ADelimiter: string;
  AQuoteValues: Boolean; AQuoteChar: Char): string;
var
  I  : Integer;
  S  : string;
begin
  for I := 0 to Count - 1 do
  begin
    S := VarToStrDef(Items[I].Value, '');
    if AQuoteValues then
      S := AnsiQuotedStr(S, AQuoteChar);
    if I > 0 then
      Result := Result + ADelimiter + S
    else
      Result := S;
  end;
end;

//-----------------------------------------------------------------------------

{ Returns for the given list of key names the corresponding values as a Variant
  array.

  AKeyNames
    Comma seperated list of key names.

  Result
    Variant array with the corresponding values.
}

function TtsKeyValues.AsVarArray(const AKeyNames: string): Variant;
var
  VA : array of Variant;
  I  : Integer;
  N  : Integer;
begin
  N := WordCount(AKeyNames, [',']);
  SetLength(VA, N);
  for I := 0 to N - 1 do
    VA[I] := Values[ExtractWord(I + 1, AKeyNames, [','])];
  Result := VarArrayOf(VA);
end;

//-----------------------------------------------------------------------------

{ This method enables us to access the keyvalues in a record-like way. }

function TtsKeyValues.AsVarDataRecord: Variant;
begin
  Result := VarDataRecordCreate(Self);
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

//*****************************************************************************

initialization
  { Create our custom variant type, which will be registered automatically. }
  VarDataRecordType := TVarDataRecordType.Create;

//*****************************************************************************

finalization
  { Free our custom variant type. }
  FreeAndNil(VarDataRecordType);
end.

