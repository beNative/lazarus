{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwHashStrings.pas, released October 02.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 2002 Martin Waldenburg.
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

Contributor(s): _____________________________________.


Last Modified: October 02.
Current Version: 1.1

Notes:
  TmwHashStrings combines the advantages of hasching and binary search.

  TmwStringHash: Any TStrings descentand can be assigned to and hashed by it.

  TmwHashedStringList uses TmwStringHash to perform hashes.

Modification history:

Known Issues:
-----------------------------------------------------------------------------}

unit ts.Core.HashStrings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils;

type

  TCharsSet = Set of Char;
  PBooleanChars = ^TBooleanChars;
  TBooleanChars = array[#0..#255] of ByteBool;

  PmwHashStringItem = ^TmwHashStringItem;
  TmwHashStringItem = record
    HashValue: Cardinal;
    Key: String;
  end;

  PmwHashStringItemList = ^TmwHashStringItemList;
  TmwHashStringItemList = array[0..0] of PmwHashStringItem;

  TmwHashStrings = class(TObject)
  private
    FList: PmwHashStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    fCaseSensitive: Boolean;
    fSorted: Boolean;
    fIdentifiers: PBooleanChars;
    procedure SetCapacity(const NewCapacity: Integer);
    function GetItems(const Index: Integer): PmwHashStringItem;
    procedure SetCaseSensitive(const Value: Boolean);
    function SorCompare(const Item1, Item2: PmwHashStringItem): Integer;
  protected
    function CompareValue(const Value1, Value2: Cardinal): Integer;
    function CompareString(const S1, S2: String): Boolean;
    function ComparePChar(const P1: PChar; const Len: Integer; const S: String): Boolean;
    function CreateNewItem: PmwHashStringItem; virtual;
    procedure Delete(const Index: Integer);
    function Expand: Integer;
    function HashOf(const Key: string): Cardinal; overload;
    function HashOf(var Len: Integer; const P: PChar): Cardinal; overload;
    function Insert(Index: Integer; const Item: PmwHashStringItem): Integer;
  public
    destructor Destroy; override;
    procedure QuickSort;
    function Add(const S: String): Integer; overload;
    function Add(const Item: PmwHashStringItem): Integer; overload;
    function AddSorted(const Item: PmwHashStringItem): Integer; overload;
    function AddSorted(const S: String): Integer; overload;
    procedure Clear; virtual;
    function IndexOf(const S: String): Integer; overload;
    function IndexOf(var P: Pointer): Integer; overload;
    function Remove(const S: String): Integer;
    property Capacity: Integer read FCapacity;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Count: Integer read FCount;
    property Items[const Index: Integer]: PmwHashStringItem read GetItems; default;
    property List: PmwHashStringItemList read FList;
    property Sorted: Boolean read fSorted;
    property Identifiers: PBooleanChars read fIdentifiers write fIdentifiers;
  end;

  TmwQuickAndDirtyHashStrings = class(TObject)
  private
    FList: PmwHashStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    fCaseSensitive: Boolean;
    fSorted: Boolean;
    fIdentifiers: PBooleanChars;
    procedure SetCapacity(const NewCapacity: Integer);
    function GetItems(const Index: Integer): PmwHashStringItem;
    procedure SetCaseSensitive(const Value: Boolean);
    function SorCompare(const Item1, Item2: PmwHashStringItem): Integer;
  protected
    MaxHashVal: Cardinal;
    function CompareValue(const Value1, Value2: Cardinal): Integer;
    function CompareString(const S1, S2: String): Boolean;
    function ComparePChar(const P1: PChar; const Len: Integer; const S: String): Boolean;
    function CreateNewItem: PmwHashStringItem; virtual;
    procedure Delete(const Index: Integer);
    function Expand: Integer;
    function HashOf(const Key: string): Cardinal; overload;
    function HashOf(var Len: Integer; const P: PChar): Cardinal; overload;
    function Insert(Index: Integer; const Item: PmwHashStringItem): Integer;
  public
    destructor Destroy; override;
    procedure QuickSort;
    function Add(const S: String): Integer; overload;
    function Add(const Item: PmwHashStringItem): Integer; overload;
    function AddSorted(const Item: PmwHashStringItem): Integer; overload;
    function AddSorted(const S: String): Integer; overload;
    procedure Clear; virtual;
    function IndexOf(const S: String): Integer; overload;
    function IndexOf(var P: Pointer): Integer; overload;
    function Remove(const S: String): Integer;
    property Capacity: Integer read FCapacity;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Count: Integer read FCount;
    property Items[const Index: Integer]: PmwHashStringItem read GetItems; default;
    property List: PmwHashStringItemList read FList;
    property Sorted: Boolean read fSorted;
    property Identifiers: PBooleanChars read fIdentifiers write fIdentifiers;
  end;

  TmwHashItem = record
    HashValue: Cardinal;
    Index: Integer;
  end;

  TmwHashItemList = array of array of TmwHashItem;

  TmwStringHash = class(TPersistent)
  private
    FList: TmwHashItemList;
    fCaseSensitive: Boolean;
    fOwner: TStrings;
    fModulo: Cardinal;
    function CompareString(const S1, S2: String): Boolean;
    procedure SetModulo(const Value: Cardinal);
  protected
    property Modulo: Cardinal read fModulo write SetModulo;
    function HashOf(const Key: string; var ValMod: Cardinal): Cardinal;
  public
    destructor Destroy; override;
    procedure Add(const S: String; const aIndex: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function ValueOf(const Key: string): Integer;
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
    property List: TmwHashItemList read FList;
    property Owner: TStrings read fOwner write fOwner;
  end;


  TmwHashedStringList = class(TStringList)
  private
    FValueHash: TmwStringHash;
    FNameHash: TmwStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
  end;

implementation

var
  mwHashTable: array[#0..#255] of byte;

procedure InitHashTable;
var
  I: Char;
  C: array[0..1] of Char;
  S: String;
begin
  C[1]:= #0;
  for I := #0 to #255 do
  begin
    C[0]:= I;
    S:= AnsiUpperCase(C);
    case S <> '' of
      True: mwHashTable[I] := Ord(S[1]);
      False: mwHashTable[I] := Ord(I);
    end;
  end;
end;


{ TmwHashStrings }

function TmwHashStrings.Add(const S: String): Integer;
var
  Item: PmwHashStringItem;
begin
  Item:= CreateNewItem;
  Item.Key:= S;
  Item.HashValue:= HashOf(S);
  Result:= Add(Item);
end;

function TmwHashStrings.Add(const Item: PmwHashStringItem): Integer;
begin
  fSorted:= False;
  Result:= Insert(fCount, Item);
end;

function TmwHashStrings.AddSorted(const S: String): Integer;
var
  Item: PmwHashStringItem;
begin
  Item:= CreateNewItem;
  Item.Key:= S;
  Result:= AddSorted(Item);
end;

function TmwHashStrings.AddSorted(const Item: PmwHashStringItem): Integer;
var
  Val, First, Last, Temp: Integer;
  Larger: ByteBool;
begin
  Result:= 0;
  Val:= HashOf(Item.Key);
  Item.HashValue := Val;
  Larger:= False;
  Temp:= 0;
  if FCount > 0 then
  begin
    if not sorted then QuickSort;
    First:=0;
    Last:= FCount-1;
    while First<=Last do
    begin
      Temp:=(First+Last)shr 1;
      Case CompareValue(Val, fList[Temp].HashValue) of
        -1:
          begin
            Last:=Temp-1;
            Larger:=False;
          end;
        0:
          begin
            Larger:=False;
            break;
          end;
        1:
          begin
            First:=Temp+1;
            Larger:=True;
          end;
      end;
    end;
    Case Larger of
      True: Result:= Insert(Temp+1, Item);
      False: Result:= Insert(Temp, Item);
    end;
  end else Insert(0, Item);
end;

procedure TmwHashStrings.Clear;
var
  I: Integer;
begin
  if fCount = 0 then exit;
  for I:= 0 to fCount -1 do
    Dispose(fList[I]);
  ReallocMem(FList, 0);
  fCount:= 0;
  fCapacity:= 0;
end;

function TmwHashStrings.ComparePChar(const P1: PChar; const Len: Integer;
  const S: String): Boolean;
var
  I: Integer;
  P2: PChar;
begin
  Result:= False;
  P2:= PChar(S);
  if Len = Length(S) then
  begin
    Result:= True;
    case fCaseSensitive of
      True:
        for I:= Len -1 downto 0 do
        begin
          if P1[I] <>  P2[I] then
          begin
            Result:= False;
            break;
          end;
        end;
      False:
        for I:= Len -1 downto 0 do
        begin
          if mwHashTable[P1[I]] <>  mwHashTable[P2[I]] then
          begin
            Result:= False;
            break;
          end;
        end;
    end;
  end;
end;

function TmwHashStrings.CompareString(const S1, S2: String): Boolean;
begin
  Result:= False;
  if Length(S1) <> Length(S2) then exit;
  case fCaseSensitive of
    True: Result:= CompareStr(S1, S2) = 0;
    False: Result:= SameText(S1, S2);
  end;
end;

function TmwHashStrings.CompareValue(const Value1, Value2: Cardinal): Integer;
begin
  Result:= 0;
  if Value1 < Value2 then Result:= -1 else
  if Value1 > Value2 then Result:= 1;
end;

function TmwHashStrings.CreateNewItem: PmwHashStringItem;
begin
  New(Result);
end;

procedure TmwHashStrings.Delete(const Index: Integer);
begin
  if (Index <= 0) and (Index < FCount) then
  begin
    Dispose(FList[Index]);
    dec(FCount);
    if Index < FCount then
      System.Move(FList^[Index + 1], FList^[Index],
        (FCount - Index) * SizeOf(PmwHashStringItem));
  end;
end;

destructor TmwHashStrings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TmwHashStrings.Expand: Integer;
begin
  Result:= 1 + fCount + fCount div 10;
  SetCapacity(Result);
end;

function TmwHashStrings.GetItems(const Index: Integer): PmwHashStringItem;
begin
  if (Index >= 0) and(Index < fCount) then
    Result:= fList[Index]
  else
    Result:= nil;
end;

function TmwHashStrings.HashOf(const Key: string): Cardinal;
{modified version of HashDKC3 posted to  delphi.language.basm}
var
  P, P2: PChar;
begin
  P:= PChar(Key);
  P2:= P + Length(Key);
  Result := 0;
  case fCaseSensitive of
    True: while P < P2 do
      begin
        Result := Result shl 10 - Result shl 5 - Result + Ord(P^);
        inc(P);
      end;
    False: while P < P2 do
      begin
        Result := Result shl 10 - Result shl 5 - Result + mwHashTable[P^];
        inc(P);
      end;
  end;
end;

function TmwHashStrings.HashOf(var Len: Integer; const P: PChar): Cardinal;
{modified version of HashDKC3 posted to  delphi.language.basm}
begin
  Result:= 0;
  Len:= 0;
  case fCaseSensitive of
    True: while Identifiers[P[Len]] do
      begin
        Result := Result shl 10 - Result shl 5 - Result + Ord(P[Len]);
        inc(Len);
      end;
    False: while Identifiers[P[Len]] do
      begin
        Result := Result shl 10 - Result shl 5 - Result + mwHashTable[P[Len]];
        inc(Len);
      end;
  end;
end;

function TmwHashStrings.IndexOf(const S: String): Integer;
var
  Value: Cardinal;
  First, Last, Temp: Integer;
begin
  if not sorted then QuickSort;
  Value:= HashOf(s);
  Result:= -1;
  First:= 0;
  Last:= Count -1;
  Temp:= 0;
  while First <= Last do
  begin
    Temp:= (First + Last) div 2;
    case CompareValue(Value, fList[Temp].HashValue) of
      1: First:= Temp +1;
      0:
        begin
          Result:= Temp;
          break;
        end;
      -1: Last:= Temp-1;
    end;
  end;
  if Result <> -1 then
  begin
    Result:= -1;
    First:= Temp;
    repeat
      if CompareString(S, fList[First].Key) then
      begin
        Result:= First;
        Exit;
      end;
      dec(First);
    until (First < 0) or (CompareValue(Value, fList[First].HashValue) <> 0);
    Last:= Temp +1;
    if Last = Count then Exit;
    repeat
      if CompareString(S, fList[Last].Key) then
      begin
        Result:= Last;
        Exit;
      end;
      inc(Last);
    until (Last >= Count) or (CompareValue(Value, fList[Last].HashValue) <> 0);
  end;
end;

function TmwHashStrings.IndexOf(var P: Pointer): Integer;
var
  Value: Cardinal;
  First, Last, Temp, Len: Integer;
begin
  if not sorted then QuickSort;
  Value:= HashOf(Len, P);
  Result:= -1;
  First:= 0;
  Last:= Count -1;
  Temp:= 0;
  while First <= Last do
  begin
    Temp:= (First + Last) div 2;
    case CompareValue(Value, fList[Temp].HashValue) of
      1: First:= Temp +1;
      0:
        begin
          Result:= Temp;
          break;
        end;
      -1: Last:= Temp-1;
    end;
  end;
  if Result <> -1 then
  begin
    Result:= -1;
    First:= Temp;
    repeat
      if ComparePChar(P, Len, fList[First].Key) then
      begin
        Result:= First;
        inc(PChar(P), Len);
        Exit;
      end;
      dec(First);
    until (First < 0) or (CompareValue(Value, fList[First].HashValue) <> 0);
    Last:= Temp +1;
    if Last = Count then Exit;
    repeat
      if ComparePChar(P, Len, fList[Last].Key) then
      begin
        Result:= Last;
        inc(PChar(P), Len);
        Exit;
      end;
      inc(Last);
    until (Last >= Count) or (CompareValue(Value, fList[Last].HashValue) <> 0);
  end;
end;

function TmwHashStrings.Insert(Index: Integer; const Item: PmwHashStringItem): Integer;
begin
  if fCount = fCapacity then
    Expand;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(PmwHashStringItem));
  FList^[Index] := Item;
  inc(FCount);
  Result:= Index;
end;

{ Based on a non-recursive QuickSort from the SWAG-Archive.
  ( TV Sorting Unit by Brad Williams ) }
procedure TmwHashStrings.QuickSort;
var
  Left, Right, SubArray, SubLeft, SubRight: LongInt;
  Temp, Pivot: PmwHashStringItem;
  Stack: array[1..32]of record First, Last: LongInt;
  end;
begin
  if Count < 2 then Exit;
  SubArray:=1;
  Stack[SubArray].First:=0;
  Stack[SubArray].Last:=Count-1;
  repeat
    Left:=Stack[SubArray].First;
    Right:=Stack[SubArray].Last;
    Dec(SubArray);
    repeat
      SubLeft:=Left;
      SubRight:=Right;
      Pivot:=fList[(Left+Right)shr 1];
      repeat
        while SorCompare(fList[SubLeft], Pivot)<0 do Inc(SubLeft);
        while SorCompare(fList[SubRight], Pivot)>0 do Dec(SubRight);
        IF SubLeft<=SubRight then
        begin
          Temp:=fList[SubLeft];
          fList[SubLeft]:=fList[SubRight];
          fList[SubRight]:=Temp;
          Inc(SubLeft);
          Dec(SubRight);
        end;
      until SubLeft>SubRight;
      IF SubLeft<Right then
      begin
        Inc(SubArray);
        Stack[SubArray].First:=SubLeft;
        Stack[SubArray].Last:=Right;
      end;
      Right:=SubRight;
    until Left>=Right;
  until SubArray=0;
  fSorted:= True;
end; { QuickSort }

function TmwHashStrings.Remove(const S: String): Integer;
begin
  Result:= IndexOf(S);
  if Result <> -1 then
    Delete(Result);
end;

procedure TmwHashStrings.SetCapacity(const NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TmwHashStrings.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    if Count > 0 then
    begin
      raise Exception.Create('Must be empty');
      exit;
    end;
    fCaseSensitive := Value;
  end;
end;

function TmwHashStrings.SorCompare(const Item1, Item2: PmwHashStringItem): Integer;
begin
  Result:= 0;
  if Item1.HashValue < Item2.HashValue then Result:= -1 else
  if Item1.HashValue > Item2.HashValue then Result:= 1;
end;

{ TmwQuickAndDirtyHashStrings }

function TmwQuickAndDirtyHashStrings.Add(const S: String): Integer;
var
  Item: PmwHashStringItem;
begin
  Item:= CreateNewItem;
  Item.Key:= S;
  Item.HashValue:= HashOf(S);
  if Item.HashValue > MaxHashVal then MaxHashVal:= Item.HashValue;
  Result:= Add(Item);
end;

function TmwQuickAndDirtyHashStrings.Add(const Item: PmwHashStringItem): Integer;
begin
  fSorted:= False;
  Result:= Insert(fCount, Item);
end;

function TmwQuickAndDirtyHashStrings.AddSorted(const S: String): Integer;
var
  Item: PmwHashStringItem;
begin
  Item:= CreateNewItem;
  Item.Key:= S;
  Result:= AddSorted(Item);
end;

function TmwQuickAndDirtyHashStrings.AddSorted(const Item: PmwHashStringItem): Integer;
var
  Val: Cardinal;
  First, Last, Temp: Integer;
  Larger: ByteBool;
begin
  Result:= 0;
  Val:= HashOf(Item.Key);
  if Val > MaxHashVal then MaxHashVal:= Val;
  Item.HashValue := Val;
  Larger:= False;
  Temp:= 0;
  if FCount > 0 then
  begin
    if not sorted then QuickSort;
    First:=0;
    Last:= FCount-1;
    while First<=Last do
    begin
      Temp:=(First+Last)shr 1;
      Case CompareValue(Val, fList[Temp].HashValue) of
        -1:
          begin
            Last:=Temp-1;
            Larger:=False;
          end;
        0:
          begin
            Larger:=False;
            break;
          end;
        1:
          begin
            First:=Temp+1;
            Larger:=True;
          end;
      end;
    end;
    Case Larger of
      True: Result:= Insert(Temp+1, Item);
      False: Result:= Insert(Temp, Item);
    end;
  end else Insert(0, Item);
end;

procedure TmwQuickAndDirtyHashStrings.Clear;
var
  I: Integer;
begin
  if fCount = 0 then exit;
  for I:= 0 to fCount -1 do
    Dispose(fList[I]);
  ReallocMem(FList, 0);
  fCount:= 0;
  fCapacity:= 0;
end;

function TmwQuickAndDirtyHashStrings.ComparePChar(const P1: PChar; const Len: Integer;
  const S: String): Boolean;
var
  I: Integer;
  P2: PChar;
begin
  Result:= False;
  P2:= PChar(S);
  if Len = Length(S) then
  begin
    Result:= True;
    case fCaseSensitive of
      True:
        for I:= Len -1 downto 0 do
        begin
          if P1[I] <>  P2[I] then
          begin
            Result:= False;
            break;
          end;
        end;
      False:
        for I:= Len -1 downto 0 do
        begin
          if mwHashTable[P1[I]] <>  mwHashTable[P2[I]] then
          begin
            Result:= False;
            break;
          end;
        end;
    end;
  end;
end;

function TmwQuickAndDirtyHashStrings.CompareString(const S1, S2: String): Boolean;
begin
  Result:= False;
  if Length(S1) <> Length(S2) then exit;
  case fCaseSensitive of
    True: Result:= CompareStr(S1, S2) = 0;
    False: Result:= SameText(S1, S2);
  end;
end;

function TmwQuickAndDirtyHashStrings.CompareValue(const Value1, Value2: Cardinal): Integer;
begin
  Result:= 0;
  if Value1 < Value2 then Result:= -1 else
  if Value1 > Value2 then Result:= 1;
end;

function TmwQuickAndDirtyHashStrings.CreateNewItem: PmwHashStringItem;
begin
  New(Result);
end;

procedure TmwQuickAndDirtyHashStrings.Delete(const Index: Integer);
begin
  if (Index <= 0) and (Index < FCount) then
  begin
    Dispose(FList[Index]);
    dec(FCount);
    if Index < FCount then
      System.Move(FList^[Index + 1], FList^[Index],
        (FCount - Index) * SizeOf(PmwHashStringItem));
  end;
end;

destructor TmwQuickAndDirtyHashStrings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TmwQuickAndDirtyHashStrings.Expand: Integer;
begin
  Result:= 1 + fCount + fCount div 10;
  SetCapacity(Result);
end;

function TmwQuickAndDirtyHashStrings.GetItems(const Index: Integer): PmwHashStringItem;
begin
  if (Index >= 0) and(Index < fCount) then
    Result:= fList[Index]
  else
    Result:= nil;
end;

function TmwQuickAndDirtyHashStrings.HashOf(const Key: string): Cardinal;
{modified version of HashDKC3 posted to  delphi.language.basm}
var
  P, P2: PChar;
begin
  P:= PChar(Key);
  Result := 0;
  P2:= P + Length(Key);
  case fCaseSensitive of
    True: while P < P2 do
      begin
        inc(Result,  Ord(P^));
        inc(P);
      end;
    False: while P < P2 do
      begin
        inc(Result,  mwHashTable[P^]);
        inc(P);
      end;
  end;
end;

function TmwQuickAndDirtyHashStrings.HashOf(var Len: Integer; const P: PChar): Cardinal;
{modified version of HashDKC3 posted to  delphi.language.basm}
begin
  Result:= 0;
  Len:= 0;
  case fCaseSensitive of
    True: while Identifiers[P[Len]] do
      begin
        inc(Result, Ord(P[Len]));
        inc(Len);
      end;
    False: while Identifiers[P[Len]] do
      begin
        inc(Result,  mwHashTable[P[Len]]);
        inc(Len);
      end;
  end;
end;

function TmwQuickAndDirtyHashStrings.IndexOf(const S: String): Integer;
var
  Value: Cardinal;
  First, Last, Temp: Integer;
begin
  if not sorted then QuickSort;
  Value:= HashOf(s);
  Result:= -1;
  if (Value > MaxHashVal) then Exit;
  First:= 0;
  Last:= Count -1;
  Temp:= 0;
  while First <= Last do
  begin
    Temp:= (First + Last) div 2;
    case CompareValue(Value, fList[Temp].HashValue) of
      1: First:= Temp +1;
      0:
        begin
          Result:= Temp;
          break;
        end;
      -1: Last:= Temp-1;
    end;
  end;
  if Result <> -1 then
  begin
    Result:= -1;
    First:= Temp;
    repeat
      if CompareString(S, fList[First].Key) then
      begin
        Result:= First;
        Exit;
      end;
      dec(First);
    until (First < 0) or (CompareValue(Value, fList[First].HashValue) <> 0);
    Last:= Temp +1;
    if Last = Count then Exit;
    repeat
      if CompareString(S, fList[Last].Key) then
      begin
        Result:= Last;
        Exit;
      end;
      inc(Last);
    until (Last >= Count) or (CompareValue(Value, fList[Last].HashValue) <> 0);
  end;
end;

function TmwQuickAndDirtyHashStrings.IndexOf(var P: Pointer): Integer;
var
  Value: Cardinal;
  First, Last, Temp, Len: Integer;
begin
  if not sorted then QuickSort;
  Value:= HashOf(Len, P);
  Result:= -1;
  First:= 0;
  Last:= Count -1;
  Temp:= 0;
  while First <= Last do
  begin
    Temp:= (First + Last) div 2;
    case CompareValue(Value, fList[Temp].HashValue) of
      1: First:= Temp +1;
      0:
        begin
          Result:= Temp;
          break;
        end;
      -1: Last:= Temp-1;
    end;
  end;
  if Result <> -1 then
  begin
    Result:= -1;
    First:= Temp;
    repeat
      if ComparePChar(P, Len, fList[First].Key) then
      begin
        Result:= First;
        inc(PChar(P), Len);
        Exit;
      end;
      dec(First);
    until (First < 0) or (CompareValue(Value, fList[First].HashValue) <> 0);
    Last:= Temp +1;
    if Last = Count then Exit;
    repeat
      if ComparePChar(P, Len, fList[Last].Key) then
      begin
        Result:= Last;
        inc(PChar(P), Len);
        Exit;
      end;
      inc(Last);
    until (Last >= Count) or (CompareValue(Value, fList[Last].HashValue) <> 0);
  end;
end;

function TmwQuickAndDirtyHashStrings.Insert(Index: Integer; const Item: PmwHashStringItem): Integer;
begin
  if fCount = fCapacity then
    Expand;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(PmwHashStringItem));
  FList^[Index] := Item;
  inc(FCount);
  Result:= Index;
end;

{ Based on a non-recursive QuickSort from the SWAG-Archive.
  ( TV Sorting Unit by Brad Williams ) }
procedure TmwQuickAndDirtyHashStrings.QuickSort;
var
  Left, Right, SubArray, SubLeft, SubRight: LongInt;
  Temp, Pivot: PmwHashStringItem;
  Stack: array[1..32]of record First, Last: LongInt;
  end;
begin
  if Count < 2 then Exit;
  SubArray:=1;
  Stack[SubArray].First:=0;
  Stack[SubArray].Last:=Count-1;
  repeat
    Left:=Stack[SubArray].First;
    Right:=Stack[SubArray].Last;
    Dec(SubArray);
    repeat
      SubLeft:=Left;
      SubRight:=Right;
      Pivot:=fList[(Left+Right)shr 1];
      repeat
        while SorCompare(fList[SubLeft], Pivot)<0 do Inc(SubLeft);
        while SorCompare(fList[SubRight], Pivot)>0 do Dec(SubRight);
        IF SubLeft<=SubRight then
        begin
          Temp:=fList[SubLeft];
          fList[SubLeft]:=fList[SubRight];
          fList[SubRight]:=Temp;
          Inc(SubLeft);
          Dec(SubRight);
        end;
      until SubLeft>SubRight;
      IF SubLeft<Right then
      begin
        Inc(SubArray);
        Stack[SubArray].First:=SubLeft;
        Stack[SubArray].Last:=Right;
      end;
      Right:=SubRight;
    until Left>=Right;
  until SubArray=0;
  fSorted:= True;
end; { QuickSort }

function TmwQuickAndDirtyHashStrings.Remove(const S: String): Integer;
begin
  Result:= IndexOf(S);
  if Result <> -1 then
    Delete(Result);
end;

procedure TmwQuickAndDirtyHashStrings.SetCapacity(const NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TmwQuickAndDirtyHashStrings.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    if Count > 0 then
    begin
      raise Exception.Create('Must be empty');
      exit;
    end;
    fCaseSensitive := Value;
  end;
end;

function TmwQuickAndDirtyHashStrings.SorCompare(const Item1, Item2: PmwHashStringItem): Integer;
begin
  Result:= 0;
  if Item1.HashValue < Item2.HashValue then Result:= -1 else
  if Item1.HashValue > Item2.HashValue then Result:= 1;
end;

{ TmwStringHash }

procedure TmwStringHash.Add(const S: String; const aIndex: Integer);
var
  Val, ModVal, SubCount: Cardinal;
begin
  Val:= HashOf(S, ModVal);
  SubCount:= Length(FList[ModVal]);
  SetLength(FList[ModVal], SubCount +1);
  FList[ModVal][SubCount].HashValue := Val;
  FList[ModVal][SubCount].Index:= aIndex;
end;

procedure TmwStringHash.Assign(Source: TPersistent);
var
  I: Integer;
  Strings: TStrings;
begin
  if Source is TStrings then
  begin
    Clear;
    Strings:= TStrings(Source);
    fOwner:= Strings;
    Modulo:= Strings.Count div 3;
    for I := 0 to Strings.Count - 1  do
      Add(Strings[I], I);
  end
  else
    inherited Assign(Source);
end;

procedure TmwStringHash.Clear;
var
  I: Integer;
begin
  for I:= 0 to fModulo -1 do
    SetLength(FList[I], 0);
  SetLength(FList, 0);
  fModulo:= 0;
end;

function TmwStringHash.CompareString(const S1, S2: String): Boolean;
begin
  Result:= False;
  if Length(S1) <> Length(S2) then exit;
  case fCaseSensitive of
    True: Result:= CompareStr(S1, S2) = 0;
    False: Result:= SameText(S1, S2);
  end;
end;

destructor TmwStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TmwStringHash.HashOf(const Key: string; var ValMod: Cardinal): Cardinal;
{modified version of HashDKC3 posted to  delphi.language.basm}
var
  P, P2: PChar;
begin
  P:= PChar(Key);
  P2:= P + Length(Key);
  Result := 0;
  case fCaseSensitive of
    True: while P < P2 do
      begin
        Result := Result shl 10 - Result shl 5 - Result + Ord(P^);
        inc(P);
      end;
    False: while P < P2 do
      begin
        Result := Result shl 10 - Result shl 5 - Result + mwHashTable[P^];
        inc(P);
      end;
  end;
  ValMod:= Result mod fModulo;
end;

procedure TmwStringHash.SetModulo(const Value: Cardinal);
begin
  fModulo := Value;
  SetLength(FList, Value +1);
end;

function TmwStringHash.ValueOf(const Key: string): Integer;
var
  I: Integer;
  Val, ModVal, SubCount: Cardinal;
begin
  Result:= -1;
  Val:= HashOf(Key, ModVal);
  SubCount:= Length(FList[ModVal]);
  for I:= 0 to SubCount -1 do
  case FList[ModVal][I].HashValue = Val of
    True:
      if CompareString(Key, fOwner[FList[ModVal][I].Index]) then
      begin
        Result:= FList[ModVal][I].Index;
        Exit;
      end;
  end;
end;

{ TmwHashedStringList }

procedure TmwHashedStringList.Changed;
begin
  inherited;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor TmwHashedStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited;
end;

function TmwHashedStringList.IndexOf(const S: string): Integer;
begin
  UpdateValueHash;
  Result :=  FValueHash.ValueOf(S);
end;

function TmwHashedStringList.IndexOfName(const Name: string): Integer;
begin
  UpdateNameHash;
  Result := FNameHash.ValueOf(Name);
end;

procedure TmwHashedStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: string;
begin
  if FNameHashValid then Exit;
  if FNameHash = nil then
  begin
    FNameHash := TmwStringHash.Create;
    FNameHash.fOwner:= Self;
  end
  else
    FNameHash.Clear;
  FNameHash.CaseSensitive := CaseSensitive;
  FNameHash.Modulo:= Count div 3;
  for I := 0 to Count - 1  do
  begin
    Key := Get(I);
    P := AnsiPos('=', Key);
    if P <> 0 then
    begin
      Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TmwHashedStringList.UpdateValueHash;
begin
  if FValueHashValid then Exit;
  if FValueHash = nil then
    FValueHash := TmwStringHash.Create;
  FValueHash.CaseSensitive := CaseSensitive;
  FValueHash.Assign(Self);
  FValueHashValid := True;
end;

initialization
  InitHashTable;
end.


