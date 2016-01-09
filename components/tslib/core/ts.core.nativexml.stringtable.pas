{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

{ unit sdStringTable

  An optimized table of *unique* strings, using two separate sorted indices:
  - by (string) ID
  - by sdCompareRefString method

  The sdCompareRefString method does not use common alphabetical compare, but
  rather a comparison from first character, then last character, then 2nd,
  then before-last, etc. until all characters are compared, or a mismatch is
  found.

  Since many (programmer) strings have numbers at the end of the string,
  (e.g. "MyNewNode1", "MyNewNode2", etc), the comparison terminates earlier than
  with a common alphabetical compare.

  sdStringTable is used by NativeXml but can also be used independently in
  your projects.

  Author: Nils Haeck M.Sc. (n.haeck@simdesign.nl)
  Original Date: 28 May 2007

  Modified:
  05jan2011: enhancement, no longer uses stringrec
  17jun2011: changed TStringTable ancestor from TDebugPersistent to TDebugComponent
  24jun2011: "find" fix
  18jul2011: renamed TsdStringTable to TsdSymbolTable and added TsdSymbolStyle

  It is NOT allowed under ANY circumstances to publish or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.

  Copyright (c) 2007 - 2011 Simdesign BV
}
unit ts.Core.NativeXml.StringTable;

{$IFDEF lcl}{$MODE Delphi}{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,

  ts.Core.NativeXml.Debug;

  // symbol styles (cardinal)
  // Default symbol style is ssUnknown, but highlevel code can
  // distinguish between symbol styles. TsdSymbolTable just stores
  // the symbol as counted Utf8String.

const

  ssUnknown      =  0; // data not determined yet
  ssString       =  1; // data is a string
  ssBase64Binary =  2; // data is binary and will be handled by Base64 funcs
  ssHexBinary    =  3; // data is binary and will be handled by BinHex funcs
  ssBoolean      =  4; // boolean (stored in a Byte, just 0 and 1 of cardinal)
  ssCardinal     =  5; // cardinal (1..N bytes, see TBinaryXml.ReadCardinal)
  ssInteger      =  6; // Integer (1..N bytes)
  ssDecimal      =  7; // decimal value (see TNativeXml.EncodeDecimalSymbol)
  ssDate         =  8; // date (see TNativeXml.EncodeDateSymbol)
  ssTime         =  9; // time (see TNativeXml.EncodeTimeSymbol)
  ssDateTime     = 10; // datetime (see TNativeXml.EncodeDateTimeSymbol)

  // These were the default symbol styles as used by NativeXml. Other units may
  // define more symbols after the last default symbol

type
  // A symbol table, holding a collection of unique strings, sorted in 2 ways
  // for fast access. Strings can be added with AddString or AddStringRec.
  // When a string is added or updated, an ID is returned which the application
  // can use to retrieve the string, using GetString.
  TsdSymbolTable = class(TDebugComponent)
  private
    FByID: TObjectList;
    FBySymbol: TObjectList;
    FPluralSymbolCount: Integer;
    function GetSymbolCount: Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Clear the string table
    procedure Clear;

    // Add a potentially new string S to the table, the function
    // returns its string ID.
    function AddString(const S: Utf8String): Integer;

    // retrieve the string based on its string ID. The string ID is only unique
    // within this string table, so do not use IDs from other tables.
    function GetString(ID: Integer): Utf8String;

    // total number of symbols in the table
    property SymbolCount: Integer read GetSymbolCount;

    // plural symbols in the table. plural symbols are symbols that have
    // a frequency > 1. ie the symbol is found more than once in the app.
    // PluralCount is only valid after method SortByFrequency.
    property PluralSymbolCount: Integer read FPluralSymbolCount;


    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(S: TStream);
    function LoadSymbol(S: TStream): Cardinal;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(S: TStream; ACount: Integer);
    procedure SaveSymbol(S: TStream; ASymbolID: Cardinal);

    procedure ClearFrequency;
    procedure IncrementFrequency(ID: Integer);
    procedure SortByFrequency(var ANewIDs: array of Cardinal);
  end;

{utility functions}

// compare two bytes
function sdCompareByte(Byte1, Byte2: Byte): Integer;

// compare two integers
function sdCompareInteger(Int1, Int2: Integer): Integer;

// unicode UTF8 <> UTF16LE coversion functions
function sdUtf16ToUtf8Mem(Src: Pword; Dst: Pbyte; Count: Integer): Integer;
function sdUtf8ToUtf16Mem(var Src: Pbyte; Dst: Pword; Count: Integer): Integer;

// stream methods
function sdStreamReadCardinal(S: TStream): Cardinal;
function sdStreamReadString(S: TStream; ACharCount: Cardinal): Utf8String;
procedure sdStreamWriteCardinal(S: TStream; ACardinal: Cardinal);
procedure sdStreamWriteString(S: TStream; const AString: Utf8String);

implementation

type

  // A symbol item used in symbol lists (do not use directly)
  TsdSymbol = class
  private
    FID: Integer;
    FFreq: Cardinal;
    FSymbolStyle: Cardinal;
    FFirst: Pbyte;
    FCharCount: Integer;
  public
    destructor Destroy; override;
    function AsString: Utf8String;
    property SymbolStyle: Cardinal read FSymbolStyle;
    property CharCount: Integer read FCharCount;
  end;

  // A list of symbols (do not use directly)
  TsdSymbolList = class(TObjectList)
  private
    function GetItems(Index: Integer): TsdSymbol;
  protected
    // Assumes list is sorted by refstring
    function Find(ASymbol: TsdSymbol; var Index: Integer): boolean;
  public
    property Items[Index: Integer]: TsdSymbol read GetItems; default;
  end;


// compare two symbols. This is NOT an alphabetic compare. symbols are first
// compared by length, then by first Byte, then last Byte then second, then
// N-1, until all bytes are compared.
function sdCompareSymbol(Symbol1, Symbol2: TsdSymbol): Integer;
var
  CharCount: Integer;
  First1, First2, Last1, Last2: Pbyte;
  IsEqual: boolean;
begin
  // Compare string length first
  Result := sdCompareInteger(Symbol1.CharCount, Symbol2.CharCount);
  if Result <> 0 then
    Exit;

  // Compare FFirst
  Result := sdCompareByte(Symbol1.FFirst^, Symbol2.FFirst^);
  if Result <> 0 then
    Exit;

  // CharCount of RS1 (and RS2, since they are equal)
  CharCount := Symbol1.CharCount;

  // Setup First & Last pointers
  First1 := Symbol1.FFirst;
  First2 := Symbol2.FFirst;

  // compare memory (boolean op). CompareMem might have optimized code depending
  // on memory manager (ASM, MMX, SSE etc) to binary compare the block.
  // Since sdCompareRefString may be used to compare relatively large blocks of
  // text, which are often exact copies, using CompareMem before special comparison
  // is warrented.
  IsEqual := CompareMem(First1, First2, CharCount);
  if IsEqual then
  begin
    Result := 0;
    Exit;
  end;

  // finally the special conparison: Compare each time last ptrs then first ptrs,
  // until they meet in the middle
  Last1 := First1;
  Inc(Last1, CharCount);
  Last2 := First2;
  Inc(Last2, CharCount);

  repeat

    dec(Last1);
    dec(Last2);
    if First1 = Last1 then
      Exit;

    Result := sdCompareByte(Last1^, Last2^);
    if Result <> 0 then
      Exit;

    Inc(First1);
    Inc(First2);
    if First1 = Last1 then
      Exit;

    Result := sdCompareByte(First1^, First2^);
    if Result <> 0 then
      Exit;

  until False;
end;

{ TsdSymbol }

function TsdSymbol.AsString: Utf8String;
begin
  SetString(Result, PAnsiChar(FFirst), FCharCount);
end;

destructor TsdSymbol.Destroy;
begin
  FreeMem(FFirst);
  inherited;
end;

{ TsdSymbolList }

function TsdSymbolList.GetItems(Index: Integer): TsdSymbol;
begin
  Result := Get(Index);
end;

function TsdSymbolList.Find(ASymbol: TsdSymbol; var Index: Integer): boolean;
var
  AMin, AMax: Integer;
begin
  Result := False;

  // Find position - binary method
  AMin := 0;
  AMax := Count;
  while AMin < AMax do
  begin
    Index := (AMin + AMax) div 2;
    case sdCompareSymbol(Items[Index], ASymbol) of
    -1: AMin := Index + 1;
     0: begin
          Result := True;
          Exit;
        end;
     1: AMax := Index;
    end;
  end;
  Index := AMin;
end;

{ TsdSymbolTable }

function TsdSymbolTable.AddString(const S: Utf8String): Integer;
var
  Found: boolean;
  L, BySymbolIndex: Integer;
  ASymbol, Item: TsdSymbol;
begin
  BySymbolIndex := 0;
  Result := 0;
  L := length(S);

  // zero-length string
  if L = 0 then
    Exit;

  ASymbol := TsdSymbol.Create;
  try
    ASymbol.FFirst := PByte(@S[1]);
    ASymbol.FCharCount := L;

    // Try to find the new string
    Found := TsdSymbolList(FBySymbol).Find(ASymbol, BySymbolIndex);
    if Found then
    begin
      // yes it is found
      Item := TsdSymbol(FBySymbol[BySymbolIndex]);
      Result := Item.FID;
      Exit;
    end;

    // Not found.. must make new item
    Item := TsdSymbol.Create;
    Item.FCharCount := ASymbol.FCharCount;

    // reallocate memory and copy the string data
    ReallocMem(Item.FFirst, Item.FCharCount);
    Move(S[1], Item.FFirst^, Item.FCharCount);

    // add to the ByID objectlist
    FByID.Add(Item);
    Item.FID := FByID.Count;
    Result := Item.FID;

    // insert into the ByRS list
    FBySymbol.Insert(BySymbolIndex, Item);

  finally
    // this ensures we do not deallocate the memory that may be in use elsewhere
    ASymbol.FFirst := nil;
    ASymbol.Free;
  end;

end;

procedure TsdSymbolTable.Clear;
begin
  FByID.Clear;
  FBySymbol.Clear;
end;

procedure TsdSymbolTable.ClearFrequency;
var
  i: Integer;
begin
  for i := 0 to FByID.Count - 1 do
    TsdSymbol(FByID[i]).FFreq := 0;
end;

constructor TsdSymbolTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FByID := TObjectList.Create(True);
  FBySymbol := TsdSymbolList.Create(False);
end;

destructor TsdSymbolTable.Destroy;
begin
  FreeAndNil(FBySymbol);
  FreeAndNil(FByID);
  inherited;
end;

function TsdSymbolTable.GetSymbolCount: Integer;
begin
  Result := FByID.Count;
end;

function TsdSymbolTable.GetString(ID: Integer): Utf8String;
begin
  // Find the ID

  // zero string
  if ID <= 0 then
  begin
    Result := '';
    Exit;
  end;

  // out of bounds?
  if ID > FByID.Count then
  begin
    // output warning
    DoDebugOut(Self, wsWarn, 'string ID not found');
    Result := '';
  end;

  Result := TsdSymbol(FByID[ID - 1]).AsString;
end;

procedure TsdSymbolTable.IncrementFrequency(ID: Integer);
var
  RS: TsdSymbol;
begin
  RS := TsdSymbol(FByID[ID - 1]);
  Inc(RS.FFreq);
end;

procedure TsdSymbolTable.LoadFromFile(const AFileName: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(AFileName);
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TsdSymbolTable.LoadFromStream(S: TStream);
var
  i: Integer;
  TableCount: Cardinal;
begin
  Clear;

//  DoDebugOut(Self, wsInfo, format('stream position: %d', [S.Position]));

  // table count
  TableCount := sdStreamReadCardinal(S);
  if TableCount = 0 then
    Exit;

  for i := 0 to TableCount - 1 do
  begin
    LoadSymbol(S);
  end;
end;

function TsdSymbolTable.LoadSymbol(S: TStream): Cardinal;
var
  Symbol: TsdSymbol;
  BySymbolIndex: Integer;
  Found: boolean;
begin
  BySymbolIndex := 0;
  Symbol := TsdSymbol.Create;

  // For now, we just use ssString uniquely as symbol style,.
  // In updates, different symbol styles can be added.
  Symbol.FSymbolStyle := sdStreamReadCardinal(S);

  Symbol.FCharCount := sdStreamReadCardinal(S);

  if Symbol.FCharCount > 0 then
  begin
    // reallocate memory and copy the string data
    ReallocMem(Symbol.FFirst, Symbol.FCharCount);
    S.Read(Symbol.FFirst^, Symbol.FCharCount);
  end;

  // add to the ByID objectlist
  FByID.Add(Symbol);
  Symbol.FID := FByID.Count;
  Result := Symbol.FID;

  // find the symbol
  Found := TsdSymbolList(FBySymbol).Find(Symbol, BySymbolIndex);
  if Found then
  begin
    DoDebugOut(Self, wsFail, 'duplicate symbol!');
    Exit;
  end;

  // insert into the ByRS list
  FBySymbol.Insert(BySymbolIndex, Symbol);
end;

procedure TsdSymbolTable.SaveToFile(const AFileName: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S, SymbolCount);
    S.SaveToFile(AFileName);
  finally
    S.Free;
  end;
end;

procedure TsdSymbolTable.SaveToStream(S: TStream; ACount: Integer);
var
  i: Integer;
begin
  // write (part of the) symbol table
  sdStreamWriteCardinal(S, ACount);
  for i := 0 to ACount - 1 do
  begin
    SaveSymbol(S, i + 1);
  end;
end;

procedure TsdSymbolTable.SaveSymbol(S: TStream; ASymbolID: Cardinal);
var
  RS: TsdSymbol;
  StringVal: Utf8String;
  CharCount: Cardinal;
begin
  if ASymbolID <= 0 then
    DoDebugOut(Self, wsFail, 'symbol ID <= 0');
  RS := TsdSymbol(FByID[ASymbolID - 1]);

  // For now, we just use ssString uniquely as symbol style.
  // In updates, different symbol styles can be added.
  sdStreamWriteCardinal(S, RS.SymbolStyle);

  StringVal := RS.AsString;
  CharCount := length(StringVal);
  sdStreamWriteCardinal(S, CharCount);
  sdStreamWriteString(S, StringVal);
end;

procedure TsdSymbolTable.SortByFrequency(var ANewIDs: array of Cardinal);
  // local
  function CompareFreq(Pos1, Pos2: Integer): Integer;
  var
    RS1, RS2: TsdSymbol;
  begin
    RS1 := TsdSymbol(FByID[Pos1]);
    RS2 := TsdSymbol(FByID[Pos2]);
    if RS1.FFreq > RS2.FFreq then
      Result := -1
    else
      if RS1.FFreq < RS2.FFreq then
        Result := 1
      else
        Result := 0;
  end;
  // local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid:= (Lo + Hi) div 2;
    repeat
      while CompareFreq(Lo, Mid) < 0 do
        Inc(Lo);
      while CompareFreq(Hi, Mid) > 0 do
        Dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        FByID.Exchange(Lo, Hi);
        if Mid = Lo then
          Mid := Hi
        else
          if Mid = Hi then
            Mid := Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    if Hi > iLo then
      QuickSort(iLo, Hi);

    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;
// main
var
  i: Integer;
begin
  // sort by frequency
  QuickSort(0, FByID.Count - 1);

  // plural count
  FPluralSymbolCount := 0;
  i := 0;
  while i < FByID.Count do
  begin
    if TsdSymbol(FByID[i]).FFreq >= 2 then
      Inc(FPluralSymbolCount)
    else
      break;
    Inc(i);
  end;

  // tell app about new ID
  for i := 0 to FByID.Count - 1 do
  begin
    ANewIDs[TsdSymbol(FByID[i]).FID] := i + 1;
  end;

  // then rename IDs
  for i := 0 to FByID.Count - 1 do
  begin
    TsdSymbol(FByID[i]).FID := i + 1;
  end;
end;

{utility functions}

function sdCompareByte(Byte1, Byte2: Byte): Integer;
begin
  if Byte1 < Byte2 then
    Result := -1
  else
    if Byte1 > Byte2 then
      Result := 1
    else
      Result := 0;
end;

function sdCompareInteger(Int1, Int2: Integer): Integer;
begin
  if Int1 < Int2 then
    Result := -1
  else
    if Int1 > Int2 then
      Result := 1
    else
      Result := 0;
end;

function sdUtf16ToUtf8Mem(Src: Pword; Dst: Pbyte; Count: Integer): Integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
var
  W: Word;
  DStart: PByte;
begin
  DStart := Dst;
  while Count > 0 do
  begin
    W := Src^;
    Inc(Src);
    if W <= $7F then
    begin
      Dst^ := Byte(W);
      Inc(Dst);
    end else
    begin
      if W > $7FF then
      begin
        Dst^ := Byte($E0 or (W shr 12));
        Inc(Dst);
        Dst^ := Byte($80 or ((W shr 6) and $3F));
        Inc(Dst);
        Dst^ := Byte($80 or (W and $3F));
        Inc(Dst);
      end else
      begin //  $7F < W <= $7FF
        Dst^ := Byte($C0 or (W shr 6));
        Inc(Dst);
        Dst^ := Byte($80 or (W and $3F));
        Inc(Dst);
      end;
    end;
    Dec(Count);
  end;
  //Result := Byte(Dst) - Byte(DStart);
  Result := Byte(Dst - DStart);
end;

function sdUtf8ToUtf16Mem(var Src: Pbyte; Dst: Pword; Count: Integer): Integer;
// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one Byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: Word;
  C: Byte;
  DStart: PWord;
  SClose: PByte;
begin
  DStart := Dst;
  SClose := Src;
  Inc(SClose, Count);
  while Src < SClose do
  begin
    // 1st Byte
    W := Src^;
    Inc(Src);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd Byte
        C := Src^;
        Inc(Src);
        if C and $C0 <> $80 then
          // malformed trail Byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd Byte
      C := Src^;
      Inc(Src);
      if C and $C0 <> $80 then
        // malformed trail Byte
        Continue;
      Dst^ := (W shl 6) or (C and $3F);
      Inc(Dst);
    end else
    begin
      Dst^ := W;
      Inc(Dst);
    end;
  end;
  Result := NativeUInt(Dst - DStart) div 2;
end;

{ stream methods }

function sdStreamReadCardinal(S: TStream): Cardinal;
var
  C: Byte;
  Bits: Integer;
begin
  C := 0;
  Result := 0;
  Bits := 0;
  repeat
    S.Read(C, 1);
    if C > 0 then
    begin
      Inc(Result, (C and $7F) shl Bits);
      Inc(Bits, 7)
    end;
  until(C and $80) = 0;
end;

function sdStreamReadString(S: TStream; ACharCount: Cardinal): Utf8String;
begin
  SetLength(Result, ACharCount);
  if ACharCount = 0 then
    Exit;
  S.Read(Result[1], ACharCount);
end;

procedure sdStreamWriteCardinal(S: TStream; ACardinal: Cardinal);
var
  C: Byte;
begin
  repeat
    if ACardinal <= $7F then
    begin
      C := ACardinal;
      S.Write(C, 1);
      Exit;
    end else
      C := (ACardinal and $7F) or $80;
    S.Write(C, 1);
    ACardinal := ACardinal shr 7;
  until ACardinal = 0;
end;

procedure sdStreamWriteString(S: TStream; const AString: Utf8String);
var
  L: Integer;
begin
  L := Length(AString);
  if L > 0 then
  begin
    S.Write(AString[1], L);
  end;
end;

end.
