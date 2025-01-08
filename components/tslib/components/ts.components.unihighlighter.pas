{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Components.UNIHighlighter;

{ This unit is based on the Synedit Universal Highlighter by Kirill Burtsev,
  Vitaly Nevzorov and Flávio Etrusco.
  Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net

  Tim Sinaeve:
  - removed AV risks
  - consistent naming
  - proper classes (no public fields, but properties)
  - records holding objects to classes
  - TList => TObjectList
  - removed useless code and type declarations
}

{$REGION 'documentation'}
{
(info taken from http://wiki.lazarus.freepascal.org/SynEdit_Highlighter)

General remarks on SynEdit and how it relates to highlighter instances.
-------------------------------------------------------------------------------

SynEdit <-> Highlighter have a n to 1 relationship.
  - 1 (instance of a) Highlighter can serve n (many) (instances of) SynEdits
  - Each SynEdit only has one Highlighter
  - But: one text (text-buffer) can have many highlighters, if shared by several
    SynEdit (each SynEdit will have one HL, but all HL will work on the same
    document)

As a result of this:
 - no Highlighter Instance has a (fixed) reference to the SynEdit.
   (Highlighters however keep a list of SynEditTextBuffers to which they are
   attached)
 - All data for the Highlighter is (and must be) stored on the SynEdit (actually
   on the TextBuffer of SynEdit (referred to as "Lines").

However SynEdit ensures before each call to the Highlighter that
Highlighter.CurrentLines is set to the current SynEdits Lines. This way the
highlighter can access the data whenever needed. The Format of the data-storage
is determined by the highlighter (TSynCustomHighlighter.AttachToLines)

Scanning and Returning Highlight attributes
-------------------------------------------------------------------------------

The Highlighter is expected to work on a per Line base.
If any text was modified, SynEdit will call (TSynCustomHighlighter.ScanFrom /
Currently called from TSynEdit.ScanFrom) with the line range. The Highlighter
should know the state of the previous line.

If Highlight attributes are required SynEdit will request them per Line too.
SynEdit will loop through individual tokens on a line. This currently happens
from nested proc PaintLines in SynEdit.PaintTextLines. It calls
TSynCustomHighlighter.StartAtLineIndex, followed by
HL.GetTokenEx/HL.GetTokenAttribute for as long as HL.GetEol is false.

Also the BaseClass for the Highlighter's data (see AttachToLines) is based on
per line storage, and SynEdit's TextBuffer (Lines) do maintenance on this data
to keep it synchronized. That is when ever lines of text are inserted or
removed, so are entries inserted or removed from the highlighters data (hence
it must have one entry per line).

Usually Highlighters store the end-of-line-status in this field. So if the
highlighter is going to work on a line, it will continue with the state-entry
from the previous line.

Folding
-------------------------------------------------------------------------------

SynEdit's folding is handled by unit SynEditFoldedView and SynGutterCodeFolding.
Highlighters that implement folding are to be based on TSynCustomFoldHighlighter.

The basic information for communication between SynEditFoldedView and the HL
requires 2 values stored for each line. (Of course the highlighter itself can
store more information):

  - FoldLevel at the end of line
  - Minimum FoldLevel encountered anywhere on the line

The Foldlevel indicates how many (nested) folds exist. It goes up whenever a
fold begins, and down when a fold ends.

example:

                           EndLvl   MinLvl
 procedure a;               1 -      0
 Begin                      2 --     1 -
   b:= 1;                   2 --     2 --
   if c > b then begin      3 ---    2 --
     c:=b;                  3 ---    3 ---
   end else begin           3 ---    2 --
     b:=c;                  3 ---    3 ---
   end;                     2 --     2 --
 end;                       0        0  // The end closes both: begin and procedure fold

}
{$ENDREGION}

interface

uses
  SysUtils, Graphics, Classes, FileUtil, SynEditTypes, Contnrs,

  SynEditHighlighter;

const
  ROOT_NAME      = 'Root';
  NEW_RANGE_NAME = 'New';

type
  TVersionType = (
    vtInternalTest,
    vtBeta,
    vtRelease
  );

  TAuthorInfo = record
    Name      : string;
    Email     : string;
    Web       : string;
    Copyright : string;
    Company   : string;
    Remark    : string;
  end;

  TVerInfo = record
    Version     : Integer;
    Revision    : Integer;
    VersionType : TVersionType;
    ReleaseDate : TDateTime;
  end;

  THighInfo = record
    Name         : string;
    FileTypeName : string;
    Layout       : string;
  end;

  TInfo = record
    Author  : TAuthorInfo;
    Version : TVerInfo;
    General : THighInfo;
    History : TStringList;
    Sample  : TStringList;
  end;

  TSynRange = class;

  TSymbolBreakType = (
    btUnspecified,
    btAny,
    btTerm
  );

  TSynSymbol = class(TPersistent)
  strict private
    FAttributes : TSynHighlighterAttributes;
    FOpenRule   : TSynRange;
    FBreakType  : TSymbolBreakType;
    FSymbol     : string;

  public
    constructor Create(
      const ASymbol     : string;
            AAttributes : TSynHighlighterAttributes
    ); virtual;

    property Symbol: string
      read FSymbol write FSymbol;

    property OpenRule: TSynRange
      read FOpenRule write FOpenRule;

    property BreakType : TSymbolBreakType
      read FBreakType write FBreakType;

    property Attributes: TSynHighlighterAttributes
      read FAttributes write FAttributes;
  end;

  TSynSymbolGroup = class(TPersistent)
  private
    FAttributes : TSynHighlighterAttributes;
    FKeywords   : TStringList;
    FGroupName  : string;
    FName       : string;

  public
    constructor Create(
      const AKeywords   : string;
            AAttributes : TSynHighlighterAttributes
    );
    destructor Destroy; override;

    property Name: string
      read FName write FName;

    property GroupName: string
      read FGroupName write FGroupName;

    property Attributes: TSynHighlighterAttributes
      read FAttributes;

    property Keywords: TStringList
      read FKeywords;
  end;

  TSymbolsSet   = set of Char;

  TSymbolList = class;

  { TSymbolNode }

  TSymbolNode = class
  strict private
    FSymbolChar  : Char;
    FBreakType   : TSymbolBreakType;
    FNextSymbols : TSymbolList;
    FSynSymbol   : TSynSymbol;

  public
    constructor Create(
      ASymbolChar : Char;
      ASynSymbol  : TSynSymbol;
      ABreakType  : TSymbolBreakType
    ); overload; virtual;
    constructor Create(ASymbolChar: Char); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property SymbolChar: Char
      read FSymbolChar;

    property NextSymbols: TSymbolList
      read FNextSymbols;

    property BreakType: TSymbolBreakType
      read FBreakType write FBreakType;

    property SynSymbol: TSynSymbol
      read FSynSymbol write FSynSymbol;
  end;

  { TSymbolList }

  TSymbolList = class
  strict private
    FSymbolList: TObjectList;

  public
    function FindSymbol(AChar: Char): TSymbolNode;
    procedure AddSymbol(ASymbolNode: TSymbolNode);
    procedure SetSymbolNode(AIndex: Integer; AValue: TSymbolNode);
    function GetSymbolNode(AIndex: Integer): TSymbolNode;
    function GetCount: Integer;

    property Nodes[AIndex: Integer]: TSymbolNode
      read GetSymbolNode write SetSymbolNode;

    property Count: Integer
      read GetCount;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TSynUniSyn = class;

  TAbstractSymbols = class
  public
    function GetToken(
          AParser    : TSynUniSyn;
      var ASynSymbol : TSynSymbol
    ): Boolean; virtual; abstract;
  end;

  TSymbols = class(TAbstractSymbols)
  strict private
    FHeadNode: TSymbolNode;

  public
    constructor Create(AChar: Char; ASynSymbol: TSynSymbol;
      ABreakType: TSymbolBreakType);
      reintroduce; virtual;
    procedure BeforeDestruction; override;

    function GetToken(
          AParser    : TSynUniSyn;
      var ASynSymbol : TSynSymbol
    ): Boolean; override;

    procedure AddSymbol(const AString: string; ASynSymbol: TSynSymbol;
      ABreakType: TSymbolBreakType);
    function FindSymbol(const AString: string): TSymbolNode;

    property HeadNode: TSymbolNode
      read FHeadNode;
  end;

  TDefaultSymbols = class(TAbstractSymbols)
  strict private
    FSynSymbol: TSynSymbol;

  public
    constructor Create(ASynSymbol: TSynSymbol); reintroduce; virtual;
    destructor Destroy; override;

    function GetToken(AParser: TSynUniSyn; var ASynSymbol: TSynSymbol): Boolean; override;
  end;

  TDefaultTermSymbols = class(TAbstractSymbols)
  strict private
    FSynSymbol: TSynSymbol;
  public
    constructor Create(ASynSymbol: TSynSymbol); virtual;
    function GetToken(AParser: TSynUniSyn;
      var ASynSymbol: TSynSymbol): Boolean; override;
    destructor Destroy; override;
  end;

  TNumberSymbols = class(TAbstractSymbols)
  strict private
    FSynSymbol: TSynSymbol;
  public
    constructor Create(ASynSymbol: TSynSymbol); virtual;
    function GetToken(AParser: TSynUniSyn;
      var ASynSymbol: TSynSymbol): Boolean; override;
    destructor Destroy; override;
  end;

  TClosingSymbolSet = record
    Symbol: TSynSymbol;
    AllowPredClose: Boolean;
  end;

  PClosingSymbolSet = ^TClosingSymbolSet;

  TAbstractSymbolList = array [Char] of TAbstractSymbols;

  { TSynRange }

  TSynRange = class(TPersistent)
  strict private
    FCloseSymbol  : TSynSymbol;
    FOpenSymbol   : TSynSymbol;
    FCloseOnTerm  : Boolean;
    FCloseOnEol   : Boolean;
    FCaseSensitive: Boolean;
    FOwner        : TSynRange;
    FClosingSymbol: TClosingSymbolSet;

    FSynSymbols  : TObjectList;
    FSynRanges   : TObjectList;
    FSymbolGroups: TObjectList;

    FDefaultSynSymbol : TSynSymbol;
    FNumberSymbol     : TNumberSymbols;
    FDefaultSymbols   : TDefaultSymbols;
    FDefaultTermSymbol: TDefaultTermSymbols;

    FDefaultAttributes: TSynHighlighterAttributes;
    FNumberAttributes : TSynHighlighterAttributes;
    FAttributes       : TObjectList;

    FTermSymbols : TSymbolsSet;
    FSymbolList  : TAbstractSymbolList;
    FPrepared    : Boolean;
    FName        : string;

    function GetSynSymbol(Index: Integer): TSynSymbol;
    function GetSynRange(Index: Integer): TSynRange;
    function GetSynSymbolGroup(Index: Integer): TSynSymbolGroup;
    function GetRangeCount: Integer;
    function GetSymbolCount: Integer;
    function GetSymbolGroupCount: Integer;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);

  protected
    CaseFunct       : function(c: Char) : Char;
    StringCaseFunct : function(const s: string): string;

  public
    constructor Create(
      const AOpenSymbs  : string = '';
      const ACloseSymbs : string = ''
    ); virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure AddSymbolGroup(ASymbolGroup: TSynSymbolGroup);
    procedure AddSymbol(ASymbol: TSynSymbol);
    procedure AddRange(ARange: TSynRange);
    function GetSymbol(const AString: string): TSynSymbol;
    function FindSymbol(const AString: string): TSynSymbol;
    function FindSymbolOwner(ASymbol: TSynSymbol): TSynSymbolGroup;

    procedure DeleteRange(AIndex: Integer); overload;
    procedure DeleteRange(ASynRange: TSynRange); overload;
    procedure DeleteSymbolGroup(AIndex: Integer); overload;
    procedure DeleteSymbolGroup(ASymbolGroup: TSynSymbolGroup); overload;
    function AddNewAttribs(AName: string): TSynHighlighterAttributes;
    function AttribsByName(AName: string): TSynHighlighterAttributes;
    function AddAttribs(AAttributes: TSynHighlighterAttributes): Integer;
    procedure DeleteAttribs(AIndex: Integer); overload;
    procedure DeleteAttribs(AName: string); overload;

    procedure Prepare(AOwner: TSynRange);
    procedure Reset;
    procedure Clear;

    procedure LoadFromStream(AStream: TStream);

  public
    property TermSymbols: TSymbolsSet
      read FTermSymbols write FTermSymbols;

    property Symbols[Index: Integer]: TSynSymbol
      read GetSynSymbol;

    property SymbolGroups[Index: Integer]: TSynSymbolGroup
      read GetSynSymbolGroup;

    property SymbolList: TAbstractSymbolList
      read FSymbolList;

    property Ranges[Index: Integer]: TSynRange
      read GetSynRange;

    property ClosingSymbol: TClosingSymbolSet
      read FClosingSymbol;

    property OpenSymbol : TSynSymbol
      read FOpenSymbol;

    property CloseSymbol: TSynSymbol
      read FCloseSymbol;

    property CloseOnTerm: Boolean
      read FCloseOnTerm write FCloseOnTerm;

    property CloseOnEol : Boolean
      read FCloseOnEol write FCloseOnEol;

    property RangeCount : Integer
      read GetRangeCount;

    property SymbolCount: Integer
      read GetSymbolCount;

    property SymbolGroupCount: Integer
      read GetSymbolGroupCount;

    property NumberAttributes : TSynHighlighterAttributes
      read FNumberAttributes;

    property DefaultAttributes: TSynHighlighterAttributes
      read FDefaultAttributes;

    property CaseSensitive: Boolean
      read GetCaseSensitive write SetCaseSensitive;

    property Prepared: Boolean
      read FPrepared;

    property Owner: TSynRange
      read FOwner;

    property Name: string
      read FName write FName;

    property DefaultSynSymbol: TSynSymbol
      read FDefaultSynSymbol;
  end;

  TSynUniSyn = class(TSynCustomHighlighter)
  strict private
    FMainRules : TSynRange;
    FEol       : Boolean;
    FPrEol     : Boolean;
    FTrueLine  : PChar;
    FUpLine    : string;
    FLine      : PChar;
    FLineNumber: Integer;
    FRun       : Integer;
    FTokenPos  : Integer;
    FInfo      : TInfo;
    FCurrToken : TSynSymbol;

    FCurrentRule : TSynRange;
    FSymbols     : TSymbols;
    FSymbolList  : TAbstractSymbolList;

    FPrepared: Boolean;

    procedure ReadSyntax(Reader: TReader);
    procedure WriteSyntax(Writer: TWriter);

    procedure SpaceProc;
    procedure NullProc;

  protected
    function GetIdentChars: TSynIdentChars; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;

    property Line: PChar
      read FLine;

    property Run: Integer
      read FRun write FRun;

    property CurrentRule: TSynRange
      read FCurrentRule;

  public
    class function GetLanguageName: string; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;

    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
      override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: Integer;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): Boolean;
      override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(
      const NewValue   : string;
            LineNumber : Integer
      ); override;
    procedure SetRange(AValue: Pointer); override;
    procedure Reset;
    procedure Clear;
    procedure Prepare;
    procedure CreateStandardRules;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);

  public
    property Info: TInfo
      read FInfo;

    property SymbolList: TAbstractSymbolList
      read FSymbolList;

    property MainRules: TSynRange
      read FMainRules write FMainRules;

  published
    property Enabled default False;

  end;

  TNodeType = (
    ntRange,
    ntRootRange,
    ntKeyWords,
    ntNone
  );

const
  DefaultTermSymbols: TSymbolsSet =
    ['*', '/', '+', '-', '=', '\', '|', '&', '(', ')',
    '[', ']', '{', '}', '`', '~', '!', '@', ',', '$', '%', '^',
    '?', ':', ';', '''', '"', '.',
    '>', '<', '#'];

implementation

uses
  SynEditStrConst;

const
  AbsoluteTermSymbols: TSymbolsSet = [' ', #13, #0, #10];

  xitAnyTerm        = 0;
  xitAttri          = 1;
  xitAuthor         = 2;
  xitBack           = 3;
  xitCaseSensitive  = 4;
  xitCloseOnEol     = 5;
  xitCloseOnTerm    = 6;
  xitCloseSymbol    = 7;
  xitCompany        = 8;
  xitCopyright      = 9;
  xitDate           = 10;
  xitDef            = 11;
  xitDelimiterChars = 12;
  xitEmail          = 13;
  xitFileTypeName   = 14;
  xitFore           = 15;
  xitGeneral        = 16;
  xitH              = 17;
  xitHistory        = 18;
  xitInfo           = 19;
  xitKW             = 20;
  xitLayout         = 21;
  xitName           = 22;
  xitNum            = 23;
  xitOpenSymbol     = 24;
  xitRange          = 25;
  xitRemark         = 26;
  xitRevision       = 27;
  xitS              = 28;
  xitSample         = 29;
  xitStyle          = 30;
  xitType           = 31;
  xitUniHighlighter = 32;
  xitVersion        = 33;
  xitW              = 34;
  xitWeb            = 35;

procedure BuildXMLIndexes(AStrings: TStringList);
begin
  AStrings.Add('AnyTerm');
  AStrings.Add('Attri');
  AStrings.Add('Author');
  AStrings.Add('Back');
  AStrings.Add('CaseSensitive');
  AStrings.Add('CloseOnEol');
  AStrings.Add('CloseOnTerm');
  AStrings.Add('CloseSymbol');
  AStrings.Add('Company');
  AStrings.Add('Copyright');
  AStrings.Add('Date');
  AStrings.Add('Def');
  AStrings.Add('DelimiterChars');
  AStrings.Add('Email');
  AStrings.Add('FileTypeName');
  AStrings.Add('Fore');
  AStrings.Add('General');
  AStrings.Add('H');
  AStrings.Add('History');
  AStrings.Add('Info');
  AStrings.Add('KW');
  AStrings.Add('Layout');
  AStrings.Add('Name');
  AStrings.Add('Num');
  AStrings.Add('OpenSymbol');
  AStrings.Add('Range');
  AStrings.Add('Remark');
  AStrings.Add('Revision');
  AStrings.Add('S');
  AStrings.Add('Sample');
  AStrings.Add('Style');
  AStrings.Add('Type');
  AStrings.Add('UniHighlighter');
  AStrings.Add('Version');
  AStrings.Add('W');
  AStrings.Add('Web');
end;

function String2Set(AString: string): TSymbolsSet;
var
  I: Integer;
begin
  Result := [];
  for I := 1 to Length(AString) do
    Result := Result + [AString[I]];
end;

function Set2String(ASymbolsSet: TSymbolsSet): string;
var
  B: Byte;
begin
  Result := '';
  for B := 1 to 255 do
    if (chr(B) in ASymbolsSet) and (not(chr(B) in AbsoluteTermSymbols)) then
      Result := Result + chr(B);
end;

function String2Fs(AString: string): TFontStyles;
begin
  Result := [];
  if Pos('B', AString) > 0 then
    Include(Result, fsBold);
  if Pos('I', AString) > 0 then
    Include(Result, fsItalic);
  if Pos('U', AString) > 0 then
    Include(Result, fsUnderline);
  if Pos('S', AString) > 0 then
    Include(Result, fsStrikeOut);
end;

function Fs2String(AStyle: TFontStyles): string;
begin
  Result := '';
  if fsBold in AStyle then
    Result := Result + 'B';
  if fsItalic in AStyle then
    Result := Result + 'I';
  if fsUnderline in AStyle then
    Result := Result + 'U';
  if fsStrikeOut in AStyle then
    Result := Result + 'S';
end;

function CaseNone(c: Char): Char;
begin
  Result := c;
end;

function StringCaseNone(const s: string): string;
begin
  Result := s;
end;

{$REGION 'TSynSymbolGroup'}
constructor TSynSymbolGroup.Create(const AKeywords: string; AAttributes: TSynHighlighterAttributes);
begin
  FAttributes          := AAttributes;
  FKeywords            := TStringList.Create;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.Sorted     := True;
  FKeywords.Text       := AKeywords;
end;

destructor TSynSymbolGroup.Destroy;
begin
  FKeywords.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'TSynSymbol'}
constructor TSynSymbol.Create(const ASymbol: string; AAttributes: TSynHighlighterAttributes);
begin
  FAttributes := AAttributes;
  FSymbol     := ASymbol;
  FOpenRule   := nil;
  FBreakType  := btUnspecified;
end;
{$ENDREGION}

{$REGION 'TSynRange'}
procedure TSynRange.AddRange(ARange: TSynRange);
begin
  FSynRanges.Add(ARange);
end;

procedure TSynRange.AddSymbol(ASymbol: TSynSymbol);
var
  SS: TSynSymbol;
begin
  SS := FindSymbol(ASymbol.Symbol);
  if SS <> nil then
  begin
    FSynSymbols.Remove(SS);
//    SS.Free;
  end;
  FSynSymbols.Add(ASymbol);
end;

procedure TSynRange.AddSymbolGroup(ASymbolGroup: TSynSymbolGroup);
begin
  FSymbolGroups.Add(ASymbolGroup);
end;

constructor TSynRange.Create(const AOpenSymbs: string; const ACloseSymbs: string);
begin
  FOpenSymbol := TSynSymbol.Create(AOpenSymbs, nil);
  FCloseSymbol := TSynSymbol.Create(ACloseSymbs, nil);

  FDefaultAttributes := TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage);
  FNumberAttributes := TSynHighlighterAttributes.Create(SYNS_AttrNumber);

  FillChar(FSymbolList, sizeof(FSymbolList), 0);
  CaseFunct := @CaseNone;
  StringCaseFunct := @StringCaseNone;

  FPrepared := False;
  FCloseOnTerm := False;
  FCloseOnEol := False;

  FAttributes := TObjectList.Create(True);
  FSymbolGroups := TObjectList.Create(True);
  FSynSymbols := TObjectList.Create(True);
  FSynRanges := TObjectList.Create(True);
  FTermSymbols := DefaultTermSymbols;
end;

procedure TSynRange.AfterConstruction;
begin
  inherited AfterConstruction;
end;

destructor TSynRange.Destroy;
begin
  FOpenSymbol.Free;
  FCloseSymbol.Free;
  FreeAndNil(FDefaultSymbols);
  FreeAndNil(FDefaultSynSymbol);
  FreeAndNil(FDefaultTermSymbol);
  FreeAndNil(FDefaultAttributes);
  FreeAndNil(FNumberAttributes);
  FSymbolGroups.Free;
  FSynSymbols.Free;
  FSynRanges.Free;
  FAttributes.Free;
  inherited;
end;

function TSynRange.FindSymbol(const AString: string): TSynSymbol;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FSynSymbols.Count - 1 do
    if TSynSymbol(FSynSymbols.Items[I]).Symbol = AString then
    begin
      Result := TSynSymbol(FSynSymbols.Items[I]);
      Exit;
    end;
end;

function TSynRange.FindSymbolOwner(ASymbol: TSynSymbol): TSynSymbolGroup;
var
  I: Integer;
  J: Integer;
begin
  Result := nil;
  for I := 0 to FSymbolGroups.Count - 1 do
    if TSynSymbolGroup(FSymbolGroups[I]).FKeywords.Find(
      ASymbol.Symbol, J) then
    begin
      Result := TSynSymbolGroup(FSymbolGroups[I]);
      Exit;
    end;
end;

function TSynRange.GetRangeCount: Integer;
begin
  Result := FSynRanges.Count;
end;

function TSynRange.GetSymbol(const AString: string): TSynSymbol;
begin
  Result := FindSymbol(AString);
end;

function TSynRange.GetSymbolCount: Integer;
begin
  Result := FSynSymbols.Count;
end;

function TSynRange.GetSymbolGroupCount: Integer;
begin
  Result := FSymbolGroups.Count;
end;

function TSynRange.GetSynRange(Index: Integer): TSynRange;
begin
  Result := TSynRange(FSynRanges[Index]);
end;

function TSynRange.GetSynSymbol(Index: Integer): TSynSymbol;
begin
  Result := TSynSymbol(FSynSymbols[Index]);
end;

function TSynRange.GetSynSymbolGroup(Index: Integer): TSynSymbolGroup;
begin
  Result := TSynSymbolGroup(FSymbolGroups[Index]);
end;

procedure TSynRange.Prepare(AOwner: TSynRange);
var
  I, J     : Integer;
  SynSymbol: TSynSymbol;
  S        : string;
  FirstChar: Char;
  BrakeType: TSymbolBreakType;

  procedure SortSymbolList(AList: TObjectList);
  var
    I    : Integer;
    Last : Boolean;
  begin
    Last := False;
    while not Last do
    begin
      Last := True;
      for I := 0 to AList.Count - 2 do
        if TSynSymbol(AList[I]).Symbol > TSynSymbol(AList[I + 1]).Symbol then
        begin
          AList.Exchange(I, I + 1);
          Last := False;
        end;
    end;
  end;

  function SafeInsertSymbol(ASynSymbol: TSynSymbol; ARules: TSynRange;
    AAttributes: TSynHighlighterAttributes): TSynSymbol;
  begin
    Result := ARules.FindSymbol(ASynSymbol.Symbol);
    if Result = nil then
    begin
      Result := TSynSymbol.Create(ASynSymbol.Symbol, ASynSymbol.Attributes);
      Result.BreakType := ASynSymbol.BreakType;
      ARules.AddSymbol(Result);
    end;
    if Result.Attributes = nil then
      Result.Attributes := AAttributes;
  end;

begin
  Reset;
  FOwner := AOwner;

  FDefaultSynSymbol := TSynSymbol.Create('', FDefaultAttributes);
  FDefaultTermSymbol := TDefaultTermSymbols.Create(
    TSynSymbol.Create('', FDefaultAttributes));
  FDefaultSymbols := TDefaultSymbols.Create(
    TSynSymbol.Create('', FDefaultAttributes));
  FNumberSymbol := TNumberSymbols.Create(
    TSynSymbol.Create('', FNumberAttributes));
  FTermSymbols := FTermSymbols + AbsoluteTermSymbols;

  //Add all keywords in Symbol list.
  for I := 0 to FSymbolGroups.Count - 1 do
    for J := 0 to TSynSymbolGroup(FSymbolGroups[I]).FKeywords.Count - 1 do
      AddSymbol(TSynSymbol.Create(TSynSymbolGroup(FSymbolGroups[I]).FKeywords
        [J], TSynSymbolGroup(FSymbolGroups[I]).FAttributes));

  //Assign range opening and closing symbols and Prepare range rules.
  for I := 0 to FSynRanges.Count - 1 do
  begin
    //Assign range opening symbol
    SynSymbol :=
      SafeInsertSymbol(TSynRange(FSynRanges[I]).FOpenSymbol,
      Self, TSynRange(FSynRanges[I]).FDefaultAttributes);
    SynSymbol.OpenRule := TSynRange(FSynRanges[I]);

    //Assing range closing symbols
    SynSymbol := SafeInsertSymbol(TSynRange(FSynRanges[I]).FCloseSymbol,
      TSynRange(FSynRanges[I]), TSynRange(FSynRanges[I]).FDefaultAttributes);
    TSynRange(FSynRanges[I]).FClosingSymbol.Symbol := SynSymbol;

    TSynRange(FSynRanges[I]).Prepare(Self);
  end;

  //Build tokens table

  SortSymbolList(FSynSymbols);
  for I := 0 to FSynSymbols.Count - 1 do
  begin
    SynSymbol := TSynSymbol(FSynSymbols[I]);
    if Length(SynSymbol.Symbol) < 1 then
      continue;
    S := SynSymbol.Symbol;
    FirstChar := S[1];
    if SynSymbol.BreakType <> btUnspecified then
      BrakeType := SynSymbol.BreakType
    else if S[Length(S)] in FTermSymbols then
      BrakeType := btAny
    else
      BrakeType := btTerm;
    if FSymbolList[CaseFunct(FirstChar)] = nil then
    begin
      if Length(S) = 1 then
        FSymbolList[CaseFunct(FirstChar)] :=
          TSymbols.Create(FirstChar, SynSymbol, BrakeType)
      else
      begin
        FSymbolList[CaseFunct(FirstChar)] :=
          TSymbols.Create(FirstChar, FDefaultSynSymbol, BrakeType);
        TSymbols(FSymbolList[CaseFunct(FirstChar)]).AddSymbol(
          StringCaseFunct(copy(S, 2, Length(S) - 1)), SynSymbol, BrakeType);
      end;
    end
    else
    begin
      if Length(S) = 1 then
      else
        TSymbols(FSymbolList[CaseFunct(FirstChar)]).AddSymbol(
          StringCaseFunct(Copy(S, 2, Length(S) - 1)), SynSymbol, BrakeType);
    end;
  end;

  //Fill remaining table
  for I := 0 to 255 do
    if FSymbolList[Char(I)] = nil then
    begin
      if Char(I) in FTermSymbols then
        FSymbolList[Char(I)] := FDefaultTermSymbol
      else if Char(I) in ['0' .. '9'] then
        FSymbolList[Char(I)] := FNumberSymbol
      else
        FSymbolList[Char(I)] := FDefaultSymbols;
    end;

  FPrepared := True;
end;

function TSynRange.GetCaseSensitive: Boolean;
begin
  Result := Pointer(@CaseFunct) = Pointer(@UpCase);
end;

procedure TSynRange.SetCaseSensitive(const Value: Boolean);
begin
  FCaseSensitive:=Value;
  if Value then
  begin
    CaseFunct := @UpCase;
    StringCaseFunct := @UpperCase;
  end
  else
  begin
    CaseFunct := @CaseNone;
    StringCaseFunct := @StringCaseNone;
  end;
end;

function TSynRange.AddAttribs(AAttributes: TSynHighlighterAttributes): Integer;
begin
  Result := FAttributes.Add(AAttributes);
end;

function TSynRange.AddNewAttribs(AName: string): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName);
  FAttributes.Add(Result);
end;

function TSynRange.AttribsByName(AName: string): TSynHighlighterAttributes;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FAttributes.Count - 1 do
    if TSynHighlighterAttributes(FAttributes[I]).Name = AName then
    begin
      Result := TSynHighlighterAttributes(FAttributes[I]);
      Exit;
    end;
end;

procedure TSynRange.DeleteAttribs(AIndex: Integer);
begin
//  TSynHighlighterAttributes(FAttributes[AIndex]).Free;
  FAttributes.Delete(AIndex);
end;

procedure TSynRange.DeleteAttribs(AName: string);
var
  HA: TSynHighlighterAttributes;
begin
  HA := AttribsByName(AName);
//  HA.Free;
  FAttributes.Remove(HA);
end;

procedure TSynRange.Reset;
var
  I: Integer;
begin
  if not FPrepared then
    Exit;
  FDefaultSynSymbol.Free;
  FDefaultTermSymbol.Free;
  FDefaultSymbols.Free;
  FNumberSymbol.Free;

  for I := 0 to 255 do
    FSymbolList[Char(I)] := nil;

  for I := 0 to FSynRanges.Count - 1 do
    TSynRange(FSynRanges[I]).Reset;

  FSynSymbols.Clear;
end;

procedure TSynRange.Clear;
var
  I: Integer;
begin
  Reset;
  for I := 0 to FSynRanges.Count - 1 do
    TSynRange(FSynRanges[I]).Clear;

  FSynRanges.Clear;
  FSynSymbols.Clear;
  FSymbolGroups.Clear;
  FAttributes.Clear;
end;

procedure TSynRange.LoadFromStream(AStream: TStream);
var
  Buf, Sav   : PChar;
  BufSize    : Integer;
  CurTagIndex: Integer;
  LineNumber : Integer;
  Param      : string;
  SL         : TStringList;

  function GetNextTag(var AIndex: Integer; var ATagParam: string;
    AIgnoreUnknown: Boolean): Boolean;
  var
    S   : string;
    sPos: PChar;
  begin
    AIndex := -1;
    Result := True;
    ATagParam := '';
    while Buf^ <> '<' do
    begin
      if Buf^ = #0 then
        Exit;
      if Buf^ = #13 then
        Inc(LineNumber);
      Inc(Buf);
    end;
    Inc(Buf);
    while (Buf^ = ' ') or (Buf^ = #32) do
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);

    if Buf^ = '/' then
    begin
      Result := False;
      Inc(Buf);
    end;
    sPos := Buf;
    while (Buf^ <> #32) and (Buf^ <> '>') do
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);
    SetLength(S, Pointer(Buf) - Pointer(sPos));
    Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));

    if not SL.Find(S, AIndex) then
      if not AIgnoreUnknown then
        raise Exception.Create('Tag "' + S + '" is unknown (line ' +
          IntToStr(LineNumber) + ')')
      else
      begin
        AIndex := -1;
        Result := True;
        Exit;
      end;

    while Buf^ <> '>' do
    begin
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber));
      if Buf^ = '"' then
      begin
        Inc(Buf);
        sPos := Buf;
        while (Buf^ <> '"') do
          if Buf^ = #0 then
          begin
            Result := False;
            Exit;
          end
          else
            Inc(Buf);
        SetLength(ATagParam, Pointer(Buf) - Pointer(sPos));
        Move(sPos^, Pointer(ATagParam)^, Pointer(Buf) - Pointer(sPos));
      end;
      Inc(Buf);
    end;
    Inc(Buf);
  end;

  function GetReplacement: string;
  var
    sPos: PChar;
  begin
    Result := '';
    sPos := Buf;
    Inc(Buf);
    if Buf^ = 'l' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '<';
      end;
    end
    else if Buf^ = 'g' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '>';
      end;
    end
    else if Buf^ = 'q' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '"';
      end;
    end
    else if Buf^ = 'a' then
    begin
      Inc(Buf);
      if Buf^ = 'm' then
      begin
        Inc(Buf);
        if Buf^ = 'p' then
        begin
          Inc(Buf);
          if Buf^ = ';' then
            Result := '&';
        end;
      end;
    end;
    if Result = '' then
    begin
      Dec(Buf);
      SetLength(Result, Pointer(Buf) - Pointer(sPos));
      Move(sPos^, Pointer(Result)^, Pointer(Buf) - Pointer(sPos));
    end
    else
      Inc(Buf);
  end;

  function GetData(ATagIndex: Integer): string;
  var
    S    : string;
    sPos : PChar;
    I    : Integer;
  begin
    I := 0;
    Result := '';
    sPos := Buf;
    while Buf^ <> '<' do
    begin
      if Buf^ = '&' then
      begin
        SetLength(S, Pointer(Buf) - Pointer(sPos));
        Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
        Result := Result + S + GetReplacement;
        sPos := Buf;
      end
      else if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);
    end;
    SetLength(S, Pointer(Buf) - Pointer(sPos));
    Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
    Result := Result + S;
    if (GetNextTag(I, S, False)) or (I <> CurTagIndex) then
      raise Exception.Create('Close tag: /' + SL[I] +
        ' is not found. Line ' + IntToStr(LineNumber));
  end;

  procedure ReadInfo;
    procedure ReadGeneral;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitName:
            GetData(xitName);
          xitFileTypeName:
            GetData(xitFileTypeName);
          xitLayout:
            GetData(xitLayout);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitGeneral then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadVersion;
      function GetType(s: string): TVersionType;
      begin
        if s = 'Beta' then
          Result := vtBeta
        else if s = 'Release' then
          Result := vtRelease
        else
          Result := vtInternalTest;
      end;

    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitVersion:
            GetData(xitVersion);
          xitRevision:
            GetData(xitRevision);
          xitDate:
            GetData(xitDate);
          xitType:
            GetData(xitType);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitVersion then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadAuthor;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitName:
            GetData(xitName);
          xitEmail:
            GetData(xitEmail);
          xitWeb:
            GetData(xitWeb);
          xitCopyright:
            GetData(xitCopyright);
          xitCompany:
            GetData(xitCompany);
          xitRemark:
            GetData(xitRemark);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitAuthor then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadHistroy;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitH:
            GetData(xitH);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitHistory then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadSample;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitS:
            GetData(xitS);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitSample then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitGeneral:
          ReadGeneral;
        xitVersion:
          ReadVersion;
        xitAuthor:
          ReadAuthor;
        xitHistory:
          ReadHistroy;
        xitSample:
          ReadSample;
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitInfo then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

  procedure ReadKW(SymbGr: TSynSymbolGroup);

    procedure ReadAttri;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            SymbGr.FAttributes.Background := strtointdef(GetData(xitBack), 0);
          xitFore:
            SymbGr.FAttributes.Foreground := strtointdef(GetData(xitFore), 0);
          xitStyle:
            SymbGr.FAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitAttri then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitAttri:
          ReadAttri;
        xitW:
          SymbGr.FKeywords.Add(GetData(xitW));
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitKW then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

  procedure ReadRange(CurRange: TSynRange);
  var
    NewRange      : TSynRange;
    NewSymbolGroup: TSynSymbolGroup;

    procedure ReadDef;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            CurRange.DefaultAttributes.Background :=
              strtointdef(GetData(xitBack), 0);
          xitFore:
            CurRange.DefaultAttributes.Foreground :=
              strtointdef(GetData(xitFore), 0);
          xitStyle:
            CurRange.DefaultAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitDef then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadNum;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            CurRange.NumberAttributes.Background :=
              strtointdef(GetData(xitBack), 0);
          xitFore:
            CurRange.NumberAttributes.Foreground :=
              strtointdef(GetData(xitFore), 0);
          xitStyle:
            CurRange.DefaultAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitNum then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitDef:
          ReadDef;
        xitOpenSymbol:
          CurRange.OpenSymbol.Symbol := GetData(xitOpenSymbol);
        xitCloseSymbol:
          CurRange.CloseSymbol.Symbol := GetData(xitCloseSymbol);
        xitCloseOnTerm:
          CurRange.CloseOnTerm :=
            lowercase(GetData(xitCloseOnTerm)) = 'true';
        xitCloseOnEol:
          CurRange.CloseOnEol :=
            lowercase(GetData(xitCloseOnEol)) = 'true';
        xitAnyTerm:
          if lowercase(GetData(xitAnyTerm)) = 'true' then
            CurRange.OpenSymbol.BreakType := btAny
          else
            CurRange.OpenSymbol.BreakType := btTerm;
        xitDelimiterChars:
          CurRange.TermSymbols := String2Set(GetData(xitDelimiterChars));
        xitNum:
          ReadNum;
        xitCaseSensitive:
          CurRange.CaseSensitive :=
            lowercase(GetData(xitCaseSensitive)) = 'true';
        xitKW:
          begin
            NewSymbolGroup :=
              TSynSymbolGroup.Create('', CurRange.AddNewAttribs('unknown'));
            NewSymbolGroup.FName := Param;
            CurRange.AddSymbolGroup(NewSymbolGroup);
            ReadKW(NewSymbolGroup);
          end;
        xitRange:
          begin
            NewRange := TSynRange.Create;
            NewRange.Name := Param;
            CurRange.AddRange(NewRange);
            ReadRange(NewRange);
          end;
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitRange then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

begin
  Clear;

  try
    BufSize := AStream.Size;
    GetMem(Buf, BufSize);
    AStream.ReadBuffer(Buf^, BufSize);
  except
    FreeMem(Buf);
    raise;
  end;
  Sav := Buf;
  LineNumber := 0;

  SL := nil;
  try
    SL := TStringList.Create;
    BuildXMLIndexes(SL);
    ////TL added the third parameter
    if (not GetNextTag(CurTagIndex, Param, False)) or
      (CurTagIndex <> xitUniHighlighter) then
      raise Exception.Create(
        'Highlighter header tag ("<UniHighlighter>") is not found.');

    while GetNextTag(CurTagIndex, Param, True) do
    begin
      case CurTagIndex of
        xitInfo:
          ReadInfo;
        xitRange:
          ReadRange(Self);
        xitCopyright:
          GetData(xitCopyright);
      end;
    end;
    if CurTagIndex <> xitUniHighlighter then
      raise Exception.Create('Closing tag: /' +
        SL[xitUniHighlighter] +
        ' was not found. Line ' + IntToStr(LineNumber));
  finally
    FreeMem(Sav);
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'TSymbolList'}
procedure TSymbolList.AfterConstruction;
begin
  inherited AfterConstruction;
  FSymbolList := TObjectList.Create(True);
end;

procedure TSymbolList.BeforeDestruction;
begin
  FSymbolList.Free;
  inherited BeforeDestruction;
end;

procedure TSymbolList.AddSymbol(ASymbolNode: TSymbolNode);
begin
  FSymbolList.Add(ASymbolNode);
end;

function TSymbolList.FindSymbol(AChar: Char): TSymbolNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FSymbolList.Count - 1 do
    if TSymbolNode(FSymbolList[I]).SymbolChar = AChar then
    begin
      Result := TSymbolNode(FSymbolList[I]);
      Break;
    end;
end;

function TSymbolList.GetCount: Integer;
begin
  Result := FSymbolList.Count;
end;

function TSymbolList.GetSymbolNode(AIndex: Integer): TSymbolNode;
begin
  Result := TSymbolNode(FSymbolList[AIndex]);
end;

procedure TSymbolList.SetSymbolNode(AIndex: Integer; AValue: TSymbolNode);
begin
  if AIndex < FSymbolList.Count then
    TSymbolNode(FSymbolList[AIndex]).Free;
  FSymbolList[AIndex] := AValue;
end;
{$ENDREGION}

{$REGION 'TSymbols'}
procedure TSymbols.AddSymbol(const AString: string; ASynSymbol: TSynSymbol; ABreakType: TSymbolBreakType);
var
  I  : Integer;
  J  : Integer;
  SN : TSymbolNode;
  SL : TSymbolList;
begin
  SL := FHeadNode.NextSymbols;
  SN := nil;
  J := Length(AString);
  for I := 1 to J do
  begin
    SN := SL.FindSymbol(AString[I]);
    if SN = nil then
    begin
      SN := TSymbolNode.Create(AString[I]);
      SL.AddSymbol(SN);
    end;
    SL := SN.NextSymbols;
  end;
  SN.BreakType := ABreakType;
  SN.SynSymbol := ASynSymbol;
end;

constructor TSymbols.Create(AChar: Char; ASynSymbol: TSynSymbol;
  ABreakType: TSymbolBreakType);
begin
  inherited Create;
  FHeadNode := TSymbolNode.Create(AChar, ASynSymbol, ABreakType);
end;

procedure TSymbols.BeforeDestruction;
begin
  FHeadNode.Free;
  inherited;
end;

function TSymbols.FindSymbol(const AString: string): TSymbolNode;
var
  I        : Integer;
  N        : Integer;
  Node     : TSymbolNode;
  LastNode : TSymbolNode;
begin
  if Assigned(FHeadNode) then
  begin
    Node := FHeadNode;
    N := Length(AString);
    for I := 1 to N do
    begin
      LastNode := Node.NextSymbols.FindSymbol(AString[I]);
      if LastNode = nil then
        Break;
      Node := LastNode;
    end;
    Result := Node;
  end;
end;

function TSymbols.GetToken(AParser: TSynUniSyn;
  var ASynSymbol: TSynSymbol): Boolean;
var
  Node     : TSymbolNode;
  NextNode : TSymbolNode;
begin
  Result := False;
  Node := FHeadNode;
  NextNode := nil;

  while (Node.NextSymbols.Count > 0) and (AParser.Line[AParser.Run] <> #0) do
  begin
    AParser.Run := AParser.Run + 1;
    NextNode := Node.NextSymbols.FindSymbol(AParser.Line[AParser.Run]);
    if NextNode = nil then
      Break;
    Node := NextNode;
  end;

  if Node.SynSymbol = nil then
    Exit;

  if (NextNode = nil) and (Node.NextSymbols.Count > 0) then
    AParser.Run := AParser.Run - 1;

  if AParser.Line[AParser.Run] <> #0 then
    AParser.Run := AParser.Run + 1;

  if Node.BreakType = btAny then
  begin
    Result := True;
    ASynSymbol := Node.SynSymbol;
    Exit;
  end;

  if AParser.Line[AParser.Run] in AParser.CurrentRule.TermSymbols then
  begin
    Result := True;
    ASynSymbol := Node.SynSymbol;
  end;
end;
{$ENDREGION}

{$REGION 'TSymbolNode'}
constructor TSymbolNode.Create(ASymbolChar: Char; ASynSymbol: TSynSymbol;
  ABreakType: TSymbolBreakType);
begin
  inherited Create;
  FSymbolChar := ASymbolChar;
  FBreakType  := ABreakType;
  FSynSymbol  := ASynSymbol;
end;

constructor TSymbolNode.Create(ASymbolChar: Char);
begin
  inherited Create;
  FSymbolChar := ASymbolChar;
end;

procedure TSymbolNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FNextSymbols := TSymbolList.Create;
end;

procedure TSymbolNode.BeforeDestruction;
begin
  FNextSymbols.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'TDefaultSymbols'}
constructor TDefaultSymbols.Create(ASynSymbol: TSynSymbol);
begin
  inherited Create;
  FSynSymbol := ASynSymbol;
end;

destructor TDefaultSymbols.Destroy;
begin
  FSynSymbol.Free;
  inherited;
end;

function TDefaultSymbols.GetToken(AParser: TSynUniSyn;
  var ASynSymbol: TSynSymbol): Boolean;
begin
  AParser.Run := AParser.Run + 1;
  Result := False;
end;
{$ENDREGION}

{$REGION 'TNumberSymbols'}
constructor TNumberSymbols.Create(ASynSymbol: TSynSymbol);
begin
  inherited Create;
  FSynSymbol := ASynSymbol;
end;

destructor TNumberSymbols.Destroy;
begin
  FSynSymbol.Free;
  inherited;
end;

function TNumberSymbols.GetToken(AParser: TSynUniSyn;
  var ASynSymbol: TSynSymbol): Boolean;
begin
  repeat
    AParser.Run := AParser.Run + 1;
  until not(AParser.Line[AParser.Run] in ['0' .. '9']);
  if AParser.Line[AParser.Run] in AParser.CurrentRule.TermSymbols then
  begin
    Result := True;
    ASynSymbol := FSynSymbol;
  end
  else
    Result := False;
end;
{$ENDREGION}

{$REGION 'TDefaultTermSymbols'}
constructor TDefaultTermSymbols.Create(ASynSymbol: TSynSymbol);
begin
  inherited Create;
  FSynSymbol := ASynSymbol;
end;

destructor TDefaultTermSymbols.Destroy;
begin
  FSynSymbol.Free;
  inherited;
end;

function TDefaultTermSymbols.GetToken(AParser: TSynUniSyn;
  var ASynSymbol: TSynSymbol): Boolean;
begin
  if AParser.Line[AParser.Run] <> #0 then
    AParser.Run := AParser.Run + 1;
  ASynSymbol := FSynSymbol;
  Result := True;
end;
{$ENDREGION}

{$REGION 'TSynUniSyn'}
constructor TSynUniSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfo.History := TStringList.Create;
  FInfo.Sample := TStringList.Create;
  FPrepared := False;
  FSymbols := TSymbols.Create(' ', nil, btAny);
  FMainRules := TSynRange.Create;
  FMainRules.Name := ROOT_NAME;
  FEol := False;
  FPrEol := False;
  FCurrentRule := FMainRules;
end;

destructor TSynUniSyn.Destroy;
begin
  FCurrentRule := nil;
  FSymbols.Free;
  FMainRules.Free;
  FInfo.History.Free;
  FInfo.Sample.Free;
  if FLine <> nil then
    FreeMem(FLine);
  inherited;
end;

procedure TSynUniSyn.SetLine(const NewValue: string;
  LineNumber: Integer);
var
  L, I: Integer;
begin
  if not FCurrentRule.Prepared then
    Prepare;
  FTrueLine := PChar(NewValue);
  L := Length(NewValue);
  if FLine <> nil then
    FreeMem(FLine);
  GetMem(FLine, L + 1);
  SetLength(FUpLine, L + 1);
  for I := 0 to L do
    FLine[I] := FCurrentRule.CaseFunct(FTrueLine[I]);
  FRun := 0;
  FTokenPos := 0;
  FLineNumber := LineNumber;
  FEol := False;
  FPrEol := False;
  Next;
end;

procedure TSynUniSyn.SpaceProc;
begin
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or (FLine[FRun] in [#0, #10, #13]);
end;

function TSynUniSyn.IsKeyword(const AKeyword: string): Boolean;
begin
  Result := FSymbols.FindSymbol(AKeyword) <> nil;
end;

procedure TSynUniSyn.Next;
begin
  if FPrEol then
  begin
    if (FCurrentRule.CloseOnEol) or (FCurrentRule.CloseOnTerm) then
      FCurrentRule := FCurrentRule.Owner;
    FEol := True;
    Exit;
  end;

  FTokenPos := FRun;
  if (FCurrentRule.CloseOnTerm) and
    (FLine[FRun] in FCurrentRule.TermSymbols) then
    FCurrentRule := FCurrentRule.Owner;

  if not FCurrentRule.SymbolList[FLine[FRun]].GetToken(Self, FCurrToken) then
  begin
    FCurrToken := FCurrentRule.DefaultSynSymbol;
    while not(FLine[FRun] in FCurrentRule.TermSymbols) do
      Inc(FRun);
  end
  else if FCurrentRule.ClosingSymbol.Symbol = FCurrToken then
    FCurrentRule := FCurrentRule.Owner
  else if FCurrToken.OpenRule <> nil then
    FCurrentRule := FCurrToken.OpenRule;

  if FLine[FRun] = #0 then
    FPrEol := True;
end;

function TSynUniSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT:
      Result := FCurrentRule.DefaultAttributes;
    SYN_ATTR_IDENTIFIER:
      Result := FCurrentRule.DefaultAttributes;
    SYN_ATTR_KEYWORD:
      Result := FCurrentRule.DefaultAttributes;
    SYN_ATTR_STRING:
      Result := FCurrentRule.DefaultAttributes;
    SYN_ATTR_WHITESPACE:
      Result := FCurrentRule.DefaultAttributes;
  else
    Result := nil;
  end;
end;

function TSynUniSyn.GetEOL: Boolean;
begin
  Result := FEol;
end;

function TSynUniSyn.GetRange: Pointer;
begin
  Result := FCurrentRule;
end;

function TSynUniSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := FRun - FTokenPos;
  Setstring(Result, (FTrueLine + FTokenPos), Len);
end;

procedure TSynUniSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: Integer);
begin
  TokenLength := FRun - FTokenPos;
  TokenStart := FTrueLine + FTokenPos;
end;

function TSynUniSyn.GetTokenID: Integer;
begin
  Result := 1; // CODE_REVIEW FCurrToken.ID;
end;

function TSynUniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FCurrToken.Attributes;
end;

function TSynUniSyn.GetTokenKind: Integer;
begin
  Result := 1; // CODE_REVIEW   FCurrToken.ID;
end;

function TSynUniSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynUniSyn.ResetRange;
begin
  FCurrentRule := FMainRules;
end;

procedure TSynUniSyn.SetRange(AValue: Pointer);
begin
  FCurrentRule := TSynRange(AValue);
end;

class function TSynUniSyn.GetLanguageName: string;
begin
  Result := 'UniLanguage';
end;

procedure TSynUniSyn.Clear;
begin
  MainRules.Clear;
end;

procedure TSynUniSyn.CreateStandardRules;
var
  SR : TSynRange;
  SSG: TSynSymbolGroup;

begin
  if (DefaultFilter <> '') and (FileExists(DefaultFilter)) then
    LoadFromFile(DefaultFilter)
  else
  begin
    MainRules.Clear;
    MainRules.DefaultAttributes.Foreground := clBlack;
    MainRules.DefaultAttributes.Background := clWhite;
    MainRules.NumberAttributes.Foreground := clMaroon;
    MainRules.NumberAttributes.Background := clWhite;
    MainRules.CaseSensitive := False;

    SR := TSynRange.Create;
    SR.Name := 'Strings ''..''';
    SR.DefaultAttributes.Foreground := clRed;
    SR.DefaultAttributes.Background := clWhite;
    SR.NumberAttributes.Foreground := clRed;
    SR.NumberAttributes.Background := clWhite;
    SR.CaseSensitive := False;
    SR.OpenSymbol.BreakType := btAny;
    MainRules.AddRange(SR);

    SR := TSynRange.Create('"', '"');
    SR.Name := 'Strings ".."';
    SR.DefaultAttributes.Foreground := clRed;
    SR.DefaultAttributes.Background := clWhite;
    SR.NumberAttributes.Foreground := clRed;
    SR.NumberAttributes.Background := clWhite;
    SR.CaseSensitive := False;
    SR.OpenSymbol.BreakType := btAny;
    MainRules.AddRange(SR);

    SR := TSynRange.Create('{', '}');
    SR.Name := 'Remarks {..}';
    SR.DefaultAttributes.Foreground := clNavy;
    SR.DefaultAttributes.Background := clWhite;
    SR.NumberAttributes.Foreground := clNavy;
    SR.NumberAttributes.Background := clWhite;
    SR.CaseSensitive := False;
    SR.OpenSymbol.BreakType := btAny;
    MainRules.AddRange(SR);

    SR := TSynRange.Create('(*', '*)');
    SR.Name := 'Remarks (*..*)';
    SR.DefaultAttributes.Foreground := clNavy;
    SR.DefaultAttributes.Background := clWhite;
    SR.NumberAttributes.Foreground := clNavy;
    SR.NumberAttributes.Background := clWhite;
    SR.CaseSensitive := False;
    SR.OpenSymbol.BreakType := btAny;
    MainRules.AddRange(SR);

    SR := TSynRange.Create('/*', '*/');
    SR.Name := 'Remarks /*..*/';
    SR.DefaultAttributes.Foreground := clNavy;
    SR.DefaultAttributes.Background := clWhite;
    SR.NumberAttributes.Foreground := clNavy;
    SR.NumberAttributes.Background := clWhite;
    SR.CaseSensitive := False;
    SR.OpenSymbol.BreakType := btAny;
    MainRules.AddRange(SR);

    SSG := TSynSymbolGroup.Create('', TSynHighlighterAttributes.Create('unknown'));
    SSG.FName := 'Key words';
    SSG.FAttributes.Foreground := clGreen;
    SSG.FAttributes.Background := clWhite;
    MainRules.AddSymbolGroup(SSG);
  end;
end;

procedure TSynUniSyn.Prepare;
begin
  FMainRules.Prepare(FMainRules);
end;

procedure TSynUniSyn.NullProc;
begin
  // FEol:=True;
end;

procedure TSynUniSyn.Reset;
begin
  FMainRules.Reset;
end;

procedure TSynUniSyn.SaveToStream(AStream: TStream);

  procedure WriteString(const AString: string);
  begin
    if Length(AString) > 0 then
    begin
      AStream.Write(AString[1], Length(AString));
      AStream.Write(#13#10, 1);
    end;
  end;

  function Indent(AIndent: Integer): string;
  begin
    if AIndent > 0 then
    begin
      SetLength(Result, AIndent);
      FillChar(Result[1], AIndent, #32);
    end;
  end;

  function GetValidValue(AValue: string): string;
  begin
    AValue := StringReplace(AValue, '&', '&amp;', [rfReplaceAll, rfIgnoreCase]);
    AValue := StringReplace(AValue, '<', '&lt;', [rfReplaceAll, rfIgnoreCase]);
    AValue := StringReplace(AValue, '"', '&quot;', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(AValue, '>', '&gt;', [rfReplaceAll, rfIgnoreCase]);
  end;

  procedure InsertTag(AIndent: Integer; const AName: string; const AValue: string);
  begin
    WriteString(Format('%s<%s>%s</%s>', [Indent(AIndent), AName,
      GetValidValue(AValue), AName]));
  end;

  procedure OpenTag(AIndent: Integer; const AName: string; const AParam: string = '';
    const AParamValue: string = '');
  begin
    if AParam = '' then
      WriteString(Format('%s<%s>', [Indent(AIndent), AName]))
    else
      WriteString(Format('%s<%s %s="%s">', [Indent(AIndent), AName, AParam,
        GetValidValue(AParamValue)]));
  end;

  procedure SaveColor(const AMainTag: string; AIndent, AFore, ABack: Integer;
    AStyle: TFontStyles);
  begin
    OpenTag(AIndent, AMainTag);
    InsertTag(AIndent + 1, 'Back', IntToStr(ABack));
    InsertTag(AIndent + 1, 'Fore', IntToStr(AFore));
    InsertTag(AIndent + 1, 'Style', Fs2String(AStyle));
    OpenTag(AIndent, '/' + AMainTag);
  end;

  procedure SaveKWGroup(AIndent: Integer; ASymbolGroup: TSynSymbolGroup);
  var
    I: Integer;
  begin
    OpenTag(AIndent, 'KW', 'Name', ASymbolGroup.FName);
    SaveColor('Attri', AIndent + 1, ASymbolGroup.Attributes.Foreground, ASymbolGroup.Attributes.Background,
      ASymbolGroup.Attributes.Style);
    for I := 0 to ASymbolGroup.Keywords.Count - 1 do
      InsertTag(AIndent + 1, 'W', ASymbolGroup.Keywords[I]);
    OpenTag(AIndent, '/KW');
  end;

  procedure SaveRange(AIndent: Integer; ARange: TSynRange);
  var
    I: Integer;

    procedure InsertTagBool(AIndent: Integer; const AName: string; AValue: Boolean);
    begin
      if AValue then
        WriteString(Format('%s<%s>True</%s>', [Indent(AIndent), AName, AName]))
      else
        WriteString(Format('%s<%s>False</%s>', [Indent(AIndent), AName, AName]));
    end;

    procedure SaveRangeColor(AIndent: Integer; ARange: TSynRange);
    begin
      SaveColor('Def', AIndent, ARange.DefaultAttributes.Foreground,
        ARange.DefaultAttributes.Background,
        ARange.DefaultAttributes.Style);
      SaveColor('Num', AIndent, ARange.NumberAttributes.Foreground, ARange.NumberAttributes.Background,
        ARange.NumberAttributes.Style);
    end;

  begin
    OpenTag(AIndent, 'Range', 'Name', ARange.Name);
    SaveRangeColor(AIndent, ARange);
    InsertTag(AIndent, 'OpenSymbol', ARange.OpenSymbol.Symbol);
    InsertTag(AIndent, 'CloseSymbol', ARange.CloseSymbol.Symbol);

    InsertTag(AIndent, 'DelimiterChars', Set2String(ARange.TermSymbols));
    if ARange.OpenSymbol.BreakType = btAny then
      InsertTag(AIndent, 'AnyTerm', 'True')
    else
      InsertTag(AIndent, 'AnyTerm', 'False');
    InsertTagBool(AIndent, 'CloseOnTerm', ARange.CloseOnTerm);
    InsertTagBool(AIndent, 'CloseOnEol', ARange.CloseOnEol);
    InsertTagBool(AIndent, 'CaseSensitive', ARange.CaseSensitive);
    for I := 0 to ARange.SymbolGroupCount - 1 do
      SaveKWGroup(AIndent, ARange.SymbolGroups[I]);
    for I := 0 to ARange.RangeCount - 1 do
      SaveRange(AIndent + 1, ARange.Ranges[I]);
    OpenTag(AIndent, '/Range');
  end;

  procedure SaveInfo;
  var
    I: Integer;
  begin
    OpenTag(1, 'Info');

    OpenTag(2, 'General');
    InsertTag(3, 'Name', FInfo.General.Name);
    InsertTag(3, 'FileTypeName', FInfo.General.FileTypeName);
    InsertTag(3, 'Layout', FInfo.General.Layout);
    OpenTag(2, '/General');

    OpenTag(2, 'Author');
    InsertTag(3, 'Name', FInfo.Author.Name);
    InsertTag(3, 'Email', FInfo.Author.Email);
    InsertTag(3, 'Web', FInfo.Author.Web);
    InsertTag(3, 'Copyright', FInfo.Author.Copyright);
    InsertTag(3, 'Company', FInfo.Author.Company);
    InsertTag(3, 'Remark', FInfo.Author.Remark);
    OpenTag(2, '/Author');

    OpenTag(2, 'Version');
    InsertTag(3, 'Version', IntToStr(FInfo.Version.Version));
    InsertTag(3, 'Revision', IntToStr(FInfo.Version.Revision));
    InsertTag(3, 'Date', floattostr(FInfo.Version.ReleaseDate));
    case FInfo.Version.VersionType of
      vtInternalTest:
        InsertTag(3, 'Type', 'Internal Test');
      vtBeta:
        InsertTag(3, 'Type', 'Beta');
      vtRelease:
        InsertTag(3, 'Type', 'Release');
    end;
    OpenTag(2, '/Version');

    OpenTag(2, 'History');
    for I := 0 to FInfo.History.Count - 1 do
      InsertTag(3, 'H', FInfo.History[I]);
    OpenTag(2, '/History');

    OpenTag(2, 'Sample');
    for I := 0 to FInfo.Sample.Count - 1 do
      InsertTag(3, 'S', FInfo.Sample[I]);
    OpenTag(2, '/Sample');

    OpenTag(1, '/Info');
  end;

begin
  OpenTag(0, 'UniHighlighter');
  OpenTag(1, 'ImportantInfo');
  OpenTag(1, '/ImportantInfo');
  SaveInfo;
  SaveRange(1, Self.MainRules);
  OpenTag(0, '/UniHighlighter');
end;

procedure TSynUniSyn.LoadFromStream(AStream: TStream);
var
  Buf, Sav   : PChar;
  BufSize    : Integer;
  CurTagIndex: Integer;
  LineNumber : Integer;
  Param      : string;
  SL         : TStringList;

  function GetReplacement: string;
  var
    sPos: PChar;
  begin
    Result := '';
    sPos := Buf;
    Inc(Buf);
    if Buf^ = 'l' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '<';
      end;
    end
    else if Buf^ = 'g' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '>';
      end;
    end
    else if Buf^ = 'q' then
    begin
      Inc(Buf);
      if Buf^ = 't' then
      begin
        Inc(Buf);
        if Buf^ = ';' then
          Result := '"';
      end;
    end
    else if Buf^ = 'a' then
    begin
      Inc(Buf);
      if Buf^ = 'm' then
      begin
        Inc(Buf);
        if Buf^ = 'p' then
        begin
          Inc(Buf);
          if Buf^ = ';' then
            Result := '&';
        end;
      end;
    end;
    if Result = '' then
    begin
      Dec(Buf);
      SetLength(Result, Pointer(Buf) - Pointer(sPos));
      Move(sPos^, Pointer(Result)^, Pointer(Buf) - Pointer(sPos));
    end
    else
      Inc(Buf);
  end;

  function GetNextTag(var AIndex: Integer; var ATagParam: string;
    AIgnoreUnknown: Boolean = False): Boolean;
  var
    S, T: string;
    sPos   : PChar;
  begin
    AIndex := -1;
    Result := True;
    ATagParam := '';
    while Buf^ <> '<' do
    begin
      if Buf^ = #0 then
        Exit;
      if Buf^ = #13 then
        Inc(LineNumber);
      Inc(Buf);
    end;
    Inc(Buf);
    while (Buf^ = #32) do
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);

    if Buf^ = '/' then
    begin
      Result := False;
      Inc(Buf);
    end;
    sPos := Buf;
    while (Buf^ <> #32) and (Buf^ <> '>') do
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);
    SetLength(S, Pointer(Buf) - Pointer(sPos));
    Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));

    if (not SL.Find(S, AIndex)) then
      if (not AIgnoreUnknown) then
        raise Exception.Create('Tag "' + S + '" is unknown (line ' +
          IntToStr(LineNumber) + ')')
      else
      begin
        AIndex := -1;
        Result := True;
        Exit;
      end;

    while Buf^ <> '>' do
    begin
      if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber));
      if Buf^ = '"' then
      begin
        Inc(Buf);
        sPos := Buf;
        T := '';
        while (Buf^ <> '"') do
          if Buf^ = #0 then
          begin
            Result := False;
            Exit;
          end
          else if Buf^ = '&' then
          begin
            SetLength(S, Pointer(Buf) - Pointer(sPos));
            Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
            T := T + S + GetReplacement;
            sPos := Buf;
          end
          else
            Inc(Buf);
        SetLength(S, Pointer(Buf) - Pointer(sPos));
        Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
        ATagParam := T + S;
      end;
      Inc(Buf);
    end;
    Inc(Buf);
  end;

  function GetData(ATagIndex: Integer): string;
  var
    S   : string;
    sPos: PChar;
    Idx : Integer;
  begin
    Idx := 0;
    Result := '';
    sPos := Buf;
    while Buf^ <> '<' do
    begin
      if Buf^ = '&' then
      begin
        SetLength(S, Pointer(Buf) - Pointer(sPos));
        Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
        Result := Result + S + GetReplacement;
        sPos := Buf;
      end
      else if (Buf^ = #0) or (Buf^ = #13) then
        raise Exception.Create('Unexpected end of line. Line ' +
          IntToStr(LineNumber))
      else
        Inc(Buf);
    end;
    SetLength(S, Pointer(Buf) - Pointer(sPos));
    Move(sPos^, Pointer(S)^, Pointer(Buf) - Pointer(sPos));
    Result := Result + S;
    if (GetNextTag(Idx, S)) or (Idx <> CurTagIndex) then
      raise Exception.Create('Close tag: /' + SL[Idx] +
        ' is not found. Line ' + IntToStr(LineNumber));
  end;

  procedure ReadInfo;

    procedure ReadGeneral;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitName:
            FInfo.General.Name := GetData(xitName);
          xitFileTypeName:
            FInfo.General.FileTypeName := GetData(xitFileTypeName);
          xitLayout:
            FInfo.General.Layout := GetData(xitLayout);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitGeneral then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadVersion;

      function GetType(s: string): TVersionType;
      begin
        if s = 'Beta' then
          Result := vtBeta
        else if s = 'Release' then
          Result := vtRelease
        else
          Result := vtInternalTest;
      end;

    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitVersion:
            FInfo.Version.Version := strtointdef(GetData(xitVersion), 0);
          xitRevision:
            FInfo.Version.Revision := strtointdef(GetData(xitRevision), 0);
          xitDate:
            FInfo.Version.ReleaseDate := strtointdef(GetData(xitDate), 0);
          xitType:
            FInfo.Version.VersionType := GetType(GetData(xitType));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitVersion then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadAuthor;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitName:
            FInfo.Author.Name := GetData(xitName);
          xitEmail:
            FInfo.Author.Email := GetData(xitEmail);
          xitWeb:
            FInfo.Author.Web := GetData(xitWeb);
          xitCopyright:
            FInfo.Author.Copyright := GetData(xitCopyright);
          xitCompany:
            FInfo.Author.Company := GetData(xitCompany);
          xitRemark:
            FInfo.Author.Remark := GetData(xitRemark);
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitAuthor then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadHistroy;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitH:
            FInfo.History.Add(GetData(xitH));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitHistory then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadSample;
    begin
      FInfo.Sample.Clear;
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitS:
            FInfo.Sample.Add(GetData(xitS));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitSample then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitGeneral:
          ReadGeneral;
        xitVersion:
          ReadVersion;
        xitAuthor:
          ReadAuthor;
        xitHistory:
          ReadHistroy;
        xitSample:
          ReadSample;
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitInfo then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

  procedure ReadKW(SymbGr: TSynSymbolGroup);

    procedure ReadAttri;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            SymbGr.FAttributes.Background := strtointdef(GetData(xitBack), 0);
          xitFore:
            SymbGr.FAttributes.Foreground := strtointdef(GetData(xitFore), 0);
          xitStyle:
            SymbGr.FAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitAttri then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitAttri:
          ReadAttri;
        xitW:
          SymbGr.FKeywords.Add(GetData(xitW));
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitKW then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

  procedure ReadRange(CurRange: TSynRange);
  var
    NewRange      : TSynRange;
    NewSymbolGroup: TSynSymbolGroup;

    procedure ReadDef;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            CurRange.DefaultAttributes.Background :=
              strtointdef(GetData(xitBack), 0);
          xitFore:
            CurRange.DefaultAttributes.Foreground :=
              strtointdef(GetData(xitFore), 0);
          xitStyle:
            CurRange.DefaultAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitDef then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

    procedure ReadNum;
    begin
      while GetNextTag(CurTagIndex, Param, False) do
      begin
        case CurTagIndex of
          xitBack:
            CurRange.NumberAttributes.Background :=
              strtointdef(GetData(xitBack), 0);
          xitFore:
            CurRange.NumberAttributes.Foreground :=
              strtointdef(GetData(xitFore), 0);
          xitStyle:
            CurRange.DefaultAttributes.Style := String2Fs(GetData(xitStyle));
        else
          raise Exception.Create('Unexpected tag: ' +
            SL[CurTagIndex] +
            ' line ' + IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex <> xitNum then
        raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
    end;

  begin
    while GetNextTag(CurTagIndex, Param, False) do
    begin
      case CurTagIndex of
        xitDef:
          ReadDef;
        xitOpenSymbol:
          CurRange.OpenSymbol.Symbol := GetData(xitOpenSymbol);
        xitCloseSymbol:
          CurRange.CloseSymbol.Symbol := GetData(xitCloseSymbol);
        xitCloseOnTerm:
          CurRange.CloseOnTerm :=
            lowercase(GetData(xitCloseOnTerm)) = 'true';
        xitCloseOnEol:
          CurRange.CloseOnEol :=
            lowercase(GetData(xitCloseOnEol)) = 'true';
        xitAnyTerm:
          if lowercase(GetData(xitAnyTerm)) = 'true' then
            CurRange.OpenSymbol.BreakType := btAny
          else
            CurRange.OpenSymbol.BreakType := btTerm;
        xitDelimiterChars:
          CurRange.TermSymbols := String2Set(GetData(xitDelimiterChars));
        xitNum:
          ReadNum;
        xitCaseSensitive:
          CurRange.CaseSensitive :=
            lowercase(GetData(xitCaseSensitive)) = 'true';
        xitKW:
          begin
            NewSymbolGroup :=
              TSynSymbolGroup.Create('', CurRange.AddNewAttribs('unknown'));
            NewSymbolGroup.FName := Param;
            CurRange.AddSymbolGroup(NewSymbolGroup);
            ReadKW(NewSymbolGroup);
          end;
        xitRange:
          begin
            NewRange := TSynRange.Create;
            NewRange.Name := Param;
            CurRange.AddRange(NewRange);
            ReadRange(NewRange);
          end;
      else
        raise Exception.Create('Unexpected tag: ' +
          SL[CurTagIndex] +
          ' line ' + IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex <> xitRange then
      raise Exception.Create('Unexpected tag: /' + SL[CurTagIndex] +
        ' line ' + IntToStr(LineNumber));
  end;

begin
  Clear;

  try
    BufSize := AStream.Size;
    GetMem(Buf, BufSize);
    AStream.ReadBuffer(Buf^, BufSize);
  except
    FreeMem(Buf);
    raise;
  end;
  Sav := Buf;
  LineNumber := 0;

  SL := TStringList.Create;
  try
    BuildXMLIndexes(SL);
    if (not GetNextTag(CurTagIndex, Param, False)) or
      (CurTagIndex <> xitUniHighlighter) then
      raise Exception.Create(
        'Highlighter header tag ("<UniHighlighter>") was not found.');

    while GetNextTag(CurTagIndex, Param, True) do
    begin
      case CurTagIndex of
        xitInfo:
          ReadInfo;
        xitRange:
          begin
            Self.MainRules.Name := Param;
            ReadRange(Self.MainRules);
          end;
        xitCopyright:
          GetData(xitCopyright);
      end;
    end;
    if CurTagIndex <> xitUniHighlighter then
      raise Exception.Create('Closing tag: /' +
        SL[xitUniHighlighter] +
        ' was not found. Line ' + IntToStr(LineNumber));
  finally
    FreeMem(Sav);
    SL.Free;
  end;
  DefHighlightChange(Self);
end;

procedure TSynRange.DeleteRange(ASynRange: TSynRange);
begin
  FSynRanges.Remove(ASynRange);
  ASynRange.Free;
end;

procedure TSynRange.DeleteRange(AIndex: Integer);
begin
  TSynRange(FSynRanges[AIndex]).Free;
  FSynRanges.Delete(AIndex);
end;

procedure TSynRange.DeleteSymbolGroup(ASymbolGroup: TSynSymbolGroup);
begin
  FSymbolGroups.Remove(ASymbolGroup);
  ASymbolGroup.Free;
end;

procedure TSynRange.DeleteSymbolGroup(AIndex: Integer);
begin
  TSynSymbolGroup(FSymbolGroups[AIndex]).Free;
  FSymbolGroups.Delete(AIndex);
end;

procedure TSynUniSyn.LoadFromFile(const AFileName: string);
var
  FS: TFileStream;
begin
  if AFileName = '' then
    raise Exception.Create('FileName is empty');
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TSynUniSyn.SaveToFile(const AFileName: string);
var
  FS: TFileStream;
begin
  if AFileName = '' then
    raise Exception.Create('FileName is empty');
  FS := TFileStream.Create(AFileName, fmOpenWrite);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TSynUniSyn.DefineProperties(Filer: TFiler);
//var
//  B: Boolean;
begin
  inherited;
  //if Filer.Ancestor <> nil then
  //begin
  //  B := True;
  //end
  //else
  //  B := MainRules.RangeCount > 0;
  //Filer.DefineProperty('Syntax', @ReadSyntax, @WriteSyntax, B);
end;

(*
//### Code Folding ###
procedure TSynUniSyn.ReadCodeFolding(xml: TXMLParser;
	ParentFold: TFoldRegionItem);
var
	AddCloseKeyWord, AllowNoSubFolds, WholeWords: Boolean;
  FoldRegionType: TFoldRegionType;
  OpenKeyWord, CloseKeyWord, OpenRegExp, CloseRegExp: String;
  OpenUseRegExp, CloseUseRegExp: Boolean;
  FoldRegion: TFoldRegionItem;

  function Entities2Chars(S: String): String;
  var
    i: Integer;
  const
    sEntities: array[0..3] of String = ('&quot;', '&lt;', '&gt;', '&apos;');
    sChars: array[0..3] of String = ('"', '<', '>', '''');
  begin
    Result := S;

    for i := 0 to 3 do
      Result := StringReplace(Result, sEntities[i], sChars[i], [rfReplaceAll]);
  end;
begin
	// Stuff to shut up the compiler
  AddCloseKeyWord := False;
  AllowNoSubFolds := False;
  WholeWords := False;
  FoldRegionType := rtChar;

	while xml.Scan do
  begin
  	if xml.CurPartType = ptStartTag then
    begin
    	if Verify('FoldRegion',xml) then
      begin
      	AddCloseKeyWord := StrToBool( xml.CurAttr.Value('AddCloseKeyword') );
        AllowNoSubFolds := StrToBool( xml.CurAttr.Value('AllowNoSubFolds') );
        FoldRegionType := TFoldRegionType( StrToInt( xml.CurAttr.Value('Type') ) );
        WholeWords := StrToBool(xml.CurAttr.Value('WholeWords') );
      end
      else if Verify('SkipRegions',xml) then
      	// Iterate through skip regions
      	while xml.Scan do
        begin
        	if (xml.CurPartType = ptEmptyTag) then
          begin
            if (Verify('String',xml)) then
          	  FoldRegions.SkipRegions.Add(
                Entities2Chars(xml.CurAttr.Value('Open')),
          		  Entities2Chars(xml.CurAttr.Value('Close')),
                Entities2Chars(xml.CurAttr.Value('Escape')),
                itString)
            else if (Verify('MultiLineComment',xml)) then
          	  FoldRegions.SkipRegions.Add(
                Entities2Chars(xml.CurAttr.Value('Open')),
          		  Entities2Chars(xml.CurAttr.Value('Close')),
                '',
                itMultiLineComment)
            else if (Verify('SingleLineComment',xml)) then
          	  FoldRegions.SkipRegions.Add(
                Entities2Chars(xml.CurAttr.Value('Open')),
          		  '',
                '',
                itSingleLineComment);
          end
          else if (xml.CurPartType = ptEndTag) and (Verify('SkipRegions',xml)) then
            Break;
        end
      else if Verify('SubFoldRegions',xml) then
      begin
      	if ParentFold = nil then
        	FoldRegion := FoldRegions.Add(FoldRegionType, AddCloseKeyWord,
          	AllowNoSubFolds, WholeWords, PChar(OpenKeyWord), PChar(CloseKeyWord),
            ParentFold)
        else
        	FoldRegion := ParentFold.SubFoldRegions.Add(FoldRegionType,
          	AddCloseKeyWord, AllowNoSubFolds, WholeWords, PChar(OpenKeyWord),
          	PChar(CloseKeyWord), ParentFold);

        ReadCodeFolding(xml, FoldRegion);
      end;
    end
    else if xml.CurPartType = ptEmptyTag then
    begin
    	if Verify('Open',xml) then
      begin
      	OpenKeyWord := xml.CurAttr.Value('Keyword');
        OpenRegExp := xml.CurAttr.Value('RegExp');
        OpenUseRegExp := StrToBool( xml.CurAttr.Value('UseRegExp') );
      end
      else if Verify('Close',xml) then
      begin
      	CloseKeyWord := xml.CurAttr.Value('Keyword');
        CloseRegExp := xml.CurAttr.Value('RegExp');
        CloseUseRegExp := StrToBool( xml.CurAttr.Value('UseRegExp') );
      end;
    end
    else if (xml.CurPartType = ptEndTag)
    and ((xml.CurName = 'SubFoldRegions') or (xml.CurName = 'CodeFolding')) then
    	Exit;
  end;
end;
//### End Code Folding ###
*)

procedure TSynUniSyn.ReadSyntax(Reader: TReader);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Reader.ReadString);
  try
    SS.Position := 0;
    LoadFromStream(SS);
  finally
    SS.Free;
  end;
end;

procedure TSynUniSyn.WriteSyntax(Writer: TWriter);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    SaveToStream(SS);
    SS.Position := 0;
    Writer.WriteString(SS.DataString);
  finally
    SS.Free;
  end;
end;

function TSynUniSyn.GetIdentChars: TSynIdentChars;
begin
  Result := [#32 .. #255] - FCurrentRule.TermSymbols;
end;

function TSynUniSyn.GetSampleSource: string;
begin
  Result := FInfo.Sample.Text;
end;

procedure TSynUniSyn.SetSampleSource(Value: string);
begin
  FInfo.Sample.Text := Value;
end;
{$ENDREGION}

end.
