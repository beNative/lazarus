{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterLua.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: synhighlighterLua.pas 21053 2009-08-01 10:48:48Z martin $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A Lua language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2010-01-07)
The SynHighlighterLua implements a highlighter for Lua for the SynEdit projects.
}
unit SynHighlighterLua;

interface

{$I SynEdit.inc}

uses
  Graphics,
  SynEditHighlighter,
  SynEditTypes,
  SysUtils,
  Classes;

const
  ALPHA_CHARS = ['_', 'a'..'z', 'A'..'Z'];
  IDENTIFIER_CHARS = ['0'..'9'] + ALPHA_CHARS;
  SYNS_FilterLua =  'Lua Files (*.lua)|*.lua';
  SYNS_LangLua = 'Lua';

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword, tkTrippleQuotedString,
    tkSystemDefined, tkHex, tkOct, tkFloat, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3 //this is to indicate if a string is made multiline by backslash char at line end (as in C++ highlighter)
                );

  TProcTableProc = procedure of object;

type

  { TSynLuaSyn }

  TSynLuaSyn = class(TSynCustomHighLighter)
  private
    fStringStarter: char;  // used only for rsMultilineString3 stuff
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;

    fStringAttri: TSynHighlighterAttributes;
    fDocStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonKeyAttri: TSynHighlighterAttributes;
    fSystemAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;

    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure UnicodeStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure String3Proc;
    procedure StringEndProc(EndChar:char);
    procedure UnknownProc;
    procedure MakeMethodTables;

  protected
    Run: LongInt;
    fStringLen: Integer;

    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IdentKind(MayBe: PChar): TtkTokenKind; virtual;
    function GetKeywordIdentifiers: TStringList; virtual;
    property TokenID: TtkTokenKind read FTokenID;

  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
                      LineNumber: Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    property IdentChars;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property Keywords: TStringlist read FKeywords;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
    write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
    write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NonKeyAttri: TSynHighlighterAttributes read fNonKeyAttri
      write fNonKeyAttri;
    property SystemAttri: TSynHighlighterAttributes read fSystemAttri
      write fSystemAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
    write fNumberAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property OctalAttri: TSynHighlighterAttributes read fOctalAttri
      write fOctalAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
    write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
    write fStringAttri;
    property DocStringAttri: TSynHighlighterAttributes read fDocStringAttri
      write fDocStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
    write fSymbolAttri;
    property ErrorAttri: TSynHighlighterAttributes read fErrorAttri
      write fErrorAttri;
  end;

implementation

uses
  SynEditStrConst;

var
  GlobalKeywords: TStringList;

function TSynLuaSyn.GetKeywordIdentifiers: TStringList;
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 22;
  KEYWORDSIdents: array [1..KEYWORDCOUNT] of string =

    (
    'and',
    'break',
    'do',
    'elseif',
    'else',
    'end',
    'false',
    'for',
    'function',
    'global',
    'if',
    'in',
    'local',
    'nil',
    'not',
    'or',
    'repeat',
    'return',
    'then',
    'true',
    'until',
    'while'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 142;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of string =
    (

    'add',
//    'sub',
    'mul',
    'div',
    'mod',
//    'pow',
    'unm',
//    'concat',
//    'len',
    'eq',
    'lt',
    'le',
    'index',
    'newindex',
    'call',

    'assert',
    'collectgarbage',
    'dofile',
    'error',
    '_G',
    'getfenv',
    'getmetatable',
    'ipairs',
    'load',
    'loadfile',
    'loadstring',
    'next',
    'pairs',
    'pcall',
    'print',
    'rawequal',
    'rawget',
    'rawset',
    'select',
    'setfenv',
    'setmetatable',
    'tonumber',
    'tostring',
    'type',
    'unpack',
    '_VERSION',
    'xpcall',

    'coroutine',
    'create',
    'resume',
    'running',
    'status',
    'wrap',
    'yield',

    'module',
    'require',
    'package',
    'cpath',
    'loaded',
    'loaders',
    'loadlib',
    'path',
    'preload',
    'seeall',

    'string',
    'byte',
    'char',
    'dump',
    'find',
    'format',
    'gmatch',
    'gsub',
    'len',
    'lower',
    'match',
    'rep',
    'reverse',
    'sub',
    'upper',

    'table',
    'concat',
    'insert',
    'maxn',
    'remove',
    'sort',

    'math',
    'abs',
    'acos',
    'asin',
    'atan',
    'atan2',
    'ceil',
    'cos',
    'cosh',
    'deg',
    'exp',
    'floor',
    'fmod',
    'frexp',
    'huge',
    'ldexp',
    'log',
    'log10',
    'max',
    'min',
    'modf',
    'pi',
    'pow',
    'rad',
    'random',
    'randomseed',
    'sin',
    'sinh',
    'sqrt',
    'tan',
    'tanh',

    'io',
    'close',
    'flush',
    'input',
    'lines',
    'open',
    'output',
    'popen',
    'read',
    'tmpfile',
//    'type',
    'write',

    'file',
    'seek',
    'setvbuf',

    'os',
    'clock',
    'date',
    'difftime',
    'execute',
    'exit',
    'getenv',
//    'remove',
    'rename',
    'setlocale',
    'time',
    'tmpname',

    'debug',
//    'getfenv',
    'gethook',
    'getinfo',
    'getlocal',
//    'getmetatable',
    'getregistry',
    'getupvalue',
//    'setfenv',
    'sethook',
    'setlocal',
//    'setmetatable',
    'setupvalue',
    'traceback'
    );
var
  f: Integer;
begin
  if not Assigned (GlobalKeywords) then begin
    // Create the string list of keywords - only once
    GlobalKeywords := TStringList.Create;

    for f := 1 to KEYWORDCOUNT do
      GlobalKeywords.AddObject (KEYWORDSIdents[f],
        TObject(Ord(tkKey)));
    for f := 1 to NONKEYWORDCOUNT do
      GlobalKeywords.AddObject (NONKEYWORDS[f],
        TObject(Ord(tkNonKeyword)));
  end; // if
  Result := GlobalKeywords;
end;

function TSynLuaSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  index: Integer;
  temp: PChar;
  s: string;

begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  fToIdent := MayBe;
  temp := MayBe;
  while temp^ in IDENTIFIER_CHARS do
    Inc (temp);
  fStringLen := temp - fToIdent;

  // Check to see if it is a keyword
  SetString (s, fToIdent, fStringLen);
  if FKeywords.Find (s, index) then begin
    // TStringList is not case sensitive!
    if s <> FKeywords[index] then
      index := -1;
  end else begin
    index := -1;
  end; // if

  if index <> -1 then
    Result := TtkTokenKind (PtrInt(FKeywords.Objects[index]))

  // Check if it is a system identifier (__*__)
  else if (fStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[fStringLen-1] = '_') and (MayBe[fStringLen-2] = '_') and
     (MayBe[fStringLen-3] <> '_') then
    Result := tkSystemDefined

  // Else, hey, it is an ordinary run-of-the-mill identifier!
  else
    Result := tkIdentifier;
end;
  
procedure TSynLuaSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '-': fProcTable[I] := @CommentProc;
	  '[': fProcTable[I] := @String3Proc;
      ']': fProcTable[I] := @String3Proc;
      '&', '}', '{', ':', ',', (* ']',  '[', *) '*', '`',
      '^', ')', '(', ';', '/', '=', (* '-', *) '+', '!', '\',
      '%', '|', '~' :
        fProcTable[I] := @SymbolProc;
      #13: fProcTable[I] := @CRProc;
      '>': fProcTable[I] := @GreaterProc;
      'A'..'Q', 'S', 'T', 'V'..'Z', 'a'..'q', 's', 't', 'v'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I] := @LFProc;
      '<': fProcTable[I] := @LowerProc;
      #0: fProcTable[I] := @NullProc;
      '.', '0'..'9': fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      'r', 'R': fProcTable[I] := @PreStringProc;
      'u', 'U': fProcTable[I] := @UnicodeStringProc;
      '''': fProcTable[I] := @StringProc;
      '"': fProcTable[I] := @String2Proc;
    else
      fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynLuaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Older compilers do not ave hashed string list - so use less efficient
  //   TStringList instead - but keep it sorted
  FKeywords := TStringList.Create;
  FKeywords.Sorted := True; 
  FKeywords.Duplicates := dupError;
  FKeywords.Assign (GetKeywordIdentifiers);

  fRange := rsUnknown;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsBold,fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_XML_AttrNonReservedKeyword);
  fNonKeyAttri.Foreground := clNavy;
  fNonKeyAttri.Style := [fsBold];
  AddAttribute (fNonKeyAttri);
  fSystemAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_XML_AttrSystem);
  fSystemAttri.Style := [fsBold];
  AddAttribute (fSystemAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_XML_AttrHexadecimal);
  fHexAttri.Foreground := clBlue;
  AddAttribute(fHexAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_XML_AttrOctal);
  fOctalAttri.Foreground := clBlue;
  AddAttribute(fOctalAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_XML_AttrFloat);
  fFloatAttri.Foreground := clBlue;
  AddAttribute(fFloatAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground := clGreen;
  AddAttribute(fStringAttri);
  fDocStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_XML_AttrDocumentation);
  fDocStringAttri.Foreground := clTeal;
  AddAttribute(fDocStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Foreground := clRed;
  AddAttribute(fSymbolAttri);
  fErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_XML_AttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);
  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterLua;
end; { Create }

destructor TSynLuaSyn.Destroy;
begin
  FKeywords.Free;
  
  inherited;
end;

procedure TSynLuaSyn.SetLine(const NewValue: string;
  LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynLuaSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else
    inc(Run);
  end;
end;

procedure TSynLuaSyn.CommentProc;
begin
  fTokenID := tkComment;
  if FLine[Run+1]<>'-' then
     SymbolProc
  else begin
       inc(Run);
       while not (FLine[Run] in [#13, #10, #0]) do
             inc(Run);
  end;
end;

procedure TSynLuaSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynLuaSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
end;

procedure TSynLuaSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynLuaSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynLuaSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynLuaSyn.NumberProc;
const
  INTCHARS = ['0' .. '9'];
  HEXCHARS = ['a' .. 'f', 'A' .. 'F'] + INTCHARS;
  OCTCHARS = ['0' .. '7'];
  HEXINDICATOR = ['x', 'X'];
  LONGINDICATOR = ['l', 'L'];
  IMAGINARYINDICATOR = ['j', 'J'];
  EXPONENTINDICATOR = ['e', 'E'];
  EXPONENTSIGN = ['+', '-'];
  DOT = '.';
  ZERO = '0';

type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsFloatNeeded,
    nsHex,
    nsOct,
    nsExpFound
    );

var
  temp: Char;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      DOT: begin
        // .45
        if FLine[Run] in INTCHARS then begin
          Inc (Run);
          fTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (FLine[Run] = DOT) and (FLine[Run+1] = DOT) then
            Inc (Run, 2);
          fTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      ZERO: begin
        temp := FLine[Run];
        // 0x123ABC
        if temp in HEXINDICATOR then begin
          Inc (Run);
          fTokenID := tkHex;
          State := nsHex;
        // 0.45
        end else if temp = DOT then begin
          Inc (Run);
          State := nsDotFound;
          fTokenID := tkFloat;
        end else if temp in INTCHARS then begin
          Inc (Run);
          // 0123 or 0123.45
          if temp in OCTCHARS then begin
            fTokenID := tkOct;
            State := nsOct;
          // 0899.45
          end else begin
            fTokenID := tkFloat;
            State := nsFloatNeeded;
          end; // if
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    fTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while FLine[Run] in (IDENTIFIER_CHARS + ['.']) do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    fTokenID := tkFloat;
    // Skip e[+/-]
    if FLine[Run+1] in EXPONENTSIGN then
      Inc (Run);
    // Invalid token : 1.0e
    if not (FLine[Run+1] in INTCHARS) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (FLine[Run+1] <> DOT) or (FLine[Run+2] <> DOT);
    if Result then begin
      State := nsDotFound;
      fTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // 1234
    if temp in INTCHARS then begin
      Result := True;
    //123e4
    end else if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = DOT then begin
      Result := HandleDot;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // 1.0e4
    if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 123.45
    end else if temp in INTCHARS then begin
      Result := True;
    // 123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckFloatNeeded: Boolean;
  begin
    // 091.0e4
    if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 0912345
    end else if temp in INTCHARS then begin
      Result := True;
    // 09123.45
    end else if temp = DOT then begin
      Result := HandleDot or HandleBadNumber; // Bad octal
    // 09123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // End of number (error: Bad oct number) 0912345
    end else begin
      Result := HandleBadNumber;
    end;
  end; // CheckFloatNeeded

  function CheckHex: Boolean;
  begin
    // 0x123ABC
    if temp in HEXCHARS then begin
      Result := True;
    // 0x123ABCL
    end else if temp in LONGINDICATOR then begin
      Inc (Run);
      Result := False;
    // 0x123.45: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckOct: Boolean;
  begin
    // 012345
    if temp in INTCHARS then begin
      if not (temp in OCTCHARS) then begin
        State := nsFloatNeeded;
        fTokenID := tkFloat;
      end; // if
      Result := True;
    // 012345L
    end else if temp in LONGINDICATOR then begin
      Inc (Run);
      Result := False;
    // 0123e4
    end else if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 0123j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 0123.45
    end else if temp = DOT then begin
      Result := HandleDot;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckOct

  function CheckExpFound: Boolean;
  begin
    // 1e+123
    if temp in INTCHARS then begin
      Result := True;
    // 1e+123j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  fTokenID := tkNumber;

  temp := FLine[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := FLine[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsFloatNeeded:
        if not CheckFloatNeeded then Exit;
      nsHex:
        if not CheckHex then Exit;
      nsOct:
        if not CheckOct then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while

{
begin
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
}
end;

procedure TSynLuaSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TSynLuaSyn.String2Proc;
var fBackslashCount:integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString2;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if FLine[Run + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then inc(Run)
               end;
               inc(Run);
            end;// '\':

        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10:EXIT;
        #13:EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13 : begin
        if FLine[Run-1] = '\' then begin
          fStringStarter := '"';
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;
      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = '"');
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynLuaSyn.String3Proc;
begin
  fTokenID := tkString;

  if (FLine[Run] = '[') and (FLine[Run + 1] = '[') then begin
  
    fTokenID := tkTrippleQuotedString;
    inc(Run, 2);

    fRange:=rsMultilineString2;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        ']':
          if (fLine[Run] = ']') and (fLine[Run + 1] = ']') then begin
            fRange := rsUnKnown;
            inc(Run, 2);
            EXIT;
          end else
            inc(Run);
        #10:EXIT;
        #13:EXIT;
        else
          inc(Run);
      end;
    end;
  end
  else SymbolProc;
  
  // if FLine[Run] <> #0 then inc(Run);
end;


procedure TSynLuaSyn.PreStringProc;
var
  temp: Char;

begin
  // Handle Lua raw strings
  // r""
  temp := FLine[Run + 1];
  if temp = '''' then begin
    Inc (Run);
    StringProc;
  end else if temp = '"' then begin
    Inc (Run);
    String2Proc;
  end else begin
    // If not followed by quote char, must be ident
    IdentProc;
  end; // if
end;

procedure TSynLuaSyn.UnicodeStringProc;
begin
  // Handle Lua raw and unicode strings
  // Valid syntax: u"", or ur""
  if (FLine[Run + 1] in ['r', 'R']) and (FLine[Run + 2] in ['''', '"']) then
    // for ur, Remove the "u" and...
    Inc (Run);
  // delegate to raw strings
  PreStringProc;
end;

procedure TSynLuaSyn.StringProc;
var fBackslashCount:integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if FLine[Run + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then inc(Run)
              end;
              inc(Run);
            end;// '\':

        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13 : begin
        if FLine[Run-1] = '\' then begin
          fStringStarter := #39;
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;

      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = #39);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynLuaSyn.StringEndProc(EndChar:char);
var fBackslashCount:integer;
begin
  if fRange = rsMultilineString3 then
    fTokenID := tkString
  else
    fTokenID := tkTrippleQuotedString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  if fRange = rsMultilineString3 then begin
    repeat
      if FLine[Run]=fStringStarter then begin
        inc(Run);
        fRange:=rsUnknown;
        EXIT;
      end else if FLine[Run]='\' then ;  {The same backslash stuff above...}
          begin
             if FLine[Run + 1] = fStringStarter then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run);
             end;
           end;// if FLine[Run]...

      inc(Run);
    until (FLine[Run] in [#0, #10, #13]);
    if FLine[Run-1]<>'\' then begin
      fRange:=rsUnknown;
      EXIT;
    end;
  end else
  repeat
    if FLine[Run] = '\' then
    begin
       if FLine[Run + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then inc(Run,2);
       end;
     end;// if FLine[Run]...
    if (FLine[Run]=EndChar) and (FLine[Run+1]=EndChar) and (FLine[Run+2]=EndChar) then begin
      inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;
    inc(Run);
  until (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynLuaSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLuaSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(fStringStarter);
    else
      fProcTable[fLine[Run]];
  end;
end;

function TSynLuaSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynLuaSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynLuaSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynLuaSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer
  );
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynLuaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLuaSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonKeyword: Result := fNonKeyAttri;
    tkSystemDefined: Result := fSystemAttri;
    tkNumber: Result := fNumberAttri;
    tkHex: Result := fHexAttri;
    tkOct: Result := fOctalAttri;
    tkFloat: Result := fFloatAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkTrippleQuotedString: Result := fDocStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLuaSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynLuaSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLuaSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynLuaSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynLuaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLua;
end;

function TSynLuaSyn.GetSampleSource: string;
begin
  Result :=
    '#!/usr/local/bin/Lua'#13#10 +
    'require "vcl"'#13#10 +
    '-- Lua comment '#13#10 +
    'if string.len(str)==1 then'#13#10 +
    '    print "Usage: celsius temp1 temp2 ..."'#13#10 +
    'end';
end;

initialization
  RegisterPlaceableHighlighter(TSynLuaSyn);

finalization
  GlobalKeywords.Free;
end.

