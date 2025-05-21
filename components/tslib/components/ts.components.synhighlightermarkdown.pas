{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Author of this file is Andrew Haines.
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

$Id$

Known Issues:

These don't work
- Superscript/subscript
- Definition lists
- References  [id]: http://some.link
- Footnotes
- Tags that have multiple paragraphs like footnotes
- escape tags '\'
- Custom containers
- Syntax **highlighting** for code fences   ``` pascal
- Probably more.

*wouldn't it be nice if markdown worked in this comment?*
-------------------------------------------------------------------------------}
{
@abstract(A Basic Markdown language highlighter for SynEdit)
@author(Andrew Haines>)
@created(2023-01-03)
The SynHighlighterMarkdown implements a highlighter for Markdown for the SynEdit projects.
}

unit ts.Components.SynHighlighterMarkdown;

{$mode ObjFPC}{$H+}
{$ModeSwitch typehelpers}
{$define TryMultiLine}
{ $define USE_LONG_EMOJI_LIST}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter, SynEditHighlighterFoldBase, fgl;

type

  { TSynMarkdownSyn }

  TSynMarkdownSyn = class(TSynCustomFoldHighlighter)
  private type
    TAttr = (
      atText,
      atH1,
      atH2,
      atH3,
      atUnorderedList,
      atItalic,
      atBold,
      atBlockQuote,
      atCode,
      atFencedCode,
      atStrikethrough,
      atHighlight,
      atStrongBold,
      atHorizontalRule,
      atEmoji,
      atOrderedList,
      atBoldAlt,
      atIndentedCode,
      atHeaderAlt,
      atOpenParen,
      atCloseParen,
      atOpenBracket,
      atCloseBracket,
      atTable
    );
    TPairKind = (pkNone, pkMatch, pkOpen, pkClose, pkMany);
    TAttrs = set of TAttr;

    TRange = (rCode, rBlank, rIndentedCode, rTable);
    TRangeSet = set of TRange;

    { TTokenRec }

    TTokenRec = object
      Token: String;      // the symbol i.e '**', '---' etc
      PairKind: TPairKind;    // a matched pair. there must be two of this token to be valid
      FirstOnly: Boolean; // may only be the first token on a line. sometimes whitespace is ok
      IsVarLength: Boolean; // at least Length(Token) chars long but longer is also allowed
      IsMultiline: Boolean; // can span multiple lines
      MatchFor: TAttr;   // atText for disabled. otherwise it's a pair but this symbol is the end for another one
      PrevToken: TAttr;
      RequirePrev: TRangeSet;
      Attr: TAttr; // keep as last field and it can be ommitted in the consts. it's set at initialization
      function IsPair: Boolean;
      function IsMatchFor(AAttr: TAttr): Boolean;
    end;

  const
    cTokens: array [TAttr] of TTokenRec = (
      (Token:  #255  ; PairKind: pkNone;  FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText), // everything that isn't something else. text
      (Token:  '#'   ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '##'  ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '###' ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: True;  IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '-'   ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '*'   ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '**'  ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '>'   ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: True;  IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '`'   ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '```' ; PairKind: pkMatch; FirstOnly: True;  IsVarLength: False; IsMultiline: True;  MatchFor: atText; PrevToken: atText),
      (Token:  '~~'  ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '=='  ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '***' ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  '---' ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: True;  IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  ':'   ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token:  #255  ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText), // ordered list. 1. 2. 3. etc. No specific keyword
      (Token:  '__'  ; PairKind: pkMatch; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token: '    ' ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: True;  IsMultiline: False; MatchFor: atText; PrevToken: atText; RequirePrev: [rIndentedCode, rBlank]), // code indent 4+ spaces
      (Token: '====' ; PairKind: pkNone;  FirstOnly: True;  IsVarLength: True;  IsMultiline: False; MatchFor: atText; PrevToken: atText),
      (Token: '('    ; PairKind: pkOpen;  FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atCloseParen; PrevToken: atCloseBracket),
      (Token: ')'    ; PairKind: pkClose; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atOpenParen; PrevToken: atText),
      (Token: '['    ; PairKind: pkOpen;  FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atCloseBracket; PrevToken: atText),
      (Token: ']'    ; PairKind: pkClose; FirstOnly: False; IsVarLength: False; IsMultiline: False; MatchFor: atOpenBracket; PrevToken: atText),
      (Token: '|'    ; PairKind: pkMany;  FirstOnly: True;  IsVarLength: False; IsMultiline: False; MatchFor: atText; PrevToken: atText; RequirePrev: [])
    );

    SpecialChars: TCharArray = (
      '#', '*', '>', '`', '=', '-','[',']','{','}', '^', '|', '~', ':', '_', '+', ' ','(',')'
    );
    cHeaderAttr = [atH1, atH2, atH3, atHeaderAlt];
    cSubAttrs = [atBold, atItalic, atStrongBold, atBoldAlt, atCode, atHighlight, atEmoji];

  type

    { TMarkdownRange }

    TMarkdownRange = class(TSynCustomHighlighterRange)
    private

    public
      Range: TRangeSet;
      procedure Clear; override;
      function Compare(ACompareTo: TSynCustomHighlighterRange): integer; override;
      procedure Assign(Src: TSynCustomHighlighterRange); override;
      function MaxFoldLevel: Integer; override;
    end;

    TAttrInstance = Class
    private
      FText: PChar;
    public
      Attr: TAttr;
      Start: PChar;
      Length: Integer;
      Matched: Boolean;
      function IsWhitespace: Boolean;
      function Text: String;
      constructor Create(AAttr: TAttr; AStart: PChar; ALength: Integer);
      function NeedsMatch: Boolean;
    end;

  private
    FActiveAttrs: TAttrs;
    FAttrs: Array[TAttr] of TSynHighlighterAttributes;
    FLineTokens: specialize TFPGObjectList<TAttrInstance>;
    FCurrentAttr: TSynHighlighterAttributes; // a composite
    function GetAttr(AIndex: TAttr): TSynHighlighterAttributes;
    function PrevTokensAreWhiteSpaceOrThis(AToken: TAttr): Boolean;
    procedure SetAttr(AIndex: TAttr; AValue: TSynHighlighterAttributes);
    procedure TokenizeLine;
    function Token: TAttrInstance;
    function PrevToken: TAttr;
    procedure AddCodeFoldPoint(AToken: TAttrInstance);
  protected
    FRange: TRangeSet;
    FTokenIndex: Integer;
    FLineNumber: Integer;
    FLineText: String;
  public
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    class function GetLanguageName: string; override;
    function GetTokenKind: integer; override;
    class function Markdown_GetSampleSource: string;
    function GetSampleSource: string; override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    /// GetToken Attribute
    function  GetTokenAttribute: TSynHighlighterAttributes; override;

    function  SynRange: TMarkdownRange;
  public
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsFilterStored: boolean; override;
  published
    property TextAttr: TSynHighlighterAttributes index atText read GetAttr write SetAttr;
    property H1Attr: TSynHighlighterAttributes index atH1 read GetAttr write SetAttr;
    property H2Attr: TSynHighlighterAttributes index atH2 read GetAttr write SetAttr;
    property H3Attr: TSynHighlighterAttributes index atH3 read GetAttr write SetAttr;
    property BlockquoteAttr: TSynHighlighterAttributes index atBlockQuote read GetAttr write SetAttr;
    property OrderedListAttr: TSynHighlighterAttributes index atOrderedList read GetAttr write SetAttr;
    property UnorderedListAttr: TSynHighlighterAttributes index atUnorderedList read GetAttr write SetAttr;
    property CodeBlockAttr: TSynHighlighterAttributes index atFencedCode read GetAttr write SetAttr;
    property HighlightAttr: TSynHighlighterAttributes index atHighlight read GetAttr write SetAttr;
    property HorizontalRuleAttr: TSynHighlighterAttributes index atHorizontalRule read GetAttr write SetAttr;
    property EmojiAttr: TSynHighlighterAttributes index atEmoji read GetAttr write SetAttr;
  end;

implementation
uses
  TypInfo, SynEditStrConst;

resourcestring
  SYNS_FilterMarkdown           =  'Markdown Files (*.md)|*.md';

 // Currently the language names are used to identify the language
 // ToDo: create translation table
const
   SYNS_LangMarkdown             =  'Markdown text';

type

  { TCharArrayHelper }

  TCharArrayHelper = type helper for TCharArray
    function Contains(C: Char): Boolean;
  end;


operator and (A, B: TSynMarkdownSyn.TAttrs): TSynMarkdownSyn.TAttrs;
var
  e: TSynMarkdownSyn.TAttr;
begin
  Result := [];
  for e in A do
    if e in B then
      Include(Result,e);
end;

operator and (A, B: TSynMarkdownSyn.TRangeSet): TSynMarkdownSyn.TRangeSet;
var
  e: TSynMarkdownSyn.TRange;
begin
  Result := [];
  for e in A do
    if e in B then
      Include(Result,e);
end;


{ TSynMarkdownSyn.TMarkdownRange }

procedure TSynMarkdownSyn.TMarkdownRange.Clear;
begin
  inherited Clear;
  Range := [];
end;

function TSynMarkdownSyn.TMarkdownRange.Compare(
  ACompareTo: TSynCustomHighlighterRange): integer;
var
  lCompareTo: TSynMarkdownSyn.TMarkdownRange;
  lHas: Integer = 0;
  lCompare: Integer = 0;
  R: TRange;
begin
  Result:=inherited Compare(ACompareTo);
  if Result <> 0 then
    Exit;
  lCompareTo := TSynMarkdownSyn.TMarkdownRange(ACompareTo);
  if lCompareTo.Range <> Range then
  begin
    for R in TRange do
    begin
      lHas+= ((1 shl (Ord(r)+1)) * Ord(R in Range));
      lCompare+=((1 shl (Ord(r)+1)) * Ord(R in lCompareTo.Range));
    end;
    Result := lHas-lCompare;
  end;
end;

procedure TSynMarkdownSyn.TMarkdownRange.Assign(Src: TSynCustomHighlighterRange  );
begin
  inherited Assign(Src);
  if Assigned(Src) then
    Range := TSynMarkdownSyn.TMarkdownRange(Src).Range
  else
    Range := [];
end;

function TSynMarkdownSyn.TMarkdownRange.MaxFoldLevel: Integer;
begin
  Result:=inherited MaxFoldLevel;
end;

{ TSynMarkdownSyn.TTokenRec }

function TSynMarkdownSyn.TTokenRec.IsPair: Boolean;
begin
  Result := PairKind in [pkMatch, pkOpen, pkClose];
end;

function TSynMarkdownSyn.TTokenRec.IsMatchFor(AAttr: TAttr): Boolean;
begin
  Result :=  ((PairKind = pkMatch) and (Attr = AAttr))
           or((PairKind in [pkOpen, pkClose]) and (MatchFor = AAttr));
end;

{ TSynMarkdownSyn.TAttrInstance }

function TSynMarkdownSyn.TAttrInstance.IsWhitespace: Boolean;
var
  i: Integer;
begin
  Result := True;
  i := 0;
  while Result and (i < Length) do
  begin
    Result := Start[i] in [' ', #9];
    Inc(i);
  end;
end;

function TSynMarkdownSyn.TAttrInstance.Text: String;
begin
  Result := Copy(Start, 0, Length);
end;

constructor TSynMarkdownSyn.TAttrInstance.Create(AAttr: TAttr; AStart: PChar;
  ALength: Integer);
begin
  Attr:=AAttr;
  Start:=AStart;
  Length:=ALength;
end;

function TSynMarkdownSyn.TAttrInstance.NeedsMatch: Boolean;
begin
  Result := cTokens[Attr].PairKind in [pkMatch, pkClose, pkOpen];
end;

{ TCharArrayHelper }

function TCharArrayHelper.Contains(C: Char): Boolean;
var
  i: Integer;
begin
  for i := Low(Self) to High (Self) do
    if Self[i] = C then
      Exit(True);
  Result := False;
end;

{ TSynMarkdownSyn }

function TSynMarkdownSyn.GetAttr(AIndex: TAttr): TSynHighlighterAttributes;
begin
  Result := FAttrs[AIndex];
end;

procedure TSynMarkdownSyn.SetAttr(AIndex: TAttr;
  AValue: TSynHighlighterAttributes);
begin
  FAttrs[AIndex].Assign(AValue);
end;

procedure TSynMarkdownSyn.TokenizeLine;
   function IsToken(const AText: String; out AAttr: TAttr): Boolean;
   var
     lAttr: TAttr;
     FirstChar, C: Char;
     IsSameChar: Boolean = True;
   begin
     AAttr:= atText;
     // exact matches first
     for lAttr in TAttr do
     begin
       if (AText = cTokens[lAttr].Token) and ((cTokens[lAttr].RequirePrev = []) or ((cTokens[lAttr].RequirePrev and FRange) <> [])) then
       begin
         AAttr:=lAttr;
         Exit(True);
       end;
     end;
     FirstChar := AText[1];

     for C in AText do
     begin
       IsSameChar := IsSameChar and (C = FirstChar);
       if not IsSameChar then break;
     end;

     if IsSameChar then
     begin // check for var length
       for lAttr in TAttr do
       begin
         if (cTokens[lAttr].IsVarLength and (Pos(cTokens[lAttr].Token, AText) > 0))
         and ((cTokens[lAttr].RequirePrev = []) or ((cTokens[lAttr].RequirePrev and FRange) <> [])) then
         begin
           AAttr:=lAttr;
           Exit(True);
         end;
       end;
     end;
     if AText = '+' then
     begin
       AAttr:=atUnorderedList;
       Exit(True);
     end;
     if AText = '_' then
     begin
       AAttr:=atItalic;
       Exit(True);
     end;
     Result := False;
   end;
var
  lAttr: TAttr = atText;
  lPos: Integer = 1;
  l, lLen, i, j, lCachedLen: Integer;
  C: Char;
  lToken: String;
  lTokenInstance: TAttrInstance;
  lLastAttr, lInternalAttr: TAttr;
  lFoundIndent: Boolean = False;
  lFoundTable: Boolean = False;
  lIsHeaderCell: Boolean;

  function ReadLineUntil(AChars: TCharArray): Boolean;
  begin
    lLen := 0;
    while (lPos+lLen <= l) and not(AChars.Contains(FLineText[lPos+lLen])) do
      Inc(lLen);

    Result := not (lPos+lLen >= l) or (AChars.Contains(FLineText[lPos+lLen]));
    //WriteLn('Found text: "', Copy(FLineText, lPos, lLen),'" Result: ',Result);
  end;

  // uses lPos and lLen. does not modify lPos.
  // if OnlyAllow is assigned it reads until it finds a different char or until the line ends
  // if OnlyAllow is empty is scans for SpecialChars. If it finds one it reads until the char changes
  // for example it finds '#' in the string '### test'. it will read until it encounters ' '
  function ReadNextToken(AOnlyAllow: TCharArray = []): Boolean;
  var c: char;
  begin
    lLen := 0;
    while (lPos+lLen <= l) do
    begin
      if (Length(AOnlyAllow) > 0) then
      begin
        C := FLineText[lPos+lLen];
        if AOnlyAllow.Contains(FLineText[lPos+lLen]) then
        begin
          Inc(lLen);
          continue;
        end
        else
          break;
      end;

      if SpecialChars.Contains(FLineText[lPos+lLen])  then
      begin
        if lLen > 0 then
           Break;
        C := FLineText[lPos+lLen];
        while (lPos+lLen <= l) and (FLineText[lPos+lLen] = C) do
        begin
          Inc(lLen);
        end;
        break;
      end
      else
        Inc(lLen);
    end;
//    WriteLn('Found: ', Copy(FLineText, lPos, lLen));
    Result := lLen > 0;
  end;
  procedure TryAddEmoji(AAttr: TAttr);
  var
    C: Char;
  begin
    if (AAttr = atEmoji) and (lPos < l) then
    begin
      lLen := 0;
      C := FLineText[lPos];
      while (lPos+lLen < l) and (C in ['0'..'9', 'a'..'z','_','+','-']) do
      begin
        Inc(lLen);
        C := FLineText[lPos+lLen];
      end;
      if (lLen > 0) and (lPos+lLen <= l) and (FLineText[lPos+lLen] = ':') then
      begin
        FLineTokens.Add(TAttrInstance.Create(atText, @FLineText[lPos], lLen));
        Inc(lPos, lLen);
        FLineTokens.Add(TAttrInstance.Create(atEmoji, @FLineText[lPos], 1));
        Inc(lPos, 1);
      end
      else
        FLineTokens[FLineTokens.Count-1].Attr:=atText;
    end;
  end;

begin
  FLineTokens.Clear;

  l := Length(FLineText);

  while lPos <= l do
  begin
    if (FLineTokens.Count = 1) and (FLineTokens[0].Attr = atIndentedCode) and ([rIndentedCode, rBlank] and SynRange.Range <> [])
    then
    begin
      // evertthing else is one line of text
      FLineTokens.Add(TAttrInstance.Create(atText, @FLineText[lPos], l-lPos+1));
      Exit;
    end;

    if ReadNextToken then
    begin
      lToken := Copy(FLineText, lPos, lLen);
      lLastAttr := lAttr;
      if IsToken(lToken, lAttr)
      and (cTokens[lAttr].PrevToken in [atText, lLastAttr])
      then
      begin
        FLineTokens.Add(TAttrInstance.Create(lAttr, @FLineText[lPos], lLen));
        if (FLineTokens.Count = 1) and (lAttr = atIndentedCode) and ([rIndentedCode, rBlank] and SynRange.Range <> []) then
        begin
          FLineTokens[FLineTokens.Count-1].Length:=4;
          lPos := 5;
          lLen := 4;
          Continue;
        end;
      end
      else
      begin
        // its text. maybe it's a number
        // resets lLen
        lCachedLen := lLen;
        if (FLineText[lPos] in ['0'..'9'])
        and (lPos<4)
        and ((FLineTokens.Count = 0) or ((FLineTokens.Count = 1) and FLineTokens[0].IsWhitespace))
        and ReadNextToken(['0','1','2','3','4','5','6','7','8','9'])
        and (lLen + lPos < l)
        and (FLineText[lLen+lPos] = '.') then
        begin
          Inc(lLen); // for '.'
          FLineTokens.Add(TAttrInstance.Create(atOrderedList, @FLineText[lPos], lLen));
        end
        else
        begin
          lLen := lCachedLen;
          FLineTokens.Add(TAttrInstance.Create(atText, @FLineText[lPos], lLen));
        end;
      end;
      Inc(lPos, lLen);
      // emoji's... yuck
      TryAddEmoji(lAttr);

      // if we found an open tag then scan and add everything until the close tag as text
      // this might need improvement :(
      if (lAttr <> atText) and (cTokens[lAttr].PairKind = pkOpen) then
      begin
        while ReadNextToken do
        begin
          lToken := Copy(FLineText, lPos, lLen);
          IsToken(lToken, lInternalAttr);
          // until we find the close pair, only look for these tags cSubAttrs+[cTokens[cTokens[lAttr].MatchFor].Attr
          if lInternalAttr in (cSubAttrs+[cTokens[cTokens[lAttr].MatchFor].Attr]) then
          begin
            FLineTokens.Add(TAttrInstance.Create(lInternalAttr, @FLineText[lPos], lLen));
            Inc(lPos, lLen);
            TryAddEmoji(lInternalAttr); // to cleanup : nonsense
          end
          else
          begin
            FLineTokens.Add(TAttrInstance.Create(atText, @FLineText[lPos], lLen));
            Inc(lPos, lLen);
          end;

          // we found the matching close symbol
          if lInternalAttr = cTokens[cTokens[lAttr].MatchFor].Attr then
          begin
            lAttr:=lInternalAttr;
            break;
          end;
        end;
      end
      else if (lAttr <> atText) and (cTokens[lAttr].PairKind = pkMany) then
      begin
        case lAttr of                                                               //// TABLE ////
{TABLE}   atTable:
            begin // check if it's probably a valid table.

              if (lPos > 5) or (FLineText[Length(FLineText)] <> '|') or ((FLineTokens.Count > 1) and ((FLineTokens.Count = 2) and (not(FLineTokens[0]).IsWhitespace))) then
              begin
                // not a valid line
                FLineTokens[FLineTokens.Count-1].Attr := atText;
                lFoundTable := False;
                continue;
              end;
              lFoundTable := True;
              while ReadNextToken do
              begin
                lToken := Copy(FLineText, lPos, lLen);
                IsToken(lToken, lInternalAttr);
                // until we find the header add safe string tokens and atTable
                if lInternalAttr in (cSubAttrs+[atTable]) then
                begin
                  FLineTokens.Add(TAttrInstance.Create(lInternalAttr, @FLineText[lPos], lLen));
                  Inc(lPos, lLen);
                  TryAddEmoji(lInternalAttr); // cleanup mismatched :
                end
                else // not a token '|'
                begin
                  for C in lToken do
                  begin
                    if not (C in [' ', '-', ':']) then
                    begin
                      // it's a text cell and not part of the header
                      lIsHeaderCell:=False;
                      break;
                    end
                  end;
                  if lIsHeaderCell then
                  begin
                    // add the [:] ----- [:] as atTable
                    FLineTokens.Add(TAttrInstance.Create(atTable, @FLineText[lPos], lLen));
                  end
                  else
                  begin
                    FLineTokens.Add(TAttrInstance.Create(atText, @FLineText[lPos], lLen));
                  end;
                  Inc(lPos, lLen);
                end;
              end;
              lLen := 0
              // we just added the first '|' token
            end;
        else { case}
          //raise Exception.Create('Unhandled pkMany token');
          // probably an exception os too much
        end;
      end;
    end

  end;

  for i := 0 to FLineTokens.Count -1 do
  begin
    lTokenInstance := FLineTokens[i];
    if (FLineTokens.Count > 1) and (lTokenInstance.Attr in [atHorizontalRule, atHeaderAlt]) then
      lTokenInstance.Attr := atText; // only valid if it's the only token on the line
    if cTokens[lTokenInstance.Attr].IsPair and not lTokenInstance.Matched then
    begin
      for j := i+1 to FLineTokens.Count -1 do
      begin
        if (cTokens[FLineTokens[j].Attr].IsMatchFor(lTokenInstance.Attr)) and not FLineTokens[j].Matched then
        begin
          lTokenInstance.Matched:=True;
          FLineTokens[j].Matched:=True;
          Break;
        end;
      end;

      if not lTokenInstance.Matched and ((i = 0) or ((i=1) and (FLineTokens[0].Attr <> atIndentedCode)))
      and (lTokenInstance.Text = '*')
      then
        lTokenInstance.Attr:=atUnorderedList
      else if not lTokenInstance.Matched and (lTokenInstance.Attr <> atFencedCode)then
        lTokenInstance.Attr:=atText; // a match was not found. treat as text
    end;
    if (lTokenInstance.Attr = atIndentedCode) then
    begin
      if (i < FLineTokens.Count -2) and not (FLineTokens[i+1].Attr = atText) then
        lTokenInstance.Attr:=atText
      else
      begin
        Include(FRange, rIndentedCode);
        lFoundIndent:=True;
      end;
    end;
  end;
  if (rIndentedCode in FRange) and not lFoundIndent then
    Exclude(FRange, rIndentedCode);
  if (rTable in FRange) and not lFoundTable then
    Exclude(FRange, rTable);

  // now consolidate text, skipping ' '
  i := 0;
  while i < FLineTokens.Count-2 do
  begin
    if (FLineTokens[i].Attr = atText)
    and (FLineTokens[i+1].Attr = atText)
    and (FLineTokens[i].Start[0] <>  ' ')
    and (FLineTokens[i+1].Start[0] <>  ' ')
    then
    begin
      FLineTokens[i].Length+=FLineTokens[i+1].Length;
      FLineTokens.Delete(i+1);
    end
    else
      Inc(i);
  end;
end;

function TSynMarkdownSyn.Token: TAttrInstance;
begin
  if GetEol then Exit(nil);
  Result := FLineTokens[FTokenIndex];
end;

function TSynMarkdownSyn.PrevToken: TAttr;
begin
  if FTokenIndex > 0 then
    Result := FLineTokens[FTokenIndex-1].Attr
  else
    Result := atText;
end;

procedure TSynMarkdownSyn.AddCodeFoldPoint(AToken: TAttrInstance);
begin
  StartCodeFoldBlock(Pointer(Ord(AToken.Attr)));
end;

function TSynMarkdownSyn.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  Result := TSynMarkdownSyn.TMarkdownRange;
end;

function TSynMarkdownSyn.GetRange: Pointer;
begin
  TMarkdownRange(CodeFoldRange).Range := FRange;
  Result :=inherited GetRange;
end;

procedure TSynMarkdownSyn.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  FRange := TMarkdownRange(CodeFoldRange).Range;
end;

procedure TSynMarkdownSyn.ResetRange;
begin
  inherited ResetRange;
  FRange := [];
end;

class function TSynMarkdownSyn.GetLanguageName: string;
begin
  Result:= SYNS_LangMarkdown;
end;

function TSynMarkdownSyn.GetSampleSource: string;
begin
  Result := Markdown_GetSampleSource;
end;

procedure TSynMarkdownSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FActiveAttrs:=[];
  FCurrentAttr.Assign(FAttrs[atText]);
  FTokenIndex:=-1;
  FLineText := NewValue;

  FLineNumber := LineNumber;
  TokenizeLine;
  Next;
end;

procedure TSynMarkdownSyn.Next;
begin
  if FLineTokens.Count = 0 then
    FRange+=[rBlank]
  else
    FRange-=[rBlank];

  if FTokenIndex >= FLineTokens.Count then
    Exit;

  Inc(FTokenIndex);

  if GetEol then
  begin
    Exit;
  end;

  if Token.Attr in cHeaderAttr then
  begin
    //lRange := SynRange;
    EndCodeFoldBlock();
    AddCodeFoldPoint(Token);
  end;

  if ((rCode in FRange) {or (rTable in FRange)}) and not(Token.Attr in [atFencedCode{, atTable}]) then
    Token.Attr:=atText;

  {$IFDEF TRYMULTILINE}
  if (Token.Attr = atFencedCode) and not (Token.Matched) and IsScanning then
  begin
    if not(rCode in FRange) then
      Include(FRange, rCode)
    else
      Exclude(Frange,rCode);
  end;

  if Token.Attr = atIndentedCode then
  begin
    Include(FRange, rIndentedCode);
  end;

  if Token.Attr = atTable then
  begin
    Include(FRange, rTable);
  end;

  {$ENDIF}

end;

function TSynMarkdownSyn.GetEol: Boolean;
begin
  Result := FTokenIndex >= FLineTokens.Count;
end;

procedure TSynMarkdownSyn.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := Token.Start;
  TokenLength := Token.Length;
end;




function TSynMarkdownSyn.PrevTokensAreWhiteSpaceOrThis(AToken: TAttr): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTokenIndex do
  begin
    if not (FLineTokens[i].Attr in [atText, AToken])then
      Exit
    else if (FLineTokens[i].Attr = atText) and not FLineTokens[i].IsWhitespace then
      Exit;
  end;
  Result := True;
end;

function TSynMarkdownSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  lAdd: TAttrs = [];
  lToken: TAttr;
begin
  lToken := Token.Attr;

  // hard check for atIndentedCode;
  {if FLineTokens[0].IsWhitespace
  and (Length(FLineTokens[0].Text) >= 4)
  and (rBlank in SynRange.Range)
  and not (rCode in SynRange.Range)
  then
  begin
    Result := FAttrs[atIndentedCode];
    Exit;
  end;}

  if cTokens[lToken].FirstOnly and (PrevTokensAreWhiteSpaceOrThis(lToken))
  then
  // some tokens are only valid if they are the first on a line. whitespace is ok before it.
  begin
    if lToken in [atHorizontalRule, atHeaderAlt, atTable] then // highlight this immediately and not on the following token
        Include(FActiveAttrs, lToken)
    else
      Include(lAdd, lToken);
  end
  else if not cTokens[lToken].FirstOnly then
  // the token can be any token that isn't required to be first on a line.
  begin
    if not (lToken in FActiveAttrs) and
    (not cTokens[lToken].IsPair or (Token.Matched and not (cTokens[Token.Attr].PairKind = pkClose))) then
    begin
      Include(lAdd, ltoken) // the next toke will use this
    end
    else if (lToken in FActiveAttrs) and Token.Matched then
    begin
      Exclude(FActiveAttrs, lToken); // don't format closing matched symbol
    end
    else if Token.Matched and (cTokens[Token.Attr].PairKind = pkClose) and (cTokens[Token.Attr].MatchFor in FActiveAttrs) then
    begin
      Exclude(FActiveAttrs, cTokens[Token.Attr].MatchFor);
    end;
  end;

  if (rTable in FRange) and (Token.Attr = atTable) then
    Include(FActiveAttrs, atTable)
  else
    Exclude(FActiveAttrs, atTable);

  {$IFDEF TRYMULTILINE}
  if (rCode in FRange)then
      Include(FActiveAttrs, atFencedCode);
  {$ENDIF}
  if (atIndentedCode in FActiveAttrs) then
    FCurrentAttr.Assign(FAttrs[atCode])
  else
  begin
    FCurrentAttr.Assign(FAttrs[atText]);
    if ([atH1, atHeaderAlt] and FActiveAttrs <> []) then FCurrentAttr.Assign(FAttrs[atH1])
    ;{else} if (atH2 in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atH2])
    ;{else} if (atH3 in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atH3])
    ;{else} if (atBlockQuote in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atBlockQuote])
    ;{else} if (atUnorderedList in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atUnorderedList])
    ;{else} if (atHorizontalRule in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atHorizontalRule])
    ;{else} if (atOrderedList in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atOrderedList])
    ;{else} if (atHighlight in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atHighlight])
    ;{else} if (atEmoji in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atEmoji])
    ;{else} if (atOpenParen in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atOpenParen])
    ;{else} if (atOpenBracket in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atOpenBracket])
    ;{else} if (atCode in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atCode])
    ;{else} if (atFencedCode in FActiveAttrs) and (not (Token.Attr = atFencedCode)) then FCurrentAttr.Assign(FAttrs[atFencedCode])
    ;{else} if (atTable in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atTable])
    ;{else} if (atIndentedCode in FActiveAttrs) then FCurrentAttr.Assign(FAttrs[atCode])
    ;{else}
    ;//else FCurrentAttr.Assign(FAttrs[atText]);

    if (atOpenParen in FActiveAttrs) and (PrevToken = atOpenParen) then
    begin
      //FCurrentAttr.Style:=FCurrentAttr.Style+[fsUnderline];
      FCurrentAttr.Foreground:=clNavy;
    end;

    if (rTable in FRange) and not(rTable in SynRange.Range) and (Token.Attr = atText)then
    begin
      // First Row of Table
      FCurrentAttr.Assign(FAttrs[atTable]);// := FCurrentAttr.Style + FAttrs[atBold].Style;
    end;


    if (atItalic in FActiveAttrs) then FCurrentAttr.Style := FCurrentAttr.Style + FAttrs[atItalic].Style;
    if (atBold in FActiveAttrs) then FCurrentAttr.Style := FCurrentAttr.Style + FAttrs[atBold].Style;
    if (atStrikethrough in FActiveAttrs) then FCurrentAttr.Style := FCurrentAttr.Style + FAttrs[atStrikethrough].Style;
    if (atStrongBold in FActiveAttrs) then FCurrentAttr.Style := FCurrentAttr.Style + FAttrs[atStrongBold].Style;
    if (atBoldAlt in FActiveAttrs) then FCurrentAttr.Style := FCurrentAttr.Style + FAttrs[atBoldAlt].Style;
  end;
  if (Token.Attr <> atText) and (Token.Matched) then
      FCurrentAttr.Foreground:=clLtGray;

  Result := FCurrentAttr;

  FActiveAttrs+=lAdd;
end;

function TSynMarkdownSyn.SynRange: TMarkdownRange;
begin
  Result := TMarkdownRange(CodeFoldRange);
end;

function TSynMarkdownSyn.GetToken: String;
begin
  Result := Token.Text;
end;

function TSynMarkdownSyn.GetTokenPos: Integer;
begin
  Result := Token.Start-@FLineText[1];
end;

function TSynMarkdownSyn.GetTokenKind: integer;
begin
  // Map Attribute into a unique number
  Result := ord(Token.Attr)
end;

class function TSynMarkdownSyn.Markdown_GetSampleSource: string;
begin
  Result :=
    '# Heading 1'+LineEnding+
    '## Heading 2'+LineEnding+
    'Something *italic* and **bold** and not something that is'+LineEnding+
    '~~completed~~ or ==highlighted== or ***really bold***.'+LineEnding+
    'With some code `programming = fun` (hopefully) '+LineEnding+
    '- Concise'+LineEnding+
    '- Specific'+LineEnding+
    '## Indented code'+LineEnding+LineEnding+
    '    a := b;'+LineEnding+
    '    b := 1;'+LineEnding+
    '----------------------------------------------------------'+LineEnding+
    '> Block quote'+LineEnding+
    '> Regards'+LineEnding+LineEnding+
    '| Table C1 | C2        |'+LineEnding+
    '| -------: | --------- |'+LineEnding+
    '| Data     | *text*    |';
end;

function TSynMarkdownSyn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_COMMENT: Result := FAttrs[atItalic];
    SYN_ATTR_IDENTIFIER: Result := FAttrs[atCode];
    SYN_ATTR_WHITESPACE: Result := FAttrs[atText];
    else Result := nil;
  end;
end;

constructor TSynMarkdownSyn.Create(AOwner: TComponent);
var
  lAttr: TAttr;
begin
  inherited Create(AOwner);
  FLineTokens := specialize TFPGObjectList<TAttrInstance>.Create;
  FCurrentAttr := TSynHighlighterAttributes.Create;

  for lAttr := Low(TAttr) to High(TAttr) do
  case lAttr of
    atBlockQuote:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('blockquote','blockquote');
        FAttrs[lAttr].Foreground:=clGreen;
        AddAttribute(FAttrs[lAttr]);
      end;
    atBold:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create; // bold
        //AddAttribute(FAttrs[lAttr]);
        FAttrs[lAttr].Style:=[fsBold];
      end;
    atStrongBold:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create;

        FAttrs[lAttr].Style:=[fsBold,fsItalic];
      end;
    atCode:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('code','code');
        FAttrs[lAttr].Foreground:=clRed;
        FAttrs[lAttr].Background:=TColor($f9f2f4);
        AddAttribute(FAttrs[lAttr]);
      end;
    atFencedCode:
      begin
        FAttrs[lAttr] := FAttrs[atCode];
      end;
    atH1:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('h1','h1');
        FAttrs[lAttr].Style:=[fsBold];
        FAttrs[lAttr].Background:=clSkyBlue;
        FAttrs[lAttr].Foreground:=clBlack;
        FAttrs[lAttr].FrameEdges:=sfeAround;
        AddAttribute(FAttrs[lAttr]);
      end;
    atH2:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('h2','h2');
        FAttrs[lAttr].Style:=[fsBold];
        FAttrs[lAttr].FrameEdges:=sfeAround;
        AddAttribute(FAttrs[lAttr]);
      end;
    atH3:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('h3+','h3+');
        FAttrs[lAttr].Style:=[fsBold];
        FAttrs[lAttr].FrameEdges:=sfeAround;
        AddAttribute(FAttrs[lAttr]);
      end;
    atHighlight:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('highlight','highlight');
        FAttrs[lAttr].Background:=TColor($e3f8fc);//  $fcf8e3);
        FAttrs[lAttr].Foreground:=clBlack;
        AddAttribute(FAttrs[lAttr]);
      end;
    atItalic:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create;  // Italic
        //AddAttribute(FAttrs[lAttr]);
        FAttrs[lAttr].Style:=[fsItalic];
      end;
    atStrikethrough:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create; // Strikethrough
        //AddAttribute(FAttrs[lAttr]);
        FAttrs[lAttr].Style:=[fsStrikeOut];
      end;
    atText:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('text','text');
        AddAttribute(FAttrs[lAttr]);
      end;
    atUnorderedList:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('list','list');
        FAttrs[lAttr].Foreground:=clBlue;
        AddAttribute(FAttrs[lAttr]);
      end;
    atHorizontalRule:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('horizontalrule','horizontalrule');
        FAttrs[lAttr].Background:=clSilver;
        FAttrs[lAttr].Foreground:=clSilver+1; // basically invisible
        AddAttribute(FAttrs[lAttr]);
      end;
    atEmoji:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create('emoji','emoji');
        FAttrs[lAttr].Background:=clNavy;
        FAttrs[lAttr].Foreground:=clYellow;
        AddAttribute(FAttrs[lAttr]);
      end;
    atOrderedList:
      begin
        FAttrs[lAttr] := FAttrs[atUnorderedList];
      end;
    atBoldAlt:
      begin
        FAttrs[lAttr] := FAttrs[atBold];
      end;
    atIndentedCode:
      begin
        FAttrs[lAttr] := FAttrs[atCode];  // just link it to code
      end;
    atHeaderAlt:
      begin
        FAttrs[lAttr] := FAttrs[atH1]; // link to h1
      end;
    atOpenParen:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create;  // Italic
        //AddAttribute(FAttrs[lAttr]);
        //FAttrs[lAttr].Style:=[fsItalic];
      end;
    atCloseParen:
      begin
        FAttrs[lAttr] := FAttrs[atOpenParen];
      end;
    atOpenBracket:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create;  // Bold
        //AddAttribute(FAttrs[lAttr]);
        FAttrs[lAttr].Style:=[fsBold];
      end;
    atCloseBracket:
      begin
        FAttrs[lAttr] := FAttrs[atOpenBracket];
      end;
    atTable:
      begin
        FAttrs[lAttr] := TSynHighlighterAttributes.Create;  // Bold
        //AddAttribute(FAttrs[lAttr]);
        FAttrs[lAttr].Foreground:=clNavy;//FAttrs[atCode].Foreground;
        FAttrs[lAttr].Style:=[fsBold];
      end;


  else
    // yes I know this will cause a format error too. It's good. all attributes must be defined

    raise Exception.CreateFmt('attribute not created or assigned! %s', [ GetEnumName(TypeInfo(TAttr), Ord(lAttr))]);
  end;

  SetAttributesOnChange(@DefHighlightChange);
end;

destructor TSynMarkdownSyn.Destroy;
begin
  FCurrentAttr.Free;
  FLineTokens.Free;
  inherited Destroy;
end;

function TSynMarkdownSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterMarkdown;
end;

procedure FixTokens;
var
  T: TSynMarkdownSyn.Tattr;
begin
  for T in TSynMarkdownSyn.TAttr do
    TSynMarkdownSyn.CTokens[T].Attr := T;
end;

initialization

  FixTokens;
  RegisterPlaceableHighlighter(TSynMarkdownSyn);

end.



