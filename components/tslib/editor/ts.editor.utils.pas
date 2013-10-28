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

unit ts.Editor.Utils;

{ Some static editor helper routines. }

{$MODE Delphi}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Classes, SysUtils, TypInfo,

  sharedlogger;

const
  LineEnding: string = System.LineEnding;

type
  TSearchOptions = set of (
    soMatchCase,
    soWholeWord,
    soBackwards
  );

const
  AllChars = [Low(Char) .. High(Char)];
  DefaultWordBorders = AllChars - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  WhiteSpaces = [' ', #9, #10, #13];

type
  // comments
  TCommentType = (
    ctDefault,     // decide automatically
    ctNone,        // no comment
    ctPascal,      // {}
    ctDelphi,      // //
    ctTurboPascal, // (* *)
    ctCPP,         // /* */
    ctPerl,        // #
    ctSQL,         // --
    ctHtml,        // <!-- -->
    ctBatch,       // ::
    ctINI          // ;
  );

  TCommentTypes = set of TCommentType;

const
  ALineBreaks : array[TTextLineBreakStyle] of string = (
    #10,
    #13#10,
    #13
  );

function CompressSpace(
  const AString         : string;
        APreserveIndent : Boolean = True
): string;

{ Replace any number of consecutive whitespace (including newlines)
  with a single whitespace. This is nice when you have a string
  (possibly multiline) supplied by user, and you want to use this
  for some UI item (like window's caption or menu item) --- this
  "sanitizes" whitespace inside such string. }
function CompressWhiteSpace(const AString: string): string;

procedure AlignLines(
        AStrings                : TStrings;
  const AToken                  : string;
        ACompressWS             : Boolean = True;
        AInsertSpaceBeforeToken : Boolean = True;
        AInsertSpaceAfterToken  : Boolean = True
); overload;

function AlignLines(
  const AString                 : string;
  const AToken                  : string;
        ACompressWS             : Boolean;
        AInsertSpaceBeforeToken : Boolean;
        AInsertSpaceAfterToken  : Boolean
): string; overload;

function AlignLines(
  const AString                 : string;
  const AToken                  : string;
        ACompressWS             : Boolean;
        AInsertSpaceBeforeToken : Boolean;
        AInsertSpaceAfterToken  : Boolean;
        AAlignInParagraphs      : Boolean
): string; overload;

function BreakLines(
  const AString      : string;
  const AToken       : string;
        ABeforeToken : Boolean = False;
        AIndent      : Integer = 0;
        ATrimSpace   : Boolean = True;
        ABreakStyle  : TTextLineBreakStyle = tlbsCRLF
): string;

function JoinLines(
  const AString     : string;
        ABreakStyle : TTextLineBreakStyle = tlbsCRLF
): string;

function CompressLines(
  const AString         : string;
        APreserveIndent : Boolean = True
): string;

function TrimLines(
  const AString : string;
        ALeft   : Boolean;
        ARight  : Boolean;
        AIndent : Integer = 0;
        AChars  : TSysCharSet = [' ']
): string;

function QuoteLines(
  const AString    : string;
  const AQuoteChar : Char = '''';
        ATrimSpace : Boolean = False
): string;

function QuoteLinesAndDelimit(
  const AString    : string;
  const AQuoteChar : Char = '''';
  const ADelimiter : string = ', ';
        ATrimSpace : Boolean = False
): string;

function DequoteLines(
  const AString    : string;
  const AQuoteChar : Char = '''';
        ATrimSpace : Boolean = False
): string;

function CommentText(
  const AString      : string;
        ACommentType : TCommentType
): string;

function UnCommentText(
  const AString      : string;
        ACommentType : TCommentType
): string;

function FormatXML(const AXMLString: string): string;

function PascalStringOf(
  const AString    : string;
        ATrimLines : Boolean = False
): string;

procedure FilterLines(
  ASource               : TStrings;
  AInclude              : TStrings;
  AExclude              : TStrings;
  ADestination          : TStrings;
  AIncludeCaseSensitive : Boolean = True;
  AExcludeCaseSensitive : Boolean = True
);

procedure AddStringsPresentInString(
        ASource       : TStrings;
        ADest         : TStrings;
  const AFilterString : string
);

function IsXML(const AString: string): Boolean;

function IsPAS(const AString: string): Boolean;

function IsSQL(const AString: string): Boolean;

function IsLOG(const AString: string): Boolean;

function IsLFM(const AString: string): Boolean;

function IsHTML(const AString: string): Boolean;

function GuessHighlighterType(const AText: string): string;

function ChangeLineBreakStyle(
  const AString         : string;
        ALineBreakStyle : TTextLineBreakStyle
): string;

function GuessLineBreakStyle(const AString: string): TTextLineBreakStyle;

function StrToLineBreakStyle(const AString: string): TTextLineBreakStyle;

function StripChars(
  const AString      : string;
        AFirst       : Boolean;
        ALast        : Boolean;
        AIgnoreChars : TSysCharSet = [' ']
): string;

function StripMarkup(S: string): string;

function RemoveDoubles(const AString: string): string;

procedure MergeBlankStream(Stream: TStream);

function StripPASComments(const AString: string): string;

function StripLastLineEnding(const AString: string): string;

function MatchRegExpr(
  const AString        : string;
  const ARegExpr       : string;
        ACaseSensitive : Boolean = True
): Boolean; overload;

function MatchRegExpr(
  const AString        : string;
  const ARegExpr       : string;
    var AMatch         : string;
    var AMatchPos      : Integer;
        ACaseSensitive : Boolean = True
): Boolean; overload;

function MatchRegExpr2(
  const AString        : string;
  const ARegExpr       : string;
        ACaseSensitive : Boolean = True
): Boolean; overload;

function MatchRegExpr2(
  const AString        : string;
  const ARegExpr       : string;
    var AMatch         : string;
    var AMatchPos      : Integer;
        ACaseSensitive : Boolean = True
): Boolean; overload;

// Find SubString in S; do not consider case;
// this works exactly the same as the Pos function,
// except for case-INsensitivity.
function CaseInsensitivePos(const Pat, Text: string): Integer; overload;

function StrContains(
  const SubStr, Str: string;
        CaseSensitive: Boolean = True
): Boolean;

function StrPos(
  const SubStr, Str: string;
        CaseSensitive: Boolean = True
): Integer;

function PointToPos(AStrings: TStrings; APoint: TPoint): Integer;

function FileIsText(const AFilename: string): Boolean;

function WrapText(const ASource: string; AMaxCol: Integer): string;

function TabsToSpaces(
  const ASource   : string;
        ATabWidth : Integer
): string;

{ Find substring SubText within Text. Returns 0 if not found.
  Similar to a standard Pos function, with some improvements.

  @param(StartPosition Starts searching for SubText starting from this position.
    Note that the resulting position is still returned with respect
    to the string beginning. Just like standard PosEx.)

  @param(Count Looks only at Count characters from Text.
    You can say that the search is done only within Copy(Text, StartPosition, Count).)

  @param(Options Various searching options:

    @unorderedList(
      @item(soMatchCase: makes searching case-sensitive (by default,
        case is ignored, taking locale into account).)

      @item(soWholeWord: looks only for SubText occurrences surrounded
        by characters from WordBorders (or the beginning/end of Text).

        Note that, while the beginning/end of Text is always treated like a word border,
        but the mere beginning/end of the searching range (StartPosition, Count)
        is not a word border.
        For example FindPos('cat', 'foocat dog', 4, MaxInt, [soWholeWord])
        will answer 0 (not found), because the only 'cat' occurrence is not
        surrounded by default word borders.)

      @item(soBackwards: search from the end, that is return rightmost
        found occurrence.)
    )
  ) }
function FindPos(const SubText, Text: string; StartPosition, Count: integer;
  Options: TSearchOptions;
  const WordBorders: TSysCharSet = DefaultWordBorders): integer;

function IsPrefix(
  const Prefix     : string;
  const S          : string;
        IgnoreCase : Boolean = True
): Boolean;

function IsSuffix(
  const Suffix     : string;
  const S          : string;
        IgnoreCase : Boolean = True
): Boolean;

{ Removes the prefix, if it is present. More precisely, if
  IsPrefix(Prefix, S, IgnoreCase) then returns S with this prefix
  removed. Else returns S. }
function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;

{ Like PrefixRemove, but checks for and removes Suffix. }
function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;

{ Extract file extensions from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: expects FileFilter to be in the form of
  @code('xxxx|name1.ext1;name2.ext2'). Where "xxxx" is just about anything
  (it is ignored), and in fact whole "xxxx|" (with bar) may be omitted.
  The rest (after "|") is treated as a filename list, separated by semicolon ";".

  As Extensions contents, we set an array of all extensions extracted from these
  filenames. For example above, we would set Extensions to array
  with two items: @code(['.ext1', '.ext2']). }
procedure GetFileFilterExts(const FileFilter: string; Extensions: TStringList);

{ Extract file filter name, from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: if we do not see bar "|" character, then this is
  the filter name. Otherwise, everything on the right of "|" is "extensions"
  and everything on the left is "filter name".

  Additionally, if filter name ends with extensions value in parenthesis,
  they are removed. In other words, for 'Pascal files (*.pas)|*.pas',
  this will return just 'Pascal files'. The '(*.pas)' was removed
  from the filter name, because we detected this just repeats the extensions
  on the right of "|". Extensions on the right of "|" must be separated by
  semicolons, extensions within parenthesis on the left of "|" may
  be separated by semicolons ";" or colons ",". }
function GetFileFilterName(const FileFilter: string): string;

{ Search in FileFilter for the bar character "|", and return everything
  after it. This is a simple basis for GetFileFilterExts.

  If no "|" found, we return an empty string (in other words,
  file filter without "|" is treated as just a filter name, without
  any extensions). }
function GetFileFilterExtsStr(const FileFilter: string): string;

implementation

uses
  StrUtils, Dialogs, Math,

  LazFileUtils, LazUTF8,

  SynRegExpr,

  DOM, XMLRead, XMLWrite,

  ts.Core.BRRE, ts.Core.BRREUnicode,

  ts_Editor_Resources, ts.Editor.CommentStripper;

{ Will remove all spaces from the given string, but preserves one space
  between words. Spaces used to indent the given string will not be removed
  if APreserveIndent is True. }

function CompressSpace(const AString: string; APreserveIndent: Boolean): string;
var
  I  : Integer;
  N  : Integer;
  L  : Integer;
  B  : Boolean;
begin
  Result := '';
  B      := False;
  N      := 1;
  L      := Length(AString);
  if APreserveIndent then
  begin
    while (N <= L) and (AString[N] = ' ') do
    begin
      Result := Result + ' ';
      Inc(N);
    end;
  end;
  for I := N to L do
  begin
    if AString[I] = ' ' then
    begin
      // leave space as it is when it is followed by a alphanumeric
      // space is not followed by a delimiter
      if not B and ((I < L) and not (AString[I + 1] in WordDelimiters)) then
      begin
        Result := Result + ' ';
        B      := True;
      end
      else
      begin
        B := False;
      end;
    end
    else
    begin
      Result := Result + AString[I];
      B      := AString[I] in WordDelimiters;
    end;
  end;
end;

procedure AlignLines(AStrings: TStrings; const AToken: string;
  ACompressWS: Boolean; AInsertSpaceBeforeToken: Boolean;
  AInsertSpaceAfterToken: Boolean);
var
  I : Integer;
  P : Integer;
  N : Integer; // position to align AToken to
  S : string;
  T : string;
  K : string;
begin
  N := 0;
  for I := 0 to AStrings.Count - 1 do
  begin
    if ACompressWS then
      AStrings[I] := CompressSpace(AStrings[I]);
    N := Max(N, Pos(AToken, AStrings[I]));
  end;

  T := IfThen(AInsertSpaceBeforeToken, ' ', '');
  K := IfThen(AInsertSpaceAfterToken, ' ', '');
  for I := 0 to AStrings.Count - 1 do
  begin
    P := Pos(AToken, AStrings[I]);
    if P > 0 then
    begin
      S           := DupeString(' ', N - P) + T + AToken + K;
      AStrings[I] := StringReplace(AStrings[I], AToken, S, []);
    end;
  end;
end;

function AlignLines(const AString: string; const AToken: string;
  ACompressWS: Boolean; AInsertSpaceBeforeToken: Boolean;
  AInsertSpaceAfterToken: Boolean): string; overload;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    AlignLines(SL, AToken, ACompressWS, AInsertSpaceBeforeToken,
      AInsertSpaceAfterToken);
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function AlignLines(const AString: string; const AToken: string;
  ACompressWS: Boolean; AInsertSpaceBeforeToken: Boolean;
  AInsertSpaceAfterToken: Boolean; AAlignInParagraphs: Boolean): string; overload;
var
  SLIn  : TStringList;
  SLOut : TStringList;
  Par   : TStringList;
  I     : Integer;
  N     : Integer;
begin
  if AAlignInParagraphs then
  begin
    SLIn := TStringList.Create;
    try
      SLOut := TStringList.Create;
      try
        Par := TStringList.Create;
        try
          I := 0;
          SLIn.Text := AString;
          N := SLIn.Count;
          while I < N do
          begin
            Par.Clear;
            while (I < N) and (Trim(SLIn[I]) <> '') do
            begin
              Par.Add(SLIn[I]);
              Inc(I);
            end;
            AlignLines(Par, AToken, ACompressWS, AInsertSpaceBeforeToken,
              AInsertSpaceAfterToken);
            if I < N then
              Par.Add(SLIn[I]);
            SLOut.AddStrings(Par);
            Inc(I);
          end;
          Result := SLOut.Text;
        finally
          FreeAndNil(Par);
        end;
      finally
        FreeAndNil(SLOut);
      end;
    finally
      FreeAndNil(SLIn);
    end;
  end
  else
    Result := AlignLines(AString, AToken, ACompressWS, AInsertSpaceBeforeToken,
      AInsertSpaceAfterToken);
end;

function BreakLines(const AString: string; const AToken: string;
  ABeforeToken: Boolean; AIndent: Integer; ATrimSpace: Boolean;
  ABreakStyle: TTextLineBreakStyle): string;
var
  S  : string;
  BR : string;
begin
  BR := ALineBreaks[ABreakStyle];
  if ATrimSpace then
    S := Trim(AToken)
  else
    S := AToken;
  if ABeforeToken then
    S := BR + DupeString(' ', AIndent) + S
  else
    S := S + BR + DupeString(' ', AIndent);
  Result := StringsReplace(
    AString,
    [AToken],
    [S],
    [rfReplaceAll]
  );
end;

function JoinLines(const AString: string; ABreakStyle: TTextLineBreakStyle): string;
var
  BR: string;
begin
  BR := ALineBreaks[ABreakStyle];
  Result := StringsReplace(
    AString,
    [BR],
    [' '],
    [rfReplaceAll]
  );
end;

function CompressLines(const AString: string; APreserveIndent: Boolean): string;
var
  SL : TStringList;
  I  : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      SL[I] := CompressSpace(SL[I], APreserveIndent)
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TrimLines(const AString: string; ALeft: Boolean; ARight: Boolean;
  AIndent: Integer; AChars : TSysCharSet): string;
var
  SL : TStringList;
  I  : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      if ALeft and ARight then
        SL[I] := TrimSet(SL[I], AChars)
      else if ALeft then
        SL[I] := TrimLeftSet(SL[I], AChars)
      else if ARight then
        SL[I] := TrimRightSet(SL[I], AChars);
      if AIndent > 0 then
        SL[I] := DupeString(' ', AIndent) + SL[I];
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function QuoteLines(const AString: string; const AQuoteChar: Char;
  ATrimSpace: Boolean): string;
var
  SL : TStringList;
  I :  Integer;
  T :  string;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      if ATrimSpace then
        T := Trim(SL[I])
      else
        T := SL[I];
      SL[I] := AnsiQuotedStr(T, AQuoteChar);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function QuoteLinesAndDelimit(const AString: string; const AQuoteChar: Char;
  const ADelimiter: string; ATrimSpace: Boolean): string;
var
  SL : TStringList;
  I :  Integer;
  T :  string;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      if ATrimSpace then
        T := Trim(SL[I])
      else
        T := SL[I];
      if I < SL.Count - 1 then
        Result := Result + AnsiQuotedStr(T, AQuoteChar) + ADelimiter
      else
        Result := Result + AnsiQuotedStr(T, AQuoteChar);
    end;
  finally
    FreeAndNil(SL);
  end;
end;

function DequoteLines(const AString: string; const AQuoteChar: Char;
  ATrimSpace: Boolean): string;
var
  SL : TStringList;
  I :  Integer;
  T :  string;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      if ATrimSpace then
        T := Trim(SL[I])
      else
        T := SL[I];
      SL[I] := AnsiDequotedStr(T, AQuoteChar);
      if SL[I] = '''''' then
        SL[I] := '';
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function CommentText(const AString: string; ACommentType: TCommentType): string;

  procedure GetTextInfo(out ALen, ALineCount: Integer; out ALastLineEmpty: Boolean);
  var
    P : Integer;
  begin
    ALen       := Length(AString);
    ALineCount := 1;
    P         := 1;
    while P <= ALen do
      if not (AString[P] in [#10, #13]) then
      begin
        Inc(P);
      end
      else
      begin
        Inc(P);
        Inc(ALineCount);
        if (P <= ALen) and (AString[P] in [#10, #13]) and (AString[P] <> AString[P - 1]) then
          Inc(P);
      end;
    ALastLineEmpty := (ALen = 0) or (AString[ALen] in [#10, #13]);
  end;

  procedure DoCommentBlock(const AFirstLineStart, ALineStart, ALastLine: string);
  var
    OldLen, NewLen, ALineCount, OldPos, NewPos : Integer;
    ALastLineEmpty : Boolean;
  begin
    GetTextInfo(OldLen, ALineCount, ALastLineEmpty);

    NewLen := OldLen + Length(AFirstLineStart) +
      (ALineCount - 1) * Length(ALineStart);
    if ALastLineEmpty then
      Dec(NewLen, Length(ALineStart))
    else
      Inc(NewLen, Length(LineEnding));
    if (ALastLine <> '') then
    begin
      Inc(NewLen, Length(ALastLine) + Length(LineEnding));
    end;

    SetLength(Result, NewLen);
    NewPos := 1;
    OldPos := 1;

    // add first line start
    if AFirstLineStart <> '' then
    begin
      System.Move(AFirstLineStart[1], Result[NewPos], Length(AFirstLineStart));
      Inc(NewPos, Length(AFirstLineStart));
    end;
    // copy all lines and add new Alinestart
    while (OldPos <= OldLen) do
    begin
      if (not (AString[OldPos] in [#10, #13])) then
      begin
        Result[NewPos] := AString[OldPos];
        Inc(OldPos);
        Inc(NewPos);
      end
      else
      begin
        Result[NewPos] := AString[OldPos];
        Inc(OldPos);
        Inc(NewPos);
        if (OldPos <= OldLen) and (AString[OldPos] in [#10, #13]) and
          (AString[OldPos] <> AString[OldPos - 1]) then
        begin
          Result[NewPos] := AString[OldPos];
          Inc(OldPos);
          Inc(NewPos);
        end;
        // start new line
        if (ALineStart <> '') and (OldPos < OldLen) then
        begin
          System.Move(ALineStart[1], Result[NewPos], Length(ALineStart));
          Inc(NewPos, Length(ALineStart));
        end;
      end;
    end;
    if not ALastLineEmpty then
    begin
      System.Move(LineEnding[1], Result[NewPos], Length(LineEnding));
      Inc(NewPos, Length(LineEnding));
    end;
    // add last line
    if ALastLine <> '' then
    begin
      System.Move(ALastLine[1], Result[NewPos], Length(ALastLine));
      Inc(NewPos, Length(ALastLine));
      System.Move(LineEnding[1], Result[NewPos], Length(LineEnding));
      Inc(NewPos, Length(LineEnding));
    end;
    if NewPos <> NewLen + 1 then
      raise Exception.Create('CommentText ERROR: ' +
        IntToStr(NewPos - 1) + '<>' + IntToStr(NewLen));
  end;

begin
  Result := AString;
  if ACommentType = ctNone then
    Exit;
  if ACommentType = ctDefault then
    ACommentType := ctPascal;

  case ACommentType of
    ctPascal:
      DoCommentBlock('{ ', '  ', '}');
    ctDelphi:
      DoCommentBlock('// ', '// ', '');
    ctTurboPascal:
      DoCommentBlock('(* ', ' * ', ' *)');
    ctCPP:
      DoCommentBlock('/* ', ' * ', ' */');
    ctPerl:
      DoCommentBlock('# ', '# ', '');
    ctHtml:
      DoCommentBlock('<!-- ', '  ', '-->');
    ctBatch:
      DoCommentBlock(':: ', ':: ', '');
    ctINI:
      DoCommentBlock('; ', '; ', '');
  end;
end;

function UnCommentText(const AString: string; ACommentType: TCommentType): string;
begin
  Result := AString;
//  function TUnCommentExpert.ProcessSelected(Lines: TStrings): Boolean;
//
//  function RemoveFirstString(const SubString, InString: string): string;
//  var
//    Success: Boolean;
//
//    function RemoveFirstInternal(const SubString, InString: string): string;
//    var
//      SubStringPos: Integer;
//    begin
//      if StrLComp(PChar(Trim(InString)), PChar(SubString), Length(SubString)) = 0 then
//      begin
//        SubStringPos := Pos(SubString, InString);
//        if SubStringPos > 1 then
//        begin
//          Result := Copy(InString, 1, SubStringPos - 1) +
//            Copy(InString, SubStringPos + Length(SubString), MaxInt)
//        end
//        else
//          Result := Copy(InString, Length(SubString) + 1, MaxInt);
//
//        Success := True;
//      end
//      else
//        Result := InString;
//    end;
//
//  begin
//    Success := False;
//    // If spaces are to be removed, try to first kill
//    // WITH space, otherwise continue with default
//    // comment removal.
//    if InsertRemoveSpace then
//    begin
//      Result := RemoveFirstInternal(SubString + ' ', InString);
//      if Success then
//        Exit;
//    end;
//
//    Result := RemoveFirstInternal(SubString, InString);
//  end;
//
//  function RemoveLastString(const SubString, InString: string): string;
//  var
//    Success: Boolean;
//
//    function RemoveLastInternal(const SubString, InString: string): string;
//    var
//      SubStringStartPos: Integer;
//      TempString: string;
//    begin
//      TempString := TrimRight(InString);
//
//      SubStringStartPos := Length(TempString) - Length(SubString) + 1;
//
//      if SubString = Copy(TempString, SubStringStartPos, Length(SubString)) then
//      begin
//        Result := Copy(TempString, 1, SubStringStartPos - 1);
//        Success := True;
//      end
//      else
//        Result := InString;
//    end;
//
//  begin
//    Success := False;
//    // If spaces are to be removed, try to first kill
//    // WITH space, otherwise continue with default
//    // comment removal.
//    if InsertRemoveSpace then
//    begin
//      Result := RemoveLastInternal(' ' + SubString, InString);
//      if Success then
//        Exit;
//    end;
//
//    Result := RemoveLastInternal(SubString, InString);
//  end;
//
//var
//  i: Integer;
//begin
//  Assert(Assigned(Lines));
//
//  case CommentType of
//    ctSlash:
//      for i := 0 to Lines.Count - 1 do
//        Lines[i] := RemoveFirstString('//', Lines[i]);
//    ctC:
//      begin
//        Lines[0] := RemoveFirstString('(*', Lines[0]);
//        Lines[Lines.Count - 1] := RemoveLastString('*)', Lines[Lines.Count - 1]);
//      end;
//    ctCpp:
//      begin
//        Lines[0] := RemoveFirstString('/*', Lines[0]);
//        Lines[Lines.Count - 1] := RemoveLastString('*/', Lines[Lines.Count - 1]);
//      end;
//    ctPascal:
//      begin
//        Lines[0] := RemoveFirstString('{', Lines[0]);
//        Lines[Lines.Count - 1] := RemoveLastString('}', Lines[Lines.Count - 1]);
//      end;
//  end;
//  Result := True;
//end;
end;

{ REMARK: In Delphi we can use a native VCL function to do this! }

function FormatXML(const AXMLString: string): string;
var
  Doc : TXMLDocument;
  SS :  TStringStream;
begin
  SS := TStringStream.Create(AXMLString);
  try
    Doc := TXMLDocument.Create;
    try
      try
        ReadXMLFile(Doc, SS);
        SS.Position := 0;
        SS.Size     := 0;
        WriteXMLFile(Doc, SS);
        SS.Position := 0;
        Result := SS.DataString;
      except
        Result := AXMLString;
      end;
    finally
      FreeAndNil(Doc);
    end;
  finally
    FreeAndNil(SS);
  end;
end;

function PascalStringOf(const AString: string; ATrimLines: Boolean): string;
var
  SL : TStringList;
  I :  Integer;
  S :  string;
  T :  string;
begin
  S  := '';
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      if ATrimLines then
        T := Trim(SL[I])
      else
        T := SL[I];

      if T <> '' then
        T := QuotedStr(T);

      if I <> SL.Count - 1 then
      begin
        if T <> '' then
          S := S + T + ' + #13#10 + ' + #13#10
        else
          S := S + '#13#10 +' + #13#10;
      end
      else
        S := S + T;
    end;
  finally
    Result := S;
  end;
end;

procedure FilterLines(ASource, AInclude, AExclude, ADestination: TStrings;
    AIncludeCaseSensitive: Boolean; AExcludeCaseSensitive: Boolean);
var
  I : Integer;
  J : Integer;
  B : Boolean;
begin
  ADestination.Clear;
  ADestination.BeginUpdate;
  try
    for I := 0 to ASource.Count - 1 do
    begin
      J := 0;
      B := False;
      while not B and (J < AInclude.Count) do
      begin
        if AIncludeCaseSensitive then
          B := AnsiContainsStr(ASource[I], AInclude[J])
        else
          B := AnsiContainsText(ASource[I], AInclude[J]);
        Inc(J);
      end;
      J := 0;
      while not B and (J < AExclude.Count) do
      begin
        if AExcludeCaseSensitive then
          B := not AnsiContainsStr(ASource[I], AInclude[J])
        else
          B := not AnsiContainsText(ASource[I], AInclude[J]);
        Inc(J);
      end;
      if B then
      begin
        ADestination.Add(ASource[I]);
      end;
    end;
  finally
    ADestination.EndUpdate;
  end;
end;

procedure AddStringsPresentInString(ASource, ADest: TStrings; const AFilterString: string);
var
  I: Integer;
  S: string;
begin
  ADest.Clear;
  for I := 0 to ASource.Count - 1 do
  begin
    S := ASource[I];
    if StrContains(S, AFilterString) then
      ADest.Add(S);
  end;
end;

function IsXML(const AString: string): Boolean;
begin
  Result := AnsiContainsText(AString, '<?xml')
    or AnsiContainsText(AString, '<xmlroot>')
    or AnsiContainsText(AString, '<xmlns="');
end;

function IsPAS(const AString: string): Boolean;
const
  MATCH =  '(unit|program|package|library) .+;(.|\n)*end\.';
begin
  Result := MatchRegExpr(AString, MATCH, False);
end;

function IsSQL(const AString: string): Boolean;
const
  MATCH_SELECT = 'select (.|\n)*from (.|\n)*';
  MATCH_UPDATE = 'update (.|\n)*set (.|\n)*';
  MATCH_DELETE = 'delete (.|\n)*from (.|\n)*';
  MATCH_INSERT = 'insert (.|\n)*values[\s\n]+([\w,]+)';
begin
  // TODO: optimize expressions
  Result := False;
  //Result := MatchRegExpr(AString, MATCH_SELECT, False);
   //or MatchRegExpr(AString, MATCH_UPDATE, False)
   //or MatchRegExpr(AString, MATCH_INSERT, False)
   //or MatchRegExpr(AString, MATCH_DELETE, False);
end;

function IsLOG(const AString: string): Boolean;
const
  MATCH =  ' (I|P|W|E): \[.+\]';
begin
  Result := MatchRegExpr(AString, MATCH);
end;

function IsLFM(const AString: string): Boolean;
const
  MATCH = '(object|inherited) .+:.+\n(.\n)*end';
begin
  Result := MatchRegExpr(AString, MATCH);
end;

function IsHTML(const AString: string): Boolean;
begin
  Result := AnsiContainsText(AString, '<!DOCTYPE HTML')
    or AnsiContainsText(AString, '<HTML');
end;

function GuessHighlighterType(const AText: string): string;
begin
  Result := '';
  //if IsLOG(AText) then
  //  Result := HL_LOG
  //else if IsPAS(AText) then
  //  Result := HL_PAS
  //else if IsLFM(AText) then
  //  Result := HL_LFM
  //else if IsHTML(AText) then
  //  Result := HL_HTML
  //else if IsXML(AText) then
  //  Result := HL_XML
  //else if IsSQL(AText) then
  //  Result := HL_SQL;
end;

function ChangeLineBreakStyle(const AString: string;
    ALineBreakStyle: TTextLineBreakStyle): string;
var
  NewLength     : Integer;
  P             : Integer;
  StartPos      : Integer;
  Src           : PChar;
  Dest          : PChar;
  EndLen        : Integer;
  EndPos        : PChar;
  NewLineEnding : string;
begin
  case ALineBreakStyle of
    tlbsLF   : NewLineEnding := #10;
    tlbsCRLF : NewLineEnding := #13#10;
    tlbsCR   : NewLineEnding := #13;
  end;
  NewLineEnding := ALineBreaks[ALineBreakStyle];
  if AString = '' then
  begin
    Result := AString;
    Exit;
  end;
  EndLen := Length(NewLineEnding);
  NewLength := Length(AString);
  P := 1;
  while P < Length(AString) do
  begin
    if AString[P] in [#10, #13] then
    begin
      StartPos := P;
      Inc(P);
      if (AString[P] in [#10, #13]) and (AString[P] <> AString[P - 1]) then
        Inc(P);
      Inc(NewLength, EndLen - (P - StartPos));
    end
    else
      Inc(P);
  end;
  SetLength(Result, NewLength);
  Src := PChar(AString);
  Dest := PChar(Result);
  EndPos := Dest + NewLength;
  while Dest < EndPos do
  begin
    if Src^ in [#10, #13] then
    begin
      for P := 1 to EndLen do
      begin
        Dest^ := NewLineEnding[P];
        Inc(Dest);
      end;
      if (Src[1] in [#10, #13]) and (Src^ <> Src[1]) then
        Inc(Src, 2)
      else
        Inc(Src);
    end
    else
    begin
      Dest^ := Src^;
      Inc(Src);
      Inc(Dest);
    end;
  end;
end;

function GuessLineBreakStyle(const AString: string): TTextLineBreakStyle;
var
  I: Integer;
begin
  I := 1;
  Result := tlbsCRLF;
  while I <= Length(AString) do
  begin
    if AString[I] in [#10,#13] then
    begin
      if AString[I] = #10 then
        Result := tlbsLF
      else if (I < Length(AString)) and (AString[I + 1] = #10) then
        Result := tlbsCRLF
      else
        Result := tlbsCR;
      Break;
    end;
    Inc(I);
  end;
end;

function StrToLineBreakStyle(const AString: string): TTextLineBreakStyle;
begin
  if SameText(AString, 'CR') then
    Result := tlbsCR
  else if SameText(AString, 'LF') then
    Result := tlbsLF
  else
    Result := tlbsCRLF;
end;

{ Removes the first and/or last character for each line of the given text which
  is not included in the AIgnoreChars set. }

function StripChars(const AString: string; AFirst: Boolean; ALast: Boolean;
  AIgnoreChars: TSysCharSet): string;
var
  SL : TStringList;
  S  : string;
  I  : Integer;
  I1 : Integer;
  I2 : Integer;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      S  := SL[I];
      I1 := 1;
      I2 := Length(S);
      if AFirst then
      begin
        while (I1 < Length(S)) and (S[I1] in AIgnoreChars) do
          Inc(I1);
        Inc(I1);
      end;
      if ALast then
      begin
        while (I2 > I1)  and (S[I2] in AIgnoreChars) do
          Dec(I2);
        Dec(I2);
      end;
      if I1 <= I2 then
        S := Copy(S, I1, I2);
      SL[I] := S;
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function StripMarkup(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos( '<', S);      // search position of first <

  while (TagBegin > 0) do begin  // while there is a < in S
    TagEnd := Pos('>', S);              // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    Delete(S, TagBegin, TagLength);     // delete the tag
    TagBegin:= Pos( '<', S);            // search for next <
  end;

  S := StringReplace(S,'&nbsp;',' ',[rfReplaceAll]);
  S := StringReplace(S,'&amp;','&',[rfReplaceAll]);
  S := StringReplace(S,'&lt;','<',[rfReplaceAll]);
  S := StringReplace(S,'&gt;','>',[rfReplaceAll]);
  S := StringReplace(S,'&quot;','"',[rfReplaceAll]);
  Result := S;                   // give the result
end;

{ Will remove double lines after sorting each line. }

function RemoveDoubles(const AString: string): string;
var
  SL1: TStringList;
  SL2: TStringList;
  S  : string;
begin
  SL1 := TStringList.Create;
  try
    SL1.Text := AString;
    SL2 := TStringList.Create;
    SL2.Sorted := True;
    SL2.Duplicates := dupIgnore;
    try
      for S in SL1 do
        SL2.Add(S);
      Result := SL2.Text;
    finally
      FreeAndNil(SL2);
    end;
  finally
    FreeAndNil(SL1);
  end;
end;

procedure MergeBlankStream(Stream: TStream);
var
  Strings: TStringList;
  I: Integer;
  PreIsBlank, CurIsBlank: Boolean;

  function IsBlankLine(const ALine: string): Boolean;
  var
    S: string;
    I: Integer;
  begin
    Result := True;
    S := Trim(ALine);
    if S = '' then
      Exit
    else
    begin
      for I := 1 to Length(S) do
      begin
        if not (S[I] in [' ', #9, #13, #10]) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

begin
  Strings := TStringList.Create;
  try
    Stream.Position := 0;
    Strings.LoadFromStream(Stream);

    I := Strings.Count - 1;
    PreIsBlank := False;
    while I >= 0 do
    begin
      if not IsBlankLine(Strings[I]) then
        CurIsBlank := False
      else
      begin
        if PreIsBlank then
          Strings.Delete(I);
        CurIsBlank := True;
      end;
      Dec(I);
      PreIsBlank := CurIsBlank;
    end;

    Stream.Size := 0;
    Strings.SaveToStream(Stream);
  finally
    FreeAndNil(Strings);
  end;
end;

function StripPASComments(const AString: string): string;
var
  SSIn  : TStringStream;
  SSOut : TStringStream;
  CS    : TPasCommentStripper;
  C     : Char;
begin
  CS := TPasCommentStripper.Create(nil);
  try
    SSIn := TStringStream.Create('');
    try
      SSIn.WriteString(AString);
      C := #0;
      SSIn.Write(C, 1);
      ShowMessage(IntToStr(SSIn.Size));
      SSIn.Position := 0;
      SSOut := TStringStream.Create('');
      try
        CS.InStream  := SSIn;
        CS.OutStream := SSOut;
        CS.Parse;
        SSOut.Position := 0;
        Result := SSOut.ReadString(SSOut.Size);
      finally
        SSOut.Free;
      end;
    finally
      SSIn.Free;
    end;
  finally
    CS.Free;
  end;
end;

function StripLastLineEnding(const AString: string): string;
var
  S  : string;
  N1 : Integer;
  N2 : Integer;
begin
  N1 := Length(LineEnding);
  N2 := Length(AString);
  if N2 >= N1 then
  begin
    S := Copy(AString, N2 - N1 + 1, N2);
    if S = LineEnding then
      Result := Copy(AString, 1, N2 - N1)
    else
      Result := AString;
  end
  else
    Result := AString;
end;

function MatchRegExpr(const AString: string; const ARegExpr: string;
  ACaseSensitive: Boolean): Boolean;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.ModifierI := not ACaseSensitive;
    RE.Expression := ARegExpr;
    Result := RE.Exec(AString);
  finally
    RE.Free;
  end;
end;

function MatchRegExpr(const AString: string; const ARegExpr: string;
  var AMatch: string; var AMatchPos: Integer; ACaseSensitive: Boolean): Boolean;
var
  RE : TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.ModifierI := not ACaseSensitive;
    RE.Expression := ARegExpr;
    Result := RE.Exec(AString);
    if Result then
    begin
      AMatch := RE.Match[0];
      AMatchPos := RE.MatchPos[0];
    end;
  finally
    RE.Free;
  end;
end;

function MatchRegExpr2(const AString: string; const ARegExpr: string; ACaseSensitive: Boolean): Boolean;
var
  RE: TBRRERegExp;
begin
  RE := TBRRERegExp.Create(ARegExpr);
  try
    RE.Flags := RE.Flags or brrefUTF8;
    if not ACaseSensitive then
      RE.Flags := RE.Flags or brrefIGNORECASE;
    if RE.Test(AString) then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  finally
    RE.Free;
  end;
end;

function MatchRegExpr2(const AString: string; const ARegExpr: string;
  var AMatch: string; var AMatchPos: Integer; ACaseSensitive: Boolean): Boolean;
var
  RE: TBRRERegExp;
  R : TBRRERegExpCapture;
  C : TBRRERegExpCaptures;
begin
  RE := TBRRERegExp.Create(ARegExpr);
  try
    RE.Flags := RE.Flags or brrefUTF8;
    if not ACaseSensitive then
      RE.Flags := RE.Flags or brrefIGNORECASE;
    if RE.Match(AString, 1, 1, C) then
    begin
      Result := True;
      for R in C do
      begin
        AMatch := Copy(AString, R.StartCodeUnit, R.EndCodeUnit);
        AMatchPos := R.StartCodePoint;
      end;
    end
    else
    begin
      Result := False;
    end;
  finally
    RE.Free;
  end;
end;

function CaseInsensitivePos(const Pat, Text: string): Integer;
begin
  Result := Pos(AnsiUpperCase(Pat), AnsiUpperCase(Text));
end;

function StrContains(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := CaseInsensitivePos(SubStr, Str) > 0;
end;

function StrPos(const SubStr, Str: string; CaseSensitive: Boolean): Integer;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str)
  else
    Result := CaseInsensitivePos(SubStr, Str);
end;

function PointToPos(AStrings: TStrings; APoint: TPoint): Integer;
  function LineLength(const AData: string): Integer;
  begin
    Result := Length(AData) + Length(LineEnding);
  end;

var
  I: Integer;
  P: TPoint;
begin
  Result := 0;
  I      := 0;
  P      := APoint;
  while (I < (P.Y - 1)) and (I < AStrings.Count) do
  begin
    Result := Result + LineLength(AStrings[I]);
    Inc(I);
  end;
  if I < AStrings.Count then
    Result := Result + P.X;
end;

function FileIsText(const AFilename: string): Boolean;
begin
  Result := LazFileUtils.FileIsText(AFilename);
end;

function WrapText(const ASource: string; AMaxCol: Integer): string;
var
  L : Integer;
  P : Integer;
  S : Integer;
  E : Integer;
begin
  if AMaxCol <= 0 then
    Result := ASource
  else
  begin
    L:= Length(ASource);
    Result:= '';
    P:= 1;
    while P <= L do begin
      S  := P;
      while (S <= L) and (ASource[S] = ' ') do Inc(S);
      E:= Min(S + AMaxCol - 1, L);
      if (E < L) and (ASource[E+1] <> ' ') then
        while (E > S) and (ASource[E] <> ' ') do Dec(E);
      if (E = S) then
        E:= Min(S + AMaxCol - 1, L)
      else
        while (E > S) and (ASource[E] = ' ') do Dec(E);
      Result:= Result + System.copy(ASource, S, E - S + 1) + LineEnding;
      P:= E + 1;
    end;
  end;
end;

{ Convert all tabs to TabWidth number of spaces. }

function TabsToSpaces(const ASource: string; ATabWidth: Integer): string;
begin
  Result := StringReplace(
    ASource,
    #9,
    StringOfChar(#32, ATabWidth),
    [rfReplaceAll]
  );
end;

function FindPos(const SubText, Text: string; StartPosition, Count: integer;
  Options: TSearchOptions; const WordBorders: TSysCharSet): integer;

var S, SubS: string;
     i: integer;
function MatchingPos(i: integer): boolean;


  { sprawdz czy i jest dobra Position wystapienia SubS w S.
    Uwzglednij przy tym czy soWholeWord in Options, zachowuj sie zawsze
    jakby bylo soMatchCase in Options. }
  var realI: integer;
  begin
   result := false;
   if Copy(S, i, Length(SubS)) = SubS then
   begin
    if soWholeWord in Options then
    begin
     realI := i+StartPosition-1;
     if ( (realI = 1) or (Text[realI-1] in wordBorders) ) and
        ( (realI+length(subS)-1 = length(Text)) or (Text[realI+length(subS)] in WordBorders) )
     then result := true
    end else result := true;
   end;
  end;




  begin
   S := copy(Text, StartPosition, Count);
   SubS := SubText;
   if not (soMatchCase in Options) then
   begin
    S := AnsiUpperCase(S);
    SubS := AnsiUpperCase(SubS);
   end;
   result := 0;
   if soBackwards in Options then
   begin
    for i := Count-Length(SubS)+1 downto 1 do
     if MatchingPos(i) then begin result := i; break end;
   end else
   begin
    for i := 1 to Count-Length(SubS)+1 do
     if MatchingPos(i) then begin result := i; break end;
   end;
   if result > 0 then result := result+StartPosition-1;
end;

function CharPos(c: char; const s: string; Offset: Integer = 1): integer;
var i: integer;
begin
for i := Offset to length(s) do
if s[i] = c then begin result := i; exit end;
result := 0;
end;

function SEnding(const S: string; P: integer): string;
begin
 result := Copy(S, P, MaxInt)
end;

function SRight(const s: string; const rpart: integer): string;
begin
 if Length(s) < rpart then
  result := s else
  result := Copy(s, Length(s)-rpart+1, rpart);
end;

function IsPrefix(const Prefix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(Copy(S, 1, Length(Prefix)), Prefix) = 0 else
  result := AnsiCompareStr(Copy(S, 1, Length(Prefix)), Prefix) = 0;
end;

function IsSuffix(const Suffix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(SRight(S, Length(Suffix)), Suffix) = 0 else
  result := AnsiCompareStr(SRight(S, Length(Suffix)), Suffix) = 0;
end;

function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;

function SEnding(const S: string; P: integer): string;
begin
 result := Copy(S, P, MaxInt)
end;

begin
 if IsPrefix(Prefix, S, IgnoreCase) then
  Result := SEnding(S, Length(Prefix) + 1) else
  Result := S;
end;



function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;
begin
 Result := S;
 if IsSuffix(Suffix, S, IgnoreCase) then
 begin
  { doing assignment and SetLength should be a little faster
    than doing Result := Copy(S, 1, ...) }
  SetLength(Result, Length(s) - Length(Suffix));
 end;
end;

procedure GetFileFilterExts(const FileFilter: string; Extensions: TStringList);
var p, SeekPos: integer;
    ExtsStr, filemask: string;

    function NextToken(const S: string; var SeekPos: Integer;
      const TokenDelims: TSysCharSet): string;
    var
      TokStart: Integer;
    begin
      repeat
        if SeekPos > Length(s) then begin Result := ''; Exit end;
        if S[SeekPos] in TokenDelims then Inc(SeekPos) else Break;
      until false;
      TokStart := SeekPos; { TokStart := first character not in TokenDelims }

      while (SeekPos <= Length(s)) and not(S[SeekPos] in TokenDelims) do Inc(SeekPos);

      { Calculate result := s[TokStart, ... , SeekPos-1] }
      result := Copy(s, TokStart, SeekPos-TokStart);

      { We don't have to do Inc(seekPos) below. But it's obvious that searching
        for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
      Inc(SeekPos);
    end;

begin
 Extensions.Clear;
 ExtsStr := GetFileFilterExtsStr(FileFilter);
 SeekPos := 1;
 repeat
  filemask := NextToken(ExtsStr, SeekPos,[';']);
  if filemask = '' then break;
  p := CharPos('.', filemask);
  if p > 0 then
   Delete(filemask, 1, p-1) else { delete name from filemask }
   filemask := '.'+filemask; { it means there was no name and dot in filemask. So prepend dot. }
  Extensions.Add(filemask);
 until false;
end;

function GetFileFilterName(const FileFilter: string): string;
var ffLeft, ffRight: string;
    p, len: integer;



    function SReplaceChars(const s: string; FromChar, ToChar: char): string;
var i: Integer;
begin
 Result := S;
 for i := 1 to Length(Result) do
  if Result[i] = FromChar then Result[i] := ToChar;
end;

begin
 p := CharPos('|', FileFilter);
 if p = 0 then result := Trim(FileFilter) else
 begin
  ffLeft := Trim(Copy(FileFilter, 1, p-1));
  ffRight := Trim(SEnding(FileFilter, p+1));
  if ffRight = '' then
  begin
   result := ffLeft;
   { if FileFilter = 'xxx()|' then it matches to pattern 'xxx(exts)|exts'
     so we should return 'xxx', not 'xxx()'.
     This is often really useful when FileFilter was constructed in an
     automatic way (e.g. as in mine edytorek). }
   if IsSuffix('()', Result) then
   begin
    SetLength(Result, Length(Result)-2);
    { trim once again to delete rightmost whitespace (as in 'xxx ()|') }
    Result := TrimRight(Result);
   end;
  end else
  begin
   p := FindPos(ffRight, ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then
    p := FindPos(SReplaceChars(ffRight, ';', ','), ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then result := ffLeft else
   begin
    len := Length(ffRight);
    {zwieksz len tak zeby objelo biale znaki az do ')'}
    while p+len <= Length(ffLeft) do
    begin
     if ffLeft[p+len] = ')' then
      begin Inc(len); break end else
     if ffLeft[p+len] in WhiteSpaces then
      Inc(len) else
      break;
    end;
    {zmniejsz p tak zeby objelo biale znaki az do '('}
    while p-1 >= 1 do
    begin
     if ffLeft[p-1] = '(' then
      begin Dec(p); Inc(len); break end else
     if ffLeft[p-1] in WhiteSpaces then
      begin Dec(p); Inc(len) end else
      break;
    end;
    {koniec; wypieprz p, len}
    Delete(ffLeft, p, len);
    result := Trim(ffLeft);
   end;
  end;
 end;
end;

function GetFileFilterExtsStr(const FileFilter: string): string;
var p: integer;
begin
 p := CharPos('|', FileFilter);
 if p > 0 then
  result := SEnding(FileFilter, p+1) else
  result := '';
end;

function CompressWhiteSpace(const AString: string): string;

function SCharIs(const s: string; index: integer; const chars: TSysCharSet): boolean;
begin result:=(index <= Length(s)) and (s[index] in chars) end;

var
  ResultPos: Integer; { this is always next free result position }
  SPos: Integer; { this is always next unhandled AString position }
  NextSPos: Integer;
begin
  ResultPos := 1;
  SPos := 1;
  SetLength(Result, Length(AString)); { resulting string is at most as long as AString }

  if SCharIs(AString, 1, WhiteSpaces) then
  begin
    Result[1] := ' ';
    Inc(ResultPos);
    while SCharIs(AString, SPos, WhiteSpaces) do Inc(SPos);
  end;

  while SPos <= Length(AString) do
  begin
    Assert(not (AString[SPos] in WhiteSpaces));

    { read next non-white-space chunk }

    NextSPos := SPos + 1;
    while (NextSPos <= Length(AString)) and
          not (AString[NextSPos] in WhiteSpaces) do
      Inc(NextSPos);

    Move(AString[SPos], Result[ResultPos], NextSPos - SPos);

    ResultPos += NextSPos - SPos;
    SPos := NextSPos;

    { omit next white-space chunk }

    if SCharIs(AString, SPos, WhiteSpaces) then
    begin
      Result[ResultPos] := ' ';
      Inc(ResultPos);
      while SCharIs(AString, SPos, WhiteSpaces) do Inc(SPos);
    end;
  end;

  { assert we didn't do buffer overflow just now }
  Assert(ResultPos - 1 <= Length(Result));

  SetLength(Result, ResultPos - 1);
end;

end.
//  /**/ style comments : /\*[\d\D]*?\*/
// (\/\*(\s*|.*?)*\*\/)|(\/\/.*)
