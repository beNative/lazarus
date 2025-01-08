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

{
    This file is part of the Free Component Library
    Copyright (c) 2010 by the Free Pascal development team

    SQL source lexical scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE DELPHI}

unit ts.Core.SQLScanner;

interface

uses
   SysUtils, Classes, Contnrs;

resourcestring
  SErrInvalidCharacter         = 'Invalid character''%s''';
  SErrOpenString               = 'string exceeds end of line';
  SErrIncludeFileNotFound      = 'Could not find include file''%s''';
  SErrIfXXXNestingLimitReached = 'Nesting of$IFxxx too deep';
  SErrInvalidPPElse            = '$ELSE without matching$IFxxx';
  SErrInvalidPPEndif           = '$ENDIF without matching$IFxxx';
  SInvalidHexadecimalNumber    = 'Invalid decimal number';
  SErrInvalidNonEqual          = 'SyntaxError:!=or!==expected';
  SBarExpected                 = '|character expected';

type

  TSQLToken = (tsqlUnknown,
   // Specials
    tsqlEOF, tsqlWhiteSpace, tsqlString, tsqlIdentifier, tsqlIntegerNumber,
    tsqlFloatNumber, tsqlComment,
    tsqlBraceOpen, tsqlBraceClose, tsqlSquareBraceOpen, tsqlSquareBraceClose,
    tsqlPlaceHolder, tsqlCOMMA, tsqlCOLON, tsqlDOT, tsqlSEMICOLON, tsqlGT,
    tsqlLT,
    tsqlPLUS, tsqlMINUS, tsqlMUL, tsqlDIV, tsqlConcatenate,
    tsqlEQ, tsqlGE, tsqlLE, tsqlNE,
   { Reserved words start here. They must be last }
    tsqlALL, tsqlAND, tsqlANY, tsqlASC, tsqlASCENDING, tsqlAVG, tsqlALTER,
    tsqlAdd, tsqlActive, tsqlAction, tsqlAs, tsqlAt, tsqlAuto, tsqlAfter,
    tsqlAdmin,
    tsqlBETWEEN, tsqlBY, tsqlBLOB, tsqlBegin, tsqlBefore,
    tsqlCOLLATE, tsqlCONTAINING, tsqlCOUNT, tsqlCREATE, tsqlCOLUMN,
    tsqlCONSTRAINT, tsqlChar, tsqlCHARACTER, tsqlCHECK, tsqlComputed,
    tsqlCASCADE, tsqlCast, tsqlCommit, tsqlConnect, tsqlCache, tsqlConditional,
    tsqlCString,
    tsqlDESC, tsqlDESCENDING, tsqlDISTINCT, tsqlDEFAULT, tsqlDELETE, tsqlDO,
    tsqlDECLARE, tsqlDROP, tsqlDomain, tsqlDecimal, tsqlDate, tsqlDatabase,
    tsqlESCAPE, tsqlEXISTS, tsqlELSE, tsqlException, tsqlExternal, tsqlExecute,
    tsqlEnd, tsqlExit, tsqlEntrypoint, tsqlExtract,
    tsqlFROM, tsqlFULL, tsqlFOREIGN, tsqlFOR, tsqlFUNCTION, tsqlFLOAT, tsqlFile,
    tsqlFreeIt,
    tsqlGenerator, tsqlGROUP, tsqlGenID, tsqlGDSCODE, tsqlGrant,
    tsqlHAVING,
    tsqlIF, tsqlIN, tsqlINNER, tsqlINSERT, tsqlINT, tsqlINTEGER, tsqlINTO,
    tsqlIS, tsqlINDEX, tsqlInactive,
    tsqlJOIN,
    tsqlKEY,
    tsqlLEFT, tsqlLIKE, tsqlLength,
    tsqlMAX, tsqlMIN, tsqlMERGE, tsqlManual, tsqlModuleName,
    tsqlNOT, tsqlNULL, tsqlNUMERIC, tsqlNChar, tsqlNATIONAL, tsqlNO, tsqlNatural,
    tsqlON, tsqlOR, tsqlORDER, tsqlOUTER, tsqlOption,
    tsqlPRIMARY, tsqlProcedure, tsqlPosition, tsqlPlan, tsqlPassword, tsqlPage,
    tsqlPages, tsqlPageSize, tsqlPostEvent, tsqlPrivileges, tsqlPublic,
    tsqlRIGHT, tsqlROLE, tsqlReferences, tsqlRollBack, tsqlRelease, tsqlretain,
    tsqlReturningValues, tsqlReturns, tsqlrevoke,
    tsqlSELECT, tsqlSET, tsqlSINGULAR, tsqlSOME, tsqlSTARTING, tsqlSUM,
    tsqlSKIP, tsqlSUBTYPE, tsqlSize, tsqlSegment, tsqlSORT, tsqlSnapShot,
    tsqlSchema, tsqlShadow, tsqlSuspend, tsqlSQLCode, tsqlSmallint,
    tSQLTABLE, tsqlTrigger, tsqlTime, tsqlTimeStamp, tsqlType, tsqlTo,
    tsqlTransaction, tsqlThen,
    tsqlUNION, tsqlUPDATE, tsqlUPPER, tsqlUNIQUE, tsqlUSER,
    tsqlValue, tsqlVALUES, tsqlVARIABLE, tsqlVIEW, tsqlVARCHAR, TSQLVARYING,
    tsqlWHERE, tsqlWITH, tsqlWHILE, tsqlWork, tsqlWhen
  );
  TSQLTokens = set of TSQLToken;

const
  FirstKeyword   = tsqlALL;
  LastKeyWord    = tsqlWhen;
  sqlComparisons = [tsqlEQ, tsqlGE, tsqlLE, tsqlNE, tsqlGT, tsqlLT, tsqlIN,
    tsqlIS,
    tsqlBETWEEN, tsqlLIKE, tsqlCONTAINING, tsqlSTARTING, tsqlNOT];
  sqlInvertableComparisons = [tsqlLIKE, tsqlCONTAINING, tsqlSTARTING, tsqlIN,
    tsqlIS, tsqlBETWEEN];

  TokenInfos: array [TSQLToken] of string = ('unknown',
       // Specials
    'EOF', 'whitespace', 'String', 'identifier', 'integer number',
    'float number', 'comment',
    '(', ')', '[', ']',
    '?', ',', ':', '.', ';', '>', '<',
    '+', '-', '*', '/', '||',
    '=', '>=', '<=', '<>',
       // Identifiers last
    'ALL', 'AND', 'ANY', 'ASC', 'ASCENDING', 'AVG', 'ALTER', 'ADD', 'ACTIVE',
    'ACTION', 'AS', 'AT', 'AUTO', 'AFTER', 'ADMIN',
    'BETWEEN', 'BY', 'BLOB', 'BEGIN', 'BEFORE',
    'COLLATE', 'CONTAINING', 'COUNT', 'CREATE', 'COLUMN', 'CONSTRAINT', 'CHAR',
    'CHARACTER', 'CHECK', 'COMPUTED', 'CASCADE', 'CAST', 'COMMIT', 'CONNECT',
    'CACHE', 'CONDITIONAL', 'CSTRING',
    'DESC', 'DESCENDING', 'DISTINCT', 'DEFAULT', 'DELETE', 'DO', 'DECLARE',
    'DROP', 'DOMAIN', 'DECIMAL', 'DATE', 'DATABASE',
    'ESCAPE', 'EXISTS', 'ELSE', 'EXCEPTION', 'EXTERNAL', 'EXECUTE', 'END',
    'EXIT', 'ENTRY_POINT', 'EXTRACT',
    'FROM', 'FULL', 'FOREIGN', 'FOR', 'FUNCTION', 'FLOAT', 'FILE', 'FREE_IT',
    'GENERATOR', 'GROUP', 'GEN_ID', 'GDSCODE', 'GRANT',
    'HAVING',
    'IF', 'IN', 'INNER', 'INSERT', 'INT', 'INTEGER', 'INTO', 'IS', 'INDEX',
    'INACTIVE',
    'JOIN',
    'KEY',
    'LEFT', 'LIKE', 'LENGTH',
    'MAX', 'MIN', 'MERGE', 'MANUAL', 'MODULE_NAME',
    'not', 'NULL', 'NUMERIC', 'NCHAR', 'NATIONAL', 'NO', 'NATURAL',
    'ON', 'OR', 'ORDER', 'OUTER', 'OPTION',
    'PRIMARY', 'PROCEDURE', 'POSITION', 'PLAN', 'PASSWORD', 'PAGE', 'PAGES',
    'PAGE_SIZE', 'POST_EVENT', 'PRIVILEGES', 'PUBLIC',
    'RIGHT', 'ROLE', 'REFERENCES', 'ROLLBACK', 'RELEASE', 'RETAIN',
    'RETURNING_VALUES', 'RETURNS', 'REVOKE',
    'SELECT', 'SET', 'SINGULAR', 'SOME', 'STARTING', 'SUM', 'SKIP', 'SUB_TYPE',
    'SIZE', 'SEGMENT', 'SORT', 'SNAPSHOT', 'SCHEMA', 'SHADOW', 'SUSPEND',
    'SQLCODE', 'SMALLINT',
    'TABLE', 'TRIGGER', 'TIME', 'TIMESTAMP', 'TYPE', 'TO', 'TRANSACTION', 'THEN',
    'UNION', 'UPDATE', 'UPPER', 'UNIQUE', 'USER',
    'VALUE', 'VALUES', 'VARIABLE', 'VIEW', 'VARCHAR', 'VARYING',
    'WHERE', 'WITH', 'WHILE', 'WORK', 'WHEN'
    );

Type
  TLineReader = class
  public
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: string; virtual; abstract;
  end;

  { TStreamLineReader }

  TStreamLineReader = class(TLineReader)
  private
    FStream : TStream;
    Buffer  : array [0 .. 1024] of Byte;
    FBufPos : Integer;
    FBufLen : Integer;
    procedure FillBuffer;

  public
    constructor Create(AStream : TStream);
    function IsEOF: Boolean; override;
    function ReadLine: string; override;
  end;

  TFileLineReader = class(TLineReader)
  private
    FTextFile : Text;
    FileOpened: Boolean;

  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: string; override;
  end;

  ESQLScannerError = class(Exception);

  { TSQLScanner }
  TSQLScannerOption = (
    soReturnComments,
    soReturnWhiteSpace,
    soBackslashEscapes,
    soNoDoubleDelimIsChar,
    soDoubleQuoteStringLiteral, // Default: single quote is string literal
    soSingleQuoteIdentifier,
    // Default: double quote is identifier. Ignored if soDoubleQuoteStringLiteral is not specified
    soBackQuoteIdentifier // Default: double quote is identifier
  );
  TSQLScannerOptions = set of TSQLScannerOption;

  TSQLScanner = class
  private
    FOptions         : TSQLScannerOptions;
    FSourceFile      : TLineReader;
    FSourceFilename  : string;
    FCurRow          : Integer;
    FCurToken        : TSQLToken;
    FCurTokenString  : string;
    FCurLine         : string;
    TokenStr         : PChar;
    FSourceStream    : TStream;
    FOwnSourceFile   : Boolean;
    FKeyWords        : TFPHashList;
    FExclude         : TStringList;
    function CommentDiv: TSQLToken;
    function DoIdentifier: TSQLToken;
    function DoMultiLineComment: TSQLToken;
    function DoNumericLiteral: TSQLToken;
    function DoSingleLineComment: TSQLToken;
    function DoStringLiteral: TSQLToken;
    function DoWhiteSpace: TSQLToken;
    function FetchLine: Boolean;
    function GetCurColumn: Integer;
    function GetExcludeKeywords: TStrings;
    function ReadUnicodeEscape: WideChar;
    procedure SetExcludeKeywords(const AValue: TStrings);
    procedure Setoptions(const AValue: TSQLScannerOptions);
    procedure ClearKeywords(Sender: TObject);

  protected
    procedure BuildKeyWords; virtual;
    procedure Error(const Msg: string); overload;
    procedure Error(const Msg: string; Args: array of Const ); overload;

  public
    constructor Create(ALineReader: TLineReader); overload;
    constructor Create(AStream : TStream); overload;
    destructor Destroy; override;
    procedure OpenFile(const AFilename: string);
    function FetchToken: TSQLToken;
    function IsEndOfLine : Boolean;
    property Options : TSQLScannerOptions read FOptions write Setoptions;
    property SourceFile: TLineReader read FSourceFile;
    property CurFilename: string read FSourceFilename;
    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurToken: TSQLToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
    property ExcludeKeywords : TStrings read GetExcludeKeywords
      write SetExcludeKeywords;
  end;

implementation

var
  IdentifierTokens   : array [FirstKeyword .. LastKeyWord] of TSQLToken;
  IdentifierTokensOK : Boolean = False;

resourcestring
  SErrUNknownToken = 'Unknown token: %s';

procedure BuildIdentifierTokens;
var
  T : TSQLToken;
begin
  For T := FirstKeyword to LastKeyWord do
    IdentifierTokens[T] := T;
end;

constructor TFileLineReader.Create(const AFilename: string);
begin
  inherited Create;
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  FileOpened := true;
end;

destructor TFileLineReader.Destroy;
begin
  if FileOpened then
    Close(FTextFile);
  inherited Destroy;
end;

function TFileLineReader.IsEOF: Boolean;
begin
  Result := EOF(FTextFile);
end;

function TFileLineReader.ReadLine: string;
begin
  ReadLn(FTextFile, Result);
end;

constructor TSQLScanner.Create(ALineReader: TLineReader);
begin
  inherited Create;
  FSourceFile := ALineReader;
  FKeyWords := TFPHashList.Create;
end;

constructor TSQLScanner.Create(AStream: TStream);
begin
  FSourceStream := AStream;
  FOwnSourceFile := true;
  Create(TStreamLineReader.Create(AStream));
end;

destructor TSQLScanner.Destroy;
begin
  if FOwnSourceFile then
    FSourceFile.Free;
  FreeAndNil(FKeyWords);
  inherited Destroy;
end;

procedure TSQLScanner.OpenFile(const AFilename: string);
begin
  FSourceFile := TFileLineReader.Create(AFilename);
  FOwnSourceFile := true;
  FSourceFilename := AFilename;
end;

procedure TSQLScanner.Error(const Msg: string);
begin
  raise ESQLScannerError.Create(Msg);
end;

procedure TSQLScanner.Error(const Msg: string; Args: array of Const );
begin
  raise ESQLScannerError.CreateFmt(Msg, Args);
end;

function TSQLScanner.FetchLine: Boolean;
begin
  if FSourceFile.IsEOF then
  begin
    FCurLine := '';
    TokenStr := nil;
    Result := false;
  end
  else
  begin
    FCurLine := FSourceFile.ReadLine;
    TokenStr := PChar(CurLine);
    Result := true;
    Inc(FCurRow);
  end;
end;

function TSQLScanner.DoWhiteSpace : TSQLToken;
begin
  Result := tsqlWhiteSpace;
  repeat
    Inc(TokenStr);
    if TokenStr[0] = #0 then
      if not FetchLine then
      begin
        FCurToken := Result;
        Exit;
      end;
  until not(TokenStr[0] in [#9, ' ']);
end;

function TSQLScanner.DoSingleLineComment : TSQLToken;
var
  TokenStart : PChar;
  Len        : Integer;
begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  while TokenStr[0] <> #0 do
    Inc(TokenStr);
  Len := TokenStr - TokenStart;
  SetLength(FCurTokenString, Len);
  if (Len > 0) then
    Move(TokenStart^, FCurTokenString[1], Len);
  Result := tsqlComment;
end;

function TSQLScanner.DoMultiLineComment : TSQLToken;
var
  TokenStart : PChar;
  Len, OLen  : Integer;
  PrevToken  : Char;
begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  FCurTokenString := '';
  OLen := 0;
  PrevToken := #0;
  while not((TokenStr[0] = '/') and (PrevToken = '*')) do
  begin
    if (TokenStr[0] = #0) then
    begin
      Len := TokenStr - TokenStart + 1;
      SetLength(FCurTokenString, OLen + Len);
      if Len > 1 then
        Move(TokenStart^, FCurTokenString[OLen + 1], Len - 1);
      Inc(OLen, Len);
      FCurTokenString[OLen] := #10;
      if not FetchLine then
      begin
        Result := tsqlEOF;
        FCurToken := Result;
        Exit;
      end;
      TokenStart := TokenStr;
      PrevToken := #0;
    end
    else
    begin
      PrevToken := TokenStr[0];
      Inc(TokenStr);
    end;
  end;
  Len := TokenStr - TokenStart - 1; // -1 for *
  SetLength(FCurTokenString, OLen + Len);
  if (Len > 0) then
  begin
    Move(TokenStart^, FCurTokenString[OLen + 1], Len);
  end;
  if TokenStr[0] <> #0 then
    Inc(TokenStr);
  Result := tsqlComment;
end;

function TSQLScanner.CommentDiv: TSQLToken;
begin
  FCurTokenString := '';
  Inc(TokenStr);
  if (TokenStr[0] = '*') then
    Result := DoMultiLineComment
  else
    Result := tsqlDIV;
end;

function TSQLScanner.ReadUnicodeEscape: WideChar;
var
  S : string;
  I : Integer;
begin
  S := '0000';
  For I := 1 to 4 do
  begin
    Inc(TokenStr);
    Case TokenStr[0] of
      '0' .. '9', 'A' .. 'F', 'a' .. 'f' :
        S[I] := Upcase(TokenStr[0]);
    else
      Error(SErrInvalidCharacter, [TokenStr[0]]);
    end;
  end;
  // Takes care of conversion... This needs improvement !!
  Result := WideChar(StrToInt('$' + S));
end;

procedure TSQLScanner.SetExcludeKeywords(const AValue: TStrings);
begin
  with ExcludeKeywords do
  begin
    Clear;
    AddStrings(AValue);
  end;
end;

procedure TSQLScanner.Setoptions(const AValue: TSQLScannerOptions);
const
  F = [soDoubleQuoteStringLiteral, soSingleQuoteIdentifier];
begin
  FOptions := AValue;
  if ((FOptions * F) = [soSingleQuoteIdentifier]) then
    Exclude(FOptions, soSingleQuoteIdentifier);
end;

procedure TSQLScanner.BuildKeyWords;
var
  I : TSQLToken;
begin
  if not IdentifierTokensOK then
    BuildIdentifierTokens;
  if FKeyWords.Count > 0 then
    FKeyWords.Clear;
  for I := FirstKeyword to LastKeyWord do
    if (not Assigned(FExclude)) or (FExclude.INdexOf(TokenInfos[I]) = -1) then
      FKeyWords.Add(TokenInfos[I], @IdentifierTokens[I]);
end;

function TSQLScanner.DoStringLiteral: TSQLToken;
var
  Delim      : Char;
  TokenStart : PChar;
  Len, OLen  : Integer;
  S          : string;
  procedure AppendBufToTokenString(DoNextToken : Boolean);
  begin
    SetLength(FCurTokenString, OLen + Len + Length(S));
    if Len > 0 then
      Move(TokenStart^, FCurTokenString[OLen + 1], Len);
    if Length(S) > 0 then
      Move(S[1], FCurTokenString[OLen + Len + 1], Length(S));
    Inc(OLen, Len + Length(S));
    if DoNextToken then
      Inc(TokenStr);
    TokenStart := TokenStr + 1;
  end;

  function CheckTokenBuf : Boolean;
  begin
    Result := (TokenStr[0] <> #0);
    if not Result then
    begin
      S := '';
      Len := TokenStr - TokenStart;
      AppendBufToTokenString(false);
      Result := FetchLine;
      TokenStart := TokenStr;
    end;
  end;

begin
  Delim := TokenStr[0];
  Inc(TokenStr);
  TokenStart := TokenStr;
  OLen := 0;
  FCurTokenString := '';
  while not(TokenStr[0] = #0) do
  begin
    if (TokenStr[0] = Delim) then
    begin
      if (not(soNoDoubleDelimIsChar in Options)) and (TokenStr[1] = Delim) then
      begin
        S := Delim;
        Len := TokenStr - TokenStart;
        AppendBufToTokenString(true);
      end
      else
        Break;
    end
    else if (TokenStr[0] = '\') and (soBackslashEscapes in Options) then
    begin
      // Save length
      Len := TokenStr - TokenStart;
      Inc(TokenStr);
      if not CheckTokenBuf then
        Error(SErrOpenString);
      // Read escaped token
      case TokenStr[0] of
        '"' :
          S := '"';
        '''' :
          S := '''';
        't' :
          S := #9;
        'b' :
          S := #8;
        'n' :
          S := #10;
        'r' :
          S := #13;
        'f' :
          S := #12;
        '\' :
          S := '\';
        '/' :
          S := '/';
        'u' :
          begin
            S := string(ReadUnicodeEscape);
          end;
      else
        Error(SErrInvalidCharacter, [TokenStr[0]]);
      end;
      AppendBufToTokenString(false);
    end;
    Inc(TokenStr);
    if not CheckTokenBuf then
      Error(SErrOpenString);
  end;
  if not CheckTokenBuf then
    Error(SErrOpenString);
  S := '';
  Len := TokenStr - TokenStart;
  AppendBufToTokenString(true);
  Result := tsqlString;
end;

function TSQLScanner.DoNumericLiteral :TSQLToken;
var
  TokenStart : PChar;
  Len        : Integer;
  isFloat    : Boolean;
begin
  TokenStart := TokenStr;
  isFloat := false;
  while true do
  begin
    Inc(TokenStr);
    case TokenStr[0] of
      'x':
        if (TokenStart[0] = '0') and ((TokenStr - TokenStart) = 1) then
        begin
          Inc(TokenStr);
          while Upcase(TokenStr[0]) in ['0' .. '9', 'A' .. 'F'] do
            Inc(TokenStr);
        end
        else
          Error(SInvalidHexadecimalNumber);
      '.':
        begin
          isFloat := true;
          if TokenStr[1] in ['0' .. '9', 'e', 'E'] then
          begin
            Inc(TokenStr);
            repeat
              Inc(TokenStr);
            until not(TokenStr[0] in ['0' .. '9', 'e', 'E', '-', '+']);
          end;
          Break;
        end;
      '0' .. '9':
        ;
      'e', 'E':
        begin
          isFloat := true;
          Inc(TokenStr);
          if TokenStr[0] in ['-', '+'] then
            Inc(TokenStr);
          while TokenStr[0] in ['0' .. '9'] do
            Inc(TokenStr);
          Break;
        end;
    else
      Break;
    end;
  end;
  Len := TokenStr - TokenStart;
  SetLength(FCurTokenString, Len);
  if (Len > 0) then
    Move(TokenStart^, FCurTokenString[1], Len);
  if isFloat then
    Result := tsqlFloatNumber
  else
    Result := tsqlIntegerNumber;
end;

function TSQLScanner.DoIdentifier : TSQLToken;
var
  TokenStart:PChar;
  Len       : Integer;
  S         : ShortString;
  P         : ^TSQLToken;
begin
  Result := tsqlIdentifier;
  TokenStart := TokenStr;
  repeat
    Inc(TokenStr);
    if (TokenStr[0] = '\') and (TokenStr[1] = 'u') then
      until not(TokenStr[0] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '$']);
    Len := (TokenStr - TokenStart);
    SetLength(FCurTokenString, Len);
    if Len > 0 then
      Move(TokenStart^, FCurTokenString[1], Len);
    S := UpperCase(FCurTokenString);
    //S := FCurTokenString;
  // Check if this is a keyword or identifier
  // !!!: Optimize this!
    if FKeyWords.Count = 0 then
      BuildKeyWords;
    P := FKeyWords.Find(S);
    if (P <> nil) then
      Result := P^;
  { I:=FirstKeyword;
  while (Result=tsqlIdentifier) and (I<=Lastkeyword) do
    begin
    if (S=TokenInfos[i]) then
      begin
      Result := i;
      FCurToken := Result;
      exit;
      end;
    I:=Succ(I);
    end;}
  end;

  function TSQLScanner.FetchToken: TSQLToken;
  begin
    repeat
      if TokenStr = nil then
        if not FetchLine then
        begin
          Result := tsqlEOF;
          FCurToken := Result;
          exit;
        end;
      FCurTokenString := '';
      case TokenStr[0] of
        #0: // Empty line
          begin
            FetchLine;
            Result := tsqlWhiteSpace;
          end;
        '/' :
          Result := CommentDiv;
        #9, ' ', #10, #13:
          Result := DoWhiteSpace;
        '''':
          begin
            Result := DoStringLiteral;
            if (soSingleQuoteIdentifier in Options) then
              Result := tsqlIdentifier;
          end;
        '"':
          begin
            Result := DoStringLiteral;
            if (soDoubleQuoteStringLiteral in Options) then
              Result := tsqlString
            else
              Result := tsqlIdentifier;
          end;
        '`':
          begin
            Result := DoStringLiteral;
            if (soBackQuoteIdentifier in Options) then
              Result := tsqlIdentifier
            else
              Error(SErrUNknownToken, ['`']);
          end;
        '0' .. '9':
          Result := DoNumericLiteral;
        '?':
          begin
            Inc(TokenStr);
            Result := tsqlPlaceHolder;
          end;
        '!':
          begin
            Inc(TokenStr);
            if TokenStr[0] = '>' then
              Result := tsqlLE
            else if (TokenStr[0] = '<') then
              Result := tsqlGE
            else if (TokenStr[0] = '=') then
              Result := tsqlNE
            else
              Result := tsqlUnknown;
            Inc(TokenStr);
          end;
        '|':
          begin
            Inc(TokenStr);
            if TokenStr[0] = '|' then
            begin
              Inc(TokenStr);
              Result := tsqlConcatenate
            end
            else
              Error(SBarExpected);
          end;
        '(':
          begin
            Inc(TokenStr);
            Result := tsqlBraceOpen;
          end;
        ')':
          begin
            Inc(TokenStr);
            Result := tsqlBraceClose;
          end;
        '[':
          begin
            Inc(TokenStr);
            Result := tsqlSquareBraceOpen;
          end;
        ']':
          begin
            Inc(TokenStr);
            Result := tsqlSquareBraceClose;
          end;
        '*':
          begin
            Inc(TokenStr);
            Result := tsqlMUL;
          end;
        '+':
          begin
            Inc(TokenStr);
            Result := tsqlPLUS;
          end;
        ',':
          begin
            Inc(TokenStr);
            Result := tsqlCOMMA;
          end;
        '-':
          begin
            Inc(TokenStr);
            if (TokenStr[0] = '-') then
            begin
              Inc(TokenStr);
              Result := DoSingleLineComment
            end
            else if (TokenStr[0] in ['0' .. '9']) then
            begin
              Result := DoNumericLiteral;
              if (Result in [tsqlIntegerNumber, tsqlFloatNumber]) then
                FCurTokenString := '-' + FCurTokenString;
            end
            else
              Result := tsqlMINUS;
          end;
        '.':
          begin
            Inc(TokenStr);
            Result := tsqlDOT;
          end;
        ':':
          begin
            Inc(TokenStr);
            Result := tsqlCOLON;
          end;
        ';':
          begin
            Inc(TokenStr);
            Result := tsqlSEMICOLON;
          end;
        '<':
          begin
            Inc(TokenStr);
            if TokenStr[0] = '>' then
            begin
              Inc(TokenStr);
              Result := tsqlNE;
            end
            else if (TokenStr[0] = '=') then
            begin
              Inc(TokenStr);
              Result := tsqlLE;
            end
            else
              Result := tsqlLT;
          end;
        '=':
          begin
            Inc(TokenStr);
            Result := tsqlEQ;
          end;
        '>':
          begin
            Inc(TokenStr);
            if TokenStr[0] = '=' then
            begin
              Inc(TokenStr);
              Result := tsqlGE;
            end
            else
              Result := tsqlGT;
          end;
        'a' .. 'z',
          'A' .. 'Z':
          Result := DoIdentifier;
      else
        Error(SErrUNknownToken, [TokenStr[0]]);
      end; // Case
    until (not(Result in [tsqlComment, tsqlWhiteSpace])) or
      ((Result = tsqlComment) and (soReturnComments in Options)) or
      ((Result = tsqlWhiteSpace) and (soReturnWhiteSpace in Options));
    FCurToken := Result;
  end;

  function TSQLScanner.IsEndOfLine: Boolean;
  begin
    Result := (TokenStr = nil) or (TokenStr[0] in [#0, #10, #13]);
  end;

  function TSQLScanner.GetCurColumn: Integer;
  begin
    Result := TokenStr - PChar(FCurLine);
  end;

  procedure TSQLScanner.ClearKeywords(Sender : TObject);
  begin
    if Assigned(FKeyWords) then
      FKeyWords.Clear;
  end;

  function TSQLScanner.GetExcludeKeywords: TStrings;
  begin
    if FExclude = nil then
    begin
      FExclude := TStringList.Create;
      FExclude.Duplicates := dupIgnore;
      FExclude.Sorted := true;
      FExclude.OnChange := ClearKeywords;
    end;
    Result := FExclude;
  end;

{ TStreamLineReader }

  constructor TStreamLineReader.Create(AStream: TStream);
  begin
    FStream := AStream;
    FBufPos := 0;
    FBufLen := 0;
  end;

  function TStreamLineReader.IsEOF: Boolean;
  begin
    Result := (FBufPos >= FBufLen);
    if Result then
    begin
      FillBuffer;
      Result := (FBufLen = 0);
    end;
  end;

  procedure TStreamLineReader.FillBuffer;
  begin
    FBufLen := FStream.Read(Buffer, SizeOf(Buffer) - 1);
    Buffer[FBufLen] := 0;
    FBufPos := 0;
  end;

  function TStreamLineReader.ReadLine: string;
  var
    FPos, OLen, Len: Integer;
    PRun           : PByte;
  begin
    FPos := FBufPos;
    SetLength(Result, 0);
    repeat
      PRun := @Buffer[FBufPos];
      while (FBufPos < FBufLen) and not(PRun^ in [10, 13]) do
      begin
        Inc(PRun);
        Inc(FBufPos);
      end;
      if (FBufPos = FBufLen) then
      begin
        Len := FBufPos - FPos;
        if (Len > 0) then
        begin
          OLen := Length(Result);
          SetLength(Result, OLen + Len);
          Move(Buffer[FPos], Result[OLen + 1], Len);
        end;
        FillBuffer;
        FPos := FBufPos;
      end;
    until (FBufPos = FBufLen) or (PRun^ in [10, 13]);
    Len := FBufPos - FPos + 1;
    if (Len > 0) then
    begin
      OLen := Length(Result);
      SetLength(Result, OLen + Len);
      Move(Buffer[FPos], Result[OLen + 1], Len);
    end;
    if (PRun^ in [10, 13]) and (FBufPos < FBufLen) then
    begin
      Inc(FBufPos);
    // Check #13#10
      if (PRun^ = 13) then
      begin
        if (FBufPos = FBufLen) then
          FillBuffer;
        if (FBufPos < FBufLen) and (Buffer[FBufPos] = 10) then
        begin
          Inc(FBufPos);
          Result := Result + #10;
        end;
      end;
    end;
  end;

end.

