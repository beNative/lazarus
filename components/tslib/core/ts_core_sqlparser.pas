{
    This file is part of the Free Component Library
    Copyright (c) 2010 by the Free Pascal development team

    SQL source syntax parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ts_Core_SQLParser;

{$mode delphi}

interface

uses
  Classes, SysUtils,

  ts_Core_SQLScanner, ts_Core_SQLTree;

type
  TParseTypeFlag = (ptfAllowDomainName, ptfAlterDomain, ptfAllowConstraint,
    ptProcedureParam, ptfTableFieldDef, ptfCast, ptfExternalFunction,
    ptfExternalFunctionResult);
  TParseTypeFlags = set of TParseTypeFlag;

  TExpressionOption = (eoCheckConstraint, eoTableConstraint, eoComputedBy,
    eoOnlyNull,
    eoFieldValue, eoSelectvalue, eoParamValue, eoWhereClause, eoJoin,
    eoHaving, eoListValue, eoIF);
  TExpressionOptions = set of TExpressionOption;
  TSelectFlag        = (sfSingleTon, sfUnion, sfInto);
  TSelectFlags       = set of TSelectFlag;

  { TSQLParser }

  TSQLParser = class(TObject)
  private
    FInput          : TStream;
    FScanner        : TSQLScanner;
    FCurrent        : TSQLToken;
    FCurrentString  : string;
    FPrevious       : TSQLToken;
    FFreeScanner    : Boolean;
    FPeekToken      : TSQLToken;
    FPeekTokenString: string;

    procedure CheckEOF;

  protected
    procedure UnexpectedToken; overload;
    procedure UnexpectedToken(AExpected : TSQLTokens); overload;
    // All elements must be created with this factory function
    function CreateElement(AElementClass : TSQLElementClass;
      APArent : TSQLElement) : TSQLElement; virtual;
    function CreateLiteral(APArent: TSQLElement): TSQLLiteral;
    function CreateIdentifier(APArent : TSQLElement;
      const AName : TSQLStringType) : TSQLIdentifierName;
    procedure Expect(aToken: TSQLToken); overload;
    procedure Expect(aTokens: TSQLTokens); overload;
    procedure Consume(aToken: TSQLToken);
    procedure Error(Msg : string); overload;
    procedure Error(Fmt : string; Args : array of const); overload;
    // Expression support
    function ParseExprLevel1(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprLevel2(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprLevel3(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprLevel4(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprLevel5(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprLevel6(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseExprPrimitive(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLExpression;
    function ParseInoperand(APArent: TSQLElement): TSQLExpression;
    // Lists, primitives
    function ParseIdentifierList(APArent: TSQLElement;
      AList: TSQLelementList): integer;
    function ParseValueList(APArent: TSQLElement; EO : TExpressionOptions)
      : TSQLelementList;
    function ParseSQLValue(APArent: TSQLElement): TSQLExpression;
    function ParseCheckConstraint(APArent: TSQLElement;
      TableConstraint : Boolean = False): TSQLExpression;
    // Create/Alter statements
    function ParseAddTableElement(APArent: TSQLElement)
      : TSQLAlterTableAddElementOperation;
    function ParseAlterTableElement(APArent: TSQLElement)
      : TSQLAlterTableOperation;
    function ParseDropTableElement(APArent: TSQLElement)
      : TSQLDropTableElementOperation;
    function ParseFieldConstraint(APArent: TSQLElement): TSQLFieldConstraint;
    function ParseForeignKeyDefinition(APArent: TSQLElement)
      : TSQLForeignKeyDefinition;
    procedure ParseCharTypeDefinition(Out DT: TSQLDataType; Out Len: integer;
      Out ACharset : TSQLStringType);
    procedure ParseBlobDefinition(var ASegmentSize, ABlobType: integer;
      var ACharset : TSQLStringType);
    function ParseTypeDefinition(APArent: TSQLElement; Flags: TParseTypeFlags)
      : TSQLTypeDefinition;
    function ParseTableFieldDef(APArent: TSQLElement): TSQLTableFieldDef;
    function ParseTableConstraint(APArent: TSQLElement): TSQLTableConstraintDef;
    function ParseCreateDomainStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateOrAlterStatement;
    function ParseCreateExceptionStatement(APArent: TSQLElement;
      IsAlter: Boolean): TSQLCreateOrAlterStatement;
    function ParseCreateGeneratorStatement(APArent: TSQLElement;
      IsAlter: Boolean): TSQLCreateOrAlterStatement;
    function ParseCreateRoleStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateOrAlterStatement;
    function ParseCreateIndexStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateOrAlterStatement;
    function ParseCreateProcedureStatement(APArent: TSQLElement;
      IsAlter: Boolean): TSQLCreateOrAlterStatement;
    function ParseCreateTableStatement(APArent: TSQLElement)
      : TSQLCreateOrAlterStatement;
    function ParseAlterTableStatement(APArent: TSQLElement)
      : TSQLAlterTableStatement;
    function ParseCreateViewStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateOrAlterStatement;
    function ParseCreateTriggerStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateOrAlterStatement;
    function ParseSetGeneratorStatement(APArent: TSQLElement)
      : TSQLSetGeneratorStatement;
    function ParseCreateDatabaseStatement(APArent: TSQLElement;
      IsAlter: Boolean): TSQLCreateDatabaseStatement;
    function ParseCreateShadowStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLCreateShadowStatement;
    function ParseAlterDatabaseStatement(APArent: TSQLElement; IsAlter: Boolean)
      : TSQLAlterDatabaseStatement;
    function ParseSecondaryFile(APArent: TSQLElement): TSQLDatabaseFileInfo;
    function ParseDeclareFunctionStatement(APArent: TSQLElement)
      : TSQLDeclareExternalFunctionStatement;
    function ParseDeclareStatement(APArent: TSQLElement): TSQLStatement;
    // GRANT parsing
    procedure ParseGranteeList(APArent: TSQLElement; List: TSQLelementList;
      AllowObject, AllowGroup, AllowPublic : Boolean;
      IsRevoke: Boolean = False);
    function ParseGrantExecuteStatement(APArent: TSQLElement)
      : TSQLProcedureGrantStatement;
    function ParseGrantRoleStatement(APArent: TSQLElement)
      : TSQLRoleGrantStatement;
    function ParseGrantTableStatement(APArent: TSQLElement)
      : TSQLTableGrantStatement;
    // REVOKE parsing
    function ParseRevokeExecuteStatement(APArent: TSQLElement)
      : TSQLProcedureRevokeStatement;
    function ParseRevokeRoleStatement(APArent: TSQLElement)
      : TSQLRoleRevokeStatement;
    function ParseRevokeTableStatement(APArent: TSQLElement)
      : TSQLTableRevokeStatement;
    // SELECT parsing
    function ParseExprAggregate(APArent: TSQLElement; EO: TExpressionOptions)
      : TSQLAggregateFunctionExpression;
    procedure ParseFromClause(APArent: TSQLSelectStatement;
      AList: TSQLelementList);
    procedure ParseGroupBy(APArent: TSQLSelectStatement;
      AList: TSQLelementList);
    procedure ParseOrderBy(APArent: TSQLSelectStatement;
      AList: TSQLelementList);
    procedure ParseSelectFieldList(APArent: TSQLSelectStatement;
      AList: TSQLelementList; Singleton : Boolean);
    function ParseForUpdate(APArent: TSQLSelectStatement): TSQLelementList;
    function ParseSelectPlan(APArent: TSQLElement): TSQLSelectPlan;
    function ParseTableRef(APArent: TSQLSelectStatement): TSQLTableReference;
    procedure ParseIntoList(APArent: TSQLElement; List: TSQLelementList);
    // EXECUTE parsing
    function ParseExecuteProcedureStatement(APArent: TSQLElement)
      : TSQLExecuteProcedureStatement;
    // Stored procedure parsing
    function ParseAssignStatement(APArent: TSQLElement): TSQLAssignStatement;
    function ParseExceptionStatement(APArent: TSQLElement)
      : TSQLExceptionStatement;
    function ParseForStatement(APArent: TSQLElement): TSQLForStatement;
    function ParseIfStatement(APArent: TSQLElement): TSQLIFStatement;
    function ParsePostEventStatement(APArent: TSQLElement)
      : TSQLPostEventStatement;
    procedure ParseProcedureParamList(APArent: TSQLElement;
      AList: TSQLelementList);
    procedure ParseCreateProcedureVariableList(APArent: TSQLElement;
      AList: TSQLelementList);
    function ParseProcedureStatement(APArent: TSQLElement): TSQLStatement;
    procedure ParseStatementBlock(APArent: TSQLElement;
      Statements: TSQLelementList);
    function ParseWhenStatement(APArent: TSQLElement): TSQLWhenStatement;
    function ParseWhileStatement(APArent: TSQLElement): TSQLWhileStatement;

  public
    constructor Create(AInput: TStream); overload;
    constructor Create(AScanner : TSQLScanner); overload;
    destructor Destroy; override;
    function ParseSelectStatement(APArent : TSQLElement;
      Flags : TSelectFlags = []) : TSQLSelectStatement;
    function ParseUpdateStatement(APArent : TSQLElement) : TSQLUpdateStatement;
    function ParseInsertStatement(APArent : TSQLElement) : TSQLInsertStatement;
    function ParseDeleteStatement(APArent : TSQLElement) : TSQLDeleteStatement;
    function ParseCreateStatement(APArent : TSQLElement;
      IsAlter : Boolean = False) : TSQLCreateOrAlterStatement;
    function ParseDropStatement(APArent : TSQLElement) : TSQLDropStatement;
    function ParseRollbackStatement(APArent : TSQLElement)
      : TSQLRollbackStatement;
    function ParseCommitStatement(APArent : TSQLElement) : TSQLCommitStatement;
    function ParseSetStatement(APArent : TSQLElement) : TSQLStatement;
    function ParseConnectStatement(APArent : TSQLElement)
      : TSQLConnectStatement;
    function ParseGrantStatement(APArent: TSQLElement): TSQLGrantStatement;
    function ParseRevokeStatement(APArent: TSQLElement): TSQLGrantStatement;
    function Parse : TSQLElement;
    function ParseScript(AllowPartial : Boolean = False) : TSQLelementList;
    // Auxiliary stuff
    function CurrentToken : TSQLToken;
    function CurrentTokenString : string;
    function GetNextToken : TSQLToken;
    function PeekNextToken : TSQLToken;
    function PreviousToken : TSQLToken;
    function IsEndOfLine : Boolean;
    function CurSource: string;
    function CurLine : integer;
    function CurPos : integer;
  end;

  { ESQLParser }

  ESQLParser = class(Exception)
  private
    FCol     : integer;
    FFileName: string;
    FLine    : integer;

  public
    property Line     : integer read FLine write FLine;
    property Col      : integer read FCol write FCol;
    property FileName : string read FFileName write FFileName;
  end;

function StringToSQLExtractElement(const S : TSQLStringType;
  Out Res : TSQLExtractElement) : Boolean;

implementation

uses
  TypInfo;

Resourcestring
  SerrUnmatchedBrace = 'Expected ).';
  SErrCommaOrBraceExpected = 'Expected , or ).';
  SErrUnexpectedToken = 'Unexpected token: %s';
  SErrUnexpectedTokenOf = 'Unexpected token: %s, expected one of %s';
  SErrTokenMismatch = 'Unexpected token: ''%s'', expected: ''%s''';
  SErrExpectedDBObject = 'Expected database object type. Got: ''%s''';
  SErrDomainNotAllowed = 'Domain name not allowed in typ definition.';
  SErrExpectedChar = 'Expected CHAR or CHARACTER, got "%s"';
  SERRVaryingNotAllowed = 'VARYING not allowed at this point.';
  SErrUnknownBooleanOp = 'Unknown boolean operation';
  SErrUnknownComparison = 'unknown Comparison operation';
  SErrIntegerExpected = 'Integer expression expected';
  SErrInvalidUseOfCollate = 'Invalid use of COLLATE';
  SErrCannotAlterGenerator = 'Alter generator statement unknown';
  SErrInvalidLiteral = 'Invalid literal: "%s"';
  SErrNoAggregateAllowed = 'Aggregate function not allowed.';
  SErrAsteriskOnlyInCount = '* allowed only in COUNT aggregate';
  SErrUpperOneArgument = 'Only one argument for UPPER allowed';
  SErrHavingWithoutGroupBy = 'HAVING without GROUP BY clause not allowed';
  SErrNoAsteriskInSingleTon = '* not allowed in singleton select';
  SErrUnionFieldCountMatch = 'Field count mismatch in select union : %d <> %d';
  SErrInvalidExtract = 'Invalid element for extract: %s';

function StringToSQLExtractElement(const S : TSQLStringType;
  Out Res : TSQLExtractElement) : Boolean;

var
  I  : TSQLExtractElement;
begin
  Result := False;
  For I := Low(TSQLExtractElement) to High(TSQLExtractElement) do
    if SameText(ExtractElementNames[I], S) then
    begin
      Res := I;
      Exit(True);
    end;
end;

{ TSQLParser }

procedure TSQLParser.Expect(aToken: TSQLToken);
begin

{$IFDEF debugparser} Writeln('Expecting : ', GetEnumName(TypeInfo(TSQLToken), Ord(aToken)), ' As string: ', TokenInfos[aToken]); {$ENDIF debugparser}

  if (CurrentToken <> aToken) then
    Error(SErrTokenMismatch, [CurrentTokenString, TokenInfos[aToken]]);
end;

procedure TSQLParser.Expect(aTokens: TSQLTokens);
begin
  if not(CurrentToken in aTokens) then
    UnexpectedToken(aTokens);
end;

procedure TSQLParser.Consume(aToken: TSQLToken);
begin
  Expect(aToken);
  GetNextToken;
end;

function TSQLParser.CurSource: string;
begin
  Result := FScanner.CurFilename;
end;

function TSQLParser.CurLine: integer;
begin
  Result := FScanner.CurRow;
end;

function TSQLParser.CurPos: integer;
begin
  Result := FScanner.CurColumn;
end;

procedure TSQLParser.Error(Msg: string);

var
  ErrAt : string;
  E     : ESQLParser;

begin
  if Assigned(FScanner) then
    if FScanner.CurFilename <> '' then
      ErrAt := Format('Error: file "%s" line %d, pos %d: ',
        [FScanner.CurFilename, FScanner.CurRow, FScanner.CurColumn])
    else
      ErrAt := Format('Error: line %d, pos %d: ',
        [FScanner.CurRow, FScanner.CurColumn]);
  E := ESQLParser.Create(ErrAt + Msg);
  if Assigned(FScanner) then
  begin
    E.Line := FScanner.CurRow;
    E.Col := FScanner.CurColumn;
    E.FileName := FScanner.CurFilename;
  end;
  //raise E;
end;

procedure TSQLParser.Error(Fmt: string; Args: array of const );
begin
  Error(Format(Fmt, Args));
end;

function TSQLParser.CreateElement(AElementClass: TSQLElementClass;
  APArent: TSQLElement): TSQLElement;
begin
  Result := AElementClass.Create(APArent);
  Result.Source := CurSource;
  Result.SourceLine := CurLine;
  Result.SourcePos := CurPos;
end;

function TSQLParser.ParseTableRef(APArent : TSQLSelectStatement)
  : TSQLTableReference;
var
  T : TSQLSimpleTablereference;
  J : TSQLJoinTableReference;

begin
  if (CurrentToken = tsqlBraceOpen) then
  begin
    GetNextToken;
    Result := ParseTableRef(APArent);
    Consume(tsqlBraceClose)
  end
  else
  begin
    Expect(tsqlIdentifier);
    T := TSQLSimpleTablereference(CreateElement(TSQLSimpleTablereference,
      APArent));
    Result := T;
    T.ObjectName := CreateIdentifier(T, CurrentTokenString);
    GetNextToken;
    if CurrentToken = tsqlBraceOpen then
    begin
      T.Params := ParseValueList(APArent, [eoParamValue]);
      GetNextToken;
    end;
    if (CurrentToken = tsqlIdentifier) then
    begin
      T.AliasName := CreateIdentifier(T, CurrentTokenString);
      GetNextToken;
    end;
  end;
  Repeat
    if CurrentToken in [tsqlInner, tsqlJoin, tsqlOuter, tsqlLeft,
      tsqlRight] then
    begin
      J := TSQLJoinTableReference(CreateElement(TSQLJoinTableReference,
        APArent));
      J.Left := Result;
      Result := J;
      case CurrentToken of
        tsqlInner :
          J.JoinType := jtInner;
        tsqlJoin :
          J.JoinType := jtNone;
        tsqlOuter :
          J.JoinType := jtOuter;
        tsqlLeft :
          J.JoinType := jtLeft;
        tsqlRight :
          J.JoinType := jtRight;
      end;
      if CurrentToken <> tsqlJoin then
        GetNextToken;
      Consume(tsqlJoin);
      J.Right := ParseTableRef(APArent);
      Consume(tsqlOn);
      J.JoinClause := ParseExprLevel1(J, [eoJoin]);
    end;
  until Not(CurrentToken in [tsqlInner, tsqlJoin, tsqlOuter, tsqlLeft,
    tsqlRight]);
end;

procedure TSQLParser.ParseFromClause(APArent : TSQLSelectStatement;
  AList : TSQLelementList);

var
  T    : TSQLTableReference;
  Done : Boolean;

begin
  // On entry, we are on the FROM keyword.
  Consume(tsqlFrom);
  Repeat
    T := ParseTableRef(APArent);
    AList.Add(T);
    Done := (CurrentToken <> tsqlComma);
    if not Done then
      GetNextToken;
  until Done;
end;

procedure TSQLParser.ParseSelectFieldList(APArent : TSQLSelectStatement;
  AList : TSQLelementList; Singleton : Boolean);
var
  F : TSQLSelectField;
  B : Boolean;

begin
  // On entry, we're on the token preceding the field list.
  B := True;
  Repeat
    GetNextToken;
    if B then
    begin
      if (CurrentToken = tsqlDistinct) then
      begin
        APArent.Distinct := True;
        GetNextToken;
      end
      else if (CurrentToken = tsqlAll) then
      begin
        APArent.All := True;
        GetNextToken;
      end;
      B := False;
    end;
    if (CurrentToken = tsqlMul) then
    begin
      if Singleton then
        Error(SErrNoAsteriskInSingleTon);
      AList.Add(CreateElement(TSQLSelectAsterisk, APArent));
      GetNextToken;
    end
    else
    begin
      F := TSQLSelectField(CreateElement(TSQLSelectField, APArent));
      AList.Add(F);
      F.Expression := ParseExprLevel1(APArent, [eoSelectvalue]);
      if CurrentToken in [tsqlAs, tsqlIdentifier] then
      begin
        if CurrentToken = tsqlAs then
          GetNextToken;
        Expect(tsqlIdentifier);
        F.AliasName := CreateIdentifier(F, CurrentTokenString);
        GetNextToken;
      end;
    end;
    Expect([tsqlComma, tsqlFrom]);
  until (CurrentToken = tsqlFrom);
end;

procedure TSQLParser.ParseGroupBy(APArent : TSQLSelectStatement;
  AList : TSQLelementList);

var
  N : TSQLStringType;

begin
  // On entry we're on the GROUP token.
  Consume(tsqlGroup);
  Expect(tsqlBy);
  Repeat
    GetNextToken;
    Expect(tsqlIdentifier);
    N := CurrentTokenString;
    GetNextToken;
    if (CurrentToken = tsqlDot) then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
      N := N + '.' + CurrentTokenString;
      GetNextToken;
    end;
    AList.Add(CreateIdentifier(APArent, N));

  until (CurrentToken <> tsqlComma);
end;

function TSQLParser.ParseForUpdate(APArent : TSQLSelectStatement)
  : TSQLelementList;

begin
  // On entry we're on the FOR token.
  Consume(tsqlFor);
  Expect(tsqlUpdate);
  Result := TSQLelementList.Create(True);
  try
    Repeat
      GetNextToken;
      Expect(tsqlIdentifier);
      Result.Add(CreateIdentifier(APArent, CurrentTokenString));
    until (CurrentToken <> tsqlComma);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSQLParser.ParseOrderBy(APArent : TSQLSelectStatement;
  AList : TSQLelementList);

var
  O : TSQLOrderByElement;
  F : TSQLElement;

begin
  // On entry we're on the ORDER token.
  Consume(tsqlOrder);
  Expect(tsqlBy);
  Repeat
    GetNextToken;
    case CurrentToken of
      tsqlIdentifier :
        F := CreateIdentifier(APArent, CurrentTokenString);
      tsqlIntegerNumber :
        begin
          F := TSQLIntegerLiteral(CreateElement(TSQLIntegerLiteral, APArent));
          TSQLIntegerLiteral(F).Value := StrToInt(CurrentTokenString);
        end
    else
      UnexpectedToken([tsqlIdentifier, tsqlIntegerNumber]);
    end;
    try
      O := TSQLOrderByElement(CreateElement(TSQLOrderByElement, APArent));
      AList.Add(O);
      O.Field := F;
      F := nil;
    except
      FreeAndNil(F);
      raise;
    end;
    GetNextToken;
    if (CurrentToken = tsqlCollate) then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
      O.Collation := CreateIdentifier(O, CurrentTokenString);
      GetNextToken;
    end;
    if (CurrentToken in [tsqlDesc, tsqlAsc, tsqlDescending, tsqlAscending]) then
    begin
      if (CurrentToken in [tsqlDesc, tsqlDescending]) then
        O.OrderBy := obDescending
      else
        O.OrderBy := obAscending;
      GetNextToken;
    end;
  until (CurrentToken <> tsqlComma);
end;

function TSQLParser.ParseSelectPlan(APArent : TSQLElement) : TSQLSelectPlan;

var
  E : TSQLSelectPlanExpr;
  I : TSQLSelectPlanItem;
  L : TSQLelementList;
  N : TSQLStringType;

begin
  Result := nil;
  try
    case CurrentToken of
      tsqlIdentifier :
        begin
          if Not(APArent is TSQLSelectPlanExpr) then
            UnexpectedToken([tsqlJoin, tsqlmerge, tsqlSort]);
          N := CurrentTokenString;
          case GetNextToken of
            tsqlNatural:
              begin
                I := TSQLSelectNaturalPlan(CreateElement(TSQLSelectNaturalPlan,
                  APArent));
                Result := I;
              end;
            tsqlIndex :
              begin
                I := TSQLSelectIndexedPlan(CreateElement(TSQLSelectIndexedPlan,
                  APArent));
                Result := I;
                L := TSQLSelectIndexedPlan(I).Indexes;
                GetNextToken;
                Expect(tsqlBraceOpen);
                Repeat
                  GetNextToken;
                  Expect(tsqlIdentifier);
                  L.Add(CreateIdentifier(Result, CurrentTokenString));
                  GetNextToken;
                  Expect([tsqlComma, tsqlBraceClose]);
                until (CurrentToken = tsqlBraceClose);
              end;
            tsqlOrder:
              begin
                GetNextToken;
                Expect(tsqlIdentifier);
                I := TSQLSelectOrderedPlan(CreateElement(TSQLSelectOrderedPlan,
                  APArent));
                Result := I;
                TSQLSelectOrderedPlan(I).OrderIndex :=
                  CreateIdentifier(I, CurrentTokenString);
              end;
          else
            UnexpectedToken([tsqlNatural, tsqlIndex, tsqlOrder]);
          end;
          I.TableName := CreateIdentifier(I, N);
        end;
      tsqlJoin,
        tsqlmerge,
        tsqlSort,
        tsqlBraceOpen:
        begin
          E := TSQLSelectPlanExpr(CreateElement(TSQLSelectPlanExpr, APArent));
          Result := E;
          case CurrentToken of
            tsqlJoin,
              tsqlBraceOpen :
              E.JoinType := pjtJoin;
            tsqlSort :
              E.JoinType := pjtSort;
            tsqlmerge :
              E.JoinType := pjtMerge;
          end;
          if (CurrentToken <> tsqlBraceOpen) then
            GetNextToken;
          Expect(tsqlBraceOpen);
          repeat
            GetNextToken;
            E.Items.Add(ParseSelectPlan(E));
            Expect([tsqlComma, tsqlBraceClose]);
          until (CurrentToken = tsqlBraceClose);
        end;
    else
      UnexpectedToken([tsqlIdentifier, tsqlJoin, tsqlmerge, tsqlSort]);
    end;
    GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseSelectStatement(APArent: TSQLElement;
  Flags : TSelectFlags = []): TSQLSelectStatement;

begin
  // On entry, we're on the SELECT keyword
  Expect(tsqlSelect);
  Result := TSQLSelectStatement(CreateElement(TSQLSelectStatement, APArent));
  try
    if (PeekNextToken = tsqlTransaction) then
    begin
      Consume(tsqlSelect);
      GetNextToken;
      Expect(tsqlIdentifier);
      Result.TransactionName := CreateIdentifier(Result, CurrentTokenString);
    end;
    ParseSelectFieldList(Result, Result.Fields, sfSingleTon in Flags);
    // On return, we are on the FROM keyword.
    ParseFromClause(Result, Result.Tables);
    if CurrentToken = tsqlWhere then
    begin
      GetNextToken;
      Result.Where := ParseExprLevel1(Result, [eoWhereClause]);
    end;
    if CurrentToken = tsqlGroup then
      ParseGroupBy(Result, Result.GroupBy);
    if CurrentToken = tsqlHaving then
    begin
      if (Result.GroupBy.Count = 0) then
        Error(SErrHavingWithoutGroupBy);
      GetNextToken;
      Result.Having := ParseExprLevel1(Result, [eoHaving]);
    end;
    if (CurrentToken = tsqlUnion) then
    begin
      GetNextToken;
      if (CurrentToken = tsqlAll) then
      begin
        Result.UnionAll := True;
        GetNextToken;
      end;
      Result.Union := ParseSelectStatement(Result, Flags + [sfUnion]);
      if (Result.Fields.Count <> Result.Union.Fields.Count) then
        Error(SErrUnionFieldCountMatch, [Result.Fields.Count,
          Result.Union.Fields.Count])
    end;
    if (CurrentToken = tsqlPlan) then
    begin
      GetNextToken;
      Result.Plan := ParseSelectPlan(Result);
    end;
    if not(sfUnion in Flags) then
    begin
      if (CurrentToken = tsqlOrder) then
        ParseOrderBy(Result, Result.OrderBy);
      if (CurrentToken = tsqlFor) then
        Result.ForUpdate := ParseForUpdate(Result);
    end;
    if (sfInto in Flags) then
    begin
      if (CurrentToken = tsqlInto) then
      begin
        Result.Into := TSQLelementList.Create(True);
        ParseIntoList(Result, Result.Into);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseUpdateStatement(APArent: TSQLElement
  ): TSQLUpdateStatement;

var
  P : TSQLUpdatePair;
  N : string;
begin
  // On entry, we're on the UPDATE keyword
  Consume(tsqlUpdate);
  Expect(tsqlIdentifier);
  Result := TSQLUpdateStatement(CreateElement(TSQLUpdateStatement, APArent));
  try
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    Expect(tsqlSet);
    Repeat
      GetNextToken;
      Expect(tsqlIdentifier);
      P := TSQLUpdatePair(CreateElement(TSQLUpdatePair, Result));
      Result.Values.Add(P);
      N := CurrentTokenString;
      GetNextToken;
      if (CurrentToken = tsqlDot) then
      begin
        GetNextToken;
        Expect(tsqlIdentifier);
        N := N + '.' + CurrentTokenString;
        GetNextToken;
      end;
      Consume(tsqlEq);
      P.FieldName := CreateIdentifier(P, N);
      P.Value := ParseExprLevel1(P, [eoFieldValue]);
    until (CurrentToken <> tsqlComma);
    if (CurrentToken = tsqlWhere) then
    begin
      GetNextToken;
      Result.WhereClause := ParseExprLevel1(P, [eoWhereClause]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseInsertStatement(APArent: TSQLElement)
  : TSQLInsertStatement;

begin
  // On entry, we're on the INSERT statement
  Consume(tsqlInsert);
  Consume(tsqlInto);
  Expect(tsqlIdentifier);
  Result := TSQLInsertStatement(CreateElement(TSQLInsertStatement, APArent));
  try
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    if CurrentToken = tsqlBraceOpen then
    begin
      Result.Fields := TSQLelementList.Create(True);
      Repeat
        GetNextToken;
        Expect(tsqlIdentifier);
        Result.Fields.Add(CreateIdentifier(Result, CurrentTokenString));
        GetNextToken;
        Expect([tsqlBraceClose, tsqlComma]);
      Until (CurrentToken = tsqlBraceClose);
      GetNextToken;
    end;
    case CurrentToken of
      tsqlSelect :
        Result.Select := ParseSelectStatement(Result);
      tsqlValues :
        begin
          GetNextToken;
          Result.Values := ParseValueList(Result, [eoFieldValue]);
          GetNextToken; // consume )
        end;
    else
      UnexpectedToken([tsqlSelect, tsqlValues]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseDeleteStatement(APArent: TSQLElement
  ): TSQLDeleteStatement;
begin
  // On entry, we're on the DELETE token.
  Consume(tsqlDelete);
  Consume(tsqlFrom);
  Expect(tsqlIdentifier);
  Result := TSQLDeleteStatement(CreateElement(TSQLDeleteStatement, APArent));
  try
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    if CurrentToken = tsqlIdentifier then
    begin
      Result.AliasName := CreateIdentifier(Result, CurrentTokenString);
      GetNextToken;
    end;
    if CurrentToken = tsqlWhere then
    begin
      Consume(tsqlWhere);
      Result.WhereClause := ParseExprLevel1(Result, [eoWhereClause]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseTableFieldDef(APArent : TSQLElement)
  : TSQLTableFieldDef;
begin
  // on entry, we're on the field name
  Result := TSQLTableFieldDef(CreateElement(TSQLTableFieldDef, APArent));
  try
    Result.FieldName := CreateIdentifier(Result, CurrentTokenString);
    if PeekNextToken = tsqlComputed then
    begin
      GetNextToken;
      Consume(tsqlComputed);
      if CurrentToken = tsqlBy then
        GetNextToken;
      Consume(tsqlBraceOpen);
      Result.ComputedBy := ParseExprLevel1(Result, [eoComputedBy]);
      Consume(tsqlBraceClose);
    end
    else
      Result.FieldType := ParseTypeDefinition(Result,
        [ptfAllowDomainName, ptfAllowConstraint, ptfTableFieldDef]);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseTableConstraint(APArent: TSQLElement
  ): TSQLTableConstraintDef;

  procedure ParseFieldList(R : TSQLTableFieldsConstraintDef);

  begin
    GetNextToken;
    Consume(tsqlBraceOpen);
    ParseIdentifierList(APArent, R.FieldList);
//    Consume(tsqlBraceClose);
  end;

var
  N : TSQLStringType;
  K : TSQLTableForeignKeyConstraintDef;

begin
  if CurrentToken = tsqlConstraint then
  begin
    GetNextToken;
    Expect(tsqlIdentifier);
    N := CurrentTokenString;
    GetNextToken
  end;
  Result := nil;
  try
    case CurrentToken of
      tsqlUnique :
        begin
          Result := TSQLTableUniqueConstraintDef
            (CreateElement(TSQLTableUniqueConstraintDef, APArent));
          ParseFieldList(TSQLTableFieldsConstraintDef(Result));
        end;
      tsqlPrimary :
        begin
          GetNextToken;
          Expect(tsqlKey);
          Result := TSQLTablePrimaryKeyConstraintDef
            (CreateElement(TSQLTablePrimaryKeyConstraintDef, APArent));
          ParseFieldList(TSQLTableFieldsConstraintDef(Result));
        end;
      tsqlForeign :
        begin
          GetNextToken;
          Expect(tsqlKey);
          K := TSQLTableForeignKeyConstraintDef
            (CreateElement(TSQLTableForeignKeyConstraintDef, APArent));
          Result := K;
          ParseFieldList(TSQLTableFieldsConstraintDef(Result));
          Expect(tsqlReferences);
          K.Definition := ParseForeignKeyDefinition(K);
        end;
      tsqlCheck:
        begin
          Result := TSQLTableCheckConstraintDef
            (CreateElement(TSQLTableCheckConstraintDef, APArent));
          TSQLTableCheckConstraintDef(Result).Check :=
            ParseCheckConstraint(Result, True);
        end
    else
      UnexpectedToken([tsqlUnique, tsqlPrimary, tsqlForeign, tsqlCheck]);
    end;
    if (N <> '') then
      Result.ConstraintName := CreateIdentifier(Result, N);
  //  GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseCreateTableStatement(APArent: TSQLElement)
  : TSQLCreateOrAlterStatement;

var
  C  : TSQLCreateTableStatement;
  HC : Boolean;

begin
  // On enter, we're on the TABLE token.
  Consume(tsqlTable);
  C := TSQLCreateTableStatement(CreateElement(TSQLCreateTableStatement,
    APArent));
  try
    Expect(tsqlIdentifier);
    C.ObjectName := CreateIdentifier(C, CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlExternal) then
    begin
      GetNextToken;
      if (CurrentToken = tsqlFile) then
        GetNextToken;
      Expect(tsqlString);
      C.ExternalFileName := CreateLiteral(C) as TSQLStringLiteral;
      GetNextToken;
    end;
    Expect(tsqlBraceOpen);
    HC := False;
    Repeat
      GetNextToken;
      case CurrentToken of
        tsqlIdentifier :
          begin
            if HC then
              UnexpectedToken;
            C.FieldDefs.Add(ParseTableFieldDef(C));
          end;
        tsqlCheck,
          tsqlConstraint,
          tsqlForeign,
          tsqlPrimary,
          tsqlUnique:
          begin
            C.Constraints.Add(ParseTableConstraint(C));
            HC := True;
          end
      else
        UnexpectedToken([tsqlIdentifier, tsqlCheck, tsqlConstraint, tsqlForeign,
          tsqlPrimary, tsqlUnique]);
      end;
      Expect([tsqlBraceClose, tsqlComma]);
    until (CurrentToken = tsqlBraceClose);
    GetNextToken;
    Result := C;
  except
    FreeAndNil(C);
    raise;
  end;
end;

function TSQLParser.ParseDropTableElement(APArent : TSQLElement)
  : TSQLDropTableElementOperation;

var
  C : Boolean;
begin
  // On entry, we are on DROP token
  C := (GetNextToken = tsqlConstraint);
  if C then
    GetNextToken;
  Expect(tsqlIdentifier);
  if C then
    Result := TSQLDropTableConstraintOperation
      (CreateElement(TSQLDropTableConstraintOperation, APArent))
  else
    Result := TSQLDropTableFieldOperation
      (CreateElement(TSQLDropTableFieldOperation, APArent));
  Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
  GetNextToken;
end;

function TSQLParser.ParseAddTableElement(APArent : TSQLElement)
  : TSQLAlterTableAddElementOperation;

begin
  Result := nil;
  try
    case GetNextToken of
      tsqlIdentifier :
        begin
          Result := TSQLAlterTableAddElementOperation
            (CreateElement(TSQLAlterTableAddFieldOPeration, APArent));
          Result.Element := ParseTableFieldDef(Result);
        end;
      tsqlCheck,
        tsqlConstraint,
        tsqlForeign,
        tsqlPrimary,
        tsqlUnique:
        begin
          Result := TSQLAlterTableAddElementOperation
            (CreateElement(TSQLAlterTableAddConstraintOperation, APArent));
          Result.Element := ParseTableConstraint(Result);
        end
    else
      UnexpectedToken([tsqlIdentifier, tsqlCheck, tsqlConstraint, tsqlForeign,
        tsqlPrimary, tsqlUnique]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseAlterTableElement(APArent : TSQLElement)
  : TSQLAlterTableOperation;

var
  N : TSQLStringType;

begin
  Result := nil;
  if GetNextToken = tsqlColumn then
    GetNextToken;
  Expect(tsqlIdentifier);
  N := CurrentTokenString;
  try
    case GetNextToken of
      tsqlTo :
        begin
          GetNextToken;
          Result := TSQLAlterTableOperation
            (CreateElement(TSQLAlterTableFieldNameOperation, APArent));
          TSQLAlterTableFieldNameOperation(Result).NewName :=
            CreateIdentifier(Result, CurrentTokenString);
          GetNextToken;
        end;
      tsqltype:
        begin
          Result := TSQLAlterTableOperation
            (CreateElement(TSQLAlterTableFieldTypeOperation, APArent));
          TSQLAlterTableFieldTypeOperation(Result).NewType :=
            ParseTypeDefinition(Result, [ptfAllowDomainName, ptfAllowConstraint,
            ptfTableFieldDef]);
        end;
      tsqlPosition:
        begin
          GetNextToken;
          Expect(tsqlIntegerNumber);
          Result := TSQLAlterTableOperation
            (CreateElement(TSQLAlterTableFieldPositionOperation, APArent));
          TSQLAlterTableFieldPositionOperation(Result).NewPosition :=
            StrToInt(CurrentTokenString);
          GetNextToken;
        end
    else
      UnexpectedToken([tsqlTo, tsqltype, tsqlPosition]);
    end;
    Result.ObjectName := CreateIdentifier(Result, N);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseAlterTableStatement(APArent: TSQLElement)
  : TSQLAlterTableStatement;

begin
  // On enter, we're on the TABLE token.
  Consume(tsqlTable);
  Result := TSQLAlterTableStatement(CreateElement(TSQLAlterTableStatement,
    APArent));
  try
    Expect(tsqlIdentifier);
    Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
    Repeat
      GetNextToken;
      case CurrentToken of
        tsqlAdd:
          begin
            Result.Operations.Add(ParseAddTableElement(Result));
          end;
        tsqlAlter:
          begin
            Result.Operations.Add(ParseAlterTableElement(Result));
          end;
        tsqlDrop :
          begin
            Result.Operations.Add(ParseDropTableElement(Result));
          end;
      else
        UnexpectedToken([tsqlAdd, tsqlAlter, tsqlDrop]);
      end;
    until (CurrentToken <> tsqlComma);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseCreateIndexStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;

var
  O : TIndexOptions;
  C : TSQLCreateIndexStatement;
  A : TSQLAlterIndexStatement;
begin
  O := [];
  // On enter, we're on the UNIQUE, ASCENDING, DESCENDING or INDEX token
  if IsAlter then
  begin
    Expect(tsqlIndex);
    Consume(tsqlIndex);
    A := TSQLAlterIndexStatement(CreateElement(TSQLAlterIndexStatement,
      APArent));
    try
      Expect(tsqlIdentifier);
      A.ObjectName := CreateIdentifier(A, CurrentTokenString);
      GetNextToken;
      Expect([tsqlActive, tsqlInactive]);
      A.Inactive := CurrentToken = tsqlInactive;
      GetNextToken; // Token after ) or (in)Active
      Result := A;
    except
      FreeAndNil(A);
      raise;
    end;
  end
  else
  begin
    C := TSQLCreateIndexStatement(CreateElement(TSQLCreateIndexStatement,
      APArent));
    try
      if (CurrentToken = tsqlUnique) then
      begin
        GetNextToken;
        Include(O, ioUnique);
      end;
      if (CurrentToken = tsqlAscending) then
      begin
        GetNextToken;
        Include(O, ioAscending);
      end
      else if (CurrentToken = tsqlDescending) then
      begin
        GetNextToken;
        Include(O, ioDescending);
      end;
      C.Options := O;
      Consume(tsqlIndex);
      Expect(tsqlIdentifier);
      C.ObjectName := CreateIdentifier(C, CurrentTokenString);
      GetNextToken;
      Consume(tsqlOn);
      Expect(tsqlIdentifier);
      C.TableName := CreateIdentifier(C, CurrentTokenString); // name of table
      GetNextToken;
      Consume(tsqlBraceOpen);
      ParseIdentifierList(C, C.FieldNames);
      Result := C;
    except
      FreeAndNil(C);
      raise;
    end;
  end;
end;

function TSQLParser.ParseCreateViewStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;

var
  V : TSQLCreateViewStatement;

begin
  // on entry, we're on the VIEW token.
  if IsAlter then
    UnexpectedToken;
  Result := nil;
  Consume(tsqlView);
  Expect(tsqlIdentifier);
  V := TSQLCreateViewStatement(CreateElement(TSQLCreateViewStatement, APArent));
  Result := V;
  try
    V.ObjectName := CreateIdentifier(V, CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlBraceOpen) then
    begin
      GetNextToken;
      ParseIdentifierList(Result, V.Fields);
    end;
    Consume(tsqlAs);
    V.Select := ParseSelectStatement(V, []);
    if (CurrentToken = tsqlWith) then
    begin
      GetNextToken;
      Consume(tsqlCheck);
      Consume(tsqlOption);
      V.WithCheckOption := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSQLParser.ParseProcedureParamList(APArent: TSQLElement;
  AList : TSQLelementList);

var
  P : TSQLProcedureParamDef;

begin
  // On Entry, we're on the ( token
  Repeat
    GetNextToken;
    Expect(tsqlIdentifier);
    P := TSQLProcedureParamDef(CreateElement(TSQLProcedureParamDef, APArent));
    try
      AList.Add(P);
    except
      P.free;
      raise;
    end;
    P.ParamName := CreateIdentifier(P, CurrentTokenString);
    // Typedefinition will go to next token
    P.ParamType := ParseTypeDefinition(P, [ptProcedureParam]);
  Until (CurrentToken <> tsqlComma);
  Consume(tsqlBraceClose);
end;

procedure TSQLParser.ParseCreateProcedureVariableList(APArent: TSQLElement;
  AList : TSQLelementList);

var
  P : TSQLProcedureParamDef;

begin
  // On Entry, we're on the DECLARE token
  Repeat
    Consume(tsqlDeclare);
    Consume(tsqlVariable);
    Expect(tsqlIdentifier);
    P := TSQLProcedureParamDef(CreateElement(TSQLProcedureParamDef, APArent));
    Try
      AList.Add(P);
    except
      P.free;
      raise;
    end;
    P.ParamName := CreateIdentifier(P, CurrentTokenString);
    // Typedefinition will go to next token
    P.ParamType := ParseTypeDefinition(P, [ptProcedureParam]);
    Consume(tsqlSemicolon);
  Until (CurrentToken <> tsqlDeclare);
end;

function TSQLParser.ParseIfStatement(APArent : TSQLElement) : TSQLIFStatement;

var
  Pt : TSQLToken;

begin
  // On Entry, we're on the if token
  Consume(tsqlIf);
  Consume(tsqlBraceOpen);
  Result := TSQLIFStatement(CreateElement(TSQLIFStatement, APArent));
  try
    Result.Condition := ParseExprLevel1(APArent, [eoIF]);
    Consume(tsqlBraceClose);
    Consume(tsqlThen);
    Result.TrueBranch := ParseProcedureStatement(Result);
    if (CurrentToken = tsqlSemicolon) and (PeekNextToken = tsqlElse) then
    begin
      Pt := CurrentToken;
      GetNextToken;
    end
    else if (CurrentToken = tsqlElse) then
      if not(PreviousToken = tsqlEnd) then
        UnexpectedToken;
    if CurrentToken = tsqlElse then
    begin
      GetNextToken;
      Result.FalseBranch := ParseProcedureStatement(Result);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSQLParser.ParseIntoList(APArent : TSQLElement;
  List : TSQLelementList);

begin
  // On Entry, we're on the INTO token
  Repeat
    GetNextToken;
    if (CurrentToken = tsqlColon) then
      Consume(tsqlColon);
    Expect(tsqlIdentifier);
    List.Add(CreateIdentifier(APArent, CurrentTokenString));
    GetNextToken;
  Until (CurrentToken <> tsqlComma);
end;

function TSQLParser.ParseForStatement(APArent : TSQLElement) : TSQLForStatement;

begin
  // On Entry, we're on the FOR token
  Consume(tsqlFor);
  Expect(tsqlSelect);
  Result := TSQLForStatement(CreateElement(TSQLForStatement, APArent));
  try
    Result.Select := ParseSelectStatement(Result, []);
    Expect(tsqlInto);
    ParseIntoList(Result, Result.FieldList);
    Consume(tsqlDo);
    Result.Statement := ParseProcedureStatement(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseExceptionStatement(APArent : TSQLElement)
  : TSQLExceptionStatement;

begin
  // On Entry, we're on the EXCEPTION token
  Consume(tsqlException);
  Expect(tsqlIdentifier);
  Result := TSQLExceptionStatement(CreateElement(TSQLExceptionStatement,
    APArent));
  try
    Result.ExceptionName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseAssignStatement(APArent : TSQLElement)
  : TSQLAssignStatement;

var
  N : TSQLStringType;

begin
  // On entry, we're on the identifier token;
  Expect(tsqlIdentifier);
  Result := TSQLAssignStatement(CreateElement(TSQLAssignStatement, APArent));
  try
    N := CurrentTokenString;
    GetNextToken;
    if (CurrentToken = tsqlDot) and (Uppercase(N) = 'NEW') then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
      N := N + '.' + CurrentTokenString;
      GetNextToken;
    end;
    Result.Variable := CreateIdentifier(Result, N);
    Consume(tsqlEq);
    Result.Expression := ParseExprLevel1(Result, []);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParsePostEventStatement(APArent : TSQLElement)
  : TSQLPostEventStatement;

begin
  // On Entry, we're on the POST_EVENT token
  Consume(tsqlPostEvent);
  Result := TSQLPostEventStatement(CreateElement(TSQLPostEventStatement,
    APArent));
  try
    case CurrentToken of
      tsqlIdentifier :
        Result.ColName := CreateIdentifier(Result, CurrentTokenString);
      tsqlString :
        Result.EventName := CurrentTokenString;
    else
      UnexpectedToken([tsqlIdentifier, tsqlString]);
    end;
    GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseWhileStatement(APArent : TSQLElement)
  : TSQLWhileStatement;

begin
  // On entry, we're on the while Token
  Consume(tsqlWhile);
  Consume(tsqlBraceOpen);
  Result := TSQLWhileStatement(CreateElement(TSQLWhileStatement, APArent));
  try
    Result.Condition := ParseExprLevel1(Result, [eoIF]);
    Consume(tsqlBraceClose);
    Consume(tsqlDo);
    Result.Statement := ParseProcedureStatement(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseWhenStatement(APArent : TSQLElement)
  : TSQLWhenStatement;

var
  E : TSQLWhenException;
  S : TSQLWhenSQLError;
  G : TSQLWhenGDSError;

begin
  Consume(tsqlWhen);
  Result := TSQLWhenStatement(CreateElement(TSQLWhenStatement, APArent));
  try
    if (CurrentToken = tsqlAny) then
    begin
      Result.AnyError := True;
      GetNextToken
    end
    else
      Repeat
        if (Result.Errors.Count > 0) then
          GetNextToken;
        case CurrentToken of
          tsqlException:
            begin
              GetNextToken;
              Expect(tsqlIdentifier);
              E := TSQLWhenException(CreateElement(TSQLWhenException, APArent));
              E.ExceptionName := CreateIdentifier(E, CurrentTokenString);
              Result.Errors.Add(E);
            end;
          tsqlSQLCode:
            begin
              GetNextToken;
              Expect(tsqlIntegerNumber);
              S := TSQLWhenSQLError(CreateElement(TSQLWhenSQLError, APArent));
              S.ErrorCode := StrToInt(CurrentTokenString);
              Result.Errors.Add(S);
            end;
          tsqlGDSCODE:
            begin
              GetNextToken;
              Expect(tsqlIntegerNumber);
              G := TSQLWhenGDSError(CreateElement(TSQLWhenGDSError, APArent));
              G.GDSErrorNumber := StrToInt(CurrentTokenString);
              Result.Errors.Add(G);
            end;
        else
          UnexpectedToken([tsqlException, tsqlSQLCode, tsqlGDSCODE]);
        end;
        GetNextToken;
      until (CurrentToken <> tsqlComma);
    Consume(tsqlDo);
    Result.Statement := ParseProcedureStatement(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseProcedureStatement(APArent : TSQLElement)
  : TSQLStatement;

begin
  Result := nil;
  case CurrentToken of
    tsqlBegin :
      begin
        Result := TSQLStatementBlock(CreateElement(TSQLStatementBlock,
          APArent));
        ParseStatementBlock(Result, TSQLStatementBlock(Result).Statements);
      end;
    tsqlIf :
      Result := ParseIfStatement(APArent);
    tsqlFor :
      Result := ParseForStatement(APArent);
    tsqlException :
      Result := ParseExceptionStatement(APArent);
    tsqlIdentifier :
      Result := ParseAssignStatement(APArent);
    tsqlExecute :
      Result := ParseExecuteProcedureStatement(APArent);
    tsqlExit :
      begin
        Result := TSQLExitStatement(CreateElement(TSQLExitStatement, APArent));
        GetNextToken;
      end;
    tsqlSuspend :
      begin
        Result := TSQLSuspendStatement(CreateElement(TSQLSuspendStatement,
          APArent));
        GetNextToken;
      end;
    tsqlPostEvent :
      Result := ParsePostEventStatement(APArent);
    tsqlWhile :
      Result := ParseWhileStatement(APArent);
    tsqlWhen :
      Result := ParseWhenStatement(APArent);
    tsqlSelect :
      Result := ParseSelectStatement(APArent, [sfInto]);
    tsqlInsert :
      Result := ParseInsertStatement(APArent);
    tsqlDelete :
      Result := ParseDeleteStatement(APArent);
    tsqlUpdate :
      Result := ParseUpdateStatement(APArent);
  else
    UnexpectedToken;
  end;
end;

procedure TSQLParser.ParseStatementBlock(APArent: TSQLElement;
  Statements : TSQLelementList);

var
  S: TSQLStatement;

begin
  Consume(tsqlBegin);
  while (CurrentToken <> tsqlEnd) do
  begin
    S := ParseProcedureStatement(APArent);
    Statements.Add(S);
    if not(PreviousToken = tsqlEnd) then
      Consume(tsqlSemicolon);
  end;
  Consume(tsqlEnd);
end;

function TSQLParser.ParseCreateProcedureStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;

var
  P : TSQLAlterCreateProcedureStatement;

begin
  // On entry, we're on the procedure statement.
  Consume(tsqlProcedure);
  Expect(tsqlIdentifier);
  if IsAlter then
    P := TSQLAlterProcedureStatement
      (CreateElement(TSQLAlterProcedureStatement, APArent))
  else
    P := TSQLCreateProcedureStatement
      (CreateElement(TSQLCreateProcedureStatement, APArent));
  Result := P;
  try
    Result.ObjectName := CreateIdentifier(P, CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlBraceOpen) then
      ParseProcedureParamList(Result, P.InputVariables);
    if (CurrentToken = tsqlReturns) then
    begin
      GetNextToken;
      Expect(tsqlBraceOpen);
      ParseProcedureParamList(Result, P.OutputVariables);
    end;
    Consume(tsqlAs);
    if (CurrentToken = tsqlDeclare) then
      ParseCreateProcedureVariableList(Result, P.LocalVariables);
    Expect(tsqlBegin);
    ParseStatementBlock(Result, P.Statements);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseCreateGeneratorStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;
begin
  GetNextToken;
  Expect(tsqlIdentifier);
  if IsAlter then
    Error(SErrCannotAlterGenerator);
  Result := TSQLCreateOrAlterStatement
    (CreateElement(TSQLCreateGeneratorStatement, APArent));
  try
    Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
  except
    FreeAndNil(Result);
    raise;
  end;
  GetNextToken; // Comma;
end;

function TSQLParser.ParseCreateRoleStatement(APArent: TSQLElement;
  IsAlter: Boolean): TSQLCreateOrAlterStatement;
begin
  if IsAlter then
    UnexpectedToken; // no ALTER ROLE
  GetNextToken;
  Expect(tsqlIdentifier);
  Result := TSQLCreateOrAlterStatement(CreateElement(TSQLCreateRoleStatement,
    APArent));
  Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
  GetNextToken; // Comma;
end;

procedure TSQLParser.ParseCharTypeDefinition(Out DT : TSQLDataType;
  Out Len : integer; Out ACharset : TSQLStringType);

begin
  Len := 0;
  case CurrentToken of
    tsqlNCHAR :
      DT := sdtNchar;
    tsqlVarChar :
      DT := sdtVarChar;
    tsqlCharacter,
      tsqlChar :
      DT := sdtChar;
    tsqlCString :
      DT := sdtCstring;
    tsqlNational :
      begin
        DT := sdtNchar;
        GetNextToken;
        Expect([tsqlCharacter, tsqlChar]);
      end;
  end;
  GetNextToken; // VARYING, Start of size, CHARACTER set or end
  if (CurrentToken = tsqlVarying) then // CHAR VARYING or CHARACTER VARYING;
  begin
    if (DT in [sdtNchar, sdtChar]) then
    begin
      if DT = sdtNchar then
        DT := sdtNVARCHAR
      else
        DT := sdtVarChar;
      GetNextToken
    end
    else
      Error(SERRVaryingNotAllowed);
  end;
  if (CurrentToken = tsqlBraceOpen) then // (LEN)
  begin
    GetNextToken;
    Expect(tsqlIntegerNumber);
    Len := StrToInt(CurrentTokenString);
    GetNextToken;
    Expect(tsqlBraceClose);
    GetNextToken;
  end
  else if (DT = sdtCstring) then
    UnexpectedToken;
  if (CurrentToken = tsqlCharacter) then // Character set NNN
  begin
    if (DT = sdtCstring) then
      UnexpectedToken;
    GetNextToken;
    Consume(tsqlSet);
    Expect(tsqlIdentifier);
    ACharset := CurrentTokenString;
    GetNextToken;
  end;
end;

procedure TSQLParser.ParseBlobDefinition(var ASegmentSize, ABlobType : integer;
  var ACharset : TSQLStringType);

begin
  // On entry, we are on the blob token.
  GetNextToken;
  if (CurrentToken = tsqlBraceOpen) then // (segment[,subtype])
  begin
    GetNextToken;
    Expect(tsqlIntegerNumber);
    ASegmentSize := StrToInt(CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlComma) then
    begin
      GetNextToken;
      Expect(tsqlIntegerNumber);
      ABlobType := StrToInt(CurrentTokenString);
      GetNextToken;
    end;
    Consume(tsqlBraceClose);
    if CurrentToken in [tsqlSubtype, tsqlSegment] then
      Error(SErrUnexpectedToken, [CurrentTokenString]);
  end
  else
  begin
    if CurrentToken = tsqlSubtype then // SUB_TYPE T
    begin
      GetNextToken;
      Expect(tsqlIntegerNumber);
      ABlobType := StrToInt(CurrentTokenString);
      GetNextToken;
    end;
    if (CurrentToken = tsqlSegment) then // SEGMENT SIZE S
    begin
      GetNextToken;
      Consume(tsqlSize);
      Expect(tsqlIntegerNumber);
      ASegmentSize := StrToInt(CurrentTokenString);
      GetNextToken;
    end;
  end;
  if (CurrentToken = tsqlCharacter) then // CHARACTER set NNN
  begin
    GetNextToken;
    Consume(tsqlSet);
    Expect(tsqlIdentifier);
    ACharset := CurrentTokenString;
    GetNextToken;
  end;
end;

function TSQLParser.ParseForeignKeyDefinition(APArent : TSQLElement)
  : TSQLForeignKeyDefinition;

  // On entry, we're on ON Return true if On delete
  function ParseForeignKeyAction(Out Res : TForeignKeyAction) : Boolean;

  begin
    GetNextToken;
    case CurrentToken of
      tsqlDelete,
        tsqlUpdate:
        Result := CurrentToken = tsqlDelete;
    else
      UnexpectedToken([tsqlDelete, tsqlUpdate]);
    end;
    case GetNextToken of
      tsqlNo :
        begin
          GetNextToken;
          Expect(tsqlAction);
          Res := fkaNoAction;
        end;
      tsqlCascade :
        Res := fkaCascade;
      tsqlSet:
        begin
          case GetNextToken of
            tsqlDefault :
              Res := fkaSetDefault;
            tsqlNull:
              Res := fkaSetNull;
          else
            UnexpectedToken([tsqlDefault, tsqlNull]);
          end;
        end
    else
      UnexpectedToken([tsqlNo, tsqlCascade, tsqlSet]);
    end;
    GetNextToken;
  end;

var
  FKA : TForeignKeyAction;

begin
  Result := nil;
  // on entry, we are on the 'REFERENCES' token
  GetNextToken;
  Expect(tsqlIdentifier);
  Result := TSQLForeignKeyDefinition(CreateElement(TSQLForeignKeyDefinition,
    APArent));
  try
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlBraceOpen) then
    begin
      GetNextToken;
      ParseIdentifierList(Result, Result.FieldList);
    end;
    if (CurrentToken = tsqlOn) then
    begin
      if ParseForeignKeyAction(FKA) then
        Result.OnDelete := FKA
      else
        Result.OnUpdate := FKA;
    end;
    if (CurrentToken = tsqlOn) then
    begin
      if ParseForeignKeyAction(FKA) then
        Result.OnDelete := FKA
      else
        Result.OnUpdate := FKA;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseFieldConstraint(APArent : TSQLElement)
  : TSQLFieldConstraint;

var
  N : TSQLStringType;
  K : TSQLForeignKeyFieldConstraint;
  C : TSQLCheckFieldConstraint;
  L : TSQLFieldConstraintList;
  P : Boolean;
begin
  Result := nil;
  L := nil;
  P := False;
  try
    Repeat
      if (Result <> nil) then
      begin
        L := TSQLFieldConstraintList.Create(APArent);
        L.List.Add(Result);
        Result := nil;
      end;
      if CurrentToken = tsqlConstraint then
      begin
        GetNextToken;
        Expect(tsqlIdentifier);
        N := CurrentTokenString;
        GetNextToken
      end;
      case CurrentToken of
        tsqlUnique :
          begin
            if P then
              Error('Only one primary/unique field constraint allowed');
            Result := TSQLFieldConstraint
              (CreateElement(TSQLUniqueFieldConstraint, APArent));
            GetNextToken;
            P := True;
          end;
        tsqlPrimary :
          begin
            if P then
              Error('Only one primary/unique field constraint allowed');
            GetNextToken;
            Expect(tsqlKey);
            Result := TSQLFieldConstraint
              (CreateElement(TSQLPrimaryKeyFieldConstraint, APArent));
            GetNextToken;
            P := True;
          end;
        tsqlReferences :
          begin
            K := TSQLForeignKeyFieldConstraint
              (CreateElement(TSQLForeignKeyFieldConstraint, APArent));
            Result := K;
            K.Definition := ParseForeignKeyDefinition(K);
          end;
        tsqlCheck :
          begin
            C := TSQLCheckFieldConstraint
              (CreateElement(TSQLCheckFieldConstraint, APArent));
            Result := C;
            C.Expression := ParseCheckConstraint(K, True);
          end
      else
        UnexpectedToken([tsqlUnique, tsqlPrimary, tsqlReferences, tsqlCheck]);
      end;
      if (N <> '') then
        Result.ConstraintName := CreateIdentifier(Result, N);
    Until Not(CurrentToken in [tsqlUnique, tsqlPrimary, tsqlReferences,
      tsqlCheck, tsqlConstraint]);
    if Assigned(L) then
    begin
      L.List.Add(Result);
      Result := L;
    end;
  except
    if (L <> Result) then
      FreeAndNil(L);
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseTypeDefinition(APArent : TSQLElement;
  Flags : TParseTypeFlags) : TSQLTypeDefinition;

var
  TN       : string;
  AD       : integer;
  DT       : TSQLDataType;
  AA       : Boolean; // Allow array
  GN       : Boolean; // Do GetNextToken ?
  NN       : Boolean; // Not Null ?
  sc, prec : integer;
  bt       : integer;
  D        : TSQLTypeDefinition;
  cs       : TSQLStringType;
  Coll     : TSQLCollation;
  C        : TSQLFieldConstraint;

begin
 // We are positioned on the token prior to the type definition.
  AA := True;
  GN := True;
  prec := 0;
  sc := 0;
  bt := 0;
  NN := True;
  Coll := nil;
  case GetNextToken of
    tsqlIdentifier :
      if not(ptfAllowDomainName in Flags) then
        Error(SErrDomainNotAllowed)
      else
      begin
        DT := sdtDomain;
        TN := CurrentTokenString;
      end;
    tsqlInt,
      tsqlInteger :
      DT := sdtInteger;
    tsqlSmallInt :
      DT := sdtSmallInt;
    tsqlDate:
      DT := sdtDate;
    tsqlTimeStamp:
      DT := sdtDateTime;
    tsqlFloat:
      DT := sdtFloat;
    tsqlTime:
      DT := sdtTime;
    tsqlDecimal,
      tsqlNumeric:
      begin
        if CurrentToken = tsqlDecimal then
          DT := sdtDecimal
        else
          DT := sdtNumeric;
        GetNextToken;
        GN := False;
        if (CurrentToken = tsqlBraceOpen) then
        begin
          GetNextToken;
          Expect(tsqlIntegerNumber);
          prec := StrToInt(CurrentTokenString);
          if (GetNextToken = tsqlBraceClose) then
            sc := 0
          else
          begin
            GetNextToken;
            Expect(tsqlIntegerNumber);
            sc := StrToInt(CurrentTokenString);
            GetNextToken;
            Expect(tsqlBraceClose);
          end;
          GetNextToken;
          // position on first token after closing brace. GN=False !
        end;
      end;
    tsqlCString,
      tsqlChar,
      tsqlNCHAR,
      tsqlVarChar,
      tsqlCharacter,
      tsqlNational :
      begin
        if (CurrentToken = tsqlCString) and
          Not(([ptfExternalFunction, ptfExternalFunctionResult] * Flags)
          <> []) then
          UnexpectedToken;
        GN := False;
        ParseCharTypeDefinition(DT, prec, cs);
      end;
    tsqlBlob :
      begin
        DT := sdtBlob;
        GN := False;
        ParseBlobDefinition(prec, bt, cs);
      end;
  else
    UnexpectedToken;
  end;
  if GN then
    GetNextToken;
  // We are now on array or rest of type.
  if (CurrentToken = tsqlSquareBraceOpen) then
  begin
    GetNextToken;
    Expect(tsqlIntegerNumber);
    AD := StrToInt(CurrentTokenString);
    GetNextToken;
    Expect(tsqlSquareBraceClose);
    GetNextToken;
  end
  else
    AD := 0;
  // Collation is here in domain (needs checking ?)
  if (CurrentToken = tsqlCollate) then
  begin
    if not(DT in [sdtChar, sdtVarChar, sdtNchar, sdtNVARCHAR, sdtBlob]) then
      Error(SErrInvalidUseOfCollate);
    GetNextToken;
    Expect(tsqlIdentifier);
    Coll := TSQLCollation(CreateElement(TSQLCollation, APArent));
    Coll.Name := CurrentTokenString;
    GetNextToken;
  end
  else
    Coll := nil;
  C := nil;
  D := TSQLTypeDefinition(CreateElement(TSQLTypeDefinition, APArent));
  try
    D.DataType := DT;
    D.TypeName := TN;
    D.Len := prec;
    D.Scale := sc;
    D.BlobType := bt;
    D.ArrayDim := AD;
    D.Charset := cs;
    D.Collation := Coll;
    D.Constraint := C;
    if (not(ptfAlterDomain in Flags)) then
    // Alternative is to raise an error in each of the following
    begin
      if (CurrentToken = tsqlDefault) then
      begin
        GetNextToken;
        D.DefaultValue := CreateLiteral(D);
        GetNextToken;
      end;
      if (CurrentToken = tsqlNot) then
      begin
        GetNextToken;
        Expect(tsqlNull);
        D.NotNull := True;
        GetNextToken;
      end;
      if (CurrentToken = tsqlCheck) and not(ptfTableFieldDef in Flags) then
      begin
        D.Check := ParseCheckConstraint(D, False);
        // Parsecheckconstraint is on next token.
      end;
      // Constraint is before collation.
      if CurrentToken in [tsqlConstraint, tsqlCheck, tsqlUnique, tsqlPrimary,
        tsqlReferences] then
      begin
        if Not(ptfAllowConstraint in Flags) then
          UnexpectedToken;
        D.Constraint := ParseFieldConstraint(APArent);
      end;
      // table definition can have PRIMARY KEY CHECK
      if (CurrentToken = tsqlCheck) and (ptfTableFieldDef in Flags) then
      begin
        D.Check := ParseCheckConstraint(D, False);
        // Parsecheckconstraint is on next token.
      end;
      // Collation is after constraint in table
      if (CurrentToken = tsqlCollate) then
      begin
        if not(DT in [sdtChar, sdtVarChar, sdtNchar, sdtNVARCHAR, sdtBlob]) then
          Error(SErrInvalidUseOfCollate);
        GetNextToken;
        Expect(tsqlIdentifier);
        Coll := TSQLCollation(CreateElement(TSQLCollation, APArent));
        Coll.Name := CurrentTokenString;
        GetNextToken;
      end
      else
        Coll := nil;
      if (CurrentToken = tsqlBy) and (ptfExternalFunctionResult in Flags) then
      begin
        GetNextToken;
        Consume(tsqlValue);
        D.ByValue := True;
      end;
    end;
    Result := D;
  except
    FreeAndNil(D);
    raise;
  end;
end;

function TSQLParser.CreateLiteral(APArent : TSQLElement) : TSQLLiteral;

begin
  Result := nil;
  case CurrentToken of
    tsqlIntegerNumber:
      begin
        Result := TSQLLiteral(CreateElement(TSQLIntegerLiteral, APArent));
        TSQLIntegerLiteral(Result).Value := StrToInt(CurrentTokenString);
      end;
    tsqlString:
      begin
        Result := TSQLLiteral(CreateElement(TSQLStringLiteral, APArent));
        TSQLStringLiteral(Result).Value := CurrentTokenString;
      end;
    tsqlFloatNumber:
      begin
        Result := TSQLLiteral(CreateElement(TSQLFloatLiteral, APArent));
        TSQLFloatLiteral(Result).Value := StrToFloat(CurrentTokenString);
      end;
    tsqlNull :
      Result := TSQLLiteral(CreateElement(TSQLNullLiteral, APArent));
    tsqlValue :
      Result := TSQLLiteral(CreateElement(TSQLValueLiteral, APArent));
    tsqlUSER :
      Result := TSQLLiteral(CreateElement(TSQLUserLiteral, APArent));
  else
    Error(SErrInvalidLiteral, [CurrentTokenString]);
  end;
end;

procedure TSQLParser.CheckEOF;

begin
  if CurrentToken = tsqlEOF then
    Error('Unexpected end of command');
end;

function TSQLParser.ParseExprLevel1(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

var
  tt   : TSQLToken;
  B    : TSQLBinaryExpression;
  Right: TSQLExpression;
  L    : TSQLLiteralExpression;

begin
  Result := ParseExprLevel2(APArent, EO);
  Try
    while (CurrentToken in [tsqlAnd, tsqlOr{,tsqlIs}]) do
    begin
      tt := CurrentToken;
      GetNextToken;
      CheckEOF;
      B := TSQLBinaryExpression(CreateElement(TSQLBinaryExpression, APArent));
      B.Left := TSQLExpression(Result);
      Result := B;
      if tt = tsqlIs then
      begin
        if CurrentToken = tsqlNot then
        begin
//           B.Operation:=boIsNot;
          GetNextToken;
        end
        else
          B.Operation := boIs;
        Expect(tsqlNull);
        L := TSQLLiteralExpression(CreateElement(TSQLLiteralExpression,
          APArent));
        L.Literal := CreateLiteral(APArent);
        B.Right := L;
        GetNextToken;
      end
      else
      begin
        case tt of
          tsqlOr :
            B.Operation := boOr;
          tsqlAnd :
            B.Operation := boAnd;
        Else
          Error(SErrUnknownBooleanOp)
        end;
        B.Right := ParseExprLevel2(APArent, EO);
      end;
    end;
  Except
    Result.free;
    raise;
  end;
end;

function TSQLParser.ParseInoperand(APArent : TSQLElement) : TSQLExpression;

var
  S    : TSQLSelectExpression;
  L    : TSQLListExpression;
  Done : Boolean;

begin
  // On entry, we're on the first token after IN token, which is the ( token.
  Consume(tsqlBraceOpen);
  try
    if (CurrentToken = tsqlSelect) then
    begin
      S := TSQLSelectExpression(CreateElement(TSQLSelectExpression, APArent));
      Result := S;
      S.Select := ParseSelectStatement(APArent, [sfSingleTon]);
      Consume(tsqlBraceClose);
    end
    else
    begin
      L := TSQLListExpression(CreateElement(TSQLListExpression, APArent));
      Result := L;
      Repeat
        L.List.Add(ParseExprLevel1(L, [eoListValue]));
        Expect([tsqlBraceClose, tsqlComma]);
        Done := (CurrentToken = tsqlBraceClose);
        GetNextToken;
      until Done;

    end;
  except
    FreeAndNil(Result);
  end;
end;

function TSQLParser.ParseExprLevel2(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;
var
  tt          : TSQLToken;
  Right       : TSQLExpression;
  B           : TSQLBinaryExpression;
  T           : TSQLTernaryExpression;
  O           : TSQLBinaryOperation;
  U           : TSQLUnaryExpression;
  I, bw, doin : Boolean;

begin

{$IFDEF debugexpr} Writeln('Level 2 ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

  Result := ParseExprLevel3(APArent, EO);
  try
    if (CurrentToken in sqlComparisons) then
    begin
      tt := CurrentToken;
      I := CurrentToken = tsqlNot;
      CheckEOF;
      GetNextToken;
      CheckEOF;
      if I then
      begin
        tt := CurrentToken;
        if Not(tt in sqlInvertableComparisons) then
          Error(SErrUnexpectedToken, [CurrentTokenString]);
        GetNextToken;
        if (tt = tsqlStarting) and (CurrentToken = tsqlWith) then
          GetNextToken;
      end
      else
        if (CurrentToken = tsqlNot) then
      begin
        GetNextToken;
        if not(tt = tsqlIs) then
          UnexpectedToken;
        I := True;
      end;

      bw := False;
      doin := False;
      case tt of
        tsqlLT :
          O := boLT;
        tsqlLE :
          O := boLE;
        tsqlGT :
          O := boGT;
        tsqlGE :
          O := boGE;
        tsqlEq :
          O := boEq;
        tsqlNE :
          O := boNE;
        tsqlLike :
          O := boLike;
        tsqlIn :
          doin := True;
        tsqlIs :
          O := boIs;
        tsqlContaining :
          O := boContaining;
        tsqlStarting :
          O := boStarting;
        tsqlBetween :
          bw := True;
      Else
        Error(SErrUnknownComparison)
      end;
      if doin then
      begin
        Right := ParseInoperand(APArent);
        B := TSQLBinaryExpression(CreateElement(TSQLBinaryExpression, APArent));
        B.Operation := boIn;
        B.Left := Result;
        Result := B;
        B.Right := Right;
      end
      else
      begin
        Right := ParseExprLevel3(APArent, EO);
        if (O = boLike) and (CurrentToken = tsqlEscape) then
        begin
          GetNextToken;
          T := TSQLTernaryExpression(CreateElement(TSQLTernaryExpression,
            APArent));
          T.Left := Result;
          Result := T;
          T.Middle := Right;
          T.Right := ParseExprLevel3(APArent, EO);
          T.Operation := toLikeEscape
        end
        else if bw then
        begin
          Consume(tsqlAnd);
          T := TSQLTernaryExpression(CreateElement(TSQLTernaryExpression,
            APArent));
          T.Left := Result;
          Result := T;
          T.Middle := Right;
          T.Right := ParseExprLevel3(APArent, EO);
          T.Operation := toBetween;
        end
        else
        begin
          B := TSQLBinaryExpression(CreateElement(TSQLBinaryExpression,
            APArent));
          B.Operation := O;
          B.Left := Result;
          Result := B;
          B.Right := Right;
        end;
      end;
      if I then
        if B.Operation = boIs then
          B.Operation := boIsNot
        else
        begin
          U := TSQLUnaryExpression(CreateElement(TSQLUnaryExpression, APArent));
          U.Operand := Result;
          U.Operation := uoNot;
          Result := U;
        end;
    end;
  Except
    Result.free;
    raise;
  end;
end;

function TSQLParser.ParseExprLevel3(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

  function NegativeNumber : Boolean; inline;

  begin
    Result := (CurrentToken in [tsqlIntegerNumber, tsqlFloatNumber]) and
      (StrToInt(CurrentTokenString) < 0)
  end;

var
  tt    : TSQLToken;
  Right : TSQLExpression;
  B     : TSQLBinaryExpression;

begin

{$IFDEF debugexpr} Writeln('Level 3 ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

  Result := ParseExprLevel4(APArent, EO);
  try

{$IFDEF debugexpr} Writeln('Level 3 continues ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

// Scanner returns -N as an negative number, not as - (positive number)
    // NegativeNumber is for the case A-1 or so: convert to A + -1
    while (CurrentToken in [tsqlConcatenate, tsqlPlus, tsqlMinus]) or
      NegativeNumber do
    begin
      tt := CurrentToken;
      if NegativeNumber then
        tt := tsqlPlus // Pretend we've eaten +
      else
      begin
        GetNextToken;
        CheckEOF;
      end;
      Right := ParseExprLevel4(APArent, EO);
      B := TSQLBinaryExpression(CreateElement(TSQLBinaryExpression, APArent));
      B.Left := Result;
      Result := B;
      B.Right := Right;
      case tt of
        tsqlPlus :
          B.Operation := boAdd;
        tsqlMinus :
          B.Operation := boSubtract;
        tsqlConcatenate :
          B.Operation := boConcat;
      end;
    end;
  Except
    Result.free;
    raise;
  end;
end;

function TSQLParser.ParseExprLevel4(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

var
  tt    : TSQLToken;
  Right : TSQLExpression;
  B     : TSQLBinaryExpression;

begin

{$IFDEF debugexpr} Writeln('Level 4 ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

  Result := ParseExprLevel5(APArent, EO);
  try
    while (CurrentToken in [tsqlMul, tsqlDiv]) do
    begin
      tt := CurrentToken;
      GetNextToken;
      CheckEOF;
      Right := ParseExprLevel5(APArent, EO);
      B := TSQLBinaryExpression(CreateElement(TSQLBinaryExpression, APArent));
      B.Left := Result;
      B.Right := Right;
      case tt of
        tsqlMul :
          B.Operation := boMultiply;
        tsqlDiv :
          B.Operation := boDivide;
      end;
    end;
  Except
    Result.free;
    raise;
  end;
end;

function TSQLParser.ParseExprLevel5(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

var
  tt : TSQLToken;
  U  : TSQLUnaryExpression;
begin

{$IFDEF debugexpr} Writeln('Level 5 ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

  tt := tsqlunknown;
  if (CurrentToken in [tsqlNot, tsqlPlus, tsqlMinus]) then
  begin
    tt := CurrentToken;
    GetNextToken;
    CheckEOF;
  end;
  Result := ParseExprLevel6(APArent, EO);
  try
    if tt <> tsqlunknown then
    begin
      U := TSQLUnaryExpression(CreateElement(TSQLUnaryExpression, APArent));
      if tt = tsqlNot then
        U.Operation := uoNot
      else
        U.Operation := uoMinus;
      U.Operand := Result;
      Result := U;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseExprLevel6(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

begin

{$IFDEF debugexpr} Writeln('Level 6 ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

  if (CurrentToken = tsqlBraceOpen) then
  begin
    GetNextToken;
    if (CurrentToken <> tsqlSelect) then
      Result := ParseExprLevel1(APArent, EO)
    else
    begin
      Result := TSQLExpression(CreateElement(TSQLSelectExpression, APArent));
      try
        TSQLSelectExpression(Result).Select := ParseSelectStatement(Result,
          [sfSingleTon]);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
    try
      if (CurrentToken <> tsqlBraceClose) then
        Error(SerrUnmatchedBrace);
      GetNextToken;
    Except
      Result.free;
      raise;
    end;
  end
  else
    Result := ParseExprPrimitive(APArent, EO);
end;

function TSQLParser.ParseIdentifierList(APArent : TSQLElement;
  AList : TSQLelementList) : integer;

var
  Done : Boolean;

begin
  // on entry, we're on first identifier
  Expect(tsqlIdentifier);
  Result := 0;
  Done := False;
  repeat
    if CurrentToken = tsqlComma then
      GetNextToken;
    Expect(tsqlIdentifier);
    AList.Add(CreateIdentifier(APArent, CurrentTokenString));
    Inc(Result);
  until (GetNextToken <> tsqlComma);
  Expect(tsqlBraceClose);
  GetNextToken;
end;

function TSQLParser.ParseValueList(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLelementList;

var
  E : TSQLExpression;

begin
  Result := nil;
  E := nil;
  // First token is (
  Expect(tsqlBraceOpen);
  Repeat
    GetNextToken;
    if (CurrentToken <> tsqlBraceClose) then
      E := ParseExprLevel1(APArent, EO);
    if (E <> nil) then
    begin
      if Result = nil then
        Result := TSQLelementList.Create(True);
      Result.Add(E);
    end;
    Expect([tsqlComma, tsqlBraceClose]);
  Until CurrentToken = tsqlBraceClose;
end;

procedure TSQLParser.UnexpectedToken;
begin
  Error(SErrUnexpectedToken, [CurrentTokenString]);
end;

procedure TSQLParser.UnexpectedToken(AExpected: TSQLTokens);

var
  S : string;
  I : TSQLToken;

begin
  S := '';
  For I := Low(TSQLToken) to High(TSQLToken) do
    if I in AExpected then
    begin
      if (S <> '') then
        S := S + ',';
      S := S + TokenInfos[I];
    end;
  Error(SErrUnexpectedTokenOf, [CurrentTokenString, S]);
end;

function TSQLParser.CreateIdentifier(APArent : TSQLElement; const AName: TSQLStringType
  ): TSQLIdentifierName;
begin
  Result := TSQLIdentifierName(CreateElement(TSQLIdentifierName, APArent));
  Result.Name := AName;
end;

function TSQLParser.ParseExprAggregate(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLAggregateFunctionExpression;
begin
  Result := TSQLAggregateFunctionExpression
    (CreateElement(TSQLAggregateFunctionExpression, APArent));
  try
    case CurrentToken of
      tsqlCount :
        Result.Aggregate := afCount;
      tsqlSum :
        Result.Aggregate := afSum;
      tsqlAvg :
        Result.Aggregate := afAvg;
      tsqlMax :
        Result.Aggregate := afMax;
      tsqlMin :
        Result.Aggregate := afMin;
    end;
    GetNextToken;
    Consume(tsqlBraceOpen);
    if CurrentToken = tsqlMul then
    begin
      if Result.Aggregate <> afCount then
        Error(SErrAsteriskOnlyInCount);
      Result.OPtion := aoAsterisk;
      GetNextToken;
    end
    else
    begin
      if (CurrentToken in [tsqlAll, tsqlDistinct]) then
      begin
        if CurrentToken = tsqlAll then
          Result.OPtion := aoAll
        else
          Result.OPtion := aoDistinct;
        GetNextToken;
      end;
      Result.Expression := ParseExprLevel1(Result, EO);
    end;
    Consume(tsqlBraceClose);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseExprPrimitive(APArent : TSQLElement;
  EO : TExpressionOptions) : TSQLExpression;

var
  L : TSQLelementList;
  N : string;
  C : TSQLElementClass;
  E : TSQLExtractElement;

begin
  Result := nil;
  try

{$IFDEF debugexpr} Writeln('Primitive ', TokenInfos[CurrentToken], ': ', CurrentTokenString); {$ENDIF debugexpr}

    case CurrentToken of
      tsqlIntegerNumber,
        tsqlString,
        tsqlFloatNumber,
        tsqlNull, // True and False belong here
      tsqlValue,
        tsqlUSER:
        begin
          Result := TSQLLiteralExpression(CreateElement(TSQLLiteralExpression,
            APArent));
          TSQLLiteralExpression(Result).Literal := CreateLiteral(APArent);
          GetNextToken;
        end;
      tsqlCast:
        begin
          GetNextToken;
          Consume(tsqlBraceOpen);
          Result := TSQLCastExpression(CreateElement(TSQLCastExpression,
            APArent));
          TSQLCastExpression(Result).Value := ParseExprLevel1(Result, EO);
          Expect(tsqlAs);
          TSQLCastExpression(Result).NewType :=
            ParseTypeDefinition(Result, [ptfCast]);
          Consume(tsqlBraceClose);
        end;
      tsqlExtract:
        begin
          GetNextToken;
          Consume(tsqlBraceOpen);
          Expect(tsqlIdentifier);
          if not StringToSQLExtractElement(CurrentTokenString, E) then
            Error(SErrInvalidExtract, [CurrentTokenString]);
          Consume(tsqlIdentifier);
          Consume(tsqlFrom);
          Result := TSQLExtractExpression(CreateElement(TSQLExtractExpression,
            APArent));
          TSQLExtractExpression(Result).Element := E;
          TSQLExtractExpression(Result).Value := ParseExprLevel1(Result, EO);
          Consume(tsqlBraceClose);
        end;
      tsqlExists,
        tsqlAll,
        tsqlAny,
        tsqlSome,
        tsqlSingular:
        begin
          case CurrentToken of
            tsqlExists :
              C := TSQLexistsExpression;
            tsqlAll :
              C := TSQLAllExpression;
            tsqlAny :
              C := TSQLAnyExpression;
            tsqlSome :
              C := TSQLSomeExpression;
            tsqlSingular :
              C := TSQLSingularExpression;
          end;
          GetNextToken;
          Consume(tsqlBraceOpen);
          Result := TSQLSelectionExpression(CreateElement(C, APArent));
          TSQLSelectionExpression(Result).Select :=
            ParseSelectStatement(Result, []);
          Consume(tsqlBraceClose);
        end;
      tsqlCount,
        tsqlSum,
        tsqlAvg,
        tsqlMax,
        tsqlMin :
        begin
          if not([eoSelectvalue, eoHaving] * EO <> []) then
            Error(SErrNoAggregateAllowed);
          Result := ParseExprAggregate(APArent, EO);
        end;
      tsqlUpper :
        begin
          GetNextToken;
          L := ParseValueList(APArent, EO);
          if L.Count <> 1 then
          begin
            FreeAndNil(L);
            Error(SErrUpperOneArgument);
          end;
          GetNextToken; // Consume );
          Result := TSQLFunctionCallExpression
            (CreateElement(TSQLFunctionCallExpression, APArent));
          TSQLFunctionCallExpression(Result).IDentifier := 'UPPER';
          TSQLFunctionCallExpression(Result).Arguments := L;
        end;
      tsqlGenID :
        begin
          GetNextToken;
          Consume(tsqlBraceOpen);
          Expect(tsqlIdentifier);
          N := CurrentTokenString;
          GetNextToken;
          Consume(tsqlComma);
          Result := TSQLGenIDExpression(CreateElement(TSQLGenIDExpression,
            APArent));
          TSQLGenIDExpression(Result).Generator := CreateIdentifier(Result, N);
          TSQLGenIDExpression(Result).Value := ParseExprLevel1(APArent, EO);
          Consume(tsqlBraceClose);
        end;
      tsqlColon:
        begin
          if (([eoCheckConstraint, eoTableConstraint, eoComputedBy] * EO)
            <> []) then
            Error(SErrUnexpectedToken, [CurrentTokenString]);
          GetNextToken;
          Expect(tsqlIdentifier);
          N := CurrentTokenString;
          Result := TSQLParameterExpression
            (CreateElement(TSQLParameterExpression, APArent));
          TSQLParameterExpression(Result).IDentifier :=
            CreateIdentifier(Result, N);
          Consume(tsqlIdentifier);
        end;
      tsqlIdentifier:
        begin
          N := CurrentTokenString;
          if (GetNextToken <> tsqlBraceOpen) then
          begin
            if (eoCheckConstraint in EO) and not(eoTableConstraint in EO) then
              Error(SErrUnexpectedToken, [CurrentTokenString]);
            if (CurrentToken = tsqlDot) then
            begin
              GetNextToken;
              Expect(tsqlIdentifier);
              N := N + '.' + CurrentTokenString;
              GetNextToken;
            end;
          // plain identifier
            Result := TSQLIdentifierExpression
              (CreateElement(TSQLIdentifierExpression, APArent));
            TSQLIdentifierExpression(Result).IDentifier :=
              CreateIdentifier(Result, N);
          // array access ?
            if (CurrentToken = tsqlSquareBraceOpen) then
            begin
              if (GetNextToken <> tsqlIntegerNumber) then
                Error(SErrIntegerExpected);
              TSQLIdentifierExpression(Result).ElementIndex :=
                StrToInt(CurrentTokenString);
              GetNextToken;
              Consume(tsqlSquareBraceClose);
            end;
          end
          else
          begin
            L := ParseValueList(APArent, EO);
            GetNextToken; // Consume );
          // function call
            Result := TSQLFunctionCallExpression
              (CreateElement(TSQLFunctionCallExpression, APArent));
            TSQLFunctionCallExpression(Result).IDentifier := N;
            TSQLFunctionCallExpression(Result).Arguments := L;
          end;
        end;
    else
      UnexpectedToken;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseSQLValue(APArent : TSQLElement) : TSQLExpression;

var
  E : TSQLExpression;
begin
  E := ParseExprLevel1(APArent, []);
  Result := E;
end;

function TSQLParser.ParseCheckConstraint(APArent : TSQLElement;
  TableConstraint : Boolean = False) : TSQLExpression;

var
  EO : TExpressionOptions;

begin
  // We are on the 'CHECK' token.
  GetNextToken;
  Consume(tsqlBraceOpen);
  EO := [eoCheckConstraint];
  if TableConstraint then
    EO := EO + [eoTableConstraint];
  Result := ParseExprLevel1(APArent, EO);
  Consume(tsqlBraceClose);
end;

function TSQLParser.ParseCreateDomainStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;

var
  D  : TSQLCreateDomainStatement;
  A  : TSQLAlterDomainStatement;
  N  : TSQLStringType;
  NN : Boolean;

begin
  Result := nil;
  GetNextToken;
  Expect(tsqlIdentifier);
  N := CurrentTokenString;
  if not IsAlter then
  begin
    D := TSQLCreateDomainStatement(CreateElement(TSQLCreateDomainStatement,
      APArent));
    try
      D.ObjectName := CreateIdentifier(D, N);
      if (PeekNextToken = tsqlAs) then
        GetNextToken;
      D.TypeDefinition := ParseTypeDefinition(D, [])
    except
      FreeAndNil(D);
      raise;
    end;
    Result := D;
  end
  else
  begin
    A := nil;
    NN := False;
    try
      case GetNextToken of
        tsqlSet:
          begin
            GetNextToken;
            Expect(tsqlDefault);
            GetNextToken;
            A := TSQLAlterDomainSetDefaultStatement
              (CreateElement(TSQLAlterDomainSetDefaultStatement, APArent));
            TSQLAlterDomainSetDefaultStatement(A).DefaultValue :=
              CreateLiteral(A);
          end;
        tsqlDrop:
          begin
            case GetNextToken of
              tsqlDefault :
                A := TSQLAlterDomainDropDefaultStatement
                  (CreateElement(TSQLAlterDomainDropDefaultStatement, APArent));
              tsqlConstraint :
                A := TSQLAlterDomainDropCheckStatement
                  (CreateElement(TSQLAlterDomainDropCheckStatement, APArent));
            else
              Error(SErrUnexpectedToken, [CurrentTokenString]);
            end;
          end;
        tsqlAdd:
          begin
            if (GetNextToken = tsqlConstraint) then
              GetNextToken;
            Expect(tsqlCheck);
            A := TSQLAlterDomainAddCheckStatement
              (CreateElement(TSQLAlterDomainAddCheckStatement, APArent));
            TSQLAlterDomainAddCheckStatement(A).Check :=
              ParseCheckConstraint(A);
            NN := True;
          end;
        tsqltype:
          begin
            A := TSQLAlterDomainTypeStatement
              (CreateElement(TSQLAlterDomainTypeStatement, APArent));
            TSQLAlterDomainTypeStatement(A).NewType :=
              ParseTypeDefinition(A, [ptfAlterDomain]);
            NN := True;
          end;
        tsqlIdentifier:
          begin
            A := TSQLAlterDomainRenameStatement
              (CreateElement(TSQLAlterDomainRenameStatement, APArent));
            TSQLAlterDomainRenameStatement(A).NewName :=
              CreateIdentifier(A, CurrentTokenString);
          end;
      else
        UnexpectedToken([tsqlSet, tsqlIdentifier, tsqlAdd, tsqltype, tsqlDrop]);
      end;
      A.ObjectName := CreateIdentifier(A, N);
      Result := A;
      if not NN then
        GetNextToken;
    except
      FreeAndNil(A);
      raise;
    end;
  end;
end;

function TSQLParser.ParseCreateExceptionStatement(APArent: TSQLElement;
  IsAlter: Boolean): TSQLCreateOrAlterStatement;

var
  E : TSQLCreateExceptionStatement;
  N : TSQLStringType;

begin
  GetNextToken;
  Expect(tsqlIdentifier);
  N := CurrentTokenString;
  try
    if IsAlter then
      E := TSQLCreateExceptionStatement
        (CreateElement(TSQLAlterExceptionStatement, APArent))
    else
      E := TSQLCreateExceptionStatement
        (CreateElement(TSQLCreateExceptionStatement, APArent));
    E.ObjectName := CreateIdentifier(E, N);
    GetNextToken;
    Expect(tsqlString);
    E.ExceptionMessage := TSQLStringLiteral
      (CreateElement(TSQLStringLiteral, E));
    E.ExceptionMessage.Value := CurrentTokenString;
    GetNextToken;
  except
    FreeAndNil(E);
    raise;
  end;
  Result := E;
end;

function TSQLParser.ParseCreateTriggerStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;

var
  T : TSQLAlterCreateTriggerStatement;

begin
  // On entry, we're on the 'TRIGGER' token.
  Consume(tsqlTrigger);
  if IsAlter then
    T := TSQLAlterTriggerStatement
      (CreateElement(TSQLAlterTriggerStatement, APArent))
  else
    T := TSQLCreateTriggerStatement(CreateElement(TSQLCreateTriggerStatement,
      APArent));
  Result := T;
  try
    Expect(tsqlIdentifier);
    Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    if Not IsAlter then
    begin
      Consume(tsqlFor);
      Expect(tsqlIdentifier);
      T.TableName := CreateIdentifier(Result, CurrentTokenString);
      GetNextToken;
    end;
    if (CurrentToken in [tsqlActive, tsqlInactive]) then
    begin
      if CurrentToken = tsqlActive then
        T.State := tsActive
      else
        T.State := tsInactive;
      GetNextToken;
    end;
    Expect([tsqlBefore, tsqlAfter]);
    if CurrentToken = tsqlBefore then
      T.Moment := tmBefore
    else
      T.Moment := tmAfter;
    Repeat
      GetNextToken;
      Expect([tsqlDelete, tsqlInsert, tsqlUpdate]);
      case CurrentToken of
        tsqlDelete :
          T.Operations := T.Operations + [toDelete];
        tsqlUpdate :
          T.Operations := T.Operations + [toUpdate];
        tsqlInsert :
          T.Operations := T.Operations + [toInsert];
      end;
      GetNextToken;
    Until (CurrentToken <> tsqlOr);
    if CurrentToken = tsqlPosition then
    begin
      GetNextToken;
      Expect(tsqlIntegerNumber);
      T.Position := StrToInt(CurrentTokenString);
      GetNextToken;
    end;
    Consume(tsqlAs);
    if (CurrentToken = tsqlDeclare) then
      ParseCreateProcedureVariableList(Result, T.LocalVariables);
    Expect(tsqlBegin);
    ParseStatementBlock(Result, T.Statements);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseSetGeneratorStatement(APArent: TSQLElement
  ): TSQLSetGeneratorStatement;
begin
  // On entry, we're on the 'GENERATOR' token
  Consume(tsqlGenerator);
  try
    Result := TSQLSetGeneratorStatement(CreateElement(TSQLSetGeneratorStatement,
      APArent));
    Expect(tsqlIdentifier);
    Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    Consume(tsqlTo);
    Expect(tsqlIntegerNumber);
    Result.NewValue := StrToInt(CurrentTokenString);
    GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseSecondaryFile(APArent: TSQLElement)
  : TSQLDatabaseFileInfo;

var
  I    : integer;
  Last : TSQLToken;

begin
  // On entry, we're on the FILE token
  Consume(tsqlFile);
  Result := TSQLDatabaseFileInfo(CreateElement(TSQLDatabaseFileInfo, APArent));
  try
    Expect(tsqlString);
    Result.FileName := CurrentTokenString;
    GetNextToken;
    I := 0;
    Last := tsqlEOF;
    while (I < 2) and (CurrentToken in [tsqlLength, tsqlStarting]) do
    begin
      Inc(I);
      if (CurrentToken = tsqlLength) then
      begin
        if Last = tsqlLength then
          UnexpectedToken;
        Last := tsqlLength;
        GetNextToken;
        if (CurrentToken = tsqlEq) then
          GetNextToken;
        Expect(tsqlIntegerNumber);
        Result.Length := StrToInt(CurrentTokenString);
        GetNextToken;
        if CurrentToken in [tsqlPage, tsqlPages] then
          GetNextToken;
      end
      else if (CurrentToken = tsqlStarting) then
      begin
        if Last = tsqlStarting then
          UnexpectedToken;
        Last := tsqlStarting;
        GetNextToken;
        if (CurrentToken = tsqlAt) then
        begin
          GetNextToken;
          if CurrentToken = tsqlPage then
            GetNextToken;
        end;
        Expect(tsqlIntegerNumber);
        Result.StartPage := StrToInt(CurrentTokenString);
        GetNextToken;
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseCreateDatabaseStatement(APArent: TSQLElement;
  IsAlter: Boolean) : TSQLCreateDatabaseStatement;

begin
  // On entry, we're on the DATABASE or SCHEMA token
  Result := TSQLCreateDatabaseStatement
    (CreateElement(TSQLCreateDatabaseStatement, APArent));
  try
    Result.UseSchema := (CurrentToken = tsqlSchema);
    GetNextToken;
    Expect(tsqlString);
    Result.FileName := CurrentTokenString;
    GetNextToken;
    if (CurrentToken = tsqlUSER) then
    begin
      GetNextToken;
      Expect(tsqlString);
      Result.UserName := CurrentTokenString;
      GetNextToken;
    end;
    if (CurrentToken = tsqlPassword) then
    begin
      GetNextToken;
      Expect(tsqlString);
      Result.Password := CurrentTokenString;
      GetNextToken;
    end;
    if (CurrentToken = tsqlPageSize) then
    begin
      GetNextToken;
      if CurrentToken = tsqlEq then
        GetNextToken;
      Expect(tsqlIntegerNumber);
      Result.Pagesize := StrtoIntDef(CurrentTokenString, 0);
      GetNextToken;
    end;
    if (CurrentToken = tsqlLength) then
    begin
      GetNextToken;
      if (CurrentToken = tsqlEq) then
        GetNextToken;
      Expect(tsqlIntegerNumber);
      Result.Length := StrtoIntDef(CurrentTokenString, 0);
      GetNextToken;
      if CurrentToken in [tsqlPage, tsqlPages] then
        GetNextToken;
    end;
    if (CurrentToken = tsqlDefault) then
    begin
      GetNextToken;
      Consume(tsqlCharacter);
      Consume(tsqlSet);
      Expect(tsqlIdentifier);
      Result.Charset := CreateIdentifier(Result, CurrentTokenString);
      GetNextToken;
    end;
    while (CurrentToken = tsqlFile) do
      Result.SecondaryFiles.Add(ParseSecondaryFile(Result));
  except
    FreeAndNil(Result);
    raise
  end;
end;

function TSQLParser.ParseCreateShadowStatement(APArent: TSQLElement;
  IsAlter: Boolean): TSQLCreateShadowStatement;
begin
  // On entry, we're on the SHADOW token.
  if IsAlter then
    UnexpectedToken;
  Consume(tsqlShadow);
  Result := TSQLCreateShadowStatement(CreateElement(TSQLCreateShadowStatement,
    APArent));
  try
    Expect(tsqlIntegerNumber);
    Result.Number := StrToInt(CurrentTokenString);
    GetNextToken;
    if (CurrentToken = tsqlManual) then
    begin
      Result.Manual := True;
      GetNextToken;
    end
    else if (CurrentToken = tsqlAuto) then
      GetNextToken;
    if (CurrentToken = tsqlConditional) then
    begin
      Result.Conditional := True;
      GetNextToken;
    end;
    Expect(tsqlString);
    Result.FileName := CurrentTokenString;
    GetNextToken;
    if (CurrentToken = tsqlLength) then
    begin
      GetNextToken;
      if (CurrentToken = tsqlEq) then
        GetNextToken;
      Expect(tsqlIntegerNumber);
      Result.Length := StrtoIntDef(CurrentTokenString, 0);
      GetNextToken;
      if CurrentToken in [tsqlPage, tsqlPages] then
        GetNextToken;
    end;
    while (CurrentToken = tsqlFile) do
      Result.SecondaryFiles.Add(ParseSecondaryFile(Result));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseAlterDatabaseStatement(APArent: TSQLElement;
  IsAlter: Boolean): TSQLAlterDatabaseStatement;
begin
  // On entry, we're on the DATABASE or SCHEMA token.
  Result := TSQLAlterDatabaseStatement(CreateElement(TSQLAlterDatabaseStatement,
    APArent));
  try
    Result.UseSchema := CurrentToken = tsqlSchema;
    GetNextToken;
    Expect(tsqlAdd);
    while (CurrentToken in [tsqlAdd, tsqlFile]) do
    begin
      if CurrentToken = tsqlAdd then
        GetNextToken;
      Expect(tsqlFile);
      Result.Operations.Add(ParseSecondaryFile(Result));
    end;
    if Result.Operations.Count = 0 then
      UnexpectedToken([tsqlAdd]);
  except
    FreeAndNil(Result);
    raise;
  end;

end;

function TSQLParser.ParseCreateStatement(APArent: TSQLElement; IsAlter: Boolean
  ): TSQLCreateOrAlterStatement;
begin
  case GetNextToken of
    tsqlTable :
      if IsAlter then
        Result := ParseAlterTableStatement(APArent)
      else
        Result := ParseCreateTableStatement(APArent);

    tsqlUnique,
      tsqlAscending,
      tsqlDescending,
      tsqlIndex :
      Result := ParseCreateIndexStatement(APArent, IsAlter);
    tsqlView :
      Result := ParseCreateViewStatement(APArent, IsAlter);
    tsqlProcedure :
      Result := ParseCreateProcedureStatement(APArent, IsAlter);
    tsqlDomain :
      Result := ParseCreateDomainStatement(APArent, IsAlter);
    tsqlGenerator :
      Result := ParseCreateGeneratorStatement(APArent, IsAlter);
    tsqlException :
      Result := ParseCreateExceptionStatement(APArent, IsAlter);
    tsqlTrigger :
      Result := ParseCreateTriggerStatement(APArent, IsAlter);
    tsqlRole :
      Result := ParseCreateRoleStatement(APArent, IsAlter);
    tsqlSchema,
      tsqlDatabase :
      if IsAlter then
        Result := ParseAlterDatabaseStatement(APArent, IsAlter)
      else
        Result := ParseCreateDatabaseStatement(APArent, IsAlter);
    tsqlShadow :
      Result := ParseCreateShadowStatement(APArent, IsAlter);
  else
    Error(SErrExpectedDBObject, [CurrentTokenString]);
  end;
end;

function TSQLParser.ParseDropStatement(APArent: TSQLElement
  ): TSQLDropStatement;

var
  C : TSQLElementClass;

begin
  // We're positioned on the DROP token.
  C := nil;
  case GetNextToken of
    {
    Filter,
    }
    tsqlExternal :
      begin
        GetNextToken;
        Expect(tsqlFunction);
        C := TSQLDropExternalFunctionStatement;
      end;
    tsqlShadow :
      C := TSQLDropShadowStatement;
    tsqlRole :
      C := TSQLDropRoleStatement;
    tsqlDatabase :
      C := TSQLDropDatabaseStatement;
    tsqlException :
      C := TSQLDropExceptionStatement;
    tsqlTable :
      C := TSQLDropTableStatement;
    tsqlIndex :
      C := TSQLDropIndexStatement;
    tsqlView :
      C := TSQLDropViewStatement;
    tsqlProcedure :
      C := TSQLDropProcedureStatement;
    tsqlDomain :
      C := TSQLDropDomainStatement;
    tsqlGenerator :
      C := TSQLDropGeneratorStatement;
    tsqlTrigger :
      C := TSQLDropTriggerStatement;
  else
    Error(SErrExpectedDBObject, [CurrentTokenString]);
  end;
  GetNextToken;
  if C = TSQLDropShadowStatement then
    Expect(tsqlIntegerNumber)
  else
    Expect(tsqlIdentifier);
  Result := TSQLDropStatement(CreateElement(C, APArent));
  Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
  GetNextToken; // Comma
end;

function TSQLParser.ParseRollbackStatement(APArent: TSQLElement
  ): TSQLRollbackStatement;

begin
  // On entry, we're on the ROLLBACK statement
  Consume(tsqlRollBack);
  Result := TSQLRollbackStatement(CreateElement(TSQLRollbackStatement,
    APArent));
  try
    if (CurrentToken = tsqlTransaction) then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
      Result.TransactionName := CreateIdentifier(Result, CurrentTokenString);
      GetNextToken;
    end;
    Result.Work := (CurrentToken = tsqlWork);
    if Result.Work then
      GetNextToken;
    Result.Release := (CurrentToken = tsqlRelease);
    if Result.Release then
      GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseCommitStatement(APArent: TSQLElement
  ): TSQLCommitStatement;
begin
  Consume(tsqlCommit);
  Result := TSQLCommitStatement(CreateElement(TSQLCommitStatement, APArent));
  try
    Result.Work := (CurrentToken = tsqlWork);
    if Result.Work then
      GetNextToken;
    if (CurrentToken = tsqlTransaction) then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
      Result.TransactionName := CreateIdentifier(Result, CurrentTokenString);
      GetNextToken;
    end;
    Result.Release := (CurrentToken = tsqlRelease);
    if Result.Release then
      GetNextToken;
    Result.Retain := (CurrentToken = tsqlRetain);
    if Result.Retain then
    begin
      GetNextToken;
      if CurrentToken = tsqlSnapshot then
        GetNextToken;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseExecuteProcedureStatement(APArent: TSQLElement)
  : TSQLExecuteProcedureStatement;

var
  NeedClose,
    Done : Boolean;
  TN     : TSQLStringType;

begin
  Result := nil;
  // On Entry, we're on the EXECUTE statement
  Consume(tsqlExecute);
  Consume(tsqlProcedure);
  if (CurrentToken = tsqlTransaction) then
  begin
    GetNextToken;
    Expect(tsqlIdentifier);
    TN := CurrentTokenString;
    GetNextToken;
  end;
  Expect(tsqlIdentifier);
  Result := TSQLExecuteProcedureStatement
    (CreateElement(TSQLExecuteProcedureStatement, APArent));
  try
    Result.ProcedureName := CreateIdentifier(Result, CurrentTokenString);
    if (TN <> '') then
      Result.TransactionName := CreateIdentifier(Result, TN);
    GetNextToken;
    // ( is optional. It CAN be part of a (SELECT, and then it is NOT part of the brackets around the params.
    NeedClose := (CurrentToken = tsqlBraceOpen) and
      (PeekNextToken <> tsqlSelect);
    if NeedClose then
      GetNextToken;
    Done := False;
    if Not(CurrentToken in [tsqlSemicolon, tsqlEOF, tsqlReturningValues]) then
      Repeat
        Result.Params.Add(ParseExprLevel1(Result, [eoFieldValue]));
        if CurrentToken = tsqlComma then
          GetNextToken
        else if (CurrentToken = tsqlBraceClose) then
        begin
          if Not NeedClose then
            UnexpectedToken;
          Done := True;
          GetNextToken;
        end
        else
        begin
          if NeedClose then
            UnexpectedToken([tsqlBraceClose]);
          Expect([tsqlEOF, tsqlSemicolon, tsqlReturningValues]);
          Done := True;
        end;
      until Done;
    if (CurrentToken = tsqlReturningValues) then
    begin
      GetNextToken;
      NeedClose := (CurrentToken = tsqlBraceOpen);
      if NeedClose then
        Consume(tsqlBraceOpen);
      Repeat
        if CurrentToken = tsqlComma then
          GetNextToken;
        if CurrentToken = tsqlColon then
          GetNextToken;
        Expect(tsqlIdentifier);
        Result.Returning.Add(CreateIdentifier(Result, CurrentTokenString));
        GetNextToken;
      until (CurrentToken <> tsqlComma);
      if NeedClose then
        Consume(tsqlBraceClose);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseSetStatement(APArent: TSQLElement): TSQLStatement;
begin
  // On Entry, we're on the set statement
  Consume(tsqlSet);
  case CurrentToken of
    tsqlGenerator :
      Result := ParseSetGeneratorStatement(APArent)
  else
    // For the time being
    UnexpectedToken;
  end;
end;

function TSQLParser.ParseConnectStatement(APArent: TSQLElement
  ): TSQLConnectStatement;
begin
  // On entry, we're on CONNECT
  Consume(tsqlConnect);
  Expect(tsqlString);
  Result := TSQLConnectStatement(CreateElement(TSQLConnectStatement, APArent));
  try
    Result.DatabaseName := CurrentTokenString;
    GetNextToken;
    if CurrentToken = tsqlUSER then
    begin
      GetNextToken;
      Expect(tsqlString);
      Result.UserName := CurrentTokenString;
      GetNextToken;
    end;
    if CurrentToken = tsqlPassword then
    begin
      GetNextToken;
      Expect(tsqlString);
      Result.Password := CurrentTokenString;
      GetNextToken;
    end;
    if CurrentToken = tsqlRole then
    begin
      GetNextToken;
      Expect(tsqlString);
      Result.Role := CurrentTokenString;
      GetNextToken;
    end;
    if CurrentToken = tsqlCache then
    begin
      GetNextToken;
      Expect(tsqlIntegerNumber);
      Result.Cache := StrtoIntDef(CurrentTokenString, 0);
      GetNextToken;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TSQLParser.Create(AInput: TStream);
begin
  FInput := AInput;
  FCurrent := tsqlunknown;
  FScanner := TSQLScanner.Create(FInput);
  FFreeScanner := True;
end;

constructor TSQLParser.Create(AScanner: TSQLScanner);
begin
  FCurrent := tsqlunknown;
  FScanner := AScanner;
  FFreeScanner := False;
end;

destructor TSQLParser.Destroy;
begin
  if FFreeScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

function TSQLParser.ParseDeclareFunctionStatement(APArent : TSQLElement)
  : TSQLDeclareExternalFunctionStatement;

begin
  // On entry, we're on the EXTERNAL token
  Consume(tsqlExternal);
  Consume(tsqlFunction);
  Expect(tsqlIdentifier);
  Result := TSQLDeclareExternalFunctionStatement
    (CreateElement(TSQLDeclareExternalFunctionStatement, APArent));
  try
    Result.ObjectName := CreateIdentifier(Result, CurrentTokenString);
    if (PeekNextToken = tsqlReturns) then
      GetNextToken
    else
      Repeat
        Result.Arguments.Add(Self.ParseTypeDefinition(Result,
          [ptfExternalFunction]));
      Until (CurrentToken <> tsqlComma);
    Expect(tsqlReturns);
    Result.ReturnType := ParseTypeDefinition(Result,
      [ptfExternalFunctionResult]);
    Result.FreeIt := (CurrentToken = tsqlFreeIt);
    if Result.FreeIt then
      GetNextToken;
    Consume(tsqlEntryPoint);
    Expect(tsqlString);
    Result.EntryPoint := CurrentTokenString;
    GetNextToken;
    Consume(tsqlModuleName);
    Expect(tsqlString);
    Result.ModuleName := CurrentTokenString;
    GetNextToken;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseDeclareStatement(APArent : TSQLElement)
  : TSQLStatement;

begin
  // On entry, we're on the DECLARE statement
  Consume(tsqlDeclare);
  // For the moment, only 'DECLARE EXTERNAL function' is supported
  case CurrentToken of
    tsqlExternal :
      Result := ParseDeclareFunctionStatement(APArent);
  else
    UnexpectedToken([tsqlExternal]);
  end;

end;

procedure TSQLParser.ParseGranteeList(APArent: TSQLElement;
  List: TSQLelementList; AllowObject, AllowGroup, AllowPublic: Boolean;
  IsRevoke: Boolean = False);

type
  TSQLGranteeClass = class of TSQLGrantee;

  function CreateGrantee(NextIdentifier : Boolean; AClass : TSQLGranteeClass)
    : TSQLGrantee;
  begin
    if NextIdentifier then
    begin
      GetNextToken;
      Expect(tsqlIdentifier);
    end;
    Result := TSQLGrantee(CreateElement(AClass, APArent));
    Result.Name := CurrentTokenString;
    List.Add(Result);
  end;

var
  E : TSQLTokens;

begin
  if IsRevoke then
    Consume(tsqlFrom)
  else
    Consume(tsqlTo);
  E := [tsqlIdentifier, tsqlUSER];
  if AllowObject then
    E := E + [tsqlProcedure, tsqlView, tsqlTrigger, tsqlPublic]
  else if AllowPublic then
    E := E + [tsqlPublic];
  if AllowGroup then
    E := E + [tsqlGroup];
  Expect(E);
  Repeat
    if CurrentToken = tsqlComma then
      GetNextToken;
    case CurrentToken of
      tsqlUSER,
        tsqlIdentifier :
        CreateGrantee(CurrentToken = tsqlUSER, TSQLUserGrantee);
      tsqlGroup :
        begin
          if Not AllowGroup then
            UnexpectedToken;
          CreateGrantee(True, TSQLGroupGrantee);
        end;
      tsqlPublic :
        begin
          if Not(AllowPublic or AllowObject) then
            UnexpectedToken;
          CreateGrantee(False, TSQLPublicGrantee);
        end;
      tsqlTrigger:
        begin
          if Not AllowObject then
            UnexpectedToken;
          CreateGrantee(True, TSQLTriggerGrantee);
        end;
      tsqlView:
        begin
          if Not AllowObject then
            UnexpectedToken;
          CreateGrantee(True, TSQLViewGrantee);
        end;
      tsqlProcedure:
        begin
          if Not AllowObject then
            UnexpectedToken;
          CreateGrantee(True, TSQLProcedureGrantee);
        end;
    end;
  Until (GetNextToken <> tsqlComma);

end;

function TSQLParser.ParseGrantTableStatement(APArent: TSQLElement)
  : TSQLTableGrantStatement;

var
  C : TSQLColumnPrivilege;
  P : TSQLPrivilege;

begin
  Result := TSQLTableGrantStatement(CreateElement(TSQLTableGrantStatement,
    APArent));
  try
    // On entry, we're on the first ALL/SELECT/UPDATE/INSERT/DELETE/REFERENCE etc. token.
    if CurrentToken = tsqlAll then
    begin
      Result.Privileges.Add(CreateElement(TSQLAllPrivilege, Result));
      if GetNextToken = tsqlPrivileges then
        GetNextToken;
    end
    else
      Repeat
        P := nil;
        C := nil;
        if CurrentToken = tsqlComma then
          GetNextToken;
        case CurrentToken of
          tsqlSelect :
            P := TSQLSelectPrivilege(CreateElement(TSQLSelectPrivilege,
              Result));
          tsqlInsert :
            P := TSQLInsertPrivilege(CreateElement(TSQLInsertPrivilege,
              Result));
          tsqlDelete :
            P := TSQLDeletePrivilege(CreateElement(TSQLDeletePrivilege,
              Result));
          tsqlUpdate,
            tsqlReferences :
            begin
              if CurrentToken = tsqlUpdate then
                C := TSQLUpdatePrivilege
                  (CreateElement(TSQLUpdatePrivilege, APArent))
              else
                C := TSQLReferencePrivilege
                  (CreateElement(TSQLReferencePrivilege, APArent));
              P := C;
              GetNextToken;
              if (CurrentToken = tsqlBraceOpen) then
              begin
                GetNextToken;
                C.Columns := TSQLelementList.Create(True);
                ParseIdentifierList(C, C.Columns);
              end;
            end;
        else
          UnexpectedToken([tsqlSelect, tsqlInsert, tsqlDelete, tsqlUpdate,
            tsqlReferences]);
        end;
        Result.Privileges.Add(P);
        if C = nil then
          GetNextToken;
      Until (CurrentToken <> tsqlComma);
    Consume(tsqlOn);
    Expect(tsqlIdentifier);
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    ParseGranteeList(Result, Result.Grantees, True, True, True);
    if (CurrentToken = tsqlWith) then
    begin
      Consume(tsqlWith);
      Consume(tsqlGrant);
      Consume(tsqlOption);
      Result.GrantOption := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseRevokeExecuteStatement(APArent: TSQLElement
  ): TSQLProcedureRevokeStatement;
BEGIN
  // On entry, we're on the EXECUTE token
  Consume(tsqlExecute);
  Consume(tsqlOn);
  Consume(tsqlProcedure);
  Expect(tsqlIdentifier);
  Result := TSQLProcedureRevokeStatement
    (CreateElement(TSQLProcedureRevokeStatement, APArent));
  try
    Result.ProcedureName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    ParseGranteeList(Result, Result.Grantees, True, False, True, True);
    if (CurrentToken = tsqlWith) then
    begin
      Consume(tsqlWith);
      Consume(tsqlGrant);
      Consume(tsqlOption);
      Result.GrantOption := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseRevokeRoleStatement(APArent: TSQLElement
  ): TSQLRoleRevokeStatement;
begin
  Result := nil;
  // On entry, we're on the identifier token
  Expect(tsqlIdentifier);
  Result := TSQLRoleRevokeStatement(CreateElement(TSQLRoleRevokeStatement,
    APArent));
  try
    Repeat
      if CurrentToken = tsqlComma then
        GetNextToken;
      Expect(tsqlIdentifier);
      Result.Roles.Add(CreateIdentifier(APArent, CurrentTokenString));
    Until (GetNextToken <> tsqlComma);
    Expect(tsqlFrom);
    ParseGranteeList(Result, Result.Grantees, False, False, True, True);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseRevokeTableStatement(APArent: TSQLElement
  ): TSQLTableRevokeStatement;
var
  C : TSQLColumnPrivilege;
  P : TSQLPrivilege;

begin
  Result := TSQLTableRevokeStatement(CreateElement(TSQLTableRevokeStatement,
    APArent));
  try
    // On entry, we're on the first GRANT,ALL/SELECT/UPDATE/INSERT/DELETE/REFERENCE etc. token.
    if (CurrentToken = tsqlGrant) then
    begin
      Consume(tsqlGrant);
      Consume(tsqlOption);
      Consume(tsqlFor);
      Result.GrantOption := True;
    end;
    if CurrentToken = tsqlAll then
    begin
      Result.Privileges.Add(CreateElement(TSQLAllPrivilege, Result));
      if GetNextToken = tsqlPrivileges then
        GetNextToken;
    end
    else
      Repeat
        P := nil;
        C := nil;
        if CurrentToken = tsqlComma then
          GetNextToken;
        case CurrentToken of
          tsqlSelect :
            P := TSQLSelectPrivilege(CreateElement(TSQLSelectPrivilege,
              Result));
          tsqlInsert :
            P := TSQLInsertPrivilege(CreateElement(TSQLInsertPrivilege,
              Result));
          tsqlDelete :
            P := TSQLDeletePrivilege(CreateElement(TSQLDeletePrivilege,
              Result));
          tsqlUpdate,
            tsqlReferences :
            begin
              if CurrentToken = tsqlUpdate then
                C := TSQLUpdatePrivilege
                  (CreateElement(TSQLUpdatePrivilege, APArent))
              else
                C := TSQLReferencePrivilege
                  (CreateElement(TSQLReferencePrivilege, APArent));
              P := C;
              GetNextToken;
              if (CurrentToken = tsqlBraceOpen) then
              begin
                GetNextToken;
                C.Columns := TSQLelementList.Create(True);
                ParseIdentifierList(C, C.Columns);
              end;
            end;
        else
          UnexpectedToken([tsqlSelect, tsqlInsert, tsqlDelete, tsqlUpdate,
            tsqlReferences]);
        end;
        Result.Privileges.Add(P);
        if C = nil then
          GetNextToken;
      Until (CurrentToken <> tsqlComma);
    Consume(tsqlOn);
    Expect(tsqlIdentifier);
    Result.TableName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    ParseGranteeList(Result, Result.Grantees, True, True, True, True);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseGrantExecuteStatement(APArent: TSQLElement)
  : TSQLProcedureGrantStatement;

begin
  // On entry, we're on the EXECUTE token
  Consume(tsqlExecute);
  Consume(tsqlOn);
  Consume(tsqlProcedure);
  Expect(tsqlIdentifier);
  Result := TSQLProcedureGrantStatement
    (CreateElement(TSQLProcedureGrantStatement, APArent));
  try
    Result.ProcedureName := CreateIdentifier(Result, CurrentTokenString);
    GetNextToken;
    ParseGranteeList(Result, Result.Grantees, True, False, True);
    if (CurrentToken = tsqlWith) then
    begin
      Consume(tsqlWith);
      Consume(tsqlGrant);
      Consume(tsqlOption);
      Result.GrantOption := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseGrantRoleStatement(APArent: TSQLElement)
  : TSQLRoleGrantStatement;

begin
  Result := nil;
  // On entry, we're on the identifier token
  Expect(tsqlIdentifier);
  Result := TSQLRoleGrantStatement(CreateElement(TSQLRoleGrantStatement,
    APArent));
  try
    Repeat
      if CurrentToken = tsqlComma then
        GetNextToken;
      Expect(tsqlIdentifier);
      Result.Roles.Add(CreateIdentifier(APArent, CurrentTokenString));
    Until (GetNextToken <> tsqlComma);
    Expect(tsqlTo);
    ParseGranteeList(Result, Result.Grantees, False, False, True);
    if (CurrentToken = tsqlWith) then
    begin
      Consume(tsqlWith);
      Consume(tsqlAdmin);
      Consume(tsqlOption);
      Result.AdminOption := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseGrantStatement(APArent: TSQLElement)
  : TSQLGrantStatement;

begin
  // On entry, we're on the GRANT token
  Result := nil;
  try
    Consume(tsqlGrant);
    case CurrentToken of
      tsqlExecute:
        Result := ParseGrantExecuteStatement(APArent);
      tsqlAll,
        tsqlUpdate,
        tsqlReferences,
        tsqlInsert,
        tsqlDelete,
        tsqlSelect :
        Result := ParseGrantTableStatement(APArent);
      tsqlIdentifier :
        Result := ParseGrantRoleStatement(APArent);
    else
      UnexpectedToken([tsqlIdentifier, tsqlExecute, tsqlAll,
        tsqlUpdate, tsqlDelete, tsqlReferences, tsqlInsert, tsqlSelect]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.ParseRevokeStatement(APArent: TSQLElement
  ): TSQLGrantStatement;
begin
  // On entry, we're on the GRANT token
  Result := nil;
  try
    Consume(tsqlRevoke);
    case CurrentToken of
      tsqlExecute:
        Result := ParseRevokeExecuteStatement(APArent);
      tsqlGrant,
        tsqlAll,
        tsqlUpdate,
        tsqlReferences,
        tsqlInsert,
        tsqlDelete,
        tsqlSelect :
        Result := ParseRevokeTableStatement(APArent);
      tsqlIdentifier :
        Result := ParseRevokeRoleStatement(APArent);
    else
      UnexpectedToken([tsqlIdentifier, tsqlExecute, tsqlGrant, tsqlAll,
        tsqlUpdate, tsqlDelete, tsqlReferences, tsqlInsert, tsqlSelect]);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLParser.Parse: TSQLElement;
begin
  GetNextToken;
  case CurrentToken of
    tsqlSelect :
      Result := ParseSelectStatement(nil, []);
    tsqlUpdate :
      Result := ParseUpdateStatement(nil);
    tsqlInsert :
      Result := ParseInsertStatement(nil);
    tsqlDelete :
      Result := ParseDeleteStatement(nil);
    tsqlCreate,
      tsqlAlter :
      Result := ParseCreateStatement(nil, (tsqlAlter = CurrentToken));
    tsqlDrop :
      Result := ParseDropStatement(nil);
    tsqlSet :
      Result := ParseSetStatement(nil);
    tsqlRollBack :
      Result := ParseRollbackStatement(nil);
    tsqlCommit :
      Result := ParseCommitStatement(nil);
    tsqlExecute :
      Result := ParseExecuteProcedureStatement(nil);
    tsqlConnect :
      Result := ParseConnectStatement(nil);
    tsqlDeclare :
      Result := ParseDeclareStatement(nil);
    tsqlGrant :
      Result := ParseGrantStatement(nil);
    tsqlRevoke :
      Result := ParseRevokeStatement(nil);
  else
    UnexpectedToken;
  end;
  if Not(CurrentToken in [tsqlEOF, tsqlSemicolon]) then
  begin
    FreeAndNil(Result);
    if (CurrentToken = tsqlBraceClose) then
      Error(SerrUnmatchedBrace);
    Error(SErrUnexpectedToken, [CurrentTokenString]);
  end;
end;

function TSQLParser.ParseScript(AllowPartial : Boolean = False)
  : TSQLelementList;

var
  E : TSQLElement;

begin
  Result := TSQLelementList.Create(True);
  try
    E := Parse;
    while (E <> nil) do
    begin
      Result.Add(E);
      E := Parse;
    end;
  except
    if Not AllowPartial then
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TSQLParser.CurrentToken: TSQLToken;
begin
  Result := FCurrent;
end;

function TSQLParser.CurrentTokenString: string;
begin
  Result := FCurrentString;
end;

function TSQLParser.GetNextToken: TSQLToken;
begin
  FPrevious := FCurrent;
  if (FPeekToken <> tsqlunknown) then
  begin
    FCurrent := FPeekToken;
    FCurrentString := FPeekTokenString;
    FPeekToken := tsqlunknown;
    FPeekTokenString := '';
  end
  else
  begin
    FCurrent := FScanner.FetchToken;
    FCurrentString := FScanner.CurTokenString;
  end;
  Result := FCurrent;

{$IFDEF debugparser}Writeln('GetNextToken : ', GetEnumName(TypeInfo(TSQLToken), Ord(FCurrent)), ' As string: ', FCurrentString); {$ENDIF debugparser}

end;

function TSQLParser.PeekNextToken: TSQLToken;
begin
  if (FPeekToken = tsqlunknown) then
  begin
    FPeekToken := FScanner.FetchToken;
    FPeekTokenString := FScanner.CurTokenString;
  end;

{$IFDEF debugparser}Writeln('PeekNextToken : ', GetEnumName(TypeInfo(TSQLToken), Ord(FPeekToken)), ' As string: ', FPeekTokenString); {$ENDIF debugparser}

  Result := FPeekToken;
end;

function TSQLParser.PreviousToken: TSQLToken;
begin
  Result := FPrevious;
end;

function TSQLParser.IsEndOfLine: Boolean;
begin
  Result := FScanner.IsEndOfLine;
end;

end.

