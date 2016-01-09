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

{
    This file is part of the Free Component Library
    Copyright (c) 2010 by the Free Pascal development team

    SQL Abstract syntax tree

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ts.Core.SQLTree;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TSQLStringType = string;
  TSQLFormatOption = (
    sfoDoubleQuotes,
    // Use double quote character for string literals
    sfoBackslashEscape,       // Backslash escapes in string literals
    sfoSingleQuoteIdentifier, // quote Identifiers using '
    sfoDoubleQuoteIdentifier, // quote Identifiers using "
    sfoBackQuoteIdentifier,   // quote Identifiers using `
    sfoLowercaseKeyword,      // Lowercase SQL keywords
    sfoOneFieldPerLine,       // One field per line in SELECT, Update, Insert
    sfoIndentFields,          // Indent fields (indent=2 space characters)
    sfoOneTablePerLine,       // One table per line in select FROM clause
    sfoIndentTables,          // Indent tables in FROM clause
    sfoNoBracketRightJoin,
    // In join, Do not put ( ) around right table if it is also a join
    sfoBracketLeftJoin,
    // In join, put ( ) around left table if it is also a join
    sfoWhereOnSeparateLine,    // Put WHERE clause on a separate line
    sfoIndentWhere,            // Indent WHERE clause
    sfoOneGroupByFieldPerLine, // One field per line in GROUP BY
    sfoIndentGroupByFields,
    // Indent GROUP BY fields (indent=2 space characters)
    sfoHavingOnSeparateLine,   // Put HAVING clause on a separate line
    sfoIndentHaving,           // Indent HAVING clause
    sfoUnionOnSeparateLine,    // Put UNION on separate line
    sfoOneOrderByFieldPerLine, // One field per line in ORDER BY
    sfoIndentOrderByFields,
    // Indent ORDER BY fields (indent=2 space characters)
    sfoPlanOnSeparateLine, // Put HAVING clause on a separate line
    sfoIndentPlan,         // Indent HAVING clause
    sfoOneLogicalPerLine, // in AND or OR clauses, put newline before AND or OR
    sfoListNoSpaceBeforeComma,
    // In comma-separated lists, do not put space before ,
    sfoListNoSpaceAfterComma,
    // In comma-separated lists, do not put space after ,
    sfoForceAscending, // In ORDER BY, explicitly write ASC
    sfoMultilineDeclareFunction,
    // Separate parts of 'Declare function' with newlines
    sfoMultilineCreateDatabase,
    // Separate parts of create/alter database with newlines
    sfoMultilineCreateShadow,
    // Separate additional filespecs of create/alter shadow with newlines
    sfoIndentProcedureBlock
    // Indent statements inside procedure/trigger statement block
    );
  TSQLFormatOptions = set of TSQLFormatOption;

const
  sfoUseSeparateLines = [sfoOneFieldPerLine, sfoOneTablePerLine,
    sfoWhereOnSeparateLine, sfoOneGroupByFieldPerLine, sfoHavingOnSeparateLine,
    sfoUnionOnSeparateLine, sfoOneOrderByFieldPerLine, sfoPlanOnSeparateLine,
    sfoOneLogicalPerLine];

const
  sfoUseIndentedLines = sfoUseSeparateLines // no use without it...
    + [sfoIndentFields, sfoIndentTables, sfoIndentWhere, sfoIndentGroupByFields,
    sfoIndentHaving, sfoIndentOrderByFields, sfoIndentPlan];

type

  { TSQLElement }

  TSQLElement = class(TObject)
  private
    FLine: Integer;
    FParent: TSQLElement;
    FPos: Integer;
    FSource: string;

  public
    constructor Create(AParent: TSQLElement); virtual;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; virtual; abstract;
    property Parent: TSQLElement read FParent;
    property Source: string read FSource write FSource;
    property SourceLine: Integer read FLine write FLine;
    property SourcePos: Integer read FPos write FPos;
  end;

  TSQLElementClass = class of TSQLElement;

  { TSQLElementList }

  TSQLElementList = class(TObjectList)
  private
    function GetE(AIndex: Integer): TSQLElement;
    procedure SetE(AIndex: Integer; const AValue: TSQLElement);

  public
    property Elements[AIndex: Integer]: TSQLElement read GetE write SetE; default;
  end;

  TSQLLiteral = class(TSQLElement);

  { TSQLNullLiteral }

  TSQLNullLiteral = class(TSQLLiteral)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLUserLiteral }

  TSQLUserLiteral = class(TSQLLiteral)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLValueLiteral }

  TSQLValueLiteral = class(TSQLLiteral)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLIntegerLiteral }

  TSQLIntegerLiteral = class(TSQLLiteral)
  private
    FValue: Integer;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Value: Integer read FValue write FValue;
  end;

  { TSQLFloatLiteral }

  TSQLFloatLiteral = class(TSQLLiteral)
  private
    FValue: double;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Value: double read FValue write FValue;
  end;

  { TSQLStringLiteral }

  TSQLStringLiteral = class(TSQLLiteral)
  private
    FValue: TSQLStringType;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Value: TSQLStringType read FValue write FValue;
  end;

  { TSQLIdentifierElement }

  TSQLIdentifierName = class(TSQLElement)
  private
    FName: TSQLStringType;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Name: TSQLStringType read FName write FName;
  end;

  TSQLCollation = class(TSQLIdentifierName);

  { TSQLExpression }

  TSQLExpression = class(TSQLElement)
  public
    function UseBrackets: Boolean; virtual;
  end;

  TSQLUnaryOperation = (uoNot, uoMinus);

  { TSQLLiteralExpression }

  TSQLLiteralExpression = class(TSQLExpression)
  private
    FLiteral: TSQLLiteral;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Literal: TSQLLiteral read FLiteral write FLiteral;
  end;

  { TSQLIdentifierExpression }

  TSQLIdentifierExpression = class(TSQLExpression)
  private
    FElementIndex: Integer;
    FIdentifier: TSQLIdentifierName;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Identifier: TSQLIdentifierName read FIdentifier write FIdentifier;
    property ElementIndex: Integer read FElementIndex write FElementIndex;
  end;

  { TSQLParameterExpression }

  TSQLParameterExpression = class(TSQLExpression)
  private
    FIdentifier: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Identifier: TSQLIdentifierName read FIdentifier write FIdentifier;
  end;

  { TSQLListExpression }

  TSQLListExpression = class(TSQLExpression)
  private
    FList: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property List: TSQLElementList read FList;
  end;

  TSQLSelectStatement = class;

  { TSQLSelectionExpression }

  TSQLSelectionExpression = class(TSQLExpression)
  private
    FSelect: TSQLSelectStatement;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Select: TSQLSelectStatement read FSelect write FSelect;
  end;

  { TSQLSelectExpression }

  TSQLSelectExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLExistsExpression }

  TSQLExistsExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSingularExpression }

  TSQLSingularExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAllExpression }

  TSQLAllExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSomeExpression }

  TSQLSomeExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAnyExpression }

  TSQLAnyExpression = class(TSQLSelectionExpression)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLUnaryExpression }

  TSQLUnaryExpression = class(TSQLExpression)
  private
    FOperand: TSQLExpression;
    FOperation: TSQLUnaryOperation;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Operation: TSQLUnaryOperation read FOperation write FOperation;
    property Operand: TSQLExpression read FOperand write FOperand;
  end;

  TSQLBinaryOperation = (boAnd, boOr, boEQ, boLT, boGT, boLE, boGE, boNE,
    boConcat, boAdd, boSubtract, boMultiply, boDivide, boIn,
    boIs, boIsNot, boLike, boContaining, boStarting);

  { TSQLBinaryExpression }

  TSQLBinaryExpression = class(TSQLExpression)
  private
    FLeft: TSQLExpression;
    FOperation: TSQLBinaryOperation;
    FRight: TSQLExpression;

  public
    destructor Destroy; override;
    function UseBrackets: Boolean; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Operation: TSQLBinaryOperation read FOperation write FOperation;
    property Left: TSQLExpression read FLeft write FLeft;
    property Right: TSQLExpression read FRight write FRight;
  end;

  TSQLTernaryOperation = (toLikeEscape, toBetween);

  { TSQLTernaryExpression }

  TSQLTernaryExpression = class(TSQLExpression)
  private
    FLeft: TSQLExpression;
    FMiddle: TSQLExpression;
    FOperation: TSQLTernaryOperation;
    FRight: TSQLExpression;

  public
    destructor Destroy; override;
    function UseBrackets: Boolean; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Operation: TSQLTernaryOperation read FOperation write FOperation;
    property Left: TSQLExpression read FLeft write FLeft;
    property Middle: TSQLExpression read FMiddle write FMiddle;
    property Right: TSQLExpression read FRight write FRight;
  end;

  { TSQLGenIDExpression }

  TSQLGenIDExpression = class(TSQLExpression)
  private
    FIdentifier: TSQLIdentifierName;
    FValue: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Generator: TSQLIdentifierName read FIdentifier write FIdentifier;
    property Value: TSQLExpression read FValue write FValue;
  end;

  { TSQLFunctionCallExpression }

  TSQLFunctionCallExpression = class(TSQLExpression)
  private
    FArguments: TSQLElementList;
    FIdentifier: TSQLStringType;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Identifier: TSQLStringType read FIdentifier write FIdentifier;
    property Arguments: TSQLElementList read FArguments write FArguments;
  end;

  TSQLAggregateFunction = (afCount, afSum, afAVG, afMax, afMin);
  TSQLAggregateOption = (aoNone, aoAsterisk, aoAll, aoDistinct);

  { TSQLAggregateFunctionExpression }

  TSQLAggregateFunctionExpression = class(TSQLExpression)
  private
    Fagg: TSQLAggregateFunction;
    FExpression: TSQLExpression;
    FOption: TSQLAggregateOption;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Aggregate: TSQLAggregateFunction read Fagg write Fagg;
    property Expression: TSQLExpression read FExpression write FExpression;
    property Option: TSQLAggregateOption read FOption write FOption;
  end;

  { TSQLForeignKeyDefinition }
  // fkaNone when none was specified. fkaNoAction is when NO ACTION is specified.
  TForeignKeyAction = (fkaNone, fkaNoAction, fkaCascade, fkaSetDefault,
    fkaSetNull);

  TSQLForeignKeyDefinition = class(TSQLElement)
  private
    FFieldList: TSQLElementList;
    FOnDelete: TForeignKeyAction;
    FOnUpdate: TForeignKeyAction;
    FTableName: TSQLIdentifierName;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
    property FieldList: TSQLElementList read FFieldList;
    property OnDelete: TForeignKeyAction read FOnDelete write FOnDelete;
    property OnUpdate: TForeignKeyAction read FOnUpdate write FOnUpdate;
  end;

  { TSQLFieldConstraint }

  { TSQLConstraintDef }

  TSQLConstraintDef = class(TSQLElement)
  private
    FConstraintName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ConstraintName: TSQLIdentifierName
      read FConstraintName write FConstraintName;
  end;

  TSQLFieldConstraint = class(TSQLConstraintDef);

  { TSQLUniqueFieldConstraint }

  TSQLUniqueFieldConstraint = class(TSQLFieldConstraint)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLPrimaryKeyFieldConstraint }

  TSQLPrimaryKeyFieldConstraint = class(TSQLFieldConstraint)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLForeignKeyFieldConstraint }

  TSQLForeignKeyFieldConstraint = class(TSQLFieldConstraint)
  private
    FDef: TSQLForeignKeyDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Definition: TSQLForeignKeyDefinition read FDef write FDef;
  end;

  { TSQLCheckFieldConstraint }

  TSQLCheckFieldConstraint = class(TSQLFieldConstraint)
  private
    FExpression: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Expression: TSQLExpression read FExpression write FExpression;
  end;

  { TSQLFieldConstraintList }

  TSQLFieldConstraintList = class(TSQLFieldConstraint)
  private
    FList: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property List: TSQLElementList read FList;
  end;

  TSQLTableConstraintDef = class(TSQLConstraintDef);

  { TSQLTableCheckConstraintDef }

  TSQLTableCheckConstraintDef = class(TSQLTableConstraintDef)
  private
    FCheck: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Check: TSQLExpression read FCheck write FCheck;
  end;

  { TSQLTableFieldsConstraintDef }

  TSQLTableFieldsConstraintDef = class(TSQLTableConstraintDef)
  private
    FFieldList: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function FieldListSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType;
    property FieldList: TSQLElementList read FFieldList;
  end;

  { TSQLTableUniqueConstraintDef }

  TSQLTableUniqueConstraintDef = class(TSQLTableFieldsConstraintDef)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLTablePrimaryKeyConstraintDef }

  TSQLTablePrimaryKeyConstraintDef = class(TSQLTableFieldsConstraintDef)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLTableForeignKeyConstraintDef }

  TSQLTableForeignKeyConstraintDef = class(TSQLTableFieldsConstraintDef)
  private
    FDef: TSQLForeignKeyDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Definition: TSQLForeignKeyDefinition read FDef write FDef;
  end;

  { TSQLTypeDefinition }
  TSQLDataType = (sdtDomain, sdtSmallInt, sdtInteger, sdtFloat,
    sdtDoublePrecision,
    sdtDecimal, sdtNumeric, sdtDate, sdtDateTime, sdtTime,
    sdtChar, sdtVarChar, sdtNChar, sdtNVarChar, sdtCstring,
    sdtBlob);

  TSQLTypeDefinition = class(TSQLElement)
  private
    FArrayDim: Integer;
    FBlobType: Integer;
    FByValue: Boolean;
    FCharSet: TSQLStringType;
    FCollation: TSQLCollation;
    FCheck: TSQLExpression;
    FConstraint: TSQLFieldConstraint;
    FDataType: TSQLDataType;
    FDefault: TSQLLiteral;
    FNotNull: Boolean;
    Flen: Integer;
    FScale: byte;
    FtypeName: string;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property DataType: TSQLDataType read FDataType write FDataType;
    property TypeName: string read FtypeName write FtypeName;
    property Len: Integer read Flen write Flen;
    // Length of string or precision for BCD
    property Scale: byte read FScale write FScale;
    property ArrayDim: Integer read FArrayDim write FArrayDim;
    property BlobType: Integer read FBlobType write FBlobType;
    property NotNull: Boolean read FNotNull write FNotNull;
    property Collation: TSQLCollation read FCollation write FCollation;
    property Check: TSQLExpression read FCheck write FCheck;
    property DefaultValue: TSQLLiteral read FDefault write FDefault;
    property Charset: TSQLStringType read FCharSet write FCharSet;
    property Constraint: TSQLFieldConstraint read FConstraint write FConstraint;
    property ByValue: Boolean read FByValue write FByValue;
  end;

  { TSQLCastExpression }

  TSQLCastExpression = class(TSQLExpression)
  private
    FNewType: TSQLTypeDefinition;
    FValue: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Value: TSQLExpression read FValue write FValue;
    property NewType: TSQLTypeDefinition read FNewType write FNewType;
  end;

  { TSQLExtractExpression }
  TSQLExtractElement = (eeYear, eeMonth, eeDay, eeHour, eeMinute, eeSecond,
    eeWeekDay, eeYearDay);

  TSQLExtractExpression = class(TSQLExpression)
  private
    FElement: TSQLExtractElement;
    FValue: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Value: TSQLExpression read FValue write FValue;
    property Element: TSQLExtractElement read FElement write FElement;
  end;

  TSQLStatement = class(TSQLElement);

  TSQLDMLStatement = class(TSQLStatement);
  TSQLDDLStatement = class(TSQLStatement);

  { TSelectField }

  TSQLSelectElement = class(TSQLElement);
  TSQLSelectAsterisk = class(TSQLSelectElement);

  { TSQLSelectField }

  TSQLSelectField = class(TSQLSelectElement)
  private
    FAliasName: TSQLIdentifierName;
    FExpression: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Expression: TSQLExpression read FExpression write FExpression;
    property AliasName: TSQLIdentifierName read FAliasName write FAliasName;
  end;

  { TSQLTableReference }

  TSQLTableReference = class(TSQLElement);

  { TSQLSimpleTableReference }

  TSQLSimpleTableReference = class(TSQLTableReference)
  private
    FAliasName: TSQLIdentifierName;
    FObjectName: TSQLIdentifierName;
    FParams: TSQLElementList;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ObjectName: TSQLIdentifierName read FObjectName write FObjectName;
    property Params: TSQLElementList read FParams write FParams;
    property AliasName: TSQLIdentifierName read FAliasName write FAliasName;
  end;

  { TSQLJoinTableReference }
  TSQLJoinType = (jtNone, jtInner, jtLeft, jtRight, jtOuter);

  TSQLJoinTableReference = class(TSQLTableReference)
  private
    FJoinClause: TSQLExpression;
    FJoinType: TSQLJoinType;
    FLeft: TSQLTableReference;
    FRight: TSQLTableReference;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Left: TSQLTableReference read FLeft write FLeft;
    property Right: TSQLTableReference read FRight write FRight;
    property JoinType: TSQLJoinType read FJoinType write FJoinType;
    property JoinClause: TSQLExpression read FJoinClause write FJoinClause;
  end;

  TSQLSelectPlan = class(TSQLElement);

  { TSQLSelectPlanItem }

  TSQLSelectPlanItem = class(TSQLSelectPlan)
  private
    FTableName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
  end;

  { TSQLSelectNaturalPlan }

  TSQLSelectNaturalPlan = class(TSQLSelectPlanItem)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSelectIndexedPlan }

  TSQLSelectIndexedPlan = class(TSQLSelectPlanItem)
  private
    FIndexes: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Indexes: TSQLElementList read FIndexes;
  end;

  { TSQLSelectOrderedPlan }

  TSQLSelectOrderedPlan = class(TSQLSelectPlanItem)
  private
    FIndex: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property OrderIndex: TSQLIdentifierName read FIndex write FIndex;
  end;

  TPlanJoinType = (pjtJoin, pjtSort, pjtMerge);

  { TSQLSelectPlanExpr }

  TSQLSelectPlanExpr = class(TSQLSelectPlan)
  private
    FItems: TSQLElementList;
    FJoinType: TPlanJoinType;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Items: TSQLElementList read FItems;
    property JoinType: TPlanJoinType read FJoinType write FJoinType;
  end;

  TSQLOrderDirection = (obAscending, obDescending);

  { TSQLOrderByElement }

  TSQLOrderByElement = class(TSQLElement)
  private
    FCollation: TSQLIdentifierName;
    FField: TSQLElement;
    FOrderBy: TSQLOrderDirection;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Field: TSQLElement read FField write FField;
    property Collation: TSQLIdentifierName read FCollation write FCollation;
    property OrderBy: TSQLOrderDirection read FOrderBy write FOrderBy;
  end;

  { TSQLSelectStatement }

  TSQLSelectStatement = class(TSQLDMLStatement)
  private
    FAll: Boolean;
    FDistinct: Boolean;
    FEndAt: TSQLExpression;
    FFields: TSQLElementList;
    FForUpdate: TSQLElementList;
    FInto: TSQLElementList;
    FOrderBy: TSQLElementList;
    FGroupBy: TSQLElementList;
    FHaving: TSQLExpression;
    FPlan: TSQLSelectPlan;
    FStartAt: TSQLExpression;
    FTables: TSQLElementList;
    FTN: TSQLIdentifierName;
    FUnion: TSQLSelectStatement;
    FUnionAll: Boolean;
    FWhere: TSQLExpression;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TransactionName: TSQLIdentifierName read FTN write FTN;
    property Tables: TSQLElementList read FTables;
    property Fields: TSQLElementList read FFields;
    property Where: TSQLExpression read FWhere write FWhere;
    property GroupBy: TSQLElementList read FGroupBy;
    property Having: TSQLExpression read FHaving write FHaving;
    property OrderBy: TSQLElementList read FOrderBy;
    property ForUpdate: TSQLElementList read FForUpdate write FForUpdate;
    property Union: TSQLSelectStatement read FUnion write FUnion;
    property Plan: TSQLSelectPlan read FPlan write FPlan;
    property Distinct: Boolean read FDistinct write FDistinct;
    property All: Boolean read FAll write FAll;
    property UnionAll: Boolean read FUnionAll write FUnionAll;
    property StartAt: TSQLExpression read FStartAt write FStartAt;
    property EndAt: TSQLExpression read FEndAt write FEndAt;
    property Into: TSQLElementList read FInto write FInto;
  end;

  { TSQLInsertStatement }

  { TSQLTableDMLStatement }

  TSQLTableDMLStatement = class(TSQLDMLStatement)
  private
    FTableName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
  end;

  TSQLInsertStatement = class(TSQLTableDMLStatement)
  private
    FFields: TSQLElementList;
    FSelect: TSQLSelectStatement;
    FValues: TSQLElementList;
    procedure SetSelect(const AValue: TSQLSelectStatement);
    procedure SetValues(const AValue: TSQLElementList);

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Fields: TSQLElementList read FFields write FFields;
    property Values: TSQLElementList read FValues write SetValues;
    property Select: TSQLSelectStatement read FSelect write SetSelect;
  end;

  { TSQLUpdatePair }

  TSQLUpdatePair = class(TSQLElement)
  private
    FFieldName: TSQLIdentifierName;
    FValue: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property FieldName: TSQLIdentifierName read FFieldName write FFieldName;
    property Value: TSQLExpression read FValue write FValue;
  end;

  { TSQLUpdateStatement }

  TSQLUpdateStatement = class(TSQLTableDMLStatement)
  private
    FValues: TSQLElementList;
    FWhereClause: TSQLExpression;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Values: TSQLElementList read FValues;
    property WhereClause: TSQLExpression read FWhereClause write FWhereClause;
  end;

  { TSQLDeleteStatement }

  TSQLDeleteStatement = class(TSQLDMLStatement)
  private
    FAliasName: TSQLIdentifierName;
    FTableName: TSQLIdentifierName;
    FWhereClause: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
    property AliasName: TSQLIdentifierName read FAliasName write FAliasName;
    property WhereClause: TSQLExpression read FWhereClause write FWhereClause;
  end;

  { TSQLTransactionStatement }

  TSQLTransactionStatement = class(TSQLStatement)
  private
    FRelease: Boolean;
    FTransactionName: TSQLIdentifierName;
    FWork: Boolean;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TransactionName: TSQLIdentifierName
      read FTransactionName write FTransactionName;
    property Work: Boolean read FWork write FWork;
    property Release: Boolean read FRelease write FRelease;
  end;

  { TSQLRollBackStatement }

  TSQLRollBackStatement = class(TSQLTransactionStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLCommitStatement }

  TSQLCommitStatement = class(TSQLTransactionStatement)
  private
    FRetain: Boolean;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Retain: Boolean read FRetain write FRetain;
  end;

  { TSQLExecuteProcedureStatement }

  TSQLExecuteProcedureStatement = class(TSQLStatement)
  private
    FOutParams: TSQLElementList;
    FParams: TSQLElementList;
    FPN: TSQLIdentifierName;
    FTN: TSQLIdentifierName;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TransactionName: TSQLIdentifierName read FTN write FTN;
    property ProcedureName: TSQLIdentifierName read FPN write FPN;
    property Params: TSQLElementList read FParams;
    property Returning: TSQLElementList read FOutParams;
  end;

  { TSQLCreateOrAlterStatement }

  TSQLCreateOrAlterStatement = class(TSQLDDLStatement)
  private
    FDBO: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ObjectName: TSQLIdentifierName read FDBO write FDBO;
  end;

  { Generator }

  TSQLCreateOrAlterGenerator = class(TSQLCreateOrAlterStatement);

  { TSQLCreateGeneratorStatement }

  TSQLCreateGeneratorStatement = class(TSQLCreateOrAlterGenerator)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSetGeneratorStatement }

  TSQLSetGeneratorStatement = class(TSQLCreateOrAlterGenerator)
  private
    FNewValue: Integer;

  public
    property NewValue: Integer read FNewValue write FNewValue;
  end;

  { TSQLCreateRoleStatement }

  TSQLCreateRoleStatement = class(TSQLCreateOrAlterStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TDomain }

  { TSQLCreateDomainStatement }

  TSQLCreateDomainStatement = class(TSQLCreateOrAlterStatement)
  private
    FType: TSQLTypeDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TypeDefinition: TSQLTypeDefinition read FType write FType;
  end;

  { TSQLAlterDomainStatement }

  TSQLAlterDomainStatement = class(TSQLCreateOrAlterStatement)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAlterDomainDropDefaultStatement }

  TSQLAlterDomainDropDefaultStatement = class(TSQLAlterDomainStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAlterDomainDropCheckStatement }

  TSQLAlterDomainDropCheckStatement = class(TSQLAlterDomainStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAlterDomainSetDefaultStatement }

  TSQLAlterDomainSetDefaultStatement = class(TSQLAlterDomainStatement)
  private
    FDefault: TSQLLiteral;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property DefaultValue: TSQLLiteral read FDefault write FDefault;
  end;

  { TSQLAlterDomainRenameStatement }

  TSQLAlterDomainRenameStatement = class(TSQLAlterDomainStatement)
  private
    FNewName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property NewName: TSQLIdentifierName read FNewName write FNewName;
  end;

  { TSQLAlterDomainTypeStatement }

  TSQLAlterDomainTypeStatement = class(TSQLAlterDomainStatement)
  private
    FNewType: TSQLTypeDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property NewType: TSQLTypeDefinition read FNewType write FNewType;
  end;

  { TSQLAlterDomainAddCheckStatement }

  TSQLAlterDomainAddCheckStatement = class(TSQLAlterDomainStatement)
  private
    FCheck: TSQLExpression;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Check: TSQLExpression read FCheck write FCheck;
  end;

  { TSQLCreateExceptionStatement }

  TSQLCreateExceptionStatement = class(TSQLCreateOrAlterStatement)
  private
    FMessage: TSQLStringLiteral;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ExceptionMessage: TSQLStringLiteral read FMessage write FMessage;
  end;

  TSQLAlterExceptionStatement = class(TSQLCreateExceptionStatement);

  TIndexOption = (ioAscending, ioDescending, ioUnique);
  TIndexOptions = set of TIndexOption;

  { TSQLCreateIndexStatement }
  TSQLCreateOrAlterIndexStatement = class(TSQLCreateOrAlterStatement);

  TSQLCreateIndexStatement = class(TSQLCreateOrAlterIndexStatement)
  private
    FOptions: TIndexOptions;
    FFieldNames: TSQLElementList;
    FTableName: TSQLIdentifierName;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Options: TIndexOptions read FOptions write FOptions;
    property FieldNames: TSQLElementList read FFieldNames;
  end;

  { TSQLAlterIndexStatement }

  TSQLAlterIndexStatement = class(TSQLCreateOrAlterIndexStatement)
  private
    FInactive: Boolean;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Inactive: Boolean read FInactive write FInactive;
  end;

  { TDeclareExternalFunctionStatement }

  { TSQLDeclareExternalFunctionStatement }

  TSQLDeclareExternalFunctionStatement = class(TSQLCreateOrAlterStatement)
  private
    FArguments: TSQLElementList;
    FEntryPoint: TSQLStringType;
    FFreeIt: Boolean;
    FModuleName: TSQLStringType;
    FReturnType: TSQLTypeDefinition;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ModuleName: TSQLStringType read FModuleName write FModuleName;
    property EntryPoint: TSQLStringType read FEntryPoint write FEntryPoint;
    property ReturnType: TSQLTypeDefinition read FReturnType write FReturnType;
    property Arguments: TSQLElementList read FArguments;
    property FreeIt: Boolean read FFreeIt write FFreeIt;
  end;

  { TSQLTableFieldDef }

  TSQLTableFieldDef = class(TSQLElement)
  private
    FComputedBy: TSQLExpression;
    FFieldName: TSQLIdentifierName;
    FFieldType: TSQLTypeDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property FieldName: TSQLIdentifierName read FFieldName write FFieldName;
    property FieldType: TSQLTypeDefinition read FFieldType write FFieldType;
    property ComputedBy: TSQLExpression read FComputedBy write FComputedBy;
  end;

  { TSQLCreateOrAlterTableStatement }

  TSQLCreateOrAlterTableStatement = class(TSQLCreateOrAlterStatement);

  { TSQLCreateTableStatement }

  TSQLCreateTableStatement = class(TSQLCreateOrAlterTableStatement)
  private
    FConstraints: TSQLElementList;
    FExternalFile: TSQLStringLiteral;
    FFieldDefs: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ExternalFileName: TSQLStringLiteral
      read FExternalFile write FExternalFile;
    property FieldDefs: TSQLElementList read FFieldDefs;
    property Constraints: TSQLElementList read FConstraints;
  end;

  { TSQLAlterTableOperation }
  TSQLAlterTableOperation = class(TSQLElement)
  private
    FName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ObjectName: TSQLIdentifierName read FName write FName;
  end;

  { TSQLDropTableElementOperation }

  TSQLDropTableElementOperation = class(TSQLAlterTableOperation);

  { TSQLDropTableFieldOperation }

  TSQLDropTableFieldOperation = class(TSQLDropTableElementOperation)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLDropTableConstraintOperation }

  TSQLDropTableConstraintOperation = class(TSQLDropTableElementOperation)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAlterTableFieldNameOperation }

  TSQLAlterTableFieldNameOperation = class(TSQLAlterTableOperation)
  private
    FNewName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property NewName: TSQLIdentifierName read FNewName write FNewName;
  end;

  { TSQLAlterTableFieldTypeOperation }

  TSQLAlterTableFieldTypeOperation = class(TSQLAlterTableOperation)
  private
    FNewType: TSQLTypeDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property NewType: TSQLTypeDefinition read FNewType write FNewType;
  end;

  { TSQLAlterTableFieldPositionOperation }

  TSQLAlterTableFieldPositionOperation = class(TSQLAlterTableOperation)
  private
    FNewPosition: Integer;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property NewPosition: Integer read FNewPosition write FNewPosition;
  end;

  { TSQLAddTableElementOperation }

  TSQLAlterTableAddElementOperation = class(TSQLElement)
  private
    FElement: TSQLElement;

  public
    destructor Destroy; override;
    property Element: TSQLElement read FElement write FElement;
  end;

  { TSQLAlterTableAddFieldOperation }

  TSQLAlterTableAddFieldOperation = class(TSQLAlterTableAddElementOperation)
  private
    function GetF: TSQLTableFieldDef;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property FieldDef: TSQLTableFieldDef read GetF;
  end;

  { TSQLAlterTableAddConstraintOperation }

  TSQLAlterTableAddConstraintOperation = class
    (TSQLAlterTableAddElementOperation)
  private
    function GetC: TSQLTableConstraintDef;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ConstraintDef: TSQLTableConstraintDef read GetC;
  end;

  { TSQLAlterTableStatement }

  TSQLAlterTableStatement = class(TSQLCreateOrAlterTableStatement)
  private
    FOperations: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Operations: TSQLElementList read FOperations;
  end;

  { TSQLAlterCreateViewStatement }

  TSQLAlterCreateViewStatement = class(TSQLCreateOrAlterStatement)
  private
    FFields: TSQLElementList;
    FSelect: TSQLSelectStatement;
    FWCo: Boolean;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    property Fields: TSQLElementList read FFields;
    property Select: TSQLSelectStatement read FSelect write FSelect;
    property WithCheckOption: Boolean read FWCo write FWCo;
  end;

  { TSQLCreateViewStatement }

  TSQLCreateViewStatement = class(TSQLAlterCreateViewStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAlterCreateDatabaseStatement }

  TSQLAlterCreateDatabaseStatement = class(TSQLCreateOrAlterStatement)
  private
    FUseSchema: Boolean;

  public
    property UseSchema: Boolean read FUseSchema write FUseSchema;
  end;

  { TSQLDatabaseFileInfo }

  TSQLDatabaseFileInfo = class(TSQLElement)
  private
    FFileName: TSQLStringType;
    FLength: Integer;
    FStartPage: Integer;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property FileName: TSQLStringType read FFileName write FFileName;
    property Length: Integer read FLength write FLength;
    property StartPage: Integer read FStartPage write FStartPage;
  end;

  { TSQLCreateDatabaseStatement }

  TSQLCreateDatabaseStatement = class(TSQLAlterCreateDatabaseStatement)
  private
    FCharSet: TSQLIdentifierName;
    FFileName: TSQLStringType;
    FLength: Integer;
    FPageSize: Integer;
    FPassword: TSQLStringType;
    FSecondaryFiles: TSQLElementList;
    FUserName: TSQLStringType;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property FileName: TSQLStringType read FFileName write FFileName;
    property UserName: TSQLStringType read FUserName write FUserName;
    property Password: TSQLStringType read FPassword write FPassword;
    property PageSize: Integer read FPageSize write FPageSize;
    property Length: Integer read FLength write FLength;
    property Charset: TSQLIdentifierName read FCharSet write FCharSet;
    property SecondaryFiles: TSQLElementList read FSecondaryFiles;
  end;

  { TSQLAlterDatabaseStatement }

  TSQLAlterDatabaseStatement = class(TSQLAlterCreateDatabaseStatement)
  private
    FOperations: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Operations: TSQLElementList read FOperations write FOperations;
  end;

  { TSQLCreateShadowStatement }

  TSQLCreateShadowStatement = class(TSQLCreateOrAlterStatement)
  private
    FConditional: Boolean;
    FFileName: string;
    FLength: Integer;
    FManual: Boolean;
    FNumber: Integer;
    FSecondaryFiles: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Number: Integer read FNumber write FNumber;
    property Manual: Boolean read FManual write FManual;
    property Conditional: Boolean read FConditional write FConditional;
    property FileName: TSQLStringType read FFileName write FFileName;
    property Length: Integer read FLength write FLength;
    property SecondaryFiles: TSQLElementList read FSecondaryFiles;
  end;

  { TSQLProcedureParamDef }

  TSQLProcedureParamDef = class(TSQLElement)
  private
    FParamName: TSQLIdentifierName;
    FParamType: TSQLTypeDefinition;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ParamName: TSQLIdentifierName read FParamName write FParamName;
    property ParamType: TSQLTypeDefinition read FParamType write FParamType;
  end;

  { TSQLStatementBlock }

  TSQLStatementBlock = class(TSQLStatement)
  private
    FStatements: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Statements: TSQLElementList read FStatements write FStatements;
  end;

  { TSQLAssignStatement }

  TSQLAssignStatement = class(TSQLStatement)
  private
    FExpression: TSQLExpression;
    FVar: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Variable: TSQLIdentifierName read FVar write FVar;
    property Expression: TSQLExpression read FExpression write FExpression;
  end;

  { TSQLIFStatement }

  TSQLIFStatement = class(TSQLStatement)
  private
    FCondition: TSQLExpression;
    FFalseBranch: TSQLStatement;
    FTrueBranch: TSQLStatement;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Condition: TSQLExpression read FCondition write FCondition;
    property TrueBranch: TSQLStatement read FTrueBranch write FTrueBranch;
    property FalseBranch: TSQLStatement read FFalseBranch write FFalseBranch;
  end;

  { TSQLForStatement }

  TSQLForStatement = class(TSQLStatement)
  private
    FFieldList: TSQLElementList;
    FSelect: TSQLSelectStatement;
    FStatement: TSQLStatement;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Select: TSQLSelectStatement read FSelect write FSelect;
    property FieldList: TSQLElementList read FFieldList write FFieldList;
    property Statement: TSQLStatement read FStatement write FStatement;
  end;

  { TSQLWhileStatement }

  TSQLWhileStatement = class(TSQLStatement)
  private
    FCondition: TSQLExpression;
    FStatement: TSQLStatement;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Condition: TSQLExpression read FCondition write FCondition;
    property Statement: TSQLStatement read FStatement write FStatement;
  end;

  { TSQLWhenError }

  TSQLWhenError = class(TSQLElement);

  { TSQLWhenSQLErrorCode }

  TSQLWhenSQLError = class(TSQLWhenError)
  private
    FErrorCode: Integer;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  { TSQLWhenException }

  TSQLWhenException = class(TSQLWhenError)
  private
    FEN: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ExceptionName: TSQLIdentifierName read FEN write FEN;
  end;

  { TSQLWhenGDSErrorCode }

  TSQLWhenGDSError = class(TSQLWhenError)
  private
    FErrorNumber: Integer;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property GDSErrorNumber: Integer read FErrorNumber write FErrorNumber;
  end;

  { TSQLWhenStatement }

  TSQLWhenStatement = class(TSQLStatement)
  private
    FAnyError: Boolean;
    FErrors: TSQLElementList;
    FStatement: TSQLStatement;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property AnyError: Boolean read FAnyError write FAnyError;
    property Errors: TSQLElementList read FErrors write FErrors;
    property Statement: TSQLStatement read FStatement write FStatement;
  end;

  { TSQLExceptionStatement }

  TSQLExceptionStatement = class(TSQLStatement)
  private
    FEN: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ExceptionName: TSQLIdentifierName read FEN write FEN;
  end;

  { TSQLExitStatement }

  TSQLExitStatement = class(TSQLStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSuspendStatement }

  TSQLSuspendStatement = class(TSQLStatement)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLPostEventStatement }

  TSQLPostEventStatement = class(TSQLStatement)
  private
    FCN: TSQLIdentifierName;
    FEN: TSQLStringType;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ColName: TSQLIdentifierName read FCN write FCN;
    property EventName: TSQLStringType read FEN write FEN;
  end;

  { TSQLCreateOrAlterProcedureTriggerStatement }

  TSQLCreateOrAlterProcedureTriggerStatement = class(TSQLCreateOrAlterStatement)
  private
    FLocalVariables: TSQLElementList;
    FStatements: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property LocalVariables: TSQLElementList
      read FLocalVariables write FLocalVariables;
    property Statements: TSQLElementList read FStatements write FStatements;
  end;

  { TSQLAlterCreateProcedureStatement }

  TSQLAlterCreateProcedureStatement = class
    (TSQLCreateOrAlterProcedureTriggerStatement)
  private
    FInputVariables: TSQLElementList;
    FOutputVariables: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property InputVariables: TSQLElementList
      read FInputVariables write FInputVariables;
    property OutputVariables: TSQLElementList
      read FOutputVariables write FOutputVariables;
  end;

  { TSQLCreateProcedureStatement }

  TSQLCreateProcedureStatement = class(TSQLAlterCreateProcedureStatement);
  TSQLAlterProcedureStatement = class(TSQLAlterCreateProcedureStatement);

  TTriggerState = (tsNone, tsActive, tsInactive);
  TTriggerMoment = (tmBefore, tmAfter);
  TTriggerOperation = (toDelete, toInsert, toUpdate);
  TTriggerOperations = set of TTriggerOperation;

  { TSQLAlterCreateTriggerStatement }

  TSQLAlterCreateTriggerStatement = class
    (TSQLCreateOrAlterProcedureTriggerStatement)
  private
    FMoment: TTriggerMoment;
    FOperations: TTriggerOperations;
    FPosition: Integer;
    FState: TTriggerState;
    FTableName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
    property State: TTriggerState read FState write FState;
    property Moment: TTriggerMoment read FMoment write FMoment;
    property Operations: TTriggerOperations read FOperations write FOperations;
    property Position: Integer read FPosition write FPosition;
  end;

  TSQLCreateTriggerStatement = class(TSQLAlterCreateTriggerStatement);
  TSQLAlterTriggerStatement = class(TSQLAlterCreateTriggerStatement);

  { TSQLDropStatement }

  TSQLDropStatement = class(TSQLDDLStatement)
  private
    FIdentifier: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function SQLObjectType(Options: TSQLFormatOptions): string;
      virtual; abstract;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ObjectName: TSQLIdentifierName read FIdentifier write FIdentifier;
  end;

  { TSQLDropTableStatement }

  TSQLDropTableStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropIndexStatement }

  TSQLDropIndexStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropViewStatement }

  TSQLDropViewStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropProcedureStatement }

  TSQLDropProcedureStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropDomainStatement }

  TSQLDropDomainStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropGeneratorStatement }

  TSQLDropGeneratorStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropTriggerStatement }

  TSQLDropTriggerStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropExceptionStatement }

  TSQLDropExceptionStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropDatabaseStatement }

  TSQLDropDatabaseStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropRoleStatement }

  TSQLDropRoleStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropExternalFunctionStatement }

  TSQLDropExternalFunctionStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

  { TSQLDropShadowStatement }

  TSQLDropShadowStatement = class(TSQLDropStatement)
    function SQLObjectType(Options: TSQLFormatOptions): string; override;
  end;

 {
 TSQLDROPFilterStatement = Class(TSQLDropStatement);
}

  { TSQLConnectStatement }

  TSQLConnectStatement = class(TSQLStatement)
  private
    FCache: Integer;
    FDBN: TSQLStringType;
    FPWD: TSQLStringType;
    FRole: TSQLStringType;
    FUN: TSQLStringType;

  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property DatabaseName: TSQLStringType read FDBN write FDBN;
    property UserName: TSQLStringType read FUN write FUN;
    property Password: TSQLStringType read FPWD write FPWD;
    property Role: TSQLStringType read FRole write FRole;
    property Cache: Integer read FCache write FCache;
  end;

  TSQLPrivilegeKind = (pkSelect, pkInsert, pkDelete, pkUpdate, pkReference);
  { TSQLPrivilege }

  TSQLPrivilege = class(TSQLElement);

  { TSQLInsertPrivilege }

  TSQLInsertPrivilege = class(TSQLPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLDeletePrivilege }

  TSQLDeletePrivilege = class(TSQLPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLSelectPrivilege }

  TSQLSelectPrivilege = class(TSQLPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLAllPrivilege }

  TSQLAllPrivilege = class(TSQLPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLColumnPrivilege }

  TSQLColumnPrivilege = class(TSQLPrivilege)
  private
    FColumns: TSQLElementList;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Columns: TSQLElementList read FColumns write FColumns;
  end;

  { TSQLUpdatePrivilege }

  TSQLUpdatePrivilege = class(TSQLColumnPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLReferencePrivilege }

  TSQLReferencePrivilege = class(TSQLColumnPrivilege)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  TSQLGrantee = class(TSQLIdentifierName);

  TSQLUserGrantee = class(TSQLGrantee);

  { TSQLGroupGrantee }

  TSQLGroupGrantee = class(TSQLGrantee)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLProcedureGrantee }

  TSQLProcedureGrantee = class(TSQLGrantee)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLViewGrantee }

  TSQLViewGrantee = class(TSQLGrantee)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLTriggerGrantee }

  TSQLTriggerGrantee = class(TSQLGrantee)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLPublicGrantee }

  TSQLPublicGrantee = class(TSQLGrantee)
  public
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLGrantStatement }

  TSQLGrantStatement = class(TSQLStatement)
  private
    FGrantees: TSQLElementList;

  public
    function GranteesAsSQL(Options: TSQLFormatOptions; AIndent: Integer;
      IsRevoke: Boolean = False): TSQLStringType;
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    property Grantees: TSQLElementList read FGrantees;
  end;

  TSQLRevokeStatement = TSQLGrantStatement;
  { TSQLTableGrantStatement }

  TSQLTableGrantStatement = class(TSQLGrantStatement)
  private
    FGrantOption: Boolean;
    FPrivileges: TSQLElementList;
    FTableName: TSQLIdentifierName;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property TableName: TSQLIdentifierName read FTableName write FTableName;
    property Privileges: TSQLElementList read FPrivileges;
    property GrantOption: Boolean read FGrantOption write FGrantOption;
  end;

  { TSQLTableRevokeStatement }

  TSQLTableRevokeStatement = class(TSQLTableGrantStatement)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLProcedureGrantStatement }

  TSQLProcedureGrantStatement = class(TSQLGrantStatement)
  private
    FGrantOption: Boolean;
    FProcedureName: TSQLIdentifierName;

  public
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property ProcedureName: TSQLIdentifierName
      read FProcedureName write FProcedureName;
    property GrantOption: Boolean read FGrantOption write FGrantOption;
  end;

  { TSQLProcedureRevokeStatement }

  TSQLProcedureRevokeStatement = class(TSQLProcedureGrantStatement)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

  { TSQLRoleGrantStatement }

  TSQLRoleGrantStatement = class(TSQLGrantStatement)
  private
    FAdminOption: Boolean;
    FRoles: TSQLElementList;

  public
    constructor Create(AParent: TSQLElement); override;
    destructor Destroy; override;
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
    property Roles: TSQLElementList read FRoles;
    property AdminOption: Boolean read FAdminOption write FAdminOption;
  end;

  { TSQLRoleRevokeStatement }

  TSQLRoleRevokeStatement = class(TSQLRoleGrantStatement)
    function GetAsSQL(Options: TSQLFormatOptions;
      AIndent: Integer = 0): TSQLStringType; override;
  end;

const
  CharTypes = [sdtChar, sdtVarChar, sdtNChar, sdtNVarChar, sdtCstring];
  ExtractElementNames: array [TSQLExtractElement] of string =
    ('YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE', 'SECOND', 'WEEKDAY',
    'YEARDAY');

// Format a SQL keyword according to OPTIONS
function SQLKeyWord(const AWord: TSQLStringType;
  Options: TSQLFormatOptions): TSQLStringType;
function SQLListSeparator(Options: TSQLFormatOptions): string;
procedure GetSepPrefixIndent(DoNewLine, DoIndent: Boolean;
  var Sep, Prefix: TSQLStringType; var AIndent: Integer);
function SQLFormatString(const AValue: TSQLStringType;
  Options: TSQLFormatOptions): TSQLStringType;

implementation

{ TSQLElementList }

function SQLFormatString(const AValue: TSQLStringType;
  Options: TSQLFormatOptions): TSQLStringType;

begin
  if sfoDoubleQuotes in Options then
    Result := '"' + StringReplace(AValue, '"', '""', [rfreplaceAll]) + '"'
  else
    Result := '''' + StringReplace(AValue, '''', '''''', [rfreplaceAll]) + '''';
end;

function SQLKeyWord(const AWord: TSQLStringType;
  Options: TSQLFormatOptions): TSQLStringType;

begin
  if (sfoLowercaseKeyword in Options) then
    Result := LowerCase(AWord)
  else
    Result := AWord;
end;

function SQLListSeparator(Options: TSQLFormatOptions): string;
begin
  Result := ' , ';
  if (sfoListNoSpaceBeforeComma in Options) then
    Delete(Result, 1, 1);
  if (sfoListNoSpaceAfterComma in Options) then
    Delete(Result, Length(Result), 1);
end;

procedure GetSepPrefixIndent(DoNewLine, DoIndent: Boolean;
  var Sep, Prefix: TSQLStringType; var AIndent: Integer);
begin
  Prefix := '';
  AIndent := 0;
  if DoNewLine then
  begin
    Sep := ',' + SlineBreak;
    if DoIndent then
    begin
      Prefix := '  ';
      AIndent := 2;
    end;
  end
  else
    Sep := ', ';
end;

function TSQLElementList.GetE(AIndex: Integer): TSQLElement;
begin
  Result := TSQLElement(Items[AIndex]);
end;

procedure TSQLElementList.SetE(AIndex: Integer; const AValue: TSQLElement);
begin
  Items[AIndex] := AValue;
end;

{ TSQLIntegerLiteral }

function TSQLIntegerLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := IntToStr(FValue);
end;

{ TSQLFloatLiteral }

function TSQLFloatLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  // Needs improvement.
  Result := FloatToStr(FValue);
end;

{ TSQLStringElement }

function TSQLStringLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLFormatString(Value, Options);
end;

{ TSQLElement }

constructor TSQLElement.Create(AParent: TSQLElement);
begin
  FParent := AParent;
end;

destructor TSQLElement.Destroy;
begin
  inherited Destroy;
end;

{ TSQLSelectStatement }

constructor TSQLSelectStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFields := TSQLElementList.Create(True);
  FTables := TSQLElementList.Create(True);
  FGroupBy := TSQLElementList.Create(True);
  FOrderBy := TSQLElementList.Create(True);
end;

destructor TSQLSelectStatement.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FTables);
  FreeAndNil(FGroupBy);
  FreeAndNil(FOrderBy);
  FreeAndNil(FWhere);
  FreeAndNil(FHaving);
  FreeAndNil(FStartAt);
  FreeAndNil(FEndAt);
  FreeAndNil(FUnion);
  FreeAndNil(FPlan);
  FreeAndNil(FForUpdate);
  FreeAndNil(FTN);
  FreeAndNil(FInto);
  inherited Destroy;
end;

function TSQLSelectStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
var
  NewLinePending: Boolean;

  procedure AddList(const AKeyWord: string; List: TSQLElementList;
    UseNewLine, UseIndent: Boolean);
  var
    S, Pref, Sep: TSQLStringType;
    I, Ind: Integer;
  begin
    S := '';
    Sep := '';
    Pref := '';
    Ind := 0;
    if not Assigned(List) or (List.Count = 0) then
      Exit;
    if (AKeyWord <> '') then
      if NewLinePending then
        Result := Result + SlineBreak + SQLKeyWord(AKeyWord, Options)
      else
        Result := Result + ' ' + SQLKeyWord(AKeyWord, Options);
    GetSepPrefixIndent(UseNewLine, UseIndent, Sep, Pref, Ind);
    for I := 0 to List.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + Pref + List[I].GetAsSQL(Options, AIndent + Ind);
    end;
    NewLinePending := UseNewLine;
    if UseNewLine then
      Result := Result + SlineBreak + S
    else
      Result := Result + ' ' + S;
  end;

  procedure AddExpression(const AKeyWord: TSQLStringType;
    AExpression: TSQLElement; UseNewLine, UseIndent: Boolean);
  var
    S, Pref, Sep: TSQLStringType;
    Ind: Integer;
  begin
    Pref := '';
    Sep := '';
    S := '';
    Ind := 0;
    if not Assigned(AExpression) then
      exit;
    if NewLinePending then
      S := SlineBreak
    else
      S := ' ';
    Result := Result + S;
    if UseNewLine then
      S := SlineBreak
    else
      S := ' ';
    Result := Result + SQLKeyWord(AKeyWord, Options) + S;
    GetSepPrefixIndent(UseNewLine, UseIndent, Sep, Pref, Ind);
    Result := Result + Pref + AExpression.GetAsSQL(Options, 0{AIndent+Ind});
    NewLinePending := UseNewLine;
  end;

begin
  Result := SQLKeyWord('SELECT', Options);
  if Distinct then
    Result := Result + ' ' + SQLKeyWord('DISTINCT', Options);
  NewLinePending := (sfoOneFieldPerLine in Options);
  AddList('', Fields, (sfoOneFieldPerLine in Options),
    (sfoIndentFields in Options));
  AddList('FROM', Tables, (sfoOneTablePerLine in Options),
    (sfoIndentTables in Options));
  AddExpression('WHERE', Where, (sfoWhereOnSeparateLine in Options),
    (sfoIndentWhere in Options));
  AddList('GROUP BY', GroupBy, (sfoOneGroupByFieldPerLine in Options),
    (sfoIndentGroupByFields in Options));
  AddExpression('HAVING', Having, (sfoHavingOnSeparateLine in Options),
    (sfoIndentHaving in Options));
  if Assigned(Union) then
    NewLinePending := NewLinePending or (sfoUnionOnSeparateLine in Options);
  AddExpression('UNION', Union, (sfoUnionOnSeparateLine in Options), False);
  if Assigned(Plan) then
    NewLinePending := NewLinePending or (sfoPlanOnSeparateLine in Options);
  AddExpression('PLAN', Plan, (sfoPlanOnSeparateLine in Options),
    (sfoIndentPlan in Options));
  AddList('ORDER BY', OrderBy, (sfoOneOrderByFieldPerLine in Options),
    (sfoIndentOrderByFields in Options));
end;

{ TSQLInsertStatement }

procedure TSQLInsertStatement.SetSelect(const AValue: TSQLSelectStatement);
begin
  FreeAndNil(FValues);
  FSelect := AValue;
end;

procedure TSQLInsertStatement.SetValues(const AValue: TSQLElementList);
begin
  FreeAndNil(FSelect);
  FValues := AValue;
end;

destructor TSQLInsertStatement.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FValues);
  FreeAndNil(FSelect);
  inherited Destroy;
end;

function TSQLInsertStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Pref, Sep: TSQLStringType;
  I, Ind: Integer;
  UseNewLine: Boolean;
begin
  S := '';
  Pref := '';
  Sep := '';
  Ind := 0;
  Result := SQLKeyWord('INSERT INTO ', Options);
  if Assigned(FTableName) then
    Result := Result + TableName.GetAsSQL(Options, AIndent);
  UseNewLine := sfoOneFieldPerLine in Options;
  if UseNewLine then
    Result := Result + SlineBreak
  else
    Result := Result + ' ';
  if Assigned(FFields) and (Fields.Count > 0) then
  begin
    GetSepPrefixIndent(UseNewLine, sfoIndentFields in Options, Sep, Pref, Ind);
    for I := 0 to Fields.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      if I > 0 then
        S := S + Pref;
      S := S + Fields[I].GetAsSQL(Options, AIndent + Ind);
    end;
    S := Pref + '(' + S + ')';
    if UseNewLine then
      Result := Result + S + SlineBreak
    else
      Result := Result + S + ' ';
  end;
  if Assigned(FSelect) then
    Result := Result + Select.GetAsSQL(Options, AIndent)
  else if Assigned(FValues) then
  begin
    Result := Result + SQLKeyWord('VALUES', Options);
    GetSepPrefixIndent(UseNewLine, sfoIndentFields in Options, Sep, Pref, Ind);
    S := '';
    for I := 0 to Values.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      if I > 0 then
        S := S + Pref;
      S := S + Values[I].GetAsSQL(Options, AIndent + Ind);
    end;
    S := Pref + '(' + S + ')';
    if UseNewLine then
      Result := Result + SlineBreak + S
    else
      Result := Result + ' ' + S;
  end;
end;

{ TSQLIdentifierName }

function TSQLIdentifierName.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  // Maybe add quoting options later on ?
  Result := FName;
  if sfoSingleQuoteIdentifier in Options then
    Result := '''' + Result + ''''
  else if sfoDoubleQuoteIdentifier in Options then
    Result := '"' + Result + '"'
  else if sfoBackQuoteIdentifier in Options then
    Result := '`' + Result + '`';
end;

{ TSQLDropStatement }

destructor TSQLDropStatement.Destroy;
begin
  FreeAndNil(FIdentifier);
  inherited Destroy;
end;

function TSQLDropStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('DROP ' + SQLObjectType(Options) + ' ', Options);
  if Assigned(FIdentifier) then
    Result := Result + ObjectName.GetAsSQL(Options, AIndent);
end;

{ TSQLCreateOrAlterStatement }

destructor TSQLCreateOrAlterStatement.Destroy;
begin
  FreeAndNil(FDBO);
  inherited Destroy;
end;

function TSQLCreateOrAlterStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FDBO) then
    Result := FDBO.GetAsSQL(Options, AIndent);
end;

{ TSQLCreateDomainStatement }

destructor TSQLCreateDomainStatement.Destroy;
begin
  FreeAndNil(FType);
  inherited Destroy;
end;

function TSQLCreateDomainStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('CREATE DOMAIN ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Assigned(FType) then
    Result := Result + ' ' + FType.GetAsSQL(Options, AIndent);
end;

{ TSQLTypeDefinition }

destructor TSQLTypeDefinition.Destroy;
begin
  FreeAndNil(FCollation);
  FreeAndNil(FConstraint);
  FreeAndNil(FDefault);
  FreeAndNil(FConstraint);
  inherited Destroy;
end;

function TSQLTypeDefinition.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
var
  Opcodes: array [TSQLDataType] of
  TSQLStringType = ('', 'SMALLINT', 'INT', 'FLOAT',
    'DOUBLE PRECISION', 'DECIMAL', 'NUMERIC', 'DATE',
    'TIMESTAMP', 'TIME', 'CHAR', 'VARCHAR', 'NATIONAL CHARACTER',
    'NATIONAL CHARACTER VARYING', 'CSTRING', 'BLOB');
begin
  if DataType = sdtDomain then
    Result := SQLKeyWord(UpperCase(TypeName), Options)
  else
    Result := SQLKeyWord(Opcodes[DataType], Options);
  if (Len > 0) then
    if (DataType in CharTypes) then
      Result := Result + Format('(%d)', [Len])
    else if (DataType in [sdtNumeric, sdtDecimal]) then
    begin
      if (Scale = 0) then
        Result := Result + Format('(%d)', [Len])
      else
        Result := Result + Format('(%d,%d)', [Len, Scale]);
    end;
  if DataType = sdtBlob then
  begin
    Result := Result + SQLKeyWord(' SUB_TYPE ', Options) + IntToStr(BlobType);
    if Len > 0 then
      Result := Result + SQLKeyWord(' SEGMENT_SIZE ', Options) + IntToStr(Len);
  end;
  if (Charset <> '') then
    Result := Result + SQLKeyWord(' CHARACTER SET ', Options) + Charset;
  if (ArrayDim <> 0) then
    Result := Result + Format(' [%d]', [ArrayDim]);
  if Assigned(FDefault) then
    Result := Result + SQLKeyWord(' DEFAULT ', Options) +
      DefaultValue.GetAsSQL(Options, AIndent);
  if NotNull then
    Result := Result + SQLKeyWord(' NOT NULL', Options);
  if Assigned(Constraint) then
    Result := Result + ' ' + Constraint.GetAsSQL(Options, AIndent)
  else if Assigned(Check) then
    Result := Result + SQLKeyWord(' CHECK ', Options) + '(' +
      Check.GetAsSQL(Options, AIndent) + ')';
  if Assigned(Collation) then
    Result := Result + SQLKeyWord(' COLLATION ', Options) +
      Collation.GetAsSQL(Options, AIndent);
end;

{ TSQLLiteralExpression }

destructor TSQLLiteralExpression.Destroy;
begin
  FreeAndNil(FLiteral);
  inherited Destroy;
end;

function TSQLLiteralExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  if Assigned(Literal) then
    Result := Literal.GetAsSQL(Options)
  else
    Result := '';
end;

{ TSQLUnaryExpression }

destructor TSQLUnaryExpression.Destroy;
begin
  FreeAndNil(FOperand);
  inherited Destroy;
end;

function TSQLUnaryExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FOperand) then
    Result := Operand.GetAsSQL(Options, AIndent);
  case Operation of
    uoNot:
      Result := SQLKeyWord('NOT', Options) + ' (' + Result + ')';
    uoMinus:
      Result := '-' + Result;
  end;
end;

{ TSQLBinaryExpression }

destructor TSQLBinaryExpression.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  inherited Destroy;
end;

function TSQLBinaryExpression.UseBrackets: Boolean;
begin
  Result := True;
end;

function TSQLBinaryExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  Opcodes: array [TSQLBinaryOperation] of string =
    ('AND', 'OR', '=', '<', '>', '<=', '>=', '<>',
    '||', '+', '-', '*', '/', 'IN',
    'IS', 'IS NOT', 'LIKE', 'CONTAINING', 'STARTING WITH');
var
  L, R, S: TSQLStringType;
begin
  if Assigned(FLeft) then
  begin
    L := Left.GetAsSQL(Options, AIndent);
    if Left.UseBrackets then
      L := '(' + L + ')';
  end;
  if Assigned(FRight) then
  begin
    R := Right.GetAsSQL(Options, AIndent);
    if Right.UseBrackets then
      R := '(' + R + ')';
  end;
  Result := L;
  S := SQLKeyWord(Opcodes[Operation], Options);
  if (Operation in [boOr, boAnd]) and (sfoOneLogicalPerLine in Options) then
    Result := Result + SlineBreak
  else
    Result := Result + ' ';
  Result := Result + S + ' ' + R;
end;

{ TSQLFunctionCallExpression }

destructor TSQLFunctionCallExpression.Destroy;
begin
  FreeAndNil(FArguments);
  inherited Destroy;
end;

function TSQLFunctionCallExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  Sep: string;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  if Assigned(FArguments) and (FArguments.Count > 0) then
    for I := 0 to FArguments.Count - 1 do
    begin
      if (Result <> '') then
        Result := Result + Sep;
      Result := Result + FArguments[I].GetAsSQL(Options, AIndent);
    end;
  Result := SQLKeyWord(Identifier, Options) + '(' + Result + ')';
end;

{ TSQLTernaryExpression }

destructor TSQLTernaryExpression.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FMiddle);
  FreeAndNil(FRight);
  inherited Destroy;
end;

function TSQLTernaryExpression.UseBrackets: Boolean;
begin
  Result := True;
end;

function TSQLTernaryExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  M, L, R: TSQLStringType;
begin
  if Assigned(FMiddle) then
    M := FMiddle.GetAsSQL(Options, AIndent);
  if Assigned(FLeft) then
    L := FLeft.GetAsSQL(Options, AIndent);
  if Assigned(FRight) then
    R := FRight.GetAsSQL(Options, AIndent);
  if Operation = toLikeEscape then
    Result := L + ' ' + SQLKeyWord('LIKE', Options) + ' ' +
      M + ' ' + SQLKeyWord('ESCAPE', Options) + ' ' + R
  else if Operation = toBetween then
    Result := L + ' ' + SQLKeyWord('BETWEEN', Options) + ' ' +
      M + ' ' + SQLKeyWord('AND', Options) + ' ' + R;
end;

{ TSQLAlterDomainSetDefaultStatement }

destructor TSQLAlterDomainSetDefaultStatement.Destroy;
begin
  FreeAndNil(FDefault);
  inherited Destroy;
end;

function TSQLAlterDomainSetDefaultStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(' SET DEFAULT ', Options);
  if Assigned(FDefault) then
    Result := Result + DefaultValue.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterDomainTypeStatement }

destructor TSQLAlterDomainTypeStatement.Destroy;
begin
  FreeAndNil(FNewType);
  inherited Destroy;
end;

function TSQLAlterDomainTypeStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent) + SQLKeyWord(' TYPE ', Options);
  if Assigned(FNewType) then
    Result := Result + NewType.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterDomainAddCheckStatement }

destructor TSQLAlterDomainAddCheckStatement.Destroy;
begin
  FreeAndNil(FCheck);
  inherited Destroy;
end;

function TSQLAlterDomainAddCheckStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if Assigned(FCheck) then
    Result := Result + SQLKeyWord(' ADD CHECK ', Options) +
      Check.GetAsSQL(Options, AIndent);
end;

{ TSQLCreateIndexStatement }

constructor TSQLCreateIndexStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFieldNames := TSQLElementList.Create(True);
end;

destructor TSQLCreateIndexStatement.Destroy;
begin
  FreeAndNil(FFieldNames);
  FreeAndNil(FTableName);
  inherited Destroy;
end;

function TSQLCreateIndexStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  S, Sep: TSQLStringType;
begin
  Result := SQLKeyWord('CREATE ', Options);
  if (ioUnique in Self.Options) then
    Result := Result + SQLKeyWord('UNIQUE ', Options);
  if ioAscending in Self.Options then
    Result := Result + SQLKeyWord('ASCENDING ', Options)
  else if ioDescending in Self.Options then
    Result := Result + SQLKeyWord('DESCENDING ', Options);
  Result := Result + SQLKeyWord('INDEX ', Options) +
    inherited GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(' ON ', Options);
  if Assigned(FTableName) then
    Result := Result + FTableName.GetAsSQL(Options, AIndent);
  if (FieldNames.Count > 0) then
  begin
    Sep := SQLListSeparator(Options);
    S := '';
    for I := 0 to FieldNames.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + FieldNames[I].GetAsSQL(Options, AIndent);
    end;
    S := '(' + S + ')';
  end;
  Result := Result + ' ' + S;
end;

{ TSQLCreateTableStatement }

constructor TSQLCreateTableStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFieldDefs := TSQLElementList.Create(True);
  FConstraints := TSQLElementList.Create(True);
end;

destructor TSQLCreateTableStatement.Destroy;
begin
  FreeAndNil(FExternalFile);
  FreeAndNil(FFieldDefs);
  FreeAndNil(FConstraints);
  inherited Destroy;
end;

function TSQLCreateTableStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep, Pref: TSQLStringType;
  I, Ind: Integer;
  S: string;
begin
  Result := '';
  Sep := '';
  Pref := '';
  Ind := 0;
  S := '';
  GetSepPrefixIndent(sfoOneFieldPerLine in Options, sfoIndentFields in Options,
    Sep, Pref, Ind);
  for I := 0 to FieldDefs.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Pref + FieldDefs[I].GetAsSQL(Options, Ind + AIndent);
  end;
  for I := 0 to Constraints.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Pref + Constraints[I].GetAsSQL(Options, Ind + AIndent);
  end;
  if (sfoOneFieldPerLine in Options) then
    Result := ' (' + SlineBreak + Result + ')'
  else
    Result := ' (' + Result + ')';
  S := SQLKeyWord('CREATE TABLE ', Options) + inherited GetAsSQL(Options, AIndent);
  if Assigned(FExternalFile) then
    S := S + SQLKeyWord(' EXTERNAL FILE ', Options) +
      ExternalFileName.GetAsSQL(Options, AIndent);
  Result := S + Result;
end;

{ TSQLTableFieldDef }

destructor TSQLTableFieldDef.Destroy;
begin
  FreeAndNil(FFieldName);
  FreeAndNil(FFieldType);
  FreeAndNil(FComputedBy);
  inherited Destroy;
end;

function TSQLTableFieldDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FFieldName) then
    Result := FieldName.GetAsSQL(Options, AIndent);
  if Assigned(FComputedBy) then
    Result := Result + SQLKeyWord(' COMPUTED BY ', Options) +
      ComputedBy.GetAsSQL(Options, AIndent)
  else if Assigned(FFieldType) then
    Result := Result + ' ' + FieldType.GetAsSQL(Options, AIndent);
end;

{ TSQLConstraintDef }

destructor TSQLConstraintDef.Destroy;
begin
  FreeAndNil(FConstraintName);
  inherited;
end;

function TSQLConstraintDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FConstraintName) then
    Result := SQLKeyWord('CONSTRAINT ', Options) +
      FConstraintName.GetAsSQL(Options, AIndent);
end;

{ TSQLForeignKeyFieldConstraint }

destructor TSQLForeignKeyFieldConstraint.Destroy;
begin
  FreeAndNil(FDef);
  inherited Destroy;
end;

function TSQLForeignKeyFieldConstraint.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  if Assigned(FDef) then
    Result := Result + SQLKeyWord('REFERENCES ', Options) +
      Definition.GetAsSQL(Options, AIndent);
end;

{ TSQLForeignKeyDefinition }

constructor TSQLForeignKeyDefinition.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFieldList := TSQLElementList.Create(True);
end;

destructor TSQLForeignKeyDefinition.Destroy;
begin
  FreeAndNil(FTableName);
  FreeAndNil(FFieldList);
  inherited Destroy;
end;

function TSQLForeignKeyDefinition.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Codes: array [TForeignKeyAction] of
  string = ('', 'NO ACTION', 'CASCADE', 'SET DEFAULT', 'SET NULL');
var
  Sep: string;
  I: Integer;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to FieldList.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + FieldList[I].GetAsSQL(Options, AIndent);
  end;
  Result := '(' + Result + ')';
  if Assigned(FTableName) then
    Result := TableName.GetAsSQL(Options, AIndent) + ' ' + Result;
  if OnUpdate <> fkaNone then
    Result := Result + SQLKeyWord(' ON UPDATE ' + Codes[OnUpdate], Options);
  if OnDelete <> fkaNone then
    Result := Result + SQLKeyWord(' ON DELETE ' + Codes[OnDelete], Options);
end;

{ TSQLTableFieldsConstraintDef }

constructor TSQLTableFieldsConstraintDef.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFieldList := TSQLElementList.Create(True);
end;

destructor TSQLTableFieldsConstraintDef.Destroy;
begin
  FreeAndNil(FFieldList);
  inherited Destroy;
end;

function TSQLTableFieldsConstraintDef.FieldListSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
var
  I: Integer;
  Sep: string;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to FieldList.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + FieldList[I].GetAsSQL(Options, AIndent);
  end;
  Result := '(' + Result + ')';
end;

{ TSQLTableForeignKeyConstraintDef }

destructor TSQLTableForeignKeyConstraintDef.Destroy;
begin
  FreeAndNil(FDef);
  inherited Destroy;
end;

function TSQLTableForeignKeyConstraintDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  Result := Result + SQLKeyWord('FOREIGN KEY', Options) + ' ' +
    FieldListSQL(Options, AIndent);
  if Assigned(FDef) then
    Result := Result + ' ' + SQLKeyWord('REFERENCES', Options) + ' ' +
      Definition.GetAsSQL(Options, AIndent);
end;

{ TSQLTableCheckConstraintDef }

destructor TSQLTableCheckConstraintDef.Destroy;
begin
  FreeAndNil(FCheck);
  inherited Destroy;
end;

function TSQLTableCheckConstraintDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  if Assigned(FCheck) then
    Result := Result + SQLKeyWord('CHECK', Options) + ' (' +
      FCheck.GetAsSQL(Options, AIndent) + ')';
end;

{ TSQLDropTableElementOperation }

destructor TSQLAlterTableOperation.Destroy;
begin
  FreeAndNil(FName);
  inherited Destroy;
end;

function TSQLAlterTableOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FName) then
    Result := FName.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterTableStatement }

constructor TSQLAlterTableStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FOperations := TSQLElementList.Create(True);
end;

destructor TSQLAlterTableStatement.Destroy;
begin
  FreeAndNil(FOperations);
  inherited Destroy;
end;

function TSQLAlterTableStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Ind, I: Integer;
  Sep, Pref: TSQLStringType;
begin
  Result := '';
  Ind := 0;
  Pref := '';
  Sep := '';
  GetSepPrefixIndent(sfoOneFieldPerLine in Options, sfoIndentFields in Options,
    Sep, Pref, Ind);
  for I := 0 to Operations.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Pref + Operations[I].GetAsSQL(Options, Ind + AIndent);
  end;
  if sfoOneFieldPerLine in Options then
    Pref := SlineBreak
  else
    Pref := ' ';
  Result := SQLKeyWord('ALTER TABLE ', Options) +
    inherited GetAsSQL(Options, AIndent) + Pref + Result;
end;

{ TSQLAddTableElementOperation }

destructor TSQLAlterTableAddElementOperation.Destroy;
begin
  FreeAndNil(FElement);
  inherited Destroy;
end;

{ TSQLAlterTableFieldNameOperation }

destructor TSQLAlterTableFieldNameOperation.Destroy;
begin
  FreeAndNil(FNewName);
  inherited Destroy;
end;

function TSQLAlterTableFieldNameOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALTER COLUMN ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Assigned(FNewName) then
    Result := Result + SQLKeyWord(' TO ', Options) +
      NewName.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterTableFieldTypeOperation }

destructor TSQLAlterTableFieldTypeOperation.Destroy;
begin
  FreeAndNil(FNewType);
  inherited Destroy;
end;

function TSQLAlterTableFieldTypeOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALTER COLUMN ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Assigned(FNewType) then
    Result := Result + SQLKeyWord(' TYPE ', Options) +
      NewType.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterTableAddFieldOperation }

function TSQLAlterTableAddFieldOperation.GetF: TSQLTableFieldDef;
begin
  Result := Element as TSQLTableFieldDef;
end;

function TSQLAlterTableAddFieldOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ADD ', Options);
  if (FieldDef <> nil) then
    Result := Result + FieldDef.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterTableAddConstraintOperation }

function TSQLAlterTableAddConstraintOperation.GetC: TSQLTableConstraintDef;
begin
  Result := Element as TSQLTableConstraintDef;
end;

function TSQLAlterTableAddConstraintOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if (ConstraintDef <> nil) and (ConstraintDef.ConstraintName <> nil) then
    Result := SQLKeyWord('ADD ', Options)
  else
    Result := SQLKeyWord('ADD CONSTRAINT ', Options);
  if (ConstraintDef <> nil) then
    Result := Result + ConstraintDef.GetAsSQL(Options, AIndent);
end;

{ TSQLCheckFieldConstraint }

destructor TSQLCheckFieldConstraint.Destroy;
begin
  FreeAndNil(FExpression);
  inherited Destroy;
end;

function TSQLCheckFieldConstraint.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  if Assigned(FExpression) then
    Result := Result + SQLKeyWord('CHECK', Options) + ' (' +
      Expression.GetAsSQL(Options, AIndent) + ')';
end;

{ TSQLDeleteStatement }

destructor TSQLDeleteStatement.Destroy;
begin
  FreeAndNil(FTableName);
  FreeAndNil(FAliasName);
  FreeAndNil(FWhereClause);
  inherited Destroy;
end;

function TSQLDeleteStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep, Pref: TSQLStringType;
begin
  Result := SQLKeyWord('DELETE FROM ', Options);
  if Assigned(FTableName) then
    Result := Result + TableName.GetAsSQL(Options, AIndent);
  if Assigned(FAliasName) then
    Result := Result + ' ' + AliasName.GetAsSQL(Options, AIndent);
  if Assigned(FWhereClause) then
  begin
    if (sfoWhereOnSeparateLine in Options) then
    begin
      Sep := SlineBreak;
      if (sfoIndentWhere in Options) then
        Pref := SlineBreak + '  '
      else
        Pref := SlineBreak;
    end
    else
    begin
      Sep := ' ';
      Pref := ' ';
    end;
    Result := Result + Sep + SQLKeyWord('WHERE', Options);
    Result := Result + Pref + WhereClause.GetAsSQL(Options, AIndent + Length(Pref));
  end;
end;

{ TSQLTableDMLStatement }

destructor TSQLTableDMLStatement.Destroy;
begin
  FreeAndNil(FTableName);
  inherited Destroy;
end;

{ TSQLUpdatePair }

destructor TSQLUpdatePair.Destroy;
begin
  FreeAndNil(FFieldName);
  FreeAndNil(FValue);
  inherited Destroy;
end;

function TSQLUpdatePair.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  F, V: TSQLStringType;
begin
  if Assigned(FFieldName) then
    F := FFieldName.GetAsSQL(Options, AIndent);
  if Assigned(FValue) then
    V := FValue.GetAsSQL(Options, AIndent);
  Result := F + ' = ' + V;
end;

{ TSQLUpdateStatement }

constructor TSQLUpdateStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FValues := TSQLElementList.Create(True);
end;

destructor TSQLUpdateStatement.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FWhereClause);
  inherited Destroy;
end;

function TSQLUpdateStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep, Pref: TSQLStringType;
  I, Ind: Integer;
  UseNewLine: Boolean;
begin
  Result := '';
  S := '';
  Sep := '';
  Pref := '';
  Ind := 0;
  SQLListSeparator(Options);
  UseNewLine := sfoOneFieldPerLine in Options;
  GetSepPrefixIndent(UseNewLine, sfoIndentFields in Options, Sep, Pref, Ind);
  S := '';
  for I := 0 to Values.Count - 1 do
  begin
    if (S <> '') then
      S := S + Sep;
    S := S + Pref + Values[I].GetAsSQL(Options, AIndent + Ind);
  end;
  Delete(Sep, 1, 1); // remove comma
  Result := SQLKeyWord('UPDATE ', Options);
  if Assigned(FTableName) then
    Result := Result + TableName.GetAsSQL(Options, AIndent) + ' ';
  Result := Result + SQLKeyWord('SET', Options) + Sep + S;
  if Assigned(FWhereClause) then
  begin
    if (sfoWhereOnSeparateLine in Options) or UseNewLine then
    begin
      Sep := SlineBreak;
      if not (sfoWhereOnSeparateLine in Options) then
        Pref := ' '
      else
      if (sfoIndentWhere in Options) then
        Pref := SlineBreak + '  '
      else
        Pref := SlineBreak;
    end
    else
    begin
      Sep := ' ';
      Pref := ' ';
    end;
    Result := Result + Sep + SQLKeyWord('WHERE', Options);
    Result := Result + Pref + WhereClause.GetAsSQL(Options, AIndent + Length(Pref));
  end;
end;

{ TSQLSelectField }

destructor TSQLSelectField.Destroy;
begin
  FreeAndNil(FExpression);
  FreeAndNil(FAliasName);
  inherited Destroy;
end;

function TSQLSelectField.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  if Assigned(FExpression) then
    Result := FExpression.GetAsSQL(Options);
  if Assigned(FAliasName) then
    Result := Result + SQLKeyWord(' AS ', Options) + FAliasName.GetAsSQL(Options);
end;

{ TSQLSimpleTableReference }

destructor TSQLSimpleTableReference.Destroy;
begin
  FreeAndNil(FObjectName);
  FreeAndNil(FParams);
  FreeAndNil(FAliasName);
  inherited Destroy;
end;

function TSQLSimpleTableReference.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
var
  I: Integer;
begin
  Result := '';
  if Assigned(FParams) and (FParams.Count > 0) then
  begin
    for I := 0 to FParams.Count - 1 do
    begin
      if (Result <> '') then
        Result := Result + ' , ';
      Result := Result + FParams[I].GetAsSQL(Options);
    end;
    Result := '(' + Result + ')';
  end;
  if Assigned(FObjectName) then
    Result := FObjectName.GetAsSQL(Options) + Result;
  if Assigned(FAliasName) then
    Result := Result + ' ' + FAliasName.GetAsSQL(Options);
end;

{ TSQLJoinTableReference }

destructor TSQLJoinTableReference.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  FreeAndNil(FJoinClause);
  inherited Destroy;
end;

function TSQLJoinTableReference.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  Opcodes: array [TSQLJoinType] of string =
    ('', 'INNER ', 'LEFT ', 'RIGHT ', 'OUTER ');
var
  L, R, O, Sep, Prefix: TSQLStringType;
  Ind: Integer;
begin
  Sep := '';
  Prefix := '';
  Ind := 0;
  GetSepPrefixIndent(sfoOneTablePerLine in Options, sfoIndentTables in Options,
    Sep, Prefix, Ind);
  Delete(Sep, 1, 1); // remove comma
  if Assigned(Left) then
  begin
    L := Left.GetAsSQL(Options, AIndent);
    if (sfoBracketLeftJoin in Options) and (Left is TSQLJoinTableReference) then
      L := '(' + L + ')';
  end;
  if Assigned(Right) then
  begin
    R := Right.GetAsSQL(Options, AIndent + Ind);
    if (not (sfoNoBracketRightJoin in Options)) and
      (Right is TSQLJoinTableReference) then
      R := '(' + R + ')';
  end;
  if Assigned(JoinClause) then
    O := JoinClause.GetAsSQL(Options, AIndent);
  Result := L + Sep;
  Result := Result + Prefix + SQLKeyWord(Opcodes[JoinType] + 'JOIN ', Options) + R;
  if (O <> '') then
    Result := Result + SQLKeyWord(' ON ', Options) + '(' + O + ')';
end;

{ TSQLAggregateFunctionExpression }

destructor TSQLAggregateFunctionExpression.Destroy;
begin
  FreeAndNil(FExpression);
  inherited Destroy;
end;

function TSQLAggregateFunctionExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  Opcodes: array [TSQLAggregateFunction] of string = ('COUNT', 'SUM', 'AVG',
    'MAX', 'MIN');
var
  E: TSQLStringType;
begin
  Result := SQLKeyWord(Opcodes[Aggregate], Options);
  case Option of
    aoAsterisk:
      E := '*';
    aoAll:
      E := SQLKeyWord('ALL', Options);
    aoDistinct:
      E := SQLKeyWord('DISTINCT', Options);
  end;
  if Assigned(FExpression) and (Option <> aoAsterisk) then
  begin
    if E <> '' then
      E := E + ' ';
    E := E + Expression.GetAsSQL(Options, AIndent);
  end;
  Result := Result + '(' + E + ')';
end;

{ TSQLGenIDExpression }

destructor TSQLGenIDExpression.Destroy;
begin
  FreeAndNil(FValue);
  FreeAndNil(FIdentifier);
  inherited Destroy;
end;

function TSQLGenIDExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  G, E: TSQLStringType;
begin
  if Assigned(FIdentifier) then
    G := FIdentifier.GetAsSQL(Options, AIndent);
  if Assigned(FValue) then
    E := FValue.GetAsSQL(Options, AIndent);

  Result := SQLKeyWord('GEN_ID', Options) + '(' + G + ',' + E + ')';
end;

{ TSQLCastExpression }

destructor TSQLCastExpression.Destroy;
begin
  FreeAndNil(FValue);
  FreeAndNil(FNewType);
  inherited Destroy;
end;

function TSQLCastExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('CAST(', Options);
  if Assigned(FValue) then
    Result := Result + FValue.GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(' AS ', Options);
  if Assigned(FNewType) then
    Result := Result + FNewType.GetAsSQL(Options, AIndent);
  Result := Result + ')';
end;

{ TSQLOrderByElement }

destructor TSQLOrderByElement.Destroy;
begin
  FreeAndNil(FField);
  FreeAndNil(FCollation);
  inherited Destroy;
end;

function TSQLOrderByElement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  Opcodes: array [TSQLOrderDirection] of TSQLStringType = ('ASC', 'DESC');
begin
  if Assigned(FField) then
    Result := FField.GetAsSQL(Options, AIndent);
  if (OrderBy = obDescending) or (sfoForceAscending in Options) then
    Result := Result + ' ' + SQLKeyWord(Opcodes[OrderBy], Options);
  if (Collation <> nil) then
    Result := Result + ' ' + Collation.GetAsSQL(Options, AIndent);
end;

{ TSQLSelectIndexedPlan }

constructor TSQLSelectIndexedPlan.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FIndexes := TSQLElementList.Create(True);
end;

destructor TSQLSelectIndexedPlan.Destroy;
begin
  FreeAndNil(FIndexes);
  inherited Destroy;
end;

function TSQLSelectIndexedPlan.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  Sep: TSQLStringType;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to FIndexes.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + Sep;
    Result := Result + FIndexes[I].GetAsSQL(Options, AIndent);
  end;
  Result := ' (' + Result + ')';
  Result := inherited GetAsSQL(Options, AIndent) +
    SQLKeyWord(' INDEX', Options) + Result;
end;

{ TSQLSelectPlanItem }

destructor TSQLSelectPlanItem.Destroy;
begin
  FreeAndNil(FTableName);
  inherited Destroy;
end;

function TSQLSelectPlanItem.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FTableName) then
    Result := FTableName.GetAsSQL(Options, AIndent);
end;

{ TSQLSelectOrderedPlan }

destructor TSQLSelectOrderedPlan.Destroy;
begin
  FreeAndNil(FIndex);
  inherited Destroy;
end;

function TSQLSelectOrderedPlan.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if Assigned(FIndex) then
    Result := Result + SQLKeyWord(' ORDER ', Options) +
      FIndex.GetAsSQL(Options, AIndent);
end;

{ TSQLSelectPlanExpr }

constructor TSQLSelectPlanExpr.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FItems := TSQLElementList.Create(True);
end;

destructor TSQLSelectPlanExpr.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSQLSelectPlanExpr.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  Opcodes: array [TPlanJoinType] of TSQLStringType = ('JOIN ', 'SORT ', 'MERGE ');
var
  I: Integer;
  Sep: string;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to FItems.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + FItems[I].GetAsSQL(Options, AIndent);
  end;
  Result := '(' + Result + ')';
  Result := SQLKeyWord(Opcodes[JoinType], Options) + Result;
end;

{ TSQLSelectionExpression }

destructor TSQLSelectionExpression.Destroy;
begin
  FreeAndNil(FSelect);
  inherited Destroy;
end;

function TSQLSelectionExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  if Assigned(FSelect) then
    Result := Select.GetAsSQL(Options);
end;

{ TSQLListExpression }

constructor TSQLListExpression.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FList := TSQLElementList.Create(True);
end;

destructor TSQLListExpression.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSQLListExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  Sep: string;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to List.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + List[I].GetAsSQL(Options, AIndent);
  end;
  Result := '(' + Result + ')';
end;

{ TSQLTransactionStatement }

destructor TSQLTransactionStatement.Destroy;
begin
  FreeAndNil(FTransactionName);
  inherited Destroy;
end;

function TSQLTransactionStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FTransactionName) then
    Result := SQLKeyWord(' TRANSACTION ', Options) +
      TransactionName.GetAsSQL(Options, AIndent);
  if Work then
    Result := Result + SQLKeyWord(' WORK', Options);
  if Release then
    Result := Result + SQLKeyWord(' RELEASE', Options);
end;

{ TSQLExecuteProcedureStatement }

constructor TSQLExecuteProcedureStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FParams := TSQLElementList.Create(True);
  FOutParams := TSQLElementList.Create(True);
end;

destructor TSQLExecuteProcedureStatement.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FOutParams);
  FreeAndNil(FPN);
  FreeAndNil(FTN);
  inherited Destroy;
end;

function TSQLExecuteProcedureStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep: TSQLStringType;
  I: Integer;
begin
  S := '';
  Result := SQLKeyWord('EXECUTE PROCEDURE', Options);
  if Assigned(FTN) then
    Result := Result + ' ' + TransactionName.GetAsSQL(Options, AIndent);
  if Assigned(FPN) then
    Result := Result + ' ' + ProcedureName.GetAsSQL(Options, AIndent);
  Sep := SQLListSeparator(Options);
  if (Params.Count > 0) then
  begin
    for I := 0 to Params.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + Params[I].GetAsSQL(Options, AIndent);
    end;
    S := '(' + S + ')';
  end;
  Result := Result + S;
  if (Returning.Count > 0) then
  begin
    S := '';
    for I := 0 to Returning.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + ':' + Returning[I].GetAsSQL(Options, AIndent);
    end;
    S := SQLKeyWord(' RETURNING_VALUES ', Options) + S;
    Result := Result + S;
  end;
end;

{ TSQLAlterDatabaseStatement }

constructor TSQLAlterDatabaseStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FOperations := TSQLElementList.Create(True);
end;

destructor TSQLAlterDatabaseStatement.Destroy;
begin
  FreeAndNil(FOperations);
  inherited Destroy;
end;

function TSQLAlterDatabaseStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  Sep: TSQLStringType;
begin
  if sfoMultilineCreateDatabase in Options then
    Sep := SlineBreak + '  '
  else
    Sep := ' ';
  Result := SQLKeyWord('ALTER ', Options);
  if UseSchema then
    Result := Result + SQLKeyWord('SCHEMA', Options)
  else
    Result := Result + SQLKeyWord('DATABASE', Options);
  Result := Result + SQLKeyWord(' ADD', Options);
  for I := 0 to Operations.Count - 1 do
    Result := Result + Sep + Operations[I].GetAsSQL(Options, AIndent);
end;

{ TSQLCreateDatabaseStatement }

constructor TSQLCreateDatabaseStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FSecondaryFiles := TSQLElementList.Create(True);
end;

destructor TSQLCreateDatabaseStatement.Destroy;
begin
  FreeAndNil(FSecondaryFiles);
  FreeAndNil(FCharSet);
  inherited Destroy;
end;

function TSQLCreateDatabaseStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: string;
  I: Integer;
begin
  if sfoMultilineCreateDatabase in Options then
    Sep := SlineBreak + '  '
  else
    Sep := ' ';
  Result := SQLKeyWord('CREATE ', Options);
  if UseSchema then
    Result := Result + SQLKeyWord('SCHEMA ', Options)
  else
    Result := Result + SQLKeyWord('DATABASE ', Options);
  Result := Result + SQLFormatString(FileName, Options);
  if (UserName <> '') then
  begin
    Result := Result + Sep + SQLKeyWord('USER ', Options) +
      SQLFormatString(UserName, Options);
    if (Password <> '') then
      Result := Result + SQLKeyWord(' PASSWORD ', Options) +
        SQLFormatString(Password, Options);
  end;
  if (PageSize <> 0) then
    Result := Result + Sep + SQLKeyWord('PAGE_SIZE ', Options) + IntToStr(PageSize);
  if (Length <> 0) then
    Result := Result + Sep + SQLKeyWord('LENGTH ', Options) + IntToStr(Length);
  if Assigned(FCharSet) then
    Result := Result + Sep + SQLKeyWord('DEFAULT CHARACTER SET ', Options) +
      Charset.GetAsSQL(Options, AIndent);
  for I := 0 to SecondaryFiles.Count - 1 do
    Result := Result + Sep + SecondaryFiles[I].GetAsSQL(Options, AIndent);
end;

{ TSQLAlterCreateViewStatement }

constructor TSQLAlterCreateViewStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFields := TSQLElementList.Create(True);
end;

destructor TSQLAlterCreateViewStatement.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FSelect);
  inherited Destroy;
end;

{ TSQLCreateShadowStatement }

constructor TSQLCreateShadowStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FSecondaryFiles := TSQLElementList.Create(True);
end;

destructor TSQLCreateShadowStatement.Destroy;
begin
  FreeAndNil(FSecondaryFiles);
  inherited Destroy;
end;

function TSQLCreateShadowStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  if sfoMultilineCreateShadow in Options then
    Sep := SlineBreak + '  '
  else
    Sep := ' ';
  Result := SQLKeyWord('CREATE SHADOW ', Options) + IntToStr(Number);
  if Manual then
    Result := Result + SQLKeyWord(' MANUAL', Options);
  if Conditional then
    Result := Result + SQLKeyWord(' CONDITIONAL', Options);
  Result := Result + ' ' + SQLFormatString(FileName, Options);
  if (Length <> 0) then
    Result := Result + SQLKeyWord(' LENGTH ', Options) + IntToStr(Length) +
      SQLKeyWord(' PAGES', Options);
  for I := 0 to SecondaryFiles.Count - 1 do
    Result := Result + Sep + SecondaryFiles[I].GetAsSQL(Options, AIndent);
end;

{ TSQLCreateProcedureStatement }

constructor TSQLAlterCreateProcedureStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FInputVariables := TSQLElementList.Create(True);
  FOutputVariables := TSQLElementList.Create(True);
end;

destructor TSQLAlterCreateProcedureStatement.Destroy;
begin
  FreeAndNil(FInputVariables);
  FreeAndNil(FOutputVariables);
  inherited Destroy;
end;

function TSQLAlterCreateProcedureStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep: TSQLStringType;
  I: Integer;
begin
  S := '';
  if Self is TSQLAlterProcedureStatement then
    Result := SQLKeyWord('ALTER ', Options)
  else
    Result := SQLKeyWord('CREATE ', Options);
  Result := Result + SQLKeyWord('PROCEDURE ', Options);
  if (ObjectName <> nil) then
    Result := Result + ObjectName.GetAsSQL(Options, AIndent);
  Sep := SQLListSeparator(Options);
  for I := 0 to InputVariables.Count - 1 do
  begin
    if (S <> '') then
      S := S + Sep;
    S := S + InputVariables[I].GetAsSQL(Options, AIndent);
  end;
  if (S <> '') then
    Result := Result + ' (' + S + ')';
  S := '';
  for I := 0 to OutputVariables.Count - 1 do
  begin
    if (S <> '') then
      S := S + Sep;
    S := S + OutputVariables[I].GetAsSQL(Options, AIndent);
  end;
  if (S <> '') then
    Result := Result + SlineBreak + 'RETURNS (' + S + ')';
  Result := Result + SlineBreak + SQLKeyWord('AS', Options) + SlineBreak;
  Result := Result + inherited GetAsSQL(Options, AIndent);
end;

{ TSQLProcedureParamDef }

destructor TSQLProcedureParamDef.Destroy;
begin
  FreeAndNil(FParamName);
  FreeAndNil(FParamType);
  inherited Destroy;
end;

function TSQLProcedureParamDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FParamName) then
    Result := ParamName.GetAsSQL(Options, AIndent);
  if Assigned(FParamType) then
    Result := Result + ' ' + ParamType.GetAsSQL(Options, AIndent);
end;

{ TSQLStatementBlock }

constructor TSQLStatementBlock.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FStatements := TSQLElementList.Create(True);
end;

destructor TSQLStatementBlock.Destroy;
begin
  FreeAndNil(FStatements);
  inherited Destroy;
end;

function SQLIndentStatement(S: string; Options: TSQLFormatOptions): string;
var
  L: TStringList;
  I: Integer;
  Sep: string;
begin
  L := TStringList.Create;
  Sep := '  ';
  try
    L.Text := S;
    if (L.Count > 0) then
      Result := Sep + L[0];
    Sep := SlineBreak + Sep;
    for I := 1 to L.Count - 1 do
      Result := Result + Sep + L[I];
  finally
    L.Free;
  end;
end;

function TSQLStatementBlock.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  S: string;
begin
  S := '';
  Result := SQLKeyWord('BEGIN', Options) + SlineBreak;
  for I := 0 to Statements.Count - 1 do
  begin
    if (S <> '') then
      S := S + SlineBreak;
    S := S + Statements[I].GetAsSQL(Options, AIndent);
    if not (Statements[I] is TSQLStatementBlock) then
      S := S + ';';
  end;
  Result := Result + SQLIndentStatement(S, Options);
  Result := Result + SlineBreak + SQLKeyWord('END', Options);
end;

{ TSQLIFStatement }

destructor TSQLIFStatement.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FFalseBranch);
  FreeAndNil(FTrueBranch);
  inherited Destroy;
end;

function TSQLIFStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('IF (', Options);
  if Assigned(FCondition) then
    Result := Result + Condition.GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(') THEN', Options) + SlineBreak;
  if Assigned(FTrueBranch) then
    Result := Result + SQLIndentStatement(TrueBranch.GetAsSQL(Options,
      AIndent), Options);
  if Assigned(FFalseBranch) then
  begin
    Result := Result + SlineBreak + SQLKeyWord('ELSE', Options) + SlineBreak;
    Result := Result + SQLIndentStatement(FalseBranch.GetAsSQL(Options,
      AIndent), Options);
  end;
end;

{ TSQLForStatement }

constructor TSQLForStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FFieldList := TSQLElementList.Create(True);
end;

destructor TSQLForStatement.Destroy;
begin
  FreeAndNil(FFieldList);
  FreeAndNil(FSelect);
  FreeAndNil(FStatement);
  inherited Destroy;
end;

function TSQLForStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep, Prefix: TSQLStringType;
  I, Ind: Integer;
  DoNewLine: Boolean;
begin
  S := '';
  Sep := '';
  Prefix := '';
  Ind := 0;
  Result := SQLKeyWord('FOR ', Options);
  if Assigned(FSelect) then
    Result := Result + Select.GetAsSQL(Options, AIndent) + SlineBreak;
  Result := Result + SQLKeyWord('INTO', Options);
  DoNewLine := sfoOneFieldPerLine in Options;
  GetSepPrefixIndent(DoNewLine, sfoIndentFields in Options, Sep, Prefix, Ind);
  for I := 0 to FieldList.Count - 1 do
  begin
    if (S <> '') then
      S := S + Sep;
    S := S + Prefix + ':' + FieldList[I].GetAsSQL(Options, AIndent);
  end;
  Result := Result + SlineBreak + S + SlineBreak + SQLKeyWord('DO', Options) +
    SlineBreak;
  if Assigned(FStatement) then
    Result := Result + SQLIndentStatement(Statement.GetAsSQL(Options,
      AIndent), Options);
end;

{ TSQLExceptionStatement }

destructor TSQLExceptionStatement.Destroy;
begin
  FreeAndNil(FEN);
  inherited Destroy;
end;

function TSQLExceptionStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('EXCEPTION ', Options);
  if Assigned(FEN) then
    Result := Result + ExceptionName.GetAsSQL(Options, AIndent);
end;

{ TSQLPostEventStatement }

destructor TSQLPostEventStatement.Destroy;
begin
  FreeAndNil(FCN);
  inherited Destroy;
end;

function TSQLPostEventStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('POST_EVENT ', Options);
  if EventName <> '' then
    Result := Result + SQLFormatString(EventName, Options)
  else
    Result := Result + ColName.GetAsSQL(Options, AIndent);
end;

{ TSQLAssignStatement }

destructor TSQLAssignStatement.Destroy;
begin
  FreeAndNil(FVar);
  FreeAndNil(FExpression);
  inherited Destroy;
end;

function TSQLAssignStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Assigned(FVar) then
    Result := Variable.GetAsSQL(Options, AIndent);
  if Assigned(FExpression) then
    Result := Result + ' = ' + Expression.GetAsSQL(Options, AIndent);
end;

{ TSQLWhileStatement }

destructor TSQLWhileStatement.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FStatement);
  inherited Destroy;
end;

function TSQLWhileStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('WHILE (', Options);
  if Assigned(FCondition) then
    Result := Result + Condition.GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(') DO', Options) + SlineBreak;
  if Assigned(FStatement) then
    Result := Result + SQLIndentStatement(Statement.GetAsSQL(Options,
      AIndent), Options);
end;

{ TSQLWhenException }

destructor TSQLWhenException.Destroy;
begin
  FreeAndNil(FEN);
  inherited Destroy;
end;

function TSQLWhenException.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('EXCEPTION ', Options);
  if Assigned(FEN) then
    Result := Result + ExceptionName.GetAsSQL(Options, AIndent);
end;

{ TSQLWhenStatement }

constructor TSQLWhenStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FErrors := TSQLElementList.Create(True);
end;

destructor TSQLWhenStatement.Destroy;
begin
  FreeAndNil(FErrors);
  FreeAndNil(FStatement);
  inherited Destroy;
end;

function TSQLWhenStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
  S, Sep: TSQLStringType;
begin
  Result := SQLKeyWord('WHEN ', Options);
  if AnyError then
    Result := Result + SQLKeyWord('ANY', Options)
  else
  begin
    S := '';
    for I := 0 to Errors.Count - 1 do
    begin
      Sep := SQLListSeparator(Options);
      if (S <> '') then
        S := S + Sep;
      S := S + Errors[I].GetAsSQL(Options, AIndent);
    end;
    Result := Result + S;
  end;
  Result := Result + SQLKeyWord(' DO', Options) + SlineBreak;
  if Assigned(FStatement) then
    Result := Result + SQLIndentStatement(Statement.GetAsSQL(Options,
      AIndent), Options);
end;

{ TSQLCreateOrAlterProcedureTriggerStatement }

constructor TSQLCreateOrAlterProcedureTriggerStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FLocalVariables := TSQLElementList.Create(True);
  FStatements := TSQLElementList.Create(True);
end;

destructor TSQLCreateOrAlterProcedureTriggerStatement.Destroy;
begin
  FreeAndNil(FLocalVariables);
  FreeAndNil(FStatements);
  inherited Destroy;
end;

function TSQLCreateOrAlterProcedureTriggerStatement.GetAsSQL(
  Options: TSQLFormatOptions; AIndent: Integer): TSQLStringType;
var
  I: Integer;
  S: TSQLStringType;
begin
  Result := '';
  for I := 0 to LocalVariables.Count - 1 do
  begin
    Result := Result + SQLKeyWord('DECLARE VARIABLE ', Options);
    Result := Result + LocalVariables[I].GetAsSQL(Options, AIndent) +
      ';' + SlineBreak;
  end;
  Result := Result + SQLKeyWord('BEGIN', Options) + SlineBreak;
  for I := 0 to Statements.Count - 1 do
  begin
    S := Statements[I].GetAsSQL(Options, AIndent);
    if sfoIndentProcedureBlock in Options then
      S := SQLIndentStatement(S, Options);
    Result := Result + S + ';' + SlineBreak;
  end;
  Result := Result + SQLKeyWord('END', Options);
end;

{ TSQLAlterCreateTriggerStatement }

destructor TSQLAlterCreateTriggerStatement.Destroy;
begin
  FreeAndNil(FTableName);
  inherited Destroy;
end;

function TSQLAlterCreateTriggerStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
const
  SStates: array [TTriggerState] of string = ('', 'ACTIVE', 'INACTIVE');
  SMoment: array [TTriggerMoment] of string = ('BEFORE', 'AFTER');
  SOperation: array [TTriggerOperation] of string = ('DELETE', 'INSERT',
    'UPDATE');
var
  A: Boolean;
  S: TSQLStringType;
  O: TTriggerOperation;
begin
  A := Self is TSQLAlterTriggerStatement;
  if A then
    Result := SQLKeyWord('ALTER ', Options)
  else
    Result := SQLKeyWord('CREATE ', Options);
  Result := Result + SQLKeyWord('TRIGGER ', Options);
  if (ObjectName <> nil) then
    Result := Result + ObjectName.GetAsSQL(Options, AIndent);
  if not A and Assigned(FTableName) then
    Result := Result + SQLKeyWord(' FOR ', Options) +
      TableName.GetAsSQL(Options, AIndent);
  Result := Result + SlineBreak;
  if (State <> tsNone) then
    Result := Result + SStates[State] + SlineBreak;
  Result := Result + SQLKeyWord(SMoment[Moment] + ' ', Options);
  S := '';
  for O := Low(TTriggerOperation) to High(TTriggerOperation) do
    if O in Operations then
    begin
      if S <> '' then
        S := S + SQLKeyWord(' OR ', Options);
      S := S + SQLKeyWord(SOperation[O], Options);
    end;
  Result := Result + S + SlineBreak;
  if (Position <> 0) then
    Result := Result + SQLKeyWord('POSITION ', Options) + IntToStr(Position) +
      SlineBreak;
  Result := Result + SQLKeyWord('AS', Options) + SlineBreak;
  Result := Result + inherited GetAsSQL(Options, AIndent);
end;

{ TDeclareExternalFunctionStatement }

constructor TSQLDeclareExternalFunctionStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FArguments := TSQLElementList.Create(True);
end;

destructor TSQLDeclareExternalFunctionStatement.Destroy;
begin
  FreeAndNil(FArguments);
  FreeAndNil(FReturnType);
  inherited Destroy;
end;

function TSQLDeclareExternalFunctionStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sp, S, Sep: TSQLStringType;
  I: Integer;
begin
  if (sfoMultilineDeclareFunction in Options) then
    Sp := SlineBreak + '  '
  else
    Sp := ' ';
  Result := SQLKeyWord('DECLARE EXTERNAL FUNCTION ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Arguments.Count > 0 then
  begin
    Sep := SQLListSeparator(Options);
    S := '';
    for I := 0 to Arguments.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + Arguments[I].GetAsSQL(Options, AIndent);
    end;
    Result := Result + Sp + S;
  end;
  if Assigned(FReturnType) then
    Result := Result + Sp + SQLKeyWord('RETURNS ', Options) +
      ReturnType.GetAsSQL(Options, AIndent);
  Result := Result + Sp + SQLKeyWord('ENTRY_POINT ', Options) +
    SQLFormatString(EntryPoint, Options);
  Result := Result + Sp + SQLKeyWord('MODULE_NAME ', Options) +
    SQLFormatString(ModuleName, Options);
end;

{ TSQLIdentifierExpression }

constructor TSQLIdentifierExpression.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FElementIndex := -1;
end;

destructor TSQLIdentifierExpression.Destroy;
begin
  FreeAndNil(FIdentifier);
  inherited Destroy;
end;

function TSQLIdentifierExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  if Assigned(FIdentifier) then
    Result := Identifier.GetAsSQL(Options);
  if (ElementIndex <> -1) then
    Result := Result + Format('[%d]', [ElementIndex]);
end;

{ TSQLSelectExpression }

function TSQLSelectExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := '(' + inherited GetAsSQL(Options) + ')';
end;

{ TSQLExistsExpression }

function TSQLExistsExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLKeyWord('EXISTS', Options) + ' (' + inherited GetAsSQL(Options) + ')';
end;

{ TSQLSingularExpression }

function TSQLSingularExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLKeyWord('SINGULAR', Options) + ' (' +
    inherited GetAsSQL(Options) + ')';
end;

{ TSQLAllExpression }

function TSQLAllExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLKeyWord('ALL', Options) + ' (' + inherited GetAsSQL(Options) + ')';
end;

{ TSQLSomeExpression }

function TSQLSomeExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLKeyWord('SOME', Options) + ' (' + inherited GetAsSQL(Options) + ')';
end;

{ TSQLAnyExpression }

function TSQLAnyExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer = 0): TSQLStringType;
begin
  Result := SQLKeyWord('ANY', Options) + ' (' + inherited GetAsSQL(Options) + ')';
end;

{ TSQLExpression }

function TSQLExpression.UseBrackets: Boolean;
begin
  Result := False;
end;

{ TSQLNullLiteral }

function TSQLNullLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('NULL', Options);
end;

{ TSQLUserLiteral }

function TSQLUserLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('USER', Options);
end;

{ TSQLValueLiteral }

function TSQLValueLiteral.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('VALUE', Options);
end;

{ TSQLUniqueFieldConstraint }

function TSQLUniqueFieldConstraint.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  Result := Result + SQLKeyWord('UNIQUE', Options);
end;

{ TSQLPrimaryKeyFieldConstraint }

function TSQLPrimaryKeyFieldConstraint.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  Result := Result + SQLKeyWord('PRIMARY KEY', Options);
end;

{ TSQLTableUniqueConstraintDef }

function TSQLTableUniqueConstraintDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  Result := Result + SQLKeyWord('UNIQUE', Options) + ' ' +
    FieldListSQL(Options, AIndent);
end;

{ TSQLTablePrimaryKeyConstraintDef }

function TSQLTablePrimaryKeyConstraintDef.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if (Result <> '') then
    Result := Result + ' ';
  Result := Result + SQLKeyWord('PRIMARY KEY', Options) + ' ' +
    FieldListSQL(Options, AIndent);
end;

{ TSQLSelectNaturalPlan }

function TSQLSelectNaturalPlan.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(' NATURAL', Options);
end;

{ TSQLRollBackStatement }

function TSQLRollBackStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ROLLBACK', Options) + inherited GetAsSQL(Options, AIndent);
end;

{ TSQLCommitStatement }

function TSQLCommitStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('COMMIT', Options) + inherited GetAsSQL(Options, AIndent);
  if Retain then
    Result := Result + SQLKeyWord(' RETAIN', Options);
end;

{ TSQLCreateGeneratorStatement }

function TSQLCreateGeneratorStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('CREATE GENERATOR ', Options) +
    inherited GetAsSQL(Options, AIndent);
end;

{ TSQLCreateRoleStatement }

function TSQLCreateRoleStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('CREATE ROLE ', Options) +
    inherited GetAsSQL(Options, AIndent);
end;

{ TSQLAlterDomainDropDefaultStatement }

function TSQLAlterDomainDropDefaultStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent) +
    SQLKeyWord(' DROP DEFAULT', Options);
end;

{ TSQLAlterDomainDropCheckStatement }

function TSQLAlterDomainDropCheckStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent) + SQLKeyWord(' DROP CHECK', Options);
end;

{ TSQLAlterDomainStatement }

function TSQLAlterDomainStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALTER DOMAIN ', Options) +
    inherited GetAsSQL(Options, AIndent);
end;

{ TSQLAlterDomainRenameStatement }

destructor TSQLAlterDomainRenameStatement.Destroy;
begin
  FreeAndNil(FNewName);
  inherited Destroy;
end;

function TSQLAlterDomainRenameStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  if Assigned(FNewName) then
    Result := Result + ' ' + NewName.GetAsSQL(Options, AIndent);
end;

{ TSQLCreateExceptionStatement }

destructor TSQLCreateExceptionStatement.Destroy;
begin
  FreeAndNil(FMessage);
  inherited Destroy;
end;

function TSQLCreateExceptionStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  if Self is TSQLAlterExceptionStatement then
    Result := SQLKeyWord('ALTER ', Options)
  else
    Result := SQLKeyWord('CREATE ', Options);
  Result := Result + SQLKeyWord('EXCEPTION ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Assigned(FMessage) then
    Result := Result + ' ' + FMessage.GetAsSQL(Options, AIndent);
end;

{ TSQLAlterIndexStatement }

function TSQLAlterIndexStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALTER INDEX ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if Inactive then
    Result := Result + SQLKeyWord(' INACTIVE', Options)
  else
    Result := Result + SQLKeyWord(' ACTIVE', Options);
end;

{ TSQLDropTableFieldOperation }

function TSQLDropTableFieldOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('DROP ', Options) + inherited GetAsSQL(Options, AIndent);
end;

{ TSQLDropTableConstraintOperation }

function TSQLDropTableConstraintOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('DROP CONSTRAINT ', Options) +
    inherited GetAsSQL(Options, AIndent);
end;

{ TSQLAlterTableFieldPositionOperation }

function TSQLAlterTableFieldPositionOperation.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALTER COLUMN ', Options) +
    inherited GetAsSQL(Options, AIndent);
  Result := Result + SQLKeyWord(' POSITION ', Options) + IntToStr(NewPosition);
end;

{ TSQLCreateViewStatement }

function TSQLCreateViewStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep: TSQLStringType;
  I: Integer;
begin
  Result := SQLKeyWord('CREATE VIEW ', Options) +
    inherited GetAsSQL(Options, AIndent);
  if (Fields.Count > 0) then
  begin
    S := '';
    Sep := SQLListSeparator(Options);
    for I := 0 to Fields.Count - 1 do
    begin
      if (S <> '') then
        S := S + Sep;
      S := S + Fields[I].GetAsSQL(Options, AIndent);
    end;
    S := ' (' + S + ')' + SQLKeyWord(' AS ', Options);
  end
  else
    S := SQLKeyWord(' AS ', Options);
  Result := Result + S;
  if Assigned(FSelect) then
    Result := Result + Select.GetAsSQL(Options, AIndent);
end;

{ TSQLDatabaseFileInfo }

function TSQLDatabaseFileInfo.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('FILE ', Options) + SQLFormatString(FileName, Options);
  if Length > 0 then
    Result := Result + SQLKeyWord(' LENGTH ', Options) + IntToStr(Length) +
      SQLKeyWord(' PAGES', Options)
  else if (StartPage > 0) then
    Result := Result + SQLKeyWord(' STARTING AT ', Options) + IntToStr(StartPage);
end;

{ TSQLExitStatement }

function TSQLExitStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('EXIT', Options);
end;

{ TSQLSuspendStatement }

function TSQLSuspendStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('SUSPEND', Options);
end;

{ TSQLWhenSQLError }

function TSQLWhenSQLError.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('SQLCODE ', Options) + IntToStr(ErrorCode);
end;

{ TSQLWhenGDSError }

function TSQLWhenGDSError.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('GDSCODE ', Options) + IntToStr(GDSErrorNumber);
end;

{ TSQLDropTableStatement }

function TSQLDropTableStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'TABLE';
end;

{ TSQLDropIndexStatement }

function TSQLDropIndexStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'INDEX';
end;

{ TSQLDropViewStatement }

function TSQLDropViewStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'VIEW';
end;

{ TSQLDropProcedureStatement }

function TSQLDropProcedureStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'PROCEDURE';
end;

{ TSQLDropDomainStatement }

function TSQLDropDomainStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'DOMAIN';
end;

{ TSQLDropGeneratorStatement }

function TSQLDropGeneratorStatement.SQLObjectType
  (Options: TSQLFormatOptions): string;
begin
  Result := 'GENERATOR';
end;

{ TSQLDropTriggerStatement }

function TSQLDropTriggerStatement.SQLObjectType(Options: TSQLFormatOptions): string;
begin
  Result := 'TRIGGER';
end;

{ TSQLDropExceptionStatement }

function TSQLDropExceptionStatement.SQLObjectType(Options: TSQLFormatOptions): string;
begin
  Result := 'EXCEPTION';
end;

{ TSQLDropDatabaseStatement }

function TSQLDropDatabaseStatement.SQLObjectType(Options: TSQLFormatOptions): string;
begin
  Result := 'DATABASE';
end;

{ TSQLDropRoleStatement }

function TSQLDropRoleStatement.SQLObjectType(Options: TSQLFormatOptions): string;
begin
  Result := 'ROLE';
end;

{ TSQLDropExternalFunctionStatement }

function TSQLDropExternalFunctionStatement.SQLObjectType(
  Options: TSQLFormatOptions): string;
begin
  Result := 'EXTERNAL FUNCTION';
end;

{ TSQLDropShadowStatement }

function TSQLDropShadowStatement.SQLObjectType(Options: TSQLFormatOptions): string;
begin
  Result := 'SHADOW';
end;

{ TSQLConnectStatement }

function TSQLConnectStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('CONNECT ', Options) + SQLFormatString(DatabaseName, Options);
  if (UserName <> '') then
    Result := Result + SQLKeyWord(' USER ', Options) +
      SQLFormatString(UserName, Options);
  if (Password <> '') then
    Result := Result + SQLKeyWord(' PASSWORD ', Options) +
      SQLFormatString(Password, Options);
  if (Cache <> 0) then
    Result := Result + SQLKeyWord(' CACHE ', Options) + IntToStr(Cache);
  if (Role <> '') then
    Result := Result + SQLKeyWord(' ROLE ', Options) +
      SQLFormatString(Role, Options);
end;

{ TSQLExtractExpression }

destructor TSQLExtractExpression.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

function TSQLExtractExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('EXTRACT(' + ExtractElementNames[Element] + ' FROM ', Options);

  if Assigned(FValue) then
    Result := Result + Value.GetAsSQL(Options, AIndent);

  Result := Result + ')';
end;

{ TSQLParameterExpression }

destructor TSQLParameterExpression.Destroy;
begin
  FreeAndNil(FIdentifier);
  inherited Destroy;
end;

function TSQLParameterExpression.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := ':';
  if Assigned(FIdentifier) then
    Result := Result + Identifier.GetAsSQL(Options, AIndent);
end;

{ TSQLFieldConstraintList }

constructor TSQLFieldConstraintList.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FList := TSQLElementList.Create;
end;

destructor TSQLFieldConstraintList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSQLFieldConstraintList.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ' ';
    Result := Result + List[I].GetAsSQL(Options, AIndent);
  end;
end;

{ TSQLColumnPrivilege }

destructor TSQLColumnPrivilege.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

function TSQLColumnPrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  Result := '';
  if Assigned(FColumns) then
  begin
    Sep := SQLListSeparator(Options);
    for I := 0 to Columns.Count - 1 do
    begin
      if (Result <> '') then
        Result := Result + Sep;
      Result := Result + Columns[I].GetAsSQL(Options);
    end;
  end;
  if (Result <> '') then
    Result := ' (' + Result + ')';
end;

{ TSQLGrantStatement }

function TSQLGrantStatement.GranteesAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer; IsRevoke: Boolean = False): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to Grantees.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Grantees[I].GetAsSQL(Options, AIndent);
  end;
  if IsRevoke then
    Result := SQLKeyWord(' FROM ', Options) + Result
  else
    Result := SQLKeyWord(' TO ', Options) + Result;
end;

constructor TSQLGrantStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FGrantees := TSQLElementList.Create(True);
end;

destructor TSQLGrantStatement.Destroy;
begin
  FreeAndNil(FGrantees);
  inherited Destroy;
end;

{ TSQLTableGrantStatement }

constructor TSQLTableGrantStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FPrivileges := TSQLElementList.Create(True);
end;

destructor TSQLTableGrantStatement.Destroy;
begin
  FreeAndNil(FPrivileges);
  FreeAndNil(FTableName);
  inherited Destroy;
end;

function TSQLTableGrantStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to Privileges.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Privileges[I].GetAsSQL(Options, AIndent);
  end;
  Result := SQLKeyWord('GRANT ', Options) + Result + SQLKeyWord(' ON ', Options);
  if Assigned(FTableName) then
    Result := Result + FTableName.GetAsSQL(Options, AIndent);
  Result := Result + Self.GranteesAsSQL(Options, AIndent);
  if GrantOption then
    Result := Result + SQLKeyWord(' WITH GRANT OPTION', Options);
end;

{ TSQLProcedureGrantStatement }

destructor TSQLProcedureGrantStatement.Destroy;
begin
  FreeAndNil(FProcedureName);
  inherited Destroy;
end;

function TSQLProcedureGrantStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('GRANT EXECUTE ON PROCEDURE ', Options);
  if Assigned(FProcedureName) then
    Result := Result + FProcedureName.GetAsSQL(Options, AIndent);
  Result := Result + GranteesAsSQL(Options, AIndent);
end;

{ TSQLRoleGrantStatement }

constructor TSQLRoleGrantStatement.Create(AParent: TSQLElement);
begin
  inherited Create(AParent);
  FRoles := TSQLElementList.Create(True);
end;

destructor TSQLRoleGrantStatement.Destroy;
begin
  FreeAndNil(FRoles);
  inherited Destroy;
end;

function TSQLRoleGrantStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to Roles.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Roles[I].GetAsSQL(Options, AIndent);
  end;
  Result := SQLKeyWord('GRANT ', Options) + Result;
  Result := Result + GranteesAsSQL(Options, AIndent);
  if AdminOption then
    Result := Result + SQLKeyWord(' WITH ADMIN OPTION', Options);
end;

{ TSQLInsertPrivilege }

function TSQLInsertPrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('INSERT', Options);
end;

{ TSQLDeletePrivilege }

function TSQLDeletePrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('DELETE', Options);
end;

{ TSQLSelectPrivilege }

function TSQLSelectPrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('SELECT', Options);
end;

{ TSQLAllPrivilege }

function TSQLAllPrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('ALL PRIVILEGES', Options);
end;

{ TSQLUpdatePrivilege }

function TSQLUpdatePrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('UPDATE', Options) + Result;
end;

{ TSQLReferencePrivilege }

function TSQLReferencePrivilege.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('REFERENCES', Options) + Result;
end;

{ TSQLGroupGrantee }

function TSQLGroupGrantee.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('GROUP ', Options) + Result;
end;

{ TSQLProcedureGrantee }

function TSQLProcedureGrantee.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('PROCEDURE ', Options) + Result;
end;

{ TSQLViewGrantee }

function TSQLViewGrantee.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('VIEW ', Options) + Result;
end;

{ TSQLTriggerGrantee }

function TSQLTriggerGrantee.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := inherited GetAsSQL(Options, AIndent);
  Result := SQLKeyWord('TRIGGER ', Options) + Result;
end;

{ TSQLPublicGrantee }

function TSQLPublicGrantee.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('PUBLIC', Options);
end;

{ TSQLTableRevokeStatement }

function TSQLTableRevokeStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  S, Sep: TSQLStringType;
  I: Integer;
begin
  Sep := SQLListSeparator(Options);
  S := '';
  for I := 0 to Privileges.Count - 1 do
  begin
    if (S <> '') then
      S := S + Sep;
    S := S + Privileges[I].GetAsSQL(Options, AIndent);
  end;
  Result := SQLKeyWord('REVOKE ', Options);
  if GrantOption then
    Result := Result + SQLKeyWord('GRANT OPTION FOR ', Options);
  Result := Result + S + SQLKeyWord(' ON ', Options);
  if Assigned(FTableName) then
    Result := Result + FTableName.GetAsSQL(Options, AIndent);
  Result := Result + Self.GranteesAsSQL(Options, AIndent, True);
end;

{ TSQLProcedureRevokeStatement }

function TSQLProcedureRevokeStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
begin
  Result := SQLKeyWord('REVOKE EXECUTE ON PROCEDURE ', Options);
  if Assigned(FProcedureName) then
    Result := Result + FProcedureName.GetAsSQL(Options, AIndent);
  Result := Result + GranteesAsSQL(Options, AIndent, True);
end;

{ TSQLRoleRevokeStatement }

function TSQLRoleRevokeStatement.GetAsSQL(Options: TSQLFormatOptions;
  AIndent: Integer): TSQLStringType;
var
  Sep: TSQLStringType;
  I: Integer;
begin
  Result := '';
  Sep := SQLListSeparator(Options);
  for I := 0 to Roles.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + Sep;
    Result := Result + Roles[I].GetAsSQL(Options, AIndent);
  end;
  Result := SQLKeyWord('REVOKE ', Options) + Result;
  Result := Result + GranteesAsSQL(Options, AIndent, True);
end;

end.
