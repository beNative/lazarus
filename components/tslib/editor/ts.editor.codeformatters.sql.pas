{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.CodeFormatters.SQL;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Core.SQLParser, ts.Core.SQLScanner, ts.Core.SQLTree,

  ts.Editor.CodeFormatters;

type
  TSqlFormatter = class(TInterfacedObject, ICodeFormatter)
  private
    FLineReader : TStreamLineReader;
    FSqlParser  : TSQLParser;
    FSqlScanner : TSQLScanner;
    FSqlStream  : TStringStream;

  protected
    function Format(const AString: string): string; virtual;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TSqlFormatter.AfterConstruction;
begin
  inherited AfterConstruction;
  FLineReader := TStreamLineReader.Create(FSqlStream);
  FSqlScanner := TSQLScanner.Create(FLineReader);
  FSqlParser  := TSQLParser.Create(FSqlScanner);
end;

destructor TSqlFormatter.Destroy;
begin
  FSqlStream.Free;
  FLineReader.Free;
  FSqlScanner.Free;
  FSqlParser.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TSqlFormatter.Format(const AString: string): string;
var
  SS : TStringStream = nil;
  LR : TStreamLineReader = nil;
  P  : TSQLParser = nil;
  S  : TSQLScanner = nil;
  E  : TSQLElement = nil;
begin
  SS := TStringStream.Create(AString);
  LR := TStreamLineReader.Create(SS);
  S  := TSQLScanner.Create(LR);
  P  := TSQLParser.Create(S);
  try
    try
      E := P.Parse;
    except
      E := nil;
      Result := AString;
    end;
   if (Assigned(E)) and (Length(AString) > 0) then
      Result := E.GetAsSQL(
        [
          //sfoDoubleQuotes,           // Use double quote character for string literals
          sfoBackslashEscape,        // Backslash escapes in string literals
          //sfoSingleQuoteIdentifier,  // quote Identifiers using '
          //sfoDoubleQuoteIdentifier,  // quote Identifiers using "
          //sfoBackQuoteIdentifier,    // quote Identifiers using `
          sfoLowercaseKeyword,       // Lowercase SQL keywords
          sfoOneFieldPerLine,        // One field per line in SELECT, Update, Insert
          sfoIndentFields,           // Indent fields (indent=2 space characters)
          sfoOneTablePerLine,        // One table per line in select FROM clause
          sfoIndentTables,           // Indent tables in FROM clause
          sfoNoBracketRightJoin,     // In join, Do not put ( ) around right table if it is also a join
          //sfoBracketLeftJoin,        // In join, put ( ) around left table if it is also a join
          sfoWhereOnSeparateLine,    // Put WHERE clause on a separate line
          sfoIndentWhere,            // Indent WHERE clause
          sfoOneGroupByFieldPerLine, // One field per line in GROUP BY
          sfoIndentGroupByFields,    // Indent GROUP BY fields (indent=2 space characters)
          sfoHavingOnSeparateLine,   // Put HAVING clause on a separate line
          sfoIndentHaving,           // Indent HAVING clause
          sfoUnionOnSeparateLine,    // Put UNION on separate line
          sfoOneOrderByFieldPerLine, // One field per line in ORDER BY
          sfoIndentOrderByFields,    // Indent ORDER BY fields (indent=2 space characters)
          sfoPlanOnSeparateLine,     // Put HAVING clause on a separate line
          sfoIndentPlan,             // Indent HAVING clause
          sfoOneLogicalPerLine,      // in AND or OR clauses, put newline before AND or OR
          sfoListNoSpaceBeforeComma, // In comma-separated lists, do not put space before ,
          //sfoListNoSpaceAfterComma,  // In comma-separated lists, do not put space after ,
          sfoForceAscending,         // In ORDER BY, explicitly write ASC
          sfoMultilineDeclareFunction, // Separate parts of 'Declare function' with newlines
          sfoMultilineCreateDatabase,  // Separate parts of create/alter database with newlines
          sfoMultilineCreateShadow,    // Separate additional filespecs of create/alter shadow with newlines
          sfoIndentProcedureBlock      // Indent statements inside procedure/trigger statement block
        ],
        2
      );
  finally
    SS.Free;
    LR.Free;
    P.Free;
    S.Free;
    E.Free;
  end;
end;
{$ENDREGION}

end.

