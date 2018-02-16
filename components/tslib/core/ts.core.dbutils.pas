{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.DBUtils;

{ Collection of common DB routines.

  Author: Tim Sinaeve }

{
  TODO:
  - Global variable tsDBUtilsADOConnection. If assigned the first parameter
    can be omitted => needs to be investigated.

  - QueryLookup with TtsKeyValues
}

interface

uses
  SysUtils, Classes, Variants, Graphics, DB, DBGrids,

  BufDataset,

  ts.Core.KeyValues;

type
  EQueryLookup = class(Exception);

function IsNumericFieldType(ADataType: TFieldType): Boolean;

function IsTemporalFieldType(ADataType: TFieldType): Boolean;

function IsBlobFieldType(ADataType: TFieldType): Boolean;

function IsAnsiStringFieldType(ADataType: TFieldType): Boolean;

function IsUnicodeStringFieldType(ADataType: TFieldType): Boolean;

function IsStringFieldType(ADataType: TFieldType): Boolean;

function IsObjectFieldType(ADataType: TFieldType): Boolean;

procedure AutoSizeDisplayWidths(ADataSet : TDataSet;
                                ACount   : Integer = 100;
                                AOffset  : Integer = 2); overload;

procedure AutoSizeDisplayWidths(ADataSet : TDataSet;
                                AFont    : TFont;
                                ACount   : Integer = 100;
                                AOffset  : Integer = 0); overload;

//function SortClientDataSet(      ADataSet    : TClientDataSet;
//                           const AFieldName  : string;
//                           var   ADescending : Boolean): Boolean;

function FieldTypeForVariant(const Value: Variant): TFieldType;

procedure CloneDataSet(ASource: TDataSet; ADest: TBufDataset);

//-----------------------------------------------------------------------------

// assign the fieldvalue; returns True if the Value was posted.

function AssignFieldValue(      AField  : TField;
                          const AValue  : Variant;
                                ADoPost : Boolean = True) : Boolean; overload;

function AssignFieldValue(       ADataSet   : TDataSet;
                           const AFieldName : string;
                           const AValue     : Variant;
                                 ADoPost    : Boolean = True) : Boolean; overload;

// clear the fieldvalue

procedure ClearFieldValue(AField  : TField;
                          ADoPost : Boolean = True); overload;

procedure ClearFieldValue(      ADataSet   : TDataSet;
                          const AFieldName : string;
                                ADoPost    : Boolean = True); overload;

//-----------------------------------------------------------------------------

// SQLSelect ?

// use KeyValues to store returned selection
{

function QueryLookup (      AConnection : TZConnection;
                      const AQuery      : string;
                      const AParams     : array of const): Variant; overload;

function QueryLookup (      AConnection : TZConnection;
                      const AQuery      : string): Variant; overload;

function QueryLookup(      AConnection   : TZConnection;
                     const ATable        : string;
                     const AResultFields : string;
                     const AKeyValue     : Variant;
                     const AKeyField     : string = 'ID') : Variant; overload;

function QueryLookup(      AConnection    : TZConnection;
                     const ATable         : string;
                     const AResultField   : string;
                           AResultStrings : TStrings;
                     const AKeyValue      : Variant;
                     const AKeyField      : string = '') : Boolean; overload;
                     }
//-----------------------------------------------------------------------------
{
function UpdateRecord(      AConnection  : TZConnection;
                      const ATable       : string;
                      const AUpdateField : string;
                      const AUpdateValue : Variant;
                      const AKeyValue    : Variant;
                      const AKeyField    : string = 'ID') : Boolean;

function UpdateRecords(      AConnection  : TZConnection;
                       const ATable       : string;
                       const AUpdateField : string;
                       const AUpdateValue : Variant;
                       const AKeyValues   : string;
                       const AKeyField    : string = 'ID') : Boolean;

function InsertRecord(      AConnection   : TZConnection;
                      const ATable        : string;
                      const AInsertFields : string;
                      const AFieldValues  : Variant) : Boolean;

//function SQLDelete(      AADOConnection : TZConnection;
//                   const ATable         : string;
//                   const AKeyValue      : Variant;
//                   const AKeyField      : string = 'ID');
}

//-----------------------------------------------------------------------------

//SQLCall?

//function ExecuteStoredProcedure(      AProcedure     : TADOStoredProc;
//                                const AParameters    : Variant;
//                                const ADefaultValue  : Variant) : Variant;
//  overload;
//
//function ExecuteStoredProcedure(      AProcedure     : TADOStoredProc;
//                                const AParameters    : Variant) : Variant;
//  overload;
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string;
//                                const AParameters    : Variant;
//                                const ADefaultValue  : Variant) : Variant;
//  overload;
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string;
//                                const AParameters    : Variant) : Variant;
//  overload;
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string) : Variant;
//  overload;

//-----------------------------------------------------------------------------

{
function GetQueryRecordCount(      AConnection    : TZConnection;
                             const AFromClause    : string;
                             const AWhereClause   : string = '';
                             const AGroupByClause : string = '') : Integer;
}

//-----------------------------------------------------------------------------

//function GetSPReturnValue(AADOCommand : TADOCommand) : Variant;

//-----------------------------------------------------------------------------

procedure KeyValuesToFields(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues); overload;

procedure KeyValuesToFields(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues;
                            const AFields    : array of string); overload;

procedure FieldsToKeyValues(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues;
                            const AFields    : array of string);

//-----------------------------------------------------------------------------

function PostData(ADataSet : TDataSet) : Boolean;

//-----------------------------------------------------------------------------

procedure ShowDataSet(ADataSet : TDataSet);

//-----------------------------------------------------------------------------

function DataSetToString(ADataSet : TDataSet;
                         AFields  : array of string): string; overload;

function DataSetToString(ADataSet : TDataSet): string; overload;

//*****************************************************************************

implementation

uses
  TypInfo, Forms, Controls, Dialogs, StrUtils;

  //ZDataset;

//=============================================================================

resourcestring
  SQueryLookupErrorRunningQuery = 'Error running query [%s]';
  SQueryLookupTooManyRecords    = 'The query [%s] returned too many records';
  SNoFieldTypeForVariantValue   = 'No fieldtype for Variant value %s';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';

//-----------------------------------------------------------------------------

const
  AnsiStringFieldTypes    = [ftString, ftFixedChar, ftGuid];
  UnicodeStringFieldTypes = [ftWideString, ftFixedWideChar];
  StringFieldTypes        = AnsiStringFieldTypes + UnicodeStringFieldTypes;
  BlobFieldTypes          = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
                             ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob,
                             ftWideMemo];
  NumericFieldTypes       = [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
                             ftBCD, ftAutoInc, ftLargeint, ftFMTBcd];
  TemporalFieldTypes      = [ftDate, ftTime, ftDateTime, ftTimeStamp];
  ObjectFieldTypes        = [ftADT, ftArray, ftReference, ftDataSet];

//=============================================================================

function IsNumericFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in NumericFieldTypes;
end;

function IsTemporalFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in TemporalFieldTypes;
end;

function IsBlobFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in BlobFieldTypes;
end;

function IsAnsiStringFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in AnsiStringFieldTypes;
end;

function IsUnicodeStringFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in UnicodeStringFieldTypes;
end;

function IsStringFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in StringFieldTypes;
end;

function IsObjectFieldType(ADataType: TFieldType): Boolean;
begin
  Result := ADataType in ObjectFieldTypes;
end;

//-----------------------------------------------------------------------------

{ Returns the corresponding fieldtype for a given Variant }

function FieldTypeForVariant(const Value: Variant): TFieldType;
begin
  case VarType(Value) and varTypeMask of
    varSmallint : Result := ftSmallint;
    varInteger  : Result := ftInteger;
    varSingle   : Result := ftFloat;
    varDouble   : Result := ftFloat;
    varCurrency : Result := ftCurrency;
    varDate     : Result := ftDateTime;
    varOleStr   : Result := ftString;
    varBoolean  : Result := ftBoolean;
    varString   : Result := ftString;
  else
    raise Exception.CreateFmt(SNoFieldTypeForVariantValue, [Value]);
  end;
end;

//-----------------------------------------------------------------------------

procedure AutoSizeDisplayWidths(ADataSet : TDataSet;
                                AFont    : TFont;
                                ACount   : Integer;
                                AOffset  : Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string; AFont: TFont): Integer;
  var
    Bitmap  : TBitmap;
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Canvas.Font.Assign(AFont);
        R := 0;
        for I := 0 to SL.Count - 1 do
        begin
          W := Bitmap.Canvas.TextWidth(SL[I]);
          if W > R then
            R := W;
        end;
        Result := R div AFont.Size;
      finally
        Bitmap.Free;
      end;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.GetBookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);

      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, ftWideMemo, ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText, AFont) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.GotoBookmark(BM);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

{
  REMARK : This method is not suitable for filtered datasets. For filtered
  datasets the records should be enumerated with the dataset's FindFirst and
  FindNext methods. We didn't use those methods because when used in combination
  with a ClientDataSet (with fetch on demand enabled) this causes the provider
  to fetch all the records from the server.
}

procedure AutoSizeDisplayWidths(ADataSet : TDataSet; ACount : Integer;
  AOffSet : Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string): Integer;
  var
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      R := 0;
      for I := 0 to SL.Count - 1 do
      begin
        W := Length(SL[I]);
        if W > R then
          R := W;
      end;
      Result := R;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.GetBookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);
      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, {ftWideMemo,} ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.GotoBookmark(BM);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

{ Sorts a ClientDataSet on a given field }

//function SortClientDataSet(ADataSet: TClientDataSet;
//  const AFieldName: string; var ADescending: Boolean): Boolean;
//var
//  I            : Integer;
//  IndexDefs    : TIndexDefs;
//  IndexName    : string;
//  IndexOptions : TIndexOptions;
//  Field        : TField;
//  bSortable    : Boolean;
//  B            : Boolean;
//begin
//  B := False;
//  Field := ADataSet.Fields.FindField(AFieldName);
//
//  bSortable := Assigned(Field) and
//               // check if field type is valid
//               not ((Field is TObjectField) or (Field is TBlobField) or
//                    (Field is TBinaryField) or (Field is TAggregateField) or
//                    (Field is TVariantField)) and
//               // check if field kind is valid
//               not (Field.FieldKind in [fkCalculated, fkLookup, fkAggregate]);
//
//  if bSortable then
//  begin
//    IndexDefs := ADataSet.IndexDefs;
//    IndexName := ADataSet.IndexName;
//    IndexDefs.Update;
//    // If an ascending index is already in use, switch to a descending index
//    if IndexName = AFieldName + '__IdxA' then
//    begin
//      IndexName    := AFieldName + '__IdxD';
//      IndexOptions := [ixDescending];
//      ADescending  := True;
//    end
//    else
//    begin
//      IndexName    := AFieldName + '__IdxA';
//      IndexOptions := [];
//      ADescending  := False;
//    end;
//
//    // Look for existing index
//    I := 0;
//    while (I < IndexDefs.Count) and (IndexDefs[I].Name <> IndexName) do
//      Inc(I);
//    B := (I < IndexDefs.Count) and (IndexDefs[I].Name = IndexName);
//
//    // If existing index not found, create one
//    if not B then
//    begin
//      ADataSet.AddIndex(IndexName, AFieldName, IndexOptions);
//      B := True;
//    end; // if not
//
//    // Set the index
//    ADataSet.IndexName := IndexName;
//  end;
//  Result := B;
//end;

//-----------------------------------------------------------------------------

{ Clones a DataSet to a TBufDataSet instance. }

procedure CloneDataSet(ASource: TDataSet; ADest: TBufDataSet);
var
  I : Integer;
begin
  if Assigned(ASource) and Assigned(ADest) then
  begin
    ASource.DisableControls;
    try
      ADest.Active := False;
      ADest.FieldDefs.Clear;
      ADest.Fields.Clear;

      // copy field definitions
      for I := 0 to Pred(ASource.FieldDefs.Count) do
        with ADest.FieldDefs.AddFieldDef do
        begin
          Assign(ASource.FieldDefs[I]);
          Name := ASource.FieldDefs[I].DisplayName;
        end;

      // create fields
      ADest.CreateDataSet;

      // copy data
      ADest.Active := True;
      ASource.First;
      while not ASource.EOF do
      begin
        ADest.Append;
        for I := 0 to Pred(ASource.FieldCount) do
        begin
          ADest.Fields[I].Assign(ASource.Fields[I]);
          ADest.Fields[I].DisplayLabel := ASource.Fields[I].DisplayLabel;
        end;
        ADest.Post;
        ASource.Next;
      end; // while not ASource.Eof do begin
    finally
      ASource.EnableControls;
    end;
  end // if Assigned(ASource) and Assigned(ADest) then...
  else
    raise Exception.Create('No value assigned to ASource and/or ADest.');
end;

//-----------------------------------------------------------------------------

function AssignFieldValue(AField : TField; const AValue : Variant;
  ADoPost : Boolean): Boolean;
begin
  Result := False;
  if not Assigned(AField) then
    raise Exception.Create('AField is not assigned');

  if AField.Value <> AValue then
  begin
    AField.DataSet.Edit;
    AField.Value := AValue;
    if ADoPost then
    begin
      AField.DataSet.Post;
      Result := True;
    end;
  end;
end;

//-----------------------------------------------------------------------------

function AssignFieldValue(ADataSet: TDataSet; const AFieldName : string;
 const AValue: Variant; ADoPost: Boolean): Boolean;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned');

  Result := False;
  if ADataSet.FieldByName(AFieldName).Value <> AValue then
  begin
    ADataSet.Edit;
    ADataSet.FieldByName(AFieldName).Value := AValue;
    if ADoPost then
    begin
      ADataSet.Post;
      Result := True;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure ClearFieldValue(AField: TField; ADoPost: Boolean);
begin
  if not Assigned(AField) then
    raise Exception.Create('AField is not assigned');

  AField.DataSet.Edit;
  AField.Clear;
  if ADoPost then
    AField.DataSet.Post;
end;

//-----------------------------------------------------------------------------

procedure ClearFieldValue(ADataSet: TDataSet; const AFieldName: string;
  ADoPost: Boolean); overload;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned');

  ADataSet.Edit;
  ADataSet.FieldByName(AFieldName).Clear;
  if ADoPost then
    ADataSet.Post;
end;

//-----------------------------------------------------------------------------
{
function QueryLookup(AConnection: TZConnection; const AQuery: string;
const AParams: array of const) : Variant;
var
  DS : TZQuery;
  I  : Integer;
begin
  if not Assigned(AConnection) then
    raise EQueryLookup.CreateFmt(SParameterNotAssigned, ['AADOConnection']);
  DS := TZQuery.Create(nil);
  try
    DS.Connection  := AConnection;
    if Length(AParams) > 0 then
      DS.SQL.Text := Format(AQuery, AParams)
    else
      DS.SQL.Text := AQuery;
    try
      DS.Active := True;
    except
      raise EQueryLookup.CreateFmt(SQueryLookupErrorRunningQuery, [DS.SQL.Text]);
    end;
    if DS.IsEmpty then
      Result := Unassigned
    else if DS.RecordCount = 1 then
// Cannot use this because fieldname can differ from the one given in the select
//        Result := DS.FieldValues[AResultField]
    begin
      if DS.Fields.Count = 1 then
        Result := DS.Fields[0].Value
      else
      begin
        Result := VarArrayCreate([0, DS.Fields.Count], varVariant);
        for I := 0 to DS.Fields.Count - 1 do
          Result[I] := DS.Fields[I].Value
      end;
    end
    else
      raise EQueryLookup.CreateFmt(SQueryLookupTooManyRecords, [DS.SQL.Text])
  finally
    DS.Free;
  end;

end;

//-----------------------------------------------------------------------------

function QueryLookup(AConnection: TZConnection; const AQuery: string)
  : Variant;
begin
  Result := QueryLookup(AConnection, AQuery, []);
end;

//-----------------------------------------------------------------------------
{
   SELECT
     <comma separated AResultFields>
   FROM
     <ATable>
   WHERE
     <AKeyField> = '<AKeyValue>'

   Returns a variant or a variant array with the result value(s).

   If the query returns more than 1 record, an exception is raised.

   When no records are returned, the return value is Unassigned. This translates
   to an empty string value.

   Null values are translated to empty strings only when the global variable
   Variants.NullStrictConvert is set to False (default is True), otherwise an
   exception is raised.
}

function QueryLookup(AConnection : TZConnection; const ATable,
  AResultFields: string; const AKeyValue: Variant; const AKeyField: string)
  : Variant;
var
  sSQL : string;
const
  SQL_SELECT_WHERE = 'SELECT %s FROM %s WHERE %s = %s';
begin
  if not Assigned(AConnection) then
    raise EQueryLookup.CreateFmt(SParameterNotAssigned, ['AConnection']);
  if VarIsNull(AKeyValue) then
    Result := Unassigned
  else
  begin
    sSQL := Format(SQL_SELECT_WHERE,
                   [AResultFields, ATable, AKeyField, QuotedStr(AKeyValue)]);
    Result := QueryLookup(AConnection, sSQL);
  end;
end;

//-----------------------------------------------------------------------------

{
   SELECT
     <AResultField>
   FROM
     <ATable>
   WHERE
     <AKeyField> = '<AKeyValue>'

   Returns True if the resulset is not empty.

   The returned values are added to the AResultStrings stringlist.
}

function QueryLookup(AConnection : TZConnection; const ATable,
  AResultField: string;  AResultStrings : TStrings; const AKeyValue: Variant;
  const AKeyField: string) : Boolean;
const
  SQL_SELECT       = 'SELECT %s FROM %s';
  SQL_SELECT_WHERE = SQL_SELECT + ' WHERE %s = %s';
var
  DS   : TZQuery;
  sSQL : string;
begin
  Result := True;
  if not Assigned(AConnection) then
    raise EQueryLookup.CreateFmt(SParameterNotAssigned, ['AConnection']);
  if not Assigned(AResultStrings) then
    raise EQueryLookup.CreateFmt(SParameterNotAssigned, ['AResultStrings']);

  AResultStrings.Clear;
  DS := TZQuery.Create(nil);
  try
    DS.Connection := AConnection;
    if AKeyField = '' then
      sSQL := Format(SQL_SELECT, [AResultField, ATable])
    else
      sSQL := Format(SQL_SELECT_WHERE,
                     [AResultField, ATable, AKeyField, QuotedStr(AKeyValue)]);
    DS.SQL.Text := sSQL;

    try
      DS.Active := True;
    except
      raise EQueryLookup.CreateFmt(SQueryLookupErrorRunningQuery, [sSQL]);
    end;

    Result := not DS.IsEmpty;
    if Result then
    begin
      DS.First;
      while not DS.Eof do
      begin
        AResultStrings.Add(DS.Fields[0].AsString);
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;
end;

}
//-----------------------------------------------------------------------------

//function ExecuteStoredProcedure(      AProcedure    : TADOStoredProc;
//                                const AParameters   : Variant;
//                                const ADefaultValue : Variant) : Variant;
//var
//  I : Integer;
//  J : Integer;
//  V : Variant;
//  P : TParameter;
//begin
//  Result := Unassigned;
//  if VarIsArray(AParameters) then
//  begin
//    J := 0;
//    for I := 0 to AProcedure.Parameters.Count - 1 do
//    begin
//      P := AProcedure.Parameters[I];
////      P.Attributes:= [paNullable, paLong];
////      P.ParameterObject.Type_:= adVarChar;
//      if P.DataType = ftString then
//        P.Size := 10000;
//      if P.Direction <> pdReturnValue then
//      begin
//        P.Value := AParameters[J];
//        Inc(J);
//      end;
//    end;
//  end
//  else
//  begin
//    if AParameters <> Null then
//    begin
//      if AProcedure.Parameters[0].Direction <> pdReturnValue then
//      begin
//        AProcedure.Parameters[0].Value := AParameters;
//        if AProcedure.Parameters[0].DataType = ftString then
//          AProcedure.Parameters[0].Size := 10000;
//      end
//      else
//      begin
//        if AProcedure.Parameters.Count > 1 then
//        begin
//          AProcedure.Parameters[1].Value := AParameters;
//          if AProcedure.Parameters[1].DataType = ftString then
//            AProcedure.Parameters[1].Size := 10000;
//        end;
//      end;
//    end;
//  end;
//  AProcedure.Prepared := True;
//  AProcedure.ExecProc;
//
//  V := Null;
//  for I := 0 to AProcedure.Parameters.Count - 1 do
//    if AProcedure.Parameters[I].Direction = pdReturnValue then
//      V := AProcedure.Parameters[I].ParameterObject.Value;
//
//  if V = Null then
//    Result := ADefaultValue
//  else
//    Result := V;
//end;
//
////-----------------------------------------------------------------------------
//
//function ExecuteStoredProcedure(      AProcedure     : TADOStoredProc;
//                                const AParameters    : Variant) : Variant;
//begin
//  Result := ExecuteStoredProcedure(AProcedure, AParameters, Null);
//end;
//
////-----------------------------------------------------------------------------
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string;
//                                const AParameters    : Variant;
//                                const ADefaultValue  : Variant) : Variant;
//var
//  SP   : TADOStoredProc;
//begin
//  SP := TADOStoredProc.Create(nil);
//  try
//    SP.Connection    := AConnection;
//    SP.ProcedureName := AProcedure;
//    SP.Parameters.Refresh;
//    Result := ExecuteStoredProcedure(SP, AParameters, ADefaultValue);
//  finally
//    SP.Free;
//  end;
//end;
//
////-----------------------------------------------------------------------------
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string;
//                                const AParameters    : Variant) : Variant;
//begin
//  Result :=
//    ExecuteStoredProcedure(AConnection, AProcedure, AParameters, Null);
//end;
//
////-----------------------------------------------------------------------------
//
//function ExecuteStoredProcedure(      AConnection : TZConnection;
//                                const AProcedure     : string) : Variant;
//begin
//  Result := ExecuteStoredProcedure(AConnection, AProcedure, Null);
//end;

//-----------------------------------------------------------------------------
{
function UpdateRecord(AConnection : TZConnection; const ATable : string;
  const AUpdateField : string; const AUpdateValue : Variant;
  const AKeyValue : Variant; const AKeyField : string) : Boolean;
const
  UPDATE_SQL = 'UPDATE %s' + #13#10 +
               '  SET %s = %s' + #13#10 +
               'WHERE %s = %s';
var
  N            : Integer;
  sSQL         : string;
  sUpdateValue : string;
begin
  N := 0;
  if AUpdateValue = Null then
    sUpdateValue := 'NULL'
  else if VarIsNumeric(AUpdateValue) then
    sUpdateValue := AUpdateValue
  else
    sUpdateValue := QuotedStr(AUpdateValue);

  sSQL := Format(UPDATE_SQL,
                 [ATable, AUpdateField, sUpdateValue, AKeyField,
                  QuotedStr(AKeyValue)]);
  AConnection.ExecuteDirect(sSQL, N);
  Result := N <> 0;
end;

//-----------------------------------------------------------------------------

function UpdateRecords(      AConnection : TZConnection;
                       const ATable         : string;
                       const AUpdateField   : string;
                       const AUpdateValue   : Variant;
                       const AKeyValues     : string;
                       const AKeyField      : string = 'ID') : Boolean;
const
  UPDATE_SQL = 'UPDATE %s' + #13#10 +
               '  SET %s = %s' + #13#10 +
               'WHERE %s IN (%s)';
var
  N            : Integer;
  sSQL         : string;
  sUpdateValue : string;
begin
  N := 0;
  if AUpdateValue = Null then
    sUpdateValue := 'NULL'
  else if VarIsType(AUpdateValue, varBoolean) then
    sUpdateValue := IfThen(AUpdateValue, '1', '0')
  else if VarIsNumeric(AUpdateValue) then
    sUpdateValue := AUpdateValue

  else
    sUpdateValue := QuotedStr(AUpdateValue);
  sSQL := Format(UPDATE_SQL,
                 [ATable, AUpdateField, sUpdateValue, AKeyField,
                  AKeyValues]);
  AConnection.ExecuteDirect(sSQL, N);
  Result := N <> 0;
end;

//-----------------------------------------------------------------------------

function InsertRecord(AConnection : TZConnection; const ATable,
  AInsertFields : string; const AFieldValues: Variant) : Boolean;
const
  SQL_INSERT = 'INSERT INTO %s (%s)' + #13#10 +
               'VALUES (%s)';
var
  S : string;
  I : Integer;
  T : string;
begin
  S := '';
  Result := True;
  if VarIsArray(AFieldValues) then
  begin
    for I := VarArrayLowBound(AFieldValues, 1) to
             VarArrayHighBound(AFieldValues, 1) do
    begin
      T := VarToStr(AFieldValues[I]);
      if not ((T[1] = '{') and (T[Length(T)] = '}')) then  // '{' and '}' are used for passing date values.
        T := QuotedStr(T);
      S := S + T;
      if I < VarArrayHighBound(AFieldValues, 1) then
        S := S + ', '
    end;
  end
  else
  begin
    if AFieldValues <> Null then
      S := QuotedStr(VarToStr(AFieldValues));
  end;
  AConnection.ExecuteDirect(Format(SQL_INSERT, [ATable, AInsertFields, S]));
end;

//-----------------------------------------------------------------------------

{ TODO:
   - when GROUP BY is used, the returned recordcount is not correct. }

function GetQueryRecordCount(AConnection : TZConnection;
  const AFromClause, AWhereClause, AGroupByClause : string) : Integer;
const
  QUERY    = 'SELECT COUNT (1) AS RecordCount FROM (%s)';
  WHERE    = 'WHERE %s';
  GROUP_BY = 'GROUP BY %s';
var
  S  : string;
  DS : TZQuery;
begin
  if AWhereClause <> '' then
    S := Format(QUERY + #13#10 + WHERE, [AFromClause, AWhereClause])
  else
    S := Format(QUERY, [AFromClause]);

  if AGroupByClause <> '' then
    S := S + #13#10 + Format(GROUP_BY, [AGroupByClause]);

  DS := TZQuery.Create(nil);
  try
    DS.Connection := AConnection;
    DS.SQL.Text := S;
    DS.Active := True;
    Result := DS.FieldByName('RecordCount').AsInteger;
    DS.Active := False;
  finally
    DS.Free;
  end;
end;

}
//-----------------------------------------------------------------------------

//{ Returns the value of the parameter holding the returnvalue after execution
//  of an ADO stored procedure. }
//
//function GetSPReturnValue(AADOCommand : TADOCommand) : Variant;
//var
//  I : Integer;
//begin
//  for I := 0 to AADOCommand.Parameters.Count - 1 do
//    if AADOCommand.Parameters[I].Direction = pdReturnValue then
//      Result := AADOCommand.Parameters[I].Value;
//end;

//-----------------------------------------------------------------------------

procedure KeyValuesToFields(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues);
var
  I : Integer;
begin
  if not (ADataSet.State in dsEditModes) then
    raise Exception.Create('DataSet not in EditMode');

  for I := 0 to AKeyValues.Count - 1 do
    ADataSet[AKeyValues.Items[I].Name] := AKeyValues.Items[I].Value;
end;

{}

procedure KeyValuesToFields(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues;
                            const AFields    : array of string);
var
  I : Integer;
begin
  if not (ADataSet.State in dsEditModes) then
    raise Exception.Create('DataSet not in EditMode');

  for I := Low(AFields) to High(AFields) do
    ADataSet[AFields[I]] := AKeyValues[AFields[I]];
end;

//-----------------------------------------------------------------------------

{}

procedure FieldsToKeyValues(      ADataSet   : TDataSet;
                                  AKeyValues : TtsKeyValues;
                            const AFields    : array of string);
var
  I : Integer;
begin
  for I := Low(AFields) to High(AFields) do
    AKeyValues[AFields[I]] := ADataSet[AFields[I]];
end;

//-----------------------------------------------------------------------------

{ Post any pending changes in the given dataset. Returns True if any changes
  were posted. }

function PostData(ADataSet : TDataSet) : Boolean;
begin
  Result := Assigned(ADataSet) and ADataSet.Active and
            (ADataSet.State in dsEditModes);
  if Result then
    ADataSet.Post;
end;

//-----------------------------------------------------------------------------

procedure ShowDataSet(ADataSet : TDataSet);
var
  Form : TForm;
  DataSource : TDataSource;
  GV         : TDBGrid;
begin
  Form := TForm.Create(nil); // prevent freenotification messages
  try
    DataSource := TDataSource.Create(Form);
    DataSource.DataSet := ADataSet;
    GV         := TDBGrid.Create(Form);
    GV.Parent  := Form;
    GV.Align   := alClient;
    GV.DataSource := DataSource;
    GV.AutoAdjustColumns;
//    GV.AutoSizeColumns;
    Form.Width := 800;
    Form.Height := 600;
    Form.Position := poScreenCenter;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

//-----------------------------------------------------------------------------

function DataSetToString(ADataSet : TDataSet;
                         AFields  : array of string): string; overload;
var
  I     : Integer;
  N     : Integer;
  F     : TField;
  sTxt  : string;
  sLine : string;
  sFmt  : string;
  BM    : TBookmark;
begin
  sLine := '';
  sTxt  := '';
  ADataSet.DisableControls;
  try
    BM := ADataSet.GetBookmark;
    try
      AutoSizeDisplayWidths(ADataSet);
      ADataSet.First;
      for I := Low(AFields) to High(AFields) do
      begin
        F := ADataSet.FieldByName(AFields[I]);
        if Assigned(F) then
        begin
          N := F.DisplayWidth;
          sFmt := '%' + IntToStr(N) + 's';
          sLine := sLine + '+' + Format(sFmt, [DupeString('-', N)]);
          sTxt := sTxt + '|' + Format(sFmt, [F.FieldName]);
        end;
      end;
      sTxt := sTxt + '|';
      sLine := sLine + '+';
      Result := sLine + #13#10 + sTxt + #13#10 + sLine;

      while not ADataSet.Eof do
      begin
        sTxt := '';
        for I := Low(AFields) to High(AFields) do
        begin
          F := ADataSet.FieldByName(AFields[I]);
          if Assigned(F) then
          begin
            N := F.DisplayWidth;
            sFmt := '%' + IntToStr(N) + 's';
            sTxt := sTxt + '|' + Format(sFmt, [F.AsString]);
          end;
        end;
        sTxt := sTxt + '|';
        Result := Result + #13#10 + sTxt;
        ADataSet.Next;
      end;
      Result := Result + #13#10 + sLine;
    finally
      ADataSet.GotoBookmark(BM);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

function DataSetToString(ADataSet : TDataSet): string; overload;
var
  I     : Integer;
  N     : Integer;
  F     : TField;
  sTxt  : string;
  sLine : string;
  sFmt  : string;
begin
  sLine := '';
  sTxt  := '';
  ADataSet.DisableControls;
  try
    AutoSizeDisplayWidths(ADataSet);
    ADataSet.First;
    for I := 0 to ADataSet.Fields.Count - 1 do
    begin
      F := ADataSet.Fields[I];
      if Assigned(F) then
      begin
        N := F.DisplayWidth;
        sFmt := '%' + IntToStr(N) + 's';
        sLine := sLine + '+' + Format(sFmt, [DupeString('-', N)]);
        sTxt := sTxt + '|' + Format(sFmt, [F.FieldName]);
      end;
    end;
    sTxt := sTxt + '|';
    sLine := sLine + '+';
    Result := sLine + #13#10 + sTxt + #13#10 + sLine;

    while not ADataSet.Eof do
    begin
      sTxt := '';
      for I := 0 to ADataSet.Fields.Count - 1 do
      begin
        F := ADataSet.Fields[I];
        if Assigned(F) then
        begin
          N := F.DisplayWidth;
          sFmt := '%' + IntToStr(N) + 's';
          sTxt := sTxt + '|' + Format(sFmt, [F.AsString]);
        end;
      end;
      sTxt := sTxt + '|';
      Result := Result + #13#10 + sTxt;
      ADataSet.Next;
    end;
    Result := Result + #13#10 + sLine;
  finally
    ADataSet.EnableControls;
  end;
end;

end.
