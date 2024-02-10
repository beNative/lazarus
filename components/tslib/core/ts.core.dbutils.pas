{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.DBUtils;

{ Collection of common DB routines.

  Author: Tim Sinaeve }

interface

uses
  SysUtils, Classes, Variants, Graphics, DB, DBGrids,
  BufDataset;

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

function FieldTypeForVariant(const Value: Variant): TFieldType;

procedure CloneDataSet(ASource: TDataSet; ADest: TBufDataset);

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

function PostData(ADataSet : TDataSet) : Boolean;

procedure ShowDataSet(ADataSet : TDataSet);

function DataSetToString(ADataSet : TDataSet;
                         AFields  : array of string): string; overload;

function DataSetToString(ADataSet : TDataSet): string; overload;

implementation

uses
  TypInfo, Forms, Controls, Dialogs, StrUtils;

resourcestring
  SQueryLookupErrorRunningQuery = 'Error running query [%s]';
  SQueryLookupTooManyRecords    = 'The query [%s] returned too many records';
  SNoFieldTypeForVariantValue   = 'No fieldtype for Variant value %s';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';

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
            [ftMemo, ftWideMemo, ftString, ftWideString]  then
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

procedure ClearFieldValue(AField: TField; ADoPost: Boolean);
begin
  if not Assigned(AField) then
    raise Exception.Create('AField is not assigned');

  AField.DataSet.Edit;
  AField.Clear;
  if ADoPost then
    AField.DataSet.Post;
end;

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

{ Post any pending changes in the given dataset. Returns True if any changes
  were posted. }

function PostData(ADataSet : TDataSet) : Boolean;
begin
  Result := Assigned(ADataSet) and ADataSet.Active and
            (ADataSet.State in dsEditModes);
  if Result then
    ADataSet.Post;
end;

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
    Form.Width := 800;
    Form.Height := 600;
    Form.Position := poScreenCenter;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

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
