{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.Value;

{$MODE DELPHI}

{$REGION 'Original license info' /FOLD}
(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the Iztok Kacin nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
*)
{$ENDREGION}
{
  Modifications by Tim Sinaeve:
  - Some adjustments to make this unit compile with FPC.
  - Renamed TAnyValue to TValue as this type does not exist (yet?) for FPC.
  - Removed array support
}

interface

uses
  SysUtils, Classes, TypInfo, Variants,

  LCLIntf, LCLType,

  ts.Core.Utils;

const
  atInteger       = 0;
  atBoolean       = 1;
  atChar          = 2;
  atExtended      = 3;
  atString        = 4;
  atPointer       = 5;
  atPChar         = 6;
  atObject        = 7;
  atClass         = 8;
  atWideChar      = 9;
  atPWideChar     = 10;
  atAnsiString    = 11;
  atCurrency      = 12;
  atVariant       = 13;
  atInterface     = 14;
  atWideString    = 15;
  atInt64         = 16;
  atUnicodeString = 17;
  atCardinal      = 18;
  atDouble        = 19;

type
  TValueType = (
    vtNull,
    vtBoolean,
    vtInteger,
    vtInt64,
    vtCardinal,
    vtFloat,
    vtString,
    vtObject,
    vtPointer,
    vtInterface,
    vtAnsiString,
    vtWideString,
    vtDateTime,
    vtDouble,
    vtVariant,
    vtException
  );

  // pointers to records
  PValue = ^TValue;
  IValue = interface;

  TValue = packed record
  private
    FData: IValue;
    function GetAsInt64: Int64; inline;
    function GetAsFloat: Extended; inline;
    function GetAsDouble: Double; inline;
    function GetAsString: string; inline;
    function GetAsObject: TObject; inline;
    function GetAsBoolean: Boolean; inline;
    function GetAsInteger: Integer; inline;
    function GetAsPointer: Pointer; inline;
    function GetAsVariant: Variant; inline;
    function GetAsCardinal: Cardinal; inline;
    function GetAsDateTime: TDateTime; inline;
    function GetAsInterface: IInterface; inline;
    function GetAsWideString: WideString; inline;
    // complex non inline getters
    function GetAsInt64WithCast: Int64;
    function GetAsFloatWithCast: Extended;
    function GetAsStringWithCast: string;
    function GetAsDoubleWithCast: Double;
    function GetAsObjectWithCast: TObject;
    function GetAsBooleanWithCast: Boolean;
    function GetAsIntegerWithCast: Integer;
    function GetAsPointerWithCast: Pointer;
    function GetAsVariantWithCast: Variant;
    function GetAsCardinalWithCast: Cardinal;
    function GetAsDateTimeWithCast: TDateTime;
    function GetAsWideStringWithCast: WideString;
  {$IFDEF UNICODE}
    function GetAsAnsiString: AnsiString; inline;
    function GetAsAnsiStringWithCast: AnsiString;
  {$ENDIF}
    procedure SetAsInt64(const Value: Int64); inline;
    procedure SetAsFloat(const Value: Extended); inline;
    procedure SetAsDouble(const Value: Double); inline;
    procedure SetAsString(const Value: string); inline;
    procedure SetAsObject(const Value: TObject); inline;
    procedure SetAsBoolean(const Value: Boolean); inline;
    procedure SetAsInteger(const Value: Integer); inline;
    procedure SetAsPointer(const Value: Pointer); inline;
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsCardinal(const Value: Cardinal); inline;
    procedure SetAsDateTime(const Value: TDateTime); inline;
    procedure SetAsInterface(const Value: IInterface); inline;
    procedure SetAsWideString(const Value: WideString); inline;
  {$IFDEF UNICODE}
    procedure SetAsAnsiString(const Value: AnsiString); inline;
  {$ENDIF}
    procedure EnsureValueObject;
  public
    procedure Clear;
    function ToString: string;
    function IsNil: Boolean; inline;
    function IsEmpty: Boolean; inline;
    function ValueSize: Integer; inline;
    function ValueType: TValueType; inline;
    class function Null: TValue; static; inline;
    function Equal(const Value: TValue): Boolean; overload; inline;
    function Equal(const Value: PValue): Boolean; overload; inline;
    class operator Implicit(const Value: Int64): TValue;
    class operator Implicit(const Value: Boolean): TValue;
    class operator Implicit(const Value: Variant): TValue;
    class operator Implicit(const Value: Cardinal): TValue;
    class operator Implicit(const Value: Extended): TValue;
    //class operator Implicit(const Value: Double): TValue;
    class operator Implicit(const Value: Integer): TValue;
    class operator Implicit(const Value: string): TValue;
    class operator Implicit(const Value: IInterface): TValue;
    class operator Implicit(const Value: WideString): TValue;
  {$IFDEF UNICODE}
    class operator Implicit(const Value: AnsiString): TValue;
  {$ENDIF}
    class operator Implicit(const Value: Pointer): TValue;
    class operator Implicit(const Value: TObject): TValue;
    class operator Implicit(const Value: TDateTime): TValue;
    class operator Implicit(const Value: TValue): Int64; inline;
    //class operator Implicit(const Value: TValue): Double; inline;
    class operator Implicit(const Value: TValue): Variant; inline;
    class operator Implicit(const Value: TValue): Cardinal; inline;
    class operator Implicit(const Value: TValue): Extended; inline;
    class operator Implicit(const Value: TValue): TObject; inline;
    class operator Implicit(const Value: TValue): string; inline;
    class operator Implicit(const Value: TValue): Integer; inline;
    class operator Implicit(const Value: TValue): WideString; inline;
  {$IFDEF UNICODE}
    class operator Implicit(const Value: TValue): AnsiString; inline;
  {$ENDIF}
    class operator Implicit(const Value: TValue): Boolean; inline;
    class operator Implicit(const Value: TValue): Pointer; inline;
    class operator Implicit(const Value: TValue): TDateTime; inline;
    class operator Implicit(const Value: TValue): IInterface; inline;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  {$IFDEF UNICODE}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
  end;

  IValue = interface(IInterface)
  ['{9D866D8B-6FEC-4633-B968-AF8677AF6B40}']
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsDouble: Double;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function GetAsVariant: Variant;
    procedure SetAsVariant(AValue: Variant);
    function ValueType: TValueType;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsInterface: IInterface;
  {$IFDEF UNICODE}
    function GetAsAnsiString: AnsiString;
  {$ENDIF}
    function GetAsWideString: WideString;
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsInterface(const Value: IInterface);
  {$IFDEF UNICODE}
    procedure SetAsAnsiString(const Value: AnsiString);
  {$ENDIF}
    procedure SetAsWideString(const Value: WideString);
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  {$IFDEF UNICODE}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

implementation

type
  TValueObject = class(TInterfacedObject, IValue)
  private
    FValue: TVarRec;
    FValueType: TValueType;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsDouble: Double;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsInterface: IInterface;
  {$IFDEF UNICODE}
    function GetAsAnsiString: AnsiString;
  {$ENDIF}
    function GetAsVariant: Variant;
    function GetAsWideString: WideString;
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsInterface(const Value: IInterface);
  {$IFDEF UNICODE}
    procedure SetAsAnsiString(const Value: AnsiString);
  {$ENDIF}
    procedure SetAsVariant(AValue: Variant);
    procedure SetAsWideString(const Value: WideString);
  public

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ValueType: TValueType;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
  {$IFDEF UNICODE}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

{ TValueObject }

procedure TValueObject.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueType := vtNull;
end;

procedure TValueObject.BeforeDestruction;
begin
  case FValueType of
    vtString: FreeMem({$IFDEF UNICODE}FValue.VPWideChar{$ELSE}FValue.VPChar{$ENDIF});
    vtInterface: IUnknown(FValue.VInterface) := nil;
    vtWideString: FreeMem(FValue.VPWideChar);
    vtDateTime: FreeMem(FValue.VExtended);
    vtFloat: FreeMem(FValue.VExtended);
    vtInt64: FreeMem(FValue.VInt64);
  end;
  inherited BeforeDestruction;
end;

{$IFDEF UNICODE}
function TValueObject.GetAsAnsiString: AnsiString;
begin
  Result := GetAsString;
end;
{$ENDIF}

function TValueObject.GetAsVariant: Variant;
begin
  Result := VarRecToVariant(FValue);
end;

function TValueObject.GetAsBoolean: Boolean;
begin
  Result := FValue.VBoolean;
end;

function TValueObject.GetAsCardinal: Cardinal;
begin
  Result := FValue.VInteger;
end;

function TValueObject.GetAsDateTime: TDateTime;
begin
  Result := FValue.VExtended^;
end;

function TValueObject.GetAsDouble: Double;
begin
    Result := FValue.VExtended^;
end;

function TValueObject.GetAsFloat: Extended;
begin
  Result := FValue.VExtended^;
end;

function TValueObject.GetAsInt64: Int64;
begin
  Result := FValue.VInt64^;
end;

function TValueObject.GetAsInteger: Integer;
begin
  Result := FValue.VInteger;
end;

function TValueObject.GetAsInterface: IInterface;
begin
  Result := IUnknown(FValue.VInterface);
end;

function TValueObject.GetAsObject: TObject;
begin
  Result := FValue.VObject;
end;

function TValueObject.GetAsPointer: Pointer;
begin
  Result := FValue.VPointer;
end;

function TValueObject.GetAsString: string;
begin
  case FValueType of
    vtNull:       Result := '';
    vtBoolean:    Result := BoolToStr(AsBoolean, True);
    vtInteger:    Result := IntToStr(AsInteger);
    vtFloat:      Result := FloatToStr(AsFloat);
    vtWideString: Result := string(AsWideString);
    vtString:
      begin
        {$IFDEF UNICODE}
          Result := FValue.VPWideChar;
        {$ELSE}
          Result := string(FValue.VPChar);
        {$ENDIF}
      end
    else
    begin
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to string',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
    end;
  end;
end;

function TValueObject.GetAsWideString: WideString;
begin
  case FValueType of
    vtNull:       Result := '';
    vtBoolean:    Result := WideString(BoolToStr(AsBoolean, True));
    vtInteger:    Result := WideString(IntToStr(AsInteger));
    vtFloat:      Result := WideString(FloatToStr(AsFloat));
    vtWideString: Result := FValue.VPWideChar;
    vtString:     Result := WideString(AsString);
    else raise Exception.CreateFmt(
      'Value of type <%s> cannot be converted to WideString',
      [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
    );
  end;
end;

{$IFDEF UNICODE}
procedure TValueObject.SetAsAnsiString(const Value: AnsiString);
begin
  AsString := Value;
end;
{$ENDIF}

procedure TValueObject.SetAsVariant(AValue: Variant);
begin
  FValue := VariantToVarRec(AValue);
  FValueType := vtVariant;
end;

procedure TValueObject.SetAsBoolean(const Value: Boolean);
begin
  FValue.VBoolean := Value;
  FValueType := vtBoolean;
end;

procedure TValueObject.SetAsCardinal(const Value: Cardinal);
begin
  GetMem(FValue.VInt64, SizeOf(Int64));
  FValue.VInt64^ := Value;
  FValueType := vtCardinal;
end;

procedure TValueObject.SetAsDateTime(const Value: TDateTime);
begin
  GetMem(FValue.VExtended, SizeOf(Extended));
  FValue.VExtended^ := Value;
  FValueType := vtDateTime;
end;

procedure TValueObject.SetAsDouble(const Value: Double);
begin
  GetMem(FValue.VExtended, SizeOf(Extended));
  FValue.VExtended^ := Value;
  FValueType := vtDouble;
end;

procedure TValueObject.SetAsFloat(const Value: Extended);
begin
  GetMem(FValue.VExtended, SizeOf(Extended));
  FValue.VExtended^ := Value;
  FValueType := vtFloat;
end;

procedure TValueObject.SetAsInt64(const Value: Int64);
begin
  GetMem(FValue.VInt64, SizeOf(Int64));
  FValue.VInt64^ := Value;
  FValueType := vtInt64;
end;

procedure TValueObject.SetAsInteger(const Value: Integer);
begin
  FValue.VInteger := Value;
  FValueType := vtInteger;
end;

procedure TValueObject.SetAsInterface(const Value: IInterface);
begin
  IUnknown(FValue.VInterface) := IUnknown(Value);
  FValueType := vtInterface;
end;

procedure TValueObject.SetAsObject(const Value: TObject);
begin
  FValue.VObject := Value;
  FValueType := vtObject;
end;

procedure TValueObject.SetAsPointer(const Value: Pointer);
begin
  FValue.VPointer := Value;
  FValueType := vtPointer;
end;

procedure TValueObject.SetAsString(const Value: string);
begin
  {$IFDEF UNICODE}
    FValueType := vtWideString;
    GetMem(FValue.VPWideChar, (Length(Value) + 1) * SizeOf(Char));
    StrCopy(FValue.VPWideChar, PChar(Value));
  {$ELSE}
    FValueType := vtString;
    GetMem(FValue.VPChar, (Length(Value) + 1) * SizeOf(Char));
    StrCopy(FValue.VPChar, PChar(Value));
  {$ENDIF}
end;

procedure TValueObject.SetAsWideString(const Value: WideString);
begin
  FValueType := vtWideString;
  GetMem(FValue.VPWideChar, Length(Value) * SizeOf(WideChar));
  Move(Value[1], FValue.VPWideChar, Length(Value) * SizeOf(WideChar));
end;

function TValueObject.ValueType: TValueType;
begin
  Result := FValueType;
end;

{ TValue }

procedure TValue.Clear;
begin
  FData := nil;
end;

function TValue.ToString: string;
const
  INFO = 'ValueType = %s' + LineEnding +
         'Value = %s';
begin
  Result := Format(
    INFO, [
      GetEnumName(TypeInfo(TValueType), Integer(ValueType)),
      AsString
    ]
  );
end;

{$IFDEF UNICODE}
function TValue.GetAsAnsiString: AnsiString;
begin
  if ValueType = vtAnsiString then
    Result := FData.AsAnsiString
  else
    Result := GetAsAnsiStringWithCast;
end;

function TValue.GetAsAnsiStringWithCast: AnsiString;
var
  Element: PValue;
begin
  case ValueType of
    vtNull: Result := '';
    vtInt64: Result := AnsiString(IntToStr(AsInt64));
    vtBoolean: Result := AnsiString(BoolToStr(AsBoolean, True));
    vtCardinal: Result := AnsiString(IntToStr(AsCardinal));
    vtInteger: Result := AnsiString(IntToStr(AsInteger));
    vtDouble: Result := AnsiString(FloatToStr(AsDouble));
    vtFloat: Result := AnsiString(FloatToStr(AsFloat));
    vtException: Result := AnsiString(IntToStr(AsInt64));
    vtVariant: Result := AnsiString(VarToStr(AsVariant));
    vtWideString: Result := AnsiString(AsWideString);
    vtDateTime: Result := AnsiString(DateTimeToStr(AsDateTime));
    vtPointer, vtObject, vtInterface: Result := AnsiString(IntToStr(AsInteger));
    vtString: Result := AnsiString(AsString);
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to AnsiString',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;
{$ENDIF}

procedure TValue.EnsureValueObject;
begin
  if  FData = nil then
    FData := TValueObject.Create;
end;

function TValue.GetAsBoolean: Boolean;
begin
  if ValueType = vtBoolean then
    Result := FData.AsBoolean
  else
    Result := GetAsBooleanWithCast;
end;

function TValue.GetAsBooleanWithCast: Boolean;
begin
  case ValueType of
    vtNull: Result := False;
    vtString: Result := StrToIntDef(AsString, 0) = 1;
    vtVariant: Result := GetAsVariant;
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Boolean',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsCardinal: Cardinal;
begin
  if ValueType = vtCardinal then
    Result := FData.AsCardinal
  else
    Result := GetAsCardinalWithCast;
end;

function TValue.GetAsCardinalWithCast: Cardinal;
begin
  case ValueType of
    vtNull: Result := 0;
    vtVariant: Result := GetAsVariant;
    vtInteger: Result := GetAsInteger;
    vtString: Result := StrToInt(GetAsString);
    vtBoolean: Result := Integer(GetAsBoolean);
    vtWideString: Result := StrToInt(string(GetAsWideString));
 {$IFDEF UNICODE}
    vtAnsiString: Result := StrToInt(string(GetAsAnsiString));
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Cardinal',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsDateTime: TDateTime;
begin
  if ValueType in [vtDateTime, vtFloat] then
    Result := FData.AsDateTime
  else
    Result := GetAsDateTimeWithCast;
end;

function TValue.GetAsDateTimeWithCast: TDateTime;
begin
  case ValueType of
    vtVariant: Result := VarToDateTime(GetAsVariant);
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to TDateTime',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsDouble: Double;
begin
  if ValueType = vtDouble then
    Result := FData.AsDouble
  else
    Result := GetAsDoubleWithCast;
end;

function TValue.GetAsDoubleWithCast: Double;
begin
  case ValueType of
    vtInt64: Result := GetAsInt64;
    vtVariant: Result := GetAsVariant;
    vtInteger: Result := GetAsInteger;
    vtCardinal: Result := GetAsCardinal;
    vtBoolean: Result := Integer(GetAsBoolean);
    vtString: Result := StrToFloat(GetAsString);
    vtWideString: Result := StrToFloat(string(GetAsWideString));
 {$IFDEF UNICODE}
    vtAnsiString: Result := StrToFloat(string(GetAsAnsiString));
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Double',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsFloat: Extended;
begin
  if ValueType = vtFloat then
  begin
  {$IFNDEF CPUX64}
    Result := FData.AsFloat
  {$ELSE}
    Result := FData.AsDouble
  {$ENDIF}
  end
  else
    Result := GetAsFloatWithCast;
end;

function TValue.GetAsFloatWithCast: Extended;
begin
  case ValueType of
    vtInt64: Result := GetAsInt64;
    vtDouble: Result := GetAsDouble;
    vtVariant: Result := AsVariant;
    vtInteger: Result := GetAsInteger;
    vtCardinal: Result := GetAsCardinal;
    vtBoolean: Result := Integer(GetAsBoolean);
    vtString: Result := StrToFloat(GetAsString);
    vtWideString: Result := StrToFloat(string(GetAsWideString));
 {$IFDEF UNICODE}
    vtAnsiString: Result := StrToFloat(string(GetAsAnsiString));
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Float',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsInt64: Int64;
begin
  if ValueType = vtInt64 then
    Result := FData.AsInt64
  else
    Result := GetAsInt64WithCast;
end;

function TValue.GetAsInt64WithCast: Int64;
begin
  case ValueType of
    vtNull: Result := 0;
    vtVariant: Result := GetAsVariant;
    vtInteger: Result := GetAsInteger;
    vtCardinal: Result := GetAsCardinal;
    vtBoolean: Result := Integer(GetAsBoolean);
    vtString: Result := StrToInt64(GetAsString);
    vtWideString: Result := StrToInt64(string(GetAsWideString));
 {$IFDEF UNICODE}
    vtAnsiString: Result := StrToInt64(string(GetAsAnsiString));
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Int64',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsInteger: Integer;
begin
  if ValueType = vtInteger then
    Result := FData.AsInteger
  else
    Result := GetAsIntegerWithCast;
end;

function TValue.GetAsIntegerWithCast: Integer;
begin
  case ValueType of
    vtNull: Result := 0;
    vtVariant: Result := GetAsVariant;
    vtBoolean: Result := Integer(GetAsBoolean);
    vtInt64: Result := Integer(GetAsInt64);
    vtString: Result := StrToInt(GetAsString);
    vtObject: Result := Integer(AsObject);
    vtPointer: Result := Integer(AsPointer);
    vtWideString: Result := StrToInt(string(GetAsWideString));
{$IFDEF UNICODE}
    vtAnsiString: Result := StrToInt(string(GetAsAnsiString));
{$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Integer',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsInterface: IInterface;
begin
  case ValueType of
    vtNull: Result := nil;
    vtVariant: Result := GetAsVariant;
    vtInterface: Result := FData.AsInterface;
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to IInterface',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsObject: TObject;
begin
  if ValueType = vtObject then
    Result := FData.AsObject
  else
    Result := GetAsObjectWithCast;
end;

function TValue.GetAsObjectWithCast: TObject;
begin
  case ValueType of
    vtNull: Result := nil;
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to TObject',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsPointer: Pointer;
begin
  if ValueType = vtPointer then
    Result:= FData.AsPointer
  else
    Result := GetAsPointerWithCast;
end;

function TValue.GetAsPointerWithCast: Pointer;
begin
  case ValueType of
    vtNull: Result := nil;
    vtObject: Result := Pointer(FData.AsObject);
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Pointer',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsString: string;
begin
  if ValueType = vtString then
    Result := FData.AsString
  else
    Result := GetAsStringWithCast;
end;

function TValue.GetAsStringWithCast: string;
begin
  case ValueType of
    vtNull: Result := '';
    vtBoolean: Result := BoolToStr(AsBoolean, True);
    vtCardinal: Result := IntToStr(AsCardinal);
    vtInteger: Result := IntToStr(AsInteger);
    vtInt64: Result := IntToStr(AsInt64);
    vtFloat: Result := FloatToStr(AsFloat);
    vtDouble: Result := FloatToStr(AsDouble);
    vtException: Result := IntToStr(AsInt64);
    vtVariant: Result := VarToStr(AsVariant);
    vtDateTime: Result := DateTimeToStr(AsDateTime);
    vtWideString: Result := string(AsWideString);
    vtPointer, vtObject, vtInterface: Result := IntToStr(AsInteger);
 {$IFDEF UNICODE}
    vtAnsiString: Result := string(AsAnsiString);
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to string',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsVariant: Variant;
begin
  if ValueType = vtVariant then
    Result := FData.AsVariant
  else
    Result := GetAsVariantWithCast;
end;


function TValue.GetAsVariantWithCast: Variant;
begin
  case ValueType of
    vtNull: Result := null;
    vtBoolean: Result := AsBoolean;
    vtInteger: Result := AsInteger;
    vtInt64: Result := AsInt64;
    vtCardinal: Result := AsCardinal;
    vtFloat: Result := AsFloat;
    vtString: Result := AsString;
    vtInterface: Result := AsInterface;
 {$IFDEF UNICODE}
    vtAnsiString: Result := AsAnsiString;
  {$ENDIF}
    vtWideString: Result := AsWideString;
    vtDateTime: Result := AsDateTime;
    vtDouble: Result := AsDouble;
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to Variant',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

function TValue.GetAsWideString: WideString;
begin
  if ValueType = vtWideString then
    Result := FData.AsWideString
  else
    Result := GetAsWideStringWithCast;
end;

function TValue.GetAsWideStringWithCast: WideString;
begin
  case ValueType of
    vtNull: Result := '';
    vtInt64: Result := WideString(IntToStr(AsInt64));
    vtBoolean: Result := WideString(BoolToStr(AsBoolean, True));
    vtCardinal: Result := WideString(IntToStr(AsCardinal));
    vtInteger: Result := WideString(IntToStr(AsInteger));
    vtDouble: Result := WideString(FloatToStr(AsDouble));
    vtFloat: Result := WideString(FloatToStr(AsFloat));
    vtException: Result := WideString(IntToStr(AsInt64));
    vtVariant: Result := WideString(VarToStr(AsVariant));
    vtWideString: Result := WideString(AsWideString);
    vtDateTime: Result := WideString(DateTimeToStr(AsDateTime));
    vtPointer, vtObject, vtInterface: Result := WideString(IntToStr(AsInteger));
 {$IFDEF UNICODE}
    vtAnsiString: Result := WideString(AsAnsiString);
  {$ENDIF}
    else
      raise Exception.CreateFmt(
        'Value of type <%s> cannot be converted to WideString',
        [GetEnumName(TypeInfo(TValueType), Integer(ValueType))]
      );
  end;
end;

class operator TValue.Implicit(const Value: string): TValue;
begin
  Result.AsString := Value;
end;

class operator TValue.Implicit(const Value: Int64): TValue;
begin
  Result.AsInt64 := Value;
end;

class operator TValue.Implicit(const Value: TObject): TValue;
begin
  Result.AsObject := Value;
end;

class operator TValue.Implicit(const Value: IInterface): TValue;
begin
  Result.AsInterface := Value;
end;

class operator TValue.Implicit(const Value: Boolean): TValue;
begin
  Result.AsBoolean := Value;
end;

class operator TValue.Implicit(const Value: Integer): TValue;
begin
  Result.AsInteger := Value;
end;

class operator TValue.Implicit(const Value: Extended): TValue;
begin
  Result.AsFloat := Value;
end;

class operator TValue.Implicit(const Value: TValue): WideString;
begin
  Result := Value.AsWideString;
end;

class operator TValue.Implicit(const Value: TValue): Integer;
begin
  Result := Value.AsInteger;
end;

class operator TValue.Implicit(const Value: TValue): Boolean;
begin
  Result := Value.AsBoolean;
end;

{$IFDEF UNICODE}
class operator TValue.Implicit(const Value: AnsiString): TValue;
begin
  Result.AsAnsiString := Value;
end;
{$ENDIF}

class operator TValue.Implicit(const Value: WideString): TValue;
begin
  Result.AsWideString := Value;
end;

class operator TValue.Implicit(const Value: TDateTime): TValue;
begin
  Result.AsDateTime := Value;
end;

function TValue.IsEmpty: Boolean;
begin
  Result := ValueType = vtNull;
end;

function TValue.IsNil: Boolean;
begin
  Result := (ValueType = vtPointer) and (AsPointer = nil);
end;

class function TValue.Null: TValue;
begin
  Result.Clear;
end;

class operator TValue.Implicit(const Value: TValue): IInterface;
begin
  Result := Value.AsInterface;
end;

class operator TValue.Implicit(const Value: Cardinal): TValue;
begin
  Result.AsCardinal := Value;
end;

class operator TValue.Implicit(const Value: TValue): Cardinal;
begin
  Result := Value.AsCardinal;
end;

class operator TValue.Implicit(const Value: Variant): TValue;
begin
  Result.AsVariant := Value;
end;

class operator TValue.Implicit(const Value: TValue): Variant;
begin
  Result := Value.AsVariant;
end;

class operator TValue.Implicit(const Value: TValue): TObject;
begin
  Result := Value.AsObject;
end;

class operator TValue.Implicit(const Value: TValue): Int64;
begin
  Result := Value.AsInt64;
end;

class operator TValue.Implicit(const Value: TValue): string;
begin
  Result := Value.AsString;
end;

class operator TValue.Implicit(const Value: TValue): Extended;
begin
  Result := Value.AsFloat;
end;

class operator TValue.Implicit(const Value: TValue): TDateTime;
begin
  Result := Value.AsDateTime;
end;

class operator TValue.Implicit(const Value: Pointer): TValue;
begin
  Result.AsPointer := Value;
end;

class operator TValue.Implicit(const Value: TValue): Pointer;
begin
  Result := Value.AsPointer;
end;

//class operator TValue.Implicit(const Value: TValue): Double;
//begin
//  Result := Value.AsDouble;
//end;

//class operator TValue.Implicit(const Value: Double): TValue;
//begin
//  Result.AsDouble := Value;
//end;

function TValue.Equal(const Value: PValue): Boolean;
begin
  Result := ValueType = Value.ValueType;

  if Result then
  begin
    case ValueType of
      vtNull: Result := Value.IsEmpty;
      vtInt64: Result := AsInt64 = Value.AsInt64;
      vtFloat: Result := AsFloat = Value.AsFloat;
      vtDouble: Result := AsDouble = Value.AsDouble;
      vtString: Result := AsString = Value.AsString;
      vtObject: Result := AsObject = Value.AsObject;
      vtPointer: Result := AsPointer = Value.AsPointer;
      vtBoolean: Result := AsBoolean = Value.AsBoolean;
      vtInteger: Result := AsInteger = Value.AsInteger;
      vtVariant: Result := AsVariant = Value.AsVariant;
      vtCardinal: Result := AsCardinal = Value.AsCardinal;
      vtDateTime: Result := AsDateTime = Value.AsDateTime;
      vtInterface: Result := AsInterface = Value.AsInterface;
      vtWideString: Result := AsWideString = Value.AsWideString;
   {$IFDEF UNICODE}
      vtAnsiString: Result := AsAnsiString = Value.AsAnsiString;
    {$ENDIF}
    end;
  end;
end;

function TValue.Equal(const Value: TValue): Boolean;
begin
  Result := Equal(@Value);
end;

{$IFDEF UNICODE}
class operator TValue.Implicit(const Value: TValue): AnsiString;
begin
  Result := Value.AsAnsiString;
end;

procedure TValue.SetAsAnsiString(const Value: AnsiString);
begin
  EnsureValueObject;
  FData.AsAnsiString := Value;
end;
{$ENDIF}

procedure TValue.SetAsBoolean(const Value: Boolean);
begin
  EnsureValueObject;
  FData.AsBoolean := Value;
end;

procedure TValue.SetAsCardinal(const Value: Cardinal);
begin
  EnsureValueObject;
  FData.AsCardinal := Value;
end;

procedure TValue.SetAsDateTime(const Value: TDateTime);
begin
  EnsureValueObject;
  FData.AsDateTime := Value;
end;

procedure TValue.SetAsDouble(const Value: Double);
begin
  EnsureValueObject;
  FData.AsDouble := Value;
end;

procedure TValue.SetAsFloat(const Value: Extended);
begin
  EnsureValueObject;
  FData.AsFloat := Value;
end;

procedure TValue.SetAsInt64(const Value: Int64);
begin
  EnsureValueObject;
  FData.AsInt64 := Value;
end;

procedure TValue.SetAsInteger(const Value: Integer);
begin
  EnsureValueObject;
  FData.AsInteger := Value;
end;

procedure TValue.SetAsInterface(const Value: IInterface);
begin
  EnsureValueObject;
  FData.AsInterface := Value;
end;

procedure TValue.SetAsObject(const Value: TObject);
begin
  EnsureValueObject;
  FData.AsObject := Value;
end;

procedure TValue.SetAsPointer(const Value: Pointer);
begin
  EnsureValueObject;
  FData.AsPointer := Value;
end;

procedure TValue.SetAsString(const Value: string);
begin
  EnsureValueObject;
  FData.AsString := Value;
end;

procedure TValue.SetAsVariant(const Value: Variant);
begin
  EnsureValueObject;
  FData.AsVariant := Value;
end;

procedure TValue.SetAsWideString(const Value: WideString);
begin
  EnsureValueObject;
  FData.AsWideString := Value;
end;

function TValue.ValueSize: Integer;
begin
  Result := 0;

  case ValueType of
    vtNull: Result := 0;
    vtInt64: Result := SizeOf(Int64);
    vtFloat: Result := SizeOf(Extended);
    vtDouble: Result := SizeOf(Double);
    vtObject: Result := SizeOf(TObject);
    vtBoolean: Result := SizeOf(Boolean);
    vtInteger: Result := SizeOf(Integer);
    vtPointer: Result := SizeOf(Pointer);
    vtException: Result := SizeOf(Int64);
    vtVariant:  Result := SizeOf(Variant);
    vtCardinal: Result := SizeOf(Cardinal);
    vtDateTime: Result := SizeOf(TDateTime);
    vtInterface: Result := SizeOf(IInterface);
    vtString: Result := Length(GetAsString) * SizeOf(Char);
    vtWideString: Result := Length(GetAsWideString) * SizeOf(WideChar);
  {$IFDEF UNICODE}
    vtAnsiString: Result := Length(GetAsAnsiString) * SizeOf(AnsiChar);
  {$ENDIF}
  end;
end;

function TValue.ValueType: TValueType;
begin
  EnsureValueObject;
  Result := FData.ValueType;
end;

end.
