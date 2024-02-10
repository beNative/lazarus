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

unit Test.Core.Logger;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,
  fpcunit, testutils, testregistry,

  ts.Core.Logger.Interfaces;

type
  TTestLogger = class(TTestCase)
  public
    class constructor Create;

  published
    procedure Test_Send_method_for_AnsiString_argument;
    procedure Test_Send_method_for_Boolean_argument;
    procedure Test_Send_method_for_Byte_argument;
    procedure Test_Send_method_for_Cardinal_argument;
    procedure Test_Send_method_for_Currency_argument;
    procedure Test_Send_method_for_Double_argument;
    procedure Test_Send_method_for_Enumeration_argument;
    procedure Test_Send_method_for_Extended_argument;
    procedure Test_Send_method_for_FixedInt_argument;
    procedure Test_Send_method_for_FixedUInt_argument;
    procedure Test_Send_method_for_Int16_argument;
    procedure Test_Send_method_for_Int32_argument;
    procedure Test_Send_method_for_Int64_argument;
    procedure Test_Send_method_for_Int8_argument;
    procedure Test_Send_method_for_Integer_argument;
    procedure Test_Send_method_for_LongInt_argument;
    procedure Test_Send_method_for_LongWord_argument;
    procedure Test_Send_method_for_NativeInt_argument;
    procedure Test_Send_method_for_NativeUInt_argument;
    procedure Test_Send_method_for_Set_argument;
    procedure Test_Send_method_for_ShortInt_argument;
    procedure Test_Send_method_for_ShortString_argument;
    procedure Test_Send_method_for_Single_argument;
    procedure Test_Send_method_for_SmallInt_argument;
    procedure Test_Send_method_for_string_argument;
    procedure Test_Send_method_for_UInt16_argument;
    procedure Test_Send_method_for_UInt32_argument;
    procedure Test_Send_method_for_UInt64_argument;
    procedure Test_Send_method_for_UInt8_argument;
    procedure Test_Send_method_for_WideString_argument;
    procedure Test_Send_method_for_Word_argument;

    procedure Test_Send_method_for_static_array_of_string_argument;
    procedure Test_Send_method_for_static_array_of_Integer_argument;
    procedure Test_Send_method_for_dynamic_array_of_Integer_argument;
    procedure Test_Send_method_for_array_of_const_argument;

    procedure Test_Send_method_for_Record_argument;
    procedure Test_Send_method_for_Object_argument;
    procedure Test_Send_method_for_Interface_argument;

    procedure Test_SendDateTime_method;
    procedure Test_SendDate_method;
    procedure Test_SendTime_method;
    procedure Test_SendRect_method;
    procedure Test_SendPoint_method;
    procedure Test_SendPointer_method;
    procedure Test_SendComponent_method;
    procedure Test_SendObject_method;
    procedure Test_SendStrings_method;
    procedure Test_SendColor_method;
    procedure Test_SendInterface_method;
    procedure Test_SendShortCut_method;
    procedure Test_SendBitmap_method;
    procedure Test_SendPersistent_method;
    procedure Test_SendDataSet_method;

  end;

implementation

uses
  Rtti, Forms, Graphics, System.UITypes,
  LCLProc,

  ts.Core.Logger, ts.Core.Logger.Channel.Ipc,

  Test.Data, Test.Utils;

class constructor TTestLogger.Create;
begin
  Logger.Channels.Add(TIpcChannel.Create);
  Logger.Clear; // first few messages can be lost by receiver instance, so
  Logger.Clear; // we send some dummy ones.
  Logger.Clear;
end;

procedure TTestLogger.Test_Send_method_for_AnsiString_argument;
var
  T : AnsiString;
begin
  T := 'Test';
  Logger.Send('TestAnsiString', T);
end;

procedure TTestLogger.Test_Send_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Logger.Send('TestBoolean', T);
end;

procedure TTestLogger.Test_Send_method_for_Byte_argument;
var
  T : Byte;
begin
  T := 128;
  Logger.Send('TestByte', T);
end;

procedure TTestLogger.Test_Send_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 8888888;
  Logger.Send('TestCardinal', T);
end;

procedure TTestLogger.Test_Send_method_for_Currency_argument;
var
  T : Currency;
begin
  T := Pi;
  Logger.Send('TestCurrency', T);
end;

procedure TTestLogger.Test_Send_method_for_Double_argument;
var
  T : Double;
begin
  T := Pi;
  Logger.Send('TestDouble', T);
end;

procedure TTestLogger.Test_Send_method_for_Enumeration_argument;
var
  T : TAlignment;
  V : TValue;
begin
  T := taCenter;
  V := TValue.From<TAlignment>(T); // no implicit conversion like in Delphi
  Logger.Send('TestEnum', V);
end;

procedure TTestLogger.Test_Send_method_for_Extended_argument;
var
  T : Extended;
begin
  T := Pi;
  Logger.Send('TestExtended', T);
end;

procedure TTestLogger.Test_Send_method_for_FixedInt_argument;
var
  T : FixedInt;
begin
  T := 12345;
  Logger.Send('TestFixedInt', T);
end;

procedure TTestLogger.Test_Send_method_for_FixedUInt_argument;
var
  T : FixedUInt;
begin
  T := 12345;
  Logger.Send('TestFixedUInt', T);
end;

procedure TTestLogger.Test_Send_method_for_Int16_argument;
var
  T : Int16;
begin
  T := 12345;
  Logger.Send('TestInt16', T);
end;

procedure TTestLogger.Test_Send_method_for_Int32_argument;
var
  T : Int32;
begin
  T := 12345;
  Logger.Send('TestInt32', T);
end;

procedure TTestLogger.Test_Send_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 1234567899843211234;
  Logger.Send('TestInt64', T);
end;

procedure TTestLogger.Test_Send_method_for_Int8_argument;
var
  T : Int8;
begin
  T := 123;
  Logger.Send('TestInt8', T);
end;

procedure TTestLogger.Test_Send_method_for_Integer_argument;
var
  T : Integer;
begin
  T := MaxInt;
  Logger.Send('TestInteger', T);
end;

procedure TTestLogger.Test_Send_method_for_LongInt_argument;
var
  T : LongInt;
begin
  T := -489;
  Logger.Send('TestLongInt', T);
end;

procedure TTestLogger.Test_Send_method_for_LongWord_argument;
var
  T : LongWord;
begin
  T := 564564123;
  Logger.Send('TestLongWord', T);
end;

procedure TTestLogger.Test_Send_method_for_NativeInt_argument;
var
  T : NativeInt;
begin
  T := 312;
  Logger.Send('TestNativeInt', T);
end;

procedure TTestLogger.Test_Send_method_for_NativeUInt_argument;
var
  T : NativeUInt;
begin
  T := 428;
  Logger.Send('TestNativeUInt', T);
end;

procedure TTestLogger.Test_Send_method_for_Set_argument;
var
  T : TBorderIcons;
  V : TValue;
begin
  T := [biSystemMenu, biMinimize, biMaximize, biHelp];
  V := TValue.From<TBorderIcons>(T);
  Logger.Send('TestSet', V);
end;

procedure TTestLogger.Test_Send_method_for_ShortInt_argument;
var
  T : ShortInt;
begin
  T := -123;
  Logger.Send('TestShortInt', T);
end;

procedure TTestLogger.Test_Send_method_for_ShortString_argument;
var
  T : ShortString;
begin
  T := 'Test ShortString';
  Logger.Send('TestShortString', T);
end;

procedure TTestLogger.Test_Send_method_for_Single_argument;
var
  T : Single;
begin
  T := Pi;
  Logger.Send('TestSingle', T);
end;

procedure TTestLogger.Test_Send_method_for_SmallInt_argument;
var
  T : SmallInt;
begin
  T := -10256;
  Logger.Send('TestSmallInt', T);
end;

procedure TTestLogger.Test_Send_method_for_string_argument;
var
  T : string;
begin
  T := 'Test';
  Logger.Send('TestString', T);
end;

procedure TTestLogger.Test_Send_method_for_UInt16_argument;
var
  T : UInt16;
begin
  T := 12345;
  Logger.Send('TestUInt16', T);
end;

procedure TTestLogger.Test_Send_method_for_UInt32_argument;
var
  T : UInt32;
begin
  T := 12345;
  Logger.Send('TestUInt32', T);
end;

procedure TTestLogger.Test_Send_method_for_UInt64_argument;
var
  T : UInt64;
begin
  T := 1234567890;
  Logger.Send('TestUInt64', T);
end;

procedure TTestLogger.Test_Send_method_for_UInt8_argument;
var
  T : UInt8;
begin
  T := 123;
  Logger.Send('TestUInt8', T);
end;

procedure TTestLogger.Test_Send_method_for_WideString_argument;
var
  T : WideString;
begin
  T := 'Test';
  Logger.Send('TestWideString', T);
end;

procedure TTestLogger.Test_Send_method_for_Word_argument;
var
  T : Word;
begin
  T := 32000;
  Logger.Send('TestWord', T);
end;

procedure TTestLogger.Test_Send_method_for_static_array_of_string_argument;
type
  TMyStaticArrayOfString = array[0..2] of string;
var
  T : TMyStaticArrayOfString;
  V : TValue;
begin
  T[0] := 'one';
  T[1] := 'two';
  T[2] := 'three';
  V := TValue.From<TMyStaticArrayOfString>(T);
  Logger.Send('TestStaticArrayOfString', V);
end;

procedure TTestLogger.Test_Send_method_for_static_array_of_Integer_argument;
type
  TMyStaticArrayOfInteger = array[0..2] of Integer;
var
  T : TMyStaticArrayOfInteger;
  V : TValue;
begin
  T[0] := 1;
  T[1] := 2;
  T[2] := 3;
  V := TValue.From<TMyStaticArrayOfInteger>(T);
  Logger.Send('TestStaticArrayOfInteger', V);
end;

procedure TTestLogger.Test_Send_method_for_dynamic_array_of_Integer_argument;
type
  TIntegerArray = TArray<Integer>;
var
  T : TIntegerArray;
  V : TValue;
begin
  T := [1, 2 , 3, 4, 5];
  V := TValue.FromOpenArray<Integer>(T);

  //V := TValue.From<TIntegerArray>(T);
  Logger.Send('TestTArrayOfInteger', V);
end;

procedure TTestLogger.Test_Send_method_for_array_of_const_argument;
type
  TVarRecArray = array[0..3] of TVarRec;
var
  T : TVarRecArray;
  S : string;
  V : TValue;
begin
  S := 'MyString';
  T[0].VType := vtUnicodeString;
  T[0].VUnicodeString := Pointer(S);
  T[1].VType := vtBoolean;
  T[1].VBoolean := False;
  T[2].VType := vtInteger;
  T[2].VInteger := 4;
  V := TValue.From<TVarRecArray>(T);
  //V := TValue.FromOpenArray<TVarRec>(T);
  Logger.Send('TestArrayOfConst', V);
end;

procedure TTestLogger.Test_Send_method_for_Record_argument;
begin
  Logger.Send('TestRecord', TValue.From<TTestRecord>(TTestUtils.CreateTestRecord));
  Fail('');
end;

procedure TTestLogger.Test_Send_method_for_Object_argument;
var
  T : TObject;
begin
  T := TTestUtils.CreateTestObject;
  try
    Logger.Send('TestObject', T);
  finally
    T.Free;
  end;
end;

procedure TTestLogger.Test_Send_method_for_Interface_argument;
var
  T : IInterface;
begin
  T := TInterfacedObject.Create;
  Logger.Send('TestInterface', T);
end;

procedure TTestLogger.Test_SendDateTime_method;
var
  T : TDateTime;
begin
  T := Now;
  Logger.SendDateTime('TestTDateTime', T);
end;

procedure TTestLogger.Test_SendDate_method;
var
  T : TDate;
begin
  T := Now;
  Logger.SendDate('TestTDate', T);
end;

procedure TTestLogger.Test_SendTime_method;
var
  T : TTime;
begin
  T := Now;
  Logger.SendTime('TestTTime', T);
end;

procedure TTestLogger.Test_SendRect_method;
var
  T : TRect;
begin
  T.Left   := 4;
  T.Right  := 5;
  T.Top    := 8;
  T.Bottom := 10;
  Logger.SendRect('TestTRect', T);
end;

procedure TTestLogger.Test_SendPoint_method;
var
  T : TPoint;
begin
  T.X := 3;
  T.Y := 8;
  Logger.SendPoint('TestTPoint', T);
end;

procedure TTestLogger.Test_SendPointer_method;
var
  T : Pointer;
begin
  T := Application;
  Logger.SendPointer('TestPointer', T);
end;

procedure TTestLogger.Test_SendComponent_method;
var
  T : TComponent;
begin
  T := TForm.Create(nil);
  try
    Logger.SendComponent('TestTComponent', T);
  finally
    T.Free;
  end;
end;

procedure TTestLogger.Test_SendObject_method;
var
  O : TObject;
begin
  O := TTestClass.Create;
  try
    Logger.SendObject('TestTObject', O);
  finally
    O.Free;
  end;
end;

procedure TTestLogger.Test_SendStrings_method;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('One');
    SL.Add('Two');
    SL.Add('Three');
    SL.Add('Four');
    Logger.SendStrings('TestTStringList', SL);
  finally
    SL.Free;
  end;
end;

procedure TTestLogger.Test_SendColor_method;
var
  T : TColor;
begin
  T := clYellow;
  Logger.SendColor('TestTColor', T);
  T := $00C08080;
  Logger.SendColor('TestTColor', T);
end;

procedure TTestLogger.Test_SendInterface_method;
//var
//  T : IInterface;
begin
  //T := TInterfacedObject.Create;
  //Logger.SendInterface('TestIInterface', T);
  Fail('Not implemented');
end;

procedure TTestLogger.Test_SendShortCut_method;
var
  T : TShortCut;
begin
  T := TextToShortCut('CTRL+S');
  Logger.SendShortCut('TestTShortCut', T);
end;

procedure TTestLogger.Test_SendBitmap_method;
var
  T : TBitmap;
begin
  T := TBitmap.Create;
  try
    T.SetSize(100,100);
    T.Canvas.Pen.Color := clRed;
    T.Canvas.Pen.Width := 2;
    T.Canvas.MoveTo(0, 0);
    T.Canvas.LineTo(100, 100);
    T.Canvas.MoveTo(100, 0);
    T.Canvas.LineTo(0, 100);
    Logger.SendBitmap('TestTBitmap', T);
  finally
    T.Free;
  end;
end;

procedure TTestLogger.Test_SendPersistent_method;
var
  F : TFont;
begin
  F := TFont.Create;
  try
    Logger.SendPersistent('Font', F);
  finally
    F.Free;
  end;
end;

procedure TTestLogger.Test_SendDataSet_method;
//var
//  DS : TDataSet;
begin
  //DS := TTestUtils.CreateDataSet(100);
  //try
  //  Logger.SendDataSet('TestDataSet', DS);
  //finally
  //  DS.Free;
  //end;
  Fail('Not supported yet');
end;

initialization
  RegisterTest(TTestLogger);


end.

