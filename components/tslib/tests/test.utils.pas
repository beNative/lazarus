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

unit Test.Utils;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,
  Test.Data;

type
  TTestUtils = class
    class function CreateTestObject: TTestClass;
    class function CreateTestRecord: TTestRecord;

  end;

implementation

class function TTestUtils.CreateTestObject: TTestClass;
begin
  Result := TTestClass.Create;
  Result.TestBoolean  := True;
  Result.TestChar     := 'A';
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := 45;
  Result.TestString   := 'Carrot';
end;

class function TTestUtils.CreateTestRecord: TTestRecord;
begin
  Result.TestBoolean  := True;
  Result.TestChar     := 'X';
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := 6;
  Result.TestString   := 'Banana';
end;

end.

