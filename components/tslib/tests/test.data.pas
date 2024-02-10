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

unit Test.Data;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

type
  TTestClass = class(TObject)
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;

  public
    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestString: string
      read FTestString write FTestString;
  end;

  TTestRecord = record
  public
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;

    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestString: string
      read FTestString write FTestString;

  end;

implementation

end.

