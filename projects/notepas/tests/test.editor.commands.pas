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

unit Test.Editor.Commands;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  fpcunit, testutils, testregistry,

  ts.Editor.Interfaces;

type
  TEditorCommandsTest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestHookUp;

  end;

implementation

{$REGION 'protected methods'}
procedure TEditorCommandsTest.SetUp;
begin

end;

procedure TEditorCommandsTest.TearDown;
begin

end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TEditorCommandsTest.TestHookUp;
begin
  Fail('Write your own test');
end;
{$ENDREGION}

initialization
  RegisterTest(TEditorCommandsTest);
end.

