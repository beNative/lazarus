{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Modules.Python;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Dialogs,

  PythonEngine;

type
  TdmPython = class(TDataModule)
    PythonEngine      : TPythonEngine;
    PythonInputOutput : TPythonInputOutput;

  public
    procedure Execute(const AStrings: TStrings);

  end;

var
  dmPython: TdmPython;

implementation

{$R *.lfm}

{$REGION 'public methods'}
procedure TdmPython.Execute(const AStrings: TStrings);
begin
  PythonEngine.ExecStrings(AStrings);
end;
{$ENDREGION}

end.

