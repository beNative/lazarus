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

unit ts.Editor.Factories.Settings;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Editor.Interfaces;

type
  TEditorSettingsFactory = class(TInterfacedObject, IEditorSettingsFactory)
  public
    function CreateInstance(
      AOwner          : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

implementation

uses
  ts.Editor.Settings;

{$REGION 'public methods'}
function TEditorSettingsFactory.CreateInstance(AOwner: TComponent;
  const AFileName: string): IEditorSettings;
var
  ES : IEditorSettings;
begin
  ES := TEditorSettings.Create(AOwner);
  if AFileName <> '' then
  begin
    ES.FileName := AFileName;
    ES.Load;
  end;
  Result := ES;
end;
{$ENDREGION}

end.

