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

unit ts.Editor.CodeFilter.Data;

{$MODE DELPHI}

interface

{ Data structure used to feed the CodeFilter view. }

uses
  Classes, SysUtils;

type
  TLine = class(TPersistent)
  private
    FIndex : Integer;
    FText  : string;

  public
    constructor Create(
      const AIndex : Integer;
      const AText  : string
    );

  published
    property Index: Integer
      read FIndex write FIndex;

    property Text: string
      read FText write FText;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TLine.Create(const AIndex: Integer; const AText: string);
begin
  inherited Create;
  FIndex := AIndex;
  FText  := AText;
end;
{$ENDREGION}

end.

