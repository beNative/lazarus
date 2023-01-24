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

unit ts.Editor.HexEditor.Settings;

{$MODE DELPHI}

interface

uses
  Classes;

const
  DEFAULT_WIDTH = 400;

type
  THexEditorSettings = class(TComponent)
  private
    FWidth : Integer;

  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;

  end;

implementation

{$REGION 'construction and destruction'}
procedure THexEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := DEFAULT_WIDTH;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure THexEditorSettings.AssignTo(Dest: TPersistent);
var
  S: THexEditorSettings;
begin
  if Dest is THexEditorSettings then
  begin
    S := THexEditorSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure THexEditorSettings.Assign(Source: TPersistent);
var
  S: THexEditorSettings;
begin
  if Source is THexEditorSettings then
  begin
    S := THexEditorSettings(Source);
    Width := S.Width;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(THexEditorSettings);

end.
