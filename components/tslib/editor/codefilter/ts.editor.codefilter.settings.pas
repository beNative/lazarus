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

unit ts.Editor.CodeFilter.Settings;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Core.FormSettings;

const
  DEFAULT_WIDTH = 400;

type
  TCodeFilterSettings = class(TPersistent)
  private
    FFormSettings : TFormSettings;

    procedure SetFormSettings(AValue: TFormSettings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property FormSettings: TFormSettings
      read FFormSettings write SetFormSettings;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TCodeFilterSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFormSettings := TFormSettings.Create;
end;

procedure TCodeFilterSettings.BeforeDestruction;
begin
  FFormSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TCodeFilterSettings.SetFormSettings(AValue: TFormSettings);
begin
  if FormSettings <> AValue then
    FFormSettings.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TCodeFilterSettings.AssignTo(Dest: TPersistent);
var
  S : TCodeFilterSettings;
begin
  if Dest is TCodeFilterSettings then
  begin
    S := TCodeFilterSettings(Dest);
    S.AssignTo(Dest);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TCodeFilterSettings.Assign(Source: TPersistent);
var
  S : TCodeFilterSettings;
begin
  if Source is TCodeFilterSettings then
  begin
    S := TCodeFilterSettings(Source);
    S.Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TCodeFilterSettings);

end.


