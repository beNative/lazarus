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

unit ts.Editor.Search.Engine.Settings;

{$MODE DELPHI}

interface

uses
  Classes,

  SynEditTypes;

type
  TSearchEngineSettings = class(TPersistent)
  private
    FOptions               : TSynSearchOptions;
    FSearchAllViews        : Boolean;
    FSearchAllViewsVisible : Boolean;

  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Options : TSynSearchOptions
      read FOptions write FOptions;

    property SearchAllViews: Boolean
      read FSearchAllViews write FSearchAllViews default False;

    property SearchAllViewsVisible: Boolean
      read FSearchAllViewsVisible write FSearchAllViewsVisible default True;

  end;

implementation

procedure TSearchEngineSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FSearchAllViews        := False;
  FSearchAllViewsVisible := True;
end;

procedure TSearchEngineSettings.AssignTo(Dest: TPersistent);
var
  SES : TSearchEngineSettings;
begin
  if Dest is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(Dest);
    SES.Options        := Options;
    SES.SearchAllViews := SearchAllViews;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSearchEngineSettings.Assign(Source: TPersistent);
var
  SES: TSearchEngineSettings;
begin
  if Source is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(Source);
    Options        := SES.Options;
    SearchAllViews := SES.SearchAllViews;
  end
  else
    inherited Assign(Source);
end;

initialization
  RegisterClass(TSearchEngineSettings);

end.
