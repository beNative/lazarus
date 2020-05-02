{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Settings;

{$MODE DELPHI}

interface

{ Application settings object for SnippetSource. }

uses
  Classes, SysUtils,

  SnippetSource.Interfaces, SnippetSource.Resources;

type

  { TSettings }

  TSettings = class(TComponent, ISettings)
  private
    FFileName       : string;
    FDataBase       : string;

  protected
    function GetDataBase: string;
    procedure SetDataBase(const AValue: string);
    function GetFileName: string;
    procedure SetFileName(const AValue: string);

    procedure Changed;

  public
    procedure AfterConstruction; override;

    procedure Load;
    procedure Save;

    property FileName: string
      read GetFileName write SetFileName;

  published
    property DataBase: string
      read GetDataBase write SetDataBase;
  end;

implementation

uses
  fpjsonrtti,

  Dialogs;

{$REGION 'construction and destruction'}
procedure TSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  Name := 'Settings';
  FFileName := SETTINGS_FILE;
  DataBase := 'snippets.db';
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TSettings.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
    Changed;
  end;
end;

function TSettings.GetDataBase: string;
begin
  Result := FDataBase;
end;

procedure TSettings.SetDataBase(const AValue: string);
begin
  if AValue <> DataBase then
  begin
    FDataBase := AValue;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TSettings.Changed;
begin
// call OnChanged event?
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TSettings.Load;
var
  LDeStreamer : TJSONDeStreamer;
  SL          : TStringList;
  S           : string;
begin
  if FileExists(FileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FileName);
      S := SL.Text;
    finally
      SL.Free;
    end;
    LDeStreamer := TJSONDeStreamer.Create(nil);
    try
      LDeStreamer.JSONToObject(S, Self);
    finally
      LDeStreamer.Free;
    end;
  end;
end;

procedure TSettings.Save;
var
  LStreamer : TJSONStreamer;
  S         : string;
  SL        : TStringList;
begin
  LStreamer := TJSONStreamer.Create(nil);
  try
    LStreamer.Options :=  LStreamer.Options + [jsoComponentsInline];
    S := LStreamer.ObjectToJSON(Self).FormatJSON;
    SL := TStringList.Create;
    try
      SL.Text := S;
      SL.SaveToFile(FileName);
    finally
      SL.Free;
    end;
  finally
    LStreamer.Free;
  end;
end;
{$ENDREGION}

end.

