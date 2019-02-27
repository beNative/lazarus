{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit SnippetSource.Settings;

{$MODE DELPHI}

interface

{ Application settings object for SnippetSource. }

uses
  Classes, SysUtils,

  ts.Editor.Settings,

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

