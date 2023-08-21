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

unit ts.Editor.CodeFormatters;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

type
  ICodeFormatter = interface
  ['{8E423CF0-8F69-476C-9D09-718645ADE97E}']
    function Format(const AString: string): string;
  end;

  TPascalFormatter = class(TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  TCppFormatter = class (TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  TJavaFormatter = class (TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  TCSharpFormatter = class (TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  TXmlFormatter = class(TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  THtmlFormatter = class(TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

  TJsonFormatter = class(TInterfacedObject, ICodeFormatter)
  protected
    function Format(const AString: string): string; virtual;
  end;

implementation

uses
  Forms,
  Process, FileUtil,
  fpjson, jsonparser,

  ts.Editor.Utils,

  ts.Core.Utils;

function RunFormatterProcess(const AExeName: string; const AParams: string;
  const AString: string; const ATempFile: string): string;
var
  LProcess : TProcess;
  SL       : TStringList;
  S        : string;
  T        : string;
begin
  S := GetApplicationConfigPath + AExeName;
  T := GetApplicationConfigPath + ATempFile;
  if FileExists(S) then
  begin
    SL := TStringList.Create;
    try
      SL.Text := AString;
      SL.SaveToFile(T);
      LProcess := TProcess.Create(nil);
      try
        LProcess.Options := [poNoConsole];
        // todo use LProcess.Params property
        LProcess.CommandLine := SysUtils.Format(S + ' ' + AParams, [T]);
        LProcess.Execute;
        while LProcess.Running do
        begin
          Application.ProcessMessages;
        end;
      finally
        FreeAndNil(LProcess);
      end;
      SL.LoadFromFile(T);
      Result := SL.Text;
    finally
      FreeAndNil(SL);
    end;
    if FileExists(T) then
      DeleteFile(T);
  end
  else
    raise Exception.CreateFmt('%s not found!', [S]);
end;

function TJsonFormatter.Format(const AString: string): string;
var
  LJsonData : TJSONData;
begin
  LJsonData := GetJSON(AString);
  Result := LJsonData.FormatJSON;
end;

function TCppFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=allman --indent=spaces=2 --suffix=none --quiet --mode=c %s',
    AString,
    'Formatter.tmp'
  );
end;

function TJavaFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=java --indent=spaces=2 --suffix=none --quiet --mode=java %s',
    AString,
    'Formatter.tmp'
  );
end;

function TCSharpFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=allman --indent=spaces=2 --suffix=none --quiet --mode=cs %s',
    AString,
    'Formatter.tmp'
  );
end;

function TXmlFormatter.Format(const AString: string): string;
begin
  Result := ts.Editor.Utils.FormatXML(AString);
end;

function THtmlFormatter.Format(const AString: string): string;
begin
  Result := ts.Editor.Utils.FormatXML(AString);
end;

function TPascalFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'Formatter.exe',
    '-silent -delphi -config Formatter.config %s',
    AString,
    'Formatter.tmp'
  );
end;

end.

