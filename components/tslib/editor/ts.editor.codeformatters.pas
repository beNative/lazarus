{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TCPPFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TJavaFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TCSharpFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TXMLFormatter = class(TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  THTMLFormatter = class(TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

implementation

uses
  Forms,
  Process, FileUtil,

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
  S := GetApplicationPath + AExeName;
  T := GetApplicationPath + ATempFile;
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

function TCPPFormatter.Format(const AString: string): string;
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

{
  Poor Man's T-SQL Formatter - a small free Transact-SQL formatting
  library for .Net 2.0, written in C#. Distributed under AGPL v3.
  Copyright (C) 2011 Tao Klerks
  v1.0.1.23412

  Usage notes:

  SqlFormatter <filename or pattern> <options>

  is  indentString (default: \t)
  st  spacesPerTab (default: 4)
  mw  maxLineWidth (default: 999)
  tc  trailingCommas (default: false)
  sac spaceAfterExpandedComma (default: false)
  ebc expandBetweenConditions (default: true)
  ebe expandBooleanExpressions (default: true)
  ecs expandCaseStatements (default: true)
  ecl expandCommaLists (default: true)
  uk  uppercaseKeywords (default: true)
  sk  standardizeKeywords (default: false)
  e   extensions (default: sql)
  r   recursive (default: false)
  b   backups (default: true)
  b   outputFileOrFolder (default: none; if set, overrides the backup option)
  h ? help

  Disable boolean options with a trailing minus, enable by just specifying them or
   with a trailing plus.

  eg:

  SqlFormatter TestFiles\* /is:"  " /tc /uc-

  or

  SqlFormatter test*.sql /o:resultfile.sql
}

//function TSQLFormatter.Format(const AString: string): string;
//begin
//  Result := RunFormatterProcess(
//    'SQLFormatter.exe',
//    '%s /is:"  " /st:2 /mw:80 /tc /uk-',
//    AString,
//    'Formatter.sql'
//  );
//end;

function TXMLFormatter.Format(const AString: string): string;
begin
  Result := ts.Editor.Utils.FormatXML(AString);
end;

function THTMLFormatter.Format(const AString: string): string;
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

