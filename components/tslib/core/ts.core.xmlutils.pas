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

unit ts.Core.XMLUtils;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  DOM;

function XMLDocumentString(ADoc: TXMLDocument): string;

procedure XMLDocument(const AString: string; ADoc: TXMLDocument);

implementation

uses
  XMLWrite, XMLRead;

function XMLDocumentString(ADoc: TXMLDocument): string;
var
  SS : TStringStream;
begin
  SS := TStringStream.Create('');
  try
    WriteXMLFile(ADoc, SS);
    SS.Position := 0;
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure XMLDocument(const AString: string; ADoc: TXMLDocument);
var
  SS : TStringStream;
begin
  SS := TStringStream.Create('');
  try
    SS.WriteString(AString);
    SS.Position := 0;
    ReadXMLFile(ADoc, SS);
  finally
    SS.Free;
  end;
end;

end.

