{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.XMLUtils;

{$MODE Delphi}

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

