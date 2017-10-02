{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

 unit ts.Editor.CodeFilter.Data;

{$MODE DELPHI}

interface

{ Data structure used to feed the CodeFilter view. }

uses
  Classes, SysUtils;

type
  TLine = class(TPersistent)
  strict private
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

constructor TLine.Create(const AIndex: Integer; const AText: string);
begin
  inherited Create;
  FIndex := AIndex;
  FText  := AText;
end;

end.

