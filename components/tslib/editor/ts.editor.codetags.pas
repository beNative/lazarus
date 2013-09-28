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

unit ts.Editor.CodeTags;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,

  ts.Core.Collections;

type
  TCodeTagItem = class(TCollectionItem)
  strict private
    FEndTag   : string;
    FStartTag : string;

    procedure SetEndTag(AValue: string);
    procedure SetStartTag(AValue: string);

  published
    property StartTag: string
      read FStartTag write SetStartTag;

    property EndTag: string
      read FEndTag write SetEndTag;
  end;

  TCodeTags = class(TOwnedCollection<TCodeTagItem>)
  end;

implementation

{$region 'property access mehods' /fold}
procedure TCodeTagItem.SetStartTag(AValue: string);
begin
  if FStartTag=AValue then Exit;
  FStartTag:=AValue;
end;

procedure TCodeTagItem.SetEndTag(AValue: string);
begin
  if FEndTag=AValue then Exit;
  FEndTag:=AValue;
end;
{$endregion}

end.

