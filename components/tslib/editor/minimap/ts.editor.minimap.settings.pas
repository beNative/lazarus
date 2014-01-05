{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.MiniMap.Settings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils;

const
  DEFAULT_WIDTH = 400;

type

  { TMiniMapSettings }

  TMiniMapSettings = class(TComponent)
  private
    FWidth: Integer;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{ TMiniMapSettings }

{$region 'public methods' /fold}
procedure TMiniMapSettings.AssignTo(Dest: TPersistent);
var
  S: TMiniMapSettings;
begin
  if Dest is TMiniMapSettings then
  begin
    S := TMiniMapSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TMiniMapSettings.Assign(Source: TPersistent);
var
  S: TMiniMapSettings;
begin
  if Source is TMiniMapSettings then
  begin
    S := TMiniMapSettings(Source);
    Width := S.Width;
  end
  else
    inherited Assign(Source);
end;
{$endregion}

initialization
  RegisterClass(TMiniMapSettings);

end.

