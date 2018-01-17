{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Search.Engine.Settings;

{$MODE DELPHI}

interface

uses
  Classes,

  SynEditTypes;

type
  TSearchEngineSettings = class(TComponent)
  private
    FOptions        : TSynSearchOptions;
    FSearchAllViews : Boolean;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Options : TSynSearchOptions
      read FOptions write FOptions;

    property SearchAllViews: Boolean
      read FSearchAllViews write FSearchAllViews default False;

  end;

implementation

procedure TSearchEngineSettings.AssignTo(Dest: TPersistent);
var
  SES: TSearchEngineSettings;
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


