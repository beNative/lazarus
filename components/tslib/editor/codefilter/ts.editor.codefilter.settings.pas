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

unit ts.Editor.CodeFilter.Settings;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Core.FormSettings;

const
  DEFAULT_WIDTH = 400;

type
  TCodeFilterSettings = class(TComponent)
  private
    FFormSettings: TFormSettings;
    procedure SetFormSettings(AValue: TFormSettings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property FormSettings: TFormSettings
      read FFormSettings write SetFormSettings;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TCodeFilterSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFormSettings := TFormSettings.Create;
end;

procedure TCodeFilterSettings.BeforeDestruction;
begin
  FFormSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TCodeFilterSettings.SetFormSettings(AValue: TFormSettings);
begin
  if FormSettings <> AValue then
    FFormSettings.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TCodeFilterSettings.AssignTo(Dest: TPersistent);
var
  S: TCodeFilterSettings;
begin
  if Dest is TCodeFilterSettings then
  begin
    S := TCodeFilterSettings(Dest);
    S.AssignTo(Dest);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TCodeFilterSettings.Assign(Source: TPersistent);
var
  S: TCodeFilterSettings;
begin
  if Source is TCodeFilterSettings then
  begin
    S := TCodeFilterSettings(Source);
    S.Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TCodeFilterSettings);

end.


