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

unit ts.Editor.HexEditor.Settings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils;

const
  DEFAULT_WIDTH = 400;

type

  { THexEditorSettings }

  THexEditorSettings = class(TComponent)
  private
    FWidth: Integer;
  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$region 'construction and destruction' /fold}
procedure THexEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := DEFAULT_WIDTH;
end;
{$endregion}

{$region 'public methods' /fold}
procedure THexEditorSettings.AssignTo(Dest: TPersistent);
var
  S: THexEditorSettings;
begin
  if Dest is THexEditorSettings then
  begin
    S := THexEditorSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure THexEditorSettings.Assign(Source: TPersistent);
var
  S: THexEditorSettings;
begin
  if Source is THexEditorSettings then
  begin
    S := THexEditorSettings(Source);
    Width := S.Width;
  end
  else
    inherited Assign(Source);
end;
{$endregion}

initialization
  RegisterClass(THexEditorSettings);

end.


