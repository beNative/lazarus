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

unit ts.Editor.Factories.Manager;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Editor.Interfaces;

type
  TEditorManagerFactory = class(TInterfacedObject, IEditorManagerFactory)
  public
    function CreateInstance(
      AOwner                  : TComponent = nil;
      APersistSettings        : Boolean = False;
      const ASettingsFileName : string = ''
    ): IEditorManager; overload;

    function CreateInstance(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ): IEditorManager; overload;
  end;

implementation

uses
  Forms,

  ts.Editor.Manager;

{$REGION 'public methods'}
function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  APersistSettings: Boolean; const ASettingsFileName: string): IEditorManager;
var
  O : TComponent;
  S : string;
begin
  if not Assigned(AOwner) then
    O := Application
  else
    O := AOwner;
  Result := TdmEditorManager.Create(O, nil);
  Result.PersistSettings := APersistSettings;
  if APersistSettings then
  begin
    if ASettingsFileName = '' then
      S := ApplicationName + '.xml'
    else
      S := ASettingsFileName;
    Result.Settings.FileName := S;
  end;
end;

function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
begin
  Result := TdmEditorManager.Create(AOwner, ASettings);
end;
{$ENDREGION}

end.

