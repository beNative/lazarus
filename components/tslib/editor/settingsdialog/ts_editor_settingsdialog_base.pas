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

unit ts_Editor_SettingsDialog_Base;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,

  ts.Editor.Interfaces;

type

  { TCustomSettingsDialog }

  TCustomSettingsDialog = class(TForm)
  strict private
    FManager: IEditorManager;
    function GetManager: IEditorManager;
    function GetSettings: IEditorSettings;

  strict protected
    procedure LinkProperty(
            AInstance     : IInterface;
            ALink         : TPropertyLink;
      const APropertyName : string = ''
    );

    procedure AutoLinkChildControls(
      AInstance : IInterface;
      AControl  : TWinControl
    );

  public
    { public declarations }
    property Manager: IEditorManager
      read GetManager;

    property Settings: IEditorSettings
      read GetSettings;

  end;

implementation

{$R *.lfm}

uses
  Character, typinfo;

{ TCustomSettingsDialog }

function TCustomSettingsDialog.GetManager: IEditorManager;
begin
  Result := Owner.Owner as IEditorManager;
end;

function TCustomSettingsDialog.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

{ If APropertyName is not assigned the propertyname will be extracted from
  the name of the control. }

procedure TCustomSettingsDialog.LinkProperty(AInstance: IInterface;
  ALink: TPropertyLink; const APropertyName: string);
var
  S : string;
  N : Integer;
  P : TPersistent;

  function GetPrefixLength(const AString: string): Integer;
  var
    C: Char;
    N: Integer;
  begin
    N := 0;
    if Length(AString) > 0 then
    begin
      C := AString[1];
      while (N < Length(AString)) and TCharacter.IsLower(C) do
      begin
        Inc(N);
        C := AString[N + 1];
      end;
    end;
    Result := N;
  end;

begin
  P := TPersistent((AInstance as IInterfaceComponentReference).GetComponent);
  if APropertyName = '' then
  begin
    S := ALink.Owner.Name;
    N := GetPrefixLength(ALink.Owner.Name);
    if N > 0 then
      S := Copy(S, N + 1, Length(S) - N + 1)
  end
  else
    S := APropertyName;
  ALink.SetObjectAndProperty(P, S);
end;

procedure TCustomSettingsDialog.AutoLinkChildControls(AInstance: IInterface;
  AControl: TWinControl);
var
  C : TControl;
  I : Integer;
begin
  for I := 0 to AControl.ControlCount - 1 do
  begin
    C := AControl.Controls[I];
    if IsPublishedProp(C, 'Link') then
      LinkProperty(AInstance, GetObjectProp(C, 'Link') as TPropertyLink);
  end;
end;



end.

