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

{ Implements a binding mechanism to bind RTTI-controls to published properties
  in the settings.
  It uses the convention over configuration paradigm. By using consistent naming
  of the controls they will automatically bind to their equivalent properties.
}

unit ts.Editor.SettingsDialog.Base;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,

  ts.Editor.Interfaces;

type
  TCustomSettingsDialog = class(TForm)
  strict private
    function GetManager: IEditorManager;
    function GetSettings: IEditorSettings;

  strict protected
    procedure LinkProperty(
            AInstance     : IInterface;
            ALink         : TPropertyLink;
      const APropertyName : string = ''
    ); overload;

    procedure LinkProperty(
            AInstance     : TPersistent;
            ALink         : TPropertyLink;
      const APropertyName : string = ''
    ); overload;

    procedure AutoLinkChildControls(
      AInstance : IInterface;
      AControl  : TWinControl
    ); overload;

    procedure AutoLinkChildControls(
      AInstance : TPersistent;
      AControl  : TWinControl
    ); overload;

  public
    property Manager: IEditorManager
      read GetManager;

    property Settings: IEditorSettings
      read GetSettings;

  end;

implementation

{$R *.lfm}

uses
  Character, TypInfo;

{$REGION 'property access mehods'}
function TCustomSettingsDialog.GetManager: IEditorManager;
begin
  Result := Owner.Owner as IEditorManager;
end;

function TCustomSettingsDialog.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ If APropertyName is not assigned the propertyname will be extracted from
  the name of the control. }

procedure TCustomSettingsDialog.LinkProperty(AInstance: IInterface;
  ALink: TPropertyLink; const APropertyName: string);
var
  P : TPersistent;
begin
  P := TPersistent((AInstance as IInterfaceComponentReference).GetComponent);
  LinkProperty(P, ALink, APropertyName);
end;

procedure TCustomSettingsDialog.LinkProperty(AInstance: TPersistent;
  ALink: TPropertyLink; const APropertyName: string);
var
  S : string;
  N : Integer;

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
  if APropertyName = '' then
  begin
    S := ALink.Owner.Name;
    N := GetPrefixLength(ALink.Owner.Name);
    if N > 0 then
      S := Copy(S, N + 1, Length(S) - N + 1)
  end
  else
    S := APropertyName;
  ALink.SetObjectAndProperty(AInstance, S);
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
    if C is TWinControl then
      AutoLinkChildControls(AInstance, TWinControl(C));
    if IsPublishedProp(C, 'Link') then // is this a Rtti control?
      LinkProperty(AInstance, GetObjectProp(C, 'Link') as TPropertyLink);
  end;
end;

procedure TCustomSettingsDialog.AutoLinkChildControls(AInstance: TPersistent;
  AControl: TWinControl);
var
  C : TControl;
  I : Integer;
begin
  for I := 0 to AControl.ControlCount - 1 do
  begin
    C := AControl.Controls[I];
    if C is TWinControl then
      AutoLinkChildControls(AInstance, TWinControl(C));
    if IsPublishedProp(C, 'Link') then // is this a Rtti control?
      LinkProperty(AInstance, GetObjectProp(C, 'Link') as TPropertyLink);
  end;
end;
{$ENDREGION}

end.

